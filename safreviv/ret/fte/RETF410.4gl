--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETF410                                                                #
#OBJETIVO     => PROGRAMA DE CONSULTA GENERAL PARA EL MODULO DE RETIROS                 #
#Fecha inicio => Diciembre 18, 2015                                                     #
#########################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
   DEFINE
      g_ar_registros          RECORD
         fecha_rechazo        DATE,
         tipo_archivo         VARCHAR(30),
         tipo_retiro          CHAR(1),
         mascara_archivo      VARCHAR(20),
         cve_rechazo          SMALLINT,
         desc_rechazo         VARCHAR(100),
         cve_afore            SMALLINT,
         nombre_afore         VARCHAR(40),
         nss                  CHAR(11),
         nombre_trabajador    VARCHAR(150),
         importe_sol_92_aivs  DECIMAL(24,6),
         importe_sol_97_aivs  DECIMAL(24,6),
         importe_sdo_92_aivs  DECIMAL(24,6),
         importe_sdo_97_aivs  DECIMAL(24,6),
         importe_dif_92_aivs  DECIMAL(24,6),
         importe_dif_97_aivs  DECIMAL(24,6),
         importe_sol_92_pesos DECIMAL(22,2),
         importe_sol_97_pesos DECIMAL(22,2),
         importe_sdo_92_pesos DECIMAL(22,2),
         importe_sdo_97_pesos DECIMAL(22,2),
         importe_dif_92_pesos DECIMAL(22,2),
         importe_dif_97_pesos DECIMAL(22,2),
         aceptado_rechazado   SMALLINT
      END RECORD
END GLOBALS 

MAIN
   DEFINE
      v_tpo_retiro            CHAR(1),
      v_fecha_inicio          DATE,
      v_fecha_fin             DATE,
      cb_tpo_retiro           ui.ComboBox,
      p_usuario_cod           LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
      p_tipo_ejecucion        SMALLINT, -- forma como ejecutara el programa
      p_s_titulo              STRING,   -- titulo de la ventana
      ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.*,
      p_regresa               SMALLINT

   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETF410.log")

   CLOSE WINDOW SCREEN
   OPEN WINDOW w_consulta WITH FORM "RETF4101"
   
   LET cb_tpo_retiro        = ui.ComboBox.forName("formonly.cb_tpo_retiro")

   CALL cb_tpo_retiro.clear()
   
   -- se capturan los datos para la consulta
   INPUT v_tpo_retiro         ,
         v_fecha_inicio       , -- fecha de inicio de consulta
         v_fecha_fin            -- fecha fin de consulta
   FROM  cb_tpo_retiro        ,
         d_ini                ,
         d_fin
   ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE  )
 
      BEFORE INPUT
         
         -- se llena el combo de tipo de retiro         
         CALL cb_tpo_retiro.clear()
         DECLARE  c_cb_tpo_retiro CURSOR FOR  
         SELECT tpo_retiro, des_corta
         FROM ret_tipo_retiro
         ORDER BY tpo_retiro
        
         CALL cb_tpo_retiro.addItem('0' ,"TODOS")
         
         -- se agregan los tipos de retiro al combo
         FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
            CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
         END FOREACH

         FREE c_cb_tpo_retiro

         LET v_tpo_retiro = 0

         -- la fecha de inicio y fecha fin se inician con la fecha del dia
         LET v_fecha_inicio = TODAY
         LET v_fecha_fin    = TODAY
         
      ON ACTION consultar

         --Valido que los campos no sean nulos
         IF v_fecha_inicio IS NULL THEN
            CALL fn_mensaje("Aviso","La fecha de inicio no debe ser nula","exclamation")
            CONTINUE INPUT
         END IF

         IF v_fecha_fin IS NULL THEN
            CALL fn_mensaje("Aviso","La fecha final no debe de ser nula","exclamation")
            CONTINUE INPUT
         END IF

         IF v_tpo_retiro IS NULL THEN
            CALL fn_mensaje("Aviso","El tipo retiro no puede ser nulo","exclamation")       
            CONTINUE INPUT
         END IF

         IF v_fecha_inicio > v_fecha_fin THEN
            CALL fn_mensaje("Aviso","La fecha inicial no puede ser mayor a la fecha final","exclamation")       
            CONTINUE INPUT
         END IF

         -- Creo tabla temporal
         CALL fn_crea_tabla_temporal()

         -- Realizo la extraccion de información
         CALL fn_solicitud_transferencia(v_fecha_inicio, v_fecha_fin, v_tpo_retiro)
         CALL fn_solicitud_disposicion(v_fecha_inicio, v_fecha_fin, v_tpo_retiro)

         -- Genero el archivo con los registros encontrados
         CALL fn_exporta_a_archivo() RETURNING p_regresa

         IF p_regresa THEN
            CONTINUE INPUT
         END IF

         -- Borro tabla temporal
         CALL fn_borra_tabla_temporal()

         EXIT INPUT

      ON ACTION cancelar
         EXIT INPUT

   END INPUT
   CLOSE WINDOW w_consulta
   
END MAIN

--Función que realiza la busqueda de registros por transferencia
FUNCTION fn_solicitud_transferencia(p_con_ini, p_con_fin, p_tpo_retiro) 
   DEFINE
      p_con_ini            DATE,
      p_con_fin            DATE,
      p_tpo_retiro         CHAR(1),
      r_trans              RECORD LIKE ret_transferencia.*,
      r_rmd                RECORD LIKE ret_matriz_derecho.*,
      r_rct                RECORD LIKE ret_cza_transferencia.*,
      r_trans_rch          RECORD LIKE ret_transferencia_rch.*,
      v_aivs97_sdo         DECIMAL(24,6),
      v_pesos97_sdo        DECIMAL(24,6),
      v_aivsvol_sdo        DECIMAL(24,6),
      v_pesosvol_sdo       DECIMAL(24,6),
      v_query              STRING,
      v_cuenta             INTEGER

   --Primero busco en la tabla ret_transferencia
   LET v_query =  " SELECT rt.*, rmd.*, rct.* ",
                  " FROM ret_transferencia rt INNER JOIN ret_matriz_derecho rmd ",
                  " ON rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho"
                  
   IF p_tpo_retiro != 0 THEN
      LET v_query = v_query CLIPPED, " AND rmd.tpo_retiro = '", p_tpo_retiro, "'"
   END IF

   LET v_query = v_query CLIPPED,   " ,ret_cza_transferencia rct,glo_folio gf  ",
                                    " WHERE rct.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'",
                                    " AND rt.folio = rct.folio                 ",
                                    " AND rt.folio = gf.folio                   ",
                                    " AND gf.proceso_cod = ", g_proceso_cod_ret_transferencia,
                                    " AND gf.status >= 0                       "
   
   DISPLAY "Consulta Transferencias: ", v_query

   LET v_cuenta = 0
      
   PREPARE pr_transferencia FROM v_query
   DECLARE cur_transferencia CURSOR FOR pr_transferencia
   FOREACH cur_transferencia INTO r_trans.*, r_rmd.*, r_rct.*

      INITIALIZE g_ar_registros TO NULL
      
      LET g_ar_registros.fecha_rechazo         = r_rct.f_carga
      LET g_ar_registros.tipo_archivo          = "Transferencia"
      LET g_ar_registros.tipo_retiro           = r_rmd.tpo_retiro
      LET g_ar_registros.mascara_archivo       = r_rct.nombre_archivo
      LET g_ar_registros.cve_rechazo           = r_trans.cod_rechazo
      LET g_ar_registros.cve_afore             = r_trans.cve_afore
      LET g_ar_registros.nombre_trabajador     = r_trans.nombre_afore CLIPPED," ",r_trans.paterno_afore CLIPPED," ",r_trans.materno_afore CLIPPED
      LET g_ar_registros.importe_sol_92_aivs   = 0
      LET g_ar_registros.importe_sdo_92_aivs   = 0
      LET g_ar_registros.importe_dif_92_aivs   = 0
      LET g_ar_registros.importe_sol_92_pesos  = 0
      LET g_ar_registros.importe_sdo_92_pesos  = 0
      LET g_ar_registros.importe_dif_92_pesos  = 0

      IF r_trans.cod_rechazo = 0 THEN
         LET g_ar_registros.aceptado_rechazado = 1
      ELSE
         LET g_ar_registros.aceptado_rechazado = 0
      END IF

      SELECT nss
      INTO g_ar_registros.nss
      FROM afi_derechohabiente
      WHERE id_derechohabiente = r_trans.id_derechohabiente
      
      SELECT des_larga
      INTO g_ar_registros.desc_rechazo
      FROM ret_rechazo
      WHERE cod_rechazo = r_trans.cod_rechazo

      SELECT afore_desc
      INTO g_ar_registros.nombre_afore
      FROM cat_afore
      WHERE afore_cod = r_trans.cve_afore

      --Obtengo saldos
      --Saldo de Cuenta Vivienda 97
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivs97_sdo,v_pesos97_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_trans.id_solicitud
      AND subcuenta = 4

      --Saldo de Cuenta Vivienda Vol Riss
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivsvol_sdo,v_pesosvol_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_trans.id_solicitud
      AND subcuenta = 55

      IF v_aivs97_sdo IS NULL THEN LET v_aivs97_sdo = 0 END IF
      IF v_pesos97_sdo IS NULL THEN LET v_pesos97_sdo = 0 END IF
      IF v_aivsvol_sdo IS NULL THEN LET v_aivsvol_sdo = 0 END IF
      IF v_pesosvol_sdo IS NULL THEN LET v_pesosvol_sdo = 0 END IF

      LET g_ar_registros.importe_sol_97_aivs   = r_trans.aivs_viv97
      LET g_ar_registros.importe_sol_97_pesos  = r_trans.aivs_viv97 * r_rct.precio_fondo
      LET g_ar_registros.importe_sdo_97_aivs   = v_aivs97_sdo + v_aivsvol_sdo
      LET g_ar_registros.importe_sdo_97_pesos  = (v_aivs97_sdo * r_rct.precio_fondo) + (v_aivsvol_sdo * r_rct.precio_fondo)
      LET g_ar_registros.importe_dif_97_aivs   = g_ar_registros.importe_sol_97_aivs - g_ar_registros.importe_sdo_97_aivs
      LET g_ar_registros.importe_dif_97_pesos  = g_ar_registros.importe_sol_97_pesos - g_ar_registros.importe_sdo_97_pesos

      LET v_cuenta = v_cuenta + 1

      INSERT INTO tb_resumen_retiro VALUES (r_trans.id_solicitud,"T",g_ar_registros.*)
      
   END FOREACH

   FREE cur_transferencia


   --Ahora busco en la tabla ret_transferencia_rch (rechazados originalmente)
   LET v_query =  " SELECT rt.*, rct.* ",
                  " FROM ret_transferencia_rch rt, ret_cza_transferencia rct, glo_folio gf ",
                  " WHERE rct.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'",
                  " AND rt.folio = rct.folio ",
                  " AND rt.folio = gf.folio ",
                  " AND gf.proceso_cod = ", g_proceso_cod_ret_transferencia,
                  " AND gf.status >= 0 "

   IF p_tpo_retiro != 0 THEN
      LET v_query = v_query CLIPPED, " AND rt.tpo_retiro = '", p_tpo_retiro, "'"
   END IF

   PREPARE pr_transferencia_rch FROM v_query
   DECLARE cur_transferencia_rch CURSOR FOR pr_transferencia_rch
   FOREACH cur_transferencia_rch INTO r_trans_rch.*, r_rct.*

      INITIALIZE g_ar_registros TO NULL
      
      LET g_ar_registros.fecha_rechazo         = r_rct.f_carga
      LET g_ar_registros.tipo_archivo          = "Transferencia"
      LET g_ar_registros.tipo_retiro           = r_trans_rch.tpo_retiro
      LET g_ar_registros.mascara_archivo       = r_rct.nombre_archivo
      LET g_ar_registros.cve_rechazo           = r_trans_rch.cod_rechazo_1
      LET g_ar_registros.cve_afore             = r_trans_rch.cve_afore
      LET g_ar_registros.nombre_trabajador     = r_trans_rch.nombre_afore CLIPPED," ",r_trans_rch.paterno_afore CLIPPED," ",r_trans_rch.materno_afore CLIPPED
      LET g_ar_registros.importe_sol_92_aivs   = 0
      LET g_ar_registros.importe_sdo_92_aivs   = 0
      LET g_ar_registros.importe_dif_92_aivs   = 0
      LET g_ar_registros.importe_sol_92_pesos  = 0
      LET g_ar_registros.importe_sdo_92_pesos  = 0
      LET g_ar_registros.importe_dif_92_pesos  = 0

      IF r_trans_rch.cod_rechazo_1 = 0 THEN
         LET g_ar_registros.aceptado_rechazado = 1
      ELSE
         LET g_ar_registros.aceptado_rechazado = 0
      END IF

      IF r_trans_rch.nss IS NULL THEN
         SELECT nss
         INTO g_ar_registros.nss
         FROM afi_derechohabiente
         WHERE id_derechohabiente = r_trans_rch.id_derechohabiente
      END IF
      
      SELECT des_larga
      INTO g_ar_registros.desc_rechazo
      FROM ret_rechazo
      WHERE cod_rechazo = r_trans_rch.cod_rechazo_1

      SELECT afore_desc
      INTO g_ar_registros.nombre_afore
      FROM cat_afore
      WHERE afore_cod = r_trans_rch.cve_afore

      LET g_ar_registros.importe_sol_97_aivs   = r_trans_rch.aivs_viv97
      LET g_ar_registros.importe_sol_97_pesos  = r_trans_rch.aivs_viv97 * r_rct.precio_fondo
      LET g_ar_registros.importe_sdo_97_aivs   = 0
      LET g_ar_registros.importe_sdo_97_pesos  = 0
      LET g_ar_registros.importe_dif_97_aivs   = g_ar_registros.importe_sol_97_aivs - g_ar_registros.importe_sdo_97_aivs
      LET g_ar_registros.importe_dif_97_pesos  = g_ar_registros.importe_sol_97_pesos - g_ar_registros.importe_sdo_97_pesos

      LET v_cuenta = v_cuenta + 1

      INSERT INTO tb_resumen_retiro VALUES (0,"T",g_ar_registros.*)

      IF r_trans_rch.cod_rechazo_2 != 0 THEN
         LET g_ar_registros.cve_rechazo           = r_trans_rch.cod_rechazo_2
         SELECT des_larga
         INTO g_ar_registros.desc_rechazo
         FROM ret_rechazo
         WHERE cod_rechazo = r_trans_rch.cod_rechazo_2
         IF r_trans_rch.cod_rechazo_2 = 0 THEN
            LET g_ar_registros.aceptado_rechazado = 1
         ELSE
            LET g_ar_registros.aceptado_rechazado = 0
         END IF
         LET v_cuenta = v_cuenta + 1
         INSERT INTO tb_resumen_retiro VALUES (0,"T",g_ar_registros.*)
      END IF

      IF r_trans_rch.cod_rechazo_3 != 0 THEN
         LET g_ar_registros.cve_rechazo           = r_trans_rch.cod_rechazo_3
         SELECT des_larga
         INTO g_ar_registros.desc_rechazo
         FROM ret_rechazo
         WHERE cod_rechazo = r_trans_rch.cod_rechazo_3
         IF r_trans_rch.cod_rechazo_3 = 0 THEN
            LET g_ar_registros.aceptado_rechazado = 1
         ELSE
            LET g_ar_registros.aceptado_rechazado = 0
         END IF
         LET v_cuenta = v_cuenta + 1
         INSERT INTO tb_resumen_retiro VALUES (0,"T",g_ar_registros.*)
      END IF
      
   END FOREACH

   FREE cur_transferencia_rch
   
END FUNCTION

--Función que realiza la busqueda de registros por transferencia
FUNCTION fn_solicitud_disposicion(p_con_ini, p_con_fin, p_tpo_retiro) 
   DEFINE
      p_con_ini            DATE,
      p_con_fin            DATE,
      p_tpo_retiro         CHAR(1),
      r_disp               RECORD LIKE ret_disposicion.*,
      r_rmd                RECORD LIKE ret_matriz_derecho.*,
      r_rct                RECORD LIKE ret_cza_transferencia.*,
      r_disp_rch           RECORD LIKE ret_disposicion_rch.*,
      v_aivs97_sdo         DECIMAL(24,6),
      v_pesos97_sdo        DECIMAL(24,6),
      v_aivs92_sdo         DECIMAL(24,6),
      v_pesos92_sdo        DECIMAL(24,6),
      v_aivsvol_sdo        DECIMAL(24,6),
      v_pesosvol_sdo       DECIMAL(24,6),
      v_query              STRING,
      v_cuenta             INTEGER,
      v_proceso_cod        SMALLINT

   --Primero busco en la tabla ret_transferencia
   LET v_query =  " SELECT rt.*, rmd.*, rct.*, gf.proceso_cod ",
                  " FROM ret_disposicion rt INNER JOIN ret_matriz_derecho rmd ",
                  " ON rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho"
                  
   IF p_tpo_retiro != 0 THEN
      LET v_query = v_query CLIPPED, " AND rmd.tpo_retiro = '", p_tpo_retiro, "'"
   END IF

   LET v_query = v_query CLIPPED,   " ,ret_cza_disposicion rct,glo_folio gf  ",
                                    " WHERE rct.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'",
                                    " AND rt.folio = rct.folio                 ",
                                    " AND rt.folio = gf.folio                   ",
                                    " AND gf.proceso_cod IN (", g_proceso_cod_ret_disposicion,",",g_proceso_cod_ret_disposicion_pmg,")",
                                    " AND gf.status >= 1                       "
   
   DISPLAY "Consulta Disposiciones: ", v_query

   LET v_cuenta = 0
      
   PREPARE pr_disposicion FROM v_query
   DECLARE cur_disposicion CURSOR FOR pr_disposicion
   FOREACH cur_disposicion INTO r_disp.*, r_rmd.*, r_rct.*, v_proceso_cod

      INITIALIZE g_ar_registros TO NULL
      
      LET g_ar_registros.fecha_rechazo         = r_rct.f_carga
      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            LET g_ar_registros.tipo_archivo       = "Disposicion de Recursos"
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            LET g_ar_registros.tipo_archivo       = "Disposicion PMG"
            EXIT CASE
      END CASE
      LET g_ar_registros.tipo_retiro           = r_rmd.tpo_retiro
      LET g_ar_registros.mascara_archivo       = r_rct.nombre_archivo
      LET g_ar_registros.cve_rechazo           = r_disp.cod_rechazo
      LET g_ar_registros.cve_afore             = r_disp.cve_afore
      LET g_ar_registros.nombre_trabajador     = r_disp.nombre_afore CLIPPED," ",r_disp.paterno_afore CLIPPED," ",r_disp.materno_afore CLIPPED

      IF r_disp.cod_rechazo = 0 THEN
         LET g_ar_registros.aceptado_rechazado = 1
      ELSE
         LET g_ar_registros.aceptado_rechazado = 0
      END IF

      SELECT nss
      INTO g_ar_registros.nss
      FROM afi_derechohabiente
      WHERE id_derechohabiente = r_disp.id_derechohabiente
      
      SELECT des_larga
      INTO g_ar_registros.desc_rechazo
      FROM ret_rechazo
      WHERE cod_rechazo = r_disp.cod_rechazo

      SELECT afore_desc
      INTO g_ar_registros.nombre_afore
      FROM cat_afore
      WHERE afore_cod = r_disp.cve_afore

      --Obtengo saldos
      --Saldo de Cuenta Vivienda 97
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivs97_sdo,v_pesos97_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_disp.id_solicitud
      AND subcuenta = 4

      --Saldo de Cuenta Vivienda 92
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivs92_sdo,v_pesos92_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_disp.id_solicitud
      AND subcuenta = 8

      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            --Saldo de Cuenta Vivienda Vol Riss
            SELECT saldo_acciones,saldo_pesos
            INTO v_aivsvol_sdo,v_pesosvol_sdo
            FROM ret_his_saldo
            WHERE id_solicitud = r_disp.id_solicitud
            AND subcuenta = 55
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            LET v_aivsvol_sdo    = 0
            LET v_pesosvol_sdo   = 0
            EXIT CASE
      END CASE

      IF v_aivs97_sdo IS NULL THEN LET v_aivs97_sdo = 0 END IF
      IF v_pesos97_sdo IS NULL THEN LET v_pesos97_sdo = 0 END IF
      IF v_aivs92_sdo IS NULL THEN LET v_aivs92_sdo = 0 END IF
      IF v_pesos92_sdo IS NULL THEN LET v_pesos92_sdo = 0 END IF
      IF v_aivsvol_sdo IS NULL THEN LET v_aivsvol_sdo = 0 END IF
      IF v_pesosvol_sdo IS NULL THEN LET v_pesosvol_sdo = 0 END IF

      LET g_ar_registros.importe_sol_97_aivs   = r_disp.aivs_viv97
      LET g_ar_registros.importe_sol_97_pesos  = r_disp.aivs_viv97 * r_rct.precio_fondo
      LET g_ar_registros.importe_sol_92_aivs   = r_disp.aivs_viv92
      LET g_ar_registros.importe_sol_92_pesos  = r_disp.aivs_viv92 * r_rct.precio_fondo
      
      LET g_ar_registros.importe_sdo_97_aivs   = v_aivs97_sdo + v_aivsvol_sdo
      LET g_ar_registros.importe_sdo_97_pesos  = (v_pesos97_sdo * r_rct.precio_fondo) + (v_pesosvol_sdo * r_rct.precio_fondo)
      LET g_ar_registros.importe_sdo_92_aivs   = v_aivs92_sdo
      LET g_ar_registros.importe_sdo_92_pesos  = (v_pesos92_sdo * r_rct.precio_fondo)
      
      LET g_ar_registros.importe_dif_97_aivs   = g_ar_registros.importe_sol_97_aivs - g_ar_registros.importe_sdo_97_aivs
      LET g_ar_registros.importe_dif_97_pesos  = g_ar_registros.importe_sol_97_pesos - g_ar_registros.importe_sdo_97_pesos
      LET g_ar_registros.importe_dif_92_aivs   = g_ar_registros.importe_sol_92_aivs - g_ar_registros.importe_sdo_92_aivs
      LET g_ar_registros.importe_dif_92_pesos  = g_ar_registros.importe_sol_92_pesos - g_ar_registros.importe_sdo_92_pesos

      LET v_cuenta = v_cuenta + 1

      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            INSERT INTO tb_resumen_retiro VALUES (r_disp.id_solicitud,"D",g_ar_registros.*)
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            INSERT INTO tb_resumen_retiro VALUES (r_disp.id_solicitud,"P",g_ar_registros.*)
            EXIT CASE
      END CASE
      
   END FOREACH

   FREE cur_disposicion


   --Ahora busco en la tabla ret_disposicion_rch (rechazados originalmente)
   LET v_query =  " SELECT rt.*, rct.*, gf.proceso_cod ",
                  " FROM ret_disposicion_rch rt, ret_cza_disposicion rct, glo_folio gf ",
                  " WHERE rct.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'",
                  " AND rt.folio = rct.folio ",
                  " AND rt.folio = gf.folio ",
                  " AND gf.proceso_cod IN (", g_proceso_cod_ret_disposicion,",",g_proceso_cod_ret_disposicion_pmg,")",
                  " AND gf.status >= 1 "

   IF p_tpo_retiro != 0 THEN
      LET v_query = v_query CLIPPED, " AND rt.tipo_retiro = '", p_tpo_retiro, "'"
   END IF

   PREPARE pr_disposicion_rch FROM v_query
   DECLARE cur_disposicion_rch CURSOR FOR pr_disposicion_rch
   FOREACH cur_disposicion_rch INTO r_disp_rch.*, r_rct.*, v_proceso_cod

      INITIALIZE g_ar_registros TO NULL
      
      LET g_ar_registros.fecha_rechazo         = r_rct.f_carga
      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            LET g_ar_registros.tipo_archivo       = "Disposicion de Recursos"
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            LET g_ar_registros.tipo_archivo       = "Disposicion PMG"
            EXIT CASE
      END CASE
      LET g_ar_registros.tipo_retiro           = r_disp_rch.tipo_retiro
      LET g_ar_registros.mascara_archivo       = r_rct.nombre_archivo
      LET g_ar_registros.cve_rechazo           = r_disp_rch.cod_rechazo_1
      LET g_ar_registros.cve_afore             = r_disp_rch.cve_afore
      LET g_ar_registros.nombre_trabajador     = r_disp_rch.nombre_afore CLIPPED," ",r_disp_rch.paterno_afore CLIPPED," ",r_disp_rch.materno_afore CLIPPED

      IF r_disp_rch.cod_rechazo_1 = 0 THEN
         LET g_ar_registros.aceptado_rechazado = 1
      ELSE
         LET g_ar_registros.aceptado_rechazado = 0
      END IF

      IF r_disp_rch.nss IS NULL THEN
         SELECT nss
         INTO g_ar_registros.nss
         FROM afi_derechohabiente
         WHERE id_derechohabiente = r_disp_rch.id_derechohabiente
      END IF
      
      SELECT des_larga
      INTO g_ar_registros.desc_rechazo
      FROM ret_rechazo
      WHERE cod_rechazo = r_disp_rch.cod_rechazo_1

      SELECT afore_desc
      INTO g_ar_registros.nombre_afore
      FROM cat_afore
      WHERE afore_cod = r_disp_rch.cve_afore

      --Obtengo saldos
      --Saldo de Cuenta Vivienda 97
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivs97_sdo,v_pesos97_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_disp_rch.id_solicitud
      AND subcuenta = 4

      --Saldo de Cuenta Vivienda 92
      SELECT saldo_acciones,saldo_pesos
      INTO v_aivs92_sdo,v_pesos92_sdo
      FROM ret_his_saldo
      WHERE id_solicitud = r_disp_rch.id_solicitud
      AND subcuenta = 8

      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            --Saldo de Cuenta Vivienda Vol Riss
            SELECT saldo_acciones,saldo_pesos
            INTO v_aivsvol_sdo,v_pesosvol_sdo
            FROM ret_his_saldo
            WHERE id_solicitud = r_disp_rch.id_solicitud
            AND subcuenta = 55
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            LET v_aivsvol_sdo    = 0
            LET v_pesosvol_sdo   = 0
            EXIT CASE
      END CASE

      IF v_aivs97_sdo IS NULL THEN LET v_aivs97_sdo = 0 END IF
      IF v_pesos97_sdo IS NULL THEN LET v_pesos97_sdo = 0 END IF
      IF v_aivs92_sdo IS NULL THEN LET v_aivs92_sdo = 0 END IF
      IF v_pesos92_sdo IS NULL THEN LET v_pesos92_sdo = 0 END IF
      IF v_aivsvol_sdo IS NULL THEN LET v_aivsvol_sdo = 0 END IF
      IF v_pesosvol_sdo IS NULL THEN LET v_pesosvol_sdo = 0 END IF

      LET g_ar_registros.importe_sol_97_aivs   = r_disp.aivs_viv97
      LET g_ar_registros.importe_sol_97_pesos  = r_disp.aivs_viv97 * r_rct.precio_fondo
      LET g_ar_registros.importe_sol_92_aivs   = r_disp.aivs_viv92
      LET g_ar_registros.importe_sol_92_pesos  = r_disp.aivs_viv92 * r_rct.precio_fondo
      
      LET g_ar_registros.importe_sdo_97_aivs   = v_aivs97_sdo + v_aivsvol_sdo
      LET g_ar_registros.importe_sdo_97_pesos  = (v_pesos97_sdo * r_rct.precio_fondo) + (v_pesosvol_sdo * r_rct.precio_fondo)
      LET g_ar_registros.importe_sdo_92_aivs   = v_aivs92_sdo
      LET g_ar_registros.importe_sdo_92_pesos  = (v_pesos92_sdo * r_rct.precio_fondo)
      
      LET g_ar_registros.importe_dif_97_aivs   = g_ar_registros.importe_sol_97_aivs - g_ar_registros.importe_sdo_97_aivs
      LET g_ar_registros.importe_dif_97_pesos  = g_ar_registros.importe_sol_97_pesos - g_ar_registros.importe_sdo_97_pesos
      LET g_ar_registros.importe_dif_92_aivs   = g_ar_registros.importe_sol_92_aivs - g_ar_registros.importe_sdo_92_aivs
      LET g_ar_registros.importe_dif_92_pesos  = g_ar_registros.importe_sol_92_pesos - g_ar_registros.importe_sdo_92_pesos

      LET v_cuenta = v_cuenta + 1
      
      CASE v_proceso_cod
         WHEN g_proceso_cod_ret_disposicion
            INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"D",g_ar_registros.*)
            EXIT CASE
         WHEN g_proceso_cod_ret_disposicion_pmg
            INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"P",g_ar_registros.*)
            EXIT CASE
      END CASE

      IF r_disp_rch.cod_rechazo_2 != 0 THEN
         LET g_ar_registros.cve_rechazo           = r_disp_rch.cod_rechazo_2
         SELECT des_larga
         INTO g_ar_registros.desc_rechazo
         FROM ret_rechazo
         WHERE cod_rechazo = r_disp_rch.cod_rechazo_2
         IF r_disp_rch.cod_rechazo_2 = 0 THEN
            LET g_ar_registros.aceptado_rechazado = 1
         ELSE
            LET g_ar_registros.aceptado_rechazado = 0
         END IF
         LET v_cuenta = v_cuenta + 1
         CASE v_proceso_cod
            WHEN g_proceso_cod_ret_disposicion
               INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"D",g_ar_registros.*)
               EXIT CASE
            WHEN g_proceso_cod_ret_disposicion_pmg
               INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"P",g_ar_registros.*)
               EXIT CASE
         END CASE
      END IF

      IF r_disp_rch.cod_rechazo_3 != 0 THEN
         LET g_ar_registros.cve_rechazo           = r_disp_rch.cod_rechazo_3
         SELECT des_larga
         INTO g_ar_registros.desc_rechazo
         FROM ret_rechazo
         WHERE cod_rechazo = r_disp_rch.cod_rechazo_3
         IF r_disp_rch.cod_rechazo_3 = 0 THEN
            LET g_ar_registros.aceptado_rechazado = 1
         ELSE
            LET g_ar_registros.aceptado_rechazado = 0
         END IF
         LET v_cuenta = v_cuenta + 1
         CASE v_proceso_cod
            WHEN g_proceso_cod_ret_disposicion
               INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"D",g_ar_registros.*)
               EXIT CASE
            WHEN g_proceso_cod_ret_disposicion_pmg
               INSERT INTO tb_resumen_retiro VALUES (r_disp_rch.id_solicitud,"P",g_ar_registros.*)
               EXIT CASE
         END CASE
      END IF
      
   END FOREACH

   FREE cur_disposicion_rch
   
END FUNCTION

--Función que exporta datos a un archivo
FUNCTION fn_exporta_a_archivo()
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT

   LET v_regresa = FALSE
   
   -- Reviso que existan registros a generar en el archivo
   SELECT COUNT(*)
   INTO v_cuenta
   FROM tb_resumen_retiro
   IF v_cuenta IS NULL OR v_cuenta <= 0 THEN
      LET v_mensaje_archivo = "Imposible generar el archivo dado que no existen resultados"
      CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
      LET v_regresa = TRUE
      RETURN v_regresa
   END IF
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son TXT para el detalle
   LET v_extension_txt = ".cgar"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Consulta_Rechazos_", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se genera el nombre del archivo de Aceptados
   LET v_nom_archivo = "Consulta_Aceptados_", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Aceptados
   LET v_v_ruta_nomarch2 = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generaran los siguientes archivos:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo de rechazados generado: ", v_v_ruta_nomarch

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "FECHA RECHAZO|TIPO ARCHIVO|TIPO RETIRO / TRANSFERENCIA|MASCARA ARCHIVO|CVE RECHAZO|",
                     "DESCRIPCION RECHAZO|CVE AFORE|AFORE|NSS|NOMBRE DEL TRABAJADOR|",
                     "IMPORTE SOLICITADO 92 AIVS|IMPORTE SOLICITADO 97 AIVS|SALDO 92 AIVS|SALDO 97 AIVS|",
                     "DIFERENCIA 92 AIVS|DIFERENCIA 97 AVIS|IMPORTE SOLICITADO 92 MXN|IMPORTE SOLICITADO 97 MXN|",
                     "SALDO 92 MXN|SALDO 97 MXN|DIFERENCIA 92 MXN|DIFERENCIA 97 MXN"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   -- Ahora imprimo todos los registros Rechazados
   DECLARE c_cursor_a CURSOR FOR
   SELECT *
   FROM tb_resumen_retiro
   WHERE aceptado_rechazado = 0
   ORDER BY id_solicitud
   FOREACH c_cursor_a INTO v_solicitud,v_tipo,g_ar_registros.*
      LET v_s_detalle = g_ar_registros.fecha_rechazo USING "dd/mm/yyyy","|",
                        g_ar_registros.tipo_archivo,"|",
                        g_ar_registros.tipo_retiro,"|",
                        g_ar_registros.mascara_archivo CLIPPED,"|",
                        g_ar_registros.cve_rechazo,"|",
                        g_ar_registros.desc_rechazo CLIPPED,"|",
                        g_ar_registros.cve_afore,"|",
                        g_ar_registros.nombre_afore CLIPPED,"|",
                        g_ar_registros.nss,"|",
                        g_ar_registros.nombre_trabajador,"|",
                        g_ar_registros.importe_sol_92_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sol_97_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sdo_92_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sdo_97_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_dif_92_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_dif_97_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sol_92_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_sol_97_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_sdo_92_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_sdo_97_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_dif_92_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_dif_97_pesos USING "<<<<<<<<<<<<<&.&&"
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()


   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch2, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "NSS|NOMBRE|TIPO RETIRO|AIVS 97|PESOS 97|AIVS 92|PESOS 92|ACEPTADO/RECHAZADO|AFORE"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   -- Ahora imprimo todos los registros Aceptados
   DECLARE c_cursor_b CURSOR FOR
   SELECT *
   FROM tb_resumen_retiro
   WHERE aceptado_rechazado = 1
   ORDER BY id_solicitud
   FOREACH c_cursor_a INTO v_solicitud,v_tipo,g_ar_registros.*
      LET v_s_detalle = g_ar_registros.nss,"|",
                        g_ar_registros.nombre_trabajador,"|",
                        g_ar_registros.tipo_retiro,"|",
                        g_ar_registros.importe_sol_97_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sol_97_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        g_ar_registros.importe_sol_92_aivs USING "<<<<<<<<<<<<<&.&&&&&&","|",
                        g_ar_registros.importe_sol_92_pesos USING "<<<<<<<<<<<<<&.&&","|",
                        "1|",
                        g_ar_registros.cve_afore,"|"
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   LET v_mensaje_archivo = "Los archivos fueron generados exitosamente:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

FUNCTION fn_crea_tabla_temporal()
   TRY
      {CALL fn_borra_tabla_temporal()}
      --
      CREATE TEMP TABLE tb_resumen_retiro (
         id_solicitud         DECIMAL(9,0),
         tipo_arc             CHAR(1),          --(D) Disposiciones, (T) Transferencias, (P) P.M.G.
         fecha_rechazo        DATE,
         tipo_archivo         VARCHAR(30),
         tipo_retiro          CHAR(1),
         mascara_archivo      VARCHAR(20),
         cve_rechazo          SMALLINT,
         desc_rechazo         VARCHAR(100),
         cve_afore            SMALLINT,
         nombre_afore         VARCHAR(40),
         nss                  CHAR(11),
         nombre_trabajador    VARCHAR(150),
         importe_sol_92_aivs  DECIMAL(24,6),
         importe_sol_97_aivs  DECIMAL(24,6),
         importe_sdo_92_aivs  DECIMAL(24,6),
         importe_sdo_97_aivs  DECIMAL(24,6),
         importe_dif_92_aivs  DECIMAL(24,6),
         importe_dif_97_aivs  DECIMAL(24,6),
         importe_sol_92_pesos DECIMAL(24,6),
         importe_sol_97_pesos DECIMAL(24,6),
         importe_sdo_92_pesos DECIMAL(24,6),
         importe_sdo_97_pesos DECIMAL(24,6),
         importe_dif_92_pesos DECIMAL(24,6),
         importe_dif_97_pesos DECIMAL(24,6),
         aceptado_rechazado   SMALLINT
      )
   END TRY
END FUNCTION

FUNCTION fn_borra_tabla_temporal()
   TRY
      DROP TABLE tb_resumen_retiro
   END TRY
END FUNCTION

