--===============================================================                                                             
-- Versión: 1.0.0
-- Fecha ultima modificación:
--===============================================================

##########################################################################
#Módulo:           => GRT                                                #
#Programa:         => GRTP30                                             #
#Objetivo:         => Programa para generar las marcas de los            #
#                     registros aceptados en SAC43 Bis                   #
#Autor:            => Mauro Muñiz Caballero, EFP                         #
#Fecha inicio:     => 8 de noviembre de 2016                             #
#Modifica          => Edgar Damian Estrada Rivera                        #
#Fecha Mod.        => 19 de mayo de 2017                                 #
#Modifica          => Emilio Abarca, EFP.                                #
#Fecha Mod.        => 26 de Octubre 2018                                 #
#Objetivo Mod.     => Conciliación de marcas 221, 223, 225.              #
##########################################################################
DATABASE safre_viv

MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación
   DEFINE p_d_folio                 LIKE cre_ctr_archivo.folio_archivo -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE p_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de la tabla de control
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE r_c_ruta_bin_cta          LIKE seg_modulo.ruta_bin -- ruta bin cta
   DEFINE r_c_ruta_list_cta         LIKE seg_modulo.ruta_bin -- ruta listados cta
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_r_cre_acreditado        RECORD LIKE cre_acreditado.* -- registro de cre acreditado
   DEFINE v_r_cta_marca_ws          RECORD LIKE cta_marca_ws.* -- registro de la tabla de Web Service
   DEFINE v_i_marca_ws              LIKE cat_tipo_credito.marca_prc -- marca del web service
   DEFINE v_i_tot_regs              INTEGER -- total de registros marcados
   DEFINE v_i_edo_marcaje           SMALLINT -- estado del marcaje
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0) --valor de inserción
   DEFINE v_folio_archivo           DECIMAL(9,0) -- valor de inserción
   DEFINE v_execute_fn              STRING  -- variable para iniciar la ejecucion de fn_verifica_id_archivo_grt
   DEFINE v_query                   STRING -- variable para guardar cadena para hacer query (actualiza tablas)
   DEFINE cont                      INTEGER -- contador para valor de f_formalización
   DEFINE cont_reg                  INTEGER -- almacena valor de registro
   --DEFINE v_cre_acre                INTEGER -- variable que recupera la secuencia de valor para cre_acreditado
   DEFINE v_id_cre_acreditado       DECIMAL(9,0)
   DEFINE v_query_reg_cre           STRING -- variable guarda una sentencia SQL a ejecutar en cre_acreditado
   DEFINE v_query_sit               STRING -- variable que guarda la sentencia para contar registro situacion 60
   DEFINE v_query_proc              STRING -- variable que guarda la sentencia para ejecutar fn_procesa_marca_cuenta
   DEFINE v_aceptada                INTEGER
   DEFINE v_rechazada               INTEGER

   DEFINE v_arr_ocg RECORD      -- Datos de registro
      id_derechohabiente            DECIMAL(9,0) ,
      id_ocg_formalizacion          DECIMAL(9,0) ,
      id_ocg_detalle                DECIMAL(9,0) ,
      id_ocg_tramite                DECIMAL(9,0) ,
      f_formalizacion               DATE
   END RECORD

   -- se recuperan los parámetros que envía el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP30.log")

   DISPLAY "=INICIA GRTP30="
   DISPLAY " USUARIO       :",p_v_usuario
   DISPLAY " PID           :",p_d_pid
   DISPLAY " PROCESO       :",p_i_proceso_cod
   DISPLAY " OP_COD        :",p_i_opera_cod
   DISPLAY " FOLIO         :",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       :",p_v_arch_proceso

   -- se inicializan variables
   LET v_c_programa_cod = "GRTP30"

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini( p_d_pid         ,
                                            p_i_proceso_cod ,
                                            p_i_opera_cod   ,
                                            p_d_folio       ,
                                            v_c_programa_cod,
                                            p_v_arch_proceso,
                                            p_v_usuario
                                                )
   DISPLAY "VALIDA", r_b_valida

   -- se verifica si fue posible finalizar la operación
   IF r_b_valida <> 0 THEN
   -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- contador para el registro situación 60
   LET v_query_sit = "SELECT COUNT(*) ",
                      " FROM ocg_formalizacion ",
                     " WHERE situacion = 60"

   PREPARE prp_count_sit60 FROM v_query_sit
   EXECUTE prp_count_sit60 INTO cont_reg

   DISPLAY "total de registros recuperados sit 60:",cont_reg

   -- recuperará registro de control de archivos.

   LET v_execute_fn = "EXECUTE FUNCTION fn_verifica_id_archivo_grt(?)"

    PREPARE prp_fn_verifica_archivo   FROM v_execute_fn
    EXECUTE prp_fn_verifica_archivo   USING p_i_proceso_cod
                                       INTO v_id_cre_ctr_archivo,
                                            v_folio_archivo

   DISPLAY " ID CTR ARCHIVO:",v_id_cre_ctr_archivo

   LET v_query = "SELECT   f.id_derechohabiente,   ",
                         " f.id_ocg_formalizacion, ",
                         " f.id_ocg_detalle,       ",
                         " f.id_ocg_tramite,       ",
                         " a.f_formalizacion       ",
                    " FROM ocg_formalizacion    f, ",
                         " ocg_detalle          d, ",
                         " ocg_acreditado       a  ",
                    " WHERE f.situacion             = 60 ",
                         " and f.id_ocg_detalle        = d.id_ocg_detalle ",
                         " and f.id_ocg_formalizacion  = a.id_ocg_formalizacion ",
                         " and d.subproceso            = 2"

   PREPARE prp_query FROM v_query
   DECLARE crs_query CURSOR FOR prp_query

   --DISPLAY "bandera para fn_verifica:",SQLCA.SQLCODE

   -- si el valor de fecha formalizacion es nulo asignar el valor TODAY
   LET cont = 1

   FOREACH crs_query INTO v_arr_ocg.*
      IF(v_arr_ocg.f_formalizacion IS NULL) THEN
         LET v_arr_ocg.f_formalizacion = TODAY
      END IF

      LET v_query_proc = "EXECUTE FUNCTION fn_grt_origina_43bis(?,?,?,?)"

      PREPARE prp_procesa_marca_cuenta FROM v_query_proc
      EXECUTE prp_procesa_marca_cuenta USING v_id_cre_ctr_archivo,
                                             v_arr_ocg.id_derechohabiente,
                                             v_arr_ocg.f_formalizacion,
                                             v_arr_ocg.id_ocg_formalizacion
                                        INTO v_i_edo_marcaje,
                                             v_aceptada,
                                             v_rechazada,
                                             v_id_cre_acreditado

      -- DISPLAY " SE PROCESA LA MARCA"

      IF v_aceptada = 1 THEN
         -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
         LET v_query_proc = "EXECUTE FUNCTION fn_marca_cuenta_orig(?,?,?,?,?,?,?)"

         -- DISPLAY "", p_v_usuario
         
         PREPARE prp_marca_cuenta_origina  FROM v_query_proc
         EXECUTE prp_marca_cuenta_origina  USING p_v_usuario,
                                                 v_folio_archivo,
                                                 v_id_cre_ctr_archivo,
                                                 p_i_proceso_cod,
                                                 v_id_cre_acreditado,
                                                 v_arr_ocg.id_ocg_formalizacion,
                                                 v_arr_ocg.id_ocg_tramite
                                            INTO v_i_edo_marcaje
                                            
         -- DISPLAY "RETORNO DE FUNCION = ", v_i_edo_marcaje
         
         -- verifica si ocurrió un error durante el proceos de marcaje
         IF v_i_edo_marcaje <> 0 THEN
            -- Ocurrió un error, se muestra el error
            DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

            -- ocurrió un error y se marca como rechazado la operación
            LET r_b_valida = fn_error_opera(p_d_pid          ,
                                            p_i_proceso_cod  ,
                                            p_i_opera_cod  )
            CONTINUE FOREACH
         END IF
      END IF

      LET v_aceptada  = 0
      LET v_rechazada = 0
      LET cont        = cont + 1
   END FOREACH

    -- se realiza el conteo de los registros marcados
   SELECT COUNT(*)
      INTO v_i_tot_regs
      FROM sfr_marca_activa s
     INNER JOIN cre_acreditado c
        ON s.id_derechohabiente   = c.id_derechohabiente
       AND s.n_referencia         = c.id_cre_acreditado
       AND c.id_cre_ctr_archivo   = p_d_id_cre_ctr_arch
     INNER JOIN cat_tipo_credito t
        ON s.marca = t.marca_inf

     DISPLAY "TOTAL DE REGISTROS MARCADOS: ",v_i_tot_regs

   -- confirma que se haya grabado la solicitud de marca a Procesar

   -- se consultan los registros de cre acreditado

   LET v_query_reg_cre = " SELECT c.*,
                             a.nss\n",
                         "   FROM cre_acreditado c,
                             afi_derechohabiente a \n",
                         "  WHERE c.id_cre_ctr_archivo = ",v_id_cre_ctr_archivo,"\n",
                         "  AND c.estado IN(20,152)\n",
                         "  AND c.id_derechohabiente = a.id_derechohabiente"

   PREPARE prp_cre_acreditado FROM v_query_reg_cre
   DECLARE cur_cre_acreditado CURSOR FOR prp_cre_acreditado

  -- DISPLAY "registro acreditado ", v_id_cre_ctr_archivo

   -- se cre sentencia que obtiene la marca correspondiente al tipo de credito
   LET v_s_qryTxt = " SELECT FIRST 1 marca_prc\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ?\n",
                    "  ORDER BY f_actualiza DESC"

   PREPARE prp_slctFrst_marcaPrc FROM v_s_qryTxt

   FOREACH cur_cre_acreditado INTO v_r_cre_acreditado.*
      EXECUTE prp_slctFrst_marcaPrc USING v_r_cre_acreditado.tpo_credito
                                     INTO v_i_marca_ws

  --DISPLAY "marca de credito  ", v_i_marca_ws

   -- se verifica si ya existe el registro en cta marca ws
      SELECT COUNT(*)
        INTO v_i_tot_regs
        FROM cta_marca_ws
       WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente

   -- se verifica si se encontraron registros con el mismo derechohabiente
      IF v_i_tot_regs > 0 THEN
   
   -- DISPLAY "llega el registro existente   ",v_r_cre_acreditado.id_derechohabiente
    
   -- se elimina el registro existente
         DELETE
           FROM cta_marca_ws
          WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      END IF

    -- DISPLAY "elimina registro existente  " ,v_r_cre_acreditado.id_derechohabiente

      -- se asignan los valores del registro a insertar en la tabla de Web services
      
      LET v_r_cta_marca_ws.id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      LET v_r_cta_marca_ws.id_origen          = v_r_cre_acreditado.id_cre_acreditado
      LET v_r_cta_marca_ws.modulo_cod         = "16" -- GRT
      LET v_r_cta_marca_ws.tpo_credito        = v_r_cre_acreditado.tpo_credito
      LET v_r_cta_marca_ws.marca              = v_i_marca_ws
      LET v_r_cta_marca_ws.f_solicita         = TODAY
      LET v_r_cta_marca_ws.intento            = 1
      LET v_r_cta_marca_ws.cod_result_op      = NULL
      LET v_r_cta_marca_ws.diagnostico        = NULL
      LET v_r_cta_marca_ws.situacion          = 2
      LET v_r_cta_marca_ws.num_credito        = v_r_cre_acreditado.num_credito
      LET v_r_cta_marca_ws.f_infonavit        = v_r_cre_acreditado.f_otorga
      LET v_r_cta_marca_ws.marca_procesar     = "02" -- 'grt' => 02 (Créditos en Garantía)
      LET v_r_cta_marca_ws.folio_archivo      = p_d_folio
      LET v_r_cta_marca_ws.usuario            = p_v_usuario

     -- se guarda el registro en la tabla del web service (cta marca ws)
      INSERT INTO cta_marca_ws VALUES (v_r_cta_marca_ws.*)

      UPDATE ocg_acreditado
         SET f_solic_marca_prcr   = TODAY
       WHERE id_ocg_formalizacion IN(SELECT id_referencia_ocg
                                       FROM ocg_transaccion_cre
                                      WHERE id_referencia_cre = id_referencia_cre
                                        AND f_proceso         = TODAY
                                        AND subproceso        = 2)
   END FOREACH

   -- Adecuación EAS
   CALL fn_concilia_marcas(p_v_usuario,p_i_proceso_cod,p_d_folio)
   
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid,
                                           p_i_proceso_cod,
                                           p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN

   -- en caso de error se muestra un mensaje a usuario y no continua
   CALL fn_desplega_inc_operacion(r_b_valida)

   -- ocurrió un error y se marca como rechazado la operación
   LET r_b_valida = fn_error_opera(p_d_pid,
                                      p_i_proceso_cod,
                                      p_i_opera_cod)

   --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

END MAIN

FUNCTION fn_concilia_marcas(p_usuario,p_proceso_cod,p_folio)

   DEFINE p_usuario             LIKE seg_usuario.usuario 
   DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod 
   DEFINE p_folio               LIKE cre_ctr_archivo.folio_archivo 
   DEFINE r_conciliacion        RECORD
      id_referencia        DECIMAL(9,0),
      id_derechohabiente   DECIMAL(9,0),
      marca_orig           SMALLINT,
      folio_marca          DECIMAL(9,0),
      tpo_originar         SMALLINT,
      marca_fin            SMALLINT
   END RECORD
   DEFINE v_cadena              STRING
   DEFINE v_t_43bis             INTEGER
   DEFINE v_id_arh_43           DECIMAL(9,0)
   DEFINE v_folio_arh_43        DECIMAL(9,0)
   DEFINE r_inf_credito         RECORD
     folio_liquida   LIKE cre_acreditado.folio_liquida,
     tpo_credito     LIKE cre_acreditado.tpo_credito,
     num_credito     LIKE cre_acreditado.num_credito,
     sdo_deudor      LIKE cre_acreditado.sdo_deudor,
     f_otorga        LIKE cre_acreditado.f_otorga
   END RECORD
   DEFINE v_folio_archivo         DECIMAL(9,0)
   DEFINE v_id_dh_ws              DECIMAL(9,0)
   DEFINE v_n_referencia_prc      DECIMAL(9,0)
   DEFINE v_seq_id_cre_uso        DECIMAL(9,0)
   DEFINE v_periodo_pago          CHAR(6)
   DEFINE v_fecha                 CHAR(8)
   DEFINE v_sts_marcaje           SMALLINT
   DEFINE v_importe97             DECIMAL(12,2)
   DEFINE v_tab_movimiento        CHAR(30)
   DEFINE v_f_movimiento          DATE
   DEFINE v_precio_fondo          DECIMAL(19,14)
   DEFINE v_folio_liq_ug          DECIMAL(9,0)
   DEFINE r_recu_acciones         RECORD
      subcuenta      SMALLINT,
      monto_aivs     DECIMAL(16,6) 
   END RECORD
   DEFINE v_aux_aivs92           DECIMAL(16,6)
   DEFINE v_aux_aivs97           DECIMAL(16,6)

   LET v_fecha = TODAY USING "yyyymmdd"
   LET v_periodo_pago = v_fecha[1,6]

   LET v_f_movimiento = MDY(MONTH(TODAY),1,YEAR(TODAY))
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH;

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_movimiento
      AND fondo = 11;

   DISPLAY ""
   DISPLAY "Fecha de evaluación: ",v_f_movimiento
   DISPLAY "Precio fondo: ",v_precio_fondo
   DISPLAY ""
   
   LET v_cadena = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"
   PREPARE prp_reversa_desm FROM v_cadena

   -- Se reversan las desmarcas (221,223,225) en caso de que la nueva originación 223 no fué exitosa
   DECLARE crs_reversa_desm CURSOR FOR
   SELECT id_referencia,
          id_derechohabiente,
          marca_ini,
          folio_marca
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229  -- Generación de marca crédito 43Bis
      AND estado     = 150   -- Marca infonavit rechazada en proceso operativo
      AND f_proceso = TODAY;

   INITIALIZE r_conciliacion.* TO NULL
   
   FOREACH crs_reversa_desm INTO r_conciliacion.id_referencia,
                                 r_conciliacion.id_derechohabiente,
                                 r_conciliacion.marca_orig,
                                 r_conciliacion.folio_marca 

      -- Ejecuta SP que reversa la desmarca.
      EXECUTE prp_reversa_desm USING r_conciliacion.id_derechohabiente,
                                     r_conciliacion.marca_orig,
                                     r_conciliacion.id_referencia,
                                     r_conciliacion.folio_marca

     -- DISPLAY "Reversa desmarca: --> ","Id_referencia: ",r_conciliacion.id_referencia," Marca_origen: ",r_conciliacion.marca_orig 
      
   END FOREACH

    -- Genera identificador archivo 43BIS para los aceptados
   SELECT COUNT(*)
     INTO v_t_43bis
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229
      AND estado IN (18,20)
      AND marca_fin = 223
      AND f_proceso = TODAY;

   -- Genera identificadores de archivos
   IF(v_t_43bis > 0) THEN
      LET v_cadena = "EXECUTE FUNCTION fn_genera_id_archivo(1202)"
      PREPARE prp_archivo_43 FROM v_cadena
      EXECUTE prp_archivo_43 INTO v_id_arh_43,v_folio_arh_43
   END IF

   DISPLAY ""
   DISPLAY "Id_archivo conciliación"
   DISPLAY "43BIS: ",v_id_arh_43
   DISPLAY ""

   LET v_cadena = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,0,0,'','',?,?)"
   PREPARE prp_marca_cuenta FROM v_cadena

   LET v_cadena = "EXECUTE FUNCTION fn_tab_movimiento(0,?,?)"
   PREPARE prp_tab_movimiento FROM v_cadena
   
   INITIALIZE r_conciliacion.* TO NULL

   -- Genera la petición de saldo de las originaciones que fueron exitosas
   -- sólo se recuperan los registros máximos en caso de existir más de 1 registro por derechohabiente
   DECLARE crs_peticion_saldo CURSOR FOR
   SELECT id_derechohabiente,
          MAX(id_referencia)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229   -- Generación de marca crédito 43Bis
      AND estado IN (18,20)
      AND marca_fin = 223
      AND f_proceso = TODAY
      GROUP BY 1;

   FOREACH crs_peticion_saldo INTO r_conciliacion.id_derechohabiente,
                                   r_conciliacion.id_referencia

      DISPLAY "id_dh: ",r_conciliacion.id_derechohabiente, " Referencia: ",r_conciliacion.id_referencia
      
      -- Recupera inf. unica por registro
      SELECT FIRST 1 
             marca_ini,
             tpo_originar,
             marca_fin
        INTO r_conciliacion.marca_orig,
             r_conciliacion.tpo_originar,
             r_conciliacion.marca_fin
        FROM cre_marca_conciliacion
       WHERE id_referencia =  r_conciliacion.id_referencia
         AND id_proceso = 1229
         AND estado IN (18,20)
         AND marca_fin = 223
         AND f_proceso = TODAY
       ORDER BY f_proceso DESC;

      INITIALIZE r_inf_credito.* TO NULL 
      LET v_folio_archivo    = 0
      LET v_id_dh_ws         = NULL
      LET v_n_referencia_prc = NULL
      LET v_importe97        = 0
      LET v_folio_liq_ug     = 0
      LET r_recu_acciones.subcuenta  = NULL
      LET r_recu_acciones.monto_aivs = 0
      LET v_aux_aivs92       = 0
      LET v_aux_aivs97       = 0
     
      
      CASE 
         --~~~~~~ Cuando se desmarcó una 221 ~~~~~~~~
         WHEN (r_conciliacion.marca_orig = 221)

           -- Recupera inf. del crédito anterior
           SELECT cre.folio_liquida,
                  cre.tpo_credito,
                  cre.num_credito,
                  cre.sdo_deudor,
                  cre.f_otorga,
                  ctr.folio_archivo
             INTO r_inf_credito.folio_liquida,
                  r_inf_credito.tpo_credito,
                  r_inf_credito.num_credito,
                  r_inf_credito.sdo_deudor,
                  r_inf_credito.f_otorga,
                  v_folio_archivo
             FROM cre_acreditado cre,
                  cre_ctr_archivo ctr
            WHERE cre.id_cre_acreditado  = r_conciliacion.id_referencia
              AND cre.id_cre_ctr_archivo = ctr.id_cre_ctr_archivo;

           IF(r_inf_credito.tpo_credito IS NULL) THEN
              LET  r_inf_credito.tpo_credito =  1
           END IF

           IF(v_folio_archivo IS NULL) THEN
              LET v_folio_archivo = 0 
           END IF

           -- Se solicita la desmarca a Procesar de la marca anterior 221,
           -- para ellos se verifica que no se haya realizado antes
           SELECT MAX(id_derechohabiente)
             INTO v_id_dh_ws
             FROM cta_marca_ws
            WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
              AND id_origen   = r_conciliacion.id_referencia
              AND modulo_cod  = "03"
              AND tpo_credito = r_inf_credito.tpo_credito
              AND marca       = 221;

         -- Realiza desmarca a Procesar mediante el ws
         IF(v_id_dh_ws IS NULL) THEN
            INSERT INTO cta_marca_ws(
                        id_derechohabiente,
                        id_origen         ,
                        modulo_cod        ,
                        tpo_credito       ,
                        marca             ,
                        f_solicita        ,
                        intento           ,
                        cod_result_op     ,
                        diagnostico       ,
                        situacion         ,
                        num_credito       ,
                        f_infonavit       ,
                        marca_procesar    ,
                        folio_archivo     ,
                        usuario)
                 VALUES(r_conciliacion.id_derechohabiente,
                        r_conciliacion.id_referencia,
                        "03",
                        r_inf_credito.tpo_credito,
                        221,
                        TODAY,
                        1,
                        NULL,
                        " ",
                        0,
                        r_inf_credito.num_credito,
                        r_inf_credito.f_otorga,
                        "01",
                        v_folio_archivo,
                        p_usuario);
                                         
         END IF

         -- Verifica si tiene la marca activa procesar 231 que corresponde a los créditos
         -- con tipo de originación = 1
         SELECT MAX(n_referencia)
           INTO v_n_referencia_prc
           FROM sfr_marca_activa
          WHERE n_referencia = r_conciliacion.id_derechohabiente
            AND marca = 231;
           
         IF(v_n_referencia_prc IS NOT NULL) THEN
            -- Verifica que no se haya realizado la petición de desmarca
            SELECT MAX(id_derechohabiente)
             INTO v_id_dh_ws
             FROM cta_marca_ws
            WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
              AND id_origen   = v_n_referencia_prc
              AND modulo_cod  = "03"
              AND tpo_credito = r_inf_credito.tpo_credito
              AND marca = 231;

            IF(v_id_dh_ws IS NULL) THEN
               INSERT INTO cta_marca_ws(
                        id_derechohabiente,
                        id_origen         ,
                        modulo_cod        ,
                        tpo_credito       ,
                        marca             ,
                        f_solicita        ,
                        intento           ,
                        cod_result_op     ,
                        diagnostico       ,
                        situacion         ,
                        num_credito       ,
                        f_infonavit       ,
                        marca_procesar    ,
                        folio_archivo     ,
                        usuario)
                 VALUES(r_conciliacion.id_derechohabiente,
                        v_n_referencia_prc,
                        "03",
                        r_inf_credito.tpo_credito,
                        231,
                        TODAY,
                        1,
                        NULL,
                        " ",
                        0,
                        r_inf_credito.num_credito,
                        r_inf_credito.f_otorga,
                        "01",
                        v_folio_archivo,
                        p_usuario); 
            END IF   
         END IF

         # Recupera aivs si tiene folio de liquidación
         IF(r_inf_credito.folio_liquida > 0) THEN 

            -- Verifica tabla de movimiento deonde se hizo el cargo
            EXECUTE prp_tab_movimiento USING r_inf_credito.folio_liquida,
                                             " "
                                       INTO v_tab_movimiento

            LET v_cadena = "SELECT subcuenta,SUM(monto_acciones) 
                             FROM ",v_tab_movimiento,"
                            WHERE folio_liquida      = ",r_inf_credito.folio_liquida,
                            " AND id_derechohabiente = ",r_conciliacion.id_derechohabiente,
                            " GROUP BY 1;"
                           
            PREPARE prp_monto_221 FROM v_cadena
            DECLARE crs_monto_221 CURSOR FOR prp_monto_221

            -- Inicializa aivs 
            LET v_aux_aivs92       = 0
            LET v_aux_aivs97       = 0
      
            FOREACH crs_monto_221 INTO r_recu_acciones.subcuenta,
                                       r_recu_acciones.monto_aivs

               IF(r_recu_acciones.subcuenta = 4) THEN
                  LET v_aux_aivs97 = v_aux_aivs97 +  r_recu_acciones.monto_aivs
               ELSE 
                  IF(r_recu_acciones.subcuenta = 8) THEN
                     LET v_aux_aivs92 = v_aux_aivs92 + r_recu_acciones.monto_aivs
                  END IF 
               END IF 
            END FOREACH 

            -- Actualiza tabla de conciliación de marcas
            UPDATE cre_marca_conciliacion
               SET aivs92 = v_aux_aivs92,
                   aivs97 = v_aux_aivs97
             WHERE id_referencia      = r_conciliacion.id_referencia
               AND id_derechohabiente = r_conciliacion.id_derechohabiente
               AND f_proceso = TODAY;

            -- Obtiene monto pesos
            LET r_inf_credito.sdo_deudor = ((v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo)

         END IF 
        
         -- Realiza peticón de la nueva marca
         SELECT seq_cre_uso.NEXTVAL
           INTO v_seq_id_cre_uso
           FROM systables
          WHERE tabid = 1;

         INSERT INTO cre_uso_garantia(id_cre_uso_garantia,
                                      id_cre_ctr_archivo ,
                                      folio_liquida      ,
                                      id_derechohabiente ,
                                      tpo_transferencia  ,
                                      tpo_uso            ,
                                      num_credito        ,
                                      f_presentacion     ,
                                      f_movimiento       ,
                                      periodo_pago       ,
                                      importe_v97        ,
                                      nss_afore          ,
                                      rfc_afore          ,
                                      paterno_afore      ,
                                      materno_afore      ,
                                      nombre_afore       ,
                                      nom_imss           ,
                                      edo_procesar       ,
                                      diagnostico        ,
                                      estado             ,
                                      f_proceso)
                              VALUES (v_seq_id_cre_uso,
                                      v_id_arh_43,
                                      0,
                                      r_conciliacion.id_derechohabiente,
                                      "18",
                                      3,
                                      0,
                                      TODAY,
                                      TODAY,
                                      v_periodo_pago,
                                      r_inf_credito.sdo_deudor,
                                      '00000000000',
                                      '',
                                      '',
                                      '',
                                      '',
                                      '',
                                      70,
                                      '',
                                      142,
                                      TODAY);

         -- Marca la cuenta como 223
         EXECUTE prp_marca_cuenta USING r_conciliacion.id_derechohabiente,
                                        r_conciliacion.marca_fin,
                                        v_seq_id_cre_uso,
                                        p_folio,
                                        p_usuario,
                                        p_proceso_cod
                                   INTO v_sts_marcaje

         -- Actualiza la solicitud anterior 221 si el crédito está vigente
         UPDATE cre_acreditado
            SET estado = 285 --> Crédito liquidado por nueva originación
          WHERE id_cre_acreditado  = r_conciliacion.id_referencia
            AND id_derechohabiente = r_conciliacion.id_derechohabiente
            AND estado IN (SELECT estado
                             FROM cat_maq_credito
                            WHERE entidad = 1);

         --~~~~~~ Cuando se desmarcó una 225 ~~~~~~~~
         WHEN (r_conciliacion.marca_orig = 225)

            -- Recupera información del crédito anterior para la solcitud 225
            SELECT FIRST 1
                   cre.tpo_credito,
                   cre.num_credito,
                   cre.sdo_deudor,
                   cre.f_otorga,
                   ctr.folio_archivo
              INTO r_inf_credito.tpo_credito,
                   r_inf_credito.num_credito,
                   r_inf_credito.sdo_deudor,
                   r_inf_credito.f_otorga,
                   v_folio_archivo
              FROM cre_acreditado cre,
                   cre_ctr_archivo ctr
             WHERE cre.id_derechohabiente = r_conciliacion.id_derechohabiente
               AND cre.id_cre_ctr_archivo = ctr.id_cre_ctr_archivo
               AND cre.tpo_originacion    = 4
               ORDER BY f_otorga DESC;

           IF(r_inf_credito.tpo_credito IS NULL) THEN
              LET  r_inf_credito.tpo_credito =  1
           END IF

           IF(v_folio_archivo IS NULL) THEN
              LET v_folio_archivo = 0 
           END IF

           -- Se solicita la desmarca via ws de la marca anterior 225
           SELECT MAX(id_derechohabiente)
             INTO v_id_dh_ws
             FROM cta_marca_ws
            WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
              AND id_origen   = r_conciliacion.id_referencia
              AND modulo_cod  = "04"
              AND tpo_credito = r_inf_credito.tpo_credito
              AND marca       = 225;

         IF(v_id_dh_ws IS NULL) THEN
            INSERT INTO cta_marca_ws(
                        id_derechohabiente,
                        id_origen         ,
                        modulo_cod        ,
                        tpo_credito       ,
                        marca             ,
                        f_solicita        ,
                        intento           ,
                        cod_result_op     ,
                        diagnostico       ,
                        situacion         ,
                        num_credito       ,
                        f_infonavit       ,
                        marca_procesar    ,
                        folio_archivo     ,
                        usuario)
                 VALUES(r_conciliacion.id_derechohabiente,
                        r_conciliacion.id_referencia,
                        "04",
                        r_inf_credito.tpo_credito,
                        225,
                        TODAY,
                        1,
                        NULL,
                        " ",
                        0,
                        r_inf_credito.num_credito,
                        r_inf_credito.f_otorga,
                        "04",
                        v_folio_archivo,
                        p_usuario);
         END IF

         -- Verifica si tiene la marca activa procesar 234 que corresponde a los créditos
         -- con tipo de originación = 4
         SELECT MAX(n_referencia)
           INTO v_n_referencia_prc
           FROM sfr_marca_activa
          WHERE n_referencia = r_conciliacion.id_derechohabiente
            AND marca = 234;

         IF(v_n_referencia_prc IS NOT NULL) THEN
            -- Verifica que no se haya realizado la petición de desmarca
            SELECT MAX(id_derechohabiente)
             INTO v_id_dh_ws
             FROM cta_marca_ws
            WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
              AND id_origen   = v_n_referencia_prc
              AND modulo_cod  = "04"
              AND tpo_credito = r_inf_credito.tpo_credito
              AND marca = 234;

            IF(v_id_dh_ws IS NULL) THEN
               INSERT INTO cta_marca_ws(
                        id_derechohabiente,
                        id_origen         ,
                        modulo_cod        ,
                        tpo_credito       ,
                        marca             ,
                        f_solicita        ,
                        intento           ,
                        cod_result_op     ,
                        diagnostico       ,
                        situacion         ,
                        num_credito       ,
                        f_infonavit       ,
                        marca_procesar    ,
                        folio_archivo     ,
                        usuario)
                 VALUES(r_conciliacion.id_derechohabiente,
                        v_n_referencia_prc,
                        "04",
                        r_inf_credito.tpo_credito,
                        234,
                        TODAY,
                        1,
                        NULL,
                        " ",
                        0,
                        r_inf_credito.num_credito,
                        r_inf_credito.f_otorga,
                        "04",
                        v_folio_archivo,
                        p_usuario); 
            END IF   
         END IF

         -- Recupera el monto de la solicitud anterior
         SELECT folio_liquida,
                importe_v97
           INTO v_folio_liq_ug,
                v_importe97
           FROM cre_uso_garantia
          WHERE id_cre_uso_garantia = r_conciliacion.id_referencia;

         # Recupera aivs si tiene folio de liquidación
         IF(v_folio_liq_ug > 0) THEN 

            -- Verifica tabla de movimiento dende se hizo el cargo de ese folio de liquidación
            EXECUTE prp_tab_movimiento USING v_folio_liq_ug,
                                             " "
                                       INTO v_tab_movimiento

            LET v_cadena = "SELECT subcuenta,SUM(monto_acciones) 
                             FROM ",v_tab_movimiento,"
                            WHERE folio_liquida      = ",v_folio_liq_ug,
                            " AND id_derechohabiente = ",r_conciliacion.id_derechohabiente,
                            " GROUP BY 1;"
                           
            PREPARE prp_monto_225 FROM v_cadena
            DECLARE crs_monto_225 CURSOR FOR prp_monto_225

            LET v_aux_aivs92       = 0
            LET v_aux_aivs97       = 0
      
            FOREACH crs_monto_225 INTO r_recu_acciones.subcuenta,
                                       r_recu_acciones.monto_aivs

               IF(r_recu_acciones.subcuenta = 4) THEN
                  LET v_aux_aivs97 = v_aux_aivs97 +  r_recu_acciones.monto_aivs
               ELSE 
                  IF(r_recu_acciones.subcuenta = 8) THEN
                     LET v_aux_aivs92 = v_aux_aivs92 + r_recu_acciones.monto_aivs
                  END IF 
               END IF 
            END FOREACH 

            -- Actualiza tabla de conciliación de marcas
            UPDATE cre_marca_conciliacion
               SET aivs92 = v_aux_aivs92,
                   aivs97 = v_aux_aivs97
             WHERE id_referencia      = r_conciliacion.id_referencia
               AND id_derechohabiente = r_conciliacion.id_derechohabiente
               AND f_proceso = TODAY;

            -- Obtiene monto pesos
            LET v_importe97 = ((v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo)

         END IF 
         
         -- Realiza petición 223
         SELECT seq_cre_uso.NEXTVAL
           INTO v_seq_id_cre_uso
           FROM systables
          WHERE tabid = 1;

         INSERT INTO cre_uso_garantia(id_cre_uso_garantia,
                                      id_cre_ctr_archivo ,
                                      folio_liquida      ,
                                      id_derechohabiente ,
                                      tpo_transferencia  ,
                                      tpo_uso            ,
                                      num_credito        ,
                                      f_presentacion     ,
                                      f_movimiento       ,
                                      periodo_pago       ,
                                      importe_v97        ,
                                      nss_afore          ,
                                      rfc_afore          ,
                                      paterno_afore      ,
                                      materno_afore      ,
                                      nombre_afore       ,
                                      nom_imss           ,
                                      edo_procesar       ,
                                      diagnostico        ,
                                      estado             ,
                                      f_proceso)
                              VALUES (v_seq_id_cre_uso,
                                      v_id_arh_43,
                                      0,
                                      r_conciliacion.id_derechohabiente,
                                      "18",
                                      3,
                                      0,
                                      TODAY,
                                      TODAY,
                                      v_periodo_pago,
                                      v_importe97,
                                      '00000000000',
                                      '',
                                      '',
                                      '',
                                      '',
                                      '',
                                      70,
                                      '',
                                      142,
                                      TODAY);

         -- Marca la cuenta como 223
         EXECUTE prp_marca_cuenta USING r_conciliacion.id_derechohabiente,
                                        r_conciliacion.marca_fin,
                                        v_seq_id_cre_uso,
                                        p_folio,
                                        p_usuario,
                                        p_proceso_cod
                                   INTO v_sts_marcaje

         -- Actualiza la solicitud anterior 225 si el crédito está vigente
         UPDATE cre_uso_garantia
            SET estado = 285 --> Crédito liquidado por nueva originación
          WHERE id_cre_uso_garantia  = r_conciliacion.id_referencia
            AND id_derechohabiente   = r_conciliacion.id_derechohabiente
            AND estado IN (SELECT estado
                             FROM cat_maq_credito
                            WHERE entidad = 1);

          --~~~~~~ Cuando se desmarcó una 223 ~~~~~~~~
         WHEN (r_conciliacion.marca_orig = 223)

            -- Recupera el monto de la solicitud anterior
            SELECT folio_liquida,
                   importe_v97
              INTO v_folio_liq_ug,
                   v_importe97
              FROM cre_uso_garantia
             WHERE id_cre_uso_garantia = r_conciliacion.id_referencia;

            # Recupera aivs si tiene folio de liquidación
            IF(v_folio_liq_ug > 0) THEN 

               -- Verifica tabla de movimiento dende se hizo el cargo de ese folio de liquidación
               EXECUTE prp_tab_movimiento USING v_folio_liq_ug,
                                                " "
                                          INTO v_tab_movimiento

               LET v_cadena = "SELECT subcuenta,SUM(monto_acciones) 
                                 FROM ",v_tab_movimiento,"
                                WHERE folio_liquida      = ",v_folio_liq_ug,
                                " AND id_derechohabiente = ",r_conciliacion.id_derechohabiente,
                                " GROUP BY 1;"
                           
            PREPARE prp_monto_223 FROM v_cadena
            DECLARE crs_monto_223 CURSOR FOR prp_monto_223

            LET v_aux_aivs92       = 0
            LET v_aux_aivs97       = 0
      
            FOREACH crs_monto_223 INTO r_recu_acciones.subcuenta,
                                       r_recu_acciones.monto_aivs

               IF(r_recu_acciones.subcuenta = 4) THEN
                  LET v_aux_aivs97 = v_aux_aivs97 +  r_recu_acciones.monto_aivs
               ELSE 
                  IF(r_recu_acciones.subcuenta = 8) THEN
                     LET v_aux_aivs92 = v_aux_aivs92 + r_recu_acciones.monto_aivs
                  END IF 
               END IF 
            END FOREACH 

            -- Actualiza tabla de conciliación de marcas
            UPDATE cre_marca_conciliacion
               SET aivs92 = v_aux_aivs92,
                   aivs97 = v_aux_aivs97
             WHERE id_referencia      = r_conciliacion.id_referencia
               AND id_derechohabiente = r_conciliacion.id_derechohabiente
               AND f_proceso = TODAY;

            -- Revalúa las acciones al precio de acción al día
            LET v_importe97 = ((v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo)

            END IF 
            
            -- Realiza petición 223
            SELECT seq_cre_uso.NEXTVAL
              INTO v_seq_id_cre_uso
              FROM systables
             WHERE tabid = 1;

            INSERT INTO cre_uso_garantia(id_cre_uso_garantia,
                                         id_cre_ctr_archivo ,
                                         folio_liquida      ,
                                         id_derechohabiente ,
                                         tpo_transferencia  ,
                                         tpo_uso            ,
                                         num_credito        ,
                                         f_presentacion     ,
                                         f_movimiento       ,
                                         periodo_pago       ,
                                         importe_v97        ,
                                         nss_afore          ,
                                         rfc_afore          ,
                                         paterno_afore      ,
                                         materno_afore      ,
                                         nombre_afore       ,
                                         nom_imss           ,
                                         edo_procesar       ,
                                         diagnostico        ,
                                         estado             ,
                                         f_proceso)
                                 VALUES (v_seq_id_cre_uso,
                                         v_id_arh_43,
                                         0,
                                         r_conciliacion.id_derechohabiente,
                                         "18",
                                         3,
                                         0,
                                         TODAY,
                                         TODAY,
                                         v_periodo_pago,
                                         v_importe97,
                                         '00000000000',
                                         '',
                                         '',
                                         '',
                                         '',
                                         '',
                                         70,
                                         '',
                                         142,
                                         TODAY);

            -- Marca la cuenta como 223
            EXECUTE prp_marca_cuenta USING r_conciliacion.id_derechohabiente,
                                           r_conciliacion.marca_fin,
                                           v_seq_id_cre_uso,
                                           p_folio,
                                           p_usuario,
                                           p_proceso_cod
                                      INTO v_sts_marcaje

            -- Actualiza la solicitud anterior 223 si el crédito está vigente
            UPDATE cre_uso_garantia
               SET estado = 285 --> Crédito liquidado por nueva originación
             WHERE id_cre_uso_garantia  = r_conciliacion.id_referencia
               AND id_derechohabiente   = r_conciliacion.id_derechohabiente
               AND estado IN (SELECT estado
                                FROM cat_maq_credito
                               WHERE entidad = 1);
                            
      END CASE 
   END FOREACH 
    
END FUNCTION 
