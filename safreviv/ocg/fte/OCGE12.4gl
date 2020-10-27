###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE10                                                  #
#Objetivo          => Programa para ejecutar el subproceso 3, sin necesidad   #
#                     de ingresar un archivo.                                 #
#Autor             => Hector Jímenez Lara                                     #
#Fecha inicio      => 03 Octubre 2015                                         #
#Modificación      => José Eduardo ventura 16/11/2016                         #
#Modificación      => Emilio Abarca Sánchez 16/Agosto/2017                    #
###############################################################################
DATABASE safre_viv

GLOBALS "OCGW03.inc"

   DEFINE v_s_qry        STRING
   DEFINE v_tpo_envio    CHAR(1)
   DEFINE v_f_envio      DATE 
   DEFINE v_ax_fecha     CHAR(8)
   DEFINE v_sp_error     SMALLINT
   -- Variables estáticas
   DEFINE v_tpo_registro SMALLINT
   DEFINE v_subproceso   CHAR(3)
   DEFINE v_s_msj        STRING
   DEFINE v_s_cmd        STRING 
   DEFINE p_v_usuario    LIKE seg_usuario.usuario

   --- variables para consulta de relación laboral en WS de TRM
   DEFINE v_nss             CHAR(11)
   DEFINE v_ws_status       SMALLINT -- estatus de ejecución del ws
   DEFINE v_indice          INTEGER
   DEFINE v_fecha_f         DATE
   DEFINE v_fecha_c         STRING
   DEFINE v_nrp             VARCHAR(100)
   DEFINE v_f_alta          DATE
   DEFINE v_f_alta_format   STRING
   DEFINE v_delegacion      DATE
   DEFINE v_nom_delegacion  VARCHAR(100)
   DEFINE v_r_nss           CHAR(11)
   DEFINE v_idx             SMALLINT
   DEFINE v_ruta            VARCHAR(150)
   DEFINE v_usuario         CHAR(40)
   DEFINE v_password        CHAR(20)
   DEFINE v_intentos        SMALLINT

   DEFINE v_ent_ef                CHAR(50)
   DEFINE v_num_ctr_ef            CHAR(18)
   DEFINE v_imp_solic_ug          CHAR(15)
   DEFINE v_f_venc_imp            DATE
   DEFINE v_tpo_credito           CHAR(1)
   DEFINE v_sol_sdo               CHAR(1)
   DEFINE v_paterno               CHAR(40)
   DEFINE v_materno               CHAR(40)
   DEFINE v_nombre                CHAR(40)
   DEFINE v_nom                   STRING
   DEFINE v_sdo_viv97             DECIMAL(15,2)
   DEFINE v_cuenta                INTEGER
   DEFINE v_cve_ef                CHAR(3)

   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_resultado_ws   SMALLINT
   DEFINE v_id_arch        DECIMAL(9,0)

MAIN
   DEFINE p_tipo_ejecucion    SMALLINT                      -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING                        -- título de la ventana

   -- se recupera la clave de usuario desde parametro
   LET p_v_usuario      = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   DISPLAY "usuario   : ", p_v_usuario
   DISPLAY "tipo ejec : ", p_tipo_ejecucion
   DISPLAY "titulo    : ",p_s_titulo

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".OCGE12.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 

   LET v_tpo_envio = "1"
   LET v_f_envio   = TODAY
   LET v_ax_fecha  = v_f_envio USING "mmddyyyy"

    CALL fn_realiza_proceso()

END MAIN

FUNCTION fn_realiza_proceso()

   DEFINE v_precio_fondo   DECIMAL(15,2)
   DEFINE v_sdo_pesos      DECIMAL(15,2)
   DEFINE v_id_dh          DECIMAL(9,0)
   DEFINE a                SMALLINT
   DEFINE cb               ui.ComboBox
   DEFINE v_sdo_pendiente  DECIMAL(15,2)
   DEFINE v_aux_pdo_pago   CHAR(8)
   DEFINE v_periodo_pago   CHAR(6)
   DEFINE v_bnd_solic      SMALLINT 

   DEFINE r_busca_periodo RECORD
      periodo_his CHAR(6),
      situacion   SMALLINT  
   END RECORD 
   

   DEFINE arr_sol_sdo DYNAMIC ARRAY OF RECORD
      tipo_sol        SMALLINT,
      solicitud_desc  CHAR(30)
   END RECORD

   LET v_tpo_registro = 2
   LET v_subproceso   = "003"
   LET v_id_arch      = 0 
  

   LET v_s_qry = " SELECT ruta_bin
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_bin FROM v_s_qry
   EXECUTE prp_r_bin INTO v_ruta_bin

    

   OPEN WINDOW w_proc WITH FORM "OCGE121"
   
      DISPLAY v_f_envio   TO f_envio
      DISPLAY v_tpo_envio TO tpo_envio

      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED)

         ON ACTION ACCEPT
         
         IF v_nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            CONTINUE  INPUT
         ELSE
       
            SELECT COUNT(*)
              INTO v_cuenta
              FROM ocg_formalizacion f,
                   ocg_detalle d,
                   cat_entidad_financiera c
             WHERE f.id_ocg_detalle     = d.id_ocg_detalle
               AND f.id_derechohabiente = d.id_derechohabiente
               AND f.cve_ent_financiera = c.cve_ent_financiera
               AND d.subproceso         = 2
               AND f.situacion IN (55,60,70,80)
               AND d.nss = v_nss

            IF v_cuenta >= 1 THEN

               LET v_s_qry = "SELECT f.ap_paterno,
                                     f.ap_materno,
                                     f.nombre,
                                     c.ent_financiera_desc,
                                     f.cve_ent_financiera,
                                     f.num_ctr_int_ef,
                                     f.tpo_credito,
                                     f.id_derechohabiente
                                FROM ocg_formalizacion f,
                                     ocg_detalle d,
                                     cat_entidad_financiera c
                               WHERE f.id_ocg_detalle     = d.id_ocg_detalle
                                 AND f.id_derechohabiente = d.id_derechohabiente
                                 AND f.cve_ent_financiera = c.cve_ent_financiera
                                 AND d.subproceso         = 2
                                 AND f.situacion IN (55,60,70,80)
                                 AND d.nss = ? "

               PREPARE prp_credito FROM v_s_qry
               EXECUTE prp_credito USING v_nss
                                    INTO v_paterno,
                                         v_materno,
                                         v_nombre,
                                         v_ent_ef,
                                         v_cve_ef,
                                         v_num_ctr_ef,
                                         v_tpo_credito,
                                         v_id_dh

               LET v_nom = v_paterno CLIPPED, ' ',v_materno CLIPPED,' ',v_nombre CLIPPED

               SELECT precio_fondo
                 INTO v_precio_fondo
                 FROM glo_valor_fondo
                WHERE f_valuacion = TODAY
                  AND fondo = 11;

               SELECT ROUND((sum(monto_acciones*v_precio_fondo)),2)
                 INTO v_sdo_pesos
                 FROM cta_movimiento
                WHERE id_derechohabiente = v_id_dh
                  AND subcuenta = 4
                  AND fondo_inversion = 11;

               LET v_sdo_pendiente = 0

               select sum (importe_solicitado)
                     into v_sdo_pendiente
                     from ocg_solicitud_uso_garantia
                    where id_derechohabiente = v_id_dh
                      and diagnostico = 1
                      and situacion in (50,90);

               IF v_sdo_pendiente IS NULL THEN
                  LET v_sdo_pendiente = 0
               END IF

               LET v_sdo_pesos = v_sdo_pesos - v_sdo_pendiente
               
               IF v_sdo_pesos < 1 THEN
                  CALL fn_mensaje("Atención","NSS ingresado tiene saldo de vivienda agotado", "stop")
               ELSE

               LET v_sdo_viv97 = v_sdo_pesos;

               DISPLAY "saldos :",v_sdo_pesos, "|", v_sdo_viv97

               DISPLAY BY NAME v_nom
               DISPLAY BY NAME v_ent_ef
               DISPLAY BY NAME v_num_ctr_ef
               DISPLAY BY NAME v_tpo_credito
               DISPLAY BY NAME v_sdo_viv97

               INPUT BY NAME v_imp_solic_ug,
                             v_f_venc_imp,
                             v_sol_sdo ATTRIBUTES(UNBUFFERED)

               BEFORE INPUT

                  # Tipos de solicitudes
                  -- Solicitud de aportación = 3
                  -- Solicitud Uso de Garantía = 2
                  
                  LET cb = ui.ComboBox.forName("v_sol_sdo")
                  
                  LET arr_sol_sdo[1].tipo_sol       = 3
                  LET arr_sol_sdo[1].solicitud_desc = "Solicitud Aportación"
                  LET arr_sol_sdo[2].tipo_sol       = 2
                  LET arr_sol_sdo[2].solicitud_desc = "Solicitud Uso Garantía"
                  
                  FOR a = 1 TO arr_sol_sdo.getLength()
                     CALL cb.addItem(arr_sol_sdo[a].tipo_sol,arr_sol_sdo[a].solicitud_desc)
                  END FOR
           
               ON ACTION ACCEPT
                  IF(v_imp_solic_ug IS NULL) THEN
                     CALL fn_mensaje("Atención","Debe ingresar el importe solicitado", "stop")
                     NEXT FIELD v_imp_solic_ug
                  END IF 

                  IF(v_f_venc_imp IS NULL) THEN
                     CALL fn_mensaje("Atención","Debe ingresar la fecha de vencimiento", "stop")
                     NEXT FIELD v_f_venc_imp
                  END IF

                  -- Busca que ese periodo de pago no se encuentre en gestión o pagado

                  LET v_bnd_solic    = 0 -- Se apaga bandera como no solicitado
                  
                  LET v_aux_pdo_pago = v_f_venc_imp USING "yyyymmdd"
                  LET v_periodo_pago = v_aux_pdo_pago[1,4]||v_aux_pdo_pago[5,6]

                  INITIALIZE r_busca_periodo.* TO NULL 
                 
                  LET v_s_qry = "SELECT YEAR(f_vencimiento)||LPAD(MONTH(f_vencimiento),2,0),
                                        situacion
                                   FROM ocg_solicitud_uso_garantia
                                  WHERE id_derechohabiente = ",v_id_dh

                  PREPARE prp_busca_periodo FROM v_s_qry
                  DECLARE crs_busca_periodo CURSOR FOR prp_busca_periodo

                  FOREACH crs_busca_periodo INTO r_busca_periodo.periodo_his,
                                                  r_busca_periodo.situacion
                                                  
                     IF (r_busca_periodo.periodo_his = v_periodo_pago) THEN 

                        -- 50 (Aceptado)
                        -- 90 (Enviado)
                        -- 100 (Aplicado)
                        -- 110 (Pagado)
                  
                        IF(r_busca_periodo.situacion = 110) OR 
                          (r_busca_periodo.situacion = 50) OR 
                          (r_busca_periodo.situacion = 90) OR
                          (r_busca_periodo.situacion = 100) THEN

                          LET v_bnd_solic = 1 -- Prende bandera como periodo ya solicitado

                          EXIT FOREACH
                          
                        END IF
                        
                     END IF 
                  END FOREACH 

                  IF(v_bnd_solic = 1) THEN
                       CALL fn_mensaje("Atención","El periodo de pago ingresado ya se encuentra en gestión o pagado", "stop")
                       NEXT FIELD v_f_venc_imp
                  ELSE 
                     -- Se realiza la solicitud
                     LET v_imp_solic_ug = v_imp_solic_ug * 100
                     CALL fn_consulta_ws()
                  END IF 
                  
                  EXIT INPUT

               ON ACTION CANCEL
                  CALL cb.clear()
                  EXIT INPUT
                  
               END INPUT

               END IF
            ELSE
               CALL fn_mensaje("","NSS ingresado no existe en Otorgamiento de Créditos 43bis","")
            END IF
         END IF

         ON ACTION CANCEL
            EXIT INPUT
            
         END INPUT
      CLOSE WINDOW w_proc

END FUNCTION


FUNCTION fn_consulta_ws()

   DEFINE v_tot_aceptados  INTEGER 
   DEFINE v_tot_rechazados INTEGER
   DEFINE v_f_venc_format  CHAR(8)
   DEFINE v_f_envio_forma  CHAR(8)

   LET v_f_envio_forma = v_f_envio USING "yyyymmdd"
   LET v_f_venc_format = v_f_venc_imp USING "yyyymmdd"

   -- Crea tmp para las solicitudes en línea del SP3     
   CALL crea_tmp_sp3_especial()

   LET v_s_qry = "INSERT INTO safre_tmp:tmp_sol_sp3(
                                           tpo_registro      ,
                                           subproceso        ,
                                           tpo_envio         ,
                                           f_envio           ,
                                           cve_ent_financiera,
                                           nss               ,
                                           num_ctrl_ef       ,
                                           imp_solic_uti_ocg ,
                                           f_venc_imp_solic  ,
                                           cred_convenidos   ,
                                           solic_saldo)
                                   VALUES (?,?,?,?,?,?,?,?,?,?,?)"

   PREPARE prp_ins_tmp_sp3 FROM v_s_qry
   EXECUTE prp_ins_tmp_sp3 USING v_tpo_registro  ,
                                 v_subproceso    ,
                                 v_tpo_envio     ,
                                 v_f_envio_forma ,
                                 v_cve_ef        ,
                                 v_nss           ,
                                 v_num_ctr_ef    ,
                                 v_imp_solic_ug  ,
                                 v_f_venc_format ,
                                 v_tpo_credito   ,
                                 v_sol_sdo

   --CALL fn_borra_arch()
         
   --se ejecuta función para consulta de relación laboral en WS de TRM
   CALL fn_crea_temporal()
   

   ---cve_cliente = ocg_1 ruta para consulta de relación laboral en TRM
   LET v_s_qry = "SELECT ruta_servidor, 
                         usuario, 
                         password, 
                         num_reintento 
                    FROM wsv_cliente 
                   WHERE cve_cliente = 'ocg_1' "

   PREPARE prp_crl FROM v_s_qry
   EXECUTE prp_crl INTO v_ruta,
                        v_usuario,
                        v_password,
                        v_intentos

   LET v_ruta = v_ruta CLIPPED

   -- Consulta Relación Laobal sólo para tipo de solicitud 2 (Uso Garantía)
   LET v_s_qry = " SELECT nss
                     FROM safre_tmp:tmp_sol_sp3
                    WHERE subproceso  = '003' 
                      AND solic_saldo = '2';"

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta WS : ",v_ruta
{
   FOREACH cur_c_tmp INTO v_nss
      LET ns1consultarTrabajador.nss = v_nss
   
      -- se invoca la consulta
      CALL consultarTrabajador_g(v_ruta) RETURNING v_ws_status
   
      -- si el webservice NO se ejecuto correctamente
      IF ( v_ws_status <> 0 ) THEN      
   
         DISPLAY "ERROR al invocar webservice de consulta de relación laboral"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action
   	  
   	  -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
         CONTINUE FOREACH
      END IF
   
      FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
         --se formatea la fecha
         LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
         LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
         LET v_fecha_f = DATE(v_fecha_c)
   
         --consulta fecha alta
         LET v_f_alta_format = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaAlta
         LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
         LET v_f_alta = DATE(v_f_alta_format)
   
         LET v_r_nss          = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.nss
         LET v_delegacion     = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].delegacion
         LET v_nom_delegacion = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nomDelegacion
         LET v_nrp            = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nrp
   
         LET v_s_qry = "INSERT INTO safre_tmp:tmp_relacion_laboral values(?,?,?);"
   
         PREPARE prp_ins_tmp_003 FROM  v_s_qry
         EXECUTE prp_ins_tmp_003 USING v_r_nss,
                                       v_f_alta,
                                       v_fecha_f

      END FOR

      LET v_idx = v_idx + 1
   END FOREACH
}

   
{ ## Código original de consulta WS Relación Laboral ##

 ---cve_cliente = ocg_1 ruta para consulta de relación laboral en TRM
   LET v_s_qry = "SELECT ruta_servidor, 
                         usuario, 
                         password, 
                         num_reintento 
                    FROM wsv_cliente 
                   WHERE cve_cliente = 'ocg_1' "

   PREPARE prp_crl FROM v_s_qry
   EXECUTE prp_crl INTO v_ruta,
                        v_usuario,
                        v_password,
                        v_intentos

   LET v_ruta = v_ruta CLIPPED

   LET v_s_qry = " SELECT nss
                     FROM safre_tmp:tmp_sol_sp3
                    WHERE subproceso = 3 "

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta WS : ",v_ruta

   FOREACH cur_c_tmp INTO v_nss
      LET ns1consultarTrabajador.nss = v_nss
   
      -- se invoca la consulta
      CALL consultarTrabajador_g(v_ruta) RETURNING v_ws_status
   
      -- si el webservice NO se ejecuto correctamente
      IF ( v_ws_status <> 0 ) THEN      
   
         DISPLAY "ERROR al invocar webservice de consulta de relación laboral"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action
   	  
   	  -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
         CONTINUE FOREACH
      END IF
   
      FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
         --se formatea la fecha
         LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
         LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
         LET v_fecha_f = DATE(v_fecha_c)
   
         --consulta fecha alta
         LET v_f_alta_format = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaAlta
         LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
         LET v_f_alta = DATE(v_f_alta_format)
   
         LET v_r_nss          = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.nss
         LET v_delegacion     = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].delegacion
         LET v_nom_delegacion = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nomDelegacion
         LET v_nrp            = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nrp
   
         LET v_s_qry = "INSERT INTO safre_tmp:tmp_relacion_laboral values(?,?,?);"
   
         PREPARE prp_ins_tmp_003 FROM  v_s_qry
         EXECUTE prp_ins_tmp_003 USING v_r_nss,
                                       v_f_alta,
                                       v_fecha_f

      END FOR

      LET v_idx = v_idx + 1
   END FOREACH
}
         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso SP003"

         DATABASE safre_viv 

         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp3_especial(?)"

         DISPLAY "qry SP: \n",v_s_qry

         PREPARE prp_exe_fn_proceso_ug FROM v_s_qry
         EXECUTE prp_exe_fn_proceso_ug USING v_id_arch
                                        INTO v_sp_error,
                                             v_tot_aceptados,
                                             v_tot_rechazados

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode

         IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN 
            CALL fn_mensaje("Alerta","La solicitud se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

END FUNCTION

FUNCTION fn_borra_arch()

      LET v_s_qry = "INSERT INTO ocg_ctr_archivo VALUES(seq_ocg_archivo.NEXTVAL,0,today,3901,2,'SP003_manual',0,0,0,1,0,0,10,today ,',",v_usuario,"')"
      PREPARE prp_ins_arch FROM v_s_qry
      EXECUTE prp_ins_arch
END FUNCTION 

FUNCTION fn_crea_temporal()
      DATABASE safre_tmp
      LET v_s_qry = "DROP TABLE IF EXISTS tmp_relacion_laboral; \n
                     CREATE TABLE tmp_relacion_laboral(
                        nss     char(11),
                        f_alta  date,
                        f_baja  date
                        );"

      PREPARE prp_Crea_t FROM v_s_qry
      EXECUTE prp_Crea_t

      IF sqlca.sqlcode = 0 THEN
         DISPLAY "Se crea la tabla temporal de relaciones laborales" 
      END IF
      DATABASE safre_viv
END FUNCTION 

FUNCTION crea_tmp_sp3_especial()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_sol_sp3

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_sol_sp3 (
                       tpo_registro       CHAR(1) ,
                       subproceso         CHAR(3) ,
                       tpo_envio          CHAR(1) ,
                       f_envio            CHAR(8) ,
                       cve_ent_financiera CHAR(3) ,
                       nss                CHAR(11),
                       num_ctrl_ef        CHAR(18),
                       imp_solic_uti_ocg  CHAR(15),
                       f_venc_imp_solic   CHAR(8) ,
                       cred_convenidos    CHAR(1) ,
                       solic_saldo        CHAR(1));

DATABASE safre_viv
      

END FUNCTION 

