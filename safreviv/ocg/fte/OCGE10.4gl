###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE10                                                  #
#Objetivo          => Programa para realizar pruebas ingresando los datos     #
#                     oblgatorios para ejecutar los subprocesose 1,2,3,5      #
#                     sin necesidad de ingresar un archivo                    #
#Autor             => Héctor F. Jiménez Lara                                  #
#Fecha inicio      => 08 Enero 2016                                           #
###############################################################################
DATABASE safre_viv

   GLOBALS "OCGW03.inc"

   DEFINE v_s_qry           STRING
   DEFINE v_tpo_envio       CHAR(1)
   DEFINE v_f_envio         DATE 
   DEFINE v_ax_fecha        CHAR(8)
   DEFINE v_sp_error        SMALLINT
   -- Variables estáticas
   DEFINE v_tpo_registro    SMALLINT
   DEFINE v_subproceso      CHAR(3)
   DEFINE v_s_msj           STRING
   DEFINE v_s_cmd           STRING 
   DEFINE p_v_usuario       LIKE seg_usuario.usuario

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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".OCGE10.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 

   LET v_tpo_envio = "E"
   LET v_f_envio   = TODAY
   LET v_ax_fecha  = v_f_envio USING "mmddyyyy"

   --CALL fn_exe_tramite()
   CALL fn_realiza_proceso()

END MAIN

FUNCTION fn_realiza_proceso()
   OPEN WINDOW w_proc WITH FORM "OCGE101"
      DISPLAY v_f_envio   TO f_envio
      DISPLAY v_tpo_envio TO tpo_envio

      MENU
         ON ACTION SP001
            CALL fn_exe_tramite()
         ON ACTION SP002
            CALL fn_exe_formalizacion()
         ON ACTION SP003
            CALL fn_exe_ug()
         ON ACTION SP005
            CALL fn_exe_sp005()
         ON ACTION salir
            EXIT MENU
            CLEAR WINDOW w_proc
      END MENU
   CLOSE WINDOW w_proc
END FUNCTION


FUNCTION fn_exe_tramite()
   DEFINE v_rec_datos RECORD
      nss                CHAR(11),
      cve_ef             CHAR(3),
      rfc                CHAR(13),
      curp               CHAR(18),
      paterno            CHAR(40),
      materno            CHAR(40),
      nombre             CHAR(40),
      cred_convenidos    CHAR(1)
   END RECORD

   LET v_tpo_registro = 2
   LET v_subproceso   = "001"

   DISPLAY v_subproceso TO subproceso

   INPUT BY NAME v_rec_datos.* ATTRIBUTES(UNBUFFERED)
      ON ACTION ACCEPT
         IF v_rec_datos.nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            EXIT INPUT
         END IF

         CALL fn_borra_tmp()

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_ocg43(tpo_registro,
                                                                subproceso,
                                                                nss,
                                                                tpo_envio,
                                                                f_envio,
                                                                cve_ent_financiera,
                                                                rfc,
                                                                curp,
                                                                ap_paterno_af,
                                                                ap_materno_af,
                                                                nombre_af,
                                                                cred_convenidos)
                                                         VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"

         PREPARE prp_ins_tmp FROM v_s_qry
         EXECUTE prp_ins_tmp USING v_tpo_registro,
                                   v_subproceso,
                                   v_rec_datos.nss,
                                   v_tpo_envio,  --v_rec_datos.tpo_envio,
                                   v_ax_fecha, --v_rec_datos.f_envio,
                                   v_rec_datos.cve_ef,
                                   v_rec_datos.rfc,
                                   v_rec_datos.curp,
                                   v_rec_datos.paterno,
                                   v_rec_datos.materno,
                                   v_rec_datos.nombre,
                                   v_rec_datos.cred_convenidos

         DISPLAY "tipo reg :",v_tpo_registro
         DISPLAY "subproceso :",v_subproceso
         DISPLAY "nss :",v_rec_datos.nss
         DISPLAY "tipo env :",v_tpo_envio  --v_rec_datos.tpo_envio,
         DISPLAY "fecha envio :",v_ax_fecha --v_rec_datos.f_envio,
         DISPLAY "cve EF :",v_rec_datos.cve_ef
         DISPLAY "RFC :",v_rec_datos.rfc
         DISPLAY "CURP :",v_rec_datos.curp
         DISPLAY "paterno :",v_rec_datos.paterno
         DISPLAY "materno :",v_rec_datos.materno
         DISPLAY "nombre :",v_rec_datos.nombre
         DISPLAY "cred conve :",v_rec_datos.cred_convenidos

        CALL fn_borra_arch()

         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso"
         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_subpr1_tramite(0,?)"

         PREPARE prp_exe_fn_proceso FROM v_s_qry
         EXECUTE prp_exe_fn_proceso INTO v_sp_error
                                   USING p_v_usuario 

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode

         IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN 
            CALL fn_mensaje("Alerta","El registro se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

         INITIALIZE v_rec_datos.* TO NULL 
   END INPUT

END FUNCTION 


FUNCTION fn_exe_formalizacion()
   DEFINE v_rec_datos  RECORD
      nss                  CHAR(11),
      cve_ef               CHAR(3),
      rfc                  CHAR(13),
      curp                 CHAR(18),
      paterno              CHAR(40),
      materno              CHAR(40),
      nombre               CHAR(40),
      cred_convenidos      CHAR(1),
      ctl_int_ef           CHAR(18),
      num_escritura        CHAR(8),
      num_notario          CHAR(4),
      ent_fed_notario      CHAR(2),
      municipio_notario    CHAR(3),
      num_rpp              CHAR(15),
      folio_real           CHAR(8),
      partida              CHAR(6),
      foja                 CHAR(8),
      volumen              CHAR(6),
      libro                CHAR(6),
      tomo                 CHAR(6),
      seccion              CHAR(6),
      ent_fed_inmueble     CHAR(2),
      domicilio_inmueble   CHAR(30),
      valor_avaluo         CHAR(15),
      monto_credito        CHAR(15),
      plazo_credito        CHAR(5),
      tpo_moneda           CHAR(2),
      tasa_base            CHAR(20),
      margen               CHAR(20),
      f_otorga_cred_ef     DATE
   END RECORD
   DEFINE v_tot_aceptados  INTEGER 
   DEFINE v_tot_rechazados INTEGER
   DEFINE v_f_otorga_forma CHAR(8)

   LET v_tpo_registro = 2
   LET v_subproceso   = "002"

   DISPLAY v_subproceso TO subproceso

   INPUT BY NAME v_rec_datos.* ATTRIBUTES(UNBUFFERED)
      ON ACTION ACCEPT
         IF v_rec_datos.nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            EXIT INPUT
         END IF

         CALL fn_borra_tmp()

         LET v_f_otorga_forma = v_rec_datos.f_otorga_cred_ef USING "yyyymmdd"

         DISPLAY "Fecha con formato : ", v_f_otorga_forma

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_ocg43(tpo_registro       ,
                                                                subproceso         ,
                                                                tpo_envio          ,
                                                                f_envio            ,
                                                                cve_ent_financiera ,
                                                                nss                ,
                                                                num_ctrl_ef        ,
                                                                rfc                ,
                                                                curp               ,
                                                                ap_paterno_af      ,
                                                                ap_materno_af      ,
                                                                nombre_af          ,
                                                                num_escritura      ,
                                                                num_notario        ,
                                                                ent_fed_notario    ,
                                                                municipio_notario  ,
                                                                num_rpp            ,
                                                                folio_real         ,
                                                                partida            ,
                                                                foja               ,
                                                                volumen            ,
                                                                libro              ,
                                                                tomo               ,
                                                                seccion            ,
                                                                ent_fed_inmueble   ,
                                                                domicilio_inmueble ,
                                                                valor_avaluo       ,
                                                                monto_credito      ,
                                                                plazo_credito      ,
                                                                tpo_moneda         ,
                                                                tasa_base          ,
                                                                margen             ,
                                                                f_otorga_cred_ef   ,
                                                                cred_convenidos    )
                                                         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

         PREPARE prp_ins_tmp_f FROM v_s_qry
         EXECUTE prp_ins_tmp_f USING v_tpo_registro,
                                     v_subproceso                  ,
                                     v_tpo_envio                   ,
                                     v_ax_fecha                    ,
                                     v_rec_datos.cve_ef            ,
                                     v_rec_datos.nss               ,
                                     v_rec_datos.ctl_int_ef        ,
                                     v_rec_datos.rfc               ,
                                     v_rec_datos.curp              ,
                                     v_rec_datos.paterno           ,
                                     v_rec_datos.materno           ,
                                     v_rec_datos.nombre            ,
                                     v_rec_datos.num_escritura     ,
                                     v_rec_datos.num_notario       ,
                                     v_rec_datos.ent_fed_notario   ,
                                     v_rec_datos.municipio_notario ,
                                     v_rec_datos.num_rpp           ,
                                     v_rec_datos.folio_real        ,
                                     v_rec_datos.partida           ,
                                     v_rec_datos.foja              ,
                                     v_rec_datos.volumen           ,
                                     v_rec_datos.libro             ,
                                     v_rec_datos.tomo              ,
                                     v_rec_datos.seccion           ,
                                     v_rec_datos.ent_fed_inmueble  ,
                                     v_rec_datos.domicilio_inmueble,
                                     v_rec_datos.valor_avaluo      ,
                                     v_rec_datos.monto_credito     ,
                                     v_rec_datos.plazo_credito     ,
                                     v_rec_datos.tpo_moneda        ,
                                     v_rec_datos.tasa_base         ,
                                     v_rec_datos.margen            ,
                                     v_f_otorga_forma              ,
                                     v_rec_datos.cred_convenidos

         CALL fn_borra_arch()

         DISPLAY "tipo crédito : ",v_rec_datos.cred_convenidos

         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso"
         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_subpr2_formalizacion(0)"

         PREPARE prp_exe_fn_proceso_f FROM v_s_qry
         EXECUTE prp_exe_fn_proceso_f INTO v_sp_error,
                                           v_tot_aceptados,
                                           v_tot_rechazados

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode

         IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN 
            CALL fn_mensaje("Alerta","El registro se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

         INITIALIZE v_rec_datos.* TO NULL 
   END INPUT

END FUNCTION


FUNCTION fn_exe_ug()
   DEFINE v_rec_datos  RECORD
      nss                   CHAR(11),
      cve_ef                CHAR(3),
      ctl_int_ef            CHAR(18),
      imp_solic_ug          CHAR(15),
      f_venc_imp            DATE,
      cred_convenidos       CHAR(1),
      solicitud_saldo       CHAR(1)
   END RECORD
   DEFINE v_tot_aceptados  INTEGER 
   DEFINE v_tot_rechazados INTEGER
   DEFINE v_f_venc_format  CHAR(8)
   DEFINE v_f_envio_forma  CHAR(8)
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_resultado_ws   SMALLINT
   DEFINE v_id_arch        DECIMAL(9,0)

   LET v_tpo_registro = 2
   LET v_subproceso   = "003"
   LET v_id_arch      = 0 

   LET v_s_qry = " SELECT ruta_bin
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_bin FROM v_s_qry
   EXECUTE prp_r_bin INTO v_ruta_bin

   DISPLAY v_subproceso TO subproceso

   INPUT BY NAME v_rec_datos.* ATTRIBUTES(UNBUFFERED)
      ON ACTION ACCEPT
         IF v_rec_datos.nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            EXIT INPUT
         END IF

         LET v_f_envio_forma = v_f_envio USING "yyyymmdd"
         LET v_f_venc_format = v_rec_datos.f_venc_imp USING "yyyymmdd"

         CALL fn_borra_tmp()

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_ocg43(tpo_registro       ,
                                                                subproceso         ,
                                                                tpo_envio          ,
                                                                f_envio            ,
                                                                cve_ent_financiera ,
                                                                nss                ,
                                                                num_ctrl_ef        ,
                                                                imp_solic_uti_ocg  ,
                                                                f_venc_imp_solic   ,
                                                                cred_convenidos    ,
                                                                solic_saldo        )
                                                         VALUES(?,?,?,?,?,?,?,?,?,?,?)"

         PREPARE prp_ins_tmp_ug FROM v_s_qry
         EXECUTE prp_ins_tmp_ug USING v_tpo_registro                ,
                                      v_subproceso                  ,
                                      v_tpo_envio                   ,
                                      v_f_envio_forma               ,
                                      v_rec_datos.cve_ef            ,
                                      v_rec_datos.nss               ,
                                      v_rec_datos.ctl_int_ef        ,
                                      v_rec_datos.imp_solic_ug,
                                      v_f_venc_format               ,
                                      v_rec_datos.cred_convenidos   ,
                                      v_rec_datos.solicitud_saldo

         CALL fn_borra_arch()
--
{
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

   LET v_s_qry = " SELECT nss
                     FROM safre_tmp:tmp_rec_det_ocg43
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
--
}
         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso SP003"

         DATABASE safre_viv 

         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp3_solic_ug(?)"

         DISPLAY "qry SP: \n",v_s_qry

         PREPARE prp_exe_fn_proceso_ug FROM v_s_qry
         EXECUTE prp_exe_fn_proceso_ug USING v_id_arch
                                        INTO v_sp_error,
                                             v_tot_aceptados,
                                             v_tot_rechazados

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode

         IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN 
            CALL fn_mensaje("Alerta","El registro se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

         INITIALIZE v_rec_datos.* TO NULL 
   END INPUT

END FUNCTION


FUNCTION fn_exe_sp005()

   DEFINE v_cant_aceptados    INTEGER
   DEFINE v_cant_rechazados   INTEGER
   DEFINE v_f_deposito        CHAR(8)
   DEFINE v_f_libera_garantia CHAR(8)
   DEFINE v_f_envio2          CHAR(8)
   DEFINE a                   SMALLINT
   DEFINE v_detalle           STRING
   DEFINE ch                  base.Channel
   DEFINE v_nom_arch          STRING
   DEFINE v_nom_arch1         STRING
   DEFINE v_cmd               STRING
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_penalizacion      DECIMAL(13,2)
   DEFINE v_imp_devolucion    DECIMAL(13,2)
   DEFINE v_f_liberacion_gtia CHAR(8)
   DEFINE v_f_pago            CHAR(8)
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_nombre            CHAR(40)
   DEFINE v_f_actual          DATE
   DEFINE v_f_desde           DATE
   DEFINE v_dias              INTEGER
   DEFINE v_cnt_pros          INTEGER
   
   DEFINE arr_pros_dev DYNAMIC ARRAY OF RECORD
          id_ocg_ctr_transaccion DECIMAL(9,0),
          id_ocg_formalizacion  DECIMAL(9,0),
          id_derechohabiente    DECIMAL(9,0),
          paterno               CHAR(40),
          materno               CHAR(40),
          nombre                CHAR(40),
          nss                   CHAR(11),
          cve_ent_financiera    SMALLINT,
          num_ctr_int_ef        char(18),
          f_pago                DATE,
          bimestre_ap_subsec    char(6),
          aiv_ap_pat            decimal(18,2),
          importe_ap_pat        decimal(13,2),
          imp_rev_f_proeceso    decimal(18,2),
          imp_revaluado_15      decimal(13,5),
          f_transaccion         CHAR(8),
          f_factura             CHAR(10),
          f_liberacion_gtia     DATE
   END RECORD

   DEFINE v_rec_datos RECORD
      nss                        CHAR(11),
      cve_ef                     CHAR(3),
      ctl_int_ef                 CHAR(18),
      bim_aportacion_subsecuente CHAR(6),
      imp_aportacion_devuelto    CHAR(15),
      causa_liquida              CHAR(1),
      f_deposito                 DATE,
      cred_convenidos            CHAR(1),
      f_libera_garantia          DATE,
      importe_garantia_devuelto  CHAR(15)
   END RECORD

   DEFINE v_aiv_eq_1      DECIMAL(13,5)
   DEFINE v_aiv_eq_2      DECIMAL(13,2)
   DEFINE v_f_transaccion STRING
   DEFINE v_f_trans       CHAR(10)

   LET v_tpo_registro = 2
   LET v_subproceso   = "005"

   DISPLAY v_subproceso TO subproceso

   INPUT BY NAME v_rec_datos.* ATTRIBUTES(UNBUFFERED)
      ON ACTION ACCEPT
         IF v_rec_datos.nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            EXIT INPUT
         END IF

      LET v_f_envio2 = TODAY USING "yyyymmdd"
{
DISPLAY "v_tpo_registro                        ",   v_tpo_registro                        
DISPLAY "v_subproceso                          ",   v_subproceso                          
DISPLAY "v_rec_datos.nss                       ",   v_rec_datos.nss                       
DISPLAY "v_tpo_envio                           ",   v_tpo_envio
DISPLAY "v_f_envio2                            ",   v_f_envio2    
DISPLAY "v_rec_datos.cve_ef                    ",   v_rec_datos.cve_ef                    
DISPLAY "v_rec_datos.bim_aportacion_subsecuente",   v_rec_datos.bim_aportacion_subsecuente
DISPLAY "v_rec_datos.imp_aportacion_devuelto   ",   v_rec_datos.imp_aportacion_devuelto   
DISPLAY "v_rec_datos.id_causa_liquida          ",   v_rec_datos.causa_liquida          
DISPLAY "v_rec_datos.f_deposito                ",   v_rec_datos.f_deposito                
DISPLAY "v_rec_datos.cred_convenidos           ",   v_rec_datos.cred_convenidos           
DISPLAY "v_rec_datos.f_libera_garantia         ",   v_rec_datos.f_libera_garantia         
DISPLAY "v_rec_datos.imp_ocg_devuelto          ",   v_rec_datos.importe_garantia_devuelto  
}
         CALL fn_borra_tmp()

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_ocg43(tpo_registro,
                                                                subproceso,
                                                                nss,
                                                                tpo_envio,
                                                                f_envio,
                                                                cve_ent_financiera,
                                                                bim_apor_subsec,
                                                                imp_subsec_devuelto,
                                                                causa_liquidacion,
                                                                f_deposito,
                                                                cred_convenidos,
                                                                f_libera_garantia,
                                                                imp_ocg_devuelto)
                                                         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)"

LET v_f_deposito        = v_rec_datos.f_deposito        USING "yyyymmdd"
LET v_f_libera_garantia = v_rec_datos.f_libera_garantia USING "yyyymmdd"

         PREPARE prp_ins_tmp005 FROM v_s_qry
         EXECUTE prp_ins_tmp005 USING v_tpo_registro,
                                      v_subproceso,
                                      v_rec_datos.nss,
                                      v_tpo_envio,  --v_rec_datos.tpo_envio,
                                      v_f_envio2, --v_rec_datos.f_envio,
                                      v_rec_datos.cve_ef,
                                      v_rec_datos.bim_aportacion_subsecuente,
                                      v_rec_datos.imp_aportacion_devuelto,
                                      v_rec_datos.causa_liquida,
                                      v_f_deposito,
                                      v_rec_datos.cred_convenidos,
                                      v_f_libera_garantia,
                                      v_rec_datos.importe_garantia_devuelto         

         CALL fn_borra_arch()
         CALL fn_crea_tmp()
         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso"
         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp5_liquidacion(0)"

         PREPARE prp_exe_fn_sp005 FROM v_s_qry
         EXECUTE prp_exe_fn_sp005 INTO v_sp_error,
                                       v_cant_aceptados,
                                       v_cant_rechazados

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode

         IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN

         -- comentado para que no se generen prospectos de devolución por acuerdo tempral con usuario 13/12/2016
           -- SELECT COUNT(*)
          --    INTO v_cnt_pros
          --    FROM safre_tmp:tmp_ocg_devolucion_prospecto p

            LET v_cnt_pros = 0

            IF v_cnt_pros >= 1 THEN

            LET v_s_qry ="SELECT p.id_referencia_dis,
                                 p.id_ocg_formalizacion,
                                 p.id_derechohabiente,
                                 a.ap_paterno_af,
                                 a.ap_materno_af,
                                 a.nombre_af,
                                 p.nss,
                                 p.cve_ent_financiera,
                                 p.num_ctr_int_ef,
                                 p.bimestre_ap_subsec,
                                 p.aiv_ap_pat,
                                 p.importe_ap_pat,
                                 p.imp_revaluado_15,
                                 p.aiv_revaluado_15,
                                 lpad(year(p.f_transaccion),4,0)||
                                 lpad(month(p.f_transaccion),2,0)||
                                 lpad(day(p.f_transaccion),2,0)
                            FROM safre_tmp:tmp_ocg_devolucion_prospecto p,
                                 afi_derechohabiente a
                           WHERE p.id_derechohabiente = a.id_derechohabiente"

      PREPARE prp_dev FROM v_s_qry
      DECLARE cur_dev CURSOR FOR prp_dev

      LET a = 1
      FOREACH cur_dev INTO arr_pros_dev[a].id_ocg_ctr_transaccion,
                           arr_pros_dev[a].id_ocg_formalizacion,
                           arr_pros_dev[a].id_derechohabiente,
                           arr_pros_dev[a].paterno,
                           arr_pros_dev[a].materno,
                           arr_pros_dev[a].nombre,
                           arr_pros_dev[a].nss,
                           arr_pros_dev[a].cve_ent_financiera,
                           arr_pros_dev[a].num_ctr_int_ef,
                           arr_pros_dev[a].bimestre_ap_subsec,
                           arr_pros_dev[a].aiv_ap_pat,
                           arr_pros_dev[a].importe_ap_pat,
                           arr_pros_dev[a].imp_rev_f_proeceso,
                           arr_pros_dev[a].imp_revaluado_15,
                           arr_pros_dev[a].f_transaccion

         SELECT l.f_liberacion_gtia
           INTO arr_pros_dev[a].f_liberacion_gtia
           FROM ocg_liquidacion l
          WHERE l.id_ocg_formalizacion = arr_pros_dev[a].id_ocg_formalizacion
            AND l.id_derechohabiente   =  arr_pros_dev[a].id_derechohabiente

            DISPLAY "f_liberacion_gtia :",arr_pros_dev[a].f_liberacion_gtia

        
         LET v_f_trans = arr_pros_dev[a].f_transaccion
         LET v_f_transaccion = v_f_trans[5,6],"/",v_f_trans[7,8],"/",v_f_trans[1,4]
         LET v_f_trans = v_f_transaccion

         DISPLAY "periodo_pago  :",arr_pros_dev[a].bimestre_ap_subsec
         DISPLAY "f_transaccion :",v_f_trans

         SELECT UNIQUE f_pago
           INTO arr_pros_dev[a].f_pago
           FROM ocg_ctr_transaccion
          WHERE id_ocg_formalizacion = arr_pros_dev[a].id_ocg_formalizacion
            AND id_derechohabiente   = arr_pros_dev[a].id_derechohabiente
            AND periodo_pago         = arr_pros_dev[a].bimestre_ap_subsec
            AND f_transaccion        = v_f_trans


            DISPLAY "f_pago :",arr_pros_dev[a].f_pago
            DISPLAY "id_ocg_f",arr_pros_dev[a].id_ocg_formalizacion
            DISPLAY "dh",arr_pros_dev[a].id_derechohabiente
            DISPLAY "periodo",arr_pros_dev[a].bimestre_ap_subsec
            DISPLAY "f transacción",v_f_trans

   
         IF arr_pros_dev[a].cve_ent_financiera IS NULL THEN 
            LET arr_pros_dev[a].cve_ent_financiera = 0 
         END IF

IF arr_pros_dev[a].num_ctr_int_ef     IS NULL THEN 
LET arr_pros_dev[a].num_ctr_int_ef     = 0
END IF

IF arr_pros_dev[a].bimestre_ap_subsec IS NULL THEN 
LET arr_pros_dev[a].bimestre_ap_subsec = " "
END IF 

IF arr_pros_dev[a].aiv_ap_pat         IS NULL THEN 
LET arr_pros_dev[a].aiv_ap_pat         = 0
END IF

IF arr_pros_dev[a].importe_ap_pat     IS NULL THEN 
LET arr_pros_dev[a].importe_ap_pat     = 0
END IF 

IF arr_pros_dev[a].imp_rev_f_proeceso IS NULL THEN 
LET arr_pros_dev[a].imp_rev_f_proeceso = 0
END IF 

IF arr_pros_dev[a].imp_revaluado_15   IS NULL THEN 
LET arr_pros_dev[a].imp_revaluado_15   = 0
END IF

IF arr_pros_dev[a].f_transaccion      IS NULL THEN 
LET arr_pros_dev[a].f_transaccion      = " "
END IF                                  

IF arr_pros_dev[a].f_factura          IS NULL THEN 
LET arr_pros_dev[a].f_factura          = " "

END IF

        --DISPLAY "arreglo : ",arr_pros_dev[a].*
         LET a = a +1
      END FOREACH

      CALL arr_pros_dev.deleteElement(a)
      END IF

      IF arr_pros_dev.getLength() >= 1 THEN

         SELECT ruta_envio
           INTO v_ruta_envio
           FROM seg_modulo
          WHERE modulo_cod = 'ocg'

         LET v_nom_arch = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,TODAY USING "DDMMYYYY" CLIPPED,".dev"
         LET v_nom_arch1 = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,".dev"
         LET ch = base.Channel.create()
         CALL ch.openFile(v_nom_arch,"w" )
         CALL ch.setDelimiter("")

         FOR a=1 TO arr_pros_dev.getLength()

            LET v_penalizacion      = (arr_pros_dev[a].imp_rev_f_proeceso - arr_pros_dev[a].importe_ap_pat)
            LET v_imp_devolucion    = (arr_pros_dev[a].importe_ap_pat + v_penalizacion)
            LET v_f_liberacion_gtia = arr_pros_dev[a].f_liberacion_gtia USING "YYYYMMDD"
            LET v_f_pago            = arr_pros_dev[a].f_pago USING "YYYYDDMM"
            LET v_aiv_eq_1   = (arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].aiv_ap_pat)
            LET v_aiv_eq_2   = (arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].imp_revaluado_15)

            LET v_paterno = arr_pros_dev[a].paterno            CLIPPED
            LET v_materno = arr_pros_dev[a].materno            CLIPPED
            LET v_nombre  = arr_pros_dev[a].nombre             CLIPPED

            LET v_dias = 0
            LET v_f_actual = TODAY
            LET v_f_desde  = arr_pros_dev[a].f_pago
            -- Se ejecuta la función que realiza cálculo de días hábiles
            LET v_s_qry = "EXECUTE FUNCTION fn_cuenta_habil(?,?)"

            PREPARE prp_exe_cuenta_habil  FROM v_s_qry
            EXECUTE prp_exe_cuenta_habil USING v_f_actual,
                                               v_f_desde
                                          INTO v_dias

            LET v_detalle = arr_pros_dev[a].nss                CLIPPED,"|",
                            v_paterno                                 ,"¿",
                            v_materno                                 ,"¿",
                            v_nombre                                  ,"|",
                            arr_pros_dev[a].bimestre_ap_subsec CLIPPED,"|",
                            v_f_liberacion_gtia                CLIPPED,"|",
                            arr_pros_dev[a].importe_ap_pat     CLIPPED,"|",
                            arr_pros_dev[a].f_transaccion      CLIPPED,"|",
                            TODAY USING "YYYYMMDD","|",
                            v_dias,"|",
                            --v_f_pago                           CLIPPED,"|",
                            arr_pros_dev[a].aiv_ap_pat         CLIPPED,"|",
                            v_aiv_eq_1                         CLIPPED,"|",
                            arr_pros_dev[a].imp_revaluado_15   CLIPPED,"|",
                            v_aiv_eq_2                         CLIPPED,"|",
                            --arr_pros_dev[a].imp_rev_f_proeceso CLIPPED,"|",
                            v_penalizacion                     CLIPPED,"|",
                            v_imp_devolucion                   CLIPPED

            CALL ch.write([v_detalle])
         END FOR
         CALL ch.close()
         --    se crea comando que elimina espacios en blanco
         LET v_cmd = "sed 's/ //g' ",v_nom_arch," > ",v_nom_arch1
         RUN v_cmd

         --    se crea comando que elimina signos en nombre
         LET v_cmd = "sed 's/¿/ /g' ",v_nom_arch1," > ",v_nom_arch
         RUN v_cmd

          --    se crea comando para dejar archivos iguales
         LET v_cmd = "sed 's/ / /g' ",v_nom_arch," > ",v_nom_arch1
         RUN v_cmd
      END IF

            CALL fn_mensaje("Alerta","El registro se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

         INITIALIZE v_rec_datos.* TO NULL 
   END INPUT
END FUNCTION


FUNCTION fn_borra_tmp()
   LET v_s_qry = "DELETE
                    FROM safre_tmp:tmp_rec_det_ocg43 
                   WHERE 1=1"

   PREPARE prp_del_tmp FROM v_s_qry
   EXECUTE prp_del_tmp
END FUNCTION

FUNCTION fn_borra_arch()
      LET v_s_qry = "DELETE
                       FROM ocg_ctr_archivo 
                      WHERE id_ocg_ctr_archivo = 0"
      PREPARE prp_del_tabla_ctr FROM v_s_qry
      EXECUTE prp_del_tabla_ctr

      LET v_s_qry = "INSERT INTO ocg_ctr_archivo VALUES(0,0,today,3901,2,'pruebas',0,0,0,0,0,0,10,today ,'SAFREVIV')"
      PREPARE prp_ins_arch FROM v_s_qry
      EXECUTE prp_ins_arch
END FUNCTION 


FUNCTION fn_crea_tmp()
   DATABASE safre_tmp

   drop table if exists tmp_ocg_devolucion_prospecto;

   create table tmp_ocg_devolucion_prospecto(
   id_ocg_devolucion     decimal(9,0),
   id_referencia_dis     decimal(9,0),
   id_ocg_detalle        decimal(9,0),
   id_ocg_formalizacion  decimal(9,0),
   id_derechohabiente    decimal(9,0),
   nss                   CHAR(11),
   cve_ent_financiera    SMALLINT,
   num_ctr_int_ef        char(18),
   bimestre_ap_subsec    char(6),
   importe_ap_pat        decimal(18,6),
   aiv_ap_pat            decimal(18,6),
   imp_revaluado_15      decimal(18,6),
   aiv_revaluado_15      decimal(18,6),
   f_transaccion         DATE,
   f_factura             DATE,
   diagnostico           char(2),
   estado                SMALLINT,
   situacion             SMALLINT)

   DATABASE safre_viv
  
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