###############################################################################
#Modulo            => GRT                                                     #
#Programa          => GRTE110                                                 #
#Objetivo          => Programa para realizar pruebas ingresando los datos     #
#                     oblgatorios para ejecutar los subprocesose 1,2,3,5      #
#                     sin necesidad de ingresar un archivo                    #
#Autor             => Héctor F. Jiménez Lara                                  #
#Fecha inicio      => 08 Enero 2016                                           #
###############################################################################
DATABASE safre_viv 

   DEFINE v_s_qry        STRING
   DEFINE v_tpo_envio    CHAR(1)
   DEFINE v_f_envio      DATE 
   DEFINE v_ax_fecha     CHAR(8)
   DEFINE v_sp_error     SMALLINT
   -- Variables estáticas
   DEFINE v_tpo_registro SMALLINT
   DEFINE v_subproceso   CHAR(3)
   DEFINE v_s_msj        STRING
   DEFINE p_v_usuario         LIKE seg_usuario.usuario

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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE102.log")

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
   OPEN WINDOW w_proc WITH FORM "GRTE1101"
      DISPLAY v_f_envio   TO f_envio
      DISPLAY v_tpo_envio TO tpo_envio

      MENU
         ON ACTION SP001
            CALL fn_exe_tramite()
         ON ACTION SP002
            CALL fn_exe_formalizacion()
         ON ACTION SP003
            EXIT MENU 
         ON ACTION SP005
            EXIT MENU
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

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_grt43(tpo_registro,
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
         LET v_s_qry = "EXECUTE FUNCTION fn_grt_valida_subpr1_tramite(0,?)"

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

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_grt43(tpo_registro       ,
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

         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso"
         LET v_s_qry = "EXECUTE FUNCTION fn_grt_valida_subpr2_formalizacion(0)"

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
      importe_solicitado    CHAR(15),
      f_vencimiento_importe DATE,
      importe_utilizado     CHAR(15),
      cred_convenidos       CHAR(1),
      solicitud_saldo       CHAR(1)
   END RECORD
   DEFINE v_tot_aceptados  INTEGER 
   DEFINE v_tot_rechazados INTEGER
   DEFINE v_f_venc_format  CHAR(8)

   LET v_tpo_registro = 2
   LET v_subproceso   = "003"

   DISPLAY v_subproceso TO subproceso

   INPUT BY NAME v_rec_datos.* ATTRIBUTES(UNBUFFERED)
      ON ACTION ACCEPT
         IF v_rec_datos.nss IS NULL THEN
            CALL fn_mensaje("","NSS necesario para procesar registro","")
            EXIT INPUT
         END IF

         LET v_f_venc_format = v_rec_datos.f_vencimiento_importe USING "yyyymmdd"

         CALL fn_borra_tmp()

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_rec_det_grt43(tpo_registro       ,
                                                                subproceso         ,
                                                                tpo_envio          ,
                                                                f_envio            ,
                                                                cve_ent_financiera ,
                                                                nss                ,
                                                                num_ctrl_ef        ,
                                                                imp_solic_uti_grt  ,
                                                                f_venc_imp_solic   ,
                                                                imp_utilizado_grt  ,
                                                                cred_convenidos    ,
                                                                solic_saldo        )
                                                         VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"

         PREPARE prp_ins_tmp_ug FROM v_s_qry
         EXECUTE prp_ins_tmp_ug USING v_tpo_registro                ,
                                      v_subproceso                  ,
                                      v_tpo_envio                   ,
                                      v_ax_fecha                    ,
                                      v_rec_datos.cve_ef            ,
                                      v_rec_datos.nss               ,
                                      v_rec_datos.ctl_int_ef        ,
                                      v_rec_datos.importe_solicitado,
                                      v_f_venc_format               ,
                                      v_rec_datos.importe_utilizado ,
                                      v_rec_datos.cred_convenidos   ,
                                      v_rec_datos.solicitud_saldo

         CALL fn_borra_arch()

         -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso SP003"
         LET v_s_qry = "EXECUTE FUNCTION fn_grt_valida_sp3_solic_ug(0)"

         PREPARE prp_exe_fn_proceso_ug FROM v_s_qry
         EXECUTE prp_exe_fn_proceso_ug INTO v_sp_error,
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



FUNCTION fn_borra_tmp()
   LET v_s_qry = "DELETE
                    FROM safre_tmp:tmp_rec_det_grt43 
                   WHERE 1=1"

   PREPARE prp_del_tmp FROM v_s_qry
   EXECUTE prp_del_tmp
END FUNCTION

FUNCTION fn_borra_arch()
      LET v_s_qry = "DELETE
                       FROM grt_ctr_archivo 
                      WHERE id_grt_ctr_archivo = 0"
      PREPARE prp_del_tabla_ctr FROM v_s_qry
      EXECUTE prp_del_tabla_ctr

      LET v_s_qry = "INSERT INTO grt_ctr_archivo VALUES(0,0,today,3901,2,'pruebas',0,0,0,0,0,0,10,today ,'SAFREVIV')"
      PREPARE prp_ins_arch FROM v_s_qry
      EXECUTE prp_ins_arch
END FUNCTION 