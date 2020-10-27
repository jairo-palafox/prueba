###############################################################################
#Modulo            => GRT                                                     #
#Programa          => GRTE104                                                 #
#Objetivo          => Programa para la captura de la cancelación o prororga   #
#Autor             => Héctor F. Jiménez Lara                                  #
#Fecha inicio      => 11 Enero 2016                                           #
###############################################################################
DATABASE safre_viv

   DEFINE v_s_qry             STRING
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod  -- clave del usuario firmado
   DEFINE v_mensaje           STRING
   DEFINE v_confirma_exe      SMALLINT  
   
MAIN
   DEFINE p_tipo_ejecucion    SMALLINT                      -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING                        -- título de la ventana
   DEFINE v_nss               CHAR(11)
   DEFINE v_r_nss_correcto    SMALLINT
   DEFINE v_ax_id_grt_tramite DECIMAL(9,0)
   DEFINE v_ax_cons_ok        SMALLINT

   -- se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTE103.log")

   CLOSE WINDOW SCREEN 
   
   OPEN WINDOW w_nss WITH FORM "GRTE1041"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED,CANCEL=FALSE)
         ON ACTION ACCEPT
            DISPLAY "INPUT NSS : ",v_nss 
            CALL fn_valida_nss(v_nss) RETURNING v_r_nss_correcto
            IF v_r_nss_correcto = TRUE THEN
               CALL fn_consulta_det_tramite(v_nss) RETURNING v_ax_id_grt_tramite, v_ax_cons_ok
               IF v_ax_cons_ok = 1 THEN
                  DISPLAY "Funcion consulta retorna: ",v_ax_id_grt_tramite
                  CALL fn_prorroga_cancela(v_nss,v_ax_id_grt_tramite)
                  EXIT INPUT
               ELSE
                  INITIALIZE v_nss TO NULL 
               END IF 
            END IF
         ON ACTION salir 
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_nss
END MAIN 

FUNCTION fn_prorroga_cancela(p_nss,p_id_grt_tramite)
   DEFINE p_id_grt_tramite   DECIMAL(9,0)
   DEFINE p_nss              CHAR(11)
   DEFINE v_rec_tramite RECORD
      id_grt_tramite         DECIMAL(9,0),
      cve_ent_financiera     SMALLINT,
      id_derechohabiente     DECIMAL(9,0),
      num_ctr_int_ef         CHAR(18),
      rfc                    CHAR(13),
      curp                   CHAR(18),
      ap_paterno             CHAR(40),
      ap_materno             CHAR(40),
      nombre                 CHAR(40),
      num_bimestres          SMALLINT,
      viv97                  DECIMAL(12,2),
      f_saldo                DATE,
      tpo_credito            CHAR(1),
      f_vigencia             DATE,
      diagnostico            CHAR(2),
      estado                 SMALLINT,
      situacion              SMALLINT
   END RECORD
   DEFINE v_f_proceso        DATE
   DEFINE v_f_prorroga       DATE
   DEFINE v_f_cancela        DATE
   DEFINE v_edo_cancela      SMALLINT
   DEFINE v_existe_formaliza SMALLINT
   DEFINE v_estado           SMALLINT

   LET v_f_proceso   = TODAY
   LET v_edo_cancela = 130 --Cancelado

   LET v_s_qry = " SELECT id_grt_tramite     ,
                           cve_ent_financiera ,
                           id_derechohabiente ,
                           rfc                ,
                           curp               ,
                           ap_paterno         ,
                           ap_materno         ,
                           nombre             ,
                           num_bimestres      ,
                           viv97              ,
                           f_saldo            ,
                           tpo_credito        ,
                           f_vigencia         ,
                           diagnostico        ,
                           estado             ,
                           situacion 
                      FROM grt_tramite
                     WHERE id_grt_tramite = ?
                       AND id_derechohabiente IN (
                           SELECT id_derechohabiente
                             FROM afi_derechohabiente
                            WHERE nss = ? ) "

    PREPARE prp_cons_tramite FROM v_s_qry
    EXECUTE prp_cons_tramite USING p_id_grt_tramite,
                                   p_nss
                              INTO v_rec_tramite.id_grt_tramite    ,
                                   v_rec_tramite.cve_ent_financiera,
                                   v_rec_tramite.id_derechohabiente,
                                   v_rec_tramite.rfc               ,
                                   v_rec_tramite.curp              ,
                                   v_rec_tramite.ap_paterno        ,
                                   v_rec_tramite.ap_materno        ,
                                   v_rec_tramite.nombre            ,
                                   v_rec_tramite.num_bimestres     ,
                                   v_rec_tramite.viv97             ,
                                   v_rec_tramite.f_saldo           ,
                                   v_rec_tramite.tpo_credito       ,
                                   v_rec_tramite.f_vigencia        ,
                                   v_rec_tramite.diagnostico       ,
                                   v_rec_tramite.estado            ,
                                   v_rec_tramite.situacion

   MENU
      ON ACTION prorroga
         LET v_mensaje = "Está seguro que desea aplicar prórroga de trámite al NSS : " || p_nss

         CALL fn_ventana_confirma("Alerta",v_mensaje,"stop") RETURNING v_confirma_exe

         IF v_confirma_exe = 1 THEN

            LET v_s_qry = " INSERT INTO grt_his_tramite VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

            PREPARE prp_ins_tramite FROM v_s_qry
            EXECUTE prp_ins_tramite USING v_rec_tramite.id_grt_tramite    ,
                                          v_rec_tramite.cve_ent_financiera,
                                          v_rec_tramite.id_derechohabiente,
                                          v_rec_tramite.num_ctr_int_ef    ,
                                          v_rec_tramite.rfc               ,
                                          v_rec_tramite.curp              ,
                                          v_rec_tramite.ap_paterno        ,
                                          v_rec_tramite.ap_materno        ,
                                          v_rec_tramite.nombre            ,
                                          v_rec_tramite.num_bimestres     ,
                                          v_rec_tramite.viv97             ,
                                          v_rec_tramite.f_saldo           ,
                                          v_rec_tramite.tpo_credito       ,
                                          v_rec_tramite.f_vigencia        ,
                                          v_rec_tramite.diagnostico       ,
                                          v_rec_tramite.estado            ,
                                          v_rec_tramite.situacion         ,
                                          v_f_proceso                     ,
                                          p_usuario_cod

            LET v_f_prorroga = TODAY + 56 UNITS DAY

            LET v_s_qry = "UPDATE grt_tramite
                              SET f_vigencia = ?
                            WHERE id_grt_tramite = ?"

            PREPARE prp_upd_tramite FROM v_s_qry
            EXECUTE prp_upd_tramite USING v_f_prorroga,
                                          p_id_grt_tramite

            IF SQLCA.sqlcode = 0 THEN
               DISPLAY v_f_prorroga TO v_f_vigencia 
               LET v_mensaje = "Prórroga aplicada al NSS : " || p_nss
               CALL fn_mensaje("Alerta",v_mensaje,"stop")
               EXIT MENU
            ELSE 
               CALL fn_mensaje("Alerta","Ocurrió un error al actualizar el registro","stop")
            END IF
         ELSE
            EXIT MENU
         END IF

      ON ACTION cancelacion
         LET v_mensaje = "Está seguro que desea cancelar el proceso de trámite al NSS : " || p_nss
         CALL fn_ventana_confirma("Alerta",v_mensaje,"stop") RETURNING v_confirma_exe

         IF v_confirma_exe = 1 THEN
            LET v_s_qry = " SELECT COUNT(*)
                              FROM grt_formalizacion 
                             WHERE id_grt_tramite = ?
                               AND estado NOT IN (20,40) "

            PREPARE prp_cons_formaliza FROM v_s_qry
            EXECUTE prp_cons_formaliza INTO v_existe_formaliza
                                      USING v_rec_tramite.id_grt_tramite

            IF v_existe_formaliza > 0 THEN
               CALL fn_mensaje("","No es posible cancelar un proceso de trámite,\n que tiene registros en formalización","")
               EXIT MENU
            END IF

            LET v_s_qry = " INSERT INTO grt_his_tramite VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

            PREPARE prp_ins_tram_canc FROM v_s_qry
            EXECUTE prp_ins_tram_canc USING v_rec_tramite.id_grt_tramite    ,
                                            v_rec_tramite.cve_ent_financiera,
                                            v_rec_tramite.id_derechohabiente,
                                            v_rec_tramite.num_ctr_int_ef    ,
                                            v_rec_tramite.rfc               ,
                                            v_rec_tramite.curp              ,
                                            v_rec_tramite.ap_paterno        ,
                                            v_rec_tramite.ap_materno        ,
                                            v_rec_tramite.nombre            ,
                                            v_rec_tramite.num_bimestres     ,
                                            v_rec_tramite.viv97             ,
                                            v_rec_tramite.f_saldo           ,
                                            v_rec_tramite.tpo_credito       ,
                                            v_rec_tramite.f_vigencia        ,
                                            v_rec_tramite.diagnostico       ,
                                            v_rec_tramite.estado            ,
                                            v_rec_tramite.situacion         ,
                                            v_f_proceso                     ,
                                            p_usuario_cod

            DISPLAY "SQLCA del insert",sqlca.sqlcode

            LET v_f_cancela = TODAY

            LET v_s_qry = "UPDATE grt_tramite
                              SET f_vigencia = ?,
                                  estado     = ?
                            WHERE id_grt_tramite = ?"

            PREPARE prp_canc_tramite FROM v_s_qry
            EXECUTE prp_canc_tramite USING v_f_cancela,
                                           v_edo_cancela,
                                           p_id_grt_tramite

            IF SQLCA.sqlcode = 0 THEN
               LET v_mensaje = "Cancelación aplicada al NSS : " || p_nss
               CALL fn_mensaje("Alerta",v_mensaje,"stop")

               EXIT MENU
               -- Se consulta la descripción del estado
               LET v_s_qry = " SELECT estado
                                 FROM grt_tramite
                                WHERE id_grt_tramite = ? "

               PREPARE prp_cons_edo_cred_b FROM v_s_qry
               EXECUTE prp_cons_edo_cred_b USING v_rec_tramite.id_grt_tramite
                                            INTO v_estado

               DISPLAY v_f_cancela  TO v_f_vigencia
               DISPLAY BY NAME v_estado

            ELSE 
               CALL fn_mensaje("Alerta","Ocurrio un error al actualizar el registro","stop")
            END IF
         ELSE
            EXIT MENU
         END IF

      ON ACTION salir
         EXIT MENU 
   END MENU
END FUNCTION 

-- Función para validar nss
FUNCTION fn_valida_nss(p_nss_modificado)
   DEFINE p_nss_modificado    LIKE afi_derechohabiente.nss
   DEFINE v_nss               STRING    -- cadena con el NSS,
   DEFINE v_mensaje           STRING    -- mensaje para el usuario
   DEFINE v_indice            SMALLINT  -- indice pivote
   DEFINE v_nss_es_correcto   SMALLINT  -- booleana que indica si un NSS esta correctamente construido

   LET v_nss = p_nss_modificado CLIPPED

   -- se asume que el NSS esta correcto
   LET v_nss_es_correcto = TRUE
   
   -- NSS debe ser de 11 digitos
   IF ( v_nss.getLength() <> 11 ) THEN
      LET v_mensaje = "La longitud del NSS debe ser de 11 dígitos"
      LET v_nss_es_correcto = FALSE
   ELSE
      -- se verifica que todos los caracteres sean numericos
      FOR v_indice = 1 TO v_nss.getLength()
         IF ( v_nss.getCharAt(v_indice) < "0" OR v_nss.getCharAt(v_indice) > "9" ) THEN
            LET v_mensaje = "El NSS contiene caracteres no numéricos."
            LET v_nss_es_correcto = FALSE
            EXIT FOR
         END IF
      END FOR
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nss_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nss_es_correcto
END FUNCTION



FUNCTION fn_consulta_det_tramite(p_nss)
   DEFINE p_nss                  CHAR(11)
   DEFINE v_rec_detalle      RECORD
      id                         DECIMAL(9,0),
      situacion                  SMALLINT,
      subproceso                 SMALLINT, --CHAR(3),
      cve_ent_financiera         SMALLINT,
      diagnostico                SMALLINT, --CHAR(2),
      f_proceso                  DATE,
      nss                        CHAR(11),
      rfc                        CHAR(13),
      nombre                     VARCHAR(80)
   END RECORD
   DEFINE v_desc_ent_financiera  CHAR(60)
   DEFINE v_curp                 CHAR(18)
   DEFINE v_control_ef           CHAR(18)
   DEFINE v_ap_paterno           CHAR(40)
   DEFINE v_ap_materno           CHAR(40)
   DEFINE v_nombre               CHAR(40)
   DEFINE v_f_envio              DATE 
   DEFINE v_producto             CHAR(1)
   DEFINE v_bim_aport            SMALLINT
   DEFINE v_sdo_97               DECIMAL(12,2)
   DEFINE v_id_grt_tramite       DECIMAL(9,0)
   DEFINE v_f_subcta             DATE
   DEFINE v_carga                DATE   -- consultar 
   DEFINE v_respuesta            DATE   -- consultar 
   DEFINE v_f_vigencia           DATE 
   DEFINE v_estado               SMALLINT
   DEFINE v_edo_desc             CHAR(40)
   DEFINE v_cons_ok              SMALLINT
   DEFINE v_desc_situacion       CHAR(40)
   
   LET v_bim_aport = 0
   LET v_rec_detalle.subproceso = 1
   LET v_rec_detalle.diagnostico = 1
   LET v_cons_ok = 0

   LET v_s_qry = "SELECT g.ap_paterno,                                 \n
                         g.ap_materno,                                 \n
                         g.nombre,                                     \n
                         g.curp,                                       \n
                         g.rfc,                                        \n
                         g.tpo_credito,                                \n
                         g.viv97,                                      \n
                         g.f_saldo,                                    \n
                         g.f_vigencia,                                 \n
                         g.id_grt_tramite,                             \n
                         g.estado,                                     \n
                         g.situacion                                   \n
                    FROM grt_tramite g,                                \n
                         afi_derechohabiente a                         \n
                   WHERE g.id_derechohabiente = a.id_Derechohabiente   \n
                     AND a.nss       = ?                               \n
                     AND diagnostico = ? "

   DISPLAY "NSS : ",p_nss
   DISPLAY "Querys 1 a a ejecutar : " ,v_s_qry

   PREPARE prp_cons_dat_tram FROM v_s_qry
   EXECUTE prp_cons_dat_tram USING p_nss,
                                   v_rec_detalle.diagnostico
                              INTO v_ap_paterno,
                                   v_ap_materno,
                                   v_nombre,
                                   v_curp,
                                   v_rec_detalle.rfc,
                                   v_producto,
                                   v_sdo_97,
                                   v_f_subcta,
                                   v_f_vigencia,
                                   v_rec_detalle.id,
                                   v_estado,
                                   v_rec_detalle.situacion

   DISPLAY "ID :",v_rec_detalle.id
   DISPLAY "DIAGNOSTICO: ",v_rec_detalle.diagnostico

   IF SQLCA.SQLCODE = 100 THEN 
      LET v_mensaje = "No se encontraron registro correspondientes al NSS " || p_nss || " o se encuentra rechazado"
      CALL fn_mensaje("Alerta",v_mensaje,"stop")
   ELSE
      LET v_cons_ok = 1

      LET v_s_qry = "SELECT situacion_desc
                       FROM cat_grt_situacion 
                      WHERE situacion = ? "

      PREPARE prp_cons_situacion FROM v_s_qry
      EXECUTE prp_cons_situacion USING v_rec_detalle.situacion
                                  INTO v_desc_situacion

      LET v_s_qry = "SELECT ga.f_lote,                                      \n
                            ga.f_proceso,                                   \n
                            gd.f_proceso                                    \n
                       FROM grt_tramite g,                                  \n
                            grt_detalle gd,                                 \n
                            grt_ctr_archivo ga                              \n
                      WHERE gd.id_derechohabiente = g.id_derechohabiente    \n
                        AND gd.cve_ent_financiera = g.cve_ent_financiera    \n
                        AND gd.id_grt_detalle     = g.id_grt_detalle        \n
                        AND ga.id_grt_ctr_archivo = gd.id_grt_ctr_archivo   \n
                        AND gd.nss           = ?                            \n
                        AND g.id_grt_tramite = ? "

      DISPLAY "Querys 2 a a ejecutar : " ,v_s_qry

      PREPARE prp_cons_dat_arch FROM v_s_qry
      EXECUTE prp_cons_dat_arch USING p_nss,
                                      v_rec_detalle.id
                                 INTO v_f_envio,
                                      v_carga,
                                      v_rec_detalle.f_proceso

      LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc   \n
                       FROM cat_ent_financiera c,                                                \n
                            grt_tramite g                                                        \n
                      WHERE g.cve_ent_financiera  = c.cve_ent_financiera                         \n
                        AND g.id_grt_tramite = ? "

      DISPLAY "Querys 3 a a ejecutar : " ,v_s_qry

      PREPARE prp_cons_dat_ef FROM v_s_qry
      EXECUTE prp_cons_dat_ef USING v_rec_detalle.id
                               INTO v_desc_ent_financiera

      -- la fecha de respuesta es un día hábil posterior al dia de la fecha de carga
      LET v_s_qry = "EXECUTE FUNCTION fn_habil_siguiente(?,2)"
      PREPARE prp_habil FROM v_s_qry
      EXECUTE prp_habil USING v_carga INTO v_respuesta

      DISPLAY v_rec_detalle.situacion   TO v_situacion
      --DISPLAY v_desc_situacion          TO v_situacion                               
      DISPLAY v_rec_detalle.subproceso  TO v_subproceso
      DISPLAY v_desc_ent_financiera     TO v_cve_ent_financiera
      DISPLAY v_rec_detalle.diagnostico TO v_diagnostico                
      DISPLAY v_rec_detalle.f_proceso   TO v_f_proceso                  
      DISPLAY p_nss                     TO v_nss
      DISPLAY v_rec_detalle.rfc         TO v_rfc
      DISPLAY BY NAME v_estado
      DISPLAY BY NAME p_nss
      DISPLAY BY NAME v_sdo_97      
      DISPLAY BY NAME v_ap_paterno
      DISPLAY BY NAME v_ap_materno
      DISPLAY BY NAME v_nombre
      DISPLAY BY NAME v_control_ef
      DISPLAY BY NAME v_producto
      DISPLAY BY NAME v_f_subcta
      DISPLAY BY NAME v_f_envio
      DISPLAY BY NAME v_curp
      DISPLAY BY NAME v_carga
      DISPLAY BY NAME v_respuesta
      DISPLAY BY NAME v_f_vigencia

   END IF

   RETURN v_rec_detalle.id, v_cons_ok
END FUNCTION 
