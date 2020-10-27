






CREATE FUNCTION "safreviv".fn_afi_integra_baja_registro(p_folio                          DECIMAL(10) ,
                                             tmp_afi_baja_tpo_movimiento      CHAR(2)     ,
                                             tmp_afi_baja_nrp                 CHAR(11)    ,
                                             tmp_afi_baja_f_movimiento        CHAR(8)     ,
                                             tmp_afi_baja_curp_rfc            CHAR(18)    ,
                                             tmp_afi_baja_t_trabajador        DECIMAL(1,0),
                                             tmp_afi_baja_nss                 CHAR(11)    ,
                                             tmp_afi_baja_nombre              CHAR(50)    ,
                                             tmp_afi_baja_presentacion_extemp DECIMAL(1,0),
                                             tmp_afi_baja_jornada_semana      DECIMAL(1,0),
                                             tmp_afi_baja_sdi                 DECIMAL(6,0),
                                             tmp_afi_baja_sexo                DECIMAL(1,0),
                                             tmp_afi_baja_nss_correcto        CHAR(11)    ,
                                             tmp_afi_baja_nombre_correcto     CHAR(50)    ,
                                             tmp_afi_baja_riss_imss           SMALLINT    ,
                                             tmp_afi_riss_inf                 SMALLINT    ,
                                             p_usuario_cod                    CHAR(20))
   RETURNING SMALLINT, SMALLINT, VARCHAR(255)

   -- campos de la tabla de rechazos afi_rch_afiliatorio
   DEFINE afi_rch_afiliatorio_tpo_movimiento      CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_espacios            CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_nrp                 CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_f_movimiento        CHAR(8)     ;
   DEFINE afi_rch_afiliatorio_curp_rfc            CHAR(18)    ;
   DEFINE afi_rch_afiliatorio_t_trabajador        DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss                 CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre              CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_presentacion_extemp DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_jornada_semana      DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_sdi                 DECIMAL(6,0);
   DEFINE afi_rch_afiliatorio_sexo                DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss_correcto        CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre_correcto     CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_cod_rechazo         SMALLINT    ;

   -- campos de la tabla afi_his_derechohabiente
   DEFINE afi_his_derechohab_id_derechohabiente   DECIMAL(9,0);
   DEFINE afi_his_derechohab_f_modifica           DATE        ;
   DEFINE afi_his_derechohab_folio_lote_modifica  DECIMAL(9,0);
   DEFINE afi_his_derechohab_ind_modifica         CHAR(18)    ;
   DEFINE afi_his_derechohab_curp                 CHAR(18)    ;
   DEFINE afi_his_derechohab_rfc                  CHAR(13)    ;
   DEFINE afi_his_derechohab_ind_nrp              CHAR(1)     ;
   DEFINE afi_his_derechohab_f_nacimiento         DATE        ;
   DEFINE afi_his_derechohab_nombre_imss          CHAR(50)    ;
   DEFINE afi_his_derechohab_nombre_af            CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_paterno_af        CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_materno_af        CHAR(40)    ;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                CHAR(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp         DATE        ;
   DEFINE afi_relacion_laboral_ind_relacion       SMALLINT    ;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        DATE        ;
   DEFINE afi_relacion_laboral_usuario            CHAR(20)    ;

   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
   DEFINE v_rfc_curp                              VARCHAR(18);
   DEFINE v_conteo                                INTEGER;
   DEFINE v_nrp                                   CHAR(11);
   DEFINE v_riss                                  SMALLINT;
   DEFINE v_riss_rl                               SMALLINT;
   DEFINE v_rl                                    SMALLINT;
   DEFINE v_verif_riss                            SMALLINT;
   DEFINE v_rl_nrp                                SMALLINT;

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE v_rechazo                               SMALLINT;

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_codigo_rechazo                        SMALLINT;

   -- conteo de bajas procesadas, aceptadas y rechazadas
   DEFINE v_bajas_aceptadas                       SMALLINT;
   DEFINE v_bajas_rechazadas                      SMALLINT;
   DEFINE v_bajas_riss_acep                       SMALLINT;
   DEFINE v_bajas_riss_rech                       SMALLINT;
   DEFINE v_f_actualiza                           DATE;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado    = sql_err;
      LET v_codigo_rechazo = v_i_resultado;

      IF tmp_afi_baja_tpo_movimiento = '09' THEN
         LET tmp_afi_baja_tpo_movimiento = '02';
         LET v_bajas_rechazadas          = 1;
         LET v_i_resultado               = -1;
      ELSE
         LET tmp_afi_baja_tpo_movimiento = '21';
         LET v_bajas_riss_rech           = 1;
         LET v_i_resultado               = -21;
      END IF

    -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento      ,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          tmp_afi_baja_riss_imss           ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION

   -- Variables que almacenan información para su validación
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de movimientos afiliatorios de baja finalizó correctamente";
   LET v_codigo_rechazo   = 0;
   LET v_nrp              = "0";
   LET v_rl               = 0;
   LET v_riss             = 0;
   LET v_rechazo          = 0;
   LET v_verif_riss       = 0;
   LET v_rl_nrp           = 0;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_baja.trace";
   --TRACE ON;

   -- el tipo de movimiento se regresa a 02
   IF tmp_afi_baja_tpo_movimiento = "09" THEN
      LET tmp_afi_baja_tpo_movimiento = "02";
      LET v_i_resultado               = 1;
   ELSE
      LET tmp_afi_baja_tpo_movimiento = "21";
      LET v_i_resultado               = 21;
   END IF

   -- se inician los contadores
   LET v_bajas_aceptadas  = 0;
   LET v_bajas_rechazadas = 0;
   LET v_bajas_riss_acep  = 0;
   LET v_bajas_riss_rech  = 0;
   LET v_f_actualiza      = TODAY;

   -- se valida la fecha de movimiento
   EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_baja_f_movimiento,"ddmmyyyy")
               INTO v_fecha_valida, v_fecha_movimiento;

   -- si la fecha de movimiento es posterior a la actual, se rechaza el registro
   IF ( v_fecha_movimiento > TODAY ) THEN
      -- se rechaza el registro por tener fecha de movimiento invalida
      LET v_rechazo        = 1;
      LET v_codigo_rechazo = 9;

      IF tmp_afi_baja_tpo_movimiento = "02" THEN
         LET v_bajas_rechazadas = 1;
         LET v_i_resultado      = -1;
      ELSE
         LET v_bajas_riss_rech = 1;
         LET v_i_resultado      = -21;
      END IF
   ELSE
      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_afi_baja_nss;

      -- si no se encuentra el NSS
      IF ( v_id_derechohabiente IS NULL ) THEN  -- NSS no existe
         LET v_rechazo        = 1;
         LET v_codigo_rechazo = 1;

         IF tmp_afi_baja_tpo_movimiento = "02" THEN
            LET v_bajas_rechazadas = 1;
            LET v_i_resultado      = -1;
         ELSE
            LET v_bajas_riss_rech = 1;
            LET v_i_resultado     = -21;
         END IF
      ELSE
         IF tmp_afi_baja_tpo_movimiento = "02" THEN
            -- se da de baja la relacion laboral
            DELETE
              FROM afi_relacion_laboral
             WHERE id_derechohabiente = v_id_derechohabiente
               AND nrp                = tmp_afi_baja_nrp;

            -- se verifica si se eliminó un registro
            IF ( DBINFO('sqlca.sqlerrd2') > 0 ) THEN
               LET v_bajas_aceptadas = 1;

               -- si ya no se tienen registros de relación laboral, se actualiza el derechohabiente
               -- en afi_derechohabiente como que no tiene relación laboral
               SELECT COUNT(*)
                 INTO v_conteo
                 FROM afi_relacion_laboral
                WHERE id_derechohabiente = v_id_derechohabiente;

               -- si no hay
               IF ( v_conteo < 1 ) THEN
                   -- se guarda el histórico del campo que se esta cambiando
                  SELECT nombre_imss  ,
                         curp         ,
                         rfc          ,
                         ind_nrp      ,
                         f_nacimiento ,
                         nombre_af    ,
                         ap_paterno_af,
                         ap_materno_af
                    INTO afi_his_derechohab_nombre_imss   ,
                         afi_his_derechohab_curp          ,
                         afi_his_derechohab_rfc           ,
                         afi_his_derechohab_ind_nrp       ,
                         afi_his_derechohab_f_nacimiento  ,
                         afi_his_derechohab_nombre_af     ,
                         afi_his_derechohab_ap_paterno_af ,
                         afi_his_derechohab_ap_materno_af
                    FROM afi_derechohabiente
                   WHERE id_derechohabiente = v_id_derechohabiente;

                  -- se asingnan los datos al registro de histórico
                  LET afi_his_derechohab_id_derechohabiente   = v_id_derechohabiente;
                  LET afi_his_derechohab_f_modifica           = TODAY; -- fecha de cambio
                  LET afi_his_derechohab_folio_lote_modifica  = p_folio;
                  LET afi_his_derechohab_ind_modifica         = 7; -- BAJA RELACION LABORAL

                  -- se inserta el histórico
                  INSERT INTO afi_his_derechohabiente
                         ( id_derechohabiente   ,
                           f_modifica           ,
                           folio_lote_modifica  ,
                           ind_modifica         ,
                           curp                 ,
                           rfc                  ,
                           ind_nrp              ,
                           f_nacimiento         ,
                           nombre_imss          ,
                           nombre_af            ,
                           ap_paterno_af        ,
                           ap_materno_af        )
                  VALUES ( afi_his_derechohab_id_derechohabiente  ,
                           afi_his_derechohab_f_modifica          ,
                           afi_his_derechohab_folio_lote_modifica ,
                           afi_his_derechohab_ind_modifica        ,
                           afi_his_derechohab_curp                ,
                           afi_his_derechohab_rfc                 ,
                           afi_his_derechohab_ind_nrp             ,
                           afi_his_derechohab_f_nacimiento        ,
                           afi_his_derechohab_nombre_imss         ,
                           afi_his_derechohab_nombre_af           ,
                           afi_his_derechohab_ap_paterno_af       ,
                           afi_his_derechohab_ap_materno_af       );

                  -- se actualiza el derechohabiente indicando que ya no tiene relaciones laborales
                  UPDATE afi_derechohabiente
                     SET ind_nrp = 0
                   WHERE id_derechohabiente = v_id_derechohabiente;
               END IF

               SELECT nrp, id_riss, id_riss_rl
                 INTO v_nrp, v_riss, v_riss_rl
                 FROM afi_riss
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND nrp                = tmp_afi_baja_nrp
                  AND id_riss            IN(1,2);

               IF v_nrp = tmp_afi_baja_nrp THEN
                  IF v_riss_rl = 0 OR v_riss_rl = 1 THEN
                     IF v_riss_rl = 1 THEN
                       LET v_rl = 2;
                     ELSE
                       LET v_rl = 4;
                     END IF;

                     LET v_verif_riss = 1;
                  END IF
               END IF
            ELSE
               SELECT nrp, id_riss, id_riss_rl
                 INTO v_nrp, v_riss, v_riss_rl
                 FROM afi_riss
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND nrp                = tmp_afi_baja_nrp
                  AND id_riss            IN(1,2);

               IF v_nrp = tmp_afi_baja_nrp THEN
                  IF v_riss_rl = 0 OR v_riss_rl = 1 THEN
                     IF v_riss_rl = 1 THEN
                       LET v_rl = 2;
                     ELSE
                       LET v_rl = 4;
                     END IF;

                     LET v_bajas_aceptadas = 1;
                     LET v_verif_riss      = 1;
                  ELSE
                     LET v_bajas_rechazadas = 1;
                     LET v_rechazo          = 1;
                     LET v_codigo_rechazo   = 19;
                     LET v_i_resultado      = -21;
                  END IF
               ELSE
                  -- no existía la relación entre el NRP y el NSS dado
                  -- se rechaza el registro
                  LET v_bajas_rechazadas = 1;
                  LET v_rechazo          = 1;
                  LET v_codigo_rechazo   = 12;
                  LET v_i_resultado      = -1;
               END IF
            END IF
         ELSE
            IF tmp_afi_baja_riss_imss = 3 THEN
               SELECT 1
                 INTO v_rl_nrp
                 FROM afi_relacion_laboral
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND nrp                = tmp_afi_baja_nrp;

               SELECT nrp, id_riss, id_riss_rl
                 INTO v_nrp, v_riss, v_riss_rl
                 FROM afi_riss
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND nrp = tmp_afi_baja_nrp
                  AND id_riss IN(1,2);

               IF v_nrp = tmp_afi_baja_nrp THEN
                  IF v_riss_rl = 0 OR v_riss_rl = 1 THEN
                     IF v_riss_rl = 1 THEN
                        IF v_rl_nrp = 1 THEN
                           LET v_rl = 2;
                        ELSE
                           LET v_rl = 4;
                        END IF
                     ELSE
                        IF v_rl_nrp = 1 THEN
                           LET v_rl = 3;
                        ELSE
                           LET v_rl = 4;
                        END IF
                     END IF;

                     LET v_bajas_riss_acep = 1;
                     LET v_verif_riss      = 1;
                  ELSE
                     LET v_bajas_riss_rech = 1;
                     LET v_rechazo         = 1;
                     LET v_codigo_rechazo  = 19;
                     LET v_i_resultado     = -21;
                  END IF
               ELSE
                  IF v_rl_nrp = 1 THEN
                     LET v_rl = 3;
                  ELSE
                     LET v_rl = 5;
                  END IF

                  LET v_bajas_riss_acep = 1;
                  LET v_verif_riss      = 2;
               END IF
            ELSE
               LET v_bajas_riss_rech = 1;
               LET v_rechazo         = 1;
               LET v_codigo_rechazo  = 15;
               LET v_i_resultado     = -21;
            END IF
         END IF
      END IF
   END IF

   IF v_rechazo = 1 THEN
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento      ,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          tmp_afi_baja_riss_imss           ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt;
   END IF

   IF v_rechazo = 0 AND v_verif_riss > 0 THEN
      IF tmp_afi_baja_riss_imss IS NULL THEN
         LET tmp_afi_baja_riss_imss = 3;
      END IF

      IF v_verif_riss = 1 THEN
         UPDATE afi_riss
            SET id_riss      = tmp_afi_baja_riss_imss,
                folio_lote   = p_folio,
                f_proceso    = v_f_actualiza,
                f_movimiento = v_fecha_movimiento,
                id_riss_rl   = v_rl,
                f_proceso    = v_f_actualiza
          WHERE id_derechohabiente = v_id_derechohabiente
            AND nrp = tmp_afi_baja_nrp
            AND id_riss IN(1,2);
      ELSE
         INSERT INTO afi_riss
                    (id_derechohabiente,
                     id_riss,
                     folio_lote,
                     f_movimiento,
                     nrp,
                     id_riss_rl,
                     f_proceso,
                     usuario)
             VALUES
                    (v_id_derechohabiente,
                     tmp_afi_baja_riss_imss,
                     p_folio,
                     v_fecha_movimiento,
                     tmp_afi_baja_nrp,
                     v_rl,
                     v_f_actualiza,
                     p_usuario_cod);
      END IF
   END IF

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Bajas realizadas: " || v_bajas_aceptadas||" Bajas RISS realizadas: "||v_bajas_riss_acep||" Bajas rechazadas: " || v_bajas_rechazadas||" Bajas RISS rechazadas: " || v_bajas_riss_rech;

   -- se devuelve el resultado de la ejecucion de la función
   RETURN v_i_resultado, isam_err, err_txt;

END FUNCTION;


