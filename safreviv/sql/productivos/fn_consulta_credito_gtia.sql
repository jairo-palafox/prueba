






CREATE FUNCTION "safreviv".fn_consulta_credito_gtia(p_rfc_usuario        CHAR(13),
                                         p_cve_ent_financiera SMALLINT,
                                         p_nss                CHAR(11))
   RETURNING SMALLINT,       ---error
             SMALLINT,       ---cod diag
             VARCHAR(255),   ---descrp
             VARCHAR(123),   ---nombre dh
             CHAR(18),       ---CURP
             CHAR(13),       ---RFC
             SMALLINT,       ---fallecido
             SMALLINT,       ---rl
             DECIMAL(12,2),  ---saldo vivienda97
             CHAR(11)        ---unificador

   ---variables de control
   DEFINE v_ax_error                SMALLINT;
   DEFINE v_cod_rch_cons            SMALLINT;
   DEFINE v_desc_resp_cons          VARCHAR(255);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_paterno                 CHAR(40);
   DEFINE v_materno                 CHAR(40);
   DEFINE v_nombre                  CHAR(40);
   DEFINE v_nombre_dh               VARCHAR(123);
   DEFINE v_curp                    CHAR(18);
   DEFINE v_rfc_dh                  CHAR(13);
   DEFINE v_fallecido               SMALLINT;
   DEFINE v_rl                      SMALLINT;
   DEFINE v_sdo                     DECIMAL(12,2);
   DEFINE v_acc                     DECIMAL(12,2);
   DEFINE v_situacion               SMALLINT;
   DEFINE v_cve_ent_fin_reg         SMALLINT;
   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_nss_orig                CHAR(11);
   DEFINE v_id_dh_orig              DECIMAL(9,0);
   DEFINE v_diag_uni                SMALLINT;
   DEFINE v_bnd_consulta            SMALLINT;
   DEFINE v_fecha_valua             DATE;
   DEFINE v_valor_fondo             DECIMAL(18,6);
   DEFINE v_f_formalizacion         DATE;

   ---variables de log
   DEFINE v_rfc                     CHAR(13);
   DEFINE v_cve_ef                  SMALLINT;
   DEFINE v_nss                     CHAR(11);
   DEFINE v_f_ini                   DATETIME HOUR TO SECOND;
   DEFINE v_f_fin                   DATETIME HOUR TO SECOND;
   DEFINE v_status                  SMALLINT;


   ON EXCEPTION SET v_ax_error
      LET v_cod_rch_cons   = -1;
      LET v_nombre_dh      = "";
      LET v_nss_unificador = "";
      LET v_curp           = "";
      LET v_rfc_dh         = "";
      LET v_fallecido      = 0;
      LET v_rl             = 0;
      LET v_sdo            = 0;
      LET v_nss_unificador = "";

      SELECT rch_desc
        INTO v_desc_resp_cons
        FROM cat_rechazo_usr_ef
       WHERE rch_cod = v_cod_rch_cons;

      -- Devolverá el código de error que ocasione la excepción
      RETURN v_ax_error, v_cod_rch_cons, v_desc_resp_cons, v_nombre_dh, v_curp, v_rfc_dh, v_fallecido, v_rl, v_sdo, v_nss_unificador;
   END EXCEPTION

   /*
   SET DEBUG FILE TO '/safreviv_int/archivos/consCredGtia.trace';
   TRACE ON;
   */

   -- se inicializan variables
   LET v_ax_error           = 0;
   LET v_cod_rch_cons       = 0;
   LET v_desc_resp_cons     = "CRÉDITO VIGENTE";
   LET v_situacion          = 0;
   LET v_cve_ent_fin_reg    = 0;
   LET v_id_derechohabiente = 0;
   LET v_f_ini              = CURRENT;
   LET v_rfc                = p_rfc_usuario;
   LET v_cve_ef             = p_cve_ent_financiera;
   LET v_nss                = p_nss;
   LET v_nombre_dh          = "";
   LET v_paterno            = "";
   LET v_materno            = "";
   LET v_nombre             = "";
   LET v_curp               = "";
   LET v_rfc_dh             = "";
   LET v_fallecido          = 0;
   LET v_rl                 = 0;
   LET v_nss_unificador     = "";
   LET v_id_dh_unificador   = 0;
   LET v_nss_orig           = "";
   LET v_id_dh_orig         = 0;
   LET v_diag_uni           = 0;
   LET v_bnd_consulta       = 0;
   LET v_sdo                = 0;
   LET v_acc                = 0;
   LET v_fecha_valua        = TODAY;

   IF p_rfc_usuario IS NULL OR p_rfc_usuario = "" OR
      p_cve_ent_financiera IS NULL OR p_cve_ent_financiera = "" OR
      p_nss IS NULL OR p_nss = "" THEN
      LET v_cod_rch_cons = -2; ---Valores nulos
   ELSE
      IF NOT EXISTS(SELECT rfc
                      FROM cat_usuario_ef
                     WHERE rfc = p_rfc_usuario) THEN
         LET v_cod_rch_cons = 1; ---Usuario no registrado
      ELSE
         IF NOT EXISTS (SELECT id_derechohabiente
                          FROM afi_derechohabiente
                         WHERE nss = p_nss) THEN
            LET v_cod_rch_cons = 6; ---NSS no existe en la base de derechohabientes
         ELSE
            SELECT id_derechohabiente
              INTO v_id_derechohabiente
              FROM afi_derechohabiente
             WHERE nss = p_nss;

            IF NOT EXISTS (SELECT f.cve_ent_financiera
                            FROM ocg_formalizacion f, ocg_acreditado a
                           WHERE f.id_derechohabiente = v_id_derechohabiente
                             AND f.id_ocg_formalizacion = a.id_ocg_formalizacion
                             AND f.situacion IN(60,70,80,300,140,150,160)) THEN
               IF NOT EXISTS(SELECT cve_ent_financiera
                               FROM ocg_tramite
                              WHERE id_derechohabiente = v_id_derechohabiente
                                AND situacion IN(50,55)) THEN
                  LET v_cod_rch_cons = 7; ---NSS no tiene crédito apoyo infonavit
               ELSE
                  -- Recupera Entidad financiera del registro en trámite
                  FOREACH
                     SELECT FIRST 1 cve_ent_financiera
                       INTO v_cve_ent_fin_reg
                       FROM ocg_tramite
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND situacion IN(50,55)
                      ORDER BY id_ocg_tramite DESC
                  END FOREACH;

                  IF (v_cve_ent_fin_reg <> p_cve_ent_financiera) AND p_cve_ent_financiera <> 0 THEN
                     LET v_cod_rch_cons = 8; ---NSS no tiene crédito apoyo infonavit con EF de usuario
                  ELSE
                     LET v_bnd_consulta = 1;
                  END IF -- Valida Entidad financiera para los registros en trámite
               END IF
            ELSE
               FOREACH
                  SELECT FIRST 1 f.situacion, f.cve_ent_financiera, a.f_formalizacion
                    INTO v_situacion, v_cve_ent_fin_reg, v_f_formalizacion
                    FROM ocg_formalizacion f, ocg_acreditado a
                   WHERE f.id_derechohabiente = v_id_derechohabiente
                     AND f.id_ocg_formalizacion = a.id_ocg_formalizacion
                     AND f.situacion IN(60,70,80,300,140,150,160)
                  ORDER BY a.f_formalizacion DESC
               END FOREACH;

               IF (v_cve_ent_fin_reg <> p_cve_ent_financiera) AND p_cve_ent_financiera <> 0 THEN
                  LET v_cod_rch_cons = 8; ---NSS no tiene crédito apoyo infonavit con EF de usuario
               ELSE
                  IF v_situacion = 300 THEN
                     LET v_cod_rch_cons = 9; ---NSS unificado
                     LET v_ax_error     = v_cod_rch_cons; ---Resultado unificado

                     EXECUTE FUNCTION fn_busca_nss_unificador(p_nss)
                                 INTO v_nss_unificador, v_id_dh_unificador, v_diag_uni;

                     -- Si tiene unificación
                     IF v_diag_uni = 1 THEN
                        LET v_nss_orig     = p_nss;
                        LET p_nss          = v_nss_unificador;
                        LET v_bnd_consulta = 9; ---Búsqueda NSS unificador
                     ELSE
                        IF NOT EXISTS(SELECT cve_ent_financiera
                                        FROM ocg_tramite
                                       WHERE id_derechohabiente = v_id_derechohabiente
                                         AND situacion IN(50, 55)) THEN
                           LET v_cod_rch_cons = 7; ---NSS no tiene crédito apoyo infonavit
                        ELSE
                           -- Recupera Entidad financiera del registro en trámite
                           FOREACH
                              SELECT FIRST 1 cve_ent_financiera
                                INTO v_cve_ent_fin_reg
                                FROM ocg_tramite
                               WHERE id_derechohabiente = v_id_derechohabiente
                                 AND situacion IN(50,55)
                               ORDER BY id_ocg_tramite DESC
                           END FOREACH;

                           IF (v_cve_ent_fin_reg <> p_cve_ent_financiera) AND p_cve_ent_financiera <> 0 THEN
                              LET v_cod_rch_cons = 8; ---NSS no tiene crédito apoyo infonavit con EF de usuario
                           ELSE
                              LET v_cod_rch_cons = 101; ---NSS tiene crédito apoyo infonavit en trámite
                           END IF
                        END IF
                     END IF
                  ELIF v_situacion >= 140 AND v_situacion <= 160 THEN
                     LET v_cod_rch_cons = 102; ---NSS tiene crédito apoyo infonavit liquidado
                  ELSE
                     IF v_situacion < 60 THEN
                        IF NOT EXISTS(SELECT cve_ent_financiera
                                        FROM ocg_tramite
                                       WHERE id_derechohabiente = v_id_derechohabiente
                                         AND situacion IN(50, 55)) THEN
                           LET v_cod_rch_cons = 7; ---NSS no tiene crédito apoyo infonavit
                        ELSE
                           -- Recupera Entidad financiera del registro en trámite
                           FOREACH
                              SELECT FIRST 1 cve_ent_financiera
                                INTO v_cve_ent_fin_reg
                                FROM ocg_tramite
                               WHERE id_derechohabiente = v_id_derechohabiente
                                 AND situacion IN(50,55)
                               ORDER BY id_ocg_tramite DESC
                           END FOREACH;

                           IF (v_cve_ent_fin_reg <> p_cve_ent_financiera) AND p_cve_ent_financiera <> 0 THEN
                              LET v_cod_rch_cons = 8; ---NSS no tiene crédito apoyo infonavit con EF de usuario
                           ELSE
                              LET v_cod_rch_cons = 101; ---NSS tiene crédito apoyo infonavit en trámite
                           END IF -- Valida EF del trémite
                        END IF
                     ELSE
                        LET v_bnd_consulta = 1; ---Búsqueda NSS
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
   END IF 

   IF v_bnd_consulta > 0 THEN
      IF NOT EXISTS (SELECT rfc
                       FROM cat_usuario_ef
                      WHERE rfc = p_rfc_usuario) THEN
         LET v_cod_rch_cons = 1; ---Usuario no está registrado
      ELSE
         IF NOT EXISTS (SELECT rfc
                          FROM cat_usuario_ef
                         WHERE rfc = p_rfc_usuario
                           AND estado = 10) THEN
            LET v_cod_rch_cons = 2; ---Usuario está dado de baja
         ELSE
            IF NOT EXISTS (SELECT rfc
                             FROM cat_usuario_ef
                            WHERE rfc = p_rfc_usuario
                              AND estado = 10
                              AND cve_ent_financiera = p_cve_ent_financiera) THEN
               LET v_cod_rch_cons = 3; ---Usuario no está registrado en EF
            ELSE
               IF p_cve_ent_financiera = 0 THEN
                  IF NOT EXISTS (SELECT e.cve_ent_financiera
                                   FROM cat_entidad_financiera e
                                  WHERE e.cve_ent_financiera = v_cve_ent_fin_reg
                                    AND e.estado_ef = 10) THEN
                     LET v_cod_rch_cons = 4; ---Entidad financiera no activa
                  END IF
               ELSE 
                  IF NOT EXISTS (SELECT e.cve_ent_financiera
                                   FROM cat_usuario_ef c, cat_entidad_financiera e
                                  WHERE c.rfc = p_rfc_usuario
                                    AND c.estado = 10
                                    AND c.cve_ent_financiera = p_cve_ent_financiera
                                    AND c.cve_ent_financiera = e.cve_ent_financiera
                                    AND e.estado_ef = 10) THEN
                     LET v_cod_rch_cons = 4; ---Entidad financiera no activa
                  END IF
               END IF

               IF v_cod_rch_cons <> 4 THEN
                  IF p_nss IS NULL OR p_nss = "" THEN
                     LET v_cod_rch_cons = 5; ---NSS no existe en la base de derechohabientes
                  ELSE
                     IF NOT EXISTS (SELECT nss
                                      FROM afi_derechohabiente
                                     WHERE nss = p_nss) THEN
                        LET v_cod_rch_cons = 6; ---NSS no existe en la base de derechohabientes
                     ELSE
                        SELECT id_derechohabiente,
                               ap_paterno_af,
                               ap_materno_af,
                               nombre_af,
                               curp,
                               rfc
                          INTO v_id_derechohabiente,
                               v_paterno,
                               v_materno,
                               v_nombre,
                               v_curp,
                               v_rfc_dh
                          FROM afi_derechohabiente
                         WHERE nss = p_nss;

                        LET v_nombre_dh =TRIM(v_paterno)||" "||TRIM(v_materno)||" "||TRIM(v_nombre);

                        IF EXISTS (SELECT estado
                                     FROM afi_fallecido
                                    WHERE id_derechohabiente = v_id_derechohabiente
                                      AND estado = 10) THEN
                           LET v_cod_rch_cons = 10;  ---Acreditado fallecido
                        ELSE
                           IF NOT EXISTS (SELECT c.id_derechohabiente
                                            FROM cre_acreditado c, cat_maq_credito m
                                           WHERE c.id_derechohabiente = v_id_derechohabiente
                                             AND c.tpo_credito = 2
                                             AND c.estado = m.estado
                                             AND m.entidad = 1) THEN
                              IF NOT EXISTS (SELECT c.id_derechohabiente
                                            FROM cre_acreditado c, cat_maq_credito m
                                           WHERE c.id_derechohabiente = v_id_derechohabiente
                                             AND c.tpo_credito = 2
                                             AND c.estado = m.estado
                                             AND m.entidad = 2) THEN
                                 IF EXISTS (SELECT situacion
                                              FROM ocg_tramite
                                             WHERE id_derechohabiente = v_id_derechohabiente
                                               AND cve_ent_financiera = p_cve_ent_financiera
                                               AND situacion IN(50,55)) THEN
                                    LET v_cod_rch_cons = 101; ---NSS tiene crédito apoyo infonavit en trámite
                                 ELSE
                                    LET v_cod_rch_cons = 7; ---NSS no tiene crédito apoyo infonavit
                                 END IF
                              ELSE
                                 IF EXISTS (SELECT situacion
                                              FROM ocg_tramite
                                             WHERE id_derechohabiente = v_id_derechohabiente
                                               AND cve_ent_financiera = p_cve_ent_financiera
                                               AND situacion IN(50,55)) THEN ---NSS tiene crédito en trámite
                                    LET v_cod_rch_cons = 101; ---NSS tiene crédito apoyo infonavit en trámite
                                 ELSE
                                    LET v_cod_rch_cons = 102; ---NSS tiene crédito apoyo infonavit liquidado
                                 END IF
                              END IF
                           ELSE
                              SELECT COUNT(*)
                                INTO v_rl
                                FROM afi_relacion_laboral
                               WHERE id_derechohabiente = v_id_derechohabiente;

                              IF v_rl IS NULL OR v_rl = "" THEN
                                 LET v_rl = 0;
                              END IF

                              IF NOT EXISTS(SELECT precio_fondo
                                              FROM glo_valor_fondo
                                             WHERE f_valuacion = v_fecha_valua
                                               AND fondo = 11) THEN

                                 LET v_cod_rch_cons = -3; ---NSS tiene crédito apoyo infonavit vigente
                              ELSE
                                 SELECT SUM(monto_acciones)
                                   INTO v_acc
                                   FROM cta_movimiento
                                  WHERE id_derechohabiente = v_id_derechohabiente
                                    AND subcuenta = 4
                                    AND fondo_inversion = 11;

                                 IF v_acc IS NULL OR v_acc = "" THEN
                                    LET v_acc = 0;
                                 END IF

                                 SELECT precio_fondo
                                   INTO v_valor_fondo
                                   FROM glo_valor_fondo
                                  WHERE f_valuacion = v_fecha_valua
                                    AND fondo = 11;

                                 LET v_sdo = v_acc * v_valor_fondo;
                                 LET v_cod_rch_cons = 100; ---NSS tiene crédito apoyo infonavit vigente
                              END IF
                           END IF  ---valida crédito en RF
                        END IF  --- valida nss fallecido
                     END IF  ---valida nss en base
                  END IF  ---valida nss no nulo
               END IF  ---valida usuario activo ef activa
            END IF  ---valida usuario_activo ef
         END IF  ---valida usuario activo
      END IF  ---valida usuario
   END IF  ---fin bandera

   ---IF v_cod_rch_cons > 0 THEN
      SELECT rch_desc
        INTO v_desc_resp_cons
        FROM cat_rechazo_usr_ef
       WHERE rch_cod = v_cod_rch_cons;
   ---END IF

   LET v_f_fin  = CURRENT;
   LET v_status = v_cod_rch_cons;

   INSERT INTO cre_log_cons_usr_ef
   VALUES (v_rfc   ,
           v_cve_ef,
           v_nss   ,
           v_f_ini ,
           v_f_fin ,
           v_status);

   LET v_desc_resp_cons = TRIM(v_desc_resp_cons);

   RETURN v_ax_error, v_cod_rch_cons, v_desc_resp_cons, v_nombre_dh, v_curp, v_rfc_dh, v_fallecido, v_rl, v_sdo, v_nss_unificador;

END FUNCTION;


