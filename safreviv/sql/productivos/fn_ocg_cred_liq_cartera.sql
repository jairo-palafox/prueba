






CREATE FUNCTION "safreviv".fn_ocg_cred_liq_cartera(p_usuario CHAR(20),
                                        p_nom_archivo CHAR(40))

   RETURNING SMALLINT

  -- DEFINE p_usuario             CHAR(20);
   DEFINE v_error               SMALLINT;
   DEFINE v_nss                 char(11);
   DEFINE v_num_credito         decimal(10,0);
   DEFINE v_estado_cta          char(4);
   DEFINE v_f_liquidacion       char(8);
   DEFINE v_producto            char(4);
   DEFINE v_cve_ent_financiera  smallint;
   DEFINE v_marca_conyuge       char(1);
   DEFINE v_nss_conyuge         char(11);
   DEFINE v_num_credito_conyuge decimal(10,0);
   DEFINE v_producto_conyuge    char(4);
   DEFINE v_rfc_conyuge         char(15);
   DEFINE bnd_cred_liq          smallint;
   DEFINE v_id_ocg_detalle      DECIMAL(9,0);
   DEFINE v_id_ocg_tramite      DECIMAL(9,0);
   DEFINE v_rfc                 CHAR(13);
   DEFINE v_curp                CHAR(18);
   DEFINE v_ap_paterno_af       CHAR(40);
   DEFINE v_ap_materno_af       CHAR(40);
   DEFINE v_nombre_af           CHAR(40);
   DEFINE v_tpo_cred_cartera    SMALLINT;
   DEFINE v_tpo_credito         SMALLINT;
   DEFINE v_cta_cre_vig         INTEGER;
   DEFINE v_id_derechohabiente  DECIMAL(9,0);
   DEFINE v_id_cre_acreditado   DECIMAL(9,0);
   DEFINE v_tpo_originacion     smallint;
   DEFINE v_num_credito_cre     decimal(10,0);
   DEFINE v_estado              smallint;
   DEFINE v_edo_procesar        smallint;
   DEFINE v_precio_fondo        DECIMAL(19,14);
   DEFINE v_sdo_pesos           DECIMAL(12,2);
   DEFINE a                     INTEGER;
   DEFINE v_diagnostico         SMALLINT;
   DEFINE v_cta_rch             INTEGER;
   DEFINE v_cta_his             INTEGER;
   DEFINE v_cta_tramite         INTEGER;
   DEFINE bnd_cred_ap_vig       SMALLINT;
   DEFINE v_cta_liq             INTEGER;
   DEFINE bnd_cred_vig          SMALLINT;
 --  DEFINE v_cta_tramite         INTEGER;
   DEFINE v_entidad             CHAR(3);
   DEFINE v_ax_cod_error        SMALLINT;
   DEFINE v_ax_cod_error_2      SMALLINT;
   DEFINE v_situacion           SMALLINT;
   DEFINE v_id_ocg_liq_cofi     DECIMAL(9,0);
   DEFINE v_r_nss               CHAR(11);
   DEFINE v_r_id_dh             DECIMAL(9,0);
   DEFINE v_r_marca             SMALLINT;
   DEFINE v_r_resp              CHAR(1);
   DEFINE v_r_ftramite          DATE;
   DEFINE v_resultado           SMALLINT;
   DEFINE v_inconsistencia      SMALLINT;
   DEFINE v_rechazo_cod         SMALLINT;
   DEFINE bnd_cta_reg           SMALLINT;
   DEFINE v_cta_pub             INTEGER;
   DEFINE v_cta_acre            INTEGER;
   DEFINE v_length_nss          SMALLINT;
   DEFINE v_length_cre          SMALLINT;
   DEFINE v_length_ef           SMALLINT;
   DEFINE v_marca_unifica       SMALLINT;
   DEFINE v_rch_marca           SMALLINT;
   DEFINE v_id_ocg_ctr_archivo  DECIMAL(9,0);
   DEFINE v_f_liquidacion2      DATE;
   DEFINE v_cnt_cre             SMALLINT;
   DEFINE v_ef                  char(3);
   DEFINE v_nss_c               char(11);
   DEFINE v_ef_origen           char(3);
   DEFINE v_ef_destino          char(3);
   DEFINE v_formato_fecha       CHAR(8);
   DEFINE v_fecha_si            SMALLINT;
   DEFINE v_entidad_maq         SMALLINT;
   DEFINE v_ent_cre             SMALLINT;
   DEFINE v_resultado_vig       SMALLINT;
   DEFINE v_tpo_org             SMALLINT;
   DEFINE v_tpo_cre             SMALLINT;
   DEFINE v_num_cre             DECIMAL(10,0);
   DEFINE v_f_otorga            DATE;
   DEFINE v_f_liquida           DATE;
   DEFINE v_tpo_desc            SMALLINT;


   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_cred_liq_cartera.trace';
   TRACE ON;

   LET v_error         = 0 ;
   LET v_formato_fecha = "yyyymmdd";
   LET v_fecha_si      = 1;
   LET v_id_ocg_ctr_archivo = seq_ocg_archivo.nextval;

   INSERT INTO ocg_ctr_archivo
        VALUES(v_id_ocg_ctr_archivo,
               0,
               TODAY,
               3913,
               2,
               p_nom_archivo,
               0,
               0,
               0,
               0,
               0,
               0,
               1,
               today,
               p_usuario );

   FOREACH
      SELECT nss,
             num_credito,
             estado_cta,
             f_liquidacion,
             producto,
             cve_ent_financiera,
             marca_conyuge,
             nss_conyuge[5,15],
             num_credito_conyuge,
             producto_conyuge,
             rfc_conyuge
        INTO v_nss,
             v_num_credito,
             v_estado_cta,
             v_f_liquidacion,
             v_producto,
             v_cve_ent_financiera,
             v_marca_conyuge,
             v_nss_conyuge,
             v_num_credito_conyuge,
             v_producto_conyuge,
             v_rfc_conyuge
        FROM safre_tmp:tmp_cred_liq_cartera

      LET v_length_nss = 1;
      LET v_length_cre = 1;
      LET v_length_ef  = 1;

      IF v_nss_conyuge = "00000000000" THEN
         LET v_nss_conyuge = NULL;
      END IF

      IF NOT EXISTS (select nss
                       from afi_derechohabiente
                      where nss = v_nss_conyuge) THEN
         LET v_nss_conyuge = NULL;
      ELSE
        LET v_nss_conyuge = v_nss_conyuge;
      END IF

      FOR v_length_nss = 1 TO LENGTH(v_nss)
         --TRACE 'NSS :'||SUBSTR(v_nss,v_length_nss,1);
         IF SUBSTR(v_nss,v_length_nss,1) NOT MATCHES '[0-9]' THEN
            INSERT INTO ocg_liq_cofi_rechazo
                 VALUES ( v_nss,
                          v_num_credito,
                          v_estado_cta,
                          v_f_liquidacion2,
                          v_producto,
                          v_cve_ent_financiera,
                          v_marca_conyuge,
                          v_nss_conyuge,
                          v_num_credito_conyuge,
                          v_producto_conyuge,
                          v_rfc_conyuge,
                          TODAY);
           -- EXIT FOR;
            CONTINUE FOREACH;
            EXIT FOR;
         END IF
      END FOR;

      EXECUTE FUNCTION fn_verifica_fecha(v_f_liquidacion, v_formato_fecha)
                                   INTO v_fecha_si, v_f_liquidacion2;

      IF v_fecha_si = 0 THEN
         INSERT INTO ocg_liq_cofi_rechazo
              VALUES ( v_nss,
                       v_num_credito,
                       v_estado_cta,
                       v_f_liquidacion,
                       v_producto,
                       v_cve_ent_financiera,
                       v_marca_conyuge,
                       v_nss_conyuge,
                       v_num_credito_conyuge,
                       v_producto_conyuge,
                       v_rfc_conyuge,
                       TODAY);

         CONTINUE FOREACH;
      END IF

      LET v_ef_destino = NULL;
      LET v_ef_origen  = NULL;
      LET v_nss_c      = NULL;
      LET v_resultado_vig       = "";
      LET v_tpo_org             = "";
      LET v_tpo_cre             = "";
      LET v_num_cre             = "";
      LET v_f_otorga            = "";
      LET v_f_liquida           = "";
      LET v_tpo_desc            = "";


      SELECT nss,
             ef_origen,
             ef_destino
        INTO v_nss_c,
             v_ef_origen,
             v_ef_destino
        FROM ocg_cambio_ef
       WHERE nss       = v_nss
         AND ef_origen = v_cve_ent_financiera;

      IF v_ef_destino IS NOT NULL THEN
         LET v_cve_ent_financiera = v_ef_destino;
      END IF

      TRACE 'entidad financiera'||v_cve_ent_financiera;

      IF v_producto = "IL3C" THEN
         LET v_tpo_cred_cartera = 7;
      END IF

      IF v_producto = "IL4C" THEN
         LET v_tpo_cred_cartera = 8;
      END IF

      IF v_producto = "CF03" THEN
         LET v_tpo_cred_cartera = 8;
      END IF

      IF v_producto_conyuge IS NULL THEN
         LET v_producto_conyuge = 0;
      END IF

      LET v_entidad = v_cve_ent_financiera;

      IF (length (v_entidad) = 2 ) AND (v_entidad <= "99") THEN
         LET v_entidad = "0"||v_entidad;
      END IF

      IF (v_producto = "IL3C") OR
         (v_producto = "IL4C") THEN

         IF v_entidad matches "*12" THEN
            LET v_cve_ent_financiera = 012;
         END IF

         IF v_entidad matches "*02" THEN
            LET v_cve_ent_financiera = 002;
         END IF
      ELSE
         IF v_producto = "CF03" THEN
            IF v_entidad matches "*12" THEN
               LET v_cve_ent_financiera = 312;
            END IF

            IF v_entidad matches "*02" THEN
               LET v_cve_ent_financiera = 302;
            END IF
         ENd IF
      END IF

      IF (v_entidad >= "002") AND
         (v_entidad <= "199") THEN
         IF v_producto = "CF03" THEN
            IF v_entidad not matches "3*" THEN
               LET v_ef = "3"||v_entidad[2,3];
               LET v_cve_ent_financiera = v_ef;
            END IF
         END IF
      END IF

       IF (v_entidad >= "600") AND
         (v_entidad <= "699") THEN
         IF v_producto = "CF03" THEN
            IF v_entidad not matches "7*" THEN
               LET v_ef = "7"||v_entidad[2,3];
               LET v_cve_ent_financiera = v_ef;
            END IF
         END IF
      END IF

      IF (v_entidad >= "900") AND
         (v_entidad <= "999") THEN
         IF v_producto = "CF03" THEN
            IF v_entidad not matches "8*" THEN
               LET v_ef = "8"||v_entidad[2,3] ;
               LET v_cve_ent_financiera = v_ef;
            END IF
         END IF
      END IF

     TRACE 'entidad financiera'||v_cve_ent_financiera;

      IF v_marca_conyuge = "1" THEN
         IF v_nss_conyuge IS NULL THEN
            LET v_marca_conyuge = "I";
         ELSE
             LET v_marca_conyuge = "C";
         END IF
      ELSE
         IF v_marca_conyuge = "0" THEN
            IF v_nss_conyuge IS NULL THEN
               LET v_marca_conyuge = "I";
            ELSE
               LET v_marca_conyuge = "C";
            END IF
         END IF
      END IF

      LET bnd_cred_ap_vig       = 0;
      LET bnd_cred_vig          = 0;
      LET v_cta_liq             = 0;
      LET v_cta_cre_vig         = 0;
      LET v_num_credito_cre     = 0;
      LET v_id_cre_acreditado   = 0;
      LET v_tpo_originacion     = 0;
      LET v_tpo_credito         = 0;
      LET v_estado              = 0;
      LET v_edo_procesar        = 0;
      LET v_diagnostico         = 0;
      LET v_situacion           = 0;
      LET bnd_cta_reg           = 0;
      LET bnd_cred_liq          = 0;

      SELECT id_derechohabiente,
             rfc,
             curp,
             ap_paterno_af,
             ap_materno_af,
             nombre_af
        INTO v_id_derechohabiente,
             v_rfc,
             v_curp,
             v_ap_paterno_af,
             v_ap_materno_af,
             v_nombre_af
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      LET a             = 1;
      LET v_diagnostico = 0;
      LET v_cta_rch     = 0;
      LET v_cta_his     = 0;
      LET v_cta_tramite = 0;

      LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      LET v_id_ocg_tramite     = seq_ocg_tramite.nextval;
      LET v_sdo_pesos          = 0;
      LET v_id_ocg_liq_cofi    = seq_ocg_liq_cofi.nextval;

      -- Se valida si tiene unificaci¦n
      LET v_marca_unifica      = 150;
      EXECUTE FUNCTION fn_verifica_marca_43(v_nss,v_marca_unifica,v_id_derechohabiente)
      INTO v_r_nss,v_r_resp,v_r_ftramite,v_r_marca,v_r_id_dh;

      -- Si tiene marca 150
      IF v_r_resp = 1 THEN
         EXECUTE FUNCTION fn_busca_nss_unificador(v_nss)
                     INTO v_r_nss, v_r_id_dh, v_resultado;

      -- Si tiene unificaci¦n
         IF v_resultado = 1 THEN
            LET v_nss                = v_r_nss;
            LET v_id_derechohabiente = v_r_id_dh;
{
         -- Se inserta la inconsistencia, pero no se rechaza
            LET v_rechazo_cod = "03";
            INSERT INTO ocg_rechazo_cartera
                 VALUES( v_id_ocg_liq_cofi,
                         v_rechazo_cod,
                         TODAY );

             let v_diagnostico = 1;
            insert into safre_tmp:tmp_cred_liq_rch
                 VALUES ( v_id_ocg_liq_cofi,
                          v_nss,
                          v_num_credito_cre,
                          v_estado_cta,
                          v_producto,
                          v_cve_ent_financiera,
                          v_nss_conyuge,
                          v_num_credito_conyuge,
                          v_producto_conyuge,
                          v_diagnostico);
}
         END IF;
      END IF

      -- se verifica si es un cr¦dito tipo 7 u 8 del que ya exista un tramite aceptado o un cr¦dito vigente

      SELECT count(*)
        INTO v_cta_tramite
        FROM ocg_tramite
       WHERE id_derechohabiente = v_id_derechohabiente
         AND tpo_credito in (7,8)
         AND situacion in (25,30);

      SELECT count(*)
        INTO v_cta_pub
        FROM ocg_tramite
       WHERE id_derechohabiente = v_id_derechohabiente
         AND tpo_credito in (7,8)
         AND situacion = 50;

      SELECT count(*)
        INTO v_cta_acre
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_derechohabiente
         AND tpo_credito in (7,8,"A","C")
         AND situacion >= 55
         AND situacion < 140;

      SELECT count(*)
        INTO v_cta_liq
        FROM ocg_liquidacion
       WHERE id_derechohabiente = v_id_derechohabiente
         AND tpo_credito in (7,8)
         AND situacion >= 140;
         ---AND situacion not in (153,155);

      --LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      --LET v_id_ocg_tramite     = seq_ocg_tramite.nextval;
      --LET v_sdo_pesos          = 0;
      --LET v_id_ocg_liq_cofi    = seq_ocg_liq_cofi.nextval;

      IF (v_cta_tramite >= 1) OR
         (v_cta_pub     >= 1) OR
         (v_cta_acre    >= 1) OR
         (v_cta_liq     >= 1) THEN
         --LET  v_cta_liq = 0;

         --TRACE "id_derechohabiente :"||v_id_derechohabiente;
         --TRACE "nss                :"||v_nss;
         --TRACE "v_num_credito      :"||v_num_credito;
        -- TRACE "v_estado_cta       :"||v_estado_cta;
         --TRACE "v_producto         :"||v_producto;
         --TRACE "v_cve_ent_financiera:"||v_cve_ent_financiera;
         --TRACE "v_nss_conyuge      :"||v_nss_conyuge;
         --TRACE "v_num_credito_conyuge :"||v_num_credito_conyuge;
         --TRACE "v_producto_conyuge :"||v_producto_conyuge;
         --TRACE "diagnostico        :"||v_diagnostico;

     --    LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
     --    LET v_id_ocg_tramite     = seq_ocg_tramite.nextval;
     --    LET v_sdo_pesos = 0;
     --    LET v_id_ocg_liq_cofi    = seq_ocg_liq_cofi.nextval;
     --    LET v_id_ocg_ctr_archivo = seq_ocg_archivo.nextval;
{
         INSERT INTO ocg_detalle
             VALUES( v_id_ocg_detalle,
                     v_id_ocg_ctr_archivo,
                     v_id_derechohabiente,
                     "1",
                     TODAY,
                     v_cve_ent_financiera,
                    v_nss );

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_id_derechohabiente,
                     TODAY,
                     TODAY,
                     '',
                     '',
                     1,
                     TODAY);

         INSERT INTO ocg_tramite
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_cve_ent_financiera,
                     v_id_derechohabiente,
                     v_rfc,
                     v_curp,
                     v_ap_paterno_af,
                     v_ap_materno_af,
                     v_nombre_af,
                     "0",
                     v_sdo_pesos,
                     TODAY,
                     v_tpo_cred_cartera,
                     "",
                     "",
                     "2",
                     "60",
                     "40" );
}
         IF v_cta_tramite >= 1 THEN
            LET v_rechazo_cod = 4;
         END IF

         IF v_cta_pub >= 1 THEN
            LET v_rechazo_cod = 4;
         END IF

         IF v_cta_acre >= 1 THEN
            LET v_rechazo_cod = 5;
         END IF

         IF v_cta_liq >= 1 THEN
            LET v_rechazo_cod = 5;
         END IF

         INSERT INTO ocg_rechazo_cartera
              VALUES (v_id_ocg_liq_cofi,
                      v_rechazo_cod,
                      TODAY);

         SELECT COUNT(*)
           INTO v_cta_rch
           FROM safre_tmp:tmp_cred_liq_rch
          WHERE id_derechohabiente = v_id_derechohabiente;

         IF v_cta_rch = 0 THEN
            let v_diagnostico = 2;

            insert into safre_tmp:tmp_cred_liq_rch
                 VALUES ( v_id_ocg_liq_cofi,
                          v_nss,
                          v_num_credito,
                          v_estado_cta,
                          v_producto,
                          v_cve_ent_financiera,
                          v_nss_conyuge,
                          v_num_credito_conyuge,
                          v_producto_conyuge,
                          v_diagnostico);
         END IF

                  SELECT COUNT(*)
                    INTO v_cta_his
                    FROM ocg_liquidacion_cofi
                   WHERE id_derechohabiente = v_id_derechohabiente;

                  LET v_estado    = 60 ;
                  LET v_situacion = 190;
                  LET v_diagnostico = 2;

                  IF v_cta_his = 0 THEN

                     INSERT INTO ocg_liquidacion_cofi
                          VALUES (v_id_ocg_liq_cofi,
                                  v_id_derechohabiente,
                                  v_nss,
                                  v_num_credito,
                                  v_estado_cta,
                                  v_f_liquidacion2,
                                  v_producto,
                                  v_cve_ent_financiera,
                                  v_marca_conyuge,
                                  v_nss_conyuge,
                                  v_num_credito_conyuge,
                                  v_producto_conyuge,
                                  v_rfc_conyuge,
                                  v_diagnostico,
                                  v_estado,
                                  v_situacion,
                                  TODAY,
                                  p_usuario );

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  ELSE
                     UPDATE ocg_liquidacion_cofi
                        SET id_ocg_liq_cofi = v_id_ocg_liq_cofi,
                            num_credito = v_num_credito,
                            f_proceso   = TODAY,
                            estado      = v_estado,
                            situacion   = v_situacion,
                            f_liquidacion = v_f_liquidacion2
                      WHERE id_derechohabiente = v_id_derechohabiente;

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  END IF
         --  END IF
      ELSE

        FOREACH
             SELECT count(*)
               INTO v_cnt_cre
               FROM cre_acreditado
              WHERE id_derechohabiente = v_id_derechohabiente
                AND num_credito = v_num_credito

             IF (v_cnt_cre is null) OR
                (v_cnt_cre = 0 ) THEN
             ELSE
                --nueva validación para verificar crédito vigente 23/05/2017

                EXECUTE FUNCTION fn_edo_cred_viv (v_id_derechohabiente,1)
                            INTO v_resultado_vig,
                                 v_tpo_org,
                                 v_tpo_cre,
                                 v_num_cre,
                                 v_f_otorga,
                                 v_f_liquida,
                                 v_tpo_desc;

                {
                IF v_cnt_cre >= 1 THEN
                  FOREACH
                   SELECT FIRST 1 c.id_cre_acreditado,
                          c.tpo_originacion,
                          c.tpo_credito,
                          c.num_credito,
                          c.estado,
                          c.edo_procesar,
                          m.entidad
                     INTO v_id_cre_acreditado,
                          v_tpo_originacion,
                          v_tpo_credito,
                          v_num_credito_cre,
                          v_estado,
                          v_edo_procesar,
                          v_ent_cre
                     FROM cre_acreditado c, cat_maq_credito m
                    WHERE c.id_derechohabiente = v_id_derechohabiente
                      AND c.num_credito = v_num_credito
                      AND m.entidad in (1,2,5)
                  ORDER BY c.f_otorga
                 END FOREACH
                ELSE
                  FOREACH
                   SELECT FIRST 1(id_cre_acreditado)
                     INTO v_id_cre_acreditado
                     FROM cre_acreditado
                    WHERE id_derechohabiente = v_id_derechohabiente
                      AND tpo_originacion = 4
                   END FOREACH

                   SELECT id_cre_acreditado,
                          tpo_originacion,
                          tpo_credito,
                          num_credito,
                          estado,
                          edo_procesar,
                          m.entidad
                     INTO v_id_cre_acreditado,
                          v_tpo_originacion,
                          v_tpo_credito,
                          v_num_credito_cre,
                          v_estado,
                          v_edo_procesar,
                          v_ent_cre
                     FROM cre_acreditado
                    WHERE id_cre_acreditado  = v_id_cre_acreditado
                      AND id_derechohabiente = v_id_derechohabiente
                      AND m.entidad in (1,2,5);
                END IF
                }
             END IF

            LET v_cnt_cre            = 0;
            LET a                    = a + 1;
            LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
            LET v_id_ocg_tramite     = seq_ocg_tramite.nextval;
            LET v_id_ocg_liq_cofi    = seq_ocg_liq_cofi.nextval;

            --TRACE "estado          :"||v_estado;
            --TRACE "estado procesar :"||v_edo_procesar;

            IF (v_resultado_vig   = 0) AND (v_tpo_cre = 2) THEN
               LET bnd_cred_liq  = 0; -- no tiene crédito liquidado
               LET v_diagnostico = 2;
                --EXECUTE FUNCTION fn_ocg_procesa_desmarca(v_id_cre_acreditado,1,p_usuario) INTO v_ax_cod_error;
            ELSE

            -- se busca si tiene un credito apoyo infonavit en tramite o pendiente de formalizar para cancelar el apoyo y otorgar el cofi
               SELECT count(*)
                 INTO v_cta_cre_vig
                 FROM ocg_tramite
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND diagnostico = 1
                  AND situacion IN (30,50)
                  AND tpo_credito in ("A","C");

               IF v_cta_cre_vig > 0 THEN
                  LET bnd_cred_ap_vig = 1;
                  LET bnd_cred_vig    = 0;
               END IF

            --   LET v_cta_liq     = 1; comentado 04/10/2016
              -- LET bnd_cred_liq  = 1;
              -- LET v_diagnostico = 1;

               --IF bnd_cred_ap_vig = 1 THEN
                 -- LET bnd_cred_liq = 1;
              -- ELSE
                --  IF (v_resultado_vig = 0) THEN
                  --   LET bnd_cred_liq  = 0; -- no tiene crédito liquidado
                    -- LET v_diagnostico  = 2;
                     --EXECUTE FUNCTION fn_ocg_procesa_desmarca(v_id_cre_acreditado,1,p_usuario) INTO v_ax_cod_error;
                 -- END IF
               --END IF

               LET bnd_cred_liq  = 1;
               LET v_diagnostico = 1;
            END IF
{
            -- se valida si estado es 170 y estado procesar es diferente de 210,se jecuta WS de consulta de marca
            IF (v_estado = 170) AND
               (v_edo_procesar <> 210) THEN
               insert into safre_tmp:tmp_nss_cred_liq values (v_id_cre_acreditado,
                                                              v_nss);
               LET bnd_cred_liq  = 0;
               LET v_diagnostico = 2;
            END IF

            IF (v_estado <> 170) AND
               (v_edo_procesar = 210) THEN
               LET bnd_cred_liq  = 0;
               LET v_diagnostico = 2;
              EXECUTE FUNCTION fn_ocg_procesa_desmarca(v_id_cre_acreditado,0,p_usuario) INTO v_ax_cod_error_2;
            END IF
}
            --IF a = 2 THEN
            IF bnd_cta_reg < 1 THEN
               SELECT precio_fondo
                 INTO v_precio_fondo
                 FROM glo_valor_fondo
                WHERE f_valuacion = today
                  AND fondo = 11;

               SELECT ROUND((sum(monto_acciones*v_precio_fondo)),2)
                 INTO v_sdo_pesos
                 FROM cta_movimiento
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND subcuenta = 4;

               --TRACE "bandera para insertar en ocg "||bnd_cred_liq;
               IF (bnd_cred_liq = 1) THEN
                  LET v_diagnostico = 1;

                  IF bnd_cred_ap_vig = 1 THEN
                     UPDATE ocg_tramite
                        SET situacion = 130
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND diagnostico = 1
                        AND situacion IN (30,50)
                        AND tpo_credito in ("A","C");

                      LET v_estado      = 70 ;
                      LET v_situacion   = 30 ;
                      LET v_diagnostico = 1  ;
{
                     LET v_rechazo_cod = 6;
                     INSERT INTO ocg_rechazo_cartera
                          VALUES(v_id_ocg_liq_cofi,
                                 v_rechazo_cod,
                                 TODAY);

                     SELECT COUNT(*)
                       INTO v_cta_rch
                       FROM safre_tmp:tmp_cred_liq_rch
                      WHERE id_derechohabiente = v_id_derechohabiente;

                     IF v_cta_rch = 0 THEN

                        insert into safre_tmp:tmp_cred_liq_rch
                             VALUES ( v_id_ocg_liq_cofi,
                                      v_nss,
                                      v_num_credito_cre,
                                      v_estado_cta,
                                      v_producto,
                                      v_cve_ent_financiera,
                                      v_nss_conyuge,
                                      v_num_credito_conyuge,
                                      v_producto_conyuge,
                                      v_diagnostico);
                     END IF
}
                   --   WHERE id_derechohabiente = v_id_derechohabiente;
                  END IF
                   --TRACE "id_detalle "||v_id_ocg_detalle;
                  {
                  -- Se valida si tiene unificaci¦n
                  LET v_marca_unifica      = 150;
                  EXECUTE FUNCTION fn_verifica_marca_43(v_nss,v_marca_unifica,v_id_derechohabiente)
                  INTO v_r_nss,v_r_resp,v_r_ftramite,v_r_marca,v_r_id_dh;

                  -- Si tiene marca 150
                  IF v_r_resp = 1 THEN
                     EXECUTE FUNCTION fn_busca_nss_unificador(v_nss)
                                 INTO v_r_nss, v_r_id_dh, v_resultado;

                  -- Si tiene unificaci¦n
                     IF v_resultado = 1 THEN
                        LET v_nss                = v_r_nss;
                        LET v_id_derechohabiente = v_r_id_dh;

                     -- Se inserta la inconsistencia, pero no se rechaza
                        LET v_rechazo_cod = "03";
                        INSERT INTO ocg_rechazo_cartera
                             VALUES( v_id_ocg_liq_cofi,
                                     v_rechazo_cod,
                                     TODAY );

                         let v_diagnostico = 1;
                     insert into safre_tmp:tmp_cred_liq_rch
                          VALUES ( v_id_ocg_liq_cofi,
                                   v_nss,
                                   v_num_credito_cre,
                                   v_estado_cta,
                                   v_producto,
                                   v_cve_ent_financiera,
                                   v_nss_conyuge,
                                   v_num_credito_conyuge,
                                   v_producto_conyuge,
                                   v_diagnostico);
                     END IF;
                  END IF

                  }
                  INSERT INTO ocg_detalle
                      VALUES( v_id_ocg_detalle,
                              v_id_ocg_ctr_archivo,
                              v_id_derechohabiente,
                              "1",
                              TODAY,
                              v_cve_ent_financiera,
                              v_nss );

                  INSERT INTO ocg_fecha_mig
                       VALUES(v_id_ocg_tramite,
                              v_id_ocg_detalle,
                              v_id_derechohabiente,
                              TODAY,
                              TODAY,
                              '',
                              '',
                              1,
                              TODAY);


                  INSERT INTO ocg_tramite
                       VALUES(v_id_ocg_tramite,
                              v_id_ocg_detalle,
                              v_cve_ent_financiera,
                              v_id_derechohabiente,
                              v_rfc,
                              v_curp,
                              v_ap_paterno_af,
                              v_ap_materno_af,
                              v_nombre_af,
                              "0",
                              v_sdo_pesos,
                              TODAY,
                              v_tpo_cred_cartera,
                              "",
                              "",
                              "1",
                              "70",
                              "30" );

                    -- Se invoca a la funci¦n que marca la cuenta
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                     206,
                                                     v_id_ocg_tramite,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     "safreviv",
                                                     3902) INTO v_rch_marca;

                   LET v_estado      = 70 ;
                   LET v_situacion   = 30 ;
                   LET v_diagnostico = 1  ;


                  SELECT COUNT(*)
                    INTO v_cta_his
                    FROM ocg_liquidacion_cofi
                   WHERE id_derechohabiente = v_id_derechohabiente;

                  IF v_cta_his = 0 THEN

                     INSERT INTO ocg_liquidacion_cofi
                          VALUES (v_id_ocg_liq_cofi,
                                  v_id_derechohabiente,
                                  v_nss,
                                  v_num_credito,
                                  v_estado_cta,
                                  v_f_liquidacion2,
                                  v_producto,
                                  v_cve_ent_financiera,
                                  v_marca_conyuge,
                                  v_nss_conyuge,
                                  v_num_credito_conyuge,
                                  v_producto_conyuge,
                                  v_rfc_conyuge,
                                  v_diagnostico,
                                  v_estado,
                                  v_situacion,
                                  TODAY,
                                  p_usuario );

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  ELSE
                     UPDATE ocg_liquidacion_cofi
                        SET id_ocg_liq_cofi = v_id_ocg_liq_cofi,
                            diagnostico = v_diagnostico,
                            num_credito = v_num_credito,
                            f_proceso   = TODAY,
                            estado      = v_estado,
                            situacion   = v_situacion,
                            f_liquidacion = v_f_liquidacion2
                      WHERE id_derechohabiente = v_id_derechohabiente;
                       -- AND num_credito = v_num_credito;

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  END IF
               ELSE

{
                   INSERT INTO ocg_detalle
                      VALUES( v_id_ocg_detalle,
                              v_id_ocg_ctr_archivo,
                              v_id_derechohabiente,
                              "1",
                              TODAY,
                              v_cve_ent_financiera,
                              v_nss );

                  INSERT INTO ocg_fecha_mig
                       VALUES(v_id_ocg_tramite,
                              v_id_ocg_detalle,
                              v_id_derechohabiente,
                              TODAY,
                              TODAY,
                              '',
                              '',
                              1,
                              TODAY);

                  INSERT INTO ocg_tramite
                       VALUES(v_id_ocg_tramite,
                              v_id_ocg_detalle,
                              v_cve_ent_financiera,
                              v_id_derechohabiente,
                              v_rfc,
                              v_curp,
                              v_ap_paterno_af,
                              v_ap_materno_af,
                              v_nombre_af,
                              "0",
                              v_sdo_pesos,
                              TODAY,
                              v_tpo_cred_cartera,
                              "",
                              "",
                              "2",
                              "60",
                              "40" );
}

                  SELECT COUNT(*)
                    INTO v_cta_rch
                    FROM safre_tmp:tmp_cred_liq_rch
                   WHERE id_derechohabiente = v_id_derechohabiente;

                  IF v_cta_rch = 0 THEN
                     let v_diagnostico = 2;
                     insert into safre_tmp:tmp_cred_liq_rch
                          VALUES ( v_id_ocg_liq_cofi,
                                   v_nss,
                                   v_num_credito_cre,
                                   v_estado_cta,
                                   v_producto,
                                   v_cve_ent_financiera,
                                   v_nss_conyuge,
                                   v_num_credito_conyuge,
                                   v_producto_conyuge,
                                   v_diagnostico);
                  END IF
                  LET v_rechazo_cod = 1;
                  INSERT INTO ocg_rechazo_cartera
                       VALUES (v_id_ocg_liq_cofi,
                               v_rechazo_cod,
                               TODAY);

                  SELECT COUNT(*)
                    INTO v_cta_his
                    FROM ocg_liquidacion_cofi
                   WHERE id_derechohabiente = v_id_derechohabiente;

                  LET v_estado    = 60 ;
                  LET v_situacion = 190;
                  LET v_diagnostico = 2;

                  IF v_cta_his = 0 THEN

                     INSERT INTO ocg_liquidacion_cofi
                          VALUES (v_id_ocg_liq_cofi,
                                  v_id_derechohabiente,
                                  v_nss,
                                  v_num_credito,
                                  v_estado_cta,
                                  v_f_liquidacion2,
                                  v_producto,
                                  v_cve_ent_financiera,
                                  v_marca_conyuge,
                                  v_nss_conyuge,
                                  v_num_credito_conyuge,
                                  v_producto_conyuge,
                                  v_rfc_conyuge,
                                  v_diagnostico,
                                  v_estado,
                                  v_situacion,
                                  TODAY,
                                  p_usuario );

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  ELSE
                     UPDATE ocg_liquidacion_cofi
                        SET id_ocg_liq_cofi = v_id_ocg_liq_cofi,
                            diagnostico = v_diagnostico,
                            num_credito = v_num_credito,
                            f_proceso   = TODAY,
                            estado      = v_estado,
                            situacion   = v_situacion,
                            f_liquidacion = v_f_liquidacion2
                      WHERE id_derechohabiente = v_id_derechohabiente;
                       -- AND num_credito = v_num_credito;

                     UPDATE ocg_fecha_mig
                        SET f_liquida_cofi    = v_f_liquidacion2
                      WHERE id_ocg_referencia = v_id_ocg_tramite
                        AND id_ocg_detalle    = v_id_ocg_detalle;

                  END IF
               END IF
               LET bnd_cta_reg = 1;
            END IF
            LET v_num_credito_cre     = NULL;
            LET v_id_cre_acreditado   = NULL;
            LET v_tpo_originacion     = NULL;
            LET v_tpo_credito         = NULL;
            LET v_estado              = NULL;
            LET v_edo_procesar        = NULL;

            LET v_nss_conyuge   = NULL;
            LET v_marca_conyuge = NULL;

            LET v_resultado_vig       = "";
            LET v_tpo_originacion     = "";
            LET v_tpo_cre             = "";
            LET v_num_cre             = "";
            LET v_f_otorga            = "";
            LET v_f_liquida           = "";
            LET v_tpo_desc            = "";
         END FOREACH;
      END IF

   END FOREACH;

   RETURN v_error;
END FUNCTION;


