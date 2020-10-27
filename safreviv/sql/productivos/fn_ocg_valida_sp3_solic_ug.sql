






CREATE FUNCTION "selefp".fn_ocg_valida_sp3_solic_ug(p_id_ocg_ctr_arch DECIMAL(9,0))

   RETURNING SMALLINT,INTEGER,INTEGER

   DEFINE v_tpo_trabajador          CHAR(1);
   DEFINE v_tpo_credito             CHAR(1);
   DEFINE v_inconsistencia          CHAR(2);
   DEFINE v_f_envio                 DATE;
   DEFINE v_f_venc_imp_solic        DATE;
   DEFINE v_nss                     CHAR(11);
   DEFINE v_num_ctrl_ef             CHAR(18);
   DEFINE v_ax_num_ctrl_ef          CHAR(18);
   DEFINE v_ax_f_envio              DATE;
   DEFINE v_f_proceso               DATE;
   DEFINE v_diagnostico             DECIMAL(2,0);
   DEFINE v_cve_ent_financiera      DECIMAL(3,0);
   DEFINE v_ax_cve_ent_financiera   DECIMAL(3,0);
   DEFINE v_viv97                   DECIMAL(8,0);
   DEFINE v_id_ocg_solic_ug         DECIMAL(9,0);
   DEFINE v_uni_id_dh               DECIMAL(9,0);
   DEFINE v_id_dh                   DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0);
   DEFINE v_id_ocg_detalle          DECIMAL(9,0);
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_ax_id_ocg_ctr_arch      DECIMAL(9,0);
   DEFINE v_imp_solic_uti_ocg       DECIMAL(13,2);
   DEFINE v_imp_solicitado          DECIMAL(13,2);
   DEFINE v_imp_utilizado_ocg       DECIMAL(13,2);
   DEFINE v_cnt_aceptados           INTEGER;
   DEFINE v_cnt_rechazados          INTEGER;
   DEFINE v_tot_regs                INTEGER;
   DEFINE v_cnt_cve_ef              SMALLINT;
   DEFINE v_solic_saldo             SMALLINT;
   DEFINE v_subproceso              SMALLINT;
   DEFINE v_cnt_vig                 SMALLINT;
   DEFINE v_ind_edo_cuenta          SMALLINT;
   DEFINE v_si_cre_vigente          SMALLINT;
   DEFINE v_cnt_cr_tram             SMALLINT;
   DEFINE v_idx_nss                 SMALLINT;
   DEFINE v_error                   SMALLINT;
   DEFINE v_bnd_inconsistencia      SMALLINT;
   DEFINE v_estado                  SMALLINT;         -- 0 aceptado , 1 rechazado
   DEFINE v_ax_edo                  SMALLINT;
   DEFINE v_situacion               SMALLINT;
   DEFINE v_situacion_ant           SMALLINT;
   DEFINE v_nss_rl                  CHAR(11);
   DEFINE v_f_alta_rl               DATE;
   DEFINE v_f_baja_rl               DATE;
   DEFINE v_bnd_rl                  SMALLINT;
   DEFINE v_f_registro_carta        DATE;
   DEFINE v_aux_ind                 SMALLINT;
   DEFINE v_f_periodo               CHAR(2);
   DEFINE bnd_rel_laboral           SMALLINT;

   DEFINE v_id_dh_aux               DECIMAL(9,0);
   DEFINE v_sdo_aivs                DECIMAL(12,2);
   DEFINE v_precio_fondo            DECIMAL(19,14);
   DEFINE v_sdo_pesos               DECIMAL(12,2);
   DEFINE v_sdo_pendiente           DECIMAL(12,2);
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_f_periodo2              CHAR(4);
   DEFINE v_error2                  SMALLINT;
   DEFINE a                         INTEGER;
   DEFINE v_periodo                 CHAR(8);
   DEFINE v_f_alta                  CHAR(10);
   DEFINE v_tpo_envio               CHAR(1);
   DEFINE v_nss_unificado           CHAR(11);
   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_diag                    SMALLINT;

   DEFINE v_periodo_arh             CHAR(6);
   DEFINE v_periodo_his             CHAR(6);

   DEFINE v_a_act                   CHAR(4);      
   DEFINE v_m_act                   CHAR(2);
   DEFINE v_m_posterior             CHAR(2);

   ON EXCEPTION SET v_error
      LET v_cnt_aceptados      = 0;
      LET v_cnt_rechazados     = 0;
      RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_valida_sp3_ug.trace';
   TRACE ON;

   LET v_error              = 0;
   LET v_subproceso         = "003";
   LET v_cnt_vig            = 0;
   LET v_f_proceso          = TODAY;
   LET v_ax_id_ocg_ctr_arch = p_id_ocg_ctr_arch;
   LET v_cnt_aceptados      = 0;
   LET v_cnt_rechazados     = 0;
   LET v_idx_nss            = 1;
   LET v_bnd_inconsistencia = 0;
   LET bnd_rel_laboral      = 0;
   LET a                    = 0;
   LET v_id_dh_aux          = 0;
   LET v_periodo_arh        = "";
   LET v_periodo_his        = "";
   LET v_id_dh              = "";
   LET v_imp_utilizado_ocg  = 0;
   LET v_imp_solic_uti_ocg  = 0;
   LET v_nss_unificado      = "";
   LET v_nss_unificador     = "";
   LET v_id_dh_unificador   = "";
   LET v_diag               = 0;
   LET v_situacion          = 0;
   LET v_situacion_ant      = 0;
   LET v_tpo_credito        = "";

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
     AND fondo = 11;

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_venc_imp_solic = NULL
    WHERE subproceso = v_subproceso
      And f_venc_imp_solic = "";

   FOREACH
      SELECT tpo_envio,
             cve_ent_financiera ,
             nss                ,
             num_ctrl_ef        ,
             f_envio[5,6] ||"/"|| f_envio[7,8] ||"/"|| f_envio[1,4],
             cred_convenidos    ,
             viv97              ,
             imp_solic_uti_ocg  ,
             f_venc_imp_solic[5,6] ||"/"|| f_venc_imp_solic[7,8] ||"/"|| f_venc_imp_solic[1,4],
             imp_utilizado_ocg  ,
             solic_saldo        ,
             f_venc_imp_solic[1,4] || f_venc_imp_solic[5,6]
        INTO v_tpo_envio          ,
             v_cve_ent_financiera ,
             v_nss                ,
             v_num_ctrl_ef        ,
             v_f_envio            ,
             v_tpo_credito        ,
             v_viv97              ,
             v_imp_solic_uti_ocg  ,
             v_f_venc_imp_solic   ,
             v_imp_utilizado_ocg  ,
             v_solic_saldo        ,
             v_periodo_arh
        FROM safre_tmp:tmp_rec_det_ocg43
       WHERE subproceso = v_subproceso
    ORDER BY nss,f_venc_imp_solic asc

      -- Se asigna a la variable el valor de la secuencia
      LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      LET v_id_ocg_solic_ug    = seq_ocg_solic_ug.nextval;
      LET v_estado             = 20;      -- Se inicializa en cero, asumiendo que el proceso no ser¦ rechazado
      LET v_bnd_inconsistencia = 0;
      LET v_situacion          = 10;
      LET v_diagnostico        = 01;      -- Se asume que ser¦ aceptado
      LET v_ax_f_envio         = v_f_envio;
      LET v_bnd_rl             = 0;
      LET a                    = a + 1;
      LET v_sdo_pendiente      = 0;
      LET v_solic_saldo        = 2; --Cambio solicitado por Mauro para evitar resagos de usos en archivo de salida --pendiente de pasar a QA y ProducciÃn

      IF v_tpo_credito <> "7" AND v_tpo_credito <> "8" AND v_tpo_credito <> "C" THEN
         LET v_tpo_credito = "A";
      END IF

      IF v_tpo_credito = "" OR v_tpo_credito = " " THEN
         LET v_tpo_credito = "A";
      END IF

      IF v_imp_solic_uti_ocg IS NULL OR v_imp_solic_uti_ocg < 0 THEN
         LET v_imp_solic_uti_ocg = 0;
      END IF;

      LET v_imp_solic_uti_ocg = v_imp_solic_uti_ocg / 100;
      LET v_imp_solicitado    = v_imp_solic_uti_ocg;

      -- Se obtiene el id_derechohabiente
      SELECT id_derechohabiente,
             tipo_trabajador,
             ind_estado_cuenta
        INTO v_id_dh,
             v_tpo_trabajador,
             v_ind_edo_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      -- Si el NSS es incorrecto        #1
      FOR v_idx_nss = 1 TO LENGTH(v_nss)
         IF SUBSTR(v_nss,v_idx_nss,1) NOT MATCHES '[0-9]' THEN
            --TRACE 'El nss no cumple con la valiac¦n: ' || v_nss;
            --TRACE 'En la posici¦n : ' || v_idx_nss;

            LET v_inconsistencia = "02";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
            EXIT FOR;
         END IF
      END FOR;

      -- Se verficia que exista en la base de derechohabientes     #2
      IF v_id_dh < 0 OR v_id_dh IS NULL THEN
         LET v_id_dh              = 0;
         LET v_inconsistencia     = "02";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia     = "43";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica que el nss sea tpo_trabajador ="I"      #3
      IF v_tpo_trabajador <> "I" THEN
         LET v_inconsistencia = "01";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica si la cuenta es inhabilitada por unificaci¦n  #4
      IF v_ind_edo_cuenta <> 0 THEN
         EXECUTE PROCEDURE fn_busca_nss_unificador (v_nss)
                      INTO v_nss_unificador, v_id_dh_unificador, v_diag;

         IF v_diag = 1 THEN
            LET v_nss_unificado  = v_nss;
            LET v_nss            = v_nss_unificador;
            LET v_id_dh          = v_id_dh_unificador;
            LET v_ind_edo_cuenta = 0;

            INSERT INTO safre_tmp:tmp_ocg_uni
            VALUES(v_nss_unificado,
                   v_nss_unificador,
                   5,
                   TODAY);
         ELSE
            LET v_inconsistencia = "43";  --"03";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF
      END IF

      -- Se inserta en la tabla de detalle
      INSERT INTO ocg_detalle
           VALUES( v_id_ocg_detalle,
                   v_ax_id_ocg_ctr_arch,
                   v_id_dh,
                   v_subproceso,
                   v_f_proceso,
                   v_cve_ent_financiera,
                   v_nss );

{
      -- Se verifica que el nss tenga un credito 43 bis vigente        #5
      SELECT COUNT(*)
        INTO v_cnt_vig
        FROM cre_acreditado a,
             afi_derechohabiente b,
             cat_maq_credito c
       WHERE a.id_derechohabiente = b.id_derechohabiente
         AND a.estado = c.estado
         AND a.tpo_originacion = 2
         AND c.entidad = 1
         AND b.nss = v_nss;

      IF v_cnt_vig = 0 THEN
         LET v_inconsistencia = "43";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      LET bnd_rel_laboral = 1;
      END IF
}

      LET v_ax_edo = NULL;

       -- Se recupera el id_ocg_formalizacion
      SELECT id_ocg_formalizacion,
             id_ocg_tramite,
             cve_ent_financiera,
             situacion
        INTO v_id_ocg_formalizacion,
             v_id_ocg_tramite,
             v_ax_cve_ent_financiera,
             v_ax_edo
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_dh
         AND diagnostico        = 1
         AND situacion          IN (55,60,70,80);

      SELECT COUNT(*)
        INTO v_cnt_cve_ef
        FROM cat_entidad_financiera
       WHERE cve_ent_financiera = v_cve_ent_financiera;

      -- Se valida que la entidad financiera exista en el cat¦logo        #8
      IF v_cnt_cve_ef = 0 THEN
         LET v_inconsistencia = "26";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      ELSE
       -- Se valida que sea la misma entidad financiera que en el proceso de formalizaci¦n #12
         IF v_ax_cve_ent_financiera <> v_cve_ent_financiera THEN
            LET v_inconsistencia     = "26";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF
      END IF

      -- Se valida que la fecha de env¦o sea menor o igual a hoy        #9
      IF v_ax_f_envio > TODAY THEN
         LET v_inconsistencia = "46";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se valida que el saldo en ss97 sea mayor a 0         #10
      SELECT ROUND((sum(monto_acciones*v_precio_fondo)),2)
          INTO v_sdo_pesos
          FROM cta_movimiento
         WHERE id_derechohabiente = v_id_dh
           AND subcuenta          = 4
           AND fondo_inversion    = 11;

      LET v_viv97 = v_sdo_pesos;

      IF v_viv97 = 0 THEN
         LET v_inconsistencia     = "57";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
             VALUES( v_id_ocg_solic_ug,
                     v_subproceso,
                     v_inconsistencia,
                     v_f_proceso );
      END IF

      IF v_id_ocg_formalizacion IS NULL THEN
-- Se realiza modificación para rechazar cuando id_ocg_formalizacion es nulo 03/04/2019
         LET v_inconsistencia = "43";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
         VALUES( v_id_ocg_solic_ug,
                 v_subproceso,
                 v_inconsistencia,
                 v_f_proceso );

         LET v_id_ocg_formalizacion = 0;   -- Pendiente se usa el id para probar
      END IF

      -- Se valida que el estado en formalizacion sea acreditado   ESTADO ACREDITADO   #13

      LET v_aux_ind = 0;

     -- IF (v_ax_edo < 60) THEN
       --  LET v_aux_ind = 1;
     -- END IF

      IF (v_ax_edo IS NULL) OR (v_ax_edo <> 80) THEN
          LET v_aux_ind = 1;
      END IF

      IF v_aux_ind = 1 THEN
         LET v_inconsistencia     = "62";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_ax_edo = 130 THEN --  ESTADO CANCELADO    #14
         LET v_inconsistencia     = "53";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se valida que la fecha de vencimiento sea mayor o igual a la fecha de registro de carta #15
      SELECT f_registro_carta
        INTO v_f_registro_carta
        FROM ocg_formalizacion
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion;

      IF v_f_venc_imp_solic < v_f_registro_carta THEN
          LET v_inconsistencia = "46";
          LET v_bnd_inconsistencia = 1;

          INSERT INTO ocg_inconsistencia
               VALUES( v_id_ocg_solic_ug,
                       v_subproceso,
                       v_inconsistencia,
                       v_f_proceso );
      END IF
--validación para aceptar gariantía maximo al mes siguiente del mes actual

      LET v_a_act       = year (today);
      LET v_m_act       = month(today);

      IF v_m_act        = "12" THEN
         LET v_a_act    = v_a_act + 1   ;
         LET v_m_posterior =  "1" ;
      ELSE
         LET v_m_posterior = v_m_act + 1 ;
      END IF

      IF year(v_f_venc_imp_solic) = v_a_act THEN
         IF month(v_f_venc_imp_solic) = "12" OR
            month(v_f_venc_imp_solic) = "1" THEN
         ELSE
            IF month(v_f_venc_imp_solic) > v_m_posterior THEN
               LET v_inconsistencia = "46";
               LET v_bnd_inconsistencia = 1;

               INSERT INTO ocg_inconsistencia
                     VALUES( v_id_ocg_solic_ug,
                             v_subproceso,
                             v_inconsistencia,
                             v_f_proceso );
            END IF
         END IF
      END IF

      IF year(v_f_venc_imp_solic) > v_a_act THEN
          LET v_inconsistencia = "46";
          LET v_bnd_inconsistencia = 1;

          INSERT INTO ocg_inconsistencia
               VALUES( v_id_ocg_solic_ug,
                       v_subproceso,
                       v_inconsistencia,
                       v_f_proceso );
      END IF
{
 -- Validacion para no relacion laboral   #16
      IF bnd_rel_laboral = 0 THEN
         FOREACH
            SELECT nss,
                   (lpad(month(f_alta)+1,2,0))||
                   "/01/"||
                   (year(f_alta)),
                   (f_baja-(day(f_baja)))
              INTO v_nss_rl,
                   v_f_alta,
                   v_f_baja_rl
              FROM ocg_relacion_laboral

            IF v_f_alta[1,2] = 13 THEN
               LET v_f_alta = "01"||"/"||"01"||"/"||((v_f_alta[7,10])+1);
               LET v_f_alta_rl = v_f_alta;
            ELSE
               LET v_f_alta_rl = v_f_alta;
            END IF

            -- Si tiene relaci¦n laboral se prente una bandera
            IF v_f_venc_imp_solic BETWEEN v_f_alta_rl AND v_f_baja_rl THEN
               LET v_bnd_rl = 1;
               EXIT FOREACH;
            END IF
         END FOREACH;
      END IF

      IF v_bnd_rl = 1 THEN
         LET v_inconsistencia = "44";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF;
}

      -- El importe solicitado debe ser menor a 55000  #17
      IF v_imp_solic_uti_ocg > 55000 THEN
         LET v_inconsistencia = "59";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      ELIF v_imp_solic_uti_ocg IS NULL OR v_imp_solic_uti_ocg <= 0 THEN
         LET v_inconsistencia = "45";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Validaci¦n para periodo que ya se pag¦ con anterioridad   #18
      -- y Validaci¦n para per¦odo aceptado pero en gesti¦n de pago   #19
      FOREACH
         SELECT YEAR(f_vencimiento)||LPAD(MONTH(f_vencimiento),2,0),
                situacion
           INTO v_periodo_his,
                v_situacion_ant
           FROM ocg_solicitud_uso_garantia
          WHERE id_derechohabiente = v_id_dh

         IF v_periodo_his = v_periodo_arh THEN
            IF v_situacion_ant = 110 THEN
               LET v_inconsistencia = "50";
               LET v_bnd_inconsistencia = 1;

               INSERT INTO ocg_inconsistencia
                    VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
            ELSE
               IF v_situacion_ant = 50 OR
                  v_situacion_ant = 90 OR
                  v_situacion_ant = 100 THEN

                  LET v_inconsistencia = "49";
                  LET v_bnd_inconsistencia = 1;

                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_solic_ug,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF
            END IF

            EXIT FOREACH;
         END IF
      END FOREACH;

      -- se valida que el tipo de credito sea solamente A,C o blanco #20
      IF v_tpo_credito IS NULL THEN
         SELECT tpo_credito
           INTO v_tpo_credito
           FROM ocg_formalizacion
          WHERE id_derechohabiente = v_id_dh
            AND situacion in (55,60,70,80);
      END IF

      IF v_tpo_envio <> "E" THEN
         IF -- (v_tpo_credito IS NULL) OR
            (v_tpo_credito = "A")   OR
            (v_tpo_credito = "C") THEN
         ELSE
            LET v_inconsistencia = "60";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                    VALUES( v_id_ocg_solic_ug,
                            v_subproceso,
                            v_inconsistencia,
                            v_f_proceso );
         END IF
      END IF

      -- Se valida si existieron inconsistencias
      IF v_bnd_inconsistencia = 1 THEN
         LET v_situacion      = 20;
         LET v_diagnostico    = 02;  -- Se rechaza
         LET v_cnt_rechazados = v_cnt_rechazados + 1;
      ELSE

   -- Validaci¦n para no relacion laboral   #16
         IF bnd_rel_laboral = 0 THEN
            FOREACH
               SELECT nss,
                      (lpad(month(f_alta)+1,2,0))||
                      "/01/"||
                      (year(f_alta)),
                      (f_baja-(day(f_baja)))
                 INTO v_nss_rl,
                      v_f_alta,
                      v_f_baja_rl
                 FROM ocg_relacion_laboral
                WHERE nss = v_nss

               IF v_f_alta[1,2] = 13 THEN
                  LET v_f_alta = "01"||"/"||"01"||"/"||((v_f_alta[7,10])+1);
                  LET v_f_alta_rl = v_f_alta;
               ELSE
                  LET v_f_alta_rl = v_f_alta;
               END IF

               -- Si tiene relaci¦n laboral se prente una bandera
               IF v_f_venc_imp_solic BETWEEN v_f_alta_rl AND v_f_baja_rl THEN
                  LET v_bnd_rl = 1;
                  EXIT FOREACH;
               END IF
            END FOREACH;
         END IF

         IF v_bnd_rl = 1 THEN
            LET v_inconsistencia = "44";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF;

         IF v_bnd_inconsistencia = 1 THEN
            LET v_situacion      = 20;
            LET v_diagnostico    = 02;  -- Se rechaza
            LET v_cnt_rechazados = v_cnt_rechazados + 1;
         ELSE
            LET v_situacion      = 50;
            LET v_cnt_aceptados = v_cnt_aceptados + 1;
         END IF
      END IF

      -- Se inserta en la tabla ocg_solicitud_uso_garantia
      INSERT INTO ocg_fecha_mig
           VALUES(v_id_ocg_solic_ug,
                  v_id_ocg_detalle,
                  v_id_dh,
                  v_f_proceso,
                  v_f_proceso,
                  '',
                  '',
                  3,
                  v_f_proceso);

      INSERT INTO ocg_solicitud_uso_garantia
           VALUES(v_id_ocg_solic_ug,
                  v_id_ocg_detalle,
                  v_id_ocg_formalizacion,
                  v_id_ocg_tramite,
                  v_id_dh,
                  v_cve_ent_financiera,
                  v_num_ctrl_ef,
                  v_imp_solicitado,
                  v_f_venc_imp_solic,
                  v_imp_utilizado_ocg,
                  v_tpo_credito,
                  v_solic_saldo,
                  v_diagnostico,
                  v_estado,
                  v_situacion
                  );

     {
      IF v_diagnostico = 01 THEN
         LET v_periodo_pago = year(v_f_venc_imp_solic)||(lpad(month(v_f_venc_imp_solic),2,0));
         LET v_periodo = "'"||v_periodo_pago||"'";

         CALL fn_integra_uso_cre(v_id_dh,
                                 v_imp_solic_uti_ocg,
                                 v_periodo_pago,
                                 v_id_ocg_solic_ug,
                                 2,
                                 "safreviv")
          RETURNING v_error2;
      END IF
     }

      LET v_id_dh              = "";
      LET v_imp_solic_uti_ocg  = 0;
      LET v_imp_utilizado_ocg  = 0;
      LET v_periodo_arh        = "";
      LET v_periodo_his        = "";
      LET v_nss_unificado      = "";
      LET v_nss_unificador     = "";
      LET v_id_dh_unificador   = "";
      LET v_diag               = 0;
      LET v_situacion          = 0;
      LET v_situacion_ant      = 0;
      LET v_tpo_credito        = "";
   END FOREACH;

   -- Se actualiza el contador de la tabla de control de archisos
   LET v_tot_regs = v_cnt_aceptados + v_cnt_rechazados;

   UPDATE ocg_ctr_archivo
      SET tot_sp3 = v_tot_regs
    WHERE id_ocg_ctr_archivo = p_id_ocg_ctr_arch;

   -- Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_detalle;
   UPDATE STATISTICS FOR TABLE ocg_solicitud_uso_garantia;
   UPDATE STATISTICS FOR TABLE ocg_inconsistencia;
   UPDATE STATISTICS FOR TABLe ocg_fecha_mig;

   RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;

END FUNCTION;


