






CREATE FUNCTION "safreviv".fn_ocg_valida_subpr1_tramite(p_id_ocg_ctr_arch DECIMAL(9,0),
                                             p_usuario         CHAR(20))
   RETURNING SMALLINT

   DEFINE v_cred_convenidos      CHAR(1);
   DEFINE v_tpo_trabajador       CHAR(1);
   DEFINE v_r_resp               CHAR(1);
   DEFINE v_inconsistencia       CHAR(2);
   DEFINE v_subproceso           CHAR(3);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_r_nss                CHAR(11);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_ax_rfc               CHAR(13);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_ax_curp              CHAR(18);
   DEFINE v_ap_paterno_af        CHAR(40);
   DEFINE v_ap_materno_af        CHAR(40);
   DEFINE v_nombre_af            CHAR(40);
   DEFINE v_ax_ap_paterno_af     CHAR(40);
   DEFINE v_ax_ap_materno_af     CHAR(40);
   DEFINE v_ax_nombre_af         CHAR(40);
   DEFINE v_r_ap_paterno_af      CHAR(40);
   DEFINE v_r_ap_materno_af      CHAR(40);
   DEFINE v_r_nombre_af          CHAR(40);
   DEFINE v_f_proceso            DATE;
   DEFINE v_r_ftramite           DATE;
   DEFINE v_f_respuesta          DATE;
   DEFINE v_diagnostico          DECIMAL(2,0);
   DEFINE v_cve_ent_financiera   DECIMAL(3,0);
   DEFINE v_cve_ent_fin_valida   DECIMAL(3,0);
   DEFINE v_num_bimestres        DECIMAL(3,0);
   DEFINE v_pesos_viv97          DECIMAL(12,2);
   DEFINE v_f_corte_subcuenta    DECIMAL(8,0);
   DEFINE v_f_vigencia           DECIMAL(8,0);
   DEFINE v_id_dh                DECIMAL(9,0);
   DEFINE v_id_ocg_detalle       DECIMAL(9,0);
   DEFINE v_ax_id_ocg_ctr_arch   DECIMAL(9,0);
   DEFINE v_r_id_dh              DECIMAL(9,0);
   DEFINE v_id_ocg_tramite       DECIMAL(9,0);
   DEFINE v_ax_id_dh             DECIMAL(9,0);
   DEFINE v_viv97                DECIMAL(16,6);
   DEFINE v_precio_fondo         DECIMAL(19,14);
   DEFINE v_cnt_aceptados        INTEGER;
   DEFINE v_cnt_rechazados       INTEGER;
   DEFINE v_tot_regs             INTEGER;
   DEFINE v_cnt_exist            SMALLINT;
   DEFINE v_cnt_vig              SMALLINT;                  -- Contador para cr¦ditos vigentes o en tr¦mite 43 bis
   DEFINE v_ind_edo_cuenta       SMALLINT;
   DEFINE v_si_cre_vigente       SMALLINT;
   DEFINE v_error                SMALLINT;
   DEFINE v_bnd_inconsistencia   SMALLINT;
   DEFINE v_estado               SMALLINT;
   DEFINE v_marca_unifica        SMALLINT;
   DEFINE v_r_marca              SMALLINT;
   DEFINE v_idx                  SMALLINT;
   DEFINE v_situacion            SMALLINT;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_valida               SMALLINT;
   DEFINE v_tpo_originacion      SMALLINT;
   DEFINE v_tpo_credito          SMALLINT;
   DEFINE v_ax_pos               SMALLINT;
   DEFINE v_marca                SMALLINT;
   DEFINE v_proceso_cod          SMALLINT;
   DEFINE v_rch_marca            SMALLINT;
   -- Varibles para las validaciones del nombre
   DEFINE v_pos_paterno          SMALLINT;
   DEFINE v_pos_materno          SMALLINT;
   DEFINE v_pos_nombre           SMALLINT;
   DEFINE v_b_pos_paterno        SMALLINT;
   DEFINE v_b_pos_materno        SMALLINT;
   DEFINE v_b_pos_nombre         SMALLINT;
   -- Variables auxiliares para la ejecuci¦n de fn_edo_cred_viv
   DEFINE v_f_otorga             DATE;
   DEFINE v_f_liquida            DATE;
   DEFINE v_num_credito          DECIMAL(10,0);
   DEFINE v_tpo_dscto            SMALLINT;
   DEFINE v_id_tr                DECIMAL(9,0);

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_valida_subpr1_tramite.trace';
   TRACE ON;

   LET v_subproceso         = "001";
   LET v_error              = 0;
   LET v_cnt_vig            = 0;
   LET v_cnt_aceptados      = 0;
   LET v_cnt_rechazados     = 0;
   LET v_bnd_inconsistencia = 0;
   LET v_valida             = 0;
   LET v_rch_marca          = 0;
   LET v_marca_unifica      = 150;
   LET v_proceso_cod        = 3902;
   LET v_marca              = 206;
   LET v_f_proceso          = TODAY;
   LET v_f_corte_subcuenta  = TODAY;
   LET v_f_vigencia         = v_f_proceso + 150;
   LET v_ax_id_ocg_ctr_arch = p_id_ocg_ctr_arch;
   LET v_num_bimestres      = NULL;
   LET v_f_respuesta        = fn_calcula_habil_siguiente(v_f_proceso);
   LET v_cve_ent_fin_valida = 0;
   LET v_cred_convenidos    = "";

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   FOREACH
      SELECT cve_ent_financiera   ,
             nss                  ,
             rfc                  ,
             curp                 ,
             ap_paterno_af        ,
             ap_materno_af        ,
             nombre_af            ,
             cred_convenidos
        INTO v_cve_ent_financiera ,
             v_nss                ,
             v_rfc                ,
             v_curp               ,
             v_ap_paterno_af      ,
             v_ap_materno_af      ,
             v_nombre_af          ,
             v_cred_convenidos
        FROM safre_tmp:tmp_rec_det_ocg43
       WHERE subproceso = v_subproceso

      -- Se asigna a la variable el valor de la secuencia
      LET v_id_ocg_tramite     = seq_ocg_tramite.nextval;
      LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      --LET v_estado             = 30;
      LET v_situacion          = 30;   -- Asumiendo que ser¦ aceptado
      LET v_bnd_inconsistencia = 0;
      --LET v_situacion          = 10;
      LET v_estado             = 10;   -- En tr¦mite
      LET v_diagnostico        = 01;
       LET v_f_proceso          = TODAY;
      LET v_f_vigencia         = v_f_proceso + 150;

      IF v_cred_convenidos <> 'C' AND v_cred_convenidos <> '7' AND v_cred_convenidos <> '8' THEN
         LET v_cred_convenidos = 'A';
      END IF;

      IF v_cred_convenidos = '' OR v_cred_convenidos = ' ' THEN
         LET v_cred_convenidos = 'A';
      END IF;

      -- Se obtiene el id_derechohabiente
      SELECT id_derechohabiente,
             tipo_trabajador,
             ind_estado_cuenta
        INTO v_id_dh,
             v_tpo_trabajador,
             v_ind_edo_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

        --TRACE 'PUNTO CONTROL 0:'||v_id_ocg_detalle;

       IF (v_cred_convenidos = "7") OR
          (v_cred_convenidos = "8") THEN

          IF NOT EXISTS ( SELECT id_ocg_tramite
                        FROM ocg_tramite
                       WHERE id_derechohabiente = v_id_dh
                         AND situacion in (10,30,50,55,60)) THEN

             LET v_inconsistencia     = "25";
             LET v_bnd_inconsistencia = 1;
             LET v_pesos_viv97        = 0;
             LET v_diagnostico        = 2;
             LET v_situacion          =20;
             LET v_estado             = 60;
             LET v_f_vigencia         = NULL;
             LET v_cnt_rechazados     = v_cnt_rechazados + 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
             LET v_inconsistencia     = "26";

             INSERT INTO ocg_inconsistencia
                  VALUES( v_id_ocg_tramite,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );

          ELSE
             LET v_id_tr = 0;

             SELECT MAX(id_ocg_tramite)
               INTO v_id_tr
               FROM ocg_tramite
              WHERE id_derechohabiente = v_id_dh
                AND situacion in (10,30,50);

             IF v_id_tr IS NULL THEN
                LET v_id_tr = 0;
             END IF

             IF v_id_tr > 0 THEN
                UPDATE ocg_tramite
                   SET situacion = 25
                 WHERE id_ocg_tramite = v_id_tr
                   AND id_derechohabiente = v_id_dh;
             END IF

          END IF

         LET v_inconsistencia     = "90";
         LET v_bnd_inconsistencia = 1;
         LET v_pesos_viv97        = 0;
         LET v_diagnostico        = 1;
         LET v_estado             = 70;
         LET v_situacion          =25;
         LET v_f_vigencia         = NULL;
         --LET v_f_respuesta        = NULL;
         LET v_cnt_rechazados     = v_cnt_rechazados + 1;


         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         -- Se inserta en la tabla de detalle
         INSERT INTO ocg_detalle
              VALUES( v_id_ocg_detalle,
                      v_ax_id_ocg_ctr_arch,
                      v_id_dh,
                      v_subproceso,
                      v_f_proceso,
                      v_cve_ent_financiera,
                      v_nss );

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     1,
                     v_f_proceso);

         -- Se inserta en la tabla ocg_tramite
         INSERT INTO ocg_tramite
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_cve_ent_financiera,
                     v_id_dh,
                     v_rfc,
                     v_curp,
                     v_ap_paterno_af,
                     v_ap_materno_af,
                     v_nombre_af,
                     v_num_bimestres,
                     v_pesos_viv97,
                     v_f_corte_subcuenta,
                     v_cred_convenidos,
                     v_f_vigencia,
                     v_f_respuesta,
                     v_diagnostico,
                     v_estado,
                     225 );   --nueva situación para no reconocidos

         CONTINUE FOREACH;
      END IF

       --TRACE 'PUNTO CONTROL 1:'||v_id_ocg_detalle;

      IF v_id_dh < 0 OR v_id_dh IS NULL THEN
         LET v_inconsistencia     = "02";
         LET v_bnd_inconsistencia = 1;
         LET v_id_dh              = 0;
         LET v_pesos_viv97        = 0;
         LET v_diagnostico        = 2;
         LET v_estado             = 60;
         LET v_situacion          =20;
         LET v_diagnostico        = 02;
         LET v_f_vigencia         = NULL;
         --LET v_f_respuesta        = NULL;
         LET v_cnt_rechazados     = v_cnt_rechazados + 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         -- Se inserta en la tabla de detalle
         INSERT INTO ocg_detalle
              VALUES( v_id_ocg_detalle,
                      v_ax_id_ocg_ctr_arch,
                      v_id_dh,
                      v_subproceso,
                      v_f_proceso,
                      v_cve_ent_financiera,
                      v_nss );

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     1,
                     v_f_proceso);


         -- Se inserta en la tabla ocg_tramite
         INSERT INTO ocg_tramite
              VALUES(v_id_ocg_tramite,
                     v_id_ocg_detalle,
                     v_cve_ent_financiera,
                     v_id_dh,
                     v_rfc,
                     v_curp,
                     v_ap_paterno_af,
                     v_ap_materno_af,
                     v_nombre_af,
                     v_num_bimestres,
                     v_pesos_viv97,
                     v_f_corte_subcuenta,
                     v_cred_convenidos,
                     v_f_vigencia,
                     v_f_respuesta,
                     v_diagnostico,
                     v_estado,
                     v_situacion );

         CONTINUE FOREACH;
      END IF

       --TRACE 'PUNTO CONTROL 2 :'||v_id_ocg_detalle;

      -- Se valida si tiene unificaci¦n
      -- CAMBIOS HFJL20102015
      EXECUTE FUNCTION fn_verifica_marca_43(v_nss,v_marca_unifica,v_id_dh)
                  INTO v_r_nss,v_r_resp,v_r_ftramite,v_r_marca,v_r_id_dh;

      -- Si tiene marca 150
      IF v_r_resp = 1 THEN
         EXECUTE FUNCTION fn_busca_nss_unificador(v_nss)
                     INTO v_r_nss, v_r_id_dh, v_resultado;

         -- Si tiene unificaci¦n
         IF v_resultado = 1 THEN
            LET v_nss   = v_r_nss;
            LET v_id_dh = v_r_id_dh;

            -- Se inserta la inconsistencia, pero no se rechaza
            LET v_inconsistencia = "03";
            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_tramite,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF;
      END IF

      --TRACE 'PUNTO CONTROL '||v_id_ocg_detalle;

      -- Se inserta en la tabla de detalle
      INSERT INTO ocg_detalle
           VALUES( v_id_ocg_detalle,
                   v_ax_id_ocg_ctr_arch,
                   v_id_dh,
                   v_subproceso,
                   v_f_proceso,
                   v_cve_ent_financiera,
                   v_nss );

      INSERT INTO ocg_fecha_mig
           VALUES(v_id_ocg_tramite,
                  v_id_ocg_detalle,
                  v_id_dh,
                  v_f_proceso,
                  v_f_proceso,
                  '',
                  '',
                  1,
                  v_f_proceso);

      -- Si el NSS es incorrecto
      FOR v_idx = 1 TO LENGTH(v_nss)
         IF SUBSTR(v_nss,v_idx,1) NOT MATCHES '[0-9]' THEN
            --TRACE 'El nss no cumple con la validaci¦n: ' || v_nss;
            --TRACE 'En la posici¦n : ' || v_idx;

            LET v_inconsistencia = "02";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_tramite,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
            EXIT FOR;
         END IF
      END FOR;

      SELECT rfc,
             curp,
             ap_paterno_af,
             ap_materno_af,
             nombre_af
        INTO v_ax_rfc,
             v_ax_curp,
             v_ax_ap_paterno_af,
             v_ax_ap_materno_af,
             v_ax_nombre_af
        FROM afi_derechohabiente
       WHERE nss = v_nss
         AND id_derechohabiente = v_id_dh;

      LET v_pos_paterno   = INSTR(v_ap_paterno_af,'¦');
      LET v_pos_materno   = INSTR(v_ap_materno_af,'¦');
      LET v_pos_nombre    = INSTR(v_nombre_af,'¦');
      LET v_b_pos_paterno = INSTR(v_ap_paterno_af,'#');
      LET v_b_pos_materno = INSTR(v_ap_materno_af,'#');
      LET v_b_pos_nombre  = INSTR(v_nombre_af,'#');

      -- valida si encuentra ¦ o numeral en la cadena del ap_paterno
      IF v_pos_paterno > 0 THEN
         --LET v_ax_ap_paterno_af[v_pos_paterno] = "¦";
         --LET SUBSTR(v_ax_ap_paterno_af,v_pos_paterno,1) = '¦';
         LET v_ax_ap_paterno_af = REPLACE(v_ax_ap_paterno_af, SUBSTR(v_ax_ap_paterno_af,v_pos_paterno,1), '¦');
      ELIF v_b_pos_paterno > 0 THEN
         LET v_ax_ap_paterno_af = REPLACE(v_ax_ap_paterno_af, SUBSTR(v_ax_ap_paterno_af,v_b_pos_paterno,1), '#');
      END IF

      -- valida si encuentra ¦ o numeral en la cadena del ap_materno
      IF v_pos_materno > 0 THEN
         LET v_ax_ap_materno_af = REPLACE(v_ax_ap_materno_af, SUBSTR(v_ax_ap_materno_af,v_pos_materno,1), '¦');
      ELIF v_b_pos_materno > 0 THEN
         LET v_ax_ap_materno_af = REPLACE(v_ax_ap_materno_af, SUBSTR(v_ax_ap_materno_af,v_b_pos_materno,1), '#');
      END IF

      -- valida si encuentra ¦ o numeral en la cadena del nombre
      IF v_pos_nombre > 0 THEN
         LET v_ax_nombre_af = REPLACE(v_ax_nombre_af, SUBSTR(v_ax_nombre_af,v_pos_nombre,1), '¦');
      ELIF v_b_pos_nombre > 0 THEN
         LET v_ax_nombre_af = REPLACE(v_ax_nombre_af, SUBSTR(v_ax_nombre_af,v_b_pos_nombre,1), '#');
      END IF

      LET v_ap_paterno_af = v_ax_ap_paterno_af;
      LET v_ap_materno_af = v_ax_ap_materno_af;
      LET v_nombre_af     = v_ax_nombre_af;

{
      IF v_ax_ap_paterno_af <> v_ap_paterno_af OR
        -- v_ax_ap_materno_af <> v_ap_materno_af OR
         v_ax_nombre_af     <> v_nombre_af     THEN

         LET v_inconsistencia = "04";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "19";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "24";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF


      IF (v_ax_ap_materno_af IS NULL AND v_ap_materno_af IS NOT NULL) OR
         (v_ax_ap_materno_af IS NOT NULL AND v_ap_materno_af IS NULL) THEN

         LET v_inconsistencia = "04";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "19";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF (v_ax_ap_materno_af IS NOT NULL)        AND
         (v_ap_materno_af    IS NOT NULL)        AND
         (v_ax_ap_materno_af <> v_ap_materno_af) THEN

         LET v_inconsistencia = "04";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "19";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
}

      IF v_ax_rfc <> v_rfc OR v_rfc IS NULL THEN
         LET v_rfc = v_ax_rfc;

         -- Se inserta inconsistencia pero no se rechaza
         LET v_inconsistencia = "05";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_ax_curp <> v_curp OR v_curp IS NULL THEN
         LET v_curp = v_ax_curp;

         -- Se inserta inconsistencia pero no se rechaza
         LET v_inconsistencia = "12";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      -- Se valida que la entidad financiera exista en el cat¦logo de EF
      IF NOT EXISTS ( SELECT cve_ent_financiera
                        FROM cat_entidad_financiera
                       WHERE cve_ent_financiera = v_cve_ent_financiera ) THEN

         LET v_inconsistencia = "26";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF;

      -- Se verifica que el nss sea tpo_trabajador ="I"
      IF v_tpo_trabajador <> "I" THEN
         LET v_inconsistencia = "01";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'Tipo Trabajador I nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      -- Se verifica que el ind_edo_cuenta = 0
      IF v_ind_edo_cuenta <> 0 THEN
         LET v_inconsistencia = "03";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'ind edo cuenta diferente de cero : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF EXISTS (SELECT id_ocg_formalizacion
                   FROM ocg_formalizacion
                  WHERE id_derechohabiente = v_id_dh
                    AND situacion NOT IN (10,20,40,120,130,140,150,153,155,158,160)
                    AND diagnostico = 1) THEN

            LET v_inconsistencia = "17";
            LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES(v_id_ocg_tramite,
                     v_subproceso,
                     v_inconsistencia,
                     v_f_proceso );

      ELSE
         -- Se valida que no tenga un tramite
         IF EXISTS ( SELECT id_ocg_tramite
                       FROM ocg_tramite
                      WHERE id_derechohabiente = v_id_dh
                        AND situacion IN (30,50)
                        AND diagnostico = 1 ) THEN

            LET v_inconsistencia = "16";
            LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES(v_id_ocg_tramite,
                     v_subproceso,
                     v_inconsistencia,
                     v_f_proceso );

         END IF

      END IF
      -- Se verifica que el nss no tenga un credito 43 bis vigente o en tr¦mite
      SELECT COUNT(*)
        INTO v_cnt_vig
        FROM cre_acreditado a,
             afi_Derechohabiente b,
             cat_maq_credito c
       WHERE a.id_derechohabiente = b.id_derechohabiente
         AND a.estado = c.estado
         AND a.tpo_originacion = 2
         AND a.estado = 18
         AND c.entidad = 1
         AND b.nss = v_nss ;

      IF v_cnt_vig > 0 THEN
         LET v_inconsistencia = "17";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'Tiene credito vigente o en tramite 43bis nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_cnt_vig = 0 THEN

      -- Se verifica que no tenga un cr¦dito vigente       6
      CALL fn_edo_cred_viv(v_id_dh, v_valida)
      RETURNING v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;

      IF v_resultado = 0 THEN
         IF v_tpo_credito <> 2 THEN
            LET v_inconsistencia = "14";
         ELSE
            LET v_inconsistencia = "17";
         END IF

         LET v_bnd_inconsistencia = 1;
         --TRACE 'Tiene credito vigente nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      END IF
      -- Se verifica que no tenga un cr¦dito en tr¦mite    #7
      IF EXISTS ( SELECT a.id_derechohabiente
                    FROM afi_derechohabiente a, sfr_marca_activa s
                   WHERE a.nss = v_nss
                     AND a.id_derechohabiente = s.id_derechohabiente
                     AND s.marca = 213)
                     {
                     AND id_derechohabiente IN (
                         SELECT id_derechohabiente
                           FROM sfr_marca_activa
                          WHERE marca = 213 )
                      )} THEN

         LET v_inconsistencia = "15";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'Tiene credito en tramite nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF
{
      -- apellido paterno y/o nombre en blancos inconsistencia 24       #8
      IF v_ap_paterno_af IS NULL OR v_ap_paterno_af = "" OR
        -- v_ap_materno_af IS NULL OR v_ap_materno_af = "" OR
         v_nombre_af IS NULL OR v_nombre_af = ""  THEN

         LET v_inconsistencia = "24";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'Apellido paterno y/o nombre en blancos nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF
}
      --TRACE 'creditos_convenidos : ' || v_cred_convenidos;
      -- Se verifica que el tipo de cr¦dito sea C, 7 u 8 en caso contrario le asigna A
      --IF v_cred_convenidos = 'C' OR v_cred_convenidos = '7' OR v_cred_convenidos = '8' THEN
         --LET v_cred_convenidos = v_cred_convenidos;
      --ELSE
         --LET v_cred_convenidos = 'A';
      --END IF;

      IF v_cred_convenidos <> 'C' AND v_cred_convenidos <> '7' AND v_cred_convenidos <> '8' THEN
         LET v_cred_convenidos = 'A';
      END IF;

      IF v_cred_convenidos = '' OR v_cred_convenidos = ' ' THEN
         LET v_cred_convenidos = 'A';
      END IF;

      -- Validaci¦n de relaci¦n laboral
      IF NOT EXISTS ( SELECT ad.id_derechohabiente
                        FROM afi_derechohabiente ad ,
                             afi_relacion_laboral arl
                       WHERE ad.nss = v_nss
                         AND ad.id_derechohabiente = arl.id_derechohabiente ) THEN
         LET v_inconsistencia = "13";
         LET v_bnd_inconsistencia = 1;
         --TRACE 'No tiene relacion laboral nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se valida que el saldo en la subcuenta de viv97 no sea menor a cero, si es menos se deja el saldo en cero
      SELECT SUM(monto_acciones)
        INTO v_viv97
        FROM cta_movimiento c,
             afi_derechohabiente ad
       WHERE nss = v_nss
         AND subcuenta = 4
         AND fondo_inversion = 11
         AND c.id_derechohabiente = ad.id_derechohabiente;

      LET v_pesos_viv97 = ROUND((v_precio_fondo * v_viv97),2);

      IF v_viv97 <= 0 OR v_viv97 IS NULL THEN
         LET v_viv97 = 0;
         LET v_pesos_viv97 = 0;

         LET v_inconsistencia = "18";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

         -- Se valida que no tenga un tramite no reconocido
       IF EXISTS ( SELECT id_ocg_tramite
                     FROM ocg_tramite
                    WHERE id_derechohabiente = v_id_dh
                      AND situacion = 25
                      AND diagnostico = 1) THEN


          SELECT cve_ent_financiera
            INTO v_cve_ent_fin_valida
            FROM ocg_tramite
           WHERE id_derechohabiente = v_id_dh
             AND situacion = 25
             AND diagnostico = 1;

          IF v_cve_ent_fin_valida <> v_cve_ent_financiera THEN
             LET v_inconsistencia = "16";
             LET v_bnd_inconsistencia = 1;

             INSERT INTO ocg_inconsistencia
                  VALUES(v_id_ocg_tramite,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
          END IF
       END IF


      -- En caso de tener una o mas inconsistencias, se asigna el estado como rechazado
      IF v_bnd_inconsistencia = 1 THEN
         LET v_cnt_rechazados = v_cnt_rechazados + 1;
         LET v_diagnostico    = 02;
         LET v_f_vigencia     = NULL;
         --LET v_estado         = 20;
         LET v_situacion      = 20;
         LET v_estado         = 60;
      ELSE
         -- Se invoca a la funci¦n que marca la cuenta
         EXECUTE FUNCTION fn_marca_cuenta(v_id_dh,
                                          v_marca,
                                          v_id_ocg_tramite,
                                          0,
                                          0,
                                          0,
                                          0,
                                          TODAY,
                                          p_usuario,
                                          v_proceso_cod) INTO v_rch_marca;

         IF v_rch_marca <> 0 THEN
           -- LET v_error = 1;
            LET v_inconsistencia = "14";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_tramite,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );

            LET v_cnt_rechazados = v_cnt_rechazados + 1;
            LET v_diagnostico    = 02;
            LET v_f_vigencia     = NULL;
            --LET v_estado         = 20;
            LET v_situacion      = 20;
            LET v_estado         = 60;

         ELSE

            LET v_cnt_aceptados = v_cnt_aceptados + 1;
            LET v_diagnostico = 01;

         END IF;

      END IF

      -- Se inserta en la tabla ocg_tramite
      INSERT INTO ocg_tramite
           VALUES(v_id_ocg_tramite,
                  v_id_ocg_detalle,
                  v_cve_ent_financiera,
                  v_id_dh,
                  v_rfc,
                  v_curp,
                  v_ap_paterno_af,
                  v_ap_materno_af,
                  v_nombre_af,
                  v_num_bimestres,
                  v_pesos_viv97,
                  v_f_corte_subcuenta,
                  v_cred_convenidos,
                  v_f_vigencia,
                  v_f_respuesta,
                  v_diagnostico,
                  v_estado,
                  v_situacion );

      LET v_cred_convenidos    = "";
      LET v_cve_ent_fin_valida = 0;
   END FOREACH

   -- Se actualiza el contador de la tabla de control de archisos
   LET v_tot_regs = v_cnt_aceptados + v_cnt_rechazados;

   UPDATE ocg_ctr_archivo
      SET tot_sp1 = v_tot_regs
    WHERE id_ocg_ctr_archivo = p_id_ocg_ctr_arch;

   -- Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_tramite;
   UPDATE STATISTICS FOR TABLE ocg_inconsistencia;
   UPDATE STATISTICS FOR TABLE ocg_detalle;

   RETURN v_error;
END FUNCTION;


