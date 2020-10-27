






CREATE FUNCTION "safreviv".fn_ocg_valida_subpr2_formalizacion(p_id_ocg_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT,INTEGER,INTEGER

   DEFINE v_tpo_trabajador          CHAR(1);
   DEFINE v_tpo_envio               CHAR(1);
   DEFINE v_tpo_credito             CHAR(1);
   DEFINE v_tpo_credito2            CHAR(1);
   DEFINE v_ax_tpo_credito          SMALLINT;
   DEFINE v_inconsistencia          CHAR(2);
   DEFINE v_subproceso              CHAR(3);
   DEFINE v_f_vigencia              DATE;   -- CHAR(8);
   DEFINE v_f_otorga_cred_ef        CHAR(8);
   DEFINE v_ax_micipio_notario      CHAR(5);
   DEFINE v_nss                     CHAR(11);
   DEFINE v_nss2                    CHAR(11);
   DEFINE v_uni_nss                 CHAR(11);
   DEFINE v_num_rpp                 CHAR(15);
   DEFINE v_num_ctrl_ef             CHAR(18);
   DEFINE v_ax_num_ctr              CHAR(18);
   DEFINE v_tasa_base               CHAR(20);
   DEFINE v_margen                  CHAR(20);
   DEFINE v_usuario_reg_carta       CHAR(20);
   DEFINE v_domicilio_inmueble      CHAR(30);
   DEFINE v_f_registro_carta        DATE;
   DEFINE v_f_proceso               DATE;
   DEFINE v_f_respuesta             DATE;
   DEFINE v_tpo_registro            DECIMAL(1,0);
   DEFINE v_tpo_moneda              DECIMAL(2,0);
   DEFINE v_diagnostico             DECIMAL(2,0);
   DEFINE v_ent_fed_inmueble        DECIMAL(2,0);
   DEFINE v_ent_fed_notario         DECIMAL(2,0);
   DEFINE v_municipio_notario       DECIMAL(3,0);
   DEFINE v_cve_ent_financiera      DECIMAL(3,0);
   DEFINE v_cve_ent_financiera2     DECIMAL(3,0);
   DEFINE v_ax_cve_ent_financiera   DECIMAL(3,0);
   DEFINE v_num_notario             DECIMAL(4,0);
   DEFINE v_mcpio_inmueble          DECIMAL(5,0);
   DEFINE v_plazo_credito           DECIMAL(5,0);
   DEFINE v_partida                 DECIMAL(6,0);
   DEFINE v_volumen                 DECIMAL(6,0);
   DEFINE v_libro                   DECIMAL(6,0);
   DEFINE v_tomo                    DECIMAL(6,0);
   DEFINE v_seccion                 DECIMAL(6,0);
   DEFINE v_foja                    DECIMAL(8,0);
   DEFINE v_num_escritura           CHAR(8);  --DECIMAL(8,0);
   DEFINE v_folio_real              DECIMAL(8,0);
   DEFINE v_ax_id_ocg_ctr_arch      DECIMAL(9,0);
   DEFINE v_id_dh                   DECIMAL(9,0);
   DEFINE v_id_ocg_detalle          DECIMAL(9,0);
   DEFINE v_uni_id_dh               DECIMAL(9,0);
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0);
   DEFINE v_pesos_viv97             DECIMAL(12,2);
   DEFINE v_valor_avaluo            DECIMAL(15,2);
   DEFINE v_monto_credito           DECIMAL(15,2);
   DEFINE v_viv97                   DECIMAL(16,6);
   DEFINE v_precio_fondo            DECIMAL(19,14);
   DEFINE v_cnt_aceptados           INTEGER;
   DEFINE v_cnt_rechazados          INTEGER;
   DEFINE v_tot_regs                INTEGER;
   DEFINE v_cnt_vig                 SMALLINT;         -- Contador para créditos vigentes o en trámite 43 bis
   DEFINE v_ind_edo_cuenta          SMALLINT;
   DEFINE v_si_cre_vigente          SMALLINT;
   DEFINE v_cnt_cr_tram             SMALLINT;
   DEFINE v_error                   SMALLINT;
   DEFINE v_bnd_inconsistencia      SMALLINT;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_ax_edo                  SMALLINT;
   DEFINE v_idx_nss                 SMALLINT;
   DEFINE v_situacion               SMALLINT;
   DEFINE v_valida                  SMALLINT;
   DEFINE v_resultado               SMALLINT;
   DEFINE v_tpo_originacion         SMALLINT;
   --Variables para verificar duplicados
   DEFINE v_dup_id_dh               DECIMAL(9,0);
   DEFINE v_ax_id_ocg_formal        DECIMAL(9,0);
   DEFINE v_dup_cnt                 INTEGER;
   DEFINE v_ax_cnt_dup              INTEGER;
   -- Variables auxiliares para la ejecución de fn_edo_cred_viv 
   DEFINE v_f_otorga                DATE;
   DEFINE v_f_liquida               DATE;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_tpo_dscto               SMALLINT;
   DEFINE v_rfc                     CHAR(13);
   DEFINE v_ax_rfc                  CHAR(13);
   DEFINE v_curp                    CHAR(18);
   DEFINE v_ax_curp                 CHAR(18);
   DEFINE v_ap_paterno_af           CHAR(40);
   DEFINE v_ap_materno_af           CHAR(40);
   DEFINE v_nombre_af               CHAR(40);
   DEFINE v_ax_ap_paterno_af        CHAR(40);
   DEFINE v_ax_ap_materno_af        CHAR(40);
   DEFINE v_ax_nombre_af            CHAR(40);
   -- Varibles para las validaciones del nombre
   DEFINE v_pos_paterno             SMALLINT;
   DEFINE v_pos_materno             SMALLINT;
   DEFINE v_pos_nombre              SMALLINT;
   DEFINE v_b_pos_paterno           SMALLINT;
   DEFINE v_b_pos_materno           SMALLINT;
   DEFINE v_b_pos_nombre            SMALLINT;
   DEFINE v_bandera_posiscion       SMALLINT;
   DEFINE v_f_saldo                 DATE;
   DEFINE v_genero                  CHAR(1);
   DEFINE v_id_ocg_detalle2         DECIMAL(9,0);
   DEFINE v_bnd_rch_cofi            SMALLINT;
   DEFINE v_bnd_nss                 SMALLINT;

   DEFINE v_nss_unificado           CHAR(11);
   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_diag                    SMALLINT;


   ON EXCEPTION SET v_error
      LET v_cnt_aceptados  = 0;
      LET v_cnt_rechazados = 0;
      RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_valida_subpr2_formalizacion.trace';
   TRACE ON;

   SELECT precio_fondo 
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   LET v_error              = 0;
   LET v_subproceso         = "002";
   LET v_cnt_vig            = 0;
   LET v_f_proceso          = TODAY;
   LET v_ax_id_ocg_ctr_arch = p_id_ocg_ctr_arch;
   LET v_cnt_aceptados      = 0;
   LET v_cnt_rechazados     = 0;
   LET v_dup_cnt            = 0;
   LET v_ax_cnt_dup         = 0;
   LET v_f_registro_carta   = NULL;
   LET v_mcpio_inmueble     = NULL;
   LET v_usuario_reg_carta  = NULL;
   LET v_bnd_inconsistencia = 0;
   LET v_valida             = 0;
   LET v_bandera_posiscion  = 0;
 --  LET v_f_respuesta        = fn_calcula_habil_siguiente(v_f_proceso);
   LET v_f_saldo            = TODAY;
   LET v_genero             = "";
   LET v_f_respuesta        = NULL;
   LET v_nss_unificado      = "";
   LET v_nss_unificador     = "";
   LET v_id_dh_unificador   = "";
   LET v_diag               = 0;
   LET v_tpo_credito        = "";

   FOREACH
      SELECT cve_ent_financiera ,
             nss                ,
             rfc                ,
             curp               ,
             ap_paterno_af      ,
             ap_materno_af      ,
             nombre_af          ,
             num_ctrl_ef        ,
             num_escritura      ,
             tpo_registro       ,
             tpo_envio          ,
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
             (valor_avaluo/100)       ,
             (monto_credito/100)      ,
             plazo_credito      ,
             tpo_moneda         ,
             tasa_base          ,
             margen             ,
             cred_convenidos    ,
             f_otorga_cred_ef[5,6] || f_otorga_cred_ef[7,8] || f_otorga_cred_ef[1,4]
        INTO v_cve_ent_financiera ,
             v_nss                ,
             v_rfc                ,
             v_curp               ,
             v_ap_paterno_af      ,
             v_ap_materno_af      ,
             v_nombre_af          ,
             v_num_ctrl_ef        ,
             v_num_escritura      ,
             v_tpo_registro       ,
             v_tpo_envio          ,
             v_num_notario        ,
             v_ent_fed_notario    ,
             v_municipio_notario  ,
             v_num_rpp            ,
             v_folio_real         ,
             v_partida            ,
             v_foja               ,
             v_volumen            ,
             v_libro              ,
             v_tomo               ,
             v_seccion            ,
             v_ent_fed_inmueble   ,
             v_domicilio_inmueble ,
             v_valor_avaluo       ,
             v_monto_credito      ,
             v_plazo_credito      ,
             v_tpo_moneda         ,
             v_tasa_base          ,
             v_margen             ,
             v_tpo_credito        ,
             v_f_otorga_cred_ef
        FROM safre_tmp:tmp_rec_det_ocg43
       WHERE subproceso = v_subproceso

      LET v_bnd_inconsistencia = 0;
      --LET v_estado             = 50;
      LET v_situacion          = 50;
      --LET v_situacion          = 10;
      LET v_estado             = 10;
      LET v_diagnostico        = 01;      -- Se asume que será aceptado
     -- LET v_valor_avaluo       = v_valor_avaluo / 100;
     -- LET v_monto_credito      = v_monto_credito / 100;
      LET v_bnd_rch_cofi       = 0;

      ---IF v_tpo_credito = "" OR v_tpo_credito = " " THEN
         ---LET v_tpo_credito = "A";
      ---END IF

      IF v_tpo_credito <> "7" AND v_tpo_credito <> "8" AND v_tpo_credito <> "C" THEN
         LET v_tpo_credito = "A";
      END IF

      IF v_tpo_credito = " " OR v_tpo_credito = "" OR v_tpo_credito = "0" THEN
         LET v_tpo_credito = "A";
      END IF

      -- Validación temporal 
--      IF v_f_otorga_cred_ef IS NULL THEN 
--         LET v_f_otorga_cred_ef = v_f_proceso;
--      END IF

      -- Se asigna a la variable el valor de la secuencia
      LET v_id_ocg_detalle       = seq_ocg_detalle.nextval;
      LET v_id_ocg_formalizacion = seq_ocg_formalizacion.nextval;

      -- Se obtiene el id_derechohabiente 
      SELECT id_derechohabiente,
             tipo_trabajador,
             ind_estado_cuenta
        INTO v_id_dh,
             v_tpo_trabajador,
             v_ind_edo_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      -- Se verifica si la cuenta es inhabilitada por unificación  #4
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
         END IF
      END IF

      IF v_valor_avaluo > 99999999.99 THEN
      
         LET v_inconsistencia = "35";
         LET v_bnd_inconsistencia = 1;

         IF(v_tpo_credito = "7") OR
           (v_tpo_credito = "8") THEN
            LET v_bnd_rch_cofi       = 1;
            LET v_situacion          = 20;
            LET v_estado             = 60;
            LET v_pesos_viv97        = 0;
            LET v_f_vigencia         = NULL;
         END IF

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_monto_credito > 99999999.99 THEN

         LET v_inconsistencia = "36";
         LET v_bnd_inconsistencia = 1;

         IF(v_tpo_credito = "7") OR
           (v_tpo_credito = "8") THEN
            LET v_bnd_rch_cofi       = 1;
            LET v_situacion          = 20;
            LET v_estado             = 60;
            LET v_pesos_viv97        = 0;
            LET v_f_vigencia         = NULL;
         END IF


         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      LET v_cve_ent_financiera2 = "";
      LET v_tpo_credito2        = "";
      LET v_id_ocg_detalle2     = "";
      LET v_bnd_nss             = 0;
{
      IF (v_tpo_credito = 7) OR (v_tpo_credito = 8) THEN

      -- Se recupera el id_ocg_tramite
      SELECT id_ocg_tramite,
             cve_ent_financiera,
             tpo_credito,
             id_ocg_detalle
        INTO v_id_ocg_tramite,
             v_cve_ent_financiera2,
             v_tpo_credito2,
             v_id_ocg_detalle2
        FROM ocg_tramite
       WHERE id_derechohabiente = v_id_dh
         AND situacion   = 30
         AND diagnostico = 1;
      ELSE}
          SELECT id_ocg_tramite,
             cve_ent_financiera,
             tpo_credito,
             id_ocg_detalle
        INTO v_id_ocg_tramite,
             v_cve_ent_financiera2,
             v_tpo_credito2,
             v_id_ocg_detalle2
        FROM ocg_tramite
       WHERE id_derechohabiente = v_id_dh
         AND situacion   = 50
         AND diagnostico = 1;
 --     END IF

     IF (v_tpo_credito = "7") OR
        (v_tpo_credito = "8") THEN

        IF EXISTS (SELECT id_ocg_formalizacion
                     FROM ocg_formalizacion
                    WHERE id_derechohabiente = v_id_dh
                      AND situacion IN (50,55,60,70,80)) THEN

           LET v_inconsistencia    = "17";
           LET v_bnd_inconsistencia = 1;
           LET v_id_ocg_tramite     = 0;
           LET v_situacion          = 20;
           LET v_estado             = 60;
           LET v_pesos_viv97        = 0;
           LET v_f_vigencia         = NULL;
           LET v_bnd_rch_cofi       = 1;

           INSERT INTO ocg_inconsistencia
                VALUES(v_id_ocg_formalizacion,
                       v_subproceso,
                       v_inconsistencia,
                       v_f_proceso );
        END IF

 --TRACE 'NSS : ' || v_nss;
        SELECT nss
          INTO v_nss2
          FROM ocg_detalle
         WHERE id_ocg_detalle = v_id_ocg_detalle2;

  --TRACE 'NSS2 : ' || v_nss2;

        IF (v_nss <> v_nss2) OR 
           (v_nss2 IS NULL) THEN

           LET v_inconsistencia    = "25";
           LET v_bnd_inconsistencia = 1;
           LET v_id_ocg_tramite     = 0;
           LET v_situacion          = 20;
           LET v_estado             = 60;
           LET v_pesos_viv97        = 0;
           LET v_f_vigencia         = NULL;
           LET v_bnd_rch_cofi       = 1;
           LET v_bnd_nss            = 1;

           INSERT INTO ocg_inconsistencia
                VALUES(v_id_ocg_formalizacion,
                       v_subproceso,
                       v_inconsistencia,
                       v_f_proceso );

        END IF

        IF v_bnd_nss = 0 THEN
  --TRACE 'v_bnd_rch_cofi : ' || v_bnd_rch_cofi;

           IF (v_tpo_credito2 <> v_tpo_credito) OR
              (v_tpo_credito2 IS NULL) THEN

              LET v_inconsistencia     = "94";
              LET v_bnd_inconsistencia = 1;
              LET v_id_ocg_tramite     = 0;
              LET v_situacion          = 20;
              LET v_estado             = 60;
              LET v_pesos_viv97        = 0;
              LET v_f_vigencia         = NULL;
              LET v_bnd_rch_cofi       = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES(v_id_ocg_formalizacion,
                          v_subproceso,
                          v_inconsistencia,
                          v_f_proceso );

           END IF

           IF v_cve_ent_financiera IS NULL THEN
           
              LET v_inconsistencia     = "26";
              LET v_bnd_inconsistencia = 1;
              LET v_id_ocg_tramite     = 0;
              LET v_situacion          = 20;
              LET v_estado             = 60;
              LET v_pesos_viv97        = 0;
              LET v_f_vigencia         = NULL;
              LET v_bnd_rch_cofi       = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES(v_id_ocg_formalizacion,
                          v_subproceso,
                          v_inconsistencia,
                          v_f_proceso );

           END IF

           IF v_cve_ent_financiera IS NOT NULL THEN
              IF (v_cve_ent_financiera2 <> v_cve_ent_financiera) OR 
                 (v_cve_ent_financiera2 IS NULL) THEN
              
                 LET v_inconsistencia     = "26";
                 LET v_bnd_inconsistencia = 1;
                 LET v_id_ocg_tramite     = 0;
                 LET v_situacion          = 20;
                 LET v_estado             = 60;
                 LET v_pesos_viv97        = 0;
                 LET v_f_vigencia         = NULL;
                 LET v_bnd_rch_cofi       = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES(v_id_ocg_formalizacion,
                          v_subproceso,
                          v_inconsistencia,
                          v_f_proceso );

              END IF
           END IF

           IF v_num_ctrl_ef IS NULL THEN

              LET v_inconsistencia     = "29";
              LET v_bnd_inconsistencia = 1;
              LET v_id_ocg_tramite     = 0;
              LET v_situacion          = 20;
              LET v_estado             = 60;
              LET v_pesos_viv97        = 0;
              LET v_f_vigencia         = NULL;
              LET v_bnd_rch_cofi       = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES(v_id_ocg_formalizacion,
                          v_subproceso,
                          v_inconsistencia,
                          v_f_proceso );

           END IF 

        END IF

        IF v_bnd_rch_cofi = 1 THEN
           LET v_diagnostico = 02;

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
              VALUES(v_id_ocg_formalizacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     2,
                     v_f_proceso);

           -- Se inserta en la tabla ocg_formalizacion
            INSERT INTO ocg_formalizacion
                 VALUES( v_id_ocg_formalizacion,
                         v_id_ocg_detalle,
                         v_id_ocg_tramite,
                         v_id_dh,
                         v_cve_ent_financiera,
                         v_num_ctrl_ef,
                         v_rfc,
                         v_curp,
                         v_ap_paterno_af,
                         v_ap_materno_af,
                         v_nombre_af,
                         v_pesos_viv97,
                         v_num_escritura,
                         v_num_notario,
                         v_ent_fed_notario,
                         v_municipio_notario,
                         v_num_rpp,
                         v_folio_real,
                         v_partida,
                         v_foja,
                         v_volumen,
                         v_libro,
                         v_tomo,
                         v_seccion,
                         v_ent_fed_inmueble,
                         v_mcpio_inmueble,
                         v_domicilio_inmueble,
                         v_valor_avaluo,
                         v_monto_credito,
                         v_plazo_credito,
                         v_tpo_moneda,
                         v_tasa_base,
                         v_margen,
                         v_tpo_credito,
                         v_f_otorga_cred_ef,
                         v_f_registro_carta,
                         v_diagnostico,
                         v_estado,
                         v_usuario_reg_carta,
                         v_situacion,
                         v_f_vigencia,
                         v_f_respuesta,
                         v_f_saldo,
                         v_genero );

         CONTINUE FOREACH;
        ELSE
            LET v_situacion          = 55;
            LET v_estado             = 20;
            LET v_pesos_viv97        = 0;
            LET v_f_vigencia         = (TODAY + 1 UNITS YEAR);
            LET v_f_registro_carta   = TODAY;
           SELECT rfc,
                  curp,
                  ap_paterno_af,
                  ap_materno_af,
                  nombre_af
             INTO v_rfc,
                  v_curp,
                  v_ap_paterno_af,
                  v_ap_materno_af,
                  v_nombre_af     
             FROM afi_derechohabiente
            WHERE id_derechohabiente = v_id_dh;

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
              VALUES(v_id_ocg_formalizacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     2,
                     v_f_proceso);

           -- Se inserta en la tabla ocg_formalizacion
         INSERT INTO ocg_formalizacion
              VALUES( v_id_ocg_formalizacion,
                      v_id_ocg_detalle,
                      v_id_ocg_tramite,
                      v_id_dh,
                      v_cve_ent_financiera,
                      v_num_ctrl_ef,
                      v_rfc,
                      v_curp,
                      v_ap_paterno_af,
                      v_ap_materno_af,
                      v_nombre_af,
                      v_pesos_viv97,
                      v_num_escritura,
                      v_num_notario,
                      v_ent_fed_notario,
                      v_municipio_notario,
                      v_num_rpp,
                      v_folio_real,
                      v_partida,
                      v_foja,
                      v_volumen,
                      v_libro,
                      v_tomo,
                      v_seccion,
                      v_ent_fed_inmueble,
                      v_mcpio_inmueble,
                      v_domicilio_inmueble,
                      v_valor_avaluo,
                      v_monto_credito,
                      v_plazo_credito,
                      v_tpo_moneda,
                      v_tasa_base,
                      v_margen,
                      v_tpo_credito,
                      v_f_otorga_cred_ef,
                      v_f_registro_carta,
                      v_diagnostico,
                      v_estado,
                      v_usuario_reg_carta,
                      v_situacion,
                      v_f_vigencia,
                      v_f_respuesta,
                      v_f_saldo,
                      v_genero );

         INSERT INTO ocg_acreditado
              VALUES (v_id_ocg_formalizacion,
                      TODAY,
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      20,
                      55);

         UPDATE ocg_tramite
            SET situacion          = 60
          WHERE id_ocg_tramite     = v_id_ocg_tramite
            AND id_derechohabiente = v_id_dh
            AND situacion          = 50
            AnD diagnostico        = 1;

         CONTINUE FOREACH;
        END IF
     END IF

      -- Se verifica que exista en la base de derechohabientes
      IF v_id_dh < 0 OR v_id_dh IS NULL THEN
         LET v_inconsistencia     = "02";
         LET v_bnd_inconsistencia = 1;
         LET v_id_dh              = 0;
         LET v_id_ocg_tramite     = 0;
         --LET v_estado             = 20;
         LET v_situacion          = 20;
         LET v_estado             = 60;
         LET v_pesos_viv97        = 0;
         LET v_f_vigencia         = NULL;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
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
              VALUES(v_id_ocg_formalizacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     2,
                     v_f_proceso);

         -- Se inserta en la tabla ocg_formalizacion
         INSERT INTO ocg_formalizacion
              VALUES( v_id_ocg_formalizacion,
                      v_id_ocg_detalle,
                      v_id_ocg_tramite,
                      v_id_dh,
                      v_cve_ent_financiera,
                      v_num_ctrl_ef,
                      v_rfc,
                      v_curp,
                      v_ap_paterno_af,
                      v_ap_materno_af,
                      v_nombre_af,
                      v_pesos_viv97,
                      v_num_escritura,
                      v_num_notario,
                      v_ent_fed_notario,
                      v_municipio_notario,
                      v_num_rpp,
                      v_folio_real,
                      v_partida,
                      v_foja,
                      v_volumen,
                      v_libro,
                      v_tomo,
                      v_seccion,
                      v_ent_fed_inmueble,
                      v_mcpio_inmueble,
                      v_domicilio_inmueble,
                      v_valor_avaluo,
                      v_monto_credito,
                      v_plazo_credito,
                      v_tpo_moneda,
                      v_tasa_base,
                      v_margen,
                      v_tpo_credito,
                      v_f_otorga_cred_ef,
                      v_f_registro_carta,
                      v_diagnostico,
                      v_estado,
                      v_usuario_reg_carta,
                      v_situacion,
                      v_f_vigencia,
                      v_f_respuesta,
                      v_f_saldo,
                      v_genero );

         CONTINUE FOREACH;
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

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_formalizacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     2,
                     v_f_proceso);

      -- Si el NSS es incorrecto
      FOR v_idx_nss = 1 TO LENGTH(v_nss)
         IF SUBSTR(v_nss,v_idx_nss,1) NOT MATCHES '[0-9]' THEN
            --TRACE 'El nss no cumple con la valiacón: ' || v_nss;
            --TRACE 'En la posición : ' || v_idx_nss;

            LET v_inconsistencia = "02";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_formalizacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
            EXIT FOR;
         ELSE
            CONTINUE FOR;
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

      LET v_pos_paterno   = INSTR(v_ap_paterno_af,'Ñ');
      LET v_pos_materno   = INSTR(v_ap_materno_af,'Ñ');
      LET v_pos_nombre    = INSTR(v_nombre_af,'Ñ');
      LET v_b_pos_paterno = INSTR(v_ap_paterno_af,'#');
      LET v_b_pos_materno = INSTR(v_ap_materno_af,'#');
      LET v_b_pos_nombre  = INSTR(v_nombre_af,'#');

      -- valida si encuentra Ñ o numeral en la cadena del ap_paterno
      IF v_pos_paterno > 0 THEN 
         LET v_ax_ap_paterno_af = REPLACE(v_ax_ap_paterno_af, SUBSTR(v_ax_ap_paterno_af,v_pos_paterno,1), 'Ñ');
      ELIF v_b_pos_paterno > 0 THEN
         LET v_ax_ap_paterno_af = REPLACE(v_ax_ap_paterno_af, SUBSTR(v_ax_ap_paterno_af,v_b_pos_paterno,1), '#');
      END IF

      -- valida si encuentra Ñ o numeral en la cadena del ap_materno
      IF v_pos_materno > 0 THEN 
         LET v_ax_ap_materno_af = REPLACE(v_ax_ap_materno_af, SUBSTR(v_ax_ap_materno_af,v_pos_materno,1), 'Ñ');
      ELIF v_b_pos_materno > 0 THEN
         LET v_ax_ap_materno_af = REPLACE(v_ax_ap_materno_af, SUBSTR(v_ax_ap_materno_af,v_b_pos_materno,1), '#');
      END IF

      -- valida si encuentra Ñ o numeral en la cadena del nombre
      IF v_pos_nombre > 0 THEN 
         LET v_ax_nombre_af = REPLACE(v_ax_nombre_af, SUBSTR(v_ax_nombre_af,v_pos_nombre,1), 'Ñ');
      ELIF v_b_pos_nombre > 0 THEN
         LET v_ax_nombre_af = REPLACE(v_ax_nombre_af, SUBSTR(v_ax_nombre_af,v_b_pos_nombre,1), '#');
      END IF

      LET v_ap_paterno_af = v_ax_ap_paterno_af;
      LET v_ap_materno_af = v_ax_ap_materno_af;
      LET v_nombre_af     = v_ax_nombre_af;

{

      IF v_ax_ap_paterno_af <> v_ap_paterno_af OR v_ap_paterno_af IS NULL OR 
         v_ax_ap_materno_af <> v_ap_materno_af OR v_ap_materno_af IS NULL OR 
         v_ax_nombre_af <> v_nombre_af OR v_nombre_af IS NULL THEN

         LET v_inconsistencia = "04";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
         LET v_inconsistencia = "19";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "24";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
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
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_ax_curp <> v_curp OR v_curp IS NULL THEN
         LET v_curp = v_ax_curp;

         -- Se inserta inconsistencia pero no se rechaza
         LET v_inconsistencia = "12";

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF 

      -- Se verifica que el nss sea tpo_trabajador ="I"
      IF v_tpo_trabajador <> "I" THEN
         LET v_inconsistencia     = "01";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      -- Se verifica que el ind_edo_cuenta = 0
      IF v_ind_edo_cuenta <> 0 THEN
         LET v_inconsistencia = "43"; --"03";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica que el nss no tenga un credito 43 bis vigente o en trámite
      SELECT COUNT(*)
        INTO v_cnt_vig
        FROM cre_acreditado a,
             afi_derechohabiente b,
             cat_maq_credito c
       WHERE a.id_derechohabiente = b.id_derechohabiente
         AND a.estado = c.estado
         AND a.tpo_originacion = 2
         AND a.estado  = 18
         AND c.entidad = 1 
         AND b.nss = v_nss;

      IF v_cnt_vig > 0 THEN
         LET v_inconsistencia = "01";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      -- Se verifica que no tenga un crédito vigente       6
      LET v_resultado       = "";
      LET v_tpo_originacion = "";
      LET v_ax_tpo_credito  = "";
      LET v_num_credito     = "";
      LET v_f_otorga        = "";
      LET v_f_liquida       = "";
      LET v_tpo_dscto       = "";
      --TRACE 'v_num_credito : ' ||v_num_credito;
      CALL fn_edo_cred_viv(v_id_dh, v_valida)
      RETURNING v_resultado, v_tpo_originacion, v_ax_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;
      --TRACE 'v_num_credito : ' ||v_num_credito;

      IF v_resultado = 0 THEN
         IF v_ax_tpo_credito <> 2 THEN
            LET v_inconsistencia = "14";
         ELSE
            LET v_inconsistencia = "17";
         END IF

         LET v_bnd_inconsistencia = 1;
         --TRACE 'Tiene credito vigente nss : ' || v_nss;
         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );   
      END IF

      -- Se verifica que no tenga un crédito en trámite    #7
      IF EXISTS ( SELECT id_derechohabiente
                    FROM afi_derechohabiente
                   WHERE nss = v_nss
                     AND id_derechohabiente IN (
                         SELECT id_derechohabiente
                           FROM sfr_marca_activa
                          WHERE marca = 213 ) 
                ) THEN

         LET v_inconsistencia = "15";

         IF EXISTS (
                    SELECT id_ocg_tramite
                      FROM ocg_tramite
                     WHERE id_derechohabiente = v_id_dh
                       AND cve_ent_financiera = v_cve_ent_financiera
                       AND situacion = 50) THEN

            LET v_inconsistencia = "";
         END IF


      END IF

{
      -- Se verifica que el tipo de crédito sea "C" en caso contrario se asigna "A"
      IF v_tpo_credito = 'C' OR v_tpo_credito = '7' OR v_tpo_credito = '8' THEN
         LET v_tpo_credito = v_tpo_credito;
      ELSE
         LET v_tpo_credito = 'A';
      END IF;
}
{
      IF (v_tpo_credito = 7) OR (v_tpo_credito= 8) THEN

         -- Se recupera el id_ocg_tramite
         SELECT id_ocg_tramite,
                cve_ent_financiera
           INTO v_id_ocg_tramite,
                v_ax_cve_ent_financiera
           FROM ocg_tramite
          WHERE id_derechohabiente = v_id_dh
            AND situacion = 30
            AND diagnostico = 1;
      ELSE}
          -- Se recupera el id_ocg_tramite
         SELECT id_ocg_tramite,
                cve_ent_financiera
           INTO v_id_ocg_tramite,
                v_ax_cve_ent_financiera
           FROM ocg_tramite
          WHERE id_derechohabiente = v_id_dh
            AND situacion = 50
            AND diagnostico = 1;
--      END IF

      IF v_id_ocg_tramite IS NULL OR v_id_ocg_tramite = "" THEN 
         LET v_id_ocg_tramite = 0;   -- Pendiente  se usa el id para probar
         LET v_inconsistencia = "25";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se valida que la entidad financiera exista en el catálogo de EF
      IF EXISTS ( SELECT cve_ent_financiera
                    FROM cat_entidad_financiera
                   WHERE cve_ent_financiera = v_cve_ent_financiera ) THEN

         IF v_cve_ent_financiera <> v_ax_cve_ent_financiera OR v_cve_ent_financiera IS NULL THEN
            LET v_inconsistencia = "26";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_formalizacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF;
      ELSE
         LET v_inconsistencia = "26";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
{
      FOR v_idx_nss = 1 TO LENGTH(v_num_ctrl_ef)
         IF SUBSTR(v_num_ctrl_ef,v_idx_nss,1) NOT MATCHES '[0-9]' OR 
            SUBSTR(v_num_ctrl_ef,v_idx_nss,1) NOT MATCHES '[A-Z]' THEN

            LET v_inconsistencia = "29";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_formalizacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );

            LET v_bandera_posiscion = 1;
            EXIT FOR;
         ELSE
            CONTINUE FOR;
         END IF
      END FOR;
      }

      --IF v_bandera_posiscion = 0 THEN 
         IF v_num_ctrl_ef IS NULL THEN 
            LET v_inconsistencia = "29";
            LET v_bnd_inconsistencia = 1;
         
            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_formalizacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF;
      --END IF 

      IF v_num_escritura IS NULL THEN 
         LET v_inconsistencia = "30";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_num_notario IS NULL THEN 
         LET v_inconsistencia = "31";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      LET v_ax_micipio_notario = v_ent_fed_notario || LPAD(v_municipio_notario,3,"0");

      --TRACE "MUNICIPIO : " || v_municipio_notario ;
      --TRACE "AUX : " || v_ax_micipio_notario ;
      IF NOT EXISTS ( SELECT entidad_federativa
                        FROM cat_entidad_federativa
                       WHERE entidad_federativa = v_ent_fed_notario ) OR 
         NOT EXISTS ( SELECT municipio
                        FROM cat_municipio_inegi
                       WHERE entidad_federativa = v_ent_fed_notario
                         AND municipio = v_ax_micipio_notario ) THEN

         LET v_inconsistencia = "32";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF NOT EXISTS ( SELECT entidad_federativa
                        FROM cat_entidad_federativa
                       WHERE entidad_federativa = v_ent_fed_inmueble ) THEN

         LET v_inconsistencia = "33";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_domicilio_inmueble IS NULL THEN

         LET v_inconsistencia = "34";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_valor_avaluo IS NULL OR v_valor_avaluo <= 0 THEN

         LET v_inconsistencia = "35";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_monto_credito IS NULL OR v_monto_credito <= 0 THEN

         LET v_inconsistencia = "36";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF v_plazo_credito IS NULL THEN

         LET v_inconsistencia = "37";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF


      IF NOT EXISTS ( SELECT tpo_moneda
                        FROM cat_tipo_moneda 
                       WHERE tpo_moneda = v_tpo_moneda ) THEN

         LET v_inconsistencia = "38";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_tasa_base IS NULL THEN

         LET v_inconsistencia = "39";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_margen IS NULL THEN

         LET v_inconsistencia = "40";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      IF v_f_otorga_cred_ef IS NULL OR v_f_otorga_cred_ef = "" THEN

         LET v_inconsistencia = "41";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      -- valida que no tenga un proceso de formalización previo vigente 
      IF EXISTS ( SELECT id_ocg_formalizacion
                    FROM ocg_formalizacion
                   WHERE id_derechohabiente = v_id_dh 
                     AND situacion IN (50,55,60,70,80) ) THEN

         LET v_inconsistencia = "17";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_formalizacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

      END IF

      SELECT SUM(monto_acciones)
        INTO v_viv97
        FROM cta_movimiento c,
             afi_derechohabiente ad
       WHERE nss = v_nss
         AND subcuenta = 4
         AND fondo_inversion = 11
         AND c.id_derechohabiente = ad.id_derechohabiente;

      LET v_pesos_viv97 = ROUND((v_precio_fondo * v_viv97),2);

      IF v_bnd_inconsistencia = 1 THEN 
         LET v_diagnostico = 02;
         --LET v_estado = 20;
         LET v_situacion  = 20;
         LET v_estado = 60;
         LET v_cnt_rechazados = v_cnt_rechazados + 1;
         LET v_f_vigencia = NULL;
      ELSE
         LET v_cnt_aceptados = v_cnt_aceptados + 1;
         --LET v_situacion = 10;
         LET v_estado     = 10;
         LET v_f_vigencia = TODAY + 1 UNITS YEAR;
      END IF

      --LET v_1stado = 20;  -- Se pone en estado diagnosticado

      --TRACE 'v_f_otorga_cred_ef : ' || v_f_otorga_cred_ef;
      --TRACE 'v_f_registro_carta : ' || v_f_registro_carta;
      --TRACE 'v_f_vigencia : ' || v_f_vigencia;
      --TRACE 'v_f_respuesta : ' || v_f_respuesta;
      --TRACE 'v_f_saldo : ' || v_f_saldo;

      -- Se inserta en la tabla ocg_formalizacion
      INSERT INTO ocg_formalizacion
           VALUES(v_id_ocg_formalizacion,
                      v_id_ocg_detalle,
                      v_id_ocg_tramite,
                      v_id_dh,
                      v_cve_ent_financiera,
                      v_num_ctrl_ef,
                      v_rfc,
                      v_curp,
                      v_ap_paterno_af,
                      v_ap_materno_af,
                      v_nombre_af,
                      v_pesos_viv97,
                      v_num_escritura,
                      v_num_notario,
                      v_ent_fed_notario,
                      v_municipio_notario,
                      v_num_rpp,
                      v_folio_real,
                      v_partida,
                      v_foja,
                      v_volumen,
                      v_libro,
                      v_tomo,
                      v_seccion,
                      v_ent_fed_inmueble,
                      v_mcpio_inmueble,
                      v_domicilio_inmueble,
                      v_valor_avaluo,
                      v_monto_credito,
                      v_plazo_credito,
                      v_tpo_moneda,
                      v_tasa_base,
                      v_margen,
                      v_tpo_credito,
                      v_f_otorga_cred_ef,
                      v_f_registro_carta,
                      v_diagnostico,
                      v_estado,
                      v_usuario_reg_carta,
                      v_situacion,
                      v_f_vigencia,
                      v_f_respuesta,
                      v_f_saldo,
                      v_genero );

   LET v_nss_unificado      = "";
   LET v_nss_unificador     = "";
   LET v_id_dh_unificador   = "";
   LET v_diag               = 0;
   LET v_tpo_credito        = "";

   END FOREACH;
{
   -- Se verifica si existen registros duplicados y se eliminan
   FOREACH
      SELECT id_derechohabiente,
             COUNT(*)
        INTO v_dup_id_dh,
             v_dup_cnt
        FROM ocg_formalizacion
      GROUP BY 1
      HAVING COUNT(*) > 1
      ORDER BY 2 DESC

      LET v_ax_cnt_dup = v_dup_cnt;

      IF v_dup_cnt > 1 THEN
         FOREACH
            SELECT id_ocg_formalizacion,
                   estado
              INTO v_ax_id_ocg_formal,
                   v_ax_edo
              FROM ocg_formalizacion
             WHERE id_derechohabiente = v_dup_id_dh

            LET v_ax_cnt_dup = v_ax_cnt_dup - 1;

            IF v_ax_cnt_dup <> 1 THEN
               DELETE 
                 FROM ocg_formalizacion
                WHERE id_ocg_formalizacion = v_ax_id_ocg_formal;

               DELETE
                 FROM ocg_inconsistencia
                WHERE id_ocg_referencia = v_ax_id_ocg_formal;

               IF v_ax_edo = 0 THEN 
                  LET v_cnt_aceptados = v_cnt_aceptados - 1;
               ELIF v_ax_edo = 1 THEN 
                  LET v_cnt_rechazados = v_cnt_rechazados - 1;
               END IF;
            ELSE
               CONTINUE FOREACH;
            END IF
         END FOREACH;
      END IF
   END FOREACH;
}
   -- Se actualiza el contador de la tabla de control de archisos
   LET v_tot_regs = v_cnt_aceptados + v_cnt_rechazados;

   UPDATE ocg_ctr_archivo
      SET tot_sp2 = v_tot_regs
    WHERE id_ocg_ctr_archivo = p_id_ocg_ctr_arch;

   -- Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_formalizacion;
   UPDATE STATISTICS FOR TABLE ocg_inconsistencia;
   UPDATE STATISTICS FOR TABLE ocg_detalle;

   RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
END FUNCTION;


