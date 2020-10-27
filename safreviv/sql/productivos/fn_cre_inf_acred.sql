






CREATE FUNCTION "safreviv".fn_cre_inf_acred()

   RETURNING SMALLINT, INTEGER, VARCHAR(250);

   DEFINE v_cod_error               SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);

   DEFINE v_nss                     CHAR(11);
   DEFINE v_rfc                     CHAR(13);
   DEFINE v_curp                    CHAR(18);
   DEFINE v_ap_paterno_af           CHAR(40);
   DEFINE v_ap_materno_af           CHAR(40);
   DEFINE v_nombre_af               CHAR(40);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_tpo_originacion         SMALLINT;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_f_otorga                DATE;
   DEFINE v_edo_procesar            SMALLINT;
   DEFINE v_entidad                 SMALLINT;

   ON EXCEPTION SET v_cod_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_cod_error, v_isam_err, v_c_msj;
   END EXCEPTION

   -- se establece la prioridad
   --SET PDQPRIORITY HIGH;

   --SET DEBUG FILE TO '/safreviv_int/archivos/infAcred.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_cod_error = 0;
   LET v_isam_err  = 0;
   LET v_c_msj     = 'El proceso finalizó correctamente';

   CREATE TEMP TABLE tmp_reg_id
   (id_derechohabiente DECIMAL(9,0));

   INSERT INTO tmp_reg_id
   SELECT UNIQUE id_derechohabiente
     FROM cre_acreditado cre,
          cat_maq_credito edo
    WHERE cre.estado = edo.estado
      AND edo.entidad IN(1,2,5);

   FOREACH
      SELECT t0.id_derechohabiente
        INTO v_id_derechohabiente
        FROM tmp_reg_id t0

      FOREACH
         SELECT FIRST 1 afi.nss,
                afi.rfc,
                afi.curp,
                afi.ap_paterno_af,
                afi.ap_materno_af,
                afi.nombre_af,
                afi.id_derechohabiente,
                cre.id_cre_acreditado,
                cre.tpo_originacion,
                cre.tpo_credito,
                cre.num_credito,
                cre.f_otorga,
                cre.edo_procesar,
                edo.entidad
           INTO v_nss,
                v_rfc,
                v_curp,
                v_ap_paterno_af,
                v_ap_materno_af,
                v_nombre_af,
                v_id_derechohabiente,
                v_id_cre_acreditado,
                v_tpo_originacion,
                v_tpo_credito,
                v_num_credito,
                v_f_otorga,
                v_edo_procesar,
                v_entidad
           FROM cre_acreditado cre,
                afi_derechohabiente afi,
                cat_maq_credito edo
          WHERE cre.id_derechohabiente = v_id_derechohabiente
            AND cre.id_derechohabiente = afi.id_derechohabiente
            AND cre.estado = edo.estado
            AND edo.entidad IN(1,2,5)
         ORDER BY edo.entidad, cre.f_otorga

          INSERT INTO safre_tmp:tmp_acred
          VALUES (v_nss,
                v_rfc,
                v_curp,
                v_ap_paterno_af,
                v_ap_materno_af,
                v_nombre_af,
                v_id_derechohabiente,
                v_id_cre_acreditado,
                v_tpo_originacion,
                v_tpo_credito,
                v_num_credito,
                v_f_otorga,
                v_edo_procesar,
                v_entidad);

       END FOREACH;
    END FOREACH;

   -- se regresa la prioridad
   --SET PDQPRIORITY DEFAULT;

   RETURN v_cod_error, v_isam_err, v_c_msj;

END FUNCTION;


