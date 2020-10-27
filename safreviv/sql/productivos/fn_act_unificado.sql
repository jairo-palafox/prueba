






CREATE FUNCTION "safreviv".fn_act_unificado(p_folio SMALLINT)
   RETURNING SMALLINT,SMALLINT

   DEFINE v_id_dh_unificador_actual DECIMAL(9,0);
   DEFINE v_nss_unificador_actual   CHAR(11);
   DEFINE v_id_ocg_unificacion      DECIMAL(9,0);
   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_unificador           DECIMAL(9,0);
   DEFINE v_nss_unificado           CHAR(11);
   DEFINE v_id_dh_unificado         DECIMAL(9,0);
   DEFINE v_ind_unificacion_1       SMALLINT;
   DEFINE v_error_1                 SMALLINT;
   DEFINE v_error                   SMALLINT;
   DEFINE v_ind_unificacion         SMALLINT;

   ON EXCEPTION SET v_error
      LET v_ind_unificacion = 0;
      RETURN v_ind_unificacion,v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_act_unificado.trace';
   TRACE ON;

   let v_error                    = 0;
   LET v_ind_unificacion          = 0;
   LET v_id_dh_unificador_actual  = NULL;
   LET v_nss_unificador_actual    = NULL;
   LET v_nss_unificador           = NULL;
   LET v_id_unificador            = NULL;
   LET v_nss_unificado            = NULL;
   LET v_id_dh_unificado          = NULL;
   LET v_ind_unificacion_1        = NULL;
   LET v_error_1                  = NULL;

   FOREACH

      SELECT id_ocg_unificacion,
             nss_unificador,
             id_unificador ,
             nss_unificado ,
             id_unificado
        INTO v_id_ocg_unificacion,
             v_nss_unificador,
             v_id_unificador,
             v_nss_unificado,
             v_id_dh_unificado
        FROM ocg_unificacion
       WHERE folio_unificacion = 100

      SELECT id_derechohabiente,
             nss_unificador
        INTO v_id_dh_unificador_actual,
             v_nss_unificador_actual
        FROM uni_det_unificador
       WHERE id_unificador = v_id_unificador;

      IF v_id_dh_unificador_actual IS NOT NULL THEN

      EXECUTE FUNCTION fn_ocg_unifica (v_id_dh_unificador_actual,v_id_unificador,200) INTO v_ind_unificacion_1,v_error_1;

      INSERT INTO tmp_ocg_unifica VALUES (v_id_unificador,v_id_dh_unificador_actual);

      UPDATE ocg_unificacion
         SET folio_unificacion = 110,
             nss_unificador    = v_nss_unificador_actual,
             id_unificador     = v_id_dh_unificador_actual
       WHERE id_ocg_unificacion = v_id_ocg_unificacion
         AND id_unificador      = v_id_unificador
         AND id_unificado       = v_id_dh_unificado
         AND nss_unificador     = v_nss_unificador
         AND nss_unificado      = v_nss_unificado
         AND folio_unificacion  = 100;

      LET v_ind_unificacion = v_ind_unificacion + 1;

      END IF

      LET v_id_dh_unificador_actual  = NULL;
      LET v_nss_unificador_actual    = NULL;
      LET v_nss_unificador           = NULL;
      LET v_id_unificador            = NULL;
      LET v_nss_unificado            = NULL;
      LET v_id_dh_unificado          = NULL;


   END FOREACH

   RETURN v_ind_unificacion,v_error;

END FUNCTION;


