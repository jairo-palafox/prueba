






CREATE FUNCTION "safreviv".fn_procesa_desmarca_pend(p_v_usuario CHAR(20),
                                         p_d_folio DECIMAL(10),
                                         p_tpo_orig SMALLINT)

   RETURNING SMALLINT

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado           DECIMAL(9,0);
   DEFINE cre_id_derechohabiente          DECIMAL(9,0);
   DEFINE cre_estado                      SMALLINT; -- estado
   DEFINE cre_edo_procesar                SMALLINT; -- estado procesar
   DEFINE cre_tpo_credito                 SMALLINT; -- tipo de crédito
   DEFINE cre_num_credito                 DECIMAL(10,0); -- número de crédito
   DEFINE cre_f_culmina                   DATE; -- fecha de culminación

   -- registro de cta tipo credito
   DEFINE cta_marca_inf                   SMALLINT; -- marca infonavit
   DEFINE cta_marca_prc                   SMALLINT; -- marca procesar

   -- REGISTRO de cta marca ws
   DEFINE ws_id_derechohabiente           DECIMAL(9,0);
   DEFINE ws_id_origen                    DECIMAL(9,0);
   DEFINE ws_modulo_cod                   CHAR(3);
   DEFINE ws_tpo_credito                  SMALLINT;
   DEFINE ws_marca                        SMALLINT;
   DEFINE ws_f_solicita                   DATE;
   DEFINE ws_intento                      SMALLINT;
   DEFINE ws_cod_result_op                SMALLINT;
   DEFINE ws_diagnostico                  SMALLINT;
   DEFINE ws_situacion                    SMALLINT;
   DEFINE ws_num_credito                  DECIMAL(10,0);
   DEFINE ws_f_infonavit                  DATE;
   DEFINE ws_marca_procesar               CHAR(2);
   DEFINE ws_folio_archivo                DECIMAL(9,0);
   DEFINE ws_usuario                      CHAR(20);

   DEFINE v_folio_sin_conciliar           SMALLINT;
   DEFINE v_error                         SMALLINT;

   LET v_error               = 0;
   LET v_folio_sin_conciliar = 0;

   IF p_tpo_orig = 2 THEN
      LET ws_marca              = 232;
      LET ws_modulo_cod         = "16";
      LET ws_marca_procesar     = "02";
   ELSE
      LET ws_marca              = 234;
      LET ws_modulo_cod         = "04";
      LET ws_marca_procesar     = "04";
   END IF

   FOREACH
    SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito, f_culmina, estado
      INTO cre_id_cre_acreditado, cre_id_derechohabiente, cre_tpo_credito, cre_num_credito, cre_f_culmina, cre_estado
      FROM cre_acreditado
     WHERE estado = 280
       AND tpo_originacion = p_tpo_orig

       SELECT COUNT(*)
         INTO v_folio_sin_conciliar
         FROM cre_uso_garantia c, cre_ctr_archivo r
        WHERE c.id_derechohabiente = cre_id_derechohabiente
          AND c.estado = 140
          AND c.edo_procesar < 120
          AND c.tpo_transferencia IN("43","48")
          AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo
          AND r.operacion IN(18,43);

       IF v_folio_sin_conciliar IS NULL OR
          v_folio_sin_conciliar = 0 THEN
          -- se valida si no existe el el id derechohabiente en proceso en la tabla del WS
          IF EXISTS (
          SELECT id_derechohabiente
            FROM cta_marca_ws
           WHERE id_derechohabiente = cre_id_derechohabiente) THEN
             -- Ya existe el derechohabiente en la tabla de WS. Se elimina
             DELETE
               FROM cta_marca_ws
              WHERE id_derechohabiente = cre_id_derechohabiente;
          END IF

          IF cre_f_culmina IS NULL THEN
             LET cre_f_culmina = TODAY;
          END IF

          -- se asignan los valores del registro a insertar en la tabla de WebService
          LET ws_id_derechohabiente = cre_id_derechohabiente;
          LET ws_id_origen          = cre_id_cre_acreditado;
          LET ws_tpo_credito        = cre_tpo_credito;
          LET ws_f_solicita         = TODAY;
          LET ws_intento            = 1;
          LET ws_cod_result_op      = NULL;
          LET ws_diagnostico        = NULL;
          LET ws_situacion          = 0;
          LET ws_num_credito        = cre_num_credito;
          LET ws_f_infonavit        = cre_f_culmina;
          LET ws_folio_archivo      = p_d_folio;
          LET ws_usuario            = p_v_usuario;

          -- se inserta el registro en la tabla del WebService
          INSERT INTO cta_marca_ws (
                      id_derechohabiente,
                      id_origen,
                      modulo_cod,
                      tpo_credito,
                      marca,
                      f_solicita,
                      intento,
                      cod_result_op,
                      diagnostico,
                      situacion,
                      num_credito,
                      f_infonavit,
                      marca_procesar,
                      folio_archivo,
                      usuario)
              VALUES (ws_id_derechohabiente,
                      ws_id_origen,
                      ws_modulo_cod,
                      ws_tpo_credito,
                      ws_marca,
                      ws_f_solicita,
                      ws_intento,
                      ws_cod_result_op,
                      ws_diagnostico,
                      ws_situacion,
                      ws_num_credito,
                      ws_f_infonavit,
                      ws_marca_procesar,
                      ws_folio_archivo,
                      ws_usuario);

          UPDATE cre_acreditado
             SET estado = 170
           WHERE id_cre_acreditado = cre_id_cre_acreditado;
       END IF

       LET v_folio_sin_conciliar  = 0;

   END FOREACH;

   RETURN v_error;

END FUNCTION;


