






CREATE FUNCTION  "safreviv".fn_reverso_liquidacion_acr(p_folio DECIMAL(9,0),
                                            p_i_tpo_originacion SMALLINT)
   RETURNING SMALLINT;

   -- Variables para la función de Desmarca
   DEFINE des_id_derechohabiente  DECIMAL(9,0);
   DEFINE des_marca_entra         SMALLINT;
   DEFINE des_n_referencia        INTEGER;
   DEFINE des_folio               DECIMAL(9,0);

   -- Variables auxiliares
   DEFINE v_id_cre_acreditado     DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE v_id_derechohabiente    DECIMAL(9,0);
   DEFINE v_estado                SMALLINT;
   DEFINE v_d_folio_archivo       DECIMAL(9,0);
   DEFINE v_error                 SMALLINT;
   DEFINE v_f_proceso             DATE;

   ON EXCEPTION SET v_error
      --Ocurrió un error al realizar el reverso de la liquidación
      --Se regresa el número de error que ocurrió
      --#############
      -- Devolverá el código de error que ocasione la excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrReversaLiq.trace';
   --TRACE ON;

   -- Se inicializa el error en 0, este valor será el retorno en caso de no ocurrir ERROR
   LET v_error = 0;

   -- se procesan los registros de la tabla maestro}
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_cre_ctr_archivo, id_cre_acreditado, id_derechohabiente, estado
        INTO v_id_cre_ctr_archivo, v_id_cre_acreditado, v_id_derechohabiente, v_estado
        FROM safre_viv:cre_acreditado
       WHERE folio_liquida = p_folio
         AND estado IN (140, 145, 148, 170)
         AND tpo_originacion = p_i_tpo_originacion

      -- se valida el estado obenido
      IF v_estado = 140 THEN
         LET v_estado = 130;
      ELIF v_estado = 148 THEN
         LET v_estado = 138;
      ELIF v_estado = 270 THEN
         LET v_estado = 137;
      ELIF  v_estado = 145 THEN
         LET v_estado = 135;
      ELSE
         LET v_estado = 137;
      END IF

      -- se busca el folio para el id ctr archivo
      SELECT folio_archivo
        INTO v_d_folio_archivo
        FROM cre_ctr_archivo
       WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

      -- se asignan los valores de los parametros de la funcion de desmarca
      LET des_id_derechohabiente  = v_id_derechohabiente;
      LET des_marca_entra         = 221; -- Transferencia de Acreditados (ACR)
      LET des_n_referencia        = v_id_cre_acreditado;
      LET des_folio               = v_d_folio_archivo;

      -- se invoca la función que reversa la desmarca
      EXECUTE PROCEDURE sp_reversa_desmarca(des_id_derechohabiente,
                                                      des_marca_entra,
                                                      des_n_referencia,
                                                      des_folio);

      FOREACH
         SELECT FIRST 1 f_proceso
           INTO v_f_proceso
           FROM cre_his_acreditado
          WHERE id_cre_acreditado = v_id_cre_acreditado
            AND estado = v_estado
      END FOREACH

      UPDATE cre_his_acreditado
         SET estado = v_edo_nvo
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND estado = v_estado
         AND f_proceso = v-f_proceso;

      -- se actualiza el registro leido
      UPDATE cre_acreditado
         SET estado = v_edo_nvo
       WHERE id_cre_acreditado = v_id_cre_acreditado;
   END FOREACH;

   -- se eliminan los registos de cta movimiento para el folio que entra como parametro
   DELETE
     FROM cta_movimiento
    WHERE folio_liquida = p_folio;

   -- se eliminan los registos de cta fondo72 para el folio que entra como parametro
   DELETE
     FROM cta_fondo72
    WHERE folio_liquida = p_folio;

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar nuevamente
   -- la función general de liquidación
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;

   RETURN v_error;

END FUNCTION;


