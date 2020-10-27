






CREATE FUNCTION  "safreviv".fn_uso_reversa_liquidacion(p_folio DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_id_cre_uso_garantia   DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE v_id_derechohabiente    DECIMAL(9,0);
   DEFINE v_d_folio_archivo       DECIMAL(9,0);
   -- Variables para la función de Desmarca
   DEFINE des_id_derechohabiente  DECIMAL(9,0);
   DEFINE des_marca_entra         SMALLINT;
   DEFINE des_n_referencia        INTEGER;
   DEFINE des_folio               DECIMAL(9,0);
   DEFINE v_error                 SMALLINT;

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtReversoLiquida.trace';
   --TRACE ON;

   -- Se inicializa el error en 0, este valor será el retorno en caso de no ocurrir ERROR
   LET v_error = 0;

   -- se procesan los registros de la tabla maestro}
   FOREACH
{
      -- se procesan los registros de uso de garantia para el folio dado
      SELECT id_cre_ctr_archivo, id_cre_uso_garantia, id_derechohabiente
        INTO v_id_cre_ctr_archivo, v_id_cre_uso_garantia, v_id_derechohabiente
        FROM safre_viv:cre_uso_garantia
       WHERE estado = 140
         AND tpo_transferencia = 18
         AND id_derechohabiente IN (SELECT id_derechohabiente
                                    FROM cta_movimiento
                                    WHERE folio_liquida = p_folio)
}
   -- se procesan los registros de uso de garantia para el folio dado
   SELECT id_cre_ctr_archivo, id_cre_uso_garantia, id_derechohabiente
     INTO v_id_cre_ctr_archivo, v_id_cre_uso_garantia, v_id_derechohabiente
     FROM safre_viv:cre_uso_garantia
    WHERE estado = 140
      AND tpo_transferencia IN ('18','48')
      AND folio_liquida = p_folio

      -- se busca el folio para el id ctr archivo
      SELECT folio_archivo
        INTO v_d_folio_archivo
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

      -- se asignan los valores de los parametros de la funcion de desmarca
      LET des_id_derechohabiente  = v_id_derechohabiente;
      LET des_marca_entra         = 223; -- Uso de Garantía 43 bis
      LET des_n_referencia        = v_id_cre_uso_garantia;
      LET des_folio               = v_d_folio_archivo;

      -- se invoca la función que reversa la desmarca
      EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(des_id_derechohabiente,
                                                      des_marca_entra,
                                                      des_n_referencia,
                                                      des_folio);

      -- se actualiza el registro leido
      UPDATE safre_viv:cre_uso_garantia
         SET estado = 130
       WHERE estado = 140
         AND tpo_transferencia IN ('18','48')
         AND id_cre_uso_garantia = v_id_cre_uso_garantia;
   END FOREACH;

   -- se eliminan los registos de cta movimiento para el folio que entra como parametro
   DELETE
     FROM safre_viv:cta_movimiento
    WHERE folio_liquida = p_folio;

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar nuevamente
   -- la función general de liquidación
   UPDATE safre_viv:glo_folio
      SET status = 1
    WHERE folio = p_folio;

   RETURN v_error;
END FUNCTION;


