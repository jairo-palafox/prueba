






CREATE FUNCTION "safreviv".fn_reverso_anualidades(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT, p_usuario_cod  VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100)

   DEFINE v_resultado             SMALLINT;
   DEFINE v_mensaje               VARCHAR(100);

   -- Variables para la función de Desmarca
   DEFINE des_id_derechohabiente  DECIMAL(9,0);
   DEFINE des_marca_entra         SMALLINT;
   DEFINE des_n_referencia        INTEGER;
   DEFINE des_folio               DECIMAL(9,0);

   -- Variables auxiliares
   DEFINE v_id_cre_acreditado     DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE v_id_derechohabiente    DECIMAL(9,0);
   DEFINE v_id_cre_uso_garantia   DECIMAL(9,0);
   DEFINE v_estado                SMALLINT;
   DEFINE v_d_folio_archivo       DECIMAL(9,0);
   DEFINE v_error                 SMALLINT;

   ON EXCEPTION SET v_resultado
      --Ocurrió un error al realizar el reverso de la liquidación
      --Se regresa el número de error que ocurrió
      --#############
      -- Devolverá el código de error que ocasione la excepción
      LET v_mensaje = "El reverso no pudo terminar correctamente";

      RETURN v_resultado, v_mensaje;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/agrReversaLiqCnt.trace';
   ---TRACE ON;

   -- Se inicializa el error en 0, este valor será el retorno en caso de no ocurrir ERROR
   LET v_error = 0;

   -- se procesan los registros de la tabla de acreditados
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_cre_ctr_archivo, id_cre_acreditado, id_derechohabiente, estado
        INTO v_id_cre_ctr_archivo, v_id_cre_acreditado, v_id_derechohabiente, v_estado
        FROM cre_acreditado
       WHERE folio_liquida = p_folio

      -- se valida el estado obtenido
      IF v_estado = 140 THEN
         LET v_estado = 20;
      ELIF v_estado = 145 THEN
         LET v_estado = 25;
      ELIF v_estado = 148 THEN
         LET v_estado = 18;
      ELSE
         LET v_estado = 20;
      END IF

      -- se busca el folio para el id ctr archivo
      SELECT folio_archivo
        INTO v_d_folio_archivo
        FROM cre_ctr_archivo
       WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

      -- se asignan los valores de los parametros de la funcion de desmarca
      LET des_id_derechohabiente  = v_id_derechohabiente;
      LET des_marca_entra         = 312; --Anualidades Garantizadas (AGR)
      LET des_n_referencia        = v_id_cre_acreditado;
      LET des_folio               = v_d_folio_archivo;

      -- se invoca la función que reversa la desmarca
      EXECUTE PROCEDURE sp_reversa_desmarca(des_id_derechohabiente,
                                            des_marca_entra,
                                            des_n_referencia,
                                            des_folio);

      -- se actualizan los registros de historicos
      UPDATE cre_his_acreditado
         SET estado = v_estado
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND estado IN (140, 145, 148, 280);

      -- se actualiza el registro leido
      UPDATE cre_acreditado
         SET estado = v_estado
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND folio_liquida = p_folio;

      LET v_estado = "";
      LET v_id_cre_acreditado = "";
   END FOREACH;

   LET v_estado = "";
   LET v_d_folio_archivo = "";

   -- se procesan los registros de la tabla de uso de garantía
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_cre_ctr_archivo, id_cre_uso_garantia, id_derechohabiente, estado
        INTO v_id_cre_ctr_archivo, v_id_cre_uso_garantia, v_id_derechohabiente, v_estado
        FROM cre_uso_garantia
       WHERE folio_liquida = p_folio

      -- se valida el estado obtenido
      IF v_estado = 140 THEN
         LET v_estado = 20;
      ELSE
         LET v_estado = 20;
      END IF

      -- se busca el folio para el id ctr archivo
      SELECT folio_archivo
        INTO v_d_folio_archivo
        FROM cre_ctr_archivo
       WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

      -- se asignan los valores de los parametros de la funcion de desmarca
      LET des_id_derechohabiente  = v_id_derechohabiente;
      LET des_marca_entra         = 312; --Anualidades Garantizadas (AGR)
      LET des_n_referencia        = v_id_cre_uso_garantia;
      LET des_folio               = v_d_folio_archivo;

      -- se invoca la función que reversa la desmarca
      EXECUTE PROCEDURE sp_reversa_desmarca(des_id_derechohabiente,
                                            des_marca_entra,
                                            des_n_referencia,
                                            des_folio);

      -- se actualiza el registro leido
      UPDATE cre_uso_garantia
         SET estado = v_estado
       WHERE id_cre_uso_garantia = v_id_cre_uso_garantia
         AND folio_liquida = p_folio;
   END FOREACH;

   -------------------------------------------
   ----- DEVOLUCIÓN DE SALDOS EXCEDENTES -----
   -------------------------------------------
   -- se eliminan los registros de históricos
   DELETE
     FROM dse_his_devolucion
    WHERE folio = p_folio;

   -- se elimina el registro en proceso los registros de la tabla maestro
   DELETE
     FROM dse_agrupa_devolucion
    WHERE folio_liquida = p_folio;

   -- se elimina la información de la tabla de control de archivo DSE
   DELETE
     FROM dse_ctr_archivo
    WHERE folio = p_folio;

   LET v_resultado = 0;
   LET v_mensaje = "El reverso terminó correctamente";

   RETURN v_resultado, v_mensaje;

END FUNCTION;


