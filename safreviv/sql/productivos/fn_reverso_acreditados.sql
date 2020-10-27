






CREATE FUNCTION "safreviv".fn_reverso_acreditados(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT, p_usuario_cod  VARCHAR(30))
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

   ---SET DEBUG FILE TO '/ds/safreviv_int/BD/acrReversaLiqCnt.trace';
   ---TRACE ON;

   -- Se inicializa el error en 0, este valor será el retorno en caso de no ocurrir ERROR
   LET v_error = 0;

   -- se procesan los registros de la tabla maestro}
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_cre_ctr_archivo, id_cre_acreditado, id_derechohabiente, estado
        INTO v_id_cre_ctr_archivo, v_id_cre_acreditado, v_id_derechohabiente, v_estado
        FROM safre_viv:cre_acreditado
       WHERE estado IN (140, 145)
         --AND tpo_originacion = p_i_tpo_originacion
         AND folio_liquida = p_folio

      -- se valida el estado obenido
      IF v_estado = 140 THEN
         LET v_estado = 20;
      ELSE
         LET v_estado = 25;
      END IF

      -- se busca el folio para el id ctr archivo
      SELECT folio_archivo
        INTO v_d_folio_archivo
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

      -- se asignan los valores de los parametros de la funcion de desmarca
      LET des_id_derechohabiente  = v_id_derechohabiente;
      LET des_marca_entra         = 221; -- Transferencia de Acreditados (ACR)
      LET des_n_referencia        = v_id_cre_acreditado;
      LET des_folio               = v_d_folio_archivo;

      -- se invoca la función que reversa la desmarca
      EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(des_id_derechohabiente,
                                                      des_marca_entra,
                                                      des_n_referencia,
                                                      des_folio);

      -- se actualizan los registros de historicos
      UPDATE safre_viv:cre_his_acreditado
         SET estado = v_estado
       WHERE estado IN (140, 145)
         AND id_cre_acreditado = v_id_cre_acreditado;

      -- se actualiza el registro leido
      UPDATE safre_viv:cre_acreditado
         SET estado = v_estado
       WHERE estado IN (140, 145)
         ---AND tpo_originacion = p_i_tpo_originacion
         AND id_cre_acreditado = v_id_cre_acreditado
         AND folio_liquida = p_folio;
   END FOREACH;

   -------------------------------------------
   ----- DEVOLUCIÓN DE SALDOS EXCEDENTES -----
   -------------------------------------------
   -- se eliminan los registros de históricos
   DELETE
     FROM safre_viv:dse_his_devolucion
    WHERE folio = p_folio;

   -- se elimina el registro en proceso los registros de la tabla maestro
   DELETE
     FROM safre_viv:dse_agrupa_devolucion
    WHERE folio_liquida = p_folio;

   -- se elimina la información de la tabla de control de archivo DSE
   DELETE
     FROM safre_viv:dse_ctr_archivo
    WHERE folio = p_folio;

   LET v_resultado = 0;
   LET v_mensaje = "El reverso terminó correctamente";

   RETURN v_resultado, v_mensaje;

END FUNCTION;


