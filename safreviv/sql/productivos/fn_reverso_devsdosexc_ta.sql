






CREATE FUNCTION "safreviv".fn_reverso_devsdosexc_ta(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT, p_usuario_cod  VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100)

   DEFINE v_resultado             SMALLINT;
   DEFINE v_mensaje               VARCHAR(100);

   -- Variables para la función de Desmarca
   DEFINE des_id_derechohabiente  DECIMAL(9,0);
   DEFINE des_marca_entra         SMALLINT;
   DEFINE des_n_referencia        INTEGER;
   DEFINE des_folio               DECIMAL(9,0);

   -- Variables auxiliares
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
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

   ---SET DEBUG FILE TO '/safreviv_int/archivos/dsetaReversaLiqCnt.trace';
   ---TRACE ON;

   -- Se inicializa el error en 0, este valor será el retorno en caso de no ocurrir ERROR
   LET v_error = 0;

   -- se procesan los registros de la tabla maestro
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_dse_grp_devolucion
        INTO v_id_dse_grp_devolucion
        FROM dse_agrupa_devolucion
       WHERE folio_liquida = p_folio

      -- se actualiza el estado de la tabla de agrupación
      UPDATE dse_agrupa_devolucion
         SET estado = 20
       WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion
         AND folio_liquida = p_folio;

      -- se actualiza el estado de la tabla de historicos
      UPDATE dse_his_devolucion
         SET estado = 20
       WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion;
   END FOREACH;

   LET v_resultado = 0;
   LET v_mensaje = "El reverso terminó correctamente";

   RETURN v_resultado, v_mensaje;

END FUNCTION;


