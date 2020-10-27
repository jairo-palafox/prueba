






CREATE FUNCTION "safreviv".fn_agr_preliquidacion(p_d_folio_liquida DECIMAL(9,0))
   RETURNING SMALLINT, SMALLINT, INTEGER, VARCHAR(250);

   --REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado   DECIMAL(9,0); -- identificador del acreditado
   DEFINE cre_id_cre_ctr_archivo  DECIMAL(9,0); -- identificador del archivo
   DEFINE cre_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE cre_tpo_originacion     SMALLINT;    -- tipo de originacion

   -- CAMPOS auxiliares
   DEFINE v_ax_precio_fondo       DECIMAL(19,14); -- precio de la acción
   DEFINE v_ax_tpo_trabajador     CHAR(1); -- tipo de trabajador
   DEFINE v_ax_folio_archivo      DECIMAL(9,0); -- folio del archivo
   DEFINE v_ax_marca_entra_ext    SMALLINT; -- marca entra extra que se inserta (223 GRT)
   DEFINE v_ax_estado_marca       SMALLINT; -- estado marca
   DEFINE v_ax_estado             SMALLINT; -- estado del registro
   DEFINE v_ax_codigo_rechazo     SMALLINT; -- código de rechazo
   DEFINE v_ax_marca_causa        SMALLINT; -- marca causa
   DEFINE v_ax_fecha_causa        DATE; -- fecha causa
   DEFINE v_ax_sts_marcaje        SMALLINT; -- estatus de retorno de la función
   DEFINE v_ax_error              SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_ax_exist_error        SMALLINT; -- status, retorno de la función que preliquida
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_folio_liq             DECIMAL(10,0); --máximo folio liquidado
   DEFINE v_id_cre_ctr_arh        DECIMAL(9,0);  --máximo lote procesado

   ON EXCEPTION SET v_ax_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_ax_exist_error, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrPreliquida.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_estado_marca    = 0;
   LET v_ax_codigo_rechazo  = 0;
   LET v_ax_marca_causa     = NULL;
   LET v_ax_fecha_causa     = "";
   LET v_ax_error           = 0;
   LET v_ax_exist_error     = 0;
   LET v_isam_err           = 0;
   LET v_c_msj              = 'El proceso finalizó correctamente';

   -- se obtiene el precio de accion para la fecha de hoy
   SELECT precio_fondo
     INTO v_ax_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

  {
   FOREACH
     SELECT MAX(folio_liquida)
       INTO v_folio_liq
       FROM cre_acreditado
      WHERE tpo_originacion = 4

     SELECT MAX(c.id_cre_ctr_archivo)
       INTO v_id_cre_ctr_arh
       FROM cre_acreditado c, cta_movimiento m
      WHERE c.folio_liquida = v_folio_liq
        AND c.folio_liquida = m.folio_liquida;

     IF v_id_cre_ctr_arh IS NOT NULL AND
        v_id_cre_ctr_arh > 0 THEN
        EXIT FOREACH;
     END IF
   END FOREACH
  }

   -- se obtienen la información a preliquidar, las marcas y el tipo de transferencia para el tipo de credito en proceso
   FOREACH
    SELECT cre.id_cre_acreditado,
           cre.id_cre_ctr_archivo,
           cre.id_derechohabiente,
           cre.tpo_originacion,
           afi.tipo_trabajador
      INTO cre_id_cre_acreditado,
           cre_id_cre_ctr_archivo,
           cre_id_derechohabiente,
           cre_tpo_originacion,
           v_ax_tpo_trabajador
      FROM safre_viv:cre_acreditado cre,
           safre_viv:cat_tipo_credito tpo,
           safre_viv:afi_derechohabiente afi
     WHERE cre.estado IN (20, 25)
       AND cre.id_derechohabiente = afi.id_derechohabiente
       AND cre.tpo_originacion = 4
       AND cre.tpo_originacion = tpo.tpo_originacion
       AND cre.tpo_credito = tpo.tpo_credito
       ---AND cre.id_cre_ctr_archivo > v_id_cre_ctr_arh
       AND tpo.id_deudor = 0

      -- se consulta el folio del archivo recurrente
      SELECT folio_archivo
        INTO v_ax_folio_archivo
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = cre_id_cre_ctr_archivo;

      -- se invoca la función que preliquida el derechohabiente en proceso
      EXECUTE FUNCTION safre_viv:fn_grt_preliquida(
                       p_d_folio_liquida,
                       v_ax_folio_archivo,
                       cre_id_cre_acreditado,
                       cre_id_derechohabiente,
                       v_ax_precio_fondo,
                       v_ax_tpo_trabajador,
                       cre_tpo_originacion)
                  INTO v_ax_exist_error;

      IF v_ax_exist_error = 1 THEN
         EXIT FOREACH;
      END IF
   END FOREACH

   -- actualiza estadisticas a la tabla de preliquidación correspondiente
   UPDATE STATISTICS FOR TABLE safre_viv:cre_ag_preliquida;

   RETURN v_ax_error, v_ax_exist_error, v_isam_err, v_c_msj;
END FUNCTION;


