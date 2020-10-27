






CREATE FUNCTION "safreviv".fn_cre_preliquidacion(p_d_folio_liquida    DECIMAL(9,0),
                                      p_ax_tpo_originacion SMALLINT,
                                      p_v_usuario          CHAR(20),
                                      p_si_proceso_cod     SMALLINT,
                                      p_f_liq              DATE)
   RETURNING SMALLINT, SMALLINT;

   --REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);  -- identificador del acreditado
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0);  -- identificador del archivo
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);  -- identificador del derechohabiente
   DEFINE cre_tpo_credito           SMALLINT;      -- tipo de crédito
   DEFINE cre_sdo_deudor            DECIMAL(12,2); -- saldo deudor del derechohabiente
   DEFINE cre_estado                SMALLINT;      -- estado del registro
   DEFINE v_id_derechohabiente      DECIMAL(9,0);  -- identificador cambio de acreditado

   -- CAMPOS auxiliares
   DEFINE v_ax_tpo_transferencia    CHAR(2);  -- tipo de transferencia
   DEFINE v_ax_precio_fondo         DECIMAL(19,14); -- precio de la acción
   DEFINE v_ax_tpo_trabajador       CHAR(1); -- tipo de trabajador
   DEFINE v_ax_folio_archivo        DECIMAL(9,0); -- folio del archivo
   DEFINE v_ax_marca_entra_ext      SMALLINT; -- marca entra extra que se inserta (223 GRT)
   DEFINE v_ax_estado_marca         SMALLINT; -- estado marca
   DEFINE v_ax_estado               SMALLINT; -- estado del registro
   DEFINE v_ax_codigo_rechazo       SMALLINT; -- código de rechazo
   DEFINE v_ax_marca_causa          SMALLINT; -- marca causa
   DEFINE v_ax_fecha_causa          DATE; -- fecha causa
   DEFINE v_ax_sts_marcaje          SMALLINT; -- estatus de retorno de la función
   DEFINE v_ax_estatus              SMALLINT; -- status, retorno de la función que preliquidac
   DEFINE v_ax_id_deudor            SMALLINT; -- booleana que indica si se genera deudor o no para un derechohabiente
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_ax_nss                  CHAR(11);
   DEFINE v_ax_ministra             SMALLINT;
   DEFINE v_ax_f_envio              DATE;
   DEFINE v_ax_sdo_aivs             DECIMAL(12,6);
   DEFINE v_f_valua                 DATE;
   DEFINE v_f_liquida               DATE;

   --variables de retorne de preliquidación deudor 0 AGR
   DEFINE p_ax_error              SMALLINT;
   DEFINE p_ax_exist_error        SMALLINT;
   DEFINE p_isam_err              INTEGER;
   DEFINE p_c_msj                 VARCHAR(250);

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_ax_estatus;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrPreliquida.trace';
   ---SET DEBUG FILE TO '/safreviv_int/archivos/acrPreliquida.trace';
   ---TRACE ON;

   SET PDQPRIORITY HIGH;

   -- se inicializan variables
   LET v_ax_error           = 0;
   LET v_ax_estado_marca    = 0;
   LET v_ax_codigo_rechazo  = 0;
   LET v_ax_marca_causa     = NULL;
   LET v_ax_fecha_causa     = "";
   LET v_ax_estatus         = 0;
   LET v_id_derechohabiente = 0;
   LET v_f_liquida          = TODAY;

   -- se obtiene el precio de accion para la fecha de hoy
   SELECT precio_fondo
     INTO v_ax_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo       = 11
      AND f_valuacion = p_f_liq;

   FOREACH
      -- se obtiene la información a preliquidar, las marcas y el tipo de transferencia para el tipo de crédito en proceso
      SELECT cre.id_cre_acreditado,
             cre.id_cre_ctr_archivo,
             cre.id_derechohabiente,
             cre.tpo_credito,
             cre.sdo_deudor,
             cre.estado,
             DECODE(tpo.id_proceso,201,"03",1201,"16",301,"43"),
             tpo.id_deudor
        INTO cre_id_cre_acreditado,
             cre_id_cre_ctr_archivo,
             cre_id_derechohabiente,
             cre_tpo_credito,
             cre_sdo_deudor,
             cre_estado,
             v_ax_tpo_transferencia,
             v_ax_id_deudor
        FROM cre_acreditado cre,
             cat_tipo_credito tpo
       WHERE cre.estado IN (18, 20, 25, 270, 275)
         AND cre.edo_procesar IN(120,7)
         AND cre.tpo_originacion = p_ax_tpo_originacion
         AND cre.tpo_originacion = tpo.tpo_originacion
         AND cre.tpo_credito     = tpo.tpo_credito
         AND tpo.f_actualiza    <= cre.f_otorga
         AND tpo.id_deudor       = 1
         AND cre.sdo_deudor      > 0
      ORDER BY 3

      IF v_id_derechohabiente = cre_id_derechohabiente THEN
          CONTINUE FOREACH;
      END IF

      IF v_ax_id_deudor = 1 THEN
         -- se obtiene el tipo de trabajador y el nss
         SELECT tipo_trabajador, nss
           INTO v_ax_tpo_trabajador, v_ax_nss
           FROM afi_derechohabiente
          WHERE id_derechohabiente = cre_id_derechohabiente;

         -- se consulta el folio del archivo recurrente
         SELECT folio_archivo
           INTO v_ax_folio_archivo
           FROM cre_ctr_archivo
          WHERE id_cre_ctr_archivo = cre_id_cre_ctr_archivo;

         IF p_ax_tpo_originacion = 1 THEN
            IF cre_estado = 20 THEN
               -- se invoca la función que preliquida el derechohabiente en proceso
               EXECUTE FUNCTION fn_acr_preliquida(
                                p_d_folio_liquida,
                                v_ax_folio_archivo,
                                cre_sdo_deudor,
                                cre_id_cre_acreditado,
                                cre_id_derechohabiente,
                                v_ax_precio_fondo,
                                v_ax_tpo_trabajador,
                                v_ax_nss,
                                p_f_liq)
                           INTO v_ax_estatus;
            ELSE
               -- se invoca la función que preliquida saldos remanentes
               EXECUTE FUNCTION fn_acr_preliquida_rem(
                                p_d_folio_liquida,
                                v_ax_folio_archivo,
                                cre_id_cre_acreditado,
                                cre_id_derechohabiente,
                                v_ax_precio_fondo,
                                v_ax_tpo_trabajador,
                                p_f_liq)
                           INTO v_ax_estatus;
            END IF
         ELIF p_ax_tpo_originacion = 2 THEN
            -- se invoca la función que preliquida el derechohabiente en proceso
            EXECUTE FUNCTION fn_grt_preliquida(p_d_folio_liquida,
                             v_ax_folio_archivo,
                             --cre_sdo_deudor,
                             cre_id_cre_acreditado,
                             cre_id_derechohabiente,
                             v_ax_precio_fondo,
                             v_ax_tpo_trabajador)
                        INTO v_ax_estatus;
         ELSE
            IF cre_estado = 20 OR cre_estado = 270 THEN
               -- se invoca la función que preliquida el derechohabiente en proceso
               EXECUTE FUNCTION fn_agr_preliquida(p_d_folio_liquida,
                                v_ax_folio_archivo,
                                cre_sdo_deudor,
                                cre_id_cre_acreditado,
                                cre_id_derechohabiente,
                                v_ax_precio_fondo,
                                v_ax_tpo_trabajador,
                                v_ax_nss,
                                p_f_liq)
                           INTO v_ax_estatus;
            ELIF cre_estado = 18 THEN
               EXECUTE FUNCTION fn_agr_prlq_ministracion(p_d_folio_liquida,
                                v_ax_folio_archivo,
                                cre_sdo_deudor,
                                cre_id_cre_acreditado,
                                cre_id_derechohabiente,
                                v_ax_precio_fondo,
                                v_ax_tpo_trabajador,
                                v_ax_nss,
                                p_f_liq)
                           INTO v_ax_estatus;
            ELSE
               -- se invoca la función que preliquida saldos remanentes
                EXECUTE FUNCTION fn_agr_preliquida_rem(p_d_folio_liquida,
                                v_ax_folio_archivo,
                                cre_id_cre_acreditado,
                                cre_id_derechohabiente,
                                v_ax_precio_fondo,
                                v_ax_tpo_trabajador,
                                p_f_liq)
                           INTO v_ax_estatus;
            END IF
         END IF

         -- si el estatus es diferente de 0(valido) y 1 (de No se encontró acción valor)
         -- continua con el siguiente registro
         IF v_ax_estatus <> 0 AND v_ax_estatus <> 1 THEN
            CONTINUE FOREACH;
         END IF

         IF v_ax_estatus = 1 THEN
            EXIT FOREACH;
         END IF
      END IF

      -- la actualización de status y marcaje de cuenta no aplica para GRT
      IF p_ax_tpo_originacion <> 2 THEN
         IF cre_estado = 20 THEN
            LET v_ax_estado = 130;
         ELIF cre_estado = 270 THEN
            LET v_ax_estado = 330;
         ELIF cre_estado = 275 THEN
            LET v_ax_estado = 335;
         ELIF cre_estado = 18 THEN
            LET v_ax_estado = 138;
         ELSE
            LET v_ax_estado = 135;
         END IF

         -- actualiza estado en acreditado
         UPDATE cre_acreditado
            SET estado            = v_ax_estado,
                folio_liquida     = p_d_folio_liquida
          WHERE id_cre_acreditado = cre_id_cre_acreditado;

         -- actualiza estado en his acreditado
         UPDATE cre_his_acreditado
            SET estado            = v_ax_estado
          WHERE id_cre_acreditado = cre_id_cre_acreditado;

         -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
         IF p_ax_tpo_originacion = 1 THEN
            -- corresponde a Transferencia de Acreditados
            LET v_ax_marca_entra_ext = 221; -- Transferencia de Acreditados

            -- Para los registros "Solo Infonavit" no realiza marcaje
            IF(v_ax_tpo_trabajador <> 'S') THEN
               -- se ejecuta la función de marcaje
               EXECUTE FUNCTION fn_marca_cuenta(cre_id_derechohabiente,
                                                v_ax_marca_entra_ext,
                                                cre_id_cre_acreditado,
                                                p_d_folio_liquida,
                                                v_ax_estado_marca,
                                                v_ax_codigo_rechazo,
                                                v_ax_marca_causa,
                                                v_ax_fecha_causa,
                                                p_v_usuario,
                                                p_si_proceso_cod)
                                           INTO v_ax_sts_marcaje;
            END IF
         END IF
      END IF

      LET v_id_derechohabiente = cre_id_derechohabiente;
   END FOREACH

   -- actualiza estadisticas a la tabla de preliquidación correspondiente
   IF p_ax_tpo_originacion = 1 THEN
      UPDATE STATISTICS FOR TABLE cre_ta_preliquida;
   ELIF p_ax_tpo_originacion = 2 THEN
      UPDATE STATISTICS FOR TABLE cre_sg_preliquida;
   ELSE
      UPDATE STATISTICS FOR TABLE cre_ag_preliquida;
   END IF;

   SET PDQPRIORITY DEFAULT;

   RETURN v_ax_error, v_ax_estatus;

END FUNCTION;


