






CREATE FUNCTION "safreviv".fn_uso_preliquidacion(p_d_folio_liquida DECIMAL(9,0),
                                      p_ax_tpo_transferencia CHAR(2),
                                      p_f_liq DATE)
   RETURNING SMALLINT, SMALLINT, INTEGER, VARCHAR(250);

   --REGISTRO cre acreditado
   DEFINE cre_id_cre_uso_garantia   DECIMAL(9,0); -- identificador del acreditado
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0); -- identificador del archivo
   DEFINE cre_id_derechohabiente    DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE cre_importe_v97           DECIMAL(22,2); -- saldo deudor del derechohabiente
   DEFINE cre_periodo_pago          CHAR(6); -- periodo de pago
   DEFINE cre_tpo_uso               SMALLINT; -- tipo de uso
   DEFINE cre_tpo_credito           SMALLINT; -- tipo de crédito

   -- CAMPOS auxiliares
   DEFINE v_ax_tpo_transferencia    VARCHAR(10); -- tipo de transferencia
   DEFINE v_ax_qryTxt               CHAR(500); -- guarda una sentencia sql a ejecutar
   DEFINE v_ax_precio_fondo         DECIMAL(19,14); -- precio de la acción
   DEFINE v_ax_folio_archivo        DECIMAL(9,0); -- folio del archivo
   DEFINE v_ax_estatus              SMALLINT; -- status, retorno de la función que preliquidación
   DEFINE v_ax_estado               SMALLINT; -- estado al que se va a actualizar
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);
   DEFINE v_f_valua                 DATE;
   DEFINE v_f_liquida               DATE;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_id_proceso              SMALLINT;
   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0);
   DEFINE v_folio_archivo           DECIMAL(10,0);

   --VARIABLES DESMARCA LIQUIDACIÓN NO PROCEDENTE
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_proceso_cod             SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;

   --VARIABLES VERIFICACIÓN TIPO DE CRÉDITO
   DEFINE v_resultado               SMALLINT;
   DEFINE v_tpo_originacion         SMALLINT;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_f_otorga                DATE;
   DEFINE v_f_liquida_crd           DATE;
   DEFINE v_tpo_dscto               SMALLINT;
   DEFINE v_valida                  SMALLINT;
   DEFINE v_tpo_transferencia       CHAR(2);

   ON EXCEPTION SET v_ax_error, v_isam_err, v_c_msj
      -- Devolverá el código de error que ocasione la excepción
      RETURN v_ax_error, v_ax_estatus, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/usoPreliquida.trace';
   ---SET DEBUG FILE TO '/safreviv_int/archivos/usoPreliquida.trace';
   ---TRACE ON;

   -- se inicializan variables
   LET v_ax_error        = 0;
   LET v_ax_estatus      = 0;
   LET v_isam_err        = 0;
   LET v_c_msj           = 'El proceso finalizó correctamente';
   LET cre_tpo_credito   = 0;
   LET v_estado_marca    = 30;
   LET v_marca_causa     = 0;

   LET v_f_liquida       = p_f_liq;
   LET v_f_valua         = p_f_liq;

   LET v_resultado       = "";
   LET v_tpo_originacion = "";
   LET v_tpo_credito     = "";
   LET v_num_credito     = "";
   LET v_f_otorga        = "";
   LET v_f_liquida_crd   = "";
   LET v_tpo_dscto       = "";
   LET v_valida          = 0;

   -- Se obtiene el precio de acción
   SELECT precio_fondo
     INTO v_ax_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = v_f_valua;

   -- se valida el tipo de transferencia
   IF p_ax_tpo_transferencia = "43" THEN
      LET v_ax_tpo_transferencia = "(43)";
      LET v_marca_entra          = 225;
      LET v_proceso_cod          = 312;
      LET v_id_proceso           = 202;
   ELSE
      LET v_ax_tpo_transferencia = "(18, 48)";
      LET v_marca_entra          = 223;
      LET v_proceso_cod          = 1217;
      LET v_id_proceso           = 1203;
   END IF

   EXECUTE FUNCTION fn_verifica_id_archivo_cre(v_id_proceso)
               INTO v_id_cre_ctr_archivo, v_folio_archivo;

   LET v_ax_qryTxt = " SELECT id_cre_uso_garantia, id_cre_ctr_archivo, id_derechohabiente," ||
                     "        importe_v97, periodo_pago, tpo_uso, tpo_transferencia, estado" ||
                     "   FROM cre_uso_garantia" ||
                     "  WHERE estado IN(20, 320)"||
                     "    AND edo_procesar IN(7,120)" ||
                     "    AND tpo_transferencia IN " || v_ax_tpo_transferencia;

   PREPARE prp_cre_uso FROM v_ax_qryTxt;
   DECLARE cur_cre_uso CURSOR FOR prp_cre_uso;

   OPEN cur_cre_uso;

   WHILE (SQLCODE == 0)

   FETCH cur_cre_uso INTO cre_id_cre_uso_garantia, cre_id_cre_ctr_archivo, cre_id_derechohabiente,
                          cre_importe_v97, cre_periodo_pago, cre_tpo_uso, v_tpo_transferencia, v_estado;
      IF (SQLCODE == 100) THEN
         EXIT WHILE;
      END IF

      -- se consulta el folio del archivo recurrente
      SELECT folio_archivo
        INTO v_ax_folio_archivo
        FROM cre_ctr_archivo
       WHERE id_cre_ctr_archivo = cre_id_cre_ctr_archivo;

      FOREACH
         SELECT FIRST 1 m.tpo_credito
           INTO cre_tpo_credito
           FROM sfr_marca_activa s, cat_tipo_credito m
          WHERE s.id_derechohabiente = cre_id_derechohabiente
            AND s.marca              = m.marca_inf
            AND m.tpo_originacion    = 4

         IF cre_tpo_credito = 0 OR cre_tpo_credito IS NULL THEN
            CALL fn_edo_cred_viv (cre_id_derechohabiente, v_valida)
            RETURNING v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida_crd, v_tpo_dscto;

            IF v_resultado = 0 THEN
               LET cre_tpo_credito = v_tpo_credito;
            END IF
         END IF
      END FOREACH

      IF cre_importe_v97 > 0 THEN
         -- verifica si el tipo de transferencia le corresponde a Anualidades Garantizadas
         IF p_ax_tpo_transferencia = "43" THEN
            -- se invoca la preliquidación de Uso de la Anualidad
            EXECUTE FUNCTION fn_uso_ag_preliquida(p_d_folio_liquida,
                                                  v_ax_folio_archivo,
                                                  cre_importe_v97,
                                                  cre_id_cre_uso_garantia,
                                                  cre_id_derechohabiente,
                                                  v_ax_precio_fondo,
                                                  cre_tpo_uso,
                                                  cre_tpo_credito,
                                                  p_f_liq,
                                                  v_estado,
                                                  v_id_cre_ctr_archivo,
                                                  v_folio_archivo)
                                             INTO v_ax_estatus;
         ELSE
            -- se invoca la preliquidación de Uso de Garantía 43 bis
            EXECUTE FUNCTION fn_uso_preliquida(p_d_folio_liquida,
                                               v_ax_folio_archivo,
                                               cre_importe_v97,
                                               cre_id_cre_uso_garantia,
                                               cre_id_derechohabiente,
                                               v_ax_precio_fondo,
                                               cre_periodo_pago,
                                               p_f_liq,
                                               cre_tpo_uso,
                                               v_tpo_transferencia,
                                               v_estado,
                                               v_id_cre_ctr_archivo,
                                               v_folio_archivo)
                                          INTO v_ax_estatus;
         END IF

         -- si el estatus es diferente de 0(valido) y 1 (de No se encontró acción valor)
         -- continua con el siguiente registro
         IF v_ax_estatus <> 0 AND v_ax_estatus <> 1 THEN
            CONTINUE WHILE;
         ELSE
            IF v_ax_estatus = 1 THEN
               EXIT WHILE;
            ELSE
               -- se asigna el estatus al que se actualizará el proceso
               IF v_estado = 20 THEN
                  LET v_ax_estado = 130;
               ELSE
                  LET v_ax_estado = 325;
               END IF

               --actualiza estado en uso garantia
               UPDATE cre_uso_garantia
                  SET estado              =  v_ax_estado,
                      folio_liquida       = p_d_folio_liquida
                WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
            END IF
         END IF
      ELSE
         FOREACH
            SELECT s.usuario_marca
              INTO v_usuario
              FROM sfr_marca_activa s
             WHERE s.marca              = v_marca_entra
               AND s.id_derechohabiente = cre_id_derechohabiente
               AND s.n_referencia       = cre_id_cre_uso_garantia

            -- se invoca la función de desmarca
            EXECUTE FUNCTION fn_desmarca_cuenta(cre_id_derechohabiente,
                                                v_marca_entra,
                                                cre_id_cre_uso_garantia,
                                                v_estado_marca,
                                                v_marca_causa,
                                                V_usuario,
                                                v_proceso_cod)
                                           INTO v_ax_cod_error;
         END FOREACH

         LET v_ax_estado = 240;

            --actualiza estado en uso garantia
            UPDATE cre_uso_garantia
               SET estado              =  v_ax_estado,
                   folio_liquida       = p_d_folio_liquida
             WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
      END IF

      LET cre_tpo_credito = 0;

      IF (SQLCODE == 100) THEN
         EXIT WHILE;
      END IF
   END WHILE

   CLOSE cur_cre_uso;
   FREE cur_cre_uso;
   FREE prp_cre_uso;

   -- actualiza estadisticas a la tabla de preliquidación correspondiente
   IF p_ax_tpo_transferencia = "43" THEN
      UPDATE STATISTICS FOR TABLE cre_ag_preliquida;
   ELSE
      UPDATE STATISTICS FOR TABLE cre_ug_preliquida;
   END IF;

   RETURN v_ax_error, v_ax_estatus, v_isam_err, v_c_msj;

END FUNCTION;


