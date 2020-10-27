






CREATE FUNCTION "safreviv".fn_dse_preliquida_grt(p_d_folio DECIMAL(10),
                                      p_usuario_cod CHAR(20),
                                      p_precio_fondo DECIMAL(19,14),
                                      p_f_valor DATE)
   RETURNING SMALLINT

   -- REGISTRO tabla (origen) de DSE agrupado
   DEFINE dsea_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE dsea_id_derechohabiente    DECIMAL(9,0);
   DEFINE dsea_num_credito           DECIMAL(10,0);
   DEFINE dsea_tpo_transferencia     CHAR(2);
   DEFINE dsea_origen_devolucion     CHAR(2);
   DEFINE dsea_f_movimiento          DATE;
   DEFINE dsea_folio_liquida         DECIMAL(9,0);
   DEFINE dsea_aivs97                DECIMAL(22,2);
   DEFINE dsea_pesos97               DECIMAL(22,2);
   DEFINE dsea_aivs92                DECIMAL(22,2);
   DEFINE dsea_pesos92               DECIMAL(22,2);
   DEFINE dsea_monto_aportacion      DECIMAL(22,2);
   DEFINE dsea_aivs_aportacion       DECIMAL(22,2);
   DEFINE dsea_nss_separacion        CHAR(11);
   DEFINE dsea_edo_procesar          SMALLINT;
   DEFINE dsea_estado                SMALLINT;
   -- TABLA preliquidacion de DSE
   DEFINE pre_f_liquida              DATE;
   DEFINE pre_id_derechohabiente     DECIMAL(9,0);
   DEFINE pre_subcuenta              SMALLINT;
   DEFINE pre_fondo_inversion        SMALLINT;
   DEFINE pre_movimiento             SMALLINT;
   DEFINE pre_folio_liquida          DECIMAL(9,0);
   DEFINE pre_id_referencia          DECIMAL(9,0);
   DEFINE pre_monto_acciones         DECIMAL(22,2);
   DEFINE pre_monto_pesos            DECIMAL(22,2);
   DEFINE pre_f_valor                DATE;
   DEFINE pre_f_registro             DATE;
   DEFINE pre_h_registro             DATETIME HOUR TO SECOND;
   DEFINE pre_origen                 CHAR(20);
   -- Campos auxiliares
   DEFINE v_ax_tipo_trabajador       CHAR(1); -- tipo trabajador
   DEFINE v_resultado                SMALLINT; -- resultado del proceso-> 0: correcto, <> 0: error
   DEFINE v_tpo_transferencia        CHAR(2); -- tipo de transferencia

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado;
   END EXCEPTION;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseGrtPreliquida.trace';
   --TRACE ON;

   -- se asume que no hay errores
   LET v_resultado = 0;
   LET v_tpo_transferencia = "19"; -- 19-DSE Créditos en Garantía 43 bis

   -- se obtienen los registros de la tabla agrupada
   FOREACH
      SELECT id_dse_grp_devolucion,
             id_derechohabiente,
             num_credito,
             tpo_transferencia,
             origen_devolucion,
             f_movimiento,
             folio_liquida,
             aivs97,
             pesos97,
             aivs92,
             pesos92,
             monto_aportacion,
             aivs_aportacion,
             nss_separacion,
             edo_procesar,
             estado
        INTO dsea_id_dse_grp_devolucion,
             dsea_id_derechohabiente,
             dsea_num_credito,
             dsea_tpo_transferencia,
             dsea_origen_devolucion,
             dsea_f_movimiento,
             dsea_folio_liquida,
             dsea_aivs97,
             dsea_pesos97,
             dsea_aivs92,
             dsea_pesos92,
             dsea_monto_aportacion,
             dsea_aivs_aportacion,
             dsea_nss_separacion,
             dsea_edo_procesar,
             dsea_estado
        FROM dse_agrupa_devolucion
       WHERE tpo_transferencia = v_tpo_transferencia
         AND edo_procesar = 120
         AND estado       = 20

      -- se obtiene el id del derechohabiente para el nss
      SELECT tipo_trabajador
        INTO v_ax_tipo_trabajador
        FROM afi_derechohabiente
       WHERE id_derechohabiente = dsea_id_derechohabiente;

      -- se asignan los datos comunes a los posibles registros que se crearan (1 por viv97 y 1 por viv92)
      LET pre_f_liquida           = p_f_valor;
      LET pre_id_derechohabiente  = dsea_id_derechohabiente;
      LET pre_fondo_inversion     = 11;
      --LET pre_movimiento          = 21;
      LET pre_folio_liquida       = p_d_folio;
      LET pre_id_referencia       = dsea_id_dse_grp_devolucion;
      LET pre_f_valor             = p_f_valor;
      LET pre_f_registro          = TODAY;
      LET pre_h_registro          = CURRENT HOUR TO SECOND;
      LET pre_origen              = "DEV SDO EXC CG";

      -- si se tiene monto de viv92
      IF dsea_aivs92 > 0 THEN
         -- se valida el tipo de trabajador
         IF v_ax_tipo_trabajador = "I" THEN
            LET pre_subcuenta  = 8;
            LET pre_movimiento = 21;
         ELSE
            LET pre_subcuenta  = 42;
            LET pre_movimiento = 91;
         END IF

         --LET pre_subcuenta       = 8; -- vivienda 92
         LET pre_monto_acciones  = dsea_aivs92; -- AIVs del registro origen
         LET pre_monto_pesos     = dsea_aivs92*p_precio_fondo; -- Monto en pesos del registro origen

         -- se inserta el registro en la tabla de preliquidacion
         INSERT INTO dse_cg_preliquida(
                     f_liquida,
                     id_derechohabiente,
                     subcuenta,
                     fondo_inversion,
                     movimiento,
                     folio_liquida,
                     id_referencia,
                     monto_acciones,
                     monto_pesos,
                     f_valor,
                     f_registro,
                     h_registro,
                     origen)
             VALUES (pre_f_liquida,
                     pre_id_derechohabiente,
                     pre_subcuenta,
                     pre_fondo_inversion,
                     pre_movimiento,
                     pre_folio_liquida,
                     pre_id_referencia,
                     pre_monto_acciones,
                     pre_monto_pesos,
                     pre_f_valor,
                     pre_f_registro,
                     pre_h_registro,
                     pre_origen);
      END IF

      -- si se tiene monto de viv97
      IF ( dsea_aivs97 > 0 ) THEN
         -- se valida el tipo de trabajador
         IF v_ax_tipo_trabajador = "I" THEN
            LET pre_subcuenta = 4;
            LET pre_movimiento = 21;
         ELSE
            LET pre_subcuenta = 44;
            LET pre_movimiento = 91;
         END IF

         --LET pre_subcuenta       = 4; -- vivienda 97
         LET pre_monto_acciones  = dsea_aivs97; -- AIVs del registro origen
         LET pre_monto_pesos     = dsea_aivs97*p_precio_fondo; -- Monto en pesos del registro origen

         -- se inserta el registro en la tabla de preliquidacion
         INSERT INTO dse_cg_preliquida(
                     f_liquida,
                     id_derechohabiente,
                     subcuenta,
                     fondo_inversion,
                     movimiento,
                     folio_liquida,
                     id_referencia,
                     monto_acciones,
                     monto_pesos,
                     f_valor,
                     f_registro,
                     h_registro,
                     origen)
             VALUES (pre_f_liquida,
                     pre_id_derechohabiente,
                     pre_subcuenta,
                     pre_fondo_inversion,
                     pre_movimiento,
                     pre_folio_liquida,
                     pre_id_referencia,
                     pre_monto_acciones,
                     pre_monto_pesos,
                     pre_f_valor,
                     pre_f_registro,
                     pre_h_registro,
                     pre_origen);
      END IF

      -- se actualiza el estado del registro en dse_agrupa a 130
      UPDATE dse_agrupa_devolucion
         SET estado = 130,
             folio_liquida = p_d_folio
       WHERE id_dse_grp_devolucion = dsea_id_dse_grp_devolucion;

      -- se actualiza el estado del registro en dse_agrupa a 130
      UPDATE dse_his_devolucion
         SET estado = 130
       WHERE id_dse_grp_devolucion = dsea_id_dse_grp_devolucion;

      -- se actualiza el estado del registro en dse_devolucion a 130
      UPDATE dse_devolucion
         SET estado = 130
       WHERE id_derechohabiente IN (
             SELECT id_derechohabiente
               FROM dse_agrupa_devolucion
              WHERE id_dse_grp_devolucion = dsea_id_dse_grp_devolucion)
         AND tpo_transferencia = v_tpo_transferencia
         AND estado = 15;
   END FOREACH;

   -- se actualiza el folio indicando que ya se preliquidó
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_d_folio;

   -- se devuelve el resultado al término del proceso
   RETURN v_resultado;

END FUNCTION
;


