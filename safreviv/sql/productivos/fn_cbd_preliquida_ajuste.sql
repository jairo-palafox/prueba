






CREATE FUNCTION "safreviv".fn_cbd_preliquida_ajuste(p_folio DECIMAL(9,0))
RETURNING SMALLINT,VARCHAR(200);

   DEFINE v_folio_ant         DECIMAL(9,0);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   DEFINE v_id_cbd_detalle_ajuste_operativo  DECIMAL(9,0);
   DEFINE v_id_derechohabiente               DECIMAL(9,0);
   DEFINE v_subcuenta                        SMALLINT;
   DEFINE v_fondo_inversion                  SMALLINT;
   DEFINE v_tipo_movimiento                  INTEGER;
   DEFINE v_movimiento                       SMALLINT;
   DEFINE v_monto_acciones                   DECIMAL(16,6);
   DEFINE v_monto_pesos                      DECIMAL(12,2);
   DEFINE v_f_valor                          DATE;
   DEFINE v_h_registro                       DATETIME HOUR TO SECOND;
   DEFINE v_precio_accion                    DECIMAL(16,6);

   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El proceso de Preliquidación se ejecutó correctamente";

   --Se crea la tabla de preliquidacion
   DROP TABLE IF EXISTS cbd_preliquida_ajuste;
   CREATE TABLE cbd_preliquida_ajuste
   (
      f_liquida             date   NOT NULL,
      id_derechohabiente    decimal(9,0)   NOT NULL,
      subcuenta             smallint   NOT NULL,
      fondo_inversion       smallint   NOT NULL,
      movimiento            smallint   NOT NULL,
      folio_liquida         decimal(9,0)   NOT NULL,
      id_referencia         decimal(9,0)   NOT NULL,
      monto_acciones        decimal(16,6)   NOT NULL,
      monto_pesos           decimal(12,2)   NOT NULL,
      f_valor               date   NOT NULL,
      f_registro            date   NOT NULL,
      h_registro            datetime hour to second   NOT NULL,
      origen                char(20)   NOT NULL
   )FRAGMENT BY ROUND ROBIN IN cbd_1_dbs, cbd_2_dbs;

   SET PDQPRIORITY HIGH;

   --Se busca el precio de accion
   SELECT precio_fondo
   INTO v_precio_accion
   FROM glo_valor_fondo
   WHERE f_valuacion = TODAY
   AND fondo = 11;

   --Se obtienen los detalles del archivo para preliquidar
   FOREACH
      SELECT
         id_cbd_detalle_ajuste_operativo,
         id_derechohabiente,
         subcuenta,
         fondo_inversion,
         tipo_movimiento,
         monto_acciones,
         f_valor
      INTO
         v_id_cbd_detalle_ajuste_operativo,
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         v_movimiento,
         v_monto_acciones,
         v_f_valor
      FROM cbd_detalle_ajuste_operativo
      WHERE folio = p_folio
      AND estado = 2

      SELECT tipo
      INTO v_tipo_movimiento
      FROM cat_movimiento 
      WHERE movimiento = v_movimiento
      AND modulo_cod = 'cbd';

      IF (v_tipo_movimiento IS NULL OR v_tipo_movimiento = 0) THEN
         LET v_resultado = 1;
         LET v_mensaje = "ERROR: Existe un tipo de movimiento no valido en el archivo";
         UPDATE cbd_detalle_ajuste_operativo SET estado = 12 WHERE folio = p_folio AND tipo_movimiento = v_movimiento;
         UPDATE cbd_ctr_ajuste_operativo SET estado = 12 WHERE folio = p_folio;
         RETURN v_resultado, v_mensaje;
      ELSE
         IF (v_tipo_movimiento = -1) THEN    --Cargo
            LET v_monto_acciones = v_monto_acciones * -1;
         END IF
      END IF

      LET v_monto_pesos = v_monto_acciones * v_precio_accion;
      LET v_h_registro = CURRENT HOUR TO SECOND;

      INSERT INTO cbd_preliquida_ajuste VALUES (TODAY,                              --f_liquida
                                                v_id_derechohabiente,               --id_derechohabiente
                                                v_subcuenta,                        --subcuenta
                                                v_fondo_inversion,                  --fondo_inversion
                                                v_movimiento,                       --movimiento
                                                p_folio,                            --folio_liquida
                                                v_id_cbd_detalle_ajuste_operativo,  --id_referencia
                                                v_monto_acciones,                   --monto_acciones
                                                v_monto_pesos,                      --monto_pesos
                                                v_f_valor,                          --f_valor
                                                TODAY,                              --f_registro
                                                v_h_registro,                       --h_registro
                                                'AJUSTE_POR_BDNSVIV'                  --origen
                                                );

   END FOREACH;
   
   UPDATE STATISTICS FOR table cbd_preliquida_ajuste;
   
   --Se actualiza el proceso al estado 3 - Preliquidado
   UPDATE cbd_ctr_ajuste_operativo SET estado = 3 WHERE folio = p_folio;
   UPDATE glo_folio SET status = 1 WHERE  folio = p_folio;
   
   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


