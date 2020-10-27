






CREATE FUNCTION "safreviv".fn_fin_integra_bdnsviv(p_folio DECIMAL(9,0))
RETURNING SMALLINT, VARCHAR(200);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   DEFINE v_inf_registros97               INTEGER;
   DEFINE v_inf_registros92               INTEGER;

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "Las cifras globales se procesaron correctamente";

   --Se insertan los registros de las cifras de conciliacion globales
   SELECT
      COUNT(id_derechohabiente)
   INTO
      v_inf_registros97
   FROM cta_saldo_mensual
   WHERE subcuenta = 4
   AND fondo_inversion = 11;

   SELECT
      COUNT(id_derechohabiente)
   INTO
      v_inf_registros92
   FROM cta_saldo_mensual
   WHERE subcuenta = 8
   AND fondo_inversion = 11;

   UPDATE safre_viv@vivop_tcp:cbd_cifras_concilia_global 
   SET total_cuentas = v_inf_registros97
   WHERE folio = p_folio AND subcuenta = 4;
   
   UPDATE safre_viv@vivop_tcp:cbd_cifras_concilia_global 
   SET total_cuentas = v_inf_registros92
   WHERE folio = p_folio AND subcuenta = 8;
   
   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


