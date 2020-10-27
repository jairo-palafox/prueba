






CREATE FUNCTION "safreviv".fn_genera_saldo_mensual_sep()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_fcorte               DATE;
   DEFINE v_fdiario              DATE;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   SET PDQPRIORITY HIGH;
   
   --Se establece la fecha de corte como el ultimo dia natural del mes inmediato anterior
   --LET v_fcorte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1;
   LET v_fcorte = "09302013"; 

   --Se inicializan las variables de salida
   LET v_resultado = 1;
   LET v_mensaje = "El saldo mensual fue generado con fecha de corte = " || v_fcorte;

   --Se valida si el ultimo saldo diario generado corresponde al saldo mensual
   {SELECT 
      MAX(f_saldo) 
   INTO
      v_fdiario
   FROM safre_sdo@vivws_tcp:glo_ctr_saldo 
   WHERE tpo_saldo = 1;

   IF (v_fdiario = v_fcorte) THEN   --Si las fechas son iguales el saldo se sacara del saldo diario

   ELSE  --Si son diferentes el saldo se sacara de cta_movimiento

   END IF
}
   --Se genera una tabla temporal con los saldos para cada derechohabiente a la fecha de corte
   DROP TABLE IF EXISTS tmp_saldo_mensual;
   SELECT id_derechohabiente,
          subcuenta,
          fondo_inversion,
          v_fcorte v_fcorte,
          SUM(monto_acciones) monto_acciones,
          SUM(monto_pesos) monto_pesos
   FROM safre_viv:cta_movimiento
   WHERE f_liquida <= v_fcorte
   GROUP BY id_derechohabiente,
          subcuenta,
          fondo_inversion
   INTO TEMP tmp_saldo_mensual;

   UPDATE STATISTICS FOR TABLE tmp_saldo_mensual;

   --Se genera el saldo mensual para cada derechohabiente

    --Saldos para los fondos de inversion con precio de accion
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_mensual
   SELECT ts.id_derechohabiente,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          (ts.monto_acciones * gf.precio_fondo),
          ts.v_fcorte
     FROM tmp_saldo_mensual ts,
          safre_viv:glo_valor_fondo gf
   WHERE	 ts.fondo_inversion		 <> 0
       AND gf.fondo              = ts.fondo_inversion
      AND gf.f_valuacion        = v_fcorte
   ;

   --Saldos para el fondo 0 en el que no se manejan acciones
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_mensual
   SELECT ts.id_derechohabiente,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          ts.monto_pesos,
          ts.v_fcorte
     FROM tmp_saldo_mensual ts
   WHERE	 ts.fondo_inversion		 = 0
   ;

   --Se genera el saldo mensual global
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_mensual_global
   SELECT
      sdo.f_saldo,
      sdo.subcuenta,
      sdo.fondo_inversion,
      SUM(sdo.monto_acciones),
      SUM(sdo.monto_pesos)
   FROM safre_sdo@vivws_tcp:cta_saldo_mensual sdo
   GROUP BY sdo.f_saldo,sdo.subcuenta, sdo.fondo_inversion
   ;

   --Saldos para los movimientos de Decreto
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_mensual_global
   SELECT
      v_fcorte,
      sdo.subcuenta,
      sdo.fondo_inversion,
      SUM(sdo.monto_acciones),
      SUM(sdo.monto_acciones * gf.precio_fondo)
   FROM safre_viv:cta_decreto sdo
   INNER JOIN safre_viv:glo_valor_fondo gf ON (gf.fondo = sdo.fondo_inversion AND gf.f_valuacion = v_fcorte)
   WHERE sdo.fondo_inversion <> 0
   AND sdo.f_liquida <= v_fcorte
   GROUP BY sdo.subcuenta,
            sdo.fondo_inversion
   ;

   --Saldos para los movimientos de Decreto
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_mensual_global
   SELECT
      v_fcorte,
      sdo.subcuenta,
      10,
      SUM(sdo.importe),
      SUM(sdo.importe)
   FROM cta_fondo72 sdo
   WHERE sdo.movimiento <> 422
   AND sdo.f_liquida <= v_fcorte
   GROUP BY sdo.subcuenta
   ;

   DROP TABLE tmp_saldo_mensual;
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;

END FUNCTION;


