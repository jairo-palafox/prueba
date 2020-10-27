






CREATE FUNCTION "safreviv".fn_genera_saldo_diario_tmp(v_fcorte DATE)
RETURNING SMALLINT, VARCHAR(100);

   --DEFINE v_fcorte               DATE;
   DEFINE v_fglobal              DATE;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   SET PDQPRIORITY HIGH;

   --Se establece la fecha de corte con el dia inmediato anterior
--   LET v_fcorte = TODAY - 1; 
   
   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "El saldo del dia " || v_fcorte || " se creo correctamente";
   
   --Se genera una tabla temporal con los saldos para cada derechohabiente a la fecha de corte
   DROP TABLE IF EXISTS tmp_saldos;
   SELECT id_derechohabiente,
          subcuenta,
          fondo_inversion,
          v_fcorte v_fcorte,
          SUM(monto_acciones) monto_acciones,
          SUM(monto_pesos) monto_pesos
     FROM safre_viv:tmp_movimiento
    WHERE f_liquida <= v_fcorte
    GROUP BY id_derechohabiente,
             subcuenta,
             fondo_inversion
    INTO TEMP tmp_saldos;

   --Se genera el saldo diario para cada derechohabiente

   --Saldos para los fondos de inversion con precio de accion
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario
   SELECT ts.id_derechohabiente,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          (ts.monto_acciones * gf.precio_fondo),
          ts.v_fcorte
     FROM tmp_saldos ts,
          safre_viv:glo_valor_fondo gf
   WHERE	 ts.fondo_inversion		 <> 0
       AND gf.fondo              = ts.fondo_inversion
      AND gf.f_valuacion        = v_fcorte
   ;

   --Saldos para el fondo 0 en el que no se manejan acciones
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario
   SELECT ts.id_derechohabiente,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          ts.monto_pesos,
          ts.v_fcorte
     FROM tmp_saldos ts
   WHERE	 ts.fondo_inversion		 = 0
   ;

   --Se genera el saldo diario global

   --Saldos para los fondos de inversion con precio de accion
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario_global
   SELECT ts.v_fcorte,
          ts.subcuenta,
          ts.fondo_inversion,
          SUM(ts.monto_acciones),
          SUM(ts.monto_acciones * gf.precio_fondo)
     FROM tmp_saldos ts,
          safre_viv:glo_valor_fondo gf
   WHERE	 ts.fondo_inversion		 <> 0
       AND gf.fondo              = ts.fondo_inversion
      AND gf.f_valuacion        = v_fcorte
   GROUP BY ts.v_fcorte,
                ts.subcuenta,
             ts.fondo_inversion
   ;

   --Saldos para el fondo 0 en el que no se manejan acciones
   INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario_global
   SELECT ts.v_fcorte,
          ts.subcuenta,
          ts.fondo_inversion,
          SUM(ts.monto_acciones),
          SUM(ts.monto_pesos)
     FROM tmp_saldos ts
   WHERE	 ts.fondo_inversion		 = 0
   GROUP BY ts.v_fcorte,
                ts.subcuenta,
             ts.fondo_inversion
   ;

   DROP TABLE tmp_saldos;
   SET PDQPRIORITY DEFAULT;

   RETURN v_resultado, v_mensaje;
END FUNCTION;


