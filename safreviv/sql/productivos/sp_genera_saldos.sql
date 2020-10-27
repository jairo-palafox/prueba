






CREATE PROCEDURE "safreviv".sp_genera_saldos( p_fcorte DATE)


SET PDQPRIORITY HIGH;

SELECT id_derechohabiente,
       subcuenta,
       fondo_inversion,
       p_fcorte f_corte,
       SUM(monto_acciones) monto_acciones
  FROM cta_movimiento
 WHERE f_liquida <= p_fcorte
 GROUP BY id_derechohabiente,
          subcuenta,
          fondo_inversion
 INTO TEMP tmp_saldos;

INSERT INTO safre_sdo@vivsd_tcp:cta_saldo_dia
SELECT ts.id_derechohabiente,
       ad.nss,
       ad.curp,
       ad.rfc,
       ts.subcuenta,
       ts.fondo_inversion,
       ts.monto_acciones,
       (ts.monto_acciones * gf.precio_fondo),
       p_fcorte,
       gf.precio_fondo,
       ad.nombre_af,
       ad.ap_paterno_af,
       ad.ap_materno_af
  FROM tmp_saldos ts,
       afi_derechohabiente ad,
       glo_valor_fondo gf
 WHERE ad.id_derechohabiente = ts.id_derechohabiente
   AND gf.fondo              = ts.fondo_inversion
   AND gf.f_valuacion        = p_fcorte
;
END PROCEDURE
;


