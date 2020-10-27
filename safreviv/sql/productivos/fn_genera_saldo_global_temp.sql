






CREATE FUNCTION "safreviv".fn_genera_saldo_global_temp(v_fcorte DATE)
RETURNING SMALLINT, VARCHAR(100);

   DEFINE p_folio 		 DECIMAL(9,0);
   DEFINE v_estado_ant           SMALLINT;
   DEFINE v_fglobal              DATE;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);
   DEFINE v_ind_consistencia     SMALLINT;

   DEFINE v_avance_apo           DECIMAL(22,2);
   DEFINE v_avance_amo           DECIMAL(22,2);

   SET PDQPRIORITY HIGH;

   --Se establece el folio
   LET p_folio = 1;
   --CALL fn_genera_folio(701,1,'safreviv') RETURNING p_folio;


   --Se establece el indicador de uno para todos los movimientos que no son de decreto
   LET v_ind_consistencia = 1;
   
   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "El saldo del dia " || v_fcorte || " se creo correctamente";
   
   --Se genera una tabla temporal con los saldos para cada derechohabiente a la fecha de corte
   DROP TABLE IF EXISTS tmp_saldos;
   SELECT subcuenta,
          fondo_inversion,
          v_fcorte v_fcorte,
          SUM(monto_acciones) monto_acciones,
          SUM(monto_pesos) monto_pesos
     FROM cta_movimiento
    WHERE f_liquida <= v_fcorte
    GROUP BY subcuenta,
             fondo_inversion
    INTO TEMP tmp_saldos;
    UPDATE STATISTICS FOR TABLE tmp_saldos;

   --Se genera el saldo diario global

   --Saldos para los fondos de inversion con precio de accion
   INSERT INTO safre_tmp:cta_sdo_global
   SELECT ts.v_fcorte,
          ts.subcuenta,
          ts.fondo_inversion,
          p_folio,
          v_ind_consistencia,
          SUM(ts.monto_acciones),
          SUM(ts.monto_acciones * gf.precio_fondo)
     FROM tmp_saldos ts,
          glo_valor_fondo gf
   WHERE	 ts.fondo_inversion		 <> 0
       AND gf.fondo              = ts.fondo_inversion
      AND gf.f_valuacion        = v_fcorte
   GROUP BY ts.v_fcorte,
                ts.subcuenta,
             ts.fondo_inversion
   ;

   --Saldos para el fondo 0 en el que no se manejan acciones
   DROP TABLE IF EXISTS tmp_liq_ctr_ava_sdo;
   SELECT 
          SUM(dd.monto_aportacion) ava_apo,
          SUM(dd.monto_amortizacion) ava_amo,
          0 can_apo,
          0 can_amo,
          NVL(sum(dc.monto_apo_avance),0) comp_apo,
          NVL(sum(dc.monto_amo_avance),0) comp_amo
     FROM dis_det_avance_pago dd,
    OUTER dis_compensa_avance dc
    WHERE dd.id_dis_det_avance_pago = dc.id_dis_det_avance_pago
      AND dd.periodo_pago           = dc.periodo_pago
      AND dd.tpo_avance             = 181
    UNION ALL
   SELECT
          0 ava_apo,
          0 ava_amo,
          SUM(da.monto_aportacion * -1) can_apo,
          SUM(da.monto_amortizacion * -1) can_amo,
          0 comp_apo,
          0 comp_amo
     FROM dis_det_avance_pago da
    WHERE da.tpo_avance IN (1811,1812)
    INTO TEMP tmp_liq_ctr_ava_sdo;

   SELECT   SUM(ava_apo - can_apo - comp_apo),
            SUM(ava_amo - can_amo - comp_amo)
   INTO     v_avance_apo,
            v_avance_amo
   FROM tmp_liq_ctr_ava_sdo;

   --Se inserta el avance
   INSERT INTO safre_tmp:cta_sdo_global
   VALUES   (  v_fcorte,
               4,
               0,
               p_folio,
               v_ind_consistencia,
               0,
               v_avance_apo
            );

   --Se inserta la amortizacion
   INSERT INTO safre_tmp:cta_sdo_global
   VALUES   (  v_fcorte,
               41,
               0,
               p_folio,
               v_ind_consistencia,
               0,
               v_avance_amo
            );

   --Saldos para los movimientos de Decreto
   DROP TABLE IF EXISTS tmp_saldos;
   SELECT
      sdo.subcuenta,
      sdo.fondo_inversion,
      afi.ind_consistencia,
      SUM(sdo.monto_acciones) AS monto_acciones
   FROM cta_decreto sdo
   INNER JOIN afi_decreto afi ON afi.id_decreto = sdo.id_decreto
   WHERE sdo.fondo_inversion <> 0
   AND sdo.f_liquida <= v_fcorte
   GROUP BY sdo.subcuenta,
            sdo.fondo_inversion,
            afi.ind_consistencia
    INTO TEMP tmp_saldos;
    UPDATE STATISTICS FOR TABLE tmp_saldos;

   
   INSERT INTO safre_tmp:cta_sdo_global
   SELECT
      v_fcorte,
      sdo.subcuenta,
      sdo.fondo_inversion,
      p_folio,
      sdo.ind_consistencia,
      sdo.monto_acciones,
      (sdo.monto_acciones * gf.precio_fondo)
   FROM tmp_saldos sdo
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = sdo.fondo_inversion AND gf.f_valuacion = v_fcorte)
   ;

   --Saldos para los movimientos de Fondo Anterior
   INSERT INTO safre_tmp:cta_sdo_global
   SELECT
      v_fcorte,
      sdo.subcuenta,
      10,
      p_folio,
      v_ind_consistencia,
      0,
      SUM(sdo.importe)
   FROM cta_fondo72 sdo
   WHERE sdo.movimiento <> 422
   AND sdo.f_liquida <= v_fcorte
   GROUP BY sdo.subcuenta
   ;
   
   DROP TABLE IF EXISTS tmp_saldos;
   DROP TABLE IF EXISTS tmp_liq_ctr_ava_sdo;
   

   SET PDQPRIORITY DEFAULT;

   RETURN v_resultado, v_mensaje;
END FUNCTION;


