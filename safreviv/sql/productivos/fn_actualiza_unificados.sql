






CREATE FUNCTION "safreviv".fn_actualiza_unificados()
RETURNING VARCHAR(200);

   DEFINE v_mensaje              VARCHAR(100);

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   SET PDQPRIORITY HIGH;

   --Primero se ponen a cero los unificados
   FOREACH
      SELECT id_derechohabiente 
      INTO v_id_derechohabiente
      FROM nss_unificado
      WHERE id_excepcion = 1

      IF EXISTS ( SELECT tabla_saldo
                 FROM safre_sdo@vivws_tcp:glo_saldo
                WHERE tabla_saldo = "cta_saldo_diario"
                  AND ind_saldo = 1
             ) THEN
         UPDATE safre_sdo@vivws_tcp:cta_saldo_diario SET monto_acciones = 0, monto_pesos = 0 WHERE id_derechohabiente = v_id_derechohabiente;
      ELSE
         UPDATE safre_sdo@vivws_tcp:cta_saldo_diario_bis SET monto_acciones = 0, monto_pesos = 0 WHERE id_derechohabiente = v_id_derechohabiente;
      END IF
   END FOREACH;

   --Se calcula el saldo de los uificadores
   FOREACH
      SELECT id_derechohabiente 
      INTO v_id_derechohabiente
      FROM nss_unificador
      WHERE id_excepcion = 1

      DROP TABLE IF EXISTS tmp_saldos;
      SELECT   id_derechohabiente,
               subcuenta,
               fondo_inversion,
               MDY(1,8,2014) v_fcorte,
               SUM(monto_acciones) monto_acciones,
               SUM(monto_pesos) monto_pesos
      FROM cta_movimiento
      WHERE f_liquida <= MDY(1,8,2014)
      AND id_derechohabiente = v_id_derechohabiente
      GROUP BY id_derechohabiente,
      subcuenta,
      fondo_inversion
      INTO TEMP tmp_saldos;
    

      IF EXISTS ( SELECT tabla_saldo
                 FROM safre_sdo@vivws_tcp:glo_saldo
                WHERE tabla_saldo = "cta_saldo_diario"
                  AND ind_saldo = 1
             ) THEN
         DELETE FROM safre_sdo@vivws_tcp:cta_saldo_diario WHERE id_derechohabiente = v_id_derechohabiente;
         
         INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario
         SELECT ts.id_derechohabiente,
                ts.subcuenta,
                ts.fondo_inversion,
                ts.monto_acciones,
                (ts.monto_acciones * gf.precio_fondo),
                ts.v_fcorte
           FROM tmp_saldos ts,
                glo_valor_fondo gf
          WHERE ts.fondo_inversion		 <> 0
            AND gf.fondo          = ts.fondo_inversion
            AND gf.f_valuacion    = v_fcorte
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
          WHERE ts.fondo_inversion = 0
          ;
      ELSE
         DELETE FROM safre_sdo@vivws_tcp:cta_saldo_diario_bis WHERE id_derechohabiente = v_id_derechohabiente;

         INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario_bis
         SELECT ts.id_derechohabiente,
                ts.subcuenta,
                ts.fondo_inversion,
                ts.monto_acciones,
                (ts.monto_acciones * gf.precio_fondo),
                ts.v_fcorte
           FROM tmp_saldos ts,
                glo_valor_fondo gf
          WHERE ts.fondo_inversion		 <> 0
            AND gf.fondo          = ts.fondo_inversion
            AND gf.f_valuacion    = v_fcorte
         ;
      
         --Saldos para el fondo 0 en el que no se manejan acciones
         INSERT INTO safre_sdo@vivws_tcp:cta_saldo_diario_bis
         SELECT ts.id_derechohabiente,
                ts.subcuenta,
                ts.fondo_inversion,
                ts.monto_acciones,
                ts.monto_pesos,
                ts.v_fcorte
           FROM tmp_saldos ts
          WHERE ts.fondo_inversion = 0
          ;
      END IF
   END FOREACH;

   DROP TABLE IF EXISTS tmp_saldos;
   LET v_mensaje = "Proceso ejecutado";
   
	RETURN v_mensaje;
END FUNCTION;


