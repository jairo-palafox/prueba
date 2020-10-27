






CREATE PROCEDURE "safreviv".sp_carga_bdnsviv_prc(p_folio DECIMAL(9,0),
                                      p_fecha_liquida DATE,
                                      p_fecha_proceso DATE)

   DEFINE v_movimiento   SMALLINT;
   DEFINE v_hora         DATETIME HOUR TO SECOND;

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_nombre_imss          CHAR(50);
   DEFINE v_nombre_afore         CHAR(40);
   DEFINE v_paterno_afore        CHAR(40);
   DEFINE v_materno_afore        CHAR(40);
   DEFINE v_aivs97               DECIMAL(16,6);
   DEFINE v_pesos97              DECIMAL(12,2);
   DEFINE v_aivs92               DECIMAL(16,6);
   DEFINE v_pesos92              DECIMAL(12,2);

   SET PDQPRIORITY HIGH;
   SET INDEXES FOR cta_movimiento DISABLED;

   LET v_movimiento = 999;
   LET v_hora       = CURRENT;

   FOREACH SELECT tm.nss,
                  tm.curp,
                  tm.rfc,
                  tm.nombre_procanase,
                  tm.nombre,
                  tm.ap_paterno,
                  tm.ap_materno,
                  (tm.aivs_viv97/1000000),
                  (tm.saldo_viv97/100),
                  (tm.aivs_viv92/1000000),
                  (tm.saldo_viv92/100)
             INTO v_nss,
                  v_curp,
                  v_rfc,
                  v_nombre_imss,
                  v_nombre_afore,
                  v_paterno_afore,
                  v_materno_afore,
                  v_aivs97,
                  v_pesos97,
                  v_aivs92,
                  v_pesos92
             FROM safre_mig:tmp_bdnsviv_prc tm

      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      IF DBINFO('sqlca.sqlerrd2') == 0 THEN
         INSERT INTO afi_derechohabiente VALUES(seq_derechohabiente.NEXTVAL,
                                                v_nss,
                                                v_curp,
                                                v_rfc,
                                                "",
                                                fn_fnacimiento(v_nss,v_curp,v_rfc),
                                                v_nombre_imss,
                                                v_nombre_afore,
                                                v_paterno_afore,
                                                v_materno_afore,
                                                v_rfc[11],
                                                "I",
                                                "I",
                                                0,
                                                "",
                                                p_folio,
                                                p_fecha_liquida,
                                                p_fecha_proceso,
                                                0,
                                                p_fecha_proceso);
         LET v_id_derechohabiente = seq_derechohabiente.CURRVAL ;
      END IF

      IF v_aivs97 <> 0 OR
         v_pesos97 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(p_fecha_liquida,
                                          v_id_derechohabiente,
                                          4,
                                          11,
                                          v_movimiento,
                                          p_folio,
                                          0,
                                          v_aivs97,
                                          v_pesos97,
                                          p_fecha_liquida,
                                          p_fecha_proceso,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF

      IF v_aivs92 <> 0 OR
         v_pesos92 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(p_fecha_liquida,
                                          v_id_derechohabiente,
                                          8,
                                          11,
                                          v_movimiento,
                                          p_folio,
                                          0,
                                          v_aivs92,
                                          v_pesos92,
                                          p_fecha_liquida,
                                          p_fecha_proceso,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF
   END FOREACH;

   SET INDEXES FOR cta_movimiento ENABLED;
   UPDATE STATISTICS FOR TABLE cta_movimiento;
   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


