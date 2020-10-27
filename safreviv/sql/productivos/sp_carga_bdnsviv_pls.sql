






CREATE PROCEDURE "safreviv".sp_carga_bdnsviv_pls(p_folio           DECIMAL(9,0),
                                      p_fecha_apertura  DATE )
   DEFINE v_movimiento   SMALLINT;
   DEFINE v_hoy          DATE;
   DEFINE v_hora         DATETIME HOUR TO SECOND;

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_nombre_imss          CHAR(50);
   DEFINE v_nombre_afore         CHAR(40);
   DEFINE v_paterno_afore        CHAR(40);
   DEFINE v_materno_afore        CHAR(40);
   DEFINE v_tipo_trabajador      CHAR(1);
   DEFINE v_marca_cred_viv       CHAR(1);
   DEFINE v_f_marca_viv          DATE;
   DEFINE v_f_valor97            DATE;
   DEFINE v_aivs97               DECIMAL(16,6);
   DEFINE v_pesos97              DECIMAL(12,2);
   DEFINE v_f_valor92            DATE;
   DEFINE v_aivs92               DECIMAL(16,6);
   DEFINE v_pesos92              DECIMAL(12,2);


   SET PDQPRIORITY HIGH;

   LET v_movimiento = 999;
   LET v_hora       = CURRENT;
   LET p_fecha_apertura = "11012012";

   FOREACH SELECT tm.nss,
                  tm.curp,
                  tm.rfc,
                  tm.nombre_imss,
                  tm.nombre_afore,
                  tm.paterno_afore,
                  tm.materno_afore,
                  tm.tipo_trabajador,
                  tm.marca_cred_viv,
                  tm.f_marca_viv,
                  tm.f_valor97,
                  (tm.aivs97/1000000),
                  (tm.pesos97),
                  tm.f_valor92,
                  (tm.aivs92/1000000),
                  (tm.pesos92)
             INTO v_nss,
                  v_curp,
                  v_rfc,
                  v_nombre_imss,
                  v_nombre_afore,
                  v_paterno_afore,
                  v_materno_afore,
                  v_tipo_trabajador,
                  v_marca_cred_viv,
                  v_f_marca_viv,
                  v_f_valor97,
                  v_aivs97,
                  v_pesos97,
                  v_f_valor92,
                  v_aivs92,
                  v_pesos92
             FROM safre_mig:tmp_bdnsviv_pls tm

      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      LET v_pesos97 = v_aivs97 * 1.683860;
      LET v_pesos92 = v_aivs92 * 1.683860;

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
                                                'S',
                                                "I",
                                                0,
                                                "",
                                                p_folio,
                                                p_fecha_apertura,
                                                p_fecha_apertura,
                                                0,
                                                p_fecha_apertura);
         LET v_id_derechohabiente = seq_derechohabiente.CURRVAL ;
      END IF

      IF v_aivs97 <> 0 OR
         v_pesos97 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(v_f_valor97,
                                          v_id_derechohabiente,
                                          44,
                                          11,
                                          v_movimiento,
                                          p_folio,
                                          0,
                                          v_aivs97,
                                          v_pesos97,
                                          v_f_valor97,
                                          p_fecha_apertura,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF

      IF v_aivs92 <> 0 OR
         v_pesos92 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(v_f_valor92,
                                          v_id_derechohabiente,
                                          42,
                                          11,
                                          v_movimiento,
                                          p_folio,
                                          0,
                                          v_aivs92,
                                          v_pesos92,
                                          v_f_valor92,
                                          p_fecha_apertura,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF
   END FOREACH;

   UPDATE STATISTICS FOR TABLE cta_movimiento;
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


