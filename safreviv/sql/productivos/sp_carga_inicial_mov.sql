






CREATE PROCEDURE "safreviv".sp_carga_inicial_mov()
RETURNING DECIMAL
   DEFINE v_folio        DECIMAL(10,0);
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
   DEFINE v_aivs97               DECIMAL(20,2);
   DEFINE v_pesos97              DECIMAL(20,2);
   DEFINE v_f_valor92            DATE;
   DEFINE v_aivs92               DECIMAL(20,2);
   DEFINE v_pesos92              DECIMAL(20,2);


   SET PDQPRIORITY HIGH;
   SET INDEXES FOR cta_movimiento DISABLED;

   LET v_folio      = fn_genera_folio(0,0,0,USER);
   LET v_hoy        = TODAY;
   LET v_movimiento = 999;
   LET v_hora       = CURRENT;

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
                  (tm.pesos97/100),
                  tm.f_valor92,
                  (tm.aivs92/1000000),
                  (tm.pesos92/100)
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
             FROM safre_tmp:tmp_bdnsviv_inicio tm
            WHERE tm.nss in ( SELECT afi.nss 
                             FROM safre_tmp:afi_derechohabiente  afi,
                                  pag_det_trabajador pag
                            WHERE afi.id_derechohabiente = pag.id_derechohabiente)

      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      IF DBINFO('sqlca.sqlerrd2') == 0 THEN
         IF v_nss MATCHES "77*" THEN
            LET v_tipo_trabajador = 'S';
         END IF

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
                                                v_tipo_trabajador,
                                                "A",
                                                v_marca_cred_viv,
                                                v_f_marca_viv);
         LET v_id_derechohabiente = seq_derechohabiente.CURRVAL ;
      END IF

      IF v_aivs97 <> 0 OR
         v_pesos97 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(v_f_valor97,
                                          v_id_derechohabiente,
                                          4,
                                          11,
                                          v_movimiento,
                                          v_folio,
                                          0,
                                          v_aivs97,
                                          v_pesos97,
                                          v_f_valor97,
                                          v_hoy,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF

      IF v_aivs92 <> 0 OR
         v_pesos92 <> 0 THEN
         INSERT INTO cta_movimiento VALUES(v_f_valor92,
                                          v_id_derechohabiente,
                                          8,
                                          11,
                                          v_movimiento,
                                          v_folio,
                                          0,
                                          v_aivs92,
                                          v_pesos92,
                                          v_f_valor92,
                                          v_hoy,
                                          v_hora,
                                          "CARGA INICIAL");
      END IF
   END FOREACH;

   SET INDEXES FOR cta_movimiento ENABLED;
   SET PDQPRIORITY DEFAULT;

   RETURN v_folio;
END PROCEDURE
;


