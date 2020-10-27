






CREATE PROCEDURE "safreviv".sp_carga_fondo72(p_folio DECIMAL(9,0),
                                  p_fecha DATE)

   DEFINE v_movimiento   SMALLINT;
   DEFINE v_hora         DATETIME HOUR TO SECOND;
   DEFINE v_subcuenta    SMALLINT;

   DEFINE v_id_fondo72           DECIMAL(9,0);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_nombre               CHAR(40);
   DEFINE v_importe              DECIMAL(20,2);
   DEFINE v_tpo_sdo_virtual      CHAR(1);
   DEFINE v_edo_pago             CHAR(1);
   DEFINE v_id_derechohabiente   DECIMAL(9,0);

   SET PDQPRIORITY HIGH;
   SET INDEXES FOR cta_fondo72 DISABLED;
   SET INDEXES FOR afi_fondo72 DISABLED;

   LET v_movimiento = 999;
   LET v_hora       = CURRENT;
   LET v_subcuenta  = 40;
   LET v_id_derechohabiente = NULL;

   FOREACH SELECT tm.rfc,
   	          tm.nss,
                  tm.nombre,
                  tm.importe_virtual,
                  tm.tpo_sdo_virtual,
                  tm.edo_pago
             INTO v_rfc,
                  v_nss,
                  v_nombre,
                  v_importe,
                  v_tpo_sdo_virtual,
                  v_edo_pago
             FROM safre_mig:tmp_fondo72 tm

     { SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;
}
      INSERT INTO afi_fondo72 VALUES(seq_afi_fondo72.NEXTVAL,
                                     v_rfc,
                                     v_nss,
                                     v_id_derechohabiente,
                                     v_nombre,
                                     p_fecha);
                                     
      LET v_id_fondo72 = seq_afi_fondo72.CURRVAL ;

      IF v_tpo_sdo_virtual <> 'N' THEN
         IF v_edo_pago = ' ' THEN
            LET v_edo_pago =  NULL;
         END IF

         INSERT INTO cta_fondo72 VALUES(v_id_fondo72,
                                        p_fecha,
                                        v_subcuenta,
                                        v_movimiento,
                                        p_folio,
                                        "",
                                        v_importe,
                                        v_edo_pago,
                                        TODAY,
                                        v_hora,
                                        "CARGA INICIAL");
      END IF
   END FOREACH;

   SET INDEXES FOR cta_fondo72 ENABLED;
   SET INDEXES FOR afi_fondo72 ENABLED;
   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


