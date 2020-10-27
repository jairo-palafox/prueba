






CREATE PROCEDURE "safreviv".sp_carga_hist_fondo72(p_folio DECIMAL(9,0))

   DEFINE v_nss                  CHAR(11);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_nombre               CHAR(40);
   DEFINE v_ejercicio            SMALLINT;
   DEFINE v_clave_mov            CHAR(2);
   DEFINE v_empresa              CHAR(40);
   DEFINE v_bimestres            SMALLINT;
   DEFINE v_importe              DECIMAL(10,2);


   SET PDQPRIORITY HIGH;
--   SET INDEXES FOR cta_his_fondo72 DISABLED;

   FOREACH 
      SELECT
         nss,
         rfc,
         nombre,
         ejercicio,
         clave_mov,
         empresa,
         bimestres,
         (importe/100)
      INTO 
         v_nss,
         v_rfc,
         v_nombre,
         v_ejercicio,
         v_clave_mov,
         v_empresa,
         v_bimestres,
         v_importe
      FROM safre_mig:tmp_hist_fondo72

      IF (v_clave_mov = 'RE' OR v_clave_mov = 'TR') THEN
         LET v_importe = v_importe * -1;
      END IF

      INSERT INTO cta_his_fondo72 VALUES(seq_cta_his_fondo72.nextval,
                                          v_nss,
                                          v_rfc,
                                          v_nombre,
                                          p_folio,
                                          v_ejercicio,
                                          v_clave_mov,
                                          v_empresa,
                                          v_bimestres,
                                          v_importe,
                                          0);
   END FOREACH;

--   SET INDEXES FOR cta_his_fondo72 ENABLED;
   UPDATE STATISTICS FOR TABLE cta_his_fondo72;
   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


