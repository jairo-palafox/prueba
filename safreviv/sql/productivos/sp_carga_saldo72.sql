






CREATE PROCEDURE "safreviv".sp_carga_saldo72()

   SET PDQPRIORITY HIGH;

   INSERT INTO cta_fondo72
   SELECT seq_cta_fondo72.NEXTVAL,
          rfc,
          nss,
          nombre,
          CASE tipo_saldo_virtual
             WHEN 'N' THEN (TO_NUMBER(importe_virtual)*-1)
             ELSE TO_NUMBER(importe_virtual)
          END CASE,
          estado,
          tipo_pension,
          prescripcion
     FROM safre_tmp:tmp_saldo72;
     
     SET PDQPRIORITY DEFAULT;
END PROCEDURE;


