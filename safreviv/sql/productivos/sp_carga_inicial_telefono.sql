






CREATE PROCEDURE "safreviv".sp_carga_inicial_telefono(p_folio DECIMAL(9,0), 
                                            p_fecha_proceso DATE)
DEFINE v_id_telefono    SMALLINT;
DEFINE v_max_reg        SMALLINT;
DEFINE v_id_derechohabiente decimal(9,0);
DEFINE v_cve_lada char(3);
DEFINE v_extension char(10);
DEFINE v_telefono char(13);
DEFINE v_tpo_telefono smallint;


   SET PDQPRIORITY HIGH;

   FOREACH SELECT a.id_derechohabiente,
          b.lada,
          b.extension,
          b.telefono,
          b.tipo_telefono
     INTO v_id_derechohabiente,
          v_cve_lada,
          v_extension,
          v_telefono,
          v_tpo_telefono
     FROM safre_mig:tmp_det_telefono b,
          afi_derechohabiente a
    WHERE a.nss = b.nss

   SELECT MAX(id_telefono)+1
     INTO v_id_telefono
     FROM afi_telefono
    WHERE id_derechohabiente = v_id_derechohabiente;

   IF v_id_telefono IS NULL THEN
     LET v_id_telefono = 1;
   END IF

   INSERT INTO afi_telefono VALUES(
          v_id_derechohabiente,
          v_id_telefono,
          v_cve_lada,
          v_extension,
          v_telefono,
          v_tpo_telefono,
          p_folio);
    END FOREACH;
   SET PDQPRIORITY DEFAULT;
END PROCEDURE
;


