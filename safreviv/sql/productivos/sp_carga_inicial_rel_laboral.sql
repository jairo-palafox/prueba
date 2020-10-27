






CREATE PROCEDURE "safreviv".sp_carga_inicial_rel_laboral(p_folio DECIMAL(9,0),
                                              p_fecha_proceso DATE)
                                              
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_nrp                CHAR(11);
   DEFINE v_falta              DATE;

   SET PDQPRIORITY HIGH;

   FOREACH SELECT a.id_derechohabiente,
                  b.nrp,
                  b.falta
             INTO v_id_derechohabiente,
                  v_nrp,
                  v_falta
             FROM safre_mig:tmp_det_relacion_laboral b,
                  afi_derechohabiente a
            WHERE a.nss = b.nss

   IF NOT EXISTS (SELECT id_derechohabiente
                    FROM afi_relacion_laboral
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND nrp                = v_nrp) THEN
   
      INSERT INTO afi_relacion_laboral VALUES(v_id_derechohabiente,
                                              v_nrp,
                                              1,
                                              v_falta,
                                              p_folio,
                                              p_fecha_proceso,
                                              USER);

   END IF;
   END FOREACH;
   SET PDQPRIORITY DEFAULT;
END PROCEDURE;


