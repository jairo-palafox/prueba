






CREATE PROCEDURE "safreviv".sp_carga_inicial_colonia(p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO cat_colonia
   SELECT colonia_cod,
          codigo_postal,
          colonia_desc
     FROM safre_mig:tmp_det_colonia;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


