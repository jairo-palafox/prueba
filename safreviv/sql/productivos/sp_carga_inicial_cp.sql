






CREATE PROCEDURE "safreviv".sp_carga_inicial_cp(p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO cat_cp
   SELECT codigo_postal,
          entidad_federativa,
          ciudad,
          municipio,
          p_fecha_proceso,
          USER
     FROM safre_mig:tmp_det_codigo_postal;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


