






CREATE PROCEDURE "safreviv".sp_carga_inicial_ciudades(p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO cat_ciudad
   SELECT estado_ciudad,
          estado_ciudad[1,2],
          desc_ciudad,
          p_fecha_proceso,
          USER
     FROM safre_mig:tmp_det_ciudad;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


