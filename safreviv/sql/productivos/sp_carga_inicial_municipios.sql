






CREATE PROCEDURE "safreviv".sp_carga_inicial_municipios(p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO cat_municipio
   SELECT estado_municipio,
          estado_municipio[1,2],
          desc_municipio,
          p_fecha_proceso,
          USER
     FROM safre_mig:tmp_det_municipio;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


