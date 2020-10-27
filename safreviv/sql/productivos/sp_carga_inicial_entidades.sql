






CREATE PROCEDURE "safreviv".sp_carga_inicial_entidades(p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO cat_entidad_federativa
   SELECT entidad_federativa,
          desc_entidad_federativa,
          abreviatura,
          cp_desde,
          cp_hasta
     FROM safre_mig:tmp_det_entidad;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


