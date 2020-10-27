






CREATE PROCEDURE "safreviv".sp_carga_inicial_referencia(p_folio DECIMAL(9,0), 
                                             p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   INSERT INTO afi_referencia
   SELECT a.id_derechohabiente,
          "1",
          b.curp,
          b.rfc,
          b.nombre,
          b.ap_paterno,
          b.ap_materno,
          b.calle,
          b.num_exterior,
          b.num_interior,
          b.colonia,
          b.codigo_postal,
          b.lada,
          b.telefono,
          b.tipo_telefono,
          b.correo_electronico,
          p_folio,
          p_fecha_proceso,
          USER
     FROM safre_mig:tmp_det_referencia b, 
          afi_derechohabiente_tmp a
    WHERE a.nss = b.nss;

   SET PDQPRIORITY DEFAULT;
END PROCEDURE
;


