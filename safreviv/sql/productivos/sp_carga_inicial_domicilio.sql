






CREATE PROCEDURE "safreviv".sp_carga_inicial_domicilio(p_folio DECIMAL(9,0), 
                                            p_fecha_proceso DATE)

   SET PDQPRIORITY HIGH;

   SET INDEXES FOR afi_domicilio DISABLED;

   INSERT INTO afi_domicilio
   SELECT a.id_derechohabiente,
          "1",
          b.tipo_domicilio,
          "1",
          b.calle,
          b.num_exterior,
          b.num_interior,
          b.colonia,
          b.cp,
          b.entre_calle1,
          b.entre_calle2,
          p_folio,
          p_fecha_proceso,
          USER
     FROM safre_mig:tmp_det_domicilio b, 
          afi_derechohabiente a
    WHERE a.nss = b.nss;

   SET INDEXES FOR afi_domicilio ENABLED;
   SET PDQPRIORITY DEFAULT;
END PROCEDURE
;


