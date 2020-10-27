






CREATE PROCEDURE "safreviv".sp_carga_inicial_afi(p_folio DECIMAL(9,0),
                                      p_fecha_apertura DATE)

   SET PDQPRIORITY HIGH;
   SET INDEXES FOR afi_derechohabiente DISABLED;


   INSERT INTO afi_derechohabiente
   SELECT seq_derechohabiente.NEXTVAL,
          nss,
          curp,
          rfc,
          ind_relacion_laboral,
          CASE WHEN (f_nacimiento IS NULL OR
                     f_nacimiento == '12/31/1899') THEN
                  fn_fnacimiento(nss,curp,rfc)
               ELSE
                  f_nacimiento
          END CASE,
          nombre_imss,
          nombre,
          ap_paterno,
          ap_materno,
          sexo,
          tipo_trabajador,
          origen_afiliacion,
          "0",
          "",
          p_folio,
          p_fecha_apertura,
          p_fecha_apertura,
          0,
          p_fecha_apertura
     FROM safre_mig:tmp_det_derechohabiente;

   SET INDEXES FOR afi_derechohabiente ENABLED;
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   SET PDQPRIORITY DEFAULT;
END PROCEDURE
;


