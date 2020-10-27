GLOBALS

DEFINE wsErr RECORD
	code		STRING,
	codeNS		STRING,
	description	STRING,
	action		STRING
END RECORD

#
# TYPE : tDT_CargarCampana_in
#
TYPE tDT_CargarCampana_in RECORD 
  archivoB64 STRING,
  fechaOperacion STRING ,
  idCampana STRING ,
  nombreArchivo STRING ,
  noColumnas STRING,
  noRegistros STRING 
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tDT_CargarCampana_res
#
TYPE tDT_CargarCampana_out RECORD 
  codigoRetorno STRING,
  descripcion STRING 
END RECORD
#-------------------------------------------------------------------------------






END GLOBALS
