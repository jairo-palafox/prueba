GLOBALS "procesarSaldoPreliminar.dir/WSDL/ret_procesar_saldo_preliminar.inc" -- saldo prelimminar PROCESAR

MAIN
DEFINE v_resultado SMALLINT

  DISPLAY "Prueba de comunicacion con PROCESAR, saldo previo"
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = 1 
  LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION

  LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = "LEMR420215HNECNY05"
  LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = "01004200265"
  
  -- se invoca el servicio
  DISPLAY "Ejecutando WSSaldoPreliminar..."
  CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado
  
  DISPLAY "Resultado de la ejecucion: ", v_resultado
  DISPLAY "CODE       : ", wsError.code
  DISPLAY "CODENS     : ", wsError.codeNS
  DISPLAY "DESCRIPTION: ", wsError.description
  DISPLAY "ACTION     : ", wsError.action
  DISPLAY "=========================================================\n"

  
  --DISPLAY "Prueba de comunicacion con PROCESAR, cuenta clabe"
  


END MAIN