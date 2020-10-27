GLOBALS "procesarCuentaClabe.dir/WSDL/ret_procesar_cuenta_clabe.inc" -- saldo prelimminar PROCESAR

MAIN
DEFINE v_resultado SMALLINT

  DISPLAY "Prueba de comunicacion con WS consulta Cuenta CLABE PROCESAR"  
  LET ns1notificacionCuentaClabeRequest.idssn.idSistema      = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.idEbusiness    = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.idPortafolio   = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.idServicio     = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.idCliente      = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.idCanal        = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.codoperCliente = 1 
  LET ns1notificacionCuentaClabeRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
  
  LET ns1notificacionCuentaClabeRequest.cuerpo.folioOperacion                       = 1 -- tns3alfanumerico_10 ATTRIBUTE(XMLName="folioOperacion",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.folioNotificacion                    = 1 -- tns3alfanumerico_29 ATTRIBUTE(XMLName="folioNotificacion",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.indicadorBeneficiario                = 1 -- tns3numerico_1 ATTRIBUTE(XMLName="indicadorBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.entidadFederativa                    = 1 -- tns3alfanumerico_2 ATTRIBUTE(XMLName="entidadFederativa",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.nss                                  = "01004200265" -- tns3nss ATTRIBUTE(XMLName="nss",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.rfcTrabajador                        = "LEMR420215FW3" -- tns3rfc ATTRIBUTE(XMLName="rfcTrabajador",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.curpTrabajador                       = "LEMR420215HNECNY05" -- tns3curp ATTRIBUTE(XMLName="curpTrabajador",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.clabe                                = "112233445566778899" -- tns3clabe ATTRIBUTE(XMLName="clabe",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.grupoTrabajador                      = "1" -- tns3alfanumerico_4_no_especiales ATTRIBUTE(XMLName="grupoTrabajador",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.secuenciaPension                     = "1" -- tns3alfanumerico_2_no_especiales ATTRIBUTE(XMLName="secuenciaPension",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.regimen                              = "73" -- tns3alfanumerico_2 ATTRIBUTE(XMLName="regimen",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.tipoRetiro                           = "E" -- tns3alfanumerico_1_no_especiales ATTRIBUTE(XMLName="tipoRetiro",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.tipoSeguro                           = "CV" -- tns3alfanumerico_2_no_especiales ATTRIBUTE(XMLName="tipoSeguro",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPension                          = "IP" -- tns3alfanumerico_2_no_especiales ATTRIBUTE(XMLName="tipoPension",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPrestacion                       = "00" -- tns3alfanumerico_2_no_especiales ATTRIBUTE(XMLName="tipoPrestacion",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.semanasCotizadas                     = "500" -- tns3alfanumerico_4_numerico ATTRIBUTE(XMLName="semanasCotizadas",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.nombreBeneficiario                   = "AQUILES" -- tns3alfanumerico_40 ATTRIBUTE(XMLName="nombreBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoPaternoBeneficiario          = "SERDAN" -- tns3alfanumerico_40 ATTRIBUTE(XMLName="apellidoPaternoBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoMaternoBeneficiario          = "PLIEGO" -- tns3alfanumerico_40 ATTRIBUTE(XMLName="apellidoMaternoBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.rfcBeneficiario                      = "AAAA510203RFC" -- tns3rfc ATTRIBUTE(XMLName="rfcBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.curpBeneficiario                     = "AAAA51023HMCGRV08" -- tns3curp ATTRIBUTE(XMLName="curpBeneficiario",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore1                        = "0" -- tns3alfanumerico_2_numerico ATTRIBUTE(XMLName="claveSiefore1",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore1                = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="claveRetiro92Siefore1",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore1                = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="claveRetiro97Siefore1",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore1   = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="otrosImportesAhorro73ClaveSiefore1",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore1   = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="importeNetoDepositadoClaveSiefore1",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore2                        = "11" -- tns3alfanumerico_2_numerico ATTRIBUTE(XMLName="claveSiefore2",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore2                = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="claveRetiro92Siefore2",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore2                = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="claveRetiro97Siefore2",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore2   = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="otrosImportesAhorro73ClaveSiefore2",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore2   = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="importeNetoDepositadoClaveSiefore2",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoSiefores        = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="importeNetoDepositadoSiefores",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92Aivs                       = 1 -- tns3numerico_13_2 ATTRIBUTE(XMLName="vivienda92Aivs",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97Aivs                       = 1 -- tns3numerico_13_2 ATTRIBUTE(XMLName="vivienda97Aivs",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable = CURRENT YEAR TO FRACTION -- DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fechaValorViviendaMovimientoContable",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92                           = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="vivienda92",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97                           = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="vivienda97",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesVivienda                = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="otrosImportesVivienda",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoVivienda        = 0 -- tns3numerico_13_2 ATTRIBUTE(XMLName="importeNetoDepositadoVivienda",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.diagnosticoRecepcion                 = "01" -- tns3alfanumerico_3 ATTRIBUTE(XMLName="diagnosticoRecepcion",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.comentarios                          = "NA" -- tns3alfanumerico_127 ATTRIBUTE(XMLName="comentarios",XMLNamespace="",XMLOptional),
  LET ns1notificacionCuentaClabeRequest.cuerpo.fechaPago                            = CURRENT YEAR TO FRACTION -- DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fechaPago",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.referenciaPago                       = "NA" -- tns3alfanumerico_20 ATTRIBUTE(XMLName="referenciaPago",XMLNamespace=""),
  LET ns1notificacionCuentaClabeRequest.cuerpo.observaciones                        = "NA" -- tns3alfanumerico_127 ATTRIBUTE(XMLName="observaciones",XMLNamespace="",XMLOptional)

  -- se ejecuta el WS
  CALL notificacionCuentaClabe_g() RETURNING v_resultado
  
  DISPLAY "Resultado de la ejecucion: ", v_resultado
  DISPLAY "CODE       : ", wsError.code
  DISPLAY "CODENS     : ", wsError.codeNS
  DISPLAY "DESCRIPTION: ", wsError.description
  DISPLAY "ACTION     : ", wsError.action
  DISPLAY "=========================================================\n"

  
  
END MAIN