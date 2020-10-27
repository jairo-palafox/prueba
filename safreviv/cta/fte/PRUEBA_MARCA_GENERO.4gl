#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "CTAW12.inc"

MAIN
   DEFINE wsstatus   INTEGER
  
   #Se llenan los parametros a enviar
   LET solicitudMarcaRequest.solicitud.apeMaterno            = 'ALVARADO'
   LET solicitudMarcaRequest.solicitud.apePaterno            = 'HERNANDEZ'
   LET solicitudMarcaRequest.solicitud.nombres               = 'JULIA'
   LET solicitudMarcaRequest.solicitud.fechaPresentacion     = '20130603'--formato AAAAMMDD
   LET solicitudMarcaRequest.solicitud.nss                   = '01006401028'
   LET solicitudMarcaRequest.solicitud.numCreditoInfonavit   = '1234567890'
   LET solicitudMarcaRequest.solicitud.rfc                   = 'HEAJ640630LX0'
   LET solicitudMarcaRequest.solicitud.sitCredito            = '2'
   LET solicitudMarcaRequest.solicitud.tipoCredito           = '01'

   DISPLAY "Parametros a enviar:"
   DISPLAY ""
   DISPLAY "nss                 = ", solicitudMarcaRequest.solicitud.nss
   DISPLAY "apeMaterno          = ", solicitudMarcaRequest.solicitud.apeMaterno
   DISPLAY "apePaterno          = ", solicitudMarcaRequest.solicitud.apePaterno
   DISPLAY "nombres             = ", solicitudMarcaRequest.solicitud.nombres
   DISPLAY "rfc                 = ", solicitudMarcaRequest.solicitud.rfc
   DISPLAY "tipoCredito         = ", solicitudMarcaRequest.solicitud.tipoCredito
   DISPLAY "numCreditoInfonavit = ", solicitudMarcaRequest.solicitud.numCreditoInfonavit
   DISPLAY "fechaPresentacion   = ", solicitudMarcaRequest.solicitud.fechaPresentacion
   DISPLAY "sitCredito          = ", solicitudMarcaRequest.solicitud.sitCredito
   
   DISPLAY ""

   --LET WsTraspasosInfonavitService_WsTraspasosInfonavitLocation = 'http://192.168.1.81:8001/WsTraspasosInfo/services/WsTraspasosInfonavit'
   
   #Se ejecuta la funcion que invoca al WS
   #NOTA: Esta funcion se ejecuta por cada registro a notificar
   #la contraseña normal es Pruebaspass34
   #la contraseña en base 64 es Duo7Pv92mUrQDRH34dubTg==
   CALL solicitudMarca("PruebasInfo","Pruebaspass34") RETURNING wsstatus

   IF wsstatus <> 0 THEN
      DISPLAY wsError.code
      DISPLAY wsError.description
   ELSE
      DISPLAY "Resultado Operacion = ", solicitudMarcaResponse.solicitudMarcaReturn.resultOperacion --Es igual al que se envia
      DISPLAY "Respuesta del Servidor = ", solicitudMarcaResponse.solicitudMarcaReturn.diagProceso    --Respuesta del servidor
   END IF
END MAIN