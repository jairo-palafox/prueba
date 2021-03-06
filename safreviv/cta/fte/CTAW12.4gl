#-------------------------------------------------------------------------------
# File: CTAW12.4gl
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "CTAW12.inc"



#-------------------------------------------------------------------------------
# Service: SolicitudCredito
# Port:    SolicitudCreditoSOAP
# Server:  /TraspasosInfonavit/SolicitudCredito/
#-------------------------------------------------------------------------------

PRIVATE DEFINE solicitudMarcaHTTPReq     com.HTTPRequest
PRIVATE DEFINE solicitudMarcaHTTPResp    com.HTTPResponse

PRIVATE DEFINE solicitudDesMarcaHTTPReq     com.HTTPRequest
PRIVATE DEFINE solicitudDesMarcaHTTPResp    com.HTTPResponse

--Parametros de conexion
PRIVATE DEFINE v_usuario         STRING 
PRIVATE DEFINE v_password        STRING

#-------------------------------------------------------------------------------

#
# Operation: solicitudMarca
#

#
# FUNCTION: solicitudMarca
#
FUNCTION solicitudMarca(p_url_servidor, p_usuario, p_password,p_apeMaterno, p_apePaterno, p_fechaPresentacion, p_nombres, p_nss, p_numCreditoInfonavit, p_rfc, p_sitCredito, p_tipoCredito)
   DEFINE p_url_servidor        STRING
   DEFINE p_usuario             STRING 
   DEFINE p_password            STRING
   DEFINE soapStatus            INTEGER
   DEFINE p_apeMaterno          STRING
   DEFINE p_apePaterno          STRING
   DEFINE p_fechaPresentacion   STRING
   DEFINE p_nombres             STRING
   DEFINE p_nss                 STRING
   DEFINE p_numCreditoInfonavit STRING
   DEFINE p_rfc                 STRING
   DEFINE p_sitCredito          STRING
   DEFINE p_tipoCredito         STRING

   LET SolicMarcaVO.apeMaterno          = p_apeMaterno
   LET SolicMarcaVO.apePaterno          = p_apePaterno
   LET SolicMarcaVO.fechaPresentacion   = p_fechaPresentacion
   LET SolicMarcaVO.nombres             = p_nombres
   LET SolicMarcaVO.nss                 = p_nss
   LET SolicMarcaVO.numCreditoInfonavit = p_numCreditoInfonavit
   LET SolicMarcaVO.rfc                 = p_rfc
   LET SolicMarcaVO.sitCredito          = p_sitCredito
   LET SolicMarcaVO.tipoCredito         = p_tipoCredito

   LET SolicitudCredito_SolicitudCreditoSOAPLocation = p_url_servidor


   IF p_usuario IS NOT NULL THEN
      LET v_usuario = p_usuario
   ELSE
      LET v_usuario = ""
   END IF

   IF p_password IS NOT NULL THEN
      LET v_password = p_password
   ELSE
      LET v_password = ""
   END IF

   LET soapStatus = solicitudMarca_g()

   RETURN soapStatus, SolicMarcaRespVO.apeMaternoBD, SolicMarcaRespVO.apeMaternoInfo, SolicMarcaRespVO.apePaternoBD, SolicMarcaRespVO.apePaternoInfo, SolicMarcaRespVO.diagProceso, SolicMarcaRespVO.nombresBD, SolicMarcaRespVO.nombresInfo, SolicMarcaRespVO.nss, SolicMarcaRespVO.resultOperacion, SolicMarcaRespVO.rfc, SolicMarcaRespVO.rfcBD, SolicMarcaRespVO.tipTrabajador
END FUNCTION

#
# FUNCTION: solicitudMarca_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL SolicMarcaVO
#   OUTPUT: GLOBAL SolicMarcaRespVO
#
FUNCTION solicitudMarca_g()
   DEFINE wsstatus   INTEGER
   DEFINE retryAuth  INTEGER
   DEFINE retryProxy INTEGER
   DEFINE retry      INTEGER
   DEFINE request    com.HTTPRequest
   DEFINE response   com.HTTPResponse
   DEFINE writer     xml.StaxWriter
   DEFINE reader     xml.StaxReader

   #
   # INIT VARIABLES
   #
   LET wsstatus   = -1
   LET retryAuth  = FALSE
   LET retryProxy = FALSE
   LET retry      = TRUE

   {IF SolicitudCredito_SolicitudCreditoSOAPLocation IS NULL THEN
     --LET SolicitudCredito_SolicitudCreditoSOAPLocation = "/TraspasosInfonavit/SolicitudCredito/"
     LET SolicitudCredito_SolicitudCreditoSOAPLocation = "http://192.168.1.81/TraspasosInfonavit/SolicitudCredito"
   END IF}

   #
   # CREATE REQUEST
   #
   TRY
     LET request = com.HTTPRequest.Create(SolicitudCredito_SolicitudCreditoSOAPLocation)
     #CALL request.clearAuthentication()
     #CALL request.setAuthentication(v_usuario, v_password,"Basic","WebLogic Server")
     CALL request.setAutoReply(TRUE)
     CALL request.setMethod("POST")
     CALL request.setCharset("UTF-8")
     CALL request.setHeader("SOAPAction","\"http://www.procesar.com.mx/TraspasosInfonavit/SolicitudCredito/solicitudCredito/\"")
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
     RETURN wsstatus
   END TRY

   # START LOOP
   WHILE retry
     LET retry = FALSE

     #
     # Stax request
     #
     TRY
       LET writer = request.beginXmlRequest()
       CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
       CALL WSHelper_WriteStaxSOAP11StartBody(writer)
       CALL xml.Serializer.VariableToStax(SolicMarcaVO,writer)
       CALL WSHelper_WriteStaxSOAP11EndBody(writer)
       CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
       CALL request.endXmlRequest(writer)
     CATCH
       LET wsstatus = STATUS
       CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
       RETURN wsstatus
     END TRY

     #
     # PROCESS RESPONSE
     #
     TRY
       LET response = request.getResponse()

       CASE response.getStatusCode()

         WHEN 500 # SOAP Fault
           #
           # STAX SOAP FAULT
           #
           LET reader = response.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             CALL reader.nextSibling() # Skip SOAP headers
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11Fault(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL response.endXmlResponse(reader)

         WHEN 200 # SOAP Result
           #
           # STAX SOAP RESPONSE
           #
           LET reader = response.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             IF NOT reader.isEmptyElement() THEN
               CALL WSHelper_FillSOAP11WSError("Client","No SOAP Header expected")
               EXIT CASE
             ELSE
               CALL reader.nextTag()
             END IF
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           # Retrieve SOAP Message taking soap:root attribute into account
           IF NOT WSHelper_RetrieveStaxSOAP11Message(reader) THEN
             EXIT CASE
           END IF
           CALL xml.Serializer.StaxToVariable(reader,SolicMarcaRespVO)
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL response.endXmlResponse(reader)
           LET wsstatus = 0

         WHEN 401 # HTTP Authentication
           IF retryAuth THEN
             CALL WSHelper_FillSOAP11WSError("Server","HTTP Error 401 ("||response.getStatusDescription()||")")
           ELSE
             LET retryAuth = TRUE
             LET retry = TRUE
           END IF
         
         WHEN 407 # Proxy Authentication
           IF retryProxy THEN
             CALL WSHelper_FillSOAP11WSError("Server","HTTP Error 407 ("||response.getStatusDescription()||")")
           ELSE
             LET retryProxy = TRUE
             LET retry = TRUE
           END IF

         OTHERWISE
           CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||response.getStatusCode()||" ("||response.getStatusDescription()||")")

       END CASE
     CATCH
       LET wsstatus = status
       CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
       RETURN wsstatus
     END TRY

   # END LOOP
   END WHILE

   RETURN wsstatus

END FUNCTION


FUNCTION solicitudMarcaRequest_g()
   DEFINE wsstatus   INTEGER
   DEFINE writer     xml.StaxWriter

   #
   # CHECK PREVIOUS CALL  
   #
   IF solicitudMarcaHTTPReq IS NOT NULL AND solicitudMarcaHTTPResp IS NULL THEN
     # Request was sent but there was no response yet
     CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
     RETURN -2 # waiting for the response
   ELSE
     {IF SolicitudCredito_SolicitudCreditoSOAPLocation IS NULL THEN
       LET SolicitudCredito_SolicitudCreditoSOAPLocation = "http://192.168.1.81/TraspasosInfonavit/SolicitudCredito"
     END IF}
   END IF

   #
   # CREATE REQUEST
   #
   TRY
     LET solicitudMarcaHTTPReq = com.HTTPRequest.Create(SolicitudCredito_SolicitudCreditoSOAPLocation)
     #CALL solicitudMarcaHTTPReq.clearAuthentication()
     #CALL solicitudMarcaHTTPReq.setAuthentication(v_usuario, v_password,"Basic","WebLogic Server")
     CALL solicitudMarcaHTTPReq.setAutoReply(TRUE)
     CALL solicitudMarcaHTTPReq.setMethod("POST")
     CALL solicitudMarcaHTTPReq.setCharset("UTF-8")
     CALL solicitudMarcaHTTPReq.setHeader("SOAPAction","\"http://www.procesar.com.mx/TraspasosInfonavit/SolicitudCredito/solicitudCredito/\"")
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
     LET solicitudMarcaHTTPReq = NULL
     RETURN wsstatus
   END TRY

     #
     # Stax request
     #
     TRY
       LET writer = solicitudMarcaHTTPReq.beginXmlRequest()
       CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
       CALL WSHelper_WriteStaxSOAP11StartBody(writer)
       CALL xml.Serializer.VariableToStax(SolicMarcaVO,writer)
       CALL WSHelper_WriteStaxSOAP11EndBody(writer)
       CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
       CALL solicitudMarcaHTTPReq.endXmlRequest(writer)
     CATCH
       LET wsstatus = STATUS
       CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
       LET solicitudMarcaHTTPReq = NULL
       RETURN wsstatus
     END TRY

   #
   # PROCESS RESPONSE
   #
   TRY
     LET solicitudMarcaHTTPResp = solicitudMarcaHTTPReq.getAsyncResponse()
     RETURN 0 # SUCCESS
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
     LET solicitudMarcaHTTPReq = NULL
     RETURN wsstatus
   END TRY
END FUNCTION


FUNCTION solicitudMarcaResponse_g()
   DEFINE wsstatus   INTEGER
   DEFINE reader     xml.StaxReader

   LET wsstatus = -1

   #
   # CHECK PREVIOUS CALL  
   #
   IF solicitudMarcaHTTPReq IS NULL THEN
     # No request was sent
     CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
     RETURN -1
   END IF

   TRY
     #
     # PROCESS RESPONSE
     #
     IF solicitudMarcaHTTPResp IS NULL THEN
       # Still no response, try again
       LET solicitudMarcaHTTPResp = solicitudMarcaHTTPReq.getAsyncResponse()
     END IF

     IF solicitudMarcaHTTPResp IS NULL THEN
       # We got no response, still waiting for
       CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
       RETURN -2
     END IF

       CASE solicitudMarcaHTTPResp.getStatusCode()

         WHEN 500 # SOAP Fault
           #
           # STAX SOAP FAULT
           #
           LET reader = solicitudMarcaHTTPResp.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             CALL reader.nextSibling() # Skip SOAP headers
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11Fault(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL solicitudMarcaHTTPResp.endXmlResponse(reader)

         WHEN 200 # SOAP Result
           #
           # STAX SOAP RESPONSE
           #
           LET reader = solicitudMarcaHTTPResp.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             IF NOT reader.isEmptyElement() THEN
               CALL WSHelper_FillSOAP11WSError("Client","No SOAP Header expected")
               EXIT CASE
             ELSE
               CALL reader.nextTag()
             END IF
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           # Retrieve SOAP Message taking soap:root attribute into account
           IF NOT WSHelper_RetrieveStaxSOAP11Message(reader) THEN
             EXIT CASE
           END IF
           CALL xml.Serializer.StaxToVariable(reader,SolicMarcaRespVO)
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL solicitudMarcaHTTPResp.endXmlResponse(reader)
           LET wsstatus = 0

         OTHERWISE
           CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||solicitudMarcaHTTPResp.getStatusCode()||" ("||solicitudMarcaHTTPResp.getStatusDescription()||")")

       END CASE
     CATCH
       LET wsstatus = status
       CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
     END TRY

   #
   # RESET VARIABLES
   #
   LET solicitudMarcaHTTPReq = NULL
   LET solicitudMarcaHTTPResp = NULL
   RETURN wsstatus
END FUNCTION



#
# Operation: solicitudDesMarca
#

#
# FUNCTION: solicitudDesMarca
#
FUNCTION solicitudDesMarca(p_url_servidor,p_usuario,p_password,p_apeMaterno, p_apePaterno, p_fechaPresentacion, p_nombres, p_nss, p_numCreditoInfonavit, p_rfc)
   DEFINE p_url_servidor        STRING
   DEFINE p_usuario             STRING 
   DEFINE p_password            STRING
   DEFINE soapStatus            INTEGER
   DEFINE p_apeMaterno          STRING
   DEFINE p_apePaterno          STRING
   DEFINE p_fechaPresentacion   STRING
   DEFINE p_nombres             STRING
   DEFINE p_nss                 STRING
   DEFINE p_numCreditoInfonavit STRING
   DEFINE p_rfc                 STRING

   LET SolicDesmarcaVO.apeMaterno          = p_apeMaterno
   LET SolicDesmarcaVO.apePaterno          = p_apePaterno
   LET SolicDesmarcaVO.fechaPresentacion   = p_fechaPresentacion
   LET SolicDesmarcaVO.nombres             = p_nombres
   LET SolicDesmarcaVO.nss                 = p_nss
   LET SolicDesmarcaVO.numCreditoInfonavit = p_numCreditoInfonavit
   LET SolicDesmarcaVO.rfc                 = p_rfc

   LET SolicitudCredito_SolicitudCreditoSOAPLocation = p_url_servidor

   IF p_usuario IS NOT NULL THEN
      LET v_usuario = p_usuario
   ELSE
      LET v_usuario = ""
   END IF

   IF p_password IS NOT NULL THEN
      LET v_password = p_password
   ELSE
      LET v_password = ""
   END IF

   LET soapStatus = solicitudDesMarca_g()

   RETURN soapStatus, SolicDesmarcaRespVO.apeMaternoBD, SolicDesmarcaRespVO.apeMaternoInfo, SolicDesmarcaRespVO.apePaternoBD, SolicDesmarcaRespVO.apePaternoInfo, SolicDesmarcaRespVO.diagProceso, SolicDesmarcaRespVO.nombresBD, SolicDesmarcaRespVO.nombresInfo, SolicDesmarcaRespVO.nss, SolicDesmarcaRespVO.resultOperacion, SolicDesmarcaRespVO.rfc, SolicDesmarcaRespVO.rfcBD, SolicDesmarcaRespVO.tipCreditDesm
END FUNCTION

#
# FUNCTION: solicitudDesMarca_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL SolicDesmarcaVO
#   OUTPUT: GLOBAL SolicDesmarcaRespVO
#
FUNCTION solicitudDesMarca_g()
   DEFINE wsstatus   INTEGER
   DEFINE retryAuth  INTEGER
   DEFINE retryProxy INTEGER
   DEFINE retry      INTEGER
   DEFINE request    com.HTTPRequest
   DEFINE response   com.HTTPResponse
   DEFINE writer     xml.StaxWriter
   DEFINE reader     xml.StaxReader

   #
   # INIT VARIABLES
   #
   LET wsstatus = -1
   LET retryAuth = FALSE
   LET retryProxy = FALSE
   LET retry = TRUE

   {IF SolicitudCredito_SolicitudCreditoSOAPLocation IS NULL THEN
     LET SolicitudCredito_SolicitudCreditoSOAPLocation = "http://192.168.1.81/TraspasosInfonavit/SolicitudCredito"
   END IF}

   #
   # CREATE REQUEST
   #
   TRY
     LET request = com.HTTPRequest.Create(SolicitudCredito_SolicitudCreditoSOAPLocation)
     #CALL request.setAuthentication(v_usuario, v_password,"Basic","WebLogic Server")
     CALL request.setAutoReply(TRUE)
     CALL request.setMethod("POST")
     CALL request.setCharset("UTF-8")
     CALL request.setHeader("SOAPAction","\"http://www.procesar.com.mx/TraspasosInfonavit/SolicitudCredito/solicitudCredito/\"")
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
     RETURN wsstatus
   END TRY

   # START LOOP
   WHILE retry
     LET retry = FALSE

     #
     # Stax request
     #
     TRY
       LET writer = request.beginXmlRequest()
       CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
       CALL WSHelper_WriteStaxSOAP11StartBody(writer)
       CALL xml.Serializer.VariableToStax(SolicDesmarcaVO,writer)
       CALL WSHelper_WriteStaxSOAP11EndBody(writer)
       CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
       CALL request.endXmlRequest(writer)
     CATCH
       LET wsstatus = STATUS
       CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
       RETURN wsstatus
     END TRY

     #
     # PROCESS RESPONSE
     #
     TRY
       LET response = request.getResponse()

       CASE response.getStatusCode()

         WHEN 500 # SOAP Fault
           #
           # STAX SOAP FAULT
           #
           LET reader = response.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             CALL reader.nextSibling() # Skip SOAP headers
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11Fault(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL response.endXmlResponse(reader)

         WHEN 200 # SOAP Result
           #
           # STAX SOAP RESPONSE
           #
           LET reader = response.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             IF NOT reader.isEmptyElement() THEN
               CALL WSHelper_FillSOAP11WSError("Client","No SOAP Header expected")
               EXIT CASE
             ELSE
               CALL reader.nextTag()
             END IF
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           # Retrieve SOAP Message taking soap:root attribute into account
           IF NOT WSHelper_RetrieveStaxSOAP11Message(reader) THEN
             EXIT CASE
           END IF
           CALL xml.Serializer.StaxToVariable(reader,SolicDesmarcaRespVO)
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL response.endXmlResponse(reader)
           LET wsstatus = 0

         WHEN 401 # HTTP Authentication
           IF retryAuth THEN
             CALL WSHelper_FillSOAP11WSError("Server","HTTP Error 401 ("||response.getStatusDescription()||")")
           ELSE
             LET retryAuth = TRUE
             LET retry = TRUE
           END IF

         WHEN 407 # Proxy Authentication
           IF retryProxy THEN
             CALL WSHelper_FillSOAP11WSError("Server","HTTP Error 407 ("||response.getStatusDescription()||")")
           ELSE
             LET retryProxy = TRUE
             LET retry = TRUE
           END IF

         OTHERWISE
           CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||response.getStatusCode()||" ("||response.getStatusDescription()||")")

       END CASE
     CATCH
       LET wsstatus = status
       CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
       RETURN wsstatus
     END TRY

   # END LOOP
   END WHILE

   RETURN wsstatus

END FUNCTION


FUNCTION solicitudDesMarcaRequest_g()
   DEFINE wsstatus   INTEGER
   DEFINE writer     xml.StaxWriter

   #
   # CHECK PREVIOUS CALL  
   #
   IF solicitudDesMarcaHTTPReq IS NOT NULL AND solicitudDesMarcaHTTPResp IS NULL THEN
     # Request was sent but there was no response yet
     CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
     RETURN -2 # waiting for the response
   ELSE
     {IF SolicitudCredito_SolicitudCreditoSOAPLocation IS NULL THEN
       LET SolicitudCredito_SolicitudCreditoSOAPLocation = "http://192.168.1.81/TraspasosInfonavit/SolicitudCredito"
     END IF}
   END IF

   #
   # CREATE REQUEST
   #
   TRY
     LET solicitudDesMarcaHTTPReq = com.HTTPRequest.Create(SolicitudCredito_SolicitudCreditoSOAPLocation)
     #CALL solicitudDesMarcaHTTPReq.setAuthentication(v_usuario, v_password,"Basic","WebLogic Server")
     CALL solicitudDesMarcaHTTPReq.setAutoReply(TRUE)
     CALL solicitudDesMarcaHTTPReq.setMethod("POST")
     CALL solicitudDesMarcaHTTPReq.setCharset("UTF-8")
     CALL solicitudDesMarcaHTTPReq.setHeader("SOAPAction","\"http://www.procesar.com.mx/TraspasosInfonavit/SolicitudCredito/solicitudCredito/\"")
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
     LET solicitudDesMarcaHTTPReq = NULL
     RETURN wsstatus
   END TRY

     #
     # Stax request
     #
     TRY
       LET writer = solicitudDesMarcaHTTPReq.beginXmlRequest()
       CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
       CALL WSHelper_WriteStaxSOAP11StartBody(writer)
       CALL xml.Serializer.VariableToStax(SolicDesmarcaVO,writer)
       CALL WSHelper_WriteStaxSOAP11EndBody(writer)
       CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
       CALL solicitudDesMarcaHTTPReq.endXmlRequest(writer)
     CATCH
       LET wsstatus = STATUS
       CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
       LET solicitudDesMarcaHTTPReq = NULL
       RETURN wsstatus
     END TRY

   #
   # PROCESS RESPONSE
   #
   TRY
     LET solicitudDesMarcaHTTPResp = solicitudDesMarcaHTTPReq.getAsyncResponse()
     RETURN 0 # SUCCESS
   CATCH
     LET wsstatus = STATUS
     CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
     LET solicitudDesMarcaHTTPReq = NULL
     RETURN wsstatus
   END TRY
END FUNCTION


FUNCTION solicitudDesMarcaResponse_g()
   DEFINE wsstatus   INTEGER
   DEFINE reader     xml.StaxReader

   LET wsstatus = -1

   #
   # CHECK PREVIOUS CALL  
   #
   IF solicitudDesMarcaHTTPReq IS NULL THEN
     # No request was sent
     CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
     RETURN -1
   END IF

   TRY
     #
     # PROCESS RESPONSE
     #
     IF solicitudDesMarcaHTTPResp IS NULL THEN
       # Still no response, try again
       LET solicitudDesMarcaHTTPResp = solicitudDesMarcaHTTPReq.getAsyncResponse()
     END IF

     IF solicitudDesMarcaHTTPResp IS NULL THEN
       # We got no response, still waiting for
       CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
       RETURN -2
     END IF

       CASE solicitudDesMarcaHTTPResp.getStatusCode()

         WHEN 500 # SOAP Fault
           #
           # STAX SOAP FAULT
           #
           LET reader = solicitudDesMarcaHTTPResp.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             CALL reader.nextSibling() # Skip SOAP headers
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11Fault(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL solicitudDesMarcaHTTPResp.endXmlResponse(reader)

         WHEN 200 # SOAP Result
           #
           # STAX SOAP RESPONSE
           #
           LET reader = solicitudDesMarcaHTTPResp.beginXmlResponse() # Begin Streaming Response
           IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
             EXIT CASE
           END IF
           IF WSHelper_CheckStaxSOAP11Header(reader) THEN
             IF NOT reader.isEmptyElement() THEN
               CALL WSHelper_FillSOAP11WSError("Client","No SOAP Header expected")
               EXIT CASE
             ELSE
               CALL reader.nextTag()
             END IF
           END IF
           IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
             EXIT CASE
           END IF
           # Retrieve SOAP Message taking soap:root attribute into account
           IF NOT WSHelper_RetrieveStaxSOAP11Message(reader) THEN
             EXIT CASE
           END IF
           CALL xml.Serializer.StaxToVariable(reader,SolicDesmarcaRespVO)
           IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
             EXIT CASE
           END IF
           IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
             EXIT CASE
           END IF
           # End Streaming Response
           CALL solicitudDesMarcaHTTPResp.endXmlResponse(reader)
           LET wsstatus = 0

         OTHERWISE
           CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||solicitudDesMarcaHTTPResp.getStatusCode()||" ("||solicitudDesMarcaHTTPResp.getStatusDescription()||")")

       END CASE
     CATCH
       LET wsstatus = status
       CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
     END TRY

   #
   # RESET VARIABLES
   #
   LET solicitudDesMarcaHTTPReq = NULL
   LET solicitudDesMarcaHTTPResp = NULL
   RETURN wsstatus
END FUNCTION





