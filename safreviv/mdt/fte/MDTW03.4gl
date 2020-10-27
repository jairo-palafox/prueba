####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTW03.4gl                                    #
#Objetivo          =>Programa que contiene el cliente que se       #
#                    conecta al WS de mandatos                     #
#Fecha inicio      =>09 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "MDTW03.inc"



#-------------------------------------------------------------------------------
# Service: NotificaInstruccionMandatosService
# Port:    NotificaInstruccionMandatosService
# Server:  v_url_servidor
#-------------------------------------------------------------------------------

PRIVATE DEFINE notifica_instruccion_mdt_canHTTPReq     com.HTTPRequest
PRIVATE DEFINE notifica_instruccion_mdt_canHTTPResp    com.HTTPResponse

--Parametros de conexion
PRIVATE DEFINE v_url_servidor    STRING 
PRIVATE DEFINE v_usuario         STRING 
PRIVATE DEFINE v_password        STRING

#-------------------------------------------------------------------------------

#  Metodo para notificar un mandato que recibe como parametro los atrubutos de conexion
#
#  FUNCTION: notificaInstruccion
#  RETURNING: soapStatus
FUNCTION notificaInstruccion(p_url_servidor, p_usuario, p_password)
   DEFINE p_url_servidor STRING 
   DEFINE p_usuario STRING 
   DEFINE p_password STRING 
   DEFINE wsstatus   INTEGER

   IF p_url_servidor IS NULL OR p_url_servidor.trim() == "" THEN
      --ERROR
      DISPLAY "La ruta del servidior no es valida o es nula"
      RETURN -1
   ELSE 
      LET v_url_servidor = p_url_servidor
   END IF 

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

   CALL notifica_instruccion_mdt_can_g() RETURNING wsstatus

   RETURN wsstatus
END FUNCTION

#-------------------------------------------------------------------------------

#
# Operation: notifica_instruccion_mdt_can
#
#
# FUNCTION: notifica_instruccion_mdt_can_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1notifica_instruccion_mdt_can
#   OUTPUT: GLOBAL ns1notifica_instruccion_mdt_canResponse
#
FUNCTION notifica_instruccion_mdt_can_g()
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

  IF NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation IS NULL THEN
    LET NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation = v_url_servidor
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation)
    CALL request.setAuthentication(v_usuario, v_password,"","")
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"\"")
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
      CALL xml.Serializer.VariableToStax(ns1notifica_instruccion_mdt_can,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,ns1notifica_instruccion_mdt_canResponse)
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


FUNCTION notifica_instruccion_mdt_canRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF notifica_instruccion_mdt_canHTTPReq IS NOT NULL AND notifica_instruccion_mdt_canHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation IS NULL THEN
      LET NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation = v_url_servidor
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET notifica_instruccion_mdt_canHTTPReq = com.HTTPRequest.Create(NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation)
    CALL notifica_instruccion_mdt_canHTTPReq.setAuthentication(v_usuario, v_password,"","")
    CALL notifica_instruccion_mdt_canHTTPReq.setMethod("POST")
    CALL notifica_instruccion_mdt_canHTTPReq.setCharset("UTF-8")
    CALL notifica_instruccion_mdt_canHTTPReq.setHeader("SOAPAction","\"\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET notifica_instruccion_mdt_canHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = notifica_instruccion_mdt_canHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(ns1notifica_instruccion_mdt_can,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL notifica_instruccion_mdt_canHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET notifica_instruccion_mdt_canHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET notifica_instruccion_mdt_canHTTPResp = notifica_instruccion_mdt_canHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET notifica_instruccion_mdt_canHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


PRIVATE FUNCTION notifica_instruccion_mdt_canResponse_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF notifica_instruccion_mdt_canHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF notifica_instruccion_mdt_canHTTPResp IS NULL THEN
      # Still no response, try again
      LET notifica_instruccion_mdt_canHTTPResp = notifica_instruccion_mdt_canHTTPReq.getAsyncResponse()
    END IF

    IF notifica_instruccion_mdt_canHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE notifica_instruccion_mdt_canHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = notifica_instruccion_mdt_canHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL notifica_instruccion_mdt_canHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = notifica_instruccion_mdt_canHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,ns1notifica_instruccion_mdt_canResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL notifica_instruccion_mdt_canHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||notifica_instruccion_mdt_canHTTPResp.getStatusCode()||" ("||notifica_instruccion_mdt_canHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET notifica_instruccion_mdt_canHTTPReq = NULL
  LET notifica_instruccion_mdt_canHTTPResp = NULL
  RETURN wsstatus
END FUNCTION


