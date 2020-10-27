####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW07.4gl                                    #
#Objetivo          =>Programa que contiene el cliente que se       #
#                    conecta al WS de separacion de cuentas        #
#Fecha inicio      =>17 Mayo 2012                                  #
####################################################################


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "SEPW07.inc"



#-------------------------------------------------------------------------------
# Service: RecibeDatosExpedienteService
# Port:    RecibeDatosExpedienteServicePortType
# Server:  v_url_servidor
#-------------------------------------------------------------------------------

PRIVATE DEFINE recibeDatosExpedienteHTTPReq     com.HTTPRequest
PRIVATE DEFINE recibeDatosExpedienteHTTPResp    com.HTTPResponse

--Parametros de conexion
PRIVATE DEFINE v_url_servidor    STRING 
PRIVATE DEFINE v_usuario         STRING 
PRIVATE DEFINE v_password        STRING

#-------------------------------------------------------------------------------

#  Metodo para senviar los datos del expediente que recibe como parametro los atrubutos de conexion
#
#  FUNCTION: recibeDatosExpediente
#  RETURNING: soapStatus
FUNCTION recibeDatosExpediente(p_url_servidor, p_usuario, p_password)
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

   CALL recibeDatosExpediente_g() RETURNING wsstatus

   RETURN wsstatus
END FUNCTION

#
# Operation: recibeDatosExpediente
#
#
# FUNCTION: recibeDatosExpediente_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1recibeDatosExpediente
#   OUTPUT: GLOBAL ns1recibeDatosExpedienteResponse
#
FUNCTION recibeDatosExpediente_g()
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

  IF RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation IS NULL THEN
    LET RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation = v_url_servidor
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation)
    CALL request.setAuthentication(v_usuario, v_password,"","")
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"recibeDatosExpediente\"")
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
      CALL xml.Serializer.VariableToStax(ns1recibeDatosExpediente,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,ns1recibeDatosExpedienteResponse)
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


FUNCTION recibeDatosExpedienteRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF recibeDatosExpedienteHTTPReq IS NOT NULL AND recibeDatosExpedienteHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation IS NULL THEN
      LET RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation = v_url_servidor
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET recibeDatosExpedienteHTTPReq = com.HTTPRequest.Create(RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation)
    CALL recibeDatosExpedienteHTTPReq.setAuthentication(v_usuario, v_password,"","")
    CALL recibeDatosExpedienteHTTPReq.setMethod("POST")
    CALL recibeDatosExpedienteHTTPReq.setCharset("UTF-8")
    CALL recibeDatosExpedienteHTTPReq.setHeader("SOAPAction","\"recibeDatosExpediente\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET recibeDatosExpedienteHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = recibeDatosExpedienteHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(ns1recibeDatosExpediente,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL recibeDatosExpedienteHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET recibeDatosExpedienteHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET recibeDatosExpedienteHTTPResp = recibeDatosExpedienteHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET recibeDatosExpedienteHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION recibeDatosExpedienteResponse_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF recibeDatosExpedienteHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF recibeDatosExpedienteHTTPResp IS NULL THEN
      # Still no response, try again
      LET recibeDatosExpedienteHTTPResp = recibeDatosExpedienteHTTPReq.getAsyncResponse()
    END IF

    IF recibeDatosExpedienteHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE recibeDatosExpedienteHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = recibeDatosExpedienteHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL recibeDatosExpedienteHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = recibeDatosExpedienteHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,ns1recibeDatosExpedienteResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL recibeDatosExpedienteHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||recibeDatosExpedienteHTTPResp.getStatusCode()||" ("||recibeDatosExpedienteHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET recibeDatosExpedienteHTTPReq = NULL
  LET recibeDatosExpedienteHTTPResp = NULL
  RETURN wsstatus
END FUNCTION


