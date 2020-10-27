####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW03.4gl                                    #
#Objetivo          =>Programa que contiene el cliente que se       #
#                    conecta al WS de separacion de cuentas        #
#Fecha inicio      =>17 Mayo 2012                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "SEPW03.inc"


#-------------------------------------------------------------------------------
# Service: AsignaNumeroCasoService
# Port:    AsignaNumeroCasoServicePortType
# Server:  v_url_servidor
#-------------------------------------------------------------------------------

PRIVATE DEFINE fn_asigna_numero_casoHTTPReq     com.HTTPRequest
PRIVATE DEFINE fn_asigna_numero_casoHTTPResp    com.HTTPResponse

--Parametros de conexion
PRIVATE DEFINE v_url_servidor    STRING 
PRIVATE DEFINE v_usuario         STRING 
PRIVATE DEFINE v_password        STRING

#-------------------------------------------------------------------------------

#  Metodo para solicitar un numero de caso que recibe como parametro los atrubutos de conexion
#
#  FUNCTION: asignaNumeroCaso
#  RETURNING: soapStatus
FUNCTION asignaNumeroCaso(p_url_servidor, p_usuario, p_password)
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

   CALL fn_asigna_numero_caso_g() RETURNING wsstatus

   RETURN wsstatus
END FUNCTION


#
# Operation: fn_asigna_numero_caso
#
#
# FUNCTION: fn_asigna_numero_caso_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1asignaCaso
#   OUTPUT: GLOBAL ns1asignaCasoResponse
#
FUNCTION fn_asigna_numero_caso_g()
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

  IF AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation IS NULL THEN
    LET AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation = v_url_servidor
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation)
    CALL request.setAuthentication(v_usuario, v_password,"","")
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"fn_asigna_numero_caso\"")
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
      CALL xml.Serializer.VariableToStax(ns1asignaCaso,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,ns1asignaCasoResponse)
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


FUNCTION fn_asigna_numero_casoRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF fn_asigna_numero_casoHTTPReq IS NOT NULL AND fn_asigna_numero_casoHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation IS NULL THEN
      LET AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation = v_url_servidor
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET fn_asigna_numero_casoHTTPReq = com.HTTPRequest.Create(AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation)
    CALL fn_asigna_numero_casoHTTPReq.setAuthentication(v_usuario, v_password,"","")
    CALL fn_asigna_numero_casoHTTPReq.setMethod("POST")
    CALL fn_asigna_numero_casoHTTPReq.setCharset("UTF-8")
    CALL fn_asigna_numero_casoHTTPReq.setHeader("SOAPAction","\"fn_asigna_numero_caso\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET fn_asigna_numero_casoHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = fn_asigna_numero_casoHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(ns1asignaCaso,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL fn_asigna_numero_casoHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET fn_asigna_numero_casoHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET fn_asigna_numero_casoHTTPResp = fn_asigna_numero_casoHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET fn_asigna_numero_casoHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION fn_asigna_numero_casoResponse_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF fn_asigna_numero_casoHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF fn_asigna_numero_casoHTTPResp IS NULL THEN
      # Still no response, try again
      LET fn_asigna_numero_casoHTTPResp = fn_asigna_numero_casoHTTPReq.getAsyncResponse()
    END IF

    IF fn_asigna_numero_casoHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE fn_asigna_numero_casoHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = fn_asigna_numero_casoHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL fn_asigna_numero_casoHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = fn_asigna_numero_casoHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,ns1asignaCasoResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL fn_asigna_numero_casoHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||fn_asigna_numero_casoHTTPResp.getStatusCode()||" ("||fn_asigna_numero_casoHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET fn_asigna_numero_casoHTTPReq = NULL
  LET fn_asigna_numero_casoHTTPResp = NULL
  RETURN wsstatus
END FUNCTION


