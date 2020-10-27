#-------------------------------------------------------------------------------
# File: ws_retiroLey73.4gl
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "ws_retiroLey73.inc"



#-------------------------------------------------------------------------------
# Service: RetiroLey73
# Port:    RetiroLey73PortType
# Server:  http://172.16.16.204:6699/RetiroLey73
#-------------------------------------------------------------------------------

PRIVATE DEFINE fn_genera_solicitud_retiro_ley73HTTPReq     com.HTTPRequest
PRIVATE DEFINE fn_genera_solicitud_retiro_ley73HTTPResp    com.HTTPResponse

#-------------------------------------------------------------------------------

#
# Operation: fn_genera_solicitud_retiro_ley73
#

#
# FUNCTION: fn_genera_solicitud_retiro_ley73
#
FUNCTION fn_genera_solicitud_retiro_ley73(p_g_nss, p_g_grupo_tr)
  DEFINE	soapStatus		INTEGER
  DEFINE	p_g_nss		tfn_genera_solicitud_retiro_ley73_g_nss
  DEFINE	p_g_grupo_tr		tfn_genera_solicitud_retiro_ley73_g_grupo_tr

  LET fn_genera_solicitud_retiro_ley73.g_nss = p_g_nss
  LET fn_genera_solicitud_retiro_ley73.g_grupo_tr = p_g_grupo_tr

  LET soapStatus = fn_genera_solicitud_retiro_ley73_g()

  RETURN soapStatus, fn_genera_solicitud_retiro_ley73Response.r_g_nss, fn_genera_solicitud_retiro_ley73Response.r_nombre_af, fn_genera_solicitud_retiro_ley73Response.r_paterno_af, fn_genera_solicitud_retiro_ley73Response.r_materno_af, fn_genera_solicitud_retiro_ley73Response.r_importe_viv97, fn_genera_solicitud_retiro_ley73Response.r_importe_viv92, fn_genera_solicitud_retiro_ley73Response.r_cod_retorno, fn_genera_solicitud_retiro_ley73Response.r_mensaje_retorno, fn_genera_solicitud_retiro_ley73Response.r_rfc_af, fn_genera_solicitud_retiro_ley73Response.r_curp_af, fn_genera_solicitud_retiro_ley73Response.r_marca_juridico
END FUNCTION

#
# FUNCTION: fn_genera_solicitud_retiro_ley73_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL fn_genera_solicitud_retiro_ley73
#   OUTPUT: GLOBAL fn_genera_solicitud_retiro_ley73Response
#
FUNCTION fn_genera_solicitud_retiro_ley73_g()
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

  IF RetiroLey73_RetiroLey73PortTypeLocation IS NULL THEN
    LET RetiroLey73_RetiroLey73PortTypeLocation = "http://172.16.16.204:6699/RetiroLey73"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(RetiroLey73_RetiroLey73PortTypeLocation)
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
      CALL xml.Serializer.VariableToStax(fn_genera_solicitud_retiro_ley73,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,fn_genera_solicitud_retiro_ley73Response)
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


FUNCTION fn_genera_solicitud_retiro_ley73Request_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF fn_genera_solicitud_retiro_ley73HTTPReq IS NOT NULL AND fn_genera_solicitud_retiro_ley73HTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF RetiroLey73_RetiroLey73PortTypeLocation IS NULL THEN
      LET RetiroLey73_RetiroLey73PortTypeLocation = "http://172.16.16.204:6699/RetiroLey73"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET fn_genera_solicitud_retiro_ley73HTTPReq = com.HTTPRequest.Create(RetiroLey73_RetiroLey73PortTypeLocation)
    CALL fn_genera_solicitud_retiro_ley73HTTPReq.setMethod("POST")
    CALL fn_genera_solicitud_retiro_ley73HTTPReq.setCharset("UTF-8")
    CALL fn_genera_solicitud_retiro_ley73HTTPReq.setHeader("SOAPAction","\"\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET fn_genera_solicitud_retiro_ley73HTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = fn_genera_solicitud_retiro_ley73HTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(fn_genera_solicitud_retiro_ley73,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL fn_genera_solicitud_retiro_ley73HTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET fn_genera_solicitud_retiro_ley73HTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET fn_genera_solicitud_retiro_ley73HTTPResp = fn_genera_solicitud_retiro_ley73HTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET fn_genera_solicitud_retiro_ley73HTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION fn_genera_solicitud_retiro_ley73Response_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF fn_genera_solicitud_retiro_ley73HTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF fn_genera_solicitud_retiro_ley73HTTPResp IS NULL THEN
      # Still no response, try again
      LET fn_genera_solicitud_retiro_ley73HTTPResp = fn_genera_solicitud_retiro_ley73HTTPReq.getAsyncResponse()
    END IF

    IF fn_genera_solicitud_retiro_ley73HTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE fn_genera_solicitud_retiro_ley73HTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = fn_genera_solicitud_retiro_ley73HTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL fn_genera_solicitud_retiro_ley73HTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = fn_genera_solicitud_retiro_ley73HTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,fn_genera_solicitud_retiro_ley73Response)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL fn_genera_solicitud_retiro_ley73HTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||fn_genera_solicitud_retiro_ley73HTTPResp.getStatusCode()||" ("||fn_genera_solicitud_retiro_ley73HTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET fn_genera_solicitud_retiro_ley73HTTPReq = NULL
  LET fn_genera_solicitud_retiro_ley73HTTPResp = NULL
  RETURN wsstatus
END FUNCTION


