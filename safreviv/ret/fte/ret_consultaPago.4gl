#-------------------------------------------------------------------------------
# File: ret_consultaPago.4gl
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "ret_consultaPago.inc"



#-------------------------------------------------------------------------------
# Service: ConsultaPago_ConsultaPagoHttpService
# Port:    ConsultaPago_ConsultaPagoHttpPort
# Server:  http://10.90.0.118:80/ConsultaPagoAcreedorMediationSapV6Web/sca/ConsultaPago
#-------------------------------------------------------------------------------

PRIVATE DEFINE consultarHTTPReq     com.HTTPRequest
PRIVATE DEFINE consultarHTTPResp    com.HTTPResponse

#-------------------------------------------------------------------------------

#
# Operation: consultar
#
#
# FUNCTION: consultar_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL consultar
#   OUTPUT: GLOBAL consultarResponse
#
FUNCTION consultar_g()
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

  IF ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation IS NULL THEN
    LET ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation = "http://10.90.0.118:80/ConsultaPagoAcreedorMediationSapV6Web/sca/ConsultaPago"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation)
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
      CALL xml.Serializer.VariableToStax(consultar,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,consultarResponse)
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


FUNCTION consultarRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF consultarHTTPReq IS NOT NULL AND consultarHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation IS NULL THEN
      LET ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation = "http://10.90.0.118:80/ConsultaPagoAcreedorMediationSapV6Web/sca/ConsultaPago"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET consultarHTTPReq = com.HTTPRequest.Create(ConsultaPago_ConsultaPagoHttpService_ConsultaPago_ConsultaPagoHttpPortLocation)
    CALL consultarHTTPReq.setMethod("POST")
    CALL consultarHTTPReq.setCharset("UTF-8")
    CALL consultarHTTPReq.setHeader("SOAPAction","\"\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET consultarHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = consultarHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(consultar,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL consultarHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET consultarHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET consultarHTTPResp = consultarHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET consultarHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION consultarResponse_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF consultarHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF consultarHTTPResp IS NULL THEN
      # Still no response, try again
      LET consultarHTTPResp = consultarHTTPReq.getAsyncResponse()
    END IF

    IF consultarHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE consultarHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = consultarHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL consultarHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = consultarHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,consultarResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL consultarHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||consultarHTTPResp.getStatusCode()||" ("||consultarHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET consultarHTTPReq = NULL
  LET consultarHTTPResp = NULL
  RETURN wsstatus
END FUNCTION


