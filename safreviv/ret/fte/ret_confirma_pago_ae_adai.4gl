#-------------------------------------------------------------------------------
# File: ret_confirma_pago_ae_adai.4gl
# GENERATED BY fglwsdl 141859
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "ret_confirma_pago_ae_adai.inc"



#-------------------------------------------------------------------------------
# Service: ZWS_CONFIRMACIONPAGO
# Port:    zws_confirmacionpago
# Server:  http://091402AI72.infonavit.net:8000/sap/bc/srt/rfc/sap/zws_confirmacionpago/300/zws_confirmacionpago/zws_confirmacionpago
#-------------------------------------------------------------------------------

PRIVATE DEFINE ZcrmWsConfirmacionPagoHTTPReq     com.HTTPRequest
PRIVATE DEFINE ZcrmWsConfirmacionPagoHTTPResp    com.HTTPResponse

#-------------------------------------------------------------------------------

#
# Operation: ZcrmWsConfirmacionPago
#
#
# FUNCTION: ZcrmWsConfirmacionPago_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns2ZcrmWsConfirmacionPago
#   OUTPUT: GLOBAL ns2ZcrmWsConfirmacionPagoResponse
#
FUNCTION ZcrmWsConfirmacionPago_g()
  DEFINE wsstatus   INTEGER
  DEFINE retryAuth  INTEGER
  DEFINE retryProxy INTEGER
  DEFINE retry      INTEGER
  DEFINE nb         INTEGER
  DEFINE uri        STRING
  DEFINE setcookie  STRING
  DEFINE mustUnderstand INTEGER
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
  LET uri = com.WebServiceEngine.GetOption("SoapModuleURI")

  IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri IS NULL THEN
    --LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri = "http://091402AI72.infonavit.net:8000/sap/bc/srt/rfc/sap/zws_confirmacionpago/300/zws_confirmacionpago/zws_confirmacionpago"
    LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri = "alias://confirmapagoadaiae"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri)
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"\"")
    CALL WSHelper_SetRequestHeaders(request, ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Request.Headers)
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Version IS NOT NULL THEN
      CALL request.setVersion(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Version)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL request.setHeader("Cookie",ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL request.setConnectionTimeout(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL request.setTimeout(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL request.setHeader("Content-Encoding",ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.CompressRequest)
    END IF
    CALL request.setHeader("Accept-Encoding","gzip, deflate")
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
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(ns2ZcrmWsConfirmacionPago,writer)
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

      #
      # RETRIEVE SERVICE SESSION COOKIE
      #
      LET setcookie = response.getHeader("Set-Cookie")
      IF setcookie IS NOT NULL THEN
        LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(response, ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Response.Headers)
      CASE response.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = response.beginXmlResponse() # Begin Streaming Response
          IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
            EXIT CASE
          END IF
          # Process SOAP headers 
          IF WSHelper_CheckStaxSOAP11Header(reader) AND NOT reader.isEmptyElement() THEN
            CALL reader.nextTag()
            WHILE (reader.getEventType()=="START_ELEMENT")
              IF WSHelper_CheckStaxSOAP11HeaderActor(reader,uri) THEN
                LET mustUnderstand = WSHelper_GetStaxSOAP11HeaderMustUnderstand(reader)
                IF mustUnderstand = -1 THEN
                  CALL WSHelper_FillSOAP11WSError("Client","Invalid mustUnderstand value")
                  EXIT CASE
                END IF
                IF mustUnderstand THEN
                  CALL WSHelper_FillSOAP11WSError("MustUnderstand","Mandatory header block not understood")
                  EXIT CASE
                ELSE
                  CALL reader.nextSibling() # Skip header, not necessary
                END IF
              ELSE
                CALL reader.nextSibling() # Skip header, not intended to us
              END IF
            END WHILE
            IF NOT WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL WSHelper_FillSOAP11WSError("Client","No ending header tag found")
              EXIT CASE
            ELSE
              CALL reader.nextTag()
            END IF
          ELSE
            IF WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL reader.nextSibling() # Skip SOAP headers
            END IF
          END IF
          IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11StartFault(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11FaultUntilDetail(reader) THEN
            EXIT CASE
          END IF
          IF WSHelper_CheckStaxSOAP11FaultDetail(reader) THEN
            #
            # STAX SOAP FAULT DESERIALIZE
            #
            CALL reader.nextSibling() # Skip SOAP detail
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndFault(reader) THEN
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
          # Process SOAP headers 
          IF WSHelper_CheckStaxSOAP11Header(reader) AND NOT reader.isEmptyElement() THEN
            LET nb = 0
            CALL reader.nextTag()
            WHILE (reader.getEventType()=="START_ELEMENT")
              IF WSHelper_CheckStaxSOAP11HeaderActor(reader,uri) THEN
                LET mustUnderstand = WSHelper_GetStaxSOAP11HeaderMustUnderstand(reader)
                IF mustUnderstand = -1 THEN
                  CALL WSHelper_FillSOAP11WSError("Client","Invalid mustUnderstand value")
                  EXIT CASE
                END IF
                #
                # STAX SOAP RESPONSE HEADER DESERIALIZE
                #
                IF mustUnderstand THEN
                  CALL WSHelper_FillSOAP11WSError("MustUnderstand","Mandatory header block not understood")
                  EXIT CASE
                ELSE
                  CALL reader.nextSibling() # Skip header, not necessary
                END IF
              ELSE
                CALL reader.nextSibling() # Skip header, not intended to us
              END IF
            END WHILE
            IF NOT WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL WSHelper_FillSOAP11WSError("Client","No ending header tag found")
              EXIT CASE
            ELSE
              IF nb != 0 THEN
                CALL WSHelper_FillSOAP11WSError("Client","One or more headers are missing")
                EXIT CASE
              ELSE
                CALL reader.nextTag()
              END IF
            END IF
          ELSE
           IF reader.isEmptyElement() THEN
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
          #
          # STAX SOAP RESPONSE DESERIALIZE
          #
          CALL xml.Serializer.StaxToVariable(reader,ns2ZcrmWsConfirmacionPagoResponse)
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


FUNCTION ZcrmWsConfirmacionPagoRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF ZcrmWsConfirmacionPagoHTTPReq IS NOT NULL AND ZcrmWsConfirmacionPagoHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri IS NULL THEN
      --LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri = "http://091402AI72.infonavit.net:8000/sap/bc/srt/rfc/sap/zws_confirmacionpago/300/zws_confirmacionpago/zws_confirmacionpago"
      LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri = "alias://confirmapagoadaiae"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET ZcrmWsConfirmacionPagoHTTPReq = com.HTTPRequest.Create(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri)
    CALL ZcrmWsConfirmacionPagoHTTPReq.setMethod("POST")
    CALL ZcrmWsConfirmacionPagoHTTPReq.setCharset("UTF-8")
    CALL ZcrmWsConfirmacionPagoHTTPReq.setHeader("SOAPAction","\"\"")
    CALL WSHelper_SetRequestHeaders(ZcrmWsConfirmacionPagoHTTPReq, ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Request.Headers)
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Version IS NOT NULL THEN
      CALL ZcrmWsConfirmacionPagoHTTPReq.setVersion(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Version)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL ZcrmWsConfirmacionPagoHTTPReq.setHeader("Cookie",ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL ZcrmWsConfirmacionPagoHTTPReq.setConnectionTimeout(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL ZcrmWsConfirmacionPagoHTTPReq.setTimeout(ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL ZcrmWsConfirmacionPagoHTTPReq.setHeader("Content-Encoding",ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.CompressRequest)
    END IF
    CALL ZcrmWsConfirmacionPagoHTTPReq.setHeader("Accept-Encoding","gzip, deflate")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET ZcrmWsConfirmacionPagoHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = ZcrmWsConfirmacionPagoHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(ns2ZcrmWsConfirmacionPago,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL ZcrmWsConfirmacionPagoHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET ZcrmWsConfirmacionPagoHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET ZcrmWsConfirmacionPagoHTTPResp = ZcrmWsConfirmacionPagoHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET ZcrmWsConfirmacionPagoHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION ZcrmWsConfirmacionPagoResponse_g()
  DEFINE wsstatus        INTEGER
  DEFINE nb              INTEGER
  DEFINE uri             STRING
  DEFINE setcookie       STRING
  DEFINE mustUnderstand  INTEGER
  DEFINE reader          xml.StaxReader

  LET wsstatus = -1

  LET uri = com.WebServiceEngine.GetOption("SoapModuleURI")
  #
  # CHECK PREVIOUS CALL  
  #
  IF ZcrmWsConfirmacionPagoHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF ZcrmWsConfirmacionPagoHTTPResp IS NULL THEN
      # Still no response, try again
      LET ZcrmWsConfirmacionPagoHTTPResp = ZcrmWsConfirmacionPagoHTTPReq.getAsyncResponse()
    END IF

    IF ZcrmWsConfirmacionPagoHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      #
      # RETRIEVE SERVICE SESSION COOKIE
      #
      LET setcookie = ZcrmWsConfirmacionPagoHTTPResp.getHeader("Set-Cookie")
      IF setcookie IS NOT NULL THEN
        LET ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(ZcrmWsConfirmacionPagoHTTPResp, ZWS_CONFIRMACIONPAGO_zws_confirmacionpagoEndpoint.Binding.Response.Headers)
      CASE ZcrmWsConfirmacionPagoHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = ZcrmWsConfirmacionPagoHTTPResp.beginXmlResponse() # Begin Streaming Response
          IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
            EXIT CASE
          END IF
          # Process SOAP headers 
          IF WSHelper_CheckStaxSOAP11Header(reader) AND NOT reader.isEmptyElement() THEN
            CALL reader.nextTag()
            WHILE (reader.getEventType()=="START_ELEMENT")
              IF WSHelper_CheckStaxSOAP11HeaderActor(reader,uri) THEN
                LET mustUnderstand = WSHelper_GetStaxSOAP11HeaderMustUnderstand(reader)
                IF mustUnderstand = -1 THEN
                  CALL WSHelper_FillSOAP11WSError("Client","Invalid mustUnderstand value")
                  EXIT CASE
                END IF
                IF mustUnderstand THEN
                  CALL WSHelper_FillSOAP11WSError("MustUnderstand","Mandatory header block not understood")
                  EXIT CASE
                ELSE
                  CALL reader.nextSibling() # Skip header, not necessary
                END IF
              ELSE
                CALL reader.nextSibling() # Skip header, not intended to us
              END IF
            END WHILE
            IF NOT WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL WSHelper_FillSOAP11WSError("Client","No ending header tag found")
              EXIT CASE
            ELSE
              CALL reader.nextTag()
            END IF
          ELSE
            IF WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL reader.nextSibling() # Skip SOAP headers
            END IF
          END IF
          IF NOT WSHelper_ReadStaxSOAP11StartBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11StartFault(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11FaultUntilDetail(reader) THEN
            EXIT CASE
          END IF
          IF WSHelper_CheckStaxSOAP11FaultDetail(reader) THEN
            #
            # STAX SOAP FAULT DESERIALIZE
            #
            CALL reader.nextSibling() # Skip SOAP detail
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndFault(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL ZcrmWsConfirmacionPagoHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = ZcrmWsConfirmacionPagoHTTPResp.beginXmlResponse() # Begin Streaming Response
          IF NOT WSHelper_ReadStaxSOAP11StartEnvelope(reader) THEN
            EXIT CASE
          END IF
          # Process SOAP headers 
          IF WSHelper_CheckStaxSOAP11Header(reader) AND NOT reader.isEmptyElement() THEN
            LET nb = 0
            CALL reader.nextTag()
            WHILE (reader.getEventType()=="START_ELEMENT")
              IF WSHelper_CheckStaxSOAP11HeaderActor(reader,uri) THEN
                LET mustUnderstand = WSHelper_GetStaxSOAP11HeaderMustUnderstand(reader)
                IF mustUnderstand = -1 THEN
                  CALL WSHelper_FillSOAP11WSError("Client","Invalid mustUnderstand value")
                  EXIT CASE
                END IF
                #
                # STAX SOAP RESPONSE HEADER DESERIALIZE
                #
                IF mustUnderstand THEN
                  CALL WSHelper_FillSOAP11WSError("MustUnderstand","Mandatory header block not understood")
                  EXIT CASE
                ELSE
                  CALL reader.nextSibling() # Skip header, not necessary
                END IF
              ELSE
                CALL reader.nextSibling() # Skip header, not intended to us
              END IF
            END WHILE
            IF NOT WSHelper_CheckStaxSOAP11Header(reader) THEN
              CALL WSHelper_FillSOAP11WSError("Client","No ending header tag found")
              EXIT CASE
            ELSE
              IF nb != 0 THEN
                CALL WSHelper_FillSOAP11WSError("Client","One or more headers are missing")
                EXIT CASE
              ELSE
                CALL reader.nextTag()
              END IF
            END IF
          ELSE
           IF reader.isEmptyElement() THEN
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
          #
          # STAX SOAP RESPONSE DESERIALIZE
          #
          CALL xml.Serializer.StaxToVariable(reader,ns2ZcrmWsConfirmacionPagoResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL ZcrmWsConfirmacionPagoHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||ZcrmWsConfirmacionPagoHTTPResp.getStatusCode()||" ("||ZcrmWsConfirmacionPagoHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET ZcrmWsConfirmacionPagoHTTPReq = NULL
  LET ZcrmWsConfirmacionPagoHTTPResp = NULL
  RETURN wsstatus

END FUNCTION


