#-------------------------------------------------------------------------------
# File: ret_busca_caso_crm.4gl
# GENERATED BY fglwsdl 141859
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "ret_busca_caso_crm.inc"



#-------------------------------------------------------------------------------
# Service: SI_BusquedaCasosCRMSSV_SOService
# Port:    HTTP_Port
# Server:  http://091402ai203.infonavit.net:50000/XISOAPAdapter/MessageServlet?senderParty=&senderService=BC_PORTAL&receiverParty=&receiverService=&interface=SI_BusquedaCasosCRMSSV_SO&interfaceNamespace=http%3A%2F%2Finfonavit.org.mx%2FADJUNTA%2FsndBusquedaCasosCRMSSV
#-------------------------------------------------------------------------------

PRIVATE DEFINE SI_BusquedaCasosCRMSSV_SOHTTPReq     com.HTTPRequest
PRIVATE DEFINE SI_BusquedaCasosCRMSSV_SOHTTPResp    com.HTTPResponse

#-------------------------------------------------------------------------------

#
# Operation: SI_BusquedaCasosCRMSSV_SO
#
#
# FUNCTION: SI_BusquedaCasosCRMSSV_SO_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL MT_BusquedaCasosCRM_SSV_req
#   OUTPUT: GLOBAL MT_BusquedaCasosCRM_SSV_res
#
FUNCTION SI_BusquedaCasosCRMSSV_SO_g()
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

  IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri IS NULL THEN
    --LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri = "http://091402ai203.infonavit.net:50000/XISOAPAdapter/MessageServlet?senderParty=&senderService=BC_PORTAL&receiverParty=&receiverService=&interface=SI_BusquedaCasosCRMSSV_SO&interfaceNamespace=http%3A%2F%2Finfonavit.org.mx%2FADJUNTA%2FsndBusquedaCasosCRMSSV"
    LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri = "alias://buscacasocrm"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri)
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"http://sap.com/xi/WebService/soap1.1\"")
    CALL WSHelper_SetRequestHeaders(request, SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Request.Headers)
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Version IS NOT NULL THEN
      CALL request.setVersion(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Version)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL request.setHeader("Cookie",SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL request.setConnectionTimeout(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ConnectionTimeout)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL request.setTimeout(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL request.setHeader("Content-Encoding",SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.CompressRequest)
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
      CALL xml.Serializer.VariableToStax(MT_BusquedaCasosCRM_SSV_req,writer)
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
        LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(response, SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Response.Headers)
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
          CALL xml.Serializer.StaxToVariable(reader,MT_BusquedaCasosCRM_SSV_res)
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


FUNCTION SI_BusquedaCasosCRMSSV_SORequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF SI_BusquedaCasosCRMSSV_SOHTTPReq IS NOT NULL AND SI_BusquedaCasosCRMSSV_SOHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri IS NULL THEN
      --LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri = "http://091402ai203.infonavit.net:50000/XISOAPAdapter/MessageServlet?senderParty=&senderService=BC_PORTAL&receiverParty=&receiverService=&interface=SI_BusquedaCasosCRMSSV_SO&interfaceNamespace=http%3A%2F%2Finfonavit.org.mx%2FADJUNTA%2FsndBusquedaCasosCRMSSV"
      LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri = "alias://buscacasocrm"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET SI_BusquedaCasosCRMSSV_SOHTTPReq = com.HTTPRequest.Create(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri)
    CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setMethod("POST")
    CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setCharset("UTF-8")
    CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setHeader("SOAPAction","\"http://sap.com/xi/WebService/soap1.1\"")
    CALL WSHelper_SetRequestHeaders(SI_BusquedaCasosCRMSSV_SOHTTPReq, SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Request.Headers)
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Version IS NOT NULL THEN
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setVersion(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Version)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setHeader("Cookie",SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setConnectionTimeout(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ConnectionTimeout)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setTimeout(SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setHeader("Content-Encoding",SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.CompressRequest)
    END IF
    CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.setHeader("Accept-Encoding","gzip, deflate")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET SI_BusquedaCasosCRMSSV_SOHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = SI_BusquedaCasosCRMSSV_SOHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(MT_BusquedaCasosCRM_SSV_req,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL SI_BusquedaCasosCRMSSV_SOHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET SI_BusquedaCasosCRMSSV_SOHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET SI_BusquedaCasosCRMSSV_SOHTTPResp = SI_BusquedaCasosCRMSSV_SOHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET SI_BusquedaCasosCRMSSV_SOHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION SI_BusquedaCasosCRMSSV_SOResponse_g()
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
  IF SI_BusquedaCasosCRMSSV_SOHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF SI_BusquedaCasosCRMSSV_SOHTTPResp IS NULL THEN
      # Still no response, try again
      LET SI_BusquedaCasosCRMSSV_SOHTTPResp = SI_BusquedaCasosCRMSSV_SOHTTPReq.getAsyncResponse()
    END IF

    IF SI_BusquedaCasosCRMSSV_SOHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      #
      # RETRIEVE SERVICE SESSION COOKIE
      #
      LET setcookie = SI_BusquedaCasosCRMSSV_SOHTTPResp.getHeader("Set-Cookie")
      IF setcookie IS NOT NULL THEN
        LET SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(SI_BusquedaCasosCRMSSV_SOHTTPResp, SI_BusquedaCasosCRMSSV_SOService_HTTP_PortEndpoint.Binding.Response.Headers)
      CASE SI_BusquedaCasosCRMSSV_SOHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = SI_BusquedaCasosCRMSSV_SOHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL SI_BusquedaCasosCRMSSV_SOHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = SI_BusquedaCasosCRMSSV_SOHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,MT_BusquedaCasosCRM_SSV_res)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL SI_BusquedaCasosCRMSSV_SOHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||SI_BusquedaCasosCRMSSV_SOHTTPResp.getStatusCode()||" ("||SI_BusquedaCasosCRMSSV_SOHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET SI_BusquedaCasosCRMSSV_SOHTTPReq = NULL
  LET SI_BusquedaCasosCRMSSV_SOHTTPResp = NULL
  RETURN wsstatus

END FUNCTION


