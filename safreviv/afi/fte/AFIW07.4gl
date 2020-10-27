####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => AFIW07                                       #
#Objetivo          => Cliente para consultar la CURP en RENAPO     #
#Fecha Inicio      => 07 - Septiembre - 2017                       #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "AFIW07.inc"

#-------------------------------------------------------------------------------
# Service: ConsultaPorCurpService
# Port:    ConsultaPorCurpServiceHttpSoap11Endpoint
# Server:  alias://consultaCurpRenapo
#-------------------------------------------------------------------------------

PRIVATE DEFINE consultarPorCurpHTTPReq     com.HTTPRequest
PRIVATE DEFINE consultarPorCurpHTTPResp    com.HTTPResponse

PRIVATE DEFINE getConfirmHTTPReq     com.HTTPRequest

#-------------------------------------------------------------------------------

#
# Operation: consultarPorCurp
#
#
# FUNCTION: consultarPorCurp_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1consultarPorCurp
#   OUTPUT: GLOBAL ns1consultarPorCurpResponse
#
FUNCTION consultarPorCurp_g()
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

  IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri IS NULL THEN
    LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri = "alias://consultaCurpRenapo"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"urn:consultarPorCurp\"")
    CALL WSHelper_SetRequestHeaders(request, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Request.Headers)
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version IS NOT NULL THEN
      CALL request.setVersion(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL request.setHeader("Cookie",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL request.setConnectionTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL request.setTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL request.setHeader("Content-Encoding",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest)
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
      CALL xml.Serializer.VariableToStax(ns1consultarPorCurp,writer)
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
        LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(response, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Response.Headers)
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
          CALL xml.Serializer.StaxToVariable(reader,ns1consultarPorCurpResponse)
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


FUNCTION consultarPorCurpRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF consultarPorCurpHTTPReq IS NOT NULL AND consultarPorCurpHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri IS NULL THEN
      LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri = "alias://consultaCurpRenapo"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET consultarPorCurpHTTPReq = com.HTTPRequest.Create(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
    CALL consultarPorCurpHTTPReq.setMethod("POST")
    CALL consultarPorCurpHTTPReq.setCharset("UTF-8")
    CALL consultarPorCurpHTTPReq.setHeader("SOAPAction","\"urn:consultarPorCurp\"")
    CALL WSHelper_SetRequestHeaders(consultarPorCurpHTTPReq, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Request.Headers)
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version IS NOT NULL THEN
      CALL consultarPorCurpHTTPReq.setVersion(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL consultarPorCurpHTTPReq.setHeader("Cookie",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL consultarPorCurpHTTPReq.setConnectionTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL consultarPorCurpHTTPReq.setTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL consultarPorCurpHTTPReq.setHeader("Content-Encoding",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest)
    END IF
    CALL consultarPorCurpHTTPReq.setHeader("Accept-Encoding","gzip, deflate")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET consultarPorCurpHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = consultarPorCurpHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(ns1consultarPorCurp,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL consultarPorCurpHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET consultarPorCurpHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET consultarPorCurpHTTPResp = consultarPorCurpHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET consultarPorCurpHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION consultarPorCurpResponse_g()
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
  IF consultarPorCurpHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF consultarPorCurpHTTPResp IS NULL THEN
      # Still no response, try again
      LET consultarPorCurpHTTPResp = consultarPorCurpHTTPReq.getAsyncResponse()
    END IF

    IF consultarPorCurpHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      #
      # RETRIEVE SERVICE SESSION COOKIE
      #
      LET setcookie = consultarPorCurpHTTPResp.getHeader("Set-Cookie")
      IF setcookie IS NOT NULL THEN
        LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(consultarPorCurpHTTPResp, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Response.Headers)
      CASE consultarPorCurpHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = consultarPorCurpHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL consultarPorCurpHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = consultarPorCurpHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,ns1consultarPorCurpResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL consultarPorCurpHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||consultarPorCurpHTTPResp.getStatusCode()||" ("||consultarPorCurpHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET consultarPorCurpHTTPReq = NULL
  LET consultarPorCurpHTTPResp = NULL
  RETURN wsstatus

END FUNCTION



#
# Operation: getConfirm
#

#
# FUNCTION: getConfirm
#
FUNCTION getConfirm(p_sessionID, p_Mssg)
  DEFINE	p_sessionID		STRING
  DEFINE	p_Mssg		STRING
  DEFINE	soapStatus		INTEGER


  LET ns1getConfirm.sessionID = p_sessionID
  LET ns1getConfirm.Mssg = p_Mssg

  LET soapStatus = getConfirm_g()

  RETURN soapStatus
END FUNCTION

#
# FUNCTION: getConfirm_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1getConfirm
#   OUTPUT: NONE
#
FUNCTION getConfirm_g()
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

  #
  # INIT VARIABLES
  #
  LET wsstatus = -1
  LET retryAuth = FALSE
  LET retryProxy = FALSE
  LET retry = TRUE
  LET uri = com.WebServiceEngine.GetOption("SoapModuleURI")

  IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri IS NULL THEN
    LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri = "alias://consultaCurpRenapo"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"urn:getConfirm\"")
    CALL WSHelper_SetRequestHeaders(request, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Request.Headers)
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version IS NOT NULL THEN
      CALL request.setVersion(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL request.setHeader("Cookie",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL request.setConnectionTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL request.setTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL request.setHeader("Content-Encoding",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest)
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
      CALL xml.Serializer.VariableToStax(ns1getConfirm,writer)
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
        LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(response, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Response.Headers)
      CASE response.getStatusCode()

        WHEN 200 # OK
          LET wsstatus = 0

        WHEN 201 # Created
          LET wsstatus = 0

        WHEN 202 # Accepted
          LET wsstatus = 0

        WHEN 203 # Non-Authoritative Information
          LET wsstatus = 0

        WHEN 204 # No Content
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


FUNCTION getConfirmRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CREATE REQUEST
  #
  TRY
    LET getConfirmHTTPReq = com.HTTPRequest.Create(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri)
    CALL getConfirmHTTPReq.setMethod("POST")
    CALL getConfirmHTTPReq.setCharset("UTF-8")
    CALL getConfirmHTTPReq.setHeader("SOAPAction","\"urn:getConfirm\"")
    CALL WSHelper_SetRequestHeaders(getConfirmHTTPReq, ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Request.Headers)
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version IS NOT NULL THEN
      CALL getConfirmHTTPReq.setVersion(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Version)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL getConfirmHTTPReq.setHeader("Cookie",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.Cookie)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL getConfirmHTTPReq.setConnectionTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL getConfirmHTTPReq.setTimeout(ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL getConfirmHTTPReq.setHeader("Content-Encoding",ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Binding.CompressRequest)
    END IF
    CALL getConfirmHTTPReq.setHeader("Accept-Encoding","gzip, deflate")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET getConfirmHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = getConfirmHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(ns1getConfirm,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL getConfirmHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET getConfirmHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # SKIP PROCESS RESPONSE
  #
  LET getConfirmHTTPReq = NULL
  RETURN 0 # SUCCESS
END FUNCTION


