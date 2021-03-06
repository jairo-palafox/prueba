#-------------------------------------------------------------------------------
# File: servicio_cliente.4gl
# GENERATED BY fglwsdl 141859
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "servicio_cliente.inc"



#-------------------------------------------------------------------------------
# Service: ws_prueba_monitor
# Port:    ws_prueba_monitorPortType
# Server:  http://172.16.16.204:6130/ws/r/ws_prueba_monitor
#-------------------------------------------------------------------------------

PRIVATE DEFINE sumaHTTPReq     com.HTTPRequest
PRIVATE DEFINE sumaHTTPResp    com.HTTPResponse

MAIN
DEFINE v_resultado_soap INTEGER,
       v_resultado DECIMAL (10,2)

   CALL suma(2,6) RETURNING v_resultado_soap,v_resultado

   DISPLAY "v_resultado_soap:",v_resultado_soap
   DISPLAY "v_resultado:",v_resultado

END MAIN

#-------------------------------------------------------------------------------

#
# Operation: suma
#

#
# FUNCTION: suma
#
FUNCTION suma(p_v_sumando1, p_v_sumando2)
  DEFINE	p_v_sumando1		tsuma_v_sumando1
  DEFINE	p_v_sumando2		tsuma_v_sumando2
  DEFINE	soapStatus		INTEGER


  LET suma.v_sumando1 = p_v_sumando1
  LET suma.v_sumando2 = p_v_sumando2

  LET soapStatus = suma_g()

  RETURN soapStatus, sumaResponse.v_resultado
END FUNCTION

#
# FUNCTION: suma_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL suma
#   OUTPUT: GLOBAL sumaResponse
#
FUNCTION suma_g()
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

  IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri IS NULL THEN
    LET ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri = "http://172.16.16.204:6130/ws/r/ws_prueba_monitor"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri)
    CALL request.setMethod("POST")
    CALL request.setCharset("UTF-8")
    CALL request.setHeader("SOAPAction","\"\"")
    CALL WSHelper_SetRequestHeaders(request, ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Request.Headers)
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Version IS NOT NULL THEN
      CALL request.setVersion(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Version)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL request.setHeader("Cookie",ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL request.setConnectionTimeout(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL request.setTimeout(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL request.setHeader("Content-Encoding",ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.CompressRequest)
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
      CALL xml.Serializer.VariableToStax(suma,writer)
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
        LET ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(response, ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Response.Headers)
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
          CALL xml.Serializer.StaxToVariable(reader,sumaResponse)
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


FUNCTION sumaRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF sumaHTTPReq IS NOT NULL AND sumaHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri IS NULL THEN
      LET ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri = "http://172.16.16.204:6130/ws/r/ws_prueba_monitor"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET sumaHTTPReq = com.HTTPRequest.Create(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri)
    CALL sumaHTTPReq.setMethod("POST")
    CALL sumaHTTPReq.setCharset("UTF-8")
    CALL sumaHTTPReq.setHeader("SOAPAction","\"\"")
    CALL WSHelper_SetRequestHeaders(sumaHTTPReq, ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Request.Headers)
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Version IS NOT NULL THEN
      CALL sumaHTTPReq.setVersion(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Version)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie IS NOT NULL THEN
      CALL sumaHTTPReq.setHeader("Cookie",ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ConnectionTimeout <> 0 THEN
      CALL sumaHTTPReq.setConnectionTimeout(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ConnectionTimeout)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ReadWriteTimeout <> 0 THEN
      CALL sumaHTTPReq.setTimeout(ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.ReadWriteTimeout)
    END IF
    IF ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.CompressRequest IS NOT NULL THEN
      CALL sumaHTTPReq.setHeader("Content-Encoding",ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.CompressRequest)
    END IF
    CALL sumaHTTPReq.setHeader("Accept-Encoding","gzip, deflate")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET sumaHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = sumaHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      #
      # STAX SOAP REQUEST SERIALIZE
      #
      CALL xml.Serializer.VariableToStax(suma,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL sumaHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET sumaHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET sumaHTTPResp = sumaHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET sumaHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION sumaResponse_g()
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
  IF sumaHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF sumaHTTPResp IS NULL THEN
      # Still no response, try again
      LET sumaHTTPResp = sumaHTTPReq.getAsyncResponse()
    END IF

    IF sumaHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      #
      # RETRIEVE SERVICE SESSION COOKIE
      #
      LET setcookie = sumaHTTPResp.getHeader("Set-Cookie")
      IF setcookie IS NOT NULL THEN
        LET ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Cookie = WSHelper_ExtractServerCookie(setcookie,ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Address.Uri)
      END IF

      #
      # RETRIEVE HTTP RESPONSE Headers
      #
      CALL WSHelper_SetResponseHeaders(sumaHTTPResp, ws_prueba_monitor_ws_prueba_monitorPortTypeEndpoint.Binding.Response.Headers)
      CASE sumaHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = sumaHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL sumaHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = sumaHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,sumaResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL sumaHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||sumaHTTPResp.getStatusCode()||" ("||sumaHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET sumaHTTPReq = NULL
  LET sumaHTTPResp = NULL
  RETURN wsstatus

END FUNCTION


