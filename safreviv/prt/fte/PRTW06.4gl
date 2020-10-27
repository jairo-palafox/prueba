#-------------------------------------------------------------------------------
# File: PRTW06.4gl
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "PRTW06.inc"



#-------------------------------------------------------------------------------
# Service: AMPCPOR1Service
# Port:    AMPCPOR1Port
# Server:  http://10.90.1.24:30018/WebService/ConPorAls
#-------------------------------------------------------------------------------

PRIVATE DEFINE AMPCPOR1OperationHTTPReq     com.HTTPRequest
PRIVATE DEFINE AMPCPOR1OperationHTTPResp    com.HTTPResponse


FUNCTION consultaCreditoCartera(p_nss,p_id_credito)
DEFINE p_nss        CHAR(11),
       p_id_credito VARCHAR(10),
       v_ws_error   INTEGER,
       v_monto      MONEY(13,2),
       v_saldo      MONEY(13,2),
       v_monto_dec  MONEY(13,2),
       v_saldo_dec  MONEY(13,2)

   LET ns1AMPCPOR1Operation.datos_entrada_epor.e_num_nss = p_nss
   LET ns1AMPCPOR1Operation.datos_entrada_epor.e_num_crd = p_id_credito

   CALL AMPCPOR1Operation_g() RETURNING v_ws_error

   # Conversión de tipo moneda a decimal
   LET v_monto = ns2AMPCPOR1OperationResponse.datos_salida_spor.s_monto_origen
   LET v_saldo = ns2AMPCPOR1OperationResponse.datos_salida_spor.s_saldo_al_dia
   LET v_monto_dec = v_monto
   LET v_saldo_dec = v_saldo

   RETURN v_ws_error,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_num_nss,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_num_crd,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_iden_procede,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_tipo_credito,
          --ns2AMPCPOR1OperationResponse.datos_salida_spor.s_saldo_al_dia,
          v_saldo_dec,
          --ns2AMPCPOR1OperationResponse.datos_salida_spor.s_monto_origen,
          v_monto_dec,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_fecha_origen,
          ns2AMPCPOR1OperationResponse.datos_salida_spor.s_cod_ret
END FUNCTION
#-------------------------------------------------------------------------------

#
# Operation: AMPCPOR1Operation
#
#
# FUNCTION: AMPCPOR1Operation_g
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1AMPCPOR1Operation
#   OUTPUT: GLOBAL ns2AMPCPOR1OperationResponse
#
FUNCTION AMPCPOR1Operation_g()
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

  IF AMPCPOR1Service_AMPCPOR1PortLocation IS NULL THEN
    LET AMPCPOR1Service_AMPCPOR1PortLocation = "http://10.90.1.24:30018/WebService/ConPorAls"
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET request = com.HTTPRequest.Create(AMPCPOR1Service_AMPCPOR1PortLocation)
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
      CALL xml.Serializer.VariableToStax(ns1AMPCPOR1Operation,writer)
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
          CALL xml.Serializer.StaxToVariable(reader,ns2AMPCPOR1OperationResponse)
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


FUNCTION AMPCPOR1OperationRequest_g()
  DEFINE wsstatus   INTEGER
  DEFINE writer     xml.StaxWriter

  #
  # CHECK PREVIOUS CALL  
  #
  IF AMPCPOR1OperationHTTPReq IS NOT NULL AND AMPCPOR1OperationHTTPResp IS NULL THEN
    # Request was sent but there was no response yet
    CALL WSHelper_FillSOAP11WSError("Client","Cannot issue a new request until previous response was received")
    RETURN -2 # waiting for the response
  ELSE
    IF AMPCPOR1Service_AMPCPOR1PortLocation IS NULL THEN
      LET AMPCPOR1Service_AMPCPOR1PortLocation = "http://10.90.1.24:30018/WebService/ConPorAls"
    END IF
  END IF

  #
  # CREATE REQUEST
  #
  TRY
    LET AMPCPOR1OperationHTTPReq = com.HTTPRequest.Create(AMPCPOR1Service_AMPCPOR1PortLocation)
    CALL AMPCPOR1OperationHTTPReq.setMethod("POST")
    CALL AMPCPOR1OperationHTTPReq.setCharset("UTF-8")
    CALL AMPCPOR1OperationHTTPReq.setHeader("SOAPAction","\"\"")
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Client","Cannot create HTTPRequest")
    LET AMPCPOR1OperationHTTPReq = NULL
    RETURN wsstatus
  END TRY

    #
    # Stax request
    #
    TRY
      LET writer = AMPCPOR1OperationHTTPReq.beginXmlRequest()
      CALL WSHelper_WriteStaxSOAP11StartEnvelope(writer)
      CALL WSHelper_WriteStaxSOAP11StartBody(writer)
      CALL xml.Serializer.VariableToStax(ns1AMPCPOR1Operation,writer)
      CALL WSHelper_WriteStaxSOAP11EndBody(writer)
      CALL WSHelper_WriteStaxSOAP11EndEnvelope(writer)
      CALL AMPCPOR1OperationHTTPReq.endXmlRequest(writer)
    CATCH
      LET wsstatus = STATUS
      CALL WSHelper_FillSOAP11WSError("Client",SQLCA.SQLERRM)
      LET AMPCPOR1OperationHTTPReq = NULL
      RETURN wsstatus
    END TRY

  #
  # PROCESS RESPONSE
  #
  TRY
    LET AMPCPOR1OperationHTTPResp = AMPCPOR1OperationHTTPReq.getAsyncResponse()
    RETURN 0 # SUCCESS
  CATCH
    LET wsstatus = STATUS
    CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    LET AMPCPOR1OperationHTTPReq = NULL
    RETURN wsstatus
  END TRY
END FUNCTION


FUNCTION AMPCPOR1OperationResponse_g()
  DEFINE wsstatus   INTEGER
  DEFINE reader     xml.StaxReader

  LET wsstatus = -1

  #
  # CHECK PREVIOUS CALL  
  #
  IF AMPCPOR1OperationHTTPReq IS NULL THEN
    # No request was sent
    CALL WSHelper_FillSOAP11WSError("Client","No request has been sent")
    RETURN -1
  END IF

  TRY
    #
    # PROCESS RESPONSE
    #
    IF AMPCPOR1OperationHTTPResp IS NULL THEN
      # Still no response, try again
      LET AMPCPOR1OperationHTTPResp = AMPCPOR1OperationHTTPReq.getAsyncResponse()
    END IF

    IF AMPCPOR1OperationHTTPResp IS NULL THEN
      # We got no response, still waiting for
      CALL WSHelper_FillSOAP11WSError("Client","Response was not yet received")
      RETURN -2
    END IF

      CASE AMPCPOR1OperationHTTPResp.getStatusCode()

        WHEN 500 # SOAP Fault
          #
          # STAX SOAP FAULT
          #
          LET reader = AMPCPOR1OperationHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL AMPCPOR1OperationHTTPResp.endXmlResponse(reader)

        WHEN 200 # SOAP Result
          #
          # STAX SOAP RESPONSE
          #
          LET reader = AMPCPOR1OperationHTTPResp.beginXmlResponse() # Begin Streaming Response
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
          CALL xml.Serializer.StaxToVariable(reader,ns2AMPCPOR1OperationResponse)
          IF NOT WSHelper_ReadStaxSOAP11EndBody(reader) THEN
            EXIT CASE
          END IF
          IF NOT WSHelper_ReadStaxSOAP11EndEnvelope(reader) THEN
            EXIT CASE
          END IF
          # End Streaming Response
          CALL AMPCPOR1OperationHTTPResp.endXmlResponse(reader)
          LET wsstatus = 0

        OTHERWISE
          CALL WSHelper_FillSOAP11WSError("Server","HTTP Error "||AMPCPOR1OperationHTTPResp.getStatusCode()||" ("||AMPCPOR1OperationHTTPResp.getStatusDescription()||")")

      END CASE
    CATCH
      LET wsstatus = status
      CALL WSHelper_FillSOAP11WSError("Server",SQLCA.SQLERRM)
    END TRY

  #
  # RESET VARIABLES
  #
  LET AMPCPOR1OperationHTTPReq = NULL
  LET AMPCPOR1OperationHTTPResp = NULL
  RETURN wsstatus
END FUNCTION


