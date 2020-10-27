IMPORT com

##############################################################################
#
# Web Service Server:
# - Publication of the services
# - Handle the SOAP protocol
#

IMPORT com
IMPORT XML
IMPORT util

GLOBALS "CTAWS17.inc"

MAIN
DEFINE v_respuesta_ws INTEGER

   CALL createservice() RETURNING v_respuesta_ws

   # Puerto 61700
   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA CTA WS CONSULTA RESUMEN MOVIMIENTOS",CURRENT YEAR TO SECOND

   WHILE TRUE
      # Process each incoming requests (infinite loop)
      LET v_respuesta_ws = com.WebServiceEngine.ProcessServices(-1)

      CASE v_respuesta_ws
         WHEN 0
            DISPLAY "Respuesta WS procesada. ",CURRENT YEAR TO SECOND
         WHEN -1
            DISPLAY "Tiempo de espera terminado. ",CURRENT YEAR TO SECOND
         WHEN -2
            DISPLAY "Desconectado desde el servidor de aplicación. ",CURRENT YEAR TO SECOND
            EXIT PROGRAM 
         WHEN -3
            DISPLAY "Conexión del cliente perdida. ",CURRENT YEAR TO SECOND        
         WHEN -4
            DISPLAY "Servidor interrumpido con Ctrl-C. ",CURRENT YEAR TO SECOND
         WHEN -5
            DISPLAY "Encabezado HTTP erróneo ",CURRENT YEAR TO SECOND
         WHEN -6
            DISPLAY "Envoltura SOAP Malformada. ",CURRENT YEAR TO SECOND
         WHEN -7
            DISPLAY "Documento XML Malformado . ",CURRENT YEAR TO SECOND
         WHEN -8
            DISPLAY "Error interno HTTP. ",CURRENT YEAR TO SECOND
         WHEN -9
            DISPLAY "Operación no soportada. ",CURRENT YEAR TO SECOND        
         WHEN -10
            DISPLAY "Error desconocido. ",CURRENT YEAR TO SECOND
         WHEN -11
            DISPLAY "No se pudo generar WSDL. ",CURRENT YEAR TO SECOND
         WHEN -12
            DISPLAY "Servicio WSDL no encontrado. ",CURRENT YEAR TO SECOND
         WHEN -13
            DISPLAY "Reservado. ",CURRENT YEAR TO SECOND
         WHEN -14
            DISPLAY "Petición excede el tamaño máximo. ",CURRENT YEAR TO SECOND
         WHEN -15
            DISPLAY "El servidor no fue inicializado. ",CURRENT YEAR TO SECOND
         WHEN -16
            DISPLAY "La petición aún está en proceso. ",CURRENT YEAR TO SECOND
         WHEN -17
            DISPLAY "Error stax. ",CURRENT YEAR TO SECOND
         WHEN -18
            DISPLAY "Error en manejador de petición de entrada. ",CURRENT YEAR TO SECOND
         WHEN -19
            DISPLAY "Error en manejador de petición de salida. ",CURRENT YEAR TO SECOND
         WHEN -20
            DISPLAY "Error en manejador de WSDL. ",CURRENT YEAR TO SECOND
         WHEN -21
            DISPLAY "Error de versión SOAP. ",CURRENT YEAR TO SECOND
         WHEN -22
            DISPLAY "Error en encabezado SOAP. ",CURRENT YEAR TO SECOND
         WHEN -23
            DISPLAY "Error de deserialización. ",CURRENT YEAR TO SECOND
      END CASE

      IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
      END IF 
    END WHILE

    DISPLAY "Servidor Detenido"
END MAIN

#-------------------------------------------------------------------------------
# Service: SolicitaResumenMovimientosSACI
# Port:    SolicitaResumenMovimientosSACIPort
#-------------------------------------------------------------------------------
#
# FUNCTION createservice
#   RETURNING soapstatus
#
FUNCTION createservice()
DEFINE service   com.WebService    # A WebService
DEFINE operation com.WebOperation  # Operation of a WebService

   TRY
      # Create Web Service
      LET service = com.WebService.CreateWebService("SolicitaResumenMovimientosCta","http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/")
      CALL service.setFeature("Soap1.1",TRUE)

      # Handle HTTP register methods
      CALL service.registerInputHttpVariable(CTAWS17HttpIn)
      CALL service.registerOutputHttpVariable(CTAWS17HttpOut)


      #
      # Operation: notificarGeneracionFolio
      #

      # Publish Operation : notificarGeneracionFolio
      LET operation = com.WebOperation.CreateDOCStyle("solicitaResumenMovimientos","solicitaResumenMovimientos",ns1solicitaResumenMovimientosRequest,ns1solicitaResumenMovimientosResponse)
      CALL service.publishOperation(operation,"http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")


      #
      # Register Service
      #
      CALL com.WebServiceEngine.RegisterService(service)
      RETURN 0

   CATCH
      RETURN STATUS
   END TRY
    
END FUNCTION

# Descripción:
FUNCTION solicitaResumenMovimientos()

   DISPLAY "DATOS RECIBIDOS:"
   DISPLAY "nss: ",ns1solicitaResumenMovimientosRequest.cuerpo.nss
   DISPLAY "fInicio: ",ns1solicitaResumenMovimientosRequest.cuerpo.fInicio
   DISPLAY "fFin: ",ns1solicitaResumenMovimientosRequest.cuerpo.fFin
   DISPLAY "tipoDocumento: ",ns1solicitaResumenMovimientosRequest.cuerpo.tipoDocumento

   LET ns1solicitaResumenMovimientosResponse.objetoRespuesta.resultadoOperacion = "01"
   LET ns1solicitaResumenMovimientosResponse.objetoRespuesta.motivoRechazo = "000"
   LET ns1solicitaResumenMovimientosResponse.objetoRespuesta.ticketAtencion = util.math.rand(32767) USING "&&&&&",util.math.rand(32767) USING "&&&&&",util.math.rand(2767) USING "&&&&"

END FUNCTION