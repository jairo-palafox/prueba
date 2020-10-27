--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/08/2015
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSWS03                                                       #
#Objetivo     => Registra solicitud de cancelación de pagos                    #
#Fecha inicio => 10 Agosto 2015                                                #
################################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "HPSWS03.inc"

MAIN
DEFINE v_respuesta_ws INTEGER
   
   LET g_usuario_cod = "SAFREVIV" # Por no intervenir un usuario especifico, se usa safreviv
   
   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio() RETURNING v_respuesta_ws

   # Puerto 
   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA WS SOLICITUD CANCELACIÓN PAGOS",CURRENT YEAR TO SECOND

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
        
         WHEN -10
            DISPLAY "Error interno de servidor. ",CURRENT YEAR TO SECOND
        
      END CASE
      
      IF( INT_FLAG <> 0 )THEN
         LET INT_FLAG = 0
         EXIT WHILE
      END IF
  
   END WHILE

DISPLAY "Server stopped"
   
END MAIN

#-------------------------------------------------------------------------------
# Service: SolicitudCancelacionPagosService
# Port:    SolicitudCancelacionPagosS
#-------------------------------------------------------------------------------
#
# FUNCTION Createuntitled-1Service
#   RETURNING soapstatus
#
FUNCTION fn_crea_servicio()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("SolicitudCancelacionPagosService","http://infonavit.hipoteca.solicitud.cancelacion.com.portabilidad")


    #
    # Operation: solicitaCancelacionPago
    #

    # Publish Operation : solicitaCreditoFovissste
    LET operation = com.WebOperation.CreateDOCStyle("solicitaCancelacionPagos","solicitaCancelacionPagos",mensajeEntradaSolicitud,mensajeSalidaSolicitud)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

#Objetivo: Función invocada por WS para generar la solicitud de cancelación de pagos
FUNCTION solicitaCancelacionPagos()

   CALL fn_determina_tipo_consulta()

END FUNCTION