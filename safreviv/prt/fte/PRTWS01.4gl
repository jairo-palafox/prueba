--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS01                                                       #
#Objetivo     => Registra solicitud de portabilidad cedente                    #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "PRTWS01.inc"

MAIN
DEFINE v_respuesta_ws INTEGER
   
   LET g_usuario_cod = "SAFREVIV" # Por no intervenir un usuario especifico, se usa safreviv
   
   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio() RETURNING v_respuesta_ws

   # Puerto 8071
   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA WS SOLICITUD PAQUETE NIST ",CURRENT YEAR TO SECOND

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
# Service: SolicitudCreditoFovisssteService
# Port:    SolicitudCreditoFovissste
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
    LET service = com.WebService.CreateWebService("SolicitudCreditoFovisssteService","http://fovissste.credito.solicita.com.portabilidad")


    #
    # Operation: solicitaCreditoFovissste
    #

    # Publish Operation : solicitaCreditoFovissste
    LET operation = com.WebOperation.CreateDOCStyle("solicitaCreditoFovissste","solicitaCreditoFovissste",mensajeEntradaSolicitud,mensajeSalidaSolicitud)
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

#Objetivo: Función invocada por WS para generar la solicitud de portabilidad cedente
FUNCTION solicitaCreditoFovissste()

   CALL fn_determina_tipo_consulta()

END FUNCTION