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
DATABASE safre_viv

GLOBALS "PRTWS01.inc"

MAIN
DEFINE v_respuesta_ws INTEGER
   
   LET g_usuario_cod = "SAFREVIV" # Por no intervenir un usuario especifico, se usa safreviv
   
   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio()

   # Puerto 8071
   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA WS SOLICITUD PORTABILIDAD CEDENTE ",CURRENT YEAR TO SECOND

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

#Objetivo: Función para crear servcio de solicitud de portabilidad cedente
FUNCTION fn_crea_servicio()
DEFINE v_servicio     com.WebService,
       v_operacion    com.WebOperation
       

   LET v_servicio = com.WebService.CreateWebService("solicitudCreditoFovissste","http://www.portabilidad.com.solicita.credito.fovissste")
   
   LET v_operacion = com.WebOperation.CreateRPCStyle("solicitaCreditoFovissste", # Funcion a ejecutar del servicio 
                                                     "solicitaCreditoFovissste",          # Nombre de la operacion
                                                     mensajeEntradaSolicitud,            # Datos de entrada
                                                     mensajeSalidaSolicitud)             # Datos salida

   CALL v_servicio.publishOperation(v_operacion,"solicitaCreditoFovissste") -- quitamos el null
   CALL com.WebServiceEngine.RegisterService(v_servicio)

END FUNCTION

#Objetivo: Función invocada por WS para generar la solicitud de portabilidad cedente
FUNCTION solicitaCreditoFovissste()

   CALL fn_determina_tipo_consulta()

END FUNCTION


