--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/02/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS06                                                       #
#Objetivo     => Responde solicitud de crédito a FOVISSSTE                     #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "PRTWS06.inc"

MAIN
DEFINE v_respuesta_ws INTEGER
   
   LET g_usuario_cod = "SFAREVIV"
   
   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio()

   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA WS ENVIO CRÉDITO INFONAVIT RECEPTORA ", CURRENT YEAR TO SECOND

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

#Objetivo: Función para crear servcio de solicitud de portabilidad receptora
FUNCTION fn_crea_servicio()
DEFINE v_servicio     com.WebService,
       v_operacion    com.WebOperation
       

   LET v_servicio = com.WebService.CreateWebService("envioCreditoInfonavitService","http://infonavit.portabilidad.credito.envia")
   
   LET v_operacion = com.WebOperation.CreateRPCStyle("enviaCreditoInfonavit", # Funcion a ejecutar del servicio 
                                                     "enviaCreditoInfonavit", # Nombre de la operacion
                                                     mensajeEntradaSolicitud,    # Datos de entrada
                                                     mensajeSalidaSolicitud)     # Datos salida

   CALL v_servicio.publishOperation(v_operacion,"enviaCreditoInfonavitReceptora")
   CALL com.WebServiceEngine.RegisterService(v_servicio)

END FUNCTION

#Objetivo: Función invocada por WS para generar la solicitud de portabilidad receptora
FUNCTION enviaCreditoInfonavit()

   CALL fn_inicia_solicitud()
   {CALL fn_registra_solicitud_cedente() RETURNING r_error,
                                                  r_id_prt_solicitud

   IF(r_error = 0)THEN
      CALL fn_valida_solicitud_portabilidad() RETURNING r_error
      IF(r_error = 0)THEN 
         # Invocar sp para ejecutar safre bus y a su vez el servicio de fovissste
         CALL fn_consulta_credito_cartera() RETURNING r_error
         IF NOT( r_error )THEN                                                   
            # función para actulizar registros según infromación devuelta por fovissste
            CALL fn_actualiza_solicitud() RETURNING r_error
            IF NOT( r_error )THEN
               LET mensajeSalidaSolicitud.idMotivo = 0
               LET mensajeSalidaSolicitud.descripcion = NULL
            END IF
         END IF
      END IF
   END IF}

END FUNCTION


