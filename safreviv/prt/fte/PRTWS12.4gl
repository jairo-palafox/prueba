--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/11/2015
--==============================================================================
################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS12                                                       #
#Objetivo     => Cancelacion de Solicitudes de portabilidad                    #
#Fecha inicio => 20 Noviembre 2015                                             #
################################################################################
IMPORT com
DATABASE safre_viv

GLOBALS "PRTWS12.inc"

MAIN
DEFINE v_respuesta_servicio INTEGER,
       v_estado_servicio INTEGER

   CALL fn_crea_servicio() RETURNING v_estado_servicio
    
   DISPLAY "INICIA WS CANCELACION SOLICITUDES PORTABILIDAD ",CURRENT YEAR TO SECOND

   CALL com.WebServiceEngine.Start()

   WHILE TRUE
      # Procesa las peticiones
      LET v_respuesta_servicio = com.WebServiceEngine.ProcessServices(-1)

      CASE v_respuesta_servicio
         WHEN 0
            DISPLAY "Respuesta WS procesada.",CURRENT YEAR TO SECOND
            
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

   DISPLAY "Servicio detenido"
END MAIN

FUNCTION fn_crea_servicio()
DEFINE v_servicio  com.WebService,    # A WebService
       v_operacion com.WebOperation   # Operation of a WebService

   TRY
      #Create WebService object
      LET v_servicio = com.WebService.CreateWebService("CancelacionSolicitudPortabilidadService", "http://infonavit.portabilidad.cancelacion.com")

      #Create WebOperation object
      LET v_operacion = com.WebOperation.CreateDOCStyle("cancelacionSolicitudPortabilidad", "cancelacionSolicitudPortabilidad", mensajeEntradaCancelacion, mensajeSalidaCancelacion )

      #publish the operation, associating with the WebService object
      CALL v_servicio.publishOperation(v_operacion,"")

      #Register the webService 
      CALL com.WebServiceEngine.RegisterService(v_servicio)
     
      
   CATCH
      RETURN STATUS
   END TRY

   RETURN 0
END FUNCTION

FUNCTION cancelacionSolicitudPortabilidad()
   CALL fn_genera_diagnostico_cancelacion(mensajeEntradaCancelacion.mensajeEntradaCancelacion.noCaso)
   RETURNING mensajeSalidaCancelacion.mensajeSalidaCancelacion.nss,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.noCaso,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.apPaterno,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.apMaterno,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.nombre,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.estatusAclaracion,
             mensajeSalidaCancelacion.mensajeSalidaCancelacion.diagnostico
END FUNCTION