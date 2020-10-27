--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/11/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS10                                                       #
#Objetivo     => Registra solicitud de portabilidad cedente                    #
#Fecha inicio => 10 Noviembre 2015                                             #
################################################################################
IMPORT com
DATABASE safre_viv

GLOBALS "PRTWS10.inc"

MAIN
DEFINE v_respuesta_servicio INTEGER,
       v_estado_servicio INTEGER

   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio() RETURNING v_estado_servicio
    
   DISPLAY "INICIA WS ACLARACIONES PORTABILIDAD ",CURRENT YEAR TO SECOND

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
      LET v_servicio = com.WebService.CreateWebService("DiagnosticoAclaracionPortabilidadService", "http://infonavit.portabilidad.aclaracion.diagnostico.com")

      #Create WebOperation object
      LET v_operacion = com.WebOperation.CreateRPCStyle("diagnosticoAclaracionPortabilidad", "diagnosticaAclaracionPortabilidad", mensajeEntradaAclaracion, mensajeSalidaAclaracion )

      #publish the operation, associating with the WebService object
      CALL v_servicio.publishOperation(v_operacion,"")

      #Register the webService 
      CALL com.WebServiceEngine.RegisterService(v_servicio)
     
      
   CATCH
      RETURN STATUS
   END TRY

   RETURN 0
END FUNCTION

FUNCTION diagnosticoAclaracionPortabilidad()

   CALL fn_determina_tipificacion(mensajeEntradaAclaracion.mensajeEntradaAclaracion.nss)
   RETURNING mensajeSalidaAclaracion.mensajeSalidaAclaracion.nss,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.apPaterno,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.apMaterno,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.nombre,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.estatusAclaracion,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.tipo,
             mensajeSalidaAclaracion.mensajeSalidaAclaracion.diagnostico

END FUNCTION