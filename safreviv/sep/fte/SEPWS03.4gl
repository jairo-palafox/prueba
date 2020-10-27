--==============================================================================
################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPWS03                                                       #
#Objetivo     => Consulta de marcas de separación de cuentas                   #
#Fecha inicio => 28 de Junio de 2016                                           #
################################################################################
IMPORT com

GLOBALS "SEPWS03.inc"
DATABASE "safre_viv"

MAIN
DEFINE v_respuesta_servicio INTEGER,
       v_estado_servicio INTEGER

   CALL fn_crea_servicio() RETURNING v_estado_servicio
    
   DISPLAY "INICIA WS CONSULTA DE MARCAS DE SEPARACIÓN DE CUENTAS ",CURRENT YEAR TO SECOND

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
      LET v_servicio = com.WebService.CreateWebService("ConsultaMarcaSeparacionService", "http://infonavit.separacion.consultamarca.com")
      CALL v_servicio.setFeature("Soap1.1","TRUE")
      #Create WebOperation object                                                                                                         
      LET v_operacion = com.WebOperation.CreateDOCStyle("consultaMarcaSeparacion", "consultaMarcaSeparacion", mensajeEntradaConsulta, mensajeSalidaConsulta )

      #publish the operation, associating with the WebService object
      CALL v_servicio.publishOperation(v_operacion,"consultaMarcaSeparacion")

      #Register the webService 
      CALL com.WebServiceEngine.RegisterService(v_servicio)
     
   CATCH
      RETURN STATUS
   END TRY

   RETURN 0
END FUNCTION

FUNCTION consultaMarcaSeparacion()
   CALL fn_consulta_marca_sep(mensajeEntradaConsulta.entradaConsulta.nss)   
   RETURNING mensajeSalidaConsulta.salidaConsulta.nss,
             mensajeSalidaConsulta.salidaConsulta.diagMarcaActiva,
             mensajeSalidaConsulta.salidaConsulta.codAforeActiva,
             mensajeSalidaConsulta.salidaConsulta.descAforeActiva,
             mensajeSalidaConsulta.salidaConsulta.diagMarcaHistorica,
             mensajeSalidaConsulta.salidaConsulta.codAforeHistorica,
             mensajeSalidaConsulta.salidaConsulta.descAforeHistorica,
             mensajeSalidaConsulta.salidaConsulta.resultadoOperacion
END FUNCTION

