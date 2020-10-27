####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS06.4gl                                   #
#Objetivo          =>Programa que contiene el webServices que      #
#                    expone el servicio de consulta del detalle del#
#                    saldo 9297                                    #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "CTAWS06.inc"

FUNCTION executaServidor()
    DEFINE servicio INTEGER
    DEFINE respuesta INTEGER

    CALL fn_prepara_consulta()
    
    CALL CreateconsultaDetalle9297Service() RETURNING servicio

    CALL com.WebServiceEngine.Start()

    DISPLAY("The server is listening.")

   WHILE TRUE
      #
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      #
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            --DISPLAY "Request processed." 
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM   # The Application server has closed the connection
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF     
   END WHILE
END FUNCTION

#-------------------------------------------------------------------------------
# Service: ConsultaDetalles9297Service
# Port:    ConsultaDetalles9297
#-------------------------------------------------------------------------------
#
# FUNCTION CreateconsultaDetalle9297Service
#   RETURNING soapstatus
#
FUNCTION CreateconsultaDetalle9297Service()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaDetalles9297Service","http://service.infonavit.org.mx")


    #
    # Operation: consultaDetalle9297
    #

    # Publish Operation : consultaDetalle9297
    LET operation = com.WebOperation.CreateDOCStyle("consultaDetalle9297","consultaDetalle9297",consultaDetalle9297,consultaDetalle9297Response)
    CALL service.publishOperation(operation,"consultaDetalle9297")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION consultaDetalle9297()
   CALL fn_consultaDetalle9297()
END FUNCTION