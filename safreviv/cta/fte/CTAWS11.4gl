####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS11.4gl                                   #
#Objetivo          =>Programa que contiene el webServices que      #
#                    expone el servicio de consulta de saldo       # 
#                    anterior                                      #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "CTAWS11.inc"

FUNCTION executaServidor()
    DEFINE servicio INTEGER
    DEFINE respuesta INTEGER

    CALL fn_prepara_consulta()
    
    CALL CreateconsultaSaldoAnteriorService() RETURNING servicio

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
            #DISPLAY "Request processed." 
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
# Service: ConsultaFondosService
# Port:    ConsultaFondos
#-------------------------------------------------------------------------------
#
# FUNCTION CreateconsultaSaldoAnteriorService
#   RETURNING soapstatus
#
FUNCTION CreateconsultaSaldoAnteriorService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaSaldoAnteriorService","http://service.infonavit.org.mx")


    #
    # Operation: consultaSaldoAnterior
    #

    # Publish Operation : consultaSaldoAnterior
    LET operation = com.WebOperation.CreateDOCStyle("consultaSaldoAnterior","consultaSaldoAnterior",consultaSaldoAnterior,consultaSaldoAnteriorResponse)
    CALL service.publishOperation(operation,"consultaSaldoAnterior")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION consultaSaldoAnterior()
   CALL fn_consultaSaldoAnterior()
END FUNCTION