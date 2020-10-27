####################################################################
#Modulo            =>NOT                                           #
#Programa          =>NOTWS01.4gl                                   #
#Objetivo          =>Programa que expone el servicio de            #
#                    consulta de aclaratorio                       #
#Fecha inicio      =>01 JUNIO 2015                                 #
####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "NOTWS01.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 
   
   CALL CreateConsultaNotAclaratorioService() RETURNING servicio

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
END MAIN


#-------------------------------------------------------------------------------
# Service: ConsultaAclaratorioService
# Port:    ConsultaAclaratorioService
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConsultaAclaratorioService
#   RETURNING soapstatus
#
FUNCTION CreateConsultaNotAclaratorioService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaNotAclaratorioService","http://consultaAclaratorio.safre.efp.com")


    #
    # Operation: consultarAclaratorio
    #

    # Publish Operation : consultarAclaratorio
    LET operation = com.WebOperation.CreateDOCStyle("consultarAclaratorio","consultarAclaratorio",consultaRequest,consultaReturn)
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


FUNCTION consultarAclaratorio()
   CALL fn_consultar_aclaratorio()
END FUNCTION