####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS01.4gl                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    asi como la publicacion del webServices de    #
#                    homonimia                                     #
#Fecha inicio      =>05 DICIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AFIWS01.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateConsultaHomonimiaServices() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            --DISPLAY "Request processed."
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM
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
# Service: ConsultaHomonimiaServicesService
# Port:    ConsultaHomonimiaServices
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConsultaHomonimiaServices
#   RETURNING soapstatus
#
FUNCTION CreateConsultaHomonimiaServices()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaHomonimiaServices","http://services.safre.efp.com")


    #
    # Operation: consultarHomonimia
    #

    # Publish Operation : consultarHomonimia
    LET operation = com.WebOperation.CreateDOCStyle("consultarHomonimia","consultarHomonimia",ns1consultarHomonimia,ns1consultarHomonimiaResponse)
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

FUNCTION consultarHomonimia()
   CALL fn_consultar_homonimia()
END FUNCTION