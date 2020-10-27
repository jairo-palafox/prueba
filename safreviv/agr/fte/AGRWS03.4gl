####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS03.4gl                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    asi como la publicacion del webServices de    #
#                    solicitud de marca                            #
#Fecha inicio      =>04 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AGRWS03.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateSolicitaMarcaServices() RETURNING servicio

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

FUNCTION CreateSolicitaMarcaServices()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Crea Web Service
    LET service = com.WebService.CreateWebService("SolicitaMarcaServices","http://services.safre.efp.com")
    CALL service.setComment("Servicio para solicitar la marca de crédito en el sistema SAFRE-SACI")

    # Publica Operacion : solicitaMarca
    LET operation = com.WebOperation.CreateDOCStyle("solicitaMarca","solicitaMarca",ns2request,ns2solicitaMarcaReturn)
    CALL service.publishOperation(operation,"")

    # Registra Servicio
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION solicitaMarca()
   CALL fn_solicita_marca()
END FUNCTION