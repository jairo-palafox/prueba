####################################################################
#Modulo            =>SRV                                           #
#Programa          =>SRVWS01.4gl                                   #
#Objetivo          =>Programa que contiene el webServices que      #
#                    expone el servicio de estado de cuenta        #                        #
#Fecha inicio      =>15 MARZO 2012                                 #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "SRVWS01.inc"

MAIN
   DEFINE servicio     INTEGER
   DEFINE respuesta    INTEGER 
   #Se ejecuta la funcion que prepara el query para obtener el saldo
   
   CALL CreateEdoCuentaService() RETURNING servicio

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
END MAIN



#-------------------------------------------------------------------------------
# Service: EdoCuentaService
# Port:    EdoCuentaService
#-------------------------------------------------------------------------------
#
# FUNCTION CreateEdoCuentaService
#   RETURNING soapstatus
#
FUNCTION CreateEdoCuentaService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("EdoCuentaService","http://edoCuenta.safre.efp.com")


    #
    # Operation: buscarEdoCuenta
    #

    # Publish Operation : buscarEdoCuenta
    LET operation = com.WebOperation.CreateDOCStyle("buscarEdoCuenta","buscarEdoCuenta",ns2buscarEdoCuentaRequest,ns2buscarEdoCuentaReturn)
    CALL service.publishOperation(operation,"buscarEdoCuenta")


    #
    # Operation: obtenerEdoCuenta
    #

    # Publish Operation : obtenerEdoCuenta
    LET operation = com.WebOperation.CreateDOCStyle("obtenerEdoCuenta","obtenerEdoCuenta",ns2edoCuentaRequest,ns2obtenerEdoCuentaReturn)
    CALL service.publishOperation(operation,"obtenerEdoCuenta")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION buscarEdoCuenta()
   CALL fn_buscarEdoCuenta()
END FUNCTION

FUNCTION obtenerEdoCuenta()
   CALL fn_obtenerEdoCuenta()
END FUNCTION