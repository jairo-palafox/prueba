####################################################################
#Modulo            =>SEP                                           #
#Programa          =>"SEPWS01.4gl                                  #
#Objetivo          =>Programa que contiene el webServices que      #
#                    expone el servicio de Notificacion de contacto#
#Fecha inicio      =>15 MAYO 2012                                  #
####################################################################


IMPORT FGL WSHelper
IMPORT com
IMPORT XML

GLOBALS "SEPWS01.inc"

MAIN
   DEFINE servicio INTEGER
    DEFINE respuesta INTEGER
   #Se ejecuta la funcion que prepara el query para obtener el saldo
   CALL fn_prepara_consultas()
   
    CALL CreateRecibeNotificacionContactoService() RETURNING servicio

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
# Service: RecibeNotificacionContactoService
# Port:    RecibeNotificacionContacto
#-------------------------------------------------------------------------------
#
# FUNCTION CreateRecibeNotificacionContactoService
#   RETURNING soapstatus
#
FUNCTION CreateRecibeNotificacionContactoService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("RecibeNotificacionContactoService","http://separacion.safre.efp.com")


    #
    # Operation: recibeNotificacionContacto
    #

    # Publish Operation : recibeNotificacionContacto
    LET operation = com.WebOperation.CreateDOCStyle("recibeNotificacionContacto","recibeNotificacionContacto",ns1recibeNotificacionContacto,ns1recibeNotificacionContactoResponse)
    CALL service.publishOperation(operation,"recibeNotificacionContacto")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION recibeNotificacionContacto()
   CALL fn_recibe_notificacion_contacto()
END FUNCTION

