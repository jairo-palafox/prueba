#####################################################################
#Modulo            => AFI                                           #
#Programa          => AFIWS08                                       #
#Objetivo          => Servidor para la consulta de notificaciones   #
#                     registradas por SMS o via correo electrónico  #
#                     por derechohabiente                           #
#Fecha inicio      => 10 Noviembre 2015                             #
#####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AFIWS08.inc"
MAIN
    DEFINE servicio       INTEGER
    DEFINE respuesta      INTEGER

   CALL consultaNotificaciones() RETURNING servicio

   DISPLAY "SERVICIO : " , servicio
   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            DISPLAY "Request processed."
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

     IF int_flag <> 0 THEN
        LET int_flag = 0
        EXIT WHILE
     END IF
   END WHILE
END MAIN


FUNCTION consultaNotificaciones()
  DEFINE service          com.WebService
  DEFINE operation        com.WebOperation

  TRY
    # Create Web Service
    --LET service = com.WebService.CreateWebService("ActDatosMaestros","http://services.safre.efp.com")
    LET service = com.WebService.CreateWebService("ConsultaNotificaciones","http://services.safre.efp.com")


    #
    # Operation: Consulta Afore
    #

    # Publish Operation : Consulta Afore
    LET operation = com.WebOperation.CreateDOCStyle("consulta_notificaciones","consulta_notificaciones",request,response)
    
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

-- Fcunción que tiene el negocio
FUNCTION consulta_notificaciones()
   DISPLAY "Se ejecuta la funcion"
   CALL fn_consulta_notificaciones()
END FUNCTION

