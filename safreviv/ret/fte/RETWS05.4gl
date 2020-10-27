#-------------------------------------------------------------------------------
# File: RETWS05Service.4gl
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------

IMPORT FGL WSHelper
IMPORT com
IMPORT XML


GLOBALS "RETWS05.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 
   #Se ejecuta la funcion que prepara el query para obtener el saldo
   CALL fn_prepara_consulta()
   
   CALL CreateComprobantePagoService() RETURNING servicio

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
# Service: ComprobantePagoService
# Port:    ComprobantePago
#-------------------------------------------------------------------------------
#
# FUNCTION CreateComprobantePagoService
#   RETURNING soapstatus
#
FUNCTION CreateComprobantePagoService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ComprobantePagoService","http://services.safre.efp.com")


    #
    # Operation: comprobantePago
    #

    # Publish Operation : comprobantePago
    LET operation = com.WebOperation.CreateDOCStyle("comprobantePago","comprobantePago",ns2request,ns2comprobantePagoReturn)
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

FUNCTION comprobantePago()
   CALL fn_comprobantePago()
END FUNCTION