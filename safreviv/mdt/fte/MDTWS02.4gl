######################################################################
#Modulo            =>MDT                                             #
#Programa          =>MDTWS02.4gl                                     #
#Objetivo          =>Programa que contiene el webServices que        #
#                    expone el servicio de validacion de instruccion #
#                    de mandato                                      #
#Fecha inicio      =>05 MARZO 2012                                   #
######################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

GLOBALS "MDTWS02.inc"

FUNCTION executaServidor()
    DEFINE servicio INTEGER
    DEFINE respuesta INTEGER

    CALL CreateValidaInstruccionMandatosServiceService() RETURNING servicio

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
# Service: ValidaInstruccionMandatosServiceService
# Port:    ValidaInstruccionMandatosService
#-------------------------------------------------------------------------------
#
# FUNCTION CreateValidaInstruccionMandatosServiceService
#   RETURNING soapstatus
#
FUNCTION CreateValidaInstruccionMandatosServiceService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ValidaInstruccionMandatosService","http://mandatos.safre.efp.com")


    #
    # Operation: valida_instruccion_mdt_can
    #

    # Publish Operation : valida_instruccion_mdt_can
    LET operation = com.WebOperation.CreateDOCStyle("valida_instruccion_mdt_can","valida_instruccion_mdt_can",ns1valida_instruccion_mdt_can,ns1valida_instruccion_mdt_canResponse)
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

FUNCTION valida_instruccion_mdt_can()
   CALL fn_valida_instruccion_mdt_can()
END FUNCTION