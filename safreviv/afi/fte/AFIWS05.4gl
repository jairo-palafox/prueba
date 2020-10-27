#####################################################################
#Modulo            => AFI                                           #
#Programa          => AFIWS05                                       #
#Objetivo          => Servidor que consulta la afore a la cual esta #
#                     registrado un derechohabiente                 #
#Fecha inicio      => 09 Noviembre 2015                             #
#####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AFIWS05.inc"
MAIN
    DEFINE servicio       INTEGER
    DEFINE respuesta      INTEGER

   CALL consultaAfore() RETURNING servicio

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

     IF int_flag <> 0 THEN
        LET int_flag = 0
        EXIT WHILE
     END IF
   END WHILE
END MAIN


FUNCTION consultaAfore()
  DEFINE service          com.WebService
  DEFINE operation        com.WebOperation

  TRY
    # Create Web Service
    --LET service = com.WebService.CreateWebService("ActDatosMaestros","http://services.safre.efp.com")
    LET service = com.WebService.CreateWebService("ConsultaAfore","http://services.safre.efp.com")


    #
    # Operation: Consulta Afore
    #

    # Publish Operation : Consulta Afore
    LET operation = com.WebOperation.CreateDOCStyle("consulta_afore","consulta_afore",v_rec_in,resp_out)
    DISPLAY "NSS IN ", v_rec_in.nss_in
    --LET operation = com.WebOperation.CreateDOCStyle(
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
FUNCTION consulta_afore()
   CALL fn_consulta_afore()
END FUNCTION