####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSWS01.4gl                                   #
#Objetivo          =>Programa que expone el servicio del           #
#                    bus de tramites                               #
#Fecha inicio      =>08 NOVIEMBRE 2013                             #
####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml


GLOBALS "BUSWS001.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 

   #Se ejecuta la funcion que prepara las consultas a ejecutar
   #CALL fn_prepara_consulta()
   
   CALL CreateWSBusSafreService() RETURNING servicio

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
# Service: BusTramites
# Port:    busTramitesPort
#-------------------------------------------------------------------------------
#
# FUNCTION Createuntitled-1Service
#   RETURNING soapstatus
#
FUNCTION CreateWSBusSafreService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("BusSafrevivService","http://www.infonavit.safrebus.com.mx/BusTramites/")


    #
    # Operation: notificarTramite
    #

    # Publish Operation : notificarTramite
    LET operation = com.WebOperation.CreateDOCStyle("notificarTramite","notificarTramite",ns2notificarTramiteRequest,ns2notificarTramiteResponse)
    CALL service.publishOperation(operation,"http://www.infonavit.safrebus.com.mx/BusTramites/notificarTramite/")

    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION notificarTramite()
   DISPLAY "notificarTramite"
   CALL fn_notificar_tramite()
END FUNCTION
