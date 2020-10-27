####################################################################
#Modulo            =>SRV                                           #
#Programa          =>SRVWS01.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices de estado de cuenta               #
#Fecha inicio      =>15 MARZO 2012                                 #
####################################################################
IMPORT os

GLOBALS "SRVWS01.inc"

FUNCTION fn_buscarEdoCuenta()
   #Se buscan los estados de cuenta que se generaron para un nss especifico
   LET ns2buscarEdoCuentaReturn.diagnostico = "01"
   LET ns2buscarEdoCuentaReturn.listaEdoCuenta = "Edo_Cuenta_41998328524"
   LET ns2buscarEdoCuentaReturn.nss = ns2buscarEdoCuentaRequest.nss
END FUNCTION

FUNCTION fn_obtenerEdoCuenta()
   DEFINE v_ruta_archivo     STRING
   #Se valida que exista el repositorio
   IF NOT(os.Path.exists("/ds/safreviv/srv/edo_cuenta"))THEN
      LET ns2obtenerEdoCuentaReturn.diagnostico = "02"
      LET ns2obtenerEdoCuentaReturn.nss = ""
      RETURN
   END IF
   LET v_ruta_archivo = "/ds/safreviv/srv/edo_cuenta/" || ns2edoCuentaRequest.nombreEdoCuenta
   IF NOT os.Path.exists(v_ruta_archivo) THEN
      LET ns2obtenerEdoCuentaReturn.diagnostico = "03"
      LET ns2obtenerEdoCuentaReturn.nss = ""
   ELSE 
      LET ns2obtenerEdoCuentaReturn.diagnostico = "01"
      LET ns2obtenerEdoCuentaReturn.nss = ns2edoCuentaRequest.nss

      LOCATE ns2obtenerEdoCuentaReturn.archivo IN MEMORY
      CALL ns2obtenerEdoCuentaReturn.archivo.readFile(v_ruta_archivo)
   END IF
   
   
   
END FUNCTION