####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS01.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    homonimia                                     #
#Fecha inicio      =>05 DICIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML


GLOBALS "AFIWS01.inc"

PRIVATE DEFINE v_nss                CHAR(11)
PRIVATE DEFINE v_codigoRespuesta    CHAR(1)
PRIVATE DEFINE v_fecha_tramite      DATE 

PRIVATE DEFINE v_consulta           STRING 


FUNCTION fn_consultar_homonimia()

   DEFINE v_marca                   SMALLINT
   DATABASE safre_viv

   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss                = ns1consultarHomonimia.consultaHomonimiaRequest.nss
   LET v_marca = 280
   
   LET v_consulta = "EXECUTE FUNCTION fn_verifica_marca(?,?)"
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING v_nss, v_marca INTO v_nss, v_codigoRespuesta, v_fecha_tramite
   --CLOSE exe_consulta
   --FREE exe_consulta
   --CLOSE DATABASE  
   
   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns1consultarHomonimiaResponse.consultarHomonimiaReturn.nss               = v_nss
   LET ns1consultarHomonimiaResponse.consultarHomonimiaReturn.codigoRespuesta   = v_codigoRespuesta
   LET ns1consultarHomonimiaResponse.consultarHomonimiaReturn.fechaTramite       = v_fecha_tramite USING 'mmddyyyy'
   
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION
