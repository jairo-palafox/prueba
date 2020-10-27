####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS17.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta de fallecido                         #
#Fecha inicio      =>16 de mayo de 2018                            #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AFIWS16.inc"

PRIVATE DEFINE v_error     SMALLINT
PRIVATE DEFINE v_nss       CHAR(11)
PRIVATE DEFINE v_codResp   CHAR(2)
PRIVATE DEFINE v_descResp  CHAR(25)
PRIVATE DEFINE v_qryTxt   STRING

FUNCTION fn_consulta_fallecido()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss = ns2request.nss

   LET v_qryTxt = "EXECUTE FUNCTION fn_verifica_fallecido(?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss
                         INTO v_codResp, v_descResp

   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns2consultaFallecidoReturn.codigoRespuesta = v_codResp
   LET ns2consultaFallecidoReturn.descRespuesta   = v_descResp

   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION
