####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta de marca                             #
#Fecha inicio      =>04 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv


GLOBALS "AGRWS01.inc"

PRIVATE DEFINE v_error     SMALLINT
PRIVATE DEFINE v_nss       CHAR(11)
PRIVATE DEFINE v_codResp   CHAR(4)
PRIVATE DEFINE v_descResp  VARCHAR(140)
PRIVATE DEFINE v_saldo92   DECIMAL(12,2)
PRIVATE DEFINE v_saldo97   DECIMAL(12,2)
PRIVATE DEFINE v_qryTxt   STRING

FUNCTION fn_consulta_marca()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss = ns2request.nss

   LET v_qryTxt = "EXECUTE FUNCTION fn_consulta_marca_ws(?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss
                         INTO v_error, v_nss, v_codResp, v_descResp, v_saldo92, v_saldo97

   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns2consultaMarcaReturn.nss             = v_nss
   LET ns2consultaMarcaReturn.codigoRespuesta = v_codResp
   LET ns2consultaMarcaReturn.descripcion     = v_descResp
   LET ns2consultaMarcaReturn.saldoVivienda92 = v_saldo92
   LET ns2consultaMarcaReturn.saldoVivienda97 = v_saldo97

   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION