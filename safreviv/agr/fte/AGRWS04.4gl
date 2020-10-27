####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS04.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    solicitud de marca                            #
#Fecha inicio      =>04 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AGRWS03.inc"

PRIVATE DEFINE v_error           SMALLINT
PRIVATE DEFINE v_nss             CHAR(11)
PRIVATE DEFINE v_numero_credito  CHAR (10)
PRIVATE DEFINE v_f_vigencia      DATE
PRIVATE DEFINE v_qryTxt          STRING
PRIVATE DEFINE v_codResp         CHAR(4)
PRIVATE DEFINE v_descResp        VARCHAR(140)
PRIVATE DEFINE v_diasReintento   SMALLINT

FUNCTION fn_solicita_marca()
   DEFINE v_mes     CHAR(2)
   DEFINE v_dia     CHAR(2)
   DEFINE v_año     CHAR(4)
   DEFINE v_c_fecha CHAR(8)

   LET v_c_fecha = ns2request.fechaVigencia   -- Se pasa a char el string yyyymmdd
   LET v_año = v_c_fecha[1,4]
   LET v_mes = v_c_fecha[5,6]
   LET v_dia = v_c_fecha[7,8]

   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss            = ns2request.nss
   LET v_numero_credito = ns2request.numeroCredito
   LET v_f_vigencia     = v_mes || v_dia || v_año
   LET v_f_vigencia = v_f_vigencia USING "mm-dd-yyyy"
   --LET v_f_vigencia     = ns2request.fechaVigencia

   LET v_qryTxt = "EXECUTE FUNCTION fn_marca_tramite(?,?,?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss, v_numero_credito, v_f_vigencia
                         INTO v_error, v_nss, v_codResp, v_descResp, v_diasReintento

   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns2solicitaMarcaReturn.nss             = v_nss
   LET ns2solicitaMarcaReturn.codigoRespuesta = v_codResp
   LET ns2solicitaMarcaReturn.descripcion     = v_descResp
   LET ns2solicitaMarcaReturn.diasReintento   = v_diasReintento
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION