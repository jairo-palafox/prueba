####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS06.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    credito ejercido                              #
#Fecha inicio      =>18 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AGRWS05.inc"

PRIVATE DEFINE v_error              SMALLINT
PRIVATE DEFINE v_nss                CHAR(11) 
PRIVATE DEFINE v_tipo_credito       CHAR(3) 
PRIVATE DEFINE v_numero_credito     DECIMAL(10,0) 
PRIVATE DEFINE v_fecha_otorgamiento DATE 
PRIVATE DEFINE v_tipo_descuento     SMALLINT 
PRIVATE DEFINE v_valor_descuento    DECIMAL(8,0) 
PRIVATE DEFINE v_monto_liquida      DECIMAL(15,2)
PRIVATE DEFINE v_fecha_proceso      DATE 
PRIVATE DEFINE v_nrp                CHAR(11) 
PRIVATE DEFINE v_tipo_operacion     CHAR(3)
PRIVATE DEFINE v_qryTxt             STRING
PRIVATE DEFINE v_codResp            CHAR(4)
PRIVATE DEFINE v_descResp           VARCHAR(140)
PRIVATE DEFINE v_fecha_temp         STRING

FUNCTION fn_informar_credito_ejercido()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss                = ns1informarCreditoEjercido.request.nss
   LET v_tipo_credito       = ns1informarCreditoEjercido.request.tipoCredito
   LET v_numero_credito     = ns1informarCreditoEjercido.request.numeroCredito
   --LET v_fecha_otorgamiento = ns1informarCreditoEjercido.request.fechaOtorgamiento
   LET v_tipo_descuento     = ns1informarCreditoEjercido.request.tipoDescuento
   LET v_valor_descuento    = ns1informarCreditoEjercido.request.valorDescuento
   LET v_monto_liquida      = ns1informarCreditoEjercido.request.montoLiquida
   --LET v_fecha_proceso      = ns1informarCreditoEjercido.request.fechaProceso
   LET v_nrp                = ns1informarCreditoEjercido.request.nrp
   LET v_tipo_operacion     = ns1informarCreditoEjercido.request.tipoOperacion

   LET v_fecha_temp = ns1informarCreditoEjercido.request.fechaOtorgamiento
   LET v_fecha_otorgamiento = MDY(v_fecha_temp.subString(5,6),v_fecha_temp.subString(7,8),v_fecha_temp.subString(1,4))

   --DISPLAY "v_fecha_otorgamiento: ", v_fecha_otorgamiento

   LET v_fecha_temp = ns1informarCreditoEjercido.request.fechaProceso
   LET v_fecha_proceso = MDY(v_fecha_temp.subString(5,6),v_fecha_temp.subString(7,8),v_fecha_temp.subString(1,4))
   --DISPLAY "v_fecha_proceso: ", v_fecha_proceso

   LET v_qryTxt = "EXECUTE FUNCTION fn_credito_ejercido(?,?,?,?,?,?,?,?,?,?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss               ,
                              v_tipo_credito      ,
                              v_numero_credito    ,
                              v_fecha_otorgamiento,
                              v_tipo_descuento    ,
                              v_valor_descuento   ,
                              v_monto_liquida     ,
                              v_fecha_proceso     ,
                              v_nrp               ,
                              v_tipo_operacion    
             INTO v_error, v_nss, v_codResp, v_descResp, v_numero_credito, v_tipo_credito 

   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.nss               = v_nss
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.codigoRespuesta   = v_codResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.descripcion       = v_descResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.numeroCredito     = v_numero_credito
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.tipoCredito       = v_tipo_credito
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION