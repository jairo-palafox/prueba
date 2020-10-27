####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AGRWS08.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    solicitud saldo cambiavit                     #
#Fecha inicio      =>18 OCTUBRE 2018                               #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AGRWS07.inc"

PRIVATE DEFINE v_error              SMALLINT
PRIVATE DEFINE v_qryTxt             STRING
PRIVATE DEFINE v_nss                CHAR(11)
PRIVATE DEFINE v_numero_credito     DECIMAL(10,0)
PRIVATE DEFINE v_tipo_solicitud     SMALLINT
PRIVATE DEFINE v_numero_caso        CHAR(10)

PRIVATE DEFINE v_result_operacion   CHAR(2)
PRIVATE DEFINE v_sdo_garantia       DECIMAL(15,2)
PRIVATE DEFINE v_motivo             CHAR(2)

FUNCTION fn_solicitud_saldo_cambiavit()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss                = ns2request.nss
   LET v_numero_credito     = ns2request.numeroCredito
   LET v_tipo_solicitud     = ns2request.tipoSolicitud
   LET v_numero_caso        = ns2request.numeroCaso

   LET v_qryTxt = "EXECUTE FUNCTION fn_saldo_cambiavit(?,?,?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss, v_numero_credito, v_tipo_solicitud
                         INTO v_error,
                              v_result_operacion,
                              v_sdo_garantia,
                              v_motivo

   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns2Return.resultOperacion = v_result_operacion
   LET ns2Return.sdoGarantia     = v_sdo_garantia
   LET ns2Return.motivo          = v_motivo
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion

END FUNCTION
