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

GLOBALS "HPSWS01.inc"

PRIVATE DEFINE v_error              SMALLINT


# variables de origienacion
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

# variables de mandatos (hps)

PRIVATE DEFINE v_ind_alta_servicios             SMALLINT
PRIVATE DEFINE v_cve_mandato_predial            CHAR(18)
PRIVATE DEFINE v_f_ini_predial                  DATE
PRIVATE DEFINE v_f_fin_predial                  DATE
PRIVATE DEFINE v_mto_fondo_predial              DECIMAL(15,2)
PRIVATE DEFINE v_f_primer_pago_predial          DATE
PRIVATE DEFINE v_mto_primer_pago_predial        DECIMAL(15,2)
PRIVATE DEFINE v_referencia_predial             CHAR(40)

PRIVATE DEFINE v_cve_mandato_conservacion       CHAR(18)
PRIVATE DEFINE v_f_ini_conservacion             DATE
PRIVATE DEFINE v_f_fin_conservacion             DATE
PRIVATE DEFINE v_mto_fondo_conservacion         DECIMAL(15,2)
PRIVATE DEFINE v_f_primer_pago_conservacion     DATE
PRIVATE DEFINE v_mto_primer_pago_conservacion   DECIMAL(15,2)
PRIVATE DEFINE v_referencia_conservacion        CHAR(40)

PRIVATE DEFINE v_qryTxt             STRING
 
#variables de salida
PRIVATE DEFINE v_codResp            CHAR(4)
PRIVATE DEFINE v_descResp           VARCHAR(140)



FUNCTION fn_informar_credito_ejercido_servicios()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss                        =   ns1informarCreditoEjercido.request.nss
   LET v_tipo_credito               =   ns1informarCreditoEjercido.request.tipoCredito
   LET v_numero_credito             =   ns1informarCreditoEjercido.request.numeroCredito
   LET v_fecha_otorgamiento         =   ns1informarCreditoEjercido.request.fechaOtorgamiento
   LET v_tipo_descuento             =   ns1informarCreditoEjercido.request.tipoDescuento
   LET v_valor_descuento            =   ns1informarCreditoEjercido.request.valorDescuento
   LET v_monto_liquida              =   ns1informarCreditoEjercido.request.montoLiquida
   LET v_fecha_proceso              =   ns1informarCreditoEjercido.request.fechaProceso
   LET v_nrp                        =   ns1informarCreditoEjercido.request.nrp
   LET v_tipo_operacion             =   ns1informarCreditoEjercido.request.tipoOperacion
   LET v_ind_alta_servicios         =   ns1informarCreditoEjercido.request.identificadorAltaMandato
   LET v_cve_mandato_predial        =   ns1informarCreditoEjercido.request.identificadorAltaPredial
   LET v_f_ini_predial              =   ns1informarCreditoEjercido.request.fechaInicioPredial
   LET v_f_fin_predial              =   ns1informarCreditoEjercido.request.fechaFinPredial
   LET v_mto_fondo_predial          =   ns1informarCreditoEjercido.request.montoFondoPredial
   LET v_f_primer_pago_predial      =   ns1informarCreditoEjercido.request.fechaPrimerPagoPredial
   LET v_mto_primer_pago_predial    =   ns1informarCreditoEjercido.request.montoPrimerPagoPredial
   LET v_referencia_predial         =   ns1informarCreditoEjercido.request.referenciaPagoPredial
   LET v_cve_mandato_conservacion        =   ns1informarCreditoEjercido.request.identificadorAltaConservacion
   LET v_f_ini_conservacion              =   ns1informarCreditoEjercido.request.fechaInicioConservacion
   LET v_f_fin_conservacion              =   ns1informarCreditoEjercido.request.fechaFinConservacion
   LET v_mto_fondo_conservacion          =   ns1informarCreditoEjercido.request.montoFondoConservacion
   LET v_f_primer_pago_conservacion      =   ns1informarCreditoEjercido.request.fechaPrimerPagoConservacion
   LET v_mto_primer_pago_conservacion    =   ns1informarCreditoEjercido.request.montoPrimerPagoConservacion
   LET v_referencia_conservacion         =   ns1informarCreditoEjercido.request.referenciaPagoConservacion


   --LET v_qryTxt = "EXECUTE FUNCTION fn_credito_ejercido(?,?,?,?,?,?,?,?,?,?)"
   LET v_qryTxt = "EXECUTE FUNCTION sp_hps_registra_servicio(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

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
                              v_tipo_operacion                  ,
                              v_ind_alta_servicios              ,
                              v_cve_mandato_predial             ,
                              v_f_ini_predial                   ,
                              v_f_fin_predial                   ,
                              v_mto_fondo_predial               ,
                              v_f_primer_pago_predial           ,
                              v_mto_primer_pago_predial         ,
                              v_referencia_predial              ,
                              v_cve_mandato_conservacion        ,
                              v_f_ini_conservacion              ,
                              v_f_fin_conservacion              ,
                              v_mto_fondo_conservacion          ,
                              v_f_primer_pago_conservacion      ,
                              v_mto_primer_pago_conservacion    ,
                              v_referencia_conservacion     

             INTO v_error, v_nss, v_codResp, v_descResp, v_numero_credito, v_tipo_credito 


   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.nss               = v_nss
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.codigoRespuesta   = v_codResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.descripcion       = v_descResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.numeroCredito     = v_numero_credito
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.tipoCredito       = v_tipo_credito
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION
