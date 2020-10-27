#######################################################################
#Modulo            =>MDT                                              #
#Programa          =>MDTWS03.4gl                                      #
#Objetivo          =>Programa que contiene la implementacion del      #
#                    webServices que expone el servicio de validacion #
#                    de instruccion de mandato                        #
#Fecha inicio      =>05 MARZO 2012                                    #
#######################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

DATABASE safre_viv

GLOBALS "MDTWS03.inc"
GLOBALS "MDTWS02.inc"

FUNCTION fn_valida_instruccion_mdt_can()
   DEFINE v_solicitud solicitaMandato
   DEFINE v_solicitud_request tns2ValidaInstruccionRequest
   DEFINE v_solicitud_response tns2ValidaInstruccionResponse
   DEFINE v_diagnostico LIKE mdt_solicitud_mandato.diagnostico

   #Se obtiene el objeto con los parametros de entrada
   LET v_solicitud_request.* = ns1valida_instruccion_mdt_can.validaInstruccionRequest.*

   #se llenan los valores fijos del response
   LET v_solicitud_response.id_origen 			=	v_solicitud_request.id_origen
   LET v_solicitud_response.nss 				=	v_solicitud_request.nss
   LET v_solicitud_response.id_credito 			=	v_solicitud_request.id_credito
   --LET v_solicitud_response.id_mandato 		=	v_solicitud_request.id_mandato
   LET v_solicitud_response.tpo_descuento 		=	v_solicitud_request.tpo_descuento
   LET v_solicitud_response.valor_descuento     =	v_solicitud_request.valor_descuento
   LET v_solicitud_response.f_canales 			=	v_solicitud_request.f_canales
   LET v_solicitud_response.f_inicio_mandato 	=	v_solicitud_request.f_inicio_mandato
   LET v_solicitud_response.f_culmina_mandato   =	v_solicitud_request.f_culmina_mandato
   LET v_solicitud_response.referencia 			=	v_solicitud_request.referencia
   LET v_solicitud_response.id_canales 			=	v_solicitud_request.id_canales
   LET v_solicitud_response.tipo_operacion 		=	v_solicitud_request.tipo_operacion
   LET v_solicitud_response.cve_mandato 		=	v_solicitud_request.cve_mandato

   #Se llenan los valores que se enviaran a la funcion que ejecuta las validaciones
   LET v_solicitud.id_origen 		       =	v_solicitud_request.id_origen
   LET v_solicitud.nss 					   =	v_solicitud_request.nss
   LET v_solicitud.id_credito 			   =	v_solicitud_request.id_credito
   --LET v_solicitud.id_mandato 		   =	v_solicitud_request.id_mandato
   LET v_solicitud.cve_mandato 		       =	v_solicitud_request.cve_mandato
   LET v_solicitud.tpo_descuento_mandato   =	v_solicitud_request.tpo_descuento
   LET v_solicitud.valor_descuento_mandato =	v_solicitud_request.valor_descuento
   LET v_solicitud.f_canales 			   =	v_solicitud_request.f_canales
   LET v_solicitud.f_inicio_mandato 	   =	v_solicitud_request.f_inicio_mandato
   LET v_solicitud.f_culmina_mandato       =	v_solicitud_request.f_culmina_mandato
   LET v_solicitud.referencia 			   =	v_solicitud_request.referencia
   LET v_solicitud.id_canales 			   =	v_solicitud_request.id_canales
   LET v_solicitud.tipo_operacion 		   =	v_solicitud_request.tipo_operacion
   
   
   #Instruccion para ejecutar la funcion que valida la instruccion de mandato
   PREPARE exe_fn_valida_instruccion_mdt_can FROM "EXECUTE FUNCTION safre_viv:fn_valida_instruccion_mdt_can(?,?,?,?,?,?,?,?,?,?,?,?)"
   EXECUTE exe_fn_valida_instruccion_mdt_can USING v_solicitud.* INTO v_diagnostico

   IF v_diagnostico <> "000" THEN
      LET v_solicitud_response.resultado_operacion = "02"
   ELSE
      LET v_solicitud_response.resultado_operacion = "01"
   END IF

   LET v_solicitud_response.diagnostico = v_diagnostico

   #Se asignan los valores de retorno del WS
   LET ns1valida_instruccion_mdt_canResponse.valida_instruccion_mdt_canReturn.* = v_solicitud_response.*
END FUNCTION