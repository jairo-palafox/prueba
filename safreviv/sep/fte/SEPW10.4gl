####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW10                                        #
#Objetivo          =>Interface de safre que manda a llamar el WS   #
#                    de separacion de cuentas                      #
#Fecha inicio      =>18 MAYO 2012                                  #
####################################################################

DATABASE safre_viv

GLOBALS "SEPW10.inc"    #Archivo de variables globales del lanzador
GLOBALS "SEPW12.inc"    #Archivo de variables globales del WS

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

FUNCTION fn_aviso_reclamo(p_solicitud)
   DEFINE p_solicitud            avisoReclamo
   DEFINE v_respuesta_reclamo    respuestaAvisoReclamo
   DEFINE v_solicita             tns2ReclamoAmortizacionIndebidaRequest
   DEFINE v_respuesta            tns2ReclamoAmortizacionIndebidaResponse

   CALL fn_configura_ws()

   #Se llenan los parametros que se enviaran al WS
   LET v_solicita.nssAsociado                = p_solicitud.nssAsociado
   LET v_solicita.numeroCaso                 = p_solicitud.numeroCaso
   LET v_solicita.impAmortizacionIndebida    = p_solicitud.impAmortizacionIndebida

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_aviso_reclamo_amortizacion( v_url_servidor  CLIPPED, 
                                       v_usuario         CLIPPED, 
                                       v_password        CLIPPED, 
                                       v_intentos, 
                                       v_solicita.*) 
   RETURNING v_respuesta.*

   # se asignan los valores de respuesta
   LET v_respuesta_reclamo.impAmortizacionIndebida    = v_respuesta.impAmortizacionIndebida
   LET v_respuesta_reclamo.nssAsociado                = v_respuesta.nssAsociado
   LET v_respuesta_reclamo.numeroCaso                 = v_respuesta.numeroCaso
   LET v_respuesta_reclamo.resultOperacion            = v_respuesta.resultOperacion
   LET v_respuesta_reclamo.diagRechazo                = v_respuesta.diagRechazo

   RETURN v_respuesta_reclamo.*
   
END FUNCTION

PRIVATE FUNCTION fn_configura_ws()
   DEFINE v_consulta    STRING

   #La clave '4' del catalogo de clientes de webServices corresponde a la asignacion de caso para separacion
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING WS_RECLAMO INTO  v_url_servidor,
                              v_usuario,
                              v_password,
                              v_intentos
END FUNCTION