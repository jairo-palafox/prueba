####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW01                                        #
#Objetivo          =>Interface de safre que manda a llamar el WS   #
#                    de separacion de cuentas                      #
#Fecha inicio      =>17 MAYO 2012                                  #
####################################################################

DATABASE safre_viv

GLOBALS "SEPW01.inc"    #Archivo de variables globales del lanzador
GLOBALS "SEPW03.inc"    #Archivo de variables globales del WS

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

FUNCTION fn_asigna_numero_caso(p_solicitud)
   DEFINE p_solicitud         asignaNumeroCaso
   DEFINE v_respuesta_caso    respuestaNumeroCaso
   DEFINE v_solicita          tns2AsignaCasoRequest
   DEFINE v_respuesta         tns2AsignaCasoResponse

   CALL fn_configura_ws()

   #Se llenan los parametros que se enviaran al WS
   LET v_solicita.nssAsociado         = p_solicitud.nssAsociado
   LET v_solicita.nssInvadido         = p_solicitud.nssInvadido

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_asigna_caso( v_url_servidor  CLIPPED, 
                           v_usuario         CLIPPED, 
                           v_password        CLIPPED, 
                           v_intentos, 
                           v_solicita.*) 
   RETURNING v_respuesta.*

   # se asignan los valores de respuesta
   LET v_respuesta_caso.nssAsociado            = v_respuesta.nssAsociado
   LET v_respuesta_caso.nssInvadido            = v_respuesta.nssInvadido
   LET v_respuesta_caso.numeroCaso             = v_respuesta.numeroCaso
   LET v_respuesta_caso.indExistencia          = v_respuesta.indExistencia
   LET v_respuesta_caso.resultOperacion        = v_respuesta.resultOperacion
   LET v_respuesta_caso.diagRechazo            = v_respuesta.diagRechazo

   RETURN v_respuesta_caso.*
   
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
   EXECUTE exe_consulta USING WS_NUM_CASO INTO  v_url_servidor,
                              v_usuario,
                              v_password,
                              v_intentos
END FUNCTION