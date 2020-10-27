####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW05                                        #
#Objetivo          =>Interface de safre que manda a llamar el WS   #
#                    de separacion de cuentas                      #
#Fecha inicio      =>18 MAYO 2012                                  #
####################################################################

DATABASE safre_viv

GLOBALS "SEPW05.inc"    #Archivo de variables globales del lanzador
GLOBALS "SEPW07.inc"    #Archivo de variables globales del WS

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

FUNCTION fn_envia_datos_expediente(p_solicitud)
   DEFINE p_solicitud         datosExpediente
   DEFINE v_respuesta_datos    respuestaDatosExpediente
   DEFINE v_solicita          tns2DatosExpedienteRequest
   DEFINE v_respuesta         tns2DatosExpedienteResponse

   CALL fn_configura_ws()

   #Se llenan los parametros que se enviaran al WS
   LET v_solicita.nssAsociado          = p_solicitud.nssAsociado
   LET v_solicita.nssInvadido          = p_solicitud.nssInvadido
   LET v_solicita.nombreReclamante     = p_solicitud.nombreReclamante
   LET v_solicita.numeroCaso           = p_solicitud.numeroCaso
   LET v_solicita.tipoFlujo            = p_solicitud.tipoFlujo
   LET v_solicita.folioProcesar        = p_solicitud.folioProcesar
   LET v_solicita.fCaptura             = p_solicitud.fCaptura
   LET v_solicita.fRecepcion           = p_solicitud.fRecepcion

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_recibe_datos_expediente( v_url_servidor  CLIPPED, 
                                    v_usuario         CLIPPED, 
                                    v_password        CLIPPED, 
                                    v_intentos, 
                                    v_solicita.*) 
   RETURNING v_respuesta.*

   # se asignan los valores de respuesta
   LET v_respuesta_datos.numeroCaso          = v_respuesta.numeroCaso
   LET v_respuesta_datos.resultOperacion     = v_respuesta.resultOperacion
   LET v_respuesta_datos.diagRechazo         = v_respuesta.diagRechazo

   RETURN v_respuesta_datos.*
   
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
   EXECUTE exe_consulta USING WS_EXPEDIENTE INTO  v_url_servidor,
                              v_usuario,
                              v_password,
                              v_intentos
END FUNCTION