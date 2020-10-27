####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTW01                                        #
#Objetivo          =>Interface de safre que manda a llamar el WS   #
#                    de notificacion de mandatos                   #
#Fecha inicio      =>09 MARZO 2012                                 #
####################################################################

DATABASE safre_viv

GLOBALS "MDTW01.inc"    #Archivo de variables globales del lanzador
GLOBALS "MDTW03.inc"    #Archivo de variables globales del WS

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

FUNCTION fn_notifica_instruccion_mdt_can(p_solicitud)
   DEFINE p_solicitud         notificaMandato
   DEFINE v_respuesta_mandato respuestaMandato
   DEFINE v_solicita          tns2NotificaInstruccionRequest
   DEFINE v_respuesta         tns2NotificaInstruccionResponse

   CALL fn_configura_ws()

   #Se llenan los parametros que se enviaran al WS
   LET v_solicita.id_origen         = p_solicitud.id_origen
   LET v_solicita.nss               = p_solicitud.nss
   LET v_solicita.id_credito        = p_solicitud.id_credito
   --LET v_solicita.id_mandato        = p_solicitud.id_mandato
   LET v_solicita.cve_mandato       = p_solicitud.cve_mandato
   LET v_solicita.tpo_descuento     = p_solicitud.tpo_descuento
   LET v_solicita.valor_descuento   = p_solicitud.valor_descuento
   LET v_solicita.f_canales         = p_solicitud.f_canales
   LET v_solicita.f_inicio_mandato  = p_solicitud.f_inicio_mandato
   LET v_solicita.f_culmina_mandato = p_solicitud.f_culmina_mandato
   LET v_solicita.referencia        = p_solicitud.referencia
   LET v_solicita.id_canales        = p_solicitud.id_canales
   LET v_solicita.tipo_operacion    = p_solicitud.tipo_operacion
   LET v_solicita.diagnostico       = p_solicitud.diagnostico
   LET v_solicita.resultado_operacion = p_solicitud.resultado_operacion

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_notifica_mandato( v_url_servidor  CLIPPED, 
                           v_usuario         CLIPPED, 
                           v_password        CLIPPED, 
                           v_intentos, 
                           v_solicita.*) 
   RETURNING v_respuesta.*

   # se asignan los valores de respuesta
   LET v_respuesta_mandato.id_origen               = v_respuesta.id_origen
   LET v_respuesta_mandato.nss                     = v_respuesta.nss
   LET v_respuesta_mandato.id_credito              = v_respuesta.id_credito
   --LET v_respuesta_mandato.id_mandato              = v_respuesta.id_mandato
   LET v_respuesta_mandato.cve_mandato             = v_respuesta.cve_mandato
   LET v_respuesta_mandato.tpo_descuento           = v_respuesta.tpo_descuento
   LET v_respuesta_mandato.valor_descuento         = v_respuesta.valor_descuento
   LET v_respuesta_mandato.f_canales               = v_respuesta.f_canales
   LET v_respuesta_mandato.f_inicio_mandato        = v_respuesta.f_inicio_mandato
   LET v_respuesta_mandato.f_culmina_mandato       = v_respuesta.f_culmina_mandato
   LET v_respuesta_mandato.referencia              = v_respuesta.referencia
   LET v_respuesta_mandato.id_canales              = v_respuesta.id_canales
   LET v_respuesta_mandato.tipo_operacion          = v_respuesta.tipo_operacion
   LET v_respuesta_mandato.resultado_operacion     = v_respuesta.resultado_operacion
   LET v_respuesta_mandato.diagnostico             = v_respuesta.diagnostico
   LET v_respuesta_mandato.diag_notifica           = v_respuesta.diag_notifica

   RETURN v_respuesta_mandato.*
   
END FUNCTION

PRIVATE FUNCTION fn_configura_ws()
   DEFINE v_consulta    STRING

   #La clave '2' del catalogo de clientes de webServices corresponde a la notificacion de mandatos
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING WS_INSTRUCCION INTO  v_url_servidor,
                                                   v_usuario,
                                                   v_password,
                                                   v_intentos
END FUNCTION