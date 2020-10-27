--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP70                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                para retiro por fortalecimiento al credito                             #
#Fecha inicio => Marzo 08, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,v_error_isam          SMALLINT
       ,v_mensaje             VARCHAR(255)
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,p_programa_cod        VARCHAR(10)

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros por fortalecimiento al credito
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
   RETURNING p_folio
   
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACIÓN")

   -- se contruye el enuncionado SQL
   
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_fc(?,?,?,?,?)"            

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integraretfc FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integraretfc USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje
   
   DISPLAY "La integración se terminó completamente."
   DISPLAY "Estatus de integración:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : RETIRO POR FORTALECIMIENTO AL CRÉDITO\n",
                   "Operación    : INTEGRACIÓN\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n\n"

   
   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY "Integración realizada con éxito."
      DISPLAY "Ya se puede continuar con la Preliquidación."

      
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración realizada con éxito\n.Ya se puede continuar con la Preliquidación"

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper
      
   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Preliquidación."

      DISPLAY "Integración realizada pero con errores de validación."
      DISPLAY v_mensaje

      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado

   END IF

   LET p_titulo = "Finalización de operación - RETIROS FORTALECIMIENTO AL CRÉDITO - INTEGRACIÓN"
      
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACIÓN")


END MAIN