--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP355                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de la notificacion de TIPN_BUT_INT                                     #
#Fecha inicio => MARZO 2015                                                             #
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
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,v_nss_error           CHAR(11)
   
   ##Ejecuta prevalidación de saldos
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
   LET g_proceso_cod = p_proceso_cod  -- notificacion 
   LET g_opera_cod   = p_opera_cod    -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
   
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_but(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
   
   DISPLAY "fn_ret_integra_but "
   DISPLAY "p_usuario_cod  ",p_usuario_cod
   DISPLAY "p_folio        ",p_folio
   DISPLAY "g_pid          ",g_pid
   DISPLAY "p_proceso_cod  ",p_proceso_cod
   DISPLAY '1505'
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, p_folio, g_pid, p_proceso_cod, '1505'   
      INTO v_i_resultado, v_error_isam, v_mensaje
   
   -- Cierra la operación
   DISPLAY "La integración se terminó completamente."
   DISPLAY "Estatus de integración:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : NOTIFICACION TIPN_BUT_INT\n",
                   "Operación    : INTEGRACIÓN\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n",
                   "El folio asociado a su operación es: ", p_folio, "\n\n"

   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY v_mensaje

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración realizada con éxito.\n"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación.\n"
      DISPLAY "El proceso de Integración ha finalizado pero con errores de validación.\n"
      DISPLAY "Error (SQL)         : ", v_i_resultado
      DISPLAY "Error (ISAM)        : ", v_error_isam
      DISPLAY "Error (Mensaje)     : ", v_mensaje
      DISPLAY "Ultimo NSS procesado: ", v_nss_error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado

   END IF

   LET p_titulo = "Finalización de operación - NOTIFICACION TIPN_BUT_INT - INTEGRACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION")


END MAIN