--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP326                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de las marcas de embargo                                               #
#Fecha inicio => Dciembre 1, 2014                                                       #
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
       ,v_contador            INTEGER
   
   ##Ejecuta prevalidaci�n de saldos
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
   LET g_proceso_cod = p_proceso_cod -- notificacion DAPS vencidos
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
   
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_marca_embargo(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje
   
   -- Cierra la operaci�n
   DISPLAY "La integraci�n se termin� completamente."
   DISPLAY "Estatus de integraci�n:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : MARCA EMBARGOS\n",
                   "Operaci�n    : INTEGRACI�N\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n",
                   "El folio asociado a su operaci�n es: ", p_folio, "\n\n"

   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY v_mensaje

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integraci�n realizada con �xito.\n"
      LET p_mensaje = p_mensaje || "Cifras Control:\n\n"
      -- Se obtienen cifras control
      -- Registros Procesados
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio; 
       LET p_mensaje = p_mensaje || "Registros Procesados                    : ", v_contador, "\n"
      -- Registros Marcados
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio
         AND estado_solicitud = 0; 
       LET p_mensaje = p_mensaje || "Registros Marcados                      : ", v_contador, "\n"
      -- Registros Con error en marca
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio
         AND estado_solicitud = 100
         AND cod_rechazo NOT IN (7,52,333); 
       LET p_mensaje = p_mensaje || "Registros Con Error en Marcado          : ", v_contador, "\n"
      -- Registros Con problemas de Datos
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio
         AND estado_solicitud = 100
         AND cod_rechazo = 333; 
       LET p_mensaje = p_mensaje || "Registros Con Problemas de Datos        : ", v_contador, "\n"
      -- Registros Inexistentes
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio
         AND estado_solicitud = 100
         AND cod_rechazo = 7; 
       LET p_mensaje = p_mensaje || "Registros Inexistentes                  : ", v_contador, "\n"
      -- Registros Encontrados Mas de una ocasi�n
      LET v_contador = 0;
      SELECT COUNT(*)
        INTO v_contador
        FROM ret_marca_embargo
       WHERE folio = p_folio
         AND estado_solicitud = 100
         AND cod_rechazo = 52; 
       LET p_mensaje = p_mensaje || "Registros Encontrados mas de una Ocasi�n: ", v_contador, "\n"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integraci�n ha finalizado pero con errores de validaci�n.\n"
      DISPLAY "El proceso de Integraci�n ha finalizado pero con errores de validaci�n.\n"
      DISPLAY "Error (SQL)         : ", v_i_resultado
      DISPLAY "Error (ISAM)        : ", v_error_isam
      DISPLAY "Error (Mensaje)     : ", v_mensaje
      DISPLAY "Ultimo NSS procesado: ", v_nss_error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado

   END IF

   LET p_titulo = "Finalizaci�n de operaci�n - MARCA DE EMBARGOS - INTEGRACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION")


END MAIN