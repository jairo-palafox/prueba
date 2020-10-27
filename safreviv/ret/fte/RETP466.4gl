--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP466                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    # 
#                de la respuesta de notificaci�n de pago de grupos 2, 3 y 4             # 
#Fecha inicio => Abril 8, 2018                                                          # 
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
       v_i_resultado          INTEGER, -- resultado del proceso
       r_bnd_fin_oper         SMALLINT,
       v_si_correcto_integra  SMALLINT,
       p_titulo               STRING, -- titulo del mensaje enviado en el correo
       p_mensaje              STRING, -- cuerpo del mensaje enviado
       v_error_isam           INTEGER,
       v_mensaje              VARCHAR(250),
       v_nss                  LIKE afi_derechohabiente.nss

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
   LET g_proceso_cod = p_proceso_cod -- Respuesta de la Notificaci�n de pago de grupos 2, 3 y 4
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0
   LET v_nss                 = ""

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")

   -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING p_folio 
   DISPLAY "Armando el llamado a la funcion de integracion"
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_resp_notifica_gpo(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql

   DISPLAY "Llamando a la funcion"
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje,v_nss

   LET p_mensaje = "ID Proceso   : ", g_pid,
                   "Proceso      : RESPUESTA DE NOTIFICACI�N DE PAGO DE GRUPOS 2, 3 Y 4",
                   "Operaci�n    : INTEGRACI�N",
                   "Fecha Inicio : ", TODAY, 
                   "Fecha Fin    : ", TODAY, 
                   "El folio asociado a su operaci�n es: ", p_folio

   -- se muestra el mensaje
   DISPLAY p_mensaje
      
   -- si el proceso finalizo correctamente
   IF ( v_i_resultado = 0 ) THEN 
	  -- Cierra la operaci�n
      DISPLAY "\nLa integraci�n se termin� completamente."
      DISPLAY "Estatus de integraci�n:",v_i_resultado

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integraci�n realizada con �xito "
         
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                      RETURNING r_bnd_fin_oper

      LET p_titulo = "Finalizaci�n de operaci�n - RESPUESTA DE NOTIFICACI�N DE PAGO DE GRUPOS 2, 3 Y 4 - INTEGRACION"
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
      
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "INTEGRACION")

   ELSE
      -- ocurrio un error al ejecutar el proceso
      DISPLAY "Error (SQL)    : ", v_i_resultado
	  DISPLAY "Error (ISAM)   : ", v_error_isam
	  DISPLAY "Error (mensaje): ", v_mensaje
      DISPLAY "NSS (con error): ", v_nss

	  DISPLAY "\n\nEl proceso de Integraci�n ha finalizado pero con errores de validaci�n."
	  
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "\n\nEl proceso de Integraci�n ha finalizado pero con errores de validaci�n."

      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                   RETURNING v_i_resultado
   END IF
END MAIN