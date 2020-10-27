--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP431                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del detalle para la consulta de cargos SSV                             #
#Fecha inicio => Junio 16, 2017                                                         #
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
       ,v_nss                 char(11)
       ,v_s_comando           STRING
   
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
   LET g_proceso_cod = p_proceso_cod --retiros de fondo ahorro
   LET g_opera_cod   = p_opera_cod   --integracion

   --Se obtiene el folio --ERV
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio

   --Se asume que el proceso termina correctamente--
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   --Se envia la cadena que indica el inicio de etapa--
   CALL fn_display_proceso(0, "INTEGRACION")

   DISPLAY "Parametros de ejecucion: "
   DISPLAY "Usuario_cod: ", p_usuario_cod    
   DISPLAY "PID        : ", p_pid            
   DISPLAY "Proceso_cod: ", p_proceso_cod    
   DISPLAY "Opera_cod  : ", p_opera_cod      
   DISPLAY "Folio      : ", p_folio          
   DISPLAY "Archivo    : ", p_nombre_archivo 

   
   --Se contruye el enuncionado SQL--
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_det_consulta_cargos(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql

   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
              INTO v_i_resultado, v_error_isam, v_mensaje, v_nss
   
   --Se finaliza aunque existan errores
   IF ( v_i_resultado = 0 ) THEN
      -- Cierra la operaci�n
      DISPLAY "La integraci�n se termin� completamente."
      DISPLAY "Estatus de integraci�n:",v_i_resultado

      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : DETALLE PARA CONSULTA DE CARGOS SSV\n",
                      "Operaci�n    : INTEGRACI�N\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operaci�n es: ", p_folio, "\n\n"

      
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integraci�n realizada con �xito."

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

      LET p_titulo = "Finalizaci�n de operaci�n - Detalle Consulta Cargos SSV - INTEGRACION"
      
      CALL fn_display_proceso(1, "INTEGRACION")

   ELSE 
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integraci�n ha finalizado pero con errores."                                    
      DISPLAY v_mensaje
      DISPLAY p_mensaje
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      DISPLAY "NSS         : ", v_nss
                                             
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
   END IF

END MAIN