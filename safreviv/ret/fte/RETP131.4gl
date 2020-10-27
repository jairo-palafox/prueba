 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP131                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro LEY 73 72-92                                               #
#Fecha inicio => Febrero 23, 2012                                                       #
#Fecha modificacion =>                                                                  #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod,  -- codigo de operacion
       g_folio       LIKE ret_preliquida.folio_liquida --folio liquidacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid,         -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
       p_folio          LIKE ret_preliquida.folio_liquida,
       v_s_sql          STRING,                             -- cadena con una instruccion SQL
       v_i_resultado    INTEGER                             -- resultado del proceso
       ,r_bnd_fin_oper  SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_doc_cod       VARCHAR(20)
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,p_programa_cod        VARCHAR(10)
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,v_s_comando           STRING

   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- Retiro Ley73 
   LET g_opera_cod   = p_opera_cod   -- preliquidacion
   LET g_folio       = p_folio

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "PRELIQUIDACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_preliquidacion_retiro_ley73_conting(?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integraret FROM  v_s_sql
   EXECUTE sid_integraret USING g_folio,
                                g_proceso_cod,
                                g_opera_cod,
                                p_usuario_cod,
                                g_pid
                 INTO v_i_resultado, v_error_isam, v_mensaje
   
   LET p_mensaje = "ID Proceso   : ", g_pid,
                   "Proceso      : RETIRO POR LEY 73",
                   "Operación    : PRELIQUDIACION",
                   "Fecha Inicio : ", TODAY, 
                   "Fecha Fin    : ", TODAY, 
                   "El folio asociado a su operación es: ", p_folio
				   
   DISPLAY p_mensaje
   
   -- si el proceso finalizo correctamente
   IF ( v_i_resultado = 0 ) THEN
      -- Cierra la operación
      DISPLAY "La preliquidacion se terminó completamente."
      DISPLAY "Estatus de preliquidacion:",v_i_resultado

      DISPLAY "___________________________________________________________________________________"
      DISPLAY "Generando archivo de salida en texto con los abonos mayores a 1000 AIVS: "
      LET v_s_comando = "fglrun RETS131.42r ",p_folio ," ", p_usuario_cod
      RUN v_s_comando
      
      LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación"
      LET p_titulo = "Preliquidación Retiro LEY 73 "

      DISPLAY "Ya se puede continuar con la Liquidación"

      -- se obtiene el codigo de programa
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = g_proceso_cod
      AND    opera_cod   = p_opera_cod

      CALL fn_reporte_liquidacion(g_folio, "ret_preliquida",
                                  p_usuario_cod, p_pid,
                                  g_proceso_cod, p_opera_cod,
                                  p_programa_cod, FALSE)
                         
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper
   
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
							 
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "PRELIQUIDACION")
   ELSE
      -- si ocurrio un error al finalizar el proceso
      DISPLAY "Error (SQL)    : ", v_i_resultado
	  DISPLAY "Error (ISAM)   : ", v_error_isam
	  DISPLAY "Error (mensaje): ", v_mensaje
      DISPLAY "\nEl proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
	  
      LET p_mensaje = p_mensaje || "\nEl proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."

	  -- se finaliza la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
	       RETURNING v_i_resultado
   END IF
END MAIN



