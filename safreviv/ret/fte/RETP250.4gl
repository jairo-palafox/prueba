 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP250                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro de amortizaciones excedentes                               #
#Fecha inicio => Octubre 08, 2013                                                       #
#Fecha modificacion =>                                                                  #
#Ivan Vega       16Dic2013          - Se verifica si se tienen registros por preliquidar#
#                                     de lo contrario, se despliega Proceso Vacio a fin #
#                                     de que el proceso en modo Batch pueda terminar    #
#                                     correctamente
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_pid                   LIKE bat_ctr_operacion.pid,         -- PID del proceso
       p_proceso_cod           LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod             LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
       p_usuario_cod           LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
       p_folio                 LIKE ret_preliquida.folio_liquida,
       v_s_sql                 STRING,                             -- cadena con una instruccion SQL
       v_i_resultado           INTEGER                             -- resultado del proceso
       ,v_error_isam           INTEGER
       ,v_mensaje              VARCHAR(255)
       ,r_bnd_fin_oper         SMALLINT
       ,v_si_correcto_integra  SMALLINT
       ,p_doc_cod              VARCHAR(40)
       ,p_titulo               STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje              STRING -- cuerpo del mensaje enviado
       ,p_programa_cod         VARCHAR(10)
       ,v_solicitud_error      LIKE ret_fondo_ahorro.id_solicitud
       ,v_conteo               DECIMAL(9,0) -- contador de registros

   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 
   
   -- se genera el folio
   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod) RETURNING p_folio 

   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   LET v_si_correcto_integra = 0
                     
   DISPLAY "Ejecutando rutina de preliquidaci�n..."
   
   
   -- 16Dic2013. Se verifica si hay datos para preliquidar
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_amort_excedente
   WHERE  estado_solicitud = 15 -- listos para preliquidar
   
   -- si no hay registros para preliquidar
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Preliquidaci�n Retiro Amortizaciones Excedentes "
     -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", p_pid, "\n", 
                      "Proceso      : RETIRO AMORTIZACIONES EXCEDENTES\n",
                      "Operaci�n    : PRELIQUIDACI�N\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para preliquidar. No es necesario ejecutar la etapa de liquidaci�n.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(p_pid, p_proceso_cod, p_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE -- se ejecuta la preliquidacion
                                         
      -- se contruye el enuncionado SQL
      LET v_s_sql = "EXECUTE FUNCTION fn_ret_preliquida_amort_excedentes(?,?,?,?,?)"
                        
      -- se prepara la ejecucion del stored procedure para la integracion
      PREPARE sid_integraret FROM  v_s_sql
      EXECUTE sid_integraret USING p_folio,
                                   p_proceso_cod,
                                   p_opera_cod,
                                   p_usuario_cod,
                                   p_pid
              INTO v_i_resultado, v_error_isam, v_mensaje, v_solicitud_error
              
      -- Se finaliza aunque existan errores
      IF ( v_i_resultado = 0 ) THEN
         -- Cierra la operaci�n
         DISPLAY "La preliquidacion se termin� completamente."
         DISPLAY "Estatus de preliquidacion:",v_i_resultado
                  
         LET p_mensaje = "ID Proceso  : ", p_pid, "\n", 
                         "Proceso      : RETIRO AMORTIZACIONES EXCEDENTES\n",
                         "Operaci�n    : PRELIQUIDACI�N\n",
                         "Fecha Inicio : ", TODAY, "\n",
                         "Fecha Fin    : ", TODAY, "\n\n"
                                  
         -- si se termino correctamente
         DISPLAY "Preliquidacion realizada con exito"
         LET p_mensaje = p_mensaje || "Preliquidacion realizada con �xito\n.Ya se puede continuar con la Liquidaci�n"
                     
         SELECT programa_cod
         INTO   p_programa_cod
         FROM   cat_operacion
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = p_opera_cod
               
         CALL fn_reporte_liquidacion(p_folio, "ret_preliquida", 
                                     p_usuario_cod, p_pid, p_proceso_cod, 
                                     p_opera_cod, p_programa_cod, 
                                     FALSE)
         
         DISPLAY "Ya se puede Continuar con la Liquidaci�n\n"
         
         CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
                                     RETURNING r_bnd_fin_oper
      
         CALL fn_correo_proceso(p_pid, p_proceso_cod, p_opera_cod,
                                NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                                p_titulo,
                                p_mensaje)
      ELSE
         -- el proceso finalizo con errores
         DISPLAY "Preliquidaci�n realizada pero con errores de validaci�n."
         DISPLAY "Error (SQL) : ", v_i_resultado
         DISPLAY "Error (ISAM): ", v_error_isam
         DISPLAY "Mensaje     : ", v_mensaje
         DISPLAY "Solicitud   : ", v_solicitud_error
         
         LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validaci�n.\nNo se puede continuar con el proceso de Liquidaci�n."
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_i_resultado
      END IF
   END IF

END MAIN



