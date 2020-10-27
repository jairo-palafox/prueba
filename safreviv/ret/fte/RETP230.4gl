 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP230                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro de aportaciones voluntarias                                #
#Fecha inicio => Agosto 09, 2013                                                        #
#Fecha modificacion =>                                                                  #
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
                                         
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_preliquida_ap_voluntarias(?,?,?,?,?)"
                     
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
      -- Cierra la operación
      DISPLAY "La preliquidacion se terminó completamente."
      DISPLAY "Estatus de preliquidacion:",v_i_resultado
               
      LET p_mensaje = "ID Proceso  : ", p_pid, "\n", 
                      "Proceso      : RETIRO APORTACIONES VOLUNTARIAS\n",
                      "Operación    : PRELIQUIDACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n"
                               
      -- si se termino correctamente
      DISPLAY "Preliquidacion realizada con exito"
      LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación"
      LET p_titulo = "Preliquidación Retiro Aportaciones Voluntarias "
                  
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = p_proceso_cod
      AND    opera_cod   = p_opera_cod
            
      CALL fn_reporte_liquidacion(p_folio, "ret_preliquida", 
                                  p_usuario_cod, p_pid, p_proceso_cod, 
                                  p_opera_cod, p_programa_cod, 
                                  FALSE)
      
      DISPLAY "Ya se puede Continuar con la Liquidación\n"
      
      CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
                                  RETURNING r_bnd_fin_oper
   
      CALL fn_correo_proceso(p_pid, p_proceso_cod, p_opera_cod,
                             NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
      -- el proceso finalizo con errores
      DISPLAY "Preliquidación realizada pero con errores de validación."
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      DISPLAY "Solicitud   : ", v_solicitud_error
      
      LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_i_resultado
   END IF

END MAIN



