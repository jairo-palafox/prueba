--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: <  
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETP457                                                  #
#Objetivo          => Programa batch para la preliquidacion de la restituci�n  #
#                     de las excepeciones de la devoluci�n del SSV             #
#Autor             =>                                                          #
#Fecha inicio      => 12 Septiembre 2017                                       #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

DEFINE g_pid         LIKE bat_ctr_proceso.pid,         # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod,     # codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod,     # codigo de operacion
       g_folio       LIKE ret_preliquida.folio_liquida # folio liquidacion

MAIN
DEFINE p_pid              LIKE bat_ctr_operacion.pid,         # PID del proceso
       p_proceso_cod      LIKE bat_ctr_operacion.proceso_cod, # codigo del proceso
       p_opera_cod        LIKE bat_ctr_operacion.opera_cod,   # codigo de la operacion
       p_usuario_cod      LIKE seg_usuario.usuario_cod,       # clave del usuario firmado
       p_folio            LIKE ret_preliquida.folio_liquida,
       p_cod_rechazo      SMALLINT,                           # codigo de rechazo
       v_conteo           INTEGER, -- contador de registros
       v_consulta         STRING,                             # cadena con una instruccion SQL
       v_i_resultado      INTEGER,                            # resultado del proceso       
       p_archivo          VARCHAR(20),
       p_titulo           STRING, # titulo del mensaje enviado en el correo
       p_mensaje          STRING, # cuerpo del mensaje enviado
       p_programa_cod     VARCHAR(10),
       v_estado_operacion SMALLINT,
       v_proceso_desc     LIKE cat_proceso.proceso_desc,
       v_opera_desc       LIKE cat_operacion.opera_desc,
       v_estado_solicitud SMALLINT,
       v_ind              SMALLINT, 
       v_diag             CHAR(3),
       v_error_sql        INTEGER,
       v_isam_error       INTEGER,
       v_msg_error        CHAR(254),
       v_estado_preliq_restitucion SMALLINT
  
   # recupera parametros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_archivo       = ARG_VAL(6)
  
  
   -- selecciona el tipo de c�digo de rechazo
--   CASE p_proceso_cod
   	
--   	WHEN g_proceso_cod_restitucion_ret_generico_amortexc 
--   		LET p_cod_rechazo=65
--   		
--   	WHEN g_proceso_cod_restitucion_rechazo_fico
--   		LET p_cod_rechazo=64
--   		
--   END CASE

   
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod # restitucion retiros genericos
   LET g_opera_cod   = p_opera_cod   # preliquidaci�n
   LET g_folio       = p_folio

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : ",v_proceso_desc,"\n",
                   "Operaci�n    : ",v_opera_desc,"\n",
                   "Fecha Inicio : ", TODAY, "\n\n"

   LET p_titulo = "Preliquidaci�n restituci�n retiro gen�rico amortizaci�n excedentes"

   CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING g_folio

   -- 16Dic2013. Se verifica si hay datos para restituci�n de rechazo
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_excep_devol_ssv
   WHERE  estado_solicitud = 90 AND cod_rechazo IN (64,65) -- listos para restituci�n de rechazo
   
   -- si no hay registros para restituci�n de rechazo
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Restituci�n de rechazo"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : EXCEPCIONES DE LA DEVOLUCI�N DEL SSV\n",
                      "Operaci�n    : RETITUCI�N DE RECHAZO\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para la restituci�n de rechazo.\nNo es necesario ejecutar esta etapa.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE -- se ejecuta la restituci�n de rechazo
      
      DISPLAY p_mensaje
      LET v_estado_solicitud          = gi_estado_restitucion_rechazo
      LET v_estado_preliq_restitucion = gi_estado_preliq_restitucion_ret_generico 
            
      LET v_consulta = "EXECUTE FUNCTION fn_restitucion_ret_excep_devol_ssv(?,?,?,?,?,?,?,?)"

      # se prepara la ejecucion del stored procedure para la preliquidacion
      PREPARE prp_restitucion_ret_generico FROM v_consulta
      EXECUTE prp_restitucion_ret_generico USING g_folio,
                                                 g_proceso_cod,
                                                 g_opera_cod,
                                                 p_usuario_cod,
                                                 g_pid,
                                                 v_estado_solicitud,
                                                 v_estado_preliq_restitucion,
                                                 p_cod_rechazo
                                            INTO v_ind, 
                                                 v_diag,
                                                 v_error_sql,
                                                 v_isam_error,
                                                 v_msg_error

      # si se termino correctamente  
      IF( v_ind = 0 )THEN
         IF(v_error_sql = 0)THEN
            CALL fn_actualiza_opera_fin(g_pid,
                                        g_proceso_cod,
                                        g_opera_cod)RETURNING v_estado_operacion
            IF( v_estado_operacion  <> 0 )THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(v_estado_operacion)
               # trata de establecer erronea la operacion
               CALL fn_error_opera(g_pid,
                                   g_proceso_cod,
                                   g_opera_cod) RETURNING v_estado_operacion
               IF( v_estado_operacion  <> 0 )THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(v_estado_operacion)
               END IF
            ELSE
               DISPLAY "Preliquidaci�n de la restituci�n realizada con exito"
               LET p_mensaje = p_mensaje || "Preliquidaci�n de la restituci�n realizada con �xito\n.Ya se puede continuar con la Liquidaci�n de la restituci�n\n"
               DISPLAY "Ya se puede Continuar con la Liquidaci�n de la restituci�n"
               DISPLAY "\n\n"         
            END IF
         ELSE
            DISPLAY "Preliquidacion de la restituci�n realizada pero con errores de validaci�n"
            DISPLAY "C�DIGO DE ERROR: ",v_error_sql
            DISPLAY "MENSAJE:         ",v_msg_error
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod) RETURNING v_estado_operacion
            IF( v_estado_operacion  <> 0 )THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(v_estado_operacion)
            END IF
            LET p_mensaje = p_mensaje || "El proceso de Preliquidacion de la restituci�n ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidaci�n.\n" 
         END IF
      ELSE
         DISPLAY "Preliquidacion de la restituci�n realizada pero con errores de validaci�n"
         DISPLAY "C�DIGO DE ERROR: ",v_diag
         DISPLAY "MENSAJE:         ",v_msg_error
         CALL fn_error_opera(g_pid,
                             g_proceso_cod,
                             g_opera_cod) RETURNING v_estado_operacion
         IF( v_estado_operacion  <> 0 )THEN
            # Imprime el mensaje de inconsistencia en consola
            CALL fn_desplega_inc_operacion(v_estado_operacion)
         END IF
         LET p_mensaje = p_mensaje || "El proceso de Preliquidacion de la restituci�n ha finalizado pero con errores de validaci�n.\nNo se puede continuar con el proceso de Liquidaci�n.\n"
      END IF
         

      LET p_mensaje = p_mensaje,"Fecha Fin    : ", TODAY, "\n"
      CALL fn_correo_proceso(g_pid, 
                             g_proceso_cod, 
                             g_opera_cod,
                             NULL,
                             p_titulo,
                             p_mensaje)
   END IF
END MAIN