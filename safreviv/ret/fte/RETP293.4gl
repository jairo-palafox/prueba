--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15-10-2013
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETP293                                                  #
#Objetivo          => Programa batch para la preliquidacion de restitución de  #
#                     retiros genéricos ley 73                                 #
#Autor             =>                                                          #
#Fecha inicio      => 15 Octubre 2013                                          #
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
       v_consulta         STRING,                             # cadena con una instruccion SQL
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
   
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod # restitucion retiros genericos
   LET g_opera_cod   = p_opera_cod   # preliquidación
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
                   "Operación    : ",v_opera_desc,"\n",
                   "Fecha Inicio : ", TODAY, "\n\n"

   LET p_titulo = "Preliquidación restitución retiro genérico ley 73"

   DISPLAY p_mensaje
   LET v_estado_solicitud          = gi_estado_restitucion_rechazo
   LET v_estado_preliq_restitucion = gi_estado_preliq_restitucion_ret_generico 
         
   LET v_consulta = "EXECUTE FUNCTION fn_restitucion_retiros_genericos_ley73(?,?,?,?,?,?,?)"

   # se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE prp_restitucion_ret_generico_ley73 FROM v_consulta
   EXECUTE prp_restitucion_ret_generico_ley73 USING g_folio,
                                                    g_proceso_cod,
                                                    g_opera_cod,
                                                    p_usuario_cod,
                                                    g_pid,
                                                    v_estado_solicitud,
                                                    v_estado_preliq_restitucion
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
            DISPLAY "Preliquidacion realizada con exito"
            LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación \n"
            DISPLAY "Ya se puede Continuar con la Liquidación"
            DISPLAY "\n\n"         
         END IF
      ELSE
         DISPLAY "Preliquidacion realizada pero con errores de validación"
         DISPLAY "CÓDIGO DE ERROR: ",v_error_sql
         DISPLAY "MENSAJE:         ",v_msg_error
         CALL fn_error_opera(g_pid,
                             g_proceso_cod,
                             g_opera_cod) RETURNING v_estado_operacion
         IF( v_estado_operacion  <> 0 )THEN
            # Imprime el mensaje de inconsistencia en consola
            CALL fn_desplega_inc_operacion(v_estado_operacion)
         END IF
         LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación.\n" 
      END IF
   ELSE
      DISPLAY "Preliquidacion realizada pero con errores de validación"
      DISPLAY "CÓDIGO DE ERROR: ",v_diag
      DISPLAY "MENSAJE:         ",v_msg_error
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod) RETURNING v_estado_operacion
      IF( v_estado_operacion  <> 0 )THEN
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(v_estado_operacion)
      END IF
      LET p_mensaje = p_mensaje || "El proceso de Preliquidación ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación.\n"
   END IF
      

   LET p_mensaje = p_mensaje,"Fecha Fin    : ", TODAY, "\n"
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod,
                          NULL,
                          p_titulo,
                          p_mensaje)

END MAIN