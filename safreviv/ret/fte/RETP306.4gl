--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15-10-2013
-- Fecha ultima modificacion: 15-10-2015, se agrego la restitucion para el
--                            codigo de rechazo respuesta SIAFF.
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETP306                                                  #
#Objetivo          => Programa batch para la preliquidacion de restitución de  #
#                     retiros ley 73 rechazados por FICO y BANCO               #
#Autor             =>                                                          #
#Fecha inicio      => 08 Noviembre 2013                                        #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

DEFINE g_pid         LIKE bat_ctr_proceso.pid,         # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod,     # codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod,     # codigo de operacion
       g_folio       LIKE ret_preliquida.folio_liquida # folio liquidacion

       {
======================================================================
Clave: 
Nombre: MAIN
Fecha creacion: Octubre 15, 2013
Autor: Codigo legado
Narrativa del proceso que realiza:
Funcion principal del programa lanzado que invoca el SP que realiza los
calculos y ejecuta las reglas de negocio de preliquidacion de restitucion de
retiro generico ley73 FICO, SIAFF y BANCO

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Noviembre 11, 2020     - PLAGC138. Anadir cifras control e invocacion de reporte
                                       de cifras control al finalizar el proceso
                                       Este programa preliquida los registros de restitucion de retiro
                                       generico ley73 FICO, SIAFF y BANCO; restringe el conjunto de datos
                                       a partir del codigo de proceso y este a su vez indica el estatus de rechazo
                                       en el que busca los registros
======================================================================
}

MAIN
DEFINE p_pid              LIKE bat_ctr_operacion.pid,         # PID del proceso
       p_proceso_cod      LIKE bat_ctr_operacion.proceso_cod, # codigo del proceso
       p_opera_cod        LIKE bat_ctr_operacion.opera_cod,   # codigo de la operacion
       p_usuario_cod      LIKE seg_usuario.usuario_cod,       # clave del usuario firmado
       p_folio            LIKE ret_preliquida.folio_liquida,
       p_cod_rechazo      SMALLINT,                           # codigo de rechazo
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
       v_estado_preliq_restitucion SMALLINT,
       v_subcuenta        LIKE ret_preliquida.subcuenta,
       v_subcuenta_desc   LIKE cat_subcuenta.subcuenta_desc,
       v_movimiento       LIKE ret_preliquida.movimiento,
       v_movimiento_desc  LIKE cat_movimiento.movimiento_desc,
       v_sum_acciones     LIKE ret_preliquida.monto_acciones,
       v_sum_pesos        LIKE ret_preliquida.monto_pesos
  
   # recupera parametros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_archivo       = ARG_VAL(6)
  
  
   -- selecciona el tipo de codigo de rechazo
   CASE p_proceso_cod
   	
   	 WHEN g_proceso_cod_restitucion_ret_generico_ley73 
   	 	LET p_cod_rechazo = 65
   		
   	 WHEN g_proceso_cod_restitucion_ret_generico_ley73fico
   		LET p_cod_rechazo = 64

     WHEN g_proceso_restitucion_rechazo_siaff
        LET p_cod_rechazo = 66
   		
   END CASE

   
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

   LET p_titulo = "Preliquidación restitución de Retiro Ley 73"

   CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING g_folio
   
   DISPLAY p_mensaje
   LET v_estado_solicitud          = gi_estado_restitucion_rechazo
   LET v_estado_preliq_restitucion = gi_estado_preliq_restitucion_ret_generico 
         
   LET v_consulta = "EXECUTE FUNCTION fn_restitucion_ret_ley73_ws(?,?,?,?,?,?,?,?)"

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
   IF (v_ind = 0) THEN
      IF (v_error_sql = 0) THEN

         -- 20201109 se agregan cifras de control e invocacion de reporte de cifras de control
         SELECT programa_cod
         INTO   p_programa_cod
         FROM   cat_operacion
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = p_opera_cod

         -- se obtiene el folio que se genero durante la preliquidacion
         SELECT folio
         INTO   p_folio
         FROM   bat_ctr_operacion
         WHERE  pid = p_pid
         AND    proceso_cod = p_proceso_cod
         AND    opera_cod = p_opera_cod
               
         CALL fn_reporte_liquidacion(p_folio, "ret_preliquida", 
                                     p_usuario_cod, p_pid, p_proceso_cod, 
                                     p_opera_cod, p_programa_cod, 
                                     FALSE)
      
         CALL fn_actualiza_opera_fin(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod)RETURNING v_estado_operacion
                                     
         IF ( v_estado_operacion  <> 0 ) THEN
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

            -- reporte en log de cifras preliquidadas
            DISPLAY "= CIFRAS PRELIQUIDADAS ="

            DECLARE cur_cifras_control CURSOR FOR
            SELECT pre.subcuenta
                   ,cs.subcuenta_desc
                   ,pre.movimiento
                   ,cm.movimiento_desc
                   ,SUM(pre.monto_acciones)
                   ,SUM(pre.monto_pesos)
            FROM ret_preliquida pre
            INNER JOIN cat_subcuenta cs ON pre.subcuenta = cs.subcuenta
            INNER JOIN cat_movimiento cm ON pre.movimiento = cm.movimiento
            WHERE folio_liquida = p_folio
            GROUP BY pre.subcuenta, cs.subcuenta_desc, pre.movimiento, cm.movimiento_desc
            ORDER BY pre.subcuenta
  
            FOREACH cur_cifras_control INTO v_subcuenta, v_subcuenta_desc, v_movimiento, v_movimiento_desc,
                                            v_sum_acciones, v_sum_pesos
               DISPLAY "SUBCUENTA           ", v_subcuenta, " ", v_subcuenta_desc
               DISPLAY "MOVIMIENTO          ", v_movimiento, " ", v_movimiento_desc
               DISPLAY "MONTO ACCIONES      ", v_sum_acciones
               DISPLAY "MONTO PESOS         ", v_sum_pesos
            END FOREACH

            FREE cur_cifras_control
            
            DISPLAY "\n\nYa se puede Continuar con la Liquidación"
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
      LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación.\n"
   END IF

   LET p_mensaje = p_mensaje,"Fecha Fin    : ", TODAY, "\n"
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod,
                          NULL,
                          p_titulo,
                          p_mensaje)

END MAIN