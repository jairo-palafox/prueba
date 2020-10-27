--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/10/2012
--==============================================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPP71                                                     #
#Objetivo        => Programa de liquidación de expedientes fondo72             #
#Fecha Inicio    => 10 Julio 2013
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_pid             LIKE bat_ctr_proceso.pid,
       p_proceso_cod     LIKE cat_proceso.proceso_cod,
       p_opera_cod       LIKE cat_operacion.opera_cod,
       p_folio           LIKE glo_folio.folio,
       --p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       p_id_expediente_fondo72 LIKE sep_expediente_fondo72.id_expediente_fondo72,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

MAIN
DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_fecha_inicio DATETIME YEAR TO MINUTE,
       v_fecha_fin    DATETIME YEAR TO MINUTE,
       v_consulta     STRING,
       v_total_expedienes INTEGER,
       v_total_cargo      DECIMAL(16,6),
       v_total_abono      DECIMAL(16,6),
       v_error_sql        INTEGER,
       v_msg_error        VARCHAR(200),
       r_resultado_opera  SMALLINT,
       v_mensaje          STRING,
       v_senial_expediente SMALLINT

   WHENEVER ERROR CONTINUE 
   
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   --LET p_nom_archivo = ARG_VAL(6)
   LET p_id_expediente_fondo72 = ARG_VAL(6)

   LET v_fecha_inicio = CURRENT YEAR TO MINUTE
   LET v_mensaje = " "
   
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
                        RETURNING p_folio

   UPDATE bat_ctr_operacion 
   SET folio = p_folio
   WHERE pid = p_pid 
   AND   opera_cod = 1
                        
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   # recupera descripción de operación
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   DISPLAY "\n"
   DISPLAY "PROCESO:   ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_opera_desc
   DISPLAY "INICIO:    ",v_fecha_inicio
   # señal para avanzar maquinaria de expediente
   LET v_senial_expediente = 80 # liquidar previo

   # Ejecuta SP de preliquidación eexpedientes solo infonavit
   LET v_consulta = "EXECUTE FUNCTION safre_viv:fn_sep_preliquidar_fondo72(?,?,?,?,?)"
   PREPARE prp_preliquida_expedientes FROM v_consulta
   EXECUTE prp_preliquida_expedientes USING p_folio,
                                            p_pid,
                                            p_proceso_cod,
                                            p_opera_cod,
                                            p_id_expediente_fondo72
                                       INTO v_total_expedienes,
                                            v_total_cargo,
                                            v_total_abono,
                                            v_error_sql,
                                            v_msg_error

   IF(SQLCA.SQLCODE <> 0 OR v_error_sql <> 0)THEN
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_error_sql
      DISPLAY "MENSAJE: ",v_msg_error
      LET v_mensaje = "El proceso de Preliquidación ha finalizado con errores."
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                                RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   ELSE
      # Actualiza el folio de la operación
      UPDATE bat_ctr_operacion
         SET folio = p_folio
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod

      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera <> 0)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
         LET v_mensaje = "El proceso de Preliquidación ha finalizado con errores."         
      ELSE
         LET v_fecha_fin = CURRENT YEAR TO MINUTE
         DISPLAY "FIN:       ",v_fecha_fin

         DISPLAY "FOLIO:                  ",p_folio 
         DISPLAY "EXPEDIENTES PROCESADOS: ",v_total_expedienes
         DISPLAY "TOTAL CARGO:            ",v_total_cargo
         DISPLAY "TOTAL ABONO:            ",v_total_abono
         DISPLAY "\n"
{         
         # Ejecuta elaboración de reporte
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI61.42r '",
                                   p_usuario_cod CLIPPED,"' ",
                                   p_pid         CLIPPED," ",
                                   p_proceso_cod CLIPPED," ",
                                   p_opera_cod   CLIPPED," ",
                                   p_folio       CLIPPED," '",
                                   p_nom_archivo CLIPPED,"'"
      
         RUN v_comando
         IF(STATUS)THEN
            DISPLAY "Ocurrió un error al generar el reporte"
         END IF
}         
      END IF
   END IF

   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          '',
                          'Finalización de operación - Liquidación de Expedientes Separacion Aclaración Fondo 72',
                          ' ID Proceso: '||p_pid||
                          ' Proceso: '||p_proceso_cod||
                          ' Operacion: '||p_opera_cod||
                          ' Fecha Inicio: '||v_fecha_inicio||
                          ' Fecha Fin: '||v_fecha_fin||' '||
                          v_mensaje
                          )

END MAIN