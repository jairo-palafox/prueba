--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20-07-2012 
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPR38                                                   #
#Objetivo          => Batch reverso de preliquidacion Operacion 28             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Julio, 2012                                           #
#Modificado        =>                                                          #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Usuario
       p_pid_rev         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod_rev LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod_rev   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio           LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       v_pid             LIKE bat_ctr_proceso.pid,     # identificador de proceso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # Código del proceso
       v_opera_cod       LIKE cat_operacion.opera_cod # Código de la operacion

MAIN
DEFINE v_proceso_desc   LIKE cat_proceso.descripcion,
       v_opera_desc     LIKE cat_operacion.descripcion,
       v_consulta       STRING,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_sql_error      INTEGER,
       v_isam_error     SMALLINT,
       v_msg_error      CHAR(100),
       v_tot_reversados INTEGER,
       v_mensaje        STRING,
       r_res_opera      SMALLINT

   # Se recuperan los parámetros
   LET p_usuario_cod     = ARG_VAL(1)
   LET p_pid_rev         = ARG_VAL(2)
   LET p_proceso_cod_rev = ARG_VAL(3)
   LET p_opera_cod_rev   = ARG_VAL(4)
   LET p_folio           = ARG_VAL(5)
   LET p_nom_archivo     = ARG_VAL(6)


   # Recuepra descripcion de proceso y operacion de reverso
   CALL fn_proceso_cod_desc(p_proceso_cod_rev) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(p_proceso_cod_rev, p_opera_cod_rev) RETURNING v_opera_desc
   DISPLAY "PROCESO      :",v_proceso_desc
   DISPLAY "OPERACIÓN    :",v_opera_desc
   DISPLAY "FOLIO        :",p_folio
   DISPLAY "\n"

   -- proceso y operaciones iniciales
   LET v_proceso_cod = 2202 # operacion 28
   LET v_opera_cod   = 3    # preliquidacion de operacion 28

   # recupera el pid que le corresponde al proceso y folio
   SELECT pid
     INTO v_pid
     FROM bat_ctr_operacion
    WHERE proceso_cod = v_proceso_cod
      AND opera_cod   = v_opera_cod
      AND folio       = p_folio

   LET v_consulta = "EXECUTE FUNCTION fn_sep_r_preliquidar_op28(?,?)"
   PREPARE prp_reverso_integracion FROM v_consulta
   EXECUTE prp_reverso_integracion USING p_folio,
                                         v_pid
                                    INTO v_ind,
                                         v_diag,
                                         v_sql_error,
                                         v_isam_error,
                                         v_msg_error,
                                         v_tot_reversados

   IF ( v_sql_error <> 0 ) THEN
      DISPLAY "\nError en ejecución de SP (Codigo): ",v_sql_error
      DISPLAY "Error en SP (Mensaje):",v_msg_error,"\n"
      LET v_mensaje = "Error al realizar el reverso. Código: ",v_sql_error,", mensaje: ",v_msg_error
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF ( r_res_opera <> 0 ) THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   ELSE
      IF ( v_ind <> 0 ) THEN
         LET v_mensaje = "Error al realizar el reverso. "
         # Muestra las incosnistencias      
         DISPLAY "\nError en ejecución de SP: "
         CASE v_diag
            WHEN "001" 
              DISPLAY "Error en funcion general de rev de preliquidacion\n"
              LET v_mensaje = v_mensaje," Error en funcion general de liquidacion"
            WHEN "002" 
              DISPLAY "Error al actualizar operacion\n"
              LET v_mensaje = v_mensaje," Error al actualizar el proceso bat"
         END CASE
         CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
         IF ( r_res_opera <> 0 ) THEN
            CALL fn_desplega_inc_operacion(r_res_opera)
         END IF
      ELSE
         LET v_mensaje = "Operación finalizada correctamente"
         # imprime totales reversados
         DISPLAY "\nTOTAL REGISTROS REVERSADOS: ",v_tot_reversados
         DISPLAY "\n"
         # finaliza operacion de reverso
         CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
            IF ( r_res_opera  <> 0 ) THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(r_res_opera)
               # trata de establecer erronea la operacion
               CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
               IF ( r_res_opera  <> 0 ) THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(r_res_opera)
               END IF
            END IF
      END IF
   END IF
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de integración Op 29',
                          v_mensaje
                          )

END MAIN