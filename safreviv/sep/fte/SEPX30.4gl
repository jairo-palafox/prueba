--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27-06-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPX30                                                   #
#Objetivo          => Programa batch de validacion para sumario y detalle op28 # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 27, 2012                                           #
################################################################################
DATABASE safre_viv



DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo


MAIN
DEFINE v_conteo_det_02    INTEGER,
       v_conteo_det_03    INTEGER,
       v_conteo_det_total INTEGER,
       v_total_sum_02     INTEGER,
       v_total_sum_03     INTEGER,
       v_total_sum        INTEGER,
       r_resultado_opera  INTEGER,
       v_tiempo             CHAR(23),
       v_edo_error          SMALLINT,
       v_archivo_nohup      STRING,
       v_archivo_errnohup   STRING,
       v_ruta_listado_nohup LIKE seg_modulo.ruta_listados,
       v_comando            STRING,
       v_error_val_det      BOOLEAN,
       v_bnd_error_val      BOOLEAN,
       v_ind_tipo_ejecucion LIKE bat_ctr_operacion.ind_tipo_ejecucion

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   LET v_conteo_det_02 = 0
   LET v_conteo_det_03 = 0
   LET v_conteo_det_total = 0
   LET v_total_sum_02 = 0
   LET v_total_sum_03 = 0
   LET v_total_sum = 0
   LET v_edo_error = 3
   LET v_error_val_det = FALSE
   LET v_bnd_error_val = FALSE

   CALL fn_valida_detalle() RETURNING v_error_val_det
   IF(v_error_val_det)THEN
      LET v_bnd_error_val = TRUE
   END IF

   SELECT COUNT(*)
     INTO v_conteo_det_02
     FROM safre_tmp:tmp_sep_det02_op28
    WHERE 1 = 1

   SELECT COUNT(*)
     INTO v_conteo_det_03
     FROM safre_tmp:tmp_sep_det03_op28
    WHERE 1 = 1

   LET v_conteo_det_total = v_conteo_det_02 + v_conteo_det_03 

   SELECT total_registro_det2,
          total_registro_det3,
          total_registros
     INTO v_total_sum_02,
          v_total_sum_03,
          v_total_sum
     FROM safre_tmp:tmp_sep_sum_op28
    WHERE 1 = 1

   IF(v_conteo_det_02 <> v_total_sum_02 OR v_conteo_det_03 <> v_total_sum_03 OR v_conteo_det_total <> v_total_sum)THEN
      DISPLAY "\n================================================================="
      DISPLAY "ERROR. SE ENCONTRARON INCONSITENCIAS ENTRE EL DETALLE Y EL SUMARIO"
      DISPLAY "=================================================================\n"
      LET v_bnd_error_val = TRUE
   END IF

   # Establece erronea la operacion
   IF(v_bnd_error_val)THEN
      CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
      LET v_tiempo = CURRENT YEAR TO SECOND;
      # actualiza los estado del proceso y operacion de manera manual, ya que
      # no se puede utilizar fn_error_opera cuando se ha finalizado la operacion
      UPDATE bat_ctr_operacion
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod;

      UPDATE bat_ctr_proceso
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod;

      SELECT ruta_listados
        INTO v_ruta_listado_nohup
        FROM seg_modulo
        WHERE modulo_cod = 'bat'

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
        INTO v_ind_tipo_ejecucion
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod
         
      IF(v_ind_tipo_ejecucion = 0)THEN # para el caso de ejecucion batch, solo se imprime la leyenda para que realice la tarea por si sola
         
         # se cambia el log de la operacion, para que la funcion de correo lo tome correctamente
         # y no provoque error
         LET v_archivo_nohup = v_ruta_listado_nohup CLIPPED, "/finnohup:",
                                p_pid         USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod   USING "&&&&&"

         LET v_archivo_errnohup = v_ruta_listado_nohup CLIPPED, "/errnohup:",
                                  p_pid         USING "&&&&&",":",
                                  p_proceso_cod USING "&&&&&",":",
                                  p_opera_cod   USING "&&&&&" 

         LET v_comando = "mv "||v_archivo_nohup||" "||v_archivo_errnohup
         RUN v_comando
      ELSE
         DISPLAY "Program stopped"
      END IF
   END IF

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPX30                                                   #
#Descripcion       => Validación de los campos del detalle de archivo para     #
#                     Operación 28                                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Junio 2012                                            #
################################################################################
FUNCTION fn_valida_detalle()
DEFINE v_consulta   STRING,
       v_conteo_inc INTEGER,
       v_error_validacion BOOLEAN,
       v_mensaje          VARCHAR(254)

   LET v_error_validacion = FALSE 

   PREPARE prp_crea_tbl_tmp FROM "CREATE TEMP TABLE sep_tmp_inconsistencias_val_op28(mensaje CHAR(254))"
   EXECUTE prp_crea_tbl_tmp

   LET v_consulta = "EXECUTE PROCEDURE sp_sep_valida_op28()"
   PREPARE prp_valida_carga FROM v_consulta
   EXECUTE prp_valida_carga
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "Ocurrió un error en validación de carga de recurrentes"
   END IF

   
   LET v_conteo_inc = 0
   SELECT COUNT(*) 
     INTO v_conteo_inc
     FROM sep_tmp_inconsistencias_val_op28
    WHERE 1 = 1

   IF(v_conteo_inc > 0)THEN
      DISPLAY "\n================================================================="
      DECLARE cur_rec_inconsistencias CURSOR FOR SELECT mensaje
                                                   FROM sep_tmp_inconsistencias_val_op28
                                                  WHERE 1 = 1
      FOREACH cur_rec_inconsistencias INTO v_mensaje
         DISPLAY v_mensaje
      END FOREACH
      FREE cur_rec_inconsistencias
      DISPLAY "=================================================================\n"
      LET v_error_validacion = TRUE
   END IF

   RETURN v_error_validacion

END FUNCTION