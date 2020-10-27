--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/03/2015
--==============================================================================

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTX01                                                   #
#Objetivo          => Programa batch de validacion traspaso de saldos receptora#
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE v_conteo_det_total INTEGER,
       v_total_sum_02     INTEGER,
       v_total_sum        INTEGER,
       v_tiempo             CHAR(23),
       v_edo_error          SMALLINT,
       v_edo_procesando     SMALLINT,
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
   LET p_nom_archivo = ARG_VAL(6)

   # Los errores SQL son manejados y el programa no debe detenerse 
   WHENEVER ERROR CONTINUE

   LET v_conteo_det_total = 0
   LET v_total_sum_02     = 0
   LET v_total_sum        = 0
   LET v_edo_procesando   = 2
   LET v_edo_error        = 3
   LET v_error_val_det = FALSE
   LET v_bnd_error_val = FALSE

   --CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
   SELECT MAX(pid)
     INTO p_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = p_proceso_cod
      AND estado_cod = v_edo_procesando

   CALL fn_valida_detalle() RETURNING v_error_val_det
   IF(v_error_val_det)THEN
      LET v_bnd_error_val = TRUE
   END IF

   SELECT COUNT(*)
     INTO v_conteo_det_total
     FROM safre_tmp:tmp_prt_det_traspaso_receptora
    WHERE 1 = 1

   SELECT total_registros
     INTO v_total_sum
     FROM safre_tmp:tmp_prt_sum_traspaso_receptora
    WHERE 1 = 1

    
   IF(v_conteo_det_total <> v_total_sum)THEN
      DISPLAY "\n================================================================="
      DISPLAY "ERROR. SE ENCONTRARON INCONSITENCIAS ENTRE EL DETALLE Y EL SUMARIO"
      DISPLAY "=================================================================\n"
      LET v_bnd_error_val = TRUE
   END IF

   # Establece erronea la operacion
   IF(v_bnd_error_val)THEN

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

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
        INTO v_ind_tipo_ejecucion
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod
         
      IF(v_ind_tipo_ejecucion = 0)THEN # para el caso de ejecucion batch, solo se imprime la leyenda para que realice la tarea por si sola
         SELECT ruta_listados
           INTO v_ruta_listado_nohup
           FROM seg_modulo
          WHERE modulo_cod = 'bat'
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

# Descripción: Validación de los campos del detalle de archivo para traspaso de saldos receptora
FUNCTION fn_valida_detalle()
DEFINE v_consulta         STRING,
       v_conteo_inc       INTEGER,
       v_error_validacion BOOLEAN,
       v_registro_arch    DECIMAL(9,0),
       v_rechazo_desc     VARCHAR(254),
       v_registro         VARCHAR(294),
       v_canal            base.Channel,
       v_ruta_envio       LIKE seg_modulo.ruta_envio,
       v_archivo_destino  STRING

   LET v_error_validacion = FALSE 
   
   PREPARE prp_crea_tbl_tmp_rch FROM "CREATE TEMP TABLE tmp_prt_rechazados(registro VARCHAR(213))"
   EXECUTE prp_crea_tbl_tmp_rch
   PREPARE prp_crea_tbl_tmp FROM "CREATE TEMP TABLE tmp_prt_inconsistencias(registro DECIMAL(9,0),rechazo_cod CHAR(3))"
   EXECUTE prp_crea_tbl_tmp

   LET v_consulta = "EXECUTE PROCEDURE sp_prt_valida_traspaso_sdo_receptora()"
   PREPARE prp_valida_carga FROM v_consulta
   EXECUTE prp_valida_carga
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "Ocurrió un error en validación de carga de traspaso saldos receptora"
   END IF
   
   LET v_conteo_inc = 0
   SELECT COUNT(*) 
     INTO v_conteo_inc
     FROM tmp_prt_inconsistencias
    WHERE 1 = 1

   IF(v_conteo_inc > 0)THEN
      DISPLAY "INCONSISTENCIAS:"
      DISPLAY "\n================================================================="
      LET v_consulta = " SELECT tmp.registro,",
                       "        cat.rechazo_desc",
                       "   FROM tmp_prt_inconsistencias tmp LEFT OUTER JOIN prt_cat_rch_traspasos cat",
                       "     ON tmp.rechazo_cod = cat.rechazo_cod ",
                       "  WHERE 1 = 1"
      PREPARE prp_rec_inconsistencias FROM v_consulta
      DECLARE cur_rec_inconsistencias CURSOR FOR prp_rec_inconsistencias      
      FOREACH cur_rec_inconsistencias INTO v_registro_arch,
                                           v_rechazo_desc
         DISPLAY v_registro_arch||" "||v_rechazo_desc
      END FOREACH
      FREE cur_rec_inconsistencias
      DISPLAY "=================================================================\n"
      LET v_error_validacion = TRUE

      
      # Genera archivo rechazado
      SELECT ruta_envio
        INTO v_ruta_envio
        FROM seg_modulo
       WHERE modulo_cod = 'prt'

      SELECT nom_archivo
        INTO p_nom_archivo
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod
         
      LET v_archivo_destino = v_ruta_envio CLIPPED||"/"||p_nom_archivo CLIPPED||"rch" 
      LET v_canal = base.Channel.create()
      CALL v_canal.openFile(v_archivo_destino,"w")
      LET v_consulta = " SELECT registro",
                       "   FROM tmp_prt_rechazados",
                       "  WHERE 1 = 1"
      PREPARE prp_rec_registros FROM v_consulta
      DECLARE cur_rec_registros CURSOR FOR prp_rec_registros      
      FOREACH cur_rec_registros INTO v_registro
         CALL v_canal.writeLine(v_registro)
      END FOREACH
      FREE cur_rec_registros
      CALL v_canal.close()
      
   END IF

   RETURN v_error_validacion

END FUNCTION
