--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01-07-2015
--===============================================================
################################################################################
#Modulo            => SEP                                                      #
#Programa          => HPSX02                                                   #
#Objetivo          => Programa batch de validacion para sumario y detalle de   # 
#actualizacion catalogo entidades receptoras mandato                                                                              #
#Fecha inicio      => Julio, 2015                                              #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN

   --Definiendo variables locales
   DEFINE bnd_valida BOOLEAN 
   DEFINE bnd_err    BOOLEAN
   
   DEFINE v_tiempo   CHAR(23),
       v_edo_error          SMALLINT,
       v_archivo_nohup      STRING,
       v_archivo_errnohup   STRING,
       v_ruta_listado_nohup LIKE seg_modulo.ruta_listados,
       v_comando            STRING,
       v_ind_tipo_ejecucion LIKE bat_ctr_operacion.ind_tipo_ejecucion

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   --Inicialización de variables
   LET bnd_valida = TRUE
   LET bnd_err    = TRUE

   --Valida archivo de integracion
   CALL fn_valida_catalogo() RETURNING bnd_valida

   --Verificando posibles inconsistencias entre sumario y registros totales
   CALL fn_valida_inconsis() RETURNING bnd_err

   --Establece erronea la operacion
   IF bnd_valida OR bnd_err THEN
      CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
      LET v_tiempo = CURRENT YEAR TO SECOND;
      LET v_edo_error = 3;
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

FUNCTION fn_valida_catalogo()

--Variables internas
   DEFINE v_valida BOOLEAN
   DEFINE v_error_sql  INTEGER
   DEFINE v_isam_error INTEGER
   DEFINE v_msg_error  CHAR(254)

   DEFINE tot_reg      INTEGER
   DEFINE v_mensaje    CHAR(255)

   --inicia la funcion
   PREPARE prp_tmp_table FROM "CREATE TEMP TABLE hps_diag_tmp_inconsistencias_mdt(mensaje CHAR(255));"
   EXECUTE prp_tmp_table

   PREPARE prp_exec_func FROM "EXECUTE PROCEDURE safre_viv:sp_hps_valida_catalogo_mdt()"
   DECLARE cur_func CURSOR FOR prp_exec_func
   OPEN cur_func
   FETCH cur_func INTO v_error_sql,v_isam_error,v_msg_error
   CLOSE cur_func
   FREE cur_func
   FREE prp_exec_func
   FREE prp_tmp_table

   IF v_error_sql <> 0 THEN
      DISPLAY "Ocurrió un error en validación de carga de actualización de catálogo de entidades receptoras de mandatos : ",v_error_sql," ",v_msg_error," ",v_isam_error
      LET v_valida = TRUE --TRUE SI EXISTE ERROR
      RETURN v_valida
   END IF

   SELECT COUNT(*) 
      INTO tot_reg 
      FROM safre_tmp:hps_diag_tmp_inconsistencias_mdt

   IF tot_reg > 0 THEN
      DISPLAY "INCONSISTENCIAS DETECTADAS EN REGISTROS DE DETALLE"
       --actualizando encabezado
      UPDATE safre_tmp:hps_tmp_cza_acmdt 
	      SET resultado_opera = "02",
		      diagnostico 	  = "001"

      --Desplegar inconsistencias de la tabla
      DISPLAY "\n================================================================="
      DECLARE cur_rec_inconsistencias CURSOR FOR SELECT mensaje FROM hps_diag_tmp_inconsistencias_mdt ORDER BY 1
      FOREACH cur_rec_inconsistencias INTO v_mensaje
         DISPLAY v_mensaje
      END FOREACH
      FREE cur_rec_inconsistencias
      DISPLAY "=================================================================\n"
      LET v_valida = TRUE
   ELSE 
      LET v_valida = FALSE
   END IF
    
   RETURN v_valida

END FUNCTION

FUNCTION fn_valida_inconsis()

   DEFINE v_valida      BOOLEAN
   DEFINE v_detalle     INTEGER
   DEFINE v_detalle02   INTEGER
   DEFINE v_totaldet    INTEGER
   DEFINE s_detalle     INTEGER
   DEFINE s_detalle02   INTEGER
   DEFINE s_totaldet    INTEGER

   SELECT COUNT(*)
      INTO v_detalle
      FROM safre_tmp:hps_tmp_det_acmdt

   SELECT COUNT(*)
      INTO v_detalle02
      FROM safre_tmp:hps_tmp_det02_acmdt

   LET v_totaldet = v_detalle + v_detalle02

   SELECT tot_reg_detalle01,
          tot_reg_detalle02,
          total_registros_detalle
      INTO s_detalle,
           s_detalle02,
           s_totaldet
      FROM safre_tmp:hps_tmp_sum_acmdt

   IF(v_detalle <> s_detalle OR v_detalle02 <> s_detalle02 OR v_totaldet <> s_totaldet)THEN
      DISPLAY "\n================================================================="
      DISPLAY "ERROR. SE ENCONTRARON INCONSITENCIAS ENTRE EL DETALLE Y EL SUMARIO"
      DISPLAY "=================================================================\n"
      LET v_valida = TRUE -- TRUE SI EXISTE ERROR
   ELSE
      LET v_valida = FALSE
   END IF 
   
   RETURN v_valida 

END FUNCTION