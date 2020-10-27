################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSP03                                                   #
#Objetivo          => Integración de Actualiza entidad receptora mandato       # 
#Fecha inicio      => Julio 10, 2015                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio, # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo # nombre del archivo a integrar

MAIN

   DEFINE r_folio           LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
   v_proceso_desc    LIKE cat_proceso.proceso_desc,
   v_total_predial   INTEGER,
   v_total_cuota     INTEGER,
   v_consulta        VARCHAR(254),
   v_detalle_temp    VARCHAR(254),
   v_error_sql       INTEGER,
   v_isam_error      INTEGER,
   v_msg_error       CHAR(254),
   r_resultado_opera SMALLINT,
   v_mensaje         CHAR(254),
   v_muestra_detalle INTEGER

   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario)
                        RETURNING r_folio
   
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   #Se genera una tabla temporal para vaciar los detalles en la integración.
   CREATE TEMP TABLE hps_actualiza_ent_mdt(mensaje CHAR(255))

   #Se recupera la cantidad de registros a integrar
   SELECT COUNT(*) 
      INTO v_total_predial 
      FROM safre_tmp:hps_tmp_det_acmdt
      WHERE tipo_mandato = 1
   SELECT COUNT(*)
      INTO v_total_cuota
      FROM safre_tmp:hps_tmp_det_acmdt
      WHERE tipo_mandato = 2

   #Imprime en el log los datos recabados
   DISPLAY "\n"
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "ARCHIVO: ",v_nom_archivo
   DISPLAY "FOLIO: ",r_folio
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA TIPO PREDIAL :",v_total_predial USING "###,##&"
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA CUOTA CONSERVACION :",v_total_cuota USING "###,##&"
   DISPLAY "\n"

   #Ejecuta SP de integración
   LET v_consulta = "EXECUTE PROCEDURE sp_hps_integra_cat_ent_rec_mdt(?,?,?,?)"
   PREPARE prp_consulta FROM v_consulta
   EXECUTE prp_consulta USING p_usuario,p_folio,p_proceso_cod,v_nom_archivo INTO v_error_sql,v_isam_error,v_msg_error

   IF v_error_sql <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_error_sql
      DISPLAY "MENSAJE: ",v_msg_error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                                RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      EXIT PROGRAM
   END IF 

   ## Mostrando detalles de la tabla temporal
   SELECT 
      COUNT(*) 
      INTO v_muestra_detalle
      FROM hps_actualiza_ent_mdt
   IF v_muestra_detalle > 0 THEN
      DISPLAY "SE ENCONTRARON LOS SIGUIENTES DETALLES EN LA INTEGRACION"
      LET v_detalle_temp = "SELECT * FROM hps_actualiza_ent_mdt"
      PREPARE prp_detalles FROM v_detalle_temp
      DECLARE cur_detalle CURSOR FOR prp_detalles
      FOREACH cur_detalle INTO v_mensaje 
         DISPLAY v_mensaje
      END FOREACH
      CLOSE cur_detalle
      FREE cur_detalle
      FREE prp_detalles
   ELSE
      DISPLAY "LOS REGISTROS SE INTEGRARON CORRECTAMENTE"
   END IF

   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF
    
END MAIN