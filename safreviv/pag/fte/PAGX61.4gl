--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-04-2013
--===============================================================

################################################################################
#Modulo            => PAG                                                      #
#Programa          => PAGX61                                                   #
#Objetivo          => Programa batch de validacion para CURP y NSS de          # 
#                     aportaciones voluntarias                                 # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 16 Abril del 2013                                        #
################################################################################
DATABASE safre_viv



DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo


MAIN
DEFINE v_tiempo             CHAR(23),
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

   LET v_error_val_det = FALSE
   LET v_bnd_error_val = FALSE
   LET v_edo_error     = 3
   
   DISPLAY "INICIA VALIDACION DE DETALLE"
   CALL fn_valida_detalle() RETURNING v_error_val_det
   
   
   # Establece erronea la operacion
   IF(v_error_val_det)THEN
      CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
      LET v_tiempo = CURRENT YEAR TO SECOND;
   
      # actualiza los estado del proceso y operacion de manera manual, ya que
      # no se puede utilizar fn_error_opera cuando se ha finalizado la operacion
      UPDATE bat_ctr_operacion
      SET    fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
      WHERE  pid         = p_pid
      AND    proceso_cod = p_proceso_cod
      AND    opera_cod   = p_opera_cod;

      UPDATE bat_ctr_proceso
      SET    fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
      WHERE  pid         = p_pid
      AND    proceso_cod = p_proceso_cod;

      SELECT ruta_listados
      INTO   v_ruta_listado_nohup
      FROM   seg_modulo
      WHERE  modulo_cod = 'bat'

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
      INTO   v_ind_tipo_ejecucion
      FROM   bat_ctr_operacion
      WHERE  pid = p_pid
      AND    proceso_cod = p_proceso_cod
      AND    opera_cod = p_opera_cod

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
   ELSE
      DISPLAY "FINALIZA VALIDACIÓN CORRECTAMENTE"
   END IF

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPX10                                                   #
#Descripcion       => Validación de los campos del detalle de archivo para     #
#                     marca Op27                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Junio 2012                                            #
################################################################################
FUNCTION fn_valida_detalle()
DEFINE v_consulta         STRING,
       v_error_validacion BOOLEAN,
       v_id_referencia    DECIMAL(9,0),
       v_curp             CHAR(18),
       v_curp_aux         STRING,
       v_nss              CHAR(11),
       v_nss_aux          STRING
       

   LET v_error_validacion = FALSE 

   LET v_consulta = "\n SELECT id_referencia,",
                    "\n         TRIM(curp) as curp,",
                    "\n         TRIM(nss) as nss",
                    "\n    FROM safre_tmp:tmp_pag_det_apvol",
                    "\n   WHERE curp = ''",
                    "\n      OR nss = ''"
   PREPARE prp_recupera_det_apvol FROM v_consulta
   DECLARE cur_recupera_det_apvol CURSOR FOR prp_recupera_det_apvol
   FOREACH cur_recupera_det_apvol INTO v_id_referencia,
                                       v_curp,
                                       v_nss

      LET v_curp_aux = v_curp CLIPPED
      LET v_nss_aux  = v_nss CLIPPED

------------------------------------------------------
-- SE COMENTA CURP POR CORREO DE HAMIR DEL 21-ENE-2015
-- QUE DICE QUE LA CURP ES OPCIONAL
        
--      IF(v_curp_aux IS NULL OR v_curp_aux = ' ')THEN
--         LET v_error_validacion = TRUE 
--         DISPLAY "\nSE ENCONTRÓ CURP NULA PARA ID REFERENCIA: "||v_id_referencia
--      END IF

      IF(v_nss_aux IS NULL OR v_nss_aux = ' ')THEN
         LET v_error_validacion = TRUE 
         DISPLAY "\nSE ENCONTRÓ NSS NULO PARA ID REFERENCIA: "||v_id_referencia
      END IF

   END FOREACH
   FREE cur_recupera_det_apvol
   DISPLAY "\n"

   RETURN v_error_validacion

END FUNCTION