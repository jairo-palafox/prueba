--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-06-2012
--===============================================================

####################################################################
#Modulo            =>SEPE                                          #
#Programa          =>SEPE41                                        #
#Objetivo          =>Programa de integracion de operacion 29       # 
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>18 Junio   2012                               #
####################################################################
DATABASE safre_viv

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          v_v_nom_archivo      VARCHAR(80), -- nombre del archivo de salida
          v_ch_arch_solTransf  BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_folio_liquidacion  LIKE cta_movimiento.folio_liquida,
          v_f_liquida          LIKE cta_movimiento.f_liquida, -- fecha de liquidacion
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_env_sep     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE p_fec_ejecucion         DATE
   DEFINE v_r_rpt_res   RECORD -- registro de resumen
             des_origen   CHAR(40),
             proceso_desc LIKE cat_proceso.proceso_desc,
             lote         CHAR(10),
             f_lote       CHAR(10),
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD
   DEFINE v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE v_estado          CHAR(1)
   DEFINE v_tot_integra     INTEGER
   DEFINE v_tot_integra_det02 INTEGER
   DEFINE v_tot_desmarcados INTEGER
   DEFINE v_tot_rechazados  INTEGER
   DEFINE v_tot_integrados  INTEGER
   DEFINE v_tot_aceptados   INTEGER
   DEFINE v_tot_infonavit   INTEGER
   DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_count_valida    INTEGER

--TMP AHM   DEFINE v_r_rpt_res_edo RECORD
--TMP AHM             f_reporte       CHAR(10),
--TMP AHM             nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
--TMP AHM             folio           LIKE glo_ctr_archivo.folio,
--TMP AHM             tot_integrado   INTEGER,
--TMP AHM             tot_marcadas    INTEGER,
--TMP AHM             tot_rechazadas  INTEGER,
--TMP AHM             tot_infonavit   INTEGER
--TMP AHM          END RECORD  
--TMP AHM   DEFINE v_r_reporte_det DYNAMIC ARRAY OF RECORD
--TMP AHM             gpo                   smallint,--= 1
--TMP AHM             nss                   LIKE sep_det_02_op27.invadido,--= '01234567890'
--TMP AHM             rch_cod               LIKE tmp_sep_rch_marca.rch_cod,--= '001'
--TMP AHM             nss1                  LIKE sep_det_02_op27.invadido,--= '01234567890'
--TMP AHM             nss2                  LIKE sep_det_02_op27.invadido,--= '01234567890'
--TMP AHM             nss3                  LIKE sep_det_02_op27.invadido --= '01234567890'
--TMP AHM          END RECORD

   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_pos              INTEGER
   DEFINE v_pos_nss          SMALLINT
   DEFINE v_nss_info         LIKE sep_det_02_op27.invadido
   DEFINE v_comando          STRING
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados,
          v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
          v_ruta_bin_aux     LIKE seg_modulo.ruta_bin,
          v_sql_error        INTEGER,
          v_isam_error       INTEGER,
          v_msg_error        VARCHAR(254),
          v_mensaje          STRING

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_archivo = ARG_VAL(6) -- archivo procesado
  
   WHENEVER ERROR CONTINUE
                             
   LET v_mensaje = " "
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPL41-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   PREPARE prp_numFolio FROM "EXECUTE FUNCTION fn_genera_folio(?, ?, ?)"
   EXECUTE prp_numFolio USING p_i_proceso_cod, p_i_opera_cod,p_v_usuario
                         INTO p_d_folio
   
   -- asigna el folio en la variable de folio liquidación
   LET v_c_programa_cod = "SEPE41"

   SELECT nombre_archivo
     INTO p_v_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod
   
   IF LENGTH(p_v_archivo CLIPPED) = 0 THEN
      DISPLAY "No existe archivo para el proceso de integración"
      LET v_mensaje = "No existe archivo para el proceso de integración"
   ELSE
   
      SELECT NVL(COUNT(*),0)
        INTO v_count_valida
        FROM sep_cza_op29
       WHERE nombre_archivo = p_v_archivo
       
      IF v_count_valida > 0 THEN
         DISPLAY "Archivo: "||p_v_archivo CLIPPED||" integrado con anterioridad"
         LET v_mensaje = "Archivo: "||p_v_archivo CLIPPED||" integrado con anterioridad"
         DISPLAY "Cerrando proceso. Archivo no Integrado"
         CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                             RETURNING r_b_valida
         IF(r_b_valida <> 0)THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
         END IF
      ELSE
      
         DISPLAY "PID asignado: ",p_d_pid
         DISPLAY "Proceso asignado: ",p_i_proceso_cod
         DISPLAY "Operacion asignado: ",p_i_opera_cod
         DISPLAY "Fecha de Proceso: ",today
         DISPLAY "Archivo: ",p_v_archivo
          
         LET v_tot_integra_det02 = 0 
         LET v_tot_integra       = 0 
          
         LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                          "   FROM safre_tmp:tmp_sep_det_02_op29 "
                          
         PREPARE EnuTotRegDet02 FROM v_s_qryTxt
         
         EXECUTE EnuTotRegDet02 INTO v_tot_integra_det02

         LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                          "   FROM safre_tmp:tmp_sep_det_03_op29 "
                          
         PREPARE EnuTotRegDet03 FROM v_s_qryTxt
         
         EXECUTE EnuTotRegDet03 INTO v_tot_integra
         
         DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS (DET02): ",v_tot_integra_det02
         DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS (DET03): ",v_tot_integra
         DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS        : ",v_tot_integra_det02+v_tot_integra
         
         LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_sep_integra_nrp_op29_tmp(?,?)"
         PREPARE EnuSPIntNot FROM v_s_qryTxt
         EXECUTE EnuSPIntNot USING p_d_folio, p_v_archivo
                              INTO v_sql_error,
                                   v_isam_error,
                                   v_msg_error, 
                                   v_tot_integrados, 
                                   v_tot_aceptados,
                                   v_tot_rechazados
         
         DISPLAY "TOTAL RECHAZADOS:",v_tot_rechazados
         DISPLAY "TOTAL ACEPTADOS:",v_tot_aceptados
         DISPLAY "TOTAL INTEGRADOS:",v_tot_integrados

         IF(v_sql_error <> 0)THEN
            LET v_mensaje = "Ocurrió un error al ejecutar SP de integración"
            DISPLAY "Ocurrió un error al ejecutar SP, código:",v_sql_error
            DISPLAY "ISAM:",v_isam_error
            DISPLAY "Mensaje:",v_msg_error
            CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                RETURNING r_b_valida
            IF(r_b_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
            END IF
         ELSE
            IF v_tot_rechazados > 0 THEN
               DISPLAY "Generando archivo de rechazos"
               IF NOT fn_genera_salida_rch_op29(p_d_folio) THEN
                  LET v_mensaje = "Error al generar archivo de salida de rechazos op29"
                  DISPLAY "Error al generar archivo de salida de rechazos op29"
                  CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                RETURNING r_b_valida
                  IF(r_b_valida <> 0)THEN
                     # En caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_desplega_inc_operacion(r_b_valida)
                  END IF
                  
               END IF
               IF(SQLCA.SQLCODE <> 0)THEN
                  CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                RETURNING r_b_valida
                  IF(r_b_valida <> 0)THEN
                     # En caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_desplega_inc_operacion(r_b_valida)
                  END IF
               END IF
            END IF
            # se recupera la ruta ejecutable del módulo
            SELECT ruta_bin
              INTO v_ruta_ejecutable
              FROM seg_modulo
             WHERE modulo_cod = 'sep'
            
            CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
             
            -- Generacion del reporte
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI41.42r ",
                                            p_v_usuario, " ",
                                            p_d_pid, " ",
                                            p_i_proceso_cod," ",
                                            p_i_opera_cod," ",
                                            p_d_folio," '",
                                            p_v_archivo,
                            "' 1>>", v_ruta_listados CLIPPED,
                            "/nohup:",p_d_pid USING "&&&&&",":",
                                      p_i_proceso_cod USING "&&&&&",":",
                                      p_i_opera_cod USING "&&&&&",
                            " 2>&1 &"
            RUN v_comando
            
            IF(STATUS)THEN
               DISPLAY "Advertencia, ocurrio un error al generar el reporte de integración"
            END IF
            
            LET v_mensaje = "Operación finalizada correctamente"
            -- se invoca la función que deja la operación en estado Finalizado
            LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
            
            -- se verifica si fue posible finalizar la operacion
            IF r_b_valida <> 0 THEN
               LET v_mensaje = "Ocurrió un error al finalizar operación"
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                RETURNING r_b_valida
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
            ELSE
               UPDATE bat_ctr_operacion
                  SET folio = p_d_folio
                WHERE pid = p_d_pid
                  AND proceso_cod = p_i_proceso_cod
                  AND opera_cod = p_i_opera_cod
            END IF
         END IF
      END IF
   END IF

   -- Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_d_pid, 
                          p_i_proceso_cod, 
                          p_i_opera_cod, 
                          '', -- TMP AHM adjunto ?
                          'Integrar archivo de Notificación de NRPs (op29)',
                          v_mensaje
                          )
   
   
END MAIN