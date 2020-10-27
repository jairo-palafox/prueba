--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24-07-2012
--==============================================================================

################################################################################
#Modulo            => SEPE                                                     #
#Programa          => SEPE04                                                   #
#Objetivo          => Programa de integracion de operacion 40                  # 
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha inicio      => 05 Junio   2012                                          #
#Modificación      => Ajustes de llamadas a funciones generales                #
#Autor             => Hugo César Ramírez García                                #
################################################################################
DATABASE safre_viv

DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo,
       v_ruta_envio      LIKE seg_modulo.ruta_envio,
       v_conteo          INTEGER       

MAIN
   DEFINE v_ind                SMALLINT ,
          v_diag               char(003),
          v_estado_destino     SMALLINT 
          
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
   
   DEFINE v_count_valida    INTEGER

   DEFINE v_r_rpt_res_edo RECORD
             f_reporte       CHAR(10),
             nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
             folio           LIKE glo_ctr_archivo.folio,
             tot_integrado   INTEGER,
             tot_marcadas    INTEGER,
             tot_rechazadas  INTEGER,
             tot_infonavit   INTEGER
          END RECORD  
   DEFINE v_r_reporte_det DYNAMIC ARRAY OF RECORD
             gpo                   smallint,--= 1
             nss                   LIKE sep_det_02_op27.invadido,--= '01234567890'
             rch_cod               LIKE tmp_sep_rch_marca.rch_cod,--= '001'
             nss1                  LIKE sep_det_02_op27.invadido,--= '01234567890'
             nss2                  LIKE sep_det_02_op27.invadido,--= '01234567890'
             nss3                  LIKE sep_det_02_op27.invadido --= '01234567890'
          END RECORD

   DEFINE v_res_opera        SMALLINT          
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
          v_conteo_aceptadas INTEGER,
          v_mensaje          STRING,
          v_sql_error        INTEGER,
          v_isam_error       INTEGER,
          v_msg_error        VARCHAR(254);

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario     = ARG_VAL(1)
   LET p_d_pid         = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod   = ARG_VAL(4)
   LET p_d_folio       = ARG_VAL(5)
   LET p_v_archivo     = ARG_VAL(6) -- archivo procesado
  
   --WHENEVER ERROR STOP
   
   -- se crear el archivo log
   --CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPE31.log")
   
   LET v_mensaje = ''
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPL31-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   CALL fn_genera_folio(p_i_proceso_cod, p_i_opera_cod,p_v_usuario) 
          RETURNING p_d_folio

   IF(p_d_folio IS NULL)THEN
      DISPLAY "\nOcurrió un error al generar folio\n"
      LET v_mensaje = 'Ocurrió un error al generar folio'
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                              RETURNING r_b_valida
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   
   ELSE
      -- asigna el folio en la variable de folio liquidación
      LET v_c_programa_cod = "SEPE04"
      
      {SELECT nombre_archivo
        INTO p_v_archivo
        FROM glo_ctr_archivo
       WHERE proceso_cod = p_i_proceso_cod
         AND opera_cod   = p_i_opera_cod
         AND nombre_archivo = p_v_archivo
         AND estado = 1 # archivo cargado}
      
      IF LENGTH(p_v_archivo CLIPPED) = 0 THEN
         DISPLAY "No existe archivo para el proceso de integración"
         LET v_mensaje = 'No existe archivo para el proceso de integración'
         CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                   RETURNING r_b_valida
         IF(r_b_valida <> 0)THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
         END IF
      ELSE
         SELECT NVL(COUNT(*),0)
           INTO v_count_valida
           FROM sep_cza_op28
          WHERE nombre_archivo = p_v_archivo
          
         IF v_count_valida > 0 THEN
            DISPLAY "Archivo: "||p_v_archivo CLIPPED||" integrado con anterioridad\n"
            LET v_mensaje = "Archivo: "||p_v_archivo CLIPPED||" integrado con anterioridad\n"
            CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                   RETURNING r_b_valida
            IF(r_b_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
            END IF
         ELSE
         
            DISPLAY "PID       : ",p_d_pid
            DISPLAY "PROCESO   : ",p_i_proceso_cod
            DISPLAY "OPERACIÓN : ",p_i_opera_cod
            DISPLAY "FOLIO     : ",p_d_folio
            DISPLAY "FECHA     : ",today
            DISPLAY "ARCHIVO   : ",p_v_archivo
             
            LET v_tot_integra_det02 = 0 
            LET v_tot_integra       = 0 
             
            LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                             "   FROM safre_tmp:tmp_sep_det02_op40 "
                             
            PREPARE EnuTotRegDet02 FROM v_s_qryTxt
            EXECUTE EnuTotRegDet02 INTO v_tot_integra_det02
      
            LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                             "   FROM safre_tmp:tmp_sep_det03_op40 "
                             
            PREPARE EnuTotRegDet03 FROM v_s_qryTxt
            EXECUTE EnuTotRegDet03 INTO v_tot_integra
            
            DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS (DET02): ",v_tot_integra_det02
            DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS (DET03): ",v_tot_integra
            DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS        : ",v_tot_integra_det02+v_tot_integra
            DISPLAY ""

            # Tabla temporal de registros rechazados para generar archivo de salida
            CREATE TEMP TABLE tmp_sep_rechazados(registro CHAR(200))
            
            LET v_s_qryTxt = "EXECUTE FUNCTION sp_sep_integra_op40(?,?,?,?)"
            PREPARE EnuSPIntNot FROM v_s_qryTxt
            EXECUTE EnuSPIntNot USING p_d_folio, p_v_archivo, p_v_usuario, p_i_proceso_cod
                                 INTO v_sql_error,
                                      v_isam_error,
                                      v_msg_error,
                                      v_tot_integrados, 
                                      v_tot_aceptados,
                                      v_tot_rechazados,
                                      v_tot_desmarcados

            # Funcion de archivo de rechazos
            LET v_conteo = 0            
            
            CALL fn_genera_archivo_rechazos()
            
            --DISPLAY "TOTAL DESMARCADOS        :",v_tot_desmarcados
            --DISPLAY "TOTAL RECHAZADOS DESMARCA:",v_tot_rechazados
            
            LET v_tot_aceptados = v_tot_integrados - v_conteo
            
            DISPLAY "TOTAL PAREJAS ACEPTADAS    :",v_tot_aceptados
            DISPLAY "TOTAL PAREJAS INTEGRADAS   :",v_tot_integrados
            DISPLAY ""

            IF(v_sql_error <> 0)THEN
               DISPLAY "Error de integración en SP, código:",v_sql_error
               DISPLAY "Mensaje:",v_msg_error
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                  RETURNING r_b_valida
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF

            END IF

            
      
            # se recupera la ruta ejecutable del módulo
            SELECT ruta_bin
              INTO v_ruta_ejecutable
              FROM seg_modulo
             WHERE modulo_cod = 'sep'
         
            CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
      
            -- Generacion del reporte
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI04.42r ",
                                            p_v_usuario, " ",
                                            p_d_pid, " ",
                                            p_i_proceso_cod," ",
                                            p_i_opera_cod," ",
                                            p_d_folio," ",
                                            p_v_archivo,
                                    " 1>>", v_ruta_listados CLIPPED,
                                  "/nohup:",p_d_pid USING "&&&&&",":",
                                            p_i_proceso_cod USING "&&&&&",":",
                                            p_i_opera_cod USING "&&&&&",
                                  " 2>&1 &"
            RUN v_comando
            
            IF(STATUS)THEN
               DISPLAY "Advertencia, ocurrio un error al generar el reporte de integración"
            END IF
            
              PREPARE prp_maq_ind FROM " EXECUTE FUNCTION safre_viv:fn_maquinaria(?,?,?)"
              EXECUTE prp_maq_ind USING "maq_sep_ctr_op28",
                                        "15",
                                        "10"
                                  INTO  v_ind ,
                                        v_diag ,
                                        v_estado_destino                               

              IF v_ind <> 0 THEN 
                 LET v_mensaje = "Ocurrió un error al actualizar el estado de Op 28"
                 CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                   RETURNING r_b_valida
                 IF(r_b_valida <> 0)THEN
                    # En caso de error se muestra un mensaje a usuario y no continua
                    CALL fn_desplega_inc_operacion(r_b_valida)
                 END IF
              ELSE               
                 UPDATE sep_cza_op40
                    SET estado = v_estado_destino
                  WHERE folio = p_d_folio
                 -- se invoca la función que deja la operación en estado Finalizado
                 LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                 -- se verifica si fue posible finalizar la operacion
                 IF(r_b_valida <> 0)THEN
                    LET v_mensaje = "Ocurrió un error al finalizar la operación"
                    -- en caso de error se muestra un mensaje a usuario y no continua
                    CALL fn_desplega_inc_operacion(r_b_valida)
                    CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                   RETURNING r_b_valida
                    IF(r_b_valida <> 0)THEN
                      # En caso de error se muestra un mensaje a usuario y no continua
                       CALL fn_desplega_inc_operacion(r_b_valida)
                    END IF
                 END IF
                     -- se verifica si hay aceptadas
                     LET v_conteo_aceptadas = 0 
                     
                     SELECT COUNT(*)
                       INTO v_conteo_aceptadas
                       FROM sep_det_02_op40
                      WHERE folio = p_d_folio
                        AND estado = 10 # integradas
                     -- si no hay aceptadas se cierra el lote
                     IF v_conteo_aceptadas = 0 THEN
                     
                        UPDATE sep_cza_op40
                        SET estado = 25 -- se forza a liquidado para cerrar el lote
                        WHERE folio = p_d_folio 

                        LET v_res_opera = fn_actualiza_opera_ini(p_d_pid           , # PID
                                                                 p_i_proceso_cod   , # proceso
                                                                 p_i_opera_cod + 1 , # operacion
                                                                 0                 , # folio
                                                                 "SEPL05"          , # programa
                                                                 p_v_archivo       , # archivo
                                                                 p_v_usuario  )

                        LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod + 1)
                       
                        LET v_res_opera = fn_actualiza_opera_ini(p_d_pid           , # PID
                                                                 p_i_proceso_cod   , # proceso
                                                                 p_i_opera_cod + 2 , # operacion
                                                                 0                 , # folio
                                                                 "SEPL06"          , # programa
                                                                 p_v_archivo       , # archivo
                                                                 p_v_usuario  )

                       LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod + 2)
                     
                     END IF
                 
              END IF              
              
            UPDATE bat_ctr_operacion
               SET folio = p_d_folio
             WHERE pid =  p_d_pid
               AND proceso_cod = p_i_proceso_cod
               AND opera_cod = p_i_opera_cod 
               
            UPDATE bat_ctr_operacion
               SET folio = p_d_folio
             WHERE pid =  p_d_pid
               AND proceso_cod = p_i_proceso_cod + 1 
               
            UPDATE bat_ctr_operacion
               SET folio = p_d_folio
             WHERE pid =  p_d_pid
               AND proceso_cod = p_i_proceso_cod + 2
               
         END IF
      END IF
   END IF
   -- Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_d_pid, 
                          p_i_proceso_cod, 
                          p_i_opera_cod, 
                          '', -- TMP AHM adjunto ?
                          'Integrar archivo de Corrección Separacion',
                          v_mensaje
                          {'ID Proceso   : '||p_d_pid||
                          'Proceso      : '||p_i_proceso_cod||
                          'Operacion    : '||p_i_opera_cod||
                          'Fecha Inicio : '||p_fec_ejecucion||
                          'Fecha Fin    : '||DATE}
                          )
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE31                                                   #
#Objetivo          => Genera archivo de regitros rechados                      #
#Autor             => Hugo César Ramírez Gracía                                #
################################################################################
FUNCTION fn_genera_archivo_rechazos()
DEFINE v_registro_rechazado VARCHAR(300), # El tamaño de tipo registro 03 es el mayor con 65
       v_consulta  STRING,
       v_canal     BASE.CHANNEL

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   LET v_conteo = 0

   LET v_consulta = " SELECT COUNT(registro)",
                    "   FROM tmp_sep_rechazados"
   PREPARE prp_recupera_conteo FROM v_consulta
   EXECUTE prp_recupera_conteo INTO v_conteo

   IF(v_conteo > 0)THEN
      LET v_canal = base.Channel.create()
      # Archivo donde se depositaran los rechazos
      CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||p_v_archivo CLIPPED||"_rch", "w")
      
      DISPLAY "ARCHIVO DE RECHAZOS GENERADO EN : "||v_ruta_envio CLIPPED||"/",p_v_archivo CLIPPED||"_rch"
      DISPLAY ""
      LET v_conteo = (v_conteo-2)/2 # calcúla registros rechazados eliminando encabezado y sumario entre detalles 02 y 03
      # Recupera registros de tabla temporal de rechazos, registrados en sp
      LET v_consulta = " SELECT registro",
                       "   FROM tmp_sep_rechazados"
      PREPARE prp_recupera_rechazados FROM v_consulta
      DECLARE cur_recupera_rechazados CURSOR FOR prp_recupera_rechazados
      FOREACH cur_recupera_rechazados INTO v_registro_rechazado
         CALL v_canal.writeLine(v_registro_rechazado CLIPPED) 
      END FOREACH 
      FREE cur_recupera_rechazados
      CALL v_canal.writeLine(NULL) 
      CALL v_canal.close()
   END IF
   # indica los registros que fueron rechazados por falta de op 27
   DISPLAY "TOTAL PAREJAS RECHAZADAS :",v_conteo
   
END FUNCTION