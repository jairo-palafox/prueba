--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE13                                                   #
#Objetivo          => Integración de diagnóstico operación 27                  # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 01, 2012                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio, # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       v_ruta_rescate    LIKE seg_modulo.ruta_rescate,
       v_ruta_envio      LIKE seg_modulo.ruta_envio

MAIN
DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_total_registros_det02 INTEGER,
       v_total_registros_det03 INTEGER,
       v_total_cuentas_procesadas INTEGER,
       v_total_cuentas_marcadas   INTEGER,
       v_consulta STRING,
       r_folio    LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_fecha_actual    DATE,
       v_error_sql       INTEGER,
       v_msg_error       VARCHAR(200),
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,       
       r_bandera         SMALLINT
             
   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)
  
   DATABASE safre_tmp   
   DROP TABLE   tmp_sep_integrados
   CREATE TABLE tmp_sep_integrados(folio DECIMAL(9,0),
                                   id_det_02_op27 DECIMAL(9),
                                   tipo_ocurrencia SMALLINT,
                                   diag_confronta CHAR(2),
                                   clasifica_separacion_ant CHAR(1),
                                   clasifica_separacion_nva CHAR(1)
                                     );
   DATABASE safre_viv

   # Tabla temporal de registros rechazados, para despositar en archivo de salida
   CREATE TEMP TABLE tmp_sep_rechazados(--tipo_registro CHAR(2),
                                        registro CHAR(274))

   LET v_fecha_actual = TODAY
   SELECT ruta_bin,
          ruta_rescate,
          ruta_envio
     INTO v_ruta_ejecutable,
          v_ruta_rescate,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'sep'
      
   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario)
                        RETURNING r_folio
   
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT COUNT(*)
     INTO v_total_registros_det02
     FROM safre_tmp:tmp_sep_det02_op27_diag

   SELECT COUNT(*)
     INTO v_total_registros_det03
     FROM safre_tmp:tmp_sep_det_03_op27_diag

   # Imprime en log datos de integración
   DISPLAY "\n"
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "ARCHIVO: ",v_nom_archivo
   DISPLAY "FOLIO: ",r_folio
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA DETALLE 02 :",v_total_registros_det02 USING "###,##&"
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA DETALLE 03 :",v_total_registros_det03 USING "###,##&"
   DISPLAY "\n"

   # Ejecuta SP de integracion diagnóstico Op27
   LET v_consulta = "EXECUTE FUNCTION fn_sep_integra_diagnostico_op27(?,?,?,?,?)"
   PREPARE prp_integra_diagnostico_op27 FROM v_consulta
   EXECUTE prp_integra_diagnostico_op27 USING r_folio,
                                              v_nom_archivo,
                                              p_usuario,
                                              p_proceso_cod,
                                              p_pid
                                        INTO v_total_cuentas_procesadas,v_total_cuentas_marcadas,v_error_sql,v_msg_error


   IF(SQLCA.SQLCODE <> 0 OR v_error_sql <> 0)THEN
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
   # Estadística de SP
   DISPLAY ""
   DISPLAY "RESULTADO DE LA INTEGRACION DEL ARCHIVO" 
   DISPLAY ""
   DISPLAY "TOTAL DE PAREJAS PROCESADAS                         :",v_total_cuentas_procesadas USING "###,##&"
   DISPLAY "TOTAL DE CUENTAS MARCADAS                           :",v_total_cuentas_marcadas   USING "###,##&" 
   
   # Función que genera archivo de rechazos
   CALL fn_genera_archivo_rechazos()

   {UPDATE bat_ctr_operacion
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod}
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   ELSE
      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '',
                             'Integración de Diagnóstico Op. 27',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||v_fecha_actual||
                             'Fecha Fin    : '||DATE
                             )
      # Ejecuta elaboración de reporte
      LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI13.42r '",
                                p_usuario CLIPPED, "' ",
                                p_pid CLIPPED, " ",
                                p_proceso_cod CLIPPED," ",
                                p_opera_cod CLIPPED," ",
                                r_folio CLIPPED, " '",
                                v_nom_archivo CLIPPED,"'"
      
      RUN v_comando
      IF(STATUS)THEN
         DISPLAY "Ocurrió un error al generar el reporte"
      END IF

    CALL fn_genera_notificacion(r_folio)
   
   END IF   
   
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE13                                                   #
#Objetivo          => Genera archivo de regitros rechados                      #
#Autor             => Hugo César Ramírez Gracía                                #
################################################################################
FUNCTION fn_genera_archivo_rechazos()
DEFINE v_registro_rechazado VARCHAR(300), # El tamaño de tipo registro 03 es el mayor con 274
       v_consulta  STRING,
       v_canal     BASE.CHANNEL,
       v_conteo    SMALLINT

   LET v_conteo = 0
   LET v_consulta = " SELECT COUNT(registro)",
                    "   FROM tmp_sep_rechazados",
                    " WHERE registro[1,2] = '02' "
                  
   PREPARE prp_recupera_conteo FROM v_consulta
   EXECUTE prp_recupera_conteo INTO v_conteo

   IF(v_conteo > 0)THEN
      DISPLAY "TOTAL DE RECHAZADOS POR FALTA DE MARCA OP27         :",v_conteo USING "###,##&"
      DISPLAY ""      
      DISPLAY "ARCHIVO DE RECHAZOS GENERADO EN : ",v_ruta_envio CLIPPED||"/"||v_nom_archivo CLIPPED||"_rch"
      DISPLAY ""      
      LET v_canal = base.Channel.create()
      # Archivo donde se depositaran los rechazos
      CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_nom_archivo CLIPPED||"_rch", "w")

      # Recupera registros de tabla temporal de rechazos, registrados en sp
      LET v_consulta = " SELECT registro",
                       "   FROM tmp_sep_rechazados"
      PREPARE prp_recupera_rechazados FROM v_consulta
      DECLARE cur_recupera_rechazados CURSOR FOR prp_recupera_rechazados
      FOREACH cur_recupera_rechazados INTO v_registro_rechazado
         CALL v_canal.writeLine(v_registro_rechazado) 
      END FOREACH 
      FREE cur_recupera_rechazados
      CALL v_canal.writeLine(NULL) 
      CALL v_canal.close()
   END IF
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE13                                                   #
#Objetivo          => Ejecuta la funcionalidad de notificaciones
#Autor             => JESUS DAVID YAÑEZ MORENO
################################################################################
FUNCTION fn_genera_notificacion(fp_folio)

DEFINE fp_folio       DECIMAL(9,0) 
DEFINE v_consulta    STRING
DEFINE v_id_sep_det_02_op27 decimal(9,0)
DEFINE v_folio              decimal(9,0)
DEFINE v_id_derechohabiente_inv decimal(9,0)
DEFINE v_id_derechohabiente_asc decimal(9,0)
DEFINE v_tipo_nss           smallint    
DEFINE v_ind_notificado     smallint  

   LET v_consulta = " SELECT a.id_det_02_op27, a.folio, a.id_derechohabiente_invadido,b.id_derechohabiente_asociado ",
                    " FROM sep_det_02_op27 a , sep_det_03_op27 b ",
                    " WHERE a.folio =  ? ",
                    " AND   a.folio = b.folio ",
                    " AND   a.id_det_02_op27 = b.id_det_02_op27 ",
                    " AND   a.estado in (30,35) ",
                    " AND   (a.id_derechohabiente_invadido not in (select c.id_derechohabiente from sep_notifica_op27 c) OR ",
                    "        b.id_derechohabiente_asociado not in (select d.id_derechohabiente from sep_notifica_op27 d)) "

   PREPARE prp_notifica FROM v_consulta
   DECLARE cur_notifica CURSOR FOR prp_notifica   
   FOREACH cur_notifica USING fp_folio 
                        INTO  v_id_sep_det_02_op27     ,
                              v_folio                  ,
                              v_id_derechohabiente_inv ,
                              v_id_derechohabiente_asc 

            LET v_ind_notificado = 0
            LET v_tipo_nss = 1                             
            INSERT INTO sep_notifica_op27 VALUES (v_id_sep_det_02_op27 ,
                                                  v_folio              ,
                                                  v_id_derechohabiente_inv ,
                                                  v_tipo_nss               ,
                                                  v_ind_notificado)                                                  

            IF v_id_derechohabiente_asc IS NOT NULL THEN                                                  
               LET v_tipo_nss = 2                             
               INSERT INTO sep_notifica_op27 VALUES (v_id_sep_det_02_op27 ,
                                                     v_folio              ,
                                                     v_id_derechohabiente_asc ,
                                                     v_tipo_nss               ,
                                                     v_ind_notificado)                                                  
            END IF

   END FOREACH

   CALL fn_notifica_proceso(fp_folio, p_proceso_cod, p_usuario)

END FUNCTION

