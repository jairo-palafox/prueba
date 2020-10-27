--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/04/2015
--==============================================================================

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTE07                                                   #
#Objetivo          => Batch de integración de traspaso subsecuentes receptora  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 06 Abril 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTWS07.inc"

DEFINE p_usuario      LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid          LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod  LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod    LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio        LIKE glo_ctr_archivo.folio, # numero de folio
       p_nom_archivo  LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       v_ruta_rescate LIKE seg_modulo.ruta_rescate,
       v_ruta_envio   LIKE seg_modulo.ruta_envio

MAIN
DEFINE v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_operacion_desc  LIKE cat_operacion.opera_desc,
       v_total_registros INTEGER,
       v_total_procesados INTEGER,
       v_total_rechazados INTEGER,
       v_consulta STRING,
       --r_folio    LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_fecha_actual    DATE,
       v_error_sql       INTEGER,
       v_error_isam      INTEGER,
       v_msg_error       VARCHAR(254),
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin
             
   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)
  
   DATABASE safre_tmp   
   PREPARE prp_elimina_tabla_tmp FROM "DROP TABLE IF EXISTS tmp_prt_integrados_subsec"
   EXECUTE prp_elimina_tabla_tmp
   --EXECUTE "DROP TABLE IF EXISTS tmp_prt_integrados"
   --DROP TABLE IF EXISTS tmp_prt_integrados
   CREATE TABLE tmp_prt_integrados_subsec(tpo_operacion  CHAR(2),
                                          nss            CHAR(11),
                                          rechazado      SMALLINT,
                                          monto_pesos    DECIMAL(22,2),
                                          motivo_rechazo CHAR(3));
   DATABASE safre_viv

   --CREATE TEMP TABLE tmp_prt_msj_rechazados(registro CHAR(281))
   
   # Tabla temporal de registros rechazados, para despositar en archivo de salida
   CREATE TEMP TABLE tmp_prt_rechazados(registro CHAR(213))
   
   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,
                        p_opera_cod,
                        p_usuario) RETURNING p_folio
   LET v_fecha_actual = TODAY

   SELECT ruta_bin,
          ruta_rescate,
          ruta_envio
     INTO v_ruta_ejecutable,
          v_ruta_rescate,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'prt'
   
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_operacion_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod
    
   SELECT COUNT(*)
     INTO v_total_registros
     FROM safre_tmp:tmp_prt_det_trasp_subsec_recep

   # Imprime en log datos de integración
   DISPLAY "\n"
   DISPLAY "PROCESO:   ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_operacion_desc
   DISPLAY "ARCHIVO:   ",p_nom_archivo
   DISPLAY "FOLIO:     ",p_folio
   DISPLAY "TOTAL DE REGISTROS A PROCESAR:",v_total_registros USING "##,###,##&"
   DISPLAY "\n"

   # Ejecuta SP de integración traspasos subsecuentes receptora
   LET v_consulta = "EXECUTE FUNCTION fn_prt_integra_trasp_subsec_receptora(?,?,?,?,?,?)"
   PREPARE prp_integra_traspaso_sdo_receptora FROM v_consulta
   EXECUTE prp_integra_traspaso_sdo_receptora USING p_usuario,
                                                    p_pid,
                                                    p_proceso_cod,
                                                    p_folio,
                                                    p_nom_archivo,
                                                    C_ESTADO_SALDO_LIQUIDADO_RECEPTORA
                                               INTO v_error_sql,
                                                    v_error_isam,
                                                    v_msg_error,
                                                    v_total_procesados,
                                                    v_total_rechazados

   IF( v_error_sql <> 0 )THEN
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_error_sql
      DISPLAY "MENSAJE: ",v_msg_error
      CALL fn_error_opera(p_pid,
                          p_proceso_cod,
                          p_opera_cod) RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      EXIT PROGRAM
   END IF
   # Estadística de SP
   DISPLAY ""
   DISPLAY "RESULTADO DE LA INTEGRACIÓN DEL ARCHIVO" 
   DISPLAY ""
   DISPLAY "TOTAL DE REGISTROS RECHAZADOS: ",v_total_rechazados USING "##,###,##&"
   DISPLAY "TOTAL DE REGISTROS PROCESADOS: ",v_total_procesados USING "##,###,##&" 

   IF( v_total_rechazados > 0 )THEN
      # Función que genera archivo de rechazos   
      CALL fn_genera_archivo_rechazos()
      CALL fn_error_opera(p_pid,
                          p_proceso_cod,
                          p_opera_cod) RETURNING r_resultado_opera
   ELSE
      CALL fn_actualiza_opera_fin(p_pid,
                               p_proceso_cod,
                               p_opera_cod) RETURNING r_resultado_opera   
   END IF
   
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF( r_resultado_opera )THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
   ELSE
      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '',
                             'Integración de traspaso de saldos receptora',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||v_fecha_actual||
                             'Fecha Fin    : '||DATE
                             )
      IF( v_total_procesados > 0 )THEN
         # Ejecuta elaboración de reporte
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/PRTI02.42r '",
                                   p_usuario CLIPPED, "' ",
                                   p_pid CLIPPED, " ",
                                   p_proceso_cod CLIPPED," ",
                                   p_opera_cod CLIPPED," ",
                                   p_folio CLIPPED," ",
                                   "PRTL07"

         --DISPLAY v_comando 
         RUN v_comando
         IF( STATUS )THEN
            DISPLAY "Ocurrió un error al generar el reporte"
         END IF
      END IF
   END IF   
   
END MAIN

# Descripción: Genera archivo de regitros rechados
FUNCTION fn_genera_archivo_rechazos()
DEFINE v_registro_rechazado VARCHAR(213),
       v_consulta  STRING,
       v_canal     BASE.CHANNEL

   DISPLAY ""      
         
   LET v_canal = base.Channel.create()
   # Archivo donde se depositaran los rechazos
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||p_nom_archivo CLIPPED||"rch", "w")

   # Recupera registros de tabla temporal de rechazos, registrados en sp
   LET v_consulta = " SELECT registro",
                    "   FROM tmp_prt_rechazados"
   PREPARE prp_recupera_rechazados FROM v_consulta
   DECLARE cur_recupera_rechazados CURSOR FOR prp_recupera_rechazados
   FOREACH cur_recupera_rechazados INTO v_registro_rechazado
      CALL v_canal.writeLine(v_registro_rechazado) 
   END FOREACH 
   FREE cur_recupera_rechazados
   CALL v_canal.writeLine(NULL) 
   CALL v_canal.close()

   DISPLAY "ARCHIVO DE RECHAZOS GENERADO EN : ",v_ruta_envio CLIPPED||"/"||p_nom_archivo CLIPPED||"rch"
   DISPLAY ""
   
END FUNCTION