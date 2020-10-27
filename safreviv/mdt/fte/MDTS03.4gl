--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17-04-2012
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTS03                                                   #
#Objetivo          => Se integra la informacion validada proveniente de        #
#                     sustentabilidad                                          #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha Inicio      =>                                                          #
################################################################################
GLOBALS "MDTG02.4gl"

DATABASE safre_viv

DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, # Código del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, # Código de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, # Clave de usuario
   g_ruta_envio                LIKE seg_modulo.ruta_envio,
   v_manejador_rpt    OM.SaxDocumentHandler # Contenedor de Documentos para el reporte


#Objetivo:
MAIN
DEFINE 
   p_num_folio       DECIMAL(9),
   v_indice          INTEGER,
   v_consulta        STRING,
   v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
   v_layout_cod      LIKE cat_operacion.layout_cod,
   v_tabla           LIKE cat_layout.tabla,
   v_registros       INTEGER,
   v_registros_stbd  RECORD
     v_nss           LIKE mdt_solicitud_mandato.nss,
     v_id_credito    LIKE mdt_solicitud_mandato.id_credito,
     v_res_operacion CHAR(2),
     v_f_inicio_mandato  CHAR(8),--LIKE mdt_solicitud_mandato.f_inicio_mandato,
     v_f_culmina_mandato CHAR(8),--LIKE mdt_solicitud_mandato.f_culmina_mandato,
     v_f_captura         CHAR(8), --LIKE mdt_solicitud_mandato.f_canales,
     v_cve_mandato       CHAR(7),--LIKE mdt_solicitud_mandato.cve_mandato,
     v_tpo_operacion     CHAR(1)--LIKE mdt_solicitud_mandato.tipo_operacion
   END RECORD,
   p_archivo         LIKE glo_ctr_archivo.nombre_archivo,
   p_folio           LIKE glo_folio.folio,
   r_resultado_opera SMALLINT,
   --v_lote            LIKE mdt_solicitud_mandato.id_lote
   v_folio           LIKE mdt_solicitud_mandato.folio
DEFINE p_fec_ejecucion DATE,
       v_datos_generales RECORD 
        v_origen         LIKE mdt_cat_origen.des_origen,
        v_proceso        LIKE cat_operacion.opera_desc,
        v_folio          LIKE mdt_solicitud_mandato.folio,
        --v_lote_mdt       LIKE mdt_solicitud_mandato.id_lote,
        v_fecha_lote     CHAR(10),--LIKE mdt_solicitud_mandato.f_lote,
        v_altas          INTEGER,
        v_bajas          INTEGER,
        v_modificaciones INTEGER
       END RECORD,
       v_det_operacion RECORD
        v_altas_aceptadas          INTEGER,
        v_bajas_aceptadas          INTEGER,
        v_modificaciones_aceptadas INTEGER,
        v_altas_rechazadas         INTEGER,
        v_bajas_rechazadas         INTEGER,
        v_mdificaciones_rechazadas INTEGER
       END RECORD,    
       v_detalle_rechazos RECORD
        v_nss         LIKE afi_derechohabiente.nss,
        v_mandato     LIKE mdt_cat_mandato.desc_mandato,
        v_diagnostico LIKE mdt_solicitud_mandato.diagnostico
       END RECORD,
       v_nombre_reporte STRING,
       r_ruta_bin       LIKE seg_modulo.ruta_bin, 
       r_ruta_listados  LIKE seg_modulo.ruta_listados,
       v_contador       SMALLINT,
       v_cad_auc        CHAR(2),
       v_det_mandatos   RECORD 
        tpo_mandato     LIKE mdt_tpo_mandato.desc_tpo_mandato,
        operacion       CHAR(20),
        total_mdt       INTEGER
       END RECORD,
       v_aux_cad        STRING,
       v_aux_cad2        STRING,
       v_id_solicitud_mandato LIKE mdt_solicitud_mandato.id_solicitud_mandato 
       
   WHENEVER ERROR CONTINUE
   
   #Si se ha recibido parámetros se continua    
   #Primer parámetro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo parámetro
   LET g_pid         = ARG_VAL(2)
   #Tercer parámetro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto parámetro
   LET g_opera_cod   = ARG_VAL(4)  --numero de proceso
   #Quinto parámetro
   LET p_folio       = ARG_VAL(5)
   #Sexto parámetro
   LET p_archivo     = ARG_VAL(6)
   
   LET p_fec_ejecucion = DATE
       
   # genera folio 
   CALL fn_genera_folio(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING p_folio
   
   {CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"MDTS03",
                               p_archivo,p_usuario_cod) RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF}
   
   LET p_num_folio = 0
   SELECT ruta_envio
     INTO g_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'

   SELECT layout_cod
     INTO v_layout_cod
     FROM cat_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod

   SELECT tabla
     INTO v_tabla
     FROM cat_layout
    WHERE layout_cod = v_layout_cod

   LET v_contador = 0
   LET v_tabla = "tmp_sol_mandato"
   # Se revisa si exite infromación para procesar, de lo contrario se establece operacion erronea
   #  si existe
   LET v_consulta = "\n SELECT NVL(COUNT(*),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n    AND sol.tipo_operacion = tmp.tipo_operacion",
                    "\n  WHERE sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #canales
   
   PREPARE prp_recupera_total_registros FROM v_consulta 
   EXECUTE prp_recupera_total_registros INTO v_contador
   IF(v_contador = 0)THEN
      DISPLAY "No existe información para procesar"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                                RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
         EXIT PROGRAM
      END IF
      EXIT PROGRAM
   END IF

   CALL fn_display_proceso(0,"INTEGRACIÓN DE ARCHIVO SUSTENTABILIDAD")
   
   SELECT nombre_archivo
     INTO v_nom_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod
         
   # registros aceptados
   LET v_registros = 0
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n    AND sol.tipo_operacion = tmp.tipo_operacion",
                    "\n  WHERE tmp.resultado_operacion = '02'", # Aceptados
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #canales
   PREPARE prp_registros_aceptados FROM v_consulta
   EXECUTE prp_registros_aceptados INTO v_registros
   DISPLAY "\n TOTAL DE REGISTROS RECHAZADOS:",v_registros

   LET v_registros = 0
   # registros rechazados
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n    AND sol.tipo_operacion = tmp.tipo_operacion",
                    "\n  WHERE tmp.resultado_operacion = '01'", # Rechazados
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #canales
   PREPARE prp_registros_rechazados FROM v_consulta
   EXECUTE prp_registros_rechazados INTO v_registros
   DISPLAY "\n TOTAL DE REGISTROS ACEPTADOS:",v_registros

   # Actualiza el registro a aceptado
   LET v_consulta = "\n UPDATE mdt_solicitud_mandato ",
                    "\n    SET estado = 105", #aceptado
                    "\n  WHERE id_solicitud_mandato = ?"
                    {"\n  WHERE nss = ?",v_id_solicitud_mandato
                    "\n    AND id_credito = ?",
                    "\n    AND cve_mandato = ?",
                    "\n    AND estado = 104",   # Enviada a sustentabilidad
                    "\n    AND id_origen = 2" #canales}
   PREPARE prp_solicitud_aceptada FROM v_consulta

   # Actualiza el registro a rechazado
   LET v_consulta = "\n UPDATE mdt_solicitud_mandato ",
                    "\n    SET estado = 108", # rechazado
                    "\n  WHERE id_solicitud_mandato = ?"
                    {"\n  WHERE nss = ?",
                    "\n    AND id_credito = ?",
                    "\n    AND cve_mandato = ?",
                    "\n    AND estado = 104",    # Enviada a sustentabilidad
                    "\n    AND id_origen = 2" #canales}
   PREPARE prp_solicitud_rechazada FROM v_consulta
   #*****************************************************
   # Consultas para el reporte
   # se recupera la descripcion del origen para generar el reporte
   LET v_datos_generales.v_origen = ' '
   SELECT des_origen
     INTO v_datos_generales.v_origen
     FROM mdt_cat_origen
    WHERE id_origen = 2 # origen canales 
   # se genera la descripcion de la operacion para generar el reporte
   SELECT opera_desc
     INTO v_datos_generales.v_proceso
     FROM cat_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod  = g_opera_cod
   # se recupera el conteo de altas totales para generar el reporte
   LET v_consulta = "\n SELECT NVL(COUNT(tmp.tipo_operacion),0)",                  
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n    AND sol.tipo_operacion = tmp.tipo_operacion",
                    "\n  WHERE tmp.tipo_operacion = 'A'", #alta
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #canales
   PREPARE prp_rec_total_altas FROM v_consulta
   LET v_datos_generales.v_altas = 0
   EXECUTE prp_rec_total_altas INTO v_datos_generales.v_altas
   # se recupera el conteo de bajas totales para generar el reporte
   LET v_consulta = "\n SELECT NVL(COUNT(tmp.tipo_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'B'", # baja
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_total_bajas FROM v_consulta
   LET v_datos_generales.v_bajas = 0
   EXECUTE prp_rec_total_bajas INTO v_datos_generales.v_bajas   
   # se recupera el conteo de modificaciones totales para generar el reporte
   LET v_consulta = "\n SELECT NVL(COUNT(tmp.tipo_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'M'", #modificacion
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_total_modificaciones FROM v_consulta
   LET v_datos_generales.v_modificaciones = 0
   EXECUTE prp_rec_total_modificaciones INTO v_datos_generales.v_modificaciones
   # recupera las instrucciones de alta aceptadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'A'", #alta
                    "\n    AND tmp.resultado_operacion = '01'", # aceptada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_aceptadas_altas FROM v_consulta
   LET v_det_operacion.v_altas_aceptadas = 0
   EXECUTE prp_rec_aceptadas_altas INTO v_det_operacion.v_altas_aceptadas
   DISPLAY "ACEPTADAS ALTAS: ",v_det_operacion.v_altas_aceptadas
   # recupera las instrucciones de baja aceptadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'B'", #baja
                    "\n    AND tmp.resultado_operacion = '01'", #aceptada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_aceptadas_bajas FROM v_consulta
   LET v_det_operacion.v_bajas_aceptadas = 0
   EXECUTE prp_rec_aceptadas_bajas INTO v_det_operacion.v_bajas_aceptadas
   DISPLAY "ACEPTADAS BAJAS: ",v_det_operacion.v_bajas_aceptadas
   # recupera las instrucciones de modificaciones aceptadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'M'", #modificacion
                    "\n    AND tmp.resultado_operacion = '01'", #aceptada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_aceptadas_modificacion FROM v_consulta
   LET v_det_operacion.v_modificaciones_aceptadas = 0
   EXECUTE prp_rec_aceptadas_modificacion INTO v_det_operacion.v_modificaciones_aceptadas
   DISPLAY "ACEPTADAS MODIFICACION: ",v_det_operacion.v_modificaciones_aceptadas
   # recupera las instrucciones de alta rechazadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'A'", #alta
                    "\n    AND tmp.resultado_operacion = '02'", #rechazada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_rechazadas_altas FROM v_consulta
   LET v_det_operacion.v_altas_rechazadas = 0
   EXECUTE prp_rec_rechazadas_altas INTO v_det_operacion.v_altas_rechazadas
   DISPLAY "RECHAZADAS ALTAS: ",v_det_operacion.v_altas_rechazadas
   # recupera las instrucciones de baja rechazadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'B'", #baja
                    "\n    AND tmp.resultado_operacion = '02'", #rechazada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_rechazadas_bajas FROM v_consulta
   LET v_det_operacion.v_bajas_rechazadas = 0
   EXECUTE prp_rec_rechazadas_bajas INTO v_det_operacion.v_bajas_rechazadas
   DISPLAY "RECHAZADAS BAJAS: ",v_det_operacion.v_bajas_rechazadas
   # recupera las instrucciones de modificaciones rechazadas
   LET v_consulta = "\n SELECT NVL(COUNT(resultado_operacion),0)",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE tmp.tipo_operacion = 'M'", #modificacion
                    "\n    AND tmp.resultado_operacion = '02'", #rechazada
                    "\n    AND sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   PREPARE prp_rec_rechazadas_mdificaciones FROM v_consulta
   LET v_det_operacion.v_mdificaciones_rechazadas = 0
   EXECUTE prp_rec_rechazadas_mdificaciones INTO v_det_operacion.v_mdificaciones_rechazadas
   DISPLAY "RECHAZADAS MODIFICACION: ",v_det_operacion.v_mdificaciones_rechazadas   

   #*****************************************************

   LET v_indice = 1
   # recupera los registros provenientes de sustentabilidad
   # para ser analizados y aceptarlos o rechazar según sea la respuesta de sustentabilidad
   LET v_consulta = "\n SELECT id_solicitud_mandato,tmp.nss, tmp.id_credito, tmp.resultado_operacion,",
                    "\n        tmp.f_inicio_mandato, tmp.f_culmina_mandato, tmp.f_captura, ",
                    "\n        tmp.cve_mandato, tmp.tipo_operacion, sol.folio",
                    "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.nss = tmp.nss",
                    "\n    AND sol.id_credito = tmp.id_credito",
                    "\n    AND sol.cve_mandato = tmp.cve_mandato",
                    "\n  WHERE sol.estado = 104", # Enviada a sustentabilidad
                    "\n    AND sol.id_origen = 2" #origen canales
   
   LET v_datos_generales.v_folio = 0
   PREPARE prp_recupera_reg_sustentabilidad FROM v_consulta
   DECLARE cur_integra_sustentabilidad CURSOR FOR prp_recupera_reg_sustentabilidad
   FOREACH cur_integra_sustentabilidad INTO v_id_solicitud_mandato,
                                           v_registros_stbd.v_nss,                                            
                                            v_registros_stbd.v_id_credito,
                                            v_registros_stbd.v_res_operacion,
                                            v_registros_stbd.v_f_inicio_mandato,
                                            v_registros_stbd.v_f_culmina_mandato,
                                            v_registros_stbd.v_f_captura,
                                            v_registros_stbd.v_cve_mandato,
                                            v_registros_stbd.v_tpo_operacion, 
                                            v_datos_generales.v_folio

      LET v_aux_cad = v_registros_stbd.v_id_credito
      LET v_registros_stbd.v_id_credito = v_aux_cad.trim()
      LET v_aux_cad = v_datos_generales.v_folio
      LET v_datos_generales.v_folio = v_aux_cad.trim()
            
      IF(v_registros_stbd.v_res_operacion = '01')THEN # 01 Aceptadas
         # si el registro regresó como aceptado
         EXECUTE prp_solicitud_aceptada USING v_id_solicitud_mandato
         {v_registros_stbd.v_nss,
                                              v_registros_stbd.v_id_credito,
                                              v_registros_stbd.v_cve_mandato}
         IF(SQLCA.SQLCODE <> 0)THEN
            CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                                RETURNING r_resultado_opera
            # si ocurrió un error con la actualizacion de la operacion operacion 
            # muestra el mensaje
            IF(r_resultado_opera)THEN
               CALL fn_desplega_inc_operacion(r_resultado_opera)
               EXIT PROGRAM
            END IF
         END IF
      END IF
      IF(v_registros_stbd.v_res_operacion = '02')THEN # 02 rechazadas
         # si el registro regresó como rechazado
         EXECUTE prp_solicitud_rechazada USING v_id_solicitud_mandato
         {v_registros_stbd.v_nss,
                                               v_registros_stbd.v_id_credito,
                                               v_registros_stbd.v_cve_mandato}
         IF(SQLCA.SQLCODE <> 0)THEN
            CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                                RETURNING r_resultado_opera
            # si ocurrió un error con la actualizacion de la operacion operacion 
            # muestra el mensaje
            IF(r_resultado_opera)THEN
               CALL fn_desplega_inc_operacion(r_resultado_opera)
               EXIT PROGRAM
            END IF
         END IF
      END IF
      # Conservamos la fecha para despues usarla para el reporte
      LET v_aux_cad = v_registros_stbd.v_f_captura
   END FOREACH

   # acutaliza el lote a lote concluido con el ultimo solicitud recuperada de 
   # de la tabla temporal
   LET v_folio = 0
   LET v_consulta ="\n SELECT FIRST 1 folio",
                   "\n   FROM mdt_solicitud_mandato",
                   "\n  WHERE id_solicitud_mandato = ? "
                   {"\n  WHERE nss = ? ",
                   "\n    AND id_credito = ?",
                   --"\n    AND f_inicio_mandato = ?",
                   --"\n    AND f_culmina_mandato = ?",
                   "\n    AND cve_mandato = ?",
                   "\n    AND tipo_operacion = ?",
                   "\n    AND id_origen = 2" #origen canales}
   PREPARE prp_recupera_lote FROM v_consulta
   EXECUTE prp_recupera_lote USING v_id_solicitud_mandato
   {v_registros_stbd.v_nss, v_registros_stbd.v_id_credito, --v_registros_stbd.v_f_inicio_mandato,
                                   --v_registros_stbd.v_f_culmina_mandato,
                                   v_registros_stbd.v_cve_mandato,v_registros_stbd.v_tpo_operacion}
                              INTO v_folio

   #****************************************************************************
   # Reporte
   
   # se recupera el dia
   LET v_aux_cad2 = v_aux_cad
   LET v_aux_cad = v_aux_cad2.subString(7,8),"-",v_aux_cad2.subString(5,6),"-",v_aux_cad2.subString(1,4)
   LET v_datos_generales.v_fecha_lote = v_aux_cad

   CALL fn_rutas("mdt") RETURNING r_ruta_bin, r_ruta_listados
   
   IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/MDTG011.4rp") THEN
     
      
      # se indica la salida del reporte
      CALL fgl_report_selectDevice("PDF")
      
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-MDTS03-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nombre_reporte)
      # sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)
      # solo se recuperan 12 registros, ya que son el número maximo de variables a presentar
      LET v_consulta = "\n SELECT FIRST 12 tpo.desc_tpo_mandato, ",
                       "\n        CASE tmp.tipo_operacion",
                       "\n           WHEN 'A'",
                       "\n              THEN 'ALTA'",
                       "\n           WHEN 'B'",
                       "\n              THEN 'BAJA'",
                       "\n           WHEN 'M'",
                       "\n              THEN 'MODIFICACIÓN'",
                       "\n        END CASE, COUNT(tmp.tipo_operacion)",
                       "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol ",
                       "\n     ON sol.nss = tmp.nss",
                       "\n    AND sol.id_credito = tmp.id_credito",
                       "\n    AND sol.cve_mandato = tmp.cve_mandato",
                       "\n        JOIN mdt_tpo_mandato tpo",
                       "\n     ON tpo.tpo_mandato  = sol.cve_mandato[2]", # se hace la consulta directa a tpo mandato con los dos primeros caracteres de cve_mandato
                       {"\n        JOIN mdt_cat_mandato cat",  
                       "\n     ON cat.cve_mandato = sol.cve_mandato",
                       "\n        JOIN mdt_tpo_mandato tpo",
                       "\n     ON tpo.tpo_mandato  = cat.tpo_mandato",}
                       "\n  WHERE tmp.resultado_operacion = ?",
                       "\n    AND (sol.estado = 105 OR sol.estado = 108)", # Aceptadas y rechazadas
                       --"\n    AND sol.estado = 104", # Aceptadas y rechazadas
                       "\n    AND sol.id_origen = 2", #origen canales
                       "\n  GROUP BY 1,2",
                       "\n  ORDER BY 1,2"
      PREPARE prp_recupera_det_tpo_mandato FROM v_consulta
      DECLARE cur_recupera_det_tpo_mandato CURSOR FOR prp_recupera_det_tpo_mandato
      # recupera el detalle de tipo de mandato para el reporte
      # Aceptadas
      LET v_cad_auc = '01'
      LET v_indice = 1
      INITIALIZE v_r_rpt_aceptadas[1].* TO NULL
      FOREACH cur_recupera_det_tpo_mandato USING v_cad_auc INTO v_det_mandatos.tpo_mandato,
                                                                v_det_mandatos.operacion,
                                                                v_det_mandatos.total_mdt
         LET v_r_rpt_aceptadas[v_indice].tpo_mandato = v_det_mandatos.tpo_mandato
         LET v_r_rpt_aceptadas[v_indice].operacion   = v_det_mandatos.operacion
         LET v_r_rpt_aceptadas[v_indice].total_mdt   = v_det_mandatos.total_mdt
         LET v_indice = v_indice + 1
      END FOREACH
      
      # recupera el detalle de tipo de mandato para el reporte
      # Rechazadas
      LET v_cad_auc = '02'
      LET v_indice = 1
      INITIALIZE v_r_rpt_canceladas[1].* TO NULL 
      FOREACH cur_recupera_det_tpo_mandato USING v_cad_auc INTO v_det_mandatos.tpo_mandato,
                                                                v_det_mandatos.operacion,
                                                                v_det_mandatos.total_mdt
         LET v_r_rpt_canceladas[v_indice].tpo_mandato = v_det_mandatos.tpo_mandato
         LET v_r_rpt_canceladas[v_indice].operacion   = v_det_mandatos.operacion
         LET v_r_rpt_canceladas[v_indice].total_mdt   = v_det_mandatos.total_mdt
         LET v_indice = v_indice + 1
      END FOREACH
      
      
      --v_r_rpt_aceptadas
      LET v_contador = 0
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT rpt_solicitudes_mandatos TO XML HANDLER v_manejador_rpt
         LET v_consulta = "\n SELECT DISTINCT tmp.nss,cat.desc_mandato,tmp.diagnostico",
                          "\n   FROM safre_tmp:",v_tabla CLIPPED," tmp JOIN mdt_solicitud_mandato sol",
                          "\n     ON sol.nss = tmp.nss",
                          "\n    AND sol.id_credito = tmp.id_credito",
                          "\n    AND sol.cve_mandato = tmp.cve_mandato",
                          "\n        JOIN mdt_cat_mandato_paquete paq",  # se agrega la tabla mdt_cat_mandato_paquete ya que ahora es con esta la relacion de clave 
                          "\n     ON paq.cve_mandato = sol.cve_mandato", #para poder obtener descripciones de mandatos
                          "\n        JOIN mdt_cat_mandato cat",
                          "\n     ON cat.id_cat_mandato = paq.id_cat_mandato",
                          "\n  WHERE sol.id_origen = 2", #origen canales
                          "\n    AND sol.estado = 108", # Rechazadas 
                          "\n  ORDER BY tmp.nss,cat.desc_mandato,tmp.diagnostico"
         PREPARE prp_recupera_detalle_instrucciones FROM v_consulta
         DECLARE cur_recupera_detalle_instrucciones CURSOR FOR prp_recupera_detalle_instrucciones 
         FOREACH cur_recupera_detalle_instrucciones INTO v_detalle_rechazos.*
            OUTPUT TO REPORT rpt_solicitudes_mandatos(v_datos_generales.*, v_det_operacion.*,v_detalle_rechazos.*)
            LET v_contador = v_contador + 1
         END FOREACH
         IF(v_contador = 0)THEN
            OUTPUT TO REPORT rpt_solicitudes_mandatos(v_datos_generales.*, v_det_operacion.*," "," "," ")
         END IF
         
         FREE cur_recupera_detalle_instrucciones
         FINISH REPORT rpt_solicitudes_mandatos
   ELSE
      DISPLAY "no fué posible generar el reporte"
   END IF   
                              
   UPDATE mdt_lote_mandato
      SET estado = 102
    WHERE folio = v_folio
    
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   ELSE
      -- Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(g_pid, 
                             g_proceso_cod, 
                             g_opera_cod, 
                             '',
                             'Integracion de validacion de sustentabilidad',
                             'ID Proceso   : '||g_pid||
                             'Proceso      : '||g_proceso_cod||
                             'Operacion    : '||g_opera_cod||
                             'Fecha Inicio : '||p_fec_ejecucion||
                             'Fecha Fin    : '||DATE
                             )

   END IF
   CALL fn_act_edo_archivo(p_archivo,p_folio,2,p_usuario_cod)
                           RETURNING r_resultado_opera
   IF(r_resultado_opera <> 0)THEN
      DISPLAY "\n ERROR AL ACTUALIZAR ESTADO DE ARCHIVO (CÓDIGO):",r_resultado_opera
   END IF

   FREE cur_integra_sustentabilidad
   CALL fn_display_proceso(1,"INTEGRACIÓN DE ARCHIVO SUSTENTABILIDAD")
END MAIN

