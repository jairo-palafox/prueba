--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22-06-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP28                                                   #
#Objetivo          => Programa batch de solicitud de restituciones             # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_registros_restitucion RECORD
         v_id_expediente 	VARCHAR(9),--DECIMAL(9,0),
         v_invadido 		CHAR(011),
         v_asociado 		CHAR(011),
         v_nss_restitucion  CHAR(011),
         v_aivs_sar92 		DECIMAL(22,2),
         v_pesos_sar92 		DECIMAL(22,2),
         v_Aivs_viv97 		DECIMAL(22,2),
         v_pesos_viv97 		DECIMAL(22,2),
         v_aivs_subsc 		DECIMAL(22,2),
         v_pesos_subsc 		DECIMAL(22,2),
         v_tipo_restitucion CHAR(40)
       END RECORD,
       v_manejador_rpt     OM.SaxDocumentHandler,
       v_fecha_actual      STRING,
       v_total_procesados  INTEGER

MAIN
DEFINE v_consulta           STRING,
       v_proceso_desc       LIKE cat_proceso.proceso_desc,
       v_opera_desc         LIKE cat_operacion.opera_desc,
       v_conteo_solicitudes INTEGER,
       v_conteo_sol_anlisis INTEGER,
       v_conteo_sol_115     INTEGER,
       v_conteo_sol_no_apl  INTEGER,
       r_resultado_opera    SMALLINT,
       v_sql_error          INTEGER,
       v_msg_error          VARCHAR(200),
       v_msg_correo         STRING,
       r_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
       r_ruta_lst           LIKE seg_modulo.ruta_listados,
       v_nom_reporte        STRING,
       v_contador           INTEGER

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   --WHENEVER ERROR CONTINUE
   LET v_total_procesados = 0
   LET v_msg_correo = ' ' 
   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   LET v_fecha_actual = MONTH(TODAY) USING "&&" CLIPPED,"-",
                        DAY(TODAY) USING "&&" CLIPPED,"-",
                        v_fecha_actual.trim()

   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
         RETURNING p_folio

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = 1

   SELECT NVL(COUNT(*),0)
     INTO v_conteo_sol_anlisis
     FROM sep_mto_restitucion_analisis
    WHERE ind_restitucion = 1

   SELECT NVL(COUNT(*),0)*2
     INTO v_conteo_sol_115
     FROM sep_115_restitucion
    WHERE ind_restitucion = 1

   SELECT NVL(COUNT(*),0)
     INTO v_conteo_sol_no_apl
     FROM sep_mto_restitucion_no_aplicados
    WHERE ind_restitucion = 1

   LET v_conteo_solicitudes = 0
   LET v_conteo_solicitudes = v_conteo_sol_anlisis + v_conteo_sol_115 + v_conteo_sol_no_apl

   IF(v_conteo_solicitudes > 0)THEN
      DATABASE safre_tmp
      LET v_consulta = "\n CREATE TEMP TABLE safre_tmp:tmp_sep_restitucion(",
                       "\n     id_expediente 	 DECIMAL(9,0),",
                       "\n     invadido 		 CHAR(011),",
                       "\n     asociado 		 CHAR(011),",
                       "\n     nss_restitucion   CHAR(011),",
                       "\n     aivs_sar92 		 DECIMAL(22,2),",
                       "\n     pesos_sar92 		 DECIMAL(22,2),",
                       "\n     Aivs_viv97 		 DECIMAL(22,2),",
                       "\n     pesos_viv97 		 DECIMAL(22,2),",
                       "\n     aivs_subsc 		 DECIMAL(22,2),",
                       "\n     pesos_subsc 		 DECIMAL(22,2),",
                       "\n     tipo_restitucion  CHAR(40)",
                       "\n );"
      PREPARE prp_crea_tbl_reporte FROM v_consulta
      EXECUTE prp_crea_tbl_reporte 
      DATABASE safre_viv
      DISPLAY "PROCESO: ",v_proceso_desc
      DISPLAY "OPERACIÓN: ",v_opera_desc
      DISPLAY "FEHCA: ",TODAY USING "dd-mm-yyyy"
      DISPLAY "TOTAL SOLICITUDES A PROCESAR: ",v_conteo_solicitudes
      DISPLAY "\n"
      LET v_sql_error = 0
      LET v_consulta = "EXECUTE FUNCTION fn_registra_solicitud_restitucion_ssv(?,?,?)"
      PREPARE prp_ejecuta_sol_restitucion FROM v_consulta
      EXECUTE prp_ejecuta_sol_restitucion USING p_proceso_cod,
                                                p_opera_cod,
                                                p_usuario_cod
                                           INTO v_total_procesados, 
                                                p_folio,
                                                v_sql_error,
                                                v_msg_error

      IF(v_sql_error = 0)THEN
         LET v_conteo_solicitudes = 0
         SELECT COUNT(*)
           INTO v_conteo_solicitudes
           FROM safre_tmp:tmp_sep_restitucion
          WHERE 1 = 1
          
         DISPLAY "FOLIO GENERADO: ",p_folio
         DISPLAY "TOTAL DE SOLICITUDES REGISTRADAS: ",v_total_procesados
         DISPLAY "\n"

         # Actualiza los documentos de restitucion a procesados
         UPDATE sep_expediente
            SET ind_restitucion = 3
          WHERE estado IN (40,45,46, 50) # 40 = Dictamen Registrado, 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
            AND ind_restitucion = 2
            
         UPDATE sep_expediente
            SET ind_restitucion_no_aplicados = 3
          WHERE estado IN (45,46, 50) # 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
            AND ind_restitucion_no_aplicados = 2

         # Si no se han cargado complementarias, no se actualizarán, ya que no coincidirán los indicadores
         --UPDATE sep_expediente
         --   SET ind_restitucion_complementario_1 = 3
         -- WHERE estado IN (45,46, 50) # 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
         --   AND ind_restitucion_complementario_1 = 2

         # Para complementaria 2 debe haber complementaria 1, trata de actualizar directamente ya que es controlado por
         # los indicadores, si ya existe complementaria 1 se actualizará complementaria 2
         --UPDATE sep_expediente
         --   SET ind_restitucion_complementario_2 = 3
         -- WHERE estado IN (45,46, 50) # 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
         --   AND ind_restitucion_complementario_2 = 2
               
         CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                                  RETURNING r_resultado_opera
         # si ocurrió un error con la actualizacion de la operacion operacion 
         # muestra el mensaje
         IF(r_resultado_opera)THEN
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            LET v_msg_correo = "OCURRIÓ UN ERROR AL FINALIZAR OPERACIÓN"
         END IF

         ### CREACIÓN DE REPORTE
         CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   
         IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPP281.4rp"))THEN
            # Salida del reporte
            CALL fgl_report_selectDevice("PDF")
            LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPP28-", 
                                p_pid USING "&&&&&", "-", 
                                p_proceso_cod USING "&&&&&", "-", 
                                p_opera_cod USING "&&&&&"
                          
            # ruta de salida del reporte
            CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
            # Indica que no hay previsualizacion
            CALL fgl_report_selectPreview(0)
            
      
            # se asigna la configuración en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
            START REPORT fn_rpt_restitucion TO XML HANDLER v_manejador_rpt
               DECLARE cur_rec_datos_rpt CURSOR FOR SELECT id_expediente,
                                                           invadido,
                                                           asociado,
                                                           nss_restitucion,
                                                           aivs_sar92,
                                                           pesos_sar92,
                                                           Aivs_viv97,
                                                           pesos_viv97,
                                                           aivs_subsc,
                                                           pesos_subsc,
                                                           tipo_restitucion
                                                      FROM safre_tmp:tmp_sep_restitucion
                                                     WHERE 1 = 1
               LET v_contador = 1
               FOREACH cur_rec_datos_rpt INTO v_registros_restitucion.*
                  OUTPUT TO REPORT fn_rpt_restitucion(v_contador,v_registros_restitucion.*)
                  LET v_contador = v_contador + 1
               END FOREACH
            FINISH REPORT fn_rpt_restitucion
         ELSE
            DISPLAY "\n"
            DISPLAY "No fue posible generar el reporte"
            DISPLAY "\n"
         END IF
         
      ELSE
         DISPLAY "OCURRIÓ UN ERROR AL GENERAR INFORMACIÓN"
         DISPLAY "CÓDIGO DE ERROR:",v_sql_error
         DISPLAY "MENSAJE DE ERROR:",v_msg_error
         DISPLAY "\n"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                                RETURNING r_resultado_opera
         # si ocurrió un error con la actualizacion de la operacion operacion 
         # muestra el mensaje
         IF(r_resultado_opera)THEN
            CALL fn_desplega_inc_operacion(r_resultado_opera)
         END IF
         
      END IF
   ELSE
      DISPLAY "\n"
      DISPLAY "SIN SOLICITUDES DE RESTITUCIÓN A REGISTRAR"
      DISPLAY "\n"
      LET v_msg_correo = "SIN SOLICITUDES DE RESTITUCIÓN A REGISTRAR"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                             RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
          CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   END IF
      
   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          '',
                          'Solicitud de restitución al SSV',
                          v_msg_correo||
                          ''||
                          'ID Proceso   : '||p_pid||
                          'Proceso      : '||p_proceso_cod||
                          'Operacion    : '||p_opera_cod||
                          'Fecha Inicio : '||v_fecha_actual||
                          'Fecha Fin    : '||DATE
                          )
   
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP28                                                   #
#Descripcion       => Reporte de restitucion ssv                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 27, 2012                                           #
################################################################################
REPORT fn_rpt_restitucion(v_reg_rpt_restitucion)
DEFINE v_reg_rpt_restitucion RECORD
         v_contador         INTEGER,
         v_id_expediente 	VARCHAR(9),--DECIMAL(9,0),
         v_invadido 		CHAR(011),
         v_asociado 		CHAR(011),
         v_nss_restitucion  CHAR(011),
         v_aivs_sar92 		DECIMAL(22,2),
         v_pesos_sar92 		DECIMAL(22,2),
         v_Aivs_viv97 		DECIMAL(22,2),
         v_pesos_viv97 		DECIMAL(22,2),
         v_aivs_subsc 		DECIMAL(22,2),
         v_pesos_subsc 		DECIMAL(22,2),
         v_tipo_restitucion CHAR(40)
       END RECORD,
       v_pagina  SMALLINT,
       v_sum_aivs_sar92 	DECIMAL(22,2),
       v_sum_pesos_sar92 	DECIMAL(22,2),
       v_sum_Aivs_viv97 	DECIMAL(22,2),
       v_sum_pesos_viv97 	DECIMAL(22,2),
       v_sum_aivs_subsc 	DECIMAL(22,2),
       v_sum_pesos_subsc 	DECIMAL(22,2),
       v_folio_rpt STRING


   FORMAT

      FIRST PAGE HEADER
         PRINTX v_fecha_actual
         LET v_folio_rpt = p_folio USING "#########"
         PRINTX v_folio_rpt USING "#########"
         PRINTX v_total_procesados

      ON EVERY ROW
         PRINTX v_reg_rpt_restitucion.v_contador
         PRINTX v_reg_rpt_restitucion.v_id_expediente USING "#########"
         PRINTX v_reg_rpt_restitucion.v_invadido
         PRINTX v_reg_rpt_restitucion.v_asociado
         PRINTX v_reg_rpt_restitucion.v_nss_restitucion
         PRINTX v_reg_rpt_restitucion.v_aivs_sar92
         PRINTX v_reg_rpt_restitucion.v_pesos_sar92
         PRINTX v_reg_rpt_restitucion.v_Aivs_viv97
         PRINTX v_reg_rpt_restitucion.v_pesos_viv97
         PRINTX v_reg_rpt_restitucion.v_aivs_subsc
         PRINTX v_reg_rpt_restitucion.v_pesos_subsc
         PRINTX v_reg_rpt_restitucion.v_tipo_restitucion
         

      ON LAST ROW
         LET v_sum_aivs_sar92 	= SUM(v_reg_rpt_restitucion.v_aivs_sar92)
         LET v_sum_pesos_sar92 	= SUM(v_reg_rpt_restitucion.v_pesos_sar92)
         LET v_sum_Aivs_viv97 	= SUM(v_reg_rpt_restitucion.v_Aivs_viv97)
         LET v_sum_pesos_viv97 	= SUM(v_reg_rpt_restitucion.v_pesos_viv97)
         LET v_sum_aivs_subsc 	= SUM(v_reg_rpt_restitucion.v_aivs_subsc)
         LET v_sum_pesos_subsc 	= SUM(v_reg_rpt_restitucion.v_pesos_subsc)
         PRINT v_sum_aivs_sar92
         PRINT v_sum_pesos_sar92
         PRINT v_sum_Aivs_viv97
         PRINT v_sum_pesos_viv97
         PRINT v_sum_aivs_subsc
         PRINT v_sum_pesos_subsc
         

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina


END REPORT
