DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
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
DEFINE 
       v_sql_error          INTEGER,
       v_msg_error          VARCHAR(200),
       v_msg_correo         STRING,
       r_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
       r_ruta_lst           LIKE seg_modulo.ruta_listados,
       v_nom_reporte        STRING,
       v_contador           INTEGER

   # Se recuperan los parámetros
   --LET p_usuario_cod = ARG_VAL(1)
   --LET p_pid         = ARG_VAL(2)
   --LET p_proceso_cod = ARG_VAL(3)
   --LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(1)

   --WHENEVER ERROR CONTINUE
   LET v_total_procesados = 0
   LET v_msg_correo = ' ' 
   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   LET v_fecha_actual = MONTH(TODAY) USING "&&" CLIPPED,"-",
                        DAY(TODAY) USING "&&" CLIPPED,"-",
                        v_fecha_actual.trim()
   LET v_fecha_actual = "10-15-2015"


         ### CREACIÓN DE REPORTE
         CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   
         IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPP282.4rp"))THEN
            # Salida del reporte
            CALL fgl_report_selectDevice("PDF")
            LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPP28-", 
                                p_pid USING "&&&&&", "-", 
                                p_proceso_cod USING "&&&&&", "-", 
                                p_opera_cod USING "&&&&&"
                          
            # ruta de salida del reporte
            CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
            # Indica que no hay previsualizacion
            CALL fgl_report_selectPreview(1)
                  
            # se asigna la configuración en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
            SELECT 
               COUNT(*) 
               INTO v_total_procesados
               FROM safre_tmp:tmp_sep_restitucion_alt
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
                                                      FROM safre_tmp:tmp_sep_restitucion_alt
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
            DISPLAY "OCURRIÓ UN ERROR AL GENERAR INFORMACIÓN"
            DISPLAY "CÓDIGO DE ERROR:",v_sql_error
            DISPLAY "MENSAJE DE ERROR:",v_msg_error
            DISPLAY "\n"
         # si ocurrió un error con la actualizacion de la operacion operacion 
         # muestra el mensaje
         END IF

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
