--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23/03/2015
--==============================================================================

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTI02                                                   #
#Objetivo          => Genera reporte de integración traspasos receptora        # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 23 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"


DEFINE p_pid                  LIKE bat_ctr_proceso.pid,     
       p_proceso_cod          LIKE cat_proceso.proceso_cod, 
       p_opera_cod LIKE cat_operacion.opera_cod, 
       p_usuario_cod          LIKE seg_usuario.usuario_cod, 
       p_folio                LIKE glo_folio.folio,
       p_lanzador             LIKE glo_ctr_archivo.nombre_archivo,
       r_ruta_lst             LIKE seg_modulo.ruta_listados,
       r_ruta_ejecutable      LIKE seg_modulo.ruta_bin,
       v_nom_reporte          STRING,
       v_manejador_rpt        OM.SaxDocumentHandler

MAIN
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_lanzador    = ARG_VAL(6)

   CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, 
                                  r_ruta_lst

   CALL fn_recupera_registros()
   
END MAIN

# Descripción: 
FUNCTION fn_recupera_registros()
DEFINE v_consulta         STRING,
       v_total_procesados INTEGER,
       v_integrados RECORD
         v_tpo_operacion_desc VARCHAR(50),
         v_resultado          VARCHAR(40),
         v_motivo             VARCHAR(100),
         v_registros          INTEGER,
         v_monto_pesos        DECIMAL(22,2)
       END RECORD,
       v_tabla STRING
       

   # CREACIÓN DE REPORTE
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/PRTI021.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-",
                          p_lanzador CLIPPED ,"-", 
                          p_pid         USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod   USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_rpt_integracion_trasp_recept TO XML HANDLER v_manejador_rpt
      
      DISPLAY "p_proceso_cod:",p_proceso_cod
      
               LET v_tabla = "   FROM safre_tmp:tmp_prt_integrados tmp"
               SELECT COUNT(*)
                 INTO v_total_procesados
                 FROM safre_tmp:tmp_prt_integrados
                WHERE 1 = 1
                
         LET v_consulta = " SELECT CASE tmp.tpo_operacion WHEN '01' THEN 'TRANSFERENCIA TOTAL' WHEN '02' THEN 'TRANSFERENCIA PARCIAL' WHEN '03' THEN 'DEVOLUCIÓN' ELSE 'N/C' END CASE,",
                          "        CASE tmp.rechazado WHEN 1 THEN 'RECHAZADO' ELSE 'ACEPTADO' END CASE,",
                          "        cat.rechazo_desc,",
                          "        COUNT(tmp.nss),",
                          "        SUM(monto_pesos)",                          
                          v_tabla,
                          "        LEFT OUTER JOIN prt_cat_rch_contingente cat",
                          "     ON tmp.motivo_rechazo = cat.diagnostico",
                          "  WHERE 1 = 1",
                          "  GROUP BY 1,2,3"
         --DISPLAY v_consulta
         PREPARE prp_recupera_reg FROM v_consulta
         DECLARE cur_recupera_reg CURSOR FOR prp_recupera_reg
         FOREACH cur_recupera_reg INTO v_integrados.*
            OUTPUT TO REPORT fn_rpt_integracion_trasp_recept(p_folio,v_total_procesados,v_integrados.*)
         END FOREACH
         FREE cur_recupera_reg
         
      FINISH REPORT fn_rpt_integracion_trasp_recept
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

# Descripción: Genera reporte de integración
REPORT fn_rpt_integracion_trasp_recept(p_folio,
                                       p_total_procesados,
                                       p_integrados)
DEFINE p_folio      LIKE glo_folio.folio,
       p_total_procesados INTEGER,
       p_integrados RECORD
         v_tpo_operacion_desc VARCHAR(50),
         v_resultado          VARCHAR(40),
         v_motivo             VARCHAR(100),
         v_registros          INTEGER,
         v_monto_pesos        DECIMAL(22,2)
       END RECORD,
       v_pagina       SMALLINT,
       v_fecha_actual DATE,
       v_titulo_rpt   STRING
       
   FORMAT

      FIRST PAGE HEADER
         LET v_titulo_rpt = "REPORTE INTEGRACIÓN RECHAZOS CONTINGENTE SAR 92"
         LET v_fecha_actual = TODAY
         PRINTX v_titulo_rpt,
                p_folio,
                p_total_procesados,
                v_fecha_actual USING "dd-mm-yyyy"
         
      ON EVERY ROW
         PRINTX p_integrados.*
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT
