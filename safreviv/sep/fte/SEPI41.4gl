--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI41                                                   #
#Objetivo          => Reporte de integración op 29                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 19, 2012                                           #
################################################################################
DATABASE safre_viv
DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       INTEGER,--DECIMAL(9,0),--LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,  # nombre del archivo a integrar
       v_encabezado  RECORD 
         v_fecha            STRING,
         v_archivo          STRING,
         v_folio            INTEGER,--DECIMAL(9,0),--LIKE glo_folio.folio,
         v_total_archivo    INTEGER,
         v_total_rechazadas INTEGER,
         v_total_aceptadas  INTEGER
       END RECORD,
       v_detalle_rpt DYNAMIC ARRAY OF RECORD
         v_consecutivo INTEGER,
         v_invadido    LIKE sep_det_02_op29.invadido,
         v_nrp_inv     LIKE sep_det_05_op29.nrp,
         v_asociado    LIKE sep_det_03_op29.asociado,
         v_nrp_asc     LIKE sep_det_06_op29.nrp,
         v_total       INTEGER,
         v_tipo        SMALLINT
       END RECORD

MAIN

   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   CALL fn_recupera_informacion()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI41                                                   #
#Descripcion       => Recupera informacion para reporte                        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 19, 2012                                           #
################################################################################
FUNCTION fn_recupera_informacion()
DEFINE v_fecha_actual STRING,
       v_consulta     STRING,
       v_detalle RECORD
         v_invadido LIKE sep_det_02_op29.invadido,
         v_nrp_inv  LIKE sep_det_05_op29.nrp,
         v_asociado LIKE sep_det_03_op29.asociado,
         v_nrp_asc  LIKE sep_det_06_op29.nrp,
         v_total    INTEGER
       END RECORD,
       v_indice   INTEGER,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_nom_reporte     STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_indice_aux      INTEGER
       

   INITIALIZE v_encabezado.* TO NULL
   LET v_fecha_actual = YEAR(TODAY)
   LET v_fecha_actual = MONTH(TODAY) USING "&&","-" CLIPPED,
                        DAY(TODAY)   USING "&&","-" CLIPPED,
                        v_fecha_actual.trim()
   
   LET v_encabezado.v_fecha   = v_fecha_actual
   LET v_encabezado.v_archivo = p_nom_archivo
   LET v_encabezado.v_folio   = p_folio

   # Recuepra el total de parejas procesadas para el reporte
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM sep_det_02_op29 det2 JOIN sep_det_03_op29 det3",
                    "\n     ON det3.id_det_02_op29 = det2.id_det_02_op29",
                    "\n  WHERE det2.folio = ?"
   PREPARE prp_rec_tot_parejas FROM v_consulta 
   EXECUTE prp_rec_tot_parejas USING p_folio INTO v_encabezado.v_total_archivo

   # Recupera el total de parejas rechazadas
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM sep_det_02_op29",
                    "\n  WHERE folio = ?",
                    "\n    AND estado = 10" # Pareja Rechazada

   PREPARE prp_rec_rechazadas FROM v_consulta
   EXECUTE prp_rec_rechazadas USING p_folio INTO v_encabezado.v_total_rechazadas

   # Recupera el total de parejas aceptadas
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM sep_det_02_op29",
                    "\n  WHERE folio = ?",
                    "\n    AND estado = 15" # Pareja Aceptadas

   PREPARE prp_rec_aceptadas FROM v_consulta
   EXECUTE prp_rec_aceptadas USING p_folio INTO v_encabezado.v_total_aceptadas

   # Detalle integracion op 29 Rechazadas
   LET v_consulta = "\n SELECT det2.invadido, det5.nrp,",
                    "\n        det3.asociado, det6.nrp, COUNT(det2.id_det_02_op29)",
                    "\n   FROM sep_det_02_op29 det2 JOIN sep_det_05_op29 det5",
                    "\n     ON det5.id_det_02_op29 = det2.id_det_02_op29",
                    "\n        JOIN sep_det_03_op29 det3",
                    "\n     ON det3.id_det_02_op29 = det2.id_det_02_op29",
                    "\n        JOIN sep_det_06_op29 det6",
                    "\n     ON det6.id_det_03_op29 = det3.id_det_03_op29",
                    "\n  WHERE det2.folio = ?",
                    "\n    AND det2.estado = 10", # rechazadas
                    "\n  GROUP BY 1,2,3,4"

   PREPARE prp_rec_det_rechazas FROM v_consulta
   DECLARE cur_rec_det_rechazas CURSOR FOR prp_rec_det_rechazas
   LET v_indice = 1
   FOREACH cur_rec_det_rechazas USING p_folio INTO v_detalle.*

      LET v_detalle_rpt[v_indice].v_consecutivo = v_indice
      LET v_detalle_rpt[v_indice].v_invadido    = v_detalle.v_invadido
      LET v_detalle_rpt[v_indice].v_nrp_inv     = v_detalle.v_nrp_inv
      LET v_detalle_rpt[v_indice].v_asociado    = v_detalle.v_asociado
      LET v_detalle_rpt[v_indice].v_nrp_asc     = v_detalle.v_nrp_asc
      LET v_detalle_rpt[v_indice].v_total       = v_detalle.v_total
      LET v_detalle_rpt[v_indice].v_tipo        = 1 # identificado para ubicar registro en tabla de rechazadas

      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_rec_det_rechazas


   # Detalle integracion op 29 Aceptadas
   LET v_consulta = "\n SELECT det2.invadido, det5.nrp,",
                    "\n        det3.asociado, det6.nrp, COUNT(det2.id_det_02_op29)",
                    "\n   FROM sep_det_02_op29 det2 JOIN sep_det_05_op29 det5",
                    "\n     ON det5.id_det_02_op29 = det2.id_det_02_op29",
                    "\n        JOIN sep_det_03_op29 det3",
                    "\n     ON det3.id_det_02_op29 = det2.id_det_02_op29",
                    "\n        JOIN sep_det_06_op29 det6",
                    "\n     ON det6.id_det_03_op29 = det3.id_det_03_op29",
                    "\n  WHERE det2.folio = ?",
                    "\n    AND det2.estado = 15", # Aceptadas
                    "\n  GROUP BY 1,2,3,4"

   LET v_indice = v_detalle_rpt.getLength() + 1
   LET v_indice_aux = 1
   PREPARE prp_rec_det_aceptadas FROM v_consulta
   DECLARE cur_rec_det_aceptadas CURSOR FOR prp_rec_det_aceptadas
   FOREACH cur_rec_det_aceptadas USING p_folio INTO v_detalle.*

      LET v_detalle_rpt[v_indice].v_consecutivo = v_indice_aux
      LET v_detalle_rpt[v_indice].v_invadido    = v_detalle.v_invadido
      LET v_detalle_rpt[v_indice].v_nrp_inv     = v_detalle.v_nrp_inv
      LET v_detalle_rpt[v_indice].v_asociado    = v_detalle.v_asociado
      LET v_detalle_rpt[v_indice].v_nrp_asc     = v_detalle.v_nrp_asc
      LET v_detalle_rpt[v_indice].v_total       = v_detalle.v_total
      LET v_detalle_rpt[v_indice].v_tipo        = 2 # identificado para ubicar registro en tabla de aceptadas

      LET v_indice = v_indice + 1
      LET v_indice_aux = v_indice_aux + 1

   END FOREACH
   FREE cur_rec_det_aceptadas

   #******** CREACIÓN DE REPORTE **********
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPI411.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario CLIPPED, "-SEPL41-", 
                          p_pid USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_genera_rpt_integracion_op29 TO XML HANDLER v_manejador_rpt
         FOR v_indice = 1 TO v_detalle_rpt.getLength()
            OUTPUT TO REPORT fn_genera_rpt_integracion_op29(v_detalle_rpt[v_indice].*)
         END FOR
      FINISH REPORT fn_genera_rpt_integracion_op29
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI41                                                   #
#Descripcion       => Reporte de integracion op29                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 19, 2012                                           #
################################################################################
REPORT fn_genera_rpt_integracion_op29(v_det_rpt)
DEFINE v_pagina SMALLINT,
       v_det_rpt  RECORD
         v_consecutivo INTEGER,
         v_invadido    LIKE sep_det_02_op29.invadido,
         v_nrp_inv     LIKE sep_det_05_op29.nrp,
         v_asociado    LIKE sep_det_03_op29.asociado,
         v_nrp_asc     LIKE sep_det_06_op29.nrp,
         v_total       INTEGER,
         v_tipo        SMALLINT
       END RECORD
   
   FORMAT

      FIRST PAGE HEADER
         PRINTX v_encabezado.*
         --DISPLAY v_encabezado.v_folio 

      BEFORE GROUP OF v_det_rpt.v_tipo

      ON EVERY ROW
         PRINTX v_det_rpt.*

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT