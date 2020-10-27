--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--===============================================================

####################################################################
#Modulo            =>SEPE                                          #
#Programa          =>SEPE11                                        #
#Objetivo          =>Programa de integracion de marca operacion 27 # 
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>11 MAYO    2012                               #
####################################################################
DATABASE safre_viv

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   DEFINE p_fec_ejecucion         DATE
   DEFINE v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_registros       INTEGER   

   DEFINE v_r_rpt         RECORD
             f_reporte        CHAR(10),
             nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
             folio            LIKE glo_ctr_archivo.folio,
             tot_parejas      INTEGER,
             tot_cambio_diag  INTEGER,
             tot_desmarca_rch INTEGER,
             tot_desmarca_dia INTEGER,
             tot_vueltas_marc INTEGER,
             tot_existe_virtual INTEGER,
             tot_sin_03_04      INTEGER,
             tot_apertura_cta   INTEGER,
             tot_diag_no_01     INTEGER,
             tot_misma_familia  INTEGER,
             tot_reactivados    INTEGER
          END RECORD  
   DEFINE v_r_reporte_det DYNAMIC ARRAY OF RECORD
             tipo_ocurrencia       SMALLINT,
             desc_tipo             CHAR(70),
             id_det_02_op27        LIKE sep_det_02_op27.id_det_02_op27,
             invadido              LIKE sep_det_02_op27.invadido,
             asociado              LIKE sep_det_03_op27.asociado,
             diag_confronta        LIKE sep_det_02_op27.diag_confronta,
             clasifica_separacion  LIKE sep_det_02_op27.clasifica_separacion,
             clasifica_historico   LIKE sep_det_02_op27.clasifica_separacion
          END RECORD

   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_pos              INTEGER
   DEFINE v_tot_det          INTEGER
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_archivo      = ARG_VAL(6) -- archivo procesado
  
   WHENEVER ERROR STOP
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPI13.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPE13-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   SELECT nombre_archivo
     INTO p_v_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = p_i_proceso_cod
      AND opera_cod   = 1 # operacion de carga
      AND estado = 2 # estado integrado
      AND nombre_archivo = p_v_archivo      
      --AND opera_cod   = p_i_opera_cod
   IF LENGTH(p_v_archivo CLIPPED) = 0 THEN
      DISPLAY "No existe archivo para el proceso de integración"
   
   ELSE
         --tipo de ocurrencia
         -- 1 = cambio de diagnóstico
         -- 2 = desmarcados por rechazo procesar
         -- 3 = desmarcados por diagnóstico improcedente
         -- 4 = vueltos a marcar
         -- 5 = rechazado ya existe nss virtual 
         -- 6 = rechazado requisito 03 04
         -- 7 = rechazado error al aperturar cuenta
         -- 8 = rechazado diagnostico distinto de 01
         -- 9 = Separacion debido a una unificacion, No corresponde a la misma familia
         -- 10 = Detalle de NSS reactivados posterior a una Unificación."
         
         LET v_s_qryTxt = "SELECT unique tipo_ocurrencia, ",
                                 "CASE tipo_ocurrencia ",
                                 " WHEN 1 THEN  'Cambio de diagnóstico'",
                                 " WHEN 2 THEN  'Desmarcados por rechazo procesar'",
                                 " WHEN 3 THEN  'Desmarcados por diagnóstico improcedente'",
                                 " WHEN 4 THEN  'Vueltos a marcar'",
                                 " WHEN 5 THEN  'Rechazado ya existe nss virtual'", 
                                 " WHEN 6 THEN  'Rechazado requisito 03 04'",
                                 " WHEN 7 THEN  'Rechazado error al aperturar cuenta'",
                                 " WHEN 8 THEN  'Rechazado diagnostico distinto de 01'",
                                 " WHEN 9 THEN  'Rechazo nss no pertenecen a la misma familia en Unificacion'",
                                 " WHEN 10 THEN 'Nss Reactivados posterior a una Unificacion'",
                                 "END CASE, ",
                                 " det02.id_det_02_op27, invadido, asociado, ",
                                 " det02.diag_confronta, clasifica_separacion_ant, ",
                                 " clasifica_separacion_nva",
                          "  FROM sep_det_02_op27 det02, ",
                          "       OUTER sep_det_03_op27 det03, ",
                          "       safre_tmp:tmp_sep_integrados inte ",
                          " WHERE det02.id_det_02_op27 = det03.id_det_02_op27 ",
                          "   AND det02.id_det_02_op27 = inte.id_det_02_op27 ",
                          " ORDER BY 1,3 "                          
         PREPARE EnuArrRchMarca FROM v_s_qryTxt
         DECLARE CurArrRchMarca CURSOR FOR EnuArrRchMarca
         
         LET v_pos = 1
         CALL v_r_reporte_det.clear()
         
         FOREACH CurArrRchMarca INTO v_r_reporte_det[v_pos].*
--DISPLAY v_pos         
--DISPLAY    v_r_reporte_det[v_pos].id_det_02_op27
            LET v_pos = v_pos + 1
         END FOREACH
         
         IF LENGTH(v_r_reporte_det[v_r_reporte_det.getLength()].tipo_ocurrencia CLIPPED) = 0 THEN
            CALL v_r_reporte_det.deleteElement(v_r_reporte_det.getLength())
         END IF

--         IF(v_r_reporte_det.getLength() > 0)THEN
            ---- recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("sep") RETURNING r_ruta_bin, r_ruta_listados
            
            -- se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/SEPI131.4rp") THEN
               -- se indica la salida del reporte
               CALL fgl_report_selectDevice("PDF") 
            
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
          
               -- sin indica que no es necesario el preview
               CALL fgl_report_selectPreview(0)
         
               -- se asigna la configuración en el menejo del reporte
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               -- inicia el reporte de registros procesados
               START REPORT rpt_solicitudes_diag_op27 TO XML HANDLER v_manejador_rpt
         
               LET v_day = DAY(TODAY) USING "&&"
               LET v_mes = MONTH(TODAY) USING "&&"
               LET v_ano = YEAR(TODAY) USING "&&&&"
               LET v_r_rpt.f_reporte = v_day,"-",v_mes,"-",v_ano

               LET v_r_rpt.nombre_archivo = p_v_archivo
               LET v_r_rpt.folio          = p_d_folio
         
               LET v_tot_det = 0
               LET v_r_rpt.tot_parejas = 0
         
               SELECT NVL(COUNT(*),0)
                 INTO v_tot_det 
                 FROM sep_det_02_op27
                WHERE folio = p_d_folio
    
               -- Acumulado de parejas det02
               LET v_r_rpt.tot_parejas = v_r_rpt.tot_parejas + v_tot_det
         
               SELECT NVL(COUNT(*),0)
                 INTO v_tot_det 
                 FROM sep_det_03_op27 det03
                WHERE det03.id_det_02_op27 IN 
               (SELECT det02.id_det_02_op27
                 FROM sep_det_02_op27 det02
                WHERE det02.folio = p_d_folio)

               -- Acumulado de parejas det03
               LET v_r_rpt.tot_parejas = v_r_rpt.tot_parejas + v_tot_det
               LET v_r_rpt.tot_parejas = v_r_rpt.tot_parejas / 2               
               LET v_r_rpt.tot_cambio_diag  = 0
               LET v_r_rpt.tot_desmarca_rch = 0
               LET v_r_rpt.tot_desmarca_dia = 0
               LET v_r_rpt.tot_vueltas_marc = 0
               LET v_r_rpt.tot_existe_virtual  = 0
               LET v_r_rpt.tot_sin_03_04       = 0 
               LET v_r_rpt.tot_apertura_cta    = 0
               LET v_r_rpt.tot_diag_no_01      = 0
               LET v_r_rpt.tot_misma_familia   = 0
               LET v_r_rpt.tot_reactivados     = 0
         
               LET v_s_qryTxt = "SELECT NVL(SUM(CASE WHEN tipo_ocurrencia = 1 THEN 1 ELSE 0 END),0),",
                                "       NVL(SUM(CASE WHEN tipo_ocurrencia = 2 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 3 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 4 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 5 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 6 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 7 THEN 1 ELSE 0 END),0),",
       	                        "       NVL(SUM(CASE WHEN tipo_ocurrencia = 8 THEN 1 ELSE 0 END),0),",
                                "       NVL(SUM(CASE WHEN tipo_ocurrencia = 9 THEN 1 ELSE 0 END),0),",
                                "       NVL(SUM(CASE WHEN tipo_ocurrencia = 10 THEN 1 ELSE 0 END),0)",
                                "  FROM safre_tmp:tmp_sep_integrados"
                                

               PREPARE EnuTotDetOP27 FROM  v_s_qryTxt
               DECLARE CurToDet CURSOR FOR EnuTotDetOP27
               OPEN CurToDet 
               FETCH CurToDet INTO  v_r_rpt.tot_cambio_diag, 
                                    v_r_rpt.tot_desmarca_rch, 
                                    v_r_rpt.tot_desmarca_dia,
                                    v_r_rpt.tot_vueltas_marc,
                                    v_r_rpt.tot_existe_virtual,
                                    v_r_rpt.tot_sin_03_04,
                                    v_r_rpt.tot_apertura_cta,
                                    v_r_rpt.tot_diag_no_01,
                                    v_r_rpt.tot_misma_familia,
                                    v_r_rpt.tot_reactivados
              CLOSE CurToDet
              FREE CurToDet
              FREE EnuTotDetOP27

--DISPLAY "tipo 6: ",v_r_rpt.tot_sin_03_04

               IF v_r_reporte_det.getLength() = 0 THEN
                  LET v_registros = 1
               ELSE
                  LET v_registros = v_r_reporte_det.getLength()
               END IF
         
               --FOR v_pos_rep_det = 1 TO v_r_reporte_det.getLength()
               --DISPLAY "IMPRIMIENDO REPORTE"
               --DISPLAY "REGISTROS : ",v_registros
               
               FOR v_pos_rep_det = 1 TO v_registros

                  --LET v_r_reporte_det[v_pos_rep_det].tipo_ocurrencia = 4
                  --LET v_r_reporte_det[v_pos_rep_det].desc_tipo = "1234"
                  --LET v_r_rpt.tot_misma_familia = 0
                {
                  DISPLAY "TIPO: ",v_r_reporte_det[v_pos_rep_det].tipo_ocurrencia
                  DISPLAY "OCURRENCIA :",v_r_reporte_det[v_pos_rep_det].desc_tipo
                  DISPLAY "ID :",v_r_reporte_det[v_pos_rep_det].id_det_02_op27
                  DISPLAY "INVADIDO :",v_r_reporte_det[v_pos_rep_det].invadido
                  DISPLAY "ASOCIADO :",v_r_reporte_det[v_pos_rep_det].asociado
                  DISPLAY "DIAGNOSTICO :",v_r_reporte_det[v_pos_rep_det].diag_confronta
                  DISPLAY "CLASIFICA SEP :",v_r_reporte_det[v_pos_rep_det].clasifica_separacion
                  DISPLAY "CLASIFICA HIS :",v_r_reporte_det[v_pos_rep_det].clasifica_historico
                 } 

                 --EN EL REPORTE EL VALOR tot_cambio_diag está cuchareado, favor de revisar
                 LET v_r_rpt.tot_cambio_diag = v_r_rpt.tot_cambio_diag + 1 
                  
                  OUTPUT TO REPORT rpt_solicitudes_diag_op27(v_r_rpt.*,
                                                        v_r_reporte_det[v_pos_rep_det].*)
               END FOR
         
               -- finaliza el reporte
               FINISH REPORT rpt_solicitudes_diag_op27
            ELSE
               DISPLAY "no fue posible generar el reporte"
               {CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida
         
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
                  EXIT PROGRAM
               END IF}
            END IF
--         ELSE
--            DISPLAY "\nNo hay información para generar reporte"
--         END IF
         
   END IF
   
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI13                                                   #
#Objetivo          => Reporte de diagnostico operacion 27                      #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 01/06/2012                                               #
################################################################################
REPORT rpt_solicitudes_diag_op27(v_det_operacion,v_detalle_diag_op27)
DEFINE v_det_operacion RECORD
             f_reporte        CHAR(10),
             nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
             folio            LIKE glo_ctr_archivo.folio,
             tot_parejas      INTEGER,
             tot_cambio_diag  INTEGER,
             tot_desmarca_rch INTEGER,
             tot_desmarca_dia INTEGER,
             tot_vueltas_marc INTEGER,
             tot_existe_virtual INTEGER,
             tot_sin_03_04      INTEGER,
             tot_apertura_cta   INTEGER,
             tot_diag_no_01     INTEGER,
             tot_misma_familia  INTEGER,
             tot_reactivados    INTEGER
       END RECORD,
       v_detalle_diag_op27 RECORD
       	     tipo_ocurrencia       SMALLINT,
             desc_tipo             CHAR(70),
             id_det_02_op27        INTEGER,--LIKE sep_det_02_op27.id_det_02_op27,
             invadido              LIKE sep_det_02_op27.invadido,
             asociado              LIKE sep_det_03_op27.asociado,
             diag_confronta        LIKE sep_det_02_op27.diag_confronta,
             clasifica_separacion  LIKE sep_det_02_op27.clasifica_separacion,
             clasifica_historico   LIKE sep_det_02_op27.clasifica_separacion
       END RECORD,
       v_tot_gpo         INTEGER,
       v_pagina          SMALLINT

   FORMAT

      FIRST PAGE HEADER
         PRINTX v_det_operacion.f_reporte,
                v_det_operacion.nombre_archivo,
                v_det_operacion.folio,
                v_det_operacion.tot_parejas,
                v_det_operacion.tot_cambio_diag,
                v_det_operacion.tot_desmarca_rch,
                v_det_operacion.tot_desmarca_dia,
                v_det_operacion.tot_vueltas_marc,
                v_det_operacion.tot_existe_virtual,
                v_det_operacion.tot_sin_03_04,
                v_det_operacion.tot_apertura_cta,
                v_det_operacion.tot_diag_no_01,
                v_det_operacion.tot_misma_familia,
                v_det_operacion.tot_reactivados     
         LET v_tot_gpo = 0
         
      BEFORE GROUP OF v_detalle_diag_op27.desc_tipo
         PRINTX v_detalle_diag_op27.desc_tipo

      AFTER GROUP OF v_detalle_diag_op27.desc_tipo
         PRINTX v_tot_gpo
         
      ON EVERY ROW

         PRINTX v_detalle_diag_op27.tipo_ocurrencia     ,
                v_detalle_diag_op27.id_det_02_op27      ,
                v_detalle_diag_op27.invadido            ,
                v_detalle_diag_op27.asociado            ,
                v_detalle_diag_op27.diag_confronta      ,
                v_detalle_diag_op27.clasifica_separacion,
                v_detalle_diag_op27.clasifica_historico 
         
                LET v_tot_gpo = v_tot_gpo + 1

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT