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
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE p_fec_ejecucion         DATE
   DEFINE v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo

   DEFINE v_r_rpt         RECORD
             f_reporte        CHAR(10),
             nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
             folio            LIKE glo_ctr_archivo.folio,
             tot_parejas      INTEGER,
             tot_desmarca_rch INTEGER,
             tot_desmarca_ace INTEGER
          END RECORD  
   DEFINE v_r_reporte_det_agr DYNAMIC ARRAY OF RECORD
             v_gpo                 SMALLINT,
             v_clasificacion       CHAR(1),
             v_invadido            STRING,
             v_asociado            STRING,
             v_saldo92_invadido    DECIMAL(16,6),
             v_saldo92_asociado    DECIMAL(16,6),
             v_saldo97_invadido    DECIMAL(16,6),
             v_saldo97_asociado    DECIMAL(16,6),
             v_saldo92             DECIMAL(16,6),
             v_saldo97             DECIMAL(16,6),
             v_total_parejas       STRING
          END RECORD
   DEFINE v_saldos_separar_aux RECORD
   	         gpo                  SMALLINT,
             clasifica_separacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
             total_inv_92         DECIMAL(16,6),
             total_aso_92         DECIMAL(16,6),
             total_inv_97         DECIMAL(16,6),
             total_aso_97         DECIMAL(16,6),
             v_total              INTEGER
          END RECORD
   DEFINE v_saldos_separar_aux2 RECORD
   	         gpo                  SMALLINT,
             clasifica_separacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
             invadido             CHAR(11),
             asociado             CHAR(11),
             total_inv_92         DECIMAL(16,6),
             total_aso_92         DECIMAL(16,6),
             total_inv_97         DECIMAL(16,6),
             total_aso_97         DECIMAL(16,6)
          END RECORD
   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_tot_det          INTEGER
   DEFINE v_consulta         STRING
   DEFINE v_indice           INTEGER
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_archivo = ARG_VAL(6) -- archivo procesado
  
   --WHENEVER ERROR STOP
   
   -- se crear el archivo log
   --CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPI31.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPL31-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   {SELECT nombre_archivo
     INTO p_v_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod
   
   IF LENGTH(p_v_archivo CLIPPED) = 0 THEN
      DISPLAY "No existe archivo para el proceso de integración"
   
   ELSE}
   
         
         ---- recupera la ruta de listados en el que se enviara el archivo
         CALL fn_rutas("sep") RETURNING r_ruta_bin, r_ruta_listados
         --DISPLAY "Ruta bin - ", r_ruta_bin
         --DISPLAY "Ruta lst - ", r_ruta_listados

         -- se indica que el reporte usara la plantilla creada
         IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/SEPI311.4rp") THEN
            -- se indica la salida del reporte
            CALL fgl_report_selectDevice("PDF") 
            
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
         
            -- sin indica que no es necesario el preview
            CALL fgl_report_selectPreview(0)
         
            -- se asigna la configuración en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
         ELSE
            DISPLAY "no fue posible generar el reporte"
            CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida
         
            IF(r_b_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
               EXIT PROGRAM
            END IF
         END IF
         
         -- inicia el reporte de registros procesados
         START REPORT rpt_solicitudes_op28 TO XML HANDLER v_manejador_rpt
         
         LET v_day = DAY(TODAY) USING "&&"
         LET v_mes = MONTH(TODAY) USING "&&"
         LET v_ano = YEAR(TODAY) USING "&&&&"
         LET v_r_rpt.f_reporte = v_day,"-",v_mes,"-",v_ano
         LET v_r_rpt.nombre_archivo = p_v_archivo
         LET v_r_rpt.folio          = p_d_folio
        --DISPLAY "folio = ",p_d_folio 
         LET v_tot_det = 0
         LET v_r_rpt.tot_parejas = 0
         LET v_r_rpt.tot_desmarca_rch = 0
         LET v_r_rpt.tot_desmarca_ace = 0

         LET v_indice = 1
         CALL v_r_reporte_det_agr.clear()
         
         LET v_consulta = " SELECT case estado when 10 THEN 1 ELSE 2 end case, ",
                          " clasifica_separacion,",
                          "        sum(NVL(det02.saldo_viv_92,0)),",
                          "        sum(NVL(det02.saldo_viv_97,0)),",
                          "        sum(NVL(det03.saldo_viv_92,0)),",
                          "        sum(NVL(det03.saldo_viv_97,0)),",
                          "        count(*)",
                          "   FROM sep_det_02_op28 det02,",
                          "        sep_det_03_op28 det03",
                          "  WHERE det02.id_det_02_op28 = det03.id_det_02_op28",
                          "  AND   det02.folio = ",p_d_folio,
                          "  GROUP BY 1,2",
                          "  ORDER BY 1,2"
         --DISPLAY "Acumulado integra op28: ",v_consulta
         PREPARE prp_recupera_posibles_cuentas FROM v_consulta
         DECLARE cur_recupera_posibles_cuentas CURSOR FOR prp_recupera_posibles_cuentas
         FOREACH cur_recupera_posibles_cuentas INTO v_saldos_separar_aux.*
      
            LET v_r_reporte_det_agr[v_indice].v_gpo              = v_saldos_separar_aux.gpo
            LET v_r_reporte_det_agr[v_indice].v_clasificacion    = v_saldos_separar_aux.clasifica_separacion
            LET v_r_reporte_det_agr[v_indice].v_saldo92_invadido = v_saldos_separar_aux.total_inv_92 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97_invadido = v_saldos_separar_aux.total_aso_92 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo92_asociado = v_saldos_separar_aux.total_inv_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97_asociado = v_saldos_separar_aux.total_aso_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo92          = v_saldos_separar_aux.total_inv_92+v_saldos_separar_aux.total_inv_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97          = v_saldos_separar_aux.total_aso_92+v_saldos_separar_aux.total_aso_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_total_parejas    = v_saldos_separar_aux.v_total
      
            LET v_r_rpt.tot_parejas      = v_r_rpt.tot_parejas + v_saldos_separar_aux.v_total
            IF v_saldos_separar_aux.gpo = 1 THEN -- Estado Aceptado
            	 LET v_r_rpt.tot_desmarca_ace = v_r_rpt.tot_desmarca_ace + v_saldos_separar_aux.v_total
            ELSE
            	 IF v_saldos_separar_aux.gpo = 2 THEN -- Estado Rechazado
                  LET v_r_rpt.tot_desmarca_rch = v_r_rpt.tot_desmarca_rch + v_saldos_separar_aux.v_total
               END IF
            END IF
      
            LET v_indice = v_indice + 1
         END FOREACH
      
         FREE cur_recupera_posibles_cuentas
            
         LET v_consulta = " SELECT case estado when 10 THEN 3 ELSE 4 end case, clasifica_separacion, invadido, asociado,",
                          "        NVL(det02.saldo_viv_92,0),",
                          "        NVL(det03.saldo_viv_92,0),",
                          "        NVL(det02.saldo_viv_97,0),",
                          "        NVL(det03.saldo_viv_97,0)",
                          "   FROM sep_det_02_op28 det02,",
                          "        sep_det_03_op28 det03",
                          "  WHERE det02.id_det_02_op28 = det03.id_det_02_op28 ",
                          "  AND   det02.folio = ",p_d_folio,
                          "  ORDER BY 1,2,3 "

         PREPARE prp_recupera_posibles_cuentas2 FROM v_consulta
         DECLARE cur_recupera_posibles_cuentas2 CURSOR FOR prp_recupera_posibles_cuentas2
         FOREACH cur_recupera_posibles_cuentas2 INTO v_saldos_separar_aux2.*
      
            LET v_r_reporte_det_agr[v_indice].v_gpo              = v_saldos_separar_aux2.gpo
            LET v_r_reporte_det_agr[v_indice].v_clasificacion    = v_saldos_separar_aux2.clasifica_separacion
            LET v_r_reporte_det_agr[v_indice].v_invadido         = v_saldos_separar_aux2.invadido
            LET v_r_reporte_det_agr[v_indice].v_asociado         = v_saldos_separar_aux2.asociado
            LET v_r_reporte_det_agr[v_indice].v_saldo92_invadido = v_saldos_separar_aux2.total_inv_92 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo92_asociado = v_saldos_separar_aux2.total_aso_92 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97_invadido = v_saldos_separar_aux2.total_inv_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97_asociado = v_saldos_separar_aux2.total_aso_97 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo92          = v_saldos_separar_aux2.total_inv_92+v_saldos_separar_aux2.total_aso_92 USING '########.##'
            LET v_r_reporte_det_agr[v_indice].v_saldo97          = v_saldos_separar_aux2.total_inv_97+v_saldos_separar_aux2.total_aso_97 USING '########.##'
      
            LET v_indice = v_indice + 1
         END FOREACH
      
         IF v_r_reporte_det_agr.getLength() > 0 THEN
            IF LENGTH(v_r_reporte_det_agr[v_r_reporte_det_agr.getLength()].v_clasificacion CLIPPED) = 0 THEN
            	  CALL v_r_reporte_det_agr.DeleteElement(v_r_reporte_det_agr.getLength())
            END IF
         END IF

         FREE cur_recupera_posibles_cuentas2
            
         FOR v_pos_rep_det = 1 TO v_r_reporte_det_agr.getLength()
            OUTPUT TO REPORT rpt_solicitudes_op28(v_r_rpt.*,
                                                  v_r_reporte_det_agr[v_pos_rep_det].*)
         END FOR
         
         -- finaliza el reporte
         FINISH REPORT rpt_solicitudes_op28
         
   --END IF
   
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI13                                                   #
#Objetivo          => Reporte de diagnostico operacion 27                      #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 01/06/2012                                               #
################################################################################
REPORT rpt_solicitudes_op28(v_det_operacion,v_detalle_agr)
DEFINE v_det_operacion RECORD
             f_reporte        CHAR(10),
             nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
             folio            LIKE glo_ctr_archivo.folio,
             tot_parejas      INTEGER,
             tot_desmarca_rch INTEGER,
             tot_desmarca_ace INTEGER
       END RECORD,
       v_detalle_agr RECORD
             v_gpo                 SMALLINT,
             v_clasificacion       CHAR(1) ,
             v_invadido            STRING,
             v_asociado            STRING,
             v_saldo92_invadido    DECIMAL(16,6),
             v_saldo92_asociado    DECIMAL(16,6),
             v_saldo97_invadido    DECIMAL(16,6),
             v_saldo97_asociado    DECIMAL(16,6),
             v_saldo92             DECIMAL(16,6),
             v_saldo97             DECIMAL(16,6),
             v_total_parejas       STRING
       END RECORD,
       v_tot_inv_92      DECIMAL(16,6),
       v_tot_inv_97      DECIMAL(16,6),
       v_tot_aso_92      DECIMAL(16,6),
       v_tot_aso_97      DECIMAL(16,6),
       v_tot_inv         DECIMAL(16,6),
       v_tot_aso         DECIMAL(16,6),
       v_tot_parejas     INTEGER,
       v_ban             SMALLINT,
       v_pagina          SMALLINT
DEFINE v_titulo          STRING

   FORMAT

      FIRST PAGE HEADER
         PRINTX v_det_operacion.*

      BEFORE GROUP OF v_detalle_agr.v_gpo

         LET v_tot_inv_92  = 0
         LET v_tot_inv_97  = 0
         LET v_tot_aso_92  = 0
         LET v_tot_aso_97  = 0
         LET v_tot_inv     = 0
         LET v_tot_aso     = 0
         LET v_tot_parejas = 0

      	 CASE v_detalle_agr.v_gpo 
      	 	  WHEN 1 LET v_titulo = 'RESUMEN ACEPTADAS'
      	    WHEN 2 LET v_titulo = 'RESUMEN RECHAZADAS'
      	    WHEN 3 LET v_titulo = 'DETALLE DE PAREJAS ACEPTADAS'
      	    WHEN 4 LET v_titulo = 'DETALLE DE PAREJAS RECHAZADAS'
      	 END CASE

         PRINTX v_titulo

      AFTER GROUP OF v_detalle_agr.v_gpo
         
         PRINTX v_tot_inv_92  USING '########.##',
                v_tot_inv_97  USING '########.##',
                v_tot_aso_92  USING '########.##',
                v_tot_aso_97  USING '########.##',
                v_tot_inv     USING '########.##',
                v_tot_aso     USING '########.##',
                v_tot_parejas USING '########.##'

      ON EVERY ROW
         
            PRINTX v_detalle_agr.*
            
            LET v_tot_inv_92  = v_tot_inv_92  + v_detalle_agr.v_saldo92_invadido
            LET v_tot_inv_97  = v_tot_inv_97  + v_detalle_agr.v_saldo97_invadido
            LET v_tot_aso_92  = v_tot_aso_92  + v_detalle_agr.v_saldo92_asociado
            LET v_tot_aso_97  = v_tot_aso_97  + v_detalle_agr.v_saldo97_asociado
            LET v_tot_parejas = v_tot_parejas + v_detalle_agr.v_total_parejas
            LET v_tot_inv     = v_tot_inv     + v_detalle_agr.v_saldo92
            LET v_tot_aso     = v_tot_aso     + v_detalle_agr.v_saldo97
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT
