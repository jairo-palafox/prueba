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
DEFINE vtt     integer
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE p_fec_ejecucion         DATE
   DEFINE v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE v_tot_integra     INTEGER
   DEFINE v_tot_marcados    INTEGER
   DEFINE v_tot_rechazados  INTEGER
   DEFINE v_tot_integrados  INTEGER
   DEFINE v_tot_infonavit   INTEGER
   DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo
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
             rch_cod               CHAR(003),--= '001'
             rch_desc              LIKE cat_rch_marca.rch_desc,
             nss1                  LIKE sep_det_02_op27.invadido,--= '01234567890'
             nss2                  LIKE sep_det_02_op27.invadido,--= '01234567890'
             nss3                  LIKE sep_det_02_op27.invadido --= '01234567890'
          END RECORD

   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_pos              INTEGER
   DEFINE v_pos_nss          SMALLINT
   DEFINE v_nss_info         LIKE sep_det_02_op27.invadido
   DEFINE r_resultado_opera  SMALLINT # bandera de resultado de cambio de estatus   
   
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_archivo = ARG_VAL(6) -- archivo procesado
  
   WHENEVER ERROR STOP
   
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPE11.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPE11-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   PREPARE prp_numFolio FROM "EXECUTE FUNCTION fn_genera_folio(?, ?, ?)"
   EXECUTE prp_numFolio USING p_i_proceso_cod, p_i_opera_cod,p_v_usuario
                         INTO p_d_folio

   UPDATE bat_ctr_operacion
   SET    folio       = p_d_folio  ,
          nom_archivo = p_v_archivo
   WHERE  pid         = p_d_pid
   AND    proceso_cod = p_i_proceso_cod
   AND    opera_cod   = p_i_opera_cod 

   -- asigna el folio en la variable de folio liquidación
   LET v_c_programa_cod = "SEPE11"

   -- se invoca la función que deja la operación en estado Procesando

-- no debe ir en el lanzado se elimina
   
--   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid, p_i_proceso_cod, p_i_opera_cod,
                                           --p_d_folio, v_c_programa_cod,
                                           --v_v_nom_reporte, p_v_usuario)

   -- se verifica si fue posible inicializar la operacion
   --IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_desplega_inc_operacion(r_b_valida)
      --DISPLAY "ERROR en fn_actualiza_opera_ini"
      --EXIT PROGRAM
   --END IF

-- no debe ir en el lanzado se elimina

   --EL archivo de entrada viene por parámetro
   --SELECT nombre_archivo
   --  INTO p_v_archivo
   --  FROM glo_ctr_archivo
   -- WHERE proceso_cod = p_i_proceso_cod
   --   AND opera_cod   = 1  # se recupera el archio que se genera en la operacion de carga(opera_cod = 1)
   --   AND estado = 1
   
   IF LENGTH(p_v_archivo CLIPPED) = 0 THEN
      DISPLAY "No existe archivo para el proceso de integración"
   
   ELSE
   
      SELECT NVL(COUNT(*),0)
        INTO v_count_valida
        FROM sep_cza_op27
       WHERE nombre_archivo = p_v_archivo
       
      IF v_count_valida > 0 THEN
         DISPLAY "Archivo: "||p_v_archivo CLIPPED||" integrado con anterioridad"
         DISPLAY "Cerrando proceso. Archivo no Integrado"
      ELSE
      
         DISPLAY "PID asignado: ",p_d_pid
         DISPLAY "Proceso asignado: ",p_i_proceso_cod
         DISPLAY "Operacion asignado: ",p_i_opera_cod
         DISPLAY "Fecha de Proceso: ",today
         DISPLAY "Archivo: ",p_v_archivo
          
         WHENEVER ERROR CONTINUE
         LET v_s_qryTxt = "DROP table tmp_sep_rch_marca; "
         PREPARE EnuTmpProcDrop FROM v_s_qryTxt
         --Borra tabla temporal 
         EXECUTE EnuTmpProcDROP
         IF SQLCA.SQLCODE = -214 THEN
            DISPLAY "LA tabla temporal de rechazos se encuentra bloqueda. Err: ",SQLCA.SQLCODE
         END IF
         WHENEVER ERROR STOP
         
         LET v_s_qryTxt = "create table tmp_sep_rch_marca ",
                          "  (",
                          "    nss char(11),",
                          "    marca_entra smallint,",
                          "    rch_cod smallint,",
                          "    id_registro dec(9,0)",
                          "  );"
         PREPARE EnuTmpProc FROM v_s_qryTxt
         EXECUTE EnuTmpProc 
         
         LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                          "   FROM safre_tmp:tmp_sep_det_02_op27 "
                          
         PREPARE EnuTotReg FROM v_s_qryTxt
         
         EXECUTE EnuTotReg INTO v_tot_integra
         
         DISPLAY "TOTAL DE REGISTOS A SER INTEGRADOS: ",v_tot_integra
         
         LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_sep_integra_notificacion_op27(?,?,?,?)"
         PREPARE EnuSPIntNot FROM v_s_qryTxt
         EXECUTE EnuSPIntNot USING p_d_folio, p_v_archivo, p_v_usuario,p_i_proceso_cod
                             INTO v_tot_marcados, 
                                  v_tot_rechazados,
                                  v_tot_integrados

         IF(SQLCA.SQLCODE <> 0)THEN
            DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
            CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                      RETURNING r_resultado_opera
            # si ocurrió un error con la actualizacion de la operacion operacion 
            # muestra el mensaje
            IF(r_resultado_opera)THEN
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF
            EXIT PROGRAM
         END IF
         
         DISPLAY "TOTAL MARCADOS  :",v_tot_marcados
         DISPLAY "TOTAL RECHAZADOS:",v_tot_rechazados
         DISPLAY "TOTAL INTEGRADOS:",v_tot_integrados

         DISPLAY "\n"
      
         ---- recupera la ruta de listados en el que se enviara el archivo
         CALL fn_rutas("sep") RETURNING r_ruta_bin, r_ruta_listados
         --DISPLAY "v_v_nom_reporte:",v_v_nom_reporte
         --DISPLAY "Ruta bin - ", r_ruta_bin
         --DISPLAY "Ruta lst - ", r_ruta_listados
         -- se indica que el reporte usara la plantilla creada
         LET r_ruta_bin = r_ruta_bin CLIPPED 
         IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/SEPL111.4rp") THEN
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
            END IF
         END IF
         -- inicia el reporte de registros procesados
         START REPORT rpt_solicitudes_marca_op27 TO XML HANDLER v_manejador_rpt
         
         LET v_day = DAY(TODAY) USING "&&"
         LET v_mes = MONTH(TODAY) USING "&&"
         LET v_ano = YEAR(TODAY) USING "&&&&"
         LET v_r_rpt_res_edo.f_reporte = v_day,"-",v_mes,"-",v_ano
         LET v_r_rpt_res_edo.nombre_archivo = p_v_archivo
         LET v_r_rpt_res_edo.folio          = p_d_folio
         LET v_r_rpt_res_edo.tot_integrado  = v_tot_integrados
         LET v_r_rpt_res_edo.tot_marcadas   = v_tot_marcados
         LET v_r_rpt_res_edo.tot_rechazadas = v_tot_rechazados
         
         {LET v_s_qryTxt = "SELECT 1, nss, rch_cod",
                          "  FROM tmp_sep_rch_marca"}
         LET v_s_qryTxt = "\n SELECT 1, sep.nss, sep.rch_cod, NVL(cat.rch_desc,' ')",
                          "\n   FROM tmp_sep_rch_marca sep LEFT OUTER JOIN cat_rch_marca cat",
                          "\n     ON cat.rch_cod = sep.rch_cod"

         PREPARE EnuArrRchMarca FROM v_s_qryTxt
         DECLARE CurArrRchMarca CURSOR FOR EnuArrRchMarca
         
         LET v_pos = 1
         CALL v_r_reporte_det.clear()
         
         FOREACH CurArrRchMarca INTO v_r_reporte_det[v_pos].*
            LET v_pos = v_pos + 1
         END FOREACH
         
         LET v_s_qryTxt = "SELECT invadido",
                          "  FROM sep_det_02_op27",
                          " WHERE LENGTH(TRIM(credito_infonavit)) > 0",
                          " AND folio = ",p_d_folio
         PREPARE EnuArrCredInfo FROM v_s_qryTxt
         DECLARE CurArrCredInfo CURSOR FOR EnuArrCredInfo
         
         LET v_pos_nss = 1
         LET v_tot_infonavit = 0
         
         FOREACH CurArrCredInfo INTO v_nss_info
            CASE v_pos_nss
               WHEN 1 
                  LET v_r_reporte_det[v_pos].nss1 = v_nss_info
                  LET v_r_reporte_det[v_pos].gpo  = 2
               WHEN 2 
                  LET v_r_reporte_det[v_pos].nss2 = v_nss_info
               WHEN 3 
                  LET v_r_reporte_det[v_pos].nss3 = v_nss_info
            END CASE
            LET v_tot_infonavit = v_tot_infonavit + 1
            IF v_pos_nss = 3 THEN
               LET v_pos_nss = 1
               LET v_pos = v_pos + 1
            ELSE
               LET v_pos_nss = v_pos_nss + 1
            END IF
            
         END FOREACH

         LET v_r_rpt_res_edo.tot_infonavit  = v_tot_infonavit
       
 
         IF LENGTH(v_r_reporte_det[v_r_reporte_det.getLength()].gpo CLIPPED) = 0 THEN
            CALL v_r_reporte_det.deleteElement(v_r_reporte_det.getLength())
         END IF
         LET vtt = 0 
         IF v_r_reporte_det.getLength() = 0 then 
            LET vtt = 1
         ELSE 
            LET vtt = v_r_reporte_det.getLength()
         END IF      
     
         --FOR v_pos_rep_det = 1 TO v_r_reporte_det.getLength()
         FOR v_pos_rep_det = 1 TO vtt
            OUTPUT TO REPORT rpt_solicitudes_marca_op27(v_r_rpt_res_edo.*,
                                                  v_r_reporte_det[v_pos_rep_det].*)
         END FOR
         
         -- finaliza el reporte
         FINISH REPORT rpt_solicitudes_marca_op27
         
      
         -- Envío de correo de notificación de proceso finalizado

         -- se invoca la función que deja la operación en estado Finalizado
         LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
         
         
         CALL fn_correo_proceso(p_d_pid, 
                                p_i_proceso_cod, 
                                p_i_opera_cod, 
                                '', -- TMP AHM adjunto ?
                                'Integrar archivo de Notificación de Posibles cuentas a Separar (op27)',
                                'ID Proceso   : '||p_d_pid||
                                'Proceso      : '||p_i_proceso_cod||
                                'Operacion    : '||p_i_opera_cod||
                                'Fecha Inicio : '||p_fec_ejecucion||
                                'Fecha Fin    : '||DATE
                                )
      

         
         -- se verifica si fue posible finalizar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
            DISPLAY "ERROR en fn_actualiza_opera_fin"
         END IF
         RETURN

      END IF
   
   END IF

   CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                                RETURNING r_b_valida
   IF(r_b_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF
   
END MAIN

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG01                                                   #
#Objetivo          => Reporte de solicitudes de mandatos                       #
#Autor             => Hugo César Ramírez Gracía                                #
#Modifico          => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 03/04/2012                                               #
#Fecha Modificacion=> 04/04/2012                                               #
################################################################################
REPORT rpt_solicitudes_marca_op27(v_det_operacion,v_detalle_rechazos)
DEFINE v_det_operacion RECORD
             f_reporte       CHAR(10),
             nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
             folio           LIKE glo_ctr_archivo.folio,
             tot_integrado   INTEGER,
             tot_marcadas    INTEGER,
             tot_rechazadas  INTEGER,
             tot_infonavit   INTEGER
       END RECORD,
       v_detalle_rechazos RECORD
             gpo                   smallint,--= 1
             nss                   char(11),--= '01234567890'
             rch_cod               smallint,--= '001'
             rch_desc              LIKE cat_rch_marca.rch_desc,
             nss1                  char(11),--= '01234567890'
             nss2                  char(11),--= '01234567890'
             nss3                  char(11) --= '01234567890'
       END RECORD,
       v_tot_gpo         INTEGER,
       v_ban             SMALLINT,
       v_pagina          SMALLINT

   FORMAT

      FIRST PAGE HEADER
         PRINTX v_det_operacion.f_reporte     ,
                v_det_operacion.nombre_archivo,
                v_det_operacion.folio         ,
                v_det_operacion.tot_integrado ,
                v_det_operacion.tot_marcadas  ,
                v_det_operacion.tot_rechazadas,
                v_det_operacion.tot_infonavit 
         LET v_tot_gpo = 0
         
      AFTER GROUP OF v_detalle_rechazos.gpo
         PRINTX v_tot_gpo
         
      ON EVERY ROW
         PRINTX v_detalle_rechazos.gpo,
                v_detalle_rechazos.nss,
                v_detalle_rechazos.rch_cod,
                v_detalle_rechazos.rch_desc,
                v_detalle_rechazos.nss1,
                v_detalle_rechazos.nss2,
                v_detalle_rechazos.nss3
         
         IF v_detalle_rechazos.gpo = 2 THEN
            IF LENGTH(v_detalle_rechazos.nss3 CLIPPED) <> 0 THEN
               LET v_tot_gpo = v_tot_gpo + 3
            ELSE
               IF LENGTH(v_detalle_rechazos.nss2 CLIPPED) <> 0 THEN
                  LET v_tot_gpo = v_tot_gpo + 2
               ELSE
                  IF LENGTH(v_detalle_rechazos.nss1 CLIPPED) <> 0 THEN
                     LET v_tot_gpo = v_tot_gpo + 1
                  END IF
               END IF
            END IF
         ELSE
            LET v_tot_gpo = v_tot_gpo + 1
         END IF
         IF v_detalle_rechazos.gpo = 2 AND v_ban = 0 THEN
            LET v_ban = 1
            IF LENGTH(v_detalle_rechazos.nss3 CLIPPED) <> 0 THEN
               LET v_tot_gpo = 3
            ELSE
               IF LENGTH(v_detalle_rechazos.nss2 CLIPPED) <> 0 THEN
                  LET v_tot_gpo = 2
               ELSE
                  IF LENGTH(v_detalle_rechazos.nss1 CLIPPED) <> 0 THEN
                     LET v_tot_gpo = 1
                  END IF
               END IF
            END IF
         END IF
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT
