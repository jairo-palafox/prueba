--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-06-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => MDT                                                     #
#Programa          => MDTE21                                                  #
#Objetivo          => Generar Archivo de integracion                          #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha Inicio      => 09 Junio 2012                                                        #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_prog_reporte              CHAR(200),
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,  -- codigo de operacion
   g_ruta_envio      LIKE seg_modulo.ruta_envio,
   v_nom_archivo     CHAR(200)
   --v_nom_archivo     STRING
DEFINE v_total_casos, v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado  CHAR(1)
     ,casos   INTEGER
   END RECORD
DEFINE v_total_procesados INTEGER
DEFINE v_total_x_procesar INTEGER
DEFINE v_r_rpt_res   RECORD -- registro de resumen
          des_origen   LIKE mdt_cat_origen.des_origen,
          proceso_desc LIKE cat_proceso.proceso_desc,
          lote         LIKE mdt_solicitud_mandato.folio,
          f_lote       LIKE mdt_lote_mandato.f_proceso,
          altas        INTEGER,
          bajas        INTEGER,
          modif        INTEGER
       END RECORD,
       v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
       v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
       r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
       r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
DEFINE tot_altas_proc          INTEGER,
       tot_bajas_proc          INTEGER,
       tot_modificaciones_proc INTEGER,
       r_b_valida              SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
DEFINE p_num_folio             LIKE glo_folio.folio
DEFINE r_resultado_opera       SMALLINT
END GLOBALS

#Objetivo:
MAIN
DEFINE 
   v_estatus         SMALLINT,
   v_indice          INTEGER,
   v_hora            CHAR(8),
   v_query           STRING,
   v_fec_ejecucion   DATETIME YEAR TO SECOND 

   # se recupera la hora a la que inició la operacion
   LET v_fec_ejecucion = CURRENT YEAR TO SECOND 
   #Si se ha recibido parámetros se continua    

   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)  
   LET p_num_folio   = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   LET v_hora = TIME (CURRENT HOUR TO SECOND) 
   
   LET v_query = "\n SELECT ruta_envio         "
                ,"\n FROM   seg_modulo         "
                ,"\n WHERE  modulo_cod = 'mdt' "
   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO g_ruta_envio

   IF p_num_folio = 0 OR p_num_folio IS NULL THEN
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_num_folio 
   END IF
   
   --LET v_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   --                   ,v_hora[1,2] USING "&&", v_hora[4,5] USING "&&", v_hora[7,8] USING "&&", "02.mdt"
   
   -- TMP Por integrar DISPLAY " PID ASIGNADO :",g_pid
   CALL fn_display_proceso(0,"GENERAR ARCHIVO DE MANDATOS - INTEGRACION")
   
   --Se escribe la descripción del proceso
   DISPLAY "\n DESCRIPCIÓN DEL PROCESO:"
   DISPLAY " Generar archivo resumen de integracion."

   CALL fn_total_registros()
   
   DISPLAY "\n TOTAL DE REGISTROS X INTEGRAR:",v_total_x_procesar
   DISPLAY '\n GENERANDO ARCHIVO DE INTEGRACION'
   --Se marca el proceso como inciado
   {CALL fn_actualiza_opera_ini (g_pid
                              ,g_proceso_cod
                              ,g_opera_cod
                              ,p_num_folio
                              ,"MDTE21"
                              ,v_nom_archivo
                              ,p_usuario_cod)
                              RETURNING v_estatus}
   --Se genera el archivo
   --IF v_estatus THEN
      CALL f_integra_mandatos() RETURNING v_total_procesados
      
      UPDATE bat_ctr_operacion 
      SET nom_archivo = v_nom_archivo ,
          folio = p_num_folio
      WHERE pid  =  g_pid
      AND   proceso_cod = g_proceso_cod   
      AND   opera_cod = g_opera_cod   

   {ELSE
   	  DISPLAY "Error al actualizar operacion"
   	  RETURN
   END IF}

   CALL fn_rutas("mdt") RETURNING r_ruta_bin, r_ruta_listados
   DISPLAY "Ruta bin - ", r_ruta_bin
   DISPLAY "Ruta lst - ", r_ruta_listados    
   
   LET g_prog_reporte  = r_ruta_bin CLIPPED ,"/MDTL211.4rp" 
   
   DISPLAY g_prog_reporte CLIPPED 
   
   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings(g_prog_reporte CLIPPED ) THEN
      -- se indica la salida del reporte
      CALL fgl_report_selectDevice("PDF") 
      LET v_v_nom_reporte = p_usuario_cod CLIPPED, "-MDTE21-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
   
      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)
   
      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                         RETURNING r_b_valida
   
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF
   
   -- inicia el reporte de registros procesados
   START REPORT reporte_reg_proc TO XML HANDLER v_manejador_rpt

   -- Recupera descripcion del tipo de origen
   SELECT des_origen 
     INTO v_r_rpt_res.des_origen 
     FROM mdt_cat_origen
   WHERE id_origen = 1
   
   -- Recupera descripcion del proceso
   SELECT proceso_desc 
     INTO v_r_rpt_res.proceso_desc 
     FROM cat_proceso
   WHERE proceso_cod = g_proceso_cod

   LET v_query = "SELECT NVL(COUNT(*),0) ",
                 "  FROM mdt_solicitud_mandato",
                 " WHERE folio = ",p_num_folio,
                 "   AND tipo_operacion = ?"
   PREPARE EnuTotIntegrados FROM v_query
               
   EXECUTE EnuTotIntegrados USING 'A' INTO tot_altas_proc
   EXECUTE EnuTotIntegrados USING 'B' INTO tot_bajas_proc
   EXECUTE EnuTotIntegrados USING 'M' INTO tot_modificaciones_proc

   LET v_r_rpt_res.lote   = p_num_folio
   LET v_r_rpt_res.f_lote = TODAY
   LET v_r_rpt_res.altas = tot_altas_proc
   LET v_r_rpt_res.bajas = tot_bajas_proc
   LET v_r_rpt_res.modif = tot_modificaciones_proc
   
   OUTPUT TO REPORT reporte_reg_proc(v_r_rpt_res.*)
   
   -- finaliza el reporte
   FINISH REPORT reporte_reg_proc

   CALL fn_actualiza_opera_fin (g_pid
                               ,g_proceso_cod
                               ,g_opera_cod)
                               RETURNING v_estatus

   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod, 
                          NULL, 
                          'Integra mandatos - Origen recurrente',
                          'ID Proceso   : '||g_pid||
                          'Proceso      : '||g_proceso_cod||
                          'Operacion    : '||g_opera_cod||
                          'Fecha Inicio : '||v_fec_ejecucion||
                          'Fecha Fin    : '||CURRENT YEAR TO SECOND 
                         )
   CALL fn_act_edo_archivo(v_nom_archivo,p_num_folio,2,p_usuario_cod)
                           RETURNING r_resultado_opera
                           
   UPDATE glo_folio
      SET STATUS = 1
     WHERE proceso_cod = g_proceso_cod 
       AND opera_cod = g_opera_cod
     
   IF(r_resultado_opera <> 0)THEN
      DISPLAY "\n ERROR AL ACTUALIZAR ESTADO DE ARCHIVO (CÓDIGO):",r_resultado_opera
   END IF

   DISPLAY "\n TOTAL DE REGISTROS PROCESADOS:",v_total_procesados
   
   DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",v_nom_archivo
   
   CALL fn_display_proceso(1,"GENERAR ARCHIVO DE MANDATOS - INTEGRACION")
END MAIN

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTE21                                                   #
#Objetivo          => Integracion de registros de mandatos origen recurrente   #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 11 Junio 2012                                            #
#Modificación      => se agrega ciclo de busqueda al SP                        #
################################################################################
FUNCTION f_integra_mandatos()
DEFINE v_qry                 STRING
DEFINE v_r_tmp_mdt_recurrente_acr RECORD LIKE safre_tmp:tmp_acr_transf_30.*
DEFINE v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
DEFINE v_f_inicio            DATE
DEFINE v_f_culmina           DATE
DEFINE v_f_temporal          DATE
DEFINE v_integrados          INTEGER
DEFINE v_cve_mandato         LIKE mdt_cat_mandato.cve_mandato
DEFINE r_folio               LIKE glo_folio.folio
DEFINE v_sql_error      INTEGER
DEFINE v_msg_error      CHAR(200)

   LET v_f_temporal = TODAY
   LET v_integrados = 0
   
   LET v_qry = "EXECUTE PROCEDURE sp_mdt_inserta_inst_recurrente(?,?)"
   PREPARE EnuRegRecurente FROM v_qry


   EXECUTE EnuRegRecurente USING p_num_folio,
                                 p_usuario_cod
                            INTO v_integrados,
                                 v_sql_error,
                                 v_msg_error
   IF(v_sql_error <> 0)THEN
      DISPLAY "OCURRIÓ UN ERROR AL ALMACENAR INFORMACIÓN"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                        RETURNING r_b_valida
      
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF
   
   RETURN v_integrados
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTE21                                                  #
#Objetivo          => Obtiene el total de registros que se integraron         #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha Inicio      => 09 Junio 2012                                                        #
###############################################################################
FUNCTION fn_total_registros()
   DEFINE v_query  STRING
         ,v_indice INTEGER
   LET v_total_x_procesar = 0
   
   SELECT NVL(COUNT(*),0) INTO v_total_x_procesar
     FROM safre_tmp:tmp_acr_transf_30

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTE21                                                  #
#Objetivo          => Validacion de registros temporales de mandatos origen rec.
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha Inicio      => 11 Junio 2012                                                        #
###############################################################################
FUNCTION fn_valida_mandato()
   
   
   RETURN TRUE
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTE21                                                  #
#Objetivo          => Genera el reporte de integrados                         #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha Inicio      => 09 Junio 2012                                           #
###############################################################################
REPORT reporte_reg_proc(p_r_res)
   DEFINE p_r_res   RECORD -- registro de resumen
             des_origen   LIKE mdt_cat_origen.des_origen,
             proceso_desc LIKE cat_proceso.proceso_desc,
             lote         LIKE mdt_solicitud_mandato.folio,
             f_lote       LIKE mdt_lote_mandato.f_proceso,
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD

   FORMAT

   FIRST PAGE HEADER
      PRINTX p_r_res.des_origen  
      PRINTX p_r_res.proceso_desc
      PRINTX p_r_res.lote        
      PRINTX p_r_res.f_lote      
      PRINTX p_r_res.altas USING "#########&"
      PRINTX p_r_res.bajas USING "#########&"
      PRINTX p_r_res.modif USING "#########&"

END REPORT