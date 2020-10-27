--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTS06                                                   #
#Objetivo          => Generar Archivo con Solicitudes de instrucciones de      #
#                     mandatos con origen CANALES para enviar a Validación     #
#                     por Sustentabilidad.                                     #
#Autor             => Francisco López                                          #
#Fecha Inicio      =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,  -- codigo de operacion
   g_ruta_envio      LIKE seg_modulo.ruta_envio,
   v_nom_archivo     STRING
DEFINE v_total_casos, v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado  CHAR(1)
     ,casos   INTEGER
   END RECORD
DEFINE v_total_procesados INTEGER
DEFINE v_total_x_procesar INTEGER
DEFINE v_r_rpt_res   RECORD -- registro de resumen
          des_origen   LIKE mdt_cat_origen.des_origen,
          proceso_desc LIKE cat_proceso.proceso_desc,
          folio         LIKE mdt_solicitud_mandato.folio,
          f_proceso       LIKE mdt_lote_mandato.f_proceso,
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
       r_b_valida              SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
       p_num_folio       DECIMAL(9)
END GLOBALS

#Objetivo:
MAIN
DEFINE 
   
   v_estatus         SMALLINT,
   v_indice          INTEGER,
   v_hora            CHAR(8),
   v_query           STRING,
   v_fec_ejecucion   DATETIME YEAR TO SECOND,
   v_resultado_opera SMALLINT

   # se recupera la hora a la que inició la operacion
   LET v_fec_ejecucion = CURRENT YEAR TO SECOND 
   #Si se ha recibido parámetros se continua    
      #Primer parámetro
      LET p_usuario_cod = ARG_VAL(1)
      #Segundo parámetro
      LET g_pid         = ARG_VAL(2)
      #Tercer parámetro
      LET g_proceso_cod = ARG_VAL(3)
      #Cuarto parámetro
      LET g_opera_cod   = ARG_VAL(4)  --numero de proceso
      LET v_hora = TIME (CURRENT HOUR TO SECOND) 
      #Quinto parámetro
      LET p_num_folio   = ARG_VAL(5)
      #Sexto parámetro
      LET v_nom_archivo = ARG_VAL(6)

      {CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
         RETURNING v_resultado_opera
      IF(v_resultado_opera <> 0)THEN
         --CALL fn_muestra_inc_operacion(v_estatus)
         EXIT PROGRAM
      END IF}
      
      --LET p_num_folio = 0
      LET v_query = "\n SELECT ruta_envio         "
                   ,"\n FROM   seg_modulo         "
                   ,"\n WHERE  modulo_cod = 'mdt' "
      PREPARE prp_ruta_archivo FROM v_query
      EXECUTE prp_ruta_archivo INTO g_ruta_envio

      IF p_num_folio = 0 OR p_num_folio IS NULL THEN
         CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_num_folio 
      END IF
      
      LET v_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                         ,v_hora[1,2] USING "&&", v_hora[4,5] USING "&&", v_hora[7,8] USING "&&", "02.VSU"
      
      DISPLAY " PID ASIGNADO :",g_pid
      CALL fn_display_proceso(0,"GENERAR ARCHIVO DE MANDATOS - CANALES")
      
      --Se escribe la descripción del proceso
      DISPLAY "\n DESCRIPCIÓN DEL PROCESO:"
      DISPLAY " Generar Archivo con Solicitudes de instrucciones de mandatos con origen CANALES para enviar a Validación por Sustentabilidad."

      CALL fn_total_registros()
      DISPLAY "\n TOTAL DE REGISTROS POR PROCESAR:",v_total_x_procesar
      DISPLAY "\n REGISTROS A ACTUALIZAR POR ESTADO:"
      FOR v_indice = 1 TO v_total_casos.getLength()
         DISPLAY "    ESTADO: ",v_total_casos[v_indice].estado," - CASOS : ",v_total_casos[v_indice].casos
      END FOR
      DISPLAY '\n GENERANDO ARCHIVO PARA VALIDACION POR SUSTENTABILIDAD'
      --Se marca el proceso como inciado
      {CALL fn_actualiza_opera_ini (g_pid
                                  ,g_proceso_cod
                                  ,g_opera_cod
                                  ,p_num_folio
                                  ,"MDTS06"
                                  ,v_nom_archivo
                                  ,p_usuario_cod)
                                  RETURNING v_estatus}
                                  
      --Se genera el archivo
      CALL f_genera_archivo_mandatos()

      CALL fn_actualiza_opera_fin (g_pid
                                  ,g_proceso_cod
                                  ,g_opera_cod)
                                  RETURNING v_estatus
      IF(v_estatus <> 0)THEN
         
         CALL fn_desplega_inc_operacion(v_estatus)
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING v_estatus
         IF(v_estatus)THEN
            # Imprime el mensaje de inconsistencia en consola y archivo
            
            CALL fn_desplega_inc_operacion(v_estatus)
            
         END IF
         EXIT PROGRAM
      END IF

      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(g_pid, 
                             g_proceso_cod, 
                             g_opera_cod, 
                             NULL, 
                             'Registrar instrucciones de mandatos - Origen recurrente',
                             'ID Proceso   : '||g_pid||
                             'Proceso      : '||g_proceso_cod||
                             'Operacion    : '||g_opera_cod||
                             'Fecha Inicio : '||v_fec_ejecucion||
                             'Fecha Fin    : '||CURRENT YEAR TO SECOND 
                            )
      DISPLAY "\n TOTAL DE REGISTROS PROCESADOS:",v_total_procesados
      
      DISPLAY "\n REGISTROS PROCESADOS POR ESTADO:"
      FOR v_indice = 1 TO v_total_casos_procesados.getLength()
         DISPLAY "    ESTADO: ",v_total_casos_procesados[v_indice].estado," - CASOS : ",v_total_casos_procesados[v_indice].casos
      END FOR

      DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",v_nom_archivo
      
      CALL fn_display_proceso(1,"GENERAR ARCHIVO DE MANDATOS - CANALES")
END MAIN

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => Obtiene el total de registros que se van a modificar    #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_total_registros()
   DEFINE v_query  STRING
         ,v_indice INTEGER
   LET v_total_x_procesar = 0
   
   LET v_query =  "\n SELECT tipo_operacion,COUNT(*)                   "
                 ,"\n FROM safre_viv:mdt_solicitud_mandato             "
                 ,"\n WHERE estado = 101                               "
                 ,"\n AND id_origen = 2                                "
                 ,"\n GROUP BY 1                                       "
   PREPARE prp_total_casos_previo FROM v_query
   DECLARE cur_total_casos_previo CURSOR FOR prp_total_casos_previo
   LET v_indice = 1
   FOREACH cur_total_casos_previo INTO v_total_casos[v_indice].*
      LET v_total_x_procesar = v_total_x_procesar + v_total_casos[v_indice].casos
      LET v_indice = v_indice + 1
   END FOREACH
   IF v_total_casos[v_total_casos.getLength()].estado IS NULL THEN
      CALL v_total_casos.deleteElement(v_total_casos.getLength())
   END IF
END FUNCTION
###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => Genera el archivo de mandatos                           #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_genera_archivo_mandatos()
   --Se definen las variables para el archivo fisico
   DEFINE v_archivo_mandatos         BASE.CHANNEL
         ,v_query                    STRING
         ,v_string_detalle           STRING
         ,v_indice                   INTEGER
         ,v_id_lote                  INTEGER
         --,v_id_lote_mandato          LIKE mdt_lote_mandato.id_lote_mandato 
         ,v_folio               LIKE mdt_lote_mandato.folio

   --Record del detalle del archivo
   DEFINE v_rec_detalle RECORD
       id_01 CHAR(2 )      --X-tipo registro            
      ,id_02 CHAR(11)      --X-nss                      
      ,id_03 DECIMAL(10,0) --9-número de crédito        
      ,id_04 INTEGER --DECIMAL( 5,0) --9-consecutivo              
      ,id_05 CHAR(7 )      --X-identificador de Mandato 
      ,id_06 CHAR(1)       --x-Tipo de Operación        
      ,id_07 DATE          --X-Fecha Captura            
      ,id_08 DATE          --X-Fecha inicio mandato     
      ,id_09 DATE          --X-Fecha culminación mandato
      ,id_10 DECIMAL( 1,0) --N-Tipo Descuento           
      ,id_11 DECIMAL(16,6) --N-Valor Descuento          
      ,id_12 CHAR(40)      --X-Referencia               
      ,id_13 DECIMAL(1,0 ) --9-Orígen                   
      ,id_14 CHAR(2 )      --X-Resultado Operación      
      ,id_15 CHAR(3 )      --X-Diagnóstico              
      ,id_16 CHAR(18)      --X-Cve_mandato -- Ajuste 20120410 Campo nuevo          
      ,id_solicitud_mandato  LIKE mdt_solicitud_mandato.id_solicitud_mandato
   END RECORD
   DEFINE v_rec_detalle_string RECORD
       id_01 CHAR(2 )   --X-tipo registro            
      ,id_02 CHAR(11)   --X-nss                      
      ,id_03 CHAR(10)   --9-número de crédito        
      ,id_04 CHAR(5 )   --9-consecutivo              
      ,id_05 CHAR(7 )   --X-identificador de Mandato 
      ,id_06 CHAR(1 )   --9-Tipo de Operación        
      ,id_07 CHAR(8 )   --X-Fecha Captura            
      ,id_08 CHAR(8 )   --X-Fecha inicio mandato     
      ,id_09 CHAR(8 )   --X-Fecha culminación mandato
      ,id_10 CHAR(1 )   --N-Tipo Descuento           
      ,id_11 CHAR(15)   --N-Valor Descuento          
      ,id_12 CHAR(40)   --X-Referencia               
      ,id_13 CHAR(1 )   --9-Orígen                   
      ,id_14 CHAR(2 )   --X-Resultado Operación      
      ,id_15 CHAR(3 )   --X-Diagnóstico              
      ,id_16 CHAR(18)   --X-Cve_mandato -- Ajuste 20120410 Campo nuevo
   END RECORD

   -- se crea el manejador de archivo
   LET v_archivo_mandatos = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_mandatos.openFile(g_ruta_envio CLIPPED||"/"||v_nom_archivo, "w" )
   CALL v_archivo_mandatos.setDelimiter("")
   LET v_total_procesados = 0
   LET v_id_lote = 1
   --Se otiene el consecutivo del dia
   {SELECT MAX(f_proceso)
     INTO v_folio
     FROM mdt_lote_mandato
    WHERE f_lote = TODAY}

   
   DISPLAY "1"
   
   LET v_query =  "\n SELECT '02',nss,id_credito, 0 ,'       ',       "   --id_mandato ya existe para la modificacion de la tabla y solo se pasan los 7 espacios de la columna
                 ,"\n        tipo_operacion,f_canales,f_inicio_mandato,       "
                 ,"\n        f_culmina_mandato,tpo_descuento_mandato,         "
                 ,"\n        valor_descuento_mandato,referencia,2,' ',' ',    "
                 ,"\n        cve_mandato ,                                    " -- Ajuste 20120410 Campo nuevo
                 ,"\n        id_solicitud_mandato                             "
                 ,"\n FROM mdt_solicitud_mandato                       "
                 ,"\n WHERE estado = 101                               "
                 ,"\n AND id_origen = 2                                "

   PREPARE prp_datos_archivo FROM v_query
   DECLARE cur_datos_archivo CURSOR FOR prp_datos_archivo


--Se inserta el registro en la tabla mdt_lote_mandato
   {SELECT (NVL(MAX(id_lote_mandato),0) + 1)
     INTO v_id_lote_mandato
     FROM mdt_lote_mandato}
   
   INSERT INTO mdt_lote_mandato
   (folio,id_origen,f_proceso,estado)
   VALUES
   (
   p_num_folio
   ,2
   ,TODAY
   ,104
   )
   DISPLAY "ESTADO SQL", SQLCA.SQLCODE
   {(id_lote_mandato,id_origen,f_lote,lote,f_carga_scr,f_procesof_mdt,estado)
   VALUES
   (
   v_id_lote_mandato
   ,2
   ,TODAY
   ,v_lote_x_dia
   ,NULL
   ,TODAY
   ,104
   )}
   DISPLAY "2"
   FOREACH cur_datos_archivo INTO v_rec_detalle.id_01
                                 ,v_rec_detalle.id_02
                                 ,v_rec_detalle.id_03
                                 ,v_rec_detalle.id_04
                                 ,v_rec_detalle.id_05
                                 ,v_rec_detalle.id_06
                                 ,v_rec_detalle.id_07
                                 ,v_rec_detalle.id_08
                                 ,v_rec_detalle.id_09
                                 ,v_rec_detalle.id_10
                                 ,v_rec_detalle.id_11
                                 ,v_rec_detalle.id_12
                                 ,v_rec_detalle.id_13
                                 ,v_rec_detalle.id_14
                                 ,v_rec_detalle.id_15
                                 ,v_rec_detalle.id_16 -- Ajuste 20120410 Campo nuevo
                                 ,v_rec_detalle.id_solicitud_mandato
      --Se pasan los datos obtenidos a su equivalente en tipo char
      LET v_rec_detalle_string.id_01 = v_rec_detalle.id_01
      LET v_rec_detalle_string.id_02 = v_rec_detalle.id_02
      LET v_rec_detalle_string.id_03 = v_rec_detalle.id_03 USING "&&&&&&&&&&" --DECIMAL(10,0) --9-número de crédito   
      LET v_rec_detalle_string.id_04 = v_id_lote USING "&&&&&"                --DECIMAL( 5,0) --9-consecutivo 
      LET v_rec_detalle_string.id_05 = v_rec_detalle.id_05
      LET v_rec_detalle_string.id_06 = v_rec_detalle.id_06
      LET v_rec_detalle_string.id_07 = YEAR(v_rec_detalle.id_07) USING "&&&&", MONTH(v_rec_detalle.id_07) USING "&&",DAY(v_rec_detalle.id_07) USING "&&"
      LET v_rec_detalle_string.id_08 = YEAR(v_rec_detalle.id_08) USING "&&&&", MONTH(v_rec_detalle.id_08) USING "&&",DAY(v_rec_detalle.id_08) USING "&&"
      LET v_rec_detalle_string.id_09 = YEAR(v_rec_detalle.id_09) USING "&&&&", MONTH(v_rec_detalle.id_09) USING "&&",DAY(v_rec_detalle.id_09) USING "&&"
      LET v_rec_detalle_string.id_10 = v_rec_detalle.id_10 USING "&"          --DECIMAL( 1,0) --N-Tipo Descuento   
      LET v_rec_detalle_string.id_11 = (v_rec_detalle.id_11 * 1000000) USING "&&&&&&&&&&&&&&&" --CHAR(15)   --N-Valor Descuento
      LET v_rec_detalle_string.id_12 = v_rec_detalle.id_12
      LET v_rec_detalle_string.id_13 = v_rec_detalle.id_13 USING "&"          --DECIMAL( 1,0) --9-Orígen                   
      LET v_rec_detalle_string.id_14 = v_rec_detalle.id_14
      LET v_rec_detalle_string.id_15 = v_rec_detalle.id_15
      LET v_rec_detalle_string.id_16 = v_rec_detalle.id_16 -- Ajuste 20120410 Campo nuevo
      --Se asiga todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_string_detalle = v_rec_detalle_string.id_01
                            ,v_rec_detalle_string.id_02
                            ,v_rec_detalle_string.id_03
                            ,v_rec_detalle_string.id_04
                            ,v_rec_detalle_string.id_05
                            ,v_rec_detalle_string.id_06
                            ,v_rec_detalle_string.id_07
                            ,v_rec_detalle_string.id_08
                            ,v_rec_detalle_string.id_09
                            ,v_rec_detalle_string.id_10
                            ,v_rec_detalle_string.id_11
                            ,v_rec_detalle_string.id_12
                            ,v_rec_detalle_string.id_13
                            ,v_rec_detalle_string.id_14
                            ,v_rec_detalle_string.id_15
                            ,v_rec_detalle_string.id_16 -- Ajuste 20120410 Campo nuevo


       
      # Se escribe en el archivo los datos obtenidos
      CALL v_archivo_mandatos.WRITE([v_string_detalle])
      DISPLAY "3"
      # Se actualiza el estatus del registro insertado
      UPDATE mdt_solicitud_mandato
      SET    --f_lote  = TODAY
             folio    = p_num_folio
            --,id_lote = v_id_lote
            ,estado  = 104
      WHERE folio IS NULL
        AND id_origen = 2
        AND estado = 101

     DISPLAY "ESTADO SQL", SQLCA.SQLCODE
      --folio = p_num_folio
      --id_solicitud_mandato = v_rec_detalle.id_solicitud_mandato

     DISPLAY "4"
      --Se incrementa el contador general
      LET v_total_procesados = v_total_procesados + 1
      LET v_id_lote = v_id_lote + 1
      --Se verifica que el array de control de datos insertados por tipo no este vacio 
      IF v_total_casos_procesados.getLength() = 0 THEN
         LET v_total_casos_procesados[1].estado = v_rec_detalle_string.id_06
         LET v_total_casos_procesados[1].casos = 0
      END IF
      --Se incrementa el contador por tipo de registro
      FOR v_indice = 1 TO v_total_casos_procesados.getLength()
         DISPLAY "5"
         IF v_total_casos_procesados[v_indice].estado = v_rec_detalle_string.id_06 THEN
            LET v_total_casos_procesados[v_indice].casos = v_total_casos_procesados[v_indice].casos + 1
            EXIT FOR
         END IF
         --Si el caso no se encontro se registra uno nuevo
         IF v_indice = v_total_casos_procesados.getLength() THEN 
            LET v_total_casos_procesados[v_indice+1].estado = v_rec_detalle_string.id_06
            LET v_total_casos_procesados[v_indice+1].casos = 1
            EXIT FOR
         END IF
      END FOR
      --Se dejan los arrays como nulos
      INITIALIZE v_rec_detalle TO NULL
      INITIALIZE v_rec_detalle_string TO NULL
   END FOREACH

   --Se cierra el archivo
   CALL v_archivo_mandatos.CLOSE()
   DISPLAY "5"
   -- INICIA Generación del reporte de canales
   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("mdt") RETURNING r_ruta_bin, r_ruta_listados
   DISPLAY "Ruta bin - ", r_ruta_bin
   DISPLAY "Ruta lst - ", r_ruta_listados    
   
   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/MDTS061.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_selectDevice("PDF") 
      LET v_v_nom_reporte = p_usuario_cod CLIPPED, "-MDTS06-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
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
   DISPLAY "6"
   -- inicia el reporte de registros procesados
   START REPORT reporte_reg_proc TO XML HANDLER v_manejador_rpt

   -- Recupera descripcion del tipo de origen
   SELECT des_origen 
     INTO v_r_rpt_res.des_origen 
     FROM mdt_cat_origen
   WHERE id_origen = 2
   
   -- Recupera descripcion del proceso
   SELECT proceso_desc 
     INTO v_r_rpt_res.proceso_desc 
     FROM cat_proceso
   WHERE proceso_cod = g_proceso_cod

   LET tot_altas_proc = 0
   LET tot_bajas_proc = 0
   LET tot_modificaciones_proc = 0

   FOR v_indice = 1 TO v_total_casos_procesados.getLength()
      IF v_total_casos_procesados[v_indice].estado = 'A' THEN
         LET tot_altas_proc = v_total_casos_procesados[v_indice].casos
      END IF
      IF v_total_casos_procesados[v_indice].estado = 'B' THEN
         LET tot_bajas_proc = v_total_casos_procesados[v_indice].casos
      END IF
      IF v_total_casos_procesados[v_indice].estado = 'M' THEN
         LET tot_modificaciones_proc = v_total_casos_procesados[v_indice].casos
      END IF
   END FOR
   
   LET v_r_rpt_res.folio   = p_num_folio
   LET v_r_rpt_res.f_proceso = TODAY
   LET v_r_rpt_res.altas = tot_altas_proc
   LET v_r_rpt_res.bajas = tot_bajas_proc
   LET v_r_rpt_res.modif = tot_modificaciones_proc
   
   OUTPUT TO REPORT reporte_reg_proc(v_r_rpt_res.*)
   
   -- finaliza el reporte
   FINISH REPORT reporte_reg_proc
      
   
END FUNCTION

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTM06                                                   #
#Objetivo          => Genera reporte de mandatos                               #
#Autor             =>                                                          #
#Fecha Inicio      =>                                                          #
################################################################################
REPORT reporte_reg_proc(p_r_res)
   DEFINE p_r_res   RECORD -- registro de resumen
             des_origen   LIKE mdt_cat_origen.des_origen,
             proceso_desc LIKE cat_proceso.proceso_desc,
             lote         LIKE mdt_solicitud_mandato.folio,
             f_lote       LIKE mdt_lote_mandato.f_proceso,
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD,
          v_total INTEGER

   FORMAT

   FIRST PAGE HEADER
      PRINTX p_r_res.des_origen  
      PRINTX p_r_res.proceso_desc
      PRINTX p_r_res.lote        
      PRINTX p_r_res.f_lote
      LET v_total = p_r_res.altas + p_r_res.bajas + p_r_res.modif        
      PRINTX p_r_res.altas USING "#########&"
      PRINTX p_r_res.bajas USING "#########&"
      PRINTX p_r_res.modif USING "#########&"
      PRINTX v_total USING "#########&"

END REPORT