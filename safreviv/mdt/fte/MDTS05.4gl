--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 31-03-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => MDT                                                     #
#Programa          => MDTS05                                                  #
#Objetivo          => Generar Archivo con Solicitudes de instrucciones de     #
#                     mandatos rechazados con origen CANALES provenientes de  #
#                     sustentabilidad                                         #
#Autor             => Hugo César Ramírez Gracía                               #
#Fecha Inicio      => 17/02/2012                                              #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, # Código del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, # Código de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, # Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod, # Código de operacion
   g_ruta_envio                LIKE seg_modulo.ruta_envio,
   v_nom_archivo               STRING,
   v_total_casos    DYNAMIC ARRAY OF RECORD
      estado  LIKE mdt_solicitud_mandato.tipo_operacion,
      casos   INTEGER
   END RECORD,
   v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado                LIKE mdt_solicitud_mandato.tipo_operacion,
      grupo_diagnostico     DYNAMIC ARRAY OF RECORD
         diagnostico        LIKE mdt_solicitud_mandato.diagnostico,
         casos              INTEGER
      END RECORD      
   END RECORD,
   v_total_procesados INTEGER,
   v_total_x_procesar INTEGER
DEFINE g_folio LIKE glo_ctr_archivo.folio

END GLOBALS

#Objetivo:
MAIN
DEFINE 
   p_num_folio       DECIMAL(9),  
   v_estatus         SMALLINT,
   v_indice          INTEGER,
   v_indice_diag     INTEGER,
   v_hora            CHAR(8), 
   v_consulta        STRING,
   v_proceso_desc    LIKE cat_proceso.proceso_desc,
   v_operacion_desc  LIKE cat_operacion.opera_desc,
   v_fec_ejecucion   DATETIME YEAR TO SECOND 

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
   LET g_opera_cod   = ARG_VAL(4)  # Número de operacion
   #Quinto parámetro
   LET g_folio       = ARG_VAL(5)
   #Sexto parámetro
   LET v_nom_archivo = ARG_VAL(6)
   
   LET v_hora = TIME (CURRENT HOUR TO SECOND) 
   LET p_num_folio = 0

   SELECT ruta_envio
     INTO g_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'mdt' 

   # recuepra la descripcion del proceso y operacion
   LET v_consulta = "\n SELECT FIRST 1 pro.proceso_desc, ope.opera_desc ",
                    "\n   FROM cat_proceso pro JOIN cat_operacion ope",
                    "\n     ON ope.proceso_cod = pro.proceso_cod",
                    "\n  WHERE pro.proceso_cod = ?"
   PREPARE prp_rec_desc_proceso FROM v_consulta
   EXECUTE prp_rec_desc_proceso USING g_proceso_cod INTO v_proceso_desc, v_operacion_desc
    
   LET v_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                      ,v_hora[1,2] USING "&&", v_hora[4,5] USING "&&", v_hora[7,8] USING "&&", "02.RCN"
      
   --CALL fn_display_proceso(0,"GENERAR ARCHIVO DE MANDATOS - CANALES")
   DISPLAY " PID ASIGNADO :",g_pid
   # Se escribe la descripción del proceso
   DISPLAY "\n PROCESO:   ",v_proceso_desc CLIPPED
   DISPLAY "\n OPERACIÓN: ",v_operacion_desc CLIPPED
   DISPLAY "\n FECHA:     ",TODAY

   # Recupera conteo de registros rechazados y agrupados por tipo de operacion
   CALL fn_recupera_total_registros()
   DISPLAY "\n TOTAL DE REGISTROS POR PROCESAR:",v_total_x_procesar
   DISPLAY "\n REGISTROS A ACTUALIZAR POR ESTADO:"
   FOR v_indice = 1 TO v_total_casos.getLength()
      DISPLAY "    ESTADO: ",v_total_casos[v_indice].estado," - CASOS : ",v_total_casos[v_indice].casos
   END FOR
   DISPLAY '\n GENERANDO RECHAZOS DE INSTRUCCIONES DE MANDATOS A CANALES'
   # Se marca el proceso como inciado
   CALL fn_actualiza_opera_ini (g_pid, g_proceso_cod, g_opera_cod, p_num_folio,
                                "MDTS05", v_nom_archivo, p_usuario_cod)
                                RETURNING v_estatus
   IF(v_estatus)THEN
      # Imprime el mensaje de inconsistencia en consola y archivo
      CALL fn_desplega_inc_operacion(v_estatus)
      EXIT PROGRAM           
   END IF
                                  
   # Se genera el archivo
   CALL f_genera_archivo_mandatos()

   CALL fn_actualiza_opera_fin (g_pid, g_proceso_cod, g_opera_cod)
                                RETURNING v_estatus
   IF(v_estatus)THEN
      # Imprime el mensaje de inconsistencia en consola y archivo
      CALL fn_desplega_inc_operacion(v_estatus)
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
   DISPLAY "\n TOTAL DE REGISTROS PROCESADOS: ",v_total_procesados
   DISPLAY "\n REGISTROS PROCESADOS POR ESTADO:"
   FOR v_indice = 1 TO v_total_casos_procesados.getLength()
      DISPLAY "\n   ESTADO: ",v_total_casos_procesados[v_indice].estado      
      FOR v_indice_diag = 1 TO v_total_casos_procesados[v_indice].grupo_diagnostico.getLength()
         DISPLAY "\n     DIAGNOSTICO: ",v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].diagnostico," - CASOS : ",v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].casos      
      END FOR
   END FOR

   DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",v_nom_archivo
   DISPLAY "\n FECHA FIN:        ",TODAY
   DISPLAY "\n HORA FIN:         ",TIME(CURRENT)
   
END MAIN

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM05                                                  #
#Objetivo          => Obtiene el total de registros rechazados                #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 17/02/2012                                              #
###############################################################################
FUNCTION fn_recupera_total_registros()
DEFINE v_consulta STRING,
       v_indice   INTEGER
       
   LET v_total_x_procesar = 0
   LET v_consulta = "\n SELECT tipo_operacion,COUNT(*)",
                    "\n FROM mdt_solicitud_mandato",
                    "\n WHERE estado = 106",
                    "\n AND id_origen = 2",
                    "\n GROUP BY 1"
   PREPARE prp_total_casos_previo FROM v_consulta
   DECLARE cur_total_casos_previo CURSOR FOR prp_total_casos_previo
   LET v_indice = 1
   FOREACH cur_total_casos_previo INTO v_total_casos[v_indice].*
      LET v_indice = v_indice + 1
      LET v_total_x_procesar = v_total_x_procesar + v_total_casos[v_indice].casos
   END FOREACH
   IF v_total_casos[v_total_casos.getLength()].estado IS NULL THEN
      CALL v_total_casos.deleteElement(v_total_casos.getLength())
   END IF
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM065                                                 #
#Objetivo          => Genera el archivo de rechazos de solicitudes de         #
#                     mandatos                                                #
#Autor             => Hugo César Ramírez Gracía                               #
#Fecha Inicio      => 17/02/2012                                              #
###############################################################################
FUNCTION f_genera_archivo_mandatos()
   #Se definen las variables para el archivo fisico
DEFINE v_canal            BASE.CHANNEL,
       v_query            STRING,
       v_string_detalle   STRING,
       v_indice           INTEGER,
       v_id_lote          INTEGER,
       v_id_lote_mandato  LIKE mdt_lote_mandato.folio, 
       v_lote_x_dia       LIKE mdt_lote_mandato.folio,
       v_rec_detalle RECORD # Record del detalle del archivo
         id_01       CHAR(2 ),      --X-tipo registro            
         id_02       CHAR(11),      --X-nss                      
         id_03       DECIMAL(10,0), --9-número de crédito        
         id_04       INTEGER,       --DECIMAL( 5,0) --9-consecutivo              
         id_05       CHAR(7 ),      --X-identificador de Mandato 
         id_06       CHAR(1),       --x-Tipo de Operación        
         id_07       DATE,          --X-Fecha Captura            
         id_08       DATE,          --X-Fecha inicio mandato     
         id_09       DATE,          --X-Fecha culminación mandato
         id_10       DECIMAL( 1,0), --N-Tipo Descuento           
         id_11       DECIMAL(16,6), --N-Valor Descuento          
         id_12       CHAR(40),      --X-Referencia               
         id_13       DECIMAL(1,0),  --9-Orígen                   
         id_14       CHAR(2 ),      --X-Resultado Operación      
         id_15       CHAR(3 ),      --X-Diagnóstico              
         id_solicitud_mandato  LIKE mdt_solicitud_mandato.id_solicitud_mandato
       END RECORD,
       v_rec_detalle_string RECORD
         id_01 CHAR(2),    --X-tipo registro            
         id_02 CHAR(11),   --X-nss                      
         id_03 CHAR(10),   --9-número de crédito        
         id_04 CHAR(5),    --9-consecutivo              
         id_05 CHAR(7),    --X-identificador de Mandato 
         id_06 CHAR(1),    --9-Tipo de Operación        
         id_07 CHAR(8),    --X-Fecha Captura            
         id_08 CHAR(8),    --X-Fecha inicio mandato     
         id_09 CHAR(8),    --X-Fecha culminación mandato
         id_10 CHAR(1),    --N-Tipo Descuento           
         id_11 CHAR(15),   --N-Valor Descuento          
         id_12 CHAR(40),   --X-Referencia               
         id_13 CHAR(1),    --9-Orígen                   
         id_14 CHAR(2),    --X-Resultado Operación      
         id_15 CHAR(3)     --X-Diagnóstico              
       END RECORD,
       v_indice_diag  INTEGER

   # Se crea el manejador de archivo
   LET v_canal = BASE.CHANNEL.CREATE()
   # Se crea archivo y se indica que se escribira en el mismo
   CALL v_canal.openFile(g_ruta_envio CLIPPED||"/"||v_nom_archivo, "w" )
   CALL v_canal.setDelimiter("")
   LET v_total_procesados = 0
   LET v_id_lote = 1
   # Se obtiene el consecutivo del dia
   SELECT (NVL(MAX(lote),0)+1) 
     INTO v_lote_x_dia
     FROM mdt_lote_mandato
    WHERE f_lote = TODAY      
   
   LET v_query =  "\n SELECT '02',nss,id_credito, 0 ,id_mandato,       "
                 ,"\n tipo_operacion,f_canales,f_inicio_mandato,       "
                 ,"\n f_culmina_mandato,tpo_descuento_mandato,         "
                 ,"\n valor_descuento_mandato,referencia,2,'02',diagnostico ,"
                 ,"\n id_solicitud_mandato                             "
                 ,"\n FROM mdt_solicitud_mandato                       "
                 ,"\n WHERE estado = 106                               "
                 ,"\n AND id_origen = 2                                "

   PREPARE prp_datos_archivo FROM v_query
   DECLARE cur_datos_archivo CURSOR FOR prp_datos_archivo


   # Se inserta el registro en la tabla mdt_lote_mandato
   SELECT (NVL(MAX(id_lote_mandato),0) + 1)
     INTO v_id_lote_mandato
     FROM mdt_lote_mandato
   
   INSERT INTO mdt_lote_mandato
          (id_lote_mandato, id_origen, f_lote, lote,
           f_carga_scr, f_procesof_mdt, estado)
   VALUES(v_id_lote_mandato, 2, TODAY, v_lote_x_dia,
          NULL, TODAY, 103)
          
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
                                 ,v_rec_detalle.id_solicitud_mandato
      # Se pasan los datos obtenidos a su equivalente en tipo char
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
      # Se asiga todo lo obtenido a una sola cadena que sera escrita en el archivo
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

      # Se escribe en el archivo los datos obtenidos
      CALL v_canal.WRITE([v_string_detalle])
      ##************************************************************************
      ##
      # Se invoca funcion que envia rechazadas a canales

      #
      ##
      ##************************************************************************

      # Se actualiza el estatus del registro insertado
      UPDATE mdt_solicitud_mandato
         SET f_lote  = TODAY,
             lote    = v_lote_x_dia,  # consecutivo del dia de generacion
             id_lote = v_id_lote,     # del consecutivo(serial) id_lote_mandato
             estado  = 107
       WHERE id_solicitud_mandato = v_rec_detalle.id_solicitud_mandato

      
      # Se incrementa el contador general
      LET v_total_procesados = v_total_procesados + 1
      LET v_id_lote = v_id_lote + 1
      # Se verifica que el array de control de datos insertados por tipo no este vacio 
      IF v_total_casos_procesados.getLength() = 0 THEN
         LET v_total_casos_procesados[1].estado = v_rec_detalle_string.id_06
         LET v_total_casos_procesados[1].grupo_diagnostico[1].diagnostico = v_rec_detalle_string.id_15
         LET v_total_casos_procesados[1].grupo_diagnostico[1].casos = 0
      END IF
      # Se incrementa el contador por tipo de registro
      FOR v_indice = 1 TO v_total_casos_procesados.getLength()
         IF v_total_casos_procesados[v_indice].estado = v_rec_detalle_string.id_06 THEN
            --LET v_total_casos_procesados[v_indice].casos = v_total_casos_procesados[v_indice].casos + 1
            FOR v_indice_diag = 1 TO v_total_casos_procesados[v_indice].grupo_diagnostico.getLength()
               IF(v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].diagnostico = v_rec_detalle_string.id_15)THEN
                  LET v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].casos = v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].casos + 1
                  EXIT FOR
               END IF
               IF(v_indice_diag = v_total_casos_procesados[v_indice].grupo_diagnostico.getLength())THEN
                  LET v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].diagnostico = v_rec_detalle_string.id_15
                  LET v_total_casos_procesados[v_indice].grupo_diagnostico[v_indice_diag].casos = 1
               END IF 
            END FOR
            EXIT FOR
         END IF
         # Si el caso no se encontro se registra uno nuevo
         IF(v_indice = v_total_casos_procesados.getLength())THEN
            LET v_total_casos_procesados[v_indice+1].estado = v_rec_detalle_string.id_06
            LET v_total_casos_procesados[v_indice+1].grupo_diagnostico[1].diagnostico = v_rec_detalle_string.id_15
            LET v_total_casos_procesados[v_indice+1].grupo_diagnostico[1].casos = 1
            EXIT FOR
         END IF
      END FOR
      --Se dejan los arrays como nulos
      INITIALIZE v_rec_detalle TO NULL
      INITIALIZE v_rec_detalle_string TO NULL
   END FOREACH
   --Se cierra el archivo
   CALL v_canal.CLOSE()
   
END FUNCTION