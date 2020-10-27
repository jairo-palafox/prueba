--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13-07-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Modulo            => MDT                                                     #
#Programa          => MDTS07                                                  #
#Objetivo          => Generar Archivo con Solicitudes de instrucciones de     #
#                     mandatos con origen CANALES que fueron aceptadas ó      #
#                     rechazadas                                              #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv

DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_usuario_cod LIKE seg_usuario.usuario_cod, -- Clave de usuario
       g_ruta_envio  LIKE seg_modulo.ruta_envio,
       p_archivo     STRING,
       v_nom_archivo STRING
   
DEFINE v_total_casos, 
       v_total_casos_procesados DYNAMIC ARRAY OF RECORD
         estado  CHAR(1),
         casos   INTEGER
       END RECORD,
       v_total_procesados INTEGER,
       v_total_x_procesar INTEGER,
       p_num_folio        DECIMAL(9),
       v_cod_ws           STRING


######################################################################
#               Inicia el MAIN
######################################################################
MAIN
DEFINE v_estatus       SMALLINT,
       v_indice        INTEGER,
       v_hora          CHAR(8),
       v_query         STRING,
       v_fec_ejecucion DATETIME YEAR TO SECOND,
       v_error_ws      BOOLEAN 
  
   # se recupera la hora a la que inició la operacion
   LET v_fec_ejecucion = CURRENT YEAR TO SECOND 
   #Si se ha recibido parámetros se continua
   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)  --numero de proceso
   LET p_num_folio = ARG_VAL(5)
   LET p_archivo    = ARG_VAL(6)
   LET v_hora = TIME (CURRENT HOUR TO SECOND)

   SELECT ruta_envio
     INTO g_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   --Formato del nombre del archivo AAAAMMDDHHSSO2.RVS 
   LET v_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                       ,v_hora[1,2] USING "&&", v_hora[4,5] USING "&&", v_hora[7,8] USING "&&", "02.RVS"

   DISPLAY " PID ASIGNADO :",g_pid
   CALL fn_display_proceso(0,"GENERAR ARCHIVO DE MANDATOS - CANALES")
      
   --Se escribe la descripción del proceso
   DISPLAY "\n DESCRIPCIÓN DEL PROCESO:"
   DISPLAY " Generar Archivo con Solicitudes de instrucciones de mandatos con origen CANALES que fueron validadas y rechazadas ó aceptadas por Sustentabilidad."

   CALL fn_total_registros()
      
   DISPLAY "\n TOTAL DE REGISTROS POR PROCESAR:",v_total_x_procesar
   DISPLAY "\n REGISTROS A ACTUALIZAR POR ESTADO:"
   FOR v_indice = 1 TO v_total_casos.getLength()
      DISPLAY "    ESTADO: ",v_total_casos[v_indice].estado," - CASOS : ",v_total_casos[v_indice].casos
   END FOR
   DISPLAY '\n GENERANDO NOTIFICACION DE VALIDACIÓN SUSTENTABILIDAD'
   --Se marca el proceso como iniciado
   {CALL fn_actualiza_opera_ini (g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                p_num_folio,
                                "MDTS07",
                                v_nom_archivo,
                                p_usuario_cod)
               RETURNING v_estatus}
                                  
   --Se genera el archivo
   CALL f_genera_archivo_mandatos() RETURNING v_error_ws

   IF(v_error_ws)THEN
      # no se establece la operacion en erronea, ya que se si se llegaron a enviar
      # mandatos, no se pueden volver a tomar en cuenta
      # solo se imprime un mensaje que hubó un fallo con el servicio web
      DISPLAY "\n OCURRIÓ UN FALLO CON EL SERVICIO WEB, SE DETUVO LA TRANSFERENCIA DE INFORMACIÓN"
      DISPLAY "PUEDE VOLVER A INTENTAR EN OTRO MOMENTO\n"
      DISPLAY "CÓDIGO WS:",v_cod_ws
      DISPLAY "\n\n"
   ELSE
      CALL fn_actualiza_opera_fin (g_pid,
                                   g_proceso_cod,
                                   g_opera_cod)
                    RETURNING v_estatus
   END IF


   DISPLAY "\n TOTAL DE REGISTROS PROCESADOS:",v_total_procesados
      
   DISPLAY "\n REGISTROS PROCESADOS POR ESTADO:"

   FOR v_indice = 1 TO v_total_casos_procesados.getLength()
      DISPLAY "    ESTADO: ",v_total_casos_procesados[v_indice].estado," - CASOS : ",v_total_casos_procesados[v_indice].casos
   END FOR

   DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",v_nom_archivo

   CALL fn_display_proceso(1,"GENERAR ARCHIVO DE MANDATOS - CANALES")
      
   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod, 
                          NULL, 
                          'Notificación de instrucciones de mandatos',
                          'ID Proceso   : '||g_pid||
                          'Proceso      : '||g_proceso_cod||
                          'Operacion    : '||g_opera_cod||
                          'Fecha Inicio : '||v_fec_ejecucion||
                          'Fecha Fin    : '||CURRENT YEAR TO SECOND 
                          )
   
END MAIN

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Obtiene el total de registros que se van a modificar    #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_total_registros()
   DEFINE v_query  STRING
         ,v_indice INTEGER,
         v_folio_aux SMALLINT
   LET v_total_x_procesar = 0
   LET v_folio_aux = p_num_folio
   
   LET v_query ="\n SELECT a.tipo_operacion, COUNT(*)           ",
                "\n   FROM safre_viv:mdt_solicitud_mandato AS a ",
                "\n  WHERE a.estado IN (105, 108)               ",
                "\n    AND a.id_origen = 2                      ",
                "\n    AND EXISTS(SELECT 'OK' FROM safre_viv:mdt_lote_mandato AS b ",
                "\n                WHERE b.folio = a.folio  ",
                "\n                  AND b.id_origen = 2    ",
                "\n                  AND b.estado    = 102  ",
                "\n                  AND b.folio = ?)       ",
                "\n  GROUP BY 1"
   
   PREPARE prp_total_casos_previo FROM v_query
   DECLARE cur_total_casos_previo CURSOR FOR prp_total_casos_previo
   LET v_indice = 1
   FOREACH cur_total_casos_previo USING v_folio_aux INTO v_total_casos[v_indice].*
      LET v_total_x_procesar = v_total_x_procesar + v_total_casos[v_indice].casos
      LET v_indice = v_indice + 1
   END FOREACH
   IF v_total_casos[v_total_casos.getLength()].estado IS NULL THEN
      CALL v_total_casos.deleteElement(v_total_casos.getLength())
   END IF
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Genera el archivo de mandatos                           #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_genera_archivo_mandatos()
   --Se definen las variables para el archivo fisico
DEFINE v_archivo_mandatos BASE.CHANNEL,
       v_query            STRING,
       v_string_detalle   STRING,
       v_indice           INTEGER,
       v_id_lote          INTEGER,
       v_estado           LIKE mdt_lote_mandato.estado,
       v_estado_aux       LIKE mdt_lote_mandato.estado

   --Record del detalle del archivo
DEFINE v_rec_detalle RECORD
         v_tpo_registro    CHAR(2),      --X-tipo registro            
         v_nss             LIKE mdt_solicitud_mandato.nss,--CHAR(11),      --X-nss                      
         v_id_credito      LIKE mdt_solicitud_mandato.id_credito,--DECIMAL(10,0), --9-número de crédito        
         v_consecutivo     INTEGER, --DECIMAL( 5,0) --9-consecutivo              
         v_cve_mandato     LIKE mdt_solicitud_mandato.cve_mandato,--CHAR(7),      --X-identificador de Mandato 
         v_tpo_operacion   LIKE mdt_solicitud_mandato.tipo_operacion,-- CHAR(1),       --x-Tipo de Operación        
         v_f_captura       LIKE mdt_solicitud_mandato.f_canales,-- DATE,          --X-Fecha Captura            
         v_f_ini_mandato   LIKE mdt_solicitud_mandato.f_inicio_mandato,-- DATE,          --X-Fecha inicio mandato     
         v_f_fin_mandato   LIKE mdt_solicitud_mandato.f_culmina_mandato,-- DATE,          --X-Fecha culminación mandato
         v_tpo_descuento   LIKE mdt_solicitud_mandato.tpo_descuento_mandato,-- DECIMAL(1,0), --N-Tipo Descuento           
         v_valor_descuento LIKE mdt_solicitud_mandato.valor_descuento_mandato,-- DECIMAL(16,6) --N-Valor Descuento          
         v_referencia      LIKE mdt_solicitud_mandato.referencia,-- CHAR(40)      --X-Referencia               
         v_id_origen       LIKE mdt_solicitud_mandato.id_origen,-- DECIMAL(1,0 ) --9-Orígen                   
         v_resultado_operacion CHAR(2 ),      --X-Resultado Operación      
         v_diagnostico     LIKE mdt_solicitud_mandato.diagnostico,-- CHAR(3 )      --X-Diagnóstico              
         v_id_solicitud_mandato LIKE mdt_solicitud_mandato.id_solicitud_mandato,
         v_id_canales           LIKE mdt_solicitud_mandato.id_canales
       END RECORD
DEFINE v_rec_detalle_string RECORD
         v_tpo_registro        CHAR(2),   # tipo registro            
         v_nss                 CHAR(11),  # nss                      
         v_id_credito          CHAR(10),  # número de crédito        
         v_consecutivo         CHAR(5),   # consecutivo              
         --v_id_mandato         CHAR(7),  # identificador de Mandato
         v_cve_mandato         CHAR(18),  # clave de Mandato  
         v_tpo_operacion       CHAR(1),   # Tipo de Operación        
         v_f_captura           CHAR(8),   # Fecha Captura            
         v_f_ini_mandato       CHAR(8),   # Fecha inicio mandato     
         v_f_fin_mandato       CHAR(8),   # Fecha culminación mandato
         v_tpo_descuento       CHAR(1),   # Tipo Descuento           
         v_valor_descuento     CHAR(15),  # Valor Descuento          
         v_referencia          CHAR(40),  # Referencia               
         v_id_origen           CHAR(1),   # Orígen                   
         v_resultado_operacion CHAR(2),   # Resultado Operación      
         v_diagnostico         CHAR(3)    # Diagnóstico              
       END RECORD
   # Record que devuelve el Web Service como respuesta de lo que sucedió al enviar la información
   # de instrucciones de mandatos aceptadas y rechazadas
DEFINE v_respuesta_mandato RECORD
         v_id_origen           LIKE mdt_solicitud_mandato.id_origen,
         v_nss                 LIKE mdt_solicitud_mandato.nss,
         v_id_credito          LIKE mdt_solicitud_mandato.id_credito,
         v_cve_mandato         LIKE mdt_solicitud_mandato.cve_mandato,
         v_tpo_descuento       LIKE mdt_solicitud_mandato.tpo_descuento_mandato,
         v_valor_descuento     LIKE mdt_solicitud_mandato.valor_descuento_mandato,
         v_f_canales           LIKE mdt_solicitud_mandato.f_canales,
         v_f_inicio_mandato    LIKE mdt_solicitud_mandato.f_inicio_mandato,
         v_f_culmina_mandato   LIKE mdt_solicitud_mandato.f_culmina_mandato,
         v_referencia          LIKE mdt_solicitud_mandato.referencia,
         v_id_canales          LIKE mdt_solicitud_mandato.id_canales,
         v_tipo_operacion      LIKE mdt_solicitud_mandato.tipo_operacion,
         v_resultado_operacion STRING,
         v_diagnostico         LIKE mdt_solicitud_mandato.diagnostico,
         v_diag_notifica       STRING
       END RECORD,
       v_error_ws BOOLEAN,
       v_folio_aux    SMALLINT

   # suponiendo que el web servis no fallará
   LET v_error_ws = FALSE
   -- se crea el manejador de archivo
   LET v_archivo_mandatos = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_mandatos.openFile(g_ruta_envio CLIPPED||"/"||v_nom_archivo, "w" )
   CALL v_archivo_mandatos.setDelimiter("")
   LET v_total_procesados = 0
   LET v_id_lote = 1
   LET v_folio_aux = p_num_folio

   LET v_query =  "\n SELECT '02', nss, id_credito, 0 ,cve_mandato," # id_mandato ya no existe, solo se agregan espacios, REVISAR QUE SE DEJAN ESPACIOS 
                 ,"\n        tipo_operacion, f_canales, f_inicio_mandato,"
                 ,"\n        f_culmina_mandato, tpo_descuento_mandato,"
                 ,"\n        valor_descuento_mandato, referencia, 2, '02',"
                 ,"\n        diagnostico, id_solicitud_mandato, estado,"
                 ,"\n        id_canales"
                 ,"\n   FROM mdt_solicitud_mandato AS a"
                 ,"\n  WHERE a.id_origen = 2"
                 ,"\n    AND a.estado IN (105, 108)"
                 ,"\n    AND EXISTS(SELECT 'OK' "
                 ,"\n                 FROM safre_viv:mdt_lote_mandato AS b "
                 ,"\n                WHERE b.folio = a.folio"
                 ,"\n                  AND b.id_origen = 2"
                 ,"\n                  AND b.estado    = 102"
                 ,"\n                  AND b.folio = ?)     "

   PREPARE prp_datos_archivo FROM v_query
   DECLARE cur_datos_archivo CURSOR FOR prp_datos_archivo
   
   FOREACH cur_datos_archivo USING v_folio_aux
                             INTO v_rec_detalle.v_id_origen
                                 ,v_rec_detalle.v_nss
                                 ,v_rec_detalle.v_id_credito
                                 ,v_rec_detalle.v_consecutivo
                                 ,v_rec_detalle.v_cve_mandato
                                 ,v_rec_detalle.v_tpo_operacion
                                 ,v_rec_detalle.v_f_captura
                                 ,v_rec_detalle.v_f_ini_mandato
                                 ,v_rec_detalle.v_f_fin_mandato
                                 ,v_rec_detalle.v_tpo_descuento
                                 ,v_rec_detalle.v_valor_descuento
                                 ,v_rec_detalle.v_referencia
                                 ,v_rec_detalle.v_id_origen
                                 ,v_rec_detalle.v_resultado_operacion
                                 ,v_rec_detalle.v_diagnostico
                                 ,v_rec_detalle.v_id_solicitud_mandato
                                 ,v_estado
                                 ,v_rec_detalle.v_id_canales
      --Se pasan los datos obtenidos a su equivalente en tipo char
      LET v_rec_detalle_string.v_id_origen           = v_rec_detalle.v_id_origen
      LET v_rec_detalle_string.v_nss                 = v_rec_detalle.v_nss
      LET v_rec_detalle_string.v_id_credito          = v_rec_detalle.v_id_credito USING "&&&&&&&&&&" --DECIMAL(10,0) --9-número de crédito   
      LET v_rec_detalle_string.v_consecutivo         = v_id_lote USING "&&&&&"                --DECIMAL( 5,0) --9-consecutivo 
      LET v_rec_detalle_string.v_cve_mandato         = v_rec_detalle.v_cve_mandato
      LET v_rec_detalle_string.v_tpo_operacion       = v_rec_detalle.v_tpo_operacion
      LET v_rec_detalle_string.v_f_captura           = YEAR(v_rec_detalle.v_f_captura) USING "&&&&", MONTH(v_rec_detalle.v_f_captura) USING "&&",DAY(v_rec_detalle.v_f_captura) USING "&&"
      LET v_rec_detalle_string.v_f_ini_mandato       = YEAR(v_rec_detalle.v_f_ini_mandato) USING "&&&&", MONTH(v_rec_detalle.v_f_ini_mandato) USING "&&",DAY(v_rec_detalle.v_f_ini_mandato) USING "&&"
      LET v_rec_detalle_string.v_f_fin_mandato       = YEAR(v_rec_detalle.v_f_fin_mandato) USING "&&&&", MONTH(v_rec_detalle.v_f_fin_mandato) USING "&&",DAY(v_rec_detalle.v_f_fin_mandato) USING "&&"
      LET v_rec_detalle_string.v_tpo_descuento       = v_rec_detalle.v_tpo_descuento USING "&"          --DECIMAL( 1,0) --N-Tipo Descuento   
      LET v_rec_detalle_string.v_valor_descuento     = (v_rec_detalle.v_valor_descuento * 1000000) USING "&&&&&&&&&&&&&&&" --CHAR(15)   --N-Valor Descuento
      LET v_rec_detalle_string.v_referencia          = v_rec_detalle.v_referencia
      LET v_rec_detalle_string.v_id_origen           = v_rec_detalle.v_id_origen USING "&"          --DECIMAL( 1,0) --9-Orígen                   
      LET v_rec_detalle_string.v_resultado_operacion = v_rec_detalle.v_resultado_operacion
      LET v_rec_detalle_string.v_diagnostico         = v_rec_detalle.v_diagnostico
      --Se asiga todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_string_detalle = v_rec_detalle_string.v_id_origen
                            ,v_rec_detalle_string.v_nss
                            ,v_rec_detalle_string.v_id_credito
                            ,v_rec_detalle_string.v_consecutivo
                            ,v_rec_detalle_string.v_cve_mandato
                            ,v_rec_detalle_string.v_tpo_operacion
                            ,v_rec_detalle_string.v_f_captura
                            ,v_rec_detalle_string.v_f_ini_mandato
                            ,v_rec_detalle_string.v_f_fin_mandato
                            ,v_rec_detalle_string.v_tpo_descuento
                            ,v_rec_detalle_string.v_valor_descuento
                            ,v_rec_detalle_string.v_referencia
                            ,v_rec_detalle_string.v_id_origen
                            ,v_rec_detalle_string.v_resultado_operacion
                            ,v_rec_detalle_string.v_diagnostico
      
      --Se escribe en el archivo los datos obtenidos
      CALL v_archivo_mandatos.WRITE([v_string_detalle])
      
      ##************************************************************************
      ##
      # HCRG
      # Se invoca funcion que envia aceptadas y rechazadas a canales
      
      CALL fn_notifica_instruccion_mdt_can(v_rec_detalle.v_id_origen, #id_origen
                                           v_rec_detalle.v_nss, #nss
                                           v_rec_detalle.v_id_credito, #id_credito
                                           --v_rec_detalle.id_05, #id_mandato
                                           v_rec_detalle.v_cve_mandato, #cve_mandato
                                           v_rec_detalle.v_tpo_descuento, #tpo_descuento_mandato
                                           v_rec_detalle.v_valor_descuento, #valor_descuento_mandato
                                           v_rec_detalle.v_f_captura, #f_canales
                                           v_rec_detalle.v_f_ini_mandato, #f_inicio_mandato
                                           v_rec_detalle.v_f_fin_mandato, #f_culmina_mandato
                                           v_rec_detalle.v_referencia, #referencia
                                           v_rec_detalle.v_id_canales, #id_canales
                                           v_rec_detalle.v_tpo_operacion, #tipo_operacion
                                           v_rec_detalle.v_resultado_operacion, #resultado_operacion
                                           v_rec_detalle.v_diagnostico  #diagnostico
                                           ) 
        RETURNING v_respuesta_mandato.*

      DISPLAY "RESPUESTA WS"
      DISPLAY "Diagnóstico: ",v_respuesta_mandato.v_diag_notifica
      # se revia si falló el web service
      IF(v_respuesta_mandato.v_diag_notifica CLIPPED <> '0')THEN
         LET v_cod_ws = v_respuesta_mandato.v_diag_notifica 
         # falló el web service y se detiene el envio de información
         LET v_error_ws = TRUE
         EXIT FOREACH
      END IF
      #
      ##
      ##************************************************************************

      --Se verifica el estatus que traia el registro
      CASE v_estado
         WHEN 108  LET v_estado_aux = 109 
         WHEN 105  LET v_estado_aux = 110
         OTHERWISE LET v_estado_aux = v_estado
      END CASE
      
      --Se actualiza el estatus del registro insertado
      UPDATE mdt_solicitud_mandato
         SET estado = v_estado_aux
       WHERE folio = p_num_folio

      --Se incrementa el contador general
      LET v_total_procesados = v_total_procesados + 1
      LET v_id_lote = v_id_lote + 1
      --Se verifica que el array de control de datos insertados por tipo no este vacio 
      IF v_total_casos_procesados.getLength() = 0 THEN
         LET v_total_casos_procesados[1].estado = v_rec_detalle_string.v_tpo_operacion
         LET v_total_casos_procesados[1].casos = 0
      END IF
      
      --Se incrementa el contador por tipo de registro
      FOR v_indice = 1 TO v_total_casos_procesados.getLength()
         IF v_total_casos_procesados[v_indice].estado = v_rec_detalle_string.v_tpo_operacion THEN
            LET v_total_casos_procesados[v_indice].casos = v_total_casos_procesados[v_indice].casos + 1
            EXIT FOR
         END IF
         --Si el caso no se encontro se registra uno nuevo
         IF v_indice = v_total_casos_procesados.getLength() THEN 
            LET v_total_casos_procesados[v_indice+1].estado = v_rec_detalle_string.v_tpo_operacion
            LET v_total_casos_procesados[v_indice+1].casos = 1
            EXIT FOR
         END IF
      END FOR
      --Se dejan los arrays como nulos
      INITIALIZE v_rec_detalle TO NULL
      INITIALIZE v_rec_detalle_string TO NULL
   END FOREACH
   FREE cur_datos_archivo
   --Se cierra el archivo
   CALL v_archivo_mandatos.CLOSE()
   # si fallo el web service no se actualiza el lote, para poderlo procesar en otra corrida
   # HCRG
   IF(v_error_ws = FALSE)THEN
      # el web service no falló y se actualiza el lote como enviado
      UPDATE mdt_lote_mandato 
         SET estado = 103
       WHERE id_origen = 2    
         AND estado = 102  
         AND folio = p_num_folio
   END IF
   RETURN v_error_ws
END FUNCTION