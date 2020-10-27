#############################################################################
#Módulo          => AFI                                                     #        
#Programa        => AFIE11.4gl                                              #
#Objetivo        => Programa general de carga de archivo en modo batch      #
#Fecha Inicio    => 06 MARZO 2013                                           #
#############################################################################
DATABASE safre_viv
GLOBALS 
DEFINE v_reg_cargados_x_tabla DYNAMIC ARRAY OF RECORD
        v_registro CHAR(2),  --LIKE cat_layout.registro
        v_tabla    CHAR(30), --LIKE cat_layout.tabla
        v_conteo   INTEGER
       END RECORD
       
END GLOBALS 
#Objetivo: Carga Archivo en proceso nohup 
MAIN
DEFINE p_nom_archivo       STRING,
       p_proceso           LIKE cat_proceso.proceso_cod,
       p_operacion         LIKE cat_operacion.opera_cod,
       p_pid               DECIMAL(9,0),
       p_usuario           CHAR(20),
       p_prog_a_lanzar       STRING,
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      LIKE seg_modulo.ruta_rescate, -- ruta de rescate del modulo
       v_ruta_archivo      STRING, -- ruta completa del archivo leido
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_indice            INTEGER,
       v_detalle_monitoreo STRING,
       v_archivo_monitoreo STRING,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_reg_archivo       INTEGER,
       v_reg_aceptados     INTEGER,
       v_reg_rechazados    INTEGER,
       r_bnd_carga         BOOLEAN,
       r_resultado_opera   SMALLINT,
       g_reg_tab_cargados  INTEGER,
       v_comando           STRING,
       v_cadena_registros  STRING,
       v_canal             base.Channel,
       v_ltr_archivo       STRING,
       v_ltr_archivo_aux   STRING,
       v_reg_no_procesados INTEGER,
       v_fecha_inicio      DATETIME YEAR TO SECOND,
       v_mensaje           STRING,
       v_continua          BOOLEAN,
       v_sql               STRING,
       v_registro_archivo  RECORD    -- registro del archivo
         nss10              CHAR(10), -- NSS a 10 posiciones
         nss11              CHAR(11), -- NSS a 11 posiciones
         rfc                CHAR(13)  -- RFC que se actualizara
       END RECORD,
       v_ch_archivo        base.channel -- archivo que se carga
       
             
   --CALL STARTLOG("GLOE02.log")
   #Parametros
   CALL ARG_VAL(1) RETURNING p_usuario
   CALL ARG_VAL(2) RETURNING p_pid
   CALL ARG_VAL(3) RETURNING p_proceso
   CALL ARG_VAL(4) RETURNING p_operacion
   CALL ARG_VAL(5) RETURNING p_nom_archivo
   CALL ARG_VAL(6) RETURNING p_prog_a_lanzar

   LET v_fecha_inicio = CURRENT YEAR TO SECOND
   LET r_resultado_opera = 0
   LET v_indice = 0   
   LET v_mensaje = " "
   #Recuper la información necesaria para cargar el archivo seleccionado 
   #dependidendo del proceso y operación
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario 
   
   #Encabezado para el archivo de monitoreo
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " NOMBRE ARCHIVO     : ",p_nom_archivo,"\n",
                             " FECHA              : ",TODAY,"\n",
                             " HORA               : ",TIME(CURRENT),"\n \n \n"
                             
   DISPLAY "Inicio ","\n",v_detalle_monitoreo 
   #Nombre del archivo de monitoreo
   LET v_archivo_monitoreo = "nohup:",p_pid USING "&&&&&",":",p_proceso USING "&&&&&",":",p_operacion USING "&&&&&"
   DISPLAY "========================",v_archivo_monitoreo

   #Genera archivo de monitoreo
   CALL fn_monitorea_proceso(v_archivo_monitoreo,v_ruta_listados,v_detalle_monitoreo)    
   
   #Se elimina los espacios al final de cada variable
   LET v_ruta_rescate = v_ruta_rescate CLIPPED
   LET v_usuario      = v_usuario CLIPPED

   DISPLAY "Preparando lectura de archivo : ", p_nom_archivo
   
   -- se crea el apuntador para apertura y lectura de archivo
   LET v_ch_archivo = base.Channel.create()
   
   -- se obtiene la ruta de rescate del proceso
   SELECT ruta_rescate
   INTO   v_ruta_rescate
   FROM   seg_modulo
   WHERE modulo_cod = "afi"
   
   -- la ruta completa del archivo es la ruta rescate mas el nombre del archivo
   LET v_ruta_archivo = v_ruta_rescate CLIPPED, "/", p_nom_archivo
   
   DISPLAY "Ruta archivo: ", v_ruta_archivo
   
   -- se abre el archivo
   CALL v_ch_archivo.openFile(v_ruta_archivo,"r")

   -- se genera la tabla temporal en base de datos TMP
   DISPLAY "Generando tabla temporal tmp_afi_det_cambio_rfc"

   -- cambio a base de datos temporal
   DATABASE safre_tmp

   -- se regenera la tabla temporal
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_afi_det_cambio_rfc"
   
   -- se inician los contadores
   LET v_reg_archivo   = 0
   LET v_reg_aceptados = 0
   
   PREPARE sid_tabla_temporal FROM v_sql
   EXECUTE sid_tabla_temporal
   
   -- se crea la tabla temporal
   LET v_sql = "\nCREATE TABLE tmp_afi_det_cambio_rfc (",
               "\n             nss10 CHAR(10),         ",
               "\n             nss11 CHAR(11),         ",
               "\n             rfc   CHAR(13)          ",
               "\n             );                      "
                                                       
   PREPARE sid_crea_tabla FROM v_sql
   EXECUTE sid_crea_tabla
   
   -- se lee el archivo para guardar los datos
   WHILE ( NOT v_ch_archivo.isEof() )
      -- se lee un registro y se separa
      LET v_cadena_registros = v_ch_archivo.readLine()
     
      --DISPLAY "Registro leido: ", v_cadena_registros
     
      -- se descartan los registros vacios
      IF ( v_cadena_registros IS NOT NULL ) THEN
     
         -- se cuenta un registro leido
         LET v_reg_archivo = v_reg_archivo + 1
         
         -- los campos van seguidos unos de los otros de acuerdo a sus dimensiones
         LET v_registro_archivo.nss10 = v_cadena_registros.subString( 1, 10 )
         LET v_registro_archivo.nss11 = v_cadena_registros.subString( 11, 21 )
         LET v_registro_archivo.rfc   = v_cadena_registros.subString( 22, 34 )
         
         ---DISPLAY "REgistro a insercion: ", v_registro_archivo.*
         
         -- se inserta un registro
         INSERT INTO tmp_afi_det_cambio_rfc values (v_registro_archivo.*)
        
         -- si no hubo error en la insercion
         IF ( SQLCA.SQLCODE = 0 ) THEN
            -- se cuenta un registro aceptado
            LET v_reg_aceptados = v_reg_aceptados + 1
         END IF
      END IF

   END WHILE

   -- se regresa a safreviv
   DATABASE safre_viv

   -- si el numero de registros leidos es igual al de aceptados
   IF ( v_reg_archivo = v_reg_aceptados ) THEN
      -- la carga es correcta
      DISPLAY "La carga se ha realizdo correctamente..."
      
      -- Almacena registro para archivo procesado correctamente
      CALL fn_ingresa_etapa(p_proceso, p_operacion, p_nom_archivo) RETURNING r_bnd_carga
      
      IF ( r_bnd_carga ) THEN
         -- Finaliza la operacion de carga de archivo
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso,p_operacion)
                          RETURNING r_resultado_opera
      END IF

      LET v_mensaje = "El proceso de carga ha finalizado correctamente"
      
      -- Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
      
                          
   ELSE
      -- hubo un error en la carga
      DISPLAY "Hubo un error en la carga"
      LET v_mensaje = "Hubo un error en la carga"

      
      CALL fn_error_opera(p_pid,p_proceso,p_operacion) 
           RETURNING r_resultado_opera

            
      # Envia correo de estado de operación
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
   END IF
END MAIN


