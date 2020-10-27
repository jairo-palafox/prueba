#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETE455                                                 #
#Objetivo        => Programa carga de archivo de Oficios Rojos              #
#Fecha Inicio    => Septiembre 14, 2017                                     #
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
        nss              CHAR(11),
        fch_contable     CHAR(10),
        imp_pago         CHAR(12)
      END RECORD,
      v_ch_archivo        base.channel -- archivo que se carga
       
   #Parametros
   CALL ARG_VAL(1) RETURNING p_usuario
   CALL ARG_VAL(2) RETURNING p_pid
   CALL ARG_VAL(3) RETURNING p_proceso
   CALL ARG_VAL(4) RETURNING p_operacion
   CALL ARG_VAL(5) RETURNING p_nom_archivo
   CALL ARG_VAL(6) RETURNING p_prog_a_lanzar

   -- se inicia el log del programa
   LET v_cadena_registros = p_usuario CLIPPED, ".", "RETE455.log"
   CALL STARTLOG(v_cadena_registros)
   
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
   WHERE  modulo_cod = "ret"
   
   -- la ruta completa del archivo es la ruta rescate mas el nombre del archivo
   LET v_ruta_archivo = v_ruta_rescate CLIPPED, "/", p_nom_archivo
   
   DISPLAY "Ruta archivo: ", v_ruta_archivo
   
   -- se abre el archivo
   CALL v_ch_archivo.openFile(v_ruta_archivo,"r")

   -- los registros del archivo estan separados por pipes
   CALL v_ch_archivo.setDelimiter("|")

   -- se genera la tabla temporal en base de datos TMP
   DISPLAY "Generando tabla temporal tmp_ret_oficios_rojos"

   -- cambio a base de datos temporal
   DATABASE safre_tmp

   -- se regenera la tabla temporal
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_ret_oficios_rojos"
   
   -- se inician los contadores
   LET v_reg_archivo   = 0
   LET v_reg_aceptados = 0
   
   PREPARE sid_tabla_temporal FROM v_sql
   EXECUTE sid_tabla_temporal
   -- se crea la tabla temporal
   LET v_sql = "\nCREATE TABLE tmp_ret_oficios_rojos(",
               "\n nss              CHAR(11), ",
               "\n fch_contable     CHAR(10), ",
               "\n imp_pago         CHAR(12)  ",
               "\n             );                    "

               
   PREPARE sid_crea_tabla FROM v_sql
   EXECUTE sid_crea_tabla
   
   -- se lee el archivo para guardar los datos
   WHILE ( v_ch_archivo.read([v_registro_archivo.*]) )
      --DISPLAY "Registro leido (NSS): ", v_registro_archivo.nss
          
         -- se cuenta un registro leido
         LET v_reg_archivo = v_reg_archivo + 1
         IF v_reg_archivo > 0 THEN 
            -- se mueve campo por campo para eliminar los espacios y tabuladores
            DISPLAY "El registro antes de formatearlo >", v_registro_archivo.*
            CALL fn_elimina_espacios(v_registro_archivo.*) RETURNING v_registro_archivo.*
            DISPLAY "El registro despues de formatearlo >", v_registro_archivo.*
            -- se inserta un registro
            INSERT INTO tmp_ret_oficios_rojos (nss, fch_contable, imp_pago)
                   VALUES (v_registro_archivo.nss,     
                           v_registro_archivo.fch_contable,      
                           v_registro_archivo.imp_pago)
           
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
   IF ( v_reg_archivo  = v_reg_aceptados ) THEN
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

FUNCTION fn_elimina_espacios(p_registro_archivo)
DEFINE p_registro_archivo  RECORD    -- registro del archivo
        nss              CHAR(11),
        fch_contable     CHAR(10),
        imp_pago         CHAR(12)
      END RECORD
      --DISPLAY "El registro antes de formatearlo >", p_registro_archivo.*
      LET p_registro_archivo.nss          = fn_valida_palabra_null(p_registro_archivo.nss          CLIPPED)
      LET p_registro_archivo.fch_contable = fn_valida_palabra_null(p_registro_archivo.fch_contable CLIPPED)
      LET p_registro_archivo.imp_pago     = fn_valida_palabra_null(p_registro_archivo.imp_pago     CLIPPED)
      
RETURN p_registro_archivo.*

      
END FUNCTION

FUNCTION fn_valida_palabra_null(p_cadena)
DEFINE p_cadena CHAR(60);
DEFINE v_paso   CHAR(60);
   LET v_paso = p_cadena CLIPPED 

   LET v_paso = fn_elimina_tabulador(v_paso)
   IF v_paso = "NULL" THEN 
      LET p_cadena = " "
   ELSE 
      LET p_cadena = v_paso
   END IF 
   
   RETURN p_cadena
   
END FUNCTION 

FUNCTION fn_elimina_tabulador(p_cadena)
DEFINE p_cadena       CHAR(60);
DEFINE v_paso         CHAR(60);
DEFINE v_cadena_final CHAR(60)
DEFINE v_iter         SMALLINT  
DEFINE v_num_carac    SMALLINT 

   LET v_paso = p_cadena CLIPPED 
   LET v_cadena_final = NULL
   LET v_num_carac = 1   
   
   FOR v_iter = 1 TO LENGTH(v_paso) 
      IF (v_paso[v_iter,v_iter]) <> ASCII(9) THEN
         LET v_cadena_final[v_num_carac, v_num_carac] = v_paso[v_iter,v_iter]
         LET v_num_carac = v_num_carac + 1
      END IF 
   END FOR  
   RETURN v_cadena_final
   
END FUNCTION 

