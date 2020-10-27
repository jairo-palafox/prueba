#########################################################################################
#M�dulo          => RET                                                                 #        
#Programa        => RETE444                                                             #
#Objetivo        => Programa carga del archivo del Excepciones de la Devoluci�n del SSV #
#Fecha Inicio    => 8 AGOSTO 2017                                                       #
#########################################################################################
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
       v_linea_lectura     CHAR(376), 
       v_registro_archivo  RECORD    -- registro del archivo
            num_delega       CHAR(02),
            nss              CHAR(11),
            beneficiario     CHAR(60),
            importe          CHAR(11),
            entidad          CHAR(02),
            juicio           CHAR(10),
            num_acuerdo      CHAR(10),
            desc_juez        CHAR(40),
            facultado        CHAR(50),
            puesto           CHAR(40),
            fch_ejecuta      CHAR(08),
            procede_juicio   CHAR(40),
            tipo_sol         CHAR(02),
            tipo_prod        CHAR(02),
            correo_elec      CHAR(40),
            cve_rechazo      CHAR(03),
            desc_rechazo     CHAR(40)
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
   LET v_cadena_registros = p_usuario CLIPPED, ".", "RETE444.log"
   CALL STARTLOG(v_cadena_registros)
   
   LET v_fecha_inicio = CURRENT YEAR TO SECOND
   LET r_resultado_opera = 0
   LET v_indice = 0   
   LET v_mensaje = " "
   #Recuper la informaci�n necesaria para cargar el archivo seleccionado 
   #dependidendo del proceso y operaci�n
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario 
   
   #Encabezado para el archivo de monitoreo
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACI�N          : ",v_opera_desc,"\n",
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
   --CALL v_ch_archivo.setDelimiter("|")

   -- se genera la tabla temporal en base de datos TMP
   DISPLAY "Generando tabla temporal tmp_ret_excep_devol_ssv"

   -- cambio a base de datos temporal
   DATABASE safre_tmp

   -- se regenera la tabla temporal
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_ret_excep_devol_ssv"
   
   -- se inician los contadores
   LET v_reg_archivo   = 0
   LET v_reg_aceptados = 0
   
   PREPARE sid_tabla_temporal FROM v_sql
   EXECUTE sid_tabla_temporal
   -- se crea la tabla temporal
   LET v_sql = "\nCREATE TABLE tmp_ret_excep_devol_ssv (",
               "\n num_delega       CHAR(02), ",
               "\n nss              CHAR(11), ",
               "\n beneficiario     CHAR(60), ",
               "\n importe          CHAR(11), ",
               "\n entidad          CHAR(02), ",
               "\n juicio           CHAR(10), ",
               "\n num_acuerdo      CHAR(10), ",
               "\n desc_juez        CHAR(40), ",
               "\n facultado        CHAR(50), ",
               "\n puesto           CHAR(40), ",
               "\n fch_ejecuta      CHAR(08), ",
               "\n procede_juicio   CHAR(40), ",
               "\n tipo_sol         CHAR(02), ",
               "\n tipo_prod        CHAR(02), ",
               "\n correo_elec      CHAR(40), ",
               "\n cve_rechazo      CHAR(03), ",
               "\n desc_rechazo     CHAR(40)  ",
               "\n             );                    "

               
   PREPARE sid_crea_tabla FROM v_sql
   EXECUTE sid_crea_tabla
   
   -- se lee el archivo para guardar los datos
   WHILE TRUE 
      LET v_linea_lectura =  v_ch_archivo.readLine()
      IF v_ch_archivo.isEof() THEN 
         EXIT WHILE 
      END IF 
      LET v_registro_archivo.num_delega     = v_linea_lectura[1,2]
      LET v_registro_archivo.nss            = v_linea_lectura[3,13]
      LET v_registro_archivo.beneficiario   = v_linea_lectura[14,73]
      LET v_registro_archivo.importe        = v_linea_lectura[74,84]
      LET v_registro_archivo.entidad        = v_linea_lectura[85,86]
      LET v_registro_archivo.juicio         = v_linea_lectura[87,96]
      LET v_registro_archivo.num_acuerdo    = v_linea_lectura[97,106]
      LET v_registro_archivo.desc_juez      = v_linea_lectura[107,148]
      LET v_registro_archivo.facultado      = v_linea_lectura[149,200]
      LET v_registro_archivo.puesto         = v_linea_lectura[201,240]
      LET v_registro_archivo.fch_ejecuta    = v_linea_lectura[241,248]
      LET v_registro_archivo.procede_juicio = v_linea_lectura[249,288]
      LET v_registro_archivo.tipo_sol       = v_linea_lectura[289,290]
      LET v_registro_archivo.tipo_prod      = v_linea_lectura[291,292]
      LET v_registro_archivo.correo_elec    = v_linea_lectura[293,332]
      LET v_registro_archivo.cve_rechazo    = v_linea_lectura[333,335]
      LET v_registro_archivo.desc_rechazo   = v_linea_lectura[336,375]

      --DISPLAY "Registro leido (NSS): ", v_registro_archivo.nss
          
         -- se cuenta un registro leido
         LET v_reg_archivo = v_reg_archivo + 1
         IF v_reg_archivo > 0 THEN 
            -- se mueve campo por campo para eliminar los espacios y tabuladores
            DISPLAY "El registro antes de formatearlo >", v_registro_archivo.*
            CALL fn_elimina_espacios(v_registro_archivo.*) RETURNING v_registro_archivo.*
            DISPLAY "El registro despues de formatearlo >", v_registro_archivo.*
            -- se inserta un registro
            INSERT INTO tmp_ret_excep_devol_ssv 
                          (num_delega, nss,       beneficiario, importe,
                           entidad,    juicio,    num_acuerdo,  desc_juez,
                           facultado,  puesto,    fch_ejecuta,  procede_juicio,
                           tipo_sol,   tipo_prod, correo_elec,  cve_rechazo, 
                           desc_rechazo)
                   VALUES (v_registro_archivo.num_delega,     v_registro_archivo.nss,      
                           v_registro_archivo.beneficiario,   v_registro_archivo.importe,
                           v_registro_archivo.entidad,        v_registro_archivo.juicio,       
                           v_registro_archivo.num_acuerdo,    v_registro_archivo.desc_juez,
                           v_registro_archivo.facultado,      v_registro_archivo.puesto,      
                           v_registro_archivo.fch_ejecuta,    v_registro_archivo.procede_juicio,
                           v_registro_archivo.tipo_sol,       v_registro_archivo.tipo_prod, 
                           v_registro_archivo.correo_elec,    v_registro_archivo.cve_rechazo,
                           v_registro_archivo.desc_rechazo)
           
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
   DISPLAY "Los contadores :"
   DISPLAY "Registros :", v_reg_archivo
   DISPLAY "Aceptados :", v_reg_aceptados
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
      
      -- Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
      
                          
   ELSE
      -- hubo un error en la carga
      DISPLAY "Hubo un error en la carga"
      LET v_mensaje = "Hubo un error en la carga"

      
      CALL fn_error_opera(p_pid,p_proceso,p_operacion) 
           RETURNING r_resultado_opera

            
      # Envia correo de estado de operaci�n
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
   END IF
END MAIN

FUNCTION fn_elimina_espacios(p_registro_archivo)
DEFINE p_registro_archivo  RECORD    -- registro del archivo
           num_delega       CHAR(02),
           nss              CHAR(11),
           beneficiario     CHAR(60),
           importe          CHAR(11),
           entidad          CHAR(02),
           juicio           CHAR(10),
           num_acuerdo      CHAR(10),
           desc_juez        CHAR(40),
           facultado        CHAR(50),
           puesto           CHAR(40),
           fch_ejecuta      CHAR(08),
           procede_juicio   CHAR(40),
           tipo_sol         CHAR(02),
           tipo_prod        CHAR(02),
           correo_elec      CHAR(40),
           cve_rechazo      CHAR(03),
           desc_rechazo     CHAR(40)
      END RECORD
      --DISPLAY "El registro antes de formatearlo >", p_registro_archivo.*
      LET p_registro_archivo.num_delega = fn_valida_palabra_null(p_registro_archivo.num_delega CLIPPED )
      LET p_registro_archivo.nss = fn_valida_palabra_null(p_registro_archivo.nss CLIPPED)
      LET p_registro_archivo.beneficiario = fn_valida_palabra_null(p_registro_archivo.beneficiario CLIPPED)
      LET p_registro_archivo.importe = fn_valida_palabra_null(p_registro_archivo.importe CLIPPED)
      LET p_registro_archivo.entidad = fn_valida_palabra_null(p_registro_archivo.entidad CLIPPED)
      LET p_registro_archivo.juicio = fn_valida_palabra_null(p_registro_archivo.juicio CLIPPED)
      LET p_registro_archivo.num_acuerdo = fn_valida_palabra_null(p_registro_archivo.num_acuerdo CLIPPED)
      LET p_registro_archivo.desc_juez = fn_valida_palabra_null(p_registro_archivo.desc_juez CLIPPED)
      LET p_registro_archivo.facultado = fn_valida_palabra_null(p_registro_archivo.facultado CLIPPED)
      LET p_registro_archivo.puesto = fn_valida_palabra_null(p_registro_archivo.puesto CLIPPED)
      LET p_registro_archivo.fch_ejecuta = fn_valida_palabra_null(p_registro_archivo.fch_ejecuta CLIPPED)
      LET p_registro_archivo.procede_juicio = fn_valida_palabra_null(p_registro_archivo.procede_juicio CLIPPED)
      LET p_registro_archivo.tipo_sol = fn_valida_palabra_null(p_registro_archivo.tipo_sol CLIPPED)
      LET p_registro_archivo.tipo_prod = fn_valida_palabra_null(p_registro_archivo.tipo_prod CLIPPED)
      LET p_registro_archivo.correo_elec = fn_valida_palabra_null(p_registro_archivo.correo_elec CLIPPED)
      LET p_registro_archivo.cve_rechazo = fn_valida_palabra_null(p_registro_archivo.cve_rechazo CLIPPED) 
      LET p_registro_archivo.desc_rechazo = fn_valida_palabra_null(p_registro_archivo.desc_rechazo CLIPPED) 
      
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

