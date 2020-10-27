#############################################################################
#Módulo          => RET                                                     #
#Programa        => RETL15.4gl                                              #
#Objetivo        => Lanzador de la carga de archivo de nuevos NSS de TRM    #
#Fecha Inicio    => 13 Enero 2014                                           #
#############################################################################
IMPORT os
GLOBALS "AFIG01.4gl"
DATABASE safre_viv
DEFINE g_ruta_rescate      STRING,--LIKE seg_modulo.ruta_rescate,
       g_usuario           LIKE seg_modulo.usuario,
       g_reg_archivo       INTEGER,
       g_reg_rechazados    INTEGER,
       g_reg_aceptados     INTEGER,
       g_detalle_monitoreo STRING,
       g_archivo_monitoreo STRING,
       g_ruta_listados     STRING,
       g_reg_tab_cargados  INTEGER
    
#Objetivo: Programa que permite elegir un archivo para cargar los NSS nuevos que llegan por TRM
MAIN
DEFINE p_proceso         LIKE cat_proceso.proceso_cod,
       p_operacion       LIKE cat_operacion.opera_cod,
       p_pid             DECIMAL(9,0),
       p_usuario         LIKE seg_usuario.usuario_cod,
       p_programa        LIKE bat_ctr_operacion.programa_cod,
	   v_folio           LIKE glo_folio.folio, -- folio del proceso
       v_layout          LIKE cat_operacion.layout_cod,
       v_extension       LIKE cat_operacion.extension,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_desc      LIKE cat_operacion.opera_desc,
       v_nom_archivo     STRING,
       v_nom_arch_opera  CHAR(40),
       --v_nom_arch_opera  LIKE bat_ctr_operacion.nom_archivo,
       v_nom_ventana     STRING,
       v_forma           STRING,
       v_bnd_ejecuta     BOOLEAN,
       v_bnd_cifras      BOOLEAN,
       r_bnd_carga       BOOLEAN,
       v_indice          INTEGER,
       v_aux_pid         STRING,
       v_aux_proc        STRING,
       v_aux_opera       STRING,
       v_comando         STRING,
       v_bnd_continua    BOOLEAN,
       v_cb_archivo      ui.ComboBox,
       v_ventana         ui.Window,
       v_forma_actual    ui.Form,
       r_resultado_opera SMALLINT,
       v_cadena_registros STRING,
       v_fecha_inicio     DATETIME YEAR TO SECOND,
       v_mensaje          STRING,
	   p_tipo_ejecucion   SMALLINT,
	   p_s_titulo         STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   # se inicia con 100 solo para no entrar en la parte de finalizar operacion
   LET r_resultado_opera = 0
   
   # Recuper la información necesaria para cargar el archivo seleccionado 
   # dependidendo del proceso y operación
   CALL STARTLOG(p_usuario CLIPPED||".RETL15.log")

   -- se especifica proceso y operacion
   LET p_proceso   = g_proceso_cod_afi_carga_nss_trm
   LET p_operacion = g_opera_cod_afi_carga_nss_trm

   -- se obtienen los datos del proceso
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc ,
                                         v_extension    , 
                                         v_opera_desc   ,
                                         v_layout       , 
                                         g_ruta_rescate ,
                                         g_ruta_listados,
                                         g_usuario 
                                         
   -- Elimina espacios en blanco al final de la cadena
   LET g_ruta_rescate = g_ruta_rescate CLIPPED

   -- Consulta que recupera la ruta en la que se ubicaran los archivos compilados
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = 'glo'

   LET v_bnd_ejecuta = FALSE

   -- se usa la forma de la carga general
   LET v_forma = v_ruta_ejecutable CLIPPED, "/GLOE011"
   
   #nombre del proceso para desplegar en la ventana 
   LET v_nom_ventana = v_opera_desc CLIPPED
   
   OPEN WINDOW w_gloe01 WITH FORM v_forma ATTRIBUTES (TEXT = v_nom_ventana)
   
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma_actual = v_ventana.getForm()
   
   #Oculta el grupo de cifras control, junto con los elementos contenidos
   CALL v_forma_actual.setElementHidden("grp_total_registros",1)
   
   #Se estableces el titulo de la ventana
   CALL v_ventana.setText(v_nom_ventana)
   
   #Recupera las propiedades del ComoBox, pasando el nombre del Combo como parámentro 
   LET v_cb_archivo = ui.ComboBox.forName("v_nom_archivo")
   
   #Elimina todos los elementos del combo
   CALL v_cb_archivo.clear()

   #Recupera los nombres de los archivos en el comboBox
   CALL fn_recupera_archivos_ruta(p_proceso, p_operacion,v_cb_archivo, v_extension, g_ruta_rescate) RETURNING v_bnd_continua

   -- eleccion del archivo que se cargara
   INPUT BY NAME v_nom_archivo
   ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      BEFORE INPUT
         
         # Bandera para determinar si se realizó el proceso de carga
         # o si se canceló 
         LET r_bnd_carga = FALSE
         LET r_resultado_opera = 0
         
         DISPLAY BY NAME v_proceso_desc, v_opera_desc
         
		 #Si no se ha recuperado algún archivo, se desactiva el boton Aceptar
         IF NOT(v_bnd_continua)THEN
            CALL DIALOG.setActionActive("accept",FALSE)
         END IF

      ON ACTION ACCEPT
         IF ( v_nom_archivo IS NULL OR v_nom_archivo = " ") THEN
            MESSAGE "Seleccione un archivo" ATTRIBUTE(REVERSE)
            NEXT FIELD v_nom_archivo
         END IF
                     
         # Nombre de archivo a tipo char
         LET v_nom_arch_opera  = v_nom_archivo
		 
         #Verifica los permisos de lectura y escritura del archivo
         IF ( fn_verifica_permisos_archivo(g_ruta_rescate ,v_nom_archivo) ) THEN
            # Inicializa el codigo a 0; no hay inconsistencia
            LET r_resultado_opera = 0

            LET p_pid = 0
            # se valida si se puede generar el proceso
            CALL fn_valida_operacion(p_pid,p_proceso,p_operacion) RETURNING r_resultado_opera
			
            IF ( r_resultado_opera <> 0 ) THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               LET v_bnd_ejecuta = FALSE
               EXIT INPUT
            END IF
			
            # se genera el pid para el proceso
            CALL fn_genera_pid(p_proceso,p_operacion,p_usuario)
                      RETURNING p_pid
            
			-- se inicia el proceso
			CALL fn_inicializa_proceso(p_pid,p_proceso,p_operacion,0,
                                       p_programa,v_nom_archivo,p_usuario)
                              RETURNING r_resultado_opera
            IF ( r_resultado_opera <> 0 ) THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               LET v_bnd_ejecuta = FALSE
               EXIT INPUT
            END IF
			
            # Inicia operación
            CALL fn_actualiza_opera_ini(p_pid,p_proceso,p_operacion,v_folio,"AFIL15",
                                  v_nom_arch_opera,p_usuario) RETURNING r_resultado_opera 
         
            # En el caso de que exista una inconsistencia al iniciar el proceso, se
            # Muestra un mensaje con la descripcion
            IF ( r_resultado_opera ) THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               #No se mostraran las cifras control
               LET v_bnd_ejecuta = FALSE
               EXIT INPUT
            END IF
			
            #se captura la fecha y hora en que se inició la operación
            LET v_fecha_inicio = CURRENT YEAR TO SECOND
                           
            #Quita los espacios al principio y al final de cada variable para el paso de parametros
            LET v_aux_pid   = p_pid
            LET v_aux_pid   = v_aux_pid.trim()                  
            LET v_aux_proc  = p_proceso
            LET v_aux_proc  = v_aux_proc.trim()
            LET v_aux_opera = p_operacion
            LET v_aux_opera = v_aux_opera.trim()
			LET v_folio = 0
			
			-- se obtiene la ruta ejecutable de afi
            SELECT ruta_bin
            INTO   v_ruta_ejecutable
            FROM   seg_modulo
            WHERE  modulo_cod = 'afi'
			
            #Construye comando
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/AFIP15.42r ",
                                            p_usuario      ," ",
											v_aux_pid      ," ",
											v_aux_proc     ," ",
                                            v_aux_opera    ," ",
											v_folio        ," '",
											v_nom_archivo  ,"' ",
                                          "1>>", g_ruta_listados CLIPPED ,
                                          "/nohup:",v_aux_pid USING "&&&&&",":",
                                                    v_aux_proc USING "&&&&&",":",
                                                    v_aux_opera   USING "&&&&&" ,
                                          " 2>&1 &"
                             
            DISPLAY v_comando 
            #Llamada al proceso de carga nohup (proceso independeinte al flujo de la aplicacion)
            RUN v_comando
			
            IF ( STATUS ) THEN
               CALL fn_mensaje("Carga de Archivo", 
                               "Ocurrió un error al iniciar el proceso batch",
                               "bn_about")
               CONTINUE INPUT
            ELSE
               # Se indica que se realizo el proceso de carga
               LET r_bnd_carga = TRUE
               CALL fn_mensaje("Carga de Archivo", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_aux_pid,
                               "bn_about")
            END IF
            #No se mostraran las cifras control
            LET v_bnd_ejecuta = FALSE
            
            EXIT INPUT
         ELSE
            CALL fn_mensaje("Carga de Archivo",
                            "El archivo a procesar no cuenta con los permisos suficientes",
                            "bn_about")
            LET r_bnd_carga = FALSE 
            NEXT FIELD v_nom_archivo
         END IF
         EXIT INPUT
         
      ON ACTION cancelar
         #No se mostraran las cifras control
         LET v_bnd_ejecuta = FALSE
         LET r_bnd_carga   = FALSE

         EXIT INPUT

   END INPUT 
   
   CLOSE WINDOW w_gloe01      

END MAIN

#Objetivo: Llena el comboBox de los nombres de archivo que se encuentren en la ruta correspondiente al proceso
FUNCTION fn_recupera_archivos_ruta(p_proceso, p_operacion,v_cb_archivo, p_extension, p_ruta_rescate)
DEFINE p_proceso         LIKE cat_proceso.proceso_cod,
       p_operacion       LIKE cat_operacion.opera_cod,
       v_cb_archivo      ui.ComboBox,
       v_archivo         STRING,
       v_lista_archivos  INTEGER,
       p_extension       STRING,
       v_existe_archivos BOOLEAN,
       v_bnd_continua    BOOLEAN,
       v_sql_qry         STRING,
       v_existe_archivo  BOOLEAN,
       v_tmp_archivo     VARCHAR(40),
	   p_ruta_rescate    STRING

   LET v_existe_archivos = FALSE
   LET v_bnd_continua = TRUE
   #Valida si existe la ruta del proceso donde se recuperaran los nombres de archivo
   IF NOT(os.Path.exists(p_ruta_rescate))THEN
      CALL fgl_winwait("No existe la ruta "||p_ruta_rescate)
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF
   
   #Valida si es un directorio valido
   IF NOT os.Path.isdirectory(p_ruta_rescate) THEN
      CALL fgl_winwait( "El parámetro proporcionado no es un directorio")
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF
   
   #Consulta que determina si ya se ha procesado el archivo
   LET v_sql_qry = "\n SELECT FIRST 1 NVL(1,0)",
                   "\n   FROM glo_ctr_archivo",
                   "\n  WHERE nombre_archivo = ?",
                   "\n    AND proceso_cod = ?",
                   "\n    AND opera_cod = ?",
                   "\n    AND estado IN (1,2)"
   PREPARE prp_existe_archivo FROM v_sql_qry 
   #Ordena los archivos por nombre descendente para los archivos de diropen   
   CALL os.Path.dirsort("name", "-1")
   #Solo se recuperan archivos
   # 1 - Excluye archivos ocultos, 2 - Excluye directorios, 4 - Excluye Links simbólicos 
   CALL os.Path.dirfmask( 1 + 2 + 4 )
   #Recupera la lista de archivos
   LET v_lista_archivos = os.Path.diropen(p_ruta_rescate)
   #Recupera el primer nombre de archivo
   LET v_archivo = os.Path.dirnext(v_lista_archivos)
   #Mientras no se recupere un valor nulo
   WHILE v_archivo IS NOT NULL
      
      #Filtro para solo agregar archivos al combo
      IF(v_archivo = "." OR v_archivo = ".." OR
         os.Path.isdirectory(v_archivo))THEN
         CONTINUE WHILE
      ELSE
         --DISPLAY "ARCHIVOS:--> ",v_archivo
         #Si la extencion corresponde a la extension del proceso, lo agrega al combo
         IF(os.Path.extension(v_archivo) = p_extension)THEN
            --DISPLAY "ARCHIVOS CON EXTENSION:--> ",v_archivo," EXT:",p_extension
            #Cambia de variable de tipo string a char, para poder usar en consulta
            LET v_tmp_archivo = v_archivo
            
            #Verifica si ya se ha procesado el archivo en glo_ctr_archivo
            EXECUTE prp_existe_archivo USING v_tmp_archivo,p_proceso,p_operacion INTO v_existe_archivo
            --DISPLAY "SQL:--> ",SQLCA.SQLCODE
            IF(SQLCA.SQLCODE = 100)THEN
               LET v_existe_archivo = 0
               --DISPLAY "BOOL:--> ",v_existe_archivo
            END IF
            
            IF NOT(v_existe_archivo)THEN
               --DISPLAY "ARCHIVOS A MOSTRAR:--> ",v_archivo
               #Añade el nombre del archivo al combo box
               CALL v_cb_archivo.additem( v_archivo, v_archivo )
            END IF
         END IF
      END IF
      #Recupera el siguiente archivo
      LET v_archivo = os.Path.dirnext(v_lista_archivos)
   END WHILE
   #Indica si no existe algun archivo
   IF NOT(v_cb_archivo.getItemCount())THEN
      CALL fgl_winwait("No se ha encontrado algún archivo")
      LET v_bnd_continua = FALSE
   END IF
   #Cierra la lista de archivos
   CALL os.Path.dirclose(v_lista_archivos)
   RETURN v_bnd_continua
END FUNCTION

