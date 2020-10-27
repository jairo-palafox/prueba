-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Módulo          => AFI                                                     -- 
-- Programa        => AFIG11.4gl                                             -- 
-- Objetivo        => Programa general de carga de archivo                    -- 
-- Fecha Inicio    => 06 marzo 2013                                           -- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
IMPORT os
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

DEFINE v_reg_cargados_x_tabla DYNAMIC ARRAY OF RECORD
        v_registro CHAR(2),  --LIKE cat_layout.registro
        v_tabla    CHAR(30), --LIKE cat_layout.tabla
        v_conteo   INTEGER
       END RECORD
    
-- Objetivo: Elige el archivo del combobox de acuerdo al proceso que llama la funcion y ejecuta el proceso en linea (1)
-- o en nohup (2) dependiendo del parametro p_tipo_carga
FUNCTION fn_carga_archivo(p_pid, p_proceso, p_operacion, p_tipo_carga, p_programa, p_prog_a_lanzar, p_usuario,p_inicia_proceso)
DEFINE p_proceso         LIKE cat_proceso.proceso_cod,
       p_operacion       LIKE cat_operacion.opera_cod,
       p_tipo_carga      SMALLINT,
       p_pid             DECIMAL(9,0),
       p_usuario         LIKE seg_usuario.usuario_cod,
       p_programa        LIKE bat_ctr_operacion.programa_cod,
       p_prog_a_lanzar   STRING,
       p_inicia_proceso  BOOLEAN
DEFINE v_layout          LIKE cat_operacion.layout_cod,
       v_extension       LIKE cat_operacion.extension,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_desc      LIKE cat_operacion.opera_desc,
       v_nom_archivo     STRING,
       v_nom_arch_opera  CHAR(40),       
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
       v_mensaje          STRING

CONSTANT c_folio = 0
   --  se inicia con 100 solo para no entrar en la parte de finalizar operacion
   LET r_resultado_opera = 0

   --  Aúnque este bloqueada la tabla, recupera la información   
   PREPARE prp_sqlIsolation FROM "SET ISOLATION TO DIRTY READ;"
   --EXECUTE prp_sqlIsolation
   
   --  Recuper la información necesaria para cargar el archivo seleccionado 
   --  dependidendo del proceso y operación
   CALL STARTLOG(p_usuario CLIPPED||".AFIG11.log")
   
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         g_ruta_rescate,g_ruta_listados,
                                         g_usuario 
   
   -- Elimina espacios en blanco al final de la cadena
   LET g_ruta_rescate = g_ruta_rescate CLIPPED
   -- Consulta que recupera la ruta en la que se ubicaran los archivos compilados
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'glo'

   LET v_bnd_ejecuta = FALSE
   -- Ruta en la que se encuntra la forma para la carga de archivos
   LET v_forma = v_ruta_ejecutable CLIPPED, "/GLOE011"
   -- nombre del proceso para desplegar en la ventana 
   LET v_nom_ventana = v_opera_desc CLIPPED
   
   -- abre ventana para elegir el archivo
   OPEN WINDOW w_RETGE01 WITH FORM v_forma ATTRIBUTES (TEXT = v_nom_ventana)
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma_actual = v_ventana.getForm()
      -- Oculta el grupo de cifras control, junto con los elementos contenidos
      CALL v_forma_actual.setElementHidden("grp_total_registros",1)
      -- Se estableces el titulo de la ventana
      CALL v_ventana.setText(v_nom_ventana)
    
      -- Recupera las propiedades del ComoBox, pasando el nombre del Combo como parámentro 
      LET v_cb_archivo = ui.ComboBox.forName("v_nom_archivo")
      -- Elimina todos los elementos del combo
      CALL v_cb_archivo.clear()
      -- Recupera los nombres de los archivos en el comboBox
      CALL fn_recupera_archivos(p_proceso, p_operacion,v_cb_archivo, v_extension) RETURNING v_bnd_continua

      INPUT BY NAME v_nom_archivo ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT            
            
            --  Bandera para determinar si se realizó el proceso de carga
            --  o si se canceló 
            LET r_bnd_carga = FALSE
            LET r_resultado_opera = 0
            
            DISPLAY BY NAME v_proceso_desc, v_opera_desc
            -- Si no se ha recuperado algún archivo, se desactiva el boton Aceptar
            IF NOT(v_bnd_continua ) THEN
               CALL DIALOG.setActionActive("accept",FALSE)
            END IF

         ON ACTION ACCEPT
            --  Recupera el nombre de archivo seleccionado
            LET v_nom_archivo = GET_FLDBUF(v_nom_archivo) CLIPPED
            
            IF ( v_nom_archivo IS NULL OR v_nom_archivo = " " ) THEN
               MESSAGE "Seleccione un archivo" ATTRIBUTE(REVERSE)
               NEXT FIELD v_nom_archivo
            END IF
                        
            --  Nombre de archivo a tipo char
            LET v_nom_arch_opera  = v_nom_archivo            
            -- Verifica los permisos de lectura y escritura del archivo
            IF ( fn_verifica_permisos_archivo(g_ruta_rescate ,v_nom_archivo) ) THEN
               --  Inicializa el codigo a 0; no hay inconsistencia
               LET r_resultado_opera = 0

               IF ( p_inicia_proceso ) THEN
                  --  En caso de que sea nulo se indica pid = 0 
                  IF ( p_pid IS NULL OR p_pid = ' ' ) THEN
                     LET p_pid = 0
                  END IF
                  --  se valida si se puede generar el proceso
                  CALL fn_valida_operacion(p_pid,p_proceso,p_operacion) RETURNING r_resultado_opera
                  
                  IF ( r_resultado_opera <> 0 ) THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                     LET v_bnd_ejecuta = FALSE
                     EXIT INPUT
                  END IF
                  
                  --  se genera el pid para el proceso
                  CALL fn_genera_pid(p_proceso,p_operacion,p_usuario)
                            RETURNING p_pid
                  
                  CALL fn_inicializa_proceso(p_pid,p_proceso,p_operacion,0,
                                             p_programa,v_nom_archivo,p_usuario)
                                    RETURNING r_resultado_opera
                  
                  IF ( r_resultado_opera <> 0 ) THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                     LET v_bnd_ejecuta = FALSE
                     EXIT INPUT
                  END IF
               END IF
               
               --  Inicia operación
               CALL fn_actualiza_opera_ini(p_pid,p_proceso,p_operacion,c_folio,"AFIG11",
                                     v_nom_arch_opera,p_usuario) RETURNING r_resultado_opera 
            
               --  En el caso de que exista una inconsistencia al iniciar el proceso, se
               --  Muestra un mensaje con la descripcion
               IF ( r_resultado_opera ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  -- No se mostraran las cifras control
                  LET v_bnd_ejecuta = FALSE
                  EXIT INPUT
               END IF
               
               -- se captura la fecha y hora en que se inició la operación
               LET v_fecha_inicio = CURRENT YEAR TO SECOND
               
               -- Quita los espacios al principio y al final de cada variable para el paso de parametros
               LET v_aux_pid   = p_pid
               LET v_aux_pid   = v_aux_pid.trim()                  
               LET v_aux_proc  = p_proceso
               LET v_aux_proc  = v_aux_proc.trim()
               LET v_aux_opera = p_operacion
               LET v_aux_opera = v_aux_opera.trim()

               -- Construye comando
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/AFIE11.42r "
                                              ,p_usuario," "
                                              ,v_aux_pid," "
                                              ,v_aux_proc," "
                                              ,v_aux_opera
                                              ," '",v_nom_archivo,"' '"
                                              ,p_prog_a_lanzar
                                              
                                ,"' 1>>", g_ruta_listados CLIPPED 
                                ,"/nohup:",v_aux_pid USING "&&&&&",":",
                                           v_aux_proc USING "&&&&&",":",
                                           v_aux_opera   USING "&&&&&" ,
                                " 2>&1 &"
                                
               DISPLAY v_comando 
               
               -- Llamada al proceso de carga nohup (proceso independeinte al flujo de la aplicacion)
               RUN v_comando                  
               
               IF ( STATUS ) THEN
                  CALL fn_mensaje("Cargar de Archivo", 
                                  "Ocurrió un error al iniciar el proceso batch",
                                  "bn_about")
                  CONTINUE INPUT
               ELSE
                  --  Se indica que se realizo el proceso de carga
                  LET r_bnd_carga = TRUE
                  CALL fn_mensaje("Carga de Archivo", 
                                  "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_aux_pid,
                                  "bn_about")
               END IF
               -- No se mostraran las cifras control
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
            -- No se mostraran las cifras control
            LET v_bnd_ejecuta = FALSE
            LET r_bnd_carga   = FALSE
            --  si el parametro indica que desde la carga se inicia el proceso
            IF ( p_inicia_proceso ) THEN
                  --  se indica que NO se inicio el proceso
               LET r_resultado_opera = -1
            END IF
            --LET r_resultado_opera = 100
            EXIT INPUT

      END INPUT 
      
      -- Condición para determinar si se muestran las cifras control y si se ha procesado el archivo correctamente
      IF ( v_bnd_ejecuta ) THEN
         -- Muestra el grupo de cifras control
         CALL v_forma_actual.setElementHidden("grp_total_registros",FALSE)
         -- Calcula las cifras control
         CALL fn_obtiene_cifras(g_ruta_rescate,v_nom_archivo) 
                    RETURNING g_reg_archivo,g_reg_aceptados, g_reg_rechazados 
         -- Desplega cifras control
         DISPLAY g_reg_archivo    TO reg_archivo
         DISPLAY g_reg_rechazados TO reg_rechazados
         DISPLAY g_reg_aceptados  TO reg_procesados
         -- Pregunta si las cifras son corrctas, para almacenar en BD que el archivo se ha procesado 
         CALL fn_ventana_confirma ("Carga de Archivo",
                                   "¿Las cifras control son correctas?",
                                   "about") RETURNING v_bnd_cifras
         -- Si las cifras son correctas, se almacena un registro que indica que el archivo se ha procesado         
         IF ( v_bnd_cifras ) THEN
            -- Almacena registro de archivo procesado
            --  si devuelve true la carga se ha completado
            CALL fn_ingresa_etapa(p_proceso, p_operacion, v_nom_archivo) RETURNING r_bnd_carga 
         ELSE
            LET v_nom_archivo = ""
            --  no se realizó la carga
            LET r_bnd_carga       = FALSE
            LET r_resultado_opera = 0
         END IF
      ELSE
         LET v_nom_archivo = ""
      END IF      

   CLOSE WINDOW w_RETGE01      

   --  Solo si se inició el proceso
   IF ( r_resultado_opera = 0 ) THEN
      IF ( r_bnd_carga ) THEN
         -- Fianaliza carga de archivo para el modo en linea
         IF ( p_tipo_carga = 1 ) THEN
            --  Finaliza la operacion de carga de archivo
            CALL fn_actualiza_opera_fin(p_pid,p_proceso,p_operacion) RETURNING r_resultado_opera

            IF NOT(r_resultado_opera ) THEN
               --  Ejecuta la operacion que se ha pasado como parámetro
               CALL fn_ejecuta_lanzado(p_prog_a_lanzar)
               LET v_mensaje = "El proceso de carga ha finalizado correctamente"
               --  Envia correo de estado de operación               
               CALL fn_correo_proceso(p_pid, 
                                      p_proceso, 
                                      p_operacion, 
                                      '', --  Archivo adjunto
                                      'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                       v_mensaje
                                      )
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               --  Actualiza a estado erroneo
               CALL fn_error_opera(p_pid,p_proceso,p_operacion) RETURNING r_resultado_opera
               IF ( r_resultado_opera ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
               END IF
               LET v_mensaje = "Ocurrió un error al actualizar el estado de la operación"
               --  Envia correo de estado de operación               
               CALL fn_correo_proceso(p_pid, 
                                      p_proceso, 
                                      p_operacion, 
                                      '', --  Archivo adjunto
                                      'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                       v_mensaje
                                      )
            END IF
         END IF 
      END IF
   END IF
   RETURN r_bnd_carga
END FUNCTION

-- Objetivo: Recupera la informacion necesaria del proceso para cargar el archivo
FUNCTION fn_recupera_inf_proceso(p_proceso,p_operacion)
DEFINE p_proceso       LIKE cat_proceso.proceso_cod,
       p_operacion     LIKE cat_operacion.opera_cod,
       v_layout        LIKE cat_operacion.layout_cod,
       v_extension     LIKE cat_operacion.extension,
       v_modulo        LIKE cat_proceso.modulo_cod,
       v_proceso_desc  LIKE cat_proceso.proceso_desc,
       v_opera_desc    LIKE cat_operacion.opera_desc,
       v_ruta_rescate  LIKE seg_modulo.ruta_rescate,
       v_usuario       LIKE seg_modulo.usuario,
       v_ruta_listados LIKE seg_modulo.ruta_listados
       
   -- Consulta que recupera el módulo y descripción del proceso
   SELECT modulo_cod, proceso_desc
     INTO v_modulo, v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

   -- Consulta que recupera los datos necesarios para conformar las tablas temporales del proceso
   SELECT extension, opera_desc, layout_cod
     INTO v_extension, v_opera_desc, v_layout
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
   -- Consulta que recupera el usuario y la ruta donde estan ubicados los archivos del proceso
   SELECT ruta_rescate, usuario
     INTO v_ruta_rescate, v_usuario
     FROM seg_modulo
    WHERE modulo_cod = v_modulo

   -- Ruta donde se coloca el archivo de monitoreo
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   RETURN v_proceso_desc, v_extension, v_opera_desc,
          v_layout, v_ruta_rescate, v_ruta_listados,  v_usuario
END FUNCTION

-- Objetivo: Llena el comboBox de los nombres de archivo que se encuentren en la ruta correspondiente al proceso
FUNCTION fn_recupera_archivos(p_proceso, p_operacion,v_cb_archivo, p_extension)
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
       v_tmp_archivo     VARCHAR(40)

   LET v_existe_archivos = FALSE
   LET v_bnd_continua = TRUE
   -- Valida si existe la ruta del proceso donde se recuperaran los nombres de archivo
   IF NOT(os.Path.exists(g_ruta_rescate) ) THEN
      CALL fgl_winwait("No existe la ruta "||g_ruta_rescate)
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF
   -- Valida si es un directorio valido
   IF NOT os.Path.isdirectory(g_ruta_rescate) THEN
      CALL fgl_winwait( "El parámetro proporcionado no es un directorio")
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF
   -- Consulta que determina si ya se ha procesado el archivo
   LET v_sql_qry = "\n SELECT FIRST 1 NVL(1,0)",
                   "\n   FROM glo_ctr_archivo",
                   "\n  WHERE nombre_archivo = ?",
                   "\n    AND proceso_cod = ?",
                   "\n    AND opera_cod = ?",
                   "\n    AND estado IN (1,2)"
   PREPARE prp_existe_archivo FROM v_sql_qry 
   -- Ordena los archivos por nombre descendente para los archivos de diropen   
   CALL os.Path.dirsort("name", "-1")
   -- Solo se recuperan archivos
   --  1 - Excluye archivos ocultos, 2 - Excluye directorios, 4 - Excluye Links simbólicos 
   CALL os.Path.dirfmask( 1 + 2 + 4 )
   -- Recupera la lista de archivos
   LET v_lista_archivos = os.Path.diropen(g_ruta_rescate)
   -- Recupera el primer nombre de archivo
   LET v_archivo = os.Path.dirnext(v_lista_archivos)
   -- Mientras no se recupere un valor nulo
   WHILE v_archivo IS NOT NULL
      
      -- Filtro para solo agregar archivos al combo
      IF ( v_archivo = "." OR v_archivo = ".." OR
         os.Path.isdirectory(v_archivo) ) THEN
         CONTINUE WHILE
      ELSE
         --DISPLAY "ARCHIVOS:--> ",v_archivo
         -- Si la extencion corresponde a la extension del proceso, lo agrega al combo
         IF ( os.Path.extension(v_archivo) = p_extension ) THEN
            --DISPLAY "ARCHIVOS CON EXTENSION:--> ",v_archivo," EXT:",p_extension
            -- Cambia de variable de tipo string a char, para poder usar en consulta
            LET v_tmp_archivo = v_archivo
            
            -- Verifica si ya se ha procesado el archivo en glo_ctr_archivo
            EXECUTE prp_existe_archivo USING v_tmp_archivo,p_proceso,p_operacion INTO v_existe_archivo
            --DISPLAY "SQL:--> ",SQLCA.SQLCODE
            IF ( SQLCA.SQLCODE = 100 ) THEN
               LET v_existe_archivo = 0
               --DISPLAY "BOOL:--> ",v_existe_archivo
            END IF
            
            IF NOT(v_existe_archivo ) THEN
               --DISPLAY "ARCHIVOS A MOSTRAR:--> ",v_archivo
               -- Añade el nombre del archivo al combo box
               CALL v_cb_archivo.additem( v_archivo, v_archivo )
            END IF
         END IF
      END IF
      -- Recupera el siguiente archivo
      LET v_archivo = os.Path.dirnext(v_lista_archivos)
   END WHILE
   -- Indica si no existe algun archivo
   IF NOT(v_cb_archivo.getItemCount() ) THEN
      CALL fgl_winwait("No se ha encontrado algún archivo")
      LET v_bnd_continua = FALSE
   END IF
   -- Cierra la lista de archivos
   CALL os.Path.dirclose(v_lista_archivos)
   RETURN v_bnd_continua
END FUNCTION

-- Objetivo: Verifica que se pueda leer y escribir en el archivo 
FUNCTION fn_verifica_permisos_archivo(p_ruta_rescate,p_archivo)
DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo        STRING,
       v_bnd_continua   SMALLINT,
       v_cambio_permiso INTEGER

   LET p_archivo = p_ruta_rescate CLIPPED||"/"||p_archivo
   -- Valida si se puede leer el archivo
   IF os.Path.readable(p_archivo) THEN
      -- Valida si se puede escribir en el archivo
      IF os.Path.writable(p_archivo) THEN
         LET v_bnd_continua = TRUE
      ELSE
         -- Intenta cambiar los permisos de lectura y escritura del archivo
         CALL os.Path.chrwx(p_archivo, 777) RETURNING v_cambio_permiso
         -- Si v_cambio_permiso = TRUE, cambió los permisos correctamente
         IF ( v_cambio_permiso ) THEN
            LET v_bnd_continua = TRUE
         ELSE
            LET v_bnd_continua = FALSE
         END IF
      END IF
   ELSE
      -- Intenta cambiar los permisos de lectura y escritura del archivo
      CALL os.Path.chrwx(p_archivo, 777) RETURNING v_cambio_permiso
      -- Si v_cambio_permiso = TRUE, cambió los permisos correctamente
      IF ( v_cambio_permiso ) THEN
         LET v_bnd_continua = TRUE
      ELSE
         LET v_bnd_continua = FALSE
      END IF
   END IF
   RETURN v_bnd_continua 
END FUNCTION

-- Objetivo: Función que construye las consultas y separa los bloques de información para que corresponda a cada tabla
FUNCTION fn_valida_archivo(p_ruta_rescate,p_archivo, p_layout,p_usuario,p_archivo_monitoreo,p_ruta_listados)
DEFINE p_ruta_rescate      STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo           STRING,
       p_layout            LIKE cat_operacion.layout_cod,
       p_usuario           LIKE seg_modulo.usuario,
       v_reg_layout        RECORD LIKE cat_layout.*,
       v_reg_campos        RECORD LIKE cat_campo.*,
       v_sql_crea_tablas   STRING,
       v_sql_consulta      STRING,
       v_sql_campos        STRING,
       v_bnd_cursor        SMALLINT, -- Bandera para omitir la primera coma en la construcción de los campos de la tabla
       v_reg_procesados    INTEGER,
       v_indice            INTEGER,
       p_archivo_monitoreo STRING,
       p_ruta_listados     STRING,
       v_cadena_registros  STRING,
       v_cadena_reg_aux    STRING

   LET g_reg_aceptados = 0
   LET g_archivo_monitoreo = p_archivo_monitoreo 
   LET g_ruta_listados = p_ruta_listados 
   LET v_cadena_registros = "^"
   
   -- Cambia a la base de datos safre_tmp
   DATABASE safre_tmp
   -- Recupera la informacion necesaria para crar las tablas, dependiendo del módulo
   LET v_sql_consulta = "\n SELECT * ",
                        "\n   FROM safre_viv:cat_layout",
                        "\n  WHERE layout_cod = ?"
   PREPARE prp_recupera_tablas FROM v_sql_consulta 
   DECLARE cur_recupera_tablas CURSOR FOR prp_recupera_tablas

   -- Recupera la información de los campos que contendran las tablas, dependiendo del módulo
   LET v_sql_consulta = "\n SELECT * ",
                        "\n   FROM safre_viv:cat_campo ",
                        "\n  WHERE layout_cod   = ?",
                        "\n    AND registro = ? ",
                        "\n  ORDER BY campo_cod "
   PREPARE prp_recupera_campos FROM v_sql_consulta
   DECLARE cur_recupera_campos CURSOR FOR prp_recupera_campos
   --DISPLAY "+++++++++++++++++++++++++++" --ERV
   --DISPLAY v_sql_consulta --ERV
   --DISPLAY "+++++++++++++++++++++++++++" --ERV
   LET v_indice = 0
   --  Inicializa conteo de registros separados
   LET g_reg_tab_cargados = 0
   
   -- Ciclo que recupera las tablas a crear
   FOREACH cur_recupera_tablas USING p_layout INTO v_reg_layout.*
   --DISPLAY " *******************layout_cod " ,v_reg_layout.layout_cod --ERV
   --DISPLAY " *******************registro entro" ,v_reg_layout.registro --ERV 
      LET v_cadena_reg_aux = v_reg_layout.registro
      LET v_cadena_registros = v_cadena_registros,v_cadena_reg_aux.trim() ,"|^"
      -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
      -- Si es el proceso en linea, envia NULL
      IF ( g_archivo_monitoreo IS NOT NULL ) THEN
         -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
         CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"CREA TABLA: "||v_reg_layout.tabla)
      END IF
      -- primera parte del sql para crear las tablas
      LET v_sql_crea_tablas = "CREATE TABLE "||v_reg_layout.tabla|| "( "
      LET v_bnd_cursor = FALSE
      --DISPLAY " ///////////////////registro salio" ,v_reg_layout.registro --ERV 
      -- Ciclo para recuperar los campos de las tablas
      FOREACH cur_recupera_campos USING v_reg_layout.layout_cod, v_reg_layout.registro
                                   INTO v_reg_campos.*
         --IF v_reg_layout.tipo_carga = 2 AND
            --v_reg_campos.num_valida = 1 THEN
            --CONTINUE FOREACH
         --END IF

         --DISPLAY ".........................",v_reg_campos.* --ERV

         -- Switch para construir el campo dependiendo del tipo de dato
         CASE v_reg_campos.tipo_dato
            -- Tipo de dato Carácter
            WHEN "X"
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED||
                                 " CHAR ("||v_reg_campos.longitud CLIPPED||")"
            -- Tipo de dato Fecha
            WHEN "F" 
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED||
                                 " DATE "
            -- Tipo de dato Numérico
            WHEN "#"
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED ||
                                  " DECIMAL ("||v_reg_campos.longitud +
                                  v_reg_campos.precision || ",0)"
         END CASE
         -- Si v_bnd_cursor = FALS, omite la primera coma, para concatenar los campos de la tabla
         IF NOT(v_bnd_cursor ) THEN
            LET v_sql_crea_tablas = v_sql_crea_tablas || v_sql_campos
            LET v_bnd_cursor = TRUE 
         ELSE
            LET v_sql_crea_tablas = v_sql_crea_tablas || " , " || v_sql_campos
            
         END IF
      END FOREACH
      -- Ultima parte del sql para crear la tabla
      LET v_sql_crea_tablas = v_sql_crea_tablas || " ) "
      DISPLAY v_sql_crea_tablas --ERV
      IF p_layout = 2101 THEN
         LET v_sql_crea_tablas = v_sql_crea_tablas || 
                              " fragment by round robin in bdnsv_1_dbs, bdnsv_2_dbs, bdnsv_3_dbs, bdnsv_4_dbs ;"
                              
      ELSE
         LET v_sql_crea_tablas = v_sql_crea_tablas || ";"
      END IF

            
      WHENEVER ERROR CONTINUE
      
      -- Elimina la tabla si es que existe
      LET v_sql_consulta = "DROP TABLE IF EXISTS "|| v_reg_layout.tabla
      --DISPLAY "\n",v_sql_consulta,"\n"
      PREPARE prp_drop FROM v_sql_consulta
      EXECUTE prp_drop
      IF ( SQLCA.sqlcode < 0 ) THEN
         --DISPLAY SQLCA.sqlcode 
         DISPLAY "\nOcurrió error al borrar tabla "||v_reg_layout.tabla CLIPPED||", Código:",SQLCA.sqlcode,"\n"
         LET v_indice = 0
         EXIT FOREACH
      END IF
      --WHENEVER ERROR STOP

      -- Crea la tabla con la cadena construida
      --DISPLAY v_sql_crea_tablas
      PREPARE prp_crea_tabla FROM v_sql_crea_tablas
      EXECUTE prp_crea_tabla
      
      -- Si la tabla no puede ser creada, iguala el v_indice a cero para indicar que no se a procesado ningun registro
      -- y aparecera el mensaje de que no hay layout para el proceso
      IF ( SQLCA.sqlcode < 0 ) THEN
         --DISPLAY SQLCA.sqlcode 
         DISPLAY "\nOcurrió error al crear tabla "||v_reg_layout.tabla CLIPPED||", Código:",SQLCA.sqlcode,"\n"
         LET v_indice = 0
         EXIT FOREACH
      END IF
      -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
      -- Si es el proceso en linea, envia NULL
      IF ( g_archivo_monitoreo IS NOT NULL ) THEN
         -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
         CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"CREA TABLA: "||v_reg_layout.tabla)
      END IF
      --  Se recupera el nombre de tabla para el conteo de registros insertados a la misma
      LET v_reg_cargados_x_tabla[v_reg_cargados_x_tabla.getLength()+1].v_tabla  = v_reg_layout.tabla
      LET v_reg_cargados_x_tabla[v_reg_cargados_x_tabla.getLength()].v_registro = v_reg_layout.registro
      -- Funcion que separa la información correspondiente a cada tabla de base de datos
      --CALL fn_separa_archivo(p_ruta_rescate,p_archivo, v_reg_layout.registro, 
                             --v_reg_layout.archivo,p_usuario)
      
      CASE v_reg_layout.tipo_carga

         WHEN 3   
            DISPLAY  "fn_load_archivo_tabla"         
            CALL fn_load_archivo_tabla (p_ruta_rescate,p_archivo, v_reg_layout.tabla, 
                                         v_reg_layout.layout_cod, v_reg_layout.registro,p_usuario)
            DISPLAY "Finalizado"
         OTHERWISE
            --  caraga por defecto con dbload si no se ha especificado tipo_carga
            DISPLAY "CARGA POR DEFECTO REALIZADA CON DBLOAD. NO SE HA ESPECIFICADO EL TIPO DE CARGA"
            CALL fn_carga_archivo_tabla (p_ruta_rescate,v_reg_layout.archivo, v_reg_layout.tabla, 
                                         v_reg_layout.layout_cod, v_reg_layout.registro,p_usuario)
      END CASE

      --  Se actualiza las estaditicas de la tabla
      LET v_sql_consulta = "UPDATE STATISTICS FOR TABLE safre_tmp:",v_reg_layout.tabla
      PREPARE prp_actualiza_est FROM v_sql_consulta
      EXECUTE prp_actualiza_est 

      --DISPLAY "///////////////////////////",v_reg_layout.tabla --ERV
      -- Recupera el total de registros en base de datos de la tabla procesada
      LET v_sql_consulta = " SELECT count(*) FROM safre_tmp:",v_reg_layout.tabla
      PREPARE prp_cifras FROM v_sql_consulta
      --DISPLAY  " 5555555555555555 ",v_sql_consulta, " 5555555555555555"--ERV
      EXECUTE prp_cifras INTO v_reg_procesados
      -- Se acumulan los registros de todas las ls -l
      LET g_reg_aceptados = g_reg_aceptados + v_reg_procesados
      LET v_indice = v_indice + 1
   END FOREACH

   LET v_cadena_registros = v_cadena_registros.subString(1,v_cadena_registros.getLength()-2)
   
   CLOSE cur_recupera_tablas
   FREE cur_recupera_tablas
   FREE cur_recupera_campos
   -- Vuelve a usar la base de datos safre_viv 
   DATABASE safre_viv

   RETURN v_indice, g_reg_tab_cargados, v_cadena_registros
END FUNCTION

-- Objetivo: Función que separa los bloques de información del archivo a procesar para que corresponda a cada tabla de base de datos
FUNCTION fn_separa_archivo(p_ruta_rescate, p_archivo, p_registro, p_archivo_out,p_usuario)
DEFINE p_ruta_rescate STRING,--LIKE seg_modulo.ruta_rescate,
       p_registro     LIKE cat_layout.registro,
       p_archivo_out  LIKE cat_layout.archivo,
       p_usuario      LIKE seg_modulo.usuario,
       p_archivo      STRING,
       v_comando      STRING,
       v_str_archivo  STRING,
       v_str_comando  STRING,
       v_canal        base.Channel,
       v_ltr_archivo  STRING,
       v_registros    INTEGER
   
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"SEPARA ARCHIVO")
   END IF
   --  si el tipo de registro especificado en cat_layout = 80 se contemplan todos los registros del archivo
   --  para cargar a una tabla, por lo que en lugar de separa se hace una copia(cp) con todos los registros
   IF ( p_registro <> "80" ) THEN
      -- Comando para separar los registros que comiencen con lo que contenga p_registro
      -- y los deposita en otro archivo, para poder cargar directamente a base de datos
      LET v_comando = "sed -e '/^"||p_registro CLIPPED||"/!d' '"||
                      p_ruta_rescate CLIPPED|| "/"||p_archivo CLIPPED||
                      "' >"||p_ruta_rescate CLIPPED||"/"||p_usuario CLIPPED||"."||p_archivo_out
   ELSE
      --  contempla todos los registros del archivo para cargar en una sola tabla, ya que esposible que
      --  no se tenga el tipo de registro en el archivo
      LET v_comando = "cp "||p_ruta_rescate CLIPPED|| "/"||p_archivo CLIPPED||
                      " "||p_ruta_rescate CLIPPED||"/"||p_usuario CLIPPED||"."||p_archivo_out
   END IF

   -- Ejecuta el comando
   --DISPLAY v_comando
   RUN v_comando
   
   -- -- -- -- -- 
   --  Recupera el conteo de registros que se van a cargar a base de datos
   --  tienen que ser iguales pero el de channel no soporta comillas
   LET v_str_archivo = "'",p_ruta_rescate CLIPPED,"/tot_reg_tab_archivo."||p_usuario CLIPPED||"."||p_archivo_out CLIPPED,"'"
   LET v_ltr_archivo = p_ruta_rescate CLIPPED,"/tot_reg_tab_archivo."||p_usuario CLIPPED||"."||p_archivo_out CLIPPED

   --  Realiza el conteo de los registros que se separaron para insertar en la tabla en cuestion
   LET v_str_comando = "wc -l '"||p_ruta_rescate CLIPPED||"/"||p_usuario CLIPPED||"."||p_archivo_out CLIPPED||
                       "' | awk '{ print $1 }'  > ", v_str_archivo
   --DISPLAY v_str_comando
   RUN v_str_comando
   
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ltr_archivo,"r")
   LET v_registros = 0
   LET v_registros = v_canal.readLine()
   LET g_reg_tab_cargados = g_reg_tab_cargados + v_registros
   CALL v_canal.close()

   LET v_reg_cargados_x_tabla[v_reg_cargados_x_tabla.getLength()].v_conteo = v_registros
   -- Elimina archivo que contiene el conteo de registros
   LET v_str_comando = "rm ", v_str_archivo
   --DISPLAY v_str_comando
   -- Ejecuta comando
   --RUN v_str_comando
   -- 
   -- -- -- -- -- -- -- -- -- 
   
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"SEPARA ARCHIVO")
   END IF  
END FUNCTION

-- Objetivo: Función que separa los bloques de información del archivo a procesar para que corresponda a cada tabla de base de datos
FUNCTION fn_carga_archivo_tabla(p_ruta_rescate,p_archivo_salida, p_tabla, p_layout, p_registro,p_usuario)
DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo_salida STRING ,
       p_tabla          LIKE cat_layout.tabla,
       p_layout         LIKE cat_layout.layout_cod,
       p_registro       LIKE cat_layout.registro,
       p_usuario        LIKE seg_modulo.usuario,
       v_str_archivo    STRING,
       v_str_campo      STRING,
       v_reg_campos     RECORD LIKE cat_campo.*,
       --v_indice         SMALLINT,
       v_canal          base.Channel,
       v_sql_consulta   STRING
    
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"GENERA ARCHIVO PARA DBLOAD")
   END IF
   -- Ruta y nombre en que se generara el archivo para el dbload
   LET v_str_archivo = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".data"
   -- Crea el canal para manipular el archivo
   LET v_canal = base.Channel.create()
   -- Indica el archivo y el modo de escritura
   CALL v_canal.openFile(v_str_archivo,"w")
   -- Indica la tabla y campos a insrtar y las posiciones de cada campo   
   CALL v_canal.writeLine( 'FILE "'||p_usuario CLIPPED||"."||p_archivo_salida CLIPPED || '" (')
   -- Recupera los campos de la tabla en que se insertara la informacion
   LET v_sql_consulta = "\n SELECT *",
                        "\n   FROM safre_viv:cat_campo",
                        "\n  WHERE layout_cod = ?",
                        "\n    AND registro = ?",
                        "\n  ORDER BY 3"
   PREPARE prp_recupera_campos_bdload FROM v_sql_consulta                        
   DECLARE cur_db CURSOR FOR prp_recupera_campos_bdload
   
   LET v_str_campo = ""
   -- Ciclo para recuperar los campos
   FOREACH cur_db USING p_layout, p_registro INTO v_reg_campos.*
      -- Escribe en archivo los campos
      CALL v_canal.writeLine(v_str_campo)

      -- Concatena la especificacion del campo      
      LET v_str_campo = v_reg_campos.campo_desc CLIPPED,
                        v_reg_campos.pos_inicial CLIPPED,
                        "-" CLIPPED,
                        v_reg_campos.pos_final USING "&&&&" CLIPPED ," ,"
   END FOREACH
   -- Recupera la cadena sin la ultima coma
   LET v_str_campo = v_str_campo.subString(1,v_str_campo.getLength()-1)|| " ); "
   -- Escribe en archivo la ultima parte de la especificacion de la tabla 
   CALL v_canal.writeLine(v_str_campo)
   -- Escrbe en archivo que se realizara una insercion a la tabla del proceso
   CALL v_canal.writeLine("INSERT INTO  "|| p_tabla  CLIPPED|| ";")
   CALL v_canal.writeLine("")
   CALL v_canal.close()
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"GENERA ARCHIVO PARA DBLOAD")
   END IF

   CALL fn_ejecuta_dbload(p_ruta_rescate,p_archivo_salida,p_usuario)
END FUNCTION

-- Objetivo: Función que realiza el dbload
FUNCTION fn_ejecuta_dbload(p_ruta_rescate, p_archivo_salida,p_usuario)
DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
       v_comando        STRING,
       p_archivo_salida LIKE cat_layout.archivo,
       p_usuario        LIKE seg_modulo.usuario

   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"CARGA ARCHIVO A BASE DE DATOS")
   END IF
   -- El comando especifica el formato de fecha a usar, se ubica en la ruta del archivo con las especificaciones para realizar el dbload
   -- Ejecuta dbload indicado base de datos (-d), ruta de archivo de comandos (-c), log de errores (-l), limit de errores (-e), cada cuantos registros se realiza commit (-n) y bloqueo de tablas (-k)
   -- Remueve el formato de fecha especificado
   LET v_comando = "export DBDATE='Y4MD'; cd ",p_ruta_rescate CLIPPED,               
                       "/;dbload -d safre_tmp -c ",
                       p_usuario CLIPPED,".", p_archivo_salida CLIPPED, ".data ",
                       " -l " CLIPPED," ", p_usuario CLIPPED,".", p_archivo_salida CLIPPED,
                       ".err" CLIPPED," -e 100 -n 1000 -k ; unset DBDATE  "
   -- Ejecuta las instrucciones
   DISPLAY "INICIA CARGA DE INFORMACIÓN"
   DISPLAY v_comando
   RUN v_comando
   DISPLAY "FINALIZA CARGA DE INFORMACIÓN"
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"CARGA ARCHIVO A BASE DE DATOS")
   END IF
END FUNCTION

-- Objetivo: Función que calcula las cifras control de los registros procesados
FUNCTION fn_obtiene_cifras(p_ruta_rescate,p_archivo)
DEFINE p_ruta_rescate  STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo       STRING,
       v_str_comando   STRING,
       v_str_archivo   STRING,
       v_ltr_archivo   STRING,
       --v_str_registros STRING,
       v_canal         base.Channel
       --v_bnd_cifras    BOOLEAN

   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"RECUPERA CIFRAS CONTROL")
   END IF
   -- Archivo de cifras control
   LET v_str_archivo = "'",p_ruta_rescate CLIPPED,"/tot_reg_archivo."||p_archivo CLIPPED,"'"
   LET v_ltr_archivo = p_ruta_rescate CLIPPED,"/tot_reg_archivo."||p_archivo CLIPPED
   -- Comando para realizar el conteo de lineas
   LET v_str_comando = "wc -l '"||p_ruta_rescate CLIPPED,"/", p_archivo,
                       "' | awk '{ print $1 }'  > ", v_str_archivo
   --DISPLAY "$$$$$$$$$$$$$$$$$$$$$",v_str_comando --ERV
   RUN v_str_comando
   DISPLAY "====================",v_ltr_archivo --ERV    
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ltr_archivo,"r")
   --LET v_str_registros = v_canal.readLine()
   LET g_reg_archivo = v_canal.readLine()
   CALL v_canal.close()

   LET g_reg_rechazados = g_reg_archivo - g_reg_aceptados

   -- Elimina archivo que contiene el conteo de registros
   LET v_str_comando = "rm ", v_str_archivo
   --DISPLAY v_str_comando
   -- Ejecuta comando
   RUN v_str_comando
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"RECUPERA CIFRAS CONTROL")
   END IF
   RETURN g_reg_archivo,g_reg_aceptados,g_reg_rechazados
END FUNCTION

-- Objetivo: Función que almacena información para el control de archivos cargados
FUNCTION fn_ingresa_etapa(p_proceso, p_operacion, p_archivo)
DEFINE p_proceso    LIKE cat_proceso.proceso_cod,
       p_operacion  LIKE cat_operacion.opera_cod,
       p_archivo    CHAR(40),
       v_bnd_etapa  BOOLEAN,
       v_fecha      DATE

   LET v_bnd_etapa = TRUE
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"INGRESA ETAPA")
   END IF

   --  Si ya existe el registro del achivo como reversado, lo elimina 
   DELETE 
     FROM glo_ctr_archivo
    WHERE nombre_archivo = nombre_archivo
      AND proceso_cod = proceso_cod
      AND opera_cod = opera_cod
      AND estado = 3
   
   -- inserta información de carga del archivo y el proceso al que corresponde
   -- estado = 1 -> archivo cargado
   INSERT INTO glo_ctr_archivo(proceso_cod,opera_cod,nombre_archivo,
                               estado,f_actualiza,usuario) 
          VALUES(p_proceso,p_operacion,p_archivo, 1,TODAY, USER)

   IF ( SQLCA.SQLCODE = 0 ) THEN
      LET v_bnd_etapa = TRUE
   ELSE
      LET v_bnd_etapa = FALSE
   END IF   
   -- Si es el proceso batch, registra etapa, ya que solo el proceso batch envia el nombre del archivo de monitoreo
   -- Si es el proceso en linea, envia NULL
   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"INGRESA ETAPA")
   END IF

   RETURN v_bnd_etapa
END FUNCTION

-- Objetivo: Función que genera el archivo de monitoreo del proceso
FUNCTION fn_monitorea_proceso(p_archivo, p_ruta_destino, p_detalle)
DEFINE v_canal_entrada BASE.CHANNEL,
       p_archivo       STRING,
       p_ruta_destino  STRING,
       p_detalle       STRING

   WHENEVER ERROR CONTINUE
   -- Crea el canal para la escritura en archivo
   LET v_canal_entrada = BASE.CHANNEL.CREATE()
   -- Intenta agregar la informacion al archivo solo si ya existe.
   CALL v_canal_entrada.openFile(p_ruta_destino CLIPPED||"/"||p_archivo CLIPPED, "a")
   -- Condición para revisar si se realizó correctamente la instruccion anterior
   IF ( STATUS ) THEN
      -- No se ha encontrado el archivo para concatenar la información y se intenta crear el archivo
      CALL v_canal_entrada.openFile(p_ruta_destino CLIPPED||"/"||p_archivo, "w")
      IF ( STATUS ) THEN
         -- No se ha podido crear el archivo         
      ELSE
         -- Se agrega la información al archivo
         CALL v_canal_entrada.writeLine(p_detalle)
         -- Se cierra el canal
         CALL v_canal_entrada.close()
      END IF
   ELSE
      -- Se agrega la información al archivo
      CALL v_canal_entrada.writeLine(p_detalle)
      -- Se cierra el canal
      CALL v_canal_entrada.close()
   END IF
   --DISPLAY p_detalle
END FUNCTION

-- Objetivo: Función que agrega la etapa para el monitoreo
FUNCTION fn_registra_monitoreo_etapa(p_etapa,p_archivo,p_ruta_rescate,p_detalle)
DEFINE p_etapa        BOOLEAN,
       p_archivo      STRING,
       p_ruta_rescate STRING,
       p_detalle      STRING,
       v_fecha        DATE

   IF ( p_detalle IS NULL ) THEN
      LET p_detalle  = " "
   END IF
   -- Si p_etapa = 1 ->  inició etapa
   -- Si p_etapa = 0 ->  finalizó etapa
   IF ( p_etapa ) THEN
      LET g_detalle_monitoreo = " INICIO ETAPA       : "||p_detalle||"\n"
   ELSE
      LET g_detalle_monitoreo = " FIN ETAPA          : "||p_detalle||"\n"
   END IF
   LET v_fecha = TODAY
   LET g_detalle_monitoreo =  g_detalle_monitoreo,
                              " FECHA              : ",v_fecha USING "DD-MM-YYYY"||"\n",
                              " HORA               : ",TIME(CURRENT),"\n"
   -- registra etapa en archivo de monitoreo
   CALL fn_monitorea_proceso(p_archivo,p_ruta_rescate,g_detalle_monitoreo)
   DISPLAY g_detalle_monitoreo

END FUNCTION

-- Objetivo: Función que muestra mensaje de inconsistencia para la operacion
{FUNCTION fn_muestra_inc_operacion(p_resultado_opera)
DEFINE p_resultado_opera SMALLINT,
       v_descripcion     LIKE cat_bat_parametro_salida.descripcion

   --  Se recupera la descripcion del codigo devuelto
   SELECT descripcion
     INTO v_descripcion 
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera
   --  Se muestra el mensaje
   DISPLAY v_descripcion
   CALL fn_mensaje("Error de operación",v_descripcion,"bn_about")
   
END FUNCTION}

-- Objetivo: Función que muestra mensaje de inconsistencia para la operacion
{FUNCTION fn_desplega_inc_operacion(p_resultado_opera)
DEFINE p_resultado_opera SMALLINT,
       v_descripcion     LIKE cat_bat_parametro_salida.descripcion

   --  Se recupera la descripcion del codigo devuelto
   SELECT descripcion
     INTO v_descripcion 
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera
   
   DISPLAY v_descripcion

END FUNCTION}

-- Objetivo: Carga Archivo en proceso nohup 
FUNCTION fn_ejecuta_lanzado(p_prog_a_lanzar)
DEFINE p_prog_a_lanzar STRING

   --DISPLAY "Comando: ", p_prog_a_lanzar
   --  Ejecuta programa a ser lanzado
   RUN p_prog_a_lanzar
   
END FUNCTION

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Módulo          => GLO                                                     -- 
-- Programa        => GLOE02.4gl                                              -- 
-- Objetivo        => Funcion que valida el conteo de registros que se        -- 
--                    insertaron a cada tabla del correspondiente layout      -- 
-- Fecha Inicio    => 10 ABRIL 2012                                           -- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
FUNCTION fn_valida_carga_tablas()
DEFINE v_continua BOOLEAN, 
       v_indice   SMALLINT,
       v_bandera  BOOLEAN

   -- Indica que se continua con el flujo correcto
   LET v_continua = TRUE
   --  FALSE = que no hay registros para todos los tipos registro
   --LET v_bandera  = FALSE
   --  se recorre el arreglo para validar que en para todas tablas se haya obtenido información del archivo
   FOR v_indice = 1 TO v_reg_cargados_x_tabla.getLength()
      --  si no se recupero registros del archivo que se separo para cargar a la tabla
      IF ( v_reg_cargados_x_tabla[v_indice].v_conteo < 1 ) THEN
         DISPLAY "NO SE ENCOTRARON REGISTROS PARA TIPO REGISTRO '",v_reg_cargados_x_tabla[v_indice].v_registro,"' (",v_reg_cargados_x_tabla[v_indice].v_tabla CLIPPED,")"
         LET v_continua = FALSE
      {ELSE
         LET v_bandera  = TRUE}
      END IF
   END FOR
   {IF ( v_bandera ) THEN
      --  si hay tipo registro(cat_layout.registro) que no tenian registros, se permite continuar
      LET v_continua = TRUE
   ELSE
      --  todos los tipo registro(cat_layout.registro) no tienen registros cargados, se detiene el flujo
      --  debido a archivo nulo o ningún tipo registro coinside con layout
      LET v_continua = FALSE
   END IF}
   
   RETURN v_continua 
END FUNCTION

-- Objetivo: Obtiene la extensión del archivo para el proceso y operación que vienen como parámetro
FUNCTION fn_recupera_extension(p_proceso, p_operacion)
DEFINE p_proceso       LIKE cat_proceso.proceso_cod,
       p_operacion     LIKE cat_operacion.opera_cod,
       v_extension     LIKE cat_operacion.extension
       
   -- Consulta que recupera la extensión para el proceso y operación en cuestión
   SELECT extension
     INTO v_extension
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
   RETURN v_extension
END FUNCTION


-- Objetivo: Función que carga la informacion del archivo por dbload
FUNCTION fn_load_archivo_tabla(p_ruta_rescate,p_archivo_salida, p_tabla, p_layout, p_registro,p_usuario)
DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo_salida STRING,
       p_tabla          LIKE cat_layout.tabla,
       p_layout         LIKE cat_layout.layout_cod,
       p_registro       LIKE cat_layout.registro,
       p_usuario        LIKE seg_modulo.usuario,
       v_str_archivo    VARCHAR(100),
       v_str_campo      STRING,
       v_reg_campos     RECORD LIKE cat_campo.*,
       v_tmp_campos     RECORD 
       nss                     VARCHAR (50),
       est_ssv                 VARCHAR (50),
       desc_st_ssv             VARCHAR (50),
       est_jfca                VARCHAR (50),
       desc_jfca               VARCHAR (50),
       tpo_proceso             VARCHAR (50),
       desc_tpo_proceso        VARCHAR (50),
       nombre_afore            VARCHAR (50),
       uno_apellido_afore      VARCHAR (50),
       dos_apellido_afore      VARCHAR (50),
       beneficiario            VARCHAR (50),
       nombre_benef            VARCHAR (50),
       ape_pat_benef           VARCHAR (50),
       ape_mat_benef           VARCHAR (50),
       curp                    VARCHAR (50),
       rfc                     VARCHAR (50),
       entidad_fed             VARCHAR (50),
       f_inicio_tram           VARCHAR (50),
       f_autorizacion_pago     VARCHAR (50),
       num_doc_cta_pago_fico   VARCHAR (50),
       eje_fis_cpp_fico        VARCHAR (50),
       no_doc_pago_fico        VARCHAR (50),
       f_pago_fico             VARCHAR (50),
       imp_pago_fico           VARCHAR (50),
       ref_pago_fico           VARCHAR (50),
       num_caso_adai           VARCHAR (50),
       num_laudo               VARCHAR (50),
       num_junta_esp           VARCHAR (50),
       imp_pago_ant            VARCHAR (50),
       f_pago_ant              VARCHAR (50),
       cve_banco               VARCHAR (50),
       cuenta_bancaria         VARCHAR (50),
       imp_transf_ssv          VARCHAR (50),
       f_transf_ssv            VARCHAR (50),
       ssv_dif_ini_legacy      VARCHAR (50),
       f_marca                 VARCHAR (50),
       ssv_erro_fico           VARCHAR (50),
       ssv_cve_afore           VARCHAR (50),
       ssv_imp97_pesos         VARCHAR (50),
       ssv_imp97_ivs           VARCHAR (50),
       ssv_imp92_pesos         VARCHAR (50),
       ssv_imp92_ivs           VARCHAR (50),
       f_valuacion             VARCHAR (50)
         END RECORD ,
       --v_indice         SMALLINT,
       v_canal          base.Channel,
       v_sql_consulta   STRING

   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      --CALL fn_registra_monitoreo_etapa(1,g_archivo_monitoreo,g_ruta_listados,"GENERA ARCHIVO PARA DBLOAD")
   END IF

   
   -- Ruta y nombre en que se generara el archivo para el dbload
   LET v_str_archivo = p_ruta_rescate CLIPPED,"/",p_archivo_salida CLIPPED
   -- Crea el canal para manipular el archivo
   LET v_canal = base.Channel.create()
   DISPLAY "****",v_str_archivo   
   -- Indica el archivo y el modo de escritura
   CALL v_canal.openFile(v_str_archivo,"r")
   CALL  v_canal.setDelimiter("|")
   -- Indica la tabla y campos a insrtar y las posiciones de cada campo

   DISPLAY "INICIA CARGA DE INFORMACIÓN"
   WHENEVER ERROR STOP 
   WHILE  v_canal.read([v_tmp_campos.*])
      --DISPLAY "@@@@@@@@@@@@@@@@@@", v_tmp_campos.*
      LET v_tmp_campos.f_autorizacion_pago = v_tmp_campos.f_autorizacion_pago[4,5] ||"/"|| v_tmp_campos.f_autorizacion_pago[1,2] ||"/"|| v_tmp_campos.f_autorizacion_pago[7,10];
      LET v_tmp_campos.f_inicio_tram = v_tmp_campos.f_inicio_tram[4,5] ||"/"|| v_tmp_campos.f_inicio_tram[1,2] ||"/"|| v_tmp_campos.f_inicio_tram[7,10];
      LET v_tmp_campos.f_marca       = v_tmp_campos.f_marca[4,5] ||"/"|| v_tmp_campos.f_marca[1,2] ||"/"|| v_tmp_campos.f_marca[7,10];
      LET v_tmp_campos.f_pago_ant    = v_tmp_campos.f_pago_ant[4,5] ||"/"|| v_tmp_campos.f_pago_ant[1,2] ||"/"|| v_tmp_campos.f_pago_ant[7,10];
      LET v_tmp_campos.f_pago_fico   = v_tmp_campos.f_pago_fico[4,5] ||"/"|| v_tmp_campos.f_pago_fico[1,2] ||"/"|| v_tmp_campos.f_pago_fico[7,10];
      LET v_tmp_campos.f_transf_ssv  = v_tmp_campos.f_transf_ssv[4,5] ||"/"|| v_tmp_campos.f_transf_ssv[1,2] ||"/"|| v_tmp_campos.f_transf_ssv[7,10];
      LET v_tmp_campos.f_valuacion   = v_tmp_campos.f_valuacion[4,5] ||"/"|| v_tmp_campos.f_valuacion[1,2] ||"/"|| v_tmp_campos.f_valuacion[7,10];
      
      INSERT INTO safre_tmp:tmp_ret_det_ley73 VALUES (v_tmp_campos.*)
      DISPLAY v_tmp_campos.*

      IF ( SQLCA.SQLCODE <> 0 ) THEN        
         DISPLAY "Error en carga de archivo ",SQLCA.SQLCODE
      END IF
   END WHILE

   DISPLAY "FINALIZA CARGA DE INFORMACIÓN"

   IF ( g_archivo_monitoreo IS NOT NULL ) THEN
      -- Registra la etapa en el archivo de monitoreo; 1 = inició etapa, 0 = finalizó etapa 
      CALL fn_registra_monitoreo_etapa(0,g_archivo_monitoreo,g_ruta_listados,"GENERA ARCHIVO PARA DBLOAD")
   END IF

END FUNCTION
