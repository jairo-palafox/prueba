################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL380                                                      #
#Ojetivo       => Validacion del archivo con cuentas clabe para transferencia  #
#Fecha inicio  => 11 de Agosto, 2015.                                          #
################################################################################

IMPORT os

DATABASE safre_viv
GLOBALS "RETG01.4gl"

--Datos del PID, proceso y operacion
DEFINE v_pid            LIKE bat_ctr_proceso.pid      --Id del proceso
DEFINE v_proceso_cod    LIKE cat_proceso.proceso_cod  --Codigo del proceso
DEFINE v_opera_cod      LIKE cat_operacion.opera_cod  --Codigo de la operacion

--Parametros de entrada
DEFINE p_usuario        CHAR(30)
DEFINE p_tpo_ejecucion  SMALLINT
DEFINE p_nom_ventana    CHAR(30)

--Valor regresado
DEFINE r_band_carga     SMALLINT

DEFINE g_ruta_rescate      STRING, --LIKE seg_modulo.ruta_rescate
       g_usuario           LIKE seg_modulo.usuario,
       g_ruta_listados     STRING,
       g_reg_tab_cargados  INTEGER

MAIN
   DEFINE
      v_cod_error           SMALLINT,
      v_nom_archivo         STRING,
      v_nom_arc_fin         STRING,
      v_hash_es_correcto    SMALLINT

   --Valores heredados
   LET p_usuario        = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_nom_ventana    = ARG_VAL(3)

   --Inicializaciones de PID, proceso y operacion
   LET v_pid = 0
   LET v_proceso_cod = g_proceso_carga_cuentas_clabe
   LET v_opera_cod = g_opera_cod_carga_cuentas_clabe_carga

   --Se asigna el nombre de la ventana
   IF (p_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nom_ventana)
   END IF

   --Solicita el archivo SHA*.txt
   CALL fn_solicita_archivo(v_proceso_cod,v_opera_cod)
   RETURNING v_nom_archivo

   --Nombre del archivo dat con el consolidado de datos
   LET v_nom_arc_fin = os.Path.rootname(v_nom_archivo) || ".clabe"

   --Valido el archivo SHA*.txt y los documentos que trae con su respectivo SHA
   CALL fn_valida_SHA(v_nom_archivo,v_nom_arc_fin) RETURNING v_hash_es_correcto
   IF NOT v_hash_es_correcto THEN
      LET r_band_carga = FALSE
      RETURN r_band_carga
   END IF
   
   --Se verifica que se pueda realizar la operacion
   IF (fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) = 0) THEN
      CALL fn_carga_archivo(v_pid,              --PID
                            v_proceso_cod,      --Clave de Proceso
                            v_opera_cod,        --Clave de Operacion
                            2,                  --Ejecucion por nohup
                            "RETL380",          --Clave del programa
                            "",                 --Programa que ejecutara validaciones
                            p_usuario,          --Usuario que ejecuta
                            TRUE,               --La carga inicializara el proceso
                            v_nom_arc_fin)      --Nombre del archivo a cargar
                  RETURNING r_band_carga        --Se regresa bandera de transaccion de carga
      IF (r_band_carga = FALSE) THEN
         CALL fn_mensaje("Atención","Carga Cancelada","about")
      END IF
   ELSE
      --Obtengo el codigo de error
      CALL fn_valida_operacion(v_pid,           --PID
                               v_proceso_cod,   --Clave de proceso
                               v_opera_cod)     --Clave de operacion
                     RETURNING v_cod_error      --Codigo de error
      --Envio mensaje de error en la operacion
      CALL fn_muestra_inc_operacion(v_cod_error)
   END IF

   --Regreso resultado de la operacion
   RETURN r_band_carga
   
END MAIN

FUNCTION fn_solicita_archivo(p_proceso, p_operacion)
   DEFINE
      p_proceso         LIKE cat_proceso.proceso_cod,
      p_operacion       LIKE cat_operacion.opera_cod,
      v_proceso_desc    LIKE cat_proceso.proceso_desc,
      v_opera_desc      LIKE cat_operacion.opera_desc,
      v_bnd_continua    BOOLEAN,
      v_ruta_rescate    STRING,--LIKE seg_modulo.ruta_rescate
      v_nom_archivo     STRING,
      v_ventana         ui.Window,
      v_cb_archivo      ui.ComboBox,
      v_comando         STRING

   #Consulta que recupera el módulo y descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

   #Consulta que recupera los datos necesarios para conformar las tablas temporales del proceso
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion

   OPEN WINDOW w_solic WITH FORM "RETL3801" ATTRIBUTES (TEXT = "Seleccione archivo")
   
      LET v_ventana = ui.Window.getCurrent()
      #Se estableces el titulo de la ventana
      CALL v_ventana.setText("Seleccion de Archivo")
      #Recupera las propiedades del ComoBox, pasando el nombre del Combo como parámentro 
      LET v_cb_archivo = ui.ComboBox.forName("v_nom_archivo")
      #Elimina todos los elementos del combo
      CALL v_cb_archivo.clear()
      #Recupera los nombres de los archivos en el comboBox
      CALL fn_recupera_archivos(v_cb_archivo, "txt", "SHA")
      RETURNING v_bnd_continua,v_ruta_rescate

      INPUT BY NAME v_nom_archivo ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            DISPLAY BY NAME v_proceso_desc, v_opera_desc
         ON ACTION ACCEPT
            # Recupera el nombre de archivo seleccionado
            LET v_nom_archivo = GET_FLDBUF(v_nom_archivo) CLIPPED

            IF(v_nom_archivo IS NULL OR v_nom_archivo = " ")THEN
               MESSAGE "Seleccione un archivo" ATTRIBUTE(REVERSE)
               NEXT FIELD v_nom_archivo
            END IF
            EXIT INPUT

         ON ACTION cancelar
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_solic

   RETURN v_nom_archivo
END FUNCTION

#Objetivo: Funcion especifica que Llena el comboBox de los nombres de archivo en base a una extension enviada
FUNCTION fn_recupera_archivos(v_cb_archivo, p_extension, p_prefijo)
DEFINE p_proceso         LIKE cat_proceso.proceso_cod,
       p_operacion       LIKE cat_operacion.opera_cod,
       v_cb_archivo      ui.ComboBox,
       v_archivo         STRING,
       v_lista_archivos  INTEGER,
       p_extension       STRING,
       v_prefijo,
       p_prefijo         STRING,
       v_existe_archivos BOOLEAN,
       v_bnd_continua    BOOLEAN,
       v_sql_qry         STRING,
       v_existe_archivo  BOOLEAN,
       v_tmp_archivo     VARCHAR(40),
       v_ruta_rescate    LIKE seg_modulo.ruta_rescate,
       s_ruta_rescate    STRING

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = "ret"

   LET s_ruta_rescate = v_ruta_rescate CLIPPED
   --LET s_ruta_rescate = "/ds"

   LET v_existe_archivos = FALSE
   LET v_bnd_continua = TRUE
   #Valida si existe la ruta del proceso donde se recuperaran los nombres de archivo
   IF NOT(os.Path.exists(s_ruta_rescate))THEN
      CALL fgl_winwait("No existe la ruta "||v_ruta_rescate)
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua,s_ruta_rescate
   END IF
   #Valida si es un directorio valido
   IF NOT os.Path.isdirectory(s_ruta_rescate) THEN
      CALL fgl_winwait( "El parámetro proporcionado no es un directorio")
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua,s_ruta_rescate
   END IF
   #Ordena los archivos por nombre descendente para los archivos de diropen
   CALL os.Path.dirsort("name", "-1")
   #Solo se recuperan archivos
   # 1 - Excluye archivos ocultos, 2 - Excluye directorios, 4 - Excluye Links simbólicos
   CALL os.Path.dirfmask( 1 + 2 + 4 )
   #Recupera la lista de archivos
   LET v_lista_archivos = os.Path.diropen(s_ruta_rescate)
   #Recupera el primer nombre de archivo
   LET v_archivo = os.Path.dirnext(v_lista_archivos)
   #Mientras no se recupere un valor nulo
   WHILE v_archivo IS NOT NULL
      #Genero el prefijo del archivo
      IF v_archivo.getLength() >= p_prefijo.getLength()  THEN
         LET v_prefijo = v_archivo.subString(1,p_prefijo.getLength())
      END IF

      #Filtro para solo agregar archivos al combo
      IF(v_archivo = "." OR v_archivo = ".." OR
         os.Path.isdirectory(v_archivo))THEN
         CONTINUE WHILE
      ELSE
         --DISPLAY "ARCHIVOS:--> ",v_archivo
         #Si la extencion corresponde a la extension del proceso, lo agrega al combo
         IF (os.Path.extension(v_archivo) = p_extension)    --Coincida extensión
         AND (v_prefijo = p_prefijo) THEN                   --Coincida prefijo
            #DISPLAY "ARCHIVOS CON EXTENSION:--> ",v_archivo," EXT:",p_extension
            #Cambia de variable de tipo string a char, para poder usar en consulta
            LET v_tmp_archivo = v_archivo

            CALL v_cb_archivo.additem( v_archivo, v_archivo )
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
   RETURN v_bnd_continua,s_ruta_rescate
END FUNCTION

FUNCTION fn_valida_SHA(v_nom_archivo,v_nom_arc_fin)
   DEFINE
      v_comando            STRING,
      v_nom_archivo        STRING,
      v_nom_arc_fin        STRING,
      v_hash               STRING,
      v_resultado_hash     STRING,
      v_hash_es_correcto   SMALLINT,
      v_tokenizer          base.StringTokenizer,
      v_chpipe             base.Channel
   --Se conforma el comando para validar HASH
   LET v_comando = "sh VerificaCLABES_SHA.sh ", v_nom_archivo, " ", v_nom_arc_fin
   DISPLAY "Verificando HASH del archivo de respuesta: ", v_comando

   --Se abre comunicacion para leer la salida standard
   LET v_chpipe = base.channel.create()
   CALL v_chpipe.openPipe(v_comando, "u")

   CALL v_chpipe.setDelimiter(" ")

   -- se lee el resultado del hash
   WHILE ( v_chpipe.read([v_hash]) )
   
      -- se crea un tokenizer para obtener el hash y el nombre del archivo
      LET v_tokenizer = base.StringTokenizer.create(v_hash," ")
      
      -- si hay tokens
      IF ( v_tokenizer.hasMoreTokens() ) THEN
        
         -- =====================================================================
         -- se lee el resultado de la comparacion
         LET v_resultado_hash =  v_tokenizer.nextToken()
         DISPLAY "Resultado de hash: ", v_resultado_hash
         
         -- se verifica si fue correcto
         CASE v_resultado_hash.trim()
            WHEN "OK"
                DISPLAY "Validación de HASH realizada correctamente... procesando integración."
                LET v_hash_es_correcto = TRUE
            WHEN "1"
                DISPLAY "La validación del HASH no fue correcta. No se procesará el archivo..."
                CALL fn_mensaje("Aviso",
                                "Número de parámetros incorrecto para la validación del archivo SHELL",
                                "stop")
                LET v_hash_es_correcto = FALSE
            WHEN "2"
                DISPLAY "La validación del HASH no fue correcta. No se procesará el archivo..."
                CALL fn_mensaje("Aviso",
                                "ARCHIVO: " || v_nom_archivo || " NO EXISTE EN LA RUTA",
                                "stop")
                LET v_hash_es_correcto = FALSE
            WHEN "3"
                DISPLAY "La validación del HASH no fue correcta. No se procesará el archivo..."
                CALL fn_mensaje("Aviso",
                                "ARCHIVO: " || v_nom_archivo || " ESTA VACÍO",
                                "stop")
                LET v_hash_es_correcto = FALSE
            WHEN "4"
                DISPLAY "La validación del HASH no fue correcta. No se procesará el archivo..."
                CALL fn_mensaje("Aviso",
                                "LA VALIDACION DEL HASH ES INCORRECTA",
                                "stop")
                LET v_hash_es_correcto = FALSE
         END CASE
         EXIT WHILE
         
      END IF
   END WHILE
   RETURN v_hash_es_correcto
END FUNCTION

#Objetivo: Elige el archivo del combobox de acuerdo al proceso que llama la funcion y ejecuta el proceso en linea (1)
#o en nohup (2) dependiendo del parametro p_tipo_carga
FUNCTION fn_carga_archivo(p_pid, p_proceso, p_operacion, p_tipo_carga, p_programa, p_prog_a_lanzar, p_usuario,p_inicia_proceso, v_nom_archivo)

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
       v_mensaje          STRING

   CONSTANT c_folio = 0
   IF p_tipo_carga = 1 THEN
      LET r_bnd_carga = FALSE
   END IF

   #Consulta que recupera la ruta en la que se ubicaran los archivos compilados
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'glo'
   
   # Recuper la información necesaria para cargar el archivo seleccionado 
   # dependidendo del proceso y operación
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         g_ruta_rescate,g_ruta_listados,
                                         g_usuario 

   # se inicia con 100 solo para no entrar en la parte de finalizar operacion
   LET r_resultado_opera = 0

   LET v_bnd_ejecuta = FALSE
    
    # Nombre de archivo a tipo char
    LET v_nom_arch_opera  = v_nom_archivo
    #Verifica los permisos de lectura y escritura del archivo
    IF(fn_verifica_permisos_archivo(g_ruta_rescate ,v_nom_archivo))THEN
       # Inicializa el codigo a 0; no hay inconsistencia
       LET r_resultado_opera = 0

       IF(p_inicia_proceso)THEN
          # En caso de que sea nulo se indica pid = 0 
          IF(p_pid IS NULL OR p_pid = ' ')THEN
             LET p_pid = 0
          END IF
          # se valida si se puede generar el proceso
          CALL fn_valida_operacion(p_pid,p_proceso,p_operacion) RETURNING r_resultado_opera
          IF ( r_resultado_opera <> 0 ) THEN
             CALL fn_muestra_inc_operacion(r_resultado_opera)
             LET v_bnd_ejecuta = FALSE
          END IF
          # se genera el pid para el proceso
          CALL fn_genera_pid(p_proceso,p_operacion,p_usuario)
                    RETURNING p_pid
          CALL fn_inicializa_proceso(p_pid,p_proceso,p_operacion,0,
                                     p_programa,v_nom_archivo,p_usuario)
                            RETURNING r_resultado_opera
          IF ( r_resultado_opera <> 0 ) THEN
             CALL fn_muestra_inc_operacion(r_resultado_opera)
             LET v_bnd_ejecuta = FALSE
          END IF
       END IF
       # Inicia operación
       CALL fn_actualiza_opera_ini(p_pid,p_proceso,p_operacion,c_folio,"GLOE01",
                             v_nom_arch_opera,p_usuario) RETURNING r_resultado_opera 

       # En el caso de que exista una inconsistencia al iniciar el proceso, se
       # Muestra un mensaje con la descripcion
       IF(r_resultado_opera)THEN
          CALL fn_muestra_inc_operacion(r_resultado_opera)
          #No se mostraran las cifras control
          LET v_bnd_ejecuta = FALSE
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
       #Construye comando
       LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/GLOE02.42r ",
                                       p_usuario," ",v_aux_pid," ",v_aux_proc," ",
                                       v_aux_opera," '",v_nom_archivo,"' '",p_prog_a_lanzar,
                        "' 1>>", g_ruta_listados CLIPPED ,
                        "/nohup:",v_aux_pid USING "&&&&&",":",
                                  v_aux_proc USING "&&&&&",":",
                                  v_aux_opera   USING "&&&&&" ,
                        " 2>&1 &"

       DISPLAY v_comando
       #Llamada al proceso de carga nohup (proceso independeinte al flujo de la aplicacion)
       RUN v_comando
       IF(STATUS)THEN
          CALL fn_mensaje("Cargar De Archivo",
                          "Ocurrió un error al iniciar el proceso batch",
                          "bn_about")
       ELSE
          # Se indica que se realizo el proceso de carga
          LET r_bnd_carga = TRUE
          CALL fn_mensaje("Carga De Archivo", 
                          "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_aux_pid,
                          "bn_about")
       END IF
       #No se mostraran las cifras control
       LET v_bnd_ejecuta = FALSE
       
    ELSE
       CALL fn_mensaje("Carga De Archivo",
                       "El archivo a procesar no cuenta con los permisos suficientes",
                       "bn_about")
       LET r_bnd_carga = FALSE
    END IF
    
   RETURN r_bnd_carga
END FUNCTION

#Objetivo: Recupera la informacion necesaria del proceso para cargar el archivo
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

   #Consulta que recupera el módulo y descripción del proceso
   SELECT modulo_cod, proceso_desc
     INTO v_modulo, v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

   #Consulta que recupera los datos necesarios para conformar las tablas temporales del proceso
   SELECT extension, opera_desc, layout_cod
     INTO v_extension, v_opera_desc, v_layout
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion

   #Consulta que recupera el usuario y la ruta donde estan ubicados los archivos del proceso
   SELECT ruta_rescate, usuario
     INTO v_ruta_rescate, v_usuario
     FROM seg_modulo
    WHERE modulo_cod = v_modulo

   #Ruta donde se coloca el archivo de monitoreo
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
    --inicialización de variables para PRUEBAS UNITARIAS
    --LET v_ruta_rescate = "/ds/safreviv/glo/bin"
    --LET v_proceso_desc = "Descripción de proceso"
    --LET v_opera_desc = "Descripción de operacion"
    --LET v_usuario = "desa3"
    --LET v_extension = "F110912"
    --LET v_layout = 30604
    --LET v_layout = 4983

   RETURN v_proceso_desc, v_extension, v_opera_desc,
          v_layout, v_ruta_rescate, v_ruta_listados,  v_usuario
END FUNCTION

#Objetivo: Verifica que se pueda leer y escribir en el archivo 
FUNCTION fn_verifica_permisos_archivo(p_ruta_rescate,p_archivo)
DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
       p_archivo        STRING,
       v_bnd_continua   SMALLINT,
       v_cambio_permiso INTEGER

   LET p_archivo = p_ruta_rescate CLIPPED||"/"||p_archivo
   DISPLAY "El archivo a cambiar:"
   DISPLAY p_archivo
   #Valida si se puede leer el archivo
   IF os.Path.readable(p_archivo) THEN
      #Valida si se puede escribir en el archivo
      IF os.Path.writable(p_archivo) THEN
         LET v_bnd_continua = TRUE
      ELSE
         #Intenta cambiar los permisos de lectura y escritura del archivo
         CALL os.Path.chrwx(p_archivo, 777) RETURNING v_cambio_permiso
         #Si v_cambio_permiso = TRUE, cambió los permisos correctamente
         IF(v_cambio_permiso)THEN
            LET v_bnd_continua = TRUE
         ELSE
            LET v_bnd_continua = FALSE
         END IF
      END IF
   ELSE
      #Intenta cambiar los permisos de lectura y escritura del archivo
      CALL os.Path.chrwx(p_archivo, 777) RETURNING v_cambio_permiso
      #Si v_cambio_permiso = TRUE, cambió los permisos correctamente
      IF(v_cambio_permiso)THEN
         LET v_bnd_continua = TRUE
      ELSE
         LET v_bnd_continua = FALSE
      END IF
   END IF
   RETURN v_bnd_continua 
END FUNCTION