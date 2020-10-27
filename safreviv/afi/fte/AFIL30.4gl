################################################################################
# Modulo       => AFI                                                          #
# Programa     => AFIL30                                                       #
# Objetivo     => Lanzador para carga de archivo de modificación Fecha de Nac  #
# Autor        => Antonio Gómez                                                #
# Fecha        => Agosto 21, 2017                                              #
################################################################################
IMPORT os

DATABASE safre_viv

   DEFINE g_usuario              CHAR (20)
   DEFINE g_tipo_ejecucion       SMALLINT                      -- forma como se ejecutara el programa
   DEFINE g_nom_ventana          STRING                        -- título de la ventana
   DEFINE g_pid                  DECIMAL (9,0)                 -- ID del proceso
   DEFINE g_proceso_cod          LIKE cat_proceso.proceso_cod  -- código del proceso
   DEFINE g_opera_cod            LIKE cat_operacion.opera_cod  -- código de operacion
   DEFINE v_cb_archivo           ui.ComboBox
   DEFINE v_extension            STRING
   DEFINE v_bnd_continua         BOOLEAN
   DEFINE v_nom_archivo          STRING
   DEFINE g_ruta_rescate         STRING
   DEFINE v_resultado            SMALLINT 

MAIN 

   DEFINE v_s_comando        STRING
   DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados
   DEFINE v_status           SMALLINT
   

   LET g_usuario           = ARG_VAL (1)
   LET g_tipo_ejecucion    = ARG_VAL (2)     --forma como se ejecutara el programa
   LET g_nom_ventana       = ARG_VAL (3)

   LET g_proceso_cod = 1823        --se asigna valor al proceso
   LET g_opera_cod   = 1           --se asigna la operacion
   LET v_extension   = "fnac"    --se asigna el tipo de extenxion que se tomara de la ruta rescate

   --Obtiene ruta de los binarios
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   --Obtiene ruta listados batch
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- se asigna el titulo del programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   -- se inicia el log del programa
   CALL STARTLOG (g_usuario CLIPPED||".AFIL30.log")

   OPEN WINDOW AFIL30 WITH FORM "AFIL301"

      #Recupera las propiedades del ComoBox, pasando el nombre del Combo como parámentro 
      LET v_cb_archivo = ui.ComboBox.forName("v_nom_archivo")

      #Recupera los nombres de los archivos en el comboBox
      CALL fn_recupera_archivo_afi_cvat(g_proceso_cod, g_opera_cod,v_cb_archivo, v_extension) RETURNING v_bnd_continua

      INPUT BY NAME v_nom_archivo ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
            --se valida que el usuario si elija un archivo
            IF v_nom_archivo IS NULL THEN
               CALL fn_mensaje("INFORMACION", "Se tiene que elegir un archivo", "about")
               NEXT FIELD v_nom_archivo
            END IF 

            -- se invoca la funcion que valida la operacion
            CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING v_resultado

            -- se verifica si la operacion en proceso es valida
            IF v_resultado <> 0 THEN  
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(v_resultado)
               DISPLAY "ERROR en fn_valida_operacion"
            ELSE

               --Se genera PID del proceso
               CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid
               CALL fn_inicializa_proceso(g_pid,
                                          g_proceso_cod,
                                          g_opera_cod,
                                          0,
                                          'AFIL30',
                                          '',
                                          g_usuario)
                                RETURNING v_resultado

               CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,
                                           g_opera_cod,"",
                                           "AFIL30","",
                                           g_usuario)  RETURNING v_resultado

               LET v_s_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                 "/AFIE12 ",g_usuario CLIPPED," ",g_pid," ",
                                 g_proceso_cod," ",g_opera_cod," '",v_nom_archivo,"' ",
                                 " ' '  1>", v_ruta_listados CLIPPED ,
                                 "/nohup:",g_pid USING "&&&&&",":",
                                 g_proceso_cod USING "&&&&&",":",
                                 g_opera_cod USING "&&&&&" ," 2>&1 &"
               RUN v_s_comando

               LET v_s_comando = "Se ejecutó la generación de archivos"," ",
                                 "Verificar en el monitor de proceso la ejecución el PID ", g_pid USING "<<<<<<<<<"
               CALL fn_mensaje("Cuentas",v_s_comando,"information")            

               DISPLAY "nombre de archivo", v_nom_archivo
            END IF
            EXIT INPUT 

         ON ACTION CANCEL 
            EXIT INPUT 
      END INPUT 

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_status

   CLOSE WINDOW AFIL30
END MAIN 

#Objetivo: Llena el comboBox de los nombres de archivo que se encuentren en la ruta correspondiente al proceso
FUNCTION fn_recupera_archivo_afi_cvat(p_proceso, p_operacion,v_cb_archivo, p_extension)
   DEFINE p_proceso         LIKE cat_proceso.proceso_cod
   DEFINE p_operacion       LIKE cat_operacion.opera_cod
   DEFINE v_cb_archivo      ui.ComboBox
   DEFINE v_archivo         STRING
   DEFINE v_lista_archivos  INTEGER
   DEFINE p_extension       STRING
   DEFINE v_existe_archivos BOOLEAN
   DEFINE v_bnd_continua    BOOLEAN
   DEFINE v_sql_qry         STRING
   DEFINE v_existe_archivo  BOOLEAN
   DEFINE v_tmp_archivo     VARCHAR(40)
   DEFINE v_ruta_rescate    LIKE seg_modulo.ruta_rescate

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   LET g_ruta_rescate    = v_ruta_rescate CLIPPED
   LET v_existe_archivos = FALSE
   LET v_bnd_continua    = TRUE

   #Valida si existe la ruta del proceso donde se recuperaran los nombres de archivo
   IF NOT(os.Path.exists(g_ruta_rescate))THEN
      CALL fgl_winwait("No existe la ruta "||g_ruta_rescate)
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF
   #Valida si es un directorio valido
   IF NOT os.Path.isdirectory(g_ruta_rescate) THEN
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
   LET v_lista_archivos = os.Path.diropen(g_ruta_rescate)
   #Recupera el primer nombre de archivo
   LET v_archivo = os.Path.dirnext(v_lista_archivos)
   #Mientras no se recupere un valor nulo
   WHILE v_archivo IS NOT NULL

      #Filtro para solo agregar archivos al combo
      IF(v_archivo = "." OR v_archivo = ".." OR
         os.Path.isdirectory(v_archivo))THEN
         CONTINUE WHILE
      ELSE
       --  DISPLAY "ARCHIVOS:--> ",v_archivo
         #Si la extencion corresponde a la extension del proceso, lo agrega al combo
       --  DISPLAY os.Path.extension(v_archivo)
        -- DISPLAY p_extension
         IF(os.Path.extension(v_archivo) = p_extension)THEN
         
            --DISPLAY "ARCHIVOS CON EXTENSION:--> ",v_archivo," EXT:",p_extension
            #Cambia de variable de tipo string a char, para poder usar en consulta
            LET v_tmp_archivo = v_archivo
            DISPLAY "DATOS CONSULTA : ", v_tmp_archivo,p_proceso,p_operacion
            #Verifica si ya se ha procesado el archivo en glo_ctr_archivo
            EXECUTE prp_existe_archivo USING v_tmp_archivo,p_proceso,p_operacion INTO v_existe_archivo
           -- DISPLAY "SQL:--> ",SQLCA.SQLCODE
            IF(SQLCA.SQLCODE = 100)THEN
               LET v_existe_archivo = 0
          --    DISPLAY "BOOL:--> ",v_existe_archivo
            END IF

            IF NOT(v_existe_archivo)THEN
            --  DISPLAY "ARCHIVOS A MOSTRAR:--> ",v_archivo
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
