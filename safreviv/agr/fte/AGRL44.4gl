--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL44                                        #
#Objetivo          =>Programa que permite la carga de información  #
#                    del archivo de Uso de Garantía Estados y      #
#                    Municipios                                    #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>05 Julio 2013                                 #
####################################################################

IMPORT os
DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE m_d_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       m_si_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       m_si_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       m_s_ruta_rescate  STRING,--LIKE seg_modulo.ruta_rescate,
       m_c_usuario       LIKE seg_modulo.usuario,
       m_s_ruta_listados STRING

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recuperan los valores que vienen desde parametro 
   LET m_c_usuario      = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(m_c_usuario CLIPPED ||".AGRL44.log" )

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se asigna proceso y operacion
   LET m_si_proceso_cod = g_proc_cod_agr_gtia_edo_mcpio -- Recepción uso gtÍa edos y mcpios
   LET m_si_opera_cod   = 1 -- Integrar Uso Gtía edo mcpio

   -- se invoca la funcion de carga
   CALL fn_carga_archivo_deudor()
END MAIN

# Objetivo: Elige el archivo del combobox de acuerdo al proceso que llama la funcion y ejecuta el proceso en linea (1)
#           o en nohup (2) dependiendo del parametro p_tipo_carga
FUNCTION fn_carga_archivo_deudor()
DEFINE p_programa         LIKE bat_ctr_operacion.programa_cod,
       p_folio            DECIMAL(9,0),
       v_layout           LIKE cat_operacion.layout_cod,
       v_extension        LIKE cat_operacion.extension,
       v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
       v_proceso_desc     LIKE cat_proceso.proceso_desc,
       v_opera_desc       LIKE cat_operacion.opera_desc,
       v_nom_archivo      STRING,
       v_nom_arch_opera   VARCHAR(40),
       v_bnd_ejecuta      BOOLEAN,
       r_bnd_carga        BOOLEAN,
       v_aux_pid          STRING,
       v_aux_proc         STRING,
       v_aux_opera        STRING,
       v_comando          STRING,
       v_bnd_continua     BOOLEAN,
       v_cb_archivo       ui.ComboBox,
       v_ventana          ui.Window,
       v_forma_actual     ui.Form,
       r_resultado_opera  SMALLINT,
       v_fecha_inicio     DATETIME YEAR TO SECOND
       
   -- se inicializan variables
   LET r_resultado_opera = 0 -- se asume que no hay error
   LET v_bnd_ejecuta = FALSE   

   -- se invoca la función que obtiene la información del proceso
   DISPLAY "Recupera informacion del proceso ", m_si_proceso_cod
   CALL fn_recupera_inf_proceso() RETURNING v_proceso_desc,
                                            v_extension, 
                                            v_opera_desc,
                                            v_layout,
                                            m_s_ruta_rescate,
                                            m_s_ruta_listados,
                                            m_c_usuario
   
   CLOSE WINDOW SCREEN
{
   CALL ui.Interface.loadActionDefaults("carga")
   CALL ui.Interface.loadStyles("carga")
   
   CALL ui.interface.settype("child")
   CALL ui.interface.setcontainer("mdi")
}
   OPEN WINDOW w_carga_spes WITH FORM "AGRL441" ATTRIBUTES (TEXT = v_opera_desc)
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma_actual = v_ventana.getForm()

   -- se obtiene el apuntador al combo en pantalla
   LET v_cb_archivo = ui.ComboBox.forName("v_nom_archivo")

   -- Elimina todos los elementos del combo
   CALL v_cb_archivo.clear()

   DISPLAY "recupera archivos"
   -- Recupera los nombres de los archivos en el comboBox
   CALL fn_recupera_archivos(v_cb_archivo, v_extension) RETURNING v_bnd_continua

   INPUT BY NAME v_nom_archivo ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      BEFORE INPUT   
         --  Bandera para determinar si se realizó o canceló el proceso de carga
         LET r_bnd_carga = FALSE
         LET r_resultado_opera = 0
         
         DISPLAY BY NAME v_proceso_desc, v_opera_desc

         -- Si no se ha recuperado algún archivo, se desactiva el boton Aceptar
         IF NOT(v_bnd_continua)THEN
            CALL DIALOG.setActionActive("accept",FALSE)
         END IF

      ON ACTION ACCEPT
         -- Recupera el nombre de archivo seleccionado
         LET v_nom_archivo = GET_FLDBUF(v_nom_archivo) CLIPPED
         
         IF ( v_nom_archivo IS NULL OR v_nom_archivo = " " ) THEN
            MESSAGE "Seleccione un archivo" ATTRIBUTE(REVERSE)
            NEXT FIELD v_nom_archivo
         END IF
                     
         -- Nombre de archivo a tipo char
         LET v_nom_arch_opera  = v_nom_archivo            
         
         -- se verifica si se puede iniciar la operacion
         CALL fn_valida_operacion(0, m_si_proceso_cod, m_si_opera_cod) RETURNING r_resultado_opera

         -- si se puede lanzar la carga
         IF ( r_resultado_opera = 0 ) THEN
            --  se genera el pid para el proceso
            CALL fn_genera_pid(m_si_proceso_cod, m_si_opera_cod, m_c_usuario) RETURNING m_d_pid

            -- se obtiene el folio
            CALL fn_genera_folio(m_si_proceso_cod, m_si_opera_cod, m_c_usuario) RETURNING p_folio

            DISPLAY "folio: ", p_folio 

            -- se inicia el proceso
            CALL fn_inicializa_proceso(m_d_pid, m_si_proceso_cod, m_si_opera_cod,
                                       p_folio, p_programa, v_nom_archivo,
                                       m_c_usuario) RETURNING r_resultado_opera

            -- En el caso de que exista una inconsistencia al iniciar el proceso, se
            -- Muestra un mensaje con la descripcion
            IF ( r_resultado_opera ) THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               -- No se mostraran las cifras control
               LET v_bnd_ejecuta = FALSE
               EXIT INPUT
            END IF

           -- se captura la fecha y hora en que se inició la operación
           LET v_fecha_inicio = CURRENT YEAR TO SECOND

           -- Quita los espacios al principio y al final de cada variable para el paso de parametros
           LET v_aux_pid   = m_d_pid
           LET v_aux_pid   = v_aux_pid.trim()                  
           LET v_aux_proc  = m_si_proceso_cod
           LET v_aux_proc  = v_aux_proc.trim()
           LET v_aux_opera = m_si_opera_cod
           LET v_aux_opera = v_aux_opera.trim()

           SELECT ruta_bin
             INTO v_ruta_ejecutable
             FROM seg_modulo
            WHERE modulo_cod = "agr"

           -- Construye comando que ejecutara la carga
           LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/AGRP29.42r ",
                            m_c_usuario," ", -- usuario
                            v_aux_pid," ", -- pid
                            v_aux_proc," ", -- proceso
                            v_aux_opera," ", -- operacion
                            p_folio," '", -- folio
                            v_nom_archivo,"' ", -- nombre de archivo
                            " 1>", m_s_ruta_listados CLIPPED ,
                            "/nohup:",v_aux_pid   USING "&&&&&",":",
                                      v_aux_proc  USING "&&&&&",":",
                                      v_aux_opera USING "&&&&&" ,
                            " 2>&1 &"
                            
           DISPLAY v_comando 
           
           -- Llamada al proceso de carga nohup (proceso independeinte al flujo de la aplicacion)
           RUN v_comando                  
           
           IF ( STATUS ) THEN
              CALL fn_mensaje("Carga de Archivo", "Ocurrió un error al iniciar el proceso batch", "bn_about")
              CONTINUE INPUT
           ELSE
              --  Se indica que se realizo el proceso de carga
              LET r_bnd_carga = TRUE
              CALL fn_mensaje("Carga De Archivo", "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_aux_pid, "bn_about")
           END IF
         ELSE -- si se puede enviar la operacion
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         END IF
         EXIT INPUT
         
      ON ACTION cancelar
         EXIT INPUT

   END INPUT 
      
   CLOSE WINDOW w_carga_spes      
END FUNCTION

#Objetivo: Recupera la informacion necesaria del proceso para cargar el archivo
FUNCTION fn_recupera_inf_proceso()
DEFINE v_layout        LIKE cat_operacion.layout_cod,
       v_extension     LIKE cat_operacion.extension,
       v_modulo        LIKE cat_proceso.modulo_cod,
       v_proceso_desc  LIKE cat_proceso.proceso_desc,
       v_opera_desc    LIKE cat_operacion.opera_desc,
       v_ruta_rescate  LIKE seg_modulo.ruta_rescate,
       v_usuario       LIKE seg_modulo.usuario,
       v_ruta_listados LIKE seg_modulo.ruta_listados
       
   -- consulta que recupera el módulo y descripción del proceso
   SELECT modulo_cod, proceso_desc
     INTO v_modulo, v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = m_si_proceso_cod

    DISPLAY "Modulo y proceso desc: ", v_modulo, " ", v_proceso_desc
    
   -- consulta que recupera los datos necesarios para conformar las tablas temporales del proceso
   SELECT extension, opera_desc, layout_cod
     INTO v_extension, v_opera_desc, v_layout
     FROM cat_operacion
    WHERE proceso_cod = m_si_proceso_cod
      AND opera_cod   = m_si_opera_cod

   DISPLAY "v_extension, v_opera_desc, v_layout: ", v_extension, v_opera_desc, v_layout
      
   -- consulta que recupera el usuario y la ruta donde estan ubicados los archivos del proceso
   SELECT ruta_rescate, usuario
     INTO v_ruta_rescate, v_usuario
     FROM seg_modulo
    WHERE modulo_cod = v_modulo

   DISPLAY "v_ruta_rescate, v_usuario: ", v_ruta_rescate, v_usuario
    
   -- ruta donde se coloca el archivo de monitoreo
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   RETURN v_proceso_desc, v_extension, v_opera_desc,
          v_layout, v_ruta_rescate CLIPPED, v_ruta_listados CLIPPED,  v_usuario CLIPPED
END FUNCTION

#Objetivo: Llena el comboBox de los nombres de archivo que se encuentren en la ruta correspondiente al proceso
FUNCTION fn_recupera_archivos(v_cb_archivo, p_extension)
DEFINE v_cb_archivo      ui.ComboBox,
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

   DISPLAY "Ruta rescate ", m_s_ruta_rescate
   
   -- valida si existe la ruta del proceso donde se recuperaran los nombres de archivo
   IF NOT(os.Path.exists(m_s_ruta_rescate))THEN
      CALL fgl_winwait("No existe la ruta "||m_s_ruta_rescate)
      DISPLAY "No existe la ruta " , m_s_ruta_rescate
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF

   -- valida si es un directorio valido
   IF NOT os.Path.isdirectory(m_s_ruta_rescate) THEN
      DISPLAY "El parámetro proporcionado no es un directorio"
      CALL fgl_winwait( "El parámetro proporcionado no es un directorio")
      LET v_bnd_continua = FALSE
      RETURN  v_bnd_continua
   END IF

   -- consulta que determina si ya se ha procesado el archivo
   LET v_sql_qry = " SELECT FIRST 1 NVL(1,0)\n",
                   "   FROM glo_ctr_archivo\n",
                   "  WHERE proceso_cod = ?\n",
                   "    AND opera_cod = ?\n",
                   "    AND nombre_archivo = ?\n",
                   "    AND estado IN (1,2)"

   PREPARE prp_existe_archivo FROM v_sql_qry 

   -- ordena los archivos por nombre descendente para los archivos de diropen   
   CALL os.Path.dirsort("name", "-1")

   -- solo se recuperan archivos
   -- 1 - Excluye archivos ocultos, 2 - Excluye directorios, 4 - Excluye Links simbólicos 
   CALL os.Path.dirfmask( 1 + 2 + 4 )

   -- recupera la lista de archivos
   LET v_lista_archivos = os.Path.diropen(m_s_ruta_rescate)

   -- recupera el primer nombre de archivo
   LET v_archivo = os.Path.dirnext(v_lista_archivos)

   -- mientras no se recupere un valor nulo
   WHILE v_archivo IS NOT NULL
      -- filtro para solo agregar archivos al combo
      IF(v_archivo = "." OR v_archivo = ".." OR
         os.Path.isdirectory(v_archivo))THEN
         CONTINUE WHILE
      ELSE
         --DISPLAY "ARCHIVOS:--> ",v_archivo
         DISPLAY "Buscando archivo de extension ", p_extension
         
         -- si la extencion corresponde a la extension del proceso, lo agrega al combo
         IF ( os.Path.extension(v_archivo) = p_extension ) THEN
            --DISPLAY "ARCHIVOS CON EXTENSION:--> ",v_archivo," EXT:",p_extension
            -- cambia de variable de tipo string a char, para poder usar en consulta
            LET v_tmp_archivo = v_archivo
            
            -- verifica si ya se ha procesado el archivo en glo_ctr_archivo
            EXECUTE prp_existe_archivo USING v_tmp_archivo, m_si_proceso_cod, m_si_opera_cod INTO v_existe_archivo
            
            IF ( SQLCA.SQLCODE = 100 ) THEN
               LET v_existe_archivo = 0
               --DISPLAY "BOOL:--> ",v_existe_archivo
            END IF
            
            IF ( NOT v_existe_archivo ) THEN
               DISPLAY "ARCHIVOS A MOSTRAR:--> ",v_archivo
               -- añade el nombre del archivo al combo box
               CALL v_cb_archivo.additem( v_archivo, v_archivo )
            END IF
         END IF
      END IF

      -- recupera el siguiente archivo
      LET v_archivo = os.Path.dirnext(v_lista_archivos)
   END WHILE

   -- indica si no existe algun archivo
   IF NOT(v_cb_archivo.getItemCount())THEN
      CALL fgl_winwait("No se ha encontrado algún archivo")
      DISPLAY "No se ha encontrado algún archivo"
      LET v_bnd_continua = FALSE
   END IF

   -- cierra la lista de archivos
   CALL os.Path.dirclose(v_lista_archivos)
   RETURN v_bnd_continua
END FUNCTION


