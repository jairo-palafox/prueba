#############################################################################
#Módulo          => GLO                                                     #        
#Programa        => GLOE02.4gl                                              #
#Objetivo        => Programa general de carga de archivo en modo batch      #
#Fecha Inicio    => 23 DICIEMBRE 2011                                       #
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
       v_ruta_rescate      STRING,
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
       v_continua          BOOLEAN
       
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
   
   #Genera las tablas, separa la informacion del archivo y carga las tablas
   CALL fn_valida_archivo(v_ruta_rescate,p_nom_archivo, 
                          v_layout,v_usuario,
                          v_archivo_monitoreo, v_ruta_listados) RETURNING v_indice, g_reg_tab_cargados, v_cadena_registros
                          
   # Validacion para revisar si hay registros de layout
   DISPLAY "Registro v_indice ",v_indice, g_reg_tab_cargados, v_cadena_registros --ERV
   IF ( v_indice < 1 ) THEN
      # Actualiza a estado erroneo
      CALL fn_error_opera(p_pid,p_proceso,p_operacion)
                RETURNING r_resultado_opera
      IF ( r_resultado_opera ) THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = "Ha ocurrido un error al realizar la carga de archivo"||
                      "\nNo existe el layout para el proceso"
      # Envia correo de estado de operación
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
   ELSE
      # se inicializa pensando que el flujo es correcto
      LET v_continua = TRUE
      
      #Cálcula las cifras control
      CALL fn_obtiene_cifras(v_ruta_rescate,p_nom_archivo) 
              RETURNING v_reg_archivo,v_reg_aceptados,v_reg_rechazados
              
      IF NOT ( v_reg_archivo > 0 ) THEN
         DISPLAY "\n****************************************************"
         DISPLAY "\nNO SE ENCONTRÓ INFORMACIÓN EN ARCHIVO:",p_nom_archivo,"\n\n"
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso,p_operacion) 
                RETURNING r_resultado_opera
         IF ( r_resultado_opera ) THEN
            # Muestra el mensaje de inconsistencia en archivo y consola
            CALL fn_desplega_inc_operacion(r_resultado_opera)
         END IF
         LET v_mensaje = "\nNO SE ENCONTRÓ INFORMACIÓN EN ARCHIVO:",p_nom_archivo,"\n"
         # Envia correo de estado de operación               
         CALL fn_correo_proceso(p_pid, 
                                p_proceso, 
                                p_operacion, 
                                '', # Archivo adjunto
                                'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                 v_mensaje
                                )
      ELSE
         # v_reg_archivo > 0 validacion para que se cargue un archivo con registros
         IF ( v_reg_archivo = v_reg_aceptados ) THEN
            
            #Almacena registro para archivo procesado correctamente
            CALL fn_ingresa_etapa(p_proceso, p_operacion, p_nom_archivo) RETURNING r_bnd_carga
            
            IF ( r_bnd_carga ) THEN
               # Finaliza la operacion de carga de archivo
               CALL  fn_actualiza_opera_fin(p_pid,p_proceso,p_operacion)
                                RETURNING r_resultado_opera
               IF ( r_resultado_opera = 0 ) THEN
                  # Ejecuta la operacion que se ha pasado como parámetro
                  CALL fn_ejecuta_lanzado(p_prog_a_lanzar)
                  LET v_mensaje = "El proceso de carga ha finalizado correctamente"
                  # Envia correo de estado de operación               
                  CALL fn_correo_proceso(p_pid, 
                                         p_proceso, 
                                         p_operacion, 
                                         '', # Archivo adjunto
                                         'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                          v_mensaje
                                         )
               ELSE              
                 # Actualiza a estado erróneo
                 CALL fn_error_opera(p_pid,p_proceso,p_operacion) 
                        RETURNING r_resultado_opera
                 IF ( r_resultado_opera ) THEN
                    # Muestra el mensaje de inconsistencia en archivo y consola
                    CALL fn_desplega_inc_operacion(r_resultado_opera)
                 END IF
                 LET v_mensaje = "Ocurrió un error al actualizar el estado de la operación"
                 # Envia correo de estado de operación               
                 CALL fn_correo_proceso(p_pid, 
                                        p_proceso, 
                                        p_operacion, 
                                        '', # Archivo adjunto
                                        'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                         v_mensaje
                                        )
               END IF               
            ELSE
               # Actualiza a estado erroneo
               CALL fn_error_opera(p_pid,p_proceso,p_operacion)
                       RETURNING r_resultado_opera
               IF ( r_resultado_opera ) THEN
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
               END IF
            END IF         
         ELSE
            LET v_mensaje = "Ha ocurrido un error al realizar la carga de archivo"
            
            LET v_mensaje = v_mensaje||"\nNúmero de registros de archivo no corresponde a número de registros insertados:" 
            LET v_mensaje = v_mensaje||"\nRegistros en archivo:",v_reg_archivo||
                                       "\nRegistros insertados:",v_reg_aceptados
            
            DISPLAY "\nHA OCURRIDO UN ERROR AL REALIZAR LA CARGA DE ARCHIVO"
            DISPLAY "\nNÚMERO DE REGISTROS DE ARCHIVO NO CORRESPONDE A NÚMERO DE REGISTROS INSERTADOS:" 
            DISPLAY "\nREGISTROS EN ARCHIVO:",v_reg_archivo
            DISPLAY "\nREGISTROS INSERTADOS:",v_reg_aceptados
            # si el conteo de registros de cada tabla es diferente al conteo de registros del archivo
            # en el caso de que un registro no contega el tipo de registro de cat_layout
            IF ( g_reg_tab_cargados <> v_reg_archivo ) THEN
               LET v_mensaje = v_mensaje||"\nRegistros procesados:",g_reg_tab_cargados
               DISPLAY "REGISTROS PROCESADOS:",g_reg_tab_cargados
               LET v_ltr_archivo = v_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".reg_no_procesados.dat"
               LET v_ltr_archivo_aux = v_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".cont_no_procesados.dat"
               #genera archivo de registros no procesados
               LET v_comando = "egrep -v '"||v_cadena_registros||"' '"||v_ruta_rescate CLIPPED||"/"||p_nom_archivo CLIPPED||
                                           "' > "||v_ltr_archivo
               RUN v_comando
               LET v_comando = "wc -l '"||v_ltr_archivo,
                          "' | awk '{ print $1 }'  > '", v_ltr_archivo_aux,"'"
               --DISPLAY v_comando
               RUN v_comando
               DISPLAY v_ltr_archivo_aux
               LET v_canal = base.Channel.create()
               CALL v_canal.openFile(v_ltr_archivo_aux,"r")
               LET v_reg_no_procesados = v_canal.readLine()
               CALL v_canal.close()
               LET v_mensaje = v_mensaje||"\nRegistros no procesados:",v_reg_no_procesados,"\n"
               DISPLAY "REGISTROS NO PROCESADOS:",v_reg_no_procesados,"\n"
               --DISPLAY "REGISTROS NO PROCESADOS POR TIPO DE REGISTRO DIREFENTE AL ESPERADO EN cat_layout"
               LET v_mensaje = v_mensaje||"\nRegistros no procesados porque presentan tipo de registro inválido" 
                
               DISPLAY "\nREGISTROS NO PROCESADOS PORQUE PRESENTAN TIPO DE REGISTRO INVÁLIDO"  
               # imprime registros no procesados
               RUN "head -n 5 "||v_ruta_rescate||"/"||p_usuario CLIPPED||".reg_no_procesados.dat"
               DISPLAY "..."
               --RUN "tail "||v_ruta_rescate||"/"||p_usuario CLIPPED||".reg_no_procesados.dat"
         
               --RUN "rm "||v_ltr_archivo
               RUN "rm "||v_ltr_archivo_aux
               
            END IF
            # Actualiza registro, para indicar que el estado del proceso batch es erróneo
            # Indica que la operacion es erronea
            CALL fn_error_opera(p_pid,p_proceso,p_operacion)
                     RETURNING r_resultado_opera
            IF ( r_resultado_opera ) THEN
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF         
            # Envia correo de estado de operación
            CALL fn_correo_proceso(p_pid, 
                                   p_proceso, 
                                   p_operacion, 
                                   '', # Archivo adjunto
                                   'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                                    v_mensaje
                                   )
         END IF 
      END IF
   END IF
END MAIN


