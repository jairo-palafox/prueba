################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 09/11/2017                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL18                                                   #
#Objetivo          => Programa lanzador para extraer archivo de Conciliación   #
#                     RENAPO                                                   #
#Fecha inicio      => 09/11/2017                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE p_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE p_tipo_ejecucion     SMALLINT    -- Forma como ejecutara el programa 
PRIVATE DEFINE p_titulo             VARCHAR(30) -- Almacena opción del menú

PRIVATE DEFINE v_pid                LIKE bat_ctr_proceso.pid 
PRIVATE DEFINE v_proceso_cod        LIKE cat_proceso.proceso_cod -- codigo del proceso
PRIVATE DEFINE v_opera_cod          LIKE cat_operacion.opera_cod -- codigo de operacion
PRIVATE DEFINE v_nom_archivo        CHAR(80)
PRIVATE DEFINE arr_detalle          DYNAMIC ARRAY OF RECORD
      v_nom_archivo                 CHAR(80),
      v_fecha                       DATE
END RECORD 

MAIN

   DEFINE v_result_carga            SMALLINT -- Bandera de carga de archivo
   DEFINE v_comando                 STRING
   DEFINE v_ruta_ejecutable         LIKE seg_modulo.ruta_bin -- Ruta del ejecutable 
   DEFINE v_ruta_listados           LIKE seg_modulo.ruta_listados -- Ruta del log   
   DEFINE r_resultado_opera         INTEGER
   DEFINE v_respuesta               SMALLINT
   DEFINE tot_reg                   DECIMAL(10,0)
   DEFINE v_ruta_envio              CHAR(40)
   DEFINE cve_dependencia           CHAR(30)
   DEFINE v_fecha_archivo           DATE,  
          v_hora_archivo            DATETIME HOUR TO HOUR ,
          v_min_archivo             DATETIME MINUTE TO MINUTE,
          v_sec_archivo             DATETIME SECOND TO SECOND,
          v_hora                    STRING
   
   
   -- se asignan los parametros que vienen del fglrun
   LET p_usuario = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   LET v_proceso_cod = 2122
   LET v_opera_cod = 1



   LET v_fecha_archivo = TODAY 
   LET v_hora_archivo  = CURRENT HOUR TO HOUR
   LET v_min_archivo   = CURRENT MINUTE TO MINUTE
   LET v_sec_archivo   = CURRENT SECOND TO SECOND
   LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo

   LET v_nom_archivo = "concilia_renapo_",v_hora CLIPPED,".txt"   

   SELECT ruta_bin
   INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

   SELECT ruta_listados
   INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   CLOSE WINDOW SCREEN

   OPEN WINDOW w1 WITH FORM "CBDL181"
   DIALOG ATTRIBUTES(UNBUFFERED)
      
   DISPLAY ARRAY arr_detalle TO r_detalle.* --ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      BEFORE DISPLAY 

         LET arr_detalle[1].v_nom_archivo = v_nom_archivo
         LET arr_detalle[1].v_fecha       = v_fecha_archivo
         
         CALL ui.interface.refresh()


   
      ON ACTION Generar   

         LET v_respuesta = 0
         CALL fn_ventana_confirma("Aviso", 
                                  "¿Está seguro que desea extraer la Conciliación RENAPO?", 
                                  "quest")
         RETURNING v_respuesta
         IF v_respuesta = 1 THEN
            -- Valida operacion para verificar si se puede continuar.
            CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera

            IF ( r_resultado_opera <> 0 ) THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            ELSE
               # se genera el pid para el proceso
               CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario)
               RETURNING v_pid               

               CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                          "CBDS09",v_nom_archivo,p_usuario)
               RETURNING r_resultado_opera

               IF ( r_resultado_opera <> 0 ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
               ELSE
                  # Inicia operación
                  CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,null,"CBDS09",
                  v_nom_archivo,p_usuario) RETURNING r_resultado_opera
                  # En el caso de que exista una inconsistencia al iniciar el proceso, se
                  # Muestra un mensaje con la descripcion
                  IF(r_resultado_opera)THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                  ELSE
                     LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS09.42r ",
                                                     p_usuario," ",
                                                     v_pid," ",
                                                     v_proceso_cod," ",
                                                     v_opera_cod," ",
                                                     arr_detalle[1].v_nom_archivo," ",
                                                     " 1>", v_ruta_listados CLIPPED ,
                                                     "/nohup:",v_pid USING "&&&&&",":",
                                                     v_proceso_cod USING "&&&&&",":",
                                                     v_opera_cod USING "&&&&&" ," 2>&1 &"

                     DISPLAY v_comando
                     RUN v_comando

                     IF(STATUS)THEN
                        CALL fn_mensaje("Generacion de Archivo", 
                                        "Ocurrió un error al iniciar el proceso batch",
                                        "bn_about")
                     ELSE
                        # Se indica que se realizo el proceso de carga
                        CALL fn_mensaje("Generacion de Archivo", 
                                        "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                                        "bn_about")
                        EXIT DIALOG
                     END IF
                  END IF 
               END IF
            END IF
         END IF 

         ON ACTION Cancelar
            EXIT DIALOG      

      END DISPLAY

      {ON ACTION CANCEL
         EXIT DIALOG} 

   END DIALOG    
END MAIN