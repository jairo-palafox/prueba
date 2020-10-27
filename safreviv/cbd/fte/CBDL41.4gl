

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario

DATABASE safre_viv
MAIN
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio
   DEFINE v_usuario           VARCHAR(30) -- Almacena al usuario
   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados -- Rute del log
   
   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)

   DEFINE v_comando           STRING

   DEFINE v_fecha             DATE --Fecha con la que se relizara la extracción 

     -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)

    -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   LET v_proceso_cod = 2120
   LET v_opera_cod = 1
   LET v_usuario = 'safreviv'
   LET v_folio = 0
   --LET v_nom_archivo = 'prueba'
   
   CLOSE WINDOW SCREEN 
   OPEN WINDOW extractor WITH FORM "CBDL411"
       MENU 
            BEFORE MENU 
                LET v_fecha=TODAY -1 
                DISPLAY v_fecha TO fecha
                DISPLAY "before",v_fecha
            ON ACTION Generar

               # se valida si se puede generar el proceso
               CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
               IF ( r_resultado_opera <> 0 ) THEN
                  DISPLAY "No paso la operacion"
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
               ELSE
                  # se genera el pid para el proceso
                  CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
                         RETURNING v_pid

                  CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                                         "CBDP41",v_nom_archivo,v_usuario)
                                                RETURNING r_resultado_opera
                  IF ( r_resultado_opera <> 0 ) THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                  ELSE
                     # Inicia operación
                     CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDP41",
                                           v_nom_archivo,v_usuario) RETURNING r_resultado_opera
                     # En el caso de que exista una inconsistencia al iniciar el proceso, se
                     # Muestra un mensaje con la descripcion
                     IF(r_resultado_opera)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                     ELSE
                        LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP41.42r ",
                                                         v_usuario," ",
                                                         v_pid," ",
                                                         v_proceso_cod," ",
                                                         v_opera_cod," ",v_fecha," ",
                                                         "1>", v_ruta_listados CLIPPED ,
                                                         "/nohup:",v_pid USING "&&&&&",":",
                                                                  v_proceso_cod USING "&&&&&",":",
                                                                  v_opera_cod USING "&&&&&" ," 2>&1 &"

                        DISPLAY v_comando 
                        DISPLAY "v_fecha",v_fecha
                        RUN v_comando
                        IF(STATUS)THEN
                           DISPLAY "Extractor de cuantas sin saldo, "
                           DISPLAY "Ocurrió un error al iniciar el proceso batch"
                        ELSE
                           # Se indica que se realizo el proceso de carga
                           DISPLAY "Extractor de cuantas sin saldo, "
                           CALL fn_mensaje("Error","Se ha iniciado el proceso batch. Podrá revisar el detalle en el monitor de procesos para el pid " ||v_pid,"about")
                           DISPLAY "Se ha iniciado el proceso batch. Podrá revisar el detalle en el monitor de procesos para el pid " ||v_pid
                           EXIT PROGRAM 
                        END IF
                     END IF
                  END IF
               END IF
            ON ACTION CLOSE 
                EXIT MENU 
        END MENU 
   CLOSE WINDOW  extractor
END MAIN