DATABASE safre_viv
MAIN
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio
   DEFINE v_usuario           VARCHAR(30) -- Almacena al usuario

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log
   
   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)

   DEFINE v_pago_listo        SMALLINT

   DEFINE v_comando           STRING

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'ret'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   LET v_proceso_cod = 1523
   LET v_opera_cod = 1
   LET v_usuario = 'safreviv'
   LET v_folio = 1
   LET v_nom_archivo = 'prueba'

   #Primero se valida que exista un numero de pago listo para enviar
   SELECT num_pago 
   INTO v_pago_listo
   FROM ret_ctr_pago_masivo
   where estado_pago = 1

   IF v_pago_listo IS NULL OR v_pago_listo = 0 THEN
      DISPLAY "********************************************************************"
      DISPLAY "No se puede generar un nuevo pago masivo si antes no se corre el proceso que reevalua los candidatos al programa previamente rechazados"
      DISPLAY "para poder continuar con la generacion del pago es necesario que se ejecute el proceso de reevaluacion"
      DISPLAY "********************************************************************"
      RETURN
   ELSE
      DISPLAY "********************************************************************"
      DISPLAY "El proximo pago listo para ser generado es el numero ", v_pago_listo
      DISPLAY "********************************************************************"
   END IF

   
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
                                             "RETP02",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"RETP02",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/RETP02.42r ",
                                             v_usuario," ",
                                             v_pid," ",
                                             v_proceso_cod," ",
                                             v_opera_cod," ",
                                             v_folio," '",
                                             v_nom_archivo,
                                             "' 1>", v_ruta_listados CLIPPED ,
                                             "/nohup:",v_pid USING "&&&&&",":",
                                                      v_proceso_cod USING "&&&&&",":",
                                                      v_opera_cod USING "&&&&&" ," 2>&1 &"

            DISPLAY v_comando                        
            RUN v_comando
            IF(STATUS)THEN
               DISPLAY "Ocurrió un error al iniciar el proceso batch"
            ELSE
               # Se indica que se realizo el proceso de carga
               DISPLAY "Se ha iniciado el proceso batch... \nPodrá revisar el detalle en el monitoreo de procesos para el pid " ||v_pid
            END IF
         END IF
      END IF
   END IF
END MAIN
