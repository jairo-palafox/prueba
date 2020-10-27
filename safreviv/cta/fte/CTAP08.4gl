#Proyecto          => SAFRE VIVIENDA                                            #
#Propietario       => E.F.P.                                                    #
#-------------------------------------------------------------------------------#
#Modulo            => CTA                                                       #
#Programa          => CTAP08                                                    #
#Objetivo          => PROGRAMA QUE FINALIZA LA PREPARACION DE LA TABLA DE SALDOS# 
#                     PARA EL ARCHIVO DEL AREA DE PRECALIFICACION               #
#Fecha Inicio      => 30-SEPTIEMBRE-2013                                        #
#################################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados


MAIN
   DEFINE r_resultado_opera               INTEGER
   #DEFINE v_estado_ant                    SMALLINT
   DEFINE v_fn_genera_saldo_preca         STRING

   DEFINE v_resultado                     SMALLINT
   #DEFINE v_mensaje_respuesta             VARCHAR(100)

   #DEFINE v_consulta_anterior             STRING

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)


   WHENEVER ERROR CONTINUE

	CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso  

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
      
   
   #Se finaliza la tabla de saldos para precalificacion
   DISPLAY "Finalizando el proceso que genera los saldos para precalificacion..."
   WHENEVER ERROR CONTINUE
      LET v_fn_genera_saldo_preca = "EXECUTE FUNCTION fn_finaliza_tabla_preca()"
      PREPARE exe_fn_genera_saldo_preca FROM v_fn_genera_saldo_preca
      EXECUTE exe_fn_genera_saldo_preca INTO v_resultado
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al finalizar la tabla para el saldo de precalificacion: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         CALL fn_actualiza_error()
         RETURN
      END IF
   WHENEVER ERROR STOP
   
   IF v_resultado = 1 THEN
      # Proceso correcto
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF

      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY " La tabla del saldo se genero correctamente"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_genera_archivo()
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Ocurrio un ERROR al intentar crear la tabla de saldos"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      CALL fn_actualiza_error()
   END IF
END MAIN

PRIVATE FUNCTION fn_actualiza_error()
   --Se establece la fecha de corte como el dia natural inmediato anterior
   LET v_fcorte = TODAY - 1;
   
   UPDATE safre_sdo@vivws_tcp:glo_ctr_saldo SET estado_genera = 3 
   WHERE tpo_saldo = 3
   AND f_saldo = v_fcorte;
END FUNCTION

PRIVATE FUNCTION fn_genera_archivo()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   
   
   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)

   DEFINE v_comando           STRING

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cta'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   LET v_proceso_cod = 704
   LET v_opera_cod = 1
   LET v_folio = 1
   LET v_nom_archivo = 'prueba'

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      DISPLAY "No fue posible ejecutar de forma automatica la generacion del archivo"
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "CTAP05",v_nom_archivo,p_usuario_cod)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CTAP05",
                               v_nom_archivo,p_usuario_cod) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CTAP05.42r ",
                                             p_usuario_cod," ",
                                             v_pid," ",
                                             v_proceso_cod," ",
                                             v_opera_cod," ",
                                             v_folio," '",
                                             v_nom_archivo,
                                             "' 1>", v_ruta_listados CLIPPED ,
                                             "/nohup:",v_pid USING "&&&&&",":",
                                                      v_proceso_cod USING "&&&&&",":",
                                                      v_opera_cod USING "&&&&&" ," 2>&1 &"

            #DISPLAY v_comando                        
            RUN v_comando
            IF(STATUS)THEN
               DISPLAY "No fue posible ejecutar de forma automatica la generacion del archivo"
            ELSE
               # Se indica que se realizo el proceso de carga
               DISPLAY "Se ha iniciado el proceso batch que genera el archivo para precalificacion..."
               DISPLAY "Podrá revisar el detalle en el monitor de procesos con el pid " ||v_pid
            END IF
         END IF
      END IF
   END IF
END FUNCTION