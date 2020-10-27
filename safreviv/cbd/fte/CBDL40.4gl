################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 27/06/2016                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#Desarrollador     => González Bautista Jorge                                  #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL40                                                   #
#Objetivo          => Programa de consulta de movimientos adelantados y        #
#     					 generacion de archivos de detalle de adelantos GRT    #
#Fecha inicio      => 16/07/2014                                               #
################################################################################
DATABASE safre_viv


PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE v_f_corte            DATE
PRIVATE DEFINE v_ind_adelanto		SMALLINT


DEFINE f_fecha                      DATE  

MAIN
   DEFINE v_valida_generacion       SMALLINT
   -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF
   
   CLOSE WINDOW SCREEN 
   OPEN WINDOW vtn_cbdl161 WITH FORM "CBDL401"

    INPUT  BY NAME f_fecha ATTRIBUTE(UNBUFFERED, ACCEPT=0, cancel=0)
        BEFORE INPUT 
            LET f_fecha=TODAY 
        ON ACTION Aceptar
            IF f_fecha IS NOT NULL THEN 
                CALL fn_genera_archivo()
                EXIT PROGRAM 
            ELSE
                CALL fn_mensaje ("Generar Archivo Adelantos GRT", "Es necesario ingresar una fecha" , "info") 
            END IF 
        ON ACTION Cancelar 
            EXIT PROGRAM 
    END INPUT 
         
            #CALL fn_mensaje ("Generar Archivo Adelantos", "Se canceló la generación del archivo con el detalle de adelantos" , "info")
            
   CLOSE WINDOW vtn_cbdl161
END MAIN



PRIVATE FUNCTION fn_genera_archivo()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_comando           STRING

   LET v_proceso_cod = 2119
   LET v_opera_cod = 1

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

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "CBDS15",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
          # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDS15",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            #Se asigna el nombre del archivo
            LET v_nom_archivo = "Adelantos_",v_f_corte USING "ddmmyyyy"
            

            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS15.42r ",
                                             v_usuario," ",
                                             v_pid," ",
                                             v_proceso_cod," ",
                                             v_opera_cod," ",
                                             v_folio," '",
                                             v_nom_archivo,"', ",
                                             f_fecha,
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
            END IF
         END IF
      END IF
   END IF
END FUNCTION
