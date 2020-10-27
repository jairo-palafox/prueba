###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CBDL03                                                  #
#Objetivo          => EXTRACCION DE SALDOS NEGATIVOS                          #
#Fecha Inicio      => 05-SEPTIEMBRE-2013                                      #
###############################################################################
DATABASE safre_viv

#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)

PRIVATE DEFINE v_fcorte             DATE
PRIVATE DEFINE v_hoy                DATE

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

MAIN
    DEFINE v_bandera         SMALLINT

    LET p_usuario            = ARG_VAL(1)
    LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
    LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa

    CLOSE WINDOW SCREEN

    CALL ui.Interface.setText(p_nombre_menu)
   
    OPEN WINDOW cbdl031 WITH FORM "CBDL031"

    LET ventana = ui.Window.forName("cbdl031")
    LET forma = ventana.getform()

    CALL fn_captura_parametros() RETURNING v_bandera 
    
    CLOSE WINDOW cbdl031

END MAIN

PRIVATE FUNCTION fn_captura_parametros()
   LET v_hoy = TODAY
   LET v_fcorte = v_hoy - 1

   INPUT v_fcorte FROM fcorte ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS) 
      ON ACTION accept
         IF v_fcorte IS NULL THEN
            CALL fn_mensaje("Extracción de saldos negativos",
                            "La fecha de corte no puede ser nula.",
                            "about")
         ELSE
            CALL fn_genera_archivo()
            LET int_flag = FALSE
            EXIT INPUT
         END IF
            
      ON ACTION cancel
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT
   
   RETURN int_flag
END FUNCTION

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

   LET v_proceso_cod = 2104
   LET v_opera_cod = 1
   LET v_folio = 0

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
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

      #Se asigna el nombre del archivo
      LET v_nom_archivo = "negativos"
      
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                 "CBDS05",v_nom_archivo,p_usuario)
                        RETURNING r_resultado_opera

      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,null,"CTAP01",
                               v_nom_archivo,p_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS05.42r ",
                                             p_usuario CLIPPED," ",
                                             v_pid CLIPPED," ",
                                             v_proceso_cod CLIPPED," ",
                                             v_opera_cod CLIPPED," ",
                                             v_folio," '",
                                             v_nom_archivo CLIPPED,"' ",
                                             v_fcorte, " ",
                                             "1>", v_ruta_listados CLIPPED ,
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