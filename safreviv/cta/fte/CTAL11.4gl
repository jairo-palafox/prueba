#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#Modulo            => CTA                                                     #
#Programa          => CTAP11                                                  #
#Objetivo          => PROGRAMA PARA GENERAR INFORMACION ESTADISTA PARA DWH    #
#Fecha Inicio      => DICIEMBRE-2016                                          #
###############################################################################
DATABASE safre_viv


DEFINE p_usuario            CHAR(10)
DEFINE p_tipo_proc          CHAR(1)
DEFINE p_nombre_menu        CHAR(50)

DEFINE p_proceso_cod        SMALLINT
DEFINE p_opera_cod          SMALLINT

MAIN
    DEFINE v_bandera         SMALLINT

    LET p_usuario            = ARG_VAL(1)
    LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
    LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa

    LET p_proceso_cod = 720
    LET p_opera_cod = 1

    CLOSE WINDOW SCREEN

    CALL ui.Interface.setText(p_nombre_menu)
    OPEN WINDOW ctal111 WITH FORM "CTAL111"

    CALL fn_lanza_proceso() RETURNING v_bandera 
    CLOSE WINDOW ctal111
END MAIN

FUNCTION fn_lanza_proceso()
   DEFINE v_fecha_actual                  DATE
   DEFINE v_f_ejecuta                     DATE
   DEFINE v_mes                           SMALLINT
   DEFINE v_proceso_desc             CHAR(40)
   DEFINE v_opera_desc               CHAR(40)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   #Encabezado para el archivo de monitoreo
   LET v_fecha_actual = TODAY

   #Validar que se ejecuta posterior al bimestral, mes par
   LET v_mes = MONTH(v_fecha_actual)

   IF (v_mes MOD 2) > 0 THEN
      LET v_fecha_actual = TODAY - 1 UNITS MONTH
      LET v_mes = MONTH(v_fecha_actual)
   END IF

   IF (v_mes MOD 2) > 0 THEN
      CALL fn_mensaje ("Extractor DWH",
                        "El proceso no se puede ejecutar se encuentra fuera del mes valido",
                        "")
   ELSE
      MENU ""
         BEFORE MENU
         LET v_f_ejecuta = (MDY (v_mes,1,YEAR(v_fecha_actual)) )- (1 UNITS DAY)
         DISPLAY BY NAME v_f_ejecuta, v_proceso_desc, v_opera_desc

         ON ACTION ACCEPT
            IF NOT fn_valida_ejecuta(v_f_ejecuta) THEN
               CALL fn_ejecuta_extractor()
            END IF
            EXIT MENU

         ON ACTION CANCEL
            LET int_flag = TRUE
            EXIT MENU
      END MENU
   END IF
   RETURN 0
END FUNCTION

FUNCTION fn_valida_ejecuta (p_f_ejecuta)
   DEFINE p_f_ejecuta         DATE
   DEFINE v_f_valida          DATE
   DEFINE r_bandera           SMALLINT

   LET r_bandera = 0
   SELECT f_ejecuta
     INTO v_f_valida
     FROM safre_sdo@vivws_tcp:glo_ctr_dwh
    WHERE f_corte = p_f_ejecuta

   IF v_f_valida IS NOT NULL AND
      v_f_valida <> '12/31/1899' THEN
      CALL fn_mensaje("",
                     "La extracción de información del bimestre ya fue lanzado.",
                     "")
      LET r_bandera = 1
   END IF
   RETURN r_bandera
END FUNCTION


FUNCTION fn_ejecuta_extractor()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_comando           STRING

   LET v_nom_archivo = ""
   LET v_folio = 0

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
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

      # se genera el pid para el proceso
      CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,p_proceso_cod,p_opera_cod,0,
                                 "CTAP11",v_nom_archivo,p_usuario)
                        RETURNING r_resultado_opera

      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         CALL fn_actualiza_opera_ini(v_pid,p_proceso_cod,p_opera_cod,null,"CTAP11",
                               v_nom_archivo,p_usuario) RETURNING r_resultado_opera
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CTAP11.42r ",
                                             p_usuario," ",
                                             v_pid," ",
                                             p_proceso_cod," ",
                                             p_opera_cod," '",
                                             v_folio,"' '",
                                             v_nom_archivo CLIPPED,
                                             "' 1>", v_ruta_listados CLIPPED ,
                                             "/nohup:",v_pid USING "&&&&&",":",
                                                      p_proceso_cod USING "&&&&&",":",
                                                      p_opera_cod USING "&&&&&" ," 2>&1 &"
            RUN v_comando
            IF(STATUS)THEN
               CALL fn_mensaje("Extractor", 
                               "Ocurrió un error al iniciar el proceso batch",
                               "bn_about")
            ELSE
               # Se indica que se realizo el proceso de carga
               CALL fn_mensaje("Extractor DWH", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                               "")
            END IF
         END IF
      END IF
   END IF
END FUNCTION