################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 27/01/2014                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CTA                                                      #
#Programa          => CTAL10                                                   #
#Objetivo          =>  Lanzador del programa que genera el estado de cuenta    #
#                      masivo
#Fecha inicio      => 27/01/2014                                               #
################################################################################

DATABASE safre_viv

#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)

PRIVATE DEFINE v_finicio            DATE
PRIVATE DEFINE v_ffin               DATE
PRIVATE DEFINE v_fproceso           DATE

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
   
    OPEN WINDOW ctal101 WITH FORM "CTAL101"

    LET ventana = ui.Window.forName("ctal101")
    LET forma = ventana.getform()

    CALL fn_captura_parametros() RETURNING v_bandera 
    
    CLOSE WINDOW ctal101

END MAIN

PRIVATE FUNCTION fn_captura_parametros()
   DEFINE v_dummy    STRING
   
   #Primero se valida si se puede ejecutar la generacion de saldos
   LET v_fproceso = TODAY
   --Se establece la fecha de corte como el ultimo dia natural del mes non inmediato anterior
   IF MONTH(v_fproceso) MOD 2 = 0 THEN
      #En este caso nos encontramos en mes par, por lo que obtenemos el ultimo dia del mes anterior
      LET v_ffin = MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1
   ELSE
      #En este caso nos encontramos en un mes non por lo que aun no termina el periodo en curso, 
      #se calcula la fecha fin como el ultimo dia del mes non anterior
      LET v_ffin = MDY(MONTH(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1),1,YEAR(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1)) - 1
   END IF
   #Se establece la fecha de inicio como el primer dia del mes anterior a la fecha fin
   LET v_finicio = MDY(MONTH(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1),1,YEAR(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1))

   LET v_dummy = '1'
   INPUT v_dummy FROM dummy ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT
         DISPLAY v_finicio          TO finicio
         DISPLAY v_ffin             TO ffin
      ON ACTION ACCEPT
         IF v_dummy IS NULL OR v_dummy = '0' THEN
            LET int_flag = TRUE
            EXIT INPUT
         ELSE
            CALL fn_genera_archivo()
            LET int_flag = FALSE
            EXIT INPUT
         END IF
            
      ON ACTION CANCEL
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

   DEFINE v_folio_ant         DECIMAL (9,0)

   LET v_proceso_cod = 717
   LET v_opera_cod = 1
   LET v_folio = 0

   #Primero se valida que el proceso aun NO se alla ejecutado
   SELECT folio
   INTO v_folio_ant
   FROM cta_ctr_edo_cuenta
   WHERE f_fin = v_ffin
   AND estado <> 9

   LET v_comando = v_ffin USING 'dd-mm-yyyy'

   IF v_folio_ant IS NOT NULL THEN
      CALL fn_mensaje("Estado de cuenta masivo", "El estado de cuenta con corte " || 
                      v_comando || "\nya fue generado con folio " || 
                      v_folio_ant, "bn_about")
      RETURN;
   END IF

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
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

      #Se asigna el nombre del archivo
      LET v_nom_archivo = "ESTADO_DE_CUENTA"
      
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                 "CTAP10",v_nom_archivo,p_usuario)
                        RETURNING r_resultado_opera

      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,null,"CTAP10",
                               v_nom_archivo,p_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CTAP10.42r ",
                                             p_usuario," ",
                                             v_pid," ",
                                             v_proceso_cod," ",
                                             v_opera_cod," ",
                                             v_folio," '",
                                             v_nom_archivo CLIPPED,
                                             "' 1>", v_ruta_listados CLIPPED ,
                                             "/nohup:",v_pid USING "&&&&&",":",
                                                      v_proceso_cod USING "&&&&&",":",
                                                      v_opera_cod USING "&&&&&" ," 2>&1 &"

            DISPLAY v_comando                        
            RUN v_comando
            IF(STATUS)THEN
               CALL fn_mensaje("Estado de cuenta masivo", 
                               "Ocurrió un error al iniciar el proceso batch",
                               "bn_about")
            ELSE
               # Se indica que se realizo el proceso de carga
               CALL fn_mensaje("Estado de cuenta masivo", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                               "bn_about")
            END IF
         END IF
      END IF
   END IF
   
END FUNCTION