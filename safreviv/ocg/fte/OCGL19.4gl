###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGL19                                                  #
#Objetivo          => Lanzador para la generación del archivo de desmarcas    # 
#                     43Bis.                                                  # 
#Autor             => Emilio Abarca Sánchez, EFP                              #
#Fecha inicio      => 08 de Febrero del 2017                                  #
###############################################################################

DATABASE safre_viv

GLOBALS

    DEFINE g_usuario           CHAR (20)
    DEFINE g_tpo_ejecucion     SMALLINT
    DEFINE g_nom_ventana       STRING
    DEFINE g_proceso_cod       INTEGER
    DEFINE g_opera_cod         INTEGER
    DEFINE v_ruta_ejecutable   CHAR(40)
    DEFINE v_ruta_listados     CHAR(40)
    DEFINE v_fecha_desde       DATE
    DEFINE v_fecha_hasta       DATE
    DEFINE v_fecha_actual      DATE
    DEFINE v_pid               DECIMAL(9,0)
    DEFINE r_b_valida          SMALLINT
    DEFINE v_s_comando         STRING 
    DEFINE v_notifica          STRING 
    
END GLOBALS 

MAIN 

   LET g_usuario       = ARG_VAL (1)
   LET g_tpo_ejecucion = ARG_val (2)
   LET g_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 3920
   LET g_opera_cod     = 1

   -- Se asigna el título de la ventana
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CALL STARTLOG(g_usuario CLIPPED || ".OCGL19.log")

   SELECT ruta_bin
      INTO v_ruta_ejecutable
      FROM seg_modulo
     WHERE modulo_cod = 'ocg'

   SELECT ruta_listados
      INTO v_ruta_listados
      FROM seg_modulo
     WHERE modulo_cod = 'bat'

   CLOSE WINDOW SCREEN 
   
   OPEN WINDOW vnt1 WITH FORM "OCGL191"
   
      LET v_fecha_desde  = NULL
      LET v_fecha_hasta  = TODAY -1
      LET v_fecha_Actual = TODAY    

      MENU ""
         BEFORE MENU 
         -- Recupera la última fecha de ejecución del archivo
          SELECT FIRST 1 f_proceso
             INTO v_fecha_desde
             FROM ocg_arh_rec_desmarcas
             ORDER BY f_proceso DESC

         DISPLAY v_fecha_desde USING "dd/mm/yyyy" TO f_desde
         DISPLAY v_fecha_hasta USING "dd/mm/yyyy" TO f_hasta
         DISPLAY v_fecha_actual USING "dd/mm/yyyy" TO f_actual

         ON ACTION ACCEPT
            IF (v_fecha_desde = TODAY) THEN 
               CALL fn_mensaje("","El archivo de desmarcas ya fue generado por el día de hoy","")
               EXIT MENU 
            ELSE 
               -- Se genera el PID
               LET v_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)
               
               -- Se invoca la función que valida la operación.
               CALL fn_valida_operacion(v_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

               IF(r_b_valida <> 0) THEN
                  CALL fn_muestra_inc_operacion(r_b_valida)
                  DISPLAY "ERROR en la validación de la operación"
               ELSE 
                  -- Inicializa proceso
                  CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,"","OCGS06","",g_usuario) RETURNING r_b_valida

                  -- Actualiza Operacion
                  CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod,"","OCGS06","",g_usuario) RETURNING r_b_valida

                  -- Se ejecuta el lanzado
                  LET v_s_comando = " nohup fglrun ",
                                     v_ruta_ejecutable CLIPPED,
                                     "/OCGS06 ",
                                     g_usuario," ",
                                     v_pid," ",
                                     " ' ' 1>",v_ruta_listados CLIPPED,
                                     "/nohup:",v_pid USING "&&&&&",":",
                                     g_proceso_cod   USING "&&&&&",":",
                                     g_opera_cod     USING "&&&&&" ," 2>&1 &"

                  RUN v_s_comando

                  DISPLAY "Ejecución del lanzado: ",v_s_comando

                  LET v_notifica = "Se ejecutó la generación del archivo de desmarcas"," \n",
                                   "Verificar en el monitor de procesos el PID: ",v_pid USING "<<<<<<<<<"

                  CALL fn_mensaje("",v_notifica,"")
                  EXIT MENU 
                  
               END IF 
            END IF
            
         ON ACTION CANCEL
            EXIT MENU 
            
      END MENU 
   CLOSE WINDOW vnt1
   
END MAIN 