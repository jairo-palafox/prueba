###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGL20                                                  #
#Objetivo          => Lanzador para la generación del archivo de saldos de    #
#                     créditos vigentes Apoyo Infonavit.                      # 
#Autor             => Emilio Abarca Sánchez, EFP                              #
#Fecha inicio      => 04/Abril/2017                                           #
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
    
END GLOBALS 

MAIN 

   LET g_usuario       = ARG_VAL (1)
   LET g_tpo_ejecucion = ARG_val (2)
   LET g_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 3922
   LET g_opera_cod     = 1

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CALL STARTLOG(g_usuario CLIPPED || ".OCGL20.log")

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CLOSE WINDOW SCREEN 

   CALL arh_saldos_apoyo()
   
END MAIN 

FUNCTION arh_saldos_apoyo()

   DEFINE v_f_ejecucion       DATE
   DEFINE v_respuesta         BOOLEAN
   DEFINE v_pid               DECIMAL(9,0)
   DEFINE v_valida_operacion  SMALLINT
   DEFINE v_s_comando         STRING
   DEFINE v_notifica          STRING 

OPEN WINDOW vtn1 WITH FORM "OCGL201"

   LET v_f_ejecucion = TODAY
   
   MENU ""
      BEFORE MENU 
         DISPLAY v_f_ejecucion USING "dd/mm/yyyy" TO f_ejecucion

      ON ACTION ACCEPT
         LET v_respuesta = fn_ventana_confirma("","¿Está seguro de generar el archivo de saldos de créditos vigentes apoyo infonavit?","")

         IF(v_respuesta = 0) THEN
            CALL fn_mensaje("","Se ha cancelado la operación","")
            EXIT MENU
         ELSE
            -- Genera PID
            LET v_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

            -- Valida operación
            CALL fn_valida_operacion(v_pid,g_proceso_cod,g_opera_cod) RETURNING v_valida_operacion

            IF(v_valida_operacion <> 0) THEN
                  CALL fn_muestra_inc_operacion(v_valida_operacion)
                  DISPLAY "ERROR en la validación de la operación"
            ELSE 
               -- Inicializa proceso
               CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,"","OCGP21","",g_usuario) RETURNING v_valida_operacion

               -- Actualiza operación como inicio
               CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod,"","OCGP21","",g_usuario) RETURNING v_valida_operacion

               -- Se ejecuta el lanzado
               LET v_s_comando = " nohup fglrun ",
                                   v_ruta_ejecutable CLIPPED,
                                   "/OCGP21 ",
                                   g_usuario," ",
                                   v_pid," ",
                                   g_proceso_cod," ",
                                   g_opera_cod," ",
                                   " ' ' 1>",v_ruta_listados CLIPPED,
                                   "/nohup:",v_pid USING "&&&&&",":",
                                   g_proceso_cod   USING "&&&&&",":",
                                   g_opera_cod     USING "&&&&&" ," 2>&1 &"
                                   
               DISPLAY v_s_comando
               RUN v_s_comando

               LET v_notifica = "Se ejecutó la generación del archivo de saldos apoyo infonavit"," \n",
                                 "Verificar en el monitor de procesos el PID: ",v_pid USING "<<<<<<<<<"

               CALL fn_mensaje("",v_notifica,"")
               EXIT MENU 
               
            END IF    
         END IF 

      ON ACTION CANCEL 
         EXIT MENU
         
   END MENU
   
CLOSE WINDOW vtn1

END FUNCTION 
