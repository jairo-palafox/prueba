####################################################################################
#Módulo            => AGR                                                          #
#Programa          => AGRS69                                                       #
#Objetivo          => Programa lanzador para el proceso Generación del archivo     #
#                     de solicitudes UA histórico, el cual contendrá el detalle de #
#                     los registros que Cartera (SAS) solicitó para pagos UA (AGR) #
#Autor             => Emilio Abarca, EFP / Mauro Muñiz Caballero                   #
#Fecha inicio      => 3/julio/2018                                                 #
####################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario                 CHAR(20)
   DEFINE g_titulo                  STRING
   DEFINE g_tipo_ejecucion          SMALLINT
   DEFINE cbox                      DECIMAL(9,0)
   DEFINE g_proceso_cod             SMALLINT
   DEFINE g_opera_cod               SMALLINT
   DEFINE g_pid                     DECIMAL(9,0)
   DEFINE v_f_proceso               DATE
   DEFINE v_nom_archivo             CHAR(40)
   DEFINE v_bnd                     SMALLINT 
   DEFINE v_mensaje                 STRING 
   DEFINE v_s_comando               STRING
   DEFINE v_ruta_ejecutable         CHAR(40)
   DEFINE v_ruta_listados           CHAR(40)

END GLOBALS 

MAIN

   LET g_usuario        = ARG_VAL  (1)
   LET g_tipo_ejecucion = ARG_VAL  (2)
   LET g_titulo         = ARG_VAL  (3)
   LET g_proceso_cod    = 347 -- GENERACIÓN HISTÓRICO ARCHIVO SOLICITUDES CARTERA
   LET g_opera_cod      = 1

    -- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL69.log")

   CLOSE WINDOW SCREEN

   CALL fn_principal()

END MAIN 

FUNCTION fn_principal()  

   OPEN WINDOW vtn WITH FORM "AGRL691"

   MENU
      BEFORE MENU
         LET v_f_proceso = TODAY

         DISPLAY v_f_proceso USING "DD-MM-YYYY" TO v_f_generacion

         ON ACTION ACCEPT 

            -- Genera PID
            LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

            -- Inicializa proceso
            CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRS15","",g_usuario) RETURNING v_bnd

             -- Actualiza operación como iniciada
            CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRS15",v_nom_archivo,g_usuario) RETURNING v_bnd

             -- Ejecuta lanzado
            LET v_s_comando = "nohup fglrun ",
                               v_ruta_ejecutable CLIPPED,
                               "/AGRS15"," ",
                               g_usuario," ",
                               g_pid," ",
                               g_proceso_cod," ",
                               g_opera_cod," ",
                               " ' ' 1>",v_ruta_listados CLIPPED,
                               "/nohup:",g_pid USING "&&&&&",":",
                               g_proceso_cod   USING "&&&&&",":",
                               g_opera_cod     USING "&&&&&" ," 2>&1 &"

            RUN v_s_comando

            LET v_mensaje = "Se ejecutó la generación del histórico de solicitudes UA (Cartera)"," \n",
                            "Consultar ejecución en el monitor de procesos, PID: ", g_pid

            CALL fn_mensaje("",v_mensaje,"")

            EXIT MENU

      ON ACTION CANCEL
         EXIT MENU

   END MENU

   CLOSE WINDOW vtn

END FUNCTION
