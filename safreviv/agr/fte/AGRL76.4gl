#########################################################################
#Modulo            => AGR                                               #
#Programa          => AGRL76                                            #
#Objetivo          => Programa lanzador que genera reporte de adelantos #
#                     vs Operación 09 diferente.                        #
#Autor             => Emilio Abarca, EFP                                #
#Fecha inicio      => 07/Noviembre/2018                                 #
#########################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0)

   --variables del proceso
   DEFINE v_bnd           SMALLINT
   DEFINE v_comando       STRING
   DEFINE v_mensaje       STRING

END GLOBALS

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 352   -- ADELANTOS VS OP.09
   LET g_opera_cod        = 1     -- GENERA EXTRACTOR

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL76.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL reporte_act_marca()
   
END MAIN

FUNCTION reporte_act_marca()

   DEFINE v_fecha     DATE
   DEFINE v_confirma  SMALLINT

   OPEN WINDOW vtn1 WITH FORM "AGRL761"

      LET v_fecha = TODAY 
      
      MENU ""
         BEFORE MENU  
            DISPLAY BY NAME v_fecha
            
         ON ACTION ACCEPT
            CALL fn_ventana_confirma("Alerta","¿Está seguro de generar el reporte?","stop") RETURNING v_confirma
            IF(v_confirma = 1) THEN
               #Inicializa el proceso
               -- Genera PID
               LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

               -- Inicializa proceso
               CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL76","",g_usuario) RETURNING v_bnd

               IF(v_bnd <> 0) THEN
                  -- Si no se puede ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_bnd)
                  EXIT MENU
               END IF

               -- Actualiza operación como iniciada
               CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL76","",g_usuario) RETURNING v_bnd

               IF(v_bnd <> 0) THEN
                  -- Si no se peude ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_bnd)
                  EXIT MENU
               END IF

               -- Ejecuta lanzado
               LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP55 ",
                                               g_usuario," ",
                                               g_pid," ",
                                               g_proceso_cod," ",
                                               g_opera_cod," ",
                                               v_fecha," ",
                                               " ' ' 1>",
                                               v_ruta_listados CLIPPED,
                                               "/nohup:",g_pid USING "&&&&&",":",
                                               g_proceso_cod   USING "&&&&&",":",
                                               g_opera_cod     USING "&&&&&" ," 2>&1 &"

               RUN v_comando

               LET v_mensaje = "Se ejecutó la generación de reporte de Adelantos vs Op.09"," \n",
                               "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

               CALL fn_mensaje("",v_mensaje,"")
               EXIT MENU
            ELSE 
               CALL fn_mensaje("","Se ha cancelado la operación","")
               EXIT MENU 
            END IF 
         
         ON ACTION CANCEL
            EXIT MENU 
            
      END MENU

   CLOSE WINDOW vtn1

END FUNCTION
