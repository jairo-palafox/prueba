#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRL54                                        #
#Objetivo          => (Homologación) Lanzador para la ejecución de  #
#                      cancelacion y desmarca por vencimiento       #
#Autor             => Hector Fabián Jiménez Lara                    #
#Fecha inicio      => 15 de Julio de 2015                           #
#####################################################################
DATABASE safre_viv

GLOBALS

   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE g_usuario                CHAR (20)
   DEFINE r_bandera                SMALLINT
   DEFINE p_tpo_ejecucion          SMALLINT
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE r_b_valida               SMALLINT
   DEFINE v_pid                    DECIMAL(9,0)
   DEFINE v_s_comando              STRING
   DEFINE v_ruta_ejecutable        LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_nss                    CHAR(11)
   DEFINE v_archivo                STRING
   DEFINE v_entrada                STRING
   DEFINE w ui.Window
   DEFINE f ui.Form

END GLOBALS

MAIN

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_s_titulo      = ARG_VAL (3)
   LET g_proceso_cod   = 335  -- numero de proceso correspondiente
   LET g_opera_cod     = 1    -- numero de operacion correspondiente

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   OPEN WINDOW w_ejecuta WITH FORM "AGRL541"
      MENU
         ON ACTION Aceptar
            CALL fn_ejecuta_lanzado()
         ON ACTION CANCEL
            EXIT MENU
      END MENU
   CLOSE WINDOW w_ejecuta
END MAIN

-- Función que se encarga de ejecutar el programa lanzado
FUNCTION fn_ejecuta_lanzado()
   -- se invoca la funcion que valida la operación
   CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN  
     -- en caso de error se muestra un mensaje a usuario y no continua
     CALL fn_muestra_inc_operacion(r_b_valida)
     DISPLAY "ERROR en fn_valida_operacion"
   ELSE
      --se obtiene la ruta binario
      LET v_s_comando = " SELECT ruta_bin
                            FROM seg_modulo
                           WHERE modulo_cod = 'agr' "

      PREPARE prp_bin FROM v_s_comando
      EXECUTE prp_bin INTO v_ruta_ejecutable

      -- Se obtiene la ruta envio
      LET v_s_comando = " SELECT ruta_envio
                            FROM seg_modulo
                           WHERE modulo_cod = 'agr' "

      PREPARE prp_envio FROM v_s_comando
      EXECUTE prp_envio INTO v_ruta_envio

      -- Se obtiene la ruta listados
      LET v_s_comando = " SELECT ruta_listados
                            FROM seg_modulo
                           WHERE modulo_cod = 'bat' "

      PREPARE prp_lst FROM v_s_comando
      EXECUTE prp_lst INTO v_ruta_listados

      -- Se invoca a la función que genera el PID
      CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,
                                 "","AGRP35","",g_usuario)  RETURNING r_b_valida

      CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                                  g_opera_cod,"",
                                  "AGRP35","",
                                  g_usuario)  RETURNING r_b_valida

      LET v_s_comando = "nohup fglrun ",
                         v_ruta_ejecutable CLIPPED,
                        "/AGRP35"    ," ",
                        g_usuario    ," ",
                        v_pid        ," ",
                        g_proceso_cod," ",
                        g_opera_cod  ," '",
                        " ' '  1>",
                        v_ruta_listados CLIPPED ,
                        "/nohup:",
                        v_pid         USING "&&&&&",":",
                        g_proceso_cod USING "&&&&&",":",
                        g_opera_cod   USING "&&&&&" ," 2>&1 &"

      RUN v_s_comando

      DISPLAY "v_s_comando", v_s_comando

      LET v_s_comando = "Se ejecutó la cancelacion y desmarca por vencimiento"," ",
                        "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
                        
      CALL fn_mensaje("Alerta",v_s_comando,"information")
   END IF
 END FUNCTION  
