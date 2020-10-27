#################################################################################
# Modulo       => AGR                                                           #
# Programa     => AGRL53                                                        #
# Objetivo     => Programa que lanza al extractor de intentos y reintentos      #
#                   de los web services de marca y desmarca                     #
# Autor        => Héctor Jiménez                                                #
# Fecha        => 08/Julio/2015                                                 #
#################################################################################
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
   DEFINE w                        ui.Window
   DEFINE f                        ui.Form
   DEFINE v_s_comando              STRING
   DEFINE v_s_qry                  STRING
END GLOBALS

MAIN
   DEFINE v_fec_inicio         DATE
   DEFINE v_fec_fin            DATE
   DEFINE v_fec_max_his        DATE
   DEFINE v_fec_max            DATE 

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)
   LET g_proceso_cod   = 336  -- código de proceso
   LET g_opera_cod     = 1    -- numero de operacion correspondiente

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   -- Se obtiene la ruta de binarios
   LET v_s_comando = " SELECT ruta_bin
                         FROM seg_modulo
                        WHERE modulo_cod = 'agr' "

   PREPARE prp_r_bin FROM v_s_comando
   EXECUTE prp_r_bin INTO v_ruta_ejecutable

   -- Se obtiene la ruta listados
   LET v_s_comando = " SELECT ruta_listados
                         FROM seg_modulo
                        WHERE modulo_cod = 'bat' "

   PREPARE prp_r_lst FROM v_s_comando
   EXECUTE prp_r_lst INTO v_ruta_listados

   -- Se invoca la función que genera el pid
   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid

   OPEN WINDOW w_fec WITH FORM "AGRL53"
      INPUT BY NAME v_fec_inicio,v_fec_fin ATTRIBUTES(UNBUFFERED)
         ON ACTION ACCEPT
          -- Si cualquiera de las fechas viene nula, se les asigna la fecha de hoy
          IF v_fec_fin IS NULL THEN
             LET v_fec_fin = TODAY
          END IF

          IF v_fec_inicio IS NULL THEN
          --LET v_fec_inicio = TODAY--
          -- Se toma la fecha mán antigua
          LET v_s_qry = " SELECT ch.f_a   , cm.f_b
                            FROM (SELECT MIN(f_solicita) f_a FROM cta_his_marca_ws) ch,
                                 (SELECT MIN(f_solicita) f_b FROM cta_marca_ws) cm "

          PREPARE prp_f_max FROM v_s_qry
          EXECUTE prp_f_max INTO v_fec_max_his,
                                 v_fec_max

          DISPLAY "Fecha fin historica : ",v_fec_max_his
          DISPLAY "Fecha fin           : ",v_fec_max
          -- Se valida cual es la fecha más antigua
          IF v_fec_max < v_fec_max_his THEN
             LET v_fec_fin = v_fec_max
          ELSE
             -- Se asigna la fecha de la historica que sería la más antingua
             LET v_fec_fin = v_fec_max_his
          END IF
       END IF
            DISPLAY "Fecha inicio : ",v_fec_inicio
            DISPLAY "Fecha Fin    : ",v_fec_fin
            --DISPLAY "PID          : " ,v_pid


           -- Inicializa proceso
           CALL fn_inicializa_proceso( v_pid,
                                       g_proceso_cod,
                                       g_opera_cod,
                                       "",
                                       "AGRS09",
                                       "",
                                       g_usuario ) RETURNING r_b_valida

            LET v_s_comando = "fglrun ",
                              v_ruta_ejecutable CLIPPED ,
                              "/AGRS09 ",
                              g_usuario     , " ",
                              v_pid         , " ",
                              g_proceso_cod , " ",
                              g_opera_cod   , " ",
                              "'",v_fec_inicio,"'", " ",
                              "'",v_fec_fin,"'", " ",
                              " ' '  1>",
                              v_ruta_listados CLIPPED ,
                              "/nohup:",
                              v_pid         USING "&&&&&",":",
                              g_proceso_cod USING "&&&&&",":",
                              g_opera_cod   USING "&&&&&" ," 2>&1 &"

            RUN v_s_comando

            DISPLAY v_s_comando

            -- Se arma el mensaje a desplegar posterior a la ejecución
            LET v_s_comando = "Se ejecuto la generación del archivo con extracción de rechazos\n",
                              " de marca y desmarca con Procesar "

            CALL fn_mensaje("Información",v_s_comando,"information")

            EXIT INPUT 
      END INPUT
   CLOSE WINDOW w_fec
END MAIN
