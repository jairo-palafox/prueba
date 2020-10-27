#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIL22                                                                 #
#Objetivo     => Programa que lanza extractor de notificaciones activas                 #
#Fecha inicio => MARZO   2015                                                           #
#########################################################################################
DATABASE safre_viv

DEFINE g_usuario_cod       CHAR(20)
DEFINE g_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
DEFINE g_titulo            STRING -- titulo de la ventana

MAIN
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   -- se inicia el log del programa
   CALL STARTLOG (g_usuario_cod CLIPPED||".AFIL22.log")

   CALL fn_extractor()
END MAIN

FUNCTION fn_extractor()
   DEFINE v_f_inicial               DATE
   DEFINE v_f_final                 DATE
   DEFINE v_indicador               SMALLINT
   
   DEFINE v_proceso_cod_rch         SMALLINT
   DEFINE v_opera_cod_rch           SMALLINT
   DEFINE v_pid_rch                 DECIMAL(9,0)
   DEFINE v_programa                STRING
   DEFINE v_comando                 STRING
   DEFINE v_mensaje                 STRING
   DEFINE v_ruta_bin                CHAR(40)
   DEFINE v_ruta_bat                CHAR(40)

   DEFINE v_rec_combo               RECORD
      codigo                           SMALLINT,
      descripcion                      CHAR(30)
   END RECORD
   DEFINE cb                        ui.ComboBox
   DEFINE v_resultado               SMALLINT

   LET v_proceso_cod_rch = 1817
   LET v_opera_cod_rch   = 1
   LET v_programa        = 'AFIP22'
   LET v_f_inicial       = TODAY
   LET v_f_final         = TODAY
   LET v_indicador       = NULL
   
   OPEN WINDOW afil221 WITH FORM "AFIL221"

   LET cb = ui.ComboBox.forName("v_indicador")

   DECLARE cur_combo CURSOR FOR SELECT tpo_notificacion, 
                                       desc_notificacion
                                  FROM cat_afi_tpo_notifica
   FOREACH cur_combo INTO v_rec_combo.*
      CALL cb.addItem(v_rec_combo.codigo, v_rec_combo.descripcion)
   END FOREACH
   CALL cb.addItem(NULL,"TODOS")

   INPUT BY NAME v_f_inicial, v_f_final, v_indicador ATTRIBUTES (UNBUFFERED,
                                                                 WITHOUT DEFAULTS)
      ON ACTION ACCEPT
         IF v_f_inicial IS NULL OR 
            v_f_final   IS NULL THEN
            CALL fn_mensaje("Extractor", "Las fechas no pueden ser nulas","")
            NEXT FIELD v_f_inicial
         ELSE
            ACCEPT INPUT
         END IF
      ON ACTION CANCEL
         EXIT INPUT
      ON ACTION CLOSE
         EXIT INPUT
   END INPUT

   IF NOT INT_FLAG THEN
      CALL fn_valida_operacion(0,v_proceso_cod_rch,v_opera_cod_rch) RETURNING v_resultado

      IF v_resultado = 0 THEN
         CALL fn_genera_pid(v_proceso_cod_rch,v_opera_cod_rch,g_usuario_cod) RETURNING v_pid_rch
         CALL fn_inicializa_proceso(v_pid_rch,v_proceso_cod_rch,v_opera_cod_rch,0,
                                                v_programa,'',g_usuario_cod)
                                       RETURNING v_resultado

         IF v_resultado = 0 THEN
            CALL fn_actualiza_opera_ini(v_pid_rch,
                                        v_proceso_cod_rch,
                                        v_opera_cod_rch,
                                        0,
                                        v_programa,
                                        '',
                                        g_usuario_cod)
                              RETURNING v_resultado

            IF v_resultado = 0 THEN
               SELECT s.ruta_bin
                 INTO v_ruta_bin
                 FROM seg_modulo s
                WHERE s.modulo_cod = 'afi'

               SELECT b.ruta_listados
                 INTO v_ruta_bat
                 FROM seg_modulo b
                WHERE b.modulo_cod = 'bat'
               
               LET v_comando = " nohup time fglrun ",v_ruta_bin CLIPPED,"/AFIP22 ",
                               g_usuario_cod CLIPPED, " ",
                               v_pid_rch            , " ",
                               v_proceso_cod_rch    , " ",
                               v_opera_cod_rch      , " 0 ' ' ",
                               " '",v_f_inicial,"' ",
                               " '",v_f_final,"' ",
                               " '",v_indicador,"' ",
                               " 1>",v_ruta_bat CLIPPED,
                               "/nohup:",v_pid_rch USING "&&&&&",":",
                               v_proceso_cod_rch   USING "&&&&&",":",
                               v_opera_cod_rch     USING "&&&&&" ,
                               " 2>&1 &"
               RUN v_comando
               LET v_mensaje = "Se ha enviado la generación del extractos de indicadores. \n ",
                               "podrá revisar el resultado en el monitor de procesos pid: ", v_pid_rch
               CALL fn_mensaje ("Extractor", v_mensaje, "")
               
            ELSE
               CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
               CALL fn_mensaje ("Extractor", v_mensaje, "")
            END IF
         ELSE
            CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
            CALL fn_mensaje ("Extractor", v_mensaje, "")
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         CALL fn_mensaje ("Extractor", v_mensaje, "")
      END IF
   END IF
   CLOSE WINDOW afil221
END FUNCTION