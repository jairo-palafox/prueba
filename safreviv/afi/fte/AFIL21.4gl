#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIL21                                                                 #
#Objetivo     => Programa que lanza archivo salida rechazos de notificaciones           #
#Fecha inicio => FEBRERO 2015                                                           #
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
   CALL STARTLOG (g_usuario_cod CLIPPED||".AFIL21.log")

   CALL fn_genera_rechazos()
END MAIN

FUNCTION fn_genera_rechazos()
   DEFINE v_proceso_cod_rch         SMALLINT
   DEFINE v_opera_cod_rch           SMALLINT
   DEFINE v_pid_rch                 DECIMAL(9,0)
   DEFINE v_programa                STRING
   DEFINE v_comando                 STRING
   DEFINE v_mensaje                 STRING
   DEFINE v_ruta_bin                CHAR(40)
   DEFINE v_ruta_bat                CHAR(40)

   DEFINE v_resultado               SMALLINT
   DEFINE i                         SMALLINT

   DEFINE v_arr_rechazos            DYNAMIC ARRAY OF RECORD
      folio_lote                       DECIMAL(9,0),
      desc_notificacion                CHAR(40),
      indicador                        SMALLINT,
      desc_indicador                   CHAR(40),
      cod_excepcion                    INTEGER,
      des_excepcion                    CHAR(40),
      tot_rechazos                     INTEGER
   END RECORD

   LET v_proceso_cod_rch = 1816
   LET v_opera_cod_rch   = 1
   LET v_programa = 'AFIP21'

   OPEN WINDOW afil211 WITH FORM "AFIL211"
   
   DECLARE cur_rechazos CURSOR FOR SELECT a.folio_lote,
                                          b.desc_notificacion,
                                          a.indicador,
                                          a.cod_excepcion,
                                          COUNT(*)
                                     FROM afi_rch_ind_not a,
                                          cat_afi_tpo_notifica b
                                    WHERE b.tpo_notificacion = a.tpo_notificacion
                                    GROUP BY 1,2,3,4

   LET i = 1
   FOREACH cur_rechazos INTO  v_arr_rechazos[i].folio_lote,
                              v_arr_rechazos[i].desc_notificacion,
                              v_arr_rechazos[i].indicador,
                              v_arr_rechazos[i].cod_excepcion,
                              v_arr_rechazos[i].tot_rechazos
      CASE v_arr_rechazos[i].indicador
         WHEN 0 
            LET v_arr_rechazos[i].desc_indicador = "DESMARCA DE ENVIO DE NOTIFICACIONES "
         WHEN 1 
            LET v_arr_rechazos[i].desc_indicador = "MARCA DE ENVIO DE NOTIFICACIONES "
      END CASE


      CASE v_arr_rechazos[i].cod_excepcion
         WHEN 0
            LET v_arr_rechazos[i].des_excepcion = "ACEPTADOS"
         WHEN 1
            LET v_arr_rechazos[i].des_excepcion = "EL DERECHOHABIENTE NO EXISTE"
         WHEN 2
            LET v_arr_rechazos[i].des_excepcion = "EL TIPO DE NOTIFICACIÓN NO EXISTE"
         WHEN 3
            LET v_arr_rechazos[i].des_excepcion = "EL IDENTIFICADOR O MARCA ES INCORRECTO"
         WHEN 90
            LET v_arr_rechazos[i].des_excepcion = "YA EXISTE LA MARCA DE NOTIFICACIÓN"
         WHEN 99
            LET v_arr_rechazos[i].des_excepcion = "NO CONTABA CON LA MARCA DE NOTIFICACIÓN"
      END CASE

      LET i = i + 1
   END FOREACH

   DISPLAY ARRAY v_arr_rechazos TO rec1.*
      ON ACTION ACCEPT
         ACCEPT DISPLAY
      ON ACTION CANCEL
         EXIT DISPLAY
      ON ACTION CLOSE
         EXIT DISPLAY
   END DISPLAY

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
               
               LET v_comando = " nohup time fglrun ",v_ruta_bin CLIPPED,"/AFIP21 ",
                               g_usuario_cod CLIPPED, " ",
                               v_pid_rch            , " ",
                               v_proceso_cod_rch    , " ",
                               v_opera_cod_rch      , " 0 ' ' ",
                               " 1>",v_ruta_bat CLIPPED,
                               "/nohup:",v_pid_rch USING "&&&&&",":",
                               v_proceso_cod_rch   USING "&&&&&",":",
                               v_opera_cod_rch     USING "&&&&&" ,
                               " 2>&1 &"
               RUN v_comando
               LET v_mensaje = "Se ha enviado la generación del archivo de rechazos. \n ",
                               "podrá revisar el resultado en el monitor de procesos pid: ", v_pid_rch
               CALL fn_mensaje ("Rechazos datos de contato", v_mensaje, "")
               
            ELSE
               CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
               CALL fn_mensaje ("Rechazos datos de contato", v_mensaje, "")
            END IF
         ELSE
            CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
            CALL fn_mensaje ("Rechazos datos de contato", v_mensaje, "")
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         CALL fn_mensaje ("Rechazos datos de contato", v_mensaje, "")
      END IF
   END IF
   CLOSE WINDOW afil211
END FUNCTION