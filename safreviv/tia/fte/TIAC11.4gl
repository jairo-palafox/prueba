--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => TIA                                                     #
#Programa          => TIAC11                                                  #
#Objetivo          => CONSULTA DE MOVIMIENTOS EN DECRETO POR CONSISTENTES E   #
#                     INCONSISTENTES AGRUPADOS POR EDAD                       #
#Fecha Inicio      => 26/03/2012                                              #
###############################################################################
DATABASE safre_viv

DEFINE --g_pid           LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod   LIKE cat_proceso.proceso_cod, # Código del proceso
       g_opera_cod     LIKE cat_operacion.opera_cod,  # Código de operacion
       g_ruta_bin      LIKE seg_modulo.ruta_bin,
       g_ruta_listados LIKE seg_modulo.ruta_listados

GLOBALS "TIAG01.4gl"

MAIN
   DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, # Clave del usuario
          p_tipo_ejecucion   SMALLINT,                     # Forma como ejecutara el programa
          p_titulo           STRING,                       # Titulo de la ventana
          v_fecha_inicio     DATE, -- fecha maxima de consulta
          v_fecha_fin        DATE, -- fecha maxima de consulta
          v_comando          STRING,
          r_resultado_opera  SMALLINT,
          r_confirma         SMALLINT,
          r_pid              LIKE bat_ctr_proceso.pid,
          v_folio            LIKE glo_folio.folio
       

   # Se recupera la clave de usuario desde parámetro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET g_proceso_cod = g_proceso_cod_reportes_decreto
   LET g_opera_cod = g_opera_cod_tia_genera_reportes_decreto

   -- se obtienen las rutas
   SELECT ruta_bin
   INTO   g_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "tia"

   SELECT ruta_listados
   INTO   g_ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "bat"
   

   -- se abre la ventana de captura de datos
   OPEN WINDOW w_consulta WITH FORM g_ruta_bin CLIPPED||"/TIAC111"

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se inician las fechas en blanco
   LET v_fecha_inicio = "10/01/2012"
   LET v_fecha_fin    = TODAY

   -- se captura la fecha de inicio y la fecha final de consulta
   INPUT BY NAME v_fecha_inicio, v_fecha_fin WITHOUT DEFAULTS ATTRIBUTES ( UNBUFFERED )

      ON ACTION CANCEL
         EXIT INPUT
   
      ON ACTION ACCEPT
         -- ninguna de las fechas puede ser posterior a la fecha actual
         IF ( v_fecha_inicio > TODAY ) THEN
        
            CALL fgl_winmessage("Atención","La fecha de inicio no puede ser posterior a la fecha actual","stop")
            CONTINUE INPUT
         END IF
   
         IF ( v_fecha_fin > TODAY ) THEN
            CALL fgl_winmessage("Atención","La fecha de término no puede ser posterior a la fecha actual","stop")
            CONTINUE INPUT
         END IF
   
         -- la fecha final no puede ser mayor a la inicia
         IF ( v_fecha_inicio > v_fecha_fin ) THEN
            CALL fgl_winmessage("Atención","La fecha de inicio no puede ser posterior a la final","stop")
            CONTINUE INPUT
         END IF

         -- se deben capturar ambas fechas
         IF ( v_fecha_fin IS NULL OR v_fecha_inicio IS NULL ) THEN
            CALL fgl_winmessage("Atención","Es necesario capturar ambas fechas","stop")
            CONTINUE INPUT
         END IF

         CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
         IF(r_resultado_opera = 0)THEN
            CALL fn_ventana_confirma(p_titulo,"¿Generar reporte?","question")RETURNING r_confirma
            IF(r_confirma)THEN
               CALL fn_genera_pid(g_proceso_cod,g_opera_cod,p_usuario_cod)RETURNING r_pid
               CALL fn_inicializa_proceso(r_pid,
                                          g_proceso_cod,
                                          g_opera_cod,
                                          v_folio,
                                          "TIAC11",
                                          "NA",
                                          p_usuario_cod)
               RETURNING r_resultado_opera
               IF( r_resultado_opera <> 0 )THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE INPUT
               END IF
               CALL fn_actualiza_opera_ini(r_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           v_folio,
                                           "TIAC11",
                                           "NA",
                                           p_usuario_cod)
               RETURNING r_resultado_opera
               IF( r_resultado_opera <> 0 )THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE INPUT
               END IF
               -- se ejecuta el programa que genera los reportes
               --LET v_comando = "nohup time fglrun ",g_ruta_bin CLIPPED,"/TIAS11.42r ",
               LET v_comando = "nohup fglrun ",g_ruta_bin CLIPPED,"/TIAS11.42r ",p_usuario_cod, " ", # Usuario
                                                                                 r_pid, " ",         # PID
                                                                                 g_proceso_cod, " ", # Proceso
                                                                                 g_opera_cod, " ",   # Operación
                                                                                 "0 '",              # Folio, se crea en lanzado
                                                                                 "NA' ",             # Archivo
                                                                                 " '", v_fecha_inicio, "' ", # Parámetro adicional 1, fecha inicio
                                                                                 " '", v_fecha_fin, "' ",    # Parámetro adicional 2, fecha fin
                               " 1>", g_ruta_listados CLIPPED ,"/nohup:",r_pid USING "&&&&&",":",
                                                                         g_proceso_cod USING "&&&&&",":",
                                                                         g_opera_cod USING "&&&&&" ,
                               " 2>&1 &"

               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_titulo,"Ocurrio un error al ejecutar la operación","about")
               ELSE
                  CALL fn_mensaje(p_titulo,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
               END IF
               EXIT INPUT
   
            ELSE 
               CONTINUE INPUT
            END IF   
         ELSE
            CALL fn_muestra_inc_operacion(r_resultado_opera)
            CONTINUE INPUT
         END IF
   
   
   END INPUT
   
   -- se cierra la ventana
   CLOSE WINDOW w_consulta

   
END MAIN