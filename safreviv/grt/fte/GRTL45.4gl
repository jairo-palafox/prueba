--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Modulo            => GRT                                           #
#Programa          => GRTL45                                        #
#Objetivo          => Programa que ejecuta la generación de marcas  #
#                     de Créditos en Garantía 43 bis                #
#Autor             => auro Muñiz Caballero                          #
#Fecha inicio      => 8 de noviembre de 2016                        #
#####################################################################

DATABASE safre_viv

MAIN

   DEFINE p_v_nom_prog               VARCHAR(30) -- nombre del programa
   DEFINE p_b_tipo_carga             SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_usuario                LIKE seg_usuario.usuario -- usuario firmado al sistema
   DEFINE v_i_proceso_cod            LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod              LIKE cat_operacion.opera_cod -- operación que llama la funcion
   DEFINE v_d_pid                    LIKE bat_ctr_proceso.pid -- identificador del proceso
   DEFINE v_c_programa_cod           LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_v_nom_archivo            LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_d_folio                  LIKE glo_ctr_archivo.folio -- folio
   DEFINE v_c_fecha_hoy              CHAR(8) -- fecha de hoy con formato YYYYMMDD
   DEFINE v_c_ruta_list_bat          LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_i_cont_regs              INTEGER -- contador de registro
   DEFINE v_t_cont_regs              INTEGER -- contador de registro
   DEFINE v_s_mensaje                STRING -- contiene mensaje a mostrar a usuario
   DEFINE v_s_comando                STRING -- contiene al comando a correr
   DEFINE v_c_extension              LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_i_lote                   LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_c_lote                   CHAR(1) -- lote del archivo
   DEFINE v_s_qryTxt                 STRING -- guarda una sentencia sql a ejecutar
   DEFINE r_c_ruta_bin               LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados          LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida                 SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_tabla                    CHAR(20)
   DEFINE v_mensaje_final            STRING 

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTL45.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   CLOSE WINDOW SCREEN

   -- se inicializan las variables
   LET v_i_proceso_cod  = 1229
   LET v_i_opera_cod    = 1 -- generación de marca
   LET v_d_pid          = 0
   LET v_c_programa_cod = "GRTL45"
   LET v_d_folio        = 0
   LET v_i_cont_regs    = 0
   LET v_t_cont_regs    = 0

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

display "primera validación operación"

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fecha_hoy = TODAY USING "YYYYMMDD"

   -- se abre el menu de la opcion
   OPEN WINDOW w_sol_marca WITH FORM "GRTL451"
      --se muestran las fechas en la ventana
      DISPLAY TODAY TO f_generacion

      MENU
        COMMAND "Aceptar"
           -- se crea la sentencia sql que ejecuta la funcion que genera el pid
           LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

display "pid: ",v_d_pid, " proceso: ", v_i_proceso_cod, " operacion: ",v_i_opera_cod,
"folio: ",v_d_folio," programa: ",v_c_programa_cod," archivo: ",v_v_nom_archivo," usuario: ", p_v_usuario

           -- se invoca la funcion que inicializa el proceso
           LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                  v_d_folio, v_c_programa_cod,
                                                  v_v_nom_archivo, p_v_usuario)
display ""
display "r_b_valida inicializa ", r_b_valida

           -- se verifica si fue posible inicializar el proceso
           IF r_b_valida <> 0 THEN
              -- en caso de error se muestra un mensaje a usuario y no continua
              CALL fn_muestra_inc_operacion(r_b_valida)
              EXIT PROGRAM
           END IF

              -- se crea el comando que ejecuta el modulo que genera el archivo de salida de cargo a capital
              LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTP30 ",
                                                      p_v_usuario, " ",
                                                      v_d_pid, " ",
                                                      v_i_proceso_cod, " ",
                                                      v_i_opera_cod, " ",
                                                      v_d_folio, " ",
                                                      v_v_nom_archivo, " 1> ",
                                                      v_c_ruta_list_bat CLIPPED,
                                                      "/nohup:",v_d_pid USING "&&&&&",":",
                                                      v_i_proceso_cod USING "&&&&&",":",
                                                      v_i_opera_cod USING "&&&&&",
                                                      " 2>&1 &"

display v_s_comando

              -- se ejecuta el comando armado
              RUN v_s_comando
              
              LET v_mensaje_final = "Se ejecutó el proceso de generación de marcas con el PID : "||v_d_pid
              
              -- se informa al usuario de la ejecucion del proceso
              CALL fn_mensaje("Aviso",v_mensaje_final,"information")

           EXIT MENU

         COMMAND "Cancelar"
            EXIT MENU
      END MENU

   CLOSE WINDOW w_sol_marca

END MAIN
