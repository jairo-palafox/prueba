--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

######################"###############################################
#Módulo            => GRT                                            #
#Programa          => GRTL46                                         #
#Objetivo          => Programa lanzador de proceso de recuperación   #
#                     de solicitudes de uso de garantías del sistema #
#                     administración de créditos 43 Bis              #
#Autor             => Mauro Muñiz Caballero, EFP                     #
#Fecha inicio      => 9 de enero de 2017                             #
#Modifica          => Edgar Damian Estrada Rivera                    #
#Fecha Mod.        => 25 de Agosto de 2017                           #
######################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

#Objetivo:
# Se valida la operación y se ejecuta el proceso de recuperación
# de solicitudes de uso de garantía de SAC43BIS

MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario  -- usuario firmado al sistema
   DEFINE p_b_tipo_carga            SMALLINT  -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog              VARCHAR(30)  -- nombre del programa
   DEFINE v_d_pid                   DECIMAL(9,0)  -- identificador del proceso
   DEFINE v_i_proceso_cod           LIKE cat_proceso.proceso_cod  -- proceso que llama las funciones
   DEFINE v_i_opera_cod             LIKE cat_operacion.opera_cod  -- operación que llama la función
   DEFINE v_v_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo  -- nombre del archivo en proceso
   DEFINE v_d_folio                 LIKE bat_ctr_proceso.folio  -- folio del proceso
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin  -- ruta bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados  -- ruta listados del módulo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados  -- ruta listados de bat
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod  -- nombre del programa
   DEFINE v_b_continua              SMALLINT  -- booleana que indica si se debe continuar con el proceso o no
   DEFINE v_s_comando               STRING  -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING  -- guarda una sentencia SQL a ejecutar
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_c_fecha_hoy             CHAR(8) -- fecha de hoy con formato YYYYMMDD
   DEFINE v_mensaje_final           STRING 

   -- se asignan los parámetros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL46.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF
   
   CLOSE WINDOW SCREEN

   -- se inicializan variables del modulo
   LET v_i_proceso_cod  = 1231 --11 -- recepción uso de garantía 43bis
   LET v_i_opera_cod    = 1 -- valida archivo solicitud uso 43bis
   LET v_d_pid          = 0
   LET v_c_programa_cod = "GRTL46"
   LET v_d_folio        = 0
   LET p_b_tipo_carga   = 2
   LET v_v_nom_archivo  = NULL
   
   DISPLAY "p_v_usuario: ",p_v_usuario
   DISPLAY "p_b_tipo_carga: ",p_b_tipo_carga
   DISPLAY "p_v_nom_prog: ",p_v_nom_prog

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, 
                                  r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,
                            v_i_proceso_cod, 
                            v_i_opera_cod)
      RETURNING r_b_valida
   DISPLAY "valida operación: ",r_b_valida

   -- si la operacion es valida se ejecuta
   IF r_b_valida <> 0 THEN
       -- en caso de error, se muestra un mensaje al usuario y no continua
       CALL fn_muestra_inc_operacion(r_b_valida)
       EXIT PROGRAM
   END IF 
       
   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension (v_i_proceso_cod,
                                               v_i_opera_cod)

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fecha_hoy = TODAY USING "YYYYMMDD"

   -- se abre el menu de la opción
  OPEN WINDOW w_uso_gtia WITH FORM "GRTL461" 
    
   --se muestran las fechas en la ventana
      DISPLAY TODAY TO f_generacion

      MENU
        COMMAND "Aceptar"

   -- se crea la sentencia sql que ejecuta la función que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, 
                               v_i_opera_cod, 
                               p_v_usuario)

   -- se invoca la función que inicializa el proceso
    LET r_b_valida = fn_inicializa_proceso(v_d_pid, 
                                           v_i_proceso_cod, 
                                           v_i_opera_cod,
                                           v_d_folio, 
                                           v_c_programa_cod,
                                           v_v_nom_archivo, 
                                           p_v_usuario)
                               
   Display "r_b_valida inicializa ", r_b_valida
   
      -- se verifica si fue posible inicializar el proceso
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje al usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
         EXIT PROGRAM
      END IF

        DISPLAY "Se ejecuta recuperación de solicitudes de uso de garantía"

          -- se crea el comando que ejecuta el módulo que reliza la obtención de los registros con solicitud
          LET v_s_comando = "nohup time fglrun ",r_c_ruta_bin CLIPPED ,    "/GRTP31 ",
                            p_v_usuario                               ,    " "       ,
                            v_d_pid                                   ,    " "       ,
                            v_i_proceso_cod                           ,    " "       ,
                            v_i_opera_cod                             ,    " "       ,
                            v_d_folio                                 ,    " "       ,
                            v_v_nom_archivo                           ,    " 1>> "   ,
                            v_c_ruta_list_bat CLIPPED                 ,
                            "/nohup:",v_d_pid USING                   "&&&&&",": "   ,
                            v_i_proceso_cod USING "&&&&&"             ,":"           ,
                            v_i_opera_cod USING "&&&&&"               ,
                            " 2>&1 &"

          DISPLAY v_s_comando

          RUN v_s_comando

          LET v_mensaje_final = "Se ejecutó el Proceso de Solicitud Uso Garantia 43bis con el PID : "||v_d_pid

          -- se informa al usuario la ejecución del proceso.
          CALL fn_mensaje("Aviso",v_mensaje_final,"information")

      EXIT MENU

      COMMAND "cancelar"
         EXIT MENU
      END MENU

  CLOSE WINDOW w_uso_gtia

END MAIN