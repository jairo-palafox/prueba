--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTL21                                        #
#Objetivo          =>Programa lanzador que invoca la función que   #
#                    valida la información de Devolución de Solic. #
#                    para el módulo de Uso de Garantía 43 bis      #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>26 Abril 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS
DEFINE   g_proceso_cod LIKE bat_ctr_operacion.proceso_cod,
         g_opera_cod   LIKE bat_ctr_operacion.opera_cod,
         g_s_ruta_log  STRING
END GLOBALS

#Objetivo:
#  Invoca la función que permite la validación y carga del archivo de Devolución de
#  Solicitudes para el módulo de Uso de Anualidad 43 bis
MAIN
   DEFINE p_v_nom_prog      VARCHAR(30), -- nombre del programa
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          v_d_pid           DECIMAL(9,0), -- identificador del proceso
          v_v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_d_folio         LIKE bat_ctr_proceso.folio, -- folio del proceso
          r_c_ruta_bin      LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_b_continua      SMALLINT, -- booleana que indica si se debe continuar con el proceso o no
          v_s_comando       STRING, -- contiene al comando a correr
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL21.log")

   -- se asigna el titulo a la aplicacion
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   SELECT ruta_listados
     INTO v_c_ruta_list_bat
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- se inicializan variables del modulo
   LET v_d_pid          = 0 -- aun no se tiene PID
   LET g_proceso_cod    = g_proc_cod_grt_uso_devol_solic -- recepción devol solicitudes uso 43bis
   LET g_opera_cod      = 1 -- valida devolución solicitudes
   LET v_d_folio        = 0 -- para la carga no se envia folio
   LET p_b_tipo_carga   = 2
   LET v_v_nom_archivo  = NULL
   LET v_c_programa_cod = "GRTL21"

   DISPLAY "p_v_usuario: "   , p_v_usuario
   DISPLAY "p_b_tipo_carga: ", p_b_tipo_carga
   DISPLAY "p_v_nom_prog: "  , p_v_nom_prog

   -- se invoca la funcion que valida la siguiente operacion
   CALL fn_valida_operacion(v_d_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- si la operacion es valida se ejecuta
   IF ( r_b_valida = 0 ) THEN
      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET v_v_nom_archivo = "NA"

      -- se crea el comando que ejecuta el modulo que reliza la actualizacion de la tabla de control de archivo
      --LET v_s_comando = "nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTP14 ",
      LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP14 ",
                        p_v_usuario CLIPPED, " ",
                        v_d_pid, " ",
                        g_proceso_cod, " ",
                        g_opera_cod, " ",
                        v_d_folio, " ",
                        v_v_nom_archivo CLIPPED
                        --, " 1>>",
                        --v_c_ruta_list_bat CLIPPED,
                        --"/nohup:",v_d_pid USING "&&&&&",":",
                        --g_proceso_cod USING "&&&&&",":",
                        --g_opera_cod USING "&&&&&",
                        --" 2>&1"

      --DISPLAY "comand: ",v_s_comando

      -- Se invoca la funcion que valida y carga la informacion en la tabla de paso
      CALL fn_carga_archivo(v_d_pid, g_proceso_cod, g_opera_cod, p_b_tipo_carga,
                            v_c_programa_cod, v_s_comando, p_v_usuario, TRUE) RETURNING v_b_continua
   ELSE
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)
   END IF

END MAIN