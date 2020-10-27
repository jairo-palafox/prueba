####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL07                                        #
#Objetivo          =>Programa lanzador que permite la validación   #
#                    del archivo de Devolución de Solicitudes del  #
#                    módulo de Anualidades Garantizadas            #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>09 Abril 2012                                 #
#Modificación      =>Daniel Buendia, EFP                           #
#Fecha             =>11 Abril 2012                                 #
#                  =>Ya no se invocan las funciones de genera pid  #
#                    e inicializa proceso, estos ya los invoca la  #
#                    función general de carga cuando el proceso    #
#                    inicia con la carga                           #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

GLOBALS
DEFINE   g_proceso_cod LIKE bat_ctr_operacion.proceso_cod,
         g_opera_cod   LIKE bat_ctr_operacion.opera_cod,
         g_s_ruta_log  STRING
END GLOBALS

#Objetivo:
#  Menu principal de la Integración de Devluciones del modulo de Transferencia de aceditados.
#  Permite la selección de archivo a integrar, muestra la información previa y ejecuta la integración
MAIN
   DEFINE p_v_nom_prog      VARCHAR(30), -- nombre del programa
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          v_d_pid           DECIMAL(9,0), -- identificador del proceso
          v_v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_d_folio         LIKE bat_ctr_proceso.folio, -- folio del proceso
          v_c_ruta_bin      LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_b_continua      SMALLINT, -- booleana que indica si se debe continuar con el proceso o no
          v_s_mensaje       STRING, -- mensaje a mostrar al usuario
          v_s_comando       STRING, -- contiene al comando a correr
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL07.log")

   -- se asigna el titulo a la aplicacion
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se obtienen las rutas de control del modulo
   SELECT ruta_bin
     INTO v_c_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   -- se obtienen la ruta de listados para el modulo bat
   SELECT ruta_listados
     INTO v_c_ruta_list_bat
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- se inicializan variables del modulo
   LET v_d_pid          = 0 -- aun no se tiene PID
   LET g_proceso_cod    = g_proc_cod_agr_devol_solic -- recepción devolución solicitudes ag
   LET g_opera_cod      = 1 -- valida devolución solicitudes
   LET v_d_folio        = 0 -- para la carga no se envia folio
   LET p_b_tipo_carga   = 2
   LET v_v_nom_archivo  = NULL
   LET v_c_programa_cod = "AGRL07"

   DISPLAY "p_v_usuario: "   , p_v_usuario
   DISPLAY "p_b_tipo_carga: ", p_b_tipo_carga
   DISPLAY "p_v_nom_prog: "  , p_v_nom_prog

   -- se invoca la funcion que valida la siguiente operacion
   CALL fn_valida_operacion(v_d_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- si la operacion es valida se ejecuta
   IF ( r_b_valida = 0 ) THEN
      -- se obtiene el ID del proceso
      --LET v_d_pid = fn_genera_pid(g_proceso_cod, g_opera_cod, p_v_usuario)
      --DISPLAY "PID: ", v_d_pid

      -- se inicia el proceso
      --CALL fn_inicializa_proceso(v_d_pid, g_proceso_cod, g_opera_cod, v_d_folio,
      --                           v_c_programa_cod, "NA", p_v_usuario) RETURNING v_b_continua
       
      --IF v_b_continua = 0 THEN
         -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
         LET v_v_nom_archivo = "NA"

         -- se crea el comando que ejecuta el modulo que reliza la actualizacion de la tabla de control de archivo
         --LET v_s_comando = "nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRP07 ",
         LET v_s_comando = "fglrun ",v_c_ruta_bin CLIPPED,"/AGRP07 ",
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

         -- Se invoca la funcion que valida y carga la informacion en la tabla temporal
         CALL fn_carga_archivo(v_d_pid, g_proceso_cod, g_opera_cod, p_b_tipo_carga, v_c_programa_cod, v_s_comando, p_v_usuario, TRUE) RETURNING v_b_continua

         IF v_b_continua THEN
            -- se asigna el mensaje a mostrar al usuario
            --LET v_s_mensaje = "Se ha enviado la carga con PID: ",v_d_pid CLIPPED,
            --                  ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
            --CALL fn_mensaje("Valida",v_s_mensaje,"information")
         END IF
      --ELSE 
      --   -- en caso de error se muestra un mensaje a usuario
      --   CALL fn_muestra_inc_operacion(v_b_continua)
      --END IF
   ELSE
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)
   END IF

END MAIN