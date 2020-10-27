####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL05                                        #
#Objetivo          =>Programa que permite la validación del archi- #
#                    vo de saldos para el módulo de Anualidades G. #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>04 Abril 2012                                 #
#Modificación      =>Daniel Buendia, EFP                           #
#Fecha             =>11 Abril 2012                                 #
#                  =>Ya no se invocan las funciones de genera pid  #
#                    e inicializa proceso, estos ya los invoca la  #
#                    función general de carga cuando el proceso    #
#                    inicia con la carga                           #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

#Objetivo:
#  Menu principal de Archivos entrada Saldos del modulo de Transferencia de aceditados.
#  Permite validad la informacion del archivo 
MAIN
   DEFINE p_v_nom_prog      VARCHAR(30), -- nombre del programa
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          v_d_pid           DECIMAL(9,0), -- identificador del proceso
          v_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
          v_d_folio         LIKE bat_ctr_proceso.folio, -- folio del proceso
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          v_b_continua      SMALLINT, -- booleana que indica si se debe continuar con el proceso o no
          v_s_comando       STRING, -- contiene al comando a correr
          v_s_mensaje       STRING, -- mensaje a mostrar al usuario
          v_c_ruta_bin      LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL05.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_rutaBin FROM v_s_qryTxt
   EXECUTE prp_slc_rutaBin INTO v_c_ruta_bin

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   -- se inicializan variables del modulo
   LET v_d_pid = 0
   LET v_i_proceso_cod = g_proc_cod_agr_sdos_transf -- recepción saldos transferidos ag
   LET v_i_opera_cod = 1 -- valida archivo de saldos transferidos
   LET v_d_folio = 0 -- para la carga no se envia folio
   LET p_b_tipo_carga = 2
   LET v_v_nom_archivo = NULL
   LET v_c_programa_cod = "AGRL05"

   -- se invoca la funcion que valida la siguiente operacion
   CALL fn_valida_operacion(0,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- si la operacion es valida se ejecuta
   IF r_b_valida = 0 THEN
      -- se crea la sentencia sql que ejecuta la funcion que genera el pid
      --LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

      -- se invoca la funcion que inicializa el proceso
      --LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
      --                                       v_d_folio, v_c_programa_cod,
      --                                       v_v_nom_archivo, p_v_usuario)

      -- en caso de error se muestra un mensaje a usuario
      --IF r_b_valida <> 0 THEN
      --   DISPLAY "ENTRA A VALIDA SALDOS"
      --   CALL fn_muestra_inc_operacion(r_b_valida)
      --ELSE 
         DISPLAY " NO ENTRA A VALIDA SALDOS"
         -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
         LET v_v_nom_archivo = "N/A"

         -- se crea el comando que ejecuta el modulo que reliza la insercion del registro de control
         --LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRP05 ",
         LET v_s_comando = " fglrun ",v_c_ruta_bin CLIPPED,"/AGRP05 ",
                                                 p_v_usuario, " ",
                                                 v_d_pid, " ",
                                                 v_i_proceso_cod, " ",
                                                 v_i_opera_cod, " ",
                                                 v_d_folio, " ",
                                                 v_v_nom_archivo
                                                 --, " 1>> ",
                                                 --v_c_ruta_list_bat CLIPPED,
                                                 --"/nohup:",v_d_pid USING "&&&&&",":",
                                                 --v_i_proceso_cod USING "&&&&&",":",
                                                 --v_i_opera_cod USING "&&&&&",
                                                 --" 2>&1 &"
         --DISPLAY v_s_comando

         -- Se invoca la funcion que valida y carga la informacion en la tabla de paso
         CALL fn_carga_archivo(v_d_pid, v_i_proceso_cod, v_i_opera_cod, p_b_tipo_carga, v_c_programa_cod, v_s_comando, p_v_usuario, TRUE) RETURNING v_b_continua

         IF v_b_continua THEN
            -- se asigna el mensaje a mostrar al usuario
            --LET v_s_mensaje = "Se ha enviado la carga con PID: ",v_d_pid CLIPPED,
            --                  ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
            --CALL fn_mensaje("Valida",v_s_mensaje,"information")
         END IF
      --END IF
   ELSE 
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)
   END IF

END MAIN
