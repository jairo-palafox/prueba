#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRL16                                        #
#Objetivo          => Programa que genera el archivo de salida de   #
#                     solicitud de saldos para el módulo de         #
#                     Anualidades Garantizadas                      #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 10 Abril 2012                                 #
#Modifica:         => Mauro Muñiz Caballero                         #
#Fecha modif:      => 9 de noviembre de 2015                        #
#Adecuación        => Eliminación de adelantos                      #
#Modifica:         => Emilio Abarca, EFP.                           #
#Fecha modif:      => 26 Febereo 2019                               #
#Adecuación        => Valida precio valor de fondo, en caso de no   #
#                  => existir no continúa con el proceso.           #
#####################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

GLOBALS

   DEFINE p_v_nom_prog              VARCHAR(30) -- nombre del programa
   DEFINE p_b_tipo_carga            SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- usuario firmado al sistema
   DEFINE v_i_proceso_cod           LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod             LIKE cat_operacion.opera_cod -- operación que llama la funcion
   DEFINE v_d_pid                   DECIMAL(9,0) -- identificador del proceso
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_v_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_d_folio                 LIKE glo_ctr_archivo.folio -- folio
   DEFINE v_i_tpo_originacion       LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_i_tpo_transf            LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_dt_fec_present_ax       DATE -- fecha auxiliar de presentacion
   DEFINE v_c_fecha_hoy             CHAR(8) -- fecha de hoy con formato YYYYMMDD
   DEFINE v_dt_f_solic_saldos       DATE -- fecha para la generación de solicitud de saldos
   DEFINE v_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_i_cont_regs             INTEGER -- contador de registro
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_i_lote                  LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_c_lote                  CHAR(1) -- lote del archivo
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_hoy                     DATE
   DEFINE v_s_mensaje               STRING -- mensaje a mostrar al usuario
   DEFINE v_f_valua                 DATE
   DEFINE v_valor_fondo             DECIMAL(12,6)

END GLOBALS

MAIN

   -- se asignan los parámetros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRL16.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   CLOSE WINDOW SCREEN

   -- se inicializan las variables
   LET v_i_proceso_cod     = g_proc_cod_agr_arch_solic -- generación archivo solicitud sdos ag
   LET v_i_opera_cod       = 1 -- genera archivo solicitud de saldos
   LET v_d_pid             = 0
   LET v_v_nom_archivo     = "N/A"
   LET v_c_programa_cod    = "AGRL16"
   LET v_d_folio           = 0
   LET v_i_tpo_originacion = 4 -- Anualidades Garantizadas
   LET v_i_tpo_transf      = "43" -- Anualidades Garantizadas
   LET v_hoy               = TODAY - 1 units day

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

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present_ax = v_hoy - DAY(v_hoy) + 1

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present_ax CLIPPED, "',2)" 

   PREPARE prp_obtiene_3habil FROM v_s_qryTxt
   EXECUTE prp_obtiene_3habil INTO v_dt_fec_present_ax

   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_fec_present_ax < v_hoy THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_dt_fec_present_ax = v_hoy - DAY(v_hoy) + 1
      LET v_dt_fec_present_ax = v_dt_fec_present_ax + 1 UNITS MONTH

      PREPARE prp_obtiene_3habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_fec_present_ax CLIPPED||"',2)"
      EXECUTE prp_obtiene_3habil_sig INTO v_dt_fec_present_ax
   END IF

   DISPLAY " FECHA PRESENTACIÓN:   ",v_dt_fec_present_ax

   LET v_f_valua = (v_dt_fec_present_ax - DAY(v_dt_fec_present_ax) + 1) + 1 UNITS MONTH

   DISPLAY " FECHA VALUACIÓN AVIS: ",v_f_valua

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present_ax,"'\n",
                    "    AND id_proceso = ",g_id_proceso_agr

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   -- se asigna el lote en variable CHAR(1)
   LET v_c_lote = v_i_lote

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fecha_hoy = TODAY USING "YYYYMMDD"

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_v_nom_archivo = "A" || v_c_fecha_hoy || v_c_lote || "." || v_c_extension CLIPPED

   LET v_i_cont_regs= 0

   -- se abre el menu de la opcion
   OPEN WINDOW w_sol_sdos WITH FORM "AGRL161"
      --se muestran las fechas en la ventana
      DISPLAY TODAY, v_dt_fec_present_ax, v_f_valua TO f_generacion, f_presentacion, f_valuacion

      MENU
         COMMAND "Aceptar"

            -- Verifica si existe el precio de valor de fondo para calcular AIVS
            -- de acuerdo a la fecha de valuacion.
            SELECT precio_fondo
              INTO v_valor_fondo
              FROM glo_valor_fondo
             WHERE f_valuacion = v_f_valua
               AND fondo = 11;

            IF(v_valor_fondo IS NULL) OR
              (v_valor_fondo = "") OR 
              (v_valor_fondo = 0) THEN
               CALL fn_mensaje("Aviso","NO EXISTE VALOR DE AIV","stop")
               CONTINUE MENU
            END IF
               
            -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
            LET v_dt_f_solic_saldos = v_dt_fec_present_ax - DAY(v_dt_fec_present_ax)

            -- se valida que existan registros a generar con adelantos
            LET v_s_qryTxt = " SELECT COUNT(*)\n",
                             "   FROM cre_acreditado cre,\n",
                             "        cat_tipo_credito tpo\n",
                             "  WHERE cre.estado = 140\n",
                             "    AND cre.edo_procesar IN (60, 70)\n",
                             "    AND cre.tpo_originacion = ",v_i_tpo_originacion,"\n",
                             "    AND cre.tpo_originacion = tpo.tpo_originacion\n",
                             "    AND cre.tpo_credito = tpo.tpo_credito\n",
                             "    AND tpo.id_deudor = 1"

            PREPARE prp_count_sol_sdos FROM v_s_qryTxt
            EXECUTE prp_count_sol_sdos INTO v_i_cont_regs

            -- en caso de no encontrar registros no continua con la generación del archivo
            IF v_i_cont_regs = 0 THEN
               -- se valida que existan regsitros a generar sin adelantos
               LET v_s_qryTxt =" SELECT COUNT(*)\n",
                             "   FROM cre_acreditado cre,\n",
                             "        cat_tipo_credito tpo\n",
                             "  WHERE cre.estado IN(18,20, 25) \n",
                             "    AND cre.edo_procesar IN (60, 70)\n",
                             "    AND cre.tpo_originacion = ",v_i_tpo_originacion,"\n",
                             "    AND cre.tpo_originacion = tpo.tpo_originacion\n",
                             "    AND cre.tpo_credito = tpo.tpo_credito\n",
                             "    AND tpo.id_deudor = 1"

               PREPARE prp_count_sdo_sin FROM v_s_qryTxt
               EXECUTE prp_count_sdo_sin INTO v_i_cont_regs

               -- en caso de no encontrar registros no continua con la generación del archivo
               IF v_i_cont_regs = 0 THEN
                  -- se valida que existan regsitros a generar en uso de garantia con adelantos
                  LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                   "   FROM cre_uso_garantia\n",
                                   "  WHERE estado = 140\n",
                                   "    AND edo_procesar IN (10, 60, 70)\n",
                                   "    AND tpo_transferencia = '",v_i_tpo_transf,"'"

                  PREPARE prp_count_sol_sdos_uso FROM v_s_qryTxt
                  EXECUTE prp_count_sol_sdos_uso INTO v_i_cont_regs

                  -- en caso de no encontrar registros no continua con la generación del archivo
                  IF v_i_cont_regs = 0 THEN
                     -- se valida que existan regsitros a generar en uso de garantia sin adelantos
                     LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                      "   FROM cre_uso_garantia\n",
                                      "  WHERE estado = 20\n",
                                      "    AND edo_procesar IN (10, 60, 70)\n",
                                      "    AND tpo_transferencia = '",v_i_tpo_transf,"'"

                     PREPARE prp_count_sol_sdos_sin FROM v_s_qryTxt
                     EXECUTE prp_count_sol_sdos_sin INTO v_i_cont_regs

                     -- en caso de no encontrar registros no continua con la generación del archivo
                     IF v_i_cont_regs = 0 THEN
                        CALL fn_mensaje("Solicitud de Saldos","No existe la información solicitada","stop")
                        CONTINUE MENU
                     END IF
                  END IF
               END IF
            END IF

            -- se crea la sentencia sql que ejecuta la funcion que genera el pid
            LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

            -- se invoca la funcion que inicializa el proceso
            LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                   v_d_folio, v_c_programa_cod,
                                                   v_v_nom_archivo, p_v_usuario)

            -- se verifica si fue posible inicializar el proceso
            IF r_b_valida <> 0 THEN
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               EXIT PROGRAM
            END IF

            -- se invoca la función que deja la operación en estado Procesando
            LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                    v_d_folio, v_c_programa_cod,
                                                    v_v_nom_archivo, p_v_usuario)

            -- se verifica si fue posible inicializar la operacion
            IF r_b_valida = 0 THEN
               -- se crea el comando que ejecuta el modulo que genera el archivo de salida de cargo a capital
               LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRS04 ",
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

               -- se ejecuta el comando armado
               RUN v_s_comando

               -- se informa al usuario de la ejecucion del proceso
               LET v_s_mensaje = "Se ejecutó el proceso de generación de solicitud de saldos con PID: ",v_d_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Aviso",v_s_mensaje,"information")
            ELSE
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)

               EXIT MENU
            END IF

            EXIT MENU

         COMMAND "Cancelar"
            EXIT MENU
      END MENU

   CLOSE WINDOW w_sol_sdos

END MAIN
