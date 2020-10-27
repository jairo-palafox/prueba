--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Modulo            => GRT                                           #
#Programa          => GRTL26                                        #
#Objetivo          => Programa que genera el archivo de salida de   #
#                     solicitud de saldos para el módulo de         #
#                     Uso de Garantía 43 bis                        #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 26 Abril 2012                                 #
#Modifica:         => Mauro Muñiz Caballero                         #
#Fecha modif:      => 23 de marzo de 2016                           #
#Adecuación        => Eliminación de adelantos                      #
#####################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

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
   DEFINE v_dt_fec_present_ax        DATE -- fecha auxiliar de presentacion
   DEFINE v_dt_fec_movimiento        DATE -- fecha de movimiento
   DEFINE v_c_fecha_hoy              CHAR(8) -- fecha de hoy con formato YYYYMMDD
   DEFINE v_dt_f_solic_saldos        DATE -- fecha para la generación de solicitud de saldos
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

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTL26.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan las variables
   LET v_i_proceso_cod  = g_proc_cod_grt_uso_arch_solic -- generación archivo solic sdos uso 43bis
   LET v_i_opera_cod    = 1 -- solicitud de saldos
   LET v_d_pid          = 0
   LET v_c_programa_cod = "GRTL26"
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

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present_ax = TODAY - DAY(TODAY) + 1

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present_ax CLIPPED, "',2)"

   PREPARE prp_obtiene_3habil FROM v_s_qryTxt
   EXECUTE prp_obtiene_3habil INTO v_dt_fec_present_ax

   LET v_dt_fec_movimiento = v_dt_fec_present_ax - DAY(v_dt_fec_present_ax) + 1
   LET v_dt_fec_movimiento = v_dt_fec_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentación: ",v_dt_fec_present_ax USING "DD/MM/YYYY"
   DISPLAY " Fecha movimiento  : ",v_dt_fec_movimiento USING "DD/MM/YYYY"

   -- se obtiene el precio de accion para el día de liquidación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11",
                    "    AND f_valuacion = '",v_dt_fec_movimiento,"'"

   PREPARE prp_precio_accion FROM v_s_qryTxt
   EXECUTE prp_precio_accion INTO v_i_cont_regs

   -- se valida el precio de accion
   IF v_i_cont_regs = 0 THEN
      -- se asigna el mensaje a mostrar a usuario
      LET v_s_mensaje = "No es posible la generacion de la solicitud ya que noexiste el \n",
                        "precio de acción para la fecha de movimiento: ",v_dt_fec_movimiento USING "DD/MM/YYYY"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      EXIT PROGRAM
   END IF

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present_ax,"'\n",
                    "    AND id_proceso = ",g_id_proceso_grt_uso

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

   -- se abre el menu de la opcion
   OPEN WINDOW w_sol_sdos WITH FORM "GRTL261"
      --se muestran las fechas en la ventana
      DISPLAY TODAY, v_dt_fec_present_ax TO f_generacion, f_presentacion

      MENU
        COMMAND "Aceptar"
           -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
           LET v_dt_f_solic_saldos = v_dt_fec_present_ax - DAY(v_dt_fec_present_ax)
           -- esta asignación es unicamente para pruebas. Borrar después de las mismas
           LET v_dt_f_solic_saldos = TODAY

            SELECT t1.tabla
              FROM cat_tab_movimiento t1
            INTO TEMP tmp_cat_tab_movimiento

            INSERT INTO tmp_cat_tab_movimiento
            VALUES ("cta_movimiento")

            DECLARE cur_cta_mov CURSOR FOR
            SELECT t2.tabla
              FROM tmp_cat_tab_movimiento t2

            FOREACH cur_cta_mov INTO v_tabla
               -- se valida que existan regsitros a generar generar con adelantos
               LET v_s_qryTxt = " SELECT COUNT(cre.id_derechohabiente)\n",
                                "   FROM cre_uso_garantia cre,", v_tabla," cta\n",
                                "  WHERE cre.folio_liquida = cta.folio_liquida\n",
                                "    AND cre.estado = 140\n",
                                "    AND cre.edo_procesar IN (10, 60, 70)\n",
                                "    AND cre.id_derechohabiente = cta.id_derechohabiente\n",
                                "    AND cre.tpo_transferencia IN ('18','48')\n",
                                "    AND cre.f_proceso <= '",v_dt_f_solic_saldos,"'",
                                "    AND cre.periodo_pago = cta.id_referencia\n"

               PREPARE prp_count_sol_sdos FROM v_s_qryTxt
               EXECUTE prp_count_sol_sdos INTO v_i_cont_regs

               IF v_i_cont_regs IS NULL OR v_i_cont_regs = 0 THEN
                  LET v_s_qryTxt = " SELECT COUNT(cre.id_derechohabiente)\n",
                                   "   FROM cre_uso_garantia cre\n",
                                   "  WHERE cre.estado IN(142, 20)\n",
                                   "    AND cre.edo_procesar IN (10, 60, 70)\n",
                                   "    AND cre.tpo_transferencia IN ('18','48')\n"

                  PREPARE prp_count_no_adelanto FROM v_s_qryTxt
                  EXECUTE prp_count_no_adelanto INTO v_i_cont_regs

                  IF v_i_cont_regs IS NULL THEN
                      LET v_i_cont_regs = 0
                  END IF
               END IF

               LET v_t_cont_regs = v_t_cont_regs + v_i_cont_regs
            END FOREACH

            CLOSE cur_cta_mov
            FREE cur_cta_mov

           -- en caso de no encontrar registros no continua con la generación del archivo
           IF v_i_cont_regs = 0 THEN
              CALL fn_mensaje("Solicitud de Saldos","No existe la información solicitada","stop")
              CONTINUE MENU
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
              LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTS03 ",
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
              CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de solicitud de saldos","information")
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
