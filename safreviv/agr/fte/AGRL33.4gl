--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL33                                        #
#Objetivo          =>Programa que pide al usuario la seleccion del #
#                    proceso a conciliar y lanza el proceso de     #
#                    conciliación DSE para Anualidades Garantizadas#
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>30 Mayo 2012                                  #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_nom_prog        VARCHAR(30), -- nombre del programa
          p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          v_fech_concilia     DATE, -- Fecha de conciliacion
          v_cmb_operacion     CHAR(02), -- combos
          v_i_cuenta_regis    INTEGER, -- contador de registro
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_folio             LIKE glo_folio.folio

   DEFINE v_criterio          SMALLINT
   DEFINE v_f_liquida         DATE
   DEFINE v_tabla             CHAR(20)

   # Se asignan los parámetros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   # se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL33.log")

   # Se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   # Se inializan variables
   LET v_i_proceso_cod = g_proc_cod_agr_concilia_dse -- conciliación dev sdo exc
   LET v_i_opera_cod = 1 -- conciliación dev sdo exc
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET v_v_nom_archivo = NULL
   LET v_c_programa_cod = "AGRL33"
   LET v_fech_concilia = TODAY
   LET v_criterio = 0
   LET v_f_liquida = "12/31/1899"

   # Se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   # Se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   # Se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat_dev FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat_dev INTO v_c_ruta_list_bat

   OPEN WINDOW w_genera_conciliacion_dev WITH FORM "AGRL331"
   INPUT v_cmb_operacion, v_d_folio WITHOUT DEFAULTS FROM cmb_opera, folio ATTRIBUTES(UNBUFFERED)
      BEFORE INPUT
         -- se invoca la función que llena el combo
         CALL fn_llena_combo("cmb_opera")

         -- se muestra de fecha de la conciliación
         DISPLAY v_fech_concilia USING "DD-MM-YYYY" TO f_concilia

      # Se llama a la funcion que genera el archivo
      ON ACTION ACCEPT
         # Se valida la operacion seleccionada
         IF v_cmb_operacion IS NULL THEN
            CALL fn_mensaje("Conciliación","Debe seleccionar una operación a conciliar","stop")
            CONTINUE INPUT
         END IF

         # Se valida se capture el folio
         IF v_d_folio IS NULL OR v_d_folio = 0 THEN
            CALL fn_mensaje("Conciliación","Debe capturar el folio a conciliar","stop")
            CONTINUE INPUT
         END IF

         LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

         PREPARE prp_obt_mov FROM v_s_qryTxt
         EXECUTE prp_obt_mov USING v_criterio,
                                   v_d_folio,
                                   v_f_liquida
                              INTO v_tabla

         IF v_cmb_operacion = "01" THEN
            -- se obtienen todos los registros de cta movimiento
            LET v_s_qryTxt = " SELECT COUNT(*)\n",
                             "   FROM ",v_tabla,"\n",
                             "  WHERE folio_liquida = ",v_d_folio,"\n",
                             "    AND id_derechohabiente IN (\n",
                             "        SELECT id_derechohabiente\n",
                             "          FROM safre_tmp:tmp_deudor_rech_devol_saldo_agr)"
         ELSE
            -- se obtienen todos los registros de cta movimiento
            LET v_s_qryTxt = " SELECT COUNT(*)\n",
                             "   FROM ",v_tabla,"\n",
                             "  WHERE folio_liquida = ",v_d_folio,"\n",
                             "    AND id_derechohabiente IN (\n",
                             "        SELECT id_derechohabiente\n",
                             "          FROM safre_tmp:tmp_deudor_conf_devol_saldo_agr)"
         END IF

         PREPARE prp_slc_count_cta_mov FROM v_s_qryTxt
         EXECUTE prp_slc_count_cta_mov INTO v_i_cuenta_regis

         -- se valida si existen registros a procesar
         IF v_i_cuenta_regis = 0 THEN
            CALL fn_mensaje("Conciliación","No existen registros a conciliar para el folio y operación capturada","stop")
            CONTINUE INPUT
         END IF

         -- se invoca la funcion que genera el PID
         CALL fn_genera_pid(v_i_proceso_cod,v_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

         # Se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                v_d_folio, v_c_programa_cod,
                                                v_v_nom_archivo, p_v_usuario)

         -- se valida si se pudo inicializar el proceso
         IF r_b_valida <> 0 THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         # Actualiza la operacion a inicializada
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                                 v_i_proceso_cod,
                                                 v_i_opera_cod,
                                                 v_d_folio,
                                                 v_c_programa_cod,
                                                 v_v_nom_archivo,
                                                 p_v_usuario)

         -- se valida si se pudo actualiza la operación a PROCESANDO
         IF r_b_valida <> 0 THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         # Construye la cadena para ejecutar el comando
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/"

         CASE v_cmb_operacion
            WHEN "01" -- Rechazo de saldos excedentes
               LET v_s_comando = v_s_comando,"AGRP22 "

            WHEN "02" -- Confirmacion de devolucion de saldos
               LET v_s_comando = v_s_comando,"AGRP23 "

            OTHERWISE
               # Muestra actualiza error
               CALL fn_mensaje(p_v_nom_prog,"Ocurrió un error","stop")

               CALL fn_error_opera(v_d_pid, v_i_proceso_cod, v_i_opera_cod) 
                                  RETURNING r_b_valida

               IF r_b_valida <> 0 THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)
               END IF

               EXIT INPUT
         END CASE

         LET v_folio = 0

         # Se crea el comando que ejecuta el modulo que genera la concliación
         LET v_s_comando = v_s_comando,
                           p_v_usuario, " ",
                           v_d_pid, " ",
                           v_i_proceso_cod, " ",
                           v_i_opera_cod, " ",
                           v_d_folio, " ",
                           v_v_nom_archivo, 
                           " 1> ",v_c_ruta_list_bat CLIPPED,
                           "/nohup:",v_d_pid USING "&&&&&",":",
                           v_i_proceso_cod USING "&&&&&",":",
                           v_i_opera_cod USING "&&&&&",
                           " 2>&1 &"

         --DISPLAY v_s_comando
         RUN v_s_comando

         -- se informa al usuario de la ejecución del proceso
         CALL fn_mensaje("Aviso","Se realizó la conciliación satisfactoriamente","information")

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_genera_conciliacion_dev
END MAIN

#Objetivo: Genera el ComboBox operaciones
FUNCTION fn_llena_combo(v_v_nombre_combo)
DEFINE v_v_nombre_combo VARCHAR(20),   -- nombre del combobox
       cb               ui.ComboBox # Variable de Combobox

   LET cb = ui.ComboBox.forName(v_v_nombre_combo) #Asignación del combo a la forma

   # Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   # Limpia el combo
   CALL cb.clear()

   # Opciones del combo
   CALL cb.addItem("01","43 - Rechazo de saldos")
   CALL cb.addItem("02","43 - Confirmación de saldos")   
END FUNCTION

