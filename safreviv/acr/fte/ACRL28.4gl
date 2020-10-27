--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL28                                        #
#Objetivo          =>Programa pide la confirmación del usuario de  #
#                    los saldos remanentes y se ejecuta el proceso #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>27 ENERO 2012                                 #
####################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"

MAIN
   DEFINE p_v_nom_prog        VARCHAR(30), -- nombre del programa
          p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_d_folio           LIKE glo_folio.folio, -- folio del preoceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_b_respuesta       SMALLINT, -- respuesta de la confirmación
          v_dt_fec_corte      DATE, -- Fecha de conciliacion
          v_d_monto_validar   DECIMAL(7,2), -- monto a validar
          v_s_mensaje         STRING, -- contiene mensaje a mostrar al usuario
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          v_s_qryTxt2         STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ind               INTEGER

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".ACRL28.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET v_i_proceso_cod = g_proc_cod_acr_sdos_reman -- saldos remanentes acr
   LET v_i_opera_cod = 1 -- saldos remanentes acr
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET v_v_nom_archivo = "NA"
   LET v_c_programa_cod = "ACRL28"
   LET v_ind = 0 -- contador de registros

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se asigna la fecha de corte
   LET v_dt_fec_corte = TODAY

   OPEN WINDOW w_saldos_remanentes WITH FORM "ACRL281" CLOSE WINDOW SCREEN 
   INPUT v_d_monto_validar WITHOUT DEFAULTS FROM monto_validar ATTRIBUTES(UNBUFFERED)
      BEFORE INPUT
         DISPLAY v_dt_fec_corte TO f_corte

         -- valida que existan registros de cre acreditado con estado Liquidado (140) o Migrado (900)
         LET v_s_qryTxt2 = " SELECT COUNT(*)\n",
                           "   FROM cre_acreditado\n",
                           "  WHERE estado IN (140,900,220)\n",
                           "    AND edo_procesar IN (120,5)\n",
                           "    AND tpo_originacion = 1"

         PREPARE prp_num_regs_id FROM v_s_qryTxt2
         EXECUTE prp_num_regs_id INTO v_ind

         IF v_ind <= 0 THEN 
            CALL fn_mensaje("Aviso","No existen registros para llevar a cabo esta operación ","information")
            EXIT INPUT
         END IF

      ON ACTION ACCEPT
         -- se valida la fecha de corte
         IF v_d_monto_validar IS NULL THEN
            CALL fn_mensaje("Aviso","Debe ingresar el monto a validar para continuar","information")

            CONTINUE INPUT
         END IF

         -- solicita la confirmación de usuario para ejecutar el proceso de saldos remanentes
         -- regresa: 1(confirmar) o 0(cancelar)
         LET v_s_mensaje = "Confirme la ejecución del proceso de saldos remanentes de\n",
                           "acreditados con saldo deudor liquidado"
         CALL fn_ventana_confirma("Saldos remanentes",v_s_mensaje,"quest") RETURNING v_b_respuesta

         -- si se confirmó, se ejecuta el proceso que realiza Saldos remanentes
         IF NOT v_b_respuesta THEN
            EXIT INPUT
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

            EXIT INPUT
         END IF

         -- se invoca la función que deja la operación en estado Procesando
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                 v_d_folio, v_c_programa_cod,
                                                 v_v_nom_archivo, p_v_usuario)

         -- se verifica si fue posible actualiza la operación
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         -- se enviará el monto ingresado por el usuario en la variable de folio
         LET v_d_folio = v_d_monto_validar * 100

         -- se crea el comando que ejecuta el proceso de saldos remanentes
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRP19 ",
                                                 p_v_usuario CLIPPED, " ",
                                                 v_d_pid CLIPPED,  " ",
                                                 v_i_proceso_cod CLIPPED, " ",
                                                 v_i_opera_cod CLIPPED, " ",
                                                 v_d_folio CLIPPED , " ",
                                                 v_v_nom_archivo CLIPPED, " 1> ",
                                                 v_c_ruta_list_bat CLIPPED,
                                                 "/nohup:",v_d_pid USING "&&&&&",":",
                                                 v_i_proceso_cod USING "&&&&&",":",
                                                 v_i_opera_cod USING "&&&&&",
                                                 " 2>&1 &"

         DISPLAY v_s_comando
         RUN v_s_comando

         LET v_s_mensaje = "Se ha enviado la ejecución de Saldos Remanentes con PID: ",v_d_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
         CALL fn_mensaje("Aviso",v_s_mensaje,"information")

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_saldos_remanentes
END MAIN
