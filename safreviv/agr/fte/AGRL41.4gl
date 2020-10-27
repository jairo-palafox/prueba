####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL41                                        #
#Objetivo          =>Programa que pide al usuario la seleccion del #
#                    m�dulo y el tipo para generar los archivos de #
#                    conciliaci�n                                  #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>21 Marzo 2013                                 #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog        VARCHAR(30), -- nombre del programa
          v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operaci�n que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_d_folio           DECIMAL(9,0), -- folio del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_arch_sal    VARCHAR(40), -- nombre del archivo de salida
          v_v_edos_procesar   VARCHAR(50), -- contiene la lista de estados procesar a consultar
          v_i_cta_num_regs    INTEGER, -- cuenta el numero de registros a conciliar
          v_c_fec_hoy         CHAR(8), -- fecha con formato "yyyymmdd"
          v_si_tpo_originac   SMALLINT, -- selecci�n del tipo de originaci�n
          v_si_tpo_concilia   SMALLINT, -- selecci�n del tipo de conciliaci�n
          v_dt_fec_concilia   DATE, -- fecha de conciliaci�n
          v_c_val_mod_cod     CHAR(2), -- selecci�n del modulo a conciliar
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_mensaje         STRING, -- mensaje a mostrar al usuario
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta del bin del m�dulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del m�dulo
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- recupera los parametros que vienen del principal
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL41.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET v_i_proceso_cod = g_proc_cod_agr_arch_concilia -- genera archivos de conciliaci�n
   LET v_i_opera_cod = 1 -- genera archivo de conciliaci�n
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET v_c_programa_cod = "AGRL41"
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_si_tpo_originac = NULL
   LET v_si_tpo_concilia = NULL
   LET v_dt_fec_concilia = TODAY

   -- se obtiene la ruta bin y de listados del m�dulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   OPEN WINDOW w_conciliacion WITH FORM "AGRL411"
   INPUT v_si_tpo_originac, v_si_tpo_concilia, v_dt_fec_concilia WITHOUT DEFAULTS
    FROM cbx_modulo_cod, cbx_tpo_concil, dte_f_concilia ATTRIBUTES(UNBUFFERED)
      AFTER FIELD dte_f_concilia
         NEXT FIELD cbx_modulo_cod

      ON ACTION ACCEPT
         -- se valida la opci�n de m�dulo
         IF v_si_tpo_originac IS NULL THEN
            CALL fn_mensaje("Conciliaci�n","Debe seleccionar un m�dulo para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se valida la opci�n tipo de conciliaci�n
         IF v_si_tpo_concilia IS NULL THEN
            CALL fn_mensaje("Conciliaci�n","Debe seleccionar un tipo de conciliaci�n para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se valida el tipo de originaci�n seleccionado
         IF v_si_tpo_originac = 1 THEN
            LET v_c_val_mod_cod = "TA"
         ELSE
            LET v_c_val_mod_cod = "AG"
         END IF

         -- se genera el nombre de los archivos de salida
         IF v_si_tpo_concilia = 1 THEN
            LET v_v_nom_arch_sal = "nm_" || v_c_fec_hoy || "_" || v_c_val_mod_cod || ".xtm"
            LET v_v_edos_procesar = "40, 50"
         ELSE
           IF v_si_tpo_concilia = 2 THEN
              LET v_v_nom_arch_sal = "rch_" || v_c_fec_hoy || "_" || v_c_val_mod_cod || ".xtr"
              LET v_v_edos_procesar = "70, 85"
           ELSE
              LET v_v_nom_arch_sal = "dif_" || v_c_fec_hoy || "_" || v_c_val_mod_cod || ".xtd"
              LET v_v_edos_procesar = "120"
           END IF
         END IF

         -- se armar consulta que obtiene la informaci�n para el registro detalle
         LET v_s_qryTxt = " SELECT COUNT(*)\n",
                          "   FROM cre_acreditado\n",
                          "  WHERE estado = 140\n",
                          "    AND edo_procesar IN (",v_v_edos_procesar,")\n",
                          "    AND folio_liquida IS NOT NULL\n",
                          "    AND folio_liquida <> 0\n",
                          "    AND tpo_originacion = ",v_si_tpo_originac

         PREPARE prp_cnt_numReg_cre FROM v_s_qryTxt
         EXECUTE prp_cnt_numReg_cre INTO v_i_cta_num_regs

         -- se valida el n�mero de registros
         IF v_i_cta_num_regs = 0 THEN
            IF v_si_tpo_originac = 4 THEN
               -- se armar consulta que obtiene la informaci�n para el registro detalle
               LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                "   FROM cre_uso_garantia\n",
                                "  WHERE estado = 140\n",
                                "    AND edo_procesar IN (",v_v_edos_procesar,")\n",
                                "    AND folio_liquida IS NOT NULL\n",
                                "    AND folio_liquida <> 0\n",
                                "    AND tpo_transferencia = '43'"

               PREPARE prp_cnt_numReg_uso FROM v_s_qryTxt
               EXECUTE prp_cnt_numReg_uso INTO v_i_cta_num_regs

               -- se valida el n�mero de registros
               IF v_i_cta_num_regs = 0 THEN
                  CALL fn_mensaje("Conciliaci�n","No existen registros a conciliar","stop")
                  CONTINUE INPUT
               END IF
            ELSE
               CALL fn_mensaje("Conciliaci�n","No existen registros a conciliar","stop")
               CONTINUE INPUT
            END IF
         END IF

         -- se crea la sentencia sql que ejecuta la funcion que genera el pid
         LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)   

         -- se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                                v_i_proceso_cod,
                                                v_i_opera_cod,
                                                v_d_folio,
                                                v_c_programa_cod,
                                                v_v_nom_arch_sal,
                                                p_v_usuario)

         -- se verifica si fue posible inicializar el proceso
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         -- se invoca la funci�n que deja la operaci�n en estado Procesando
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                                 v_i_proceso_cod,
                                                 v_i_opera_cod,
                                                 v_d_folio,
                                                 v_c_programa_cod,
                                                 v_v_nom_arch_sal,
                                                 p_v_usuario)

         -- se verifica si fue posible inicializar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT PROGRAM
         END IF

         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidaci�n
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/AGRS07 ",
                                                 p_v_usuario, " ",
                                                 v_d_pid, " ",
                                                 v_i_proceso_cod, " ",
                                                 v_i_opera_cod, " ",
                                                 v_d_folio, " ",
                                                 v_v_nom_arch_sal, " ",
                                                 v_si_tpo_originac, " ",
                                                 v_si_tpo_concilia, " 1> ",
                                                 v_c_ruta_list_bat CLIPPED,
                                                 "/nohup:",v_d_pid USING "&&&&&",":",
                                                 v_i_proceso_cod USING "&&&&&",":",
                                                 v_i_opera_cod USING "&&&&&",
                                                 " 2>&1 &"

         --DISPLAY v_s_comando
         RUN v_s_comando

         -- se asigna el mensaje a mostrar al usuario
         LET v_s_mensaje = "Se ha enviado la conciliaci�n con PID: ",v_d_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos"
         CALL fn_mensaje("Integraci�n",v_s_mensaje,"information")

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_conciliacion
END MAIN
