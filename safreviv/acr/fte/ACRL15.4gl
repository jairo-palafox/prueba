--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL15                                        #
#Objetivo          =>Programa pide al usuario un numero de folio   #
#                    y con éste se lanza el proceso que genera el  #
#                    archivo de salida de liquidación              #
#Autor             =>Ivan Vega, EFP                                #
#Modificó          =>Daniel Buendia, EFP                           #
#Fecha inicio      =>25 ENERO 2012                                 #
####################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"

MAIN
   DEFINE p_v_nom_prog         VARCHAR(30), -- nombre del programa
          p_b_tipo_carga       SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario          LIKE seg_usuario.usuario, -- usuario firmado al sistema
          v_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod        LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid              DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo      LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_list_bat    LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando          STRING, -- contiene al comando a correr
          v_folio_liquida      INTEGER, -- folio
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          v_c_fec_hoy          CHAR(8), -- fecha con formato "yyyymmdd"
          v_si_tpo_originacion LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_c_extension        LIKE cat_operacion.extension, -- extensión del archivo
          v_i_cuenta_regs_liq  INTEGER, -- cuenta los registros que se irán en el archivo
          r_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados    LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".ACRL15.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan las variables
   LET v_i_proceso_cod = g_proc_cod_acr_arch_liquida -- generación archivo liquidación acr
   LET v_i_opera_cod = 1 -- genera archivo liquidación
   LET v_d_pid = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "ACRL15"
   LET v_folio_liquida = NULL
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_si_tpo_originacion = 1 -- Transferencia de Acreditados

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   OPEN WINDOW w_genera_archivoliquidacion WITH FORM "ACRL151"
   INPUT v_folio_liquida WITHOUT DEFAULTS FROM folio_liquida ATTRIBUTES(UNBUFFERED)
      --se llama a la funcion que genera el archivo
      ON ACTION ACCEPT
         IF v_folio_liquida IS NULL OR v_folio_liquida = 0 THEN
            CALL fn_mensaje("Archivo salida Liquidación","Debe ingresar un número de folio para poder generar el archivo","stop")
            CONTINUE INPUT
         END IF

         # Valida que exista información para el folio catpurado
         LET v_s_qryTxt = " SELECT COUNT(*)\n",
                          "   FROM cre_saldo_deudor\n",
                          "  WHERE id_cre_acreditado IN (\n",
                          "        SELECT UNIQUE id_cre_acreditado\n",
                          "          FROM cre_acreditado\n",
                          "         WHERE estado = 140\n",
                          "           AND folio_liquida = ",v_folio_liquida,"\n",
                          "           AND tpo_originacion = ",v_si_tpo_originacion,")\n",
                          "    AND movimiento = 181\n",
                          "    AND monto_pesos > 0"

         PREPARE prp_cuenta_regs_liquida FROM v_s_qryTxt
         EXECUTE prp_cuenta_regs_liquida INTO v_i_cuenta_regs_liq

         IF v_i_cuenta_regs_liq = 0 THEN
            CALL fn_mensaje("Archivo salida Liquidación","No existe información para el folio:"||v_folio_liquida,"about")
            CONTINUE INPUT
         END IF

         -- se crea la sentencia sql que ejecuta la funcion que genera el pid
         LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)   

         -- se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                                v_i_proceso_cod,
                                                v_i_opera_cod,
                                                v_folio_liquida,
                                                v_c_programa_cod,
                                                v_v_nom_archivo,
                                                p_v_usuario)

         -- se verifica si fue posible inicializar el proceso
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         -- se obtiene la extensión del archivo
         LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

         -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
         LET v_v_nom_archivo = "A" || v_c_fec_hoy || "." || v_c_extension CLIPPED
         DISPLAY "ARCHIVO A GENERAR: ", v_v_nom_archivo

         -- se invoca la función que deja la operación en estado Procesando
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                                 v_i_proceso_cod,
                                                 v_i_opera_cod,
                                                 v_folio_liquida,
                                                 v_c_programa_cod,
                                                 v_v_nom_archivo,
                                                 p_v_usuario)

         -- se verifica si fue posible inicializar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRS10 ",
                                                 p_v_usuario, " ",
                                                 v_d_pid, " ",
                                                 v_i_proceso_cod, " ",
                                                 v_i_opera_cod, " ",
                                                 v_folio_liquida, " ",
                                                 v_v_nom_archivo, " 1> ",
                                                 v_c_ruta_list_bat CLIPPED,
                                                 "/nohup:",v_d_pid USING "&&&&&",":",
                                                 v_i_proceso_cod USING "&&&&&",":",
                                                 v_i_opera_cod USING "&&&&&",
                                                 " 2>&1 &"

         --DISPLAY v_s_comando
         RUN v_s_comando

         CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo","information")

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_genera_archivoliquidacion
END MAIN
