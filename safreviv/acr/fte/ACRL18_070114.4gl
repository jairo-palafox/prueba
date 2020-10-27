--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL18                                        #
#Objetivo          =>Programa que genera el archivo de salida de   #
#                    solicitud de saldos para el módulo de         #
#                    Transferencia de Acreditados                  #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>25 ENERO 2012                                 #
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
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_d_folio           LIKE glo_ctr_archivo.folio, -- folio
          v_i_tpo_originacion LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_dt_fec_present_ax DATE, -- fecha auxiliar de presentacion
          v_c_fecha_hoy       CHAR(8), -- fecha de hoy con formato YYYYMMDD
          v_dt_f_solic_saldos DATE, -- fecha para la generación de solicitud de saldos
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_i_cont_regs       INTEGER, -- contador de registro
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          v_i_lote            LIKE dse_ctr_archivo.lote, -- lote del archivo
          v_c_lote            CHAR(1), -- lote del archivo
          v_c_extension       LIKE cat_operacion.extension, -- extensión del archivo
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta listados del módulo
          
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRL18.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan las variables
   LET v_i_proceso_cod = g_proc_cod_acr_arch_solic -- generación archivo solicitud saldos acr
   LET v_i_opera_cod = 1 -- genera archivo solicitud de saldos
   LET v_d_pid = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "ACRL18"
   LET v_d_folio = 0
   LET v_i_tpo_originacion = 1 -- Transferencia de acreditados

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

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

   PREPARE prp_obt_3habil FROM v_s_qryTxt
   EXECUTE prp_obt_3habil INTO v_dt_fec_present_ax

   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_fec_present_ax < TODAY THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_dt_fec_present_ax = TODAY - DAY(TODAY) + 1
      LET v_dt_fec_present_ax = v_dt_fec_present_ax + 1 UNITS MONTH

      PREPARE prp_obt_3habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_fec_present_ax CLIPPED||"',2)"
      EXECUTE prp_obt_3habil_sig INTO v_dt_fec_present_ax
   END IF

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present_ax,"'\n",
                    "    AND id_proceso = ",g_id_proceso_acr

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

   -- se asigna el nombre del archivo
   LET v_v_nom_archivo = "A" || v_c_fecha_hoy || v_c_lote || "." || v_c_extension CLIPPED

   -- se abre el menu de la opcion ""
   OPEN WINDOW w_sol_sdos WITH FORM "ACRL181"
      --se muestran las fechas en la ventana
      DISPLAY TODAY, v_dt_fec_present_ax TO f_generacion, f_presentacion

      MENU
        COMMAND "Aceptar"
           -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
           LET v_dt_f_solic_saldos = v_dt_fec_present_ax - DAY(v_dt_fec_present_ax)

           -- se valida que existan regsitros a generar
           LET v_s_qryTxt = " SELECT COUNT(*)\n",
                            "   FROM cre_acreditado\n",
                            "  WHERE estado = 140\n",
                            "    AND edo_procesar = 70\n",
                            "    AND tpo_originacion = ",v_i_tpo_originacion,"\n",
                            "    AND id_cre_ctr_archivo IN (\n",
                            "        SELECT id_cre_ctr_archivo\n",
                            "          FROM cre_ctr_archivo\n",
                            "         WHERE f_proceso <= '",v_dt_f_solic_saldos,"')"

           PREPARE prp_count_sol_sdos FROM v_s_qryTxt
           EXECUTE prp_count_sol_sdos INTO v_i_cont_regs

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
              LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRS13 ",
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
