--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTL44                                        #
#Objetivo          =>Programa pide al usuario un nombre de archivo #
#                    cargado de Uso de Garantía y de éste se       #
#                    genera el archivo de salida de Rechazos de    #
#                    Uso de Garantia 43 bis                        #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>26 Septiembre 2012                            #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

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
          v_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de archivo
          v_s_comando          STRING, -- contiene al comando a correr
          v_d_folio            INTEGER, -- folio
          v_i_cuenta_regs      INTEGER, -- contador de registros
          v_c_fec_hoy          CHAR(8), -- fecha con formato "yyyymmdd"
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          v_c_extension        LIKE cat_operacion.extension, -- extensión del archivo
          r_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados    LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL44.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan las variables
   LET v_i_proceso_cod = g_proc_cod_grt_uso_arch_rechazos -- generación archivo de rechazos
   LET v_i_opera_cod = 1 -- genera archivo rechazos
   LET v_d_pid = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "GRTL44"
   LET v_d_folio = NULL
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

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

   OPEN WINDOW w_genera_archivo_rech WITH FORM "GRTL441"
   INPUT v_d_folio WITHOUT DEFAULTS FROM cbx_archivo ATTRIBUTES(UNBUFFERED)
      BEFORE INPUT
         -- se invoca la funcion que llena el combo de archivos
         CALL fn_llena_combo_archivo("cbx_archivo")

      ON ACTION ACCEPT
         IF v_d_folio IS NULL THEN
            CALL fn_mensaje("Archivo Salida","Debe seleccionar un archivo de entrada para poder continuar.","stop")
            CONTINUE INPUT
         END IF

         -- se el identificador del archivo
         LET v_s_qryTxt = " SELECT id_cre_ctr_archivo\n",
                          "   FROM cre_ctr_archivo\n",
                          "  WHERE folio_archivo = ",v_d_folio,"\n",
                          "    AND id_proceso = ",g_id_proceso_grt_uso,"\n",
                          "    AND operacion = 18\n", -- 18 - Uso de Garantía
                          "    AND estado = 20"

         PREPARE prp_id_cre_ctr_arch FROM v_s_qryTxt
         EXECUTE prp_id_cre_ctr_arch INTO v_id_cre_ctr_archivo

         -- se consultan los datos que componen el cuerpo del archivo de salida
         LET v_s_qryTxt = " SELECT COUNT(*)\n",
                          "   FROM cre_uso_garantia\n",
                          "  WHERE estado IN (240, 150)\n",
                          "    AND id_cre_ctr_archivo = ",v_id_cre_ctr_archivo

         PREPARE prp_cuenta_regs FROM v_s_qryTxt
         EXECUTE prp_cuenta_regs INTO v_i_cuenta_regs

         IF v_i_cuenta_regs = 0 THEN
            -- se muestra mensaje a usuario y no continua
            CALL fn_mensaje("Aviso","No se encontrarón registros rechazados en el archivo seleccionado","stop")

            CONTINUE INPUT
         END IF

         -- se crea la sentencia sql que ejecuta la funcion que genera el pid
         LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)   
           
         -- se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                                v_i_proceso_cod,
                                                v_i_opera_cod,
                                                v_d_folio,
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
                                                 v_d_folio,
                                                 v_c_programa_cod,
                                                 v_v_nom_archivo,
                                                 p_v_usuario)

         -- se verifica si fue posible inicializar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT INPUT
         END IF

         -- se crea el comando que ejecuta el modulo que genera el archivo de salida
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTS05 ",
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

         --DISPLAY v_s_comando
         RUN v_s_comando

         -- se  informa al usuario de la ejecución de la operacion
         CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo","information")

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_genera_archivo_rech
END MAIN

# Objetivo: Funcion que llena el combo de archivo, de Uso de Garatía 43 bis
FUNCTION fn_llena_combo_archivo(p_v_nombre_combo)
   DEFINE p_v_nombre_combo VARCHAR(20), -- nombre del combobox
          v_manejador_cbx  ui.ComboBox,  -- Manejado del Combobox
          v_d_folio_arch   LIKE cre_ctr_archivo.folio_archivo, -- folio del archivo
          v_c_nom_archivo  LIKE cre_ctr_archivo.nom_archivo, -- nombre del archivo
          v_s_qryTxt       STRING -- se asigna una sentencia sql a ejecutar

   -- Asignación del combo a la forma
   LET v_manejador_cbx = ui.ComboBox.forName(p_v_nombre_combo)

   -- Validación si el combo es nulo 
   IF v_manejador_cbx IS NULL THEN
      ERROR "Campo no encontrado en la forma"
      EXIT PROGRAM
   END IF

   # Limpia el combo
   CALL v_manejador_cbx.clear()

   -- se busca los nombres del archivo
   LET v_s_qryTxt = " SELECT folio_archivo, nom_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_grt_uso,"\n",
                    "    AND operacion = 18\n", -- 18 - Uso de Garantía
                    "    AND estado = 20"

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   DECLARE cur_folio_archivo CURSOR FOR prp_folio_archivo

   FOREACH cur_folio_archivo INTO v_d_folio_arch, v_c_nom_archivo
      -- se agregan los folios al combo
      CALL v_manejador_cbx.addItem(v_d_folio_arch,v_c_nom_archivo)
   END FOREACH
END FUNCTION