--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP14                                             #
#Objetivo          => Programa que ejecuta los procesos que generan los  #
#                     archivos de salida durante la liquidación del      #
#                     módulo de Anualidades Garantizadas                 #
#Autor             => Daniel Buendia, EFP                                #
#Fecha inicio      => 25 Mayo 2012                                       #
#Modifica          => Mauro Muñiz Caballero                              #
#                     18/08/2017                                         #
#                     Liquidación deudor crédito liquidado               #
##########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

GLOBALS

   DEFINE g_usuario_cod             LIKE seg_usuario.usuario_cod      -- clave del usuario firmado
   DEFINE g_pid                     LIKE bat_ctr_proceso.pid          -- ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod      -- código del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod      -- código de operación
   DEFINE g_folio_liquida           LIKE cta_movimiento.folio_liquida -- folio de liquidación
   DEFINE g_tpo_liq                 SMALLINT

END GLOBALS

MAIN

   DEFINE v_r_cre_acreditado DYNAMIC ARRAY OF RECORD
      id_cre_acreditado   DECIMAL(9,0), 
      id_cre_ctr_archivo  DECIMAL(9,0), 
      folio_liquida       DECIMAL(9,0), 
      id_derechohabiente  DECIMAL(9,0), 
      tpo_originacion     SMALLINT,
      tpo_credito         SMALLINT,
      tpo_registro        CHAR(2),
      num_credito         DECIMAL(10,0),
      sdo_deudor          DECIMAL(12,2),
      f_otorga            DATE,
      f_culmina           DATE,
      edo_credito         SMALLINT,
      tpo_dscto           SMALLINT,
      valor_dscto         DECIMAL(12,4),
      nrp                 CHAR(11),
      f_ini_dscto         DATE,
      nss_liberado        CHAR(11),
      f_gen_arh           DATE,
      sdo_credito         DECIMAL(12,2),
      f_prox_liq          DATE,
      f_desde             DATE,
      f_hasta             DATE,
      tpo_rch             SMALLINT,
      edo_procesar        SMALLINT,
      estado              SMALLINT
   END RECORD

   DEFINE v_cre_acred               INTEGER

   DEFINE v_opcion_fun              SMALLINT -- opcion para la funcion general
   DEFINE v_v_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_c_f_liquida_aux         CHAR(8) -- fecha de liquidación con formato "YYYYMMDD"
   --DEFINE v_r_cre_acreditado      RECORD LIKE cre_acreditado.* -- registro de cre acreditado
   DEFINE v_r_cre_uso_garantia      RECORD LIKE cre_uso_garantia.* -- registro de uso de garantia
   DEFINE v_i_estado                SMALLINT -- estado al que se va a actualizar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_proceso_cod_folio     LIKE cat_proceso.proceso_cod -- codigo del proceso para el folio
   DEFINE v_i_opera_cod_folio       LIKE cat_operacion.opera_cod -- codigo de operacion para el folio
   DEFINE v_marca_entra             SMALLINT -- marca de anualidad liquidado
   DEFINE v_estado_marca            SMALLINT -- estado de la marca
   DEFINE v_marca_causa             SMALLINT -- marca causa
   DEFINE v_edo_desmarca            SMALLINT -- guarda el valor retornado por la funcion de desmarca
   DEFINE v_desc_desmarca           LIKE cat_rch_marca.rch_desc -- descripcion del retorno de la desmarca
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE p_v_arch_proceso          LIKE bat_ctr_operacion.nom_archivo -- archivo del proceso
   DEFINE v_i_tpo_originacion       LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_c_tpo_transferencia     LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_archivo          VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_ch_arch_reporte1        BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_ch_arch_reporte2        BASE.CHANNEL -- manejador de apuntador hacia archivo

   DEFINE v_r_archivo_rep RECORD
      nss                           CHAR(11),
      monto_acciones                DECIMAL(18,6),
      monto_pesos                   DECIMAL(14,2)
   END RECORD

   DEFINE v_r_det_rpt1 RECORD
      nss                           CHAR(11),
      monto_acciones                CHAR(15),
      monto_pesos                   CHAR(15)
   END RECORD

   DEFINE v_r_det_rpt2 RECORD
      nss                           CHAR(11),
      fec_liquida                   CHAR(8)
   END RECORD

   DEFINE v_r_cta_marca_ws          RECORD LIKE cta_marca_ws.* -- registro de la tabla de Web Service
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET g_usuario_cod       = ARG_VAL(1)
   LET g_pid               = ARG_VAL(2)
   LET g_proceso_cod       = ARG_VAL(3)
   LET g_opera_cod         = ARG_VAL(4)
   LET g_folio_liquida     = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG (g_usuario_cod CLIPPED|| ".AGRP14.log")

   DISPLAY "=INICIA AGRP14="
   DISPLAY " USUARIO       : ",g_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " FOLIO         : ",g_folio_liquida USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan las variables
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET v_v_nom_archivo       = "N/A" -- no aplica para este proceso
   LET v_i_estado            = 140 -- liquidado
   LET v_i_proceso_cod_folio = g_proc_cod_agr_liquidacion -- liquidación anualidad garantizada
   LET v_i_opera_cod_folio   = 1 -- preliquida saldo
   LET v_marca_entra         = 225 --marca para anualidad
   LET v_estado_marca        = 0 
   LET v_marca_causa         = 0
   LET v_i_tpo_originacion   = 4 -- Anualidades Garantizadas
   LET v_c_tpo_transferencia = "43" -- Anualidades Garantizadas
   LET v_c_f_liquida_aux     = TODAY USING "YYYYMMDD" -- fecha de liquidación con formato AAAAMMDD
   LET g_tpo_liq             = 2

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod      = "AGRL18"
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion crea la tabla temporal liquida_deudor
   CALL fn_crea_tbl_temp_liquida_deudor()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte1 = base.Channel.create()
   LET v_ch_arch_reporte2 = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_nom_archivo = "repSdosLiq_"  || v_c_f_liquida_aux || "_AGR." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA : ", v_v_nom_archivo

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_nom_archivo CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte1.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte1.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_nom_archivo = "Agr"  || g_folio_liquida USING "&&&&&" || ".rag"
   DISPLAY " REPORTE SALIDA (FECHA LIQUIDACIÓN): ", v_v_nom_archivo

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_nom_archivo CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte2.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte2.setDelimiter("")

   --se prepara la ejecucion del procedure que actualiza estado en transferencia 
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_act_edo_acre_his_id_acre(?,?)"
   PREPARE prp_actualiza_edo1 FROM v_s_qryTxt

   LET v_cre_acred = 1

   -- se procesan los registros de cre acreditado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado IN (130, 135, 138, 137, 330, 335)\n",
                    "    AND folio_liquida = ",g_folio_liquida,"\n",
                    "    AND tpo_originacion = ",v_i_tpo_originacion

   PREPARE prp_cre_acred FROM v_s_qryTxt
   DECLARE cur_his_trans CURSOR FOR prp_cre_acred

   FOREACH cur_his_trans INTO v_r_cre_acreditado[v_cre_acred].*
---DISPLAY v_r_cre_acreditado[v_cre_acred].*
   -- se valida el estado
      IF v_r_cre_acreditado[v_cre_acred].estado = 130 THEN
         LET v_i_estado = 140 -- liquidado

         -- se actualiza situación del registro correspondiente en tabla del WS a "marcada" (1)
         -- para los registros que están con situación "marca por confirmar" (0)
         UPDATE cta_marca_ws
            SET situacion = 2
          WHERE id_derechohabiente = v_r_cre_acreditado[v_cre_acred].id_derechohabiente
            AND id_origen = v_r_cre_acreditado[v_cre_acred].id_cre_acreditado
            --AND modulo_cod = "04" -- AGR
            AND tpo_credito = v_r_cre_acreditado[v_cre_acred].tpo_credito
            AND situacion = 1
      ELSE
         IF v_r_cre_acreditado[v_cre_acred].estado = 135 THEN
            LET v_i_estado = 145 -- liquidado remanentes
         ELSE
            IF v_r_cre_acreditado[v_cre_acred].estado = 137 OR
               v_r_cre_acreditado[v_cre_acred].estado = 330 OR
               v_r_cre_acreditado[v_cre_acred].estado = 335 THEN
               LET v_i_estado = 170 -- liquidado deudor crédito liquidado
            ELSE
               LET v_i_estado = 148 -- liquidado ministración
            END IF
         END IF
      END IF

      -- ejecuta procedure que actualiza estado en transferencia 
      EXECUTE prp_actualiza_edo1 USING v_r_cre_acreditado[v_cre_acred].id_cre_acreditado, v_i_estado

      -- se obtiene el nss para el derechohabiente en proceso
      CALL fn_obt_nss(v_r_cre_acreditado[v_cre_acred].id_derechohabiente) RETURNING v_r_archivo_rep.nss

      -- se obtienen los importes liquidados
      CALL fn_obtn_imp_lqdd(v_r_cre_acreditado[v_cre_acred].id_derechohabiente, v_r_cre_acreditado[v_cre_acred].folio_liquida)
      RETURNING v_r_archivo_rep.monto_acciones, v_r_archivo_rep.monto_pesos

      -- en caso de no haber encontrado importe, no se inserta el registro
      IF v_r_archivo_rep.monto_acciones IS NULL OR v_r_archivo_rep.monto_acciones = 0 OR
         v_r_archivo_rep.monto_pesos IS NULL OR v_r_archivo_rep.monto_pesos = 0 THEN
         CONTINUE FOREACH
      END IF

      -- se asignan los valores en el registro detalle
      LET v_r_det_rpt1.nss            = v_r_archivo_rep.nss
      LET v_r_det_rpt1.monto_acciones = (v_r_archivo_rep.monto_acciones * 1000000) USING "&&&&&&&&&&&&&&&"
      LET v_r_det_rpt1.monto_pesos    = (v_r_archivo_rep.monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_det_rpt1.nss, v_r_det_rpt1.monto_acciones, v_r_det_rpt1.monto_pesos

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte1.write([v_s_registro])

      -- se asignan los valores en el registro detalle
      LET v_r_det_rpt2.nss            = v_r_archivo_rep.nss
      LET v_r_det_rpt2.fec_liquida    = TODAY USING "yyyymmdd"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_det_rpt2.nss, v_r_det_rpt2.fec_liquida

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte2.write([v_s_registro])
   END FOREACH

   CALL v_r_cre_acreditado.deleteElement(v_cre_acred)

   -- se procesan los registros de cre uso garantia
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE folio_liquida = ",g_folio_liquida,"\n",
                    "    AND estado IN(130,325)\n",
                    "    AND tpo_transferencia = '",v_c_tpo_transferencia,"'"

---DISPLAY v_s_qryTxt

   PREPARE prp_liquida_uso FROM v_s_qryTxt
   DECLARE cur_liquida_uso CURSOR FOR prp_liquida_uso

   FOREACH cur_liquida_uso INTO v_r_cre_uso_garantia.*
       IF v_r_cre_uso_garantia.estado = 130 THEN
          LET v_i_estado = 140
       ELSE
          LET v_i_estado = 340
       END IF

      -- se actualiza estado en cre uso garantia
      UPDATE cre_uso_garantia
         SET estado = v_i_estado
       WHERE id_cre_uso_garantia = v_r_cre_uso_garantia.id_cre_uso_garantia

      -- se obtiene el nss para el derechohabiente en proceso
      CALL fn_obt_nss(v_r_cre_uso_garantia.id_derechohabiente) RETURNING v_r_archivo_rep.nss

      -- se obtienen los importes liquidados
      CALL fn_obtn_imp_lqdd(v_r_cre_uso_garantia.id_derechohabiente, v_r_cre_uso_garantia.folio_liquida)
      RETURNING v_r_archivo_rep.monto_acciones, v_r_archivo_rep.monto_pesos

      -- en caso de no haber encontrado importe, no se inserta el registro
      IF v_r_archivo_rep.monto_acciones IS NULL OR v_r_archivo_rep.monto_acciones = 0 OR
         v_r_archivo_rep.monto_pesos IS NULL OR v_r_archivo_rep.monto_pesos = 0 THEN
         CONTINUE FOREACH
      END IF

      -- se asignan los valores en el registro detalle
      LET v_r_det_rpt1.nss            = v_r_archivo_rep.nss
      LET v_r_det_rpt1.monto_acciones = (v_r_archivo_rep.monto_acciones * 1000000) USING "&&&&&&&&&&&&&&&"
      LET v_r_det_rpt1.monto_pesos    = (v_r_archivo_rep.monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_det_rpt1.nss, v_r_det_rpt1.monto_acciones, v_r_det_rpt1.monto_pesos

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte1.write([v_s_registro])

      -- se asignan los valores en el registro detalle
      LET v_r_det_rpt2.nss            = v_r_archivo_rep.nss
      LET v_r_det_rpt2.fec_liquida    = TODAY USING "yyyymmdd"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_det_rpt2.nss, v_r_det_rpt2.fec_liquida

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte2.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte1.close()

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte2.close()

   --se manda el reporte de liquidacion
   DISPLAY "SE GENERA REPORTE DE LIQUIDACIÓN"
   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/AGRP16 ",
                               g_usuario_cod, " ",                                                 
                               g_pid, " ",
                               g_proceso_cod, " ",
                               g_opera_cod, " ",
                               g_folio_liquida, " ",
                               "cta_movimiento", " ",
                               v_c_programa_cod --"AGRL18"

   --DISPLAY " v_s_comando ", v_s_comando
   RUN v_s_comando

   DISPLAY "PASA REGISTROS DE LIQUIDA DEUDOR A SALDO DEUDOR"
   WHENEVER ERROR CONTINUE
   -- pasar lo que hay en temporal de cre_saldo_deudor a la temporal de liquida_deudor
   INSERT INTO safre_tmp:tmp_liquida_deudor_agr SELECT * FROM safre_tmp:tmp_cre_saldo_deudor_agr

   IF SQLCA.SQLCODE < 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL INTENTAR PASAR LOS DATOS DE tmp_liquida a tmp cre_saldo", SQLCA.SQLCODE

      EXIT PROGRAM
   END IF
   WHENEVER ERROR STOP

   DISPLAY "SE LANZA LIQUIDACIÓN FONDO 72 AGR"
   -- se inicializan las variables para el proceso de liquidación fondo 72
   LET g_opera_cod = 3 -- liquida fondo 72 agr
   LET v_v_nom_archivo = "N/A" -- no aplica para este proceso

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el la liquidación de fondo de ahorro 72
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRP26 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO LIQUIDACIÓN"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET g_opera_cod = 4 -- genera archivo liquidación

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se crea el nombre del archivo a generar
   LET v_v_nom_archivo = "liq" || v_c_f_liquida_aux || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRS01 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " ",
                                g_tpo_liq, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO AMORTIZACIÓN"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET g_opera_cod = 5 -- genera archivo amortización

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid, g_proceso_cod, g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se crea el nombre del archivo a generar
   LET v_v_nom_archivo = "A" || v_c_f_liquida_aux || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRS02 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " ",
                                g_tpo_liq, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO CARGO CAPITAL"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET g_opera_cod = 6 -- genera archivo cargo a crédito

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se crea el nombre del reporte a generar
   LET v_v_nom_archivo = "A" || v_c_f_liquida_aux || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de cargo a capital
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRS03 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " ",
                                g_tpo_liq, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO AMORTIZACIÓN FONDO 72"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET g_opera_cod = 7 -- genera archivo amortización fondo 72

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se crea el nombre del archivo a generar
   LET v_v_nom_archivo = "A" || v_c_f_liquida_aux || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización fondo 72
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRS06 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

END MAIN

# Objetivo: Función que crea la tabla temporal de deudor
FUNCTION fn_crea_tbl_temp_liquida_deudor()

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_liquida_deudor_agr

   CREATE TABLE tmp_liquida_deudor_agr(id_cre_acreditado DECIMAL(9,0),
                                       folio_liquida     DECIMAL(9,0),
                                       f_liquida         DATE,
                                       movimiento        SMALLINT,
                                       id_referencia     DECIMAL(9,0),
                                       monto_aivs        DECIMAL(22,2),
                                       monto_pesos       DECIMAL(22,2),
                                       f_movimiento      DATE)

   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que genera selecciona y regresa el importe liquidado (pesos y acciones) para un
#          nss que entra como parámentro
FUNCTION fn_obtn_imp_lqdd(p_d_id_derechohab, p_d_folio_liquida)

   DEFINE p_d_id_derechohab         DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE p_d_folio_liquida         DECIMAL(9,0) -- folio de liquidación
   DEFINE v_d_tot_mto_acc           DECIMAL(18,6) -- monto total en acciones
   DEFINE v_d_tot_mto_pss           DECIMAL(14,2) -- monto total en pesos
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar

   -- se obtiene los importes para el nss en proceso
   LET v_s_qryTxt = " SELECT ABS(SUM(monto_acciones)), ABS(SUM(monto_pesos))\n",
                    "   FROM cta_movimiento\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND id_derechohabiente = ",p_d_id_derechohab

   PREPARE prp_slct_sumMtoAcc_sumMtoPss FROM v_s_qryTxt
   EXECUTE prp_slct_sumMtoAcc_sumMtoPss INTO v_d_tot_mto_acc, v_d_tot_mto_pss

   RETURN v_d_tot_mto_acc, v_d_tot_mto_pss

END FUNCTION

#Objetivo: Función que obtiene el nss correspondiente al id derechohabiente que entra como
#          parámetro
FUNCTION fn_obt_nss(p_d_id_derechohab)

   DEFINE p_d_id_derechohab         LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente
   DEFINE v_c_nss                   LIKE afi_derechohabiente.nss -- NSS del trabajador
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar

   -- se obtiene los importes para el nss en proceso
   LET v_s_qryTxt = " SELECT nss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_d_id_derechohab

   PREPARE prp_slct_nss_afi FROM v_s_qryTxt
   EXECUTE prp_slct_nss_afi INTO v_c_nss

   RETURN v_c_nss

END FUNCTION
