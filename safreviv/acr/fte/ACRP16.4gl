--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

##########################################################################
#Modulo            => ACR                                                #
#Programa          => ACRP16                                             #
#Objetivo          => Programa que ejecuta los procesos que generan los  #
#                     archivos de salida durante la liquidación ACR      #
#Autor             => Daniel Buendia, EFP                                #
#Fecha inicio      => 25 Abril 2012                                      #
#Modifica          => Mauro Muñiz Caballero                              #
#                     18/08/2017                                         #
#                     Liquidación deudor crédito liquidado               #
##########################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"

MAIN

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_pid                     LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE p_proceso_cod             LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_opera_cod               LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE p_folio_liquida           LIKE cta_movimiento.folio_liquida -- folio de liquidación
   DEFINE p_v_arch_proceso          LIKE bat_ctr_operacion.nom_archivo -- archivo del proceso
   DEFINE v_opcion_fun              SMALLINT -- opcion para la funcion general
   DEFINE v_r_cre_acreditado        RECORD LIKE cre_acreditado.*
   DEFINE v_i_estado                SMALLINT --estado al que se va a actualizar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_dt_f_liquidacion        DATE -- fecha de liquidación
   DEFINE v_c_liquidacion           CHAR(8) -- fecha de liquidación con formato YYYYMMDD
   DEFINE v_marca_entra             SMALLINT --marca de acreditado liquidado
   DEFINE v_estado_marca            SMALLINT --estado de la marca
   DEFINE v_marca_causa             SMALLINT --marca causa
   DEFINE v_edo_desmarca            SMALLINT --guarda el valor retornado por la funcion de desmarca
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombrel del programa origen
   DEFINE v_i_proceso_cod_folio     LIKE cat_proceso.proceso_cod -- codigo del proceso para el folio
   DEFINE v_i_opera_cod_folio       LIKE cat_operacion.opera_cod -- codigo de operacion para el folio
   DEFINE v_desc_desmarca           LIKE cat_rch_marca.rch_desc --descripcion del retorno de la desmarca
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_i_tpo_originacion       LIKE cre_acreditado.tpo_originacion -- tipo de originación
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
      nss                            CHAR(11),
      fec_liquida                    CHAR(8)
   END RECORD

   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod       = ARG_VAL(1)
   LET p_pid               = ARG_VAL(2)
   LET p_proceso_cod       = ARG_VAL(3)
   LET p_opera_cod         = ARG_VAL(4)
   LET p_folio_liquida     = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACRP16.log")

   DISPLAY "=INICIA ACRP16="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",p_pid
   DISPLAY " FOLIO         : ",p_folio_liquida USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan las variables
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET p_v_arch_proceso      = "N/A" -- no aplica para este proceso
   LET v_i_proceso_cod_folio = g_proc_cod_acr_liquidacion -- liquidación transferencia acreditados
   LET v_i_opera_cod_folio   = 1 -- preliquida saldo acreditados
   LET v_marca_entra         = 221 --marca para acreditados
   LET v_estado_marca        = 0
   LET v_marca_causa         = 0
   LET v_dt_f_liquidacion    = TODAY -- fecha de liquidación
   LET v_c_liquidacion       = v_dt_f_liquidacion USING "YYYYMMDD" -- fecha liquida con formato
   LET v_i_tpo_originacion   = 1 -- Transferencia de Acreditados

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion crea la tabla temporal liquida_deudor
   CALL fn_crea_tbl_temp_liquida_deudor()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio

   -- se crea el manejador de los archivos
   LET v_ch_arch_reporte1 = base.Channel.create()
   LET v_ch_arch_reporte2 = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET p_v_arch_proceso = "repSdosLiq_"  || v_c_liquidacion || "_ACR." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA (MONTOS LIQUIDADOS): ", p_v_arch_proceso

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte1.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte1.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET p_v_arch_proceso = "Acr"  || p_folio_liquida USING "&&&&&" || ".rta"
   DISPLAY " REPORTE SALIDA (FECHA LIQUIDACIÓN): ", p_v_arch_proceso

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte2.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte2.setDelimiter("")

   --se prepara la ejecucion del procedure que actualiza estado en transferencia 
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_act_edo_acre_his_id_acre(?,?)"
   PREPARE prp_actualiza_edo1 FROM v_s_qryTxt

   --se prepara la ejecucion de la funcion que realiza la desmarca al liquidar
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE prp_desmarca_cta FROM v_s_qryTxt

   -- se procesan los registros de cre acreditado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado IN (130,135,137)\n",
                    "    AND folio_liquida = ",p_folio_liquida,"\n",
                    "    AND tpo_originacion = ",v_i_tpo_originacion

   PREPARE prp_cre_acred FROM v_s_qryTxt
   DECLARE cur_his_trans CURSOR FOR prp_cre_acred

   FOREACH cur_his_trans INTO v_r_cre_acreditado.*
      -- se valida el estado
      IF v_r_cre_acreditado.estado = 130 THEN
         LET v_i_estado = 140 -- liquidado

         -- se actualiza situación del registro correspondiente en tabla del WS a "marcada" (1)
         -- para los registros que están con situación "marca por confirmar" (0)
         UPDATE cta_marca_ws
            SET situacion = 2
          WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
            AND id_origen = v_r_cre_acreditado.id_cre_acreditado
            AND tpo_credito = v_r_cre_acreditado.tpo_credito
            AND modulo_cod = "03" -- ACR
            AND situacion = 1
      ELSE
         IF v_r_cre_acreditado.estado = 135 THEN
            LET v_i_estado = 145 -- liquidado remanentes
         ELSE
            LET v_i_estado = 170 -- liquidado deudor crédito liquidado
         END IF
      END IF

      -- ejecuta procedure que actualiza estado en transferencia
      EXECUTE prp_actualiza_edo1 USING v_r_cre_acreditado.id_cre_acreditado, v_i_estado

      -- ejecuta funcion que realiza la desmarca al liquidar
      EXECUTE prp_desmarca_cta USING v_r_cre_acreditado.id_derechohabiente,
                                     v_marca_entra,
                                     v_r_cre_acreditado.id_cre_acreditado,
                                     v_estado_marca,
                                     v_marca_causa,
                                     p_usuario_cod,
                                     p_proceso_cod
                               INTO  v_edo_desmarca

      IF v_edo_desmarca <> 0 THEN
         -- se consulta la descripción del código de error que regresa la función de desmarca
         LET v_s_qryTxt = " SELECT rch_desc\n",
                          "   FROM cat_rch_marca\n",
                          "  WHERE rch_cod = ",v_edo_desmarca

         PREPARE prp_busca_rch_desc FROM v_s_qryTxt
         EXECUTE prp_busca_rch_desc INTO v_desc_desmarca

         DISPLAY "DESMARCA: " ,v_desc_desmarca, " Derechohabiente: ",v_r_cre_acreditado.id_derechohabiente
      END IF

      -- se obtiene el nss para el derechohabiente en proceso
      CALL fn_obt_nss(v_r_cre_acreditado.id_derechohabiente) RETURNING v_r_archivo_rep.nss

      -- se obtienen los importes liquidados
      CALL fn_obtn_imp_lqdd(v_r_cre_acreditado.id_derechohabiente, v_r_cre_acreditado.folio_liquida)
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
      LET v_r_det_rpt2.nss         = v_r_archivo_rep.nss
      LET v_r_det_rpt2.fec_liquida = TODAY USING "yyyymmdd"

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
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/ACRP34 ",
                               p_usuario_cod, " ",
                               p_pid, " ",
                               p_proceso_cod, " ",
                               p_opera_cod, " ",
                               p_folio_liquida, " ",
                               "cta_movimiento", " ",
                               v_c_programa_cod --"ACRL20"

   --DISPLAY " v_s_comando ", v_s_comando
   RUN v_s_comando

   DISPLAY "PASA REGISTROS DE LIQUIDA DEUDOR A SALDO DEUDOR"
   WHENEVER ERROR CONTINUE
   -- pasar lo que hay en temporal de cre_saldo_deudor a la temporal de liquida_deudor
   INSERT INTO safre_tmp:tmp_liquida_deudor_acr SELECT * FROM safre_tmp:tmp_cre_saldo_deudor_acr

   IF SQLCA.SQLCODE < 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL INTENTAR PASAR LOS DATOS DE tmp_liquida a tmp cre_saldo", SQLCA.SQLCODE

      EXIT PROGRAM
   END IF
   WHENEVER ERROR STOP

   DISPLAY "SE LANZA LIQUIDACIÓN FONDO 72"
   -- se inicializan las variables para el proceso de liquidación fondo 72
   LET p_opera_cod = 3 -- liquida fondo 72 agr
   LET p_v_arch_proceso = "N/A" -- no aplica para este proceso

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           v_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta la opeacion de liquidación fondo ahorro 72
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/ACRP21 ",
                                p_usuario_cod, " ",
                                p_pid, " ",
                                p_proceso_cod, " ",
                                p_opera_cod, " ",
                                p_folio_liquida, " ",
                                p_v_arch_proceso, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                                " 2>&1"

   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO LIQUIDACIÓN"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET p_opera_cod = 4 -- genera archivo liquidación

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se asigna el nombre del archivo
   LET p_v_arch_proceso = "A" || v_c_liquidacion || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           v_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/ACRS10 ",
                                p_usuario_cod, " ",
                                p_pid, " ",
                                p_proceso_cod, " ",
                                p_opera_cod, " ",
                                p_folio_liquida, " ",
                                p_v_arch_proceso, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                                " 2>&1"

   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO AMORTIZACIÓN"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET p_opera_cod = 5 -- genera archivo amortización

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se asigna el nombre del archivo
   LET p_v_arch_proceso = "A" || v_c_liquidacion || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           v_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/ACRS11 ",
                                p_usuario_cod, " ",
                                p_pid, " ",
                                p_proceso_cod, " ",
                                p_opera_cod, " ",
                                p_folio_liquida, " ",
                                p_v_arch_proceso, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO CARGO CAPITAL"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET p_opera_cod = 6 -- genera archivo cargo a crédito

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se asigna el nombre del archivo
   LET p_v_arch_proceso = "A" || v_c_liquidacion || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           v_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de cargo a capital
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/ACRS12 ",
                                p_usuario_cod, " ",
                                p_pid, " ",
                                p_proceso_cod, " ",
                                p_opera_cod, " ",
                                p_folio_liquida, " ",
                                p_v_arch_proceso, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO AMORTIZACIÓN FONDO 72"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET p_opera_cod = 7 -- genera archivo amortización fondo 72

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se asigna el nombre del archivo
   LET p_v_arch_proceso = "A" || v_c_liquidacion || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           v_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización fondo 72
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/ACRS15 ",
                                p_usuario_cod, " ",
                                p_pid, " ",
                                p_proceso_cod, " ",
                                p_opera_cod, " ",
                                p_folio_liquida, " ",
                                p_v_arch_proceso, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                                " 2>&1"

   -- se ejecuta el comando armado
   RUN v_s_comando

END MAIN

# Objetivo: Función que crea la tabla temporal de deudor
FUNCTION fn_crea_tbl_temp_liquida_deudor()

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_liquida_deudor_acr
   CREATE TABLE tmp_liquida_deudor_acr(id_cre_acreditado DECIMAL(9,0),
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
                    "  WHERE id_derechohabiente = ",p_d_id_derechohab,"\n",
                    "    AND folio_liquida = ",p_d_folio_liquida

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
