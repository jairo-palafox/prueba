--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRE14                                        #
#Objetivo          =Programa para integrar el archivo de           #
#                   solicitudes no atentidas que ha sido validado  #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>20 ENERO 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

#Objetivo: Funcion que realiza la integracion del archivo de solicitudes no atendidas
MAIN

   DEFINE p_v_usuario         LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid             LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod     LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod       LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio           LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso    VARCHAR(100) -- nombre del archivo a integrar
   DEFINE v_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados -- ruta listados de bat

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros           LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados           LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados          LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen          LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_s_comando         STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt          STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_operacion       LIKE cre_ctr_archivo.operacion -- operacion
   DEFINE v_dt_f_lote         LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_lote           LIKE cre_ctr_archivo.lote -- lote
   DEFINE v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_cod_error       SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err          INTEGER
   DEFINE r_c_msj             VARCHAR(250)
   DEFINE r_c_nss             LIKE afi_derechohabiente.nss

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRE14.log")

   DISPLAY "=INICIA ACRE14="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion   = 14 -- Solicitudes no atendidas
   LET v_si_id_proceso = g_id_proceso_acr -- Transferencia de Acreditados

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)

   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_no_aten()

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, consecutivo_lote\n",
                    "   FROM safre_tmp:tmp_acr_no_at_enc"

   PREPARE prp_fec_lote FROM v_s_qryTxt
   EXECUTE prp_fec_lote INTO v_dt_f_lote, v_si_lote

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE lote = ",v_si_lote,"\n",
                    "    AND f_lote = '",v_dt_f_lote,"'\n",
                    "    AND id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO v_d_id_cre_ctr_arch

   -- se verifica si fue posible obtener el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   ELSE
      DISPLAY " ID ARCHIVO    : ",v_d_id_cre_ctr_arch
   END IF

   DISPLAY " INTEGRA NO ATENDIDAS"
   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de solicitudes no atendidas
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_acr_integra_no_aten(?,?,?,?)"

   PREPARE prp_integra_solic_no_aten FROM v_s_qryTxt
   EXECUTE prp_integra_solic_no_aten USING p_v_usuario,
                                           p_v_arch_proceso,
                                           p_d_folio,
                                           v_d_id_cre_ctr_arch
                                      INTO r_b_cod_error,
                                           r_isam_err,
                                           r_c_msj,
                                           r_c_nss

   IF r_b_cod_error <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de Integración: ",r_b_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_cod_error = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOTAL REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " TOTAL SIN ORIGEN:",v_r_cre_ctr_archivo.tot_sin_origen

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(v_d_id_cre_ctr_arch, p_i_proceso_cod, p_i_opera_cod)

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   DISPLAY " EJECUTA CONCILIACIÓN"
   -- se crea el comando que ejecuta la conciliación de Rechazo de Saldos
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRP28 ",
                                           p_v_usuario, " ",
                                           p_d_pid, " ",
                                           p_i_proceso_cod, " ",
                                           p_i_opera_cod, " ",
                                           p_d_folio, " ",
                                           p_v_arch_proceso, " ",
                                           " 1>> ",v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando

END MAIN

#Objetivo: Función que crea la tabla temporal de la integración de no atendidas
FUNCTION fn_crea_tmp_no_aten()

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_deudor_no_aten

   WHENEVER ERROR STOP

   CREATE TABLE tmp_deudor_no_aten(id_cre_acreditado DECIMAL(9,0),
                                   id_derechohabiente DECIMAL(9,0),
                                   nss CHAR(11))

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que genera el reporte de los registros integrados
FUNCTION fn_genera_rep_proc(p_id_cre_ctr_archivo, p_i_proceso_cod, p_i_opera_cod)

   DEFINE p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod        LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_v_arch_salida      VARCHAR(100) -- nombre del archivo de salida
   DEFINE v_v_ruta_archivo     VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_c_fec_hoy          CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_c_extension        LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_ch_arch_reporte    BASE.CHANNEL -- manejador de apuntador hacia archivo

   DEFINE v_r_archivo_rep RECORD
      nss                      CHAR(11),
      tpo_transf               CHAR(2),
      monto_acciones           DECIMAL(18,6),
      monto_pesos              DECIMAL(14,2)
   END RECORD

   DEFINE v_r_detalle RECORD
      nss                      CHAR(11),
      monto_acciones           CHAR(15),
      monto_pesos              CHAR(15)
   END RECORD

   DEFINE v_d_id_derechohab    DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE v_d_folio_liquida    DECIMAL(9,0) -- folio de liquidación
   DEFINE v_c_modulo_cod       CHAR(2) -- módulo cod
   DEFINE v_s_registro         STRING -- registro a insertar
   DEFINE v_c_ruta_envio       LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_i_contrador_reg    INTEGER -- contrador de registros
   DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_c_ruta_listado     LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_referencia         DECIMAL(9,0)

   -- se inicializan variables
   LET v_c_fec_hoy       = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros
   LET v_d_folio_liquida = 0

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_arch_salida = "repSdosLiq_"  || v_c_fec_hoy || "_ACR." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA : ", v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida CLIPPED

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT nss_infonavit, tpo_transf\n",
                    "   FROM safre_tmp:tmp_acr_no_at"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_r_archivo_rep.nss, v_r_archivo_rep.tpo_transf
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se verifica si el tipo de transferencia corresponde a TA
      IF v_r_archivo_rep.tpo_transf <> "43" THEN
         -- se obtiene el folio de liquidación y el identificador del derechohabiente
         LET v_s_qryTxt = " SELECT FIRST 1 id_derechohabiente, folio_liquida\n",
                          "   FROM cre_acreditado\n",
                          "  WHERE estado >= 20\n",
                          "    AND edo_procesar = 70\n",
                          "    AND id_derechohabiente IN (\n",
                          "        SELECT id_derechohabiente\n",
                          "          FROM afi_derechohabiente\n",
                          "         WHERE nss = '",v_r_archivo_rep.nss,"')\n",
                          "    AND tpo_originacion = 1\n",
                          "  ORDER BY f_otorga DESC"

         PREPARE prp_slct_idDer_folLiq FROM v_s_qryTxt
         EXECUTE prp_slct_idDer_folLiq INTO v_d_id_derechohab, v_d_folio_liquida

---DISPLAY "TA, NSS: ", v_r_archivo_rep.nss,"  ID: ", v_d_id_derechohab,"  Folio liq: ",v_d_folio_liquida

         -- si no se encontro registro en la tabla maestro se asume que fue rechazado
         IF v_d_folio_liquida IS NULL OR v_d_folio_liquida = 0 THEN
            CONTINUE FOREACH
         END IF

         -- se obtienen los importes liquidados
         CALL fn_obtn_imp_lqdd(v_d_id_derechohab, v_d_folio_liquida) RETURNING v_r_archivo_rep.monto_acciones, v_r_archivo_rep.monto_pesos

         -- en caso de no haber encontrado importe, no se inserta el registro
         IF v_r_archivo_rep.monto_acciones IS NULL OR v_r_archivo_rep.monto_acciones = 0 OR
            v_r_archivo_rep.monto_pesos IS NULL OR v_r_archivo_rep.monto_pesos = 0 THEN  
            CONTINUE FOREACH
         END IF

         -- se asignan los valores en el registro detalle
         LET v_r_detalle.nss            = v_r_archivo_rep.nss
         LET v_r_detalle.monto_acciones = (v_r_archivo_rep.monto_acciones * 1000000) USING "&&&&&&&&&&&&&&&"
         LET v_r_detalle.monto_pesos    = (v_r_archivo_rep.monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

         -- se concatenan los campos a insertar
         LET v_s_registro = v_r_detalle.nss, v_r_detalle.monto_acciones, v_r_detalle.monto_pesos

         -- se escribe el registro (detalle) en el archivo
         CALL v_ch_arch_reporte.write([v_s_registro])

         LET v_d_folio_liquida = 0
      ELSE
         LET v_d_folio_liquida = 0

         -- El tipo de transferencia corresponde a AG. Se obtiene el módulo ("AG", "UA")
         LET v_s_qryTxt = " SELECT id_derechohabiente, modulo_cod, id_referencia\n",
                          "   FROM safre_tmp:tmp_agr_solic_sdo2\n",
                          "  WHERE nss = '",v_r_archivo_rep.nss,"'"

         PREPARE prp_slct_idDer_modCod FROM v_s_qryTxt
         DECLARE cur_slct_idDer_modCod CURSOR FOR prp_slct_idDer_modCod

         FOREACH cur_slct_idDer_modCod INTO v_d_id_derechohab, v_c_modulo_cod, v_referencia
            -- se valida el módulo
            IF v_c_modulo_cod = "AG" THEN
               -- se obtiene el folio de liquidación y el identificador del derechohabiente
               LET v_s_qryTxt = " SELECT folio_liquida\n",
                                "   FROM cre_acreditado\n",
                                "  WHERE id_cre_acreditado =", v_referencia
            ELSE
               -- se obtiene el folio de liquidación y el identificador del derechohabiente
               LET v_s_qryTxt = " SELECT FIRST 1 folio_liquida\n",
                                "   FROM cre_uso_garantia\n",
                                "  WHERE estado >= 140\n",
                                "    AND edo_procesar = 70\n",
                                "    AND id_derechohabiente = ",v_d_id_derechohab,"\n",
                                "    AND tpo_transferencia = '43'\n",
                                "    AND id_cre_ctr_archivo IN (\n",
                                "        SELECT id_cre_ctr_archivo\n",
                                "          FROM cre_ctr_archivo\n",
                                "         WHERE operacion NOT IN (1,6,9,14))\n",
                                "  ORDER BY folio_liquida DESC"
            END IF

            PREPARE prp_slct_folLiq FROM v_s_qryTxt
            EXECUTE prp_slct_folLiq INTO v_d_folio_liquida

---DISPLAY "AG, NSS: ", v_r_archivo_rep.nss,"  ID: ", v_d_id_derechohab,"  Folio liq: ",v_d_folio_liquida
            -- si no se encontro registro en la tabla maestro se asume que fue rechazado
            IF v_d_folio_liquida IS NULL OR v_d_folio_liquida = 0 THEN
               CONTINUE FOREACH
            END IF

            -- se obtienen los importes liquidados
            CALL fn_obtn_imp_lqdd(v_d_id_derechohab, v_d_folio_liquida) RETURNING v_r_archivo_rep.monto_acciones, v_r_archivo_rep.monto_pesos

            -- en caso de no haber encontrado importe, no se inserta el registro
            IF v_r_archivo_rep.monto_acciones IS NULL OR v_r_archivo_rep.monto_acciones = 0 OR
               v_r_archivo_rep.monto_pesos IS NULL OR v_r_archivo_rep.monto_pesos = 0 THEN  
               CONTINUE FOREACH
            END IF

            -- se asignan los valores en el registro detalle
            LET v_r_detalle.nss            = v_r_archivo_rep.nss
            LET v_r_detalle.monto_acciones = (v_r_archivo_rep.monto_acciones * 1000000) USING "&&&&&&&&&&&&&&&"
            LET v_r_detalle.monto_pesos    = (v_r_archivo_rep.monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

            -- se concatenan los campos a insertar
            LET v_s_registro = v_r_detalle.nss, v_r_detalle.monto_acciones, v_r_detalle.monto_pesos

            -- se escribe el registro (detalle) en el archivo
            CALL v_ch_arch_reporte.write([v_s_registro])

               LET v_d_folio_liquida = 0
         END FOREACH
      END IF
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte.close()
END FUNCTION

#Objetivo: Función que selecciona y regresa el importe liquidado (pesos y acciones) para un
#          nss que entra como parámentro
FUNCTION fn_obtn_imp_lqdd(p_d_id_derechohab, p_d_folio_liquida)

   DEFINE p_d_id_derechohab DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE p_d_folio_liquida DECIMAL(9,0) -- folio de liquidación
   DEFINE v_monto_acciones  DECIMAL(18,6) -- monto total en acciones
   DEFINE v_monto_pesos     DECIMAL(14,2) -- monto total en pesos
   DEFINE v_s_qryTxt        STRING -- guarda una sentencia SQL a ejecutar

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_obtiene_mov_liq(?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING p_d_id_derechohab,
                             p_d_folio_liquida
                        INTO v_monto_acciones, v_monto_pesos

   RETURN v_monto_acciones, v_monto_pesos

END FUNCTION
