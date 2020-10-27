--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

######################################################################
#Modulo             => GRT                                           #
#Programa           => GRTE03                                        #
#Objetivo           => Programa que permite la integración del       #
#                      archivo de Saldos Transferidos para el módulo #
#                      Solicitud de Saldo en Garantía                #
#Autor              => Daniel Buendia, EFP                           #
#Fecha inicio       => 23 Abril 2012                                 #
#Autor              => Mauro Muñiz Caballero                         #
#Fecha modificación => 27 de julio de 2016                           #
#                      Generación de notificaciones                  #
######################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

#Objetivo: Funcion que realiza la integracion para el archivo que entra como parametro
MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- número de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar

   DEFINE v_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operación
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_lote                 LIKE cre_ctr_archivo.lote -- lote
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_si_cod_error            SMALLINT -- contiene el código de error en caso de ocurrir
   DEFINE r_isam_err                INTEGER
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_c_nss                   LIKE afi_derechohabiente.nss
   DEFINE v_fecha                   DATE 
   DEFINE v_ind_marca               SMALLINT

   -- se recuperan los parámetros que envía el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE03.log")

   DISPLAY "=INICIA GRTE03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion   = 9 -- Saldos Transferidos
   LET v_si_id_proceso = g_id_proceso_grt -- Solicitud de Saldos en Garantía 43 bis
   LET v_fecha         = TODAY 
   LET v_ind_marca     = NULL

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)

   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, cons_lote\n",
                    "   FROM safre_tmp:tmp_sdo_transf_enc_grt"

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
   END IF

   DISPLAY " INTEGRA SALDOS"
   -- se crea la sentencia que ejecuta el procedure que integra archivo de saldos transferidos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_grt_integra_sdos_tranf(?,?,?,?,?)"

   PREPARE prp_integra_sdos_transf FROM v_s_qryTxt
   EXECUTE prp_integra_sdos_transf USING p_v_usuario,
                                         p_v_arch_proceso,
                                         p_d_folio,
                                         v_d_id_cre_ctr_arch,
                                         p_i_proceso_cod
                                    INTO r_si_cod_error,
                                         r_isam_err,
                                         r_c_msj,
                                         r_c_nss

   IF r_si_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE INTEGRACIÓN"
      DISPLAY "CÓD. ERROR : ",r_si_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error y se marca como rechazado la operación
      LET r_si_cod_error = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   DISPLAY " INSERTA CTA CRÉDITO"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_inserta_cta_credito(?,?)"

   PREPARE prp_insrt_cta_credito FROM v_s_qryTxt
   EXECUTE prp_insrt_cta_credito USING p_i_proceso_cod, v_d_id_cre_ctr_arch

   DISPLAY "GENERA NOTIFICACIONES"
   CALL fn_notifica_proceso(p_d_folio, p_i_proceso_cod, p_v_usuario)

   DISPLAY ""

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOTAL REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " TOTAL SIN ORIGEN:",v_r_cre_ctr_archivo.tot_sin_origen

   -- Ejecuta función de confirma marca
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_confirma_marca_grt(?)"

   PREPARE prp_confirma_marca FROM v_s_qryTxt
   EXECUTE prp_confirma_marca USING v_fecha 
                                INTO v_ind_marca

   IF (v_ind_marca <> 0) THEN
      DISPLAY "OCURRIÓ UN ERROR EN LA EJECUCIÓN DE CONFIRMACIÓN DE MARCA" 
   END IF 

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(p_i_proceso_cod, p_i_opera_cod)

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF
END MAIN

#Objetivo: Función que genera el reporte de tipo de crédito
FUNCTION fn_genera_rep_proc(p_i_proceso_cod, p_i_opera_cod)

   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operacion
   DEFINE v_v_arch_salida           VARCHAR(100) -- nombre del archivo de salida
   DEFINE v_v_ruta_archivo          VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_ch_arch_reporte         BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_c_nss                   CHAR(11)

   DEFINE v_r_rpt_tpo_credito RECORD
      nss                           CHAR(11),
      tpo_credito                   CHAR(3)
   END RECORD

   DEFINE v_si_tpo_credito          SMALLINT -- tipo de credito
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocará el archivo
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_c_fec_hoy       = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_arch_salida = "Grt" || v_c_fec_hoy || "." || v_c_extension
   DISPLAY " REPORTE SALIDA (TIPO CRÉDITO): ",v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'grt'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT UNIQUE nss\n",
                    "   FROM safre_tmp:tmp_sdo_transf_det_grt"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_c_nss
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se obtiene el folio de liquidación y el identificador del derechohabiente
      LET v_s_qryTxt = " SELECT FIRST 1 tpo_credito\n",
                       "   FROM cre_acreditado\n",
                       --"  WHERE estado >= 20\n",
                       --"    AND edo_procesar = 120\n",
                       "  WHERE estado IN(20,140)\n",
                       "    AND edo_procesar IN(60, 120)\n",
                       "    AND id_derechohabiente IN (\n",
                       "        SELECT id_derechohabiente\n",
                       "          FROM afi_derechohabiente\n",
                       "         WHERE nss = '",v_c_nss,"')\n",
                       "    AND tpo_originacion = 2\n",
                       "  ORDER BY f_otorga DESC"

      PREPARE prp_slct_idDer_folLiq FROM v_s_qryTxt
      EXECUTE prp_slct_idDer_folLiq INTO v_si_tpo_credito

      -- se asignan los valores al registro a insertar en archivo (tipo de crédito)
      LET v_r_rpt_tpo_credito.nss = v_c_nss
      LET v_r_rpt_tpo_credito.tpo_credito = v_si_tpo_credito USING "&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_rpt_tpo_credito.nss,
                         v_r_rpt_tpo_credito.tpo_credito

      -- se escribe el registro (montos iguales) en el archivo
      CALL v_ch_arch_reporte.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte.close()

END FUNCTION
