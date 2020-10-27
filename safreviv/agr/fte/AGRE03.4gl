#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRE03                                        #
#Objetivo          => Programa que permite la integraci�n del       #
#                     archivo de saldos transferidos del m�dulo de  #
#                     Anualidades Garantizadas                      #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 04 Abril 2012                                 #
#Modifica:         => Mauro Mu�iz Caballero                         #
#Fecha modif:      => 9 de noviembre de 2015                        #
#Adecuaci�n        => Eliminaci�n de adelantos                      #
#####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

#Objetivo: Funcion que realiza la integracion para el archivo que entra como parametro
GLOBALS

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE v_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_f_f_movimiento    DATE -- fecha de movimiento
   DEFINE v_s_comando         STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt          STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_operacion       LIKE cre_ctr_archivo.operacion -- operacion
   DEFINE v_dt_f_lote         LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_lote           LIKE cre_ctr_archivo.lote -- lote
   DEFINE v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin -- ruta del bin del m�dulo
   DEFINE r_c_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta listados del m�dulo
   DEFINE r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err          INTEGER 
   DEFINE r_c_msj             VARCHAR(250)
   DEFINE r_c_nss             LIKE afi_derechohabiente.nss
   DEFINE v_tot_liq           DECIMAL(10,0) -- total registros con cr�dito liquidado pendientes de conciliaci�n

END GLOBALS

MAIN

   -- se recuperan los par�metros que env�a el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRE03.log")

   DISPLAY "=INICIA AGRE03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_f_f_movimiento = TODAY - DAY(TODAY) + 1
   LET v_f_f_movimiento = v_f_f_movimiento + 1 UNITS MONTH
   LET v_tot_liq        = 0

   DISPLAY " FECHA MOVIMIENTO: ",v_f_f_movimiento

   LET v_i_operacion   = 9 -- Saldos Transferidos
   LET v_si_id_proceso = g_id_proceso_agr -- Anualidades Garantizadas

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)

   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_sdos_transf()

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, con_lote_dia\n",
                    "   FROM safre_tmp:tmp_sdo_transf_enc_agr"

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
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_sdos_tranf(?,?,?,?,?)"

   PREPARE prp_integra_sdos_transf FROM v_s_qryTxt
   EXECUTE prp_integra_sdos_transf USING p_v_usuario,
                                         p_v_arch_proceso,
                                         p_d_folio,
                                         v_d_id_cre_ctr_arch,
                                         v_f_f_movimiento
                                    INTO r_b_valida,
                                         r_isam_err,
                                         r_c_msj,
                                         r_c_nss,
                                         v_tot_liq

   IF r_b_valida <> 0 THEN
      DISPLAY " Ocurri� un error durante el proceso de Integraci�n: "
      DISPLAY "ERROR      : ",r_b_valida
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurri� un error y se marca como rechazado la operaci�n
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para informaci�n del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOTAL REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " TOTAL SIN ORIGEN:",v_r_cre_ctr_archivo.tot_sin_origen
   DISPLAY " TOTAL LIQUIDADOS:",v_tot_liq


   -- se invoca la funci�n que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(v_d_id_cre_ctr_arch, p_i_proceso_cod, p_i_opera_cod)

   -- se obtiene la ruta bin y de listados del m�dulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   DISPLAY " EJECUTA CONCILIACI�N"
   -- se crea el comando que ejecuta la conciliaci�n de Rechazo de Saldos
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/AGRP06 ",
                                           p_v_usuario, " ",
                                           p_d_pid, " ",
                                           p_i_proceso_cod, " ",
                                           p_i_opera_cod, " ",
                                           p_d_folio, " ",
                                           p_v_arch_proceso,
                                           " 1>> ",v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando
END MAIN

#Objetivo: Funci�n que crea la tabla temporal de la integraci�n de saldos transferidos
FUNCTION fn_crea_tmp_sdos_transf()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_deudor_saldo_agr

   WHENEVER ERROR STOP

   CREATE TABLE tmp_deudor_saldo_agr(id_cre_acreditado DECIMAL(9,0),
                                     id_derechohabiente DECIMAL(9,0),
                                     nss CHAR(11))

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Funci�n que genera el reporte de los registros integrados
FUNCTION fn_genera_rep_proc(p_id_cre_ctr_archivo, p_i_proceso_cod, p_i_opera_cod)

   DEFINE p_id_cre_ctr_archivo      LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- c�digo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- c�digo de la operaci�n
   DEFINE v_v_arch_salida1          VARCHAR(100) -- nombre del archivo de salida1
   DEFINE v_v_arch_salida2          VARCHAR(100) -- nombre del archivo de salida2
   DEFINE v_v_arch_salida3          VARCHAR(100) -- nombre del archivo de salida3
   DEFINE v_v_arch_salida4          VARCHAR(100) -- nombre del archivo de salida4
   DEFINE v_v_arch_salida5          VARCHAR(100) -- nombre del archivo de salida5
   DEFINE v_v_ruta_archivo          VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensi�n del archivo
   DEFINE v_ch_arch_reporte1        BASE.CHANNEL -- manejador de apuntador hacia archivo1
   DEFINE v_ch_arch_reporte2        BASE.CHANNEL -- manejador de apuntador hacia archivo2
   DEFINE v_ch_arch_reporte3        BASE.CHANNEL -- manejador de apuntador hacia archivo3
   DEFINE v_ch_arch_reporte4        BASE.CHANNEL -- manejador de apuntador hacia archivo4
   DEFINE v_ch_arch_reporte5        BASE.CHANNEL -- manejador de apuntador hacia archivo5

   DEFINE v_r_archivo_rep RECORD
      nss                           CHAR(11),
      mto_acc_viv                   DECIMAL(18,6)
   END RECORD

   DEFINE v_r_det_mto_igl RECORD
      nss                           CHAR(11),
      monto_acciones                CHAR(15),
      monto_pesos                   CHAR(15)
   END RECORD

   DEFINE v_r_det_mto_dif RECORD
      nss                           CHAR(11),
      monto_acc_liq                 CHAR(15),
      monto_acc_sdo                 CHAR(15),
      monto_diferen                 CHAR(15)
   END RECORD

   DEFINE v_r_rpt_fechas_liq RECORD
      nss                           CHAR(11),
      fec_liquida                   CHAR(8),
      fec_concilia                  CHAR(8)
   END RECORD

   DEFINE v_r_rpt_tpo_credito RECORD
      nss                           CHAR(11),
      tpo_credito                   CHAR(3)
   END RECORD

   DEFINE v_d_mto_acc_viv97         DECIMAL(15,0)
   DEFINE v_d_mto_acc_viv92         DECIMAL(15,0)
   DEFINE v_d_monto_acc_liq         DECIMAL(18,6)
   DEFINE v_d_monto_pss_liq         DECIMAL(14,2)
   DEFINE v_d_id_derechohab         DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE v_d_folio_liquida         DECIMAL(9,0) -- folio de liquidaci�n
   DEFINE v_si_tpo_credito          SMALLINT -- tipo de credito
   DEFINE v_c_modulo_cod            CHAR(2) -- m�dulo cod
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_c_ruta_listado          LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo

   -- se inicializan variables
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_d_folio_liquida = 0

   -- se obtiene la extensi�n del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_arch_salida1 = "repSdosLiq_" || v_c_fec_hoy || "_AGR." || v_c_extension CLIPPED
   LET v_v_arch_salida2 = "repSdosLiq_" || v_c_fec_hoy || "_AGR." || "slea"
   LET v_v_arch_salida3 = "repSdosLiq_" || v_c_fec_hoy || "_AGR." || "slep"
   LET v_v_arch_salida4 = "Agr" || v_c_fec_hoy || "." || "09ag"
   LET v_v_arch_salida5 = "Agr" || v_c_fec_hoy || "." || "tc09"
   DISPLAY " REPORTE SALIDA (MONTOS IGUALES): ", v_v_arch_salida1
   DISPLAY " REPORTE SALIDA (MONTOS LIQUIDADO MAYOR): ", v_v_arch_salida2
   DISPLAY " REPORTE SALIDA (MONTOS LIQUIDADO MENOR): ", v_v_arch_salida3
   DISPLAY " REPORTE SALIDA (FECHAS LIQUIDACI�N): ", v_v_arch_salida4
   DISPLAY " REPORTE SALIDA (TIPO CR�DITO): ", v_v_arch_salida5

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte1 = base.Channel.create()
   LET v_ch_arch_reporte2 = base.Channel.create()
   LET v_ch_arch_reporte3 = base.Channel.create()
   LET v_ch_arch_reporte4 = base.Channel.create()
   LET v_ch_arch_reporte5 = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida1 CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte1.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte1.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida2 CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte2.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte2.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida3 CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte3.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte3.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida4 CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte4.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte4.setDelimiter("")

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojar�
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida5 CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte5.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte5.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT nss_infonavit,\n",
                    "        NVL(SUM(aplicaciones_interes_viv97),0),\n",
                    "        NVL(SUM(aplicaciones_interes_viv92),0)\n",
                    "   FROM safre_tmp:tmp_sdo_transf_det_agr\n",
                    "  GROUP BY 1"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_r_archivo_rep.nss,
                                  v_d_mto_acc_viv97,
                                  v_d_mto_acc_viv92
      -- se inicializa la variable del derechohabiente
      LET v_d_id_derechohab = NULL;

      -- se acumulan las acciones y los pesos
      LET v_r_archivo_rep.mto_acc_viv = (v_d_mto_acc_viv97 + v_d_mto_acc_viv92) / 1000000

      -- El tipo de transferencia corresponde a AG. Se obtiene el m�dulo ("AG", "UA")
      LET v_s_qryTxt = " SELECT id_derechohabiente, modulo_cod\n",
                       "   FROM safre_tmp:tmp_agr_solic_sdo\n",
                       "  WHERE nss = '",v_r_archivo_rep.nss,"'"

      PREPARE prp_slct_idDer_modCod FROM v_s_qryTxt
      DECLARE cur_slct_idDer_modCod CURSOR FOR prp_slct_idDer_modCod

      FOREACH cur_slct_idDer_modCod INTO v_d_id_derechohab, v_c_modulo_cod
         -- se inicializa el tipo de cr�dito
         LET v_si_tpo_credito = NULL
         LET v_d_folio_liquida = NULL

         -- se valida el m�dulo
         IF v_c_modulo_cod = "AG" THEN
            -- se obtiene el folio de liquidaci�n y el identificador del derechohabiente
            LET v_s_qryTxt = " SELECT FIRST 1 id_derechohabiente, folio_liquida, tpo_credito\n",
                             "   FROM cre_acreditado\n",
                             "  WHERE estado >= 140\n",
                             "    AND edo_procesar = 120\n",
                             "    AND id_derechohabiente = ",v_d_id_derechohab,"\n",
                             "    AND tpo_originacion = 4\n",
                             "  ORDER BY f_otorga DESC"

            PREPARE prp_slct_folLiq_tpoCred FROM v_s_qryTxt
            EXECUTE prp_slct_folLiq_tpoCred INTO v_d_id_derechohab, v_d_folio_liquida, v_si_tpo_credito
         ELSE
            -- se obtiene el folio de liquidaci�n y el identificador del derechohabiente
            LET v_s_qryTxt = " SELECT FIRST 1 id_derechohabiente, folio_liquida\n",
                             "   FROM cre_uso_garantia\n",
                             "  WHERE estado >= 140\n",
                             "    AND edo_procesar = 120\n",
                             "    AND id_cre_ctr_archivo IN (\n",
                             "        SELECT id_cre_ctr_archivo\n",
                             "          FROM cre_ctr_archivo\n",
                             "         WHERE operacion NOT IN (1,6,9,14))\n",
                             "    AND id_derechohabiente = ",v_d_id_derechohab,"\n",
                             "    AND tpo_transferencia = '43'\n",
                             "  ORDER BY folio_liquida DESC"

            PREPARE prp_slct_folLiq FROM v_s_qryTxt
            EXECUTE prp_slct_folLiq INTO v_d_id_derechohab, v_d_folio_liquida
         END IF

         -- si no se encontro registro en la tabla maestro se asume que fue rechazado
         IF v_d_id_derechohab IS NULL OR v_d_folio_liquida IS NULL THEN
            CONTINUE FOREACH
         END IF

         -- se asignan los valores al registro a insertar en archivo (fechas liquidaci�n)
         LET v_r_rpt_fechas_liq.nss = v_r_archivo_rep.nss
         LET v_r_rpt_fechas_liq.fec_liquida = fn_obtn_fch_lqdcn(v_d_folio_liquida)
         LET v_r_rpt_fechas_liq.fec_concilia = TODAY USING "ddmmyyyy"

         -- se concatenan los campos a insertar
         LET v_s_registro = v_r_rpt_fechas_liq.nss,
                            v_r_rpt_fechas_liq.fec_liquida,
                            v_r_rpt_fechas_liq.fec_concilia

         -- se escribe el registro (montos iguales) en el archivo
         CALL v_ch_arch_reporte4.write([v_s_registro])

         -- si el registro se trata de Anualidades Garantizadas se guarda en el archivo
         IF v_c_modulo_cod = "AG" THEN
            -- se asignan los valores al registro a insertar en archivo (tipo de cr�dito)
            LET v_r_rpt_tpo_credito.nss = v_r_archivo_rep.nss
            LET v_r_rpt_tpo_credito.tpo_credito = v_si_tpo_credito USING "&&&"

            -- se concatenan los campos a insertar
            LET v_s_registro = v_r_rpt_tpo_credito.nss,
                               v_r_rpt_tpo_credito.tpo_credito

            -- se escribe el registro (montos iguales) en el archivo
            CALL v_ch_arch_reporte5.write([v_s_registro])
         END IF

         IF v_d_folio_liquida IS NOT NULL AND v_d_folio_liquida > 0 THEN
            -- se obtienen los importes liquidados
            CALL fn_obtn_imp_lqdd(v_d_id_derechohab, v_d_folio_liquida) RETURNING v_d_monto_acc_liq, v_d_monto_pss_liq

            -- en caso de no haber encontrado importe, no se inserta el registro
            IF v_d_monto_acc_liq IS NULL OR v_d_monto_acc_liq = 0 THEN  
               CONTINUE FOREACH
            END IF

            -- se comparan los montos
            IF v_d_monto_acc_liq = v_r_archivo_rep.mto_acc_viv THEN
               -- se asignan los valores en el registro detalle
               LET v_r_det_mto_igl.nss            = v_r_archivo_rep.nss
               LET v_r_det_mto_igl.monto_acciones = (v_d_monto_acc_liq * 1000000) USING "&&&&&&&&&&&&&&&"
               LET v_r_det_mto_igl.monto_pesos    = (v_d_monto_pss_liq * 100) USING "&&&&&&&&&&&&&&&"

               -- se concatenan los campos a insertar
               LET v_s_registro = v_r_det_mto_igl.nss,
                                  v_r_det_mto_igl.monto_acciones,
                                  v_r_det_mto_igl.monto_pesos

               -- se escribe el registro (montos iguales) en el archivo
               CALL v_ch_arch_reporte1.write([v_s_registro])
            ELSE
               IF v_d_monto_acc_liq > v_r_archivo_rep.mto_acc_viv THEN
                  -- se asignan los valores en el registro (monto liquidado mayor)
                  LET v_r_det_mto_dif.nss = v_r_archivo_rep.nss
                  LET v_r_det_mto_dif.monto_acc_liq = (v_d_monto_acc_liq * 1000000) USING "&&&&&&&&&&&&&&&"
                  LET v_r_det_mto_dif.monto_acc_sdo = (v_r_archivo_rep.mto_acc_viv * 1000000) USING "&&&&&&&&&&&&&&&"
                  LET v_r_det_mto_dif.monto_diferen = ((v_d_monto_acc_liq - v_r_archivo_rep.mto_acc_viv) * 1000000) USING "&&&&&&&&&&&&&&&"

                  -- se concatenan los campos a insertar
                  LET v_s_registro = v_r_det_mto_dif.nss,
                                     v_r_det_mto_dif.monto_acc_liq,
                                     v_r_det_mto_dif.monto_acc_sdo,
                                     v_r_det_mto_dif.monto_diferen

                  -- se escribe el registro (monto liquidado mayor) en el archivo
                  CALL v_ch_arch_reporte2.write([v_s_registro])
               ELSE
                  -- se asignan los valores en el registro (monto liquidado menor)
                  LET v_r_det_mto_dif.nss = v_r_archivo_rep.nss
                  LET v_r_det_mto_dif.monto_acc_liq = (v_d_monto_acc_liq * 1000000) USING "&&&&&&&&&&&&&&&"
                  LET v_r_det_mto_dif.monto_acc_sdo = (v_r_archivo_rep.mto_acc_viv * 1000000) USING "&&&&&&&&&&&&&&&"
                  LET v_r_det_mto_dif.monto_diferen = ((v_r_archivo_rep.mto_acc_viv - v_d_monto_acc_liq) * 1000000) USING "&&&&&&&&&&&&&&&"

                  -- se concatenan los campos a insertar
                  LET v_s_registro = v_r_det_mto_dif.nss,
                                     v_r_det_mto_dif.monto_acc_liq,
                                     v_r_det_mto_dif.monto_acc_sdo,
                                     v_r_det_mto_dif.monto_diferen

                  -- se escribe el registro (monto liquidado mayor) en el archivo
                  CALL v_ch_arch_reporte3.write([v_s_registro])
               END IF
            END IF
         END IF
      END FOREACH
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte1.close()

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte2.close()

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte3.close()

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte4.close()

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte5.close()

END FUNCTION

#Objetivo: Funci�n que selecciona y regresa el importe liquidado (pesos y acciones) para un
#          nss que entra como par�mentro
FUNCTION fn_obtn_imp_lqdd(p_d_id_derechohab, p_d_folio_liquida)

   DEFINE p_d_id_derechohab         DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE p_d_folio_liquida         DECIMAL(9,0) -- folio de liquidaci�n
   DEFINE v_monto_acciones          DECIMAL(18,6) -- monto total en acciones
   DEFINE v_monto_pesos             DECIMAL(14,2) -- monto total en pesos
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_obtiene_mov_liq(?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING p_d_id_derechohab,
                             p_d_folio_liquida
                        INTO v_monto_acciones, v_monto_pesos

   RETURN v_monto_acciones, v_monto_pesos

END FUNCTION

#Objetivo: Funci�n que selecciona y regresa la fecha de liquidaci�n del folio de liquidaci�n
#          que entra como par�mentro
FUNCTION fn_obtn_fch_lqdcn(p_d_folio_liquida)

   DEFINE p_d_folio_liquida          LIKE cta_movimiento.folio_liquida
   DEFINE p_d_f_liquida              LIKE cta_movimiento.f_liquida
   DEFINE v_s_sqlQry                 STRING
   DEFINE v_criterio                 SMALLINT
   DEFINE v_s_qryTxt                 STRING
   DEFINE v_fecha                    DATE
   DEFINE v_tabla                    CHAR(20)
   DEFINE v_c_f_liquida              CHAR(8)

   LET v_criterio = 0
   LET v_fecha    = ""

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

   PREPARE prp_obt_fmov FROM v_s_qryTxt
   EXECUTE prp_obt_fmov USING v_criterio,
                             p_d_folio_liquida,
                             v_fecha
                        INTO v_tabla

   -- se selecciona la descripci�n del tipo de credito
   LET v_s_sqlQry = " SELECT FIRST 1 f_liquida\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND f_liquida IS NOT NULL"

   PREPARE prp_slct_fLiquida FROM v_s_sqlQry
   EXECUTE prp_slct_fLiquida INTO p_d_f_liquida

   LET v_c_f_liquida = p_d_f_liquida USING "ddmmyyyy"

   RETURN v_c_f_liquida

END FUNCTION
