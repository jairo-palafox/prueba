####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRS07                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo Conciliación y reporte en PDF      #
#                    las cifras de control del archivo             #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>22 Marzo 2013                                 #
####################################################################
DATABASE safre_viv

MAIN

   DEFINE p_v_usuario              LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                  LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod          LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod            LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_salida          VARCHAR(40) -- nombre del archivo de salida
   DEFINE p_si_tpo_originac        CHAR(2) -- valor del módulo cod
   DEFINE p_si_val_tpo_conc        SMALLINT -- valor del tipo de conciliación

   DEFINE v_r_det_archivo1 RECORD
      nss                          CHAR(11), -- nss
      folio_liquida                LIKE cta_movimiento.folio_liquida, -- folio de liquidación
      fec_liquida                  CHAR(8),  -- fecha de liquidación
      acciones_viv92               DECIMAL(18,6), -- monto acciones vivienda 92
      acciones_viv97               DECIMAL(18,6), -- monto acciones vivienda 97
      sum_acc_92_97                DECIMAL(18,6) -- suma de acciones vivienda 97 y 92
   END RECORD

   DEFINE v_r_det_archivo2 RECORD
      nss                          CHAR(11), -- nss
      folio_liquida                LIKE cta_movimiento.folio_liquida, -- folio de liquidación
      fec_liquida                  CHAR(8),  -- fecha de liquidación
      acciones_viv92               DECIMAL(18,6), -- monto acciones vivienda 92
      acciones_viv97               DECIMAL(18,6), -- monto acciones vivienda 97
      sum_acc_92_97                DECIMAL(18,6), -- suma de acciones vivienda 97 y 92
      acciones_liquida             DECIMAL(18,6), -- monto acciones liquidadas (saldo deudor)
      diferecia_acc                DECIMAL(18,6) -- diferencia en acciones sum_acc_92_97 - acciones_liquida
   END RECORD

   DEFINE v_c_ruta_env_agr         LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_nomarch         VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_nom_reporte          VARCHAR(40) -- nombre del reporte
   DEFINE v_ch_archivo_salida      BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_v_edos_procesar        VARCHAR(50) -- contiene la lista de estados procesar a consultar
   DEFINE v_i_total_regis_arch     INTEGER -- total de registros en el archivo

   DEFINE v_r_cre_registro RECORD
      id_cre_acreditado            LIKE cre_acreditado.id_cre_acreditado, -- identificador del acreditado
      id_derechohabiente           LIKE afi_derechohabiente.id_derechohabiente, -- identificador del derechohabiente
      folio_liquida                LIKE cta_movimiento.folio_liquida -- folio de liquidación
   END RECORD

   DEFINE v_r_cta_mov RECORD
      fec_liquida                   DATE, -- fecha de liquidación
      sum_acc_viv92                 DECIMAL(18,6), -- monto acciones vivienda 92
      sum_pss_viv92                 DECIMAL(12,2), -- monto pesos vivienda 92
      sum_acc_viv97                 DECIMAL(18,6), -- monto acciones vivienda 97
      sum_pss_viv97                 DECIMAL(12,2) -- monto pesos vivienda 97
   END RECORD

   DEFINE v_c_nss                  LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_d_acc_liquidadas       DECIMAL(18,6) -- se acumula el monto en acciones liquidadas
   DEFINE v_d_suma_acc_viv92       DECIMAL(18,6) -- se acumula el monto en acciones de viv92
   DEFINE v_d_suma_pss_viv92       DECIMAL(12,2) -- se acumula el monto en pesos de viv92
   DEFINE v_d_suma_acc_viv97       DECIMAL(18,6) -- se acumula el monto en acciones de viv97
   DEFINE v_d_suma_pss_viv97       DECIMAL(12,2) -- se acumula el monto en pesos de viv97
   DEFINE v_d_suma_acc_liquida     DECIMAL(12,2) -- se acumula el monto en pesos de viv97
   DEFINE v_s_qryTxt               STRING -- guarda una sentencia sql a ejecutar
   DEFINE r_b_valida               SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_c_ruta_listado         LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_manejador_rpt          OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_c_programa_cod         LIKE cat_operacion.programa_cod -- nombre del programa

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_salida   = ARG_VAL(6)
   LET p_si_tpo_originac = ARG_VAL(7)
   LET p_si_val_tpo_conc = ARG_VAL(8)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRS07.log")

   DISPLAY "=INICIA AGRS07="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO       : ",p_v_arch_salida
   DISPLAY " TIPO ORIGINAC : ",p_si_tpo_originac

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_env_agr, v_c_ruta_listado

   -- se crea el nombre del archivo y se concatena con la ruta
   LET v_v_ruta_nomarch = v_c_ruta_env_agr CLIPPED || "/" || p_v_arch_salida CLIPPED

   -- se crea el manejador de archivo
   LET v_ch_archivo_salida = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_archivo_salida.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_archivo_salida.setDelimiter("|")

   -- se inicializan variables
   LET v_i_total_regis_arch = 0
   LET v_d_suma_acc_viv92   = 0
   LET v_d_suma_pss_viv92   = 0
   LET v_d_suma_acc_viv97   = 0
   LET v_d_suma_pss_viv97   = 0
   LET v_d_suma_acc_liquida = 0

   -- se asignan los estados procesar a consultar dependiendo del tipo de conciliación
   IF p_si_val_tpo_conc = 1 THEN
      LET v_v_edos_procesar = "40, 50"
   ELSE
      IF p_si_val_tpo_conc = 2 THEN
         LET v_v_edos_procesar = "70, 85"
      ELSE
         LET v_v_edos_procesar = "120"
      END IF
   END IF

   -- se armar consulta que obtiene la información para el registro detalle
   LET v_s_qryTxt = " SELECT id_cre_acreditado, id_derechohabiente, folio_liquida\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 140\n",
                    "    AND edo_procesar IN (",v_v_edos_procesar,")\n",
                    "    AND folio_liquida IS NOT NULL\n",
                    "    AND folio_liquida <> 0\n",
                    "    AND tpo_originacion = ",p_si_tpo_originac

   PREPARE prp_cre_idDer_folLiq FROM v_s_qryTxt
   DECLARE cur_cre_idDer_folLiq CURSOR FOR prp_cre_idDer_folLiq

   FOREACH cur_cre_idDer_folLiq INTO v_r_cre_registro.*
      -- se invoca la funcion que obtiene el nss y el númeo de crédito del trabajador
      LET v_c_nss = fn_obt_nss(v_r_cre_registro.id_derechohabiente) 

      -- se obtiene la información liquidada para el derechohabiente y folio de liquidación en proceso
      CALL fn_obt_inf_lqdcn(v_r_cre_registro.id_derechohabiente, v_r_cre_registro.folio_liquida)
         RETURNING v_r_cta_mov.*

      -- si la fecha de liquidación es nula continua con el siguiente registro
      IF v_r_cta_mov.fec_liquida IS NULL OR v_r_cta_mov.fec_liquida = "12/31/1899" THEN
         CONTINUE FOREACH
      END IF

      -- se verifica si el tipo de conciliación es Aceptados con Diferencias
      IF p_si_val_tpo_conc = 3 THEN
         -- se obtiene el importe liquidado, guardado en saldo deudor
         CALL fn_obt_acc_lqdds(v_r_cre_registro.id_cre_acreditado)
         RETURNING v_d_acc_liquidadas

         -- se armar registro detalle
         LET v_r_det_archivo2.nss	           = v_c_nss
         LET v_r_det_archivo2.folio_liquida    = v_r_cre_registro.folio_liquida
         LET v_r_det_archivo2.fec_liquida      = v_r_cta_mov.fec_liquida USING "ddmmyyyy"
         LET v_r_det_archivo2.acciones_viv92   = v_r_cta_mov.sum_acc_viv92
         LET v_r_det_archivo2.acciones_viv97   = v_r_cta_mov.sum_acc_viv97
         LET v_r_det_archivo2.sum_acc_92_97    = v_r_cta_mov.sum_acc_viv92 + v_r_cta_mov.sum_acc_viv97
         LET v_r_det_archivo2.acciones_liquida = v_d_acc_liquidadas
         LET v_r_det_archivo2.diferecia_acc    = v_r_det_archivo2.sum_acc_92_97 + v_d_acc_liquidadas

         -- se verifica que existe diferencia entre las acciones, de no ser asi no graba el registro
         IF v_r_det_archivo2.diferecia_acc = 0 THEN
            CONTINUE FOREACH
         END IF

         -- acumulan los importes obtenidos
         LET v_d_suma_acc_liquida = v_d_suma_acc_liquida + v_d_acc_liquidadas

         -- se escribe el registro (detalle) en el archivo
         CALL v_ch_archivo_salida.write([v_r_det_archivo2.*])
      ELSE
         -- se armar registro detalle
         LET v_r_det_archivo1.nss              = v_c_nss
         LET v_r_det_archivo1.folio_liquida    = v_r_cre_registro.folio_liquida
         LET v_r_det_archivo1.fec_liquida      = v_r_cta_mov.fec_liquida USING "ddmmyyyy"
         LET v_r_det_archivo1.acciones_viv92   = v_r_cta_mov.sum_acc_viv92
         LET v_r_det_archivo1.acciones_viv97   = v_r_cta_mov.sum_acc_viv97
         LET v_r_det_archivo1.sum_acc_92_97    = v_r_cta_mov.sum_acc_viv92 + v_r_cta_mov.sum_acc_viv97

         -- se escribe el registro (detalle) en el archivo
         CALL v_ch_archivo_salida.write([v_r_det_archivo1.*])
      END IF

      -- acumulan los importes obtenidos
      LET v_d_suma_acc_viv92 = v_d_suma_acc_viv92 + v_r_cta_mov.sum_acc_viv92
      LET v_d_suma_pss_viv92 = v_d_suma_pss_viv92 + v_r_cta_mov.sum_pss_viv92
      LET v_d_suma_acc_viv97 = v_d_suma_acc_viv97 + v_r_cta_mov.sum_acc_viv97
      LET v_d_suma_pss_viv97 = v_d_suma_pss_viv97 + v_r_cta_mov.sum_pss_viv97

      -- actualiza total de registros de archivo
      LET v_i_total_regis_arch = v_i_total_regis_arch + 1
   END FOREACH

   -- se valida si el modulo solicitado es de Anualidades Garatizadas (AG)
   IF p_si_tpo_originac = 4 THEN
      -- se armar consulta que obtiene la información para el registro detalle
      LET v_s_qryTxt = " SELECT id_cre_uso_garantia, id_derechohabiente, folio_liquida\n",
                       "   FROM cre_uso_garantia\n",
                       "  WHERE estado = 140\n",
                       "    AND edo_procesar IN (",v_v_edos_procesar,")\n",
                       "    AND folio_liquida IS NOT NULL\n",
                       "    AND folio_liquida <> 0\n",
                       "    AND tpo_transferencia = '43'"

      PREPARE prp_uso_idDer_folLiq FROM v_s_qryTxt
      DECLARE cur_uso_idDer_folLiq CURSOR FOR prp_uso_idDer_folLiq

      FOREACH cur_uso_idDer_folLiq INTO v_r_cre_registro.*
         -- se invoca la funcion que obtiene el nss y el númeo de crédito del trabajador
         LET v_c_nss = fn_obt_nss(v_r_cre_registro.id_derechohabiente) 

         -- se obtiene la información liquidada para el derechohabiente y folio de liquidación en proceso
         CALL fn_obt_inf_lqdcn(v_r_cre_registro.id_derechohabiente, v_r_cre_registro.folio_liquida)
         RETURNING v_r_cta_mov.*

         -- si la fecha de liquidación es nula continua con el siguiente registro
         IF v_r_cta_mov.fec_liquida IS NULL OR v_r_cta_mov.fec_liquida = "12/31/1899" THEN
            CONTINUE FOREACH
         END IF

         -- se verifica si el tipo de conciliación es Aceptados con Diferencias
         IF p_si_val_tpo_conc = 3 THEN
            -- se obtiene el importe liquidado, guardado en saldo deudor
            CALL fn_obt_acc_lqdds(v_r_cre_registro.id_cre_acreditado)
            RETURNING v_d_acc_liquidadas

            -- se armar registro detalle
            LET v_r_det_archivo2.nss              = v_c_nss
            LET v_r_det_archivo2.folio_liquida    = v_r_cre_registro.folio_liquida
            LET v_r_det_archivo2.fec_liquida      = v_r_cta_mov.fec_liquida USING "ddmmyyyy" 
            LET v_r_det_archivo2.acciones_viv92   = v_r_cta_mov.sum_acc_viv92
            LET v_r_det_archivo2.acciones_viv97   = v_r_cta_mov.sum_acc_viv97
            LET v_r_det_archivo2.sum_acc_92_97    = v_r_cta_mov.sum_acc_viv92 + v_r_cta_mov.sum_acc_viv97
            LET v_r_det_archivo2.acciones_liquida = v_d_acc_liquidadas
            LET v_r_det_archivo2.diferecia_acc    = v_r_det_archivo2.sum_acc_92_97 + v_d_acc_liquidadas

            -- se verifica que existe diferencia entre las acciones, de no ser asi no graba el registro
            IF v_r_det_archivo2.diferecia_acc = 0 THEN
               CONTINUE FOREACH
            END IF

            -- acumulan los importes obtenidos
            LET v_d_suma_acc_liquida = v_d_suma_acc_liquida + v_d_acc_liquidadas

            -- se escribe el registro (detalle) en el archivo
            CALL v_ch_archivo_salida.write([v_r_det_archivo2.*])
         ELSE
            -- se armar registro detalle
            LET v_r_det_archivo1.nss              = v_c_nss
            LET v_r_det_archivo1.folio_liquida    = v_r_cre_registro.folio_liquida
            LET v_r_det_archivo1.fec_liquida      = v_r_cta_mov.fec_liquida
            LET v_r_det_archivo1.acciones_viv92   = v_r_cta_mov.sum_acc_viv92
            LET v_r_det_archivo1.acciones_viv97   = v_r_cta_mov.sum_acc_viv97
            LET v_r_det_archivo1.sum_acc_92_97    = v_r_cta_mov.sum_acc_viv92 + v_r_cta_mov.sum_acc_viv97

            -- se escribe el registro (detalle) en el archivo
            CALL v_ch_archivo_salida.write([v_r_det_archivo1.*])
         END IF

         -- acumulan los importes obtenidos
         LET v_d_suma_acc_viv92 = v_d_suma_acc_viv92 + v_r_cta_mov.sum_acc_viv92
         LET v_d_suma_pss_viv92 = v_d_suma_pss_viv92 + v_r_cta_mov.sum_pss_viv92
         LET v_d_suma_acc_viv97 = v_d_suma_acc_viv97 + v_r_cta_mov.sum_acc_viv97
         LET v_d_suma_pss_viv97 = v_d_suma_pss_viv97 + v_r_cta_mov.sum_pss_viv97

         -- actualiza total de registros de archivo
         LET v_i_total_regis_arch = v_i_total_regis_arch + 1
      END FOREACH
   END IF

   -- se cierra el manejador de lectura
   CALL v_ch_archivo_salida.close()

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   -- verifica si existe o se puede abrir la plantilla del reporte
   IF fgl_report_loadCurrentSettings("AGRS071.4rp") THEN
      -- se obtiene el nombrel del programa correspondiente
      LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

      -- se asigna el nombrel del report
      LET v_v_nom_reporte = p_v_usuario CLIPPED,"-",
                            v_c_programa_cod CLIPPED,"-",
                            p_d_pid USING "&&&&&","-",
                            p_i_proceso_cod USING "&&&&&","-",
                            p_i_opera_cod USING "&&&&&"

      DISPLAY " REPORTE (PDF) : ",v_v_nom_reporte
      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED|| "/" ||v_v_nom_reporte)

      -- se indica que no sera necesaria la vista previa
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el manejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      EXIT PROGRAM
   END IF

   -- Inicia el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- Salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_salida, v_d_suma_acc_viv92, v_d_suma_pss_viv92, v_d_suma_acc_viv97, v_d_suma_pss_viv97, v_d_suma_acc_liquida, v_i_total_regis_arch, p_si_val_tpo_conc)

   -- Finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   DISPLAY "=FIN="

END MAIN

#Objetivo: Función que obtiene el NSS y numero de credito del trabajador en la tabla
FUNCTION fn_obt_nss(p_d_id_derechohabiente)

   DEFINE p_d_id_derechohabiente LIKE cre_acreditado.id_derechohabiente --id del derechohabiente
   DEFINE v_c_nss                LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_s_qryTxt             STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene el numero de credito para el id derechohabiente
   LET v_s_qryTxt = " SELECT nss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_d_id_derechohabiente

   PREPARE prp_slcFrst_nssNumCred FROM v_s_qryTxt
   EXECUTE prp_slcFrst_nssNumCred INTO v_c_nss

   RETURN v_c_nss

END FUNCTION

#Objetivo: Función que obtiene la información del registro liquidado
FUNCTION fn_obt_inf_lqdcn(p_d_id_derechohabiente, p_d_folio_liquida)

   DEFINE p_d_id_derechohabiente LIKE cre_acreditado.id_derechohabiente --id del derechohabiente
   DEFINE p_d_folio_liquida      LIKE cta_movimiento.folio_liquida -- folio de liquidación
   DEFINE v_s_qryTxt             STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_criterio             SMALLINT
   DEFINE v_f_liquida            DATE
   DEFINE v_tabla                CHAR(20)

   DEFINE v_r_cta_mov RECORD
      fec_liquida                DATE, -- fecha de liquidación
      sum_acc_viv92              DECIMAL(18,6), -- monto acciones vivienda 92
      sum_pss_viv92              DECIMAL(12,2), -- monto pesos vivienda 92
      sum_acc_viv97              DECIMAL(18,6), -- monto acciones vivienda 97
      sum_pss_viv97              DECIMAL(12,2) -- monto pesos vivienda 97
   END RECORD

   LET v_criterio  = 0
   LET v_f_liquida = "12/31/1899"

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING v_criterio,
                             p_d_folio_liquida,
                             v_f_liquida
                        INTO v_tabla

   -- se asigna la sentencia que obtiene la fecha de liquidación para el folio en proceso
   LET v_s_qryTxt = " SELECT FIRST 1 f_liquida\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND f_liquida IS NOT NULL"

   PREPARE prp_slc_fLiq FROM v_s_qryTxt
   EXECUTE prp_slc_fLiq INTO v_r_cta_mov.fec_liquida

   -- se asigna la sentencia que obtiene los importes de viv92 para el derechohabiente y folio en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND id_derechohabiente = ",p_d_id_derechohabiente,"\n",
                    "    AND subcuenta = 8"

   PREPARE prp_slc_mtoAcc_mtoPss_viv92 FROM v_s_qryTxt
   EXECUTE prp_slc_mtoAcc_mtoPss_viv92 INTO v_r_cta_mov.sum_acc_viv92, v_r_cta_mov.sum_pss_viv92

   -- se validan las acciones (viv 92)
   IF v_r_cta_mov.sum_acc_viv92 IS NULL THEN
      LET v_r_cta_mov.sum_acc_viv92 = 0
   END IF

   -- se validan los pesos (viv 92)
   IF v_r_cta_mov.sum_pss_viv92 IS NULL THEN
      LET v_r_cta_mov.sum_pss_viv92 = 0
   END IF

   -- se asigna la sentencia que obtiene los importes de viv97 para el derechohabiente y folio en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND id_derechohabiente = ",p_d_id_derechohabiente,"\n",
                    "    AND subcuenta = 4"

   PREPARE prp_slc_mtoAcc_mtoPss_viv97 FROM v_s_qryTxt
   EXECUTE prp_slc_mtoAcc_mtoPss_viv97 INTO v_r_cta_mov.sum_acc_viv97, v_r_cta_mov.sum_pss_viv97

   -- se validan las acciones (viv 97)
   IF v_r_cta_mov.sum_acc_viv97 IS NULL THEN
      LET v_r_cta_mov.sum_acc_viv97 = 0
   END IF

   -- se validan los pesos (viv 97)
   IF v_r_cta_mov.sum_pss_viv97 IS NULL THEN
      LET v_r_cta_mov.sum_pss_viv97 = 0
   END IF

   RETURN v_r_cta_mov.*

END FUNCTION

#Objetivo: Funcion que las acciones liquidadas guardadas en saldo deudor
FUNCTION fn_obt_acc_lqdds(p_d_id_cre_acreditado)

   DEFINE p_d_id_cre_acreditado  LIKE cre_acreditado.id_derechohabiente --id del derechohabiente
   DEFINE sum_acc_liquidadas     DECIMAL(18,6) -- monto acciones liquidadas
   DEFINE v_s_qryTxt             STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene los importes de viv92 para el derechohabiente y folio en proceso
   LET v_s_qryTxt = " SELECT NVL(SUM(monto_aivs),0)\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado = ",p_d_id_cre_acreditado,"\n",
                    "    AND movimiento IN (272, 282)"

   PREPARE prp_slc_mtoAcc_liquidadas FROM v_s_qryTxt
   EXECUTE prp_slc_mtoAcc_liquidadas INTO sum_acc_liquidadas

   RETURN sum_acc_liquidadas

END FUNCTION

#Objetivo: Genera el reporte de conciliación
REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_salida, p_d_suma_acc_viv92, p_d_suma_pss_viv92, p_d_suma_acc_viv97, p_d_suma_pss_viv97, p_d_suma_acc_liquida, p_i_total_regis_arch, p_si_val_tpo_conc)

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod -- nombre del usuario
   DEFINE p_d_folio              INTEGER -- folio de conciliación
   DEFINE p_v_arch_salida        CHAR(100) -- nombre del archivo de salida
   DEFINE p_d_suma_acc_viv92     DECIMAL(18,6) -- se acumula el monto en acciones de viv92
   DEFINE p_d_suma_pss_viv92     DECIMAL(12,2) -- se acumula el monto en pesos de viv92
   DEFINE p_d_suma_acc_viv97     DECIMAL(18,6) -- se acumula el monto en acciones de viv97
   DEFINE p_d_suma_pss_viv97     DECIMAL(12,2) -- se acumula el monto en pesos de viv97
   DEFINE p_d_suma_acc_9297      DECIMAL(18,6) -- se acumula el monto en acciones de viv97 + viv92
   DEFINE p_d_suma_acc_liquida   DECIMAL(18,6) -- se acumula el monto en acciones liquidadas
   DEFINE p_d_suma_acc_dif       DECIMAL(18,6) -- se acumula el monto en acciones diferencia
   DEFINE p_i_total_regis_arch   INTEGER -- total de registros en el archivo
   DEFINE p_si_val_tpo_conc      SMALLINT -- valor del tipo de conciliación
   DEFINE v_fecha_reporte        DATE -- fecha de generación del reporte

   FORMAT

   FIRST PAGE HEADER
      -- se asigna la fecha de generación del reporte (HOY)
      LET v_fecha_reporte = TODAY
      LET p_d_suma_acc_9297 = p_d_suma_acc_viv92 + p_d_suma_acc_viv97
      LET p_d_suma_acc_dif = p_d_suma_acc_9297 + p_d_suma_acc_liquida

      PRINTX p_v_usuario
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_d_folio
      PRINTX p_v_arch_salida
      PRINTX p_d_suma_acc_viv92
      PRINTX p_d_suma_pss_viv92
      PRINTX p_d_suma_acc_viv97
      PRINTX p_d_suma_pss_viv97
      PRINTX p_d_suma_acc_9297
      PRINTX p_d_suma_acc_liquida
      PRINTX p_d_suma_acc_dif
      PRINTX p_i_total_regis_arch
      PRINTX p_si_val_tpo_conc

END REPORT
