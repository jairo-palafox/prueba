--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

############################################################################
#Modulo            =>AGR                                                   #
#Programa          =>AGRP23                                                #
#Objetivo          =>Programa que realiza la conciliación de la            #
#                    confirmacion DSE para Anualidades Garantizadas        #
#Autor             =>Daniel Buendia, EFP                                   #
#Fecha inicio      =>07 Mayo 2012                                          #
############################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

# Objetivo: Conciliar la información de confirmacion de devolucion de saldos
MAIN

   DEFINE p_v_usuario           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso      VARCHAR(100) -- nombre del archivo a integrar

   DEFINE v_r_rpt_res RECORD -- registro de resumen
      folio                     INTEGER, -- numero de folio con formato
      nom_archivo               LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini              LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin              LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      usuario                   LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros             INTEGER, -- numero total de registros
      tot_aceptados             INTEGER, -- numero total de regs aceptados
      tot_rechazados            INTEGER -- numero total de regs rechazados
   END RECORD

   DEFINE v_r_dse_ctr_arch      RECORD LIKE dse_ctr_archivo.* -- registro de dse ctr archivo
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación

   DEFINE v_r_pago_cred_trad_inf RECORD
      aivs97_infonavit          LIKE cre_saldo_deudor.monto_aivs,
      pesos97_infonavit         LIKE cre_saldo_deudor.monto_pesos,
      aivs92_infonavit          LIKE cre_saldo_deudor.monto_aivs,
      pesos92_infonavit         LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE v_r_pago_cred_trad_afo RECORD
      aivs97_afore              LIKE cre_saldo_deudor.monto_aivs,
      pesos97_afore             LIKE cre_saldo_deudor.monto_pesos,
      aivs92_afore              LIKE cre_saldo_deudor.monto_aivs,
      pesos92_afore             LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   --DEFINE                       v_ide_derechohabiente LIKE cre_deudor.id_derechohabiente
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt            STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_v_ruta_reporte      VARCHAR(200) -- ruta y nombre del reporte
   DEFINE v_s_titulo_correo     STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo    STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo       STRING -- contiene el cuerpo del correo 
   DEFINE v_folio_archivo       LIKE glo_folio.folio -- folio del archivo
   DEFINE v_d_pid_arch          LIKE bat_ctr_operacion.pid -- pid del proceso
   DEFINE v_c_programa_cod      LIKE cat_operacion.programa_cod -- nombre del programa origen
   DEFINE v_v_nom_reporte       VARCHAR(200) -- nombre del reporte
   DEFINE r_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE r_b_valida            SMALLINT
   DEFINE r_ruta_listados       LIKE seg_modulo.ruta_listados

   DEFINE v_tabla               CHAR(20)
   DEFINE v_aivs92              DECIMAL(16,6)
   DEFINE v_pesos92             DECIMAL(12,2)
   DEFINE v_aivs97              DECIMAL(16,6)
   DEFINE v_pesos97             DECIMAL(12,2)

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP23.log")

   DISPLAY "=INICIA AGRP23="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " CONCILIACIÓN DE CONFIRMACIÓN DE DEV. DE SDOS. EXC. AGR"

   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

   -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT UNIQUE folio_archivo\n",
                    " FROM safre_tmp:tmp_deudor_conf_devol_saldo_agr"

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_folio_archivo
   DISPLAY " Folio del archivo: ",v_folio_archivo

   DISPLAY " Crea reporte "

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRP231.4rp") THEN  -- if  the file loaded OK
      -- se asigna al nombre del reporte
      LET v_v_nom_reporte = p_v_usuario CLIPPED,"-",
                            v_c_programa_cod CLIPPED,"-",
                            p_d_pid USING "&&&&&","-",
                            p_i_proceso_cod USING "&&&&&","-",
                            p_i_opera_cod USING "&&&&&"

      -- se concatena la ruta del reporte con el nombre
      LET v_v_ruta_reporte = r_ruta_listados CLIPPED,"/",v_v_nom_reporte CLIPPED

      CALL fgl_report_setOutputFileName(v_v_ruta_reporte)
      -- sin preview

      CALL fgl_report_selectPreview(0)
      -- se indica que se escriba en archivo

      LET v_manejador_rpt = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF   
      EXIT PROGRAM
   END IF

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_Confirmacion_ds TO XML HANDLER v_manejador_rpt

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE folio = ",v_folio_archivo

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_dse_ctr_arch.*

   LET v_d_pid_arch = fn_max_pid(g_proc_cod_agr_conf_dse, 2)
   DISPLAY " PID del archivo: ",v_d_pid_arch

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",v_d_pid_arch,"\n",
                    "    AND proceso_cod = ",g_proc_cod_agr_conf_dse,"\n",
                    "    AND opera_cod = 2\n",
                    "    AND folio = ",v_folio_archivo

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   LET v_aivs92  = 0
   LET v_pesos92 = 0
   LET v_aivs97  = 0
   LET v_pesos97 = 0
   LET v_r_pago_cred_trad_inf.aivs92_infonavit  = 0
   LET v_r_pago_cred_trad_inf.pesos92_infonavit = 0
   LET v_r_pago_cred_trad_inf.aivs97_infonavit  = 0
   LET v_r_pago_cred_trad_inf.pesos97_infonavit = 0

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_dse_ctr_arch.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.usuario        = v_r_dse_ctr_arch.usuario
   LET v_r_rpt_res.tot_registros  = v_r_dse_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_dse_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_dse_ctr_arch.tot_rechazados
   LET v_r_rpt_res.folio          = v_folio_archivo

   SELECT t1.tabla
     FROM cat_tab_movimiento t1
   INTO TEMP tmp_cat_tab_movimiento

   INSERT INTO tmp_cat_tab_movimiento
   VALUES ("cta_movimiento")

   DECLARE cur_cta_mov CURSOR FOR
   SELECT t2.tabla
     FROM tmp_cat_tab_movimiento t2

   FOREACH cur_cta_mov INTO v_tabla
      -- Obtiene la suma del Pago Credito Tradicional Infonavit para Subcuenta 4 (Vivienda 97)
      LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)\n",
                       "   FROM ",v_tabla,"\n",
                       "  WHERE id_derechohabiente IN (\n",
                       "        SELECT id_derechohabiente\n",
                       "          FROM safre_tmp:tmp_deudor_conf_devol_saldo_agr)\n",
                       "    AND subcuenta IN (4,44)\n",
                       "    AND movimiento IN (21)"

   PREPARE con_pagCredTradInfo97_Rech FROM v_s_qryTxt
   EXECUTE con_pagCredTradInfo97_Rech INTO v_aivs97,
                                              v_pesos97

      -- se validan las acciones (viv 97)
      IF v_aivs97 IS NULL THEN
         LET v_aivs97 = 0
      END IF

      -- se validan los pesos (viv 97)
      IF v_pesos97 IS NULL THEN
         LET v_pesos97 = 0
      END IF

      LET v_r_pago_cred_trad_inf.aivs97_infonavit  = v_r_pago_cred_trad_inf.aivs97_infonavit + v_aivs97
      LET v_r_pago_cred_trad_inf.pesos97_infonavit = v_r_pago_cred_trad_inf.pesos97_infonavit + v_pesos97

   -- Obtiene la suma del Pago Credito Tradicional Infonavit para Subcuenta 4 (Vivienda 97)
   LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)\n",
                       "   FROM ",v_tabla,"\n",
                    "  WHERE id_derechohabiente IN (\n",
                    "        SELECT id_derechohabiente\n",
                    "          FROM safre_tmp:tmp_deudor_conf_devol_saldo_agr)\n",
                    "    AND subcuenta IN (8,42)\n",
                    "    AND movimiento IN (21)"

      PREPARE con_pagCredTradInfo92_Rech FROM v_s_qryTxt
      EXECUTE con_pagCredTradInfo92_Rech INTO v_aivs92,
                                              v_pesos92

      -- se validan las acciones (viv 92)
      IF v_aivs92 IS NULL THEN
         LET v_aivs92 = 0
      END IF

      -- se validan los pesos (viv 92)
      IF v_pesos92 IS NULL THEN
         LET v_pesos92 = 0
      END IF

      LET v_r_pago_cred_trad_inf.aivs92_infonavit  = v_r_pago_cred_trad_inf.aivs92_infonavit + v_aivs92
      LET v_r_pago_cred_trad_inf.pesos92_infonavit = v_r_pago_cred_trad_inf.pesos92_infonavit + v_pesos92
   END FOREACH

   CLOSE cur_cta_mov
   FREE cur_cta_mov

   -- Obtiene la suma del pago de Credito Tradicional AFORE Vivienda 97
   LET v_s_qryTxt = " SELECT NVL(SUM(num_aplic_interes97 / 1000000),0)\n",
                    "   FROM safre_tmp:tmp_dse_confirm_det_agr\n"

   PREPARE con_pagCredTradAfore97_Rech FROM v_s_qryTxt
   EXECUTE con_pagCredTradAfore97_Rech INTO v_r_pago_cred_trad_afo.aivs97_afore

   -- se calcula los pesos97
   LET v_r_pago_cred_trad_afo.pesos97_afore = 0

   -- Obtiene la suma del pago de Credito Tradicional AFORE Vivienda 92
   LET v_s_qryTxt = " SELECT NVL(SUM(num_aplic_interes92 / 1000000),0)\n",
                    "   FROM safre_tmp:tmp_dse_confirm_det_agr"
   PREPARE con_pagCredTradAfore92_Rech FROM v_s_qryTxt
   EXECUTE con_pagCredTradAfore92_Rech INTO v_r_pago_cred_trad_afo.aivs92_afore

   -- se calcula los pesos92
   LET v_r_pago_cred_trad_afo.pesos92_afore = 0

   # salida del reporte
   OUTPUT TO REPORT reporte_Confirmacion_ds(p_v_usuario, v_folio_archivo, v_r_rpt_res.*, v_r_pago_cred_trad_inf.*, v_r_pago_cred_trad_afo.*)
   
   --Finaliza el reporte
   FINISH REPORT reporte_Confirmacion_ds

   # Actualiza la operacion a finalizada
   CALL fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                              RETURNING r_b_valida
   IF(r_b_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida
                                     
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF

   DISPLAY " Envia correo del reporte"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE CONFIRMACIÓN DE DEVOLUCIÓN DE SALDOS EXCEDENTES ANUALIDADES GARANTIZADAS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : DEVOLUCIÓN DE SALDOS EXCEDENTES ANUALIDADES GARANTIZADAS\n",
                          "Operacion    : CONCILIACIÓN DE CONFIRMACIÓN\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(p_d_pid,
                          p_i_proceso_cod,
                          p_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)
   DISPLAY "=FIN="

END MAIN

#OBJETIVO: Genera el reporte de la conciliación de Saldos Transferidos
REPORT reporte_Confirmacion_ds(p_v_usuario, p_d_folio, p_r_res, p_r_pago_cred_trad_inf, p_r_pago_cred_trad_afo)

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE p_d_folio              LIKE glo_ctr_archivo.folio -- numero de folio

   DEFINE p_r_res RECORD -- registro de resumen
      folio                      INTEGER, -- numero de folio con formato
      nom_archivo                LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini               LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin               LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      usuario                    LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros              INTEGER, -- numero total de registros
      tot_aceptados              INTEGER, -- numero total de regs aceptados
      tot_rechazados             INTEGER -- numero total de regs rechazados
   END RECORD

   DEFINE p_r_pago_cred_trad_inf RECORD
      aivs97_infonavit           LIKE cre_saldo_deudor.monto_aivs,
      pesos97_infonavit          LIKE cre_saldo_deudor.monto_pesos,
      aivs92_infonavit           LIKE cre_saldo_deudor.monto_aivs,
      pesos92_infonavit          LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE p_r_pago_cred_trad_afo RECORD
      aivs97_afore               LIKE cre_saldo_deudor.monto_aivs,
      pesos97_afore              LIKE cre_saldo_deudor.monto_pesos,
      aivs92_afore               LIKE cre_saldo_deudor.monto_aivs,
      pesos92_afore              LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE p_i_folio              INTEGER
   DEFINE v_num_registros        INTEGER
   DEFINE v_sum_aivs             DECIMAL(22,6)
   DEFINE v_sum_pesos            DECIMAL(22,6)
   DEFINE v_fecha_reporte        DATE
   DEFINE v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion
   DEFINE v_d_sumAivs_infonavit  LIKE cre_saldo_deudor.monto_aivs
   DEFINE v_d_sumPesos_infonavit LIKE cre_saldo_deudor.monto_pesos
   DEFINE v_d_sumAivs_afore      LIKE cre_saldo_deudor.monto_aivs
   DEFINE v_d_sumPesos_afore     LIKE cre_saldo_deudor.monto_pesos
   DEFINE v_d_diferencia_aivs    LIKE cre_saldo_deudor.monto_aivs
   DEFINE v_d_diferencia_pesos   LIKE cre_saldo_deudor.monto_pesos

   FORMAT

   FIRST PAGE HEADER
      LET v_sum_aivs = 0
      LET v_sum_pesos = 0
      LET v_num_registros = 0
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte
      LET p_i_folio = p_d_folio

      -- se calcula la suma de aivs y pesos para PAGO CRÉDITO TRADICIONAL INFONAVIT
      LET v_d_sumAivs_infonavit = p_r_pago_cred_trad_inf.aivs92_infonavit + p_r_pago_cred_trad_inf.aivs97_infonavit
      LET v_d_sumPesos_infonavit = p_r_pago_cred_trad_inf.pesos92_infonavit + p_r_pago_cred_trad_inf.pesos97_infonavit

      -- se calcula la suma de aivs y pesos para PAGO CRÉDITO TRADICIONAL AFORE
      LET v_d_sumAivs_afore = p_r_pago_cred_trad_afo.aivs92_afore + p_r_pago_cred_trad_afo.aivs97_afore
      LET v_d_sumPesos_afore = p_r_pago_cred_trad_afo.pesos92_afore + p_r_pago_cred_trad_afo.pesos97_afore

      -- se calcula la diferencia entre las dos PAGO CRÉDITO TRADICIONAL
      LET v_d_diferencia_aivs = v_d_sumAivs_infonavit - v_d_sumAivs_afore
      LET v_d_diferencia_pesos = v_d_sumPesos_infonavit - v_d_sumPesos_afore

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_i_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_r_res.folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"

      -- Importes PAGO CRÉDITO TRADICIONAL INFONAVIT
      PRINTX p_r_pago_cred_trad_inf.aivs97_infonavit
      PRINTX p_r_pago_cred_trad_inf.aivs92_infonavit
      PRINTX v_d_sumAivs_infonavit
      PRINTX p_r_pago_cred_trad_inf.pesos97_infonavit
      PRINTX p_r_pago_cred_trad_inf.pesos92_infonavit
      PRINTX v_d_sumPesos_infonavit

      -- Importes PAGO CRÉDITO TRADICIONAL AFORE
      PRINTX p_r_pago_cred_trad_afo.aivs97_afore
      PRINTX p_r_pago_cred_trad_afo.aivs92_afore
      PRINTX v_d_sumAivs_afore
      PRINTX p_r_pago_cred_trad_afo.pesos97_afore
      PRINTX p_r_pago_cred_trad_afo.pesos92_afore
      PRINTX v_d_sumPesos_afore
      PRINTX v_d_diferencia_aivs
      PRINTX v_d_diferencia_pesos

END REPORT
