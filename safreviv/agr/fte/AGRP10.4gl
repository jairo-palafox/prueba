--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#########################################################################
#Módulo            =>AGR                                                #
#Programa          =>AGRP10                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                   mación de deudor vs tmp deudor no atendidas AG      #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>09 Abril 2012                                      #
#########################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

# Objetivo: Conciliar la información de recurrente
MAIN

   DEFINE p_v_usuario           LIKE seg_usuario.usuario     # Nombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid     # pid
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod # Código del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod # Código de la operacion
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio   # Número de folio
   DEFINE p_v_arch_proceso      VARCHAR(100)                 # Nombre del archivo a integrar
   DEFINE v_c_programa_cod      LIKE cat_operacion.programa_cod -- nombre del programa

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

   DEFINE v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_d_pid_arch          LIKE bat_ctr_operacion.pid -- pid del proceso
   DEFINE v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo -- id del archiovo
   DEFINE v_si_operacion        LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt            STRING                # Guarda una sentencia SQL a ejecutar
   DEFINE v_nom_reporte         VARCHAR(80) -- nombre del reporte
   DEFINE v_s_mens_correo       STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo     STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo    STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE r_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados       LIKE seg_modulo.ruta_listados
   DEFINE r_b_valida            SMALLINT

   DEFINE v_tabla                CHAR(20)
   DEFINE v_aivs92               DECIMAL(16,6)
   DEFINE v_pesos92              DECIMAL(12,2)
   DEFINE v_aivs97               DECIMAL(16,6)
   DEFINE v_pesos97              DECIMAL(12,2)

   # Se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_proceso  = ARG_VAL(6)

   # se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP10.log")

   DISPLAY "=INICIA AGRP10="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " CONCILIACIÓN DE SOLICITUDES NO ATENDIDAS AGR"

   -- Se inicializan variables
   LET v_si_operacion = 14 -- Solicitudes no atendidas

   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

   DISPLAY " Genera reporte"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   # se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRP101.4rp") THEN  -- if  the file loaded OK
      #IF fgl_report_loadCurrentSettings(NULL) THEN
      --CALL fgl_report_selectLogicalPageMapping("multipage")
      --CALL fgl_report_configureMultipageOutput(1, NULL, FALSE)
      # Se indica que el reporte saldra en excel
      --CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_agr,"\n",
                    "    AND operacion = ",v_si_operacion

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_d_id_cre_ctr_arch
   DISPLAY " Identificador del archivo: ",v_d_id_cre_ctr_arch

   -- se valida el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   # Inicia el reporte de registros con rechazo
   START REPORT reporte_noAtendidas TO XML HANDLER v_manejador_rpt

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   LET v_d_pid_arch = fn_max_pid(g_proc_cod_agr_no_atendidas, 2)
   DISPLAY " PID del archivo: ",v_d_pid_arch

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",v_d_pid_arch,"\n",
                    "    AND proceso_cod = ",g_proc_cod_agr_no_atendidas,"\n",
                    "    AND opera_cod = 2"

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
   LET v_r_rpt_res.nom_archivo    = v_r_cre_ctr_arch.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.usuario        = v_r_cre_ctr_arch.usuario
   LET v_r_rpt_res.tot_registros  = v_r_cre_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_cre_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados
   LET v_r_rpt_res.folio          = v_r_cre_ctr_arch.folio_archivo

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
                       "          FROM safre_tmp:tmp_deudor_no_aten_agr)\n",
                       "    AND subcuenta = 4\n",
                       "    AND movimiento IN (492, 82)" --(72, 82)

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

      -- Obtiene la suma del Pago Credito Tradicional Infonavit Pesos para Subcuenta 8 (Vivienda 92)
      LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)\n",
                          "   FROM ",v_tabla,"\n",
                       "  WHERE id_derechohabiente IN (\n",
                       "        SELECT id_derechohabiente\n",
                       "          FROM safre_tmp:tmp_deudor_no_aten_agr)\n",
                       "    AND subcuenta = 8\n",
                       "    AND movimiento IN (492, 82)" --(72, 82)

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

   # Salida del reporte
   OUTPUT TO REPORT reporte_noAtendidas(v_r_rpt_res.*, p_v_usuario, p_d_folio, v_r_pago_cred_trad_inf.*, v_r_pago_cred_trad_afo.*)

   # Si ocurrió algun error en la consulta, se cambia el estado de la operacion en erronea
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "ERROR EN CONSULTA (CODIGO):"||SQLCA.SQLCODE
      DISPLAY "MENSAJE SQL:"||SQLCA.SQLERRM
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida
                                     
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF
   # Finaliza el reporte
   FINISH REPORT reporte_noAtendidas

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

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE NO ATENDIDAS ANUALIDADES GARANTIZADAS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : NO ATENDIDAS AGR\n",
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

#OBJETIVO: Genera el reporte de No Aten
REPORT reporte_noAtendidas(p_r_res, p_v_usuario, p_d_folio, p_r_pago_cred_trad_inf, p_r_pago_cred_trad_afo)

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

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE p_d_folio              LIKE glo_ctr_archivo.folio -- numero de folio

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