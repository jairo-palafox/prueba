--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

############################################################################
#Modulo            =>GRT                                                   #
#Programa          =>GRTE01                                                #
#Objetivo          =>Programa que integra el archivo Recurrente validado   #
#                    para al módulo de Solicitud de Saldo en Garantía      #
#Autor             =>Daniel Buendia, EFP                                   #
#Fecha inicio      =>19 Abril 2012                                         #
############################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

   DEFINE g_v_usuario          LIKE seg_usuario.usuario      -- nombre del usuario
   DEFINE g_d_pid              LIKE bat_ctr_proceso.pid      -- pid
   DEFINE g_i_proceso_cod      LIKE cat_proceso.proceso_cod  -- codigo del proceso
   DEFINE g_i_opera_cod        LIKE cat_operacion.opera_cod  -- codigo de la operacion
   DEFINE g_d_folio            LIKE glo_ctr_archivo.folio    -- numero de folio
   DEFINE g_v_arch_proceso     VARCHAR(100)                  -- nombre del archivo a integrar
   DEFINE g_id_cre_ctr_archivo LIKE cre_acreditado.id_cre_ctr_archivo -- id del archivo
   DEFINE g_c_ruta_bin         LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE g_c_ruta_listados    LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

#Objetivo: Función que realiza la integracion del archivo recurrente
MAIN
   DEFINE v_c_ruta_list_bat    LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_dt_f_lote          LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_i_operacion        LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_si_id_proceso      LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE v_i_proceso_cod      LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_i_opera_cod        LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_d_pid              LIKE bat_ctr_operacion.pid -- pid del proceso
   DEFINE v_c_programa_cod     LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_d_folio            LIKE glo_folio.folio -- folio de liquidación
   DEFINE v_r_cre_ctr_archivo  RECORD
             tot_registros     LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados     LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados    LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
             tot_sin_origen    LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
          END RECORD
   DEFINE v_si_origen          SMALLINT -- variable (para la función de mandatos)
   DEFINE v_si_cuenta_valor    SMALLINT -- variable que consulta si hay precio de acción
   DEFINE v_si_lote            INTEGER -- lote
   DEFINE v_s_comando          STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt           STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_mens_correo      STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo    STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo   STRING -- ruta y nombre del archivo adjunto en el correo 
   DEFINE v_v_nom_reporte      VARCHAR(80) -- nombre del reporte
   DEFINE v_si_tpo_transf      LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia DSE
   DEFINE v_si_tpo_originacion LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_i_id_lote_acpt     INTEGER -- total de registros aceptados
   DEFINE r_i_id_lote_rech     INTEGER-- total de registros rechazados hasta el momento

   DEFINE v_si_cod_error       SMALLINT
   DEFINE v_r_dse_ctr_archivo  RECORD LIKE dse_ctr_archivo.* -- registro de la tabla de control DSE
   DEFINE v_i_tot_registros    SMALLINT
   DEFINE v_i_estado           SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".GRTE01.log")

   DISPLAY "=INICIA GRTE01="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " ARCHIVO:      : ",g_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 21
   LET v_si_id_proceso = g_id_proceso_grt -- Solicitud de Saldo en Garantía 43 bis
   LET v_si_origen = 1
   LET v_si_tpo_transf = "19" -- DSE Creditos en garantia 43bis
   LET v_si_tpo_originacion = 2 -- Créditos en Garantía 43 bis

   -- se genera el folio
   LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- se verifica que exista precio de la acción para el dia de hoy
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_slct_precioAcc FROM v_s_qryTxt
   EXECUTE prp_slct_precioAcc INTO v_si_cuenta_valor

   IF v_si_cuenta_valor = 0 THEN
      DISPLAY " ERROR: NO EXISTE EL PRECIO DE ACCIÓN PARA EL DÍA DE HOY,",
              " POR LO QUE NO ES POSIBLE VERIFICAR APORTACIONES DE FORTALECIMIENTO DEL CREDITO"

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se consulta la fecha de lote
   LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                    "   FROM safre_tmp:tmp_cre_acred_grt_01\n",
                    "  WHERE fec_proceso IS NOT NULL"

   PREPARE prp_slct_fProc_orig FROM v_s_qryTxt
   EXECUTE prp_slct_fProc_orig INTO v_dt_f_lote

   -- se valida si fue posible obtener la fecha de lote en los archivos con originacion
   IF v_dt_f_lote IS NULL OR v_dt_f_lote = "12/31/1899" THEN
      -- se consulta la fecha de lote de los registros sin originacion
      LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                       "   FROM safre_tmp:tmp_cre_acred_grt_02\n",
                       "  WHERE fec_proceso IS NOT NULL"

      PREPARE prp_slct_fProc_sOrig FROM v_s_qryTxt
      EXECUTE prp_slct_fProc_sOrig INTO v_dt_f_lote
   END IF

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo, lote\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_f_lote,"'\n",
                    "    AND id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO g_id_cre_ctr_archivo, v_si_lote

   -- se verifica si fue posible obtener el identificador del archivo
   IF g_id_cre_ctr_archivo IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA RECURRENTE (NUEVOS ACREDITADOS Y CAMBIO DE ESTATUS)"
   -- se crea la sentencia que ejecuta el procedure que integra el archivo recurrente
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_grt_integra_recurrente(?)"

   PREPARE prp_integra_recurrente FROM v_s_qryTxt
   EXECUTE prp_integra_recurrente USING g_id_cre_ctr_archivo
                                   INTO r_b_valida, r_i_id_lote_acpt, r_i_id_lote_rech

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE INTEGRACIÓN RECURRENTE: ",r_b_valida

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA RECURRENTE (SIN ORIGINACIÓN)"
   -- se crea la sentencia que ejecuta el procedure que inserta los tipo de
   -- registros: 02, 03, 04, 05, 06, 07, 10, 11, 13 en la tabla sin originación
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_grt_integra_recurrente_sin_orig(?,?,?,?,?,?)"

   PREPARE prp_integra_recurr_sinorig FROM v_s_qryTxt
   EXECUTE prp_integra_recurr_sinorig USING g_v_usuario, g_v_arch_proceso,
                                            g_d_folio, r_i_id_lote_acpt,
                                            r_i_id_lote_rech, g_id_cre_ctr_archivo
                                       INTO r_b_valida

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF r_b_valida <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO: ",r_b_valida

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA RECURRENTE (HISTORICO DEUDOR)"
   -- se crea sentencia que ejecuta PROCEDURE que inserta en la tabla his y deudor
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:fn_grt_integra_recurrente_his_deudor(?,?)"

   PREPARE prp_inserta_hist_deud FROM v_s_qryTxt
   EXECUTE prp_inserta_hist_deud USING g_d_folio, g_id_cre_ctr_archivo
                                  INTO r_b_valida

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF r_b_valida <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO: ",r_b_valida

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOT_REGISTROS TOT_ACEPTADOS TOT_RECHAZADOS TOT_SIN_ORIGEN"
   DISPLAY v_r_cre_ctr_archivo.*

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se cambia el codigo de operación ya que se ejecutará la operación de marcaje
   LET g_i_opera_cod = 3

   DISPLAY " EJECUTA PROCESO DE MARCAJE"
   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " nohup time fglrun ",g_c_ruta_bin CLIPPED,"/GRTP02 ",
                                    g_v_usuario, " ",
                                    g_d_pid, " ",
                                    g_i_proceso_cod, " ",
                                    g_i_opera_cod, " ",
                                    g_d_folio, " ",
                                    g_v_arch_proceso, " ",
                                    g_id_cre_ctr_archivo, " 1> ",
                                    v_c_ruta_list_bat CLIPPED,
                                    "/nohup:",g_d_pid USING "&&&&&",":",
                                    g_i_proceso_cod USING "&&&&&",":",
                                    g_i_opera_cod USING "&&&&&",
                                    " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando

   DISPLAY " GENERA REPORTE"
   -- el reporte se generará en la operación de Integración
   LET g_i_opera_cod = 2

   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",g_d_folio,"\n",
                    "  WHERE pid = ",g_d_pid,"\n",
                    "    AND proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod , g_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegRecurr(v_v_nom_reporte, v_si_tpo_originacion)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN RECURRENTE CRÉDITOS EN GARANTÍA"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                          "Proceso      : RECEPCIÓN RECURRENTE CRÉDITOS EN GARANTÍA\n",
                          "Operacion    : INTEGRA ARCHIVO RECURRENTE\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_d_pid,
                          g_i_proceso_cod,
                          g_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)

  {
   #################################
   ### PRELIQUIDACIÓN AUTOMATICA ###
   #################################
   DISPLAY " EJECUTA PRELIQUIDACIÓN PARA SALDO EN GARANTÍA"
   -- se asigna el código de proceso y operacion de la preliquidación
   LET v_i_proceso_cod = g_proc_cod_grt_liquidacion   -- liquidación fortalecmiento crédito gtia
   LET v_i_opera_cod   = 1   -- preliquidación fortalecmiento crédito gtia
   LET v_d_pid         = 0   -- Para la validación no es necesario generar PID
   LET v_d_folio       = 0

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que ejecuta la funcion que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, g_v_usuario)   

   DISPLAY " PID: ",v_d_pid
   IF v_d_pid IS NULL THEN
      DISPLAY " ERROR: No fue posible generar PID. No continuará con el proceso\n",
              "        de Preliquidación "

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " nohup time fglrun ",g_c_ruta_bin CLIPPED,"/GRTP29 ",
                                    g_v_usuario, " ",
                                    v_d_pid, " ",
                                    v_i_proceso_cod, " ",
                                    v_i_opera_cod, " ",
                                    v_d_folio, " ",
                                    g_id_cre_ctr_archivo, " 1> ",
                                    v_c_ruta_list_bat CLIPPED,
                                    "/nohup:",v_d_pid USING "&&&&&",":",
                                    v_i_proceso_cod USING "&&&&&",":",
                                    v_i_opera_cod USING "&&&&&",
                                    " 2>&1 &"

   -- se ejecuta el comando armado
   RUN v_s_comando
  }

   DISPLAY "=FIN="

END MAIN

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegRecurr(p_v_nom_reporte, p_si_tpo_originac)
   DEFINE p_v_nom_reporte   VARCHAR(80) -- nombre del reporte
   DEFINE p_si_tpo_originac LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_r_rpt_res       RECORD -- registro de resumen
             nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion   LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             tpo_originac   LIKE cat_cre_originacion.tpo_originacion, -- tipo de originación
             originac_desc  LIKE cat_cre_originacion.originacion_desc, -- descripción de originación
             usuario        LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros  INTEGER, -- numero total de registros
             tot_aceptados  INTEGER, -- numero total de regs aceptados
             tot_rechazados INTEGER, -- numero total de regs rechazados
             tot_sin_origen INTEGER  -- numero total de regs sin origen
          END RECORD
   DEFINE v_r_reporte_det   RECORD -- registro de detalle
             tpo_detalle    SMALLINT, -- tipo detalle: 1-Detalle Rechazados, 2-Detalle Sin Originación
             tpo_registro   LIKE cre_rch_acreditado.tpo_registro, -- tipo de registro
             nss            LIKE cre_rch_acreditado.nss, -- nss del derechohabiente
             num_credito    LIKE cre_rch_acreditado.num_credito, -- numero de crédito
             sdo_deudor     LIKE cre_rch_acreditado.sdo_deudor, -- saldo deudor
             f_otorga       LIKE cre_rch_acreditado.f_otorga, -- fecha de otorgamiento
             valor_dscto    LIKE cre_rch_acreditado.valor_dscto, -- valor del descuento
             f_ini_dscto    LIKE cre_rch_acreditado.f_ini_dscto, -- fecha inicial del descuento
             tpo_rch        LIKE cre_rch_acreditado.tpo_rch, -- tipo de rechazo
             estado         LIKE cre_rch_acreditado.estado, -- estado
             des_rch        LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
             num_registros  INTEGER -- numero de registros (solo se usa en las marcas)
          END RECORD
   DEFINE v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_i_cuenta_rch_marca  LIKE cre_ctr_archivo.tot_rechazados -- número de registros rechazados en el marcaje
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_i_folio_format      INTEGER -- numero de folio con formato
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt            STRING -- contiene una sentencia sql a ejecutar
   DEFINE v_si_marca            LIKE sfr_marca.marca -- marca
   DEFINE v_v_marca_aux         VARCHAR(5) -- numero de marca
   DEFINE v_i_tot_marcados      INTEGER -- total de registro marcados
   DEFINE r_b_valida            SMALLINT

   -- se inicializan variables
   LET v_i_tot_marcados = 0

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTE011.4rp") THEN
      -- se indica la salida del reporte
      --LET p_v_nom_reporte = g_v_usuario CLIPPED, "-GRTL02-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(g_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue abrir la plantilla del reporte"

      RETURN
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que cuenta el numero de registros rechazados en el marcaje
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 150\n",
                    "    AND id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cuenta_rch_marca FROM v_s_qryTxt
   EXECUTE prp_cuenta_rch_marca INTO v_i_cuenta_rch_marca

   IF v_i_cuenta_rch_marca > 0 THEN
      -- se incrementa el numero de registros rechazados y se decrementan los aceptados
      LET v_r_cre_ctr_arch.tot_aceptados = v_r_cre_ctr_arch.tot_aceptados - v_i_cuenta_rch_marca
      LET v_r_cre_ctr_arch.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados + v_i_cuenta_rch_marca
   END IF

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod,"\n",
                    "    AND folio = ",g_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.id_operacion   = v_r_cre_ctr_arch.operacion
   LET v_r_rpt_res.desc_operacion = fn_obt_desc_operacion(v_r_rpt_res.id_operacion)
   LET v_r_rpt_res.tpo_originac   = p_si_tpo_originac
   LET v_r_rpt_res.originac_desc  = fn_obt_desc_originacion(p_si_tpo_originac)
   LET v_r_rpt_res.usuario        = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros  = v_r_cre_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_cre_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados
   LET v_r_rpt_res.tot_sin_origen = v_r_cre_ctr_arch.tot_sin_origen

   -- se le da formato al folio
   LET v_i_folio_format = g_d_folio --USING "&&&&&&&&&&"

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_recurr TO XML HANDLER v_manejador_rpt

   -- Se busca el detalle de las marcas
   LET v_s_qryTxt = " SELECT a.marca, b.descripcion_marca, count(*)\n",
                    "   FROM sfr_marca_activa a, sfr_marca b\n",
                    "  WHERE a.id_derechohabiente IN (\n",
                    "        SELECT id_derechohabiente\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,")\n",
                    "    AND a.marca = b.marca\n",
                    "    AND a.folio = ",g_d_folio,"\n",
                    "  GROUP BY a.marca, b.descripcion_marca",
                    "  ORDER BY a.marca"

   PREPARE prp_regs_marcados FROM v_s_qryTxt
   DECLARE cur_regs_marcados CURSOR FOR prp_regs_marcados

   FOREACH cur_regs_marcados INTO v_si_marca, v_r_reporte_det.des_rch, v_r_reporte_det.num_registros
      -- se asigna la marca a una variable de tipo varchar
      LET v_v_marca_aux = v_si_marca

      -- se concatena la marca con la descripción
      LET v_r_reporte_det.des_rch = v_v_marca_aux CLIPPED, " - ", v_r_reporte_det.des_rch

      -- se indica que el detalle es de la marca (4)
      LET v_r_reporte_det.tpo_detalle = 4

      -- salida del reporte
      OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)

      -- se incrementa el contador de registros marcados
      LET v_i_tot_marcados = v_i_tot_marcados + 1
   END FOREACH

   -- verifica si existen registros rechazados
   IF v_r_rpt_res.tot_rechazados > 0 THEN
      -- existen registros rechazados. Se busca el detalle
      LET v_s_qryTxt = " SELECT 1, tpo_registro, nss, num_credito, sdo_deudor, f_otorga,\n",
                       "        valor_dscto, f_ini_dscto, tpo_rch, estado\n",
                       "   FROM cre_rch_acreditado\n",
                       "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

      PREPARE prp_cre_rch_acred FROM v_s_qryTxt
      DECLARE cur_cre_rch_acred CURSOR FOR prp_cre_rch_acred

      FOREACH cur_cre_rch_acred INTO v_r_reporte_det.*
         -- se invoca la función que obtiene la descripción del estado
         LET v_r_reporte_det.des_rch = f_busca_desc_rch(v_r_reporte_det.estado)

         -- salida del reporte
         OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
      END FOREACH

      -- si existen registros rechazados en la marca. Se busca el detalle
      LET v_s_qryTxt = " SELECT 1, '00', afi.nss, num_credito, sdo_deudor, f_otorga,\n",
                       "        valor_dscto, f_ini_dscto, estado, estado\n",
                       "   FROM cre_acreditado cre, afi_derechohabiente afi\n",
                       "  WHERE cre.estado = 150\n",
                       "    AND cre.id_derechohabiente = afi.id_derechohabiente\n",
                       "    AND cre.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

      PREPARE prp_cre_rch_marca FROM v_s_qryTxt
      DECLARE cur_cre_rch_marca CURSOR FOR prp_cre_rch_marca

      FOREACH cur_cre_rch_marca INTO v_r_reporte_det.*
         -- se invoca la función que obtiene la descripción del estado
         LET v_r_reporte_det.des_rch = f_busca_desc_rch_marca(v_r_reporte_det.estado)
         --LET v_r_reporte_det.tpo_detalle = 1
         -- salida del reporte
         OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
      END FOREACH
   END IF

   IF v_r_rpt_res.tot_rechazados = 0 AND
      --v_r_rpt_res.tot_sin_origen = 0  AND 
      v_i_tot_marcados = 0 THEN
      -- se asigna estatus 3, indica que no hay registros rechazados ni sin originación
      LET v_r_reporte_det.tpo_detalle = 3
      -- salida del reporte
      OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
   END IF

   -- finaliza el reporte
   FINISH REPORT reporte_integ_recurr

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_recurr(p_r_res, p_r_reporte_det, p_i_folio)
   DEFINE p_i_folio         VARCHAR(10) -- numero de folio con formato
   DEFINE p_r_res           RECORD
             nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion   LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             tpo_originac   LIKE cat_cre_originacion.tpo_originacion, -- tipo de originación
             originac_desc  LIKE cat_cre_originacion.originacion_desc, -- descripción de originación
             usuario        LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros  INTEGER, -- numero total de registros
             tot_aceptados  INTEGER, -- numero total de regs aceptados
             tot_rechazados INTEGER, -- numero total de regs rechazados
             tot_sin_origen INTEGER  -- numero total de regs sin origen
          END RECORD
   DEFINE p_r_reporte_det   RECORD
             tpo_detalle   SMALLINT, -- tipo detalle: 1-Detalle Rechazados, 2-Detalle Sin Originación
             tpo_registro   LIKE cre_rch_acreditado.tpo_registro, -- tipo de registro
             nss            LIKE cre_rch_acreditado.nss, -- nss del derechohabiente
             num_credito    LIKE cre_rch_acreditado.num_credito, -- numero de crédito
             sdo_deudor     LIKE cre_rch_acreditado.sdo_deudor, -- saldo deudor
             f_otorga       LIKE cre_rch_acreditado.f_otorga, -- fecha de otorgamiento
             valor_dscto    LIKE cre_rch_acreditado.valor_dscto, -- valor del descuento
             f_ini_dscto    LIKE cre_rch_acreditado.f_ini_dscto, -- fecha inicial del descuento
             tpo_rch        LIKE cre_rch_acreditado.tpo_rch, -- tipo de rechazo
             estado         LIKE cre_rch_acreditado.estado, -- estado
             des_rch        LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
             num_registros  INTEGER -- numero de registros (solo se usa en las marcas)
          END RECORD
   DEFINE v_v_desc_detalle     VARCHAR(50)
   DEFINE v_v_desc_campo       VARCHAR(50)
   DEFINE v_sum_sdo_deudor     DECIMAL(22,6)
   DEFINE v_sum_val_dsto       DECIMAL(22,6)
   DEFINE v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_sum_sdo_deudor = 0
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX g_v_usuario
      PRINTX p_i_folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini -- USING "DD-MM-YYYY"
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.id_operacion
      PRINTX p_r_res.desc_operacion
      PRINTX p_r_res.originac_desc
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_r_res.tot_sin_origen USING "#########&"

   BEFORE GROUP OF p_r_reporte_det.tpo_detalle
      IF p_r_reporte_det.tpo_detalle = 1 THEN
         LET v_v_desc_detalle = "DETALLE RECHAZADOS"
         LET v_v_desc_campo = "Estado"
      ELSE
         LET v_v_desc_detalle = "DETALLE SIN ORIGINACIÓN"
         LET v_v_desc_campo = "Registro"
      END IF
      PRINTX p_r_reporte_det.tpo_detalle
      PRINTX v_v_desc_detalle
      PRINTX v_v_desc_campo

   ON EVERY ROW
      IF p_r_reporte_det.tpo_detalle = 1 THEN
         LET v_v_desc_detalle = "DETALLE RECHAZADOS"
         LET v_v_desc_campo = "Estado"
      ELSE
         LET v_v_desc_detalle = "DETALLE SIN ORIGINACIÓN"
         LET v_v_desc_campo = "Registro"
      END IF
      LET v_sum_sdo_deudor = v_sum_sdo_deudor + p_r_reporte_det.sdo_deudor
      LET v_sum_val_dsto = v_sum_val_dsto + p_r_reporte_det.valor_dscto
      PRINTX p_r_reporte_det.nss
      PRINTX p_r_reporte_det.num_credito
      PRINTX p_r_reporte_det.sdo_deudor
      PRINTX p_r_reporte_det.f_otorga USING "DD-MM-YYYY"
      PRINTX p_r_reporte_det.valor_dscto
      PRINTX p_r_reporte_det.f_ini_dscto USING "DD-MM-YYYY"
      PRINTX p_r_reporte_det.tpo_rch
      PRINTX p_r_reporte_det.estado
      PRINTX p_r_reporte_det.des_rch
      PRINTX p_r_reporte_det.num_registros

   ON LAST ROW
      PRINTX v_sum_sdo_deudor
      PRINTX v_sum_val_dsto

   PAGE TRAILER
      IF p_r_reporte_det.tpo_detalle = 1 THEN
         LET v_v_desc_detalle = "DETALLE RECHAZADOS"
         LET v_v_desc_campo = "Estado"
      ELSE
         LET v_v_desc_detalle = "DETALLE SIN ORIGINACIÓN"
         LET v_v_desc_campo = "Registro"
      END IF
END REPORT

#Objetivo: Busca la descripción del estatus de rechazo en catalogo
FUNCTION f_busca_desc_rch(p_si_estado)
   DEFINE p_si_estado     LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_c_desc_estado LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_estado\n",
                    "   FROM cat_rch_acreditado\n",
                    "  WHERE estado = ",p_si_estado

   PREPARE prp_desc_estado FROM v_s_qryTxt
   EXECUTE prp_desc_estado INTO v_c_desc_estado

   -- se verifica si se encontró descripción
   IF v_c_desc_estado IS NULL THEN
      LET v_c_desc_estado = "Descripción no encontrada"
   END IF

   RETURN v_c_desc_estado
END FUNCTION

#Objetivo: Busca la descripción del estatus de rechazo en catalogo
FUNCTION f_busca_desc_rch_marca(p_si_estado)
   DEFINE p_si_estado     LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_c_desc_estado LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_rechazo\n",
                    "   FROM cat_rechazo\n",
                    "  WHERE tpo_rechazo = 'SIS'\n",
                    "    AND cod_rechazo = '",p_si_estado,"'"

   PREPARE prp_desc_rechazo FROM v_s_qryTxt
   EXECUTE prp_desc_rechazo INTO v_c_desc_estado

   -- se verifica si se encontró descripción
   IF v_c_desc_estado IS NULL THEN
      LET v_c_desc_estado = "Descripción no encontrada"
   END IF

   RETURN v_c_desc_estado
END FUNCTION

#Objetivo: Busca la descripción del tipo de registro
FUNCTION f_busca_desc_registro(p_tpo_registro)
   DEFINE p_tpo_registro  LIKE cat_registro_interno.tpo_registro -- tipo de registro
   DEFINE v_c_desc_reg    LIKE cat_registro_interno.desc_registro -- descripción del registro
   DEFINE v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_registro\n",
                    "   FROM cat_registro_interno\n",
                    "  WHERE tpo_registro = '",p_tpo_registro,"'"

   PREPARE prp_desc_registro FROM v_s_qryTxt
   EXECUTE prp_desc_registro INTO v_c_desc_reg

   -- se verifica si se encontró descripción
   IF v_c_desc_reg IS NULL THEN
      LET v_c_desc_reg = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN v_c_desc_reg
END FUNCTION

#Objetivo: Busca la descripción de la operación
FUNCTION fn_obt_desc_operacion(p_c_operacion)
   DEFINE p_c_operacion  LIKE cat_operacion_prc.operacion -- operación
   DEFINE v_c_desc_opera LIKE cat_operacion_prc.desc_operacion -- descripción de la operación
   DEFINE v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_operacion\n",
                    "   FROM cat_operacion_prc\n",
                    "  WHERE operacion = '",p_c_operacion,"'"

   PREPARE prp_desc_operacion FROM v_s_qryTxt
   EXECUTE prp_desc_operacion INTO v_c_desc_opera

   -- se verifica si se encontró descripción
   IF v_c_desc_opera IS NULL THEN
      LET v_c_desc_opera = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN v_c_desc_opera
END FUNCTION

#Objetivo: Busca la descripción del tipo de originación
FUNCTION fn_obt_desc_originacion(p_si_tpo_originac)
   DEFINE p_si_tpo_originac LIKE cat_cre_originacion.tpo_originacion -- tipo de originación
   DEFINE p_c_originac_desc LIKE cat_cre_originacion.originacion_desc -- descripción de originación
   DEFINE v_s_qryTxt        STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT originacion_desc\n",
                    "   FROM cat_cre_originacion\n",
                    "  WHERE tpo_originacion = ",p_si_tpo_originac

   PREPARE prp_desc_originacion FROM v_s_qryTxt
   EXECUTE prp_desc_originacion INTO p_c_originac_desc

   -- se verifica si se encontró descripción
   IF p_c_originac_desc IS NULL THEN
      LET p_c_originac_desc = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN p_c_originac_desc
END FUNCTION
