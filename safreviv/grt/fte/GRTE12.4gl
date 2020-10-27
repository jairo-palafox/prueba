--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

################################################################################
#Modulo            =>GRT                                                       #
#Programa          =>GRTE12                                                    #
#Objetivo          =>Programa para integrar el archivo de                      #
#                    devolución de saldos Excedentes para el                   #
#                    módulo de Créditos en Garantia 43 bis                     #
#Autor             =>Daniel Buendia, EFP                                       #
#Fecha inicio      =>29 Mayo 2012                                              #
################################################################################

DATABASE safre_viv

   DEFINE p_v_usuario       LIKE seg_usuario.usuario  -- nombre del usuario
   DEFINE p_d_pid           LIKE bat_ctr_proceso.pid  -- pid
   DEFINE p_i_proceso_cod   LIKE cat_proceso.proceso_cod  -- código del proceso
   DEFINE p_i_opera_cod     LIKE cat_operacion.opera_cod  -- código de la operación
   DEFINE p_d_folio         LIKE glo_ctr_archivo.folio  -- numero de folio
   DEFINE p_v_arch_proceso  VARCHAR(100) -- nombre del archivo a integrar

#Objetivo: Funcion que realiza la integracion del archivo de devoluciones saldo
MAIN
   DEFINE v_s_qryTxt         STRING  -- guarda una sentencia SQL a ejecutar
   DEFINE v_v_nom_reporte    VARCHAR(80)  -- nombre del reporte
   DEFINE v_s_mens_correo    STRING  -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo  STRING  -- contiene el titulo del correo
   DEFINE v_s_archivo_correo STRING  -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_r_dse_ctr_arch   RECORD
             tot_registros   LIKE dse_ctr_archivo.tot_registros, -- numero total de registros
             tot_aceptados   LIKE dse_ctr_archivo.tot_aceptados, -- numero de registros aceptados
             tot_rechazados  LIKE dse_ctr_archivo.tot_rechazados -- numero de registros rechazados
      END RECORD
   DEFINE v_c_programa_cod   LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE r_b_existe_err     BOOLEAN  -- indica si ocurrio un error durante un subproceso
   DEFINE r_c_ruta_bin       LIKE seg_modulo.ruta_bin  -- ruta del bin de cta
   DEFINE r_c_ruta_listados  LIKE seg_modulo.ruta_listados -- ruta listados cta
   DEFINE r_b_valida         SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)   

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE12.log")

   DISPLAY "=INICIA GRTE12="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET r_b_existe_err = FALSE -- se asume que no ocurrirá error en el subproceso

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY " INTEGRA DEV SALDOS EXC"
   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE PROCEDURE fn_dse_integra_devsdos_grt(?,?,?,?)"

   PREPARE prp_integra_rech_saldo FROM v_s_qryTxt
   EXECUTE prp_integra_rech_saldo USING p_d_pid, p_v_usuario, p_v_arch_proceso, p_d_folio

   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",p_d_folio,"\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se muestran las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE folio = ",p_d_folio

   PREPARE prp_ccontrol_dse FROM v_s_qryTxt
   EXECUTE prp_ccontrol_dse INTO v_r_dse_ctr_arch.*

   LET v_s_qryTxt = "SELECT COUNT(*)
                       FROM safre_tmp:tmp_nss_no_catalogados_grt"

   PREPARE prp_no_catalogados FROM v_s_qryTxt
   EXECUTE prp_no_catalogados INTO v_r_dse_ctr_arch.tot_rechazados

   DISPLAY " TOTAL REGISTROS : ",v_r_dse_ctr_arch.tot_registros
   DISPLAY " TOTAL ACEPTADOS : ",v_r_dse_ctr_arch.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS: ",v_r_dse_ctr_arch.tot_rechazados

   DISPLAY "\n GENERA REPORTE CIFRAS CONTROL\n"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- se asigna el nombre del reporte
   --LET v_v_nom_reporte = p_v_usuario CLIPPED, "-GRTL32-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegraDSE(r_c_ruta_listados, v_v_nom_reporte) RETURNING r_b_existe_err

   -- en caso de no haber ocurrido error al generar el reporte se envía éste por correo
   IF NOT r_b_existe_err THEN
      DISPLAY "\n ENVIA CORREO DEL REPORTE"
      -- se asigna el titulo del correo
      LET v_s_titulo_correo = "Proceso: DEVOLUCIÓN DE SALDOS EXCEDENTES GRT"

      -- se asigna el archivo a adjuntar
      LET v_s_archivo_correo = r_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

      -- se asigna el cuerpo del correo
      LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                             "Proceso      : DEVOLUCIÓN DE SALDOS EXCEDENTES GRT\n",
                             "Operación    : INTEGRA DEVOLUCIÓN SALDOS EXC GRT\n",
                             "Fecha Inicio : ",TODAY,"\n",
                             "Fecha Fin    : ",TODAY

      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(p_d_pid,
                             p_i_proceso_cod,
                             p_i_opera_cod,
                             v_s_archivo_correo,
                             v_s_titulo_correo,
                             v_s_mens_correo)
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegraDSE(p_c_ruta_listados, p_v_nom_reporte)
   DEFINE p_c_ruta_listados      LIKE seg_modulo.ruta_listados  -- ruta listados cta
   DEFINE p_v_nom_reporte        VARCHAR(80)  -- nombre del reporte
   DEFINE v_r_rpt_res         RECORD -- registro de resumen
             folio               INTEGER, -- numero de folio con formato
             nom_archivo         LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini        LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin        LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             usuario             LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros       INTEGER, -- numero total de registros
             tot_aceptados       INTEGER, -- numero total de regs aceptados
             tot_rechazados      INTEGER -- numero total de regs rechazados
      END RECORD
   DEFINE v_r_dse_ctr_arch       RECORD LIKE dse_ctr_archivo.*  -- registro de dse ctr archivo
   DEFINE v_r_bat_ctr_opera      RECORD LIKE bat_ctr_operacion.*  -- registro de bat ctr operación
   DEFINE v_manejador_rpt        OM.SaxDocumentHandler  # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt             STRING  -- contiene una sentencia sql a ejecutar
   DEFINE v_b_existe_err         BOOLEAN -- indica si ocurrió error durante la generación del reporte
   DEFINE v_tot_viv97            DECIMAL(15,2)
   DEFINE v_tot_rech_viv97       DECIMAL(15,2)

   -- se inicializan variables
   LET v_b_existe_err   = FALSE -- se asume que no ocurrirá error
   LET v_tot_viv97      = 0
   LET v_tot_rech_viv97 = 0

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTE121.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(p_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
      -- se indica que ha ocurrido un error y no continua
      LET v_b_existe_err = TRUE

      RETURN v_b_existe_err
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    " FROM dse_ctr_archivo\n",
                    " WHERE folio = ",p_d_folio

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_dse_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod   = ",p_i_opera_cod,"\n",
                    "    AND folio       = ",p_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   LET v_s_qryTxt = "SELECT COUNT(*)
                       FROM safre_tmp:tmp_nss_no_catalogados_grt"

   PREPARE prp_no_cat FROM v_s_qryTxt
   EXECUTE prp_no_cat INTO v_r_dse_ctr_arch.tot_rechazados

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.usuario        = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros  = v_r_dse_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_dse_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_dse_ctr_arch.tot_rechazados
   LET v_r_rpt_res.folio          = p_d_folio

   -- Se obtienen los montos totales de viv 92 y viv97
   LET v_s_qryTxt = "SELECT SUM(saldo_vivienda97) / 100
                       FROM safre_tmp:tmp_dse_devol_det_grt"

   PREPARE prp_totales FROM v_s_qryTxt
   EXECUTE prp_totales INTO v_tot_viv97

   LET v_s_qryTxt = "SELECT SUM(saldo_vivienda97) / 100
                       FROM safre_tmp:tmp_nss_no_catalogados_grt"

   PREPARE prp_totales_rech FROM v_s_qryTxt
   EXECUTE prp_totales_rech INTO v_tot_rech_viv97

   DISPLAY "\n ***   ACEPTADOS   ***"
   DISPLAY " MONTO TOTAL DE VIVIENDA 97 : ", v_tot_viv97
   DISPLAY "\n ***   RECHAZADOS   ***"
   DISPLAY " MONTO TOTAL DE VIVIENDA 97 : ", v_tot_rech_viv97

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integra_dse TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_integra_dse(v_r_rpt_res.*,v_tot_viv97,v_tot_rech_viv97)

   -- finaliza el reporte
   FINISH REPORT reporte_integra_dse

   RETURN v_b_existe_err
END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integra_dse(p_r_res,p_tot_viv97,p_tot_rech_viv97)
   DEFINE p_r_res           RECORD
             folio             INTEGER, -- numero de folio con formato
             nom_archivo       LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini      LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin      LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             usuario           LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros     INTEGER, -- numero total de registros
             tot_aceptados     INTEGER, -- numero total de regs aceptados
             tot_rechazados    INTEGER -- numero total de regs rechazados
      END RECORD
   DEFINE v_fecha_reporte      DATE
   DEFINE p_tot_viv97          DECIMAL(15,2)
   DEFINE p_tot_rech_viv97     DECIMAL(15,2)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_v_usuario
      PRINTX p_r_res.folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_tot_viv97
      PRINTX p_tot_rech_viv97

END REPORT
