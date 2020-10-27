--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRE19                                        #
#Objetivo          =>Programa para integrar el archivo fondo de    #
#                    ahorro 72 que ha sido validado                #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>26 JUNIO 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

DEFINE g_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
       g_d_pid              LIKE bat_ctr_proceso.pid, -- pid
       g_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
       g_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
       g_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
       g_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       g_c_ruta_listados    LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

#Objetivo: Función que realiza la integracion del archivo recurrente
MAIN
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_dt_f_lote         LIKE cre_ctr_archivo.f_lote, -- fecha del lote
          v_i_transferencia   LIKE dse_ctr_archivo.tpo_transferencia, -- operacion del proceso
          v_si_tpo_originac   LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_c_programa_cod    LIKE cat_operacion.programa_cod, -- nombrel del programa origen
          v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE dse_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE dse_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE dse_ctr_archivo.tot_rechazados -- total rechazados
          END RECORD,
          v_si_origen         SMALLINT, -- variable (para la función de mandatos)
          v_dt_fec_carga      DATE, -- fecha de carga (para la función de mandatos)
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_s_mens_correo     STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo   STRING, -- contiene el titulo del correo
          v_s_archivo_correo  STRING, -- ruta y nombre del archivo adjunto en el correo 
          v_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_i_id_lote_acpt    INTEGER, -- total de registros aceptados
          r_i_id_lote_rechz   INTEGER -- total de registros rechazados

   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".ACRE19.log")

   DISPLAY "=INICIA ACRE19="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " ARCHIVO:      : ",g_v_arch_proceso

   -- se inicializan variables
   LET v_i_transferencia = "15" -- Transferencia de Acreditados
   LET v_si_origen = 1
   LET v_dt_fec_carga = TODAY
   LET v_si_tpo_originac = 1 -- Transferencia de Acreditados

   -- se genera el folio
   LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acr") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se consulta la fecha de lote
   LET v_dt_f_lote = TODAY
   
   DISPLAY " INTEGRA DEVOLUCION DE FONDO DE AHORRO 72"
   -- se crea la sentencia que ejecuta el procedure que integra el archivo recurrente
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_integra_fondo_ahorro_72(?,?,?,?,?)"

   PREPARE prp_integra_recurrente FROM v_s_qryTxt
   EXECUTE prp_integra_recurrente USING g_v_usuario,
                                        g_v_arch_proceso,
                                        g_d_pid,
                                        g_d_folio,
                                        v_i_transferencia
                                   INTO r_b_valida, r_i_id_lote_acpt, r_i_id_lote_rechz

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE INTEGRACIÓN FONDO DE AHORRO 72: ",r_b_valida

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se muestran las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE folio = ",g_d_folio

   --DISPLAY "v_s_qryTxt: ", v_s_qryTxt CLIPPED
   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOTAL REGISTROS : ",v_r_cre_ctr_archivo.tot_registros
   DISPLAY "TOTAL ACEPTADOS : ",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY "TOTAL RECHAZADOS: ",v_r_cre_ctr_archivo.tot_rechazados

   DISPLAY " GENERA REPORTE"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod, g_i_opera_cod)

   -- se asigna el nombre del reporte
   --LET v_v_nom_reporte = g_v_usuario CLIPPED, "-ACRL45-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"
   LET v_v_nom_reporte = g_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED, "-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegFondo72(v_v_nom_reporte, v_si_tpo_originac)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN FONDO DE AHORRO 72"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                          "Proceso      : RECEPCIÓN FONDO DE AHORRO 72\n",
                          "Operacion    : INTEGRA ARCHIVO FONDO DE AHORRO 72\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_d_pid,
                          g_i_proceso_cod,
                          g_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)

   -- se cambia el codigo de operación ya que se ejecutará la operación de marcaje
   LET g_i_opera_cod = 3

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegFondo72(p_v_nom_reporte, p_si_tpo_originac)
   DEFINE p_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          p_si_tpo_originac LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_r_rpt_res       RECORD -- registro de resumen
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
          END RECORD,
          v_r_cre_ctr_arch  RECORD LIKE dse_ctr_archivo.*, -- registro de dse ctr archivo
          v_r_bat_ctr_opera RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_i_folio_format  INTEGER, -- numero de folio con formato
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt        STRING, -- contiene una sentencia sql a ejecutar
          r_b_valida        SMALLINT

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("ACRE19.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(g_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"
      CALL fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod) 
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE folio = ",g_d_folio

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE folio = ",g_d_folio,"\n",
                    "    AND proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.id_operacion   = g_proc_cod_acr_conf_dse -- proceso
   LET v_r_rpt_res.desc_operacion = fn_obt_desc_operacion(v_r_rpt_res.id_operacion)
   LET v_r_rpt_res.tpo_originac   = p_si_tpo_originac
   LET v_r_rpt_res.originac_desc  = fn_obt_desc_originacion(p_si_tpo_originac)
   LET v_r_rpt_res.usuario        = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros  = v_r_cre_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_cre_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados
   --LET v_r_rpt_res.tot_sin_origen = v_r_cre_ctr_arch.tot_sin_origen

   -- se le da formato al folio
   LET v_i_folio_format = g_d_folio --USING "&&&&&&&&&&"

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_recurr TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_i_folio_format)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_recurr

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_recurr(p_r_res, p_i_folio)
   DEFINE p_i_folio         VARCHAR(10), -- numero de folio con formato
          p_r_res           RECORD
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
          END RECORD,
          v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX g_v_usuario
      PRINTX p_i_folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.id_operacion
      PRINTX p_r_res.desc_operacion
      PRINTX p_r_res.originac_desc
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_r_res.tot_sin_origen USING "#########&"

END REPORT

#Objetivo: Busca la descripción del estatus de rechazo en catalogo
FUNCTION f_busca_desc_rch(p_si_estado)
   DEFINE p_si_estado     LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_c_desc_estado LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

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

#Objetivo: Busca la descripción del tipo de registro
FUNCTION f_busca_desc_registro(p_tpo_registro)
   DEFINE p_tpo_registro  LIKE cat_registro_interno.tpo_registro, -- tipo de registro
          v_c_desc_reg    LIKE cat_registro_interno.desc_registro, -- descripción del registro
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

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
   DEFINE p_c_operacion  LIKE cat_operacion_prc.operacion, -- operación
          v_c_desc_opera LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

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
   DEFINE p_si_tpo_originac LIKE cat_cre_originacion.tpo_originacion, -- tipo de originación
          p_c_originac_desc LIKE cat_cre_originacion.originacion_desc, -- descripción de originación
          v_s_qryTxt        STRING -- se asigna consulta sql a ejecutar

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
