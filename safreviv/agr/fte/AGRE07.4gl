--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRE07                                        #
#Objetivo          =>Programa para integrar el archivo Uso         #
#                    Anualidad que ha sido validado, para el       #
#                    módulo de Anualidades Garantizadas            #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>24 Mayo 2011                                  #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE g_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
       g_d_pid              LIKE bat_ctr_proceso.pid, -- pid
       g_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
       g_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
       g_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
       g_id_cre_ctr_archivo LIKE cre_acreditado.id_cre_ctr_archivo, -- id del archivo
       g_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       g_c_ruta_listados    LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

#Objetivo: Función que realiza la integracion del archivo Uso Anualidad
MAIN
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_dt_f_lote         LIKE cre_ctr_archivo.f_lote, -- fecha del lote
          v_i_operacion       LIKE cre_ctr_archivo.operacion, -- operacion del proceso
          v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          v_c_programa_cod    LIKE cat_operacion.programa_cod, -- programa de la operación
          v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
             tot_sin_origen   LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
          END RECORD,
          v_si_origen         SMALLINT, -- variable (para la función de mandatos)
          v_si_cuenta_valor   SMALLINT, -- variable que indica si existe el preccio de acción par el día de hoy
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_s_mens_correo     STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo   STRING, -- contiene el titulo del correo
          v_s_archivo_correo  STRING, -- ruta y nombre del archivo adjunto en el correo 
          v_dt_fec_carga      DATE, -- fecha de carga (para la función de mandatos)
          v_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          v_si_lote           INTEGER, -- lote
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".AGRE07.log")

   DISPLAY "=INICIA AGRE07="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " ARCHIVO:      : ",g_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 43
   LET v_si_id_proceso = g_proc_cod_agr_uso_anualid -- Anualidades Garantizadas
   LET v_si_origen = 1
   LET v_dt_fec_carga = TODAY

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- se genera el folio
   LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se consulta la fecha de lote
   LET v_s_qryTxt = " SELECT FIRST 1 f_envio\n",
                    "   FROM safre_tmp:tmp_uso_det_agr\n",
                    "  WHERE f_envio IS NOT NULL"

   PREPARE prp_fec_proceso FROM v_s_qryTxt
   EXECUTE prp_fec_proceso INTO v_dt_f_lote

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

   DISPLAY ""
   DISPLAY " EJECUTA => INTEGRA USO ANUALIDAD"
   -- se crea la sentencia que ejecuta la función que integra el archivo de uso de anualidad
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_integra_uso_anualidad(?,?,?)"

   PREPARE prp_integra_usoGarantia FROM v_s_qryTxt
   EXECUTE prp_integra_usoGarantia USING g_id_cre_ctr_archivo, g_d_folio,g_v_usuario
                                   INTO r_b_valida

   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: OCURRIÓ UN ERROR DURANTE EL PROCESO DE INTEGRACIÓN: ",r_b_valida

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se verifica que exista precio de la acción para el dia de hoy
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_slct_precioAcc FROM v_s_qryTxt
   EXECUTE prp_slct_precioAcc INTO v_si_cuenta_valor

   IF v_si_cuenta_valor = 0 THEN
      DISPLAY " ERROR: NO EXISTE EL PRECIO DE ACCIÓN PARA EL DÍA DE HOY,",
              "        POR LO TANTO NO ES POSIBLE GENERAR DEUDOR"

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " EJECUTA => INTEGRA USO ANUALIDAD (DEUDOR)"
   -- se crea sentencia que ejecuta la función que inserta en la tabla his y deudor
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_integra_uso_anualidad_deudor(?,?)"

   PREPARE prp_inserta_hist_deud FROM v_s_qryTxt
   EXECUTE prp_inserta_hist_deud USING g_d_folio, g_id_cre_ctr_archivo
                                   INTO r_b_valida

   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: OCURRIÓ UN ERROR DURANTE EL PROCESO DE INTEGRACIÓN DEUDOR: ",r_b_valida

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
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

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
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
   
   DISPLAY ""
   DISPLAY " TOTAL DE REGISTROS: ", v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS: ",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS; ",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " TOTAL SIN ORIGEN: ",v_r_cre_ctr_archivo.tot_sin_origen
   DISPLAY ""
   
{  DISPLAY " GENERA REPORTE"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod , g_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED,"-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegRecurr(v_v_nom_reporte)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN DE USO DE ANUALIDAD"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                          "Proceso      : RECEPCIÓN DE USO ANUALIDAD\n",
                          "Operacion    : INTEGRA ARCHIVO USO ANUALIDAD\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_d_pid,
                          g_i_proceso_cod,
                          g_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)
}
   -- se cambia el codigo de operación ya que se ejecutará la operación de marcaje
   LET g_i_opera_cod = 3

   DISPLAY " EJECUTA PROCESO DE MARCAJE"
   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " nohup time fglrun ",g_c_ruta_bin CLIPPED,"/AGRP13 ",
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
   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que genera el reporte de Integración Uso Anualidad
FUNCTION f_genera_rpt_IntegRecurr(p_v_nom_reporte)
   DEFINE p_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_r_rpt_res       RECORD -- registro de resumen
             nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion   LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             usuario        LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros  INTEGER, -- numero total de registros
             tot_aceptados  INTEGER, -- numero total de regs aceptados
             tot_rechazados INTEGER, -- numero total de regs rechazados
             tot_sin_origen INTEGER  -- numero total de regs sin origen
          END RECORD,
          v_r_cre_ctr_arch  RECORD LIKE cre_ctr_archivo.*, -- registro de cre ctr archivo
          v_r_bat_ctr_opera RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_i_folio_format  INTEGER, -- numero de folio con formato
          v_manejador_rpt   OM.SaxDocumentHandler, -- Contenedor de Documentos para el reporte
          v_s_qryTxt        STRING -- contiene una sentencia sql a ejecutar

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRE071.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(g_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"

      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod,"\n",
                    "    AND folio = ",g_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.id_operacion = v_r_cre_ctr_arch.operacion
   LET v_r_rpt_res.desc_operacion = fn_obt_desc_operacion(v_r_rpt_res.id_operacion)
   LET v_r_rpt_res.usuario = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros = v_r_cre_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados = v_r_cre_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados
   LET v_r_rpt_res.tot_sin_origen = v_r_cre_ctr_arch.tot_sin_origen

   -- se le da formato al folio
   LET v_i_folio_format = g_d_folio --USING "&&&&&&&&&&"

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_recurr TO XML HANDLER v_manejador_rpt

   OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_i_folio_format)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_recurr

END FUNCTION

#Objetivo: Genera el reporte de Integración de Uso Anualidad
REPORT reporte_integ_recurr(p_r_res, p_i_folio)
   DEFINE p_i_folio         VARCHAR(10), -- numero de folio con formato
          p_r_res           RECORD
             nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion   LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
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
      PRINTX p_r_res.fecha_hr_ini USING "DD-MM-YYYY"
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.id_operacion
      PRINTX p_r_res.desc_operacion
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_r_res.tot_sin_origen USING "#########&"
END REPORT

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
