--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRE15                                        #
#Objetivo          =>Programa para integrar el archivo de desmarca #
#                    que ha sido validado                          #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>20 Enero 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
       p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
       p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
       p_v_arch_proceso    VARCHAR(100) -- nombre del archivo a integrar

#Objetivo: Funcion que realiza la integracion
MAIN
   DEFINE v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
             tot_sin_origen   LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
          END RECORD,
          v_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          v_ax_error          SMALLINT, --indica si existio error en la funcion
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_s_mens_correo     STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo   STRING, -- contiene el titulo del correo
          v_s_archivo_correo  STRING, -- ruta y nombre del archivo adjunto en el correo 
          v_c_programa_cod    LIKE cat_operacion.programa_cod, -- nombrel del programa origen
          v_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_i_tpo_originacion LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_dt_f_lote         LIKE cre_ctr_archivo.f_lote, -- fecha de lote 
          v_i_operacion       LIKE cre_ctr_archivo.operacion, -- operacion
          v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          v_i_cuenta_regs     INTEGER, -- numero de registros
          r_c_ruta_bin_cta    LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
          r_c_ruta_list_cta   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRE15.log")

   DISPLAY "=INICIA ACRE15="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 30 -- Solicitud desmarca ACR
   LET v_si_id_proceso = g_id_proceso_acr -- Transferencia de Acreditados
   LET v_i_tpo_originacion = 1 -- Transferencia de Acreditados

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_desmarca_acr()

   -- se realiza la consulta que verifica si existen registros a desmarcar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM safre_tmp:tmp_acr_desmarca\n"

   PREPARE prp_slct_cnt_11 FROM v_s_qryTxt
   EXECUTE prp_slct_cnt_11 INTO v_i_cuenta_regs

   -- se verifica si hay información para originar
   IF v_i_cuenta_regs > 0 THEN
      -- se consulta la fecha de lote
      LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                       "   FROM safre_tmp:tmp_acr_desmarca\n",
                       "  WHERE fec_proceso IS NOT NULL"

      PREPARE prp_fec_proceso FROM v_s_qryTxt
      EXECUTE prp_fec_proceso INTO v_dt_f_lote

      -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
      LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE f_lote = '",v_dt_f_lote,"'\n",
                       "    AND id_proceso = ",v_si_id_proceso,"\n",
                       "    AND operacion = ",v_i_operacion,"\n",
                       "    AND estado = 10\n",
                       "  ORDER BY id_cre_ctr_archivo DESC"

      PREPARE prp_id_creCtrArch_fec FROM v_s_qryTxt
      EXECUTE prp_id_creCtrArch_fec INTO v_d_id_cre_ctr_arch
   ELSE
      DISPLAY " No existe información de Solicitud de Desmarca"
      -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
      LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                       "    AND operacion = ",v_i_operacion,"\n",
                       "    AND estado = 10\n",
                       "  ORDER BY id_cre_ctr_archivo DESC"

      PREPARE prp_id_creCtrArch FROM v_s_qryTxt
      EXECUTE prp_id_creCtrArch INTO v_d_id_cre_ctr_arch
   END IF

   -- se verifica si fue posible obtener el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF
   DISPLAY " ID ARCHIVO    : ",v_d_id_cre_ctr_arch

   DISPLAY " INTEGRA DESMARCA "
   -- se crea la sentencia que ejecuta el procedimiento que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_integra_desmarca(?,?,?,?)"

   PREPARE prp_integra_desmarca FROM v_s_qryTxt
   EXECUTE prp_integra_desmarca USING p_v_usuario, p_v_arch_proceso, p_d_folio, v_d_id_cre_ctr_arch
                                 INTO r_b_valida

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE DESMARCA: ",r_b_valida

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " PROCESA DESMARCA TA"
   -- se crea la sentencia que ejecuta el procedimiento que realiza:
   -- * la desmarca cuenta
   -- * la inserción de registros en cta marca ws
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_procesa_desmarca(?,?,?,?)"

   PREPARE prp_procesa_desmarca FROM v_s_qryTxt
   EXECUTE prp_procesa_desmarca USING p_v_usuario,
                                      p_d_folio,
                                      v_i_tpo_originacion,
                                      p_i_proceso_cod
                                 INTO v_ax_error

   IF v_ax_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE DESMARCA TA: ",v_ax_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

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
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " Total registros : ",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " Total aceptados : ",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " Total rechazados: ",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " Total sin origen: ",v_r_cre_ctr_archivo.tot_sin_origen

{  -- Esta parte ya no se ejecuta (2012/10/15) ya que se creo un programa de carga de marca/desmarca
   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("cta") RETURNING r_c_ruta_bin_cta, r_c_ruta_list_cta

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    " WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin_cta CLIPPED,"/CTAW13 ",
                                           p_i_proceso_cod, " 1>> ",
                                           v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando
}
   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_gen_arch_salida(p_i_proceso_cod, p_i_opera_cod)

   -- cuenta el número de registro a originar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM safre_tmp:tmp_acr_desmarca_01"

   PREPARE prp_cnt_tmp_desm_01 FROM v_s_qryTxt
   EXECUTE prp_cnt_tmp_desm_01 INTO v_i_cuenta_regs

   -- se verifica si hay información para originar
   IF v_i_cuenta_regs > 0 THEN
      DISPLAY " Existe información de recurrente originación"

      CALL fn_lanzador_recurrente(v_i_cuenta_regs, r_c_ruta_bin, v_c_programa_cod) 
   ELSE
      DISPLAY " No existe información de recurrente originación"
   END IF

   -- cuenta el número de registro a reactivar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM safre_tmp:tmp_acr_desmarca_04"

   PREPARE prp_cnt_tmp_desm_04 FROM v_s_qryTxt
   EXECUTE prp_cnt_tmp_desm_04 INTO v_i_cuenta_regs

   -- se verifica si hay información para originar
   IF v_i_cuenta_regs > 0 THEN
      DISPLAY " Existe información para reactivación"

      CALL fn_lanzador_reactivacion(v_d_id_cre_ctr_arch) 
   ELSE
      DISPLAY " No existe información para reactivación"
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   -- se actualiza el folio en el registro correspondiente a las tablas de control del monitor del proceso
   UPDATE bat_ctr_operacion
      SET folio = p_d_folio
    WHERE pid = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod = p_i_opera_cod

   DISPLAY " GENERA REPORTE PDF"
   -- recupera la ruta bin y de listados para el módulo en proceso
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED, "-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegDesmarca(r_c_ruta_listados, v_v_nom_reporte, v_d_id_cre_ctr_arch)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN DESMARCA ACREDITADOS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : RECEPCIÓN DESMARCA ACREDITADOS\n",
                          "Operacion    : INTEGRA ARCHIVO DESMARCA\n",
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

#Objetivo: Función que crea la tabla temporal de la integración de desmarca ACR
FUNCTION fn_crea_tmp_desmarca_acr()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_nss_desmarcados_acr

   WHENEVER ERROR STOP

   CREATE TABLE tmp_nss_desmarcados_acr(nss CHAR(11),
                                        tpo_credito SMALLINT)

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION

#Objetivo: Función que genera el reporte de Integración de Desmarca
FUNCTION f_genera_rpt_IntegDesmarca(p_c_ruta_listados, p_v_nom_reporte, p_d_id_cre_ctr_arch)
   DEFINE p_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          p_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_r_rpt_res         RECORD -- registro de resumen
             folio            INTEGER, -- numero de folio con formato
             nom_archivo      LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini     LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin     LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion     LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion   LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             usuario          LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros    INTEGER, -- numero total de registros
             tot_aceptados    INTEGER, -- numero total de regs aceptados
             tot_rechazados   INTEGER, -- numero total de regs rechazados
             tot_sin_origen   INTEGER  -- numero total de regs sin origen
          END RECORD,
          v_r_cre_ctr_arch    RECORD LIKE cre_ctr_archivo.*, -- registro de cre ctr archivo
          v_r_bat_ctr_opera   RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_manejador_rpt     OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt          STRING, -- contiene una sentencia sql a ejecutar
          r_b_valida          SMALLINT

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("ACRE151.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(p_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

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
   LET v_r_rpt_res.folio = p_d_folio

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_desmarca TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_integ_desmarca(v_r_rpt_res.*)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_desmarca

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_desmarca(p_r_res)
   DEFINE p_r_res           RECORD
             folio          INTEGER, -- numero de folio con formato
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
          v_fecha_reporte   DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_v_usuario
      PRINTX p_r_res.folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
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
          v_s_qryTxt     STRING -- se asigna consulta sql a ejecutar

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

#Objetivo: Función que actua como lanzador del proceso de Validación de Recurrente
FUNCTION fn_lanzador_recurrente(p_i_cuenta_regs, p_c_ruta_bin, p_c_programa_cod)
   DEFINE p_i_cuenta_regs   INTEGER, -- numero de registros
          v_d_pid           DECIMAL(9,0), -- identificador del proceso
          v_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_i_operacion     LIKE cre_ctr_archivo.operacion, -- operacion
          v_d_folio         LIKE bat_ctr_proceso.folio, -- folio del proceso
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          p_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_r_cre_ctr_arch  RECORD LIKE cre_ctr_archivo.*, -- registro de la tabla de control
          v_r_glo_ctr_arch  RECORD LIKE glo_ctr_archivo.*, -- registro de la tabla de control global
          v_si_lote         LIKE cre_ctr_archivo.lote, -- lote del archivo
          v_dt_f_lote       LIKE cre_ctr_archivo.f_lote, -- fecha de lote del archivo
          v_c_extension     LIKE cat_operacion.extension, -- extensión del archivo
          v_s_comando       STRING, -- contiene al comando a correr
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          p_c_ruta_bin      LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inializan las variables
   LET v_i_proceso_cod = g_proc_cod_acr_recurrente -- recepción recurrente acreditados
   LET v_i_opera_cod = 1 -- valida archivo recurrente
   LET v_i_operacion = 21 -- operacion del proceso
   LET v_d_pid = 0
   LET v_d_folio = 0

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

   -- se concatena la extension al nombre del archivo
   LET p_v_arch_proceso = p_v_arch_proceso || "." || v_c_extension CLIPPED

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat
   
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM      
   END IF 

   -- se invoca la funcion que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                          v_i_proceso_cod,
                                          v_i_opera_cod,
                                          v_d_folio,
                                          p_c_programa_cod,
                                          p_v_arch_proceso,
                                          p_v_usuario)

   -- en caso de error se muestra un mensaje a usuario
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM  
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                           v_i_proceso_cod,
                                           v_i_opera_cod,
                                           v_d_folio,
                                           p_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_v_usuario)

   -- en caso de error se muestra un mensaje a usuario
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM  
   END IF

   -- se muestra mensaje a usuario
   DISPLAY " Se ha enviado (automaticamente) la Validación con PID: ",v_d_pid CLIPPED,"\n",
           " Puede revisar el avance del proceso en el monitor de ejecución de procesos"

   -- se insertan los registros en las tablas temporales
   DELETE FROM safre_tmp:tmp_acr_transferencia WHERE 1=1
   DELETE FROM safre_tmp:tmp_acr_transf_02 WHERE 1=1

   -- se inserta la información de la tabla de Desmarca a la tabla de Recurrente
   INSERT INTO safre_tmp:tmp_acr_transferencia SELECT * FROM safre_tmp:tmp_acr_desmarca_01

   -- se consulta la fecha de lote
   LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                    "   FROM safre_tmp:tmp_acr_transferencia\n",
                    "  WHERE fec_proceso IS NOT NULL"

   PREPARE prp_slct_frst_fproc FROM v_s_qryTxt
   EXECUTE prp_slct_frst_fproc INTO v_dt_f_lote

   -- se valida la fecha de lote
   IF v_dt_f_lote IS NULL THEN
      LET v_dt_f_lote = TODAY
   END IF

   -- se busca numero de lote correspondiente al archivo
   SELECT MAX(lote)
     INTO v_si_lote
     FROM safre_viv:cre_ctr_archivo
    WHERE f_lote = v_dt_f_lote
      AND operacion = v_i_operacion

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_si_lote IS NULL THEN
      LET v_si_lote = 1
   ELSE
      LET v_si_lote = v_si_lote + 1
   END IF

   -- se inserta el registro de control
   LET v_r_cre_ctr_arch.folio_archivo = v_d_folio
   LET v_r_cre_ctr_arch.lote = v_si_lote
   LET v_r_cre_ctr_arch.f_lote = v_dt_f_lote
   LET v_r_cre_ctr_arch.id_proceso = g_id_proceso_acr -- Transferencia de Acreditados
   LET v_r_cre_ctr_arch.operacion = v_i_operacion -- Recurrente
   LET v_r_cre_ctr_arch.nom_archivo = p_v_arch_proceso
   LET v_r_cre_ctr_arch.tot_registros = p_i_cuenta_regs
   LET v_r_cre_ctr_arch.tot_aceptados = 0
   LET v_r_cre_ctr_arch.tot_rechazados = 0
   LET v_r_cre_ctr_arch.tot_sin_origen = 0
   LET v_r_cre_ctr_arch.estado = 10
   LET v_r_cre_ctr_arch.f_proceso = TODAY
   LET v_r_cre_ctr_arch.usuario = p_v_usuario

   -- se inserta el registro en la tabla de control
   INSERT INTO cre_ctr_archivo VALUES (seq_cre_archivo.NEXTVAL,
                                       v_r_cre_ctr_arch.folio_archivo,
                                       v_r_cre_ctr_arch.lote,
                                       v_r_cre_ctr_arch.f_lote,
                                       v_r_cre_ctr_arch.id_proceso,
                                       v_r_cre_ctr_arch.operacion,
                                       v_r_cre_ctr_arch.nom_archivo,
                                       v_r_cre_ctr_arch.tot_registros,
                                       v_r_cre_ctr_arch.tot_aceptados,
                                       v_r_cre_ctr_arch.tot_rechazados,
                                       v_r_cre_ctr_arch.tot_sin_origen,
                                       v_r_cre_ctr_arch.estado,
                                       v_r_cre_ctr_arch.f_proceso,
                                       v_r_cre_ctr_arch.usuario)

   -- se inserta el registro de control (global)
   LET v_r_glo_ctr_arch.proceso_cod = v_i_proceso_cod
   LET v_r_glo_ctr_arch.opera_cod = v_i_opera_cod
   LET v_r_glo_ctr_arch.nombre_archivo = p_v_arch_proceso
   LET v_r_glo_ctr_arch.folio = NULL
   LET v_r_glo_ctr_arch.estado = 1
   LET v_r_glo_ctr_arch.f_actualiza = TODAY
   LET v_r_glo_ctr_arch.usuario = p_v_usuario

   -- se inserta el registro en la tabla de control
   INSERT INTO glo_ctr_archivo VALUES (v_r_glo_ctr_arch.*)

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(v_d_pid,
                                           v_i_proceso_cod,
                                           v_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se muestra mensaje a usuario
   DISPLAY " Finalizó la Validación satisfactoriamente"

   -- se asignan los valores necesarios para la intergración
   LET v_i_opera_cod = 2 -- integra archivo recurrente

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                           v_i_proceso_cod,
                                           v_i_opera_cod,
                                           v_d_folio,
                                           p_c_programa_cod,
                                           p_v_arch_proceso,
                                           p_v_usuario)

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
   LET v_s_comando = " nohup time fglrun ",p_c_ruta_bin CLIPPED,"/ACRE10 ",
                                           p_v_usuario, " ",
                                           v_d_pid, " ",
                                           v_i_proceso_cod, " ",
                                           v_i_opera_cod, " ",
                                           v_d_folio, " ",
                                           p_v_arch_proceso, " ", " 1> ",
                                           v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",v_d_pid USING "&&&&&",":",
                                           v_i_proceso_cod USING "&&&&&",":",
                                           v_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando

   -- se muestra mensaje a usuario
   DISPLAY " Se ha enviado (automaticamente) la Integración con PID: ",v_d_pid CLIPPED,"\n",
           " Puede revisar el avance del proceso en el monitor de ejecución de procesos"

END FUNCTION

#Objetivo: Función que genera el reporte de tipo de crédito
FUNCTION fn_gen_arch_salida(p_i_proceso_cod, p_i_opera_cod)
   DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
          v_v_arch_salida      VARCHAR(100), -- nombre del archivo de salida5
          v_v_ruta_archivo     VARCHAR(150), -- ruta y nombre del archivo de salida
          v_c_fec_hoy          CHAR(8), -- fecha con formato "yyyymmdd"
          v_c_extension        LIKE cat_operacion.extension, -- extensión del archivo
          v_ch_arch_reporte    BASE.CHANNEL, -- manejador de apuntador hacia archivo5
          v_r_rpt_tpo_credito  RECORD
             nss               CHAR(11),
             tpo_credito       SMALLINT
          END RECORD,
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_envio       LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_arch_salida = "Acr" || v_c_fec_hoy || "." || v_c_extension
   DISPLAY " ARCHIVO SALIDA (TIPO CRÉDITO): ",v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

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
   LET v_s_qryTxt = " SELECT UNIQUE nss, tpo_credito\n",
                    "   FROM safre_tmp:tmp_nss_desmarcados_acr"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_r_rpt_tpo_credito.*
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_rpt_tpo_credito.nss,
                         v_r_rpt_tpo_credito.tpo_credito USING "&&&"

      -- se escribe el registro (montos iguales) en el archivo
      CALL v_ch_arch_reporte.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte.close()
END FUNCTION

#Objetivo: Función que procesa los registros para reactivación
FUNCTION fn_lanzador_reactivacion(p_d_id_cre_ctr_arch)
   DEFINE p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          r_si_cod_err        SMALLINT, --indica si existio error en la funcion
          r_i_isam_err        INTEGER,
          r_v_msj_err         VARCHAR(250),
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DISPLAY " PROCESA REACTIVACIÓN TA"
   -- se crea la sentencia que ejecuta el procedimiento de reactivación de créditos
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_acr_integra_desm_reactiva(?,?,?,?)"

   PREPARE prp_procesa_reactivacion FROM v_s_qryTxt
   EXECUTE prp_procesa_reactivacion USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO r_si_cod_err,
                                          r_i_isam_err,
                                          r_v_msj_err

   -- se valida el retorno de la función
   IF r_si_cod_err <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE REACTIVACIÓN: ",r_si_cod_err
      DISPLAY "ISAM ERR   : ",r_i_isam_err
      DISPLAY "MENSAJE ERR: ",r_v_msj_err

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF
END FUNCTION
