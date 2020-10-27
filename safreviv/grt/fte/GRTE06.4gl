--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTE06                                        #
#Objetivo          =>Programa para integrar el archivo de desmarca #
#                    que ha sido validado                          #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>20 Enero 2012                                 #
####################################################################

DATABASE safre_viv

DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
       p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
       p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
       p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
       p_i_tpo_originacion LIKE cre_acreditado.tpo_originacion, -- tipo de originación
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
          v_c_programa_cod    LIKE cat_operacion.programa_cod, -- programa de la operación
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          r_c_ruta_bin_cta    LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
          r_c_ruta_list_cta   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)
   LET p_i_tpo_originacion = ARG_VAL(8)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE06.log")

   DISPLAY "=INICIA GRTE06="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_desmarca_grt()

   DISPLAY " INTEGRA DESMARCA "
   -- se crea la sentencia que ejecuta el procedimiento que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_grt_integra_desmarca(?,?,?,?)"

   PREPARE prp_integra_desmarca FROM v_s_qryTxt
   EXECUTE prp_integra_desmarca USING p_v_usuario, p_v_arch_proceso, p_d_folio, p_d_id_cre_ctr_arch
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

   DISPLAY " PROCESA DESMARCA"
   -- se crea la sentencia que ejecuta el procedimiento que ejecuta:
   -- * desmarca cuenta
   -- * inserta registro en cta marca ws
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_procesa_desmarca(?,?,?,?)"

   PREPARE prp_procesa_desmarca FROM v_s_qryTxt
   EXECUTE prp_procesa_desmarca USING p_v_usuario,
                                      p_d_folio,
                                      p_i_tpo_originacion,
                                      p_i_proceso_cod
                                 INTO v_ax_error

   IF v_ax_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE DESMARCA: ",v_ax_error

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
                    "  WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOT_REGISTROS TOT_ACEPTADOS TOT_RECHAZADOS TOT_SIN_ORIGEN"
   DISPLAY v_r_cre_ctr_archivo.*

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

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
                                           p_i_proceso_cod," 1>> ",
                                           v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando
}
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

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(p_i_proceso_cod, p_i_opera_cod)

   DISPLAY " GENERA REPORTE"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegDesmarca(r_c_ruta_listados, v_v_nom_reporte)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN DESMARCA SOLICITUD DE SALDOS 43 BIS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : RECEPCIÓN DESMARCA SOLIC. SDOS. 43 BIS\n",
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

#Objetivo: Función que crea la tabla temporal de la integración de desmarca AGR
FUNCTION fn_crea_tmp_desmarca_grt()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_nss_desmarcados_grt

   WHENEVER ERROR STOP

   CREATE TABLE tmp_nss_desmarcados_grt(nss CHAR(11),
                                        tpo_credito SMALLINT)

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegDesmarca(p_c_ruta_listados, p_v_nom_reporte)
   DEFINE p_c_ruta_listados LIKE seg_modulo.ruta_listados, -- ruta listados cta
          p_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_r_rpt_res       RECORD -- registro de resumen
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
          v_r_cre_ctr_arch  RECORD LIKE cre_ctr_archivo.*, -- registro de cre ctr archivo
          v_r_bat_ctr_opera RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt        STRING, -- contiene una sentencia sql a ejecutar
          r_b_valida        SMALLINT

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTE061.4rp") THEN
      -- se indica la salida del reporte
      --LET p_v_nom_reporte = p_v_usuario CLIPPED, "-GRTL12-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
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
                    " FROM cre_ctr_archivo\n",
                    " WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod,"\n",
                    "    AND folio = ",p_d_folio

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

#Objetivo: Función que genera el reporte de tipo de crédito
FUNCTION fn_genera_rep_proc(p_i_proceso_cod, p_i_opera_cod)
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
   LET v_s_qryTxt = " SELECT UNIQUE nss, tpo_credito\n",
                    "   FROM safre_tmp:tmp_nss_desmarcados_grt"

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
