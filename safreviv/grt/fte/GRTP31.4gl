--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Módulo            => GRT                                           #
#Programa          => GRTP31                                        #
#Objetivo          => Programa para integrar los registros de       #
#                     solicitud de uso de garantía de SAC43BIS      #
#Autor             => Mauro Muñiz Caballero, EFP                    #
#Fecha inicio      => 9 de enero de 2017                            #
#Modifica          => Edgar Damian Estrada Rivera                   #
#Fecha Mod.        => 28 de Agosto de 2017                          #
#####################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl" 

   DEFINE g_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario         (GRTP01)
   DEFINE g_d_pid                   LIKE bat_ctr_proceso.pid -- pid                        (GRTP01)
   DEFINE g_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso     (GRTP01)
   DEFINE g_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operacion (GRTP01)
   DEFINE g_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio          (GRTP01)
   DEFINE g_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar          (GRTP01)
   DEFINE g_id_cre_ctr_archivo      LIKE cre_acreditado.id_cre_ctr_archivo -- id del archivo
   DEFINE g_c_ruta_bin              LIKE seg_modulo.ruta_bin  -- ruta del bin del módulo
   DEFINE g_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

#Objetivo: Función que realiza la integración del archivo Uso Garantía
MAIN

   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operación del proceso
   DEFINE v_execute_fn              STRING  -- variable para iniciar la ejecucion de fn_verifica_id_archivo_grt
   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0) --valor de inserción
   DEFINE v_folio_archivo           DECIMAL(9,0) -- valor de inserción

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados,-- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_si_origen               SMALLINT -- variable (para la función de mandatos)
   DEFINE v_si_cuenta_valor         SMALLINT -- variable que indica si existe el preccio de acción par el día de hoy
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_mens_correo           STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo         STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo        STRING -- ruta y nombre del archivo adjunto en el correo 
   DEFINE v_dt_fec_carga            DATE -- fecha de carga (para la función de mandatos)
   DEFINE v_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_subproc                 SMALLINT

   -- se recuperan los parámetros que envía el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".GRTP31.log")

   DISPLAY "=INICIA GRTP31="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " PROCESO       : ",g_i_proceso_cod
   DISPLAY " ARCHIVO:      : ",g_v_arch_proceso
  
   
   -- se inicializan variables
   LET v_i_operacion   = 18
   LET v_si_origen     = 1
   LET v_dt_fec_carga  = TODAY
   LET v_si_id_proceso = g_id_proceso_grt_uso -- Uso de Garantía 43 bis
   LET v_subproc       = 3

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING g_c_ruta_bin,
                                  g_c_ruta_listados

    -- recuperará registro de control de archivos. 
     
    LET v_execute_fn = "EXECUTE FUNCTION fn_verifica_id_archivo_grt(?)"

    PREPARE prp_fn_verifica_archivo   FROM v_execute_fn
    EXECUTE prp_fn_verifica_archivo   USING g_i_proceso_cod
                                       INTO v_id_cre_ctr_archivo,
                                            v_folio_archivo
                                            
   -- se verifica que exista precio de la acción para el día de hoy
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_slct_precioAcc FROM v_s_qryTxt
   EXECUTE prp_slct_precioAcc INTO v_si_cuenta_valor

   IF v_si_cuenta_valor = 0 THEN
      DISPLAY " ERROR: No existe precio de acción para el día de hoy, por lo que no es\n",
              "        posible generar movimientos de cargo por solicitud de uso de garantía"

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid,
                                      g_i_proceso_cod, 
                                      g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    " WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   LET v_s_qryTxt = "fn_verifica_id_archivo_ocg(?)"

   PREPARE prp_ctr_archivo FROM v_s_qryTxt
   EXECUTE prp_ctr_archivo USING v_subproc
                           INTO g_id_cre_ctr_archivo, 
                                g_d_folio

   DISPLAY "FOLIO ARCHIVO:   ",g_d_folio USING "#########&"

   DISPLAY "OBTENCIÓN DE REGISTROS CON SOLICITUD DE USO DE GARANTÍA"

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_obtiene_solic_ug(?,?,?)"

   PREPARE prp_obtiene_usoGarantia FROM v_s_qryTxt
   EXECUTE prp_obtiene_usoGarantia USING g_v_usuario,
                                         v_id_cre_ctr_archivo,
                                         v_folio_archivo
                                   INTO r_b_valida

   DISPLAY " INTEGRA USO GARANTÍA"
   -- se crea la sentencia que ejecuta la función que integra el archivo de uso de garantía
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_integra_uso_garantia(?,?,?)"

   PREPARE prp_integra_usoGarantia FROM v_s_qryTxt
   EXECUTE prp_integra_usoGarantia USING g_id_cre_ctr_archivo, 
                                         g_d_folio, 
                                         g_v_usuario
                                   INTO r_b_valida

   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: OCURRIÓ UN ERROR DURANTE EL PROCESO DE INTEGRACIÓN: ",r_b_valida

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, 
                                      g_i_proceso_cod, 
                                      g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA USO GARANTÍA (DEUDOR)"
   -- se crea sentencia que ejecuta la función que inserta en la tabla his y deudor
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_integra_uso_garantia_deudor(?,?)"

   PREPARE prp_inserta_hist_deud FROM v_s_qryTxt
   EXECUTE prp_inserta_hist_deud USING g_d_folio, 
                                       g_id_cre_ctr_archivo
                                   INTO r_b_valida

   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: OCURRIÓ UN ERROR DURANTE EL PROCESO DE INTEGRACIÓN DEUDOR: ",r_b_valida

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, 
                                      g_i_proceso_cod, 
                                      g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(g_d_pid, 
                                           g_i_proceso_cod, 
                                           g_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se actualiza la operación como erronea
      LET r_b_valida = fn_error_opera(g_d_pid, 
                                      g_i_proceso_cod, 
                                      g_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,   ",
                    "         tot_aceptados,  ",
                    "         tot_rechazados, ",
                    "         tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOT_REGISTROS TOT_ACEPTADOS TOT_RECHAZADOS TOT_SIN_ORIGEN"
   DISPLAY v_r_cre_ctr_archivo.*

   DISPLAY " GENERA REPORTE"
   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",g_d_folio,"\n",
                    "  WHERE pid = ",g_d_pid,"\n",
                    "    AND proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod,
                                                g_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_v_usuario CLIPPED, 
                         "-",v_c_programa_cod CLIPPED,
                         "-", g_d_pid USING "&&&&&", 
                         "-", g_i_proceso_cod USING "&&&&&", 
                         "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegRecurr(v_v_nom_reporte)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN DE USO DE GARANTÍA"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                          "Proceso      : RECEPCIÓN DE USO GARANTÍA\n",
                          "Operacion    : INTEGRA ARCHIVO USO GARANTÍA\n",
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

   DISPLAY " EJECUTA PROCESO DE MARCAJE"
   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " nohup time fglrun ",g_c_ruta_bin CLIPPED,"/GRTP09 ",
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

#Objetivo: Función que genera el reporte de Integración Uso Garantía
FUNCTION f_genera_rpt_IntegRecurr(p_v_nom_reporte)

   DEFINE p_v_nom_reporte           VARCHAR(80) -- nombre del reporte

   DEFINE v_r_rpt_res RECORD -- registro de resumen
      nom_archivo                   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini                  LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin                  LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      id_operacion                  LIKE cre_ctr_archivo.operacion, -- operación
      desc_operacion                LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
      usuario                       LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros                 INTEGER, -- número total de registros
      tot_aceptados                 INTEGER, -- número total de regs aceptados
      tot_rechazados                INTEGER, -- número total de regs rechazados
      tot_sin_origen                INTEGER  -- número total de regs sin origen
   END RECORD

   DEFINE v_r_cre_ctr_arch          RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera         RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_i_folio_format          INTEGER -- número de folio con formato
   DEFINE v_manejador_rpt           OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar

   -- se indica que el reporte usará la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTE071.4rp") THEN
      -- se indica la salida del reporte
      --LET p_v_nom_reporte = g_v_usuario CLIPPED, "-GRTL16-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"
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
                    " FROM cre_ctr_archivo\n",
                    " WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

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

   OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, 
                                         v_i_folio_format)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_recurr

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Uso Garantía
REPORT reporte_integ_recurr(p_r_res,
                            p_i_folio)

   DEFINE p_i_folio                 VARCHAR(10) -- numero de folio con formato

   DEFINE p_r_res RECORD
      nom_archivo                   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini                  LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin                  LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      id_operacion                  LIKE cre_ctr_archivo.operacion, -- operacion
      desc_operacion                LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
      usuario                       LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros                 INTEGER, -- número total de registros
      tot_aceptados                 INTEGER, -- número total de regs aceptados
      tot_rechazados                INTEGER, -- número total de regs rechazados
      tot_sin_origen                INTEGER  -- número total de regs sin origen
   END RECORD

   DEFINE v_fecha_reporte           DATE

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
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_r_res.tot_sin_origen USING "#########&"

END REPORT

#Objetivo: Busca la descripción del estatus de rechazo en catálogo
FUNCTION f_busca_desc_rch(p_si_estado)

   DEFINE p_si_estado               LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_c_desc_estado           LIKE cat_rch_acreditado.desc_estado -- descripción del estado
   DEFINE v_s_qryTxt                STRING -- se asigna consulta sql a ejecutar

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

   DEFINE p_tpo_registro            LIKE cat_registro_interno.tpo_registro -- tipo de registro
   DEFINE v_c_desc_reg              LIKE cat_registro_interno.desc_registro -- descripción del registro
   DEFINE v_s_qryTxt                STRING -- se asigna consulta sql a ejecutar

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