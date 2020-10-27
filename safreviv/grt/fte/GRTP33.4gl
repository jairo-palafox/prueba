--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Módulo            => GRT                                           #
#Programa          => GRTP33                                        #
#Objetivo          => Programa para integrar los registros de       #
#                     devolución de saldos excedentes 43BIS         #
#Autor             => Edgar Damián Estrada Rivera, EFP              #
#Fecha inicio      => 31 de Octubre de 2017                         #
#####################################################################

DATABASE safre_viv

   DEFINE g_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE g_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE g_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE g_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operacion
   DEFINE g_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE g_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE g_id_cre_ctr_archivo      LIKE cre_acreditado.id_cre_ctr_archivo -- id del archivo
   DEFINE g_c_ruta_bin              LIKE seg_modulo.ruta_bin  -- ruta del bin del módulo
   DEFINE g_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados,-- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_arr_dev RECORD      -- Datos de registro
      id_derechohabiente            DECIMAL(9,0),
      id_ocg_devolucion             DECIMAL(9,0),
      nss                           char(11),
      folio_lote                    DECIMAL(9,0)
   END RECORD

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
   DEFINE v_query_dev               STRING -- variable que guarda la sentencia para ejecutar fn_devol_saldos
   DEFINE v_query                   STRING -- variable para guardar cadena para hacer query (actualiza tablas)
   DEFINE v_aceptada                INTEGER
   DEFINE v_rechazada               INTEGER
   DEFINE bnd_dev                   SMALLINT 

MAIN

   -- se recuperan los parámetros que envía el programa lanzador GRTL48
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".GRTP33.log")

   DISPLAY ""
   DISPLAY "=INICIA GRTP33="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid USING "<<<<<<<<<"
   DISPLAY " PROCESO       : ",g_i_proceso_cod
   DISPLAY ""
  
   -- se inicializan variables
   LET v_dt_fec_carga  = TODAY   --tiene que ser los 12 d cada mes
   LET v_subproc       = 4

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING g_c_ruta_bin,
                                   g_c_ruta_listados

   CALL fn_genera_devolucion() RETURNING bnd_dev

   IF(bnd_dev <> 1) THEN 
      CALL fn_error_opera(g_d_pid,g_i_proceso_cod,g_i_opera_cod) RETURNING r_b_valida
      DISPLAY " ERROR : OCURRIÓ UN ERROR AL GENERAR LA INFORMACIÓN"
   ELSE 
      -- Finaliza operación como OK
      CALL fn_actualiza_opera_fin(g_d_pid,g_i_proceso_cod,g_i_opera_cod) RETURNING r_b_valida
      DISPLAY " > EL proceso de  devolución de saldos excedentes 43Bis ha terminado correctamente "
      DISPLAY ""
   END IF 

   
   DISPLAY "=FIN="
   
END MAIN

FUNCTION fn_genera_devolucion()

   DEFINE v_ind_dev   SMALLINT 

   LET v_ind_dev = 0 -- Bandera de que no ha iniciado la generación de solicitudes
   
   LET v_query = "SELECT  d.id_ocg_devolucion,    ",
                       "  d.id_ocg_detalle,       ",
                       "  d.id_ocg_formalizacion, ",
                       "  d.id_ocg_tramite,       ",
                       "  d.id_derechohabiente,   ",
                       "  d.cve_ent_financiera    ",
                   " FROM ocg_devolucion d,       ",
                       "  ocg_fecha_mig m         ",
                  " WHERE d.estado =    30       ",
                    " AND d.edo_registro = 200   ",
                    " AND m.subproceso = 5       "

   --PREPARE prp_query FROM v_query
   --DECLARE crs_query CURSOR FOR prp_query

                                            
   -- se llama la función para obtener devolución de saldos excedentes. 

  { LET v_query_dev = "EXECUTE FUNCTION fn_devol_saldos(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_query_dev
   EXECUTE prp_procesa_marca_cuenta USING v_id_cre_ctr_archivo,
                                           v_arr_dev.id_derechohabiente,
                                           v_arr_dev.nss,
                                           v_arr_dev.folio_lote
                                      INTO v_aceptada,
                                            v_rechazada
   }

   LET v_ind_dev = 1  -- Bandera de termino

   RETURN v_ind_dev

END FUNCTION 

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