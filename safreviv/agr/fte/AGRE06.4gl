--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

####################################################################
#Módulo            =>AGR                                           #
#Programa          =>AGRE15                                        #
#Objetivo          =>Programa para integrar el archivo de desmarca #
#                    que ha sido validado                          #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>11 mayo 2012                                  #
#Modificaciones    => Héctor Jiménez                               #
#   Descripción    => Se modifica la ubicación del llamado de la  #
#                     función fn_agr_integra_cancelación           #
#   Fecha modif    => 8 Enero 2015                                 #
####################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

GLOBALS

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- número de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar

   DEFINE g_r_encabezado_rpt RECORD
      folio                         INTEGER, -- número de folio con formato
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

   DEFINE v_cnt_aceptados           INTEGER
   DEFINE v_cnt_rechazados          INTEGER
   DEFINE v_reac_acep               INTEGER
   DEFINE v_reac_rech               INTEGER




END GLOBALS

#Objetivo: Función que realiza la integración

MAIN
    
   DEFINE v_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- programa de la operación

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_i_cuenta_regs           INTEGER -- número de registros
   DEFINE v_cuenta_orig             INTEGER --Número de originaciones
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_si_exist_error          SMALLINT -- indica si existio error durante alguna rutina o proceso
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE r_c_ruta_bin_cta          LIKE seg_modulo.ruta_bin -- ruta del bin de cta
   DEFINE r_c_ruta_list_cta         LIKE seg_modulo.ruta_bin -- ruta listados cta
   DEFINE v_tpo_originacion         SMALLINT
   DEFINE v_s_mens_correo           STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo         STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo        STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados cta
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operacion
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE v_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err                INTEGER
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_c_nss                   LIKE afi_derechohabiente.nss
   DEFINE v_existe_info_archivo     SMALLINT
   DEFINE v_tipo_operacion          VARCHAR(1)
   DEFINE v_error                   SMALLINT
   DEFINE ch                        base.Channel
   DEFINE v_nohup2                  STRING
   

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

    --Ruta listados
    SELECT ruta_listados
     INTO v_c_ruta_list_bat
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   -- inicializa valores del record
   LET g_r_encabezado_rpt.tot_registros  = 0
   LET g_r_encabezado_rpt.tot_aceptados  = 0
   LET g_r_encabezado_rpt.tot_rechazados = 0
   LET g_r_encabezado_rpt.tot_sin_origen = 0

   --se inicializan los contadores
   LET v_cnt_aceptados  = 0
   LET v_cnt_rechazados = 0
   LET v_i_cuenta_regs  = 0
   LET v_cuenta_orig    = 0

   -- inicializa variable de existencia de información
   LET v_existe_info_archivo = 0

   -- se crea el archivo log
   --CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRE06.log")

   DISPLAY "=INICIA AGRE06="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 30 -- Solicitud desmarca
   LET v_si_id_proceso = g_id_proceso_agr -- Anualidades Garantizadas

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_desmarca_agr()

   -- se realiza la consulta que verifica si existen registros a desmarcar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM safre_tmp:tmp_desmarca_det_agr\n"

   PREPARE prp_slct_cnt_11 FROM v_s_qryTxt
   EXECUTE prp_slct_cnt_11 INTO v_i_cuenta_regs

   -- se verifica si hay información para originar
   IF v_i_cuenta_regs > 0 THEN
      -- se consulta la fecha de lote
      LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                       "   FROM safre_tmp:tmp_desmarca_det_agr\n",
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
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_desmarca(?,?,?,?)"

   PREPARE prp_integra_desmarca FROM v_s_qryTxt
   EXECUTE prp_integra_desmarca USING p_v_usuario,
                                      p_v_arch_proceso,
                                      p_d_folio,
                                      v_d_id_cre_ctr_arch
                                 INTO r_b_valida,
                                      r_isam_err,
                                      r_c_msj,
                                      r_c_nss

   IF r_b_valida <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de Integración: "
      DISPLAY "ERROR      : ",r_b_valida
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " PROCESA DESMARCA AG"

   LET v_tpo_originacion = 4 --Anualidades Garantizadas

   -- se crea la sentencia que ejecuta el procedimiento que ejecuta la desmarca cuenta
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_procesa_desmarca(?,?,?,?)"

   PREPARE prp_procesa_desmarca_ag FROM v_s_qryTxt
   EXECUTE prp_procesa_desmarca_ag USING p_v_usuario,
                                         p_d_folio,
                                         v_tpo_originacion,
                                         p_i_proceso_cod
                                    INTO v_si_exist_error

   IF v_si_exist_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE DESMARCA AG: ",v_si_exist_error

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
   LET v_tpo_originacion = 1 --Transferencia de Acreditados

   -- se crea la sentencia que ejecuta el procedimiento que ejecuta la desmarca cuenta
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_procesa_desmarca(?,?,?,?)"

   PREPARE prp_procesa_desmarca_ta FROM v_s_qryTxt
   EXECUTE prp_procesa_desmarca_ta USING p_v_usuario,
                                         p_d_folio,
                                         v_tpo_originacion,
                                         p_i_proceso_cod
                                    INTO v_si_exist_error

   IF v_si_exist_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE DESMARCA TA: ",v_si_exist_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF
   
   --función desmarca 
   LET v_tipo_operacion = "D"
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_cancelacion(?,?,?)"
   PREPARE prp_fn_cancela FROM v_s_qryTxt
   EXECUTE prp_fn_cancela USING p_v_usuario, p_d_folio, v_tipo_operacion
                          INTO v_error, v_cnt_aceptados,v_cnt_rechazados

   IF v_error = 0 AND v_cnt_aceptados <= 0 AND v_cnt_rechazados <= 0 THEN
     LET v_tipo_operacion = "A"

     LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_cancelacion(?,?,?)"

     PREPARE prp_fn_cancela_marca FROM v_s_qryTxt
     EXECUTE prp_fn_cancela_marca USING p_v_usuario, p_d_folio, v_tipo_operacion
                                   INTO v_error, v_cnt_aceptados,v_cnt_rechazados
   ELSE
      IF v_error <> 0 THEN
         DISPLAY " Ocurrió un error durante la Cancelación: "
         DISPLAY "ERROR      : ",v_error
         EXIT PROGRAM
      END IF
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*
   
   DISPLAY ""
   DISPLAY " Total registros            : ",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " Total aceptados            : ",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " Total rechazados           : ",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " Total sin origen           : ",v_r_cre_ctr_archivo.tot_sin_origen
   DISPLAY " Total cancelados aceptados : ",v_cnt_aceptados
   DISPLAY " Total cancelados rechazados: ",v_cnt_rechazados
   DISPLAY ""

   -- recupera la ruta bin y de listados para el módulo en proceso
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_gen_arch_salida(p_i_proceso_cod, p_i_opera_cod)

   LET v_i_cuenta_regs  = 0

   -- cuenta el número de registro a originar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                      " FROM safre_tmp:tmp_desmarca_det_agr_01",
                     " WHERE sts_credito = 1"

   PREPARE prp_cnt_tmp_desm_01 FROM v_s_qryTxt
   EXECUTE prp_cnt_tmp_desm_01 INTO v_i_cuenta_regs

   LET v_cuenta_orig = v_i_cuenta_regs
   
   -- se verifica si hay información para originar
   IF v_i_cuenta_regs > 0 THEN
      DISPLAY " Existe información de recurrente originación"

      LET v_c_programa_cod = "AGRL01"
      CALL fn_lanzador_recurrente(v_i_cuenta_regs, r_c_ruta_bin, v_c_programa_cod) RETURNING v_si_exist_error
      DISPLAY "con_registros   ruta bin       programa cod       regresaerror"
      DISPLAY v_i_cuenta_regs, r_c_ruta_bin, v_c_programa_cod, v_si_exist_error
      -- se verifica si existió error en el proceso
      IF v_si_exist_error <> 0 THEN
         -- se invoca la función que deja la operación en estado ERRONEA
         LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

         -- se verifica si fue posible finalizar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
         END IF

         EXIT PROGRAM
      END IF
   ELSE
      DISPLAY " No existe información de recurrente originación"
   END IF

   LET v_i_cuenta_regs  = 0

   -- cuenta el número de registro a reactivar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    " FROM safre_tmp:tmp_desmarca_det_agr_04"

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
   END IF


   -- se actualiza el folio en el registro correspondiente a las tablas de control del monitor del proceso
   UPDATE bat_ctr_operacion
      SET folio = p_d_folio
    WHERE pid = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod = p_i_opera_cod

   DISPLAY " GENERA REPORTE PDF"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   --**************
   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED, "-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegDesmarca(r_c_ruta_listados, v_v_nom_reporte, v_d_id_cre_ctr_arch)

   -- verifica que haya información para generar el archivo
   CALL fn_verifica_existe_info_archivo()
   RETURNING v_existe_info_archivo

   IF v_existe_info_archivo > 0 THEN
      -- se invoca generación de archivo de texto plano
      CALL fn_genera_archivo_rechazos()
   ELSE
      --Envía mensaje indicando que no se genera archivo por falta de información
      DISPLAY "NO SE GENERA ARCHIVO DE RECHAZOS POR FALTA DE INFORMACIÓN"
   END IF
   --********** 

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: RECEPCIÓN DESMARCA ANUALIDADES GARANTIZADAS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : RECEPCIÓN DESMARCA ANUALIDADES\n",
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

     -- Ejecuta programa que genera informe y extractor de cifras globales
  
     LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/AGRI01 ",  
                                            p_v_usuario, " ",                   
                                            p_d_pid, " ",                                       
                                            p_i_proceso_cod, " ",               
                                            3, " ",
                                            v_d_id_cre_ctr_arch," ",
                                            p_d_folio, " ",                     
                                            p_v_arch_proceso, " ",
                                            v_r_cre_ctr_archivo.tot_registros," ",
                                            v_r_cre_ctr_archivo.tot_aceptados," ", 
                                            v_r_cre_ctr_archivo.tot_rechazados," ",
                                            v_r_cre_ctr_archivo.tot_sin_origen," ",
                                            v_reac_acep," ",
                                            v_reac_rech," ",
                                            v_cnt_aceptados," ",
                                            v_cnt_rechazados," ",
                                            v_cuenta_orig," "," 1> ",       
                                            v_c_ruta_list_bat CLIPPED,
                                            "/nohup:",p_d_pid USING "&&&&&",":",
                                            p_i_proceso_cod USING "&&&&&",":",
                                            3 USING "&&&&&",
                                            " 2>&1 &"

   DISPLAY "COMANDO : ",v_s_comando

   RUN v_s_comando 

END MAIN

#Objetivo: Función que crea la tabla temporal de la integración de desmarca AGR
FUNCTION fn_crea_tmp_desmarca_agr()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_nss_desmarcados_agr
   WHENEVER ERROR STOP

   CREATE TABLE tmp_nss_desmarcados_agr(nss CHAR(11),
                                        tpo_credito SMALLINT,
                                        estado      SMALLINT) --Se agrega el estado a la tabla

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegDesmarca(p_c_ruta_listados, p_v_nom_reporte, p_d_id_cre_ctr_arch)

   DEFINE p_c_ruta_listados LIKE seg_modulo.ruta_listados -- ruta listados cta
   DEFINE p_v_nom_reporte   VARCHAR(80) -- nombre del reporte
   DEFINE p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo

   DEFINE r_rpt_rechazados RECORD   --Registro para almacenar los valores de los registros rechazados
      v_nss          CHAR(11),
      v_tpo_credito  CHAR(2),
      v_des_credito  VARCHAR(30),
      v_estado       CHAR(2),
      v_des_estado  VARCHAR(25)
   END RECORD

   DEFINE v_r_cre_ctr_arch  RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_manejador_rpt   OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt        STRING -- contiene una sentencia sql a ejecutar
   DEFINE r_b_valida        SMALLINT
   DEFINE v_existe_info     INTEGER

   -- inicializa la variable de existencia de información
   LET v_existe_info = 0

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRE061.4rp") THEN
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

   #DEBUG#
   --DISPLAY "cre_ctr_archivo:\n",v_s_qryTxt
   #END DEBUG#

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod
   #DEBUG#
   --DISPLAY "bat_ctr_operacion:\n",v_s_qryTxt
   #END DEBUG#

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET g_r_encabezado_rpt.nom_archivo = v_r_bat_ctr_opera.nom_archivo
   LET g_r_encabezado_rpt.fecha_hr_ini = v_r_bat_ctr_opera.fecha_ini
   LET g_r_encabezado_rpt.fecha_hr_fin = v_r_bat_ctr_opera.fecha_fin
   LET g_r_encabezado_rpt.id_operacion = v_r_cre_ctr_arch.operacion
   LET g_r_encabezado_rpt.desc_operacion = fn_obt_desc_operacion(g_r_encabezado_rpt.id_operacion)
   LET g_r_encabezado_rpt.usuario = v_r_bat_ctr_opera.usuario
   LET g_r_encabezado_rpt.tot_registros = v_r_cre_ctr_arch.tot_registros
   LET g_r_encabezado_rpt.tot_aceptados = v_r_cre_ctr_arch.tot_aceptados
   LET g_r_encabezado_rpt.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados
   LET g_r_encabezado_rpt.tot_sin_origen = v_r_cre_ctr_arch.tot_sin_origen
   LET g_r_encabezado_rpt.folio = p_d_folio

   --Asigna 0 en caso de nulos
   IF  g_r_encabezado_rpt.tot_aceptados IS NULL THEN
      LET g_r_encabezado_rpt.tot_aceptados = 0
   END IF

   IF  g_r_encabezado_rpt.tot_rechazados IS NULL THEN
      LET g_r_encabezado_rpt.tot_rechazados = 0
   END IF

   IF g_r_encabezado_rpt.tot_sin_origen IS NULL THEN
      LET  g_r_encabezado_rpt.tot_sin_origen = 0
   END IF

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_desmarca TO XML HANDLER v_manejador_rpt

   --Verifica que exista información 
   LET v_s_qryTxt="SELECT COUNT(*) \n",
                  "FROM safre_tmp:tmp_nss_desmarcados_agr\n",
                  "WHERE estado IN (2,3,4)"

   PREPARE stm_existe_info_tmp FROM v_s_qryTxt
   EXECUTE stm_existe_info_tmp INTO v_existe_info

   -- valida resultado
   IF v_existe_info > 0 THEN
          --Obtiene la información del detalle de la tabla temporal
          LET v_s_qryTxt="SELECT * \n",
                         "FROM safre_tmp:tmp_nss_desmarcados_agr\n",
                         "WHERE estado IN (2,3,4)"

          PREPARE stm_tmp_nss FROM v_s_qryTxt
          DECLARE cur_tmp_nss CURSOR FOR stm_tmp_nss

          --Recorrido del cursor
          FOREACH cur_tmp_nss INTO r_rpt_rechazados.v_nss,
                                   r_rpt_rechazados.v_tpo_credito,
                                   r_rpt_rechazados.v_estado

             --Obtiene la descrippción del tipo de crédito
             LET r_rpt_rechazados.v_des_credito = fn_recupera_desc_credito(r_rpt_rechazados.v_tpo_credito)

             --Asigna descripción del estado del crédito
             IF r_rpt_rechazados.v_estado = 2  THEN
                --Se asigna valor de la cadena
                LET r_rpt_rechazados.v_des_estado = "DESMARCA RECHAZADA"
             END IF 
             IF r_rpt_rechazados.v_estado = 3 THEN 
                --Se asigna valor a la cadena
                LET r_rpt_rechazados.v_des_estado = "NSS RECHAZADO"
             END IF 
             IF r_rpt_rechazados.v_estado = 4 THEN 
                --Se asigna valor a la cadena
                LET r_rpt_rechazados.v_des_estado = "NSS NO PROCESADO POR TR"
             END IF

             --Envía al reporte el detalle de los rechazados
             OUTPUT TO REPORT reporte_integ_desmarca(r_rpt_rechazados.*)
          END FOREACH
    ELSE
       --Se envía leyenda de no existe información
       LET r_rpt_rechazados.v_nss         = "ND" 
       LET r_rpt_rechazados.v_tpo_credito = "ND" 
       LET r_rpt_rechazados.v_des_credito = "ND"
       LET r_rpt_rechazados.v_estado      = "ND"
       LET r_rpt_rechazados.v_des_estado  = "ND"

       OUTPUT TO REPORT reporte_integ_desmarca(r_rpt_rechazados.*)
    END IF

   -- finaliza el reporte
   FINISH REPORT reporte_integ_desmarca

   -- se devuelve si existe información de rechazos
   --RETURN v_existe_info

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_desmarca(p_r_rechazados)

   DEFINE p_r_rechazados RECORD
      v_nss          CHAR(11),
      v_tpo_credito  SMALLINT,
      v_des_credito  VARCHAR(30),
      v_estado       SMALLINT,
      v_des_estado   VARCHAR(25)
   END RECORD

   DEFINE v_fecha_reporte   DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_v_usuario
      PRINTX g_r_encabezado_rpt.folio
      PRINTX g_r_encabezado_rpt.nom_archivo
      PRINTX g_r_encabezado_rpt.fecha_hr_ini
      PRINTX g_r_encabezado_rpt.fecha_hr_fin
      PRINTX g_r_encabezado_rpt.id_operacion
      PRINTX g_r_encabezado_rpt.desc_operacion
      PRINTX g_r_encabezado_rpt.usuario
      PRINTX g_r_encabezado_rpt.tot_registros USING "&&&&&&&&&&"
      PRINTX g_r_encabezado_rpt.tot_aceptados USING "&&&&&&&&&&"
      PRINTX g_r_encabezado_rpt.tot_rechazados USING "&&&&&&&&&&"
      PRINTX g_r_encabezado_rpt.tot_sin_origen USING "&&&&&&&&&&"

   ON EVERY ROW
      PRINTX p_r_rechazados.v_nss
      --PRINTX p_r_rechazados.v_tpo_credito
      PRINTX p_r_rechazados.v_des_credito
      --PRINTX p_r_rechazados.v_estado
      PRINTX p_r_rechazados.v_des_estado
      PRINTX v_cnt_aceptados
      PRINTX v_cnt_rechazados

END REPORT

#Objetivo: Busca la descripción de la operación
FUNCTION fn_obt_desc_operacion(p_c_operacion)

   DEFINE p_c_operacion  LIKE cat_operacion_prc.operacion -- operación
   DEFINE v_c_desc_opera LIKE cat_operacion_prc.desc_operacion -- descripción de la operación
   DEFINE v_s_qryTxt     STRING -- se asigna consulta sql a ejecutar

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

   DEFINE p_i_cuenta_regs   INTEGER -- número de registros
   DEFINE p_c_ruta_bin      LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE p_c_programa_cod  LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_d_pid           DECIMAL(9,0) -- identificador del proceso
   DEFINE v_i_proceso_cod   LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod     LIKE cat_operacion.opera_cod -- operación que llama la funcion
   DEFINE v_i_operacion     LIKE cre_ctr_archivo.operacion -- operacion
   DEFINE v_d_folio         LIKE bat_ctr_proceso.folio -- folio del proceso
   DEFINE v_c_ruta_list_bat LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_r_cre_ctr_arch  RECORD LIKE cre_ctr_archivo.* -- registro de la tabla de control
   DEFINE v_r_glo_ctr_arch  RECORD LIKE glo_ctr_archivo.* -- registro de la tabla de control global
   DEFINE v_si_lote         LIKE cre_ctr_archivo.lote -- lote del archivo
   DEFINE v_dt_f_lote       LIKE cre_ctr_archivo.f_lote -- fecha de lote del archivo
   DEFINE v_c_extension     LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando       STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt        STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_si_existe_err   SMALLINT -- booleana que indica si existió algun error en la función
   DEFINE r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_nohup2          STRING
   DEFINE v_ruta_listados   STRING
   DEFINE ch                base.Channel
   DEFINE v_estado          SMALLINT
   DEFINE v_cadena          STRING


   -- se inializan las variables
   LET v_i_proceso_cod = g_proc_cod_agr_recurrente -- recepción recurrente originacion AG
   LET v_i_opera_cod   = 1 -- valida archivo recurrente
   LET v_i_operacion   = 21 -- operacion del proceso
   LET v_d_pid         = 0
   LET v_d_folio       = 0
   LET v_si_existe_err = 0 -- se asume que no existira error en la función

   -- se invoca la funcion que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

   -- se concatena la extension al nombre del archivo
   LET p_v_arch_proceso = p_v_arch_proceso || "." || v_c_extension CLIPPED

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    " WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   LET ch = base.Channel.create()

   LET v_i_opera_cod = 1

   LET v_nohup2 = v_c_ruta_list_bat CLIPPED,"/nohup:",
                  v_d_pid USING "&&&&&",":",
                  v_i_proceso_cod USING "&&&&&",":",
                  v_i_opera_cod USING "&&&&&"

   CALL ch.openFile(v_nohup2,"w")
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en Valida(L661):"
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
   END IF 


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
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en Inicializa:"
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
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
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en la función de actualización de ejecución de proceso: "
      --LET v_cadena = " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en la función de actualización de ejecución de proceso: "
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
   END IF

   -- se muestra mensaje a usuario
   DISPLAY " Se ha enviado (automáticamente) la validación con PID: ",v_d_pid CLIPPED,"\n",
           " Puede revisar el avance del proceso en el monitor de ejecución de procesos"


   -- se insertan los registros en las tablas temporales
   DELETE FROM safre_tmp:tmp_cre_acred_agr_01 WHERE 1=1
   DELETE FROM safre_tmp:tmp_cre_acred_agr_02 WHERE 1=1

   -- se inserta la información de la tabla de Desmarca a la tabla de Recurrente
   INSERT INTO safre_tmp:tmp_cre_acred_agr_01 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_01

   -- se consulta la fecha de lote
   LET v_s_qryTxt = " SELECT FIRST 1 fec_proceso\n",
                    "   FROM safre_tmp:tmp_cre_acred_agr_01\n",
                    "  WHERE fec_proceso IS NOT NULL"

   PREPARE prp_slct_frst_fproc FROM v_s_qryTxt
   EXECUTE prp_slct_frst_fproc INTO v_dt_f_lote

   -- se valida la fecha de lote
   IF v_dt_f_lote IS NULL THEN
      LET v_dt_f_lote = TODAY
   END IF

   -- se busca número de lote correspondiente al archivo
   SELECT MAX(lote)
     INTO v_si_lote
     FROM cre_ctr_archivo
    WHERE f_lote = v_dt_f_lote
      AND operacion = v_i_operacion

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_si_lote IS NULL THEN
      LET v_si_lote = 1
   ELSE
      LET v_si_lote = v_si_lote + 1
   END IF

   -- se inserta el registro de control
   LET v_r_cre_ctr_arch.folio_archivo  = v_d_folio
   LET v_r_cre_ctr_arch.lote           = v_si_lote
   LET v_r_cre_ctr_arch.f_lote         = v_dt_f_lote
   LET v_r_cre_ctr_arch.id_proceso     = g_id_proceso_agr -- Anualidades Garantizadas
   LET v_r_cre_ctr_arch.operacion      = v_i_operacion -- Recurrente
   LET v_r_cre_ctr_arch.nom_archivo    = p_v_arch_proceso
   LET v_r_cre_ctr_arch.tot_registros  = p_i_cuenta_regs
   LET v_r_cre_ctr_arch.tot_aceptados  = 0
   LET v_r_cre_ctr_arch.tot_rechazados = 0
   LET v_r_cre_ctr_arch.tot_sin_origen = 0
   LET v_r_cre_ctr_arch.estado         = 10
   LET v_r_cre_ctr_arch.f_proceso      = TODAY
   LET v_r_cre_ctr_arch.usuario        = p_v_usuario

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
   LET v_r_glo_ctr_arch.proceso_cod    = v_i_proceso_cod
   LET v_r_glo_ctr_arch.opera_cod      = v_i_opera_cod
   LET v_r_glo_ctr_arch.nombre_archivo = p_v_arch_proceso
   LET v_r_glo_ctr_arch.folio          = NULL
   LET v_r_glo_ctr_arch.estado         = 1
   LET v_r_glo_ctr_arch.f_actualiza    = TODAY
   LET v_r_glo_ctr_arch.usuario        = p_v_usuario

   -- se inserta el registro en la tabla de control
   INSERT INTO glo_ctr_archivo VALUES (v_r_glo_ctr_arch.*)

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(v_d_pid,
                                           v_i_proceso_cod,
                                           v_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en la función de actualización de ejecución de proceso: "
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
   END IF

   -- se muestra mensaje a usuario
   DISPLAY " Finalizó la Validación satisfactoriamente"

   --escribe en el archivo
   CALL ch.write([v_cadena])
   CALL ch.close()
   CALL fn_actualiza_opera_fin(v_d_pid,
                                  v_i_proceso_cod,
                                  v_i_opera_cod) RETURNING v_estado

   -- se asignan los valores necesarios para la intergración
   LET v_i_opera_cod = 2 -- integra archivo recurrente

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en Valida(L826):"
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
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
      DISPLAY " ERROR: No fue posible la ejecución de Originación. Ocurrió un error en la Actualiza ini:"
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se indica ha ocurrido una excepción
      LET v_si_existe_err = 1

      RETURN v_si_existe_err
   END IF

   -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
   LET v_s_comando = " nohup time fglrun ",p_c_ruta_bin CLIPPED,"/AGRE01 ",
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

   RETURN v_si_existe_err

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
   LET v_v_arch_salida = "Agr" || v_c_fec_hoy || "." || v_c_extension
   DISPLAY " REPORTE SALIDA (TIPO CRÉDITO): ",v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

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
                    "   FROM safre_tmp:tmp_nss_desmarcados_agr \n",
                    "   WHERE estado = 1"

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

   DEFINE p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_s_qryTxt          STRING -- guarda una sentencia SQL a ejecutar
   DEFINE r_si_cod_err        SMALLINT --indica si existio error en la funcion
   DEFINE r_i_isam_err        INTEGER
   DEFINE r_v_msj_err         VARCHAR(250)
   DEFINE r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DISPLAY " PROCESA REACTIVACIÓN AG"

   -- se crea la sentencia que ejecuta el procedimiento de reactivación de créditos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_desm_reactiva(?,?,?,?)"

   PREPARE prp_procesa_reactivacion FROM v_s_qryTxt
   EXECUTE prp_procesa_reactivacion USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO r_si_cod_err,
                                          r_i_isam_err,
                                          r_v_msj_err,
                                          v_reac_acep,
                                          v_reac_rech

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

#OBJETIVO: Obtiene la descripción del tipo de crédito del catálogo de tipo de crédito
FUNCTION fn_recupera_desc_credito (p_tpo_credito)

   DEFINE p_tpo_credito   SMALLINT
   DEFINE v_s_sql         STRING
   DEFINE v_desc_credito  VARCHAR(30)

    --Inicializa variable
    LET v_desc_credito = NULL

   --Armado del query de consulta de tipo de crédito
   LET v_s_sql="SELECT FIRST 1 desc_credito   \n",
               "FROM cat_tipo_credito \n",
               "WHERE tpo_credito = ?   "

   --PreParación y ejecución
   PREPARE stm_des_credito FROM v_s_sql
   EXECUTE stm_des_credito USING p_tpo_credito INTO v_desc_credito

   --Valida que haya encontrado información
   IF  v_desc_credito IS NULL THEN
      --Se indica que no existió descripción
      LET v_desc_credito = "SIN DESCRIPCIÓN"
   END IF

   --Regresa la descripción encontrada
   RETURN v_desc_credito

END FUNCTION

#Objetivo: Función que genera el archivo de texto plano para conformación del layout desmarcas y nss rechazados
FUNCTION  fn_genera_archivo_rechazos()

   DEFINE v_archivo_salida        STRING
   DEFINE v_s_sql                 STRING
   DEFINE v_s_linea               STRING
   DEFINE v_c_ruta_envio          VARCHAR(40)
   DEFINE v_nom_archivo_rechazos  STRING
   DEFINE v_ch_archivo            base.channel

   DEFINE r_archivo RECORD
      tpo_registro      CHAR(2), 
      nss               CHAR(11), 
      num_credito       CHAR(10), 
      ssv_92_97         CHAR(8), 
      fec_otorgamiento  DATE,
      fec_culminacion   DATE, 
      tpo_credito       CHAR(3), 
      sts_credito       CHAR(3), 
      tpo_descuento     CHAR(1), 
      val_descuento     CHAR(8),
      nrp               CHAR(11),
      fec_ini_oblig_pat DATE, 
      nss_liberado      CHAR(11), 
      fec_proceso       DATE,
      sdo_credito       CHAR(8),
      fec_prox_liquidar CHAR(8), 
      fec_dsd_avis_desa DATE, 
      fec_hst_avis_desa DATE, 
      tpo_rechazo       CHAR(2)
   END RECORD

   DEFINE v_fec_otorgamiento    CHAR(8)
   DEFINE v_fec_culminacion     CHAR(8)
   DEFINE v_fec_ini_oblig_pat   CHAR(8)
   DEFINE v_fec_proceso         CHAR(8)
   DEFINE v_fec_prox_liquidar   CHAR(8)
   DEFINE v_fec_dsd_avis_desa   CHAR(8)
   DEFINE v_fec_hst_avis_desa   CHAR(8)

   -- prepara el canal para abrirlo en modo escritura
   LET v_ch_archivo = base.channel.Create()

   -- se arma el nombre del archivo
   LET v_nom_archivo_rechazos = "dmrch",TODAY USING "ddmmyyyy",".drc"
   LET v_nom_archivo_rechazos = v_nom_archivo_rechazos.trimLeft()
   LET v_nom_archivo_rechazos = v_nom_archivo_rechazos.trimRight()

   --Obtiene el valor de la ruta de envio
   LET v_s_sql="SELECT ruta_envio FROM seg_modulo \n ",
               "WHERE modulo_cod='agr'"

   PREPARE stm_ruta_envio FROM v_s_sql
   EXECUTE stm_ruta_envio INTO v_c_ruta_envio
   -- concatena la ruta y nombre del archivo
   LET v_c_ruta_envio=v_c_ruta_envio CLIPPED

   -- concatena nombre de archivo de salida
   LET v_archivo_salida=v_c_ruta_envio,"/",v_nom_archivo_rechazos

   --abre el archivo en modo escritura
   CALL v_ch_archivo.openFile(v_archivo_salida,"w")
   CALL v_ch_archivo.setDelimiter("")

   --Prepara el query para obtener los registros con desmarca rechazada
   LET v_s_sql="SELECT b.* \n",
               "FROM safre_tmp:tmp_nss_desmarcados_agr a, \n",
               "     safre_tmp:tmp_desmarca_det_agr b\n",
               "WHERE a.estado = 2\n",
               "AND b.nss=a.nss \n",
               "AND b.tpo_credito=a.tpo_credito"
   PREPARE stm_nss_rechazados FROM v_s_sql
   DECLARE cur_nss_rechazados CURSOR FOR stm_nss_rechazados

   --recorre el recordset de resultados
   FOREACH cur_nss_rechazados INTO r_archivo.*

   --Formatea la salida al archivo
      LET r_archivo.tpo_registro      = r_archivo.tpo_registro      USING "&&&"   	
      LET r_archivo.nss               = r_archivo.nss               USING "&&&&&&&&&&&"
      LET r_archivo.num_credito       = r_archivo.num_credito       USING "&&&&&&&&&&"
      LET r_archivo.ssv_92_97         = r_archivo.ssv_92_97         USING "&&&&&&&&"
      LET v_fec_otorgamiento          = fn_formatea_fecha(r_archivo.fec_otorgamiento)
      LET v_fec_culminacion           = fn_formatea_fecha(r_archivo.fec_culminacion)
      LET r_archivo.tpo_credito       = r_archivo.tpo_credito       USING "&&&"
      LET r_archivo.sts_credito       = r_archivo.sts_credito       USING "&&&"
      LET r_archivo.tpo_descuento     = r_archivo.tpo_descuento     USING "&"
      LET r_archivo.val_descuento     = r_archivo.val_descuento     USING "&&&&&&&&&&"
      LET r_archivo.nrp               = r_archivo.nrp               USING "&&&&&&&&&&&"
      LET v_fec_ini_oblig_pat         = fn_formatea_fecha(r_archivo.fec_ini_oblig_pat)
      LET r_archivo.nss_liberado      = r_archivo.nss_liberado      USING "&&&&&&&&&&&"
      LET v_fec_proceso               = fn_formatea_fecha(r_archivo.fec_proceso)
      LET r_archivo.sdo_credito       = r_archivo.sdo_credito       USING "&&&&&&&&"
      LET v_fec_prox_liquidar         = fn_formatea_fecha(r_archivo.fec_prox_liquidar)
      LET v_fec_dsd_avis_desa         = fn_formatea_fecha(r_archivo.fec_dsd_avis_desa)
      LET v_fec_hst_avis_desa         = fn_formatea_fecha(r_archivo.fec_hst_avis_desa)
      LET r_archivo.tpo_rechazo       = 2 SPACES

      --Concatena línea
      LET v_s_linea = r_archivo.tpo_registro  ,
                      r_archivo.nss           ,
                      r_archivo.num_credito   ,
                      r_archivo.ssv_92_97     ,
                      v_fec_otorgamiento      ,
                      v_fec_culminacion       ,
                      r_archivo.tpo_credito   ,
                      r_archivo.sts_credito   ,
                      r_archivo.tpo_descuento ,
                      r_archivo.val_descuento ,
                      r_archivo.nrp           ,
                      v_fec_ini_oblig_pat     ,
                      r_archivo.nss_liberado  ,
                      v_fec_proceso           ,
                      r_archivo.sdo_credito   ,
                      v_fec_prox_liquidar     ,
                      v_fec_dsd_avis_desa     ,
                      v_fec_hst_avis_desa     ,
                      r_archivo.tpo_rechazo

      --Escribe en el archivo
      CALL v_ch_archivo.write([v_s_linea])
   END FOREACH

   --Prepara query para obtener nss rechazados
   LET v_s_sql="SELECT b.* \n",
               "FROM safre_tmp:tmp_nss_desmarcados_agr a, \n",
               "     safre_tmp:tmp_desmarca_det_agr_02 b\n",
               "WHERE a.estado IN (3,4)\n",
               "AND b.nss=a.nss \n",
               "AND b.tpo_credito=a.tpo_credito"

   PREPARE stm_desmarca_rechazada FROM v_s_sql
   DECLARE cur_desmarca_rechazada CURSOR FOR stm_desmarca_rechazada

   --Se itera el resultado
   FOREACH cur_desmarca_rechazada INTO r_archivo.*
      --Formatea la salida al archivo
      LET r_archivo.tpo_registro      = r_archivo.tpo_registro      USING "&&&"   	
      LET r_archivo.nss               = r_archivo.nss               USING "&&&&&&&&&&&"
      LET r_archivo.num_credito       = r_archivo.num_credito       USING "&&&&&&&&&&"
      LET r_archivo.ssv_92_97         = r_archivo.ssv_92_97         USING "&&&&&&&&"
      LET v_fec_otorgamiento          = fn_formatea_fecha(r_archivo.fec_otorgamiento)
      LET v_fec_culminacion           = fn_formatea_fecha(r_archivo.fec_culminacion)
      LET r_archivo.tpo_credito       = r_archivo.tpo_credito       USING "&&&"
      LET r_archivo.sts_credito       = r_archivo.sts_credito       USING "&&&"
      LET r_archivo.tpo_descuento     = r_archivo.tpo_descuento     USING "&"
      LET r_archivo.val_descuento     = r_archivo.val_descuento     USING "&&&&&&&&&&"
      LET r_archivo.nrp               = r_archivo.nrp               USING "&&&&&&&&&&&"
      LET v_fec_ini_oblig_pat         = fn_formatea_fecha(r_archivo.fec_ini_oblig_pat)
      LET r_archivo.nss_liberado      = r_archivo.nss_liberado      USING "&&&&&&&&&&&"
      LET v_fec_proceso               = fn_formatea_fecha(r_archivo.fec_proceso)
      LET r_archivo.sdo_credito       = r_archivo.sdo_credito       USING "&&&&&&&&"
      LET v_fec_prox_liquidar         = fn_formatea_fecha(r_archivo.fec_prox_liquidar)
      LET v_fec_dsd_avis_desa         = fn_formatea_fecha(r_archivo.fec_dsd_avis_desa)
      LET v_fec_hst_avis_desa         = fn_formatea_fecha(r_archivo.fec_hst_avis_desa)
      LET r_archivo.tpo_rechazo       = 2 SPACES

      --Concatena línea
     LET v_s_linea = r_archivo.tpo_registro  ,
                     r_archivo.nss           ,
                     r_archivo.num_credito   ,
                     r_archivo.ssv_92_97     ,
                     v_fec_otorgamiento      ,
                     v_fec_culminacion       ,
                     r_archivo.tpo_credito   ,
                     r_archivo.sts_credito   ,
                     r_archivo.tpo_descuento ,
                     r_archivo.val_descuento ,
                     r_archivo.nrp           ,
                     v_fec_ini_oblig_pat     ,
                     r_archivo.nss_liberado  ,
                     v_fec_proceso           ,
                     r_archivo.sdo_credito   ,
                     v_fec_prox_liquidar     ,
                     v_fec_dsd_avis_desa     ,
                     v_fec_hst_avis_desa     ,
                     r_archivo.tpo_rechazo

      --Escribe en el archivo
      CALL v_ch_archivo.write([v_s_linea])
   END FOREACH

   -- cierre del archivo
   CALL v_ch_archivo.close()

   -- se despliega mensaje indicando la ruta donde se ha creado el archivo
   DISPLAY "SE HA GENERADO ARCHIVO DE RECHAZOS EN LA RUTA: ",v_archivo_salida

END FUNCTION 

#Objetivo: Función que formatea la fecha para ajustarla a 8 caracteres en formato yyyymmdd
FUNCTION fn_formatea_fecha(p_fecha)

   DEFINE p_fecha              DATE
   DEFINE v_fecha_caracter     CHAR(8)
   DEFINE v_anho               CHAR(4)
   DEFINE v_mes                CHAR(2)
   DEFINE v_dia                CHAR(2)
   DEFINE v_t_token            base.StringTokenizer
   DEFINE v_s_indice           SMALLINT

   -- valida que la fecha no sea nulo
   IF p_fecha IS NULL THEN
      LET v_fecha_caracter = 8 SPACES
      RETURN v_fecha_caracter
   END IF

   --Crea el tokenizer para obtener las subcadenas
   LET v_t_token = base.StringTokenizer.create(p_fecha,"/")

   --Inicializa el índice
   LET v_s_indice = 1

   --Iteración de las cadenas
   WHILE v_t_token.hasMoreTokens()
      --verifica la posición para obtener el valor adecuado
      CASE v_s_indice
         WHEN 1
            LET v_mes=v_t_token.nextToken()
            LET v_s_indice= v_s_indice + 1 
         WHEN 2
            LET v_dia=v_t_token.nextToken()
            LET v_s_indice= v_s_indice + 1
         WHEN 3
            LET v_anho=v_t_token.nextToken()
            LET v_s_indice= v_s_indice + 1
      END CASE
   END WHILE

   -- concatena la salida
   LET v_fecha_caracter = v_anho,v_mes,v_dia

   -- regresa la cadena armada
   RETURN v_fecha_caracter

END FUNCTION

#Objetivo: Función que verifica si existe información en las tablas temporales para la creación del archivo de texto
FUNCTION fn_verifica_existe_info_archivo()

   DEFINE v_s_sql            STRING
   DEFINE v_i_num_registros  INTEGER
   DEFINE v_s_bandera        SMALLINT

   -- bandera de existencia de información
   LET v_s_bandera = FALSE

   --Arma la sentencia de selección para desmarca rechazada
   LET v_s_sql = " SELECT COUNT(*) \n",
                 " FROM safre_tmp:tmp_nss_desmarcados_agr a, \n",
                 "      safre_tmp:tmp_desmarca_det_agr b\n",
                 " WHERE a.estado = 2\n",
                 " AND b.nss=a.nss \n",
                 " AND b.tpo_credito=a.tpo_credito"

   PREPARE stm_existe_desmarca FROM v_s_sql
   EXECUTE stm_existe_desmarca INTO v_i_num_registros

   -- valida que exista información
   IF v_i_num_registros > 0 THEN
      -- se actualiza el valor de la bandera
      LET v_s_bandera = TRUE
   END IF

      --Arma la sentencia de selección para nss rechazados
   LET v_s_sql="SELECT COUNT(*) \n",
               "FROM safre_tmp:tmp_nss_desmarcados_agr a, \n",
               "     safre_tmp:tmp_desmarca_det_agr_02 b\n",
               "WHERE a.estado IN (3,4)\n",
               "AND b.nss=a.nss \n",
               "AND b.tpo_credito=a.tpo_credito"

   PREPARE stm_existe_nss FROM v_s_sql
   EXECUTE stm_existe_nss INTO v_i_num_registros

   -- valida que exista información 
   IF v_i_num_registros > 0 THEN
      -- se actualiza el valor de la bandera
      LET v_s_bandera = TRUE
   END IF

   -- regresa valor de la bandera
   RETURN v_s_bandera

END FUNCTION

