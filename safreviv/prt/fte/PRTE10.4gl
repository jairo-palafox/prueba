--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/04/2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTE10                                                   #
#Descripcion       => Batch de preliquidación de traspasos de saldos cedente   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Abril 2015                                            #
#Mod               => 2 ene 2019                                               #
#                     viv92                                                    #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"

DEFINE p_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod,  # Código de operacion
       p_folio       LIKE glo_folio.folio,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_hoy      CHAR(008)
   
MAIN
DEFINE r_error          SMALLINT,
       r_reultado_opera SMALLINT,
       p_titulo         STRING, # titulo del mensaje enviado en el correo
       p_mensaje        STRING,  # cuerpo del mensaje enviado
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_operacion_desc  LIKE cat_operacion.opera_desc,
       v_total_registros INTEGER,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta            LIKE seg_modulo.ruta_listados,
      v_comando  STRING 
     
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

      # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_operacion_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   LET v_comando = " SELECT COUNT(*)",
                   "   FROM prt_traspaso_cedente trs JOIN prt_solicitud_cedente sol",
                   "     ON sol.id_prt_solicitud_cedente = trs.id_prt_solicitud_cedente",
                   "  WHERE trs.estado = ?",
                   "    AND sol.tipo_portabilidad = ?",
                   "    AND sol.estado = ?"
    PREPARE prp_rec_conteo_reg FROM v_comando
    EXECUTE prp_rec_conteo_reg USING C_ESTADO_TRASPASO_NOTIFICADA_CED,
                                     C_TIPO_PRT_CRED_NUEVO,
                                     C_ESTADO_SDO_NOTIFICADO_PRO
                                INTO v_total_registros

   DISPLAY "\n"
   DISPLAY "PROCESO:   ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_operacion_desc
   DISPLAY "TOTAL DE REGISTROS A PRELIQUIDAR:",v_total_registros USING "##,###,##&"
   DISPLAY "\n"

   #Llamada a ejecución de procedimiento almacenado
   CALL fn_ejecuta_preliquidacion() RETURNING r_error

   IF NOT( r_error )THEN
      LET p_mensaje = "Preliquidación realizada con éxito"

      CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, r_ruta
      --CALL fn_rutas("bat") RETURNING r_ruta, r_ruta_lst
      CALL fn_rpt_preliquidacion_trasp_ced(p_folio,
                                           C_ESTADO_SDO_PRELIQUIDADO_CED,
                                           p_usuario_cod,
                                           p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           "PRTL10")

      {LET v_comando = "fglrun ",r_ruta_ejecutable CLIPPED,"/PRTI10.42r ",p_usuario_cod, " ",
                                                                         p_pid, " ",
                                                                         p_proceso_cod," ",
                                                                         p_opera_cod," ",
                                                                         p_folio," ",
                                                                         "PRTL10"}
                      {"' 1>>", r_ruta_lst CLIPPED,
                      "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                      " 2>&1 &"}
      {RUN v_comando
      IF( STATUS )THEN
         DISPLAY "Ocurrió un error al ejecutar el reporte de la preliquidación"
      END IF}

      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_reultado_opera
      IF( r_reultado_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_reultado_opera)
      END IF
   ELSE
      # Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod) RETURNING r_reultado_opera
      IF( r_reultado_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_reultado_opera)
      END IF
      LET p_mensaje = "El proceso de preliquidación ha finalizado con errores."
   END IF

   # Envío de correo con estado de finalizacionde operacion
   LET p_titulo = "Finalización de operación - Preliquidación traspasos cedente"

   CALL fn_correo_proceso(p_pid,
                          p_proceso_cod,
                          p_opera_cod,
                          NULL,
                          p_titulo,
                          p_mensaje)

END MAIN

# Descripción: Ejecucion de SP de preliquidación de traspasos cedente
FUNCTION fn_ejecuta_preliquidacion()

DEFINE v_consulta   STRING,
       r_sql_error  INTEGER,
       r_isam_error INTEGER,
       r_msg_error  VARCHAR(40),
       v_error      BOOLEAN,
       r_registros_procesados INTEGER,
       r_total_aivs           DECIMAL(22,2),
       r_total_pesos          DECIMAL(22,2),
       v_fecha_actual         DATE

   WHENEVER ERROR CONTINUE
   # inicializacion de flujo correcto
   LET v_error = FALSE 

   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,
                        p_opera_cod,
                        p_usuario_cod) RETURNING p_folio

   # Generar encabezado de lote
   LET v_consulta = "INSERT INTO prt_cza_cedente ",
                    "(folio_liquida,",
                    " tipo_traspaso,",
                    " fecha_presentacion,",
                    " estado)",
                    "VALUES(?,?,?,?)"
   PREPARE prp_genera_cza_lote FROM v_consulta

   # Generar sumario de lote
   LET v_consulta = "INSERT INTO prt_sum_cedente ",
                    "(folio_liquida,",
                    " total_registros,",
                    " mto_aivs_viv97,",
                    " mto_pesos_viv97)",
                    "VALUES(?,?,?,?)"
   PREPARE prp_genera_sum_lote FROM v_consulta

   UPDATE bat_ctr_operacion
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod

   UPDATE bat_ctr_proceso
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod

   LET v_consulta = "EXECUTE FUNCTION fn_prt_preliquida_trasp_ced(?,?)"
   PREPARE prp_ejecuta_preliquidacion FROM v_consulta
   EXECUTE prp_ejecuta_preliquidacion USING p_folio,
                                            p_usuario_cod
                                       INTO r_sql_error,
                                            r_isam_error,
                                            r_msg_error,
                                            r_registros_procesados,
                                            r_total_aivs,
                                            r_total_pesos

   WHENEVER ERROR STOP
   LET v_fecha_actual = TODAY
   EXECUTE prp_genera_cza_lote USING p_folio,
                                     "01", # traspaso cedente corriente
                                     v_fecha_actual,
                                     C_ESTADO_TRASPASO_PRELIQUIDADA_CED
   EXECUTE prp_genera_sum_lote USING p_folio,
                                     r_registros_procesados,
                                     r_total_aivs,
                                     r_total_pesos
   IF(r_sql_error <> 0)THEN
      LET v_error = TRUE  # Ocurrió error
      DISPLAY ""
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",r_sql_error
      DISPLAY "MENSAJE:",r_msg_error
      DISPLAY ""
   ELSE      
      DISPLAY ""
      DISPLAY "FOLIO:                   ",p_folio USING "###,###,##&"
      DISPLAY "REGISTROS PRELIQUIDADOS: ",r_registros_procesados USING "###,###,##&"
      DISPLAY "MONTO TOTAL AIVS:        ",r_total_aivs  USING "###,###,##&.&&"
      DISPLAY "MONTO TOTAL PESOS:       ",r_total_pesos USING "###,###,##&.&&"
      DISPLAY ""

      # Genera archivo de traspasos a fovissste
      CALL fn_genera_archivo_traspasos()
   END IF
   RETURN v_error
END FUNCTION

# Descripción: Genera archivo de saldos para traspaso a fovissste
FUNCTION fn_genera_archivo_traspasos()
DEFINE v_consulta   STRING,
       v_canal      base.Channel,
       v_archivo    STRING,
       v_archivo_axway STRING,
       v_ruta_envio LIKE seg_modulo.ruta_envio,
       v_fecha      DATE,
       v_fecha_tmp3 DATE,
       v_encabezado RECORD
          v_registro           CHAR(2),
          v_fecha_presentacion CHAR(8)
       END RECORD,

       v_detalle RECORD
          v_registro                        CHAR(2),
          v_instituto_origen                CHAR(3),
          v_tpo_movimiento                  CHAR(2),
          v_nss                             CHAR(11),
          v_ap_paterno                      CHAR(40),
          v_ap_materno                      CHAR(40),
          v_nombre                          CHAR(40),
          v_curp                            CHAR(18),
          v_tpo_operacion                   CHAR(2),
          v_id_credito_fovissste            DECIMAL(10),
          v_sdo_insoluto_fovissste          DECIMAL(22,2),
          v_f_originacion_fovissste         CHAR(8),
          v_mto_aivs_infonavit97_cedido     DECIMAL(22,2),
          v_mto_aivs_infonavit92_cedido     DECIMAL(22,2),
          v_mto_pesos_infonavit97_cedido    DECIMAL(22,2),
          v_mto_pesos_infonavit92_cedido    DECIMAL(22,2),
          v_f_valor_transferncia            CHAR(8)
       END RECORD,

       v_sumario RECORD
          v_registro           CHAR(2),
          v_total_registros    CHAR(10),
          v_monto_pesos        DECIMAL(22,2)
       END RECORD,
       v_txt_folio          STRING,
       v_txt_credito        STRING,
       v_txt_sdo_insoluto   STRING,
       v_txt_monto          STRING,
       v_comando            CHAR(200)

   LET v_consulta = " SELECT fecha_presentacion",
                    "   FROM prt_cza_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_cza_traspaso FROM v_consulta

   LET v_consulta = " SELECT instituto_origen,",
                    "        tpo_movimiento,",
                    "        nss,",
                    "        ap_paterno,",
                    "        ap_materno,",
                    "        nombre,",
                    "        curp,",
                    "        tpo_operacion,",
                    "        id_credito_fovissste,",
                    "        sdo_insoluto_fovissste,",
                    "        f_originacion_fovissste,",
                    "        mto_pesos_infonavit97_cedido ,",
                    "        mto_aivs_infonavit97_cedido ,",
                    "        mto_pesos_infonavit92_cedido , ",
                    "        mto_aivs_infonavit92_cedido ,",
                    "        f_valor_transferencia",
                    "   FROM prt_traspaso_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_det_traspaso FROM v_consulta
   DECLARE cur_recupera_det_traspaso CURSOR FOR prp_recupera_det_traspaso

   LET v_consulta = " SELECT total_registros,",
                    "        mto_pesos_viv97",
                    "   FROM prt_sum_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_sum_traspaso FROM v_consulta

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "prt"

   LET v_hoy       = TODAY USING"YYYYMMDD"
   
   LET v_txt_folio = p_folio USING "&&&&&&&&&"
   LET v_archivo       = v_ruta_envio CLIPPED,"/",v_hoy,v_txt_folio,".tsc"
   LET v_archivo_axway = "/safreviv_int/prt/transfer/PORTA_TSSV_INFONAVIT_",v_hoy,".txt"
   
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile( v_archivo, "w" )

   # Encabezado
   EXECUTE prp_recupera_cza_traspaso USING p_folio
                                      INTO v_fecha
                                           
   LET v_encabezado.v_registro           = C_REGISTRO_01
   LET v_encabezado.v_fecha_presentacion = v_fecha USING "yyyymmdd"

   CALL v_canal.writeLine(v_encabezado.v_registro||
                          v_encabezado.v_fecha_presentacion)
   # Detalle
   FOREACH cur_recupera_det_traspaso USING p_folio
                                      INTO v_detalle.v_instituto_origen,
                                           v_detalle.v_tpo_movimiento,
                                           v_detalle.v_nss,
                                           v_detalle.v_ap_paterno,
                                           v_detalle.v_ap_materno,
                                           v_detalle.v_nombre,
                                           v_detalle.v_curp,
                                           v_detalle.v_tpo_operacion,
                                           v_detalle.v_id_credito_fovissste,
                                           v_detalle.v_sdo_insoluto_fovissste,
                                           v_fecha,
                                           v_detalle.v_mto_pesos_infonavit97_cedido,
                                           v_detalle.v_mto_aivs_infonavit97_cedido,
                                           v_detalle.v_mto_pesos_infonavit92_cedido,
                                           v_detalle.v_mto_aivs_infonavit92_cedido,
                                           v_fecha_tmp3

      IF v_detalle.v_mto_aivs_infonavit97_cedido IS NULL OR v_detalle.v_mto_aivs_infonavit97_cedido = "" THEN
         LET v_detalle.v_mto_aivs_infonavit97_cedido  = 0;
         LET v_detalle.v_mto_pesos_infonavit97_cedido = 0;
      END IF

      IF v_detalle.v_mto_aivs_infonavit92_cedido IS NULL OR v_detalle.v_mto_aivs_infonavit92_cedido = "" THEN
         LET v_detalle.v_mto_aivs_infonavit92_cedido  = 0;
         LET v_detalle.v_mto_pesos_infonavit92_cedido = 0;
      END IF

      LET v_detalle.v_mto_pesos_infonavit97_cedido = v_detalle.v_mto_pesos_infonavit97_cedido + v_detalle.v_mto_pesos_infonavit92_cedido

      IF v_detalle.v_ap_materno IS NULL THEN
         LET v_detalle.v_ap_materno = "                                        "
      END IF

      LET v_detalle.v_registro                = C_REGISTRO_02
      LET v_txt_credito                       = v_detalle.v_id_credito_fovissste USING "&&&&&&&&&&"
      LET v_detalle.v_f_originacion_fovissste = v_fecha      USING "yyyymmdd"
      LET v_detalle.v_f_valor_transferncia    = v_fecha_tmp3 USING "yyyymmdd"
      LET v_txt_sdo_insoluto                  = v_detalle.v_sdo_insoluto_fovissste * 100 USING "&&&&&&&&&&&&"
      LET v_txt_monto                         = v_detalle.v_mto_pesos_infonavit97_cedido * 100 USING "&&&&&&&&&&&&"

      CALL v_canal.writeLine(v_detalle.v_registro||
                             v_detalle.v_instituto_origen||
                             v_detalle.v_tpo_movimiento||
                             v_detalle.v_nss||
                             v_detalle.v_ap_paterno||
                             v_detalle.v_ap_materno||
                             v_detalle.v_nombre||
                             v_detalle.v_curp||
                             v_detalle.v_tpo_operacion||
                             v_txt_credito||
                             v_txt_sdo_insoluto||
                             v_detalle.v_f_originacion_fovissste||
                             v_txt_monto||
                             v_detalle.v_f_valor_transferncia)
      
   END FOREACH
   FREE cur_recupera_det_traspaso
   
   # Sumario
   EXECUTE prp_recupera_sum_traspaso USING p_folio
                                      INTO v_sumario.v_total_registros,
                                           v_sumario.v_monto_pesos
                                           
   LET v_sumario.v_registro        = C_REGISTRO_09
   LET v_txt_monto                 = v_sumario.v_total_registros USING "&&&&&&&&&&"
   LET v_sumario.v_total_registros = v_txt_monto
   LET v_txt_monto                 = v_sumario.v_monto_pesos * 100 USING "&&&&&&&&&&&&"
   
   CALL v_canal.writeLine(v_sumario.v_registro||
                          v_sumario.v_total_registros||
                          v_txt_monto)

   CALL v_canal.close()

   DISPLAY "ARCHIVO GENERADO EN RUTA: ", v_archivo CLIPPED
   LET v_comando = "cp ",v_archivo CLIPPED," ",v_archivo_axway CLIPPED   
   RUN v_comando   
   
   DISPLAY "COPIA   GENERADA EN RUTA: ", v_archivo_axway CLIPPED
   
END FUNCTION
