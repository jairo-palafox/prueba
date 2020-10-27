--==============================================================================
-- Version: 1.0.0
-- Fecha última modificación: 25 Junio 2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTE22                                                   #
#Descripcion       => Batch de preliquidación de devolucion de saldos receptora#
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Junio 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"

DEFINE p_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod,  # Código de operacion
       p_folio       LIKE glo_folio.folio,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo
   
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

   SELECT COUNT(*)
     INTO v_total_registros
     FROM prt_traspaso_receptora
    WHERE folio_liquida = p_folio
     
   DISPLAY "\n"
   DISPLAY "PROCESO:   ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_operacion_desc
   DISPLAY "FOLIO:     ",p_folio
   DISPLAY "TOTAL DE REGISTROS A PRELIQUIDAR:",v_total_registros USING "##,###,##&"
   DISPLAY "\n"
   
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_ejecuta_preliquidacion(p_folio,p_usuario_cod) RETURNING r_error

   IF NOT( r_error )THEN
         
      LET p_mensaje = "Preliquidación realizada con éxito"
      
      CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, r_ruta
      --CALL fn_rutas("bat") RETURNING r_ruta, r_ruta_lst
      
      LET v_comando = "fglrun ",r_ruta_ejecutable CLIPPED,"/PRTI03.42r ",p_usuario_cod, " ",
                                                                         p_pid, " ",
                                                                         p_proceso_cod," ",
                                                                         p_opera_cod," ",
                                                                         p_folio," ",
                                                                         "PRTE22"
                      {"' 1>>", r_ruta_lst CLIPPED,
                      "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                      " 2>&1 &"}
      RUN v_comando
      IF( STATUS )THEN
         DISPLAY "Ocurrió un error al ejecutar el reporte de la preliquidación"
      ELSE
         --DISPLAY "Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos"
      END IF
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_reultado_opera
      IF(r_reultado_opera <> 0)THEN
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
   LET p_titulo = "Finalización de operación - Preliquidación de devolución cedente"
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          NULL, 
                          p_titulo,
                          p_mensaje)
END MAIN

# Descripción: Ejecucion de SP de preliquidación de devolución cedente
FUNCTION fn_ejecuta_preliquidacion(p_folio,p_usuario)
DEFINE p_folio      LIKE prt_cza_cedente.folio_liquida,
       p_usuario    LIKE seg_usuario.usuario_cod,
       v_consulta   STRING,
       v_sql_error  INTEGER,
       v_error_isam INTEGER,
       v_msn_error  VARCHAR(40),
       v_error      BOOLEAN,
       r_registros_preliquidados INTEGER,
       r_total_pesos            DECIMAL(22,2),
       r_total_aivs             DECIMAL(22,2)

   WHENEVER ERROR CONTINUE
   # inicializacion de flujo correcto
   LET v_error = FALSE 
   
   LET v_consulta = "EXECUTE FUNCTION fn_prt_preliquida_dev_cedente(?,?)"
   PREPARE prp_ejecuta_preliquidacion FROM v_consulta
   EXECUTE prp_ejecuta_preliquidacion USING p_folio,
                                            p_usuario_cod
                                       INTO v_sql_error,
                                            v_error_isam,
                                            v_msn_error,
                                            r_registros_preliquidados,
                                            r_total_aivs,
                                            r_total_pesos
   
   IF(v_sql_error <> 0)THEN
      LET v_error = TRUE  # Ocurrió error
      DISPLAY ""
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_sql_error
      DISPLAY "MENSAJE:",v_msn_error
      DISPLAY ""
   ELSE
      DISPLAY ""
      DISPLAY "FOLIO:                    ",p_folio USING "###,###,##&"
      DISPLAY "REGISTROS PRELIQUIDADOS:  ",r_registros_preliquidados USING "###,###,##&"
      DISPLAY "MONTO TOTAL AIVS:         ",r_total_aivs           USING "###,###,##&.&&"
      DISPLAY "MONTO TOTAL PESOS:        ",r_total_pesos          USING "###,###,##&.&&"
      DISPLAY ""
      # Genera archivo de traspasos a fovissste
      CALL fn_genera_archivo_devolucion()
   END IF
   RETURN v_error
END FUNCTION

# Descripción: Genera archivo de saldos para devolución a fovissste
FUNCTION fn_genera_archivo_devolucion()
DEFINE v_consulta   STRING,
       v_canal      base.Channel,
       v_archivo    STRING,
       v_ruta_envio LIKE seg_modulo.ruta_envio,
       v_fecha      DATE,
       v_fecha_tmp3 DATE,
       v_encabezado RECORD
          v_registro           CHAR(2),
          v_fecha_presentacion CHAR(8)
       END RECORD,
       v_detalle RECORD
          v_registro           CHAR(2),
          v_instituto_origen   CHAR(3),
          v_tpo_movimiento     CHAR(2),
          v_nss                CHAR(11),
          v_ap_paterno         CHAR(40),
          v_ap_materno         CHAR(40),
          v_nombre             CHAR(40),
          v_curp               CHAR(18),
          v_tpo_operacion      CHAR(2),
          v_id_credito_fovissste    DECIMAL(10),
          v_sdo_insoluto_fovissste  DECIMAL(22,2),
          v_f_originacion_fovissste CHAR(8),
          v_mto_pesos_infonavit97_cedido    DECIMAL(22,2),
          v_f_valor_transferncia            CHAR(8)
       END RECORD,
       v_sumario RECORD
          v_registro           CHAR(2),
          v_total_registros    CHAR(10),
          v_monto_pesos        DECIMAL(22,2)
       END RECORD,
       v_txt_folio        STRING,
       v_txt_credito      STRING,
       v_txt_sdo_insoluto STRING,
       v_txt_monto        STRING,
       v_extension        VARCHAR(20)

   
   LET v_consulta = " SELECT fecha_presentacion",
                    "   FROM prt_cza_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_cza_devolucion FROM v_consulta

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
                    "        mto_pesos_infonavit97_cedido,",
                    "        f_valor_transferencia",
                    "   FROM prt_traspaso_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_det_devolucion FROM v_consulta
   DECLARE cur_recupera_det_devolucion CURSOR FOR prp_recupera_det_devolucion

   LET v_consulta = " SELECT total_registros,",
                    "        mto_pesos_viv97",
                    "   FROM prt_sum_cedente",
                    "  WHERE folio_liquida = ?"
   PREPARE prp_recupera_sum_devolucion FROM v_consulta

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "prt"

   SELECT proceso_ext
     INTO v_extension
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   LET v_txt_folio = p_folio USING "&&&&&&&&&"
   LET v_archivo   = TODAY USING "yyyymmdd"
   LET v_archivo   = v_ruta_envio CLIPPED||"/"||v_archivo||v_txt_folio||"."||v_extension CLIPPED
   DISPLAY "ARCHIVO DE DEVOLUCIÓN GENERADO EN:"
   DISPLAY v_archivo    
   DISPLAY""

   LET v_canal = base.Channel.create()
   CALL v_canal.openFile( v_archivo, "w" )

   # Encabezado
   EXECUTE prp_recupera_cza_devolucion USING p_folio
                                        INTO v_fecha
                                           
   LET v_encabezado.v_registro           = C_REGISTRO_01
   LET v_encabezado.v_fecha_presentacion = v_fecha USING "yyyymmdd"

   CALL v_canal.writeLine(v_encabezado.v_registro||
                          v_encabezado.v_fecha_presentacion)
   # Detalle
   FOREACH cur_recupera_det_devolucion USING p_folio
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
                                             v_fecha_tmp3

      LET v_detalle.v_registro                = C_REGISTRO_02
      LET v_txt_credito                       = v_detalle.v_id_credito_fovissste USING "&&&&&&&&&&"
      LET v_detalle.v_f_originacion_fovissste = v_fecha      USING "yyyymmdd"
      LET v_detalle.v_f_valor_transferncia    = v_fecha_tmp3 USING "yyyymmdd"
      LET v_txt_sdo_insoluto                  = v_detalle.v_sdo_insoluto_fovissste USING "&&&&&&&&&&&&"
      LET v_txt_monto                         = v_detalle.v_mto_pesos_infonavit97_cedido USING "&&&&&&&&&&&&"
      
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

   FREE cur_recupera_det_devolucion
   
   # Sumario
   EXECUTE prp_recupera_sum_devolucion USING p_folio
                                        INTO v_sumario.v_total_registros,
                                             v_sumario.v_monto_pesos
                                           
   LET v_sumario.v_registro        = C_REGISTRO_09
   LET v_txt_monto                 = v_sumario.v_total_registros USING "&&&&&&&&&&"
   LET v_sumario.v_total_registros = v_txt_monto
   LET v_txt_monto                 = v_sumario.v_monto_pesos USING "&&&&&&&&&&&&"
   
   CALL v_canal.writeLine(v_sumario.v_registro||
                          v_sumario.v_total_registros||
                          v_txt_monto)

   CALL v_canal.close()
   
END FUNCTION