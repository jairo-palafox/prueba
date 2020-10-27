






CREATE FUNCTION "safreviv".fn_dpe_pre_integra(p_usuario_cod CHAR(20), 
                                       p_pid DECIMAL(9,0),
                                       p_folio_lote_solicitud DECIMAL(9,0)) RETURNING INTEGER, CHAR(200)
-- Variables utilizadas para el encabezado de solicitud de devoluciones
-- de aportaciones pagadas en exceso
DEFINE v_lote_solicitud_tpo_registro         DECIMAL(2,0);
DEFINE v_lote_solicitud_id_servicio          CHAR(2);
DEFINE v_lote_solicitud_id_operacion         CHAR(2);
DEFINE v_lote_solicitud_tpo_entidad_origen   CHAR(2);
DEFINE v_lote_solicitud_cve_entidad_origen   CHAR(3);
DEFINE v_lote_solicitud_tpo_entidad_destino  CHAR(2);
DEFINE v_lote_solicitud_cve_entidad_destino  CHAR(3);
DEFINE v_lote_solicitud_f_transferencia      CHAR(8);
DEFINE v_date_lote_solicitud_transferencia   DATE;
DEFINE v_lote_solicitud_si_consec_dia        SMALLINT;
DEFINE v_lote_solicitud_modalidad_recepcion  CHAR(2);
DEFINE v_lote_solicitud_resultado_operacion  CHAR(2);
DEFINE v_lote_solicitud_filler               CHAR(255);
DEFINE v_f_operacion_procesar_lote_solicitud DATE;
DEFINE v_tipo_registro_lote_solicitud        SMALLINT;
DEFINE v_resultado_operacion_lote_solicitud  CHAR(2);

-- Variables utilizadas para el encabezado de pago patronal de devoluciones
-- de aportaciones pagadas en exceso
DEFINE v_pago_patronal_tpo_registro            DECIMAL(2,0);
DEFINE v_pago_patronal_reg_patronal            CHAR(11);
DEFINE v_pago_patronal_periodo_pago            CHAR(6);
DEFINE v_pago_patronal_num_folio_sua           INTEGER;
DEFINE v_pago_patronal_num_traba_involucrados  INTEGER;
DEFINE v_pago_patronal_fec_pago                CHAR(8);
DEFINE v_date_pago_patronal_fec_pago           DATE;
DEFINE v_pago_patronal_fec_valor_vivienda      CHAR(8);
DEFINE v_date_pago_patronal_fec_valor_vivienda DATE;
DEFINE v_pago_patronal_fec_valor_rcv           CHAR(8);
DEFINE v_date_pago_patronal_fec_valor_rcv      DATE;
DEFINE v_pago_patronal_delegacion              CHAR(2);
DEFINE v_pago_patronal_subdelegacion           CHAR(2);
DEFINE v_pago_patronal_rfc_patron              CHAR(13);
DEFINE v_pago_patronal_nom_razon_social        CHAR(50);
DEFINE v_pago_patronal_num_solicitud           CHAR(13);
DEFINE v_pago_patronal_tpo_cotizacion          CHAR(1);
DEFINE v_pago_patronal_tot_dias_cotizados      DECIMAL(7);
DEFINE v_pago_patronal_cve_enti_recaudadora    CHAR(3);
DEFINE v_pago_patronal_result_operacion        CHAR(2);
DEFINE v_pago_patronal_secuencia_registro_lote DECIMAL(9);
DEFINE v_pago_patronal_filler                  CHAR(128);
DEFINE v_folio_pago_patronal                   DECIMAL(10);
DEFINE v_f_operacion_procesar_pago_patronal    DATE;
DEFINE v_tipo_registro_pago_patronal           SMALLINT;
DEFINE v_resultado_operacion_pago_patronal     CHAR(2);
DEFINE v_dpe_referencia                        DECIMAL(9,0);

-- Variable que determina si es que existe un error
DEFINE v_si_resultado SMALLINT;
DEFINE v_error_isam   SMALLINT;
DEFINE v_c_error CHAR(200);
-- Variable que almacena le valor maximo id_dpe_referencia
DEFINE v_d_id_referencia SMALLINT;

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET v_si_resultado, v_error_isam, v_c_error
      RETURN v_si_resultado, v_c_error;
   END EXCEPTION


---------- ENCABEZADO LOTE SOLICITUD DE DEVOLUCION -------------

-- Obtener datos de validacion de:
-- Encabezado lote solicitud de devolución de aportaciones
-- pagadas en exceso. Tabla temporal.

-- Variables que almacenan el codigo de error
   
   LET v_d_id_referencia = 0;
   LET v_si_resultado = 0;
   
-- Variables que almacenan informacion para su validacion
-- Encabezado lote solicitud a devolver
   LET v_lote_solicitud_tpo_registro = NULL;
   LET v_lote_solicitud_id_servicio  = NULL;
   LET v_lote_solicitud_id_operacion = NULL;
   LET v_lote_solicitud_tpo_entidad_origen = NULL;
   LET v_lote_solicitud_cve_entidad_origen = NULL;
   LET v_lote_solicitud_tpo_entidad_destino = NULL;
   LET v_lote_solicitud_cve_entidad_destino = NULL;
   LET v_lote_solicitud_f_transferencia = NULL;
   LET v_lote_solicitud_si_consec_dia = NULL;
   LET v_lote_solicitud_modalidad_recepcion =  NULL;
   LET v_lote_solicitud_resultado_operacion =  NULL;
   LET v_lote_solicitud_filler = NULL;

-- Variables que almacenan informacion para su validacion
-- Encabezado pago patronal a devolver
   LET v_pago_patronal_tpo_registro = NULL;          
   LET v_pago_patronal_reg_patronal = NULL;          
   LET v_pago_patronal_periodo_pago = NULL;          
   LET v_pago_patronal_num_folio_sua = NULL;
   LET v_pago_patronal_num_traba_involucrados = NULL;
   LET v_pago_patronal_fec_pago = NULL;              
   LET v_date_pago_patronal_fec_pago = NULL;        
   LET v_pago_patronal_fec_valor_vivienda = NULL;
   LET v_pago_patronal_fec_valor_rcv = NULL;
   LET v_pago_patronal_delegacion = NULL;
   LET v_pago_patronal_subdelegacion = NULL;
   
-- Selecciona la informacion insertada al cargar el archivo para su validacion
-- Encabezado lote de solicitud de devolucion de aportaciones pagadas en exceso

   LET v_c_error = "Selecciona el id_dpe_referencia maximo 1";
   --SELECT MAX(id_dpe_referencia) INTO v_d_id_referencia
   --  FROM dpe_cza_solicitud;
   --  -- Verifica que el id_referencia no venga nullo
   --  -- en caso de ser contrario, se asigna el valor que trae
   --     IF(v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
   --        LET v_d_id_referencia = 0;
   --     END IF
   
   --LET v_d_id_referencia = v_d_id_referencia + 1;
   SELECT seq_dpe_cza_solicitud.NEXTVAL
     INTO v_d_id_referencia
	   FROM SYSTABLES
	  WHERE tabname = "dpe_cza_solicitud";
   
   LET v_c_error = "Selecciona la tabla tmp_cza_lote_solicit_dpe";
   
   SELECT tpo_registro, 
          ide_servicio, 
          ide_operacion, 
          tpo_entidad_origen, 
          cve_entidad_origen,
          tpo_entidad_destino, 
          cve_entidad_destino, 
          fecha_transferencia, 
          consecutivo_dia,
          modalidad_recepcion, 
          resultado_operacion, 
          filler
     INTO v_lote_solicitud_tpo_registro, 
          v_lote_solicitud_id_servicio, 
          v_lote_solicitud_id_operacion, 
          v_lote_solicitud_tpo_entidad_origen, 
          v_lote_solicitud_cve_entidad_origen, 
          v_lote_solicitud_tpo_entidad_destino, 
          v_lote_solicitud_cve_entidad_destino, 
          v_lote_solicitud_f_transferencia, 
          v_lote_solicitud_si_consec_dia,
          v_lote_solicitud_modalidad_recepcion,
          v_lote_solicitud_resultado_operacion,
          v_lote_solicitud_filler              
     FROM safre_tmp:tmp_cza_lote_solicit_dpe;
   
   -- <Datos obligatoros en tipo de registro.>
   -- <    ERROR : '01'              >
   LET v_c_error = "Valida tipo de registro = 01";
   
   IF (v_lote_solicitud_tpo_registro <> '01' OR v_lote_solicitud_tpo_registro IS NULL) THEN
      -- ERROR de encabezado lote devolucion de pagos indebidos o en exceso.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,01,v_d_id_referencia,"No",60,
         "Tipo registro -"||v_lote_solicitud_tpo_registro);
      LET v_si_resultado = 1;   
   END IF
   
   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(v_lote_solicitud_f_transferencia)
      INTO v_date_lote_solicitud_transferencia;
   
   -- <Datos obligatoros en fecha de transferencia.>
   -- < ERROR : <= a la fecha que se efectua la operación  >
   LET v_c_error = "Valida solicitd de trasferencia";
   IF(v_date_lote_solicitud_transferencia > TODAY OR v_date_lote_solicitud_transferencia IS NULL)THEN
      -- ERROR de encabezado lote devolucion de pagos indebidos o en exceso.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,01,v_d_id_referencia,"No",61,
         "Fecha trasferencia -"||v_date_lote_solicitud_transferencia);
      LET v_si_resultado = 1;   
   END IF
   
      -- Inserta en la tabla dpe_cza_solicitud independiente de que exista error o no
      
      LET v_c_error = "Inserta en la tabla dpe_cza_solicitud";
      INSERT INTO dpe_cza_solicitud(folio,
                                              id_dpe_referencia,
                                              f_transferencia,
                                              num_consecutivo,
                                              modalidad_archivo)
         VALUES (p_folio_lote_solicitud,
                 v_d_id_referencia,
                 v_date_lote_solicitud_transferencia,
                 v_lote_solicitud_si_consec_dia,
                 v_lote_solicitud_modalidad_recepcion);
                 

-------------- ENCABEZADO PAGO PATRONAL DE DEVOLUCION

-- Obtener datos de validacion de:
-- Encabezado pago patronal de devolución de aportaciones
-- pagadas en exceso. Tabla temporal.

   LET v_d_id_referencia = 0;
   LET v_si_resultado = 0;
   
   LET v_c_error = "Selecciona el valor maximo id_dpe_referencia 2";
   
         --SELECT MAX(id_dpe_referencia) INTO v_d_id_referencia
         --   FROM dpe_patron;
         --                                                                                          
         ---- Verifica que el id_referencia no venga nullo
         ---- en caso de ser contrario, se asigna el valor que trae
         --IF (v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
         --   LET v_d_id_referencia = 0;
         --END IF
         

   LET v_c_error = "Selecciona la tabla tmp_cza_pago_patronal_dpe";
   FOREACH
   -- Selecciona la informacion insertada al cargar el archivo para su validacion
   -- Encabezado pago patronal de devolucion de aportaciones pagadas en exceso   	
      SELECT tpo_registro, 
             reg_patronal, 
             periodo_pago, 
             num_folio_sua,
             num_trabaja_involucrados, 
             fec_pago, 
             fec_valor_vivienda, 
             fec_valor_rcv,
             delegacion, 
             subdelegacion, 
             rfc_patron, 
             nom_razon_social, 
             num_solicitud,
             tpo_cotizacion, 
             tot_dias_cotizados, 
             cve_enti_recaudadora, 
             result_operacion,
             secuencia_registro_lote, 
             filer
        INTO v_pago_patronal_tpo_registro, 
             v_pago_patronal_reg_patronal,
             v_pago_patronal_periodo_pago, 
             v_pago_patronal_num_folio_sua,
             v_pago_patronal_num_traba_involucrados, 
             v_pago_patronal_fec_pago,
             v_pago_patronal_fec_valor_vivienda, 
             v_pago_patronal_fec_valor_rcv,
             v_pago_patronal_delegacion, 
             v_pago_patronal_subdelegacion,
             v_pago_patronal_rfc_patron, 
             v_pago_patronal_nom_razon_social,
             v_pago_patronal_num_solicitud, 
             v_pago_patronal_tpo_cotizacion,
             v_pago_patronal_tot_dias_cotizados, 
             v_pago_patronal_cve_enti_recaudadora,
             v_pago_patronal_result_operacion, 
             v_pago_patronal_secuencia_registro_lote,
             v_pago_patronal_filler
        FROM safre_tmp:tmp_cza_pago_patronal_dpe
            
      --LET v_d_id_referencia = v_d_id_referencia + 1;
      SELECT seq_dpe_patron.NEXTVAL
        INTO v_d_id_referencia
	      FROM SYSTABLES
	     WHERE tabname = "dpe_patron";
      
      -- <Datos obligatoros en tipo de registro.>
      -- <    ERROR : '02'              >
      LET v_c_error = "Valida tipo de registro 02";
      IF (v_pago_patronal_tpo_registro <> '02' OR v_pago_patronal_tpo_registro IS NULL) THEN
         -- ERROR de encabezado pago patronal devolucion de pagos indebidos o en exceso.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
            p_folio_lote_solicitud,02,v_d_id_referencia,"No",62,
            "Tipo Registro -"||v_pago_patronal_tpo_registro);
            LET v_si_resultado = 1;
      END IF
      
      -- <Datos obligatoros en periodo de pago.>
      -- <    Periodo que se presento a pagar aaaamm       >
      -- < año >= 1997 mes entre 1 y 12 y si es 1997 que el mes sea >=7
      {Solo aplica para mes par}
      LET v_c_error = "Valida periodo de pago 1";
      IF (v_pago_patronal_periodo_pago[1,4] < '1997' OR v_pago_patronal_periodo_pago IS NULL) THEN
         -- ERROR de encabezado pago patronal devolucion de pagos indebidos o en exceso.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
            p_folio_lote_solicitud,02,v_d_id_referencia,"No",63,
            "Periodo Pago -"||v_pago_patronal_periodo_pago);
         LET v_si_resultado = 1;
      END IF
      
      LET v_c_error = "Valida periodo de pago 2";
      IF (v_pago_patronal_periodo_pago[5,6] <= '0' OR v_pago_patronal_periodo_pago[5,6] > '12' OR 
          v_pago_patronal_periodo_pago IS NULL) THEN
          -- ERROR de encabezado pago patronal devolucion de pagos indebidos o en exceso.
          EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
             p_folio_lote_solicitud,02,v_d_id_referencia,"No",64,
             "Periodo Pago -"||v_pago_patronal_periodo_pago);
          LET v_si_resultado = 1;
      END IF
      
      LET v_c_error = "Valida periodo de pago 3";
      IF (v_pago_patronal_periodo_pago[1,4] = '1997') THEN
         IF v_pago_patronal_periodo_pago[5,6] < '07' OR v_pago_patronal_periodo_pago[5,6] > '12' THEN
            -- ERROR de encabezado pago patronal devolucion de pagos indebidos o en exceso.
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
                p_folio_lote_solicitud,02,v_d_id_referencia,"No",65,
                "Periodo Pago -"||v_pago_patronal_periodo_pago);
         LET v_si_resultado = 1;
         END IF
      END IF
      
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_pago_patronal_fec_pago) 
         INTO v_date_pago_patronal_fec_pago;
      
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_pago_patronal_fec_valor_vivienda) 
         INTO v_date_pago_patronal_fec_valor_vivienda;
      
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_pago_patronal_fec_valor_rcv) 
         INTO v_date_pago_patronal_fec_valor_rcv;
      
   ---- Inserta archivo con errores en tabla de rechazo del encabezado del lote
   ---- de la solocitud de pagos indebidos o en exceso.
   ---- Selecciona el maximo valor de id_referencia de la tabla de rechazo

   LET v_c_error = "Inserta en la tabla dpe_patron";

   SELECT id_dpe_referencia
   INTO   v_dpe_referencia
   FROM   dpe_patron
   WHERE  folio = p_folio_lote_solicitud
   AND    folio_sua = v_pago_patronal_num_folio_sua
   AND    reg_patronal_imss =  v_pago_patronal_reg_patronal
   AND    periodo_pago =  v_pago_patronal_periodo_pago;

   IF v_dpe_referencia IS NULL THEN
      INSERT INTO dpe_patron(folio,
                             id_dpe_referencia,
                             reg_patronal_imss,
                             rfc_patron,
                             periodo_pago,
                             folio_sua,
                             razon_social,
                             numero_solicitud,
                             tipo_cotizacion,
                             tot_dias_cotizados,
                             tot_tra_solicitud,
                             f_pago,
                             f_valor_viv,
                             f_valor_rcv,
                             clave_entidad_rec,
                             delegacion,
                             subdelegacion,
                             result_op,
                             sec_registro_lote)
         VALUES (p_folio_lote_solicitud,
                 v_d_id_referencia,
                 v_pago_patronal_reg_patronal,
                 v_pago_patronal_rfc_patron,
                 v_pago_patronal_periodo_pago,
                 v_pago_patronal_num_folio_sua,
                 v_pago_patronal_nom_razon_social,
                 v_pago_patronal_num_solicitud,
                 v_pago_patronal_tpo_cotizacion,
                 v_pago_patronal_tot_dias_cotizados,
                 v_pago_patronal_num_traba_involucrados,
                 v_date_pago_patronal_fec_pago,
                 v_date_pago_patronal_fec_valor_vivienda,
                 v_date_pago_patronal_fec_valor_rcv,
                 v_pago_patronal_cve_enti_recaudadora,
                 v_pago_patronal_delegacion,
                 v_pago_patronal_subdelegacion,
                 v_pago_patronal_result_operacion,
                 v_pago_patronal_secuencia_registro_lote);
      ELSE
         CONTINUE FOREACH;
      END IF
   END FOREACH;
   
   update statistics for table dpe_cza_solicitud;
   update statistics for table dpe_patron;
   
   IF v_si_resultado = 1 THEN
      -- Existio error en el archivo
      RETURN 1, v_c_error;
   ELSE
      -- Sin error el archivo
      LET v_c_error = "La pre integración se cargo correctamente";
      RETURN 0, v_c_error;
   END IF
   
END FUNCTION -- fn_dpe_pre_integra
;


