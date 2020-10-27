






CREATE FUNCTION "safreviv".fn_dae_integra_det(p_usuario_cod    CHAR(20), 
                                   p_pid            DECIMAL(9,0),
                                   p_nombre_archivo CHAR(40),
                                   p_folio          DECIMAL(9,0),
                                   p_proceso_cod    SMALLINT,
                                   p_monto_valida   DECIMAL(16,6)) 

   RETURNING INTEGER,
             INTEGER,
             CHAR(200),
             SMALLINT, 
             INTEGER, 
             INTEGER,
             INTEGER,
             INTEGER,
             INTEGER

   --Variables de tabla temporal
   DEFINE v_tmp_tipo_registro      CHAR(02);
   DEFINE v_tmp_num_credito        CHAR(10);
   DEFINE v_tmp_fecha_pago         CHAR(8) ;
   DEFINE v_tmp_periodo_pago       CHAR(4) ;
   DEFINE v_tmp_registro_pago      CHAR(8) ;
   DEFINE v_tmp_origen             CHAR(1) ;
   DEFINE v_tmp_delegacion         CHAR(2) ;
   DEFINE v_tmp_importe_amort      DECIMAL(16,6);
   DEFINE v_tmp_credito_amort      CHAR(5);
   DEFINE v_tmp_interes_amort      DECIMAL(16,6);
   DEFINE v_tmp_importe_aport      DECIMAL(16,6);
   DEFINE v_tmp_credito_aport      CHAR(5);
   DEFINE v_tmp_interes_aport      DECIMAL(16,6);
   DEFINE v_tmp_total_importe      DECIMAL(16,6);
   DEFINE v_tmp_total_credito      CHAR(5);
   DEFINE v_tmp_total_interes      DECIMAL(16,6);
   DEFINE v_tmp_devolucion_amort   CHAR(1);
   DEFINE v_tmp_devolucion_aport   CHAR(1);
   DEFINE v_tmp_tipo_pago          CHAR(3);
   DEFINE v_tmp_nss                CHAR(11);
   DEFINE v_tmp_entidad_receptora  CHAR(3);
   DEFINE v_tot_regs_insertados    INTEGER;
   DEFINE v_precio_fondo           DECIMAL(16,6);
   DEFINE v_fondo_inversion        SMALLINT; -- fondo temporal

   --Variables de rechazo          
   DEFINE v_rch_id_rechazo         DECIMAL(9,0);
   DEFINE v_rch_folio              DECIMAL(9,0);
   DEFINE v_rch_tipo_registro      SMALLINT;
   DEFINE v_rch_id_dae_referencia  DECIMAL(9,0);
   DEFINE v_rch_resul_opera        CHAR(2);
   DEFINE v_rch_diagnostico        SMALLINT;
   DEFINE v_rch_campo_valor        CHAR(70);   

   --Variables de tabla de detalles
   DEFINE v_id_dae_referencia      DECIMAL(9,0);
   DEFINE v_id_dae_ref_rech        DECIMAL(9,0);
   DEFINE v_id_derechohabiente	   DECIMAL(9,0);
   DEFINE v_folio                  DECIMAL (9,0);
   DEFINE v_num_credito            CHAR(10);
   DEFINE v_fecha_pago             DATE;
   DEFINE v_fecha_pago_consulta    DATE;
   DEFINE v_periodo_pago           CHAR(4);
   DEFINE v_registro_pago          CHAR(8);
   DEFINE v_origen                 CHAR(1);
   DEFINE v_delegacion             CHAR(2);
   DEFINE v_importe_amort          DECIMAL(16,6);
   DEFINE v_credito_amort          CHAR(5);
   DEFINE v_interes_amort          DECIMAL(16,6);
   DEFINE v_importe_aport          DECIMAL(16,6);
   DEFINE v_credito_aport          CHAR(5);
   DEFINE v_interes_aport          DECIMAL(16,6);
   DEFINE v_total_importe          DECIMAL(16,6);
   DEFINE v_total_credito          CHAR(5);
   DEFINE v_total_interes          DECIMAL(16,6);
   DEFINE v_devolucion_amort       CHAR(1);
   DEFINE v_devolucion_aport       CHAR(1);
   DEFINE v_tipo_pago              CHAR(3);
   DEFINE v_nss                    CHAR(11);
   DEFINE v_entidad_receptora      CHAR(3);
   DEFINE v_folio_liquida          DECIMAL(9,0) ;
   DEFINE v_fecha_liquida          DATE;
   DEFINE v_estado	               SMALLINT;
   DEFINE v_resul_opera            CHAR(2);
   DEFINE v_motivo_rechazo         SMALLINT;

   DEFINE v_id_derechohabiente_afi DECIMAL(9,0);
   DEFINE v_id_derechohabiente_cre DECIMAL(9,0);
   DEFINE v_num_credito_cre        DECIMAL(10,0);

   DEFINE p_valida                 SMALLINT;
                                   
   DEFINE r_resultado              SMALLINT;
   DEFINE r_id_derechohabiente     DECIMAL(9,0);
   DEFINE r_nss                    CHAR(11);
   DEFINE r_tpo_originacion        SMALLINT;
   DEFINE r_tpo_credito            SMALLINT;
   DEFINE r_num_credito            DECIMAL(10,0);
   DEFINE r_f_otorga               DATE;
   DEFINE r_f_liquida              DATE;
   DEFINE r_valida                 SMALLINT;           
   DEFINE v_total_rechazados       INTEGER;
   DEFINE v_total_aceptados        INTEGER;
   DEFINE v_total_pendientes       INTEGER;
   DEFINE v_fecha_dd               CHAR(2);
   DEFINE v_fecha_mm               CHAR(2);
   DEFINE v_fecha_yyyy             CHAR(2);             
   DEFINE v_resultado_fecha        SMALLINT;
   DEFINE v_fecha_salida           DATE;   

   DEFINE v_estado_registro        SMALLINT;
   DEFINE v_precio_aivs            DECIMAL(16,6);
   DEFINE v_monto_aivs             DECIMAL(16,6);
   DEFINE v_id_origen              SMALLINT;
   DEFINE v_status_retiro          SMALLINT;
   DEFINE v_folio_dictamen         DECIMAL(9,0);
   DEFINE v_folio_ajuste           DECIMAL(9,0);
   DEFINE v_tot_rch_ff             INTEGER;
   DEFINE v_res_fec_reg_pag        SMALLINT;
   DEFINE v_fec_reg_pago           DATE;

   --Control de monto-archivo 
   DEFINE v_ctr_mto_folio          DECIMAL(10,0);
   DEFINE v_ctr_mto_nombre_archivo CHAR(40);
   DEFINE v_ctr_mto_monto_valida   DECIMAL(16,6);
   DEFINE v_ctr_mto_estado         SMALLINT;
   DEFINE v_ctr_mto_f_actualiza    DATE;
   DEFINE v_ctr_mto_usuario        CHAR(20);
   
   DEFINE v_folio_retiro           DECIMAL(9,0);
   DEFINE v_id_ret_solicitud       DECIMAL(9,0);
   DEFINE v_ctr_folios_ret_rest    VARCHAR(200,0);
   DEFINE v_c_importe_amort        CHAR(8);
   DEFINE v_c_total_importe        CHAR(10);
  
   -- Control de Excepciones
   DEFINE sql_err               INTEGER;
   DEFINE isam_err              INTEGER;
   DEFINE v_isam_err            INTEGER;
   DEFINE err_txt               CHAR(200);
   DEFINE v_si_correcto_integra SMALLINT;          
   DEFINE v_i_resultado         SMALLINT;

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      LET v_si_correcto_integra = 1;
      LET v_isam_err = isam_err;
      LET err_txt = v_tmp_nss;

      RETURN v_i_resultado,
             isam_err, 
             err_txt,
             v_si_correcto_integra,
             v_tot_regs_insertados,
             v_total_rechazados,
             v_total_aceptados,
             v_total_pendientes,
             v_tot_rch_ff;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/dae/envio/dae_integra.trace";
   --TRACE ON;

   --Iniciar variables 
   LET v_tmp_tipo_registro   = "";
   LET v_id_dae_referencia   = 0;   
   LET v_id_dae_ref_rech     = 0;   
   LET v_id_derechohabiente	 = NULL;
   LET v_folio               = 0;   
   LET v_num_credito         = "";  
   LET v_fecha_pago          = NULL;
   LET v_fecha_pago_consulta = NULL;
   LET v_periodo_pago        = "";
   LET v_registro_pago       = "";
   LET v_origen              = "C";  --En origen se asigna C -Cartera
   LET v_delegacion          = "";
   LET v_importe_amort       = 0; 
   LET v_credito_amort       = "";
   LET v_interes_amort       = 0; 
   LET v_importe_aport       = 0; 
   LET v_credito_aport       = "";
   LET v_interes_aport       = 0; 
   LET v_total_importe       = 0; 
   LET v_total_credito       = "";
   LET v_total_interes       = 0; 
   LET v_devolucion_amort    = "";
   LET v_devolucion_aport    = "";
   LET v_tipo_pago           = "";
   LET v_nss                 = "";
   LET v_entidad_receptora   = "";
   LET v_folio_liquida       = 0; 
   LET v_fecha_liquida       = "";
   LET v_estado	             = 0; 
   LET v_resul_opera         = "";
   LET v_motivo_rechazo      = 0; 
   LET v_tot_regs_insertados = 0;
   LET v_si_correcto_integra = 0;
   LET v_i_resultado         = 0;
   LET err_txt               = "Ok";
   LET v_isam_err            = 0;
   LET v_total_rechazados    = 0;
   LET v_total_aceptados     = 0;
   LET v_estado_registro     = 0;
   LET v_fondo_inversion     = 11;
   LET v_precio_fondo        = 0;
   LET v_id_origen           = 1 ; --Cartera
   LET v_status_retiro       = "";          
   LET v_folio_dictamen      = NULL;
   LET v_folio_ajuste        = NULL;   
   LET v_precio_aivs         = 0;
   LET v_monto_aivs          = 0;
   LET v_total_pendientes    = 0;
   LET v_id_derechohabiente_afi = NULL;
   LET v_folio_retiro        = NULL; 
   LET v_id_ret_solicitud    = NULL;
   LET v_ctr_folios_ret_rest = "";
   LET v_tot_rch_ff          = 0;
   LET v_res_fec_reg_pag     = 0;
   LET v_fec_reg_pago        = "";
   LET v_tmp_importe_amort   = 0;
   LET v_tmp_total_importe   = 0;

   FOREACH
      SELECT tipo_registro,
             num_credito,
             fecha_pago,
             periodo_pago,
             registro_pago,
             origen,
             delegacion,
             importe_amort,
             total_importe,
             tipo_pago,
             nss,
             entidad_receptora
      INTO   v_tmp_tipo_registro,
             v_tmp_num_credito,
             v_tmp_fecha_pago,
             v_tmp_periodo_pago,
             v_tmp_registro_pago,
             v_tmp_origen,
             v_tmp_delegacion,
             v_c_importe_amort,
             v_c_total_importe,
             v_tmp_tipo_pago,
             v_tmp_nss,
             v_tmp_entidad_receptora
      FROM   safre_tmp:tmp_det_dev_amort_excedentes

      -- Cambia formato de fecha de YYYYMMDD a MMDDYYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha (p_proceso_cod,v_tmp_fecha_pago)
              INTO  v_resultado_fecha,
                    v_fecha_pago;


      IF v_fecha_pago < "07/01/1997" THEN
         LET v_fecha_pago_consulta = "07/01/1997";
      ELSE
         LET v_fecha_pago_consulta = v_fecha_pago;
      END IF

      LET v_tmp_importe_amort = v_c_importe_amort ;
      LET v_tmp_total_importe = v_c_total_importe ;
                    
      LET v_importe_amort = v_tmp_importe_amort / 100;
      LET v_total_importe = v_tmp_total_importe / 100;

      --Valida fecha correcta
      IF v_resultado_fecha = 0 AND v_fecha_pago IS NOT NULL THEN
         EXECUTE PROCEDURE sp_cambia_formato_fecha (p_proceso_cod,v_tmp_registro_pago)
                 INTO  v_res_fec_reg_pag,
                       v_fec_reg_pago;   

         IF v_res_fec_reg_pag = 0 AND v_fec_reg_pago IS NOT NULL THEN     
            IF v_tmp_nss = "00000000000" THEN
                  LET v_estado_registro       = 2;
                  LET v_rch_folio             = p_folio;
                  LET v_rch_tipo_registro     = 1;
                  LET v_rch_id_dae_referencia = p_folio;
                  LET v_rch_resul_opera       = "02";
                  LET v_rch_diagnostico       = 2;
                  LET v_rch_campo_valor       = "NSS INVALIDO 00000000000";
                         
                  LET v_id_derechohabiente	= "";
                  LET v_folio               = p_folio;
                  LET v_folio_liquida       = NULL;
                  LET v_fecha_liquida       = NULL;
                  LET v_estado	            = 2;
                  LET v_resul_opera         = "02";
                  LET v_motivo_rechazo      = 1;
                  
                  LET v_total_rechazados = v_total_rechazados + 1;   
            ELSE
               IF v_tmp_nss IS NULL or v_tmp_nss = "           " THEN --Se agrega validación si NSS es nulo #AG28042014
                  --SE RECHAZA         
                  LET v_estado_registro       = 2;
                  LET v_rch_folio             = p_folio;
                  LET v_rch_tipo_registro     = 1;
                  LET v_rch_id_dae_referencia = p_folio;
                  LET v_rch_resul_opera       = "02";
                  LET v_rch_diagnostico       = 2;
                  LET v_rch_campo_valor       = "SIN NSS O NSS NULO";
                         
                  LET v_id_derechohabiente	= "";
                  LET v_folio               = p_folio;
                  LET v_folio_liquida       = NULL;
                  LET v_fecha_liquida       = NULL;
                  LET v_estado	             = 2;
                  LET v_resul_opera         = "02";
                  LET v_motivo_rechazo      = 1;
                  
                  LET v_total_rechazados = v_total_rechazados + 1;   
               ELSE
                  --Obtiene el id_derechohabiente de SAFRE            
                  SELECT id_derechohabiente 
                  INTO   v_id_derechohabiente_afi
                  FROM   afi_derechohabiente 
                  WHERE  nss = v_tmp_nss;

                  --Si el Derechohabiente no existe se inserta RECHAZO;
                  IF v_id_derechohabiente_afi IS NULL THEN
                     LET v_estado_registro       = 2;
                     LET v_rch_folio             = p_folio;
                     LET v_rch_tipo_registro     = 1;
                     LET v_rch_id_dae_referencia = v_id_derechohabiente_afi;
                     LET v_rch_resul_opera       = "02";
                     LET v_rch_diagnostico       = 2;
                     LET v_rch_campo_valor       = "NO EXISTE NSS EN SACI";
                            
                     LET v_id_derechohabiente	= "";
                     LET v_folio               = p_folio;
                     LET v_folio_liquida       = NULL;
                     LET v_fecha_liquida       = NULL;
                     LET v_estado	             = 2;
                     LET v_resul_opera         = "02";
                     LET v_motivo_rechazo      = 1;
                     
                     LET v_total_rechazados = v_total_rechazados + 1;
                 ELSE
                    --Obtiene el precio de fondo en base a la fecha de pago 
                    SELECT precio_fondo
                    INTO   v_precio_aivs
                    FROM   glo_valor_fondo
                    WHERE  fondo = v_fondo_inversion
                    AND    f_valuacion = v_fecha_pago_consulta;

                    --Verifica si el monto de validacion = > al monto de devolución
                    IF v_importe_amort >= p_monto_valida  THEN                       
                       LET v_id_derechohabiente	= v_id_derechohabiente_afi;
                       LET v_folio              = p_folio;
                       LET v_folio_liquida      = NULL;
                       LET v_fecha_liquida      = NULL;
                       LET v_estado	            = 2   ;                       LET v_resul_opera        = "03";  --Pendientes 
                       LET v_motivo_rechazo     = NULL;
                       LET v_monto_aivs         = (v_importe_amort / v_precio_aivs ); --Validar fora de obtener AIVS            
                       
                       LET v_total_pendientes = v_total_pendientes + 1;
                    ELSE                       
                       LET v_id_derechohabiente	= v_id_derechohabiente_afi;
                       LET v_folio              = p_folio;
                       LET v_folio_liquida      = NULL;
                       LET v_fecha_liquida      = NULL;
                       LET v_estado	            = 2   ;
                       LET v_resul_opera        = "01";
                       LET v_motivo_rechazo     = NULL;
                       LET v_monto_aivs         = (v_importe_amort / v_precio_aivs ); --Validar fora de obtener AIVS

                       LET v_total_aceptados = v_total_aceptados + 1;
                    END IF    --IF PESOS > VALIDA 
                 END IF --IF dh IS NULL
              END IF -- SI NSS TEMPORAL IS NULL
            END IF--IF NSS = 00000000000
         ELSE
            LET v_estado_registro       = 2;
            LET v_rch_folio             = p_folio;
            LET v_rch_tipo_registro     = 1;
            LET v_rch_id_dae_referencia = v_id_derechohabiente_afi;
            LET v_rch_resul_opera       = "04";
            LET v_rch_diagnostico       = 4;
            LET v_rch_campo_valor       = v_tmp_tipo_registro
                                          ||v_tmp_num_credito
                                          ||v_tmp_fecha_pago
                                          ||v_tmp_periodo_pago
                                          ||v_tmp_registro_pago
                                          ||v_tmp_origen
                                          ||v_tmp_delegacion
                                          ||v_c_importe_amort
                                          ||v_c_total_importe
                                          ||v_tmp_tipo_pago
                                          ||v_tmp_nss
                                          ||v_tmp_entidad_receptora;
            
            LET v_folio               = p_folio;
            LET v_folio_liquida       = NULL;
            LET v_fecha_liquida       = NULL;
            LET v_estado	             = 2;
            LET v_resul_opera         = "04";
            LET v_motivo_rechazo      = 4;
            
            LET v_tot_rch_ff = v_tot_rch_ff + 1 ;  
         END IF --VALIDACION DE FECHA DE REGISTRO
      ELSE
         LET v_estado_registro       = 2;
         LET v_rch_folio             = p_folio;
         LET v_rch_tipo_registro     = 1;
         LET v_rch_id_dae_referencia = v_id_derechohabiente_afi;
         LET v_rch_resul_opera       = "04";
         LET v_rch_diagnostico       = 3;
         LET v_rch_campo_valor       = v_tmp_tipo_registro
                                       ||v_tmp_num_credito
                                       ||v_tmp_fecha_pago
                                       ||v_tmp_periodo_pago
                                       ||v_tmp_registro_pago
                                       ||v_tmp_origen
                                       ||v_tmp_delegacion
                                       ||v_c_importe_amort
                                       ||v_c_total_importe
                                       ||v_tmp_tipo_pago
                                       ||v_tmp_nss
                                       ||v_tmp_entidad_receptora;
      
         LET v_folio               = p_folio;
         LET v_folio_liquida       = NULL;
         LET v_fecha_liquida       = NULL;
         LET v_estado	            = 2;
         LET v_resul_opera         = "04";
         LET v_motivo_rechazo      = 3;
         
         LET v_tot_rch_ff = v_tot_rch_ff + 1 ;
      END IF --VALIDACIÓN DE FECHA DE PAGO
      
      INSERT INTO dae_det_solicitud 
      VALUES     (seq_dae_det_solicitud.NEXTVAL,
                  v_id_derechohabiente,
                  v_folio,
                  v_tmp_num_credito,
                  v_fecha_pago,
                  v_tmp_periodo_pago,
                  v_tmp_registro_pago,
                  v_tmp_origen,
                  v_tmp_delegacion,
                  v_importe_amort,
                  v_total_importe,
                  v_precio_aivs,
                  v_monto_aivs,
                  v_tmp_tipo_pago,    
                  v_tmp_nss,
                  v_tmp_entidad_receptora,
                  v_folio_liquida,
                  v_fecha_liquida,
                  v_estado,
                  v_resul_opera,
                  v_motivo_rechazo,
                  v_id_origen,
                  v_status_retiro,
                  v_folio_dictamen,
                  v_folio_ajuste,
                  v_folio_retiro,
                  v_id_ret_solicitud,
                  v_ctr_folios_ret_rest
                  );
      
      IF v_resul_opera = "02" 
      OR v_resul_opera = "04" THEN
         INSERT INTO dae_rch_archivo         
         VALUES      (seq_dae_rch_archivo.NEXTVAL,
                      v_rch_folio,
                      v_rch_tipo_registro,
                      seq_dae_det_solicitud.CURRVAL,
                      v_rch_resul_opera,
                      v_rch_diagnostico,
                      v_rch_campo_valor);
      END IF
      
      LET v_tot_regs_insertados = v_tot_regs_insertados + 1;
   END FOREACH    

   IF   v_i_resultado < 0 THEN 
      UPDATE bat_ctr_operacion 
      SET    folio       = p_folio, 
             nom_archivo = p_nombre_archivo
      WHERE  proceso_cod = 2400
      AND    opera_cod   = 2
      AND    pid         = p_pid;
   END IF 
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio     = p_folio, 
          estado    = 2 -- integrado
   WHERE  proceso_cod = 2400
   AND    opera_cod   = 1   -- etapa de carga
   AND    estado      = 1;  -- archivo cargado

   SELECT nom_archivo
   INTO   p_nombre_archivo
   FROM   bat_ctr_operacion
   WHERE  pid = p_pid
   AND    opera_cod = 1;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio, 
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = 2400
   AND    opera_cod   = 2
   AND    pid         = p_pid;
   
   INSERT INTO dae_ctr_archivo_montos
   VALUES(p_folio,
          p_nombre_archivo, 
          p_monto_valida,
          2, 
          TODAY,
          p_usuario_cod); 
   
   UPDATE statistics FOR TABLE dae_det_solicitud;
   UPDATE statistics FOR TABLE dae_rch_archivo;
   UPDATE statistics FOR TABLE dae_ctr_archivo_montos;

 RETURN v_i_resultado,
        v_isam_err,
        err_txt,
        v_si_correcto_integra,
        v_tot_regs_insertados, -- Totales
        v_total_rechazados,    -- Rechazadas
        v_total_aceptados,     -- Aceptados
        v_total_pendientes,    -- Pendientes
        v_tot_rch_ff;
END FUNCTION;


