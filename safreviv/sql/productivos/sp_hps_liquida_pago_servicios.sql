






CREATE FUNCTION "safreviv".sp_hps_liquida_pago_servicios(p_folio   DECIMAL(9,0),
                                              p_usuario CHAR(20))
RETURNING INTEGER,
          SMALLINT,
          CHAR(80),
          DECIMAL(9,0);                                             

DEFINE v_id_det_aplica_pago_mandato DECIMAL(9,0);

DEFINE v_estado_destino SMALLINT;   -- estado destino correspondiente a la señal y estado origen
DEFINE v_ind        SMALLINT;   -- idicador de error
DEFINE v_diag       CHAR(3);    -- diagnostico de error

DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(80);
                                                
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_id_det_aplica_pago_mandato = 0;
      RETURN v_sql_error, 
             v_isam_error, 
             v_msg_error,
             v_id_det_aplica_pago_mandato;
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_hps_liquida_pago_instruccion.trace';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = '';
   LET v_id_det_aplica_pago_mandato = 0;
   
   
   FOREACH SELECT id_det_aplica_pago_servicio
             INTO v_id_det_aplica_pago_mandato
             FROM hps_ctr_aplica_pago_servicio ctr JOIN hps_det_aplica_pago_servicio det
               ON det.id_ctr_aplica_pago_servicio = ctr.id_ctr_aplica_pago_servicio
            WHERE ctr.folio_pago_servicio = p_folio
            
      -- Avanza maquinaria

           UPDATE hps_det_aplica_monto 
           SET    estado = 103  -- liquidado           
           WHERE  id_det_aplica_pago_servicio = v_id_det_aplica_pago_mandato;
 
   END FOREACH
   
   LET v_id_det_aplica_pago_mandato = 0;
   RETURN v_sql_error, 
          v_isam_error, 
          v_msg_error,
          v_id_det_aplica_pago_mandato;

END FUNCTION;


