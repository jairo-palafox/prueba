






CREATE FUNCTION "safreviv".fn_mdt_liquida_pago_instruccion(p_folio   DECIMAL(9,0),
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

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_liquida_pago_instruccion.trace';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = '';
   LET v_id_det_aplica_pago_mandato = 0;
   
   
   FOREACH SELECT id_det_aplica_pago_mandato
             INTO v_id_det_aplica_pago_mandato
             FROM mdt_ctr_aplica_pago_mandato ctr JOIN mdt_det_aplica_pago_mandato det
               ON det.id_ctr_aplica_pago_mandato = ctr.id_ctr_aplica_pago_mandato
            WHERE ctr.folio_pago_mandato = p_folio
            
      -- Avanza maquinaria
     EXECUTE FUNCTION fn_glo_maq_individual(1, -- Maquinaria maq_mdt_AplInstMdt
                                            v_id_det_aplica_pago_mandato,
                                            15, -- Liquidar pago mandato
                                            p_usuario) 
        INTO v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   
     IF(v_ind <> 0 OR v_sql_error <> 0)THEN
        RETURN v_sql_error, 
               v_isam_error, 
               v_msg_error,
               v_id_det_aplica_pago_mandato;
     
     END IF;
            
   END FOREACH
   
   LET v_id_det_aplica_pago_mandato = 0;
   RETURN v_sql_error, 
          v_isam_error, 
          v_msg_error,
          v_id_det_aplica_pago_mandato;

END FUNCTION;


