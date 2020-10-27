






CREATE FUNCTION "safreviv".fn_mdt_r_liquidar_pagos(p_folio       DECIMAL(9,0), -- folio a reversar
                                        p_pid         DECIMAL(9,0), -- del que se va a reversar
                                        p_proceso_cod SMALLINT,
                                        p_opera_cod   SMALLINT,
                                        p_usuario     CHAR(20))
RETURNING SMALLINT,    -- v_ind 
          CHAR(3),     -- v_diag 
          INTEGER,     -- sql_error
          INTEGER,     -- isam error
          VARCHAR(250),-- mensaje de error
          INTEGER;     -- total reverdados

-- variables de control
DEFINE v_cons_ind           SMALLINT;
DEFINE v_cons_id_expediente DECIMAL(9,0);
DEFINE v_cons_sql_error     INTEGER;

-- variables de proceso
DEFINE v_id_det_aplica_pago_mandato DECIMAL(9,0);
DEFINE v_estado_destino SMALLINT;   -- estado destino correspondiente a la señal y estado origen

-- Control de Excepciones
DEFINE v_ind              SMALLINT   ; 
DEFINE v_diag             CHAR(003)  ;
DEFINE v_sql_error        INTEGER;
DEFINE v_isam_error       SMALLINT;
DEFINE v_msg_error        CHAR(80);
DEFINE v_total_reversados INTEGER    ;

   -- se establece el comportamiento ante una aparcicion de error
   ON EXCEPTION SET v_sql_error, 
                    v_isam_error, 
                    v_msg_error

      -- se indica que es error de systema no de proceso
      LET v_ind              = 0;
      LET v_diag             = "0";
      LET v_total_reversados = 0;

      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_total_reversados ;

   END EXCEPTION;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_r_liquidar_pagos.trace';

   LET v_ind              = 0;
   LET v_diag             = "000";
   LET v_sql_error        = 0;
   LET v_isam_error       = 0;
   LET v_msg_error        = "";
   LET v_total_reversados = 0;
   
   -- se elimina la liquidacion con funcion general de reverso de liquidacion
   EXECUTE FUNCTION fn_reverso_liquidacion (p_folio ) INTO v_sql_error ; 
   
   IF( v_sql_error <> 0 )THEN
      LET v_ind       = 1;   
      LET v_diag      = "001"; -- error en funcion general de liquidacion
      LET v_msg_error = "Ocurrió error en funcion general de reverso de los movimientos de liquidación";
   
      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;
   END IF
   
   -- Actualiza estado de registros a preliquidado por medio de la maquinaria
   FOREACH SELECT id_det_aplica_pago_mandato
             INTO v_id_det_aplica_pago_mandato
             FROM mdt_ctr_aplica_pago_mandato ctr JOIN mdt_det_aplica_pago_mandato det
               ON det.id_ctr_aplica_pago_mandato = ctr.id_ctr_aplica_pago_mandato
            WHERE ctr.folio_pago_mandato = p_folio
            
       -- Avanza maquinaria
     EXECUTE FUNCTION fn_glo_maq_individual(1, -- Maquinaria maq_mdt_AplInstMdt
                                            v_id_det_aplica_pago_mandato,
                                            20, -- Liquidar pago mandato
                                            p_usuario) 
        INTO v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   
     IF(v_ind <> 0 OR v_sql_error <> 0)THEN
        RETURN v_ind, 
               v_diag,
               v_sql_error,
               v_isam_error,
               v_msg_error,
               v_total_reversados ;
     
     END IF;
     LET v_total_reversados = v_total_reversados + 1;
            
   END FOREACH;

   -- se ejecuta reverso de proceso bat para liquidacion de pago de mandatos
   EXECUTE FUNCTION fn_reversa_operacion (p_pid, 
                                          p_proceso_cod, 
                                          p_opera_cod) INTO v_ind;
   
   IF( v_ind <> 0 )THEN
      LET v_ind       = 1;
      LET v_diag      = "002"; -- error al actualizar proceso bat
      LET v_msg_error = "Ocurrió error en funcion general de reverso de operación de liquidación";
      RETURN v_ind,
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_total_reversados;
   END IF
   
   -- se actualiza el folio a estatus de preliquidado
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_ind,
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error,
          v_total_reversados;

END FUNCTION;


