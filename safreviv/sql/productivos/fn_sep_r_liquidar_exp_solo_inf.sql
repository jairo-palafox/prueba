






CREATE FUNCTION "safreviv".fn_sep_r_liquidar_exp_solo_inf( p_folio             DECIMAL(9,0), -- folio a reversar
                                                p_pid               INTEGER,
                                                p_proceso_cod       SMALLINT,
                                                p_opera_cod         SMALLINT,
                                                p_senial_expediente SMALLINT,
                                                p_usuario           CHAR(20)) -- del que se va a reversar
                       
RETURNING SMALLINT    , -- v_ind 
          CHAR(003)   , -- v_diag 
          INTEGER     , -- sql_error
          INTEGER     , -- isam error
          VARCHAR(250), -- mensaje de error
          INTEGER     ; -- total reverdados

-- variables de control
DEFINE v_ind                SMALLINT   ; 
DEFINE v_diag               CHAR(003)  ;
DEFINE v_estado_destino     SMALLINT;
DEFINE v_total_reversados   INTEGER    ;

-- Control de Excepciones
DEFINE v_error_sql          INTEGER;
DEFINE v_isam_error         INTEGER;
DEFINE v_msg_error          VARCHAR(255);
DEFINE v_c_msj              VARCHAR(255);
DEFINE v_si_resultado       SMALLINT;

-- variables de proceso
DEFINE v_id_expediente      DECIMAL(9,0) ;
DEFINE v_id_derechohabiente DECIMAL(9,0);


   -- se establece el comportamiento ante una aparcicion de error
   ON EXCEPTION SET v_error_sql, v_isam_error, v_msg_error

      -- se indica que hubo error
      LET v_ind              = -1 ;
      LET v_diag             = "0";
      LET v_total_reversados = 0  ;

      RETURN v_ind,
             v_diag,
             v_error_sql,
             v_isam_error,
             v_msg_error,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
   -- SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_r_liquidar.trace';
   
   LET v_ind              = 0;
   LET v_diag             = "000";
   LET v_error_sql        = 0;
   LET v_total_reversados = 0;
   LET v_isam_error       = 0;
   LET v_msg_error        = "Reverso de la liquidación de expediente sólo INFONAVIT exitosa";
   
   -- se elimina la liquidacion con funcion general de reverso de liquidacion
   EXECUTE FUNCTION fn_reverso_liquidacion(p_folio ) INTO v_error_sql ; 
   
   IF( v_error_sql <> 0 )THEN 
   
      LET v_ind  = -1    ;   
      LET v_diag = "001" ; -- error en funcion general de liquidacion
      LET v_msg_error = "Error en función general de liquidación";
   
      RETURN v_ind,
             v_diag,
             v_error_sql,
             v_isam_error,
             v_msg_error,
             v_total_reversados;
   END IF
   
   -- se elimina el historial de liquidación
   -- FOREACH SELECT UNIQUE id_referencia
   --           INTO v_id_expediente
   --           FROM sep_preliquida_solo_infonavit
   --          WHERE folio_liquida = p_folio 
   
   FOREACH SELECT UNIQUE mov.id_expediente,
                  mov.id_derechohabiente_invadido
             INTO v_id_expediente,
                  v_id_derechohabiente
             FROM sep_preliquida_solo_infonavit pre JOIN sep_movimiento_invadido mov
               ON mov.id_expediente = pre.id_referencia
            WHERE pre.folio_liquida = p_folio
   
           -- las relacionadas al registro de op 28 
           -- con valor actual = 25 (liquidado)
           DELETE 
             FROM sep_his_expediente
            WHERE id_expediente = v_id_expediente 
              AND valor_actual  = 85 ;  -- estado liquidado 
              
           EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_expediente",
                                                      v_id_expediente,
                                                      "id_expediente",
                                                      p_senial_expediente,
                                                      p_usuario) INTO v_ind, v_diag, v_estado_destino;
                                                      
           EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                                 280,            -- marca entra
                                                 v_id_expediente, -- n_referencia
                                                 p_folio)      ; -- folio
              
           LET v_total_reversados = v_total_reversados + 1;
   END FOREACH;
   
   -- se elimina la liquidacion con funcion general de reverso de liquidacion
   EXECUTE FUNCTION fn_reverso_liquidacion (p_folio ) INTO v_error_sql ; 
   
   IF( v_error_sql <> 0 )THEN    
      LET v_ind  = -1    ;   
      LET v_diag = "001" ; -- error en funcion general de liquidacion
      LET v_msg_error = "Error al eliminar movimientos";
   
      RETURN v_ind        , 
             v_diag       ,
             v_error_sql  ,
             v_isam_error     ,
             v_msg_error      ,
             v_total_reversados ;
   END IF
   
   -- se ejecuta reverso de proceso para liquidación de expedientes solo infonavit
   EXECUTE FUNCTION fn_reversa_operacion (p_pid, p_proceso_cod, p_opera_cod) INTO v_ind;
   
   IF( v_ind <> 0 )THEN
      LET v_diag = "002" ;  -- error al actualizar proceso bat        
      LET v_msg_error = "Error al actualizar proceso";
        
      RETURN v_ind        , 
             v_diag       ,
             v_error_sql  ,
             v_isam_error     ,
             v_msg_error      ,
             v_total_reversados ;
   END IF
   
   -- se actualiza el folio a estatus de preliquidado
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_ind        , 
          v_diag       ,
          v_error_sql  ,
          v_isam_error     ,
          v_msg_error      ,
          v_total_reversados ;

END FUNCTION;


