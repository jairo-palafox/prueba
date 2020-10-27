






CREATE FUNCTION "safreviv".fn_sep_r_preliquidar_exp_solo_inf( p_folio       DECIMAL(9,0), -- folio a reversar
                                                   p_pid         DECIMAL(9,0),
                                                   p_proceso_cod SMALLINT,
                                                   p_opera_cod   SMALLINT,
                                                   p_senial_expediente SMALLINT,
                                                   p_usuario           CHAR(20)) -- del que se va a reversar
                       
RETURNING SMALLINT    , -- v_ind 
          CHAR(003)   , -- v_diag 
          INTEGER     , -- sql_error
          INTEGER     , -- isam error
          VARCHAR(250), -- mensaje
          INTEGER     ; -- total reverdados

-- variables de control
DEFINE v_ind              SMALLINT;
DEFINE v_diag             CHAR(003);
DEFINE v_estado_destino   SMALLINT;
DEFINE v_total_reversados INTEGER;

-- variables de proceso
DEFINE v_id_expediente    DECIMAL(9,0);

-- Control de Excepciones
DEFINE v_sql_error        INTEGER;
DEFINE v_isam_err         INTEGER;
DEFINE v_msg_err          VARCHAR(255);
DEFINE v_si_resultado     SMALLINT;

   ON EXCEPTION SET v_sql_error, v_isam_err, v_msg_err

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             v_isam_err   ,
             v_msg_err      ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_r_liquidar.trace'

   LET v_ind       = 0;
   LET v_diag      = "000";
   LET v_sql_error = 0;
   LET v_total_reversados = 0;
   LET v_isam_err = 0;
   LET v_msg_err  = "El reverso de la preliquidación de expediente sólo INFONAVIT se realizó con éxito";
   
   -- se elimina el historial de preliquidación
   FOREACH SELECT UNIQUE id_referencia
             INTO v_id_expediente
             FROM sep_preliquida_solo_infonavit
            WHERE folio_liquida = p_folio 
   
           -- las relacionadas al registro de op 28 
           -- con valor actual = 25 (liquidado)
           DELETE 
             FROM sep_his_expediente
            WHERE id_expediente = v_id_expediente 
              AND valor_actual  = 80 ;  -- estado preliquidado 
              
           EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_expediente",
                                                      v_id_expediente,
                                                      "id_expediente",
                                                      p_senial_expediente,
                                                      p_usuario) INTO v_ind, v_diag, v_estado_destino;
              
           LET v_total_reversados = v_total_reversados + 1;
   END FOREACH;
   -- se elimina la liquidacion con funcion general de reverso de liquidacion
   EXECUTE FUNCTION fn_reversa_preliquidacion(p_folio,p_proceso_cod,p_opera_cod) INTO v_sql_error ; 
   
   IF( v_sql_error <> 0 )THEN
      LET v_ind  = -1    ;   
      LET v_diag = "001" ; -- error en funcion general de rev de preliquidacion
      LET v_msg_err = "error en funcion general de rev de preliquidacion";
   
      RETURN v_ind       ,
             v_diag      ,
             v_sql_error ,
             v_isam_err    ,
             v_msg_err     ,
             v_total_reversados ;
   END IF
   
   -- se reversa el proceso bat para operacion 28 y preliquidar
   EXECUTE FUNCTION fn_reversa_operacion(p_pid ,p_proceso_cod, p_opera_cod) INTO v_ind;   
   -- si ocurre error al reversar el proceso bat   
   IF( v_ind <> 0 )THEN
      LET v_diag = "002" ; -- error al actualizar operacion
      LET v_msg_err = "error al actualizar operacion";
   
      RETURN v_ind       ,
             v_diag      ,
             v_sql_error ,
             v_isam_err  ,
             v_msg_err   ,
             v_total_reversados ;
   END IF
   
   UPDATE glo_folio
      SET status = 0
    WHERE folio = p_folio;
   
   RETURN v_ind         ,
          v_diag        ,
          v_sql_error   ,
          v_isam_err    ,
          v_msg_err     ,
          v_total_reversados ;
END FUNCTION;


