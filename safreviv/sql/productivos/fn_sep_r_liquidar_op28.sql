






CREATE FUNCTION "safreviv".fn_sep_r_liquidar_op28( p_folio DECIMAL(9,0)  , -- folio a reversar
                                        p_pid   INTEGER       ) -- del que se va a reversar
                       
RETURNING SMALLINT       , -- v_ind 
          CHAR(003)      , -- v_diag 
          INTEGER        , -- sql_error
          INTEGER        , -- isam error
          VARCHAR(250)   , -- mensaje de error
          INTEGER        ; -- total reverdados

-- variables de control
DEFINE v_ind                    SMALLINT   ; 
DEFINE v_diag                   CHAR(003)  ;
DEFINE v_sql_error              INTEGER    ; 
DEFINE v_total_reversados       INTEGER    ;

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 DEFINE v_c_msj                         VARCHAR(255);
 DEFINE v_si_resultado                  SMALLINT;
 
 DEFINE v_invadido             CHAR(011) ;
 DEFINE v_asociado             CHAR(011) ;
 DEFINE v_cons_ind             SMALLINT  ;
 DEFINE v_cons_id_expediente   DECIMAL(9,0);
 DEFINE v_cons_sql_error       INTEGER   ;


-- variables de proceso
DEFINE v_id_det_02_op28         DECIMAL(9,0) ;


   -- se establece el comportamiento ante una aparcicion de error
   ON EXCEPTION SET sql_err, isam_err, err_txt

      -- se indica que hubo error
      LET v_ind               = -1     ;
      LET v_diag              = "0"    ;
      LET v_sql_error         = sql_err;
      LET v_total_reversados  = 0      ;

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
--   SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_r_liquidar.trace';

LET v_ind              =     0;
LET v_diag             = "000";
LET v_sql_error        =     0;
LET v_total_reversados = 0;
LET isam_err           = 0;
LET err_txt            = "Reverso de la liq. Operacion 28 exitosa";
LET v_invadido         = '' ;
LET v_asociado         = '' ;
LET v_cons_ind         = 0;
LET v_cons_id_expediente = 0;
LET v_cons_sql_error   = 0;

-- se elimina la liquidacion con funcion general de reverso de liquidacion
EXECUTE FUNCTION fn_reverso_liquidacion (p_folio ) INTO v_sql_error ; 

IF v_sql_error <> 0 THEN 

   LET v_ind  = -1    ;   
   LET v_diag = "001" ; -- error en funcion general de liquidacion

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_total_reversados ;
END IF

-- se elimina la historia de la liquidación en sep_his_det_02_op28

SELECT NVL(COUNT(*),0) 
INTO   v_total_reversados 
FROM   sep_det_02_op28 
WHERE  folio = p_folio ;

FOREACH SELECT a.id_det_02_op28   ,
               a.invadido     
        INTO   v_id_det_02_op28   ,
               v_invadido 
        FROM   sep_det_02_op28 a
        WHERE  a.folio = p_folio 

        -- las relacionadas al registro de op 28 
        -- con valor actual = 25 (liquidado)
        DELETE FROM sep_his_det_02_op28 
        WHERE  id_det_02_op28 = v_id_det_02_op28 
        AND    valor_actual   = 25 ;  -- estado liquidado

        SELECT a.asociado 
        INTO   v_asociado
        FROM   sep_det_03_op28 a
        WHERE  a.id_det_02_op28 = v_id_det_02_op28 ;

        EXECUTE FUNCTION sp_sep_consulta_expediente(v_invadido, v_asociado, 50) INTO v_cons_ind           ,
                                                                            v_cons_id_expediente ,
                                                                            v_cons_sql_error  ;
 
        IF v_cons_ind = 1 THEN 
           UPDATE sep_expediente 
           SET    estado = 45 
           WHERE  id_expediente = v_cons_id_expediente ;
        END IF

END FOREACH;

-- se actualiza de manera masiva el estado de los detalles

UPDATE sep_det_02_op28 
SET    estado    = 15   -- cambia a preliquidado
WHERE  folio     = p_folio 
AND    estado   in (15,25);  -- preliquidado (en caso de errores) y liquidados 


-- se ejecuta reverso de proceso bat para op 28 operacion liquidar
EXECUTE FUNCTION fn_reversa_operacion (p_pid, 2202, 4) INTO v_ind;

  IF v_ind <> 0 THEN

     LET v_diag = "002" ;  -- error al actualizar proceso bat
     RETURN v_ind        , 
            v_diag       ,
            v_sql_error  ,
            isam_err     ,
            err_txt      ,
            v_total_reversados ;
  END IF

-- se actualiza el estado del encabezado (control del folio)

UPDATE sep_cza_op28
SET    estado = 20  -- preliquidado
WHERE  folio  = p_folio ;

-- se actualiza el folio a estatus de preliquidado
UPDATE glo_folio
SET status = 1
WHERE folio = p_folio;

-- se devuelve el resultado de la ejecucion
RETURN v_ind        , 
       v_diag       ,
       v_sql_error  ,
       isam_err     ,
       err_txt      ,
       v_total_reversados ;

END FUNCTION;


