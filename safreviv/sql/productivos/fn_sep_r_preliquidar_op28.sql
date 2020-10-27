






CREATE FUNCTION "safreviv".fn_sep_r_preliquidar_op28( p_folio DECIMAL(9,0)  , -- folio a reversar
                                           p_pid   INTEGER       ) -- del que se va a reversar
                       
RETURNING SMALLINT       , -- v_ind 
          CHAR(003)      , -- v_diag 
          INTEGER        , -- sql_error
          INTEGER        , -- isam error
          VARCHAR(250)   , -- mensaje
          INTEGER        ; -- total reverdados

-- variables de control
DEFINE v_ind                    SMALLINT   ; 
DEFINE v_diag                   CHAR(003)  ;
DEFINE v_sql_error              INTEGER    ; 
DEFINE v_total_reversados       INTEGER    ;

-- variables de proceso
DEFINE v_id_det_02_op28         DECIMAL(9,0) ;

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 DEFINE v_c_msj                         VARCHAR(255);
 DEFINE v_si_resultado                  SMALLINT;

   ON EXCEPTION SET v_sql_error, isam_err, err_txt

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
--   SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_r_liquidar.trace'

LET v_ind       =     0    ;
LET v_diag      = "000"    ;
LET v_sql_error =     0    ;
LET v_total_reversados = 0 ;
LET isam_err = 0;
LET err_txt  = "El reverso de la preliquidación Op28 se realizó con éxito";
-- se elimina la liquidacion con funcion general de reverso de liquidacion

EXECUTE FUNCTION fn_reversa_preliquidacion(p_folio,2202,3) INTO v_sql_error ; 

IF v_sql_error <> 0 THEN 

   LET v_ind  = -1    ;   
   LET v_diag = "001" ; -- error en funcion general de rev de preliquidacion
   LET err_txt = "error en funcion general de rev de preliquidacion";

   RETURN v_ind       ,
          v_diag      ,
          v_sql_error ,
          isam_err    ,
          err_txt     ,
          v_total_reversados ;
END IF

-- se elimina la historia de la preliquidación en sep_his_det_02_op28

SELECT NVL(COUNT(*),0) 
INTO   v_total_reversados 
FROM   sep_det_02_op28 
WHERE  folio = p_folio 
  AND  estado in (10,15); -- integradas (por error) y preliquidadas

FOREACH SELECT a.id_det_02_op28  
        INTO   v_id_det_02_op28
        FROM   sep_det_02_op28 a
        WHERE  a.folio = p_folio 

        -- las relacionadas al registro de op 28 
        -- con valor actual = 25 (liquidado)
        DELETE FROM sep_his_det_02_op28 
        WHERE  id_det_02_op28 = v_id_det_02_op28 
        AND    valor_actual   = 15 ;  -- estado preliquidado 
END FOREACH;

-- se borra el historico de detalle de movimientos de calculo de preliquidacion
DELETE FROM sep_his_preliquida_op28
WHERE  folio = p_folio ;

-- se actualiza de manera masiva el estado de los detalles
UPDATE sep_det_02_op28 
SET    estado    = 10   -- cambia a integrado
WHERE  folio     = p_folio 
AND    estado   in (15);  -- integrado (en caso de errores),rechazados y preliquidados 

-- se reversa el proceso bat para operacion 28 y preliquidar
EXECUTE FUNCTION fn_reversa_operacion(p_pid ,2202, 3) INTO v_ind;

  -- si ocurre error al reversar el proceso bat

  IF v_ind <> 0 THEN

     LET v_diag = "002" ; -- error al actualizar operacion
     LET err_txt = "error al actualizar operacion";

     RETURN v_ind       ,
            v_diag      ,
            v_sql_error ,
            isam_err    ,
            err_txt     ,
            v_total_reversados ;
  END IF

-- se actualiza el estado del encabezado (control del folio)
UPDATE sep_cza_op28
SET    estado = 15  -- rechazos informados
WHERE  folio  = p_folio ;

-- se actualiza el estado del control de archivo
UPDATE glo_ctr_archivo 
SET    estado = 2
WHERE  folio       = p_folio
AND    proceso_cod = 2202 ;

UPDATE glo_folio
SET    status = 0
WHERE  folio = p_folio;

RETURN v_ind         ,
       v_diag        ,
       v_sql_error   ,
       isam_err    ,
       err_txt     ,
       v_total_reversados ;
END FUNCTION;


