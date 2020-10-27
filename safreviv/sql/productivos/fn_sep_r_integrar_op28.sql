






CREATE FUNCTION "safreviv".fn_sep_r_integrar_op28( p_folio DECIMAL(9,0)  , -- folio a reversar
                                        p_pid   INTEGER       ) -- del que se va a reversar
                       
RETURNING SMALLINT       , -- v_ind 
          CHAR(003)      , -- v_diag 
          INTEGER        , -- sql_error
          INTEGER        , --isam_error
          CHAR(100)      , --msg_error
          INTEGER        ; -- total reverdados

-- variables de control
DEFINE v_ind                    SMALLINT   ; 
DEFINE v_diag                   CHAR(003)  ;
DEFINE v_sql_error              INTEGER    ; 
DEFINE v_isam_error             INTEGER    ;
DEFINE v_msg_error              CHAR(100)  ;
DEFINE v_total_reversados       INTEGER    ;

-- variables de proceso
DEFINE v_id_det_02_op28         DECIMAL(9,0) ;
DEFINE v_nombre_archivo         CHAR(40)     ;


   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_r_liquidar.trace'

LET v_ind       =     0     ;
LET v_diag      = "000"     ;
LET v_sql_error =     0     ;
LET v_total_reversados = 0  ;
LET v_nombre_archivo   = '' ;
LET v_isam_error = 0;
LET v_msg_error = "El reverso de la integracion de Op28 se realizó correctamente";


-- se elimina la historia de la integracion en sep_his_det_02_op28

SELECT NVL(COUNT(*),0) 
INTO   v_total_reversados 
FROM   sep_det_02_op28 
WHERE  folio = p_folio ;

SELECT NVL(a.nombre_archivo,"x")
INTO   v_nombre_archivo 
FROM   sep_cza_op28 a
WHERE  folio = p_folio ;

IF v_nombre_archivo  = "x" THEN 

   LET v_ind  = 1     ;
   LET v_diag = "006" ; -- nombre de archivo no encontrado
   LET v_msg_error = "nombre de archivo no encontrado";
   
   RETURN v_ind       ,
          v_diag      ,
          v_sql_error ,
          v_isam_error ,
          v_msg_error  ,
          v_total_reversados ;
END IF


FOREACH SELECT a.id_det_02_op28  
        INTO   v_id_det_02_op28
        FROM   sep_det_02_op28 a
        WHERE  a.folio = p_folio 

        -- las relacionadas al registro de op 28 
        -- con valor actual = 10,30,35 integrados, rechazados y rechazo enviado
        DELETE FROM sep_his_det_02_op28 
        WHERE  id_det_02_op28 = v_id_det_02_op28 
        AND    valor_actual in (10,30,35) ;  -- integrado, rechazado, rechazado enviado
END FOREACH;

-- se elimina la integracion con funcion general de reverso de integracion
EXECUTE FUNCTION fn_reversa_integracion(p_folio,2202) INTO v_sql_error ; 

IF v_sql_error <> 0 THEN 

   LET v_ind  = -1    ;   
   LET v_diag = "001" ; -- error en funcion general de rev de integracion
   LET v_msg_error = "error en funcion general de reverso de integracion";

   RETURN v_ind       ,
          v_diag      ,
          v_sql_error ,
          v_isam_error ,
          v_msg_error  ,
          v_total_reversados ;
END IF

-- se reversa el proceso bat para operacion 28 integrar
EXECUTE FUNCTION fn_reversa_operacion(p_pid ,2202, 2) INTO v_ind;

  -- si ocurre error al reversar el proceso bat
  IF v_ind <> 0 THEN

     LET v_diag = "002" ; -- error al actualizar operacion
     LET v_msg_error = "error al actualizar operacion";

     RETURN v_ind       ,
            v_diag      ,
            v_sql_error ,
            v_isam_error ,
            v_msg_error  ,
            v_total_reversados ;
  END IF

  -- se actualiza el control del archivo a 1 cargado
  UPDATE glo_ctr_archivo 
  SET    estado         = 1,
         folio          = 0
  WHERE  folio          = p_folio 
  AND    nombre_archivo = v_nombre_archivo;

RETURN v_ind         ,
       v_diag        ,
       v_sql_error   ,
       v_isam_error ,
       v_msg_error  ,
       v_total_reversados ;

END FUNCTION;


