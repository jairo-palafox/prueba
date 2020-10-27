






CREATE FUNCTION "safreviv".fn_sep_r_integrar_op29(p_folio       DECIMAL(9,0) ,
                                p_pid         DECIMAL(9,0) , -- pid de op 29
                                p_proceso_cod INTEGER      , -- proceso de op 29
                                p_opera_cod   SMALLINT     ) -- operacion de op 29

RETURNING SMALLINT    , -- indicador de resutlado
          CHAR(003)   , -- diagnostico de indicador
          INTEGER     , -- codigo de error
          SMALLINT    , -- isam error
          CHAR(100)   , -- msg error
          INTEGER     , -- d2 reversados
          INTEGER     , -- d3 reversados
          INTEGER     , -- d5 reversados
          INTEGER     ; -- d6 reversados

-- variables de control

DEFINE v_ind                 SMALLINT   ;
DEFINE v_diag                CHAR(003)  ;
DEFINE v_sql_error           INTEGER    ;
DEFINE v_isam_error          SMALLINT    ;
DEFINE v_msg_error           CHAR(100)  ;
DEFINE v_existe              SMALLINT   ;

-- variables de proceso

DEFINE v_id_det_02_op29  DECIMAL(9,0)  ;  -- id de det 02 de op 29
DEFINE v_id_det_03_op29  DECIMAL(9,0)  ;  -- id de det 03 de op 29
DEFINE v_tot_02          INTEGER       ;
DEFINE v_tot_03          INTEGER       ;
DEFINE v_tot_05          INTEGER       ;
DEFINE v_tot_06          INTEGER       ;

-- Captura el error sql

   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind    =   -1   ;  -- error sql
      LET v_diag   = "001"  ;  -- error de sql ver sql_error
      LET v_tot_02 =    0   ;
      LET v_tot_03 =    0   ;
      LET v_tot_05 =    0   ;
      LET v_tot_06 =    0   ;

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_tot_02     ,
             v_tot_03     ,
             v_tot_05     ,
             v_tot_06     ;

   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_rev_op29.trace';

-- asignación inicial de variables


LET v_ind              =     0   ;
LET v_diag             = "000"   ; -- sin error
LET v_sql_error        =     0   ;
LET v_isam_error       =     0   ;
LET v_msg_error        = ""      ;
LET v_tot_02           =     0   ;
LET v_tot_03           =     0   ;
LET v_tot_05           =     0   ;
LET v_tot_06           =     0   ;


-- asignacion inicial de variables de proceso

LET v_id_det_02_op29   =     0   ;
LET v_id_det_03_op29   =     0   ;


-- verifica que exista el folio

-- TRACE "paso1";

SELECT NVL(COUNT(*),0)
  INTO v_existe
  FROM safre_viv:sep_cza_op29
 WHERE folio = p_folio ;

IF v_existe <> 1 THEN

   LET v_ind  = 1 ;

   IF v_existe = 0 THEN
      LET v_diag = "002" ;  -- folio no existe
   ELSE  IF v_existe > 1 THEN
            LET v_diag = "003" ; -- mas de un folio
         END IF
   END IF

   RETURN v_ind        ,  -- regresa error logico
          v_diag       ,
          v_sql_error  ,
          v_isam_error ,
          v_msg_error  ,
          v_tot_02     ,
          v_tot_03     ,
          v_tot_05     ,
          v_tot_06     ;

ELSE
-- TRACE "PASO 2";
   SELECT NVL(COUNT(*),0)
   INTO   v_tot_02   -- total a reversar
   FROM   sep_det_02_op29
   WHERE  folio = p_folio ;

   IF v_tot_02 = 0 THEN
      LET v_ind              =    1  ;
      LET v_diag             = "004" ; -- sin registros de detalle

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_tot_02     ,
             v_tot_03     ,
             v_tot_05     ,
             v_tot_06     ;

   END IF
END IF

-- se eliminan los registros de la operación 29
-- el reverso elimina tanto la integracion como
-- la carga del archivo

   -- total de detalles 02 a borrar
-- TRACE "PASO 3";

   SELECT NVL(COUNT(*),0)
   INTO   v_tot_02
   FROM   sep_det_02_op29
   WHERE  folio = p_folio;

   FOREACH cur_borra_reg_op29 FOR
      SELECT d2.id_det_02_op29
        INTO v_id_det_02_op29
        FROM sep_det_02_op29 d2
       WHERE d2.folio = p_folio

-- TRACE "PASO 4" ;

       SELECT d3.id_det_03_op29
         INTO v_id_det_03_op29
         FROM sep_det_03_op29 d3
        WHERE d3.id_det_03_op29 = v_id_det_02_op29;

       -- total de det 06 a borrar
-- TRACE "PASO 5";
       SELECT NVL(COUNT(*),0)
       INTO   v_tot_06
       FROM   sep_det_06_op29
       WHERE  id_det_03_op29 = v_id_det_03_op29;

       -- borrar det 06
-- TRACE "PASO 6";
       DELETE FROM sep_det_06_op29
       WHERE  id_det_03_op29 = v_id_det_03_op29;

       -- total de det 05 a borrar

-- TRACE "PASO 7";
       SELECT NVL(COUNT(*),0)
       INTO   v_tot_05
       FROM   sep_det_05_op29
       WHERE  id_det_02_op29 = v_id_det_02_op29;

       -- borrar det 05

-- TRACE "PASO 8";
       DELETE FROM sep_det_05_op29
       WHERE  id_det_02_op29 = v_id_det_02_op29;

    END FOREACH;

    -- total de det 03 a borrar

-- TRACE "PASO 9";
    SELECT NVL(COUNT(unique d3.id_det_03_op29),0)
    INTO   v_tot_03
    FROM   sep_det_02_op29 d2 ,
           sep_det_03_op29 d3
    WHERE  d2.folio = p_folio
    AND    d2.id_det_02_op29 = d3.id_det_03_op29 ;

    -- borrar det 03

-- TRACE "PASO 10";
    DELETE FROM sep_det_03_op29
    WHERE  id_det_02_op29 IN (SELECT d2.id_det_02_op29
                              FROM   sep_det_02_op29 d2
                              WHERE  d2.folio = p_folio);

    -- borrar det 02

-- TRACE "PASO 11";
    DELETE FROM sep_det_02_op29
    WHERE  folio = p_folio;

    -- borrar cza op 29

-- TRACE "PASO 12";
    DELETE FROM sep_cza_op29
    WHERE  folio = p_folio ;

    DELETE FROM sep_sum_op29
    WHERE  folio = p_folio ;

    -- actualiza proceso en monitor de proceso como reversado.
    -- se actualiza tambien el indicador de proceso en bat_ctr_operacion
    -- como 0 (regla para ejecución manual).

-- TRACE "PASO 13";

   EXECUTE FUNCTION fn_reversa_operacion(p_pid, p_proceso_cod, p_opera_cod) INTO v_ind;

   IF v_ind <> 0 THEN 

      LET v_diag = "005" ; -- error al actualizar el proceso 
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_tot_02     ,
             v_tot_03     ,
             v_tot_05     ,
             v_tot_06     ;
   END IF

   UPDATE glo_ctr_archivo 
   SET    estado = 1
   WHERE  folio = p_folio ;


RETURN v_ind        ,
       v_diag       ,
       v_sql_error  ,
       v_isam_error ,
       v_msg_error  ,
       v_tot_02     ,
       v_tot_03     ,
       v_tot_05     ,
       v_tot_06     ;

END FUNCTION;


