






CREATE FUNCTION "safreviv".fn_sep_vr_liquidar_op28()

RETURNING SMALLINT       , -- v_ind
          CHAR(003)      , -- v_diag
          INTEGER        , -- sql_error
          INTEGER        , -- isam error
          VARCHAR(250)   , -- mensaje de error
          DECIMAL(9,0)   , -- folio a reversar
          DECIMAL(9,0)   ; -- pid a reversar

-- variables de control

DEFINE v_ind                    SMALLINT     ;
DEFINE v_diag                   CHAR(003)    ;
DEFINE v_sql_error              INTEGER      ;

-- variables de proceso
DEFINE v_folio                  DECIMAL(9,0) ;
DEFINE v_pid                    DECIMAL(9,0) ;
DEFINE v_existe_preliq          SMALLINT     ;

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 DEFINE v_c_msj                         VARCHAR(255);
 DEFINE v_si_resultado                  SMALLINT;


   -- se establece el comportamiento ante una aparcicion de error
   ON EXCEPTION SET sql_err, isam_err, err_txt

      -- se indica que hubo error
      LET v_ind           = -1     ;
      LET v_diag          = "0"    ;
      LET v_sql_error     = sql_err;
      LET v_folio         = 0      ;
      LET v_pid           = 0      ;

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;
   END EXCEPTION --WITH RESUME


-- inicializa varibales

LET v_ind           = 0      ;
LET v_diag          = "000"  ;
LET v_sql_error     = 0      ;
LET v_folio         = 0      ;
LET v_pid           = 0      ;
LET v_existe_preliq = 0      ;
LET isam_err        = 0;
LET v_c_msj         = "Termina la validacion del reverso de liquidacion de OP28 sin error";


   -- revisa si hay un folio preliquidado

   SELECT NVL(COUNT(*),0)
   INTO   v_existe_preliq
   FROM   sep_cza_op28
   WHERE  estado = 20 ;

   IF v_existe_preliq <> 0 THEN
      -- no se puede reversar liquidacion 
      -- porque se encuentra un folio preliquidado previamente 

      LET v_ind = 1        ;
      LET v_diag = "001"   ; -- folio preliquidado existente no procede rev
      LET v_sql_error = v_diag;
      LET isam_err = 0;
      LET err_txt = "Folio preliquidado existente no procede reverso";
      
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;
   END IF
  
   -- se busca el máximo folio en estado liquidado y que no exista
   -- un folio preliquidado

   SELECT NVL(MAX(a.folio),0) 
   INTO   v_folio 
   FROM   sep_cza_op28 a
   WHERE  a.estado     = 25 ;

   -- se rechaza por no haber folio que reversar

   IF v_folio = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "002" ; -- no hay folio que reversar

      LET v_sql_error = v_diag;
      LET isam_err = 0;
      LET err_txt = "No hay folio para reversar";
      
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;
   END IF

   -- se obtiene el pid correspondiente al folio que se va a reversar
   
   SELECT NVL(MAX(bat.pid),0)
   INTO   v_pid   
   FROM   bat_ctr_operacion       bat  ,
          sep_cza_op28            sep  
   WHERE  sep.folio       = v_folio 
     AND  sep.folio       = bat.folio 
     AND  bat.proceso_cod = 2202   -- operacion 28
     AND  bat.opera_cod   = 4      -- liquidacion
     AND  bat.estado_cod  in (3,4) ; -- error o finalizada ;

   -- se rechaza por no haber encontrado un pid para el folio regresado
   IF v_pid = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "003" ; -- pid no encontrado para reversar

      LET v_sql_error = v_diag;
      LET isam_err = 0;
      LET err_txt = "No se encontró PID para reversar";
      
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;
   END IF

   -- valida que el proceso bat pueda ser reversado
   EXECUTE FUNCTION fn_valida_reverso(v_pid, 2202, 4) INTO v_ind;

   IF v_ind <> 0  THEN 

      LET v_diag = "004" ; -- validacion de validacion de reverso

      LET v_sql_error = v_diag;
      LET isam_err = 0;
      LET err_txt = "Validacion de validacion de reverso";
      
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;
   END IF

 -- regresa el folio y el pid a reversar

      LET v_sql_error = 0;
      LET isam_err = 0;
      LET err_txt = "Validación para realizar reverso: Correcta";
      
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             isam_err     ,
             err_txt      ,
             v_folio      ,
             v_pid        ;

END FUNCTION ;


