






CREATE FUNCTION "safreviv".fn_sep_vr_integrar_op29()

RETURNING SMALLINT       , -- v_ind
          CHAR(003)      , -- v_diag
          INTEGER        , -- sql_error
          INTEGER        , -- isam_error
          CHAR(100)      , -- msg_error
          DECIMAL(9,0)   , -- folio a reversar
          DECIMAL(9,0)   ; -- pid a reversar

-- variables de control

DEFINE v_ind        SMALLINT ;
DEFINE v_diag       CHAR(003);
DEFINE v_sql_error  INTEGER  ;
DEFINE v_isam_error INTEGER  ;
DEFINE v_msg_error  CHAR(100);

-- variables de proceso
DEFINE v_folio                  DECIMAL(9,0) ;
DEFINE v_pid                    DECIMAL(9,0) ;

   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_folio      ,
             v_pid        ;
   END EXCEPTION WITH RESUME


-- inicializa varibales

LET v_ind        = 0     ;
LET v_diag       = "000" ;
LET v_sql_error  = 0     ;
LET v_isam_error = 0     ;
LET v_msg_error  = ""    ;
LET v_folio      = 0     ;
LET v_pid        = 0     ;

   -- se busca el máximo folio en estado integrado 

   SELECT NVL(MAX(a.folio),0) 
   INTO   v_folio 
   FROM   sep_cza_op29 a ;

   -- se rechaza por no haber folio que reversar

   IF v_folio = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "002" ;  -- no se encuentra folio a reversar

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                v_isam_error ,
                v_msg_error  ,
                v_folio       ,
                v_pid         ;
   END IF

   -- se obtiene el pid correspondiente al folio que se va a reversar
   
   SELECT NVL(MAX(bat.pid),0)
   INTO   v_pid   
   FROM   bat_ctr_operacion       bat  ,
          sep_cza_op29            sep  
   WHERE  sep.folio    = v_folio 
     AND  sep.folio    = bat.folio  
     AND  bat.proceso_cod = 2203   -- operación 29
     AND  bat.opera_cod   = 2      -- preliquidacion
     AND  bat.estado_cod  in (3,4) ; -- error o finalizado

   -- se rechaza por no haber encontrado un pid para el folio regresado

   IF v_pid = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "003" ; -- no se encuentra el pid a ser reversado

      RETURN    v_ind        ,
                v_diag       ,
                v_sql_error  ,
                v_isam_error ,
                v_msg_error  ,
                v_folio      ,
                v_pid        ;
   END IF


   -- se valida si es valido el reverso del proceso bat
   EXECUTE FUNCTION fn_valida_reverso(v_pid, 2203, 2) INTO v_ind;

   IF v_ind <> 0 THEN 

      LET v_diag = "004" ; -- error en validacion de reverso de proceso

      RETURN    v_ind        ,
                v_diag       ,
                v_sql_error  ,
                v_isam_error ,
                v_msg_error  ,
                v_folio      ,
                v_pid        ;
   END IF

 -- regresa el folio y el pid a reversar
 RETURN    v_ind        ,
           v_diag       ,
           v_sql_error  ,
           v_isam_error ,
           v_msg_error  ,
           v_folio      ,
           v_pid        ;

END FUNCTION ;


