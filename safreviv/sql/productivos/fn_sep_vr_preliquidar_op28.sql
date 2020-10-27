






CREATE FUNCTION "safreviv".fn_sep_vr_preliquidar_op28()

RETURNING SMALLINT       , -- v_ind
          CHAR(003)      , -- v_diag
          INTEGER        , -- sql_error
          INTEGER        , -- isam_error
          VARCHAR(250)   , -- mensaje
          DECIMAL(9,0)   , -- folio a reversar
          DECIMAL(9,0)   ; -- pid a reversar

-- variables de control

DEFINE v_ind                    SMALLINT     ;
DEFINE v_diag                   CHAR(003)    ;
DEFINE v_sql_error              INTEGER      ;

-- variables de proceso
DEFINE v_folio                  DECIMAL(9,0) ;
DEFINE v_pid                    DECIMAL(9,0) ;
DEFINE v_existe_integrado       SMALLINT     ;

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
            v_folio      ,
            v_pid        ;
  END EXCEPTION --WITH RESUME


  -- inicializa varibales
  LET v_ind              = 0      ;
  LET v_diag             = "000"  ;
  LET v_sql_error        = 0      ;
  LET isam_err           = 0      ;
  LET err_txt            = "La validacion del reverso se realizo correctamente";
  LET v_folio            = 0      ;
  LET v_pid              = 0      ;
  LET v_existe_integrado = 0      ;

   -- revisa si hay un folio integrado o rechazo informado previamente

   SELECT NVL(COUNT(*),0)
   INTO   v_existe_integrado
   FROM   sep_cza_op28
   WHERE  estado in (10,15) ;

   IF v_existe_integrado <> 0 THEN
      -- no se puede reversar preliquidacion 
      -- porque se encuentra un folio integrado previamente 

      LET v_ind = 1           ;
      LET v_diag = "001"      ; -- folio integrado previo rev no procede
      LET err_txt = "Folio integrado previo rev no procede";
      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;

   END IF
  
   -- se busca el máximo folio en estado preliquidado y que no exista
   -- un folio integrado

   SELECT NVL(MAX(a.folio),0) 
   INTO   v_folio 
   FROM   sep_cza_op28 a
   WHERE  a.estado     = 20 ;

   -- se rechaza por no haber folio que reversar

   IF v_folio = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "002" ; -- folio preliquidado no encontrado
      LET err_txt = "Folio preliquidado no encontrado";

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;
   END IF

   -- se obtiene el pid correspondiente al folio que se va a reversar
   SELECT NVL(MAX(bat.pid),0)
   INTO   v_pid   
   FROM   bat_ctr_operacion       bat  ,
          sep_cza_op28            sep  
   WHERE  sep.folio    = v_folio 
     AND  sep.folio    = bat.folio  
     AND  bat.proceso_cod = 2202     -- operación 28
     AND  bat.opera_cod   = 3        -- preliquidacion
     AND  bat.estado_cod  in (3,4) ; -- error o finalizado

   -- se rechaza por no haber encontrado un pid para el folio regresado
   IF v_pid = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "003" ; -- pid no encontrado
      LET err_txt = "PID no encontrado";

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;
   END IF

   -- se valida que el proceso bat pueda ser reversado
   EXECUTE FUNCTION fn_valida_reverso(v_pid, 2202, 3) INTO v_ind;

   IF v_ind <> 0 THEN 

      LET v_diag = "004" ; --operacion no actualizada
      LET err_txt = "Operacion no actualizada";

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;
   END IF

 -- regresa el folio y el pid a reversar

 RETURN    v_ind         ,
           v_diag        ,
           v_sql_error   ,
           isam_err      ,
           err_txt       ,
           v_folio       ,
           v_pid         ;

END FUNCTION ;


