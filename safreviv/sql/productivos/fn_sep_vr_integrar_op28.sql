






CREATE FUNCTION "safreviv".fn_sep_vr_integrar_op28()

RETURNING SMALLINT       , -- v_ind
          CHAR(003)      , -- v_diag
          INTEGER        , -- sql_error
          INTEGER        , -- isam error
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
LET v_folio            = 0      ;
LET v_pid              = 0      ;
LET isam_err           = 0;
LET err_txt            = "El reverso de la integración Op28 se puede realizar";

   -- se busca el máximo folio en estado integrado 

   SELECT NVL(MAX(a.folio),0) 
   INTO   v_folio 
   FROM   sep_cza_op28 a
   WHERE  a.estado     in (10,15) ;  -- integrado o rechazos informados

   -- se rechaza por no haber folio que reversar

   IF v_folio = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "002" ;  -- no se encuentra folio a reversar
      LET err_txt = "no se encuentra folio a reversar";

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;
   END IF

   -- se obtiene el pid correspondiente al folio que se va a reversar
   
   SELECT NVL(max(bat.pid),0)
   INTO   v_pid   
   FROM   bat_ctr_operacion       bat
   WHERE  bat.folio    = v_folio 
     AND  bat.proceso_cod = 2202   -- operación 28
     AND  bat.opera_cod   = 2      -- integracion
     AND  bat.estado_cod  in (3,4) ; -- error o finalizado

   -- se rechaza por no haber encontrado un pid para el folio regresado

   IF v_pid = 0 THEN 

      LET v_ind = 1     ; 
      LET v_diag = "003" ; -- no se encuentra el pid a ser reversado
      LET err_txt = "no se encuentra pid a ser reversado";

      RETURN    v_ind         ,
                v_diag        ,
                v_sql_error   ,
                isam_err      ,
                err_txt       ,
                v_folio       ,
                v_pid         ;
   END IF


   -- se valida si es valido el reverso del proceso bat
   EXECUTE FUNCTION fn_valida_reverso(v_pid, 2202, 2) INTO v_ind;

   IF v_ind <> 0 THEN 

      LET v_diag = "004" ; -- error en validacion de reverso de proceso
      LET err_txt = "error en validación de reverso de proceso";

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


