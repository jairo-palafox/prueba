






CREATE FUNCTION "safreviv".fn_sep_vr_integrar_marca_op27()

RETURNING SMALLINT       , -- v_ind
          CHAR(003)      , -- v_diag
          INTEGER        , -- sql_error
          INTEGER        , -- isam_error
          CHAR(254)      , -- msg_error
          DECIMAL(9,0)   , -- folio a reversar
          DECIMAL(9,0)   ; -- pid a reversar

-- variables de control

DEFINE v_ind             SMALLINT ;
DEFINE v_diag            CHAR(003);
DEFINE v_sql_error       INTEGER  ;
DEFINE v_isam_error      INTEGER  ;
DEFINE v_msg_error       CHAR(254);

-- variables de proceso
DEFINE v_folio           DECIMAL(9,0);
DEFINE v_pid             DECIMAL(9,0);
DEFINE v_archivo         CHAR(40);
DEFINE v_archivo_pso     CHAR(40);
DEFINE v_extension       CHAR(5);
DEFINE v_inicio          SMALLINT;
DEFINE v_fin             SMALLINT;
DEFINE v_id_expedeinte   DECIMAL(9,0);
DEFINE v_id_det_op27     DECIMAL(9,0);

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

LET v_ind              = 0      ;
LET v_diag             = "000"  ;
LET v_sql_error        = 0      ;
LET v_isam_error       = 0      ;
LET v_msg_error        = ""     ;
LET v_folio            = 0      ;
LET v_pid              = 0      ;

   -- se busca el máximo folio en estado integrado 

   SELECT NVL(MAX(a.folio),0)
     INTO v_folio 
     FROM sep_cza_op27 a;
  
   SELECT a.nombre_archivo 
   INTO   v_archivo_pso
   FROM   sep_cza_op27 a
   WHERE  a.folio = v_folio;
 
    LET v_archivo = NULL;
   -- busca el archivo correspondiente al folio

   SELECT nombre_archivo
     INTO v_archivo
     FROM glo_ctr_archivo
     WHERE folio = v_folio
      AND estado = 2 ;
    IF v_archivo is null THEN

     SELECT nombre_archivo
     INTO v_archivo
     FROM glo_ctr_archivo
     WHERE nombre_archivo = v_archivo_pso
      AND estado = 1 ;    END IF


   -- se rechaza por no haber folio que reversar
   IF(v_archivo IS NULL)THEN
      LET v_ind = 1     ; 
      LET v_diag = "002" ;  -- no se encuentra folio a reversar

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_folio      ,
             v_pid        ;
   END IF
   -- recupera las posiciones para obtener la extension del archivo
   SELECT SUBSTRING(nombre_archivo FROM LENGTH(nombre_archivo)-4
          FOR LENGTH(nombre_archivo))
     INTO v_extension
     FROM glo_ctr_archivo
    WHERE folio = v_folio
      AND estado = 2 ;   
   -- rechaza el folio si no es de diagnostico
   --IF(v_archivo[v_inicio,v_fin] <> '.op27')THEN
   IF(v_extension <> '.op27')THEN
      LET v_ind = 1     ; 
      LET v_diag = "003" ;  -- folio no corresponde a marca op 27

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_folio      ,
             v_pid        ;
   END IF

   -- se obtiene el pid correspondiente al folio que se va a reversar
   
   SELECT NVL(max(bat.pid),0)
     INTO v_pid   
     FROM bat_ctr_operacion       bat
    WHERE bat.folio    = v_folio
      AND bat.proceso_cod = 2201   -- marca op 27
      AND bat.opera_cod   = 2      -- integración
      AND bat.estado_cod  in (3,4) ; -- error o finalizado

   -- se rechaza por no haber encontrado un pid para el folio regresado
   IF(v_pid = 0)THEN 
      LET v_ind = 1     ; 
      LET v_diag = "004" ; -- no se encuentra el pid a ser reversado
      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_folio      ,
             v_pid        ;
   END IF

   -- se valida si es valido el reverso del proceso bat
   EXECUTE FUNCTION fn_valida_reverso(v_pid, 2201, 2) INTO v_ind;

   IF v_ind <> 0 THEN 
      LET v_diag = "005" ; -- error en validacion de reverso de proceso

      RETURN v_ind        ,
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_folio      ,
             v_pid        ;
   END IF

 -- regresa el folio y el pid a reversar
 RETURN v_ind        ,
        v_diag       ,
        v_sql_error  ,
        v_isam_error ,
        v_msg_error  ,
        v_folio      ,
        v_pid        ;

END FUNCTION ;


