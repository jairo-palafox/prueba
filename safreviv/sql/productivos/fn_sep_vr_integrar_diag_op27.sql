






CREATE FUNCTION "safreviv".fn_sep_vr_integrar_diag_op27(p_proceso_cod SMALLINT)

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
DEFINE v_extension       CHAR(5);
DEFINE v_inicio          SMALLINT;
DEFINE v_fin             SMALLINT;
DEFINE v_id_expedeinte   DECIMAL(9,0);
DEFINE v_id_det_op27     DECIMAL(9,0);
DEFINE ej                char(200);
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


--let ej='cd /safreviv/sep/bin; dbaccess safre_viv .crea_vista.sql';
--system ej;


   -- se busca el máximo folio en estado integrado

   SELECT NVL(MAX(a.folio),0)
     INTO v_folio
     FROM sep_cza_op27 a JOIN glo_folio glo
       ON glo.folio = a.folio
    WHERE glo.proceso_cod = p_proceso_cod;

   LET v_archivo = NULL;
   -- busca el archivo correspondiente al folio
   SELECT nombre_archivo
     INTO v_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = p_proceso_cod -- integracion de diagnostico op 27
      AND folio = v_folio
      AND estado = 2 ;
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
    WHERE proceso_cod = p_proceso_cod -- integracion de diagnostico op 27
      AND folio = v_folio
      AND estado = 2 ;
   -- rechaza el folio si no es de diagnostico
   --IF(v_archivo[v_inicio,v_fin] <> 'dop27')THEN
   IF(v_extension <> 'dop27')THEN
      LET v_ind = 1     ;
      LET v_diag = "003" ;  -- folio no corresponde a diagnóstico

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
    WHERE bat.folio       = v_folio
      AND bat.proceso_cod = p_proceso_cod   -- diagnóstico op 27
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

   -- busca un expediente ligado a operacion 27
{
   FOREACH SELECT id_expediente CLIPPED
             INTO v_id_expedeinte
             FROM sep_det_02_op27
            WHERE folio = v_folio

      IF(v_id_expedeinte IS NOT NULL)THEN
         LET v_ind = 1     ;
         LET v_diag = "005" ; -- expediente ya se encuentra ligado a Op 27
         RETURN v_ind        ,
                v_diag       ,
                v_sql_error  ,
                v_isam_error ,
                v_msg_error  ,
                v_folio      ,
                v_pid        ;
      END IF
   END FOREACH
}
   -- busca operacion 27 en operacion 28
   FOREACH SELECT id_det_op27 CLIPPED
             INTO v_id_det_op27
             FROM sep_det_02_op28 d28 JOIN sep_det_02_op27 d27
               ON d28.id_det_op27 = d27.id_det_02_op27
            WHERE d27.folio = v_folio
             AND  d28.ind_marca = 0

      IF(v_id_det_op27 IS NOT NULL)THEN
         LET v_ind = 1     ;
         LET v_diag = "006" ; -- Operación 27 ya se encuentra en operación 28
         RETURN v_ind        ,
                v_diag       ,
                v_sql_error  ,
                v_isam_error ,
                v_msg_error  ,
                v_folio      ,
                v_pid        ;
      END IF
   END FOREACH


   -- se valida si es valido el reverso del proceso bat
   EXECUTE FUNCTION fn_valida_reverso(v_pid, p_proceso_cod, 2) INTO v_ind;

   IF v_ind <> 0 THEN
      LET v_diag = "007" ; -- error en validacion de reverso de proceso

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


