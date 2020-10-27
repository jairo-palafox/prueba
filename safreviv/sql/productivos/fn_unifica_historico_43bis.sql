






CREATE FUNCTION "safreviv".fn_unifica_historico_43bis(p_clave INTEGER)
-----------------------------------------------------------
   RETURNING SMALLINT, SMALLINT
   
   DEFINE v1_id_unificador       DECIMAL(9,0) ;
   DEFINE v1_nss_unificador      CHAR(11)     ;
   DEFINE v1_id_unificado        DECIMAL(9,0) ;
   DEFINE v1_id_dh_unificador    DECIMAL(9,0) ;
   DEFINE v1_id_dh_unificado     DECIMAL(9,0) ;
   DEFINE v_error                SMALLINT     ;
   DEFINE cont_1                 SMALLINT     ;
   DEFINE v_ind_unificacion_1    SMALLINT     ;
   DEFINE v_error_1              SMALLINT     ;
 
   
   ON EXCEPTION SET v_error
      LET cont_1 = 0 ; 
      RETURN v_error, cont_1 ;
   END EXCEPTION;
   
   SET DEBUG FILE TO '/safreviv_int/BD/fn_unifica_historico_43bis.trace';
   TRACE ON;
   
   LET v_error = 0 ;
   LET cont_1  = 0 ;
              
   IF p_clave = 18 THEN
       FOREACH
           SELECT A.id_unificador      ,
                  A.id_derechohabiente ,
                  A.nss_unificador     ,
                  B.id_unificado       ,
                  C.id_dh_unificado
           INTO   v1_id_unificador     ,
                  v1_id_dh_unificador  ,
                  v1_nss_unificador    ,
                  v1_id_unificado      ,
                  v1_id_dh_unificado
           FROM   uni_det_unificado B, uni_det_unificador A, safre_tmp:ocg_uni_vigente C
           WHERE  B.id_derechohabiente = C.id_dh_unificado
           AND    B.id_unificador      = A.id_unificador
           AND    A.estado_familia     = 1
           AND    A.diagnostico        = 6
           
           EXECUTE FUNCTION fn_ocg_unifica(v1_id_unificador,v1_id_dh_unificado,0) INTO v_ind_unificacion_1,v_error_1 ;
           
           LET cont_1 = cont_1 + 1 ;
           
           UPDATE safre_tmp:ocg_uni_vigente
           SET    id_unificador_uni = v1_id_unificador    ,
                  id_dh_unificador  = v1_id_dh_unificador ,
                  nss_unificador    = v1_nss_unificador   ,
                  id_unificado_uni  = v1_id_unificado     ,
                  edo_unifica       = 1
           WHERE  id_dh_unificado = v1_id_dh_unificado ;
       END FOREACH
   END IF
   RETURN v_error, cont_1 ;
END FUNCTION ;


