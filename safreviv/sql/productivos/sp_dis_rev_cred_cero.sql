






CREATE PROCEDURE "safreviv".sp_dis_rev_cred_cero(p_folio   DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70)

--Última modificación 10032015
--Declaración de variables
DEFINE sql_err                INTEGER ;
DEFINE v_status               SMALLINT;
DEFINE isam_err               INTEGER ;
DEFINE error_info             CHAR(70);
DEFINE v_char                 CHAR(20);
DEFINE v_bnd_proceso          SMALLINT;

DEFINE v_id_derechohabiente   DECIMAL(9,0);
DEFINE v_folio_cred_cero      DECIMAL(9,0);
DEFINE v_id_referencia        DECIMAL(9,0); 
DEFINE v_folio_liquida_orig   DECIMAL(9,0);
DEFINE v_id_cred_cero         DECIMAL(9,0);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info ;
END EXCEPTION

--SET DEBUG FILE TO '/ds/safreviv_int/dis/ERR_sp_dis_rev_cred_cero.TRACE';
--TRACE ON;

LET v_bnd_proceso          = 0; --Estado correcto
LET v_id_derechohabiente   = 0.0;
LET v_folio_cred_cero      = 0.0;
LET v_id_referencia        = 0.0;
LET v_folio_liquida_orig   = 0.0;
LET v_id_cred_cero         = 0.0;

   FOREACH
      SELECT dcc.id_derechohabiente, 
             dcc.folio,
             dli.id_referencia, 
             dli.folio_liquida_orig, 
             dli.id_dis_arh_num_cred 
      INTO  v_id_derechohabiente,
            v_folio_cred_cero, 
            v_id_referencia,
            v_folio_liquida_orig,
            v_id_cred_cero
      FROM  dis_liq_inconsistente dli,
            dis_arh_num_cred_0 dcc
      WHERE dcc.id_dis_arh_num_cred = dli.id_dis_arh_num_cred 
      AND   dcc.folio = dli.folio_arh_num_cred 
      AND   dli.folio_liquida =  p_folio

      UPDATE dis_liq_inconsistente 
      SET folio_liquida = 0,
          num_credito   = NULL,
          aportacion    = 0,
          amortizacion  = 0,
          aivs          = 0,
          edo_liquida   = 0
      WHERE id_dis_arh_num_cred = v_id_cred_cero
      AND   folio_arh_num_cred  = v_folio_cred_cero
      AND   edo_liquida         IN (2,3,4);
      
      UPDATE dis_info_inconsistente
      SET tpo_inconsistente = 0
      WHERE id_derechohabiente = v_id_derechohabiente
      AND   id_referencia = v_id_referencia
      AND   folio_liquida = v_folio_liquida_orig;

      UPDATE dis_arh_num_cred_0
      SET estado = 0
      WHERE id_dis_arh_num_cred = v_id_cred_cero
      AND   folio               = v_folio_cred_cero
      AND   estado              IN (2,3,4);

   END FOREACH

   --TRACE 'Finaliza sp_dis_rev_cred_cero con valor '||v_bnd_proceso; 
   LET v_char = " Reverso Créditos Ceros finalizó correctamente";
   RETURN v_bnd_proceso , 0 , v_char;
END PROCEDURE;


