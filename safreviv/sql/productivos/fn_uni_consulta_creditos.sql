






CREATE FUNCTION "safreviv".fn_uni_consulta_creditos(
                                        p_id_derechohabiente DECIMAL(9,0),
                                        p_id_unificador      DECIMAL(9,0),
                                        p_numero_credito     DECIMAL(10,0),
                                        p_folio_unificacion  DECIMAL(9,0)
                                        ) 
RETURNING SMALLINT,
          SMALLINT,
          DECIMAL(10,0),
          SMALLINT,
          DECIMAL(8,4),
          CHAR(11),
          CHAR(11),
          INTEGER,
          INTEGER

DEFINE r_resultado         SMALLINT;
DEFINE r_tpo_originacion   SMALLINT;
DEFINE r_tpo_credito       SMALLINT;
DEFINE r_num_credito       DECIMAL(10,0);
DEFINE r_f_otorga          DATE;
DEFINE r_f_liquida         DATE;
DEFINE v_fecha_unificacion DATE;
DEFINE v_folio_unificacion DECIMAL(9,0);
DEFINE v_valor_dscto       DECIMAL(8,4);
DEFINE v_tpo_unificacion   SMALLINT;
DEFINE v_tpo_credito       SMALLINT;
DEFINE v_num_credito       DECIMAL(10,0);
DEFINE v_tpo_dscto         SMALLINT;
DEFINE v_cont_sin_credito  INTEGER;
DEFINE v_cont_con_credito  INTEGER;                          
DEFINE v_nrp               CHAR(11);
DEFINE v_nss               CHAR(11);
DEFINE v_resultado         INTEGER;
DEFINE sql_err             INTEGER;
DEFINE isam_err            INTEGER;
DEFINE v_isam_err          INTEGER;



   ON EXCEPTION SET sql_err, isam_err
      LET v_resultado = sql_err;
      LET v_isam_err = isam_err;

      RETURN v_tpo_unificacion ,
             v_tpo_credito     ,
             v_num_credito     ,
             v_tpo_dscto       ,
             v_valor_dscto     ,
             v_nrp             ,
             v_nss             ,
             v_resultado       ,
             v_isam_err;
   END EXCEPTION

LET v_cont_sin_credito = 0;
LET v_cont_con_credito = 0;
LET v_nrp              = 0;                 
LET v_tpo_dscto        = 0;
LET v_valor_dscto      = 0;
LET v_resultado        = 0;
LET v_isam_err         = 0;

   SELECT nss
   INTO   v_nss
   FROM   afi_derechohabiente 
   WHERE  id_derechohabiente = p_id_derechohabiente;

   --Se consulta si el DH tiene crédito  
   EXECUTE FUNCTION fn_credito_vivienda(p_id_derechohabiente, 0) 
   INTO  r_resultado,
         r_tpo_originacion,
         r_tpo_credito,
         r_num_credito,
         r_f_otorga,
         r_f_liquida;
   
   --Si la cuenta tiene crédito consulta valor de descuento 
   IF r_resultado = 0 THEN
      LET v_tpo_unificacion = "99";
      LET v_cont_con_credito = v_cont_con_credito + 1;
      LET v_tpo_credito = r_tpo_credito;
      LET v_num_credito = r_num_credito;
      
      LET v_tpo_dscto   = 0; 
      LET v_valor_dscto = 0;

       SELECT FIRST 1 (a.nrp)
       INTO   v_nrp
       FROM   cre_acreditado a,
              cta_credito b,
              cat_maq_credito c
       WHERE  a.id_derechohabiente = b.id_derechohabiente
       AND    a.id_derechohabiente = p_id_derechohabiente
       AND    a.num_credito        = v_num_credito
       AND    a.tpo_credito        = b.tpo_credito
       AND    a.num_credito        = a.num_credito
       AND    a.estado             = c.estado
       AND    c.entidad            = 1
       GROUP BY 1;
   ELSE
      --Si la cuenta NO tiene crédito se asigna tipo de unificación 88 - Unificadores sin crédito 
      LET v_tpo_unificacion  = "88";
      LET v_tpo_credito      = 0;
      LET v_num_credito      = 0;
      LET v_tpo_dscto        = 0;
      LET v_cont_sin_credito = v_cont_sin_credito + 1;
   END IF      

RETURN v_tpo_unificacion ,
       v_tpo_credito     ,
       v_num_credito     ,
       v_tpo_dscto       ,
       v_valor_dscto     ,
       v_nrp             ,
       v_nss             ,
       v_resultado       ,
       v_isam_err;

END FUNCTION;


