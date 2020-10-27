






CREATE PROCEDURE "safreviv".fn_dis_extractor_ca(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 15072015
--Declaración de variables
DEFINE v_nss                 CHAR(11);       --Numero seguro social
DEFINE v_derechohabiente     DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_apo_ava             DECIMAL(12,2);  --Aportaciones Avance 
DEFINE v_amo_ava             DECIMAL(12,2);  --Amortizaciones Avance
DEFINE v_edo_ava             SMALLINT;       --Estado Avance de Pagos
DEFINE v_apo_pag             DECIMAL(12,2);  --Aportaciones Pago 
DEFINE v_amo_pag             DECIMAL(12,2);  --Amortizaciones Pago
DEFINE v_monto_dif_apo       DECIMAL(12,2);  --Aportacion Diferencia
DEFINE v_monto_dif_amo       DECIMAL(12,2);  --Amortacion Diferencia
DEFINE v_edo_comp_apo        SMALLINT;
DEFINE v_edo_comp_amo        SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_f_liquida           DATE;
DEFINE v_f_interface         DATE;
DEFINE v_tipo_interface      CHAR(02);
DEFINE v_estado              SMALLINT;       --Estado del registro
DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(20);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-711/fn_dis_extractor_ca.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_nss                    = "";
LET v_derechohabiente        = 0;
LET v_num_credito            = 0;
LET v_periodo_pago           = "";
LET v_f_pago                 = "";
LET v_nrp                    = "";
LET v_apo_ava                = 0.00;
LET v_amo_ava                = 0.00;
LET v_edo_ava                = 0;
LET v_apo_pag                = 0.00;
LET v_amo_pag                = 0.00;
LET v_monto_dif_apo          = 0.00;
LET v_monto_dif_amo          = 0.00;
LET v_edo_comp_apo           = 0;
LET v_edo_comp_amo           = 0;
LET v_folio_liquida          = 0;
LET v_f_liquida              = "";
LET v_f_interface            = "";
LET v_tipo_interface         = "";
LET v_estado                 = 0;
LET v_bnd_proceso            = 0;
LET v_char                   = "";
LET v_status                 = 0;
LET sql_err                  = 0;
LET isam_err                 = 0;
LET error_info               = "";

  DROP TABLE IF EXISTS tmp_dis_ca;
  CREATE TABLE tmp_dis_ca (nss                CHAR(11) NOT NULL,
                           num_credito        DECIMAL(10,0),
                           periodo_pago       CHAR(6),
                           f_pago             DATE, 
                           nrp                CHAR(11),
                           monto_apo_ava      DECIMAL(12,2),
                           monto_amo_ava      DECIMAL(12,2),
                           estado_avance      SMALLINT,
                           monto_apo_pag      DECIMAL(12,2),
                           monto_amo_pag      DECIMAL(12,2),
                           monto_dif_apo      DECIMAL(12,2),
                           monto_dif_amo      DECIMAL(12,2),
                           estado_comp_apo    SMALLINT,
                           estado_comp_amo    SMALLINT,
                           folio_liquida      DECIMAL(9,0),
                           f_liquida          DATE,
                           f_interface        DATE,
                           tipo_interface     CHAR(02))
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

   SET PDQPRIORITY HIGH;
   
   DROP TABLE IF EXISTS tmp_nss_id_ca;
	
   SELECT afi.nss, ca.*
   FROM   afi_derechohabiente afi, dis_compensa_avance ca
   WHERE  afi.id_derechohabiente = ca.id_derechohabiente
   AND    ca.folio_dis           = p_folio
   INTO TEMP tmp_nss_id_ca;
	
   UPDATE STATISTICS FOR TABLE tmp_nss_id_ca;

   SELECT glo.f_actualiza
   INTO   v_f_liquida
   FROM   glo_folio glo
   WHERE  folio = p_folio;

   FOREACH
     SELECT afi.nss, 
            afi.num_credito, 
            afi.periodo_pago, 
            afi.f_pago, 
            afi.nrp, 
            NVL(afi.monto_apo_avance,0), 
            NVL(afi.monto_amo_avance,0),
            dap.estado,
	        NVL(afi.monto_apo_pag,0), 
            NVL(afi.monto_amo_pag,0),
            NVL(dap.monto_dif_apo,0),
            NVL(dap.monto_dif_amo,0),
            afi.edo_compensa_apo,
            afi.edo_compensa_amo,
            afi.folio_dis,
            TODAY, 
            'CA'  
     INTO   v_nss,
            v_num_credito,
            v_periodo_pago,           
            v_f_pago,                 
            v_nrp,                    
            v_apo_ava,            
            v_amo_ava,                              
            v_edo_ava, 
            v_apo_pag,            
            v_amo_pag,
            v_monto_dif_apo,
            v_monto_dif_amo,
            v_edo_comp_apo,
            v_edo_comp_amo,
            v_folio_liquida,          
            v_f_interface,            
            v_tipo_interface           
     FROM   tmp_nss_id_ca afi, 
            dis_det_avance_pago dap
     WHERE  afi.id_dis_det_avance_pago = dap.id_dis_det_avance_pago
     ORDER BY afi.periodo_pago
        
     INSERT INTO tmp_dis_ca VALUES (v_nss,
                                    v_num_credito,
                                    v_periodo_pago,           
                                    v_f_pago,                 
                                    v_nrp,                    
                                    v_apo_ava,            
                                    v_amo_ava,                              
                                    v_edo_ava, 
                                    v_apo_pag,            
                                    v_amo_pag,
                                    v_monto_dif_apo,
                                    v_monto_dif_amo,
                                    v_edo_comp_apo,
                                    v_edo_comp_amo,
                                    v_folio_liquida,          
                                    v_f_liquida,                     
                                    v_f_interface,            
                                    v_tipo_interface);

   END FOREACH;
   
   --TRACE 'Finaliza fn_dis_extractor_ca con valor '||v_bnd_proceso;
   
   LET v_char = "Terminado Extractor Dispersión Avances Compensados";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


