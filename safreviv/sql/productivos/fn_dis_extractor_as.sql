






CREATE PROCEDURE "safreviv".fn_dis_extractor_as(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 15072015
--Declaración de variables
DEFINE v_nss                 CHAR(11);       --Numero seguro social
DEFINE v_derechohabiente     DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_aivs                DECIMAL(18,6);  --AIVS
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_folio_sua           DECIMAL(6,0);   --Folio SUA
DEFINE v_folio_archivo       DECIMAL(9,0);
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_f_liquida           DATE;
DEFINE v_ind_concilia        SMALLINT;
DEFINE v_estado              SMALLINT;
DEFINE v_f_interface         DATE;
DEFINE v_tipo_interface      CHAR(02);
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

--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-711/fn_dis_extractor_as.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_nss                    = "";
LET v_derechohabiente        = 0;
LET v_num_credito            = 0;
LET v_periodo_pago           = "";
LET v_f_pago                 = "";
LET v_nrp                    = "";
LET v_imp_ap_pat             = 0.00;
LET v_aivs                   = 0.00;
LET v_imp_am_cre             = 0.00;
LET v_folio_sua              = 0;
LET v_folio_archivo          = 0;
LET v_folio_liquida          = 0;
LET v_f_liquida              = "";
LET v_ind_concilia           = 0;
LET v_estado                 = 0;
LET v_f_interface            = "";
LET v_tipo_interface         = "";
LET v_bnd_proceso            = 0;
LET v_char                   = "";
LET v_status                 = 0;
LET sql_err                  = 0;
LET isam_err                 = 0;
LET error_info               = "";

  DROP TABLE IF EXISTS tmp_dis_as;
  CREATE TABLE tmp_dis_as (nss                CHAR(11) NOT NULL,
                           periodo_pago       CHAR(6),
                           f_pago             DATE, 
                           nrp                CHAR(11),
                           monto_apo          DECIMAL(12,2),
                           aivs               DECIMAL(18,6),
                           monto_amo          DECIMAL(12,2),
                           folio_sua          DECIMAL(6,0),
                           folio_archivo      DECIMAL(9,0),
                           folio_liquida      DECIMAL(9,0),
                           f_liquida          DATE,
                           ind_concilia       SMALLINT,
                           estado             SMALLINT,
                           f_interface        DATE,
                           tipo_interface     CHAR(02))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SET PDQPRIORITY HIGH;
   
  DROP TABLE IF EXISTS tmp_nss_id_as;
	
  SELECT afi.nss, aps.folio, aps.folio_liquida, aps.id_dis_interface_ef,
         aps.id_derechohabiente, aps.folio_sua, aps.periodo_pago,
         aps.f_pago, aps.reg_pat_imss, aps.imp_apo_pat,
         aps.imp_amo_cred, aps.apl_apo_pat, aps.estado
  FROM   afi_derechohabiente afi, dis_ap_subsecuente aps
  WHERE  afi.id_derechohabiente = aps.id_derechohabiente
  AND    aps.folio              = p_folio
  INTO TEMP tmp_nss_id_as;
	
  UPDATE STATISTICS FOR TABLE tmp_nss_id_as;

  SELECT glo.f_actualiza
  INTO   v_f_liquida
  FROM   glo_folio glo
  WHERE  folio = p_folio;

  FOREACH
    SELECT afi.nss, 
           afi.periodo_pago, 
           afi.f_pago, 
           afi.reg_pat_imss, 
           NVL(afi.imp_apo_pat,0), 
           NVL(afi.apl_apo_pat,0), 
           NVL(afi.imp_amo_cred,0),
           afi.folio_sua,
           afi.folio,
           afi.folio_liquida,
           NVL(ef.ind_liquidacion,0),
           afi.estado,
           TODAY, 
           'AS'
    INTO   v_nss,
           v_periodo_pago,           
           v_f_pago,                 
           v_nrp,                    
           v_imp_ap_pat,            
           v_aivs,   
           v_imp_am_cre,
           v_folio_sua,    
           v_folio_archivo,
           v_folio_liquida,          
           v_ind_concilia,
           v_estado,         
           v_f_interface,            
           v_tipo_interface         
    FROM   tmp_nss_id_as afi, 
    OUTER  dis_interface_ef ef
    WHERE  ef.id_dis_interface_ef = afi.id_dis_interface_ef
    AND    ef.folio_liquida       = afi.folio_liquida
    ORDER BY ef.ind_liquidacion DESC, afi.periodo_pago
       
    INSERT INTO tmp_dis_as VALUES (v_nss,
                                   v_periodo_pago,           
                                   v_f_pago,                 
                                   v_nrp,                    
                                   v_imp_ap_pat,            
                                   v_aivs, 
                                   v_imp_am_cre,
                                   v_folio_sua,  
                                   v_folio_archivo,            
                                   v_folio_liquida,          
                                   v_f_liquida,             
                                   v_ind_concilia, 
                                   v_estado,
                                   v_f_interface,            
                                   v_tipo_interface);

  END FOREACH;
   
  --TRACE 'Finaliza fn_dis_extractor_as con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado Extractor Dispersión Aportaciones Subsecuentes";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


