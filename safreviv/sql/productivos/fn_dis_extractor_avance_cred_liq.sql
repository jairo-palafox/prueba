






CREATE PROCEDURE "safreviv".fn_dis_extractor_avance_cred_liq(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 18112016
--Declaración de variables

DEFINE v_nss                 CHAR(11);       --Numero seguro social
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_periodo_pago_det    CHAR(6);        --Periodo de pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp_det             CHAR(11);       --Registro patronal
DEFINE v_imp_ap_pat_det      DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_aivs                DECIMAL(18,6);  --AIVS
DEFINE v_imp_am_cre_det      DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_folio_sua           DECIMAL(6,0);   --Folio SUA
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_f_liquida           DATE;
DEFINE v_estado_ava_pag      SMALLINT;
DEFINE v_f_interface         DATE;
DEFINE v_tipo_interface      CHAR(02);


DEFINE v_estado              SMALLINT;       --Estado del registro

DEFINE v_bnd_proceso          SMALLINT;
DEFINE v_char                 CHAR(70);

DEFINE v_status               SMALLINT;
DEFINE sql_err                INTEGER ;
DEFINE isam_err               INTEGER ;
DEFINE error_info             CHAR(70);

DEFINE v_sel_det			 LVARCHAR(350);
DEFINE v_valida              SMALLINT;

DEFINE v_edo_credito         SMALLINT;
DEFINE v_tipo_trabajador     SMALLINT;
DEFINE v_tpo_credito         SMALLINT;
DEFINE v_num_credito_crd     DECIMAL(10,0);
DEFINE v_f_otorga            DATE;
DEFINE v_f_liquida_cred      DATE;

DEFINE v_id_dh_aux			 DECIMAL(9,0);
DEFINE v_nrp				 CHAR(11);
DEFINE v_periodo_pago        CHAR(6);
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito				
DEFINE v_folio_cta_hp        DECIMAL(9,0);


ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION


--SET DEBUG FILE TO '/ds/safreviv_int/dis/fn_dis_extractor_avance_cred_liq.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_nss                    = "";
LET v_id_derechohabiente     = 0;
LET v_num_credito            = 0;
LET v_periodo_pago           = "";
LET v_f_pago                 = "";
LET v_nrp_det                = "";
LET v_imp_ap_pat_det         = 0.00;
LET v_aivs                   = 0.00;
LET v_imp_am_cre_det         = 0.00;
LET v_folio_sua              = 0;
LET v_folio_liquida          = 0;
LET v_f_liquida              = "";
LET v_estado_ava_pag         = 0;
LET v_f_interface            = "";
LET v_tipo_interface         = "";


LET v_estado                 = 0;

LET v_bnd_proceso             = 0;
LET v_char                    = "";

LET v_status                  = 0;

LET sql_err                   = 0;
LET isam_err                  = 0;
LET error_info                = "";

LET v_valida				= 0;
LET v_edo_credito         	= 0;
LET v_tipo_trabajador     	= 0;
LET v_tpo_credito         	= 0;
LET v_num_credito_crd     	= 0;
LET v_f_otorga            	= '';
LET v_f_liquida_cred		= '';

LET v_id_dh_aux				= 0;
LET v_nrp					= '';
LEt v_periodo_pago			= "";
LET v_imp_ap_pat             = 0.00;
LET v_imp_am_cre             = 0.00;
LET v_folio_cta_hp			= "";


   DROP TABLE IF EXISTS tmp_avance_abierto;
 
   DROP TABLE IF EXISTS tmp_avance_cred_liq;
   CREATE TABLE tmp_avance_cred_liq (folio_liquida      DECIMAL(9,0),
									 nss				CHAR(11),
									 periodo_pago_det   CHAR(06),
									 num_credito        DECIMAL(10,0),
									 nrp                CHAR(11),
									 monto_apo          DECIMAL(12,2),
									 monto_amo          DECIMAL(12,2),
									 f_liquida			DATE,
									 folio_pago			DECIMAL(9,0),
									 folio_sua          DECIMAL(6,0),
									 f_pago             DATE,
									 periodo_pago	   	CHAR(06),									 
									 imp_ap_pat         DECIMAL(12,2),
									 imp_am_cre         DECIMAL(12,2),
									 f_liquida_cred 	DATE)
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;
   
   DROP TABLE IF EXISTS tmp_dis_pagos_avance;
   CREATE TABLE tmp_dis_pagos_avance ( id_derechohabiente DECIMAL(9,0),
									   folio_sua          DECIMAL(6,0),
									   periodo_pago       CHAR(06),
									   f_pago             DATE,
									   nrp                CHAR(11),
									   folio			  DECIMAL(9,0),
									   imp_ap_pat		  DECIMAL(12,2),
									   imp_am_cre		  DECIMAL(12,2))
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs; 

   SET PDQPRIORITY HIGH;

   
   
   LET v_sel_det =  " SELECT det.folio, "||
					"		 det.id_derechohabiente, "||
					"		 det.periodo_pago, "||
				    "		 det.num_credito, "||
					"		 det.nrp, "||
					"		 det.monto_aportacion, "||
					"		 det.monto_amortizacion "||
					"  FROM dis_det_avance_pago det "||
					"  WHERE det.tpo_avance = 181 " ||
					"    AND det.estado = 30 " ;
					
	IF p_folio IS NULL OR p_folio = 0 THEN
		LET v_sel_det = v_sel_det ||" INTO TEMP tmp_avance_abierto ";
	ELSE
		LET v_sel_det = v_sel_det ||" AND det.folio = "||p_folio||
					                " INTO TEMP tmp_avance_abierto ";
	END IF
	
	EXECUTE IMMEDIATE v_sel_det;
	
	SELECT DISTINCT(ava.id_derechohabiente), afi.nss
	FROM tmp_avance_abierto ava, afi_derechohabiente afi
	WHERE ava.id_derechohabiente = afi.id_derechohabiente
	INTO TEMP tmp_id_nss;
	
	CREATE INDEX xie1tmp_id_nss ON tmp_id_nss(id_derechohabiente) IN dis_ix_dbs;
	
	FOREACH
		SELECT cta.id_derechohabiente, 
				cta.folio_sua,
				cta.periodo_pago,
				cta.f_pago,
				cta.nrp,
				cta.folio, 
				cta.imp_ap_pat,
				cta.imp_am_cre,
				tmpid.nss
		INTO v_id_dh_aux,  
			 v_folio_sua,	         
			 v_periodo_pago,       	   
			 v_f_pago,            	  
			 v_nrp,
			 v_folio_cta_hp,
			 v_imp_ap_pat,                            
			 v_imp_am_cre,
			 v_nss
		FROM cta_his_pagos cta,
			 tmp_id_nss tmpid
		WHERE  cta.id_derechohabiente  	= tmpid.id_derechohabiente
		AND    cta.ind_liquidacion      NOT IN (1,6)
		
		INSERT INTO tmp_dis_pagos_avance VALUES(v_id_dh_aux,  
												 v_folio_sua,	         
												 v_periodo_pago,       	   
												 v_f_pago,            	  
												 v_nrp,
												 v_folio_cta_hp,
												 v_imp_ap_pat,                            
												 v_imp_am_cre);

		
	END FOREACH;
	
	CREATE INDEX xie1tmp_dis_pagos_avance ON tmp_dis_pagos_avance(id_derechohabiente ,nrp ,periodo_pago ,f_pago) IN dis_ix_dbs;
   
	SET INDEXES FOR tmp_dis_pagos_avance ENABLED;
	UPDATE STATISTICS FOR TABLE tmp_dis_pagos_avance;
	
	
   FOREACH
      SELECT folio, 
			 id_derechohabiente, 
			 periodo_pago, 
			 num_credito, 
			 nrp, 
			 NVL(monto_aportacion,0), 
			 NVL(monto_amortizacion,0) 
      INTO  v_folio_liquida,
			v_id_derechohabiente,
			v_periodo_pago_det, 
            v_num_credito,                
            v_nrp_det,                    
            v_imp_ap_pat_det,                            
            v_imp_am_cre_det        
      FROM tmp_avance_abierto
	  ORDER BY id_derechohabiente, folio
	  
	  LET v_valida = 1;
	  
	  --Identifica el crédito más reciente del derechohabiente que haya sido liquidado
		--v_edo_credito = -2: No hay NSS asociado
		--v_edo_credito = -1: No hay identificador de derechohabiente
		--v_edo_credito = 0: 	Existe crédito vigente
		--v_edo_credito = 1: 	No tiene crédito
		--v_edo_credito = 2: 	Tiene crédito Liquidado
	  EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente, v_valida)
                INTO v_edo_credito,
                     v_tipo_trabajador,
                     v_tpo_credito,
                     v_num_credito_crd,
                     v_f_otorga,
                     v_f_liquida_cred;
					 
	    IF v_edo_credito = 2 THEN  --Credito Liquidado
		
			SELECT f_actualiza
			INTO v_f_liquida
			FROM glo_folio
			WHERE folio = v_folio_liquida;
			
			SELECT nss
			INTO v_nss
			FROM tmp_id_nss
			WHERE id_derechohabiente = v_id_derechohabiente;
			
			FOREACH
				SELECT cta.id_derechohabiente, 
						 cta.folio_sua,
						 fn_bimestre_pago(cta.periodo_pago),
						 cta.f_pago,
						 cta.nrp,
						 cta.folio, 
						 cta.imp_ap_pat,
						 cta.imp_am_cre
					INTO v_id_dh_aux,  
						 v_folio_sua,	         
						 v_periodo_pago,       	   
						 v_f_pago,            	  
						 v_nrp,
						 v_folio_cta_hp,
						 v_imp_ap_pat,                            
						 v_imp_am_cre
				FROM   tmp_dis_pagos_avance cta
				WHERE  cta.id_derechohabiente  	= v_id_derechohabiente
				AND    cta.nrp					= v_nrp_det
				AND    cta.periodo_pago			= v_periodo_pago_det
				AND    cta.f_pago				>= v_f_liquida_cred
				
				INSERT INTO tmp_avance_cred_liq VALUES( v_folio_liquida,
														v_nss,
														v_periodo_pago_det, 
														v_num_credito,                
														v_nrp_det,                    
														v_imp_ap_pat_det,                            
														v_imp_am_cre_det,
														v_f_liquida,
														v_folio_cta_hp,
														v_folio_sua,
														v_f_pago,
														v_periodo_pago,
														v_imp_ap_pat,                            
														v_imp_am_cre ,
														v_f_liquida_cred);
			END FOREACH;
			
		ELSE
		
		END IF

   END FOREACH;
   
   --TRACE 'Finaliza fn_dis_extractor_avance_cred_liq con valor '||v_bnd_proceso;
   
   LET v_char = "Terminado Extractor Avance Credito Liq";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


