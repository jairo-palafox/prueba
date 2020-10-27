






CREATE FUNCTION "safreviv".fn_ocg_extractor_creditos_43bis()
   RETURNING SMALLINT,INTEGER,INTEGER

   DEFINE v_error                SMALLINT;
   DEFINE v_tot_vigentes         INTEGER;
   DEFINE v_tot_liquidados       INTEGER;
   DEFINE v_nss                  CHAR(11);
   DEFINE v_solic_marca_prc      DATE; 
   DEFINE v_conf_marca_prc       DATE;
   DEFINE v_solic_desm_prc       DATE;
   DEFINE v_conf_desm_prc        DATE;
   DEFINE v_situacion            SMALLINT;
   DEFINE v_tpo_credito          CHAR(1);
   DEFINE v_identificador        CHAR(1);
   DEFINE v_id_ocg_formalizacion DECIMAL(9,0);
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_f_formalizacion      DATE;
   DEFINE v_id_ocg_liquidacion   DECIMAL(9,0);

   ON EXCEPTION SET v_error
      LET v_tot_vigentes = 0;
      LET v_tot_liquidados = 0;
      RETURN v_error,v_tot_vigentes,v_tot_liquidados;
   END EXCEPTION;

   --SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_extrae_creditos_vigentes.trace';
   --TRACE ON;

   LET v_error                = 0;
   LET v_tot_vigentes         = 0;
   LET v_nss                  = NULL;
   LET v_solic_marca_prc      = NULL;
   LET v_conf_marca_prc       = NULL;
   LET v_solic_desm_prc       = NULL;
   LET v_conf_desm_prc        = NULL;
   LET v_situacion            = NULL;
   LET v_tpo_credito          = NULL;
   LET v_identificador        = NULL;
   LET v_id_ocg_formalizacion = NULL;
   LET v_id_derechohabiente   = NULL;
   LET v_f_formalizacion      = NULL;


   -- OBTIENE CRÉDITOS VIGENTES 43BIS
   FOREACH
      SELECT dt.nss                   ,
             fz.id_ocg_formalizacion  ,
             fz.id_derechohabiente    ,
             ac.f_formalizacion       ,
             ac.f_solic_marca_prcr    ,
             ac.f_conf_marca_prcr     ,
             ac.f_solic_desmarca_prcr ,
             ac.f_conf_desmarca_prcr  ,
             fz.situacion             ,
             fz.tpo_credito
        INTO v_nss                    ,
             v_id_ocg_formalizacion   ,
             v_id_derechohabiente     ,
             v_f_formalizacion        ,
             v_solic_marca_prc        ,
             v_conf_marca_prc         ,
	           v_solic_desm_prc         ,
	           v_conf_desm_prc          ,
	           v_situacion              ,
	           v_tpo_credito
        FROM ocg_formalizacion fz,
             ocg_detalle dt,
             ocg_acreditado ac
       WHERE fz.id_ocg_detalle       = dt.id_ocg_detalle
         AND fz.cve_ent_financiera   = dt.cve_ent_financiera
         AND fz.id_ocg_formalizacion = ac.id_ocg_formalizacion
         AND fz.tpo_credito IN ('A','C','7','8')
         AND fz.situacion IN (55,60,70,80)
         AND fz.diagnostico = 1
         AND fz.situacion   = ac.situacion
         AND dt.subproceso  = 2

      -- IDENTIFICADOR DE QUE ESTÁ VIGENTE
			IF (v_situacion = 55) OR
			   (v_situacion = 60) OR
			   (v_situacion = 70) THEN 
			   LET v_identificador = 1;
			END IF

			IF (v_situacion = 80) THEN
			   LET v_identificador = 4;
		  END IF 

			-- BÚSCA EL ACREDITADO
			IF NOT EXISTS (
			   SELECT id_derechohabiente
			      FROM cre_acreditado
			     WHERE id_derechohabiente = v_id_derechohabiente
			       AND f_otorga > v_f_formalizacion ) THEN 
			   LET v_identificador = NULL;    
		  END IF

		  -- GUARDA REGISTRO EN TMP
		  INSERT INTO safre_tmp:ext_creditos_43bis
		                        (
															id_ocg_formalizacion ,
															id_ocg_liquidacion   ,
															id_derechohabiente   ,
															nss                  ,
															f_formalizacion      ,
															f_solic_marca_prcr   ,
															f_conf_marca_prcr    ,
															f_solic_desmarca_prcr,
															f_conf_desmarca_prcr ,
															situacion            ,
															tpo_credito          ,
															identificador        ,
															tpo_credito_saci     ,
															edo_credito_saci
														)
										 VALUES ( v_id_ocg_formalizacion ,
										          NULL                   ,
										          v_id_derechohabiente   ,
										          v_nss                  ,
										          v_f_formalizacion      ,
										          v_solic_marca_prc      ,
													    v_conf_marca_prc       ,
													    v_solic_desm_prc       ,
													    v_conf_desm_prc        ,
                              v_situacion            ,
                              v_tpo_credito          ,
                              v_identificador        ,
                              2                      ,
                              v_identificador
                            );

      -- INCREMENTA CONTADOR CRÉDITOS VIGENTES
      LET v_tot_vigentes = v_tot_vigentes + 1;
      
   END FOREACH;

   -- INICIALIZA VARIABLES PARA CRÉDITOS LIQUIDADOS
   LET v_tot_liquidados       = 0;
   LET v_nss                  = NULL;
   LET v_id_ocg_liquidacion   = NULL;
   LET v_id_ocg_formalizacion = NULL;
   LET v_id_derechohabiente   = NULL;
   LET v_f_formalizacion      = NULL;
   LET v_solic_marca_prc      = NULL;
   LET v_conf_marca_prc       = NULL;
   LET v_solic_desm_prc       = NULL;
   LET v_conf_desm_prc        = NULL;
   LET v_situacion            = NULL;
   LET v_tpo_credito          = NULL;
   LET v_identificador        = NULL;

   -- OBTIENE CRÉDITOS LIQUIDADOS 43BIS
   FOREACH
      SELECT dt.nss                   ,
             lq.id_ocg_liquidacion    ,
             lq.id_ocg_formalizacion  ,
             lq.id_derechohabiente    ,
             ac.f_formalizacion       ,
             ac.f_solic_marca_prcr    ,
             ac.f_conf_marca_prcr     ,
             ac.f_solic_desmarca_prcr ,
             ac.f_conf_desmarca_prcr  ,
             lq.situacion             ,
             lq.tpo_credito
        INTO v_nss                    ,
             v_id_ocg_liquidacion     ,
             v_id_ocg_formalizacion   ,
             v_id_derechohabiente     ,
             v_f_formalizacion        ,
             v_solic_marca_prc        ,
				 	   v_conf_marca_prc         ,
				 	   v_solic_desm_prc         ,
				 		 v_conf_desm_prc          ,
				 		 v_situacion              ,
				 		 v_tpo_credito
		    FROM ocg_liquidacion lq,
		         ocg_formalizacion fz,
			       ocg_detalle dt,
			       ocg_acreditado ac
			 WHERE lq.id_ocg_detalle = dt.id_ocg_detalle
			   AND lq.cve_ent_financiera = dt.cve_ent_financiera
			   AND lq.id_ocg_formalizacion = fz.id_ocg_formalizacion
			   AND lq.id_ocg_formalizacion = ac.id_ocg_formalizacion
			   AND lq.tpo_credito IN ('A','C','7','8')
			   AND lq.situacion IN (140,150,158,160)
			   AND lq.diagnostico = 1
			   AND lq.situacion   = ac.situacion
			   AND dt.subproceso  = 5

      -- RESETEA IDENTIFICADOR A LIQUIDADO
			LET v_identificador = 5;

			-- BÚSCA EL ACREDITADO
			IF NOT EXISTS (
			   SELECT id_derechohabiente
			      FROM cre_acreditado
			     WHERE id_derechohabiente = v_id_derechohabiente
			       AND f_otorga > v_f_formalizacion ) THEN 
			   LET v_identificador = NULL;    
		  END IF

		  -- GUARDA REGISTRO EN TMP
		  INSERT INTO safre_tmp:ext_creditos_43bis
		                        (
															id_ocg_formalizacion ,
															id_ocg_liquidacion   ,
															id_derechohabiente   ,
															nss                  ,
															f_formalizacion      ,
															f_solic_marca_prcr   ,
															f_conf_marca_prcr    ,
															f_solic_desmarca_prcr,
															f_conf_desmarca_prcr ,
															situacion            ,
															tpo_credito          ,
															identificador        ,
															tpo_credito_saci     ,
															edo_credito_saci
														)
										 VALUES ( v_id_ocg_formalizacion ,
										          v_id_ocg_liquidacion   ,
										          v_id_derechohabiente   ,
										          v_nss                  ,
										          v_f_formalizacion      ,
										          v_solic_marca_prc      ,
													    v_conf_marca_prc       ,
													    v_solic_desm_prc       ,
													    v_conf_desm_prc        ,
                              v_situacion            ,
                              v_tpo_credito          ,
                              v_identificador        ,
                              2                      ,
                              v_identificador
                            );

      -- INCREMENTA CONTADOR
      LET v_tot_liquidados = v_tot_liquidados + 1;
      
   END FOREACH;

   RETURN v_error,v_tot_vigentes,v_tot_liquidados;

END FUNCTION 
;


