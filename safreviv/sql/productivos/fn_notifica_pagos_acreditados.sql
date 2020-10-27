






CREATE FUNCTION "safreviv".fn_notifica_pagos_acreditados(p_folio_proc_operativo  DECIMAL(9,0),
                                  p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada								  
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_nom_tabla              VARCHAR(100,0);  -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                    CHAR(11);        -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_imp_ap_pat             DECIMAL(12,2);   -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_imp_am_cre             DECIMAL(12,2);   -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_bimestre               CHAR(8);         -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_tpo_notificacion       SMALLINT;
   
   DEFINE v_mensaje                VARCHAR(200);
   DEFINE v_query                  CHAR(500);

   DEFINE v_con                   INTEGER;
   DEFINE v_texto_tmp             VARCHAR(200);

   DEFINE v_texto1_1              VARCHAR(200);
   DEFINE v_texto1_2              VARCHAR(200);
   DEFINE v_texto1_3              VARCHAR(200);
   DEFINE v_texto1_4              VARCHAR(200);
   DEFINE v_texto1_5              VARCHAR(200);


   DEFINE v_texto3_1              VARCHAR(200);
   DEFINE v_texto3_2              VARCHAR(200);
   DEFINE v_texto3_3              VARCHAR(200);
   DEFINE v_texto3_4              VARCHAR(200);
   DEFINE v_comodin               VARCHAR(100);

   SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

	LET v_query = 'DROP TABLE IF EXISTS ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| '(' ||
							'folio decimal(9,0) not null, ' ||
                     'nss char(11) not null, ' ||
                     'aportacion char(100) not null, ' ||
                     'amortizacion char(100) not null, '||
                     'bimestre char(100) not null, ' ||
                     'ejercicio char(100) not null)'||
                     'in not_1_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;
					  
	FOREACH 
		--SELECT
          --     distinct   (afi.nss||
            --               cta.imp_ap_pat||
        --                   cta.imp_am_cre||
        --                   fn_bimestre_pago(cta.periodo_pago)),
        --       afi.nss,
	--		   cta.imp_ap_pat,
--			   cta.imp_am_cre,
	--		   fn_bimestre_pago(cta.periodo_pago)
--		  INTO 
  --             v_comodin,
   --            v_nss,
--			   v_imp_ap_pat,
--			   v_imp_am_cre,
--			   v_bimestre			   
--		  FROM afi_derechohabiente afi, cta_his_pagos cta, afi_ind_notifica ind
--		 WHERE cta.id_derechohabiente = afi.id_derechohabiente 
----		   AND cta.id_derechohabiente =ind.id_derechohabiente  
   --      AND cta.folio = p_folio_proc_operativo
    --     AND cta.origen_archivo = 1             --Pagos LQINFO
     --    AND cta.ind_liquidacion IN (0,2,3,5)   --Registros pagados
      --   -----------------------------------------------------------------------
       --  AND ind.tpo_notificacion IN (1,2)
         -----------------------------------------------------------------------

        SELECT
               afi.nss,
               cta.imp_ap_pat,
               cta.imp_am_cre,
               fn_bimestre_pago(cta.periodo_pago)
        INTO 
               v_nss,
     	       v_imp_ap_pat,
	       v_imp_am_cre,
	       v_bimestre		
        FROM  cta_his_pagos cta,
              afi_ind_notifica ind,
              afi_derechohabiente afi
        WHERE cta.folio = p_folio_proc_operativo
          AND cta.origen_archivo = 1             --Pagos LQINFO
          AND cta.ind_liquidacion IN (0,2,3,5)   --Registros pagados
          AND ind.id_derechohabiente = cta.id_derechohabiente
          AND ind.tpo_notificacion IN (1,2)
          AND afi.id_derechohabiente = ind.id_derechohabiente

        --FROM  cta_his_pagos cta
        --      INNER JOIN afi_derechohabiente afi on afi.id_Derechohabiente = cta.id_derechohabiente
        --WHERE cta.id_derechohabiente in (select id_derechohabiente from afi_ind_notifica where tpo_notificacion IN (1,2))
        --AND cta.folio = 15546
        --AND cta.origen_archivo = 1             --Pagos LQINFO
        --AND cta.ind_liquidacion IN (0,2,3,5)   --Registros pagados

         
		IF (v_imp_ap_pat <> 0 AND v_imp_ap_pat IS NOT NULL) THEN
			IF (v_imp_am_cre <> 0 AND v_imp_am_cre IS NOT NULL) THEN
                LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss,aportacion,amortizacion,bimestre,ejercicio) VALUES ('
										  || p_folio_notificacion || ',"' || v_nss || '","' ||TRIM(TO_CHAR(v_imp_ap_pat,"###,###,##&.&&"))||'","'|| TRIM(TO_CHAR(v_imp_am_cre,"###,###,##&.&&"))||'","'|| v_bimestre[5,6]||'","'|| v_bimestre[1,4] ||'")';
                EXECUTE IMMEDIATE v_query;
			END IF;
		END IF;											
	END FOREACH;

   LET v_query = 'UNLOCK TABLE ' || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;

   LET v_query = 'create index ix_1_' || v_nom_tabla || ' on ' || v_nom_tabla || '(nss) using btree in not_ix_1_dbs';
   EXECUTE IMMEDIATE v_query;
   
	LET v_query = 'UPDATE STATISTICS FOR TABLE ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
   --Se obtiene el numero de registros para SMS
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla;
   PREPARE exe_consulta FROM v_query;
   DECLARE cur_consulta CURSOR FOR exe_consulta;
   OPEN cur_consulta; --exe_consulta;
   FETCH cur_consulta INTO v_sms;
   CLOSE cur_consulta;
   FREE cur_consulta;
   FREE exe_consulta;
   
   --Se obtiene el numero de registros para Correo
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla;
   PREPARE exe_consulta2 FROM v_query;
   DECLARE cur_consulta2 CURSOR FOR exe_consulta2;
   OPEN cur_consulta2; --exe_consulta;
   FETCH cur_consulta2 INTO v_correo;
   CLOSE cur_consulta2;
   FREE cur_consulta2;
   FREE exe_consulta2;
   
   --Se obtiene el total de registros para el folio operativo
   SELECT COUNT(*)
   INTO v_total
   FROM cta_his_pagos
   WHERE folio = p_folio_proc_operativo;

   SET PDQPRIORITY DEFAULT;
	
   RETURN 0, v_sms, v_correo, v_total;
END FUNCTION;


