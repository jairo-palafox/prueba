






CREATE FUNCTION "safreviv".fn_notifica_pagos_inf(p_folio_proc_operativo  DECIMAL(9,0),
                                      p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_nom_tabla              VARCHAR(100);    -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                    CHAR(11);        
   DEFINE v_imp_ap_pat             DECIMAL(12,2);   
   DEFINE v_imp_am_cre             DECIMAL(12,2);   
   DEFINE v_bimestre               CHAR(8);         
   DEFINE v_tpo_notificacion       SMALLINT;
   
   DEFINE v_mensaje                VARCHAR(200);
   DEFINE v_query                  CHAR(500);

   DEFINE v_con                   INTEGER;
   DEFINE v_texto_tmp             VARCHAR(200);

   DEFINE v_texto2_1              VARCHAR(200);
   DEFINE v_texto2_2              VARCHAR(200);
   DEFINE v_texto2_3              VARCHAR(200);
   DEFINE v_texto2_4              VARCHAR(200);

	SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

	 
	LET v_query = 'DROP TABLE IF EXISTS ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| '(folio decimal(9,0) not null ,' ||
                                                   'nss char(11) not null ,' ||
                                                   'aportacion char(100) not null,' ||
                                                   'bimestre   char(100) not null,' ||
                                                   'ejercicio  char(100) not null)' ||
                                                   'in not_1_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;
	
	FOREACH 
		{SELECT afi.nss,
			   cta.imp_ap_pat,
			   cta.imp_am_cre,
			   fn_bimestre_pago(cta.periodo_pago),
			   ind.tpo_notificacion
		  INTO v_nss,
			   v_imp_ap_pat,
			   v_imp_am_cre,
			   v_bimestre,
			   v_tpo_notificacion
		  FROM afi_derechohabiente afi, cta_his_pagos cta, afi_ind_notifica ind
		 WHERE cta.id_derechohabiente = afi.id_derechohabiente
           AND cta.id_derechohabiente = ind.id_derechohabiente
	       AND cta.folio = p_folio_proc_operativo
           AND cta.origen_archivo = 3           --Pagos solo infonavit
           AND cta.ind_liquidacion IN (0,2,3,5)     --Registros pagados
           ---------------------------------------------------------------------
           AND ind.tpo_notificacion IN (1,2)}
		   ---------------------------------------------------------------------

           SELECT afi.nss,
                  cta.imp_ap_pat,
                  cta.imp_am_cre,
                  fn_bimestre_pago(cta.periodo_pago)
           INTO   v_nss,
			      v_imp_ap_pat,
			      v_imp_am_cre,
                  v_bimestre
           --FROM cta_his_pagos cta
           --     INNER JOIN afi_derechohabiente afi on afi.id_Derechohabiente=cta.id_derechohabiente
           --WHERE cta.id_derechohabiente IN (Select id_derechohabiente from afi_ind_notifica where tpo_notificacion in (1,2))
           --AND cta.folio =p_folio_proc_operativo
           --AND cta.origen_archivo = 3           --Pagos solo infonavit
           --AND cta.ind_liquidacion IN (0,2,3,5)     --Registros pagados

           FROM cta_his_pagos cta,
                afi_ind_notifica ind,
                afi_derechohabiente afi
           WHERE cta.folio =p_folio_proc_operativo
             AND cta.origen_archivo = 3           --Pagos solo infonavit
             AND cta.ind_liquidacion IN (0,2,3,5)     --Registros pagados
             AND ind.id_derechohabiente=cta.id_derechohabiente
             AND ind.tpo_notificacion IN (1,2)
             AND afi.id_derechohabiente=ind.id_derechohabiente

		IF (v_imp_ap_pat <> 0 AND v_imp_ap_pat IS NOT NULL) THEN
			IF (v_imp_am_cre = 0 OR v_imp_am_cre IS NULL) THEN
                    LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss,aportacion,bimestre,ejercicio) VALUES ('
                                || p_folio_notificacion || ',"' || v_nss || '","' || TO_CHAR(v_imp_ap_pat,"###,###,##&.&&") || '","' || v_bimestre[5,6] ||'","'||v_bimestre[1,4]||'")';
                    EXECUTE IMMEDIATE v_query;            
            END IF;
        END IF;
            
	END FOREACH;

   LET v_query = 'UNLOCK TABLE ' || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;

   LET v_query = 'create index ix_1_' || v_nom_tabla || ' on ' || v_nom_tabla || '(nss) using btree in not_ix_2_dbs';
   EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'UPDATE STATISTICS FOR TABLE ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
   
   --Se obtiene el numero de registros para Correo
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla;
   PREPARE exe_consulta2 FROM v_query;
   DECLARE cur_consulta2 CURSOR FOR exe_consulta2;
   OPEN cur_consulta2; --exe_consulta;
   FETCH cur_consulta2 INTO v_correo;
   CLOSE cur_consulta2;
   FREE cur_consulta2;
   FREE exe_consulta2;
   
   Let v_total=v_correo;

   SET PDQPRIORITY DEFAULT;
	
   RETURN 0, v_correo, v_correo, v_total;
   
END FUNCTION;


