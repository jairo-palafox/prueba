






CREATE FUNCTION "safreviv".fn_notifica_voluntarias(p_folio_proc_operativo  DECIMAL(9,0),
                                        p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                INTEGER;
   DEFINE v_correo             INTEGER;
   DEFINE v_total              INTEGER;
   DEFINE v_nom_tabla          VARCHAR(100,0);  -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                CHAR(11);        -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_imp_ap_vol         DECIMAL(12,2);   -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_f_pago             DATE;            -- estas variables  almacenan los campos q se ocupan dentro del foreach
   DEFINE v_fecha              VARCHAR(80);
   DEFINE v_mes_desc           VARCHAR(10);
   DEFINE v_tpo_notificacion   SMALLINT;

   DEFINE v_mensaje                VARCHAR(200);
   DEFINE v_query                  CHAR(500);

   
   DEFINE v_texto_tmp             VARCHAR(200);

   DEFINE v_texto1              VARCHAR(200);
   DEFINE v_texto2              VARCHAR(200);
   DEFINE v_texto3              VARCHAR(200);

	SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

  
	 
	LET v_query = 'DROP TABLE IF EXISTS ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| ' (folio decimal(9,0) not null ,' ||
                                                      'nss char(11) not null ,' ||
                                                      'aportacion char (100) not null ,' ||
                                                      'bimestre char(100) not null, ' ||
                                                      'ejercicio char(100) not null) '||
                                                      'in not_3_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;
	 
	FOREACH
		SELECT afi.nss, apvol.imp_ap_vol, apvol.f_pago, ind.tpo_notificacion
		  INTO v_nss, v_imp_ap_vol, v_f_pago, v_tpo_notificacion
		  FROM pag_det_apvol apvol, afi_derechohabiente afi, afi_ind_notifica ind
		 WHERE afi.id_derechohabiente = apvol.id_derechohabiente 
		   AND ind.id_derechohabiente = apvol.id_derechohabiente
		   AND apvol.folio = p_folio_proc_operativo
           ---------------------------------------------------------------------
           AND ind.tpo_notificacion IN (1,2)
           ---------------------------------------------------------------------
        SELECT mes_desc
    	  INTO v_mes_desc 
		  FROM cat_mes
		 WHERE mes = MONTH(v_f_pago);
		 
    --  LET v_fecha = DAY(v_f_pago) || " de " || TRIM(v_mes_desc) || " de " || YEAR(v_f_pago);
    --  LET v_mensaje = TRIM(v_texto1) || TRIM(TO_CHAR(v_imp_ap_vol,"###,###,##&.&&")) || TRIM(v_texto2) || ' ' || TRIM(v_fecha) || TRIM(v_texto3);
		
	--	IF (v_mensaje IS NOT NULL) THEN
			LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss,aportacion,bimestre,ejercicio) VALUES ('
									  || p_folio_notificacion  || ',"' || v_nss || '","' || TRIM(TO_CHAR(v_imp_ap_vol,"###,###,##&.&&")) || '","' || TRIM(v_mes_desc) ||'","'||YEAR(v_f_pago) ||'")';
                
			EXECUTE IMMEDIATE v_query;
	--	END IF;
	END FOREACH;

   LET v_query = 'UNLOCK TABLE ' || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;

   LET v_query = 'create index ix_1_' || v_nom_tabla || ' on ' || v_nom_tabla || '(nss) using btree in not_ix_2_dbs';
   EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'UPDATE STATISTICS FOR TABLE ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
   --Se obtiene el numero de registros para SMS
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla ;
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
   FROM pag_det_apvol
   WHERE folio = p_folio_proc_operativo;

   SET PDQPRIORITY DEFAULT;
	
   RETURN 0, v_sms, v_correo, v_total;
END FUNCTION;


