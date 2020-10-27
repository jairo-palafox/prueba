






CREATE FUNCTION "safreviv".fn_notifica_pagos_omisos(p_folio_proc_operativo  DECIMAL(9,0),
                                         p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada 
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_nom_tabla              VARCHAR(100,0);  -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                    CHAR(11);
   DEFINE v_periodo                CHAR(2);
   DEFINE v_ejercicio              CHAR(4);
   DEFINE v_nrp                    CHAR(11);
   DEFINE v_tpo_notificacion       SMALLINT;

   DEFINE v_mensaje                VARCHAR(200);
   DEFINE v_query                  CHAR(500);

   DEFINE v_con                    INTEGER;
   DEFINE v_texto_tmp              VARCHAR(200);

   DEFINE v_texto1                 VARCHAR(200);
   DEFINE v_texto2                 VARCHAR(200);
   DEFINE v_texto3                 VARCHAR(200);

	SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

	 
	LET v_query = 'DROP TABLE IF EXISTS ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| '   (folio decimal(9,0) not null ,' ||
                                                      'nss       char(11)  not null ,' ||
                                                      'bimestre  char(100) not null ,' ||
                                                      'ejercicio char(100) not null ,' ||
                                                      'nrp       char(100) not null)' ||
                                                      'in not_3_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;
   
	FOREACH
		SELECT det.nss,
			   det.periodo,
			   det.ejercicio,
               det.nrp
		  INTO v_nss,
			   v_periodo,
			   v_ejercicio,
               v_nrp
          FROM  not_det_omisos_trm det ,
                afi_ind_notifica ind,
                afi_derechohabiente afi
         WHERE  det.folio = p_folio_proc_operativo
           AND  ind.id_derechohabiente = det.id_derechohabiente
           AND  ind.tpo_notificacion in (1,2)
           AND  afi.id_derechohabiente = ind.id_derechohabiente

			LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss,bimestre,ejercicio,nrp) VALUES ('
										  || p_folio_notificacion || ',"' || v_nss || '","' || TRIM(v_periodo) || '","' || TRIM(v_ejercicio) ||'","'||TRIM(v_nrp)||'")';
		
			EXECUTE IMMEDIATE v_query;
		
	END FOREACH;

   LET v_query = 'UNLOCK TABLE ' || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;

   LET v_query = 'create index ix_1_' || v_nom_tabla || ' on ' || v_nom_tabla || '(nss) using btree in not_ix_1_dbs';
   EXECUTE IMMEDIATE v_query;
   
	LET v_query = 'UPDATE STATISTICS FOR TABLE ' || v_nom_tabla || ';';
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
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla ;
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
   FROM not_det_omisos_trm
   WHERE folio = p_folio_proc_operativo;

   SET PDQPRIORITY DEFAULT;
	
RETURN 0, v_sms, v_correo, v_total;
END FUNCTION;


