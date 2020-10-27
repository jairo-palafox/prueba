






CREATE FUNCTION "safreviv".fn_notifica_resoluciones_retiros(p_folio_proc_operativo  DECIMAL(9,0),
                                      p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_nom_tabla              VARCHAR(100);    -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                    CHAR(11);        
  
   DEFINE v_tpo_notificacion       SMALLINT;
   
   DEFINE v_mensaje_sms            VARCHAR(200);
   DEFINE v_mensaje_correo         VARCHAR(200); 
   DEFINE v_query                  CHAR(500);


   SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

    SELECT texto
    INTO v_mensaje_sms
    FROM cat_not_plantilla
    WHERE cod_plantilla = 8;

    SELECT texto
    INTO v_mensaje_correo
    FROM cat_not_plantilla
    WHERE cod_plantilla = 9;

    LET v_query = 'DROP TABLE IF EXISTS ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| '(folio decimal(9,0) not null ,' ||
                                                   'nss char(11) not null ) ' ||
                                                   'in not_2_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;

   FOREACH 
		SELECT ret.nss
		  INTO v_nss
		  FROM ret_notificacion ret,
                       afi_ind_notifica ind,
                       afi_derechohabiente afi
                  WHERE ret.folio_notificacion=p_folio_proc_operativo
                    AND ret.ind_envio is null
		    AND ind.id_derechohabiente = ret.id_derechohabiente
                    AND ind.tpo_notificacion in (1,2)
		    AND afi.id_derechohabiente = ind.id_derechohabiente
          
		
        LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss) VALUES ('
                                     ||p_folio_notificacion||',"'||v_nss || '")';
		
        EXECUTE IMMEDIATE v_query;

        UPDATE ret_notificacion 
         set ind_envio=1,
             f_envio=today 
         where nss=v_nss
         AND   folio_notificacion=p_folio_proc_operativo;
        
   END FOREACH

   {FOREACH 
		SELECT ret.nss
		  INTO v_nss
		  FROM ret_notificacion ret, afi_ind_notifica ind
		  WHERE  ind.id_derechohabiente=ret.id_derechohabiente
          AND ind.tpo_notificacion = 2
          AND folio_notificacion=p_folio_proc_operativo
          AND ret.ind_envio is null
		
        LET v_query = 'INSERT INTO ' || v_nom_tabla || '(nss) VALUES ('
                                     || '"' || v_nss || '"' ||')';
		
        EXECUTE IMMEDIATE v_query;

        UPDATE ret_notificacion 
        set ind_envio=1,
            f_envio=today 
        where nss=v_nss
        AND   folio_notificacion=p_folio_proc_operativo;

    END FOREACH}

   UPDATE ret_notificacion 
   set ind_envio=2,
        f_envio=today
   where ind_envio is null 
   AND   folio_notificacion=p_folio_proc_operativo;
   
   
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
   LET v_query = 'SELECT COUNT(*) FROM ' || v_nom_tabla ;
   PREPARE exe_consulta2 FROM v_query;
   DECLARE cur_consulta2 CURSOR FOR exe_consulta2;
   OPEN cur_consulta2; --exe_consulta;
   FETCH cur_consulta2 INTO v_correo;
   CLOSE cur_consulta2;
   FREE cur_consulta2;
   FREE exe_consulta2;

   LET v_total=v_sms+v_correo;
   
   RETURN 0, v_sms, v_correo, v_total;
END FUNCTION;


