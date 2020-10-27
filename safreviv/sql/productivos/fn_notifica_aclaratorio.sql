






CREATE FUNCTION "safreviv".fn_notifica_aclaratorio(p_folio_proc_operativo  DECIMAL(9,0),
                                        p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_error                 SMALLINT;   -- almacena el resultado de salida en caso de error
   DEFINE v_nom_tabla             VARCHAR(100);  -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_nss                   CHAR(11);    -- almacena el nss de la consulta a la tabla afi_derechohabiente y se ocupa dentro del foreach
   DEFINE v_tpo_aclaracion        CHAR(2);     -- almacena el tpo_aclaracion de la consulta a la tabla cta_his_pagos y se ocupa dentro del foreach

   DEFINE v_tpo_notificacion      SMALLINT;   -- la variable guarda resultado de la consulta a la tabla afi_ind_notifica
   DEFINE v_mensaje               VARCHAR(200);  --Mensaje a enviar
   DEFINE v_query                 CHAR(500);

   DEFINE v_texto_tmp             VARCHAR(200);
   DEFINE v_texto1                VARCHAR(200);
   DEFINE v_texto2                VARCHAR(200);
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
	
	LET v_query = 'CREATE TABLE ' ||v_nom_tabla|| '(folio decimal(9,0) not null ,' ||
                                                   'nss char(11) not null ,' ||
                                                   'causal char(100) not null)' ||
                                                   'IN not_2_dbs';
	EXECUTE IMMEDIATE v_query;

   SET PDQPRIORITY HIGH;

   LET v_query = 'LOCK TABLE ' || v_nom_tabla || ' IN EXCLUSIVE MODE';
   EXECUTE IMMEDIATE v_query;
   
	FOREACH
      {SELECT DISTINCT (afi.nss|| cta.tpo_aclaracion),
                       afi.nss,
                       cta.tpo_aclaracion
      INTO v_comodin,v_nss, v_tpo_aclaracion
      FROM cta_his_pagos cta
      INNER JOIN afi_ind_notifica ind ON ind.id_derechohabiente = cta.id_derechohabiente
      INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = cta.id_derechohabiente
      WHERE cta.folio = p_folio_proc_operativo
      AND cta.origen_archivo = 1    --Archivo de LQ
      AND cta.ind_liquidacion = 1   --Indicador de no liquidado por Aclaracion
      AND TO_NUMBER(cta.tpo_aclaracion) <= 26
      AND cta.tpo_aclaracion NOT IN ('08','09','13','17','25')
      AND ind.tpo_notificacion in (1,2)}

      SELECT
            distinct (afi.nss||cta.tpo_aclaracion), 
            afi.nss,
            cta.tpo_aclaracion
      INTO 
           v_comodin,
           v_nss, 
           v_tpo_aclaracion
      FROM cta_his_pagos cta ,
           afi_ind_notifica ind,
           afi_derechohabiente afi
      WHERE cta.folio = p_folio_proc_operativo
      AND cta.origen_archivo = 1    --Archivo de LQ
      AND cta.ind_liquidacion = 1   --Indicador de no liquidado por Aclaracion
      AND TO_NUMBER(cta.tpo_aclaracion) <= 26
      AND ind.id_derechohabiente = cta.id_derechohabiente
      AND ind.tpo_notificacion in (1,2)
      AND afi.id_derechohabiente = ind.id_derechohabiente
               
      --FROM cta_his_pagos cta
      --INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = cta.id_derechohabiente
      --WHERE cta.id_Derechohabiente in (select id_Derechohabiente from afi_ind_notifica where tpo_notificacion in (1,2))
      --AND cta.origen_archivo = 1    --Archivo de LQ
      --AND cta.ind_liquidacion = 1   --Indicador de no liquidado por Aclaracion
      --AND TO_NUMBER(cta.tpo_aclaracion) <= 26
      --AND cta.tpo_aclaracion NOT IN ('08','09','13','17','25')
      --and cta.folio = p_folio_proc_operativo
               
      LET v_query = 'INSERT INTO ' || v_nom_tabla || '(folio,nss,causal) VALUES ('
                    || p_folio_notificacion || ',"' || v_nss || '","' || v_tpo_aclaracion ||'")';
      EXECUTE IMMEDIATE v_query;
	END FOREACH;

   LET v_query = 'UNLOCK TABLE ' || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;

   LET v_query = 'create index ix_1_' || v_nom_tabla || ' on ' || v_nom_tabla || '(nss) using btree in not_ix_2_dbs';
   EXECUTE IMMEDIATE v_query;
   
	LET v_query = 'UPDATE STATISTICS FOR TABLE ' ||v_nom_tabla|| ';';
	EXECUTE IMMEDIATE v_query;

   
   --Se obtiene el numero de registros 
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
   FROM cta_his_pagos
   WHERE folio = p_folio_proc_operativo;

   SET PDQPRIORITY DEFAULT;
	
   RETURN 0, v_correo, v_correo, v_total;
END FUNCTION;


