






CREATE FUNCTION "safreviv".fn_notifica_saldos(p_folio_proc_operativo  DECIMAL(9,0),
                                        p_folio_notificacion    DECIMAL(9,0))  -- crea la funcion con dos parametros de entrada
RETURNING SMALLINT, INTEGER, INTEGER, INTEGER

   DEFINE v_sms                    INTEGER;
   DEFINE v_correo                 INTEGER;
   DEFINE v_total                  INTEGER;
   DEFINE v_error                 SMALLINT;   -- almacena el resultado de salida en caso de error
   DEFINE v_nom_tabla             VARCHAR(100);  -- almacena el nombre de la tabla en donde se van a insertar los registros

   DEFINE v_tpo_notificacion      SMALLINT;   -- la variable guarda resultado de la consulta a la tabla afi_ind_notifica
   DEFINE v_mensaje               VARCHAR(200);  --Mensaje a enviar
   DEFINE v_query                 CHAR(500);

   DEFINE v_texto_tmp             VARCHAR(200);
   
   DEFINE v_texto1                VARCHAR(200);
   DEFINE v_texto2                VARCHAR(200);
   DEFINE v_texto3                VARCHAR(200);
   DEFINE v_texto4                VARCHAR(200);

   SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = p_folio_notificacion
   AND fol2.folio = p_folio_proc_operativo;

	 
	LET v_query = "DROP TABLE IF EXISTS " ||v_nom_tabla|| ";";
	EXECUTE IMMEDIATE v_query;
	
	LET v_query = "CREATE TABLE " ||v_nom_tabla|| "(folio decimal(9,0) not null ," ||
                                                   "nss char(11) not null ," ||
                                                   "f_saldo char(100) not null ,"||
                                                   "sdo_viv92 char(100) not null ,"||
                                                   "sdo_viv97 char(100) not null) "||
                                                   "FRAGMENT BY round robin in not_sdo_1_dbs, not_sdo_2_dbs " ||
                                                   "LOCK MODE ROW";
	EXECUTE IMMEDIATE v_query;
   
   SET PDQPRIORITY HIGH;

   LET v_query = "LOCK TABLE " || v_nom_tabla || " IN EXCLUSIVE MODE";
   EXECUTE IMMEDIATE v_query;

   LET v_query =  "INSERT INTO " || v_nom_tabla || " " ||
                  "SELECT " ||
                  p_folio_notificacion || ", " ||
                  "afi.nss, " ||
                  "to_char(today,'%d/%m/%y'),"||
                  "TRIM(TO_CHAR(sdo.sdo_viv92,'###,###,##&.&&')), " ||
                  "TRIM(TO_CHAR(sdo.sdo_viv97,'###,###,##&.&&')) " ||
                  "FROM not_saldo_diario sdo " || 
                  "INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = sdo.id_derechohabiente "  ||
                   "WHERE  (sdo.sdo_viv92 + sdo.sdo_viv97) > 100";
   EXECUTE IMMEDIATE v_query;

   LET v_query = "UNLOCK TABLE " || v_nom_tabla;
   EXECUTE IMMEDIATE v_query;
   
   LET v_query = "create index ix_1_" || v_nom_tabla || " on " || v_nom_tabla || "(nss) using btree in not_sdo_ix_dbs";
   EXECUTE IMMEDIATE v_query;
   
	LET v_query = "UPDATE STATISTICS FOR TABLE " ||v_nom_tabla|| ";";
	EXECUTE IMMEDIATE v_query;

   --Se obtiene el numero de registros para SMS
   LET v_query = "SELECT COUNT(*) FROM " || v_nom_tabla;
   PREPARE exe_consulta FROM v_query;
   DECLARE cur_consulta CURSOR FOR exe_consulta;
   OPEN cur_consulta; --exe_consulta;
   FETCH cur_consulta INTO v_sms;
   CLOSE cur_consulta;
   FREE cur_consulta;
   FREE exe_consulta;
   
   --Se obtiene el numero de registros para Correo
   LET v_query = "SELECT COUNT(*) FROM " || v_nom_tabla;
   PREPARE exe_consulta2 FROM v_query;
   DECLARE cur_consulta2 CURSOR FOR exe_consulta2;
   OPEN cur_consulta2; --exe_consulta;
   FETCH cur_consulta2 INTO v_correo;
   CLOSE cur_consulta2;
   FREE cur_consulta2;
   FREE exe_consulta2;
   
   --Se obtiene el total de registros para el folio operativo
   LET v_total = v_sms + v_correo;
	
   RETURN 0, v_sms, v_correo, v_total;
END FUNCTION;


