






CREATE PROCEDURE "safreviv".sp_dis_rev_transaccion11()
RETURNING SMALLINT, SMALLINT, CHAR(70), DECIMAL(9,0)

--Última modificación 18122014
--Declaración de variables
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(50);
DEFINE v_folio_liquida   	 DECIMAL(9,0);

DEFINE v_id_derechohabiente	 DECIMAL(9,0);
DEFINE v_id_referencia	 	 DECIMAL(9,0);
DEFINE v_folio_liquida_orig	 DECIMAL(9,0);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_folio_liquida ;
END EXCEPTION

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_transaccion11.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_transaccion11.TRACE';
--SET DEBUG FILE TO '/ds/safreviv/dis/sql/fn_dis_transaccion11.TRACE';
--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-562/fn_dis_transaccion11.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_folio_liquida = 0;
LET v_bnd_proceso = 0;
LET v_char = "";
LET v_id_derechohabiente = 0;
LET v_id_referencia = 0;
LET v_folio_liquida_orig = 0;

	SELECT FIRST 1 folio_liquida 
	INTO v_folio_liquida
	FROM dis_liq_inconsistente;
   
	EXECUTE PROCEDURE sp_dis_rev_compensa_avance(v_folio_liquida);
  
  
    FOREACH 
		SELECT id_derechohabiente, id_referencia, folio_liquida_orig
		INTO v_id_derechohabiente, v_id_referencia, v_folio_liquida_orig
		FROM dis_liq_inconsistente
	
	
		UPDATE dis_info_inconsistente
		SET tpo_inconsistente = 0
		WHERE id_derechohabiente = v_id_derechohabiente
		AND   id_referencia = v_id_referencia
		AND   folio_liquida = v_folio_liquida_orig
		AND   tpo_inconsistente = 10;
	
	END FOREACH;
	
	DELETE FROM cnt_transaccion
	WHERE folio_liquida = v_folio_liquida;

	DELETE FROM cta_movimiento
	WHERE folio_liquida = v_folio_liquida;

   --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
   LET v_char = " Retorno de información correctamente";
   RETURN v_bnd_proceso , 0 , v_char, v_folio_liquida;

END PROCEDURE;


