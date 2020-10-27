






CREATE PROCEDURE "safreviv".sp_dis_rev_cred_cero_carga(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT ,CHAR(70);

--Última modificación 10032015
--Declaración de variables
	DEFINE v_status         	SMALLINT;
	DEFINE sql_err          	INTEGER ;
	DEFINE isam_err         	INTEGER ;
	DEFINE error_info       	CHAR(70);
	DEFINE  v_char           	CHAR(30);
	DEFINE v_bnd_proceso        SMALLINT;       --Estatus del proceso

	
	ON EXCEPTION
	    SET sql_err, isam_err, error_info
			 LET v_status = sql_err;
	    RETURN  v_status ,isam_err , error_info;  
	END EXCEPTION
	
	LET v_bnd_proceso = 0; --Estado correcto
	
	--Elimina registros de dis_liq_inconsistente
	DELETE FROM safre_viv:dis_liq_inconsistente
	WHERE folio_arh_num_cred = p_folio;
	
	--Elimina registros de dis_arh_num_cred_0
	DELETE FROM safre_viv:dis_arh_num_cred_0
	WHERE folio = p_folio;

	UPDATE statistics FOR TABLE dis_liq_inconsistente;
   UPDATE statistics FOR TABLE dis_arh_num_cred_0;

	LET v_char = "Reverso de carga de archivo de créditos ceros correcto";
	RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


