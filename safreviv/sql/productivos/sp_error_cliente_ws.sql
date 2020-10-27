






CREATE PROCEDURE "safreviv".sp_error_cliente_ws(
			p_cve_cliente			LIKE safre_viv:wsv_his_err_cliente.cve_cliente,
			p_f_error				LIKE safre_viv:wsv_his_err_cliente.f_error,
			p_total_intentos		LIKE safre_viv:wsv_his_err_cliente.total_intentos,
			p_ws_status			LIKE safre_viv:wsv_his_err_cliente.ws_status,
			p_cod_error			LIKE safre_viv:wsv_his_err_cliente.cod_error,
			p_desc_error			LIKE safre_viv:wsv_his_err_cliente.desc_error,
			p_usuario				LIKE safre_viv:wsv_his_err_cliente.usuario);
	
	INSERT INTO safre_viv:wsv_his_err_cliente VALUES (safre_viv:seq_wsv_his_error_cliente.NEXTVAL,			--id_err_cliente
									p_cve_cliente,							--cve_cliente
									p_f_error,							--f_error
									p_total_intentos,						--total_intentos
									p_ws_status,							--ws_status
									p_cod_error,							--cod_error
									p_desc_error,							--desc_error
									p_usuario								--usuario
									);
END PROCEDURE;


