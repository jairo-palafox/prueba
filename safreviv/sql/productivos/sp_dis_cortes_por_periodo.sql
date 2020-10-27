






CREATE PROCEDURE "safreviv".sp_dis_cortes_por_periodo(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 21062013
--Declaración de variables
   DEFINE v_periodo_pago     CHAR(6);
   DEFINE v_tot_imp_ap_pat   DECIMAL(22,2);
   DEFINE v_tot_imp_am_cre   DECIMAL(22,2);
   DEFINE v_tot_aiv_ap_pat   DECIMAL(22,2);
   DEFINE v_tot_cuentas	     DECIMAL(10,0);
   DEFINE v_fecha_liquida    DATE;
   DEFINE v_status           SMALLINT;
   DEFINE sql_err            INTEGER ;
   DEFINE isam_err           INTEGER ;
   DEFINE error_info         CHAR(70);
   DEFINE v_char             CHAR(20);
	
   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_status = sql_err;
      RETURN  v_status ,isam_err , error_info;
   END EXCEPTION

   --Inicializa
   LET v_periodo_pago   = "";
   LET v_tot_imp_ap_pat = 0.00;
   LET v_tot_imp_am_cre = 0.00;
   LET v_tot_aiv_ap_pat = 0.00;
   LET v_tot_cuentas    = 0;
   LET v_fecha_liquida  = TODAY;

   FOREACH
     SELECT periodo_pago, 
            SUM(imp_ap_pat) apo, 
            SUM(imp_am_cre) amo, 
            SUM(aiv_ap_pat) aiv,
            COUNT(*)
     INTO   v_periodo_pago, 
            v_tot_imp_ap_pat,
            v_tot_imp_am_cre,
            v_tot_aiv_ap_pat,
            v_tot_cuentas
     FROM   dis_interface_hs
     WHERE  folio_liquida = p_folio
     GROUP BY 1
     ORDER BY 1 DESC
		
     --Insertamos en la tabla de dis_his_hs
     INSERT INTO dis_his_hs
		 (folio, 
                  f_liquida, 
                  periodo_pago, 
                  monto_aportacion, 
                  monto_amortizacion, 
                  monto_apo_aivs,
                  cuentas)
            VALUES
	         (p_folio,
                  v_fecha_liquida,
                  v_periodo_pago,
                  v_tot_imp_ap_pat,
                  v_tot_imp_am_cre,
                  v_tot_aiv_ap_pat,
                  v_tot_cuentas);

   END FOREACH;

   UPDATE statistics FOR TABLE dis_his_hs;

   LET v_char = "Cortes por periodo correctos.";

   RETURN 0, 0, v_char;

END PROCEDURE;


