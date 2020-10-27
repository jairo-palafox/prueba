






CREATE PROCEDURE "safreviv".sp_dis_apo_subs1(v_folio DECIMAL(9,0), v_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT ,CHAR(70);

--Última modificación 13112015
DEFINE v_f_transferencia     DATE;
DEFINE v_tot_registros       DECIMAL(10,0);
DEFINE v_tot_imp_apo_pat     DECIMAL(22,2);
DEFINE v_tot_imp_amo_cred    DECIMAL(22,2);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(70);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT; --estatus del proceso
DEFINE v_tot_imp_apo_aivs    DECIMAL(26,6);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

  --#Inicialización de variables
  LET v_bnd_proceso      = 0; --estado correcto
  LET v_f_transferencia  = TODAY;
  LET v_tot_registros    = 0.00;
  LET v_tot_imp_apo_pat  = 0.00;
  LET v_tot_imp_amo_cred = 0.00;
  LET v_bnd_transaccion  = 0;
  LET v_tot_imp_apo_aivs = 0.000000;
   
  SELECT f_transferencia,
         tot_registros,
         tot_imp_apo_pat,
         tot_imp_amo_cred,
         tot_aplic_int_viv_apo
  INTO   v_f_transferencia,
         v_tot_registros,
         v_tot_imp_apo_pat,
         v_tot_imp_amo_cred,
         v_tot_imp_apo_aivs        
  FROM   safre_tmp:tmp_dis_aposubs9;
    
  LET v_tot_imp_apo_pat  = (v_tot_imp_apo_pat  / 100);
  LET v_tot_imp_amo_cred = (v_tot_imp_amo_cred / 100);
  LET v_tot_imp_apo_aivs = (v_tot_imp_apo_aivs / 1000000); --se divide entre un millón

  INSERT INTO dis_ctr_ap_subsecuente VALUES (v_folio,
                                             v_f_transferencia,
                                             v_tot_registros,
                                             v_tot_imp_apo_pat,
                                             v_tot_imp_amo_cred,
                                             v_tot_imp_apo_aivs,
                                             v_edo_rech);

  LET v_char = "Terminado Apo_subs_SPL";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


