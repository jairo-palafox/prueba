






CREATE PROCEDURE "safreviv".sp_rev_dis_can_par_ap(p_folio DECIMAL(9,0))
--RETURNING SMALLINT, SMALLINT ,CHAR(70);
RETURNING SMALLINT;

--Última modificación 13112018
--Declaración de variables
DEFINE v_id_derechohabiente  DECIMAL(9,0); --Id derechohabiente segun nss
DEFINE v_periodo_pago 		 VARCHAR(06); 
DEFINE v_num_credito 		 CHAR(10);     --Número de credito
DEFINE v_nrp 				 CHAR(11);     --Numero de registro patronal
DEFINE v_status         	 SMALLINT;
DEFINE sql_err          	 INTEGER ;
DEFINE isam_err         	 INTEGER ;
DEFINE error_info       	 CHAR(70);
DEFINE v_char           	 CHAR(20);
DEFINE v_bnd_proceso         SMALLINT;     --Estatus del proceso
DEFINE v_tpo_avance			 SMALLINT;
DEFINE v_monto_dif_apo       DECIMAL(12,2);
DEFINE v_monto_dif_amo       DECIMAL(12,2);

{ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info;  
END EXCEPTION}
	
LET v_bnd_proceso        = 0; --Estado correcto
LET v_id_derechohabiente = 0;
LET v_num_credito        = "";
LET v_periodo_pago       = "";
LET v_nrp                = "";
LET v_tpo_avance         = 0;
LET v_monto_dif_apo      = 0;
LET v_monto_dif_amo      = 0;
	
  FOREACH
    --Obtenemos el nrp de dis_det_avance_pago
    SELECT id_derechohabiente, 
           periodo_pago, 
           nrp, 
           monto_dif_apo, 
           monto_dif_amo
    INTO   v_id_derechohabiente, 
           v_periodo_pago, 
           v_nrp, 
           v_monto_dif_apo, 
           v_monto_dif_amo 
    FROM   safre_viv:dis_det_avance_pago
    WHERE  folio = p_folio
			
    --Actualizamos estados de los registros afectados
    UPDATE  safre_viv:dis_det_avance_pago
    SET     estado             = 30,
            monto_aportacion   = v_monto_dif_apo,
            monto_amortizacion = v_monto_dif_amo,
            monto_dif_apo      = v_monto_dif_apo,
            monto_dif_amo      = v_monto_dif_amo
    WHERE   id_derechohabiente = v_id_derechohabiente
    AND     periodo_pago       = v_periodo_pago
    AND     nrp                = v_nrp
    AND     tpo_avance         = 181
    AND     estado            IN (30,70);
  END FOREACH;
	
  --Elimina los registros de dis_det_avance_pago
  DELETE 
  FROM   safre_viv:dis_det_avance_pago
  WHERE  folio = p_folio;
   
  --Elimina registros de dis_canc_par_ava_pag
  DELETE 
  FROM   safre_viv:dis_canc_par_ava_pag
  WHERE  folio = p_folio;
	
  UPDATE statistics FOR TABLE dis_det_avance_pago;
  UPDATE statistics FOR TABLE dis_canc_par_ava_pag;

  LET v_char = "Reverso correcto";
  --RETURN v_bnd_proceso , 0 , v_char;
  RETURN v_bnd_proceso;

END PROCEDURE;


