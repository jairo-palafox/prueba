






CREATE PROCEDURE "safreviv".fn_dif_apo_sub(v_id_derechohabiente 	DECIMAL(9,0),
			        v_folio_sua             DECIMAL(6,0),
			        v_periodo_pago          CHAR(6),
			        v_f_pago                DATE,
			        v_reg_pat_imss          CHAR(11),
			        v_imp_apo_pat           DECIMAL(12,2),
			        v_aplic_int_viv_apo_pat DECIMAL(15,6)
                               )  
RETURNING SMALLINT;

--Última modificación 21062013
--Declaración de variables
  DEFINE v_bnd_proceso			SMALLINT;
  DEFINE v_imp_ap_pat 			DECIMAL(12,2);
  DEFINE v_aiv_ap_pat                   DECIMAL(18,6);
  DEFINE v_dif_ap_pat                   DECIMAL(12,2);
  DEFINE v_ind_liquida			SMALLINT;

  --Inicialización de variables
  LET v_bnd_proceso = 0; --0 es correcto; 1 es incorrecto
  LET v_imp_ap_pat  = 0.00;
  LET v_aiv_ap_pat  = 0.00;
  LET v_dif_ap_pat  = 0.00;
  LET v_ind_liquida = 1; --Se inicializa el indicador en 1 = Conciliado

  {LIQUIDACIÓN EN PESOS
  SELECT imp_ap_pat
  INTO   v_imp_ap_pat
  LIQUIDACIÓN EN PESOS}

  SELECT aiv_ap_pat
  INTO   v_aiv_ap_pat
  FROM   dis_interface_ef
  WHERE  id_derechohabiente = v_id_derechohabiente
  AND    folio_sua          = v_folio_sua
  AND    periodo_pago       = v_periodo_pago
  AND    f_pago             = v_f_pago
  AND    nrp                = v_reg_pat_imss
  AND    ind_liquidacion    = 0;  --0 es liquidado
  IF DBINFO('sqlca.sqlerrd2') > 0 THEN

     LET v_bnd_proceso = 1;

     --LET v_dif_ap_pat  = v_imp_ap_pat - v_imp_apo_pat;
     LET v_dif_ap_pat  = v_aiv_ap_pat - v_aplic_int_viv_apo_pat;
     --TRACE 'La resta de aportaciones es: '||v_imp_ap_pat;
      
     IF (v_dif_ap_pat <> 0.00) THEN  --Si la resta da diferente a 0 entonces actualizar al estado 26
      
	UPDATE dis_ap_subsecuente
	SET    estado             = 26          --26 = rechazo por diferencia de aportaciones
	WHERE  id_derechohabiente = v_id_derechohabiente
	AND    folio_sua          = v_folio_sua
	AND    periodo_pago       = v_periodo_pago
	AND    f_pago             = v_f_pago
	AND    reg_pat_imss       = v_reg_pat_imss;
				
        --Se agrega actualización de indicador de liquidación = 2 (Conciliado con diferencias)
	LET v_ind_liquida = 2;
				
     END IF		
		   	
     UPDATE dis_interface_ef
     SET    ind_liquidacion    = v_ind_liquida
     WHERE  id_derechohabiente = v_id_derechohabiente
     AND    folio_sua          = v_folio_sua
     AND    periodo_pago       = v_periodo_pago
     AND    f_pago             = v_f_pago
     AND    nrp                = v_reg_pat_imss
     AND    ind_liquidacion    = 0;
	    		
  END IF 

  LET v_dif_ap_pat = 0.00;

  RETURN v_bnd_proceso; 
END PROCEDURE;


