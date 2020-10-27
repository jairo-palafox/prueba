






CREATE PROCEDURE "safreviv".sp_dis_apo_subs2_ea(p_folio DECIMAL(9,0), p_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 06022018
--Declaración de variables
DEFINE v_folio_liquida 	        DECIMAL(9,0);
DEFINE v_id_interface_ef        DECIMAL(9,0);
DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_reg_pag_imss 	        CHAR(11);
DEFINE v_rfc_pat                CHAR(13);
DEFINE v_periodo_pago 	        CHAR(6);
DEFINE v_folio_sua              DECIMAL(6,0);
DEFINE v_f_pago                 DATE;
DEFINE v_f_valor                DATE;
DEFINE v_nss                    CHAR(11);
DEFINE v_rfc                    CHAR(13);
DEFINE v_curp                   CHAR(18);
DEFINE v_nombre                 CHAR(50);
DEFINE v_usdi_periodo 	        DECIMAL(12,2);
DEFINE v_dias_cotizados_bim     INTEGER;
DEFINE v_dias_incapacidad_bim   INTEGER;
DEFINE v_dias_ausentismo_bim    INTEGER;
DEFINE v_imp_apo_pat 	        DECIMAL(12,2);
DEFINE v_imp_amo_cred 	        DECIMAL(12,2);
DEFINE v_dif_apo_sub 	        DECIMAL(12,2);
DEFINE v_afore                  SMALLINT;
DEFINE v_f_liquidacion 	        DATE;
DEFINE v_estado                 SMALLINT;
DEFINE v_bnd_transaccion        SMALLINT;
DEFINE v_bnd_proceso            SMALLINT;       --Estatus del proceso
DEFINE v_status                 SMALLINT;
DEFINE sql_err                  INTEGER ;
DEFINE isam_err                 INTEGER ;
DEFINE error_info               CHAR(70);
DEFINE v_char                   CHAR(20);
DEFINE v_periodo_bimestre       CHAR(6);        --Período de Pago Bimestre
DEFINE v_aplic_int_viv_apo_pat  DECIMAL(15,6);
DEFINE v_val_aplic_int_viv      DECIMAL(15,6);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

  --#Inicialización de variables
  LET v_bnd_proceso            = 0; --Estado correcto
  LET v_folio_liquida          = 0;
  LET v_id_interface_ef        = 0.00;
  LET v_id_derechohabiente     = 0;
  LET v_reg_pag_imss           = "";
  LET v_rfc_pat                = "";
  LET v_periodo_pago           = "";
  LET v_folio_sua              = 0;
  LET v_f_pago                 = "";
  LET v_f_valor                = "";
  LET v_nss                    = "";
  LET v_rfc                    = "";
  LET v_curp                   = "";
  LET v_nombre                 = "";
  LET v_usdi_periodo           = 0.00;
  LET v_dias_cotizados_bim     = 0;
  LET v_dias_incapacidad_bim   = 0;
  LET v_dias_ausentismo_bim    = 0;
  LET v_imp_apo_pat            = 0.00;
  LET v_imp_amo_cred           = 0.00;
  LET v_dif_apo_sub            = 0.00;
  LET v_afore                  = 0;
  LET v_f_liquidacion          = "";
  LET v_estado                 = 0;
  LET v_bnd_transaccion        = 0;
  LET v_periodo_bimestre       = "";
  LET v_aplic_int_viv_apo_pat  = 0.000000;
  LET v_val_aplic_int_viv      = 0.000000;

  FOREACH
    SELECT reg_pat_imss,
           rfc_pat,
           periodo_pago,
           folio_sua,
           f_pago,
           f_valor,
           nss,
           rfc,
           curp,
           nombre,
           usdi_periodo,
           dias_cotizados_bim,
           dias_incapacidad_bim,
           dias_ausentismo_bim,
           imp_apo_pat / 100,
           imp_amo_cred / 100,
           aplic_int_viv_apo_pat /1000000, --Se divide entre un millón
           val_aplic_int_viv / 1000000,
           afore,
           f_liquidacion
    INTO   v_reg_pag_imss,
           v_rfc_pat,
           v_periodo_pago,
           v_folio_sua,
           v_f_pago,
           v_f_valor,
           v_nss,
           v_rfc,
           v_curp,
           v_nombre,
           v_usdi_periodo,
           v_dias_cotizados_bim,
           v_dias_incapacidad_bim,
           v_dias_ausentismo_bim,
           v_imp_apo_pat,
           v_imp_amo_cred,
           v_aplic_int_viv_apo_pat,
           v_val_aplic_int_viv,
           v_afore,
           v_f_liquidacion
    FROM   safre_tmp:tmp_dis_aposubs2_ea

    EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
    INTO v_periodo_bimestre;

    LET v_estado = 10;
			
    --#Obtenemos id_derechohabiente según número seguro socila
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = v_nss;
			
    --#Obtiene folio de liquidación
    SELECT folio_liquida, id_dis_interface_ef
    INTO   v_folio_liquida, v_id_interface_ef
    FROM   dis_interface_ef
    WHERE  id_derechohabiente = v_id_derechohabiente
    AND    folio_sua          = v_folio_sua
    AND    periodo_pago       = v_periodo_bimestre
    AND    f_pago             = v_f_pago
    AND    nrp                = v_reg_pag_imss
    AND    ind_liquidacion    = 0;
           
    --#Asigna id_derechohabiente si no se encuentra en tabla
    IF v_id_derechohabiente IS NULL THEN
       LET v_id_derechohabiente = "999999999";
       --LET v_folio_liquida = 0;
       --#Rechazo por no existir en el maestro de derechohabientes
       LET v_estado = 23;
    END IF

    --#Valida que valor de aportacion --#amortizacion sea mayor a cero
    IF v_imp_apo_pat           <= 0.00 OR 
       v_aplic_int_viv_apo_pat <= 0.00 THEN--#AND v_imp_amo_cred <= 0.00 THEN
       --#Rechazo por no incluir montos
       LET v_estado = 24;
    END IF
   
    --TRACE 'Valida valor de periodo_pago no sea nulo o blanco';
    --#Valida valor de periodo_pago no sea nulo o blanco
    IF v_periodo_bimestre IS NULL OR LENGTH(v_periodo_pago) = 0 THEN
       --#Rechazo por no contar con el periodo de pago
       LET v_estado = 25;
    END IF

    --TRACE 'Valida valor de Folio Liquida no sea nulo o blanco';
    --#Valida valor de Liquida  no sea nulo o blanco
    IF v_folio_liquida IS NULL THEN
       LET v_folio_liquida = 0;
    END IF

    --TRACE 'Valida valor de v_id_interface_ef no sea nulo o blanco';
    --#Valida valor de v_id_interface_ef  no sea nulo o blanco
    IF v_id_interface_ef IS NULL THEN
       LET v_id_interface_ef = 0;
    END IF         

    --Se dividen los montos / 100 
    {LET v_imp_apo_pat  = (v_imp_apo_pat / 100);
    LET v_imp_amo_cred = (v_imp_amo_cred / 100);}
			
    INSERT INTO dis_ap_subsecuente_ea VALUES (p_folio,
                                              v_folio_liquida,
                                              v_id_interface_ef,
                                              v_id_derechohabiente,
                                              v_folio_sua,
                                              v_periodo_bimestre,
                                              v_f_pago,
                                              v_reg_pag_imss,
                                              v_nss,
                                              v_imp_apo_pat,
                                              v_imp_amo_cred,
                                              v_aplic_int_viv_apo_pat,
                                              v_val_aplic_int_viv,
                                              v_afore,
                                              v_f_liquidacion,
                                              p_edo_rech);

    EXECUTE FUNCTION fn_dif_apo_sub_ea(v_id_derechohabiente,
                                       v_folio_sua,
                                       v_periodo_bimestre,
                                       v_f_pago,
                                       v_reg_pag_imss,
                                       v_imp_apo_pat,
                                       v_aplic_int_viv_apo_pat)
                INTO v_dif_apo_sub;

  END FOREACH;
   
  UPDATE STATISTICS FOR TABLE dis_ap_subsecuente_ea;
   
  LET v_char = "Terminado Apo_subs2_ea_SPL";
  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


