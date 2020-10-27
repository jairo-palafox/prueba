






CREATE PROCEDURE "safreviv".sp_dis_apo_subs7(p_folio DECIMAL(9,0), p_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 14122017
--Declaración de variables
DEFINE v_folio_liquida 	           DECIMAL(9,0);
DEFINE v_id_interface_ef           DECIMAL(9,0);
DEFINE v_id_derechohabiente        DECIMAL(9,0);
DEFINE v_reg_pag_imss 	           CHAR(11);
DEFINE v_rfc_pat                   CHAR(13);
DEFINE v_periodo_pago 	           CHAR(6);
DEFINE v_folio_sua                 DECIMAL(6,0);
DEFINE v_f_pago                    DATE;
DEFINE v_f_valor                   DATE;
DEFINE v_nss                       CHAR(11);
DEFINE v_rfc                       CHAR(13);
DEFINE v_curp                      CHAR(18);
DEFINE v_nombre                    CHAR(50);
DEFINE v_usdi_periodo 	           DECIMAL(12,2);
DEFINE v_dias_cotizados_bim        INTEGER;
DEFINE v_dias_incapacidad_bim      INTEGER;
DEFINE v_dias_ausentismo_bim       INTEGER;
DEFINE v_imp_apo_pat 	           DECIMAL(12,2);
DEFINE v_imp_amo_cred 	           DECIMAL(12,2);
DEFINE v_dif_apo_sub 	           DECIMAL(12,2);
DEFINE v_afore                     SMALLINT;
DEFINE v_f_liquidacion 	           DATE;
DEFINE v_estado                    SMALLINT;
DEFINE v_bnd_transaccion           SMALLINT;
DEFINE v_bnd_proceso               SMALLINT; --estatus del proceso
DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER ;
DEFINE isam_err                    INTEGER ;
DEFINE error_info                  CHAR(70);
DEFINE v_char                      CHAR(20);
DEFINE v_periodo_bimestre          CHAR(6);  --período de Pago Bimestre
DEFINE v_aplic_int_viv_apo_pat     DECIMAL(15,6);
DEFINE v_val_aplic_int_viv         DECIMAL(15,6);
DEFINE v_ind_liquidacion           SMALLINT;

DEFINE ef_id_derechohabiente       DECIMAL(9,0);
DEFINE ef_nss                      CHAR(11);
DEFINE ef_folio_sua                DECIMAL(6,0);
DEFINE ef_periodo_pago             CHAR(6);
DEFINE ef_f_pago                   DATE;
DEFINE ef_nrp                      CHAR(11);
DEFINE ef_ind_liquidacion          SMALLINT;
DEFINE ef_tot_apo_subs             INTEGER;
DEFINE ef_id_dis_interface         DECIMAL(9,0);
DEFINE v_id_dis_interface_ef       DECIMAL(9,0);

DEFINE v_tot_dup_cargo             SMALLINT;
DEFINE d_id_derechohabiente        DECIMAL(9,0);
DEFINE d_nss                       CHAR(11);
DEFINE d_folio_sua                 DECIMAL(6,0);
DEFINE d_periodo_pago              CHAR(6);
DEFINE d_f_pago                    DATE;
DEFINE d_nrp                       CHAR(11);
DEFINE d_aiv_ap_pat                DECIMAL(18,6);
DEFINE d_id_dis_interface_ef       DECIMAL(9,0);
DEFINE d_folio_liquida             DECIMAL(9,0);
DEFINE d_hoy                       DATE;

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

  --#Inicialización de variables
  LET v_bnd_proceso            = 0; --estado correcto
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
  LET v_tot_dup_cargo          = 0;
  LET d_hoy                    = TODAY;

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
           aplic_int_viv_apo_pat / 1000000, --se divide entre un millón
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
    FROM   safre_tmp:tmp_dis_aposubs2

    EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
    INTO v_periodo_bimestre;

    LET v_estado          = 10;
    LET v_ind_liquidacion = 0;
    LET v_tot_dup_cargo   = 0;
			
    --#Obtenemos id_derechohabiente según número seguro social
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = v_nss;
			
    --#Asigna id_derechohabiente si no se encuentra en tabla
    IF v_id_derechohabiente IS NULL THEN
       LET v_id_derechohabiente = "999999999";
       --LET v_folio_liquida = 0;
       --#Rechazo por no existir en el maestro de derechohabientes
       LET v_estado = 23;
    END IF

    --#Valida que valor de aportacion --#amortizacion sea mayor a cero
    IF v_imp_apo_pat           <= 0.00 OR 
       v_aplic_int_viv_apo_pat <= 0.00 THEN --#AND v_imp_amo_cred <= 0.00 THEN
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

    --TRACE 'Valida duplicidad de pago de la aportación subsecuente';
    --#Valida duplicidad de pago de la aportación subsecuente
    SELECT COUNT(*)
    INTO   v_tot_dup_cargo
    FROM   dis_interface_ef a
    WHERE  a.id_derechohabiente = v_id_derechohabiente
    AND    a.folio_sua          = v_folio_sua
    AND    a.periodo_pago       = v_periodo_bimestre
    AND    a.f_pago             = v_f_pago
    AND    a.nrp                = v_reg_pag_imss;
    IF v_tot_dup_cargo > 1 THEN
       LET v_estado              = 105;
       LET d_id_derechohabiente  = 0;
       LET d_nss                 = "";
       LET d_folio_sua           = 0;
       LET d_periodo_pago        = "";
       LET d_f_pago              = "";
       LET d_nrp                 = "";
       LET d_aiv_ap_pat          = 0;
       LET d_id_dis_interface_ef = 0;
       LET d_folio_liquida       = 0;

       FOREACH
       SELECT a.id_derechohabiente,
              c.nss,
              b.folio_sua,
              b.periodo_pago,
              b.f_pago,
              b.nrp,
              a.aiv_ap_pat,
              MAX(b.id_dis_interface_ef),
              MAX(b.folio_liquida)
       INTO   d_id_derechohabiente, 
              d_nss,
              d_folio_sua,
              d_periodo_pago,
              d_f_pago,
              d_nrp,
              d_aiv_ap_pat,
              d_id_dis_interface_ef,
              d_folio_liquida
       FROM   dis_interface_ef a,
              dis_interface_ef b,
              afi_derechohabiente c
       WHERE  a.id_derechohabiente = v_id_derechohabiente
       AND    a.id_derechohabiente = b.id_derechohabiente 
       AND    a.id_derechohabiente = c.id_derechohabiente
       AND    b.id_derechohabiente = c.id_derechohabiente
       AND    a.folio_sua          = b.folio_sua
       AND    a.f_pago             = b.f_pago
       AND    a.nrp                = b.nrp
       AND    a.periodo_pago       = b.periodo_pago
       AND    a.folio_liquida     <> b.folio_liquida
       GROUP BY 1,2,3,4,5,6,7
       ORDER BY 3,5

       INSERT INTO dis_ap_cargo_dup VALUES (p_folio,
                                            d_folio_liquida,
                                            d_id_dis_interface_ef,
                                            d_id_derechohabiente,
                                            d_folio_sua,
                                            d_periodo_pago,
                                            d_f_pago,
                                            d_nrp,
                                            d_nss,
                                            d_aiv_ap_pat,
                                            0,
                                            0,
                                            d_hoy,
                                            v_estado);
       END FOREACH;
    END IF

    -- Actualiza indicador registros duplicados Aportaciones Subsecuentes
    SELECT FIRST 1 (a.id_derechohabiente)
    INTO   ef_id_derechohabiente
    FROM   dis_dup_ap_subsecuente a
    WHERE  a.id_derechohabiente = v_id_derechohabiente
    AND    a.folio_sua          = v_folio_sua
    AND    a.periodo_pago       = v_periodo_bimestre
    AND    a.f_pago             = v_f_pago
    AND    a.reg_pat_imss       = v_reg_pag_imss
    AND    a.ind_liquidacion   IN (1,100);
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       INSERT INTO dis_ap_subsecuente VALUES (p_folio,
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
                                              --p_edo_rech);
                                              v_estado);
    ELSE
       SELECT UNIQUE a.ind_liquidacion
       INTO   v_ind_liquidacion
       FROM   dis_dup_ap_subsecuente a
       WHERE  a.id_derechohabiente = v_id_derechohabiente
       AND    a.folio_sua          = v_folio_sua
       AND    a.periodo_pago       = v_periodo_bimestre
       AND    a.f_pago             = v_f_pago
       AND    a.reg_pat_imss       = v_reg_pag_imss;
       IF v_ind_liquidacion = 1 THEN
          UPDATE dis_dup_ap_subsecuente
          SET    ind_liquidacion    = 100
          WHERE  id_derechohabiente = v_id_derechohabiente
          AND    folio_sua          = v_folio_sua
          AND    periodo_pago       = v_periodo_bimestre
          AND    f_pago             = v_f_pago
          AND    reg_pat_imss       = v_reg_pag_imss;

          LET v_estado = 10;
       ELSE
          LET v_estado = 100;
       END IF

       INSERT INTO dis_ap_subsecuente VALUES (p_folio,
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
                                              --p_edo_rech);
                                              v_estado);
    END IF
  END FOREACH;

  UPDATE STATISTICS FOR TABLE dis_ap_subsecuente;
   
  LET v_char = "Terminado Apo_subs7_SPL";
  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


