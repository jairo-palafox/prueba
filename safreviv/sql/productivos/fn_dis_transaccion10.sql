






create procedure "safreviv".fn_dis_transaccion10(p_fecha_01mm           DATE,          --Fecha de movimiento, proceso
                                      p_usuario              CHAR(20),      --Usuario del proceso
                                      p_folio_disp           DECIMAL(9,0),  --Folio de dispersión
                                      p_pid                  DECIMAL(9,0),  --Pid generado para el proceso
                                      p_proceso_cod          SMALLINT,      --Proceso dispersión preliquidación
                                      p_opera_cod            SMALLINT,      --Operacion preliquidacion
                                      p_folio_reg_pag        DECIMAL(9,0),  --Folio de registro de pagos
                                      p_proceso_cod_reg_pago SMALLINT)      --Codigo proceso de registro de pagos
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 25042019
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE r_instruccion_mdt     SMALLINT;       --Bandera instruccion mandato
DEFINE r_bnd_epera_error     SMALLINT;       --Bandera operacion error
DEFINE v_precio_fec_hoy      DECIMAL(19,14); --Precio de fondo del dia
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_folio_sua           DECIMAL(6);     --Folio detalle trabajdor
DEFINE v_id_referencia       DECIMAL(9,0);   --ID de referencia
DEFINE v_monto_pesos         DECIMAL(22,2);  --Importe aportaciones patronales -- Se cambia la variable ap_pat por v_monto_pesos (pesos convertidos)
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales -- Son los pesos no convertidos
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_edo_credito         SMALLINT;       --Tipo de credito del derechohabiente
DEFINE v_nss                 CHAR(11);       --Numero seguro social
DEFINE v_f_pago              DATE;           --Fecha de pago patronal
DEFINE v_precio_f_pag        DECIMAL(19,14); --Precio de fecha de pago
DEFINE v_tipo_trabajador     SMALLINT;       --Codigo de proceso del tipo de trabajador
DEFINE v_monto_dif_apo       DECIMAL(12,2);  --Monto diferencia de aportaciones de avance de pagos
DEFINE v_monto_dif_amo       DECIMAL(12,2);  --Monto diferencia amortizaciones de avance de pagos
DEFINE v_comp_ap_avpag       DECIMAL(12,2);  --Opercion de la resta de la diferencia con el pago
DEFINE v_comp_am_avpag       DECIMAL(12,2);  --Opercion de la resta de la diferencia con el pago
DEFINE v_det_avance_pago     DECIMAL(9,0);   --Id de detalle avance de pago
DEFINE v_derechohabiente_real DECIMAL(9,0);  --Derechohabiente del proceso de mandatos
DEFINE v_referencia_real     DECIMAL(9,0);   --Identificador de referencia del proceso de mandatos
DEFINE v_monto_pesos_real    DECIMAL(12,2);
DEFINE v_num_credito_crd     DECIMAL(10,0);
DEFINE v_localiza_trabajador CHAR(01);       --Localización del trabajador
DEFINE v_tpo_patron          CHAR(02);       --Tipo de patron
DEFINE v_periodo_bimestre    CHAR(6);        --Período de Pago Bimestre
DEFINE v_derechohabiente_pag DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito_pag     DECIMAL(10,0);
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_compensacion_apo    SMALLINT;       --Bandera de compensación aportación
DEFINE v_compensacion_amo    SMALLINT;       --Bandera de compensación amortización
DEFINE v_destino_ap_viv      CHAR(1);        --Destino Aportacion Vivienda
DEFINE v_f_otorga            DATE;
DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_transaccion7_ap     SMALLINT;
DEFINE v_transaccion7_am     SMALLINT;
DEFINE v_aiv_ap_pat          DECIMAL(18,6);
DEFINE v_f_liquida_cred      DATE;
DEFINE v_tpo_credito         SMALLINT;
DEFINE v_valida              SMALLINT;
DEFINE v_cve_ent_receptora   CHAR(3);
DEFINE v_tpo_originacion     SMALLINT;
DEFINE v_tpo_dscto           SMALLINT;

DEFINE v_viv_cta_sub         SMALLINT;
DEFINE v_viv_cta_fondo       SMALLINT;
DEFINE v_viv_cta_m_acc       DECIMAL(16,6);
DEFINE v_viv_cta_m_pes       DECIMAL(12,2);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET r_instruccion_mdt      = 0;
LET r_bnd_epera_error      = 0;
LET v_precio_fec_hoy       = 0.00;
LET v_nrp                  = "";
LET v_periodo_pago         = "";
LET v_folio_sua            = 0;
LET v_id_referencia        = 0;
LET v_monto_pesos          = 0.00;
LET v_imp_ap_pat           = 0.00;
LET v_imp_am_cre           = 0.00;
LET v_edo_credito          = 0;
LET v_nss                  = "";
LET v_f_pago               = TODAY;
LET v_precio_f_pag         = 0.00;
LET v_tipo_trabajador      = 0;
LET v_monto_dif_apo        = 0.00;
LET v_monto_dif_amo        = 0.00;
LET v_comp_ap_avpag        = 0.00;
LET v_comp_am_avpag        = 0.00;
LET v_det_avance_pago      = 0;
LET v_derechohabiente_real = 0;
LET v_referencia_real      = 0;
LET v_monto_pesos_real     = 0.00;
LET v_num_credito_crd      = 0;
LET v_localiza_trabajador  = "";
LET v_tpo_patron           = "";
LET v_bnd_transaccion      = 0;
LET v_periodo_bimestre     = "";
LET v_derechohabiente_pag  = 0;
LET v_num_credito_pag      = 0;
LET v_num_credito          = 0;
LET v_compensacion_apo     = 0;
LET v_compensacion_amo     = 0;
LET v_destino_ap_viv       = 0;
LET v_transaccion7_ap	   = 0;
LET v_transaccion7_am	   = 0;
LET v_aiv_ap_pat           = 0.00;
LET v_f_otorga             = '';
LET v_f_liquida_cred       = '';
LET v_tpo_credito          = 0;
LET v_valida               = 0;
LET v_cve_ent_receptora	   = "";
LET v_tpo_originacion      = 0;
LET v_tpo_dscto            = 0;

LET v_viv_cta_sub          = 0;
LET v_viv_cta_fondo        = 0;
LET v_viv_cta_m_acc        = 0;
LET v_viv_cta_m_pes        = 0;

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_transaccion10.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_transaccion10.TRACE';
--TRACE ON;
--SET INDEXES FOR dis_cta_ind_pau DISABLED;
  
  SET PDQPRIORITY HIGH;

  --Identificar precio de Acción del día
  SELECT precio_fondo
  INTO   v_precio_fec_hoy
  FROM   glo_valor_fondo
  WHERE  fondo       = 11
  AND    f_valuacion = TODAY;

  --Leer todos los registros con NRP 99
  --Ingresarlos a una tabla temporal
  SELECT a.id_derechohabiente id_der_lab,
         a.nrp,
         b.nss
  FROM   afi_relacion_laboral a,
         afi_derechohabiente b
  WHERE  a.id_derechohabiente = b.id_derechohabiente
  AND    a.nrp[1,2]           = '99'
  INTO TEMP tmp_dis_rel_lab99;

  --Leer todos los registros de la tabla cta_his_pagos
  --Identifica precio de fondo de Fecha de Pago patronal
  FOREACH
    SELECT pag.id_derechohabiente,
           pag.folio_sua,
           pag.periodo_pago,
           pag.f_pago,
           pag.nrp,
           pag.folio,
           pag.id_referencia,
           pag.localiza_trabajador,
           pag.tpo_patron,
           pag.imp_ap_pat,
           pag.imp_am_cre,
           pag.aiv_ap_pat,
           pag.num_crd_ifv,
           pag.destino_ap_viv,
           fon.precio_fondo,
           pag.cve_ent_receptora,
           rel.nss 
    INTO   v_derechohabiente_pag,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp,
           p_folio_reg_pag,
           v_id_referencia,
           v_localiza_trabajador,
           v_tpo_patron,
           v_imp_ap_pat,
           v_imp_am_cre,
           v_aiv_ap_pat,
           v_num_credito_pag,
           v_destino_ap_viv,
           v_precio_f_pag,
           v_cve_ent_receptora,
           v_nss
    FROM   cta_his_pagos pag,
           tmp_dis_rel_lab99 rel,
           glo_valor_fondo fon
    WHERE  pag.folio                = p_folio_reg_pag
    AND    pag.id_derechohabiente   = rel.id_der_lab
    AND    DATE(pag.f_pago)         = DATE(fon.f_valuacion)
    AND    pag.nrp                  = rel.nrp
    AND    pag.ind_liquidacion NOT IN (1,6)
    AND    fon.fondo                = 11

    IF v_destino_ap_viv = 3 THEN -- Portabilidad
       CONTINUE FOREACH;
    END IF

    IF v_aiv_ap_pat IS NULL THEN
       LET v_aiv_ap_pat = 0;
    END IF

    LET v_imp_am_cre = 0;

    --Si las aportaciones y amortizaciones son menores o
    --iguales a cero no se dispersa el registro
    --IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
    IF (v_aiv_ap_pat <= 0.00) THEN
       CONTINUE FOREACH;
    END IF

    EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
                 INTO v_periodo_bimestre;

    LET v_num_credito     = 0;
    LET v_num_credito_crd = 0;
    LET v_tipo_trabajador = 0;
    LET v_f_otorga        = '';
    LET v_tpo_dscto       = 0;
    LET v_tpo_originacion = 0;
	 
    --Se realiza la conversión de los pesos por el precio de acción del día
    LET v_monto_pesos     = (v_aiv_ap_pat * v_precio_fec_hoy);

    --- Eliminar la asignación cuando se active el proceso de mandatos
    LET v_monto_pesos_real = v_imp_am_cre;

    --########## Se agrega nueva función para optimización ###########
    --Identifica el crédito más reciente del derechohabiente que haya sido liquidado
    --Valor resultado = -2: No hay NSS asociado
    --Valor resultado = -1: No hay identificador de derechohabiente
    --Valor resultado = 0: 	Existe crédito vigente
    --Valor resultado = 1: 	No tiene crédito
    --Valor resultado = 2: 	Tiene crédito Liquidado
    --Valor resultado = 3:  Crédito en trámite
    LET v_edo_credito    = 0;
    LET v_f_liquida_cred = '';
    LET v_tpo_credito    = 0;
    LET v_valida         = 1;
    LET v_nss            = "";
	
    EXECUTE FUNCTION fn_edo_cred_viv(v_derechohabiente_pag, v_valida)
                INTO v_edo_credito,
                     v_tipo_trabajador,
                     v_tpo_credito,
                     v_num_credito_crd,
                     v_f_otorga,
                     v_f_liquida_cred,
                     v_tpo_dscto;

    IF v_edo_credito = -2 OR v_edo_credito = -1 OR v_edo_credito = 1 THEN 
       LET v_num_credito_crd = 0;
       LET v_tipo_trabajador = 0;
       LET v_f_otorga        = '';
       LET v_tpo_credito     = 0;
    END IF 

    --- DEJAR COMO PRINCIPAL EL NUMERO DE CREDITO
    IF v_num_credito_crd IS NULL OR
       v_num_credito_crd = 0 THEN

       IF v_num_credito_pag IS NULL OR
          v_num_credito_pag = 0 THEN
          LET v_num_credito = 0;
       END IF
       LET v_num_credito     = v_num_credito_pag;
       LET v_tpo_originacion = 2;
    ELSE
       LET v_num_credito     = v_num_credito_crd;
       LET v_tpo_originacion = 1;
    END IF

    EXECUTE PROCEDURE sp_dis_transaccion23(v_derechohabiente_pag,
                                           p_folio_disp,
                                           v_num_credito,
                                           v_tipo_trabajador,
                                           v_tpo_credito,
                                           v_f_otorga,
                                           v_f_liquida_cred,
                                           v_edo_credito,
                                           v_tpo_originacion,
                                           v_destino_ap_viv,
                                           p_folio_reg_pag,
                                           v_id_referencia,
                                           v_folio_sua,
                                           v_periodo_bimestre,
                                           v_f_pago,
                                           v_nrp,
                                           v_imp_ap_pat,
                                           v_monto_pesos_real,
                                           v_aiv_ap_pat)
                 INTO v_bnd_transaccion , v_status , error_info;

    --Identificar trabajador con crédito vigente
    IF v_edo_credito = 0 THEN
       --Pago a Cartera
       EXECUTE PROCEDURE sp_dis_Transaccion3(v_derechohabiente_pag, --HS
                                             v_imp_ap_pat,
                                             v_monto_pesos,
                                             0,
                                             p_folio_disp,
                                             v_id_referencia,
                                             v_precio_fec_hoy,
                                             v_nrp,
                                             v_periodo_bimestre,
                                             v_folio_sua,
                                             v_num_credito,
                                             p_proceso_cod_reg_pago,
                                             v_tpo_patron,
                                             v_f_pago,
                                             v_tipo_trabajador,
                                             v_localiza_trabajador,
                                             v_aiv_ap_pat,
                                             v_tpo_credito)
                    INTO v_bnd_transaccion, v_status,error_info;
    ELSE
       --Afectación Solo a la Cuenta Individual (PAU)
       EXECUTE PROCEDURE sp_dis_Transaccion15(v_derechohabiente_pag,
                                              v_imp_ap_pat,
                                              v_monto_pesos,
                                              0,
                                              p_folio_disp,
                                              v_id_referencia,
                                              v_precio_fec_hoy,
                                              v_nrp,
                                              v_periodo_bimestre,
                                              v_folio_sua,
                                              v_num_credito,
                                              p_proceso_cod_reg_pago,
                                              v_tpo_patron,
                                              v_f_pago,
                                              v_tipo_trabajador,
                                              v_localiza_trabajador,
                                              v_aiv_ap_pat,
                                              v_edo_credito,
                                              p_folio_reg_pag)
                    INTO v_bnd_transaccion, v_status,error_info;
    END IF --Identificación del crédito
  END FOREACH; --cta_his_pagos

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  LET error_info = "  Preliquidación finalizo correctamente";
  RETURN v_bnd_transaccion , v_status , error_info, '';

END PROCEDURE;


