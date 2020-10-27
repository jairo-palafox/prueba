






CREATE PROCEDURE "safreviv".sp_dis_fac_apo_subs2(p_folio_factura DECIMAL(9,0)) --Entidad Financiera
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 10102016
--Declaración de variables
DEFINE v_nss                       CHAR(11);
DEFINE v_folio_sua                 DECIMAL(6,0);
DEFINE v_periodo_pago              CHAR(06);
DEFINE v_f_pago                    DATE;
DEFINE v_nrp                       CHAR(11);
DEFINE v_id_derechohabiente        DECIMAL(9,0);  --Id derechohabiente
DEFINE v_concepto                  SMALLINT;      --Concepto
DEFINE v_concepto_f                SMALLINT;      --Concepto Factura
DEFINE v_conteo                    DECIMAL(9,0);  --Conteo de registros
DEFINE v_id_dis_interface_ef       DECIMAL(9,0);  --Id Interface Entidad Financiera
DEFINE v_cve_ent_financiera        SMALLINT;
DEFINE v_num_ctr_int_ef            CHAR(18);
DEFINE v_nrp_rl                    CHAR(11);
DEFINE v_tot_rl                    SMALLINT;

DEFINE v_folio_transaccion         DECIMAL(9,0);
DEFINE v_id_ctr_transaccion        DECIMAL(9,0);
DEFINE v_f_transaccion             DATE;
DEFINE v_id_ocg_detalle            DECIMAL(9,0);
DEFINE v_folio_factura             DECIMAL(9,0);
DEFINE v_f_factura                 DATE;
DEFINE v_tpo_credito               SMALLINT;
DEFINE v_importe                   DECIMAL(22,2);
DEFINE v_id_factura                SMALLINT;
DEFINE v_max_id_fac                SMALLINT;
DEFINE v_referencia                CHAR(16);
DEFINE v_ent_fin_ref               CHAR(3);
DEFINE v_consecutivo_ref           CHAR(2);
DEFINE v_ano                       SMALLINT;
DEFINE v_mes                       SMALLINT;
DEFINe v_dia                       SMALLINT;
DEFINE v_ano_ref                   CHAR(4);
DEFINE v_mes_ref                   CHAR(2);
DEFINE v_dia_ref                   CHAR(2);
DEFINE v_f_referencia              DATE;
DEFINE v_prefijo_rfc_ef            CHAR(3);
DEFINE v_cta_contable              CHAR(10);
DEFINE v_clabe                     CHAR(18);

DEFINE v_banco_interlocutor        SMALLINT;
DEFINE v_f_hoy                     DATE;

--Estado del registro
DEFINE v_bnd_proceso               SMALLINT;
DEFINE v_char                      CHAR(20);
DEFINE v_tot_reg_fac               SMALLINT;
DEFINE v_imp_acum                  DECIMAL(22,2);

DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER ;
DEFINE isam_err                    INTEGER ;
DEFINE error_info                  CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

-- SET DEBUG FILE TO '/ds/safreviv_int/ocg/ERR_sp_dis_fac_apo_subs2.TRACE';
-- TRACE ON;

--Inicialización de variables
LET v_id_dis_interface_ef = 0.00;
LET v_id_derechohabiente  = 0.00;
LET v_concepto            = 0;
LET v_concepto_f          = 0;
LET v_conteo              = 0.00;

LET v_bnd_proceso         = 0;
LET v_char                = "";

LET v_status              = 0;

LET sql_err               = 0;
LET isam_err              = 0;
LET error_info            = "";

LET v_cve_ent_financiera  = 0;
LET v_num_ctr_int_ef      = "";
LET v_nss                 = "";
LET v_folio_sua           = 0;
LET v_periodo_pago        = "";
LET v_f_pago              = "";
LET v_nrp                 = "";
LET v_nrp_rl              = "";  
LET v_tpo_credito         = 0;   
LET v_folio_factura       = 0;
LET v_f_factura           = "";
LET v_importe             = 0;
LET v_id_factura          = 0;
LET v_max_id_fac          = 0;
LET v_referencia          = "";
LET v_ent_fin_ref         = "";
LET v_consecutivo_ref     = "";
LET v_ano                 = 0;
LET v_mes                 = 0;
LET v_dia                 = 0;
LET v_ano_ref             = "";
LET v_mes_ref             = "";
LET v_dia_ref             = "";
LET v_f_referencia        = "";
LET v_prefijo_rfc_ef      = "";
LET v_cta_contable        = "";
LET v_clabe               = "";
LET v_banco_interlocutor  = 0;
LET v_f_hoy               = TODAY;

LET v_folio_transaccion   = 0;
LET v_id_ctr_transaccion  = 0;
LET v_f_transaccion       = "";
LET v_id_ocg_detalle      = 0;
LET v_tot_rl              = 0;
LET v_tot_reg_fac         = 0;
LET v_imp_acum            = 0;

  {FOREACH
    --Busca los id_derechohabiente de la tabla principal
    SELECT b.nss, a.id_dis_interface_ef, a.id_derechohabiente, 
           a.concepto, a.folio_transaccion, a.id_ctr_transaccion, 
           a.f_transaccion, a.cve_ent_financiera, a.periodo_pago,
           a.f_pago, a.folio_sua
    INTO   v_nss, v_id_dis_interface_ef, v_id_derechohabiente, 
           v_concepto, v_folio_transaccion, v_id_ctr_transaccion, 
           v_f_transaccion, v_cve_ent_financiera, v_periodo_pago,
           v_f_pago, v_folio_sua
    FROM   afi_derechohabiente b,
           tmp_dis_fac_aps_tns a
    WHERE  b.id_derechohabiente = a.id_derechohabiente

    --4)Se asigna el estado 60, Tipos de Crédito Apoyo INFONAVIT
    --5)Se asigna el estado 60, Tipos de Crédito COFINAVIT    	
    UPDATE dis_ctr_aps_tns
    SET    estado             = 60 --En Trámite de Pago
    WHERE  folio_transaccion  = v_folio_transaccion
    AND    id_ctr_transaccion = v_id_ctr_transaccion;

    ---5)Actualizar fecha de proceso de tabla transacciones PENDIENTE Tipos de Crédito Apoyo INFONAVIT
    ---6)Actualizar fecha de proceso de tabla transacciones PENDIENTE Tipos de Crédito COFINAVIT
    UPDATE ocg_ctr_transaccion 
    SET    f_proceso              = TODAY,
           estado                 = 60 --En Trámite de Pago
    WHERE  folio_referencia       = v_folio_transaccion
    AND    id_ocg_ctr_transaccion = v_id_ctr_transaccion;

    LET v_cve_ent_financiera = 0;
    LET v_nss                = "";
    LET v_concepto_f         = 0;
    LET v_tot_rl             = 0;
  END FOREACH}

  SELECT MAX(id_factura)
  INTO   v_max_id_fac
  FROM   dis_ctr_factura_aps a
  WHERE  a.folio_factura = p_folio_factura
  AND    a.f_factura     = TODAY;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_id_factura = v_max_id_fac;
  ELSE
     LET v_id_factura = 0;
  END IF
 
  FOREACH
    SELECT a.folio_factura, a.f_factura, a.cve_ent_financiera, a.tpo_credito, SUM(a.imp_ap_pat)
    INTO   v_folio_factura, v_f_factura, v_cve_ent_financiera, v_tpo_credito, v_importe   
    FROM   dis_ctr_aps_tns a
    WHERE  a.folio_factura = p_folio_factura
    AND    a.estado        = 55  --Pre Autorizado
    GROUP BY 1,2,3,4

    --Creación de la referencia
    --Referencia = APddmmaaaRFC EF
    --ddmmaaaa (T+1 hábil)
    --RFC EF (Tres últimas posiciones)

    IF v_tpo_credito = 2 THEN
       LET v_referencia = "AP"; --Apoyo INFONAVIT
    END IF

    IF v_tpo_credito = 3 THEN
       LET v_referencia = "UG"; --Uso de Garantía
    END IF

    IF v_tpo_credito = 5 THEN
       LET v_referencia = "CO"; --COFINAVIT
    END IF

    SELECT a.rfc_ent_fin_pref, a.cta_contable, a.clabe, a.banco_interlocutor
    INTO   v_prefijo_rfc_ef, v_cta_contable, v_clabe, v_banco_interlocutor
    FROM   cat_cta_cnt_ocg a
    WHERE  a.cve_ent_financiera = v_cve_ent_financiera
    AND    a.tpo_credito        = v_tpo_credito;

    EXECUTE FUNCTION fn_habil_siguiente(v_f_hoy + 1,1)
                INTO v_f_referencia;

    LET v_ano             = YEAR(v_f_referencia);
    LET v_mes             = MONTH(v_f_referencia);
    LET v_dia             = DAY(v_f_referencia);
    LET v_ano_ref         = v_ano;
    LET v_mes_ref         = TO_CHAR(v_mes, "&&");
    LET v_dia_ref         = TO_CHAR(v_dia, "&&");
    LET v_ent_fin_ref     = TO_CHAR(v_cve_ent_financiera,"&&&");
    
    LET v_referencia = TRIM(v_referencia)|| 
                       v_dia_ref         ||
                       v_mes_ref         ||
                       v_ano_ref         ||
                       v_prefijo_rfc_ef  ||
                       v_ent_fin_ref;
    
    --IF v_tpo_credito = 2 THEN
    --   LET v_referencia = "AI06"; --Apoyo INFONAVIT (Aportaciones Subsecuentes)  
    --ELSE
    --   LET v_referencia = "AI07"; --Apoyo INFONAVIT (Uso del Saldo en Garantía)
    --END IF

    --LET v_ent_fin_ref     = TO_CHAR(v_cve_ent_financiera,"&&&&");
    --LET v_consecutivo_ref = TO_CHAR(v_id_factura, "&&");
    --LET v_ano             = YEAR(v_f_factura);
    --LET v_mes             = MONTH(v_f_factura);
    --LET v_dia             = DAY(v_f_factura);
    --LET v_ano_ref         = v_ano;
    --LET v_mes_ref         = TO_CHAR(v_mes, "&&");
    --LET v_dia_ref         = TO_CHAR(v_dia, "&&");
  
    --LET v_referencia = TRIM(v_referencia)|| 
    --                   v_ent_fin_ref     || 
    --                   v_consecutivo_ref ||
    --                   v_ano_ref[3,4]    || 
    --                  v_mes_ref         || 
    --                   v_dia_ref;

    {SELECT COUNT(*)
    INTO   v_tot_reg_fac
    FROM   dis_ctr_factura_aps a
    WHERE  a.referencia = v_referencia;
    IF v_tot_reg_fac = 0 THEN
       LET v_id_factura = v_id_factura + 1;
    ELSE
       SELECT a.importe
       INTO   v_imp_acum
       FROM   dis_ctr_factura_aps a
       WHERE  a.referencia = v_referencia;

       LET v_imp_acum = v_imp_acum + v_importe;

       UPDATE dis_ctr_factura_aps
       SET    importe    = v_imp_acum
       WHERE  referencia = v_referencia;

       LET v_tot_reg_fac = 0;
       LET v_imp_acum    = 0;

       CONTINUE FOREACH;
    END IF}

    INSERT INTO dis_ctr_factura_aps VALUES(v_folio_factura,
                                           v_f_factura,
                                           v_id_factura,
                                           v_tpo_credito,
                                           v_cve_ent_financiera,
                                           v_clabe,
                                           v_cta_contable,
                                           0,
                                           v_referencia,
                                           v_importe,
                                           v_banco_interlocutor,
                                           "");
                
    LET v_referencia          = "";
    LET v_ent_fin_ref         = "";
    LET v_consecutivo_ref     = "";
    LET v_ano                 = "";
    LET v_mes                 = "";
    LET v_dia                 = "";
    LET v_ano_ref             = 0;
    LET v_mes_ref             = 0;
    LET v_dia_ref             = 0;

  END FOREACH

  --TRACE 'Finaliza sp_dis_fac_apo_subs con valor '||v_bnd_proceso;
  LET v_char = "Terminado la autorización del pago correctamente";
	  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


