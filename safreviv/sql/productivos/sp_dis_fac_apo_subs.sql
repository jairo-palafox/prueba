






CREATE PROCEDURE "safreviv".sp_dis_fac_apo_subs(p_folio_factura DECIMAL(9,0)) --Entidad Financiera
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 01092016
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
DEFINE v_ent_fin_ref               CHAR(4);
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

--Estado del registro
DEFINE v_bnd_proceso               SMALLINT;
DEFINE v_char                      CHAR(20);

DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER ;
DEFINE isam_err                    INTEGER ;
DEFINE error_info                  CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

-- SET DEBUG FILE TO '/ds/safreviv_int/ocg/ERR_sp_dis_fac_apo_subs.TRACE';
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

LET v_folio_transaccion   = 0;
LET v_id_ctr_transaccion  = 0;
LET v_f_transaccion       = "";
LET v_id_ocg_detalle      = 0;
LET v_tot_rl              = 0;

  SET PDQPRIORITY HIGH;

  SELECT apo.id_dis_interface_ef,
         afi.id_derechohabiente,
         afi.nss,
         afi.nombre_af,
         afi.ap_paterno_af,
         afi.ap_materno_af,
         afi.curp
  FROM   tmp_dis_fac_aps_tns apo,
         afi_derechohabiente afi
  WHERE  apo.id_derechohabiente = afi.id_derechohabiente
  INTO TEMP tmp_dis_fac_apo_subs;

  UPDATE STATISTICS FOR TABLE tmp_dis_fac_apo_subs; 

  FOREACH
    --Busca los id_derechohabiente de la tabla principal
    SELECT b.nss, a.id_dis_interface_ef, a.id_derechohabiente, 
           a.concepto, a.folio_transaccion, a.id_ctr_transaccion, 
           a.f_transaccion, a.cve_ent_financiera, a.periodo_pago,
           a.f_pago, a.folio_sua
    INTO   v_nss, v_id_dis_interface_ef, v_id_derechohabiente, 
           v_concepto, v_folio_transaccion, v_id_ctr_transaccion, 
           v_f_transaccion, v_cve_ent_financiera, v_periodo_pago,
           v_f_pago, v_folio_sua
    FROM   tmp_dis_fac_aps_tns a,   
           tmp_dis_fac_apo_subs b
    WHERE  a.id_dis_interface_ef = b.id_dis_interface_ef
    AND    a.id_derechohabiente  = b.id_derechohabiente

    --LET v_id_ocg_detalle = seq_ocg_detalle.NEXTVAL;
   
    ---Punto 2 queda PENDIENTE, Tipos de Crédito Apoyo INFONAVIT
    ---Punto 2 queda PENDIENTE, Tipos de Crédito COFINAVIT

    --3)Validaciones de concepto, Tipos de Crédito Apoyo INFONAVIT
    IF v_concepto = 107 THEN -- Recurrente por Facturar
       LET v_concepto_f = 117; -- Recurrente facturado      
    END IF
    	
    IF v_concepto = 109 THEN -- Alta por facturar
       LET v_concepto_f = 127; -- Alta facturada
    END IF

    --3)Validaciones de concepto, Tipos de Crédito Uso de Garantía
    IF v_concepto = 307 THEN -- Recurrente por Facturar
       LET v_concepto_f = 317; -- Recurrente facturado      
    END IF
    
    IF v_concepto = 407 THEN -- Recurrente por Facturar
       LET v_concepto_f = 417; -- Recurrente facturado      
    END IF
    
    --3)Validaciones de concepto, Tipos de Crédito COFINAVIT
    IF v_concepto = 807 THEN -- Recurrente
       LET v_concepto_f = 817; -- Recurrente facturado

       --FALTA
       ---Punto 4 queda PENDIENTE, Tipos de Crédito COFINAVIT
       --4.1 queda PENDIENTE, Tipos de Crédito COFINAVIT (Relación Laboral) 
       SELECT COUNT(*)
       INTO   v_tot_rl
       FROM   safre_tmp:tmp_relacion_laboral a  --Pendiente de definir
       WHERE  a.nss = v_nss;
       IF v_tot_rl >= 1 THEN
          --FALTA
          --4.2 queda PENDIENTE, Tipos de Crédito COFINAVIT
          ---FALTA LA DEFINICIÓN DE LA TABLA DE APORTACIONES
       END IF
    END IF
    	
    IF v_concepto = 809 THEN -- Alta por facturar
       LET v_concepto_f = 827; -- Alta facturada       
    END IF
 
    --4)Se asigna el estado 30, Tipos de Crédito Apoyo INFONAVIT
    --5)Se asigna el estado 30, Tipos de Crédito COFINAVIT    	
    UPDATE dis_ctr_aps_tns
    SET    estado             = 30, -- Facturado
           concepto           = v_concepto_f,
           folio_factura      = p_folio_factura,
           f_factura          = TODAY
    WHERE  folio_transaccion  = v_folio_transaccion
    AND    id_ctr_transaccion = v_id_ctr_transaccion;

    ---5)Actualizar fecha de proceso de tabla transacciones PENDIENTE Tipos de Crédito Apoyo INFONAVIT
    ---6)Actualizar fecha de proceso de tabla transacciones PENDIENTE Tipos de Crédito COFINAVIT
    UPDATE ocg_ctr_transaccion 
    SET    f_proceso              = TODAY,
           concepto               = v_concepto_f,
           estado                 = 40 --Facturado
    WHERE  folio_referencia       = v_folio_transaccion
    AND    id_ocg_ctr_transaccion = v_id_ctr_transaccion;

    LET v_cve_ent_financiera = 0;
    LET v_nss                = "";
    LET v_concepto_f         = 0;
    LET v_tot_rl             = 0;
  END FOREACH

  --TRACE 'Finaliza sp_dis_fac_apo_subs con valor '||v_bnd_proceso;
  LET v_char = "Terminada factura correctamente";
  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


