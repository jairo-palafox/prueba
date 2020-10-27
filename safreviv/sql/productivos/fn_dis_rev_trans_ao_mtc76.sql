






create procedure "safreviv".fn_dis_rev_trans_ao_mtc76(p_folio_ajuste         DECIMAL(9,0),  --Folio Ajuste
                                           p_folio_nuevo_dis      DECIMAL(9,0))  --Folio de liquidación de la nueva dispersión
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 18032014
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_precio_fec_hoy      DECIMAL(19,14); --Precio de fondo del dia

DEFINE v_id_dis_interface_hs DECIMAL(9,0);   --Id de la interface de hs
DEFINE v_derechohabiente_pag DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_folio_sua           DECIMAL(6);     --Folio detalle trabajdor
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_folio_liquida       DECIMAL(9,0);   --Folio de liquidación
DEFINE v_num_crd_ifv         DECIMAL(10,0);  --Numero de crédito
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito

DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_precio_fec_hoy       = 0.00;

LET v_id_dis_interface_hs  = 0;
LET v_derechohabiente_pag  = 0;
LET v_folio_sua            = 0;
LET v_periodo_pago         = "";
LET v_f_pago               = TODAY;
LET v_nrp                  = "";
LET v_folio_liquida        = 0;
LET v_num_crd_ifv          = 0;
LET v_imp_ap_pat           = 0.00;
LET v_imp_am_cre           = 0.00;

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_rev_trans_ao_mtc76.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_rev_trans_ao_mtc76.TRACE';
--TRACE ON;

   --Leer los registros de la tabla dis_interface_hs_mtc76
   FOREACH
     SELECT mtc.id_dis_interface_hs,
            mtc.id_derechohabiente,
            mtc.folio_sua,
            mtc.periodo_pago,
            mtc.f_pago,
            mtc.nrp,
            mtc.folio_liquida,
            mtc.num_crd_ifv,
            mtc.imp_ap_pat,
            mtc.imp_am_cre
     INTO   v_id_dis_interface_hs,
            v_derechohabiente_pag,
            v_folio_sua,
            v_periodo_pago,
            v_f_pago,
            v_nrp,
            v_folio_liquida,
            v_num_crd_ifv,
            v_imp_ap_pat,
            v_imp_am_cre
     FROM   dis_interface_hs_mtc76 mtc

     --Reverso del folio de ajuste ¿?
     --Movimiento 671 - ABONO AJUSTE AMORTIZACIÓN MTC
     DELETE cta_movimiento
     WHERE  folio_liquida = p_folio_ajuste
     AND    id_referencia = v_id_dis_interface_hs;

     --Reverso del folio de la nueva dispersión ¿?
     --Movimiento 52 - CARGO AMORTIZACIÓN CRÉDITO TRADICIONAL
     DELETE cta_movimiento
     WHERE  folio_liquida = p_folio_nuevo_dis
     AND    id_referencia = v_id_dis_interface_hs;

     UPDATE dis_interface_hs 
     SET    imp_am_cre          = v_imp_am_cre
     WHERE  folio_liquida       = v_folio_liquida 
     AND    id_dis_interface_hs = v_id_dis_interface_hs
     AND    id_derechohabiente  = v_derechohabiente_pag
     AND    folio_sua           = v_folio_sua
     AND    periodo_pago        = v_periodo_pago
     AND    f_pago              = v_f_pago
     AND    nrp                 = v_nrp
     AND    num_crd_ifv         = v_num_crd_ifv;

   END FOREACH; --dis_interface_hs_mtc

   UPDATE glo_folio
   SET    status = 1 
   WHERE  folio  = p_folio_ajuste;

   UPDATE glo_folio
   SET    status = 1
   WHERE  folio  = p_folio_nuevo_dis;

   --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
   LET v_char = "  Reverso ajuste finalizo correctamente (Créditos MTC´s)";
   RETURN v_bnd_proceso , 0 , v_char, '';

END PROCEDURE;


