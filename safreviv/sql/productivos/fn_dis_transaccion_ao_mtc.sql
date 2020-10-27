






create procedure "safreviv".fn_dis_transaccion_ao_mtc(p_folio_ajuste         DECIMAL(9,0),  --Folio Ajuste
                                           p_folio_nuevo_dis      DECIMAL(9,0))  --Folio de liquidación de la nueva dispersión
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 14022014
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_precio_fec_hoy      DECIMAL(19,14); --Precio de fondo del dia

DEFINE v_derechohabiente_pag DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_nss                 CHAR(11);       --NSS del derechohabiente
DEFINE v_folio_sua           DECIMAL(6);     --Folio detalle trabajdor
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_folio_dis           DECIMAL(9,0);   --Folio de dispersión
DEFINE v_num_credito         DECIMAL(10,0);  --Numero de crédito
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_cxc_calculada       DECIMAL(12,2);  --Cuenta por Cobrar Calculada

DEFINE v_id_dis_interface_hs DECIMAL(9,0);   --ID de la tabla dis_interface_hs
DEFINE v_monto_amortizacion  DECIMAL(12,2);  --Monto de amortización
DEFINE v_f_pago              DATE;           --Fecha de pago

DEFINE v_monto_acciones      DECIMAL(16,6);  --Monto en acciones
DEFINE v_monto_pesos         DECIMAL(12,2);  --Monto en pesos
DEFINE v_f_valor             DATE;           --Fecha valor
DEFINE v_f_registro          DATE;           --Fecha registro
DEFINE v_h_registro          DATETIME HOUR TO SECOND; --Hora registro
DEFINE v_origen              CHAR(20);       --Origen del movimiento

DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_transaccion7_ap     SMALLINT;
DEFINE v_transaccion7_am     SMALLINT;
DEFINE v_aiv_ap_pat	     DECIMAL(18,6);

DEFINE v_f_liquida_cred      DATE;
DEFINE v_tpo_credito         SMALLINT;
DEFINE v_valida		     SMALLINT;
DEFINE v_cve_ent_receptora   CHAR(3);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_precio_fec_hoy       = 0.00;

LET v_derechohabiente_pag  = 0;
LET v_nss                  = "";
LET v_folio_sua            = 0;
LET v_periodo_pago         = "";
LET v_nrp                  = "";
LET v_folio_dis            = 0;
LET v_num_credito          = 0;
LET v_imp_ap_pat           = 0.00;
LET v_imp_am_cre           = 0.00;
LET v_cxc_calculada        = 0.00;

LET v_id_dis_interface_hs  = 0;
LET v_monto_amortizacion   = 0.00;
LET v_f_pago               = TODAY;

LET v_monto_acciones       = 0.00;
LET v_monto_pesos          = 0.00;
LET v_f_valor              = TODAY;
LET v_f_registro           = TODAY;
LET v_h_registro           = CURRENT HOUR TO SECOND;
LET v_origen               = "Dis Cartera-201306";                

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_transaccion_ao_mtc.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_transaccion_ao_mtc.TRACE';
--TRACE ON;

   --Leer los registros de la tabla dis_ajuste_op_mtc
   FOREACH
     SELECT afi.id_derechohabiente,
            mtc.nss,
            mtc.folio_sua,
            mtc.periodo_pago,
            mtc.nrp,
            mtc.folio_dis,
            mtc.num_crd_ifv,
            mtc.imp_ap_pat,
            mtc.imp_am_cre,
	    mtc.cxc_calculada 
     INTO   v_derechohabiente_pag,
            v_nss,
            v_folio_sua,
            v_periodo_pago,
            v_nrp,
            v_folio_dis,
            v_num_credito,
            v_imp_ap_pat,
            v_imp_am_cre,
	    v_cxc_calculada
     FROM   afi_derechohabiente afi,
            dis_ajuste_op_mtc mtc
     WHERE  afi.nss           = mtc.nss
     AND    mtc.folio_ajuste  = p_folio_ajuste

     IF v_imp_am_cre IS NULL THEN
        LET v_imp_am_cre = 0;
     END IF

     --Si las amortizaciones son menores o
     --iguales a cero no se dispersa el registro
     IF (v_imp_am_cre <= 0.00) THEN
        CONTINUE FOREACH;
     END IF

     SELECT hs.id_dis_interface_hs,
            hs.f_pago,
            hs.imp_am_cre
     INTO   v_id_dis_interface_hs,
            v_f_pago,
            v_monto_amortizacion
     FROM   dis_interface_hs hs
     WHERE  hs.folio_liquida      = v_folio_dis 
     AND    hs.id_derechohabiente = v_derechohabiente_pag
     AND    hs.folio_sua          = v_folio_sua
     AND    hs.periodo_pago       = v_periodo_pago
     AND    hs.nrp                = v_nrp
     AND    hs.num_crd_ifv        = v_num_credito;

     SELECT cta.monto_acciones * -1,
            cta.monto_pesos * -1,
            cta.f_valor,
            --cta.f_registro,
            --cta.h_registro,
            cta.origen
     INTO   v_monto_acciones,
            v_monto_pesos,
            v_f_valor,
            --v_f_registro,
            --v_h_registro,
            v_origen
     FROM   cta_movimiento cta
     WHERE  cta.folio_liquida      = v_folio_dis
     AND    cta.id_referencia      = v_id_dis_interface_hs
     AND    cta.id_derechohabiente = v_derechohabiente_pag
     AND    cta.subcuenta          = 41
     AND    cta.movimiento         = 52;

     --Movimiento 671 - ABONO AJUSTE AMORTIZACIÓN MTC
     INSERT INTO cta_movimiento VALUES (TODAY,
                                        v_derechohabiente_pag,
                                        41,
                                        10,
                                        671,
                                        p_folio_ajuste,
                                        v_id_dis_interface_hs,
                                        v_monto_acciones,
                                        v_monto_pesos,
                                        v_f_valor,
                                        v_f_registro,
                                        v_h_registro,
                                        v_origen); 

     --Movimiento 52 - CARGO AMORTIZACIÓN CRÉDITO TRADICIONAL
     INSERT INTO cta_movimiento VALUES (TODAY,
                                        v_derechohabiente_pag,
                                        41,
                                        10,
                                        52,
                                        p_folio_nuevo_dis,
                                        v_id_dis_interface_hs,
                                        v_cxc_calculada * -1,
                                        v_cxc_calculada * -1,
                                        v_f_valor,
                                        v_f_registro,
                                        v_h_registro,
                                        v_origen); 

     UPDATE dis_interface_hs 
     SET    imp_am_cre         = v_cxc_calculada
     WHERE  folio_liquida      = v_folio_dis 
     AND    id_derechohabiente = v_derechohabiente_pag
     AND    folio_sua          = v_folio_sua
     AND    periodo_pago       = v_periodo_pago
     AND    nrp                = v_nrp
     AND    num_crd_ifv        = v_num_credito;

     UPDATE dis_ajuste_op_mtc 
     SET    f_pago             = v_f_pago
     WHERE  folio_ajuste       = p_folio_ajuste 
     AND    nss                = v_nss
     AND    folio_sua          = v_folio_sua
     AND    periodo_pago       = v_periodo_pago
     AND    nrp                = v_nrp
     AND    num_crd_ifv        = v_num_credito;

   END FOREACH; --dis_ajuste_op_mtc

   UPDATE glo_folio
   SET    status = 2
   WHERE  folio  = p_folio_ajuste;

   UPDATE glo_folio
   SET    status = 2
   WHERE  folio  = p_folio_nuevo_dis;

   --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
   LET v_char = "  Ajuste finalizo correctamente (Créditos MTC´s)";
   RETURN v_bnd_proceso , 0 , v_char, '';

END PROCEDURE;


