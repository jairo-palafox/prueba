






CREATE PROCEDURE "safreviv".sp_dis_pagos_avances(p_folio    DECIMAL(9,0),
                                      p_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 12062014
--Declaración de variables
DEFINE v_nss 		        CHAR(11);
DEFINE v_num_credito            DECIMAL(10,0);
DEFINE v_bimestre               CHAR(6);
DEFINE v_ava_aportacion         DECIMAL(8,2);
DEFINE v_ava_amortizacion       DECIMAL(8,2);

DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_estado 	        SMALLINT;
DEFINE v_desc_resultado         CHAR(40);

DEFINE v_precio_fec_hoy         DECIMAL(19,14); --Derechohabiente de cuenta credito
DEFINE v_imp_ap_pat             DECIMAL(12,2);  --Importe aportaciones patronales -- Son los pesos no convertidos
DEFINE v_imp_am_cre             DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_aiv_ap_pat             DECIMAL(18,6);
DEFINE v_folio_sua              DECIMAL(6,0);   --Folio SUA
DEFINE v_monto_pesos            DECIMAL(22,2);  --

DEFINE v_dif_ap_avpag           DECIMAL(12,2);
DEFINE v_dif_am_avpag           DECIMAL(12,2);

DEFINE v_folio_liquida 	        DECIMAL(9,0);
DEFINE v_id_interface_ef        DECIMAL(9,0);
DEFINE v_reg_pag_imss 	        CHAR(11);
DEFINE v_rfc_pat 	        CHAR(13);
DEFINE v_periodo_pago 	        CHAR(6);
DEFINE v_f_pago 	        DATE;
DEFINE v_f_valor 	        DATE;
DEFINE v_rfc 		        CHAR(13);
DEFINE v_curp 		        CHAR(18);
DEFINE v_nombre 	        CHAR(50);
DEFINE v_usdi_periodo 	        DECIMAL(12,2);
DEFINE v_dias_cotizados_bim     INTEGER;
DEFINE v_dias_incapacidad_bim   INTEGER;
DEFINE v_dias_ausentismo_bim    INTEGER;
DEFINE v_imp_apo_pat 	        DECIMAL(12,2);
DEFINE v_imp_amo_cred 	        DECIMAL(12,2);
DEFINE v_dif_apo_sub 	        DECIMAL(12,2);
DEFINE v_afore 		        SMALLINT;
DEFINE v_f_liquidacion 	        DATE;

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
   LET v_nss 		        = "";
   LET v_num_credito            = 0;
   LET v_bimestre               = "";
   LET v_ava_aportacion         = 0.00;
   LET v_ava_amortizacion       = 0.00;

   LET v_id_derechohabiente     = 0;
   LET v_estado                 = 0;
   LET v_desc_resultado         = "";

   LET v_precio_fec_hoy         = 0.00;
   LET v_imp_ap_pat             = 0.00;
   LET v_imp_am_cre             = 0.00;
   LET v_aiv_ap_pat             = 0.00;
   LET v_folio_sua              = 0;
   LET v_monto_pesos            = 0.00;

   LET v_dif_ap_avpag           = 0.00;
   LET v_dif_am_avpag           = 0.00;

   LET v_bnd_proceso            = 0; --Estado correcto
   LET v_folio_liquida          = 0;
   LET v_id_interface_ef        = 0.00;
   LET v_reg_pag_imss           = "";
   LET v_rfc_pat                = "";
   LET v_periodo_pago           = "";
   LET v_f_pago                 = "";
   LET v_f_valor                = "";
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
   LET v_bnd_transaccion        = 0;
   LET v_periodo_bimestre       = "";
   LET v_aplic_int_viv_apo_pat  = 0.000000;
   LET v_val_aplic_int_viv	= 0.000000;

--SET DEBUG FILE TO '/ds/safreviv/dis/sql/sp_dis_pagos_avances.TRACE';
--SET DEBUG FILE TO '/home/safreviv/sp_dis_pagos_avances.TRACE';
-- SET DEBUG FILE TO 'sp_dis_pagos_avances.TRACE';
-- TRACE ON;

   DROP TABLE IF EXISTS dis_pago_avance;
   CREATE TABLE dis_pago_avance (id_derechohabiente DECIMAL(9,0) NOT NULL ,
                                 nss                CHAR(11),
                                 num_credito        DECIMAL(10,0),
                                 bimestre           CHAR(6),
                                 ava_aportacion     DECIMAL(8,2),
                                 ava_amortizacion   DECIMAL(8,2),
                                 pag_aivs           DECIMAL(14,6),
                                 pag_aportacion     DECIMAL(8,2),
                                 pag_amortizacion   DECIMAL(8,2),
                                 folio_sua          DECIMAL(6,0),
                                 ava_apo_pendiente  DECIMAL(8,2),
                                 ava_amo_pendiente  DECIMAL(8,2),
                                 resultado          CHAR(40))
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

   SET PDQPRIORITY HIGH;

   --Identificar precio de Acción del día
   SELECT precio_fondo
   INTO   v_precio_fec_hoy
   FROM   glo_valor_fondo
   WHERE  fondo       = 11
   AND    f_valuacion = TODAY;

   FOREACH
     SELECT ava.nss,
            ava.num_credito,
            ava.bimestre,
            ava.ava_aportacion   / 100,
            ava.ava_amortizacion / 100
     INTO   v_nss,
            v_num_credito,
            v_bimestre,
            v_ava_aportacion,
            v_ava_amortizacion
     FROM   safre_tmp:tmp_pagos_avance1 ava

     {EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
     INTO v_periodo_bimestre;}

     LET v_estado = 0;

     --Validación de Negocio NSS
     IF v_nss IS NULL THEN
        LET v_estado         = 10; --nss nulo
        LET v_desc_resultado = 'NSS NULO';
     ELSE
        IF LENGTH(v_nss) <> 11 THEN
           LET v_estado         = 20; --longitud de nss diferente de 11
           LET v_desc_resultado = 'NSS DIFERENTE A 11 POSICIONES';
        ELSE
           --#Obtenemos id_derechohabiente
           SELECT afi.id_derechohabiente
           INTO   v_id_derechohabiente
           FROM   afi_derechohabiente afi
           WHERE  afi.nss = v_nss;
           --#Asigna id_derechohabiente si no se encuentra en tabla
           IF v_id_derechohabiente IS NULL THEN
              LET v_id_derechohabiente = "999999999";

              --rechazo por no existir en el maestro de derechohabientes
              LET v_estado         = 30;
              LET v_desc_resultado = 'NSS NO EXISTE EN LA BD DE AFILIADOS';
           END IF
        END IF
     END IF

     --Validación de Negocio Numero de Crédito
     IF v_estado = 0 THEN
        IF v_num_credito IS NULL THEN
           LET v_estado         = 40; --numero de crédito nulo
           LET v_desc_resultado = 'NUMERO DE CREDITO NULO';
        END IF
     END IF

     --Validación de Negocio Bimestre
     IF v_estado = 0 THEN
        IF v_bimestre IS NULL THEN
           LET v_estado         = 50; --bimestre nulo
           LET v_desc_resultado = 'BIMESTRE NULO';
        END IF
     END IF

     --Validación de Negocio Monto Avance Aportación
     IF v_estado = 0 THEN
        IF v_ava_aportacion < 0.00 THEN
           LET v_estado         = 60; --avance aportación menor a 0
           LET v_desc_resultado = 'MONTO AVANCE APORTACION MENOR A 0';
        END IF
     END IF

     --Validación de Negocio Monto Avance Amortización
     IF v_estado = 0 THEN
        IF v_ava_amortizacion < 0.00 THEN
           LET v_estado         = 70; --avance amortización menor a 0
           LET v_desc_resultado = 'MONTO AVANCE AMORTIZACION MENOR A 0';
        END IF
     END IF

     --Obtiene los pagos del avance
     IF v_estado = 0 THEN
        FOREACH
          SELECT pg.aiv_ap_pat,
                 pg.imp_ap_pat,
                 pg.imp_am_cre,
                 pg.folio_sua
          INTO   v_aiv_ap_pat,
                 v_imp_ap_pat,
                 v_imp_am_cre,
                 v_folio_sua
          FROM   cta_his_pagos pg
          WHERE  pg.id_derechohabiente             = v_id_derechohabiente
          AND    fn_bimestre_pago(pg.periodo_pago) = v_bimestre
          --AND    pg.num_crd_ifv                    = v_num_credito
          AND    pg.ind_liquidacion           NOT IN (1,6)
          AND    pg.destino_ap_viv                 = 1

          IF v_aiv_ap_pat IS NULL THEN
             LET v_aiv_ap_pat = 0;
          END IF

          IF v_imp_am_cre IS NULL THEN
             LET v_imp_am_cre = 0;
          END IF

          --Si las aportaciones y amortizaciones son menores o
          --iguales a cero no se dispersa el registro
          IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
             --No se encontraron pagos
             LET v_aiv_ap_pat     = 0;
             LET v_monto_pesos    = 0;
             LET v_imp_am_cre     = 0;
             LET v_folio_sua      = 0;
             LET v_dif_ap_avpag   = v_ava_aportacion;
             LET v_dif_am_avpag   = v_ava_amortizacion;
             LET v_desc_resultado = 'AVANCE SIN PAGO POR CUBRIR';

             --Integra en la tabla los pagos del avance
             INSERT INTO dis_pago_avance VALUES (v_id_derechohabiente,
                                                 v_nss,
                                                 v_num_credito,
                                                 v_bimestre,
                                                 v_ava_aportacion,
                                                 v_ava_amortizacion,
                                                 v_aiv_ap_pat,
                                                 v_monto_pesos,
                                                 v_imp_am_cre,
                                                 v_folio_sua,
                                                 v_dif_ap_avpag,
                                                 v_dif_am_avpag,
	                                         v_desc_resultado);
             CONTINUE FOREACH;
          END IF

          LET v_desc_resultado = 'AVANCE CON PAGO';

          --Se realiza la conversión de los pesos por el precio de acción del día
          LET v_monto_pesos  = (v_aiv_ap_pat * v_precio_fec_hoy);

          --Realizar compensación para identificar diferencias
          LET v_dif_ap_avpag = v_ava_aportacion   - v_monto_pesos;
          LET v_dif_am_avpag = v_ava_amortizacion - v_imp_am_cre;

          --Integra en la tabla los pagos del avance
          INSERT INTO dis_pago_avance VALUES (v_id_derechohabiente,
                                              v_nss,
                                              v_num_credito,
                                              v_bimestre,
                                              v_ava_aportacion,
                                              v_ava_amortizacion,
                                              v_aiv_ap_pat,
                                              v_monto_pesos,
                                              v_imp_am_cre,
                                              v_folio_sua,
                                              v_dif_ap_avpag,
                                              v_dif_am_avpag,
	                                      v_desc_resultado);
        END FOREACH;

        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           --No se encontraron pagos
           LET v_aiv_ap_pat     = 0;
           LET v_monto_pesos    = 0;
           LET v_imp_am_cre     = 0;
           LET v_folio_sua      = 0;
           LET v_dif_ap_avpag   = v_ava_aportacion;
           LET v_dif_am_avpag   = v_ava_amortizacion;
           LET v_desc_resultado = 'AVANCE SIN PAGO POR CUBRIR';

           --Integra en la tabla los pagos del avance
           INSERT INTO dis_pago_avance VALUES (v_id_derechohabiente,
                                               v_nss,
                                               v_num_credito,
                                               v_bimestre,
                                               v_ava_aportacion,
                                               v_ava_amortizacion,
                                               v_aiv_ap_pat,
                                               v_monto_pesos,
                                               v_imp_am_cre,
                                               v_folio_sua,
                                               v_dif_ap_avpag,
                                               v_dif_am_avpag,
	                                       v_desc_resultado);
        END IF
     ELSE
        LET v_aiv_ap_pat     = 0;
        LET v_monto_pesos    = 0;
        LET v_imp_am_cre     = 0;
        LET v_folio_sua      = 0;
        LET v_dif_ap_avpag   = v_ava_aportacion;
        LET v_dif_am_avpag   = v_ava_amortizacion;

        --Integra en la tabla los pagos del avance
        INSERT INTO dis_pago_avance VALUES (v_id_derechohabiente,
                                            v_nss,
                                            v_num_credito,
                                            v_bimestre,
                                            v_ava_aportacion,
                                            v_ava_amortizacion,
                                            v_aiv_ap_pat,
                                            v_monto_pesos,
                                            v_imp_am_cre,
                                            v_folio_sua,
                                            v_dif_ap_avpag,
                                            v_dif_am_avpag,
	                                    v_desc_resultado);
     END IF
   END FOREACH;

   {CREATE INDEX ixdis_pago_avance ON dis_pago_avance
   (id_derechohabiente) IN dis_ix_dbs;}

   UPDATE STATISTICS FOR TABLE dis_pago_avance;

-- TRACE 'Finaliza sp_dis_pagos_avances con valor '||v_bnd_proceso;
   LET v_char = "Terminado Pag_avanc_SPL";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


