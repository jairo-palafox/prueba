






CREATE PROCEDURE "safreviv".fn_dis_transaccion2(p_fecha_01mm           DATE,          --Fecha de movimiento, proceso
                                     p_usuario              CHAR(20),      --Usuario del proceso
                                     p_folio_disp           DECIMAL(9,0),  --Folio de dispersión
                                     p_pid                  DECIMAL(9,0),  --Pid genrado para el proceso
                                     p_proceso_cod          SMALLINT,      --Proceso dispersión preliquidación
                                     p_opera_cod            SMALLINT,      --Operacion preliquidacion
                                     p_folio_reg_pag        DECIMAL(9,0),  --Folio de registro de pagos
                                     p_proceso_cod_reg_pago SMALLINT,      --Codigo proceso de registro de pagos
                                     p_periodo_pago_masivo  CHAR(6),       --Periodo de pago Masivo LQ
                                     p_periodo_bimestre     CHAR(6))       --Periodo bimestre Masivo LQ
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 05112018
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE r_instruccion_mdt     SMALLINT;       --Bandera instruccion mandato
DEFINE r_bnd_epera_error     SMALLINT;       --Bandera operacion error
DEFINE v_folio_avpag         DECIMAL(9,0);   --Folio de avance de pago
DEFINE v_precio_fec_hoy      DECIMAL(19,14); --Precio de fondo del dia
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_periodo_bimestre    CHAR(6);        --Periodo de pago
DEFINE v_folio_sua           DECIMAL(6);     --Folio detalle trabajdor
DEFINE v_id_referencia       DECIMAL(9,0);   --ID de referencia
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales
DEFINE v_monto_pesos         DECIMAL(22,2);  --Importe aportaciones patronales -- Se cambia la variable ap_pat por v_monto_pesos (pesos convertidos)
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
DEFINE v_derechohabiente_crd DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito_crd     DECIMAL(10,0);
DEFINE v_localiza_trabajador CHAR(01);       --Localización del trabajador
DEFINE v_tpo_aclaracion      CHAR(02);       --Tipo de aclaración
DEFINE v_tpo_patron          CHAR(02);       --Tipo de patron
DEFINE v_ind_liquidacion     SMALLINT;       --Indicador Liquidación de pagos
DEFINE v_derechohabiente_pag DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito_pag     DECIMAL(10,0);
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_compensacion_apo    SMALLINT;       --Bandera de compensación aportación
DEFINE v_compensacion_amo    SMALLINT;       --Bandera de compensación amortización
DEFINE v_tpo_avance          SMALLINT;       --Tipo de Avance (Registro o Pago Virtual)
DEFINE v_ban_dif             SMALLINT;       --Bandera de Diferencias
DEFINE v_destino_ap_viv      CHAR(1);        --Destino Aportacion Vivienda
DEFINE v_origen_dispersion   SMALLINT;       --Origen dispersión
DEFINE v_ind_masivo          SMALLINT;       --Indicador de lq masivo
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
DEFINE v_bnd_existe_nrp      SMALLINT;
DEFINE v_tot_marca_tag       SMALLINT;
DEFINE v_tpo_originacion     SMALLINT;
DEFINE v_tpo_dscto           SMALLINT;
DEFINE v_id_crd_ceros        DECIMAL(9,0); --Req SACI2018-175
DEFINE v_edo_num_credito     SMALLINT;     --Req SACI2018-175

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status, isam_err, error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET r_instruccion_mdt      = 0;
LET r_bnd_epera_error      = 0;
LET v_folio_avpag          = 0;
LET v_precio_fec_hoy       = 0.00;
LET v_nrp                  = "";
LET v_periodo_pago         = "";
LET v_periodo_bimestre     = "";
LET v_folio_sua            = 0;
LET v_id_referencia        = 0;
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
LET v_derechohabiente_crd  = 0;
LET v_num_credito_crd      = 0;
LET v_localiza_trabajador  = "";
LET v_tpo_aclaracion       = "";
LET v_tpo_patron           = "";
LET v_bnd_transaccion      = 0;
LET v_derechohabiente_pag  = 0;
LET v_num_credito_pag      = 0;
LET v_num_credito          = 0;
LET v_compensacion_apo     = 0;
LET v_compensacion_amo     = 0;
LET v_tpo_avance           = 0;
LET v_destino_ap_viv       = 0;
LET v_origen_dispersion    = 0;
LET v_transaccion7_ap	   = 0;
LET v_transaccion7_am	   = 0;
LET v_ind_masivo           = 0;
LET v_aiv_ap_pat           = 0.00;
LET v_monto_pesos          = 0.00;
LET v_f_otorga             = '';
LET v_f_liquida_cred       = '';
LET v_tpo_credito          = 0;
LET v_valida               = 0;
LET v_cve_ent_receptora	   = "";
LET v_bnd_existe_nrp       = 0;
LET v_tot_marca_tag        = 0;
LET v_tpo_originacion      = 0;
LET v_tpo_dscto            = 0;
LET v_id_crd_ceros         = 0;   --Req SACI2018-175
LET v_edo_num_credito      = 0;   --Req SACI2018-175

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_transaccion2.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_transaccion2.TRACE';
--TRACE ON;

  --Identificar precio de Acción del día
  SELECT precio_fondo
  INTO   v_precio_fec_hoy
  FROM   glo_valor_fondo
  WHERE  fondo       = 11
  AND    f_valuacion = TODAY;

  --Leer los registros de la tabla cta_his_pagos
  --donde el destino de la aportacion sea Infonavit
  --para los archivos masivos LQ
  --Identifica precio de fondo de Fecha de Pago patronal
  FOREACH
     SELECT pag.id_derechohabiente,
            pag.folio_sua,
            pag.periodo_pago,
            pag.f_pago,
            pag.nrp,
            pag.ind_liquidacion,
            pag.id_referencia,
            pag.localiza_trabajador,
            pag.tpo_aclaracion,
            pag.tpo_patron,
            pag.imp_ap_pat,
            pag.imp_am_cre,
            pag.aiv_ap_pat,
            pag.num_crd_ifv,
            pag.destino_ap_viv,
            fon.precio_fondo,
            pag.cve_ent_receptora
     INTO   v_derechohabiente_pag,
            v_folio_sua,
            v_periodo_pago,
            v_f_pago,
            v_nrp,
            v_ind_liquidacion,
            v_id_referencia,
            v_localiza_trabajador,
            v_tpo_aclaracion,
            v_tpo_patron,
            v_imp_ap_pat,
            v_imp_am_cre,
            v_aiv_ap_pat,
            v_num_credito_pag,
            v_destino_ap_viv,
            v_precio_f_pag,
            v_cve_ent_receptora
     FROM   cta_his_pagos pag,
            glo_valor_fondo fon
     WHERE  pag.folio                = p_folio_reg_pag
     AND    pag.ind_liquidacion NOT IN (1,6)
     AND    fon.fondo                = 11
     AND    pag.f_pago               = fon.f_valuacion
     AND    pag.periodo_pago        >= p_periodo_pago_masivo
     AND    pag.destino_ap_viv       = 1

     IF v_aiv_ap_pat IS NULL THEN
        LET v_aiv_ap_pat = 0;
     END IF

     IF v_imp_am_cre IS NULL THEN
        LET v_imp_am_cre = 0;
     END IF

     --Si las aportaciones y amortizaciones son menores o
     --iguales a cero no se dispersa el registro
     IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
        CONTINUE FOREACH;
     END IF

     IF v_periodo_pago > p_periodo_pago_masivo THEN
        EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
                     INTO v_periodo_bimestre;				 
     ELSE
        LET v_periodo_bimestre = p_periodo_bimestre;
     END IF

     LET v_num_credito     = 0;
     LET v_num_credito_crd = 0;
     LET v_tipo_trabajador = 0;
     LET v_f_otorga        = 0;
     LET v_tpo_dscto       = 0;
     LET v_tpo_originacion = 0;
     LET v_edo_num_credito = 0;  --Req SACI2018-175     

     --Se realiza la conversión de los pesos por el precio de acción del día
     LET v_monto_pesos 	   = (v_aiv_ap_pat * v_precio_fec_hoy);

     --- Eliminar la asignación cuando se active el proceso de mandatos
     LET v_monto_pesos_real = v_imp_am_cre;

     --########## Se agrega nueva función para optimización ###########
     --Identifica el crédito más reciente del derechohabiente que haya sido liquidado
     --Valor resultado = -2: No hay NSS asociado
     --Valor resultado = -1: No hay identificador de derechohabiente
     --Valor resultado = 0:  Existe crédito vigente
     --Valor resultado = 1:  No tiene crédito
     --Valor resultado = 2:  Tiene crédito Liquidado
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

     --Marca Activa 213 Crédito en Trámite
     IF v_edo_credito = 3 THEN
        --- DEJAR COMO PRINCIPAL EL NUMERO DE CREDITO
        IF v_num_credito_crd IS NULL OR
           v_num_credito_crd = 0 THEN

           IF v_num_credito_pag IS NULL OR
              v_num_credito_pag = 0 THEN
              LET v_num_credito = 0;

              --Req SACI2018-175
              LET v_edo_num_credito = 1;
              LET v_id_crd_ceros    = seq_dis_crd_ceros.NEXTVAL;

              INSERT INTO dis_crd_ceros VALUES (v_id_crd_ceros,
                                                v_derechohabiente_pag,
                                                v_folio_sua,
                                                v_periodo_bimestre,
                                                v_f_pago,
                                                v_nrp,
                                                p_folio_disp,
                                                v_num_credito,
                                                v_monto_pesos,
                                                v_monto_pesos_real,
                                                v_aiv_ap_pat,
                                                p_folio_reg_pag,
                                                v_id_referencia);
              --Req SACI2018-175
           END IF
           LET v_num_credito     = v_num_credito_pag;
           LET v_tpo_originacion = 2;
        ELSE
           LET v_num_credito     = v_num_credito_crd;
           LET v_tpo_originacion = 1;
        END IF
      
        --Identificar si tipo de trabajador 43Bis
        IF v_tipo_trabajador = 2 THEN
           INSERT INTO dis_info_inconsistente VALUES(v_derechohabiente_pag,
                                                     v_id_referencia,
                                                     p_folio_disp,
                                                     v_destino_ap_viv,
                                                     v_tipo_trabajador,
                                                     v_num_credito,
                                                     v_monto_pesos,
                                                     v_monto_pesos_real,
                                                     v_aiv_ap_pat,
                                                     1);
        END IF

        --Req SACI2018-175
        IF v_edo_num_credito = 1 THEN
           LET v_num_credito     = 0;
           LET v_tpo_originacion = 1;
        END IF
        --Req SACI2018-175

        --Req PROINFXVII-13
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
        --Req PROINFXVII-13

        EXECUTE PROCEDURE fn_dis_transaccion16(v_edo_credito,
                                               v_derechohabiente_pag,
                                               v_num_credito,
                                               v_periodo_bimestre,
                                               v_f_pago,
                                               v_imp_ap_pat,
                                               v_monto_pesos,
                                               v_precio_f_pag,
                                               v_precio_fec_hoy,
                                               p_fecha_01mm,
                                               p_proceso_cod_reg_pago,
                                               p_folio_disp,
                                               v_tpo_patron,
                                               v_localiza_trabajador,
                                               v_monto_pesos_real,
                                               v_id_referencia,
                                               v_nrp,
                                               v_folio_sua,
                                               p_folio_reg_pag,
                                               v_tipo_trabajador,
                                               v_aiv_ap_pat,    -- Se agrega valor de las AIVS
                                               v_cve_ent_receptora,
                                               v_nss,
                                               v_periodo_pago,
                                               v_destino_ap_viv,-- Se agrega destino
                                               v_tpo_credito)   -- Se agrega v_tpo_credito
                     INTO v_bnd_transaccion, v_status, error_info;
        CONTINUE FOREACH;
     END IF
	
     IF v_edo_credito = -2 OR v_edo_credito = -1 OR v_edo_credito = 1 THEN 
        LET v_num_credito_crd = 0;
        LET v_tipo_trabajador = 0;
        LET v_f_otorga        = '';
        LET v_tpo_credito     = 0;
     END IF 

     --- DEJAR COMO PRINCIPAL EL NUMERO DE CREDITO DEL CREDITO
     IF v_num_credito_crd IS NULL OR
        v_num_credito_crd = 0 THEN

        IF v_num_credito_pag IS NULL OR
           v_num_credito_pag = 0 THEN
           LET v_num_credito = 0;

           --Req SACI2018-175
           LET v_edo_num_credito = 1;
           --Req SACI2018-175
        END IF
        LET v_num_credito     = v_num_credito_pag;
        LET v_tpo_originacion = 2;
     ELSE
        LET v_num_credito     = v_num_credito_crd;
        LET v_tpo_originacion = 1;
     END IF

     --Req SACI2018-175
     IF v_edo_num_credito = 1 THEN
        LET v_num_credito     = 0;
        LET v_tpo_originacion = 1;
     END IF
     --Req SACI2018-175

     --Req PROINFXVII-13
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
     --Req PROINFXVII-13

     --Req SACI2018-175
     IF v_edo_num_credito = 1 THEN
        --Función Registro Número de Crédito en Ceros
        EXECUTE PROCEDURE fn_dis_transaccion18(v_edo_credito,
                                               v_derechohabiente_pag,
                                               v_num_credito,
                                               v_periodo_bimestre,
                                               v_f_pago,
                                               v_imp_ap_pat,
                                               v_monto_pesos,
                                               v_precio_f_pag,
                                               v_precio_fec_hoy,
                                               p_fecha_01mm,
                                               p_proceso_cod_reg_pago,
                                               p_folio_disp,
                                               v_tpo_patron,
                                               v_localiza_trabajador,
                                               v_monto_pesos_real,
                                               v_id_referencia,
                                               v_nrp,
                                               v_folio_sua,
                                               p_folio_reg_pag,
                                               v_tipo_trabajador,
                                               v_aiv_ap_pat,    -- Se agrega valor de las AIVS
                                               v_cve_ent_receptora,
                                               v_nss,
                                               v_periodo_pago,
                                               v_destino_ap_viv,-- Se agrega destino
                                               v_tpo_credito)   -- Se agrega v_tpo_credito
                     INTO v_bnd_transaccion, v_status, error_info;
        CONTINUE FOREACH;
     END IF
     --Req SACI2018-175

     --Identificar si tipo de trabajador 43Bis
     IF v_tipo_trabajador = 2 THEN
        INSERT INTO dis_info_inconsistente VALUES(v_derechohabiente_pag,
                                                  v_id_referencia,
                                                  p_folio_disp,
                                                  v_destino_ap_viv,
                                                  v_tipo_trabajador,
                                                  v_num_credito,
                                                  v_monto_pesos,
                                                  v_monto_pesos_real,
                                                  v_aiv_ap_pat,
                                                  1);
     END IF
     
     --Se agrega validación para verificar si es el nuevo tipo de trabajador del Estado
     IF v_tipo_trabajador = 3 THEN  --se crea registro para liquidar solo amortización
        EXECUTE PROCEDURE sp_dis_Transaccion13(v_derechohabiente_pag,
                                               0,
                                               v_monto_pesos_real,
                                               p_folio_disp,
                                               v_id_referencia,
                                               v_precio_fec_hoy,
                                               v_nrp,
                                               v_periodo_bimestre,
                                               v_folio_sua,
                                               v_num_credito,
                                               p_folio_reg_pag,
                                               p_proceso_cod_reg_pago,
                                               v_tpo_patron,
                                               v_f_pago,
                                               v_tipo_trabajador,
                                               v_localiza_trabajador,
                                               0) --Se agrega valor de las AIVS
                     INTO v_bnd_transaccion, v_status,error_info;
        CONTINUE FOREACH;
     END IF

     -- v_tpo_credito = 19  MEJORAVIT
     -- v_tpo_credito = 20  MANOS A LA OBRA
     -- v_tpo_credito = 21  UN CUARTO MÁS
     IF (v_tpo_credito = 19 OR v_tpo_credito = 20 OR v_tpo_credito = 21) THEN
        EXECUTE PROCEDURE fn_dis_transaccion9(v_edo_credito,
                                              v_derechohabiente_pag,
                                              v_num_credito,
                                              v_periodo_bimestre,
                                              v_f_pago,
                                              v_imp_ap_pat,
                                              v_monto_pesos,
                                              v_precio_f_pag,
                                              v_precio_fec_hoy,
                                              p_fecha_01mm,
                                              p_proceso_cod_reg_pago,
                                              p_folio_disp,
                                              v_tpo_patron,
                                              v_localiza_trabajador,
                                              v_monto_pesos_real,
                                              v_id_referencia,
                                              v_nrp,
                                              v_folio_sua,
                                              p_folio_reg_pag,
                                              v_tipo_trabajador,
                                              v_aiv_ap_pat,--Se agrega el valor de las AIVS
                                              v_cve_ent_receptora,
                                              v_nss,
                                              v_periodo_pago,
                                              v_destino_ap_viv,
					      v_tpo_credito) 
                     INTO v_bnd_transaccion, v_status, error_info;
        CONTINUE FOREACH;
     END IF

     -- v_tpo_credito = 23 MEJORAVIT +
     IF (v_tpo_credito = 23) THEN
        --Derechohabiente no tiene AVANCE ABIERTO
        EXECUTE PROCEDURE fn_dis_transaccion1(v_edo_credito,
                                              v_derechohabiente_pag,
                                              v_num_credito,
                                              v_periodo_bimestre,
                                              v_f_pago,
                                              v_imp_ap_pat,
                                              v_monto_pesos,
                                              v_precio_f_pag,
                                              v_precio_fec_hoy,
                                              p_fecha_01mm,
                                              p_proceso_cod_reg_pago,
                                              p_folio_disp,
                                              v_tpo_patron,
                                              v_localiza_trabajador,
                                              v_monto_pesos_real,
                                              v_id_referencia,
                                              v_nrp,
                                              v_folio_sua,
                                              p_folio_reg_pag,
                                              v_tipo_trabajador,
                                              v_aiv_ap_pat,--Se agrega el valor de las AIVS
                                              v_cve_ent_receptora,
                                              v_nss,
                                              v_periodo_pago,
                                              v_destino_ap_viv,
                                              v_tpo_credito) 
                     INTO v_bnd_transaccion, v_status, error_info;
        CONTINUE FOREACH;
     END IF
     
     --Identifica aportaciones voluntarias EFIRISS
     LET v_bnd_existe_nrp = 0;
         
     SELECT COUNT(*)
     INTO   v_bnd_existe_nrp
     FROM   cat_riss_nrp riss
     WHERE  riss.nrp     = v_nrp
     AND    riss.id_nrp IN (0,2);
     IF v_bnd_existe_nrp >= 1 THEN
        -- DISPERSION SUBCUENTA 55
        EXECUTE PROCEDURE sp_dis_Transaccion16(v_derechohabiente_pag,
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
                                               v_aiv_ap_pat) --Se agrega valor de las AIVS
                     INTO v_bnd_transaccion, v_status,error_info;
        CONTINUE FOREACH;
     END IF
    
     --Derechohabiente no tiene AVANCE ABIERTO
     EXECUTE PROCEDURE fn_dis_transaccion1(v_edo_credito,
                                           v_derechohabiente_pag,
                                           v_num_credito,
                                           v_periodo_bimestre,
                                           v_f_pago,
                                           v_imp_ap_pat,
                                           v_monto_pesos,
                                           v_precio_f_pag,
                                           v_precio_fec_hoy,
                                           p_fecha_01mm,
                                           p_proceso_cod_reg_pago,
                                           p_folio_disp,
                                           v_tpo_patron,
                                           v_localiza_trabajador,
                                           v_monto_pesos_real,
                                           v_id_referencia,
                                           v_nrp,
                                           v_folio_sua,
                                           p_folio_reg_pag,
                                           v_tipo_trabajador,
                                           v_aiv_ap_pat,--Se agrega el valor de las AIVS
                                           v_cve_ent_receptora,
                                           v_nss,
                                           v_periodo_pago,
                                           v_destino_ap_viv,
                                           v_tpo_credito) 
                  INTO v_bnd_transaccion, v_status, error_info;

  END FOREACH; --cta_his_pagos

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  LET error_info = "  Preliquidación finalizo correctamente (Masivo Infonavit)";
  RETURN v_bnd_transaccion , v_status , error_info, '';

END PROCEDURE;


