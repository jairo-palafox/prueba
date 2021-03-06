






CREATE PROCEDURE "safreviv".fn_dis_transaccion17(p_fecha_01mm           DATE,          --Fecha de movimiento, proceso
                                      p_usuario              CHAR(20),      --Usuario del proceso
                                      p_folio_disp           DECIMAL(9,0),  --Folio de dispersi�n
                                      p_pid                  DECIMAL(9,0),  --Pid genrado para el proceso
                                      p_proceso_cod          SMALLINT,      --Proceso dispersi�n preliquidaci�n
                                      p_opera_cod            SMALLINT,      --Operacion preliquidacion
                                      p_folio_reg_pag        DECIMAL(9,0),  --Folio de registro de pagos
                                      p_proceso_cod_reg_pago SMALLINT)      --Codigo proceso de registro de pagos
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--�ltima modificaci�n 25042019
--Declaraci�n de variables
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
DEFINE v_localiza_trabajador CHAR(01);       --Localizaci�n del trabajador
DEFINE v_tpo_patron          CHAR(02);       --Tipo de patron
DEFINE v_periodo_bimestre    CHAR(6);        --Per�odo de Pago Bimestre
DEFINE v_derechohabiente_pag DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito_pag     DECIMAL(10,0);
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_compensacion_apo    SMALLINT;       --Bandera de compensaci�n aportaci�n
DEFINE v_compensacion_amo    SMALLINT;       --Bandera de compensaci�n amortizaci�n
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
DEFINE v_marca_confirmada    SMALLINT;
DEFINE v_id_derhab_nuevo     DECIMAL(9,0);   --Derechohabiente Acl Nuevo
DEFINE v_bnd_existe_nrp      SMALLINT;
DEFINE v_tot_marca_tag       SMALLINT;
DEFINE v_tpo_originacion     SMALLINT;
DEFINE v_tpo_dscto           SMALLINT;
DEFINE v_edo_num_credito     SMALLINT;       --Req SACI2018-175

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN v_status, isam_err, error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicializaci�n de variables
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
LET v_marca_confirmada     = 0;
LET v_id_derhab_nuevo      = 0;
LET v_bnd_existe_nrp       = 0;
LET v_tot_marca_tag        = 0;
LET v_tpo_originacion      = 0;
LET v_tpo_dscto            = 0;
LET v_edo_num_credito      = 0;   --Req SACI2018-175

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_transaccion17.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_transaccion17.TRACE';
--TRACE ON;

  --Ejecuta stored para generar las instrucciones de mandatos
  CREATE TABLE dis_preliquida(f_liquida          DATE NOT NULL,
                              id_derechohabiente DECIMAL(9,0) NOT NULL ,
                              subcuenta          SMALLINT NOT NULL ,
                              fondo_inversion    SMALLINT NOT NULL ,
                              movimiento         SMALLINT NOT NULL ,
                              folio_liquida      DECIMAL(9,0) NOT NULL ,
                              id_referencia      DECIMAL(9,0) NOT NULL ,
                              monto_acciones     DECIMAL(22,2),
                              monto_pesos        DECIMAL(22,2),
                              f_valor            DATE,
                              f_registro         DATE,
                              h_registro         DATETIME HOUR TO SECOND,
                              origen             CHAR(20))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  CREATE TABLE dis_pre_interface_hs (id_dis_interface_hs DECIMAL(9,0),
                                     id_derechohabiente  DECIMAL(9,0),
                                     folio_sua           DECIMAL(6,0),
                                     periodo_pago        CHAR(6),
                                     f_pago              DATE,
                                     nrp                 CHAR(11),
                                     folio_liquida       DECIMAL(9,0) NOT NULL,
                                     num_crd_ifv         DECIMAL(10,0),
                                     imp_ap_pat          DECIMAL(12,2),
                                     imp_am_cre          DECIMAL(12,2),
                                     aiv_ap_pat          DECIMAL(18,6),
                                     tipo_hs             SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  CREATE TABLE dis_pre_his_trans (id_derechohabiente DECIMAL(9,0) NOT NULL,
                                  folio_liquida      DECIMAL(9,0) NOT NULL,
                                  num_credito        DECIMAL(10,0),
                                  tpo_originacion    SMALLINT,
                                  tpo_credito        SMALLINT,
                                  f_otorga           DATE,
                                  f_liquida_cred     DATE,
                                  edo_credito        SMALLINT,
                                  origen_num_credito SMALLINT,
                                  destino_ap_viv     CHAR(1),
                                  folio_reg_pagos    DECIMAL(9,0),
                                  id_ref_reg_pagos   DECIMAL(9,0),
                                  folio_sua          DECIMAL(6,0),
                                  periodo_pago       CHAR(6),
                                  f_pago             DATE,
                                  nrp                CHAR(11),
                                  imp_ap_pat         DECIMAL(12,2),
                                  imp_am_cre         DECIMAL(12,2),
                                  aiv_ap_pat         DECIMAL(18,6))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SET PDQPRIORITY HIGH;

  --Identificar precio de Acci�n del d�a
  SELECT precio_fondo
  INTO   v_precio_fec_hoy
  FROM   glo_valor_fondo
  WHERE  fondo       = 11
  AND    f_valuacion = TODAY;

  --Leer todos los registros de la tabla cta_his_pagos
  --Identifica precio de fondo de Fecha de Pago patronal
  FOREACH
    SELECT pag.id_derechohabiente,
           pag.folio_sua,
           pag.periodo_pago,
           pag.f_pago,
           pag.nrp,
           pag.id_referencia,
           pag.localiza_trabajador,
           pag.tpo_patron,
           pag.imp_ap_pat,
           pag.imp_am_cre,
           pag.aiv_ap_pat,
           pag.num_crd_ifv,
           pag.destino_ap_viv,
           com.id_derhab_nuevo,
           fon.precio_fondo,
           pag.cve_ent_receptora,
           pag.folio
    INTO   v_derechohabiente_pag,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp,
           v_id_referencia,
           v_localiza_trabajador,
           v_tpo_patron,
           v_imp_ap_pat,
           v_imp_am_cre,
           v_aiv_ap_pat,
           v_num_credito_pag,
           v_destino_ap_viv,
           v_id_derhab_nuevo,
           v_precio_f_pag,
           v_cve_ent_receptora,
           p_folio_reg_pag
    FROM   cta_his_pagos pag,
    OUTER  cta_pag_complemento com,
           glo_valor_fondo fon
    --WHERE  pag.folio           IN (52611,52644,52918,52990,53010,53030,54032,56755,56769)
    WHERE  pag.folio            = p_folio_reg_pag
    AND    pag.folio            = com.folio
    AND    pag.id_referencia    = com.id_referencia
    AND    pag.ind_liquidacion  = 5
    AND    pag.tpo_aclaracion  <> ' '
    AND    fon.fondo            = 11
    AND    pag.f_pago           = fon.f_valuacion
    AND    pag.destino_ap_viv   = 2
    AND    pag.imp_am_cre      <> 0

    IF v_aiv_ap_pat IS NULL THEN
       LET v_aiv_ap_pat = 0;
    END IF

    LET v_imp_ap_pat = 0;
    LET v_aiv_ap_pat = 0;

    IF v_imp_am_cre IS NULL THEN
       LET v_imp_am_cre = 0;
    END IF

    --Si las aportaciones y amortizaciones son menores o
    --iguales a cero no se dispersa el registro
    IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
       CONTINUE FOREACH;
    END IF

    EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
                 INTO v_periodo_bimestre;

    --PROCESO 103-REG PAGOS ACLARATORIO CAMBIO NSS
    --ASIGNA NUEVO ID DERECHOHABIENTE
    IF (p_proceso_cod_reg_pago = 103)      AND
       (v_id_derhab_nuevo     IS NOT NULL  OR
        v_id_derhab_nuevo     <> 0)        THEN
        LET v_derechohabiente_pag = v_id_derhab_nuevo;
    END IF 

    LET v_num_credito     = 0;
    LET v_num_credito_crd = 0;
    LET v_tipo_trabajador = 0;
    LET v_f_otorga        = '';
    LET v_tpo_dscto       = 0;
    LET v_tpo_originacion = 0;
    LET v_edo_num_credito = 0;  --Req SACI2018-175    
	 
    --Se realiza la conversi�n de los pesos por el precio de acci�n del d�a
    LET v_monto_pesos     = (v_aiv_ap_pat * v_precio_fec_hoy);

    --- Eliminar la asignaci�n cuando se active el proceso de mandatos
    LET v_monto_pesos_real = v_imp_am_cre;

    --########## Se agrega nueva funci�n para optimizaci�n ###########
    --Identifica el cr�dito m�s reciente del derechohabiente que haya sido liquidado
    --Valor resultado = -2: No hay NSS asociado
    --Valor resultado = -1: No hay identificador de derechohabiente
    --Valor resultado = 0: 	Existe cr�dito vigente
    --Valor resultado = 1: 	No tiene cr�dito
    --Valor resultado = 2: 	Tiene cr�dito Liquidado
    LET v_edo_credito      = 0;
    LET v_f_liquida_cred   = '';
    LET v_tpo_credito      = 0;
    LET v_valida           = 1;
    LET v_nss              = "";
    LET v_marca_confirmada = "";
	
    EXECUTE FUNCTION fn_credito_viv_conf(v_derechohabiente_pag, v_valida)
                INTO v_edo_credito,
                     v_tipo_trabajador,
                     v_tpo_credito,
                     v_num_credito_crd,
                     v_f_otorga,
                     v_f_liquida_cred,
                     v_marca_confirmada;
 
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

          IF v_destino_ap_viv <> 2 THEN
             --Req SACI2018-175
             LET v_edo_num_credito = 1;
             --Req SACI2018-175          
          END IF
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

    --Req SACI2018-175
    IF v_edo_num_credito = 1 THEN
       --Funci�n Registro N�mero de Cr�dito en Ceros
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

    --Se agrega validaci�n para verificar si es el nuevo tipo de trabajador
    IF v_tipo_trabajador = 3 THEN  --se crea registro para liquidar solo amortizaci�n
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
    -- v_tpo_credito = 21  UN CUARTO M�S
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
                                             v_aiv_ap_pat, --Se agrega valor de las AIVS
                                             v_cve_ent_receptora,
                                             v_nss,
                                             v_periodo_pago,
                                             v_destino_ap_viv,-- Se agrega destino
                                             v_tpo_credito)
                    INTO v_bnd_transaccion, v_status, error_info;
       CONTINUE FOREACH;
    END IF

    -- v_tpo_credito = 23  MEJORAVIT +
    IF (v_tpo_credito = 23) THEN
       --Si derechohabiente no tiene AVANCE ABIERTO
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
                                             v_aiv_ap_pat, --Se agrega valor de las AIVS
                                             v_cve_ent_receptora,
                                             v_nss,
                                             v_periodo_pago,
                                             v_destino_ap_viv, -- Se agrega destino
                                             v_tpo_credito)    -- Se agrega tpo credito
                    INTO v_bnd_transaccion, v_status, error_info;
       CONTINUE FOREACH;
    END IF
          
    LET v_bnd_existe_nrp = 0;
  
    --Identifica aportaciones voluntarias EFIRISS       
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
		  
    LET v_transaccion7_ap = 0;
    LET v_transaccion7_am = 0;

    SELECT id_dis_det_avance_pago,
           monto_dif_apo,
           monto_dif_amo
    INTO   v_det_avance_pago,
           v_monto_dif_apo,
           v_monto_dif_amo
    FROM   dis_det_avance_pago
    WHERE  id_derechohabiente = v_derechohabiente_pag
    AND    periodo_pago       = v_periodo_bimestre
    AND    nrp                = v_nrp
    AND    tpo_avance         = 181
    AND    estado             = 30;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       --Si derechohabiente no tiene AVANCE ABIERTO
       --Agregar validaci�n de aclaratorio y periodo entre 200605-201204 para no dispersar y hacar cargo y abono "Dispersi�n Entregada por TRM"
       --Fecha Pago
       IF v_f_pago <= '10312012' THEN 
          EXECUTE PROCEDURE sp_dis_Transaccion11(v_derechohabiente_pag, 
                                                 v_imp_ap_pat,
                                                 v_monto_pesos,
                                                 v_monto_pesos_real,
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
                                                 v_aiv_ap_pat) 
                       INTO v_bnd_transaccion, v_status,error_info;
          CONTINUE FOREACH;
       END IF

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
                                             v_aiv_ap_pat, --Se agrega valor de las AIVS
                                             v_cve_ent_receptora,
                                             v_nss,
                                             v_periodo_pago,
                                             v_destino_ap_viv, -- Se agrega destino
                                             v_tpo_credito)    -- Se agrega tpo credito
                    INTO v_bnd_transaccion, v_status, error_info;
    ELSE --Si derechohabiente tiene AVANCE ABIERTO
       IF v_monto_dif_apo IS NULL THEN
          LET v_monto_dif_apo = 0;
       END IF

       IF v_monto_dif_amo IS NULL THEN
          LET v_monto_dif_amo = 0;
       END IF

       IF v_periodo_bimestre <= '200505' THEN --PAGO VIRTUAL--
          --PAGO VIRTUAL CON APORTACION
          IF v_monto_pesos > 0 THEN
             IF v_edo_credito <> 2 THEN
                --No se encuentra liquidado
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
                                                      v_aiv_ap_pat, --Se agrega valor de las AIVS
                                                      v_tpo_credito)
                             INTO v_bnd_transaccion, v_status,error_info;
             END IF
          END IF

          IF v_monto_pesos_real > 0 THEN   -- Amortizaci�n
             LET v_compensacion_apo = NULL;

             --Realizar compensaci�n para identificar diferencias
             LET v_comp_am_avpag    = v_monto_dif_amo - v_monto_pesos_real;

             ----INICIO AMORTIZACIONES CON PAGO VIRTUAL-----
             IF v_monto_pesos_real < v_monto_dif_amo THEN
                LET v_compensacion_amo = 4; --Pago Menor al Avance DN
             END IF

             IF v_monto_pesos_real > v_monto_dif_amo THEN
                LET v_compensacion_amo = 5; --Pago Mayor al Avance DN
             END IF

             IF v_monto_pesos_real = v_monto_dif_amo THEN
                LET v_compensacion_amo = 3; --Pago Sin Diferencia
             END IF
             ----FIN AMORTIZACIONES CON PAGO VIRTUAL-----

             --Almacena cuantas que tienen avance abierto
             EXECUTE PROCEDURE sp_dis_transaccion7(p_folio_disp,
                                                   p_folio_reg_pag,
                                                   v_id_referencia,
                                                   v_derechohabiente_pag,
                                                   v_num_credito,
                                                   v_periodo_bimestre,
                                                   v_f_pago,
                                                   v_nrp,
                                                   v_det_avance_pago,
                                                   0,
                                                   v_monto_dif_amo,
                                                   0,
                                                   0,
                                                   v_monto_pesos_real,
                                                   0,
                                                   v_comp_am_avpag,
                                                   v_compensacion_apo,
                                                   v_compensacion_amo,
                                                   p_proceso_cod_reg_pago,
                                                   v_precio_fec_hoy,
                                                   v_tpo_patron,
                                                   v_localiza_trabajador,
                                                   0) --Se agrega valor de las AIVS
                          INTO v_bnd_transaccion, v_status,error_info;
          END IF -- Amortizaci�n
       ELSE  ---  Periodo de pago > 200505
          -- AVANCE DE PAGO
          --Si el DH cuenta con Avance Abierto
          LET v_compensacion_apo = NULL;
          LET v_compensacion_amo = NULL;

          --Realizar compensaci�n para identificar diferencias
          LET v_comp_ap_avpag = v_monto_dif_apo - v_monto_pesos;
          LET v_comp_am_avpag = v_monto_dif_amo - v_monto_pesos_real;

          ----INICIO APORTACIONES CON AVANCE DE PAGOS-----
          IF v_monto_pesos < v_monto_dif_apo THEN
             LET v_compensacion_apo = 1; --Pago Menor al Avance DP
          END IF

          IF v_monto_pesos > v_monto_dif_apo THEN
             LET v_compensacion_apo = 2; --Pago Mayor al Avance DN
          END IF

          --Sin diferencia en el pago - Transacci�n 8
          IF v_monto_pesos = v_monto_dif_apo THEN
             LET v_compensacion_apo = 0;
          END IF
          ----FIN APORTACIONES CON AVANCE DE PAGOS-----

          ----INICIO AMORTIZACIONES CON AVANCE DE PAGOS-----
          IF v_monto_pesos_real < v_monto_dif_amo THEN
             LET v_compensacion_amo = 1; --Pago Menor al Avance DP
          END IF

          IF v_monto_pesos_real > v_monto_dif_amo THEN
             LET v_compensacion_amo = 2; --Pago Mayor al Avance DN
          END IF

          IF v_monto_pesos_real = v_monto_dif_amo THEN
             LET v_compensacion_amo = 0;
          END IF
          ----FIN AMORTIZACIONES CON AVANCE DE PAGOS-----

          --Almacena cuantas que tienen avance abierto
          EXECUTE PROCEDURE sp_dis_transaccion7(p_folio_disp,
                                                p_folio_reg_pag,
                                                v_id_referencia,
                                                v_derechohabiente_pag,
                                                v_num_credito,
                                                v_periodo_bimestre,
                                                v_f_pago,
                                                v_nrp,
                                                v_det_avance_pago,
                                                v_monto_dif_apo,
                                                v_monto_dif_amo,
                                                v_imp_ap_pat,
                                                v_monto_pesos,
                                                v_monto_pesos_real,
                                                v_comp_ap_avpag,
                                                v_comp_am_avpag,
                                                v_compensacion_apo,
                                                v_compensacion_amo,
                                                p_proceso_cod_reg_pago,
                                                v_precio_fec_hoy,
                                                v_tpo_patron,
                                                v_localiza_trabajador,
                                                v_aiv_ap_pat) --Se agrega valor de las AIVS
                       INTO v_bnd_transaccion, v_status,error_info;
       END IF  -- Periodo de pago < 200505
    END IF -- Con avance de pago

    --## Se valida si el proceso es de aclaratorio y si no tiene destino para enviar la informaci�n a dis_info_inconsistentes 
    --Identificar si es un proceso de ACLARATORIO
    IF (v_destino_ap_viv IS NULL      OR 
        v_destino_ap_viv = 0)         THEN
        INSERT INTO dis_info_inconsistente VALUES(v_derechohabiente_pag,
                                                  v_id_referencia,
                                                  p_folio_disp,
                                                  v_destino_ap_viv,
                                                  v_tipo_trabajador,
                                                  v_num_credito,
                                                  v_monto_pesos,
                                                  v_monto_pesos_real,
                                                  v_aiv_ap_pat,
                                                  2); --Aclaratorio sin destino
        CONTINUE FOREACH;
    END IF --Del proceso de ACLARATORIO
  END FOREACH; --cta_his_pagos

  CREATE INDEX xdis_pre_interface_hs2 ON dis_pre_interface_hs 
  (folio_liquida) IN dis_ix_dbs;
  UPDATE STATISTICS FOR TABLE dis_pre_interface_hs;

  CREATE INDEX xdis_pre_his_trans2 ON dis_pre_his_trans 
  (folio_liquida) IN dis_ix_dbs;
  UPDATE STATISTICS FOR TABLE dis_pre_his_trans;

  CREATE INDEX xdis_preliquida2 ON dis_preliquida
  (folio_liquida) IN dis_ix_dbs;
  UPDATE STATISTICS FOR TABLE dis_preliquida;

  INSERT INTO dis_interface_hs
  SELECT *
  FROM   dis_pre_interface_hs
  WHERE  folio_liquida = p_folio_disp;

  INSERT INTO dis_his_transaccion
  SELECT *
  FROM   dis_pre_his_trans
  WHERE  folio_liquida = p_folio_disp;

  SET INDEXES FOR dis_info_inconsistente ENABLED;
  SET INDEXES FOR dis_interface_ef_ad ENABLED;
  SET INDEXES FOR dis_compensa_avance ENABLED;
  SET INDEXES FOR dis_crd_tramite ENABLED;
  SET INDEXES FOR dis_crd_ceros ENABLED; --Req SACI2018-175

  UPDATE STATISTICS FOR TABLE dis_preliquida;
  UPDATE STATISTICS FOR TABLE dis_info_inconsistente;
  UPDATE STATISTICS FOR TABLE dis_interface_ef_ad;
  UPDATE STATISTICS FOR TABLE dis_interface_hs;
  UPDATE STATISTICS FOR TABLE dis_compensa_avance;
  UPDATE STATISTICS FOR TABLE dis_crd_tramite;
  UPDATE STATISTICS FOR TABLE dis_his_hs;
  UPDATE STATISTICS FOR TABLE dis_crd_ceros;       --Req SACI2018-175
  UPDATE STATISTICS FOR TABLE dis_his_transaccion;

  --TRACE 'Finaliza fn_dis_transaccion17 con valor '||v_bnd_proceso;
  LET v_char = "  Preliquidaci�n esp acl finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END PROCEDURE;


