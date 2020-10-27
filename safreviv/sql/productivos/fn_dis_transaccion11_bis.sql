






CREATE PROCEDURE "safreviv".fn_dis_transaccion11_bis(p_fecha_01mm       DATE,          --Fecha de movimiento, proceso
                                          p_usuario              CHAR(20),      --Usuario del proceso
                                          p_folio_disp           DECIMAL(9,0),  --Folio de dispersión
                                          --p_pid                  DECIMAL(9,0),  --Pid genrado para el proceso
                                          p_proceso_cod          SMALLINT,      --Proceso dispersión preliquidación
                                          p_opera_cod            SMALLINT,      --Operacion preliquidacion
                                          p_folio_reg_pag        DECIMAL(9,0),  --Folio de registro de pagos
                                          p_proceso_cod_reg_pago SMALLINT,      --Codigo proceso de registro de pagos
                                          p_folio_carga_arh      DECIMAL(9,0))  --Folio de carga de archivo/ folio de consulta
RETURNING SMALLINT, SMALLINT, CHAR(70), DECIMAL(9,0)

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
DEFINE v_char                CHAR(70);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_transaccion7_ap     SMALLINT;
DEFINE v_transaccion7_am     SMALLINT;
DEFINE v_aiv_ap_pat          DECIMAL(18,6);
DEFINE v_f_liquida_cred      DATE;
DEFINE v_tpo_credito         SMALLINT;
DEFINE v_valida              SMALLINT;
DEFINE v_cve_ent_receptora   CHAR(3);

DEFINE r_bnd_proceso_cnt     SMALLINT;
DEFINE v_nombre_tabla        CHAR(25);
DEFINE v_folio_liquida_orig  DECIMAL(9,0);
DEFINE v_pid                 DECIMAL(9,0);

DEFINE v_num_credito_arch    DECIMAL(10,0);
DEFINE v_id_cred_cero        DECIMAL(9,0);
DEFINE v_folio_cred_cero     DECIMAL(9,0);
DEFINE v_tpo_originacion     SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag ;
END EXCEPTION

--SET DEBUG FILE TO '/ds/safreviv_int/dis/fn_dis_transaccion11_bis.TRACE';
--TRACE ON;

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
LET v_nombre_tabla         = "";
LET v_pid                  = 0;

LET v_num_credito_arch     = 0;
LET v_id_cred_cero         = 0;
LET v_folio_cred_cero      = 0;
LET v_tpo_originacion      = 0; 
   
  DROP TABLE IF EXISTS dis_preliquida;
  DROP TABLE IF EXISTS dis_pre_his_trans;
  DROP TABLE IF EXISTS dis_pre_interface_hs;
   
  --Ejecuta stored para generar las instrucciones de mandatos
  CREATE TABLE dis_preliquida (f_liquida          DATE NOT NULL,
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

  --p_proceso_cod, p_opera_cod, p_folio_ref, p_usuario_cod
  --EXECUTE FUNCTION fn_genera_folio(p_proceso_cod, p_opera_cod, 0, "OPSISSACI") INTO p_folio_disp; 
   
  --EXECUTE FUNCTION fn_genera_pid("PROCESO????", 1, "OPSISSACI") INTO v_pid
   
  SELECT dco.num_credito, dco.id_dis_arh_num_cred, dco.folio, 
         dli.folio_pago, dli.id_derechohabiente, dli.id_referencia, 
         dli.folio_liquida_orig,
         dco.aportacion, dco.amortizacion, dli.aivs
  FROM   dis_arh_num_cred_0 dco, dis_liq_inconsistente dli
  WHERE  dco.id_dis_arh_num_cred = dli.id_dis_arh_num_cred
  AND    dco.folio               = dli.folio_arh_num_cred
  AND    dco.folio               = p_folio_carga_arh
  AND    dli.edo_liquida         = 0
  INTO TEMP tmp_dis_cred_cero;

  --Identificar precio de Acción del día
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
           tcc.aportacion,
           tcc.amortizacion,
           tcc.aivs,
           --pag.imp_ap_pat,
           --pag.imp_am_cre,
           --pag.aiv_ap_pat,
           pag.num_crd_ifv,
           pag.destino_ap_viv,
           fon.precio_fondo,
           pag.cve_ent_receptora,
           pag.folio,
           tcc.num_credito,
           tcc.id_dis_arh_num_cred,
           tcc.folio,
           tcc.folio_liquida_orig
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
           v_precio_f_pag,
           v_cve_ent_receptora,
           p_folio_reg_pag,
           v_num_credito_arch,
           v_id_cred_cero,
           v_folio_cred_cero,
           v_folio_liquida_orig
    FROM   cta_his_pagos pag,
           glo_valor_fondo fon,
           tmp_dis_cred_cero tcc
    --WHERE pag.folio = tsc.folio_referencia --pag.folio                = p_folio_reg_pag
    WHERE pag.folio = tcc.folio_pago
    AND   pag.id_derechohabiente   = tcc.id_derechohabiente
    AND   pag.id_referencia        = tcc.id_referencia
    AND   pag.ind_liquidacion NOT IN (1,6)
    AND   fon.fondo                = 11
    AND   pag.f_pago               = fon.f_valuacion
    --AND   tcc.edo_liquida          = 0
    AND   tcc.folio                = p_folio_carga_arh

    {SELECT glo.proceso_cod
    INTO   p_proceso_cod_reg_pago
    FROM   glo_folio glo
    WHERE  glo.folio = p_folio_reg_pag;}
      
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

    LET v_num_credito     = 0;
    LET v_num_credito_crd = 0;
    LET v_tipo_trabajador = 0;
    LET v_f_otorga        = '';
	 
    --Se realiza la conversión de los pesos por el precio de acción del día
    --LET v_monto_pesos     = (v_aiv_ap_pat * v_precio_fec_hoy);
    LET v_monto_pesos     = v_imp_ap_pat;

    --- Eliminar la asignación cuando se active el proceso de mandatos
    LET v_monto_pesos_real = v_imp_am_cre;

    --########## Se agrega nueva función para optimización ###########
    --Identifica el crédito más reciente del derechohabiente que haya sido liquidado
    --Valor resultado = -2: No hay NSS asociado
    --Valor resultado = -1: No hay identificador de derechohabiente
    --Valor resultado = 0: 	Existe crédito vigente
    --Valor resultado = 1: 	No tiene crédito
    --Valor resultado = 2: 	Tiene crédito Liquidado
    LET v_edo_credito    = 0;
    LET v_f_liquida_cred = '';
    LET v_tpo_credito    = 0;
    LET v_valida         = 1;
    LET v_nss            = "";
	
    EXECUTE FUNCTION fn_credito_vivienda(v_derechohabiente_pag, v_valida)
                INTO v_edo_credito,
                     v_tipo_trabajador,
                     v_tpo_credito,
                     v_num_credito_crd,
                     v_f_otorga,
                     v_f_liquida_cred;
 
    IF v_edo_credito = -2 OR v_edo_credito = -1 OR v_edo_credito = 1 THEN 
       LET v_num_credito_crd = 0;
       LET v_tipo_trabajador = 0;
       LET v_f_otorga        = '';
       LET v_tpo_credito     = 0;
    END IF 

    LET v_tipo_trabajador = 1;
    LET v_edo_credito     = 0;
    LET v_num_credito     = v_num_credito_arch;
      
    --- DEJAR COMO PRINCIPAL EL NUMERO DE CREDITO
    IF v_num_credito_crd IS NULL OR
       v_num_credito_crd = 0 THEN

       IF v_num_credito_pag IS NULL OR
          v_num_credito_pag = 0 THEN
          LET v_num_credito = 0;
          LET v_num_credito = v_num_credito_arch;

          IF v_destino_ap_viv <> 2 THEN
             INSERT INTO dis_info_inconsistente VALUES (v_derechohabiente_pag,
                                                        v_id_referencia,
                                                        p_folio_disp,
                                                        v_destino_ap_viv,
                                                        v_tipo_trabajador,
                                                        v_num_credito,
                                                        v_monto_pesos,
                                                        v_monto_pesos_real,
                                                        v_aiv_ap_pat,
                                                        0); 
             CONTINUE FOREACH;
          END IF
       END IF
       --LET v_num_credito = v_num_credito_pag;
       LET v_num_credito     = v_num_credito_arch;
       LET v_tpo_originacion = 1;
    ELSE
       --LET v_num_credito = v_num_credito_crd;
       LET v_num_credito     = v_num_credito_arch;
       LET v_tpo_originacion = 1;
    END IF

    EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
                 INTO v_periodo_bimestre;
				  
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

    --Identificar destino de la aportacion
    --1 INFONAVIT
    IF v_destino_ap_viv = 1 THEN -- Dispersión a INFONAVIT
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
                                             v_destino_ap_viv)-- Se agrega destino
                    INTO v_bnd_transaccion, v_status, error_info;
    END IF -- Dispersión a INFONAVIT

    UPDATE dis_liq_inconsistente 
    SET    folio_liquida      = p_folio_disp,
           num_credito        = v_num_credito_crd,
           aportacion         = v_monto_pesos,
           amortizacion       = v_monto_pesos_real,
           aivs               = v_aiv_ap_pat,
           edo_liquida        = 2
    WHERE id_dis_arh_num_cred = v_id_cred_cero
    AND   folio_arh_num_cred  = v_folio_cred_cero;
	  
    UPDATE dis_info_inconsistente
    SET    tpo_inconsistente  = 10
    WHERE  id_derechohabiente = v_derechohabiente_pag
    AND    id_referencia      = v_id_referencia
    AND    folio_liquida      = v_folio_liquida_orig;

    UPDATE dis_arh_num_cred_0
    SET    estado             = 2
    WHERE id_dis_arh_num_cred = v_id_cred_cero
    AND   folio               = v_folio_cred_cero
    AND   estado              = 0;
	
  END FOREACH --cta_his_pagos

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

  --SET INDEXES FOR dis_info_inconsistente ENABLED;
  SET INDEXES FOR dis_interface_ef_ad ENABLED;
  --SET INDEXES FOR dis_interface_hs ENABLED;
  SET INDEXES FOR dis_compensa_avance ENABLED;
  --SET INDEXES FOR dis_liq_inconsistente ENABLED;
  SET INDEXES FOR dis_crd_tramite ENABLED;
  SET INDEXES FOR dis_crd_ceros ENABLED;
   
  UPDATE STATISTICS FOR TABLE dis_preliquida;
  UPDATE STATISTICS FOR TABLE dis_info_inconsistente;
  UPDATE STATISTICS FOR TABLE dis_interface_ef_ad;
  UPDATE STATISTICS FOR TABLE dis_interface_hs;
  UPDATE STATISTICS FOR TABLE dis_compensa_avance;
  UPDATE STATISTICS FOR TABLE dis_crd_tramite;
  UPDATE STATISTICS FOR TABLE dis_his_hs;
  UPDATE STATISTICS FOR TABLE dis_crd_ceros;       --Req SACI2018-175
  UPDATE STATISTICS FOR TABLE dis_his_transaccion;
  UPDATE STATISTICS FOR TABLE dis_liq_inconsistente;
  UPDATE STATISTICS FOR TABLE dis_arh_num_cred_0;
   
  INSERT INTO cta_movimiento
  SELECT *
  FROM dis_preliquida
  WHERE folio_liquida = p_folio_disp;

  UPDATE glo_folio
  SET    status = 2
  WHERE  folio  = p_folio_disp;

  EXECUTE PROCEDURE fn_dis_cnt19(p_folio_disp,
                                 TODAY,
                                 19,
                                 p_proceso_cod,
                                 0)
               INTO r_bnd_proceso_cnt;

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  LET error_info = "  Preliquidación Créditos Ceros finalizó correctamente";
  RETURN v_bnd_transaccion , v_status , error_info, '';

END PROCEDURE;


