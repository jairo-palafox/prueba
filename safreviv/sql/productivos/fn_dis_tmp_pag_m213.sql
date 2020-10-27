






CREATE PROCEDURE "safreviv".fn_dis_tmp_pag_m213(p_ini_marca DATE)   --Fecha de inicio de marca 
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 21072016
--Declaración de variables
DEFINE v_derechohabiente_pag       DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_nss                       CHAR(11);       --Numero seguro social
DEFINE v_nrp                       CHAR(11);       --Registro patronal
DEFINE v_periodo_pago              CHAR(6);        --Periodo de pago
DEFINE v_periodo_bimestre          CHAR(6);        --Periodo bimestre
DEFINE v_folio_sua                 DECIMAL(6);     --Folio detalle trabajdor
DEFINE v_aiv_ap_pat                DECIMAL(18,6);
DEFINE v_imp_ap_pat                DECIMAL(12,2);  --Importe aportaciones patronales -- Son los pesos no convertidos
DEFINE v_imp_am_cre                DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_f_pago                    DATE;           --Fecha de pago patronal
DEFINE v_destino_ap_viv            CHAR(1);        --Destino Aportacion Vivienda

DEFINE v_id_referencia_pag         DECIMAL(9,0);   --ID de referencia Registro de Pagos
DEFINE v_folio_pago                DECIMAL(9,0);   --Folio Registro de Pagos
DEFINE v_folio_pago_val            DECIMAL(9,0);   --Folio Registro de Pagos Valida

DEFINE v_f_ini_marca               DATE;           --Fecha Inicio de Marca
DEFINE v_f_fin_marca               DATE;           --Fecha Fin de Marca

DEFINE v_derechohabiente_real      DECIMAL(9,0);   --Derechohabiente
DEFINE v_precio_fec_hoy            DECIMAL(19,14); --Precio de fondo del dia
DEFINE v_monto_pesos               DECIMAL(22,2);  --Importe aportaciones patronales --Se cambia la variable ap_pat por v_monto_pesos(pesos convertidos)

DEFINE v_bnd_proceso               SMALLINT;       --Estatus del proceso
DEFINE r_bnd_epera_error           SMALLINT;       --Bandera operacion error

DEFINE v_cadena                    CHAR(300);
DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER;
DEFINE isam_err                    INTEGER;
DEFINE error_info                  CHAR(70);
DEFINE v_char                      CHAR(20);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_nss                  = "";
LET v_nrp                  = "";
LET v_periodo_pago         = "";
LET v_periodo_bimestre     = "";
LET v_folio_sua            = 0;
LET v_aiv_ap_pat           = 0.00;
LET v_imp_ap_pat           = 0.00;
LET v_imp_am_cre           = 0.00;
LET v_f_pago               = TODAY;
LET v_destino_ap_viv       = 0;

LET v_folio_pago           = 0;
LET v_id_referencia_pag    = 0;
LET v_folio_pago_val       = 0;

LET v_f_ini_marca          = "";
LET v_f_fin_marca          = "";

LET v_derechohabiente_real = 0;
LET v_derechohabiente_pag  = 0;
LET v_precio_fec_hoy       = 0.00;
LET v_monto_pesos          = 0.00;

LET v_bnd_proceso          = 0; --Estado correcto
LET r_bnd_epera_error      = 0;

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_tmp_pag_m213.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_tmp_pag_m213.TRACE';
--SET DEBUG FILE TO '/safreviv_req/PRODINFXVI-95/fn_dis_tmp_pag_m213.TRACE';
--TRACE ON;

  DROP TABLE IF EXISTS tmp_dis_pag_m213;
  CREATE TABLE tmp_dis_pag_m213(id_derechohabiente DECIMAL(9,0) NOT NULL,
                                nss                CHAR(11),
                                nrp                CHAR(11),
                                periodo_pago       CHAR(6),
                                folio_sua          DECIMAL(6,0),
                                aiv_av_pat         DECIMAL(18,6),
                                imp_ap_pat         DECIMAL(12,2),
                                imp_am_cre         DECIMAL(12,2),
                                f_pago             DATE,
                                id_referencia_pag  DECIMAL(9,0),
                                folio_pago         DECIMAL(9,0),
                                f_ini_marca        DATE,
                                f_fin_marca        DATE)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs; 

  SET PDQPRIORITY HIGH;

  DROP TABLE IF EXISTS tmp_afi_m213;
  SELECT afi.nss, mrc.*
  FROM   afi_derechohabiente afi, sfr_marca_activa mrc
  WHERE  afi.id_derechohabiente = mrc.id_derechohabiente
  AND    mrc.marca              = 213
--AND    mrc.estado_marca       = 0
  AND    mrc.f_inicio          <= p_ini_marca
  INTO TEMP tmp_afi_m213;

  UPDATE STATISTICS FOR TABLE tmp_afi_m213;

  --Identificar precio de Acción del día
  SELECT precio_fondo
  INTO   v_precio_fec_hoy
  FROM   glo_valor_fondo
  WHERE  fondo       = 11
  AND    f_valuacion = TODAY;

  --Leer los registros con marca 213 (Activos e históricos)
  --Obtener sus pagos y verificar que no hayan sido restituidos
  FOREACH
    SELECT UNIQUE a.nss,
           a.id_derechohabiente,
           a.f_inicio
           --a.f_fin
    INTO   v_nss,
           v_derechohabiente_pag,
           v_f_ini_marca
           --v_f_fin_marca
    FROM   tmp_afi_m213 a

    --Obtiene los pagos de los registros con marca 213
    FOREACH
      SELECT pag.id_derechohabiente,
             pag.folio_sua,
             pag.periodo_pago,
             pag.f_pago,
             pag.nrp,
             pag.folio,
             pag.id_referencia,
             pag.imp_ap_pat,
             pag.imp_am_cre,
             pag.aiv_ap_pat,
             pag.destino_ap_viv
      INTO   v_derechohabiente_real,
             v_folio_sua,
             v_periodo_pago,
             v_f_pago,
             v_nrp,
             v_folio_pago,
             v_id_referencia_pag,
             v_imp_ap_pat,
             v_imp_am_cre,
             v_aiv_ap_pat,
             v_destino_ap_viv
      FROM   cta_his_pagos pag,
             glo_folio glo
      WHERE  pag.id_derechohabiente   = v_derechohabiente_pag
      AND    pag.ind_liquidacion NOT IN (1,6)
      AND    pag.destino_ap_viv       = 1
      AND    pag.folio                = glo.folio
      AND    glo.f_actualiza         >= '02022016'
      IF v_derechohabiente_real IS NULL THEN
         LET v_derechohabiente_real = 0;
         LET v_folio_sua            = 0;
         LET v_periodo_pago         = "";
         LET v_periodo_bimestre     = "";
         LET v_f_pago               = TODAY;
         LET v_nrp                  = "";
         LET v_folio_pago           = 0;
         LET v_id_referencia_pag    = 0;
         LET v_imp_ap_pat           = 0.00;
         LET v_imp_am_cre           = 0.00;
         LET v_aiv_ap_pat           = 0.00;
         LET v_destino_ap_viv       = 0;
         LET v_monto_pesos          = 0.00;
         LET v_folio_pago_val       = 0;
         EXIT FOREACH;
      END IF

      IF v_aiv_ap_pat IS NULL THEN
         LET v_aiv_ap_pat = 0;
      END IF
  
      IF v_imp_am_cre IS NULL THEN
         LET v_imp_am_cre = 0;
      END IF

      --Se realiza la conversión de los pesos por el precio de acción del día
      LET v_monto_pesos   = (v_aiv_ap_pat * v_precio_fec_hoy);

      --Si las aportaciones y amortizaciones son menores o
      --iguales a cero no se dispersa el registro
      IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
         CONTINUE FOREACH;
      END IF

      {SELECT UNIQUE g.folio
      INTO   v_folio_pago_val
      FROM   glo_folio g
      WHERE  g.folio        = v_folio_pago
      AND    g.f_actualiza >= '02022016';
      IF (v_folio_pago_val IS NULL) THEN
         LET v_derechohabiente_real = 0;
         LET v_folio_sua            = 0;
         LET v_periodo_pago         = "";
         LET v_periodo_bimestre     = "";
         LET v_f_pago               = TODAY;
         LET v_nrp                  = "";
         LET v_folio_pago           = 0;
         LET v_id_referencia_pag    = 0;
         LET v_imp_ap_pat           = 0.00;
         LET v_imp_am_cre           = 0.00;
         LET v_aiv_ap_pat           = 0.00;
         LET v_destino_ap_viv       = 0;
         LET v_monto_pesos          = 0;
         LET v_folio_pago_val       = 0;
         CONTINUE FOREACH;
      END IF}
  
      EXECUTE PROCEDURE fn_bimestre_pago(v_periodo_pago)
                   INTO v_periodo_bimestre;

      --Identifica si el pago ya fue dispersado
      LET v_derechohabiente_real = 0;

      SELECT UNIQUE hs.id_derechohabiente
      INTO   v_derechohabiente_real
      FROM   dis_interface_hs hs
      WHERE  hs.id_derechohabiente = v_derechohabiente_pag
      AND    hs.folio_sua          = v_folio_sua
      AND    hs.periodo_pago       = v_periodo_bimestre
      AND    hs.f_pago             = v_f_pago
      AND    hs.nrp                = v_nrp;
      IF (v_derechohabiente_real  IS NOT NULL) THEN
         LET v_derechohabiente_real = 0;
         LET v_folio_sua            = 0;
         LET v_periodo_pago         = "";
         LET v_periodo_bimestre     = "";
         LET v_f_pago               = TODAY;
         LET v_nrp                  = "";
         LET v_folio_pago           = 0;
         LET v_id_referencia_pag    = 0;
         LET v_imp_ap_pat           = 0.00;
         LET v_imp_am_cre           = 0.00;
         LET v_aiv_ap_pat           = 0.00;
         LET v_destino_ap_viv       = 0;
         LET v_monto_pesos          = 0;
         LET v_folio_pago_val       = 0;
         CONTINUE FOREACH;
      END IF
  
      --Identifica si el pago fue restituido
      LET v_derechohabiente_real = 0;
      
      SELECT UNIQUE res.id_derechohabiente
      INTO   v_derechohabiente_real
      FROM   dse_devolucion res
      WHERE  res.id_derechohabiente = v_derechohabiente_pag
      AND    res.f_pago             = v_f_pago
      AND    res.periodo_pago       = v_periodo_bimestre
      AND    res.modulo_cod         = 'dis';
      IF (v_derechohabiente_real   IS NULL) THEN
         --Si derechohabiente no tiene saldos restituidos o devueltos
         INSERT INTO tmp_dis_pag_m213 VALUES (v_derechohabiente_pag,
                                              v_nss,
                                              v_nrp,
                                              v_periodo_bimestre,
                                              v_folio_sua,
                                              v_aiv_ap_pat,
                                              --v_imp_ap_pat,
                                              v_monto_pesos,
                                              v_imp_am_cre,
                                              v_f_pago,
                                              v_id_referencia_pag,
                                              v_folio_pago,
                                              v_f_ini_marca,
                                              v_f_fin_marca);
      END IF

      LET v_derechohabiente_real = 0;
      LET v_folio_sua            = 0;
      LET v_periodo_pago         = "";
      LET v_periodo_bimestre     = "";
      LET v_f_pago               = TODAY;
      LET v_nrp                  = "";
      LET v_folio_pago           = 0;
      LET v_id_referencia_pag    = 0;
      LET v_imp_ap_pat           = 0.00;
      LET v_imp_am_cre           = 0.00;
      LET v_aiv_ap_pat           = 0.00;
      LET v_destino_ap_viv       = 0;
      LET v_monto_pesos          = 0;
      LET v_folio_pago_val       = 0;
    END FOREACH --cta_his_pagos
  END FOREACH; --tmp_afi_m213

  UPDATE STATISTICS FOR TABLE tmp_dis_pag_m213;

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  LET v_char = "  Identificación marca 213 finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END PROCEDURE;


