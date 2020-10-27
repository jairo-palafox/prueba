






create procedure "safreviv".fn_numero_credito_hs(p_folio_liquida      DECIMAL(9,0),
                                      v_f_n_disp           DATE)  --Folio de liquidación del proceso
RETURNING SMALLINT

--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus para inserción
DEFINE v_nss                 CHAR(11);       --Numero de Seguridad Social
DEFINE v_id_dis_interface_hs DECIMAL(9,0);   --Id interface hs
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --Id derechohabiente
DEFINE v_folio_sua           DECIMAL(6,0);   --Folio SUA
DEFINE v_periodo_pago        CHAR(6);        --Periodo pago
DEFINE v_f_pago              DATE;           --Fecha Pago
DEFINE v_nrp                 CHAR(11);       --NRP
DEFINE v_folio_liquida       DECIMAL(9,0);   --Folio liquidacion dis
DEFINE v_num_crd_ifv         DECIMAL(10,0);  --Numero Credito interface hs
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Monto aportacion
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Monto amortizacion

DEFINE v_num_credito_cre     DECIMAL(10,0);  --Numero de crédito - creditos     
DEFINE v_tpo_originacion     SMALLINT;       --Tipo Originacion
DEFINE v_ban_trabajador      SMALLINT;       --Bandera tipo trabajador

DEFINE v_edo_credito         SMALLINT;       --Estado del crédito

DEFINE v_num_credito_pag     DECIMAL(10,0);  --Numero de crédito - pagos
DEFINE v_edo_pag             SMALLINT;       --Estado pago

DEFINE v_num_credito_cor     DECIMAL(10,0);  --Numero de crédito correcto
DEFINE v_dif_num_cred        SMALLINT;       --Diferencia numero de credito

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status;
END EXCEPTION

  --Inicializacón de variables
  LET v_bnd_proceso         = 1; --Estado correcto

  LET v_nss                 = '';
  LET v_id_dis_interface_hs = 0;
  LET v_id_derechohabiente  = 0;
  LET v_folio_sua           = 0;
  LET v_periodo_pago        = '';
  LET v_f_pago              = '';
  LET v_nrp                 = '';
  LET v_folio_liquida       = 0;
  LET v_num_crd_ifv         = 0;
  LET v_imp_ap_pat          = 0;
  LET v_imp_am_cre          = 0;

  LET v_num_credito_cre     = 0;
  LET v_tpo_originacion     = 0;
  LET v_ban_trabajador      = 0;

  LET v_num_credito_pag     = 0;
  LET v_edo_pag             = 0;  

  LET v_edo_credito         = 0;

  LET v_num_credito_cor     = 0;
  LET v_dif_num_cred        = 0;

  DROP TABLE dis_numero_credito_hs;

  CREATE TABLE dis_numero_credito_hs
    (nss                  char(11),
     id_dis_interface_hs  decimal(9,0),
     id_derechohabiente   decimal(9,0),
     folio_sua            decimal(6,0),
     periodo_pago         char(6),
     f_pago               date,
     nrp                  char(11),
     folio_liquida        decimal(9,0) ,
     num_crd_ifv          decimal(10,0),
     imp_ap_pat           decimal(12,2),
     imp_am_cre           decimal(12,2),
     tpo_originacion      smallint,
     num_credito_cre      decimal(10,0),
     edo_cre              smallint,
     edo_credito          smallint,
     num_credito_pag      decimal(10,0),
     edo_pag              smallint,  
     num_credito_cor      decimal(10,0),
     dif_num_cred         smallint)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT nss,
           id_dis_interface_hs,
           id_derechohabiente,
           folio_sua,
           periodo_pago,
           f_pago,
           nrp,
           folio_liquida,
           num_crd_ifv,
           imp_ap_pat,
           imp_am_cre
    INTO   v_nss,
           v_id_dis_interface_hs,
           v_id_derechohabiente,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp,
           v_folio_liquida,
           v_num_crd_ifv,
           v_imp_ap_pat,
           v_imp_am_cre
    FROM   tmp_interface_hs    

    LET v_num_credito_cre = 0;
    LET v_num_credito_cor = 0;
    LET v_tpo_originacion = 0;
    LET v_ban_trabajador  = 0;
    LET v_num_credito_pag = 0;
    LET v_edo_pag         = 0;
    LET v_edo_credito     = 0;
    LET v_dif_num_cred    = 0;

    FOREACH
      SELECT cre.num_credito, cre.tpo_originacion
      INTO   v_num_credito_cre, v_tpo_originacion
      FROM   cre_acreditado cre
      WHERE  cre.id_derechohabiente = v_id_derechohabiente
      ORDER BY f_otorga DESC
      LET v_ban_trabajador = 1;
      EXIT FOREACH;
    END FOREACH

    IF v_ban_trabajador = 0 THEN
       LET v_tpo_originacion = 0;
       LET v_num_credito_cor = 0;
       LET v_ban_trabajador  = 0;
       LET v_num_credito_cre = 0;
  
       SELECT pag.num_crd_ifv
       INTO   v_num_credito_pag
       FROM   cta_his_pagos pag
       WHERE  pag.id_derechohabiente   = v_id_derechohabiente
       AND    pag.folio_sua            = v_folio_sua
       --AND    pag.periodo_pago         = v_periodo_pago
       AND    pag.f_pago               = v_f_pago
       AND    pag.nrp                  = v_nrp
       AND    pag.ind_liquidacion NOT IN (1,6)
       AND    pag.folio                = 11214;
    
       IF v_num_credito_pag IS NULL THEN
         LET v_num_credito_pag = 0;
         LET v_num_credito_cor = 0;
         LET v_edo_pag         = 0;
       ELSE
         LET v_num_credito_cor = v_num_credito_pag;
         LET v_edo_pag         = 1;
       END IF
    ELSE
       LET v_num_credito_cor = v_num_credito_cre;
    END IF

    --Obtiene el estado del credito del derechohabiente si esta liquidado o no
    IF EXISTS (SELECT estado
               FROM   cta_his_credito
               WHERE  id_derechohabiente = v_id_derechohabiente 
               AND    num_credito        = v_num_credito_cor
               AND    f_actualiza       <= TODAY) THEN
       LET v_edo_credito = 2;
    ELSE
       LET v_edo_credito = 0;
    END IF

    IF v_num_crd_ifv <> v_num_credito_cor THEN
       LET v_dif_num_cred = 1;
    END IF

    INSERT INTO dis_numero_credito_hs VALUES (v_nss,
                                              v_id_dis_interface_hs,
                                              v_id_derechohabiente,
                                              v_folio_sua,
                                              v_periodo_pago,
                                              v_f_pago,
                                              v_nrp,
                                              v_folio_liquida,
                                              v_num_crd_ifv,
                                              v_imp_ap_pat,
                                              v_imp_am_cre,
                                              v_tpo_originacion,
                                              v_num_credito_cre,
                                              v_ban_trabajador,
                                              v_edo_credito,
                                              v_num_credito_pag,
                                              v_edo_pag,
                                              v_num_credito_cor,
                                              v_dif_num_cred);

    LET v_num_credito_cre = 0;
    LET v_num_credito_cor = 0;
    LET v_tpo_originacion = 0;
    LET v_ban_trabajador  = 0;
    LET v_num_credito_pag = 0;
    LET v_edo_pag         = 0;
    LET v_edo_credito     = 0;
    LET v_dif_num_cred    = 0;

  END FOREACH;

  UPDATE STATISTICS FOR TABLE dis_dse_num_cred;

  RETURN v_bnd_proceso;

END PROCEDURE;


