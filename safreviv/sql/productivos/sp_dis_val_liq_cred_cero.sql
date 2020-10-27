






CREATE PROCEDURE "safreviv".sp_dis_val_liq_cred_cero(p_folio    DECIMAL(9,0),
                                          p_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 24102018
--Declaración de variables
DEFINE sql_err                  INTEGER ;
DEFINE isam_err                 INTEGER ;
DEFINE error_info               CHAR(70);
DEFINE v_status                 SMALLINT;

DEFINE v_char                   CHAR(20);
DEFINE v_bnd_proceso            SMALLINT;       --Estatus del proceso

DEFINE v_nss 		        CHAR(11);
DEFINE v_folio_liquida 	        DECIMAL(9,0);
DEFINE v_id_referencia          DECIMAL(9,0);
DEFINE v_folio_referencia       DECIMAL(9,0);
DEFINE v_id_derechohabiente     DECIMAL(9,0);

DEFINE v_id_derechohabiente_pag DECIMAL(9,0);
DEFINE v_folio_sua	     	DECIMAL(6,0);
DEFINE v_periodo_pago	   	CHAR(6);
DEFINE v_f_pago	         	DATE;
DEFINE v_nrp	            	CHAR(11);

DEFINE v_edo_liq                SMALLINT;
DEFINE v_f_actualiza            DATE;

DEFINE v_folio_pag              DECIMAL(9,0);
DEFINE v_folio_dis              DECIMAL(9,0);

DEFINE v_id_derechohabiente_inf DECIMAL(9,0);
DEFINE v_tpo_inconsistente      SMALLINT;
DEFINE v_id_referencia_inf      DECIMAL(9,0);


ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status, isam_err, error_info;
END EXCEPTION

  --SET DEBUG FILE TO '/safreviv_int/dis/ERR_cred_cero.TRACE';
  --TRACE ON;

--#Inicialización de variables
  LET sql_err                   = 0;
  LET isam_err                  = 0;
  LET error_info                = "";
  LET v_status                  = 0;

  LET v_bnd_proceso             = 0; --Estado correcto

  LET v_nss 		        = "";
  LET v_folio_liquida           = 0;
  LET v_id_referencia           = 0;
  LET v_folio_referencia        = 0;
  LET v_id_derechohabiente      = 0;

  LET v_id_derechohabiente_pag  = 0;
  LET v_folio_sua               = "";
  LET v_periodo_pago            = "";
  LET v_f_pago                  = "";
  LET v_nrp                     = "";

  LET v_edo_liq                 = 0;
  LET v_f_actualiza             = TODAY;

  LET v_folio_pag               = 0;
  LET v_folio_dis               = 0;

  LET v_id_derechohabiente_inf  = 0;
  LET v_tpo_inconsistente       = 0;
  LET v_id_referencia_inf       = 0;

  SET PDQPRIORITY HIGH;

  DROP TABLE IF EXISTS tmp_dis_inf_inc;
  CREATE TABLE tmp_dis_inf_inc (id_derechohabiente  DECIMAL(9,0),
                                folio_dis           DECIMAL(9,0), 
                                folio_pag           DECIMAL(9,0),
                                id_referencia       DECIMAL(9,0),
                                nss                 CHAR(11),
                                folio_sua           DECIMAL(6,0),
                                periodo_pago        CHAR(6),
                                f_pago              DATE,
                                nrp                 CHAR(11),
                                estado_liq          SMALLINT,
                                f_actualiza         DATE)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SELECT af.nss nss,
         di.folio_liquida,
         di.id_referencia,
         gf.folio_referencia,
         af.id_derechohabiente
  FROM   dis_info_inconsistente di,
         afi_derechohabiente af,
         glo_folio gf
  WHERE  di.id_derechohabiente = af.id_derechohabiente
  AND    di.tpo_inconsistente  = 0
  AND    di.folio_liquida      = gf.folio
  INTO TEMP tmp_dis_info_inc_dup;

  UPDATE STATISTICS FOR TABLE tmp_dis_info_inc_dup;
   
  FOREACH
    SELECT a.nss,
           a.folio_liquida,
           a.id_referencia,
           a.folio_referencia,
           a.id_derechohabiente
    INTO   v_nss,
           v_folio_liquida,
           v_id_referencia,
           v_folio_referencia,
           v_id_derechohabiente
    FROM   tmp_dis_info_inc_dup a

    LET v_id_derechohabiente_pag = 0;
    LET v_folio_sua              = 0;
    LET v_periodo_pago           = "";
    LET v_f_pago                 = "";
    LET v_nrp                    = "";

    SELECT b.id_derechohabiente,
           b.folio_sua,
           fn_bimestre_pago(b.periodo_pago),
           b.f_pago,
           b.nrp
    INTO   v_id_derechohabiente_pag,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp 
    FROM   cta_his_pagos b
    WHERE  b.folio         = v_folio_referencia
    AND    b.id_referencia = v_id_referencia;

    INSERT INTO tmp_dis_inf_inc VALUES (v_id_derechohabiente,
                                        v_folio_liquida,
                                        v_folio_referencia,
                                        v_id_referencia,
                                        v_nss,
                                        v_folio_sua,
                                        v_periodo_pago,
                                        v_f_pago,
                                        v_nrp, 
                                        v_edo_liq,
                                        v_f_actualiza);
   
  END FOREACH;

  CREATE INDEX x1tmp_dis_inf_inc ON tmp_dis_inf_inc (id_derechohabiente) IN dis_ix_dbs;

  UPDATE STATISTICS FOR TABLE tmp_dis_inf_inc;

  SELECT c.id_derechohabiente,
         c.folio_dis,
         c.folio_pag,
         c.id_referencia,
         c.nss,
         c.folio_sua,
         c.periodo_pago,
         c.f_pago,
         c.nrp
  FROM   tmp_dis_inf_inc c, dis_arh_num_cred_0 d
  WHERE  c.id_derechohabiente = d.id_derechohabiente
  AND    c.folio_sua          = d.folio_sua
  AND    c.periodo_pago       = d.periodo_pago
  AND    c.f_pago             = d.f_pago
  AND    c.nrp                = d.nrp
  AND    d.estado           IN (2,3,4)
  INTO TEMP tmp_dis_por_liq_incons;
 
  CREATE INDEX x1tmp_dis_por_liq_incons ON tmp_dis_por_liq_incons (id_derechohabiente, folio_sua, periodo_pago, f_pago, nrp) IN dis_ix_dbs;
                               
  UPDATE STATISTICS FOR TABLE tmp_dis_por_liq_incons;

  FOREACH
    SELECT f.folio,
           f.id_referencia,
           e.id_derechohabiente,
           e.folio_dis,
           e.nss,
           e.folio_sua,
           e.periodo_pago,
           e.f_pago,
           e.nrp
    INTO   v_folio_pag,
           v_id_referencia,
           v_id_derechohabiente,
           v_folio_dis,
           v_nss,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp          
    FROM   tmp_dis_por_liq_incons e, cta_his_pagos f
    WHERE  e.id_referencia = f.id_referencia
    AND    e.folio_sua     = f.folio_sua
    AND    e.periodo_pago  = fn_bimestre_pago(f.periodo_pago)
    AND    e.f_pago        = f.f_pago
    AND    e.nrp           = f.nrp

    LET v_edo_liq     = 0;

    SELECT g.id_derechohabiente,
           g.tpo_inconsistente,
           g.id_referencia
    INTO   v_id_derechohabiente_inf,
           v_tpo_inconsistente,
           v_id_referencia_inf 
    FROM   dis_info_inconsistente g
    WHERE  g.id_derechohabiente = v_id_derechohabiente
    AND    g.tpo_inconsistente  = 10
    AND    g.id_referencia      = v_id_referencia;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       --Si id_referencia no esta actualizado el tpo_inconsistente a liquidado
       LET v_edo_liq = 10;

       INSERT INTO tmp_dis_inf_inc VALUES (v_id_derechohabiente,
                                           v_folio_dis,
                                           v_folio_pag,
                                           v_id_referencia,
                                           v_nss,
                                           v_folio_sua,
                                           v_periodo_pago,
                                           v_f_pago,
                                           v_nrp, 
                                           v_edo_liq,
                                           v_f_actualiza);

       {UPDATE dis_info_inconsistente
       SET    tpo_inconsistente = 10
       WHERE  id_derechohabiente = v_id_derechohabiente
       AND    tpo_inconsistente  = 0
       ANd    id_referencia      = v_id_referencia;}
    END IF
  END FOREACH;

  --TRACE 'Finaliza sp_dis_val_liq_cred_cero con valor '||v_bnd_proceso;
  LET v_char = "Terminado Val Dup Cred Cero SPL";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


