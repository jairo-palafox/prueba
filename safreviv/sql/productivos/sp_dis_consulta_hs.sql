






CREATE PROCEDURE "safreviv".sp_dis_consulta_hs(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 19012015
--Declaración de variables

DEFINE v_nss                 CHAR(11);       --Numero seguro social
DEFINE v_derechohabiente     DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_imp_ap_pat          DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_imp_am_cre          DECIMAL(12,2);  --Importe amortizaciones de credito
DEFINE v_folio_sua           DECIMAL(6);     --Folio SUA
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_derechohabiente_hs  DECIMAL(9,0);   --Derechohabiente de cuenta credito
DEFINE v_estado              SMALLINT;       --Estado del registro

DEFINE v_bnd_proceso          SMALLINT;
DEFINE v_char                 CHAR(20);

DEFINE v_status               SMALLINT;
DEFINE sql_err                INTEGER ;
DEFINE isam_err               INTEGER ;
DEFINE error_info             CHAR(70);



ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION


--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-600/sp_dis_consulta_hs.TRACE';
--TRACE ON;

--SET DEBUG FILE TO '/ds/safreviv_int/dis/sp_dis_consulta_hs.TRACE';
--SET DEBUG FILE TO '/home/safreviv/sp_dis_consulta_hs.TRACE';
--SET DEBUG FILE TO '/ds/safreviv/dis/sql/sp_dis_consulta_hs.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_nss                    = "";
LET v_derechohabiente        = 0;
LET v_num_credito            = 0;
LET v_nrp                    = "";
LET v_f_pago                 = "";
LET v_imp_ap_pat             = 0.00;
LET v_imp_am_cre             = 0.00;
LET v_folio_sua              = 0;
LET v_periodo_pago           = "";
LET v_derechohabiente_hs     = 0;
LET v_estado                 = 0;

LET v_bnd_proceso             = 0;
LET v_char                    = "";

LET v_status                  = 0;

LET sql_err                   = 0;
LET isam_err                  = 0;
LET error_info                = "";

  --Tabla temporal de registros dispersados a HS o Crédito
  --DROP TABLE IF EXISTS tmp_dis_cons_hs;
  CREATE TABLE tmp_dis_cons_hs (nss                CHAR(11) NOT NULL,
                                num_credito        DECIMAL(10,0),
                                nrp                CHAR(11),
                                f_pago             DATE, 
                                monto_apo          DECIMAL(12,2),
                                monto_amo          DECIMAL(12,2),
                                folio_sua          DECIMAL(6,0),
                                periodo_pago       CHAR(6),
                                estado             SMALLINT NOT NULL)
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SET PDQPRIORITY HIGH;

  SELECT a.nss, b.id_derechohabiente
  FROM   safre_tmp:tmp_ext_dis_cart1 a,
         afi_derechohabiente b
  WHERE  a.nss = b.nss
  INTO TEMP tmp_dis_nss_cons_hs;

  UPDATE STATISTICS FOR TABLE tmp_dis_nss_cons_hs;

  --Verifica si existe el NSS en la base de derechohabientes
  FOREACH
    SELECT UNIQUE a.nss
    INTO   v_nss
    FROM   safre_tmp:tmp_ext_dis_cart1 a
    WHERE  a.nss NOT IN (SELECT b.nss
                         FROM   tmp_dis_nss_cons_hs b)

    LET v_estado = 0;

    INSERT INTO tmp_dis_cons_hs VALUES (v_nss,
                                        v_num_credito,
                                        v_nrp,
                                        v_f_pago,
                                        v_imp_ap_pat,
                                        v_imp_am_cre,
                                        v_folio_sua,
                                        v_periodo_pago,
                                        v_estado);
  
    LET v_nss = "";
  END FOREACH;

  --Leer todos los registros de la tabla temporal tmp_dis_nss_cons_hs
  FOREACH
    SELECT UNIQUE tmp.nss, tmp.id_derechohabiente
    INTO   v_nss, v_derechohabiente
    FROM   tmp_dis_nss_cons_hs tmp

    --Registros con dispersión a cartera
    FOREACH
      SELECT hs.id_derechohabiente,
             hs.folio_sua,
             hs.periodo_pago,
             hs.f_pago,
             hs.nrp,
             hs.num_crd_ifv,
             hs.imp_ap_pat,
             hs.imp_am_cre
      INTO   v_derechohabiente_hs,
             v_folio_sua,
             v_periodo_pago,
             v_f_pago,
             v_nrp,
             v_num_credito,
             v_imp_ap_pat,
             v_imp_am_cre
      FROM   dis_interface_hs hs
      WHERE  hs.id_derechohabiente = v_derechohabiente

      LET v_estado = 1;

      INSERT INTO tmp_dis_cons_hs VALUES (v_nss,
                                          v_num_credito,
                                          v_nrp,
                                          v_f_pago,
                                          v_imp_ap_pat,
                                          v_imp_am_cre,
                                          v_folio_sua,
                                          v_periodo_pago,
                                          v_estado);
       LET v_num_credito     = 0;
       LET v_nrp             = "";
       LET v_f_pago          = "";
       LET v_imp_ap_pat      = 0.00;
       LET v_imp_am_cre      = 0.00;
       LET v_folio_sua       = 0;
       LET v_periodo_pago    = "";
    END FOREACH;

    --Registros con dispersión a devolución de saldos
    FOREACH
      SELECT de.id_derechohabiente,
             0,
             de.periodo_pago,
             de.f_pago,
             '',
             de.num_credito,
             de.monto_pesos,
             0 
      INTO   v_derechohabiente_hs,
             v_folio_sua,
             v_periodo_pago,
             v_f_pago,
             v_nrp,
             v_num_credito,
             v_imp_ap_pat,
             v_imp_am_cre
      FROM   dse_devolucion de
      WHERE  de.id_derechohabiente = v_derechohabiente
      AND    de.modulo_cod         = 'dis'

      LET v_estado = 2;

      INSERT INTO tmp_dis_cons_hs VALUES (v_nss,
                                          v_num_credito,
                                          v_nrp,
                                          v_f_pago,
                                          v_imp_ap_pat,
                                          v_imp_am_cre,
                                          v_folio_sua,
                                          v_periodo_pago,
                                          v_estado);
       LET v_num_credito     = 0;
       LET v_nrp             = "";
       LET v_f_pago          = "";
       LET v_imp_ap_pat      = 0.00;
       LET v_imp_am_cre      = 0.00;
       LET v_folio_sua       = 0;
       LET v_periodo_pago    = "";
    END FOREACH;

    --Sin dispersión a cartera y
    --Sin devolución de Saldos Exc 5%
    IF v_estado == 0 THEN

       LET v_estado = 3; 

       INSERT INTO tmp_dis_cons_hs VALUES (v_nss,
                                           v_num_credito,
                                           v_nrp,
                                           v_f_pago,
                                           v_imp_ap_pat,
                                           v_imp_am_cre,
                                           v_folio_sua,
                                           v_periodo_pago,
                                           v_estado);
    END IF

    LET v_nss             = "";
    LET v_derechohabiente = 0;
    LET v_num_credito     = 0;
    LET v_nrp             = "";
    LET v_f_pago          = "";
    LET v_imp_ap_pat      = 0.00;
    LET v_imp_am_cre      = 0.00;
    LET v_folio_sua       = 0;
    LET v_periodo_pago    = "";
    LET v_estado          = 0;
  END FOREACH;

  UPDATE STATISTICS FOR TABLE tmp_dis_cons_hs;

   --TRACE 'Finaliza sp_dis_consulta_hs con valor '||v_bnd_proceso;
   
   LET v_char = "Terminado Extractor Dispersión HS";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


