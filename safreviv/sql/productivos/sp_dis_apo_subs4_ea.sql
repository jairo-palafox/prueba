






CREATE PROCEDURE "safreviv".sp_dis_apo_subs4_ea(v_folio DECIMAL(9,0), v_edo_rech SMALLINT)
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 06022018
DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_nss                 CHAR(11);
DEFINE v_folio_sua           DECIMAL(6,0);
DEFINE v_periodo_pago        CHAR(6);
DEFINE v_f_pago              DATE;
DEFINE v_reg_pat_imss        CHAR(11);
DEFINE v_tot_apo_subs        INTEGER;

DEFINE ef_id_derechohabiente DECIMAL(9,0);
DEFINE ef_nss                CHAR(11);
DEFINE ef_folio_sua          DECIMAL(6,0);
DEFINE ef_periodo_pago       CHAR(6);
DEFINE ef_f_pago             DATE;
DEFINE ef_nrp                CHAR(11);
DEFINE ef_ind_liquidacion    SMALLINT;
DEFINE ef_tot_apo_subs       INTEGER;
DEFINE ef_id_dis_interface   DECIMAL(9,0);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(70);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT; --estatus del proceso
DEFINE v_tot_imp_apo_aivs    DECIMAL(26,6);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN v_status, isam_err, error_info;  
END EXCEPTION

  --#Inicialización de variables
  LET v_bnd_proceso         = 0; --estado correcto

  LET v_id_derechohabiente  = 0;
  LET v_nss                 = "";
  LET v_folio_sua           = 0;
  LET v_periodo_pago        = "";
  LET v_f_pago              = "";
  LET v_reg_pat_imss        = "";
  LET v_tot_apo_subs        = 0;

  LET ef_id_derechohabiente = 0;
  LET ef_nss                = "";
  LET ef_folio_sua          = 0;
  LET ef_periodo_pago       = "";
  LET ef_f_pago             = "";
  LET ef_nrp                = "";
  LET ef_ind_liquidacion    = 0;
  LET ef_tot_apo_subs       = 0;
  LET ef_id_dis_interface   = 0;

  DROP TABLE IF EXISTS dis_dup_ap_subsecuente_ea;
  CREATE TABLE dis_dup_ap_subsecuente_ea (id_derechohabiente DECIMAL(9,0) NOT NULL,
                                          nss                CHAR(11),
                                          folio_sua          DECIMAL(6,0),
                                          periodo_pago       CHAR(6),
                                          f_pago             DATE,
                                          reg_pat_imss       CHAR(11),
                                          ind_liquidacion    SMALLINT,
                                          tpo_registro       INTEGER,
                                          tot_apo_subs       INTEGER)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  -- Obtiene duplicidad del archivo
  FOREACH
    SELECT nss, 
           folio_sua, 
           periodo_pago, 
           f_pago, 
           reg_pat_imss, 
           COUNT(*)
    INTO   v_nss,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_reg_pat_imss,
           v_tot_apo_subs 
    FROM   safre_tmp:tmp_dis_aposubs2_ea
    GROUP BY 1,2,3,4,5
    HAVING COUNT(*) > 1
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       EXIT FOREACH;
    ELSE
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_nss;

      INSERT INTO dis_dup_ap_subsecuente_ea VALUES(v_id_derechohabiente,
                                                   v_nss,
                                                   v_folio_sua,
                                                   v_periodo_pago,
                                                   v_f_pago,
                                                   v_reg_pat_imss,
                                                   "",
                                                   1,
                                                   v_tot_apo_subs);
    END IF
  END FOREACH; 

  -- Obtiene duplicidad de la tabla de Entidades Financieras
  FOREACH
    SELECT id_derechohabiente, 
           folio_sua, 
           periodo_pago, 
           f_pago, 
           nrp, 
           ind_liquidacion,
           COUNT(*)
    INTO   ef_id_derechohabiente,
           ef_folio_sua,
           ef_periodo_pago,
           ef_f_pago,
           ef_nrp,
           ef_ind_liquidacion,
           ef_tot_apo_subs 
    FROM   dis_interface_ef
    WHERE  ind_liquidacion = 0
    GROUP BY 1,2,3,4,5,6
    HAVING COUNT(*) > 1
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       EXIT FOREACH;
    ELSE
      SELECT nss
      INTO   ef_nss
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = ef_id_derechohabiente;

       INSERT INTO dis_dup_ap_subsecuente_ea VALUES(ef_id_derechohabiente,
                                                    ef_nss,
                                                    ef_folio_sua,
                                                    ef_periodo_pago,
                                                    ef_f_pago,
                                                    ef_nrp,
                                                    ef_ind_liquidacion,
                                                    2,
                                                    ef_tot_apo_subs);
    END IF
  END FOREACH;

  UPDATE STATISTICS FOR TABLE dis_dup_ap_subsecuente_ea;

  LET ef_id_derechohabiente = 0;
  LET ef_nss                = "";
  LET ef_folio_sua          = 0;
  LET ef_periodo_pago       = "";
  LET ef_f_pago             = "";
  LET ef_nrp                = "";
  LET ef_ind_liquidacion    = 0;

  -- Actualiza indicador registros duplicados Entidades Financieras
  FOREACH
    SELECT id_derechohabiente, 
           folio_sua, 
           periodo_pago, 
           f_pago, 
           reg_pat_imss, 
           ind_liquidacion
    INTO   ef_id_derechohabiente,
           ef_folio_sua,
           ef_periodo_pago,
           ef_f_pago,
           ef_nrp,
           ef_ind_liquidacion
    FROM   dis_dup_ap_subsecuente_ea
    WHERE  tpo_registro = 2
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       EXIT FOREACH;
    ELSE
      SELECT FIRST 1 (id_dis_interface_ef)
      INTO   ef_id_dis_interface
      FROM   dis_interface_ef
      WHERE  id_derechohabiente = ef_id_derechohabiente
      AND    folio_sua          = ef_folio_sua
      AND    periodo_pago       = ef_periodo_pago
      AND    f_pago             = ef_f_pago
      AND    nrp                = ef_nrp
      AND    ind_liquidacion    = ef_ind_liquidacion;

      UPDATE dis_interface_ef 
      SET    ind_liquidacion      = 100
      WHERE  id_derechohabiente   = ef_id_derechohabiente
      AND    folio_sua            = ef_folio_sua
      AND    periodo_pago         = ef_periodo_pago
      AND    f_pago               = ef_f_pago
      AND    nrp                  = ef_nrp
      AND    ind_liquidacion      = ef_ind_liquidacion
      AND    id_dis_interface_ef <> ef_id_dis_interface;
    END IF
  END FOREACH;

  LET v_char = "Terminado Apo_subs4_ea_SPL";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


