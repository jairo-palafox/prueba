






CREATE PROCEDURE "safreviv".sp_dis_apo_subs6(v_folio DECIMAL(9,0), v_edo_rech SMALLINT)
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 13112015
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
   RETURN  v_status ,isam_err , error_info;  
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

  DROP TABLE IF EXISTS dis_dup_ap_subsecuente;
  CREATE TABLE dis_dup_ap_subsecuente (id_derechohabiente DECIMAL(9,0) NOT NULL,
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
           fn_bimestre_pago(periodo_pago), 
           f_pago, 
           reg_pat_imss, 
           COUNT(*)
    INTO   v_nss,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_reg_pat_imss,
           v_tot_apo_subs 
    FROM   safre_tmp:tmp_dis_aposubs2
    GROUP BY 1,2,3,4,5
    HAVING COUNT(*) > 1
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       EXIT FOREACH;
    ELSE
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_nss;

      INSERT INTO dis_dup_ap_subsecuente VALUES(v_id_derechohabiente,
                                                v_nss,
                                                v_folio_sua,
                                                v_periodo_pago,
                                                v_f_pago,
                                                v_reg_pat_imss,
                                                1,
                                                1,
                                                v_tot_apo_subs);
    END IF
  END FOREACH; 

  LET v_char = "Terminado Apo_subs6_SPL";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


