






CREATE PROCEDURE "safreviv".fn_dis_ext_acl_cam_nss(p_folio DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 07092015
--Declaración de variables
DEFINE v_id_derechohabiente    DECIMAL(9,0);
DEFINE v_folio                 DECIMAL(9,0);
DEFINE v_f_actualiza           DATE;

DEFINE v_valida                SMALLINT; 

DEFINE v_edo_credito           SMALLINT;       --Tipo de credito del derechohabiente   
DEFINE v_tipo_trabajador       SMALLINT;       --Codigo de proceso del tipo de trabajador   
DEFINE v_tpo_credito           SMALLINT;  
DEFINE v_num_credito_crd       DECIMAL(10,0);
DEFINE v_f_otorga              DATE; 
DEFINE v_f_liquida_cred        DATE;   

DEFINE v_nss                   CHAR(11);
DEFINE v_ind_estado_cuenta     SMALLINT;

DEFINE v_sub4                  SMALLINT;        --Subcuenta vivienda 97
DEFINE v_acc_4                 DECIMAL(16,6);   --Acciones vivienda 97
DEFINE v_pes_4                 DECIMAL(12,2);   --Pesos vivienda 97
DEFINE v_sub8                  SMALLINT;        --Subcuenta vivienda 92
DEFINE v_acc_8                 DECIMAL(16,6);   --Acciones vivienda 92
DEFINE v_pes_8                 DECIMAL(12,2);   --Pesos vivienda 92
DEFINE v_sub41                 SMALLINT;        --Subcuenta amortización
DEFINE v_acc_41                DECIMAL(16,6);   --Acciones amortización
DEFINE v_pes_41                DECIMAL(12,2);   --Pesos amortización
DEFINE v_resultado             SMALLINT;

DEFINE v_no_id_der             DECIMAL(9,0);
DEFINE v_no_folio              DECIMAL(9,0);
DEFINE v_no_f_actualiza        DATE;

DEFINE v_no_edo_credito        SMALLINT;       --Tipo de credito del derechohabiente   
DEFINE v_no_tipo_trabajador    SMALLINT;       --Codigo de proceso del tipo de trabajador   
DEFINE v_no_tpo_credito        SMALLINT;  
DEFINE v_no_num_credito_crd    DECIMAL(10,0);
DEFINE v_no_f_otorga           DATE; 
DEFINE v_no_f_liquida_cred     DATE;

DEFINE v_no_nss                CHAR(11);
DEFINE v_no_ind_est_cuenta     SMALLINT;

DEFINE v_no_sub4               SMALLINT;        --Subcuenta vivienda 97
DEFINE v_no_acc_4              DECIMAL(16,6);   --Acciones vivienda 97
DEFINE v_no_pes_4              DECIMAL(16,6);   --Pesos vivienda 97
DEFINE v_no_sub8               SMALLINT;        --Subcuenta vivienda 92
DEFINE v_no_acc_8              DECIMAL(16,6);   --Acciones vivienda 92
DEFINE v_no_pes_8              DECIMAL(12,2);   --Pesos vivienda 92
DEFINE v_no_sub41              SMALLINT;        --Subcuenta amortización
DEFINE v_no_acc_41             DECIMAL(16,6);   --Acciones amortización
DEFINE v_no_pes_41             DECIMAL(12,2);   --Pesos amortización
DEFINE v_no_resultado          SMALLINT;  

DEFINE v_bnd_proceso           SMALLINT;
DEFINE v_char                  CHAR(20);

DEFINE v_status                SMALLINT;
DEFINE sql_err                 INTEGER ;
DEFINE isam_err                INTEGER ;
DEFINE error_info              CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--SET DEBUG FILE TO '/safreviv_req/PRODINF-888/fn_dis_ext_acl_cam_nss.TRACE';
--TRACE ON;

  LET v_bnd_proceso = 0;
  LET v_char        = "";
  LET v_status      = 0;

  LET sql_err       = 0;
  LET isam_err      = 0;
  LET error_info    = "";

  DROP TABLE IF EXISTS tmp_dis_cred_ant;
  DROP TABLE IF EXISTS tmp_dis_fol_acl_c_nss;
  DROP TABLE IF EXISTS tmp_dis_det_acl_c_nss;
  DROP TABLE IF EXISTS tmp_dis_acl_c_nss_0;      
   	
  CREATE TABLE tmp_dis_cred_ant (folio              DECIMAL(9,0),  
                                 f_actualiza        DATE, 
                                 nss                CHAR(11), 
                                 num_credito_crd    DECIMAL(10,0),
                                 edo_credito        SMALLINT,
                                 pes_4              DECIMAL(12,2),
                                 acc_4              DECIMAL(16,6),
                                 pes_8              DECIMAL(12,2),
                                 acc_8              DECIMAL(16,6),
                                 pes_41             DECIMAL(12,2),
                                 acc_41             DECIMAL(16,6),
                                 ind_estado_cuenta  SMALLINT,      
                                 no_nss             CHAR(11),     
                                 no_num_credito_crd DECIMAL(10,0),
                                 no_edo_credito     SMALLINT,     
                                 no_pes_4           DECIMAL(12,2),
                                 no_acc_4           DECIMAL(16,6),
                                 no_pes_8           DECIMAL(12,2),
                                 no_acc_8           DECIMAL(16,6),
                                 no_pes_41          DECIMAL(12,2),
                                 no_acc_41          DECIMAL(16,6),
                                 no_ind_est_cuenta  SMALLINT,     
                                 f_archivo          DATE)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;    

  SET PDQPRIORITY HIGH;

  --Se obtienen los registros que se dispersarón por aclaratorios con cambio de NSS
  IF p_folio <> 0 THEN                                                                                                                                                                                                                                                                                                                                                                                           
     SELECT unique a.folio, a.folio_referencia, a.f_actualiza
     FROM   glo_folio a
     WHERE  a.proceso_cod       = 901
     AND    a.folio_referencia IN (SELECT b.folio
                                   FROM   glo_folio b
                                   WHERE  b.proceso_cod  = 103
                                   AND    b.f_actualiza <= '09/30/2014'     
                                   AND    b.folio        = p_folio                                           
                                   AND    b.status       = 2)
     AND a.status               = 2
     INTO TEMP tmp_dis_fol_acl_c_nss;
  ELSE
    SELECT unique a.folio, a.folio_referencia, a.f_actualiza
    FROM   glo_folio a
    WHERE  a.proceso_cod       = 901
    AND    a.folio_referencia IN (SELECT b.folio
                                  FROM   glo_folio b
                                  WHERE  b.proceso_cod  = 103      
                                  AND    b.f_actualiza <= '09/30/2014'                                                                            
                                  AND    b.status       = 2)
    AND a.status               = 2
    INTO TEMP tmp_dis_fol_acl_c_nss;
  END IF
   
  CREATE INDEX xie1tmp_dis_fol_acl_c_nss ON tmp_dis_fol_acl_c_nss 
  (folio) IN dis_ix_dbs;  
   
  UPDATE statistics FOR TABLE tmp_dis_fol_acl_c_nss;  
  
  --Obtener los registros Dispersados de Aclaratorios Cambio NSS 
  SELECT a.id_derechohabiente,
         a.folio_sua,
         a.periodo_pago,
         a.f_pago,
         a.nrp,
         b.folio,
         b.folio_referencia,
         b.f_actualiza,
         'PR' rubro --Pago Real
  FROM   dis_interface_hs a,
         tmp_dis_fol_acl_c_nss b
  WHERE  a.folio_liquida = b.folio
  UNION ALL
  SELECT a.id_derechohabiente,
         a.folio_sua,
         a.periodo_pago,
         a.f_pago,
         a.nrp,
         b.folio,
         b.folio_referencia,
         b.f_actualiza,
         'EF' rubro --Entidad Financiera
  FROM   dis_interface_ef a,
         tmp_dis_fol_acl_c_nss b
  WHERE  a.folio_liquida = b.folio
  INTO TEMP tmp_dis_det_acl_c_nss;   
   
  CREATE INDEX xie1tmp_dis_det_acl_c_nss ON tmp_dis_det_acl_c_nss 
  (folio_referencia) IN dis_ix_dbs;
   
  CREATE INDEX xie2tmp_dis_det_acl_c_nss ON tmp_dis_det_acl_c_nss 
  (id_derechohabiente, folio_sua, periodo_pago, f_pago, nrp ) IN dis_ix_dbs;
       
  --Obtener los registros Dispersados de Aclaratorios Cambio NSS antes del ajuste al proceso  
  SELECT pag.id_derechohabiente,
         pag.folio_sua,
         pag.periodo_pago,
         pag.f_pago,
         pag.nrp,
         pag.id_referencia,
         com.id_derhab_nuevo,
         det.id_derechohabiente id_dis,
         det.rubro,
         det.folio,
         det.folio_referencia,
         det.f_actualiza,
         '0 ANTES DEL CAMBIO' corte
  FROM   cta_his_pagos pag,
  OUTER  cta_pag_complemento com,
         tmp_dis_det_acl_c_nss det
  WHERE  pag.folio                          = det.folio_referencia
  AND    pag.id_derechohabiente             = det.id_derechohabiente
  AND    pag.folio_sua                      = det.folio_sua
  AND    fn_bimestre_pago(pag.periodo_pago) = det.periodo_pago
  AND    pag.f_pago                         = det.f_pago
  AND    pag.nrp                            = det.nrp
  AND    pag.ind_liquidacion                = 5
  AND    pag.tpo_aclaracion                <> ' '
  AND    pag.folio                          = com.folio
  AND    pag.id_referencia                  = com.id_referencia
  INTO TEMP tmp_dis_acl_c_nss_0;       
 
  LET v_id_derechohabiente = 0;
  LET v_folio = 0;
  LET v_f_actualiza = '';  
      
  FOREACH
    SELECT UNIQUE(id_derechohabiente), folio, f_actualiza
    INTO   v_id_derechohabiente, v_folio, v_f_actualiza
    FROM   tmp_dis_det_acl_c_nss   	
   	
   	LET v_valida          = 1;   	   
   	LET v_edo_credito     = 0;
    LET v_tipo_trabajador = 0;
    LET v_tpo_credito     = 0;
    LET v_num_credito_crd = 0;
    LET v_f_otorga        = '';
    LET v_f_liquida_cred  = '';
   	     
    EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente, v_valida)
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
      
    SELECT nss, ind_estado_cuenta
    INTO   v_nss, v_ind_estado_cuenta
    FROM   afi_derechohabiente
    WHERE  id_derechohabiente = v_id_derechohabiente;        
        
    LET v_sub4              = 4;
    LET v_resultado         = 0;
    LET v_acc_4             = 0;
    LET v_pes_4             = 0;
    LET v_ind_estado_cuenta = 0;                    
                           
    EXECUTE PROCEDURE fn_saldo_dia(v_nss, v_id_derechohabiente, v_sub4, TODAY)
                 INTO v_resultado,
                      v_acc_4,
                      v_pes_4;

    LET v_sub8              = 8;
    LET v_resultado         = 0;
    LET v_acc_8             = 0;
    LET v_pes_8             = 0;
    LET v_ind_estado_cuenta = 0;                    
                           
    EXECUTE PROCEDURE fn_saldo_dia(v_nss, v_id_derechohabiente, v_sub8, TODAY)
                 INTO v_resultado,
                      v_acc_8,
                      v_pes_8;        

    LET v_sub41             = 41;
    LET v_resultado         = 0;
    LET v_acc_41            = 0;
    LET v_pes_41            = 0;
    LET v_ind_estado_cuenta = 0;                    
                           
    EXECUTE PROCEDURE fn_saldo_dia(v_nss, v_id_derechohabiente, v_sub41, TODAY)
                 INTO v_resultado,
                      v_acc_41,
                      v_pes_41;        
      
    SELECT FIRST 1 id_derhab_nuevo, folio, f_actualiza
   	INTO   v_no_id_der, v_no_folio, v_no_f_actualiza
   	FROM   tmp_dis_acl_c_nss_0
   	WHERE  id_derechohabiente = v_id_derechohabiente
   	AND    folio              = v_folio
   	AND    f_actualiza        = v_f_actualiza;
      
    IF v_no_id_der IS NOT NULL THEN 
       EXECUTE FUNCTION fn_credito_vivienda(v_no_id_der, v_valida)
                   INTO v_no_edo_credito,
                        v_no_tipo_trabajador,
                        v_no_tpo_credito,
                        v_no_num_credito_crd,
                        v_no_f_otorga,
                        v_no_f_liquida_cred;
                          
       IF v_no_edo_credito = -2 OR v_no_edo_credito = -1 OR v_no_edo_credito = 1 THEN 
          LET v_no_num_credito_crd = 0;
          LET v_no_tipo_trabajador = 0;
          LET v_no_f_otorga        = '';
          LET v_no_tpo_credito     = 0;
       END IF
         
       LET v_no_sub4           = 4;
       LET v_no_resultado      = 0;
       LET v_no_acc_4          = 0;
       LET v_no_pes_4          = 0;             
       LET v_no_ind_est_cuenta = 0;

       LET v_no_sub8           = 8;
       LET v_no_resultado      = 0;
       LET v_no_acc_8          = 0;
       LET v_no_pes_8          = 0;

       LET v_no_sub41          = 41;
       LET v_no_resultado      = 0;
       LET v_no_acc_41         = 0;
       LET v_no_pes_41         = 0;
         
       LET v_no_nss            = '';
            
       SELECT nss, ind_estado_cuenta           
       INTO   v_no_nss, v_no_ind_est_cuenta 
       FROM   afi_derechohabiente   
       WHERE  id_derechohabiente = v_no_id_der;
               
       EXECUTE PROCEDURE fn_saldo_dia(v_no_nss, v_no_id_der, v_no_sub4, TODAY)
                    INTO v_no_resultado,
                         v_no_acc_4,
                         v_no_pes_4;

       EXECUTE PROCEDURE fn_saldo_dia(v_no_nss, v_no_id_der, v_no_sub8, TODAY)
                    INTO v_no_resultado,
                         v_no_acc_8,
                         v_no_pes_8;

       EXECUTE PROCEDURE fn_saldo_dia(v_no_nss, v_no_id_der, v_no_sub41, TODAY)
                    INTO v_no_resultado,
                         v_no_acc_41,
                         v_no_pes_41;

    END IF                                                
      
    INSERT INTO tmp_dis_cred_ant VALUES (v_folio, 
                                         v_f_actualiza,
                                         v_nss, 
                                         v_num_credito_crd,
                                         v_edo_credito,
                                         v_pes_4,
                                         v_acc_4,
                                         v_pes_8,
                                         v_acc_8,
                                         v_pes_41,
                                         v_acc_41,
                                         v_ind_estado_cuenta,                                           
                                         v_no_nss, 
                                         v_no_num_credito_crd,
                                         v_no_edo_credito,
                                         v_no_pes_4,
                                         v_no_acc_4,
                                         v_no_pes_8,
                                         v_no_acc_8,
                                         v_no_pes_41,
                                         v_no_acc_41,
                                         v_no_ind_est_cuenta,
                                         TODAY);
   
  END FOREACH 
    
  --TRACE 'Finaliza fn_dis_ext_acl_cam_nss con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado Extractor Dispersión de aclaratorios con cambio de NSS";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


