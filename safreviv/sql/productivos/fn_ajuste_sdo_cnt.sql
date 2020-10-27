






CREATE PROCEDURE "safreviv".fn_ajuste_sdo_cnt(p_indice          INTEGER,      --Indicador
                                   p_folio_liquida   DECIMAL(9,0), --Folio de liquidación
                                   p_f_liquida       DATE,         --Fecha de liquidación
                                   p_subcuenta       SMALLINT,     --Subcuenta
                                   p_movimiento      SMALLINT,     --Movimiento
                                   p_cta_contable    CHAR(10),     --Número de cuenta
                                   p_monto_pesos     DECIMAL(12,2), --Monto de pesos  
                                   p_cod_proceso_cnt SMALLINT, 
                                   p_cod_proceso     SMALLINT, 
                                   p_cod_tran_cnt    SMALLINT,
                                   p_tpo_ajuste      SMALLINT)      --Código Proceso                                  
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 14022018
--Declaración de variables  
DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(20);
DEFINE v_tot_cta_cnt_abono   SMALLINT;
DEFINE v_tot_cta_cnt_cargo   SMALLINT;
DEFINE v_max_transaccion     SMALLINT;
  
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
  
  ON EXCEPTION
     SET sql_err, isam_err, error_info
     LET v_status = sql_err;
     RETURN  v_status ,isam_err , error_info;
  END EXCEPTION

  --SET DEBUG FILE TO '/ds/safreviv/cnt/sql/fn_val_ajuste_sdo_cnt.trace';
  --TRACE ON;
  
  LET v_bnd_proceso = 0;
  LET v_char        = "";
  LET v_status      = 0;
  
  LET sql_err       = 0;
  LET isam_err      = 0;
  LET error_info    = "";

  LET v_tot_cta_cnt_abono = 0;
  LET v_tot_cta_cnt_cargo = 0;
  LET v_max_transaccion   = 0;

  SELECT COUNT(*)
  INTO   v_tot_cta_cnt_abono
  FROM   cnt_transaccion a
  WHERE  a.folio_liquida      = p_folio_liquida
  AND    a.cta_contable       = p_cta_contable
  AND    a.cod_naturaleza_cta = 1;
  IF v_tot_cta_cnt_abono = 0 THEN
     LET p_cod_tran_cnt  = 105;
  ELSE
     SELECT MAX(a.cod_transaccion_cnt)
     INTO   v_max_transaccion
     FROM   cnt_transaccion a
     WHERE  a.folio_liquida      = p_folio_liquida
     AND    a.cta_contable       = p_cta_contable
     AND    a.cod_naturaleza_cta = 1;
     LET p_cod_tran_cnt          = v_max_transaccion + 1;
  END IF
  
  SELECT COUNT(*)
  INTO   v_tot_cta_cnt_cargo
  FROM   cnt_transaccion a
  WHERE  a.folio_liquida      = p_folio_liquida
  AND    a.cta_contable       = p_cta_contable
  AND    a.cod_naturaleza_cta = 2;
  IF v_tot_cta_cnt_cargo = 0 THEN
     LET p_cod_tran_cnt  = 105;
  ELSE
     SELECT MAX(a.cod_transaccion_cnt)
     INTO   v_max_transaccion
     FROM   cnt_transaccion a
     WHERE  a.folio_liquida      = p_folio_liquida
     AND    a.cta_contable       = p_cta_contable
     AND    a.cod_naturaleza_cta = 2;
     LET p_cod_tran_cnt          = v_max_transaccion + 1;
  END IF
 
  IF p_tpo_ajuste = 0 THEN
     INSERT INTO cnt_trn_aj_sdo (id_cuenta_contable,
                                 subcuenta,
                                 movimiento,
                                 monto_pesos,
                                 cta_contable,
                                 folio_liquida,
                                 f_liquida)
     VALUES                     (p_indice, 
                                 p_subcuenta, 
                                 p_movimiento, 
                                 p_monto_pesos, 
                                 p_cta_contable, 
                                 p_folio_liquida, 
                                 p_f_liquida);
      
     INSERT INTO cnt_transaccion (id_cuenta_contable, 
                                  folio_cnt, 
                                  cod_proceso_cnt,
                                  cod_proceso, 
                                  cod_transaccion_cnt, 
                                  cod_subcta_cnt, 
                                  cta_contable, 
                                  cod_naturaleza_cta, 
                                  folio_liquida, 
                                  importe, 
                                  f_liquida, 
                                  f_emision, 
                                  tpo_transaccion, 
                                  estado)
     VALUES                      (p_indice, 
                                  0,
                                  p_cod_proceso_cnt, 
                                  p_cod_proceso, 
                                  p_cod_tran_cnt, 
                                  p_subcuenta, 
                                  p_cta_contable, 
                                 (SELECT categoria 
                                  FROM cat_movimiento
                                  WHERE movimiento = p_movimiento),         
                                  p_folio_liquida, 
                                  p_monto_pesos,                   
                                  p_f_liquida,
                                  p_f_liquida,       
                                  0,
                                  10);                    

  END IF
  
  IF p_tpo_ajuste = 1 THEN
     UPDATE cnt_trn_aj_sdo
     SET    cta_contable        = p_cta_contable,
            cod_transaccion_cnt = p_cod_tran_cnt
     WHERE  id_cuenta_contable  = p_indice
     AND    folio_liquida       = p_folio_liquida
     AND    f_liquida           = p_f_liquida;
        
     UPDATE cnt_transaccion
     SET    cta_contable        = p_cta_contable,
            cod_transaccion_cnt = p_cod_tran_cnt
     WHERE  id_cuenta_contable  = p_indice
     AND    folio_liquida       = p_folio_liquida
     AND    f_liquida           = p_f_liquida;
  END IF

  --TRACE 'Finaliza fn_dis_ext_acl_cam_nss con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado fn_ajuste_sdo_cnt exitosamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


