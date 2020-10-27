






CREATE PROCEDURE "selefp".fn_dup_ajuste_preliq(p_folio_liquida   DECIMAL(9,0)) --Folio de liquidación
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 22102015
--Declaración de variables  
DEFINE v_f_liquida           DATE;
DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_subcuenta           SMALLINT;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_id_referencia       DECIMAL(9,0);
DEFINE v_monto_acciones      DECIMAL(16,6);
DEFINE v_monto_pesos         DECIMAL(12,2);
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);
DEFINE v_mov_aux             SMALLINT;
 
DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(20);
  
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
  
ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

  --SET DEBUG FILE TO '/ds/safreviv/cnt/sql/fn_dup_ajuste_preliq.trace';
  --TRACE ON;
  LET v_mov_aux     = 0;
  LET v_bnd_proceso = 0;
  LET v_char        = "";
  LET v_status      = 0;
  
  LET sql_err       = 0;
  LET isam_err      = 0;
  LET error_info    = "";
  
  DROP TABLE IF EXISTS tmp_cbd_pre_ajuste_saldo;
  
  CREATE TABLE tmp_cbd_pre_ajuste_saldo  (f_liquida          DATE,
                                          id_derechohabiente DECIMAL(9,0),
                                          subcuenta          SMALLINT,
                                          fondo_inversion    SMALLINT,
                                          movimiento         SMALLINT,
                                          folio_liquida      DECIMAL(9,0),
                                          id_referencia      DECIMAL(9,0),
                                          monto_acciones     DECIMAL(16,6),
                                          monto_pesos        DECIMAL(12,2),
                                          f_valor            DATE,
                                          f_registro         DATE,
                                          h_registro         DATETIME HOUR TO SECOND,
                                          origen             CHAR(20));

  FOREACH 
    SELECT f_liquida, 
           id_derechohabiente, 
           subcuenta,  
           fondo_inversion, 
           movimiento, 
           folio_liquida, 
           id_referencia, 
           monto_acciones, 
           monto_pesos, 
           f_valor, 
           f_registro, 
           h_registro,
           origen
    INTO   v_f_liquida, 
           v_id_derechohabiente, 
           v_subcuenta,  
           v_fondo_inversion, 
           v_movimiento, 
           v_folio_liquida, 
           v_id_referencia, 
           v_monto_acciones, 
           v_monto_pesos, 
           v_f_valor, 
           v_f_registro, 
           v_h_registro,
           v_origen
    FROM   cbd_preliquida_ajuste_saldo 
    WHERE  folio_liquida = p_folio_liquida   
      
    {IF v_monto_acciones < 0 THEN
       LET v_monto_acciones = v_monto_acciones * -1;
    END IF
     
    IF v_monto_pesos < 0 THEN
       LET v_monto_pesos = v_monto_pesos * -1;
    END IF}
     
    IF v_movimiento == 1681 THEN  -- ABONO POR AJUSTE DE CONCILIACIÓN
       LET v_movimiento = 1; -- ABONO
       LET v_mov_aux    = 2; -- CARGO 
    ELSE
       IF v_movimiento == 1712 THEN -- CARGO POR AJUSTE DE CONCILIACIÓN    
          LET v_movimiento = 2; -- CARGO
          LET v_mov_aux    = 1; -- ABONO       
       END IF
    END IF
  	
    INSERT INTO tmp_cbd_pre_ajuste_saldo VALUES(v_f_liquida, 
                                                v_id_derechohabiente, 
                                                v_subcuenta,  
                                                v_fondo_inversion, 
                                                v_movimiento, 
                                                v_folio_liquida, 
                                                v_id_referencia, 
                                                v_monto_acciones, 
                                                v_monto_pesos, 
                                                v_f_valor, 
                                                v_f_registro,
                                                v_h_registro,
                                                v_origen);
            
    INSERT INTO tmp_cbd_pre_ajuste_saldo VALUES(v_f_liquida, 
                                                v_id_derechohabiente, 
                                                v_subcuenta,  
                                                v_fondo_inversion, 
                                                v_mov_aux, 
                                                v_folio_liquida, 
                                                v_id_referencia, 
                                                v_monto_acciones, 
                                                v_monto_pesos, 
                                                v_f_valor, 
                                                v_f_registro, 
                                                v_h_registro,
                                                v_origen);
  END FOREACH

  --TRACE 'Finaliza fn_dup_ajuste_preliq con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado fn_dup_ajuste_preliq exitosamente";
  RETURN v_bnd_proceso, 0, v_char;

END PROCEDURE;


