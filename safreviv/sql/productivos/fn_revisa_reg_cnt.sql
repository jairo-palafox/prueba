






CREATE PROCEDURE "safreviv".fn_revisa_reg_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                   p_tpo_reg_cnt        SMALLINT)      --Tipo registro contable
RETURNING SMALLINT;

--Última modificación 14022014
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_importe_abono       DECIMAL(22,2);  --Importe Abono
DEFINE v_importe_cargo       DECIMAL(22,2);  --Importe Cargo
DEFINE v_dif_importe         DECIMAL(22,2);  --Importe diferencia

DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso

DEFINE v_proceso_cod         SMALLINT;       --Código del Proceso

  --Inicialización de variables
  LET v_importe_abono       = 0.0;
  LET v_importe_cargo       = 0.0;
  LET v_dif_importe         = 0.0;

  LET v_bnd_proceso         = 0; --Estado correcto

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_revisa_reg_cnt.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;

  SELECT proceso_cod
  INTO   v_proceso_cod
  FROM   glo_folio
  WHERE  folio = p_folio_liquida;
  IF v_proceso_cod <> 2317 THEN 

  -- Obtiene los montos de abono y cargo del registro contable 
  SELECT SUM(importe)
  INTO   v_importe_abono
  FROM   cnt_transaccion
  WHERE  folio_liquida      = p_folio_liquida
  AND    f_liquida          = p_f_liquida
  AND    tpo_transaccion    = p_tpo_reg_cnt
  AND    cod_naturaleza_cta = 1;
  IF DBINFO('sqlca.sqlerrd2') == 0    OR 
     v_importe_abono          IS NULL THEN
     LET v_bnd_proceso = 1; -- ERROR:No existe monto abono registro contable
  ELSE
    SELECT sum(importe) cargo
    INTO   v_importe_cargo
    FROM   cnt_transaccion
    WHERE  folio_liquida      = p_folio_liquida
    AND    f_liquida          = p_f_liquida
    AND    tpo_transaccion    = p_tpo_reg_cnt
    AND    cod_naturaleza_cta = 2;
    IF DBINFO('sqlca.sqlerrd2') == 0    OR 
       v_importe_cargo          IS NULL THEN
       LET v_bnd_proceso = 2; -- ERROR:No existe monto cargo registro contable
    ELSE
      --Obtiene diferencia entre los montos de abono y cargo
      LET v_dif_importe = v_importe_abono - v_importe_cargo;

      IF v_dif_importe <> 0 THEN
         LET v_bnd_proceso = 3; --ERROR:Diferencia importes abono - cargo
      ELSE
         LET v_bnd_proceso = 0; --Sin diferencia en montos
      END IF
    END IF
  END IF

  IF v_bnd_proceso <> 0 THEN
     UPDATE cnt_transaccion
     SET    estado          = 100
     WHERE  folio_liquida   = p_folio_liquida
     AND    f_liquida       = p_f_liquida
     AND    tpo_transaccion = p_tpo_reg_cnt;
  END IF

  ELSE
    LET v_bnd_proceso = 0; --Sin diferencia en montos
  END IF

  RETURN v_bnd_proceso;

END PROCEDURE;


