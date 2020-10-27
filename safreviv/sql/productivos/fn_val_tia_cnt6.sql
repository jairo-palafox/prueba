






CREATE PROCEDURE "safreviv".fn_val_tia_cnt6(p_folio_liquida       DECIMAL(9,0),  --Folio de liquidación del proceso
                                 p_f_liquida           DATE,          --Fecha de liquidación del proceso
                                 p_cod_proceso_cnt     SMALLINT,      --Código Proceso contable
                                 p_cod_proceso         SMALLINT,      --Código Proceso
                                 p_transaccion         SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 24012017
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;  --Estatus del proceso
DEFINE v_tot_registros       INTEGER;   --Total de registros

--Inicialización de Variables
LET v_bnd_proceso   = 0; --Estado correcto - La póliza no se ha generado para este proceso
LET v_tot_registros = 0;
  

  --Valida si existen registros contables cuando la póliza aún no ha sido generada
  SELECT COUNT(*)
  INTO   v_tot_registros
  FROM   safre_viv:cnt_transaccion
  WHERE  folio_liquida = p_folio_liquida
  AND    cod_proceso   = p_cod_proceso
  AND    estado        = 10; --Registrado - La póliza aún no se ha generado
  IF DBINFO('sqlca.sqlerrd2') = 0 THEN
     LET v_tot_registros = 0;
  END IF

  IF v_tot_registros <> 0 THEN
     LET v_bnd_proceso = 0; -- La póliza no se ha generado aún

     DELETE
     FROM   safre_viv:cnt_transaccion
     WHERE  folio_liquida = p_folio_liquida
     AND    cod_proceso   = p_cod_proceso
     AND    estado        = 10; --Registrado - La póliza aún no se ha generado
  ELSE
     LET v_bnd_proceso = 1; -- ERROR:La póliza contable ya fue generada
     
  END IF
  
  RETURN v_bnd_proceso;

END PROCEDURE;


