






CREATE PROCEDURE "safreviv".fn_val_tia_cnt6(p_folio_liquida       DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                 p_f_liquida           DATE,          --Fecha de liquidaci�n del proceso
                                 p_cod_proceso_cnt     SMALLINT,      --C�digo Proceso contable
                                 p_cod_proceso         SMALLINT,      --C�digo Proceso
                                 p_transaccion         SMALLINT)      --C�digo Transacci�n contable
RETURNING SMALLINT;

--�ltima modificaci�n 24012017
--Declaraci�n de variables
DEFINE v_bnd_proceso         SMALLINT;  --Estatus del proceso
DEFINE v_tot_registros       INTEGER;   --Total de registros

--Inicializaci�n de Variables
LET v_bnd_proceso   = 0; --Estado correcto - La p�liza no se ha generado para este proceso
LET v_tot_registros = 0;
  

  --Valida si existen registros contables cuando la p�liza a�n no ha sido generada
  SELECT COUNT(*)
  INTO   v_tot_registros
  FROM   safre_viv:cnt_transaccion
  WHERE  folio_liquida = p_folio_liquida
  AND    cod_proceso   = p_cod_proceso
  AND    estado        = 10; --Registrado - La p�liza a�n no se ha generado
  IF DBINFO('sqlca.sqlerrd2') = 0 THEN
     LET v_tot_registros = 0;
  END IF

  IF v_tot_registros <> 0 THEN
     LET v_bnd_proceso = 0; -- La p�liza no se ha generado a�n

     DELETE
     FROM   safre_viv:cnt_transaccion
     WHERE  folio_liquida = p_folio_liquida
     AND    cod_proceso   = p_cod_proceso
     AND    estado        = 10; --Registrado - La p�liza a�n no se ha generado
  ELSE
     LET v_bnd_proceso = 1; -- ERROR:La p�liza contable ya fue generada
     
  END IF
  
  RETURN v_bnd_proceso;

END PROCEDURE;


