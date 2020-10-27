






CREATE FUNCTION "safreviv".fn_cnt_rev_rop(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                p_cod_ejecuta        SMALLINT)      --Código de validación/ejecución - 0/1 respectivamente
RETURNING SMALLINT;

--Declaración de variables

DEFINE val_f_emision         DATE;           --Fecha emisión a validar
DEFINE val_f_liquida         DATE;           --Fecha Liquidación proceso
DEFINE val_cod_proceso_cnt   SMALLINT;       --Código proceso contable
DEFINE val_folio_cnt         DECIMAL(9,0);   --Folio contable 
DEFINE val_estado            SMALLINT;       --Estado registro contable
DEFINE v_hora                DATETIME HOUR TO SECOND;        --Hora de registro
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_tot_registros       SMALLINT;       --Total de registros
DEFINE v_tipo_reverso        SMALLINT;       --Identifica el tipo de reverso
DEFINE v_cod_proceso		 SMALLINT;

DEFINE v_folio_referencia	 DECIMAL(9,0);   --Folio de referencia de glo_folio

  --Inicialización de variables
  
  LET val_f_emision         = TODAY;
  LET val_f_liquida         = TODAY;
  LET val_cod_proceso_cnt   = 0;
  LET val_folio_cnt         = 0;
  LET val_estado            = 0;
  LET v_bnd_proceso         = 0; --Estado correcto
  LET v_tot_registros       = 0;
  LET v_tipo_reverso 	    = 0;
  LET v_folio_referencia    = 0;
  LET v_cod_proceso 		= 0;

  --Obtiene folio de referencia
  SELECT folio_referencia
  INTO 	 v_folio_referencia
  FROM   glo_folio
  WHERE  folio = p_folio_liquida;
  
  --Valida que exista el folio de referencia
  IF v_folio_referencia IS NULL THEN
     LET v_bnd_proceso = 1; --Error: El folio de referencia no existe
  ELSE
  
      IF p_cod_ejecuta = 0 THEN  --Solo verifica que se pueda realizar el reverso
	     --Verifica que existan registros con estado 30 y tpo_transaccion = 1 (reverso operativo)
		 
		 SELECT COUNT(*)
		 INTO   v_tot_registros
		 FROM   cnt_transaccion
		 WHERE  folio_liquida = v_folio_referencia
		 AND    tpo_transaccion = 1
		 AND    estado <> 30;  --Que no haya sido ya contabilizada
		 
		 IF DBINFO('sqlca.sqlerrd2') == 0 THEN
			LET v_tot_registros = 0;
			LET v_bnd_proceso = 2; --No existen registros contables o la póliza ya fue contabilizada
		 END IF
		 
		 IF v_tot_registros <> 0 THEN
			LET v_bnd_proceso = 0; --Sí se puede proceder a realizar el reverso
		 ELSE 
			LET v_bnd_proceso = 2;
		 END IF 
		 
		 
	  END IF 
	  
	  IF p_cod_ejecuta = 1 THEN  --Realiza el reverso
	  
	  --Verifica que existan registros a reversar
	     SELECT COUNT(*)
		 INTO   v_tot_registros
		 FROM   cnt_transaccion
		 WHERE  folio_liquida = v_folio_referencia
		 AND    tpo_transaccion = 1
		 AND    estado <> 30;  --Que no haya sido ya contabilizada
		 
		 IF DBINFO('sqlca.sqlerrd2') == 0 THEN
			LET v_tot_registros = 0;
			LET v_bnd_proceso = 2; --No existen registros contables o la póliza ya fue contabilizada
		 END IF
		 
		 IF v_tot_registros <> 0 THEN
		 
		  --Obtiene el codigo de proceso antes de eliminar
		  SELECT cod_proceso , cod_proceso_cnt, estado, f_liquida, f_emision
		  INTO v_cod_proceso, val_cod_proceso_cnt, val_estado, val_f_liquida, val_f_emision
		  FROM cnt_transaccion
		  WHERE folio_liquida = v_folio_referencia
		  AND    tpo_transaccion = 1
		  AND    estado <> 30
		  GROUP BY 1,2,3,4,5;
		   
		   --Elimina los registros contables
		   DELETE
           FROM   cnt_transaccion
           WHERE  folio_liquida = v_folio_referencia
           AND    cod_proceso   = v_cod_proceso
		   AND    tpo_transaccion = 1
		   AND    estado <> 30;
		   
		   LET v_bnd_proceso  = 0; --Se realizó correctamente el reverso
		   LET v_hora         = CURRENT HOUR TO SECOND;
		   LET v_tipo_reverso = 8;
		   
		   --Inserta en la bitácora de reversos
		   INSERT INTO cnt_ctr_reverso VALUES(v_tipo_reverso,
                                              val_folio_cnt,
                                              v_folio_referencia,
                                              val_cod_proceso_cnt,
                                              v_cod_proceso,
                                              val_f_liquida,
                                              val_f_emision,
                                              val_estado,
                                              USER,
                                              TODAY,
                                              v_hora);
		 ELSE
			LET v_bnd_proceso = 2; --No existen registros contables o la póliza ya fue contabilizada
		 END IF 
	  
	  END IF 
  END IF

  RETURN v_bnd_proceso;

END FUNCTION;


