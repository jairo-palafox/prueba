






CREATE PROCEDURE "safreviv".fn_reverso_reg_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                    p_cod_proceso        SMALLINT)      --Código Proceso
RETURNING SMALLINT, SMALLINT;

--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_folio_cnt           DECIMAL(9,0);   --Folio contable
DEFINE v_cod_proceso_cnt     SMALLINT;       --Código proceso contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_f_liquida           DATE;           --Fecha liquidación proceso
DEFINE v_f_emision           DATE;           --Fecha emisión póliza contable

DEFINE v_estado              SMALLINT;       --Estado póliza contable
DEFINE val_f_emision         DATE;           --Fecha emisión a validar
DEFINE v_f_hoy               DATE;           --Fecha del día

DEFINE v_mes_curso           SMALLINT;       --Mes en curso
DEFINE v_mes_emision         SMALLINT;       --Mes en emision

DEFINE val_f_liquida         DATE;           --Fecha Liquidación proceso
DEFINE val_cod_proceso_cnt   SMALLINT;       --Código proceso contable
DEFINE val_folio_cnt         DECIMAL(9,0);   --Folio contable 
DEFINE val_estado            SMALLINT;       --Estado registro contable
DEFINE v_hora                DATETIME HOUR TO SECOND;        --Hora de registro

DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_tot_registros       SMALLINT;       --Total de registros
DEFINE v_tipo_reverso        SMALLINT;       --Identifica el tipo de reverso

  --Inicialización de variables
  LET v_id_cuenta_contable  = 0;
  LET v_folio_cnt           = 0;
  LET v_cod_proceso_cnt     = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_f_liquida           = NULL;
  LET v_f_emision           = NULL;
  LET v_estado              = 0;
  LET val_f_emision         = NULL;
  LET v_f_hoy               = TODAY;
  LET v_mes_curso           = MONTH(TODAY);
  LET val_f_liquida         = '';
  LET val_cod_proceso_cnt   = 0;
  LET val_folio_cnt         = 0;
  LET val_estado            = 0;

  LET v_bnd_inserta         = 0;
  LET v_bnd_proceso         = 0; --Estado correcto
  LET v_tot_registros       = 0;
  LET v_tipo_reverso 	    = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reverso_reg_cnt.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Proceso '||p_cod_proceso;

  -- Valida si el reverso se realiza posterior a la fecha de liquidación
  SELECT unique f_emision, 
         f_liquida, 
         folio_cnt,  
         cod_proceso_cnt,
         estado
  INTO   val_f_emision, 
         val_f_liquida, 
         val_folio_cnt,
         val_cod_proceso_cnt,
         val_estado
  FROM   safre_viv:cnt_transaccion
  WHERE  cod_proceso   = p_cod_proceso
  AND    folio_liquida = p_folio_liquida; 
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_bnd_proceso = 1; -- ERROR:No existe el registro contable del proceso
  ELSE
     IF val_f_emision <> v_f_hoy THEN
        LET v_bnd_proceso = 2; -- ERROR:La fecha de emisión es diferente al día de hoy
     ELSE
        SELECT COUNT(*)
        INTO   v_tot_registros
        FROM   safre_viv:cnt_transaccion
        WHERE  folio_cnt     = 0
        AND    cod_proceso   = p_cod_proceso
        AND    folio_liquida = p_folio_liquida;
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_tot_registros = 0;
        END IF
      
        IF v_tot_registros <> 0 THEN
           --Cuando no se ha generado la poliza contable se elimina
           --el registro contable
           DELETE
           FROM   safre_viv:cnt_transaccion
           WHERE  folio_cnt     = 0
           AND    cod_proceso   = p_cod_proceso
           AND    folio_liquida = p_folio_liquida;

           LET v_bnd_proceso  = 0; -- Reverso aplicado 
           LET v_hora         = CURRENT HOUR TO SECOND;

	   LET v_tipo_reverso = 1;
           INSERT INTO cnt_ctr_reverso VALUES(v_tipo_reverso,
                                              val_folio_cnt,
                                              p_folio_liquida,
                                              val_cod_proceso_cnt,
                                              p_cod_proceso,
                                              val_f_liquida,
                                              val_f_emision,
                                              val_estado,
                                              USER,
                                              TODAY,
                                              v_hora);
                                              
        ELSE
           LET v_bnd_proceso = 3; -- ERROR:La póliza contable ya fue generada
        END IF
     END IF
  END IF

  RETURN v_bnd_proceso, v_tipo_reverso;

END PROCEDURE;


