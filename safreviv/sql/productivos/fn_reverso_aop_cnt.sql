






CREATE PROCEDURE "safreviv".fn_reverso_aop_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                    p_cod_proceso        SMALLINT,      --Código Proceso
                                    p_tpo_proceso        SMALLINT)
RETURNING SMALLINT, VARCHAR(60);

--Última modificación 14022014
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_folio_cnt           DECIMAL(9,0);   --Folio contable
DEFINE v_cod_proceso         SMALLINT;       --Código proceso
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
DEFINE v_desc_bnd_proceso    VARCHAR(60);    --Descripción estatus del proceso

  --Inicialización de variables
  LET v_id_cuenta_contable  = 0;
  LET v_folio_cnt           = 0;
  LET v_cod_proceso_cnt     = 0;
  LET v_cod_proceso         = 0;
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
  LET v_desc_bnd_proceso    = NULL;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reverso_aop_cnt.trace';
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
     LET v_bnd_proceso      = 1; -- ERROR:No existe el registro contable del proceso
     LET v_desc_bnd_proceso = 'No existe el registro contable del proceso';

     IF p_cod_proceso = 2317 THEN
        LET v_bnd_proceso      = 0;
        LET v_desc_bnd_proceso = 'Reverso operativo UNIFICACIÓN CUENTAS COMPL. INFONAVIT';
     END IF
  ELSE
     IF val_f_emision <> v_f_hoy THEN
        SELECT COUNT(*)
        INTO   v_tot_registros
        FROM   safre_viv:cnt_transaccion
        WHERE  estado          = 30
        AND    cod_proceso     = p_cod_proceso
        AND    folio_liquida   = p_folio_liquida
        AND    tpo_transaccion = 0;
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_tot_registros = 0;
        END IF

        IF v_tot_registros <> 0 THEN
           IF p_tpo_proceso = 1 THEN
              FOREACH
                --Extrae información de la poliza contable para insertar la cuenta inversa
                --para el reverso
                SELECT id_cuenta_contable, folio_cnt, cod_proceso_cnt,
                       cod_transaccion_cnt, cod_subcta_cnt, cta_contable,
                       cod_naturaleza_cta, importe, f_liquida,
                       f_emision, estado
                INTO   v_id_cuenta_contable, v_folio_cnt, v_cod_proceso_cnt,
                       v_cod_transaccion_cnt, v_cod_subcta_cnt, v_cta_contable,
                       v_cod_naturaleza_cta, v_monto_pesos, v_f_liquida,
                       v_f_emision, v_estado
                FROM   safre_viv:cnt_transaccion
                WHERE  cod_proceso     = p_cod_proceso
                AND    folio_liquida   = p_folio_liquida
                AND    tpo_transaccion = 0
             
                IF v_cod_naturaleza_cta = 1 THEN
                   LET v_cod_naturaleza_cta = 2;
                ELSE
                   LET v_cod_naturaleza_cta = 1;
                END IF
             
                --Insertar las cuentas contables en la tabla de transacciones
                --TRACE 'Inserta en transaccion';
                INSERT INTO safre_viv:cnt_transaccion VALUES(v_id_cuenta_contable,
                                                             0,
                                                             v_cod_proceso_cnt,
                                                             p_cod_proceso,
                                                             v_cod_transaccion_cnt,
                                                             v_cod_subcta_cnt,
                                                             v_cta_contable,
                                                             v_cod_naturaleza_cta,
                                                             p_folio_liquida,
                                                             v_monto_pesos,
                                                             TODAY,
                                                             TODAY,
                                                             1,    -- 0>Registro Contable, 1>Reverso
                                                             10);
    
              END FOREACH;

              LET v_bnd_proceso      = 0; -- Reverso operativo aplicado 
              LET v_desc_bnd_proceso = 'Reverso operativo aplicado';
              LET v_hora             = CURRENT HOUR TO SECOND;

	      LET v_tipo_reverso = 5;
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
           END IF
        ELSE 
           LET v_bnd_proceso      = 5; -- ERROR:La póliza contable no fue confirmada
           LET v_desc_bnd_proceso = 'La póliza contable no fue confirmada';
        END IF 
     ELSE
        LET v_bnd_proceso      = 6; -- ERROR:El registro de la póliza contable es del día de hoy
        LET v_desc_bnd_proceso = 'El registro de la póliza contable es del día de hoy';
     END IF
  END IF

  RETURN v_bnd_proceso, v_desc_bnd_proceso;

END PROCEDURE;


