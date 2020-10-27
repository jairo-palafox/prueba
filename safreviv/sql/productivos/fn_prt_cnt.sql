






CREATE PROCEDURE "safreviv".fn_prt_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
						    p_f_liquida          DATE,          --Fecha de liquidación del proceso
						    p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
						    p_cod_proceso        SMALLINT,      --Código Proceso
						    p_transaccion        SMALLINT)      --Código Transaccion contable 
RETURNING SMALLINT; ---, SMALLINT, CHAR(70);

--Última modificación 06082015
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(70);


ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN v_status;  --,isam_err , error_info;  
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso         = 1; --Estado correcto
LET v_id_cuenta_contable  = 0;
LET v_cod_transaccion_cnt = 0;
LET v_cod_subcta_cnt      = 0;
LET v_cta_contable        = '0000000000';
LET v_cod_naturaleza_cta  = 0;
LET v_monto_pesos         = 0;

--SET DEBUG FILE TO '/ds/safreviv_int/dis/fn_prt_cnt.trace';
--TRACE 'Folio Liquida '||p_folio_liquida;
--TRACE 'Fecha Liquida '||p_f_liquida;
--TRACE 'Proceso Contable '||p_cod_proceso_cnt;
--TRACE 'Proceso '||p_cod_proceso;
--TRACE 'Transaccion Contable '||p_transaccion;

 FOREACH
   --Extrae información de la cuenta contable para el proceso que lo solicita
   SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
          cta_contable, cod_naturaleza_cta
   INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
          v_cta_contable, v_cod_naturaleza_cta
   FROM   safre_viv:cnt_regla_contable
   WHERE  cta_contable        <> '0000000000'
   AND    cod_proceso          = p_cod_proceso
   AND    cod_proceso_cnt      = p_cod_proceso_cnt

   SELECT SUM(monto_pesos)
   INTO   v_monto_pesos
   FROM   safre_viv:cta_movimiento
   WHERE  f_liquida     = p_f_liquida
   AND    subcuenta     = v_cod_subcta_cnt
   AND    folio_liquida = p_folio_liquida;
   IF DBINFO('sqlca.sqlerrd2') == 0 THEN
      LET v_monto_pesos  = 0;
   END IF

   IF p_cod_proceso_cnt = 88 THEN
      IF v_cta_contable = '2203120130' THEN
         SELECT SUM(monto_pesos)
         INTO   v_monto_pesos
         FROM   safre_viv:cta_movimiento
         WHERE  f_liquida     = p_f_liquida
         AND    subcuenta     = v_cod_subcta_cnt
         AND    movimiento    = 1610
         AND    folio_liquida = p_folio_liquida;
         IF DBINFO('sqlca.sqlerrd2') == 0 THEN
            LET v_monto_pesos  = 0;
         END IF
      END IF

      IF v_cta_contable = '2203220001' THEN
         SELECT SUM(monto_pesos)
         INTO   v_monto_pesos
         FROM   safre_viv:cta_movimiento
         WHERE  f_liquida     = p_f_liquida
         AND    subcuenta     = v_cod_subcta_cnt
         AND    movimiento    = 1611
         AND    folio_liquida = p_folio_liquida;
         IF DBINFO('sqlca.sqlerrd2') == 0 THEN
            LET v_monto_pesos  = 0;
         END IF
      END IF
   END IF

   -- si el monto es negativo, se obtiene el valor absoluto
   IF ( v_monto_pesos < 0 ) THEN
        LET v_monto_pesos = v_monto_pesos * (-1);
   END IF

   --Insertar las cuentas contables en la tabla de transacciones
   IF v_monto_pesos > 0 THEN
      --TRACE 'Inserta en transaccion';
      INSERT INTO safre_viv:cnt_transaccion VALUES(v_id_cuenta_contable,
                                                   0,
                                                   p_cod_proceso_cnt,
                                                   p_cod_proceso,
                                                   v_cod_transaccion_cnt,
                                                   v_cod_subcta_cnt,
                                                   v_cta_contable,
                                                   v_cod_naturaleza_cta,
                                                   p_folio_liquida,
                                                   v_monto_pesos,
                                                   p_f_liquida,
                                                   TODAY,
                                                   0,   -- 0>Registro Contable, 1>Reverso
                                                   10);
           
      LET v_monto_pesos = 0;
   END IF
 END FOREACH;

 --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

 LET v_char = "Finaliza fn_prt_cnt correctamente: "|| p_folio_liquida ;
 RETURN v_bnd_proceso;  ---, 0 , v_char;
 
END PROCEDURE;


