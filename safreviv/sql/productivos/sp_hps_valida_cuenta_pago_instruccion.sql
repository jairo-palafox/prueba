






CREATE PROCEDURE "safreviv".sp_hps_valida_cuenta_pago_instruccion(p_nss                     CHAR(11),
                                                       p_id_derechohabiente      DECIMAL(9,0),
                                                       p_subcuenta               SMALLINT,
                                                       p_cve_mandato             CHAR(18),
                                                       p_valor_descuento_mdt     DECIMAL(22,2),
                                                       p_f_primer_pago_predial   DATE,
                                                       p_mto_primer_pago_predial DECIMAL(22,2),
                                                       p_f_primer_pago_cc        DATE,
                                                       p_mto_primer_pago_cc      DECIMAL(22,2)) -- Cuota conservación
RETURNING INTEGER,
          INTEGER,
          VARCHAR(200),
          SMALLINT;

DEFINE v_sql_error         INTEGER;
DEFINE v_isam_error        SMALLINT;
DEFINE v_msg_error         VARCHAR(200);
DEFINE v_existe_reg        SMALLINT;
DEFINE v_fecha_actual      DATE;
DEFINE v_monto_liquidar    DECIMAL(9,0);
DEFINE v_movimiento_valido SMALLINT;
DEFINE v_resultado         SMALLINT;
DEFINE v_saldo_aivs        DECIMAL(9,0);
DEFINE v_saldo_pesos       DECIMAL(9,0);
DEFINE v_subcuenta_predial SMALLINT;
DEFINE v_subcuenta_cc      SMALLINT;

   ON EXCEPTION SET v_sql_error,
                    v_isam_error,
                    v_msg_error

      RETURN v_sql_error,
             v_isam_error,
             v_msg_error,
             v_movimiento_valido;

   END EXCEPTION WITH RESUME
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_hps_valida_cuenta_pago_instruccion.trace';
   SET DEBUG FILE TO '/safreviv_int/BD/sp_hps_valida_cuenta_pago_instruccion.trace';
   TRACE ON;
   LET v_monto_liquidar    = 0;
   LET v_movimiento_valido = 0; -- 0 = no valido, 1 = valido
   LET v_sql_error         = 0;
   LET v_isam_error        = 0;
   LET v_msg_error         = "";
   LET v_fecha_actual      = TODAY;
   LET v_subcuenta_predial = 51;
   LET v_subcuenta_cc      = 53;

   -- Verifica si ya existe el primer pago de predial
   IF( p_f_primer_pago_predial <= v_fecha_actual AND p_mto_primer_pago_predial <> 0 )THEN
      LET v_existe_reg = 0;
      -- Busca el primer pago predial
      SELECT FIRST 1 NVL(1,0)
        INTO v_existe_reg
        FROM hps_det_aplica_pago_servicio
       WHERE clave_convenio = p_cve_mandato
         AND valor_descuento = p_mto_primer_pago_predial;

      IF( v_existe_reg = 0 OR v_existe_reg IS NULL )THEN
         IF( p_subcuenta = v_subcuenta_predial )THEN
            -- Si es la misma subcuenta, agrega el monto para validar con la cuota
            LET v_monto_liquidar = p_mto_primer_pago_predial;
         ELSE
            -- Valida si se puede aplicar el primer pago de predial en la subcuenta de predial
            EXECUTE FUNCTION fn_saldo_dia(p_nss,
                                          p_id_derechohabiente,
                                          v_subcuenta_predial,
                                          TODAY)
                                     INTO v_resultado,
                                          v_saldo_aivs,
                                          v_saldo_pesos;
            IF( p_mto_primer_pago_predial > v_saldo_pesos)THEN
               LET v_movimiento_valido = 0;
               RETURN v_sql_error,
                      v_isam_error,
                      v_msg_error,
                      v_movimiento_valido;
            END IF
         END IF
      END IF
   END IF

   -- Verifica si ya existe el primer pago de cuota de conservación
   IF( p_f_primer_pago_cc <= v_fecha_actual AND p_mto_primer_pago_cc <> 0 )THEN
      LET v_existe_reg = 0;
      -- Busca primer pago de cuota conservación
      SELECT FIRST 1 NVL(1,0)
        INTO v_existe_reg
        FROM hps_det_aplica_pago_servicio
       WHERE clave_convenio = p_cve_mandato
         AND valor_descuento = p_mto_primer_pago_cc;

      IF( v_existe_reg = 0 OR v_existe_reg IS NULL )THEN

         IF( p_subcuenta = v_subcuenta_cc )THEN
            -- Si es la misma subcuenta, agrega el monto para validar con la cuota
            LET v_monto_liquidar = v_monto_liquidar + p_mto_primer_pago_cc;
         ELSE
            -- Valida si se puede aplicar el primer pago de CC en la subcuenta de CC
            EXECUTE FUNCTION fn_saldo_dia(p_nss,
                                          p_id_derechohabiente,
                                          v_subcuenta_cc,
                                          TODAY)
                                     INTO v_resultado,
                                          v_saldo_aivs,
                                          v_saldo_pesos;
            IF( p_mto_primer_pago_cc > v_saldo_pesos )THEN
               LET v_movimiento_valido = 0;
               RETURN v_sql_error,
                      v_isam_error,
                      v_msg_error,
                      v_movimiento_valido;
            END IF
         END IF
      END IF
   END IF

   -- Si un primer pago corresponde a la misma cuenta de la cuota se suma para validar en la misma subcuenta
   LET v_monto_liquidar = v_monto_liquidar + p_valor_descuento_mdt;
   -- Recupera los saldos actuales del nss, según la subcuenta
   EXECUTE FUNCTION fn_saldo_dia(p_nss,
                                 p_id_derechohabiente,
                                 p_subcuenta,
                                 TODAY)
                            INTO v_resultado,
                                 v_saldo_aivs,
                                 v_saldo_pesos;
   -- Si el monto a descontar es menor o igual al saldo, es valido
   IF( v_monto_liquidar <= v_saldo_pesos )THEN
      LET v_movimiento_valido = 1;
   ELSE
      LET v_movimiento_valido = 0;
   END IF

   RETURN v_sql_error,
          v_isam_error,
          v_msg_error,
          v_movimiento_valido;
END PROCEDURE;


