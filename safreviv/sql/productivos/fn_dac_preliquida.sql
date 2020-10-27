






CREATE FUNCTION "safreviv".fn_dac_preliquida(
                                  p_folio_integra    DECIMAL(9,0),
                                  p_proceso_cod      SMALLINT,
                                  p_opera_cod        SMALLINT,  
                                  p_usuario_cod      CHAR(20), 
                                  p_pid              DECIMAL(9,0)
                                  ) 

RETURNING INTEGER, SMALLINT, DECIMAL

-- tabla de preliquidacion
DEFINE v_preliq_f_liquida          DATE;
DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);
DEFINE v_preliq_subcuenta          SMALLINT;
DEFINE v_preliq_fondo_inversion    SMALLINT;
DEFINE v_preliq_movimiento         SMALLINT;
DEFINE v_preliq_folio_liquida      DECIMAL(9,0);
DEFINE v_preliq_id_referencia      DECIMAL(9,0);
DEFINE v_preliq_monto_acciones     DECIMAL(16,6);
DEFINE v_preliq_monto_pesos        DECIMAL(16,6);
DEFINE v_preliq_f_valor            DATE;
DEFINE v_preliq_f_registro         DATE;
DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;
DEFINE v_origen_preliquida         CHAR(18);
-- detalle en dpe_sol_soloinfonavit
DEFINE v_id_dac_solicitud   DECIMAL(9,0);
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_folio_integracion  DECIMAL(9,0);
DEFINE v_num_credito        DECIMAL(10,0);
DEFINE v_nss                CHAR(11); 
DEFINE v_periodo_pago       DECIMAL(4);
DEFINE v_imp_amortizacion   DECIMAL(16,6); 
DEFINE v_folio_sua          DECIMAL(6,0);
DEFINE v_nrp                CHAR(11);
DEFINE v_id_sdd             CHAR(6); 
DEFINE v_resul_opera        SMALLINT;
DEFINE v_diagnostico        SMALLINT;
DEFINE v_folio_dispersion   DECIMAL(9,0);
DEFINE v_folio_liquidacion  DECIMAL(9,0);
 -- variables de soporte al proceso
DEFINE v_resultado          INTEGER; -- resultado de la operacion
 -- subcuenta                 
DEFINE  v_subcuenta_41      SMALLINT;
DEFINE v_fondo_inversion    SMALLINT; -- fondo temporal
DEFINE v_movimiento_cargo   SMALLINT; -- clave de movimiento cargo
 --                                
DEFINE v_regs_insertados    INTEGER;
DEFINE v_si_estado          SMALLINT;
DEFINE v_c_cadena           CHAR(200);
DEFINE g_proceso_cod        SMALLINT; -- codigo del proceso
DEFINE g_opera_cod          SMALLINT; -- codigo de operacion    
DEFINE v_precio_fondo       DECIMAL(16,6);          

DEFINE v_subcuenta          SMALLINT;     
DEFINE v_acciones           DECIMAL(16,6);
DEFINE v_pesos              DECIMAL(16,6);

    ON EXCEPTION IN (-206)
      LET v_resultado = -206;
      LET v_regs_insertados = 0;
      
      RETURN v_resultado, 
             v_regs_insertados, 0;
    END EXCEPTION
    
    --SET DEBUG FILE TO "/home/safreviv/trace_dac_preliquida.txt";
    --TRACE ON;

LET g_proceso_cod       = 2601;
LET g_opera_cod         = 3;
LET v_resultado         = 0;
LET v_regs_insertados   = 0;
LET v_subcuenta_41      = 41;
                        
LET v_fondo_inversion   = 11;
LET v_movimiento_cargo  = 1452; --CARGO AMORITZACIÓN MEJORANDO TU CASA
LET v_precio_fondo      = 0;
LET v_folio_liquidacion = p_folio_integra;
LET v_subcuenta         = 0;
LET v_acciones          = 0;        
LET v_pesos             = 0;  
  
   --EXECUTE FUNCTION fn_genera_folio(2601,3,p_usuario_cod)
   --INTO v_folio_liquidacion;
    -- se obtienen todos los registros de la tabla dpe_sol_soloinfonavit
   FOREACH
      SELECT id_dac_solicitud  ,
             id_derechohabiente,
             folio_integracion ,
             num_credito       ,
             nss               ,
             periodo_pago      ,
             imp_amortizacion  ,
             folio_sua         ,
             nrp               ,
             id_sdd            ,
             resul_opera       ,
             diagnostico       ,
             folio_dispersion
      INTO   v_id_dac_solicitud  ,
             v_id_derechohabiente,
             v_folio_integracion ,
             v_num_credito       ,
             v_nss               ,
             v_periodo_pago      ,
             v_imp_amortizacion  ,
             v_folio_sua         ,
             v_nrp               ,
             v_id_sdd            ,
             v_resul_opera       ,
             v_diagnostico       ,
             v_folio_dispersion        
      FROM   dac_det_solicitud         
      WHERE  resul_opera  = 1
      AND    folio_integracion = p_folio_integra

      IF v_imp_amortizacion > 0 THEN
         FOREACH 
            EXECUTE FUNCTION fn_saldo_actual(v_nss,41,TODAY)
            INTO v_subcuenta,
                 v_fondo_inversion,
                 v_acciones,
                 v_pesos
         END FOREACH;
      
         SELECT precio_fondo
         INTO   v_precio_fondo
         FROM   glo_valor_fondo
         WHERE  fondo = v_fondo_inversion
         AND    f_valuacion = TODAY;
         
         IF v_pesos > 0 THEN 
            --(Monto Menor) SDD solicita 5,000.10 SACI tiene registrado 4,900.80; SACI deberá enviar a SDD los 4,900.80
            IF v_imp_amortizacion > v_pesos THEN 
               LET v_imp_amortizacion  = v_pesos;
            END IF
            --(Monto Igual) SDD solicita 5,000.10 SACI tiene registrado 5,000.10; SACI deberá enviar a SDD los 5,000.10
            IF v_imp_amortizacion = v_pesos THEN 
               LET v_imp_amortizacion  = v_imp_amortizacion; 
            END IF 
            --(Monto Mayor) SDD solicita 5,000.10 SACI tiene registrado 6,020.50; SACI deberá enviar a SDD solo 5,000.10 
            IF v_imp_amortizacion < v_pesos THEN 
               LET v_imp_amortizacion  = v_imp_amortizacion ;
            END IF

      	    LET v_preliq_f_liquida          = TODAY;
            LET v_preliq_id_derechohabiente = v_id_derechohabiente;
            LET v_preliq_subcuenta          = v_subcuenta_41;
            LET v_preliq_fondo_inversion    = v_fondo_inversion;
            LET v_preliq_movimiento         = v_movimiento_cargo; -- Movimiento de cargo 1452
            LET v_preliq_folio_liquida      = p_folio_integra;
            LET v_preliq_id_referencia      = v_id_dac_solicitud;
            LET v_preliq_monto_acciones     = (v_imp_amortizacion / v_precio_fondo)* -1; --cargo
            LET v_preliq_monto_pesos        = v_imp_amortizacion * -1;
            LET v_preliq_f_valor            = TODAY;
            LET v_preliq_f_registro         = TODAY;
            LET v_preliq_h_registro         = CURRENT HOUR TO SECOND;
            LET v_origen_preliquida         = v_periodo_pago||'-'||v_nrp;
            
            INSERT INTO dac_preliquida(f_liquida,
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
                                       origen)
            VALUES(v_preliq_f_liquida,
                   v_preliq_id_derechohabiente,
                   v_preliq_subcuenta,
                   v_preliq_fondo_inversion,
                   v_preliq_movimiento,
                   v_preliq_folio_liquida,
                   v_preliq_id_referencia,
                   v_preliq_monto_acciones,
                   v_preliq_monto_pesos,
                   v_preliq_f_valor,
                   v_preliq_f_registro,
                   v_preliq_h_registro,
                   v_origen_preliquida);
         END IF
      END IF

      -- Actualiza total de registros insertados mayor a cero
      LET v_regs_insertados = v_regs_insertados + 1;
         
      -- Actualiza el estado de la solicitud a 3 preliquidado
      UPDATE dac_det_solicitud
      SET    diagnostico = 3 -- Preliquidado
      WHERE  resul_opera = 1    
      AND    diagnostico = 1 -- Aceptadas
      AND    folio_integracion = p_folio_integra;
   END FOREACH;

   
   -- se actualiza folio de liquidación 
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio  = p_folio_integra;

   UPDATE STATISTICS FOR TABLE dac_preliquida;
   
 -- se devuelve el resultado de la operacion
 RETURN v_resultado,v_regs_insertados,v_folio_liquidacion;
END FUNCTION;


