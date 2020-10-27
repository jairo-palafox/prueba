






CREATE FUNCTION "safreviv".fn_dae_preliquida_dictamen(p_folio       DECIMAL(10), 
                                  p_usuario_cod CHAR(20), 
                                  p_pid         DECIMAL(9,0),
                                  v_ejecuta     SMALLINT,
                                  p_folio_dictamen DECIMAL(9,0)) 
   RETURNING INTEGER, 
             INTEGER, 
             DECIMAL

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

--tabla_det_solicitud
DEFINE v_id_dae_referencia         DECIMAL(9,0);
DEFINE v_id_derechohabiente        DECIMAL(9,0);
DEFINE v_folio                     DECIMAL(9,0);
DEFINE v_periodo_pago              CHAR(4);
DEFINE v_importe_amort             DECIMAL(16,6);
DEFINE v_importe_aport             DECIMAL(16,6);
DEFINE v_resul_opera               CHAR(2);
DEFINE v_num_credito               CHAR(10);
                                   
 -- variables de soporte al proceso
DEFINE v_i_resultado               INTEGER; -- resultado de la operacion
 -- subcuenta                      
DEFINE  v_subcuenta_46             SMALLINT;
                                   
DEFINE v_fondo_inversion           SMALLINT; -- fondo temporal
DEFINE v_movimiento_cargo          SMALLINT; -- clave de movimiento cargo
 --                                
DEFINE v_i_registros_insertados    INTEGER;
DEFINE v_si_estado                 SMALLINT;
DEFINE v_c_cadena                  CHAR(200);
DEFINE g_proceso_cod               SMALLINT; -- codigo del proceso
DEFINE g_opera_cod                 SMALLINT; -- codigo de operacion    
DEFINE v_precio_fondo              DECIMAL(16,6);   
DEFINE v_movimiento_abono_dis      SMALLINT;
DEFINE v_movimiento_abono_car      SMALLINT;
DEFINE v_folio_liquida             DECIMAL(9,0);
DEFINE v_fecha_pago                DATE;


    ON EXCEPTION IN (-206)
      LET v_i_resultado = -206;
      LET v_i_registros_insertados = 0;
      
      RETURN v_i_resultado, 
             v_i_registros_insertados, 0;
    END EXCEPTION
    
   --SET DEBUG FILE TO "/safreviv_int/BD/dae_preliquida.txt";
   --TRACE ON;

    --Movimientos
    --501	ABONO AMORTIZACIÓN EXCEDENTE DISPERSIÓN
    --511	ABONO AMORTIZACIÓN EXCEDENTE CARTERA
    
    --Subcuenta
    --46	AMORTIZACIÓN EXCEDENTE
    
    LET g_proceso_cod = 2402;
    LET g_opera_cod   = 2;

    LET v_i_resultado = 0;
    LET v_i_registros_insertados = 0;
    
    LET v_subcuenta_46 = 46;
    LET v_fondo_inversion = 11;
    LET v_movimiento_abono_dis = 501;
    LET v_movimiento_abono_car = 511;
    
    LET v_precio_fondo = 0;
    
   EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)             
   INTO v_folio_liquida;
   
    -- se obtienen todos los registros de la tabla dpe_sol_soloinfonavit
   FOREACH
      SELECT id_dae_referencia,
             folio,
             id_derechohabiente,
             total_importe,
             importe_amort,
             periodo_pago,
             resul_opera,
             fecha_pago, 
             num_credito
        INTO v_id_dae_referencia,              
             v_folio,             
             v_id_derechohabiente,
             v_importe_aport,     
             v_importe_amort,     
             v_periodo_pago,      
             v_resul_opera ,
             v_fecha_pago,
             v_num_credito
        FROM dae_det_solicitud
       WHERE resul_opera = "01"  -- solo solicitudes aceptadas
         AND folio_dictamen = p_folio_dictamen

           -- Se valida que sea derechohabiente solo imss
           IF v_importe_amort > 0 THEN
              
              SELECT precio_fondo
              INTO   v_precio_fondo
              FROM   glo_valor_fondo
              WHERE  fondo = v_fondo_inversion
              AND    f_valuacion = v_fecha_pago;
              
           	  LET v_preliq_f_liquida          = TODAY;
             -- LET v_preliq_id_derechohabiente = v_id_derechohabiente;
              LET v_preliq_subcuenta          = v_subcuenta_46;
              LET v_preliq_fondo_inversion    = v_fondo_inversion;
              LET v_preliq_movimiento         = v_movimiento_abono_car;
              LET v_preliq_folio_liquida      = v_folio_liquida;
              LET v_preliq_id_referencia      = v_id_dae_referencia;
              LET v_preliq_monto_acciones     = (v_importe_amort / v_precio_fondo);
              LET v_preliq_monto_pesos        = v_importe_amort;
              LET v_preliq_f_valor            = v_fecha_pago;
              LET v_preliq_f_registro         = TODAY;
              LET v_preliq_h_registro         = CURRENT HOUR TO SECOND;
              LET v_origen_preliquida         = v_num_credito||'-'||v_periodo_pago||'-'||"C";
              
              INSERT INTO dae_preliquida(f_liquida,
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
                     v_id_derechohabiente,
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

      -- Actualiza total de registros insertados mayor a cero
      LET v_i_registros_insertados = v_i_registros_insertados + 1;

   END FOREACH;

      -- Actualiza el estado de la solicitud a 3 preliquidado
      UPDATE dae_det_solicitud
      SET    estado = 3 -- Preliquidado
      WHERE  resul_opera = "01"
      AND    folio = p_folio
      AND    estado = 2; -- Integrado

   -- se actualiza glo_folio
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = v_folio_liquida
   AND    proceso_cod = g_proceso_cod;
   
   -- Se actualiza el folio referencia 
   UPDATE glo_folio
   SET    folio_referencia = p_folio_dictamen
   WHERE  folio = v_folio_liquida
   AND    proceso_cod = g_proceso_cod;

   UPDATE STATISTICS FOR TABLE dae_det_solicitud;
   
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,
        v_i_registros_insertados,
        v_folio_liquida;

END FUNCTION;


