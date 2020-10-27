






CREATE FUNCTION "safreviv".fn_dpe_preliq_infonavit(p_folio       DECIMAL(10), 
                                        p_usuario_cod CHAR(20), 
                                        p_pid         DECIMAL(9,0)) 
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
DEFINE v_id_dpe_referencia         DECIMAL(9,0);
DEFINE v_folio                     DECIMAL(9,0);
DEFINE v_id_derechohabiente        DECIMAL(9,0);
DEFINE v_aportacion_sol            DECIMAL(16,6);
DEFINE v_amortizacion_sol          DECIMAL(16,6);
DEFINE v_nrp                       CHAR(11);
DEFINE v_periodo_pago              CHAR(6);
DEFINE v_estado_solicitud          SMALLINT;
 -- variables de soporte al proceso
DEFINE v_i_resultado               INTEGER; -- resultado de la operacion
 -- subcuenta                      
DEFINE  v_subcuenta_4              SMALLINT;
DEFINE  v_subcuenta_41             SMALLINT;
DEFINE  v_subcuenta_44             SMALLINT;
DEFINE  v_subcuenta_43             SMALLINT;
                                   
DEFINE v_fondo_inversion           SMALLINT; -- fondo temporal
DEFINE v_fondo_inversion_sief10    SMALLINT; -- fonde de inversión SIEFORE 10 
DEFINE v_movimiento_cargo          SMALLINT; -- clave de movimiento cargo
 --                                
DEFINE v_i_registros_insertados    INTEGER;
DEFINE v_si_estado                 SMALLINT;
DEFINE v_c_cadena                  CHAR(200);
DEFINE g_proceso_cod               SMALLINT; -- codigo del proceso
DEFINE g_opera_cod                 SMALLINT; -- codigo de operacion    
DEFINE v_precio_fondo              DECIMAL(16,6);

    ON EXCEPTION IN (-206)
      LET v_i_resultado = -206;
      LET v_i_registros_insertados = 0;
      
      RETURN v_i_resultado, 
             v_i_registros_insertados, 0;
    END EXCEPTION
    
    --SET DEBUG FILE TO "/home/safreviv/trace.dpe.preliquida.INFONAVIT.txt";
    --TRACE ON;
    
    LET g_proceso_cod = 1005;
    LET g_opera_cod   = 2;
    LET v_i_resultado = 0;
    LET v_i_registros_insertados = 0;
    -- cuando es derechohabiente I
    LET v_subcuenta_4 = 4;
    LET v_subcuenta_41 = 41;
    -- cuando es derechohabiente S
    LET v_subcuenta_44 = 44;
    LET v_subcuenta_43 = 43;
    
    LET v_fondo_inversion = 11;
    LET v_fondo_inversion_sief10 = 10;
    LET v_movimiento_cargo = 512; -- Movimiento cargo DPE Créditos    
    LET v_precio_fondo = 0;
   
    -- se obtienen todos los registros de la tabla dpe_sol_soloinfonavit
   FOREACH
      SELECT id_dpe_referencia,
             folio,
             id_derechohabiente,
             aportacion_sol,
             amortizacion_sol,
             nrp,
             periodo_pago,
             estado_solicitud
        INTO v_id_dpe_referencia,
             v_folio,
             v_id_derechohabiente,
             v_aportacion_sol,
             v_amortizacion_sol,
             v_nrp,
             v_periodo_pago,
             v_estado_solicitud
        FROM dpe_sol_soloinfonavit
       WHERE estado_solicitud = 1  -- solo solicitudes aceptadas
         AND folio = p_folio
                 
        -- Se valida que sea derechohabiente solo imss
           IF v_aportacion_sol > 0 THEN
           
              SELECT precio_fondo
              INTO   v_precio_fondo
              FROM   glo_valor_fondo
              WHERE  fondo = v_fondo_inversion
              AND    f_valuacion = TODAY;
              
           	  LET v_preliq_f_liquida          = TODAY;
              LET v_preliq_id_derechohabiente = v_id_derechohabiente;
              LET v_preliq_subcuenta          = v_subcuenta_44;
              LET v_preliq_fondo_inversion    = v_fondo_inversion;
              LET v_preliq_movimiento         = v_movimiento_cargo; -- Movimiento de cargo
              LET v_preliq_folio_liquida      = p_folio;
              LET v_preliq_id_referencia      = v_id_dpe_referencia;
              LET v_preliq_monto_acciones     = (v_aportacion_sol / v_precio_fondo)* -1; --cargo
              LET v_preliq_monto_pesos        = v_aportacion_sol * -1;
              LET v_preliq_f_valor            = TODAY;
              LET v_preliq_f_registro         = TODAY;
              LET v_preliq_h_registro         = CURRENT HOUR TO SECOND;
              LET v_origen_preliquida         = v_periodo_pago||'-'||v_nrp;
              
              INSERT INTO dpe_preliquida(f_liquida,
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

      -- Actualiza total de registros insertados mayor a cero
      LET v_i_registros_insertados = v_i_registros_insertados + 1;
      
      -- Actualiza el estado de la solicitud a 3 preliquidado
      UPDATE dpe_sol_soloinfonavit
         SET estado_solicitud = 3 -- Preliquidado
       WHERE estado_solicitud = 1 -- Aceptadas
         AND folio = p_folio;
   
   END FOREACH;

   -- se actualiza glo_folio
      UPDATE glo_folio
         SET status      = 1
        WHERE folio      = p_folio
         AND opera_cod   = g_opera_cod;
   
   -- Se actualiza el folio referencia 
   UPDATE glo_folio
     SET folio_referencia = p_folio
    WHERE folio = p_folio
      AND opera_cod = g_opera_cod;
               
   update statistics for table dpe_sol_soloinfonavit;
   
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,v_i_registros_insertados,p_folio;
END FUNCTION;


