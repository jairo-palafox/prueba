






CREATE FUNCTION "safreviv".fn_dae_preliquida_ajuste(p_folio_lote  DECIMAL(10), 
                                         p_usuario_cod CHAR(20), 
                                         p_pid         DECIMAL(9,0))
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
DEFINE v_movimiento_cargo_ajuste   SMALLINT;
DEFINE v_folio_liquida             DECIMAL(9,0);
DEFINE v_fecha_pago                DATE;
--
DEFINE v_acep_id_dae_ref_aceptados DECIMAL(9,0) ;
DEFINE v_acep_id_dae_ref_ajuste    DECIMAL(9,0) ;
DEFINE v_acep_id_dae_referencia    DECIMAL(9,0) ;
DEFINE v_acep_folio_liquida        DECIMAL(9,0) ;
DEFINE v_acep_fecha_liquida        DATE         ;
DEFINE v_acep_monto_pesos          DECIMAL(16,6);
DEFINE v_acep_monto_acciones       DECIMAL(16,6);
DEFINE v_acep_fecha_valor          DATE;
DEFINE v_numero_credito            CHAR(10);
DEFINE v_bimestre                  CHAR(4);
DEFINE v_id_referenia_original     DECIMAL(9,0) ;
DEFINE v_folio_solicitud           DECIMAL(9,0) ; 

   ON EXCEPTION IN (-206)
     LET v_i_resultado = -206;
     LET v_i_registros_insertados = 0;
     
     RETURN v_i_resultado, 
            v_i_registros_insertados, 0;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/dae_preliquida_ajustes.trace";
   --TRACE ON;

   --Movimientos
   --1422 CARGO AJUSTE AMORTIZACIÓN EXCEDENTE
   
   --Subcuenta
   --46	AMORTIZACIÓN EXCEDENTE
   
   LET v_i_resultado = 0;
   LET v_i_registros_insertados = 0;
   
   LET v_subcuenta_46 = 46;
   LET v_fondo_inversion = 11;
   LET v_movimiento_cargo_ajuste = 1422;
   
   LET v_acep_id_dae_ref_aceptados = 0; 
   LET v_acep_id_dae_ref_ajuste    = 0;
   LET v_acep_id_dae_referencia    = 0;
   LET v_acep_folio_liquida        = 0;
   LET v_acep_fecha_liquida        = NULL;
   LET v_acep_monto_pesos          = 0;
   LET v_acep_monto_acciones       = 0;
   LET v_acep_fecha_valor          = NULL;
   LET g_proceso_cod               = 2403;
   LET g_opera_cod                 = 3;
   LET v_precio_fondo              = 0;
   LET v_numero_credito            = "";
   LET v_bimestre                  = "";
   LET v_folio_solicitud           = 0;

   EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)             
                    INTO v_folio_liquida;

    -- se obtienen todos los registros de la tabla dae_aceptados_ajuste
   FOREACH
      SELECT a.id_dae_ref_aceptados, 
             a.id_dae_ref_ajuste   ,
             a.id_dae_referencia   ,
             a.folio_liquida       ,
             a.fecha_liquida       ,
             a.monto_pesos         ,
             a.monto_acciones      ,
             a.fecha_valor         ,
             b.id_derechohabiente  ,
             c.num_credito         ,
             c.periodo_pago        , 
             a.id_dae_referencia   ,
             c.folio
      INTO   v_acep_id_dae_ref_aceptados , 
             v_acep_id_dae_ref_ajuste    , 
             v_acep_id_dae_referencia    , 
             v_acep_folio_liquida        , 
             v_acep_fecha_liquida        , 
             v_acep_monto_pesos          , 
             v_acep_monto_acciones       , 
             v_acep_fecha_valor          ,
             v_id_derechohabiente        ,
             v_numero_credito            ,
             v_bimestre                  ,
             v_id_referenia_original     ,
             v_folio_solicitud       
      FROM   dae_aceptados_ajuste a,
             dae_det_ajuste b,
             dae_det_solicitud c
      WHERE  a.id_dae_ref_ajuste = b.id_dae_ref_ajuste      
      AND    a.id_dae_referencia = c.id_dae_referencia
      AND    b.folio_lote        = a.folio_lote_ajuste
      AND    b.folio_lote        = p_folio_lote
      AND    b.resul_operacion   = 1
      AND    c.resul_opera       = "01"
      AND    c.folio_ajuste      = b.folio_lote
      AND    c.folio_liquida IS NOT NULL

      IF v_id_derechohabiente IS NOT NULL THEN       
         LET v_preliq_f_liquida          = TODAY;
         LET v_preliq_subcuenta          = v_subcuenta_46;
         LET v_preliq_fondo_inversion    = v_fondo_inversion;
         LET v_preliq_movimiento         = v_movimiento_cargo_ajuste;
         LET v_preliq_folio_liquida      = v_folio_liquida;
         LET v_preliq_id_referencia      = v_acep_id_dae_ref_aceptados;
         LET v_preliq_monto_acciones     = ( v_acep_monto_acciones * -1);
         LET v_preliq_monto_pesos        = ( v_acep_monto_pesos * - 1);
         LET v_preliq_f_valor            = v_acep_fecha_valor;
         LET v_preliq_f_registro         = TODAY;
         LET v_preliq_h_registro         = CURRENT HOUR TO SECOND;
         LET v_origen_preliquida         = v_numero_credito||'-'||v_bimestre||'-'||"C";

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

         -- Actualiza total de registros insertados mayor a cero
         LET v_i_registros_insertados = v_i_registros_insertados + 1;
   
         UPDATE dae_aceptados_ajuste       
         SET    folio_liquida = v_folio_liquida,
                fecha_liquida = TODAY
         WHERE  folio_liquida IS NULL
         AND    id_dae_ref_aceptados = v_acep_id_dae_ref_aceptados;
   
         --Se actualiza detalle a 2 CARGO. --AG 11/08/2014.
         UPDATE dae_det_solicitud
         SET    status_retiro = 2
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    id_dae_referencia  = v_id_referenia_original
         AND    folio              = v_folio_solicitud;
      END IF
   END FOREACH;
   -- se actualiza glo_folio
   UPDATE glo_folio
      SET status = 1,
          folio_referencia = p_folio_lote
   WHERE  folio = v_folio_liquida
   AND    proceso_cod = 2403;

   UPDATE STATISTICS FOR TABLE dae_det_solicitud;

 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,
        v_i_registros_insertados,
        v_folio_liquida;
END FUNCTION;


