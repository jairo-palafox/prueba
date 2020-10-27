






CREATE FUNCTION "safreviv".fn_dpe_preliq_complementario(p_usuario_cod       CHAR(20),
                                             p_folio_integracion DECIMAL(9,0)) 

   RETURNING INTEGER,
             DECIMAL(9,0)

-- tabla de preliquidacion
DEFINE v_preliq_f_liquida            DATE                   ;
DEFINE v_preliq_id_derechohabiente   DECIMAL(9)             ;
DEFINE v_preliq_subcuenta            SMALLINT               ;
DEFINE v_preliq_fondo_inversion      SMALLINT               ;
DEFINE v_preliq_movimiento           SMALLINT               ;
DEFINE v_preliq_folio_liquida        DECIMAL(9,0)           ;
DEFINE v_preliq_id_referencia        DECIMAL(9,0)           ;
DEFINE v_preliq_monto_acciones       DECIMAL(16,6)          ;
DEFINE v_preliq_monto_pesos          DECIMAL(16,6)          ;
DEFINE v_preliq_f_valor              DATE                   ;
DEFINE v_preliq_f_registro           DATE                   ;
DEFINE v_preliq_h_registro           DATETIME HOUR TO SECOND;
DEFINE v_origen_preliquida           CHAR(18)               ;
-- detalle en safre_viv
 DEFINE v_dpe_id_dpe_referencia     DECIMAL(9,0) ;
 DEFINE v_dpe_folio                 DECIMAL(9,0) ;
 DEFINE v_dpe_id_derechohabiente    DECIMAL(9,0)   ;
 DEFINE v_dpe_nombre_trabajador     CHAR(50)     ;
 DEFINE v_dpe_total_pagar_avis_viv_dev          DECIMAL(16,6);
 DEFINE v_dpe_imp_viv_dev           DECIMAL(16,6);
 DEFINE v_dpe_reg_patronal_imss     CHAR(11);
 DEFINE v_dpe_periodo_pago          CHAR(6);
 DEFINE v_dpe_diagnostico           SMALLINT;
 
 DEFINE v_pagado_imp_viv_dev        DECIMAL(16,6);
 DEFINE v_total_pagado_avis_viv_dev DECIMAL(16,6);
 DEFINE v_d_saldo_disponible_aivs   DECIMAL(16,6);
 DEFINE v_d_saldo_calculado_aivs   DECIMAL(16,6);

 -- variables de soporte al proceso
 DEFINE v_b_exito                       SMALLINT; -- booleana para indicar si el proceso termino bien
 DEFINE v_id_derechohabiente            decimal(9); -- ID de derechohabiente asociado a un NSS
 DEFINE v_i_resultado                   INTEGER; -- resultado de la operacion
 -- subcuenta y
 DEFINE v_subcuenta_tmp                 SMALLINT; -- subcuenta temporal
 DEFINE v_fondo_inversion_tmp           SMALLINT; -- fondo temporal
 DEFINE v_movimiento_tmp                SMALLINT; -- clave de movimiento temporal
 DEFINE v_si_procesa_insercion          SMALLINT;
 --
 DEFINE v_i_registros_insertados        INTEGER;
 DEFINE v_dte_fecha_hoy                 DATE;
 DEFINE v_si_estado                     SMALLINT;
 
 DEFINE v_act_total_avis_viv_dev        DECIMAL(16,6);
 DEFINE v_act_total_imp_viv_dev         DECIMAL(16,6);
 DEFINE v_act_acumula_avis_viv_dev      DECIMAL(16,6);
 DEFINE v_act_acumula_imp_viv_dev       DECIMAL(16,6);
 DEFINE v_si_marca_imsss                SMALLINT;
 DEFINE v_resultado                     SMALLINT;
 
 DEFINE v_si_total_pagar                SMALLINT;
 
 -- Tipos status de pagos
 DEFINE c_pago_total_nss                SMALLINT;
 DEFINE c_pago_parcial_nss              SMALLINT;
 DEFINE c_pago_por_preliquidar_total    SMALLINT;
 DEFINE c_pago_por_preliquidar_parcial  SMALLINT;
 DEFINE c_pago_preliquidado_total       SMALLINT;
 DEFINE c_pago_preliquidado_parcial     SMALLINT;
 DEFINE c_pago_liquidado_total          SMALLINT;
 DEFINE c_pago_liquidado_parcial        SMALLINT;
 DEFINE c_pago_enviado_procesar_total   SMALLINT; 
 DEFINE c_pago_enviado_procesar_parcial SMALLINT;
 
 DEFINE v_folio_liquida                 DECIMAL(9,0);
 DEFINE v_precio_accion_dia             DECIMAL(19,14);
 
 DEFINE g_proceso_cod SMALLINT;
 DEFINE g_opera_cod   SMALLINT;

 ON EXCEPTION IN (-206)
   LET v_i_resultado = -206;
   LET v_si_procesa_insercion = 0;
   LET v_i_registros_insertados = 0;
   RETURN v_i_resultado,
          v_folio_liquida;
 END EXCEPTION
 
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_dpe_preliquida_complementario.trace";
   SET DEBUG FILE TO "/safreviv_int/dpe/envio/trace_dpe_preliquida_complementario.trace";
   TRACE ON;

 -- se asume que el proceso termina correctamente
 LET v_i_resultado = 0;
 -- <Inicializa status de proceso de inserción>
 LET v_si_procesa_insercion = 0;

 -- <Inicializa total de registros insertados con importe mayor a cero>
 LET v_i_registros_insertados = 0;
 
 -- Constantes de estatos de diagnostico y de pagos
 LET c_pago_total_nss                = 0;
 LET c_pago_parcial_nss              = 1;
 LET c_pago_por_preliquidar_total    = 2;
 LET c_pago_por_preliquidar_parcial  = 3;
 LET c_pago_preliquidado_total       = 4;
 LET c_pago_preliquidado_parcial     = 5;
 LET c_pago_liquidado_total          = 6;
 LET c_pago_liquidado_parcial        = 7;
 LET c_pago_enviado_procesar_total   = 8;
 LET c_pago_enviado_procesar_parcial = 9;
 

 LET v_dte_fecha_hoy = TODAY; 
 LET g_proceso_cod = 1002;
 LET g_opera_cod   = 2;
 LET v_precio_accion_dia = 0;
 -- Generar folio para la preliquidacion
 EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
 INTO v_folio_liquida;

 -- se obtienen todos los registros de la tabla temporal
 FOREACH --cur_tmp_detalle FOR
    SELECT a.id_dpe_compl_referencia,
           d.id_derechohabiente,
           a.num_aplicaciones_inter,
           a.imp_ret_devolver,
           a.num_reg_pat_imss,
           a.per_pago,
           a.diagnostico
    INTO   v_dpe_id_dpe_referencia,
           v_dpe_id_derechohabiente,
           v_dpe_total_pagar_avis_viv_dev,
           v_dpe_imp_viv_dev,
           v_dpe_reg_patronal_imss,
           v_dpe_periodo_pago,
           v_dpe_diagnostico
    FROM   dpe_sol_trab_complementario a, 
           afi_derechohabiente d
    WHERE  a.nss_aportacion = d.nss
    AND    a.resul_operacion = 1  -- solo solicitudes aceptadas
    AND    a.resul_operacion_compl = 1
    AND    a.diagnostico IN (0,1)
     

    LET v_subcuenta_tmp       =  4;
    LET v_fondo_inversion_tmp = 11;
    
    -- El movimiento es:
    -- 342	ABONO POR DEVOLUCIÓN DE PAGOS DE INDEBIDOS O EN EXCESO
    LET v_movimiento_tmp = 342;
    
    -- Se transfieren los datos al registro de
    LET v_preliq_f_liquida          = TODAY;
    LET v_preliq_id_derechohabiente = v_dpe_id_derechohabiente;
    LET v_preliq_subcuenta          = v_subcuenta_tmp         ;
    LET v_preliq_fondo_inversion    = v_fondo_inversion_tmp   ;
    LET v_preliq_movimiento         = v_movimiento_tmp        ;
    LET v_preliq_id_referencia      = v_dpe_id_dpe_referencia ;
    LET v_preliq_monto_pesos = 0;
    -- Total calculado
    LET v_preliq_monto_acciones     = v_dpe_total_pagar_avis_viv_dev*-1;

   --Identificar precio de Acción del día
    SELECT precio_fondo
    INTO   v_precio_accion_dia
    FROM   glo_valor_fondo
    WHERE  fondo       = 11
    AND    f_valuacion = TODAY;    
    
    -- Total calculado con la fecha de hoy             
    LET v_preliq_monto_pesos        = ((v_dpe_total_pagar_avis_viv_dev * v_precio_accion_dia)*-1);
    --LET v_preliq_monto_pesos        = (v_preliq_monto_pesos* -1);
    
    LET v_preliq_f_valor            = TODAY;
    LET v_preliq_f_registro         = TODAY;
    LET v_preliq_h_registro         = CURRENT HOUR TO SECOND  ;
    -- Se agrega el origen concatenando periodo de pago - NRP
    LET v_origen_preliquida         = v_dpe_periodo_pago||'-'||v_dpe_reg_patronal_imss;
    
    -- se insertan en la tabla de preliquidacion
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
                               v_folio_liquida,
                               v_preliq_id_referencia,
                               v_preliq_monto_acciones,
                               v_preliq_monto_pesos,
                               v_preliq_f_valor,
                               v_preliq_f_registro,
                               v_preliq_h_registro,
                               v_origen_preliquida);

   LET v_si_marca_imsss = 401;

   EXECUTE FUNCTION fn_desmarca_cuenta(v_preliq_id_derechohabiente,
                                       v_si_marca_imsss,
                                       v_preliq_id_referencia,
                                       0,
                                       0,
                                       p_usuario_cod,
                                       g_proceso_cod)
      INTO v_i_resultado;

    -- [Actualiza total de registros insertados mayor a cero]
    LET v_i_registros_insertados = v_i_registros_insertados + 1;    

 END FOREACH;
  
    UPDATE glo_folio
    SET    status           = 2,
           folio_referencia = p_folio_integracion
    WHERE  folio = v_folio_liquida;
 
 UPDATE STATISTICS FOR TABLE dpe_preliquida;
 
   RETURN v_i_resultado,
          v_folio_liquida;
          
END FUNCTION;


