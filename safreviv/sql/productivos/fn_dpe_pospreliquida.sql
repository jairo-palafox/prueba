






CREATE FUNCTION "safreviv".fn_dpe_pospreliquida(p_usuario_cod CHAR(20), 
                                     p_folio_liquida DECIMAL(9,0)) 
   RETURNING INTEGER

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
 

 ON EXCEPTION IN (-206)
   LET v_i_resultado = -206;
   LET v_si_procesa_insercion = 0;
   LET v_i_registros_insertados = 0;
   RETURN v_i_resultado;
 END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.dpe.preliquida.txt";

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

 -- se obtienen todos los registros de la tabla temporal
 FOREACH --cur_tmp_detalle FOR
   SELECT
         a.id_dpe_referencia
        ,a.folio
        ,a.id_derechohabiente
        ,p.avis_viv_dev
        ,p.imp_viv_dev
        ,a.reg_patronal_imss
        ,a.periodo_pago
        ,p.diagnostico
     INTO
         v_dpe_id_dpe_referencia
        ,v_dpe_folio
        ,v_dpe_id_derechohabiente
        ,v_dpe_total_pagar_avis_viv_dev
        ,v_dpe_imp_viv_dev
        ,v_dpe_reg_patronal_imss
        ,v_dpe_periodo_pago
        ,v_dpe_diagnostico
   FROM dpe_sol_trabajador a, dpe_sol_trab_parcial p
   WHERE a.estado_solicitud = 1  -- solo solicitudes aceptadas
   --AND a.porcentaje_dev   = 1  -- indica que es un pago parcial
     AND p.diagnostico IN (c_pago_por_preliquidar_total, c_pago_por_preliquidar_parcial)
     AND a.id_dpe_referencia= p.id_dpe_referencia
     AND a.folio            = p.folio
     AND a.reg_patronal_imss= p.reg_patronal_imss
     AND a.periodo_pago     = p.periodo_pago
     AND p.folio_liquida  IS NULL
     --AND p.diagnostico      = a.diagnostico
     

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
    LET v_preliq_folio_liquida    = v_dpe_folio           ;    
    LET v_preliq_id_referencia      = v_dpe_id_dpe_referencia ;
    
    -- Total calculado
    LET v_preliq_monto_acciones     = v_dpe_total_pagar_avis_viv_dev*-1;
    -- Total calculado con la fecha de hoy
    LET v_preliq_monto_pesos        = v_dpe_imp_viv_dev       *-1;
    
    LET v_preliq_f_valor            = TODAY                   ;
    LET v_preliq_f_registro         = TODAY                   ;
    LET v_preliq_h_registro         = CURRENT HOUR TO SECOND  ;
    -- Se agrega el origen concatenando periodo de pago - NRP
    LET v_origen_preliquida         = v_dpe_periodo_pago||'-'||v_dpe_reg_patronal_imss;
    -- se insertan en la tabla de preliquidacion
    INSERT INTO dpe_preliquida
          ( f_liquida         ,
            id_derechohabiente,
            subcuenta         ,
            fondo_inversion   ,
            movimiento        ,
            folio_liquida     ,
            id_referencia     ,
            monto_acciones    ,
            monto_pesos       ,
            f_valor           ,
            f_registro        ,
            h_registro        ,
            origen 
           )
    VALUES( v_preliq_f_liquida         ,
            v_preliq_id_derechohabiente,
            v_preliq_subcuenta         ,
            v_preliq_fondo_inversion   ,
            v_preliq_movimiento        ,
            p_folio_liquida     ,
            v_preliq_id_referencia     ,
            v_preliq_monto_acciones    ,
            v_preliq_monto_pesos       ,
            v_preliq_f_valor           ,
            v_preliq_f_registro        ,
            v_preliq_h_registro        ,
            v_origen_preliquida
           );
    -- NOTA
    -- Los intereses no se insertar porque ya estan contemplados en el
    -- monto en pesos
    
    -- [Actualiza total de registros insertados mayor a cero]
    LET v_i_registros_insertados = 
        v_i_registros_insertados + 1;
    
    -- # [ Actualizar registro a preliquidado para que se  ]
    -- # [pueda utilizar la generación de archivo          ]
    -- # [  y no se vuelva a liquidar                      ]
    IF(v_dpe_diagnostico = c_pago_por_preliquidar_parcial)THEN
       -- Pone a estado pagado de un parcial
       UPDATE dpe_sol_trab_parcial
          SET diagnostico = c_pago_preliquidado_parcial,
              folio_liquida = p_folio_liquida
        WHERE id_dpe_referencia = v_preliq_id_referencia
          AND folio             = v_preliq_folio_liquida
          AND reg_patronal_imss = v_dpe_reg_patronal_imss
          AND periodo_pago      = v_dpe_periodo_pago
          AND diagnostico       = v_dpe_diagnostico;
       
    ELSE
       -- Pone a estado pagado de un total
       UPDATE dpe_sol_trab_parcial
          SET diagnostico = c_pago_preliquidado_total,
              folio_liquida = p_folio_liquida
        WHERE id_dpe_referencia = v_preliq_id_referencia
          AND folio             = v_preliq_folio_liquida
          AND reg_patronal_imss = v_dpe_reg_patronal_imss
          AND periodo_pago      = v_dpe_periodo_pago
          AND diagnostico       = v_dpe_diagnostico;
       
    END IF
    
 END FOREACH;
 
 update statistics for table dpe_preliquida;
 
   RETURN v_i_resultado;
END FUNCTION;


