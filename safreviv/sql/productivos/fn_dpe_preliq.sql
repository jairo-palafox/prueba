






CREATE FUNCTION "safreviv".fn_dpe_preliq(p_folio DECIMAL(10), 
                              p_usuario_cod CHAR(20),
                              p_pid DECIMAL(9,0)) 
       RETURNING INTEGER, SMALLINT, DECIMAL
-- tabla de preliquidacion
DEFINE v_preliq_f_liquida          DATE                   ;
DEFINE v_preliq_id_derechohabiente DECIMAL(9)             ;
DEFINE v_preliq_subcuenta          SMALLINT               ;
DEFINE v_preliq_fondo_inversion    SMALLINT               ;
DEFINE v_preliq_movimiento         SMALLINT               ;
DEFINE v_preliq_folio_liquida      DECIMAL(9,0)           ;
DEFINE v_preliq_id_referencia      DECIMAL(9,0)           ;
DEFINE v_preliq_monto_acciones     DECIMAL(16,6)          ;
DEFINE v_preliq_monto_pesos        DECIMAL(16,6)          ;
DEFINE v_preliq_f_valor            DATE                   ;
DEFINE v_preliq_f_registro         DATE                   ;
DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;

-- detalle en safre_viv
 DEFINE v_dpe_id_dpe_referencia    DECIMAL(9,0) ;
 DEFINE v_dpe_folio                DECIMAL(9,0) ;
 DEFINE v_dpe_id_derechohabiente   DECIMAL(9,0)   ;
 DEFINE v_dpe_nombre_trabajador    CHAR(50)     ;
 DEFINE v_dpe_avis_viv_dev         DECIMAL(16,6);
 DEFINE v_dpe_imp_viv_dev          DECIMAL(16,6);

 -- variables de soporte al proceso
 DEFINE v_b_exito                  SMALLINT; -- booleana para indicar si el proceso termino bien
 DEFINE v_id_derechohabiente       DECIMAL(9); -- ID de derechohabiente asociado a un NSS
 DEFINE v_i_resultado              INTEGER; -- resultado de la operacion
 -- subcuenta y
 DEFINE v_subcuenta_tmp            SMALLINT; -- subcuenta temporal
 DEFINE v_fondo_inversion_tmp      SMALLINT; -- fondo temporal
 DEFINE v_movimiento_tmp           SMALLINT; -- clave de movimiento temporal
 --
 DEFINE v_si_estado                SMALLINT;
 DEFINE v_c_cadena                 CHAR(200);
 DEFINE v_folio_liquida            DECIMAL(9,0);
 DEFINE g_proceso_cod              SMALLINT; -- codigo del proceso
 DEFINE g_opera_cod                SMALLINT; -- codigo de operacion
 DEFINE v_si_regs_encontrados_parciales INTEGER;


 -- si no se encuentra la tabla tmp_detalle_op98 entonces levantamos una excepcion
 -- The specified table (tmp_detalle_op98) is not in the database
 ON EXCEPTION IN (-206)
   LET v_i_resultado = -206;
   LET v_si_regs_encontrados_parciales = 0;
   RETURN v_i_resultado, v_si_regs_encontrados_parciales, 0;
 END EXCEPTION
 
 --SET DEBUG FILE TO 'trace.dpe.preliquida.16032012.txt';
 
 LET g_proceso_cod = 1001;
 LET g_opera_cod   = 3;
 
 LET v_i_resultado = 0;
 
 LET v_si_regs_encontrados_parciales = 0;

   -- -- se actualiza glo_folio
   --UPDATE glo_folio
   --   SET status = 1
   -- WHERE folio = p_folio;
 
 
 -- # [Generar folio para la preliquidacion de grupo de folos de total o de parciales.]
 EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   INTO v_folio_liquida;
 
 
 -- # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 -- # [Se generan las preliquidaciones de los pagos totales]
 -- # [  y parciales .                                     ]
 -- # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 
 EXECUTE FUNCTION fn_dpe_genera_pagos_parciales(p_usuario_cod) 
   INTO v_si_estado, v_si_regs_encontrados_parciales, v_c_cadena;
  
 -- # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 -- # [Se genera preliquidación de los pagos parciales     ]
 -- # [  pendientes de preliquidar.                        ]
 -- # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 EXECUTE FUNCTION fn_dpe_pospreliquida(p_usuario_cod, v_folio_liquida)
   INTO v_si_estado;
 
 -- se actualiza glo_folio
 UPDATE glo_folio
    SET status = 1,
    folio_referencia = p_folio
  WHERE folio = v_folio_liquida
    AND opera_cod = g_opera_cod
    AND proceso_cod = g_proceso_cod;
    
-- Se actualiza el folio referencia 
 --UPDATE glo_folio
 --  SET folio_referencia = p_folio
 -- WHERE folio = v_folio_liquida
 --   AND opera_cod = g_opera_cod;
 
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,v_si_regs_encontrados_parciales,v_folio_liquida;
END FUNCTION;


