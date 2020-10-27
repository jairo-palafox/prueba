






CREATE FUNCTION "safreviv".fn_uni_preliq_infonavit(p_folio DECIMAL(10), p_proceso_cod CHAR(20)) 

RETURNING INTEGER, INTEGER, INTEGER, DECIMAL                                       
                                        
 -- tabla de preliquidacion uni_preliquida
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
DEFINE v_preliq_origen             CHAR(20);
-- detalle uni_det_unificador
DEFINE v_folio_unificacion_unificador  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
--DEFINE v_nss_unificador                CHAR(11);
  
DEFINE v_folio_unificacion             DECIMAL(9,0);
DEFINE v_id_derechohabiente            DECIMAL(9,0); 
DEFINE v_nss_unificador                DECIMAL(11,0);
DEFINE v_id_unificado                  DECIMAL(9,0); 
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0); 
DEFINE v_nss_unificado                 DECIMAL(11,0);
 -- subcuenta
DEFINE v_sub_cuenta              SMALLINT;
DEFINE  v_subcuenta_4            SMALLINT;
DEFINE v_fondo_inversion         SMALLINT; -- fondo temporal
DEFINE v_movimiento_abono        SMALLINT; -- clave de movimiento abono
DEFINE v_movimiento_cargo        SMALLINT; -- clave de movimiento cargo
 --
DEFINE v_i_registros_insertados  INTEGER;
DEFINE v_i_registros_sin_saldo   INTEGER;
DEFINE v_resultado               SMALLINT;
--DEFINE v_si_estado             SMALLINT;
--DEFINE v_c_cadena              CHAR(200);
DEFINE g_proceso_cod             SMALLINT; -- codigo del proceso
DEFINE g_opera_cod               SMALLINT; -- codigo de operacion
DEFINE v_tipo_trabajador         CHAR(1); -- Tipo de trabajador tomado de afi_derechohabiente
DEFINE v_d_saldo_disponible_aivs DECIMAL(20,6);
DEFINE v_saldo_pesos             DECIMAL(16,6);
DEFINE v_saldo_aivs_unificador   DECIMAL(20,6);    
DEFINE v_saldo_pesos_unificador  DECIMAL(16,6);    
DEFINE v_folio_liquida           DECIMAL(9,0);         
DEFINE v_acciones                DECIMAL(16,6);
DEFINE v_pesos                   DECIMAL(16,6);


		ON EXCEPTION IN (-206)              
  		LET v_resultado = -206;         
  		LET v_i_registros_insertados = 0;
  		let v_i_registros_sin_saldo  = 0; 
      RETURN v_resultado, v_i_registros_insertados, v_i_registros_sin_saldo, 0;
    END EXCEPTION
    
    --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni.preliquida.INFONAVIT.txt";
    --SET DEBUG FILE TO "/safreviv/uni/bin/trace_uni_preliquida_INFONAVIT.txt";
    --TRACE ON;
    
    LET g_proceso_cod            = p_proceso_cod;
    LET g_opera_cod              = 2;
    LET v_i_registros_insertados = 0;
    LET v_i_registros_sin_saldo  = 0; 
    -- cuando es derechohabiente I

    LET v_folio_liquida=p_folio;
    LET v_fondo_inversion = 11;
    LET v_movimiento_cargo = 402; -- Movimiento cargo uni solo INFONAVIT
    LET v_movimiento_abono = 161; -- Movimiento abono uni solo INFONAVIT
    LET v_resultado = 0;
    LET v_d_saldo_disponible_aivs = 0.00;
    LET v_saldo_pesos = 0.00;    
    LET v_acciones = 0.00;
    LET v_pesos = 0.00;
    --trace "Obtiene los datos del Unificador";
   FOREACH
      SELECT id_inf_unificador,      
             folio_unificacion,  
             id_derechohabiente, 
             nss              
        INTO v_id_unificador,     
             v_folio_unificacion, 
             v_id_derechohabiente,
             v_nss_unificador
        FROM uni_inf_unificador
          WHERE estado_familia = 1  -- solo solicitudes aceptadas
            AND folio_unificacion = p_folio
                      
            --trace "Selecciona los datos  por cada unificador";
       LET v_saldo_aivs_unificador = 0; 
       LET v_saldo_pesos_unificador = 0;
       
       FOREACH
            SELECT id_inf_unificado, 
                   id_derechohabiente, 
                   nss
              INTO v_id_unificado,
                   v_id_derechohabiente_unificado,
                   v_nss_unificado
              FROM uni_inf_unificado
             WHERE id_unificador = v_id_unificador

             --trace "Toma el saldo del dia para el unificado";
             -- Buscamos si es que tiene saldo la cuenta
         FOREACH
            EXECUTE FUNCTION fn_saldo_actual(v_nss_unificado,0,TODAY)
            INTO v_sub_cuenta,
                 v_fondo_inversion,
                 v_acciones,
                 v_pesos

            IF (v_acciones > 0) AND (v_pesos > 0) THEN
               LET v_preliq_f_liquida           = TODAY;
               LET v_preliq_id_derechohabiente  = v_id_derechohabiente_unificado;
               LET v_preliq_subcuenta           = v_sub_cuenta;
               LET v_preliq_fondo_inversion     = v_fondo_inversion;
               LET v_preliq_movimiento          = v_movimiento_cargo;
               LET v_preliq_folio_liquida       = v_folio_liquida;
               LET v_preliq_id_referencia       = v_id_unificado;
               LET v_preliq_monto_acciones      = v_acciones * -1;
               LET v_preliq_monto_pesos         = v_pesos * -1;
               LET v_preliq_f_valor             = TODAY;
               LET v_preliq_f_registro          = TODAY;
               LET v_preliq_h_registro          = CURRENT HOUR TO SECOND;
               LET v_preliq_origen              = v_nss_unificador||'-N';  
               --tre "Inserta valores a la tabla preliquidacion unificado";
               INSERT INTO uni_preliquida(f_liquida,
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
                        v_preliq_origen);
                 
                 UPDATE uni_inf_unificado
                    SET diagnostico = 3 -- Preliquidado
                  WHERE id_inf_unificado = v_id_unificado
                    AND diagnostico = 1 -- Aceptadas
                    AND folio_unificacion = p_folio;

               -- Se transfieren los datos a las variables a 
               -- insertar a la tabla uni_preliquida
               -- Movimiento de abono
               --trace "Asigna valores a insertar a la tabla de preliquidación unificador";
               LET v_preliq_f_liquida           = TODAY;
               LET v_preliq_id_derechohabiente  = v_id_derechohabiente;
               LET v_preliq_subcuenta           = v_sub_cuenta;
               LET v_preliq_fondo_inversion     = v_fondo_inversion;
               LET v_preliq_movimiento          = v_movimiento_abono;
               LET v_preliq_folio_liquida       = v_folio_liquida;
               LET v_preliq_id_referencia       = v_id_unificador;
               LET v_preliq_monto_acciones      = v_acciones;
               LET v_preliq_monto_pesos         = v_pesos;
               LET v_preliq_f_valor             = TODAY;
               LET v_preliq_f_registro          = TODAY;
               LET v_preliq_h_registro          = CURRENT HOUR TO SECOND;
               LET v_preliq_origen              = v_nss_unificado||'-N';
               --trace "Inserta valores a la tabla preliquidacion unificador";
               INSERT INTO uni_preliquida(f_liquida,
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
                        v_preliq_origen);
               
                 -- Actualiza total de registros insertados mayor a cero
                 --trace "Actualiza total de registros insertados mayor a cero";
                 LET v_i_registros_insertados = v_i_registros_insertados + 1;
                 
                 -- Actualiza el estado de la solicitud a 3 preliquidado
                 -- con el folio de liquidación y su fecha de liquidación
                 --trace "Actualiza uni_det_unificador";
                 UPDATE uni_inf_unificador
                 SET    diagnostico = 3, -- Preliquidado
                        f_liquidacion = TODAY,
                        folio_liquidacion = v_folio_liquida
                 WHERE  estado_familia = 1
                 AND    diagnostico = 1 -- Aceptadas
                 AND    id_inf_unificador = v_id_unificador -- para el unificador en turno
                 AND    folio_unificacion = p_folio;
            END IF 
            
            --Si unificados no tienen saldo lo debe marcar como preliquidado
            {
            IF ( v_acciones IS NULL ) AND ( v_pesos IS NULL ) THEN
               --Si unificado no tiene saldo pero es aceptado lo debe marcar como prelíquidado
               UPDATE uni_inf_unificado
               SET    diagnostico = 4 -- Preliquidado
               WHERE  id_inf_unificado = v_id_unificado
               AND    diagnostico = 1 -- Aceptadas
               AND    folio_unificacion = p_folio;

               --trace "Actualiza uni_det_unificador";
               UPDATE uni_inf_unificador
               SET    diagnostico = 4 -- Preliquidado
               WHERE  estado_familia = 1
               AND    diagnostico = 1 -- Aceptadas
               AND    id_inf_unificador = v_id_unificador -- para el unificador en turno
               AND    folio_unificacion = p_folio;

               LET v_resultado = 3; --*C01
               
               LET v_i_registros_sin_saldo = v_i_registros_sin_saldo + 1;
            END IF
            }
         END FOREACH;
      END FOREACH;
   END FOREACH;
   
   SELECT COUNT(*)
   INTO   v_i_registros_sin_saldo
   FROM   uni_inf_unificador
   WHERE  folio_unificacion = p_folio
   AND    estado_familia = 1
   AND    diagnostico = 1; -- Aceptadas

   IF v_i_registros_sin_saldo >= 1 THEN 
      UPDATE uni_inf_unificado
      SET    diagnostico = 4 -- Preliquidado
      WHERE  diagnostico = 1 -- Aceptadas
      AND    folio_unificacion = p_folio;
      
      --trace "Actualiza uni_det_unificador";
      UPDATE uni_inf_unificador
      SET    diagnostico = 4 -- Preliquidado
      WHERE  estado_familia = 1
      AND    diagnostico = 1 -- Aceptadas
      AND    folio_unificacion = p_folio;
   END IF

   --trace "Actualiza glo_folio: "||p_folio;
   UPDATE glo_folio
   SET    status      = 1
   WHERE  folio       = p_folio
   AND    opera_cod   = g_opera_cod
   AND    proceso_cod = g_proceso_cod;

   UPDATE STATISTICS FOR TABLE uni_preliquida;
  
 RETURN v_resultado, 
        v_i_registros_insertados, 
        v_i_registros_sin_saldo,
        v_folio_liquida;
END FUNCTION;


