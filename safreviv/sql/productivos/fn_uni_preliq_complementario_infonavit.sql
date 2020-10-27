






CREATE FUNCTION "safreviv".fn_uni_preliq_complementario_infonavit(p_folio DECIMAL(10), 
                                                       p_usuario_cod CHAR(20), 
                                                       p_pid DECIMAL(9,0), 
                                                       p_proceso_cod DECIMAL(9,0)) 
                                             RETURNING SMALLINT,     --v_si_resultado,                  
                                                       INTEGER,      --v_i_registros_insertados,
                                                       DECIMAL,      --v_folio_liquida,         
                                                       INTEGER,      --isam_err,                
                                                       VARCHAR(255)  --err_txt;                 
 -- tabla de preliquidacion uni_preliquida
DEFINE v_preliq_f_liquida              DATE;
DEFINE v_preliq_id_derechohabiente     DECIMAL(9,0);
DEFINE v_preliq_subcuenta              SMALLINT;
DEFINE v_preliq_fondo_inversion        SMALLINT;
DEFINE v_preliq_movimiento             SMALLINT;
DEFINE v_preliq_folio_liquida          DECIMAL(9,0);
DEFINE v_preliq_id_referencia          DECIMAL(9,0);
DEFINE v_preliq_monto_acciones         DECIMAL(16,6);
DEFINE v_preliq_monto_pesos            DECIMAL(16,6);
DEFINE v_preliq_f_valor                DATE;
DEFINE v_preliq_f_registro             DATE;
DEFINE v_preliq_h_registro             DATETIME HOUR TO SECOND;
DEFINE v_preliq_origen                 CHAR(20);
-- detalle uni_det_unificador
DEFINE v_folio_unificacion_unificador  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
--DEFINE v_nss_unificador                CHAR(11);
  
DEFINE v_folio_unificacion             DECIMAL(9,0);
DEFINE v_id_derechohabiente            DECIMAL(9,0); 
DEFINE v_nss_unificador                DECIMAL(11,0);
DEFINE v_id_unificado                  DECIMAL(9,0); 
DEFINE v_id_dh_unificado               DECIMAL(9,0); 
DEFINE v_nss_unificado                 DECIMAL(11,0);
 -- subcuenta
DEFINE v_sub_cuenta                    SMALLINT;
DEFINE  v_subcuenta_4                  SMALLINT;
DEFINE v_fondo_inversion               SMALLINT; -- fondo temporal
DEFINE v_movimiento_abono              SMALLINT; -- clave de movimiento abono
DEFINE v_movimiento_cargo              SMALLINT; -- clave de movimiento cargo
 --                                    
DEFINE v_i_registros_insertados        INTEGER;
DEFINE v_i_resultado_saldo             SMALLINT;
DEFINE g_proceso_cod                   SMALLINT; -- codigo del proceso
DEFINE g_opera_cod                     SMALLINT; -- codigo de operacion
DEFINE v_tipo_trabajador               CHAR(1); -- Tipo de trabajador tomado de afi_derechohabiente
DEFINE v_d_saldo_disponible_aivs       DECIMAL(20,6);
DEFINE v_saldo_pesos                   DECIMAL(16,6);
DEFINE v_folio_liquida                 DECIMAL(9,0);         
DEFINE v_acciones                      DECIMAL(16,6);
DEFINE v_pesos                         DECIMAL(16,6);
-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  SMALLINT;
--Variables del complementario
DEFINE v_folio_liquida_original        DECIMAL(9,0);

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, 
             v_i_registros_insertados, 
             0, 
             isam_err, 
             err_txt;
   END EXCEPTION
    
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni.preliquida.INFONAVIT.txt";
   --trace on;
   
   LET g_proceso_cod            = p_proceso_cod;
   LET g_opera_cod              = 2;
   LET v_i_resultado_saldo      = 1;
   LET v_i_registros_insertados = 0;
   -- cuando es derechohabiente I

   LET v_folio_liquida           = 0;
   LET v_fondo_inversion         = 11;
   LET v_movimiento_cargo        = 402; -- Movimiento cargo uni solo INFONAVIT
   LET v_movimiento_abono        = 161; -- Movimiento abono uni solo INFONAVIT
   LET v_i_resultado_saldo       = 0;
   LET v_d_saldo_disponible_aivs = 0.00;
   LET v_saldo_pesos             = 0.00;    
   LET v_acciones                = 0.00;
   LET v_pesos                   = 0.00;            
   LET v_id_dh_unificado         = 0;
   LET v_folio_liquida_original  = 0;
   LET v_id_unificador           = 0;
   LET v_id_unificado            = 0;
   LET v_folio_unificacion       = 0; 
   LET v_id_derechohabiente      = 0;
   LET v_nss_unificador          = "";
   LET v_nss_unificado           = "";
   LET v_sub_cuenta              = 0;
   LET v_acciones                = 0.00;
   LET v_pesos                   = 0.00;
   LET g_proceso_cod             = p_proceso_cod;
   LET g_opera_cod               = 1;
   LET v_si_resultado            = 0;
   LET v_i_registros_insertados  = 0;
   
--   LET v_preliq_f_liquida          = TODAY;  
--   LET v_preliq_id_derechohabiente = 0;
--   LET v_preliq_subcuenta          = 0;
--   LET v_preliq_fondo_inversion    = 11;
--   LET v_preliq_movimiento         = 402;
--   LET v_preliq_folio_liquida      = 0;
--   LET v_preliq_id_referencia      = 0;
--   LET v_preliq_monto_acciones     = 0;
--   LET v_preliq_monto_pesos        = 0;
--   LET v_preliq_f_valor            = "";
--   LET v_preliq_f_registro         = "";
--   LET v_preliq_h_registro         = "";
--   LET v_preliq_origen             = "";
   LET isam_err                 = 0;
   LET err_txt                  = "Proceso finalizado sin error";
 
   --LET err_txt = "Al generar folio para la preliquidacion unificación de cuentas";
   EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   INTO v_folio_liquida;

   --LET err_txt = "Obtiene los datos del Unificador";
   FOREACH
      SELECT id_inf_unificador,      
             folio_unificacion,  
             id_derechohabiente, 
             nss,
             folio_liquidacion              
      INTO   v_id_unificador,     
             v_folio_unificacion, 
             v_id_derechohabiente,
             v_nss_unificador,
             v_folio_liquida_original
      FROM   uni_inf_unificador
      WHERE  estado_unificacion = 1
      AND    estado_familia = 1
      AND    folio_liquidacion > 0
                      
      
      FOREACH
         SELECT id_inf_unificado, 
                id_derechohabiente, 
                nss
         INTO   v_id_unificado,
                v_id_dh_unificado,
                v_nss_unificado
         FROM   uni_inf_unificado
         WHERE  id_unificador = v_id_unificador

         --LET err_txt = "Al consultar el saldo actual";

         FOREACH 
            EXECUTE FUNCTION fn_saldo_actual(v_nss_unificado,0,TODAY)
            INTO v_sub_cuenta,
                 v_fondo_inversion,
                 v_acciones,
                 v_pesos


            --LET err_txt = "Cuenta sin saldo para unificar";
            IF (v_acciones <> 0) AND (v_pesos <> 0) THEN
               LET v_preliq_f_liquida           = TODAY;
               LET v_preliq_id_derechohabiente  = v_id_dh_unificado;
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
               --trace "Inserta valores a la tabla preliquidacion unificado";
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
 
               --LET err_txt = "Asigna valores a insertar a la tabla de preliquidación unificador";
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
               -- insertar a la tabla uni_preliquida movimiento de abono
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

               --LET err_txt = "INSERTA EN uni_complementario_infonavit";
               INSERT INTO uni_complementario_infonavit(id_complementario,
                                                        id_referencia    ,
                                                        folio_unificacion,
                                                        folio_liquidacion,
                                                        f_liquidacion)
                                       VALUES(seq_uni_complementario_infonavit.NEXTVAL,
                                              v_id_dh_unificado,      
                                              v_folio_liquida_original,
                                              v_folio_liquida,           
                                              TODAY);       
               
               -- Actualiza total de registros insertados mayor a cero
               --trace "Actualiza total de registros insertados mayor a cero";
               LET v_i_registros_insertados = v_i_registros_insertados + 1;
            END IF
         END FOREACH;
      END FOREACH;
   END FOREACH;
   
   --LET err_txt = "Actualiza glo_folio ";
   UPDATE glo_folio
   SET    status      = 1
   WHERE  folio       = p_folio
   AND    opera_cod   = 2317
   AND    proceso_cod = 1;
    
   UPDATE STATISTICS FOR TABLE uni_preliquida;
   UPDATE STATISTICS FOR TABLE uni_complementario_infonavit;
  
RETURN v_si_resultado,
       v_i_registros_insertados,
       v_folio_liquida, 
       isam_err, 
       err_txt;
END FUNCTION;


