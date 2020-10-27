






CREATE FUNCTION "safreviv".fn_unifica_saldos_remanentes (p_nss_unificador CHAR(11),
                                              p_nss_unificado  CHAR(11),
                                              p_usuario_cod    CHAR(20),
                                              p_folio_liquida  DECIMAL(9,0)
                                             )

RETURNING SMALLINT, INTEGER, VARCHAR(255);

-- tabla de preliquidacion uni_preliquida
DEFINE v_mov_f_liquida                 DATE;
DEFINE v_mov_id_derechohabiente        DECIMAL(9,0);
DEFINE v_mov_subcuenta                 SMALLINT;
DEFINE v_mov_fondo_inversion           SMALLINT;
DEFINE v_mov_movimiento                SMALLINT;
DEFINE v_mov_folio_liquida             DECIMAL(9,0);
DEFINE v_mov_id_referencia             DECIMAL(9,0);
DEFINE v_mov_monto_acciones            DECIMAL(16,6);
DEFINE v_mov_monto_pesos               DECIMAL(16,6);
DEFINE v_mov_f_valor                   DATE;
DEFINE v_mov_f_registro                DATE;
DEFINE v_mov_h_registro                DATETIME HOUR TO SECOND;
DEFINE v_mov_origen                    CHAR(13); --Se cambia tamaño de variable para almacenar NSS
-- detalle uni_det_unificador
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_folio_unificacion_unificador  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
DEFINE v_nss_unificador                CHAR(11);
-- detalle uni_det_unificado
DEFINE v_nss_unificado                 CHAR(11);
DEFINE v_id_unificado                  DECIMAL(9,0);
DEFINE v_id_dh_unificado               DECIMAL(9,0);
 -- variables de soporte al proceso
DEFINE v_i_resultado                   SMALLINT; -- resultado de la operacion
DEFINE v_d_saldo_disponible_aivs       DECIMAL(20,6);
DEFINE v_saldo_pesos                   DECIMAL(16,6);
DEFINE v_saldo_aivs_unificador         DECIMAL(16,6);
DEFINE v_saldo_pesos_unificador        DECIMAL(16,6);
 -- subcuenta y movimientos
DEFINE v_sub_cuenta                    SMALLINT;
DEFINE v_fondo_inversion               SMALLINT;
DEFINE v_acciones                      DECIMAL(16,2);
DEFINE v_pesos                         DECIMAL(16,2);
DEFINE v_movimiento_abono              SMALLINT; -- clave de movimiento abono
DEFINE v_movimiento_cargo              SMALLINT; -- clave de movimiento cargo
 --                                    
DEFINE v_si_estado                     SMALLINT; -- codigo de operacion
                                       
-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  SMALLINT;

--Variables del complementario
DEFINE p_folio_liquida_original        DECIMAL(9,0);

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, 
             isam_err, 
             err_txt;
   END EXCEPTION
    
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_unifica_saldos_remanentes.txt";

   LET v_i_resultado            = 0;
   LET isam_err                 = 0;
   LET err_txt                  = "Proceso finalizado sin error";
   LET v_acciones               = 0.00;
   LET v_pesos                  = 0.00;
   
   LET v_movimiento_cargo = 412; -- CARGO COMPLEMENTARIO POR UNIFICACIÓN DE CUENTAS
   LET v_movimiento_abono = 171; -- ABONO COMPLEMENTARIO POR UNIFICACIÓN DE CUENTAS

   --trace "Inicia el proceso de preliquidacion"; 
   -- se obtienen todos los registros de la tabla uni_det_unificador
   FOREACH      
      SELECT b.id_unificado       id_ado,
             b.id_unificador      id_dor_ado,
             b.nsscta1            nss_ado,
             b.id_derechohabiente id_dh_ado,
             c.nss_unificador     nss_dor,
             c.id_derechohabiente id_dh_dor,
             c.folio_liquidacion  f_original
      INTO   v_id_unificado,
             v_id_unificador,
             v_nss_unificado,
             v_id_dh_unificado,
             v_nss_unificador,
             v_id_derechohabiente_unificador,
             p_folio_liquida_original
      FROM   uni_det_unificado b,
             uni_det_unificador c
      WHERE  c.id_unificador  = b.id_unificador
      AND    b.nsscta1        = p_nss_unificado
      AND    c.estado_familia = 1
      AND    b.diagnostico    = 6      

      -- Se consulta el saldo de cada unificado
      FOREACH
         EXECUTE FUNCTION fn_saldo_actual(p_nss_unificado,0,TODAY)
           INTO v_sub_cuenta,
                v_fondo_inversion,
                v_acciones,
                v_pesos
         IF v_sub_cuenta <> 46 THEN 
            IF (v_acciones > 0) AND (v_pesos > 0) THEN
               --trace "Asigna valores a insertar a la tabla de preliquidación unificado";
               LET v_mov_f_liquida           = TODAY;
               LET v_mov_id_derechohabiente  = v_id_dh_unificado;
               LET v_mov_subcuenta           = v_sub_cuenta;
               LET v_mov_fondo_inversion     = v_fondo_inversion;
               LET v_mov_movimiento          = v_movimiento_cargo;
               LET v_mov_folio_liquida       = p_folio_liquida;
               LET v_mov_id_referencia       = v_id_unificado;
               LET v_mov_monto_acciones      = v_acciones * -1;
               LET v_mov_monto_pesos         = v_pesos * -1;
               LET v_mov_f_valor             = TODAY;
               LET v_mov_f_registro          = TODAY;
               LET v_mov_h_registro          = CURRENT HOUR TO SECOND;
               LET v_mov_origen              = v_nss_unificador||v_id_unificador;
               
               --trace "Inserta valores a la tabla preliquidacion unificado";
               INSERT INTO safre_viv:cta_movimiento
                 VALUES(v_mov_f_liquida,
                        v_mov_id_derechohabiente,
                        v_mov_subcuenta,
                        v_mov_fondo_inversion,
                        v_mov_movimiento,
                        v_mov_folio_liquida,
                        v_mov_id_referencia,
                        v_mov_monto_acciones,
                        v_mov_monto_pesos,
                        v_mov_f_valor,
                        v_mov_f_registro,
                        v_mov_h_registro,
                        v_mov_origen);
               --   
               LET v_mov_f_liquida           = TODAY;
               LET v_mov_id_derechohabiente  = v_id_derechohabiente_unificador;
               LET v_mov_subcuenta           = v_sub_cuenta;
               LET v_mov_fondo_inversion     = v_fondo_inversion;
               LET v_mov_movimiento          = v_movimiento_abono;
               LET v_mov_folio_liquida       = p_folio_liquida;
               LET v_mov_id_referencia       = v_id_unificador;
               LET v_mov_monto_acciones      = v_acciones;
               LET v_mov_monto_pesos         = v_pesos;
               LET v_mov_f_valor             = TODAY;
               LET v_mov_f_registro          = TODAY;
               LET v_mov_h_registro          = CURRENT HOUR TO SECOND;
               LET v_mov_origen              = v_nss_unificado||v_id_unificado;
               
               INSERT INTO safre_viv:cta_movimiento
                VALUES(v_mov_f_liquida,
                       v_mov_id_derechohabiente,
                       v_mov_subcuenta,
                       v_mov_fondo_inversion,
                       v_mov_movimiento,
                       v_mov_folio_liquida,
                       v_mov_id_referencia,
                       v_mov_monto_acciones,
                       v_mov_monto_pesos,
                       v_mov_f_valor,
                       v_mov_f_registro,
                       v_mov_h_registro,
                       v_mov_origen);
            
            END IF;
         END IF;
      END FOREACH;
      
   END FOREACH;

   UPDATE STATISTICS FOR TABLE cta_movimiento;
  
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,
        isam_err, 
        err_txt;
END FUNCTION;


