






CREATE FUNCTION "safreviv".fn_uni_preliquida_complementaria(p_folio       DECIMAL(10), 
                                                 p_usuario_cod CHAR(20), 
                                                 p_pid         DECIMAL(9,0), 
                                                 p_proceso_cod SMALLINT) 

RETURNING SMALLINT, 
          INTEGER, 
          DECIMAL, 
          INTEGER, 
          VARCHAR(255)

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
DEFINE v_preliq_origen                 CHAR(13); 
DEFINE v_id_complementario             DECIMAL(9,0);
-- detalle uni_det_unificador
DEFINE v_id_dh_unificador              DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
DEFINE v_nss_unificador                CHAR(11);
-- detalle uni_det_unificado
DEFINE v_nss_unificado                 CHAR(11);
DEFINE v_id_unificado                  DECIMAL(9,0);
DEFINE v_id_dh_unificado               DECIMAL(9,0);
DEFINE v_tipo_unificacion              SMALLINT;
 -- variables de soporte al proceso
DEFINE v_i_resultado                   SMALLINT; -- resultado de la operacion
DEFINE v_d_saldo_disponible_aivs       DECIMAL(20,6);
DEFINE v_saldo_pesos                   DECIMAL(16,6);
DEFINE v_saldo_aivs_unificador         DECIMAL(16,6);
DEFINE v_saldo_pesos_unificador        DECIMAL(16,6);
DEFINE v_folio_liquida                 DECIMAL(9,0);
 -- subcuenta y movimientos
DEFINE v_sub_cuenta                    SMALLINT;
DEFINE v_fondo_inversion               SMALLINT;
DEFINE v_acciones                      DECIMAL(16,2);
DEFINE v_pesos                         DECIMAL(16,2);
DEFINE v_movimiento_abono              SMALLINT;
DEFINE v_movimiento_cargo              SMALLINT;
DEFINE v_movimiento_cargo_imss         SMALLINT;
DEFINE v_movimiento_abono_imss         SMALLINT;
DEFINE v_movimiento_cargo_ifvt         SMALLINT;
DEFINE v_movimiento_abono_ifvt         SMALLINT;
 --                                    
DEFINE v_i_registros_insertados        INTEGER;
DEFINE v_si_estado                     SMALLINT;
--DEFINE p_proceso_cod                   SMALLINT;
DEFINE v_opera_cod                     SMALLINT;
                                       
-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  SMALLINT;

--Variables del complementario
DEFINE v_folio_liquida_original        DECIMAL(9,0);

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, 
             v_i_registros_insertados, 
             0, 
             isam_err, 
             err_txt;
   END EXCEPTION
    
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_uni_preliq_compl.txt";
   --TRACE ON;
       
   LET v_i_resultado            = 0;
   LET v_i_registros_insertados = 0;
   LET isam_err                 = 0;
   LET err_txt                  = "Proceso finalizado sin error";
   LET v_acciones               = 0.00;
   LET v_pesos                  = 0.00;
   LET v_opera_cod              = 3;
   
   LET v_movimiento_cargo_imss = 392; -- CARGO COMPLEMENTARIO POR UNIFICACIÓN DE
   LET v_movimiento_abono_imss = 151; -- ABONO COMPLEMENTARIO POR UNIFICACIÓN DE
   LET v_movimiento_cargo_ifvt = 402; -- CARGO POR UNIFICACIÓN DE CUENTAS SOLO IN
   LET v_movimiento_abono_ifvt = 161; -- ABONO POR UNIFICACIÓN DE CUENTAS SOLO IN

   -- # [Generar folio para la preliquidacion unificación de cuentas.]
   EXECUTE FUNCTION fn_genera_folio(p_proceso_cod,v_opera_cod,p_usuario_cod)
      INTO v_folio_liquida;

   FOREACH    

      SELECT id_unificado, 
             id_unificador,
             id_derechohabiente_ado,
             id_derechohabiente_dor,
             nss_unificado,
             nss_unificador,
             tipo_unificacion,
             id_complementario
      INTO   v_id_unificado,
             v_id_unificador,
             v_id_dh_unificado,
             v_id_dh_unificador,
             v_nss_unificado,
             v_nss_unificador,
             v_tipo_unificacion,
             v_id_complementario
      FROM   uni_det_complementario
      WHERE  diagnostico = 1
      AND    estado      = 1
      AND    folio_complentario = p_folio

      IF v_tipo_unificacion = 1 THEN 
         LET v_movimiento_cargo = v_movimiento_cargo_imss;
         LET v_movimiento_abono = v_movimiento_abono_imss;
      END IF
      
      IF v_tipo_unificacion = 2 THEN 
         LET v_movimiento_cargo = v_movimiento_cargo_ifvt;
         LET v_movimiento_abono = v_movimiento_abono_ifvt;
      END IF

      FOREACH
         EXECUTE FUNCTION fn_saldo_actual(v_nss_unificado,0,TODAY)
           INTO v_sub_cuenta,
                v_fondo_inversion,
                v_acciones,
                v_pesos
         IF v_sub_cuenta <> 46 THEN
            IF (v_acciones > 0) AND (v_pesos > 0) THEN
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
               LET v_preliq_origen              = v_nss_unificador||"-"||v_id_unificador;
               
               --trace "Inserta valores a la tabla preliquidacion unificado";
               INSERT INTO safre_viv:uni_preliquida
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
               --   
               LET v_preliq_f_liquida           = TODAY;
               LET v_preliq_id_derechohabiente  = v_id_dh_unificador;
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
               LET v_preliq_origen              = v_nss_unificado||"-"||v_id_unificado;
               
               INSERT INTO safre_viv:uni_preliquida(f_liquida,
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
               
               
               UPDATE uni_det_complementario
               SET    diagnostico = 3
               WHERE  id_complementario = v_id_complementario;
            
               LET v_i_registros_insertados = v_i_registros_insertados + 1;        
                                                    
            END IF;
         END IF;
      END FOREACH;
      
      --trace "INSERTA EN uni_complementario";
      IF v_i_registros_insertados >= 1 THEN
         INSERT INTO uni_complementario(id_complementario,
                                        id_referencia    ,    --- id unificado
                                        folio_unificacion,    --- id unificador
                                        folio_liquidacion,
                                        f_liquidacion)
                                 VALUES(seq_uni_complementario.NEXTVAL,
                                        v_id_unificado,      
                                        v_id_unificador,
                                        v_folio_liquida,           
                                        TODAY);
         
      END IF;
      
      LET v_i_registros_insertados = 0;
   END FOREACH;
   -- se actualiza glo_folio
   UPDATE glo_folio
   SET    status = 1,
          folio_referencia = p_folio
   WHERE  folio = v_folio_liquida
   AND    opera_cod = v_opera_cod
   AND    proceso_cod = p_proceso_cod;

   UPDATE STATISTICS FOR TABLE uni_preliquida;
   UPDATE STATISTICS FOR TABLE uni_det_complementario;
   UPDATE STATISTICS FOR TABLE uni_complementario;
  
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado,
        v_i_registros_insertados,
        v_folio_liquida, 
        isam_err, 
        err_txt;
END FUNCTION;


