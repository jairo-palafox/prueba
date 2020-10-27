






CREATE FUNCTION "safreviv".fn_uni_reversa_preliquida_nvo_mod(p_folio_unificacion DECIMAL(10),
                                                  p_pid               DECIMAL(9,0),
                                                  p_proceso_cod       DECIMAL(9,0))
   RETURNING SMALLINT, 
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
DEFINE v_preliq_origen                 CHAR(13); --Se cambia tamaño de variable para almacenar NSS
-- detalle uni_det_unificador
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_folio_unificacion_unificador  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
DEFINE v_nss_unificador                CHAR(11);
DEFINE v_curp_unificador               CHAR(18);
DEFINE v_curp_afi                      CHAR(18);
-- detalle uni_det_unificado
DEFINE v_nsscta1                       CHAR(11);
DEFINE v_id_unificado                  DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0);
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
DEFINE v_movimiento_abono              SMALLINT; -- clave de movimiento abono
DEFINE v_movimiento_cargo              SMALLINT; -- clave de movimiento cargo
 --                                    
DEFINE v_i_registros_insertados        INTEGER;
DEFINE v_si_estado                     SMALLINT;
DEFINE g_proceso_cod                   SMALLINT; -- codigo del proceso
DEFINE g_opera_cod                     SMALLINT; -- codigo de operacion
DEFINE v_nombre_archivo                CHAR(40);
DEFINE v_id_dh_curp                    CHAR(18);

-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  SMALLINT;

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
    
      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION
    
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/preliquida_uni_nm.trace";
   --TRACE ON;
    
    LET g_proceso_cod            = p_proceso_cod;
    LET v_curp_unificador        = "";
    LET v_curp_afi               = "";
    LET g_opera_cod              = 3;
    LET v_nombre_archivo         = "NA";
    LET v_i_resultado            = 0;
    LET v_i_registros_insertados = 0;
    LET isam_err                 = 0;
    LET err_txt                  = "Proceso finalizado sin error";
    LET v_acciones               = 0.00;
    LET v_pesos                  = 0.00;
    LET v_movimiento_cargo       = 392; -- Movimiento cargo UNI solo IMSS
    LET v_movimiento_abono       = 151; -- Movimiento abono UNI solo IMSS
    
    SELECT folio_referencia 
    INTO   v_folio_liquida 
    FROM   glo_folio 
    WHERE  folio = p_folio_unificacion;

    DELETE FROM glo_folio 
    WHERE  folio = v_folio_liquida;

    UPDATE glo_folio 
    SET    status = 0, 
           folio_referencia = NULL
    WHERE  folio = p_folio_unificacion;

   FOREACH
      SELECT id_derechohabiente, 
             id_referencia
      INTO   v_id_derechohabiente_unificador,
             v_id_unificador
      FROM   uni_preliquida
      WHERE  folio_liquida = v_folio_liquida
      AND    movimiento    = v_movimiento_abono
      GROUP BY 1,2
      
      SELECT id_derechohabiente
      INTO   v_id_dh_curp
      FROM   afi_his_derechohabiente
      WHERE  id_derechohabiente = v_id_derechohabiente_unificador
      AND    folio_lote_modifica = p_folio_unificacion;

      IF v_id_dh_curp IS NOT NULL THEN          
         DELETE FROM afi_his_derechohabiente 
         WHERE  id_derechohabiente = v_id_derechohabiente_unificador
         AND    folio_lote_modifica = p_folio_unificacion;
         
         --Actualiza CURP en afi_derechohabiente 
         UPDATE afi_derechohabiente 
         SET    curp = v_curp_unificador
         WHERE  id_derechohabiente = v_id_derechohabiente_unificador;
      END IF   
         
      FOREACH
         SELECT id_derechohabiente, 
                id_referencia
         INTO   v_id_derechohabiente_unificado,
                v_id_unificado
         FROM   uni_preliquida
         WHERE  folio_liquida = v_folio_liquida
         AND    movimiento    = v_movimiento_cargo
         GROUP BY 1,2

         UPDATE safre_viv:uni_det_unificado           
         SET    diagnostico = 30 -- Preliquidado       
         WHERE  id_unificado = v_id_unificado
         AND    diagnostico = 3;
                                                      
         UPDATE safre_viv:uni_det_unificador          
         SET    diagnostico = 30 -- Preliquidado       
         WHERE  id_unificador = v_id_unificador
         AND    diagnostico = 3;
         
      END FOREACH         
   END FOREACH;

   DELETE FROM uni_preliquida 
   WHERE  folio_liquida = v_folio_liquida;

   --Actualiza diagnósticos a integrado
   UPDATE safre_viv:uni_det_unificado
   SET    diagnostico        = 30
   WHERE  folio_unificacion  = p_folio_unificacion
   AND    estado_unificacion = 1
   AND    diagnostico        = 3;

   UPDATE safre_viv:uni_det_unificado
   SET    diagnostico        = 30
   WHERE  folio_unificacion  IN (SELECT folio_unificacion 
                                 FROM   uni_det_procedencia
                                 WHERE  folio_resp_confronta = p_folio_unificacion
                                 AND    ind_procedencia      = 1)
   AND    estado_unificacion = 1
   AND    diagnostico        = 3;

   UPDATE safre_viv:uni_det_unificador
   SET    diagnostico        = 30, -- Liquidado
          folio_liquidacion  = NULL
   WHERE  folio_unificacion  = p_folio_unificacion
   AND    estado_familia     = 1
   AND    estado_unificacion = 1 
   AND    diagnostico        = 3;

   UPDATE safre_viv:uni_det_unificador
   SET    diagnostico        = 30,
          folio_liquidacion  = NULL
   WHERE  folio_unificacion  IN (SELECT folio_unificacion 
                                 FROM   uni_det_procedencia
                                 WHERE  folio_resp_confronta = p_folio_unificacion
                                 AND    ind_procedencia      = 1)
   AND    estado_familia     = 1
   AND    estado_unificacion = 1 
   AND    diagnostico        = 3;   

   -- se actualiza glo_folio
   UPDATE glo_folio
   SET    status = 0,
          folio_referencia = NULL
   WHERE  folio       = v_folio_liquida
   AND    opera_cod   = g_opera_cod
   AND    proceso_cod = g_proceso_cod;

   --Actualiza folio en monitor de procesos
   UPDATE bat_ctr_operacion 
   SET    folio       = NULL
   WHERE  pid         = p_pid
   AND    proceso_cod = p_proceso_cod
   AND    opera_cod   = 3;

   UPDATE STATISTICS FOR TABLE uni_preliquida;

   -- se devuelve el resultado de la operacion
   RETURN v_i_resultado,
          isam_err,
          err_txt;
END FUNCTION;


