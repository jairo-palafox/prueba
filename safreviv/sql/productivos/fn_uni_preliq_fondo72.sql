






CREATE FUNCTION "safreviv".fn_uni_preliq_fondo72(
                                      p_folio          DECIMAL(10) ,      
                                      p_usuario_cod    CHAR(20)    ,         
                                      p_pid            DECIMAL(9,0),     
                                      p_proceso_cod    DECIMAL(9,0)
                                      )
RETURNING SMALLINT, INTEGER, DECIMAL, INTEGER, VARCHAR(255)         

--table de preliquidación
DEFINE v_preliq_id_afi_fondo72_ado DECIMAL(9,0);
DEFINE v_preliq_f_liquida_ado      DATE;
DEFINE v_preliq_subcuenta_ado      SMALLINT;
DEFINE v_preliq_movimiento_ado     SMALLINT;
DEFINE v_preliq_folio_liquida_ado  DECIMAL(9,0);
DEFINE v_preliq_id_referencia_ado  DECIMAL(9,0);
DEFINE v_preliq_importe_ado        DECIMAL(16,6);
DEFINE v_preliq_estado_pago_ado    CHAR(1);
DEFINE v_preliq_f_registro_ado     DATE;
DEFINE v_preliq_h_registro_ado     DATETIME HOUR TO SECOND;
DEFINE v_preliq_origen_ado         CHAR(20);

--table de preliquidación
DEFINE v_preliq_id_afi_fondo72_dor DECIMAL(9,0);
DEFINE v_preliq_f_liquida_dor      DATE;
DEFINE v_preliq_subcuenta_dor      SMALLINT;
DEFINE v_preliq_movimiento_dor     SMALLINT;
DEFINE v_preliq_folio_liquida_dor  DECIMAL(9,0);
DEFINE v_preliq_id_referencia_dor  DECIMAL(9,0);
DEFINE v_preliq_importe_dor        DECIMAL(16,6);
DEFINE v_preliq_estado_pago_dor    CHAR(1);
DEFINE v_preliq_f_registro_dor     DATE;
DEFINE v_preliq_h_registro_dor     DATETIME HOUR TO SECOND;
DEFINE v_preliq_origen_dor         CHAR(20);
--Preunificacion 
DEFINE v_preu_id_afi_fondo72    DECIMAL(9,0);
DEFINE v_preu_nss               CHAR(11);
DEFINE v_preu_rfc               CHAR(13);
DEFINE v_preu_rfc_ado           CHAR(13);
DEFINE v_preu_nombre            CHAR(40);
DEFINE v_preu_fecha_liquidacion DATE;
DEFINE v_preu_folio_liquidacion DECIMAL(9,0);
DEFINE v_preu_movimiento        SMALLINT;
DEFINE v_preu_origen            CHAR(20);
DEFINE v_preu_monto_pesos       DECIMAL(16,6);
DEFINE v_preu_tipo_nss          SMALLINT;
DEFINE v_preu_diagnostico       SMALLINT;

-- detalle uni_det_unificador
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_folio_unificacion_unificador  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
DEFINE v_nss_unificador                CHAR(11);
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
                                       
-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  INTEGER;

-- se configura el regreso del codigo de error
ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_i_resultado = sql_err;    
   LET v_i_registros_insertados = 0;
 
   RETURN v_i_resultado, v_i_registros_insertados, 0, isam_err, err_txt;
END EXCEPTION
 
   --SET DEBUG FILE TO "/safreviv_int/BD/trace_uni_preliquida_fondo72.txt";  
   --TRACE ON;

LET g_proceso_cod            = p_proceso_cod;
LET g_opera_cod              = 1;
LET v_i_resultado            = 0;
LET v_i_registros_insertados = 0;
LET isam_err                 = 0;
LET err_txt                  = "Proceso finalizado sin error";
LET v_acciones               = 0.00;
LET v_pesos                  = 0.00;
LET v_movimiento_cargo       = 392; -- Movimiento cargo UNI solo IMSS
LET v_movimiento_abono       = 151; -- Movimiento abono UNI solo IMSS
LET v_sub_cuenta             = 40;

LET v_preliq_id_afi_fondo72_ado = 0;   
LET v_preliq_f_liquida_ado      = NULL;
LET v_preliq_subcuenta_ado      = 0;   
LET v_preliq_movimiento_ado     = 0;   
LET v_preliq_folio_liquida_ado  = 0;   
LET v_preliq_id_referencia_ado  = 0;   
LET v_preliq_importe_ado        = 0.00;
LET v_preliq_estado_pago_ado    = "";  
LET v_preliq_f_registro_ado     = NULL;
LET v_preliq_h_registro_ado     = NULL;
LET v_preliq_origen_ado         = "";  

LET v_preliq_id_afi_fondo72_dor = 0;   
LET v_preliq_f_liquida_dor      = NULL;
LET v_preliq_subcuenta_dor      = 0;   
LET v_preliq_movimiento_dor     = 0;   
LET v_preliq_folio_liquida_dor  = 0;   
LET v_preliq_id_referencia_dor  = 0;   
LET v_preliq_importe_dor        = 0.00;
LET v_preliq_estado_pago_dor    = "";  
LET v_preliq_f_registro_dor     = NULL;
LET v_preliq_h_registro_dor     = NULL;
LET v_preliq_origen_dor         = "";  

--Preunificacion 
LET v_preu_id_afi_fondo72    = 0;
LET v_preu_nss               = "";
LET v_preu_rfc               = "";
LET v_preu_rfc_ado           = "";
LET v_preu_nombre            = "";
LET v_preu_fecha_liquidacion = NULL;
LET v_preu_folio_liquidacion = 0;
LET v_preu_movimiento        = 0;
LET v_preu_origen            = "";
LET v_preu_monto_pesos       = 0;
LET v_preu_tipo_nss          = 0;
LET v_preu_diagnostico       = 0;


   -- se obtienen todos los registros de la tabla uni_det_unificador
   --trace "Selecciona los datos de la tabla uni_det_unificador"; 
   FOREACH
      SELECT id_afi_fondo72,
             nss,
             rfc,
             nombre,
             fecha_liquidacion, 
             folio_liquidacion, 
             movimiento,
             origen,
             monto_pesos,
             tipo_nss,
             diagnostico
      INTO   v_preu_id_afi_fondo72,
             v_preu_nss,
             v_preu_rfc_ado,
             v_preu_nombre,
             v_preu_fecha_liquidacion, 
             v_preu_folio_liquidacion, 
             v_preu_movimiento,
             v_preu_origen,
             v_preu_monto_pesos,
             v_preu_tipo_nss,
             v_preu_diagnostico
      FROM   uni_preunifica_fondo72
      WHERE  folio_preunifica = p_folio
      AND    diagnostico = 2
      AND    tipo_nss = 2

      --Si es UNIFICADO
      --Cargo - Quita
      --Abono + Pone      
      IF v_preu_tipo_nss = 2 THEN 
         --trace "Asigna valores a insertar a la tabla de preliquidación unificado";
         LET v_preliq_id_afi_fondo72_ado = v_preu_id_afi_fondo72;
         LET v_preliq_f_liquida_ado      = TODAY;
         LET v_preliq_subcuenta_ado      = v_sub_cuenta;
         LET v_preliq_movimiento_ado     = v_movimiento_cargo;
         LET v_preliq_folio_liquida_ado  = p_folio;
         LET v_preliq_id_referencia_ado  = 0;
         LET v_preliq_importe_ado        = (v_preu_monto_pesos * -1);
         LET v_preliq_estado_pago_ado    = 0;
         LET v_preliq_f_registro_ado     = TODAY;
         LET v_preliq_h_registro_ado     = CURRENT HOUR TO SECOND;
         LET v_preliq_origen_ado         = (v_preu_rfc_ado || '-2');

         --trace "Inserta valores a la tabla preliquidacion unificado";
         INSERT INTO uni_preliquida_fondo72
                     (
                      id_afi_fondo72,
                      f_liquida     ,
                      subcuenta     ,
                      movimiento    ,
                      folio_liquida ,
                      id_referencia ,
                      importe       ,
                      estado_pago   ,
                      f_registro    ,
                      h_registro    ,
                      origen
                     )
          VALUES     (
                      v_preliq_id_afi_fondo72_ado,
                      v_preliq_f_liquida_ado,
                      v_preliq_subcuenta_ado,
                      v_preliq_movimiento_ado,
                      v_preliq_folio_liquida_ado,
                      v_preliq_id_referencia_ado,
                      v_preliq_importe_ado,
                      v_preliq_estado_pago_ado,
                      v_preliq_f_registro_ado,
                      v_preliq_h_registro_ado,
                      v_preliq_origen_ado);

         FOREACH 
            --Consulta el unificador
            SELECT id_afi_fondo72,
                   nss,
                   rfc
            INTO   v_preu_id_afi_fondo72,
                   v_preu_nss,
                   v_preu_rfc
            FROM   uni_preunifica_fondo72
            WHERE  folio_preunifica = p_folio
            AND    diagnostico = 2
            AND    tipo_nss = 1
         END FOREACH
         
         --trace "Asigna valores a insertar a la tabla de preliquidación unificado";
         LET v_preliq_id_afi_fondo72_dor  = v_preu_id_afi_fondo72;
         LET v_preliq_f_liquida_dor       = TODAY;
         LET v_preliq_subcuenta_dor       = v_sub_cuenta;
         LET v_preliq_movimiento_dor      = v_movimiento_abono;
         LET v_preliq_folio_liquida_dor   = p_folio;
         LET v_preliq_id_referencia_dor   = 0;
         LET v_preliq_importe_dor         = v_preu_monto_pesos;
         LET v_preliq_estado_pago_dor     = 0;
         LET v_preliq_f_registro_dor      = TODAY;
         LET v_preliq_h_registro_dor      = CURRENT HOUR TO SECOND;
         LET v_preliq_origen_dor          = (v_preu_rfc_ado ||'-1');
         
         --trace "Inserta valores a la tabla preliquidacion unificado";
         INSERT INTO uni_preliquida_fondo72
                     (
                      id_afi_fondo72,
                      f_liquida     ,
                      subcuenta     ,
                      movimiento    ,
                      folio_liquida ,
                      id_referencia ,
                      importe       ,
                      estado_pago   ,
                      f_registro    ,
                      h_registro    ,
                      origen
                     )
         VALUES     (
                     v_preliq_id_afi_fondo72_dor,
                     v_preliq_f_liquida_dor,
                     v_preliq_subcuenta_dor,
                     v_preliq_movimiento_dor,
                     v_preliq_folio_liquida_dor,
                     v_preliq_id_referencia_dor,
                     v_preliq_importe_dor,
                     v_preliq_estado_pago_dor,
                     v_preliq_f_registro_dor,
                     v_preliq_h_registro_dor,
                     v_preliq_origen_dor
                     );
         
         -- Actualiza el diagnostico a 3 preliquidado
         -- con el folio de liquidación y su fecha de liquidación
         UPDATE uni_preunifica_fondo72
         SET    diagnostico    = 3 -- Preliquidado
         WHERE  id_afi_fondo72 = v_preliq_id_afi_fondo72_ado
         AND    tipo_nss       = 2
         AND    diagnostico    = 2 ;
         
         LET v_preliq_origen_ado = v_preu_rfc ||'- 2';
         
         --Actualiza el orígen del UNIFICADO 
         UPDATE uni_preliquida_fondo72
         SET    origen        = v_preliq_origen_ado
         WHERE  folio_liquida = p_folio
         AND    movimiento    = 392;
         
         -- Se inhabilita la cuenta en afi_fondo72  
         UPDATE afi_fondo72
         SET    ind_estado_cuenta = 1,
                f_estado_cuenta = TODAY
         WHERE  id_afi_fondo72 = v_preliq_id_afi_fondo72_ado;
         
         LET v_i_registros_insertados = v_i_registros_insertados + 1;

      END IF;
   END FOREACH;      

   --Se actualiza el orígen del UNIFICADOR
   UPDATE uni_preunifica_fondo72
   SET    diagnostico    = 3 -- Preliquidado
   WHERE  id_afi_fondo72 = v_preliq_id_afi_fondo72_ado
   AND    tipo_nss       = 1
   AND    diagnostico    = 2 ;

   --Se actualiza folio en monitor de procesos
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = "NA"
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 1
   AND    pid         = p_pid;      

    -- se actualiza glo_folio
   UPDATE glo_folio
   SET    status = 1,
          folio_referencia = p_folio
   WHERE  folio = p_folio
   AND    opera_cod = g_opera_cod
   AND    proceso_cod = g_proceso_cod;

   UPDATE statistics FOR TABLE uni_preunifica_fondo72;     
   UPDATE statistics FOR TABLE uni_preliquida_fondo72;

   -- se devuelve el resultado de la operacion
   RETURN v_i_resultado,
          v_i_registros_insertados,
          p_folio,
          isam_err,
          err_txt;
END FUNCTION;


