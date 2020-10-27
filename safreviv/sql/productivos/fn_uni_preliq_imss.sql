






CREATE FUNCTION "safreviv".fn_uni_preliq_imss(p_folio       DECIMAL(10),
                                   p_usuario_cod CHAR(20), 
                                   p_pid         DECIMAL(9,0),
                                   p_proceso_cod DECIMAL(9,0))
     RETURNING SMALLINT, INTEGER, DECIMAL, INTEGER, VARCHAR(255)
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
                                       
-- Control de Excepciones              
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);
DEFINE v_si_resultado                  SMALLINT;

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
    
      RETURN v_i_resultado, v_i_registros_insertados, 0, isam_err, err_txt;
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
    

   -- # [Generar folio para la preliquidacion unificación de cuentas.]
   EXECUTE FUNCTION fn_genera_folio(g_proceso_cod,
                                    g_opera_cod,
                                    p_usuario_cod)
           INTO v_folio_liquida;

   SELECT id_unificador,
          folio_unificacion,
          id_derechohabiente,
          nss_unificador,
          curp_unificador
   FROM   uni_det_unificador
   WHERE  folio_unificacion  = p_folio
   AND    estado_familia     = 1
   AND    estado_unificacion = 1  
   AND    diagnostico        = 30 -- solo solicitudes confrontado
   UNION
   SELECT id_unificador,
          folio_unificacion,
          id_derechohabiente,
          nss_unificador,
          curp_unificador
   FROM   uni_det_unificador
   WHERE  folio_unificacion IN (SELECT folio_unificacion 
                                FROM   uni_det_procedencia
                                WHERE  folio_resp_confronta = p_folio
                                AND    ind_procedencia      = 1)
   AND    estado_familia     = 1
   AND    estado_unificacion = 1  
   AND    diagnostico        = 30 -- solo solicitudes confrontado
   GROUP BY 1,2,3,4,5  
   INTO  TEMP tmp_uni_det_preliquida;

   -- se obtienen todos los registros de la tabla uni_det_unificador
   --trace "Selecciona los datos de la tabla uni_det_unificador"; 
   FOREACH
      SELECT id_unificador,
             folio_unificacion,
             id_derechohabiente,
             nss_unificador,
             curp_unificador
      INTO   v_id_unificador,
             v_folio_unificacion_unificador,
             v_id_derechohabiente_unificador,
             v_nss_unificador,
             v_curp_unificador
      FROM   tmp_uni_det_preliquida

      IF v_curp_unificador IS NOT NULL THEN 
         SELECT curp
         INTO   v_curp_afi
         FROM   afi_derechohabiente 
         WHERE  id_derechohabiente = v_id_derechohabiente_unificador;

         IF v_curp_afi IS NULL THEN 
            LET v_curp_afi = "CURP_NULA";
         END IF 

         IF v_curp_unificador <> v_curp_afi THEN
            --Inserta CURP original en tabla histórica      
            INSERT INTO afi_his_derechohabiente (id_derechohabiente   ,
                                                 f_modifica           ,  
                                                 folio_lote_modifica  ,
                                                 ind_modifica         , 
                                                 curp
                                                 )
                                         VALUES (
                                                 v_id_derechohabiente_unificador,         
                                                 TODAY,            
                                                 p_folio,
                                                 1,
                                                 v_curp_afi         
                                                 );
            
            --Actualiza CURP en afi_derechohabiente 
            UPDATE afi_derechohabiente 
            SET    curp = v_curp_unificador
            WHERE  id_derechohabiente = v_id_derechohabiente_unificador;
         END IF
      END IF   
         
      FOREACH
         -- Seleccionamos los unificados por cada unificador
         --trace "Selecciona los datos de la tabla uni_det_unificado por cada unificador";
         SELECT id_unificado,
                id_derechohabiente,
                nsscta1
         INTO   v_id_unificado,
                v_id_derechohabiente_unificado,
                v_nsscta1
         FROM   uni_det_unificado
         WHERE  id_unificador = v_id_unificador

          --trace "Toma el saldo del dia para el unificado";
          -- Buscamos si es que tiene saldo la cuenta
          --cambiar por fn_saldo_actual
         FOREACH
            EXECUTE FUNCTION fn_saldo_actual(v_nsscta1,0,TODAY)
                    INTO v_sub_cuenta,
                         v_fondo_inversion,
                         v_acciones,
                         v_pesos
            IF v_sub_cuenta <> 46 THEN
               IF (v_acciones > 0) AND (v_pesos > 0) THEN
                  --trace "Asigna valores a insertar a la tabla de preliquidación unificado";
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
                  
                  --trace "Inserta valores a la tabla preliquidacion unificado";
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
                     

                  LET v_preliq_f_liquida           = TODAY;
                  LET v_preliq_id_derechohabiente  = v_id_derechohabiente_unificador;
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
                  LET v_preliq_origen              = v_nsscta1||'-N';
                     
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
                  
                  LET v_i_registros_insertados = v_i_registros_insertados + 1;
                  
                  -- Actualiza el estado de la solicitud a 3 preliquidado
                  -- con el folio de liquidación y su fecha de liquidación
                  --trace "Actualiza uni_det_unificado";
                  UPDATE safre_viv:uni_det_unificado
                  SET    diagnostico = 3 -- Preliquidado
                  WHERE  id_unificado = v_id_unificado;
                  
                  UPDATE safre_viv:uni_det_unificador    
                  SET    diagnostico = 3 -- Preliquidado 
                  WHERE  id_unificador = v_id_unificador;
               END IF --Tiene saldo
            END IF -- Subcuenta 46
         END FOREACH
      END FOREACH         
   END FOREACH;

   --Actualiza diagnósticos a Liquidado
   UPDATE safre_viv:uni_det_unificado
   SET    diagnostico        = 4 -- Liquidado
   WHERE  folio_unificacion  = p_folio
   AND    estado_unificacion = 1
   AND    diagnostico        = 30;
  
   UPDATE safre_viv:uni_det_unificado
   SET    diagnostico        = 4 -- Liquidado
   WHERE  folio_unificacion  IN (SELECT folio_unificacion 
                                 FROM   uni_det_procedencia
                                 WHERE  folio_resp_confronta = p_folio
                                 AND    ind_procedencia      = 1)
   AND    estado_unificacion = 1
   AND    diagnostico        = 30;

   UPDATE safre_viv:uni_det_unificador
   SET    diagnostico        = 4, -- Liquidado
          folio_liquidacion  = v_folio_liquida
   WHERE  folio_unificacion  = p_folio
   AND    estado_familia     = 1
   AND    estado_unificacion = 1 
   AND    diagnostico        = 30;

   UPDATE safre_viv:uni_det_unificador
   SET    diagnostico        = 4, -- Liquidado
          folio_liquidacion  = v_folio_liquida
   WHERE  folio_unificacion  IN (SELECT folio_unificacion 
                                 FROM   uni_det_procedencia
                                 WHERE  folio_resp_confronta = p_folio
                                 AND    ind_procedencia      = 1)
   AND    estado_familia     = 1
   AND    estado_unificacion = 1 
   AND    diagnostico        = 30;   

   -- se actualiza glo_folio
   UPDATE glo_folio
   SET    status = 1,
          folio_referencia = p_folio
   WHERE  folio       = v_folio_liquida
   AND    opera_cod   = g_opera_cod
   AND    proceso_cod = g_proceso_cod;

   SELECT nom_archivo 
   INTO   v_nombre_archivo
   FROM   bat_ctr_operacion
   WHERE  pid = p_pid
   AND    opera_cod = 1;

   --Actualiza folio en monitor de procesos
   UPDATE bat_ctr_operacion 
   SET    folio       = v_folio_liquida,
          nom_archivo = v_nombre_archivo
   WHERE  pid         = p_pid
   AND    proceso_cod = p_proceso_cod
   AND    opera_cod   = 3;

   UPDATE STATISTICS FOR TABLE uni_preliquida;

   -- se devuelve el resultado de la operacion
   RETURN v_i_resultado,
          v_i_registros_insertados,
          v_folio_liquida,
          isam_err,
          err_txt;
END FUNCTION;


