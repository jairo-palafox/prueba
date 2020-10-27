






CREATE FUNCTION "safreviv".fn_ret_preliquida_rest_pago_vencido_fondo_ahorro(v_folio_liquida    DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

   DEFINE  v_b_paso                          SMALLINT;
   DEFINE  v_id_afi_fondo72                  DECIMAL(10,0);
   DEFINE  v_id_solicitud                    DECIMAL(10,0); 
   DEFINE  v_id_referencia                   DECIMAL(9,0);
   DEFINE  v_importe_rest                    DECIMAL(14,2);   
   DEFINE  v_movimiento_fondo_ahorro_ven     SMALLINT;
   DEFINE  v_movimiento_tanto_adicional_ven  SMALLINT;
   DEFINE  v_movimiento_cargo                SMALLINT;
   DEFINE  v_movimiento_abono                SMALLINT;
   DEFINE  v_origen                          CHAR(20);
   DEFINE  v_subcuenta                       SMALLINT;
   DEFINE  v_conteo                          SMALLINT; -- conteo de coincidencias de solicitante
                                    
   DEFINE  v_resultado_consulta     SMALLINT;
                                    
   DEFINE v_bnd_preli               SMALLINT;
   DEFINE v_estado_solicitud        INTEGER;  
   
   -- banderas para controlar de donde se obtendra el recurso economico
   -- para el retiro

   DEFINE p_id_afi_fondo72_temp    DECIMAL(10,0);
   DEFINE p_subcuenta              SMALLINT;
   DEFINE p_movimiento             SMALLINT;
   DEFINE p_folio                  DECIMAL(10,0);
   DEFINE p_id_solicitud           DECIMAL(10,0);
   DEFINE p_pes_viv72              DECIMAL(14,2);
   DEFINE p_tanto_adicional        DECIMAL(14,2);
   DEFINE p_saldo_diferencia       DECIMAL(14,2);  
   DEFINE v_saldo_viv72_activo     DECIMAL(14,2);

   -- Control de Excepciones
   DEFINE v_si_resultado          SMALLINT;
   DEFINE sql_err                 INTEGER;
   DEFINE isam_err                INTEGER;
   DEFINE err_txt                 VARCHAR(250);
   DEFINE v_c_msj                 VARCHAR(250);

      -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      trace "El error " || v_si_resultado;
      trace "--- " || isam_err;
      trace "--- " || err_txt;
      trace "--- " || v_id_solicitud;
      
      RETURN v_si_resultado, isam_err, err_txt, v_id_solicitud;
   END EXCEPTION

   -- se inician las variables para marca
   LET v_movimiento_fondo_ahorro_ven    = 141;
   LET v_movimiento_tanto_adicional_ven = 601;
   LET v_movimiento_cargo               = 0;
   LET v_movimiento_abono               = 0;
   LET v_origen                     = "RETIRO W";
   LET v_subcuenta                  = 40;
   LET v_estado_solicitud           = 15; -- ACEPTADAS

   LET v_bnd_preli               = 0;
   
   LET v_importe_rest            = 0;
   LET v_id_solicitud            = 0;
   LET v_id_referencia           = 0;
   LET v_resultado_consulta      = 0;
   
   LET p_subcuenta               = 0;
   LET p_movimiento              = 0;
   LET p_folio                   = 0;
   LET p_id_solicitud            = 0;
   LET p_pes_viv72               = 0;
   LET p_tanto_adicional         = 0;
   LET p_saldo_diferencia        = 0;

   -- se asume que el proceso termina bien
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso finalizó exitosamente.';

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ret_preliquidacion_rest_fondo_ahorro.trace';

   -- actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status       = 1 -- preliquidado
   WHERE  folio        = v_folio_liquida
   AND    proceso_cod  = v_proceso_cod
   AND    status       = 0;

   --actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;
   
   -- se leen las solicitudes aprobadas para su preliquidacion
   FOREACH
      SELECT DISTINCT  a.id_solicitud, 
             a.id_afi_fondo72,
             b.importe,
             b.movimiento
      INTO   v_id_solicitud,
             v_id_afi_fondo72,
             v_importe_rest,
             v_movimiento_cargo
      FROM   ret_rest_fondo_ahorro a,
             ret_preliquida72 b
      WHERE  a.estado_solicitud = v_estado_solicitud
      AND    a.folio            = v_folio_liquida
      AND    a.id_sol_cargo     = b.id_referencia
      AND    a.id_afi_fondo72   = b.id_afi_fondo72
     
     
      -- =================================================================
      --  PRELIQUIDA MONTO SOLICITADO         
      -- =================================================================

      
      LET v_importe_rest = v_importe_rest * (-1);
      trace "Importe " || v_importe_rest;   
      -- se preliquida el monto 
      -- movimiento 141 Abono Fondo Ahorro no pagado
            -- Buscamos el id_solicitud para incluirlo en la tabla de movimientos
--      FOREACH
--         SELECT id_solicitud
--         INTO   v_id_referencia
--         FROM   ret_rest_fondo_ahorro
--         WHERE  estado_solicitud = v_estado_solicitud
--         AND    folio            = v_folio_liquida
--         AND    num_documento    = v_id_solicitud
--         ORDER BY id_solicitud
--
--         IF v_id_referencia IS NOT NULL THEN --- Solo procesamos el primero
--            EXIT FOREACH;
--         ELSE
--            EXIT FOREACH;
--         END IF 
--      END FOREACH;
      
      IF v_importe_rest <> 0  THEN
         IF v_movimiento_cargo = 182 OR v_movimiento_cargo = 802 THEN
            LET v_movimiento_abono = v_movimiento_fondo_ahorro_ven;
         ELSE 
            LET v_movimiento_abono = v_movimiento_tanto_adicional_ven;
         END IF
         trace "Movimiento" || v_movimiento_abono;
         INSERT INTO ret_preliquida72 (
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
         VALUES ( 
             v_id_afi_fondo72       , 
             TODAY                  , 
             v_subcuenta            , 
             v_movimiento_abono     ,
             v_folio_liquida        , 
             v_id_solicitud         , 
             v_importe_rest         , 
             NULL                   , 
             TODAY                  , 
             CURRENT HOUR TO SECOND , 
             v_origen
             );
      END IF 
        
      -- cambia el estatus de solicitud a preliquidada
      UPDATE  ret_rest_fondo_ahorro 
      SET     estado_solicitud   = 50
      WHERE   id_solicitud       = v_id_solicitud
      AND     id_afi_fondo72     = v_id_afi_fondo72
      AND     folio              = v_folio_liquida;

      -- se indica que al menos se preliquido un registro
      LET v_bnd_preli = 1;

   END FOREACH; -- para cada solicitud aceptada

   IF ( v_bnd_preli = 0 ) THEN
      -- no se preliquidaron registros
      LET v_si_resultado = 1000;
      LET v_c_msj        = "No se preliquidaron registros. Favor de verificar";
   END IF

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida72;
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, v_id_solicitud;
END FUNCTION;


