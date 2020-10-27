






CREATE FUNCTION "safreviv".fn_insert_preliquidacion_retiro_fondo_ahorro_cont(v_folio_liquida    DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

   DEFINE  v_b_paso                      SMALLINT;
   DEFINE  v_id_afi_fondo72              DECIMAL(9,0);
   DEFINE  v_id_afi_fondo72_temp         DECIMAL(9,0);
   DEFINE  v_id_derechohabiente          DECIMAL(9,0);
   DEFINE  v_id_solicitud                DECIMAL(9,0); 
   DEFINE  v_importe                     DECIMAL(14,2);   
   DEFINE  v_saldo_del_trabajador        DECIMAL(14,2); -- saldo del trabajador en cta_fondo72
   DEFINE  v_tanto_adicional_solicitud   DECIMAL(14,2); -- tanto adicional de la solicitud
   DEFINE  v_saldo_viv72_solicitado      DECIMAL(14,2);
   DEFINE  p_saldo_viv72_solicitado      DECIMAL(14,2);
   DEFINE  v_saldo_viv72_solicitado_orig DECIMAL(14,2);
   DEFINE  v_saldo_viv72                 DECIMAL(14,2);
   DEFINE  v_id_afi_saldo_viv72          DECIMAL(14,2);
   DEFINE  v_saldo_diferencia            DECIMAL(14,2);
   DEFINE  v_movimiento_fondo_ahorro     SMALLINT;
   DEFINE  v_movimiento_tanto_adicional  SMALLINT;
   DEFINE  v_movimiento_sobregiro        SMALLINT;
   DEFINE  v_valor_mov                   SMALLINT;
   DEFINE  v_origen                      CHAR(20);
   DEFINE  v_subcuenta                   SMALLINT;
   DEFINE  v_conteo                 SMALLINT; -- conteo de coincidencias de solicitante
                                    
   DEFINE  v_resultado_consulta     SMALLINT;
                                    
   DEFINE v_i_estado_marca          INTEGER;
   DEFINE v_marca_fondo_ahorro      INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_bnd_preli               SMALLINT;
   DEFINE v_nss                     CHAR(12);
   DEFINE r_tanto_adicional         DECIMAL(14,2);
   DEFINE v_estado_solicitud        INTEGER;  
   
   DEFINE v_bnd_saldo_igual         SMALLINT;
   DEFINE v_bnd_saldo_id_igual      SMALLINT;
   DEFINE v_bnd_saldo_id_diferencia SMALLINT;
   
   -- banderas para controlar de donde se obtendra el recurso economico
   -- para el retiro
   DEFINE v_el_saldo_es_igual      SMALLINT;
   DEFINE v_saldo_entre_varios_afi SMALLINT;
   DEFINE v_saldo_con_sobregiro    SMALLINT;

   DEFINE p_id_afi_fondo72_temp    DECIMAL(9,0);
   DEFINE p_subcuenta              SMALLINT;
   DEFINE p_movimiento             SMALLINT;
   DEFINE p_folio                  DECIMAL(9,0);
   DEFINE p_id_solicitud           DECIMAL(9,0);
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
      
      RETURN v_si_resultado, isam_err, err_txt, v_id_solicitud;
   END EXCEPTION

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro         = 802; -- marca para retiro de fondo ahorro
   LET v_movimiento_fondo_ahorro    = 182;
   LET v_movimiento_tanto_adicional = 422;
   LET v_movimiento_sobregiro       = 802;
   LET v_origen                     = "RETIRO W";
   LET v_subcuenta                  = 40;
   LET v_estado_solicitud           = 15; -- ACEPTADAS

   LET v_i_estado_marca          = 0;
   LET v_bnd_preli               = 0;
   LET v_b_paso                  = 0;
   LET v_bnd_saldo_igual         = 0;
   LET v_bnd_saldo_id_igual      = 0;
   LET v_bnd_saldo_id_diferencia = 0;    
   
   LET v_importe                 = 0;
   LET v_saldo_viv72_activo      = 0;
   LET v_id_afi_fondo72          = 0;
   LET v_id_afi_fondo72_temp     = 0;
   LET v_id_derechohabiente      = 0;
   LET v_id_solicitud            = 0;
   LET v_saldo_del_trabajador    = 0; -- saldo que el trabajador tiene
   LET v_resultado_consulta      = 0;
   LET v_saldo_viv72_solicitado  = 0;
   LET p_saldo_viv72_solicitado  = 0;
   LET v_saldo_viv72_solicitado_orig  = 0;
   LET v_saldo_viv72             = 0; 
   LET v_id_afi_saldo_viv72      = 0; 
   LET v_saldo_diferencia        = 0;   
   
   LET p_id_afi_fondo72_temp     = 0;
   LET p_subcuenta               = 0;
   LET p_movimiento              = 0;
   LET p_folio                   = 0;
   LET p_id_solicitud            = 0;
   LET p_pes_viv72               = 0;
   LET p_tanto_adicional         = 0;
   LET p_saldo_diferencia        = 0;

   -- se inician las banderas del recurso economico   
   LET v_el_saldo_es_igual      = 0;
   LET v_saldo_entre_varios_afi = 0;
   LET v_saldo_con_sobregiro    = 0;

   -- se asume que el proceso termina bien
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso finalizó exitosamente.';

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_fondo_ahorro_cont.log';

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
   
   -- se obtiene el signo del movimiento de retiro fondo de ahorro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_fondo_ahorro ;

   -- se leen las solicitudes aprobadas para su preliquidacion
   FOREACH
   SELECT id_solicitud    ,
          saldo_viv72     ,
          tanto_adicional
   INTO   v_id_solicitud             , -- id de la solicitud
          v_saldo_viv72_solicitado   , -- saldo solicitado
          v_tanto_adicional_solicitud  -- tanto adicional
   FROM   ret_fondo_ahorro
   WHERE  estado_solicitud = v_estado_solicitud
   AND    folio            = v_folio_liquida

      -- se asigna cuanto se solicita originalmente
      LET v_saldo_viv72_solicitado_orig = v_saldo_viv72_solicitado;          
      LET v_nss                         = NULL ;
      
      -- se verifica cuantas veces aparece el solicitante en la tabla de coincidencias
      SELECT COUNT(*)
      INTO   v_conteo
      FROM   ret_det_fondo72
      WHERE  id_solicitud   = v_id_solicitud
      AND    estado_detalle = 1;
      
      -- =========================================================
      -- EL TRABAJADOR APARECE UNA SOLA VEZ
      -- Regla
      -- * si su saldo es suficiente se liquida
      -- * si su saldo es insuficiente, se liquida y sobregira la diferencia
      -- * si tiene tanto adicional, se liquida
      IF ( v_conteo = 1 ) THEN
         -- se obtiene el saldo que tiene
         SELECT det.saldo_viv72   ,
                det.id_afi_fondo72
         INTO   v_saldo_del_trabajador, -- monto que tiene el trabajador
                v_id_afi_fondo72        -- id_afi_fondo72
         FROM  ret_det_fondo72  det
         WHERE det.id_solicitud     = v_id_solicitud
         AND   det.estado_detalle   = 1;
         
          
         -- =================================================================
         --  PRELIQUIDA MONTO POSEIDO         
         -- =================================================================
         
         -- si el saldo del trabajador es igual o menor al solicitado, se preliquida el saldo
         -- en otro caso se preliquida lo solicitado
         IF ( v_saldo_del_trabajador <= v_saldo_viv72_solicitado ) THEN
            LET v_importe = v_saldo_del_trabajador * v_valor_mov;
         ELSE
            LET v_importe = v_saldo_viv72_solicitado * v_valor_mov;
         END IF
         
         -- se preliquida el monto 
         -- movimiento 182 Retiro Fondo Ahorro
         IF v_importe <> 0 THEN
             INSERT INTO ret_preliquida72 (
                id_afi_fondo72 ,
                f_liquida      ,
                subcuenta      ,
                movimiento     ,
                folio_liquida  ,
                id_referencia  ,
                importe        ,
                estado_pago    ,
                f_registro     ,
                h_registro     ,
                origen          
                )
             VALUES ( 
                v_id_afi_fondo72         , 
                TODAY                    , 
                v_subcuenta              , 
                v_movimiento_fondo_ahorro, 
                v_folio_liquida          , 
                v_id_solicitud           , 
                v_importe                , 
                NULL                     , 
                TODAY                    , 
                CURRENT HOUR TO SECOND   , 
                v_origen
             );
         END IF
         -- si el saldo poseido no es suficiente para cubrir el retiro
         IF ( v_saldo_viv72_solicitado > v_saldo_del_trabajador ) THEN
            -- se obtiene la diferencia y se sobregira
            LET v_importe = (v_saldo_viv72_solicitado - v_saldo_del_trabajador) * v_valor_mov;
         
            -- se preliquida la diferencia con un sobregiro
            INSERT INTO ret_preliquida72 (
               id_afi_fondo72 ,
               f_liquida      ,
               subcuenta      ,
               movimiento     ,
               folio_liquida  ,
               id_referencia  ,
               importe        ,
               estado_pago    ,
               f_registro     ,
               h_registro     ,
               origen          
               )
            VALUES ( 
               v_id_afi_fondo72         , 
               TODAY                    , 
               v_subcuenta              , 
               v_movimiento_sobregiro   , 
               v_folio_liquida          , 
               v_id_solicitud           , 
               v_importe                , 
               NULL                     , 
               TODAY                    , 
               CURRENT HOUR TO SECOND   , 
               v_origen
            );         
         END IF -- se sobregiro la cuenta
        
      ELSE
         -- =================================================================
         -- EL TRABAJADOR APARECE MAS DE UNA VEZ
         -- Regla:
         -- 2.1 Si el monto solicitado coincide con el saldo de alguno de los id afi 72,
         --     a ese id se le aplicará el cargo, dejando la cuenta en cero.

         LET v_saldo_del_trabajador = NULL;
         LET v_id_afi_fondo72       = NULL;

         -- se busca si alguno de los ids encontrados coincide exactamente con el monto solicitado
         SELECT FIRST 1 det.saldo_viv72,
                det.id_afi_fondo72
         INTO   v_saldo_del_trabajador, -- monto que tiene el trabajador
                v_id_afi_fondo72        -- id_afi_fondo72
         FROM  ret_det_fondo72  det
         WHERE det.id_solicitud     = v_id_solicitud
         AND   det.estado_detalle   = 1
         AND   det.saldo_viv72      = v_saldo_viv72_solicitado;
         
         -- si se encontro la busqueda exacta, se preliquida con el id_afi encontrado
         IF ( v_id_afi_fondo72 IS NOT NULL ) THEN
            -- se calcula el importe
            LET v_importe = v_saldo_viv72_solicitado * v_valor_mov;
            
            -- se preliquida el monto 
            -- movimiento 182 Retiro Fondo Ahorro
            INSERT INTO ret_preliquida72 (
               id_afi_fondo72 ,
               f_liquida      ,
               subcuenta      ,
               movimiento     ,
               folio_liquida  ,
               id_referencia  ,
               importe        ,
               estado_pago    ,
               f_registro     ,
               h_registro     ,
               origen          
               )
            VALUES ( 
               v_id_afi_fondo72         , 
               TODAY                    , 
               v_subcuenta              , 
               v_movimiento_fondo_ahorro, 
               v_folio_liquida          , 
               v_id_solicitud           , 
               v_importe                , 
               NULL                     , 
               TODAY                    , 
               CURRENT HOUR TO SECOND   , 
               v_origen
            );

         ELSE
         
            -- 2.2 Si el monto solicitado es igual a la suma de los saldos de los id identificados,
            --     se liquidarán los saldos a cada uno de los id afi 72 encontrados.
            SELECT SUM(det.saldo_viv72)
            INTO   v_saldo_viv72_activo
            FROM  ret_det_fondo72  det
            WHERE det.id_solicitud     = v_id_solicitud
            AND   det.estado_detalle   = 1;
            
            -- se verifica si la suma es igual al monto solicitado
            IF ( v_saldo_viv72_solicitado = v_saldo_viv72_activo ) THEN
               -- se preliquidan todos los registros
               FOREACH
               SELECT det.saldo_viv72   ,
                      det.id_afi_fondo72
               INTO   v_saldo_del_trabajador, -- monto que tiene el trabajador
                      v_id_afi_fondo72        -- id_afi_fondo72
               FROM  ret_det_fondo72  det
               WHERE det.id_solicitud     = v_id_solicitud
               AND   det.estado_detalle   = 1
               
                  -- se calcula el importe a preliquidar
                  LET v_importe = v_saldo_del_trabajador * v_valor_mov;
               
                  -- PRELIQUIDACION DEL MONTO
                  -- movimiento 182 Retiro Fondo Ahorro
                  INSERT INTO ret_preliquida72 (
                     id_afi_fondo72 ,
                     f_liquida      ,
                     subcuenta      ,
                     movimiento     ,
                     folio_liquida  ,
                     id_referencia  ,
                     importe        ,
                     estado_pago    ,
                     f_registro     ,
                     h_registro     ,
                     origen          
                     )
                  VALUES ( 
                     v_id_afi_fondo72         , 
                     TODAY                    , 
                     v_subcuenta              , 
                     v_movimiento_fondo_ahorro, 
                     v_folio_liquida          , 
                     v_id_solicitud           , 
                     v_importe                , 
                     NULL                     , 
                     TODAY                    , 
                     CURRENT HOUR TO SECOND   , 
                     v_origen
                  );
               END FOREACH;
            ELSE -- la suma no es igual
               -- SE PROCEDE A REVISAR SI LA SUMA ES MAYOR O MENOR AL MONTO SOLICITADO
               
               -- 2.4 Si la suma de los saldos de id identificados es mayor al monto solicitado,
               --     se irán afectando los montos solicitados teniendo como máximo el saldo del id fondo 72.
               IF ( v_saldo_viv72_activo > v_saldo_viv72_solicitado ) THEN
                  
                  -- se iran preliquidando los montos hasta llegar al monto solicitado
                  LET p_saldo_diferencia = v_saldo_viv72_solicitado;
                  
                  -- se preliquidan todos los registros
                  FOREACH
                  SELECT det.saldo_viv72   ,
                         det.id_afi_fondo72
                  INTO   v_saldo_del_trabajador, -- monto que tiene el trabajador
                         v_id_afi_fondo72        -- id_afi_fondo72
                  FROM  ret_det_fondo72  det
                  WHERE det.id_solicitud     = v_id_solicitud
                  AND   det.estado_detalle   = 1
                  ORDER BY det.saldo_viv72 DESC
                  
                     -- se verifica si la diferencia es mayor al saldo recuperado
                     IF ( p_saldo_diferencia > v_saldo_del_trabajador ) THEN
                        
                        -- se preliquida el saldo del trabajador
                        
                        -- se calcula el importe a preliquidar
                        LET v_importe = v_saldo_del_trabajador * v_valor_mov;
                        
                        -- PRELIQUIDACION DEL MONTO
                        -- movimiento 182 Retiro Fondo Ahorro
                        INSERT INTO ret_preliquida72 (
                           id_afi_fondo72 ,
                           f_liquida      ,
                           subcuenta      ,
                           movimiento     ,
                           folio_liquida  ,
                           id_referencia  ,
                           importe        ,
                           estado_pago    ,
                           f_registro     ,
                           h_registro     ,
                           origen          
                           )
                        VALUES ( 
                           v_id_afi_fondo72         , 
                           TODAY                    , 
                           v_subcuenta              , 
                           v_movimiento_fondo_ahorro, 
                           v_folio_liquida          , 
                           v_id_solicitud           , 
                           v_importe                , 
                           NULL                     , 
                           TODAY                    , 
                           CURRENT HOUR TO SECOND   , 
                           v_origen
                        );
                     ELSE
                        -- se preliquida la diferencia con el id_afi_fondo72
                        -- y es el ultimo registro que se preliquida pues ya se ha completado el retiro
                        -- se calcula el importe a preliquidar
                        LET v_importe = p_saldo_diferencia * v_valor_mov;
                        
                        -- PRELIQUIDACION DEL MONTO
                        -- movimiento 182 Retiro Fondo Ahorro
                        INSERT INTO ret_preliquida72 (
                           id_afi_fondo72 ,
                           f_liquida      ,
                           subcuenta      ,
                           movimiento     ,
                           folio_liquida  ,
                           id_referencia  ,
                           importe        ,
                           estado_pago    ,
                           f_registro     ,
                           h_registro     ,
                           origen          
                           )
                        VALUES ( 
                           v_id_afi_fondo72         , 
                           TODAY                    , 
                           v_subcuenta              , 
                           v_movimiento_fondo_ahorro, 
                           v_folio_liquida          , 
                           v_id_solicitud           , 
                           v_importe                , 
                           NULL                     , 
                           TODAY                    , 
                           CURRENT HOUR TO SECOND   , 
                           v_origen
                        );
                        
                        -- ya no se revisan mas registros pues ya se cumplio
                        EXIT FOREACH;
                     END IF
                  
                     -- se calcula la nueva diferencia por preliquidar
                     -- DIFERENCIA = DIFERENCIA ANTERIOR - SALDO DEL TRABAJADOR YA PRELIQUIDADO
                     LET p_saldo_diferencia = p_saldo_diferencia - v_saldo_del_trabajador;
                  END FOREACH;

               ELSE
                  -- 2.3 Si la suma de los saldos de los id identificados es menor al monto solicitado,
                  --     se liquidará el saldo de cada uno de los id identificados,
                  --     y el restante como un sobregiro a cualquiera de los id registrados.

                  -- se iran preliquidando los montos hasta llegar al monto solicitado
                  LET p_saldo_diferencia = v_saldo_viv72_solicitado;
                  
                  -- se preliquidan todos los registros
                  FOREACH
                  SELECT det.saldo_viv72   ,
                         det.id_afi_fondo72
                  INTO   v_saldo_del_trabajador, -- monto que tiene el trabajador
                         v_id_afi_fondo72        -- id_afi_fondo72
                  FROM  ret_det_fondo72  det
                  WHERE det.id_solicitud     = v_id_solicitud
                  AND   det.estado_detalle   = 1
                  ORDER BY det.saldo_viv72 DESC
                                         
                     -- se calcula el importe a preliquidar
                     LET v_importe = v_saldo_del_trabajador * v_valor_mov;
                     
                     -- PRELIQUIDACION DEL MONTO
                     -- movimiento 182 Retiro Fondo Ahorro
                     INSERT INTO ret_preliquida72 (
                        id_afi_fondo72 ,
                        f_liquida      ,
                        subcuenta      ,
                        movimiento     ,
                        folio_liquida  ,
                        id_referencia  ,
                        importe        ,
                        estado_pago    ,
                        f_registro     ,
                        h_registro     ,
                        origen          
                        )
                     VALUES ( 
                        v_id_afi_fondo72         , 
                        TODAY                    , 
                        v_subcuenta              , 
                        v_movimiento_fondo_ahorro, 
                        v_folio_liquida          , 
                        v_id_solicitud           , 
                        v_importe                , 
                        NULL                     , 
                        TODAY                    , 
                        CURRENT HOUR TO SECOND   , 
                        v_origen
                     );

                     -- se calcula la nueva diferencia por preliquidar
                     -- DIFERENCIA = DIFERENCIA ANTERIOR - SALDO DEL TRABAJADOR YA PRELIQUIDADO
                     LET p_saldo_diferencia = p_saldo_diferencia - v_saldo_del_trabajador;
                  END FOREACH;

                  -- se preliquida la diferencia con el id_afi_fondo72 final con un movimiento de sobregiro
                  LET v_importe = p_saldo_diferencia * v_valor_mov;
                  
                  -- PRELIQUIDACION DEL MONTO
                  -- movimiento 182 Retiro Fondo Ahorro
                  INSERT INTO ret_preliquida72 (
                     id_afi_fondo72 ,
                     f_liquida      ,
                     subcuenta      ,
                     movimiento     ,
                     folio_liquida  ,
                     id_referencia  ,
                     importe        ,
                     estado_pago    ,
                     f_registro     ,
                     h_registro     ,
                     origen          
                     )
                  VALUES ( 
                     v_id_afi_fondo72         , 
                     TODAY                    , 
                     v_subcuenta              , 
                     v_movimiento_sobregiro   , 
                     v_folio_liquida          , 
                     v_id_solicitud           , 
                     v_importe                , 
                     NULL                     , 
                     TODAY                    , 
                     CURRENT HOUR TO SECOND   , 
                     v_origen
                  );
               END IF -- la suma de los montos es mayor o menor
         
            END IF -- la suma de los montos no es igual
         END IF -- el trabajador aparece mas de una vez y el uno de los montos es exacto
      
      END IF -- el trabajador aparece una vez

      -- ==============================================================
      -- PRELIQUIDACION DEL TANTO ADICIONAL
      
      -- en cualquiera de los casos, si se tiene un tanto adicional se preliquida
      -- ====================================================
      -- VERIFICA SI SE TIENE, Y PRELIQUIDA TANTO ADICIONAL
      -- si se tiene tanto adicional, se preliquida
      IF ( v_tanto_adicional_solicitud > 0 ) THEN
         LET v_importe = v_tanto_adicional_solicitud * v_valor_mov;

         INSERT INTO ret_preliquida72 (
            id_afi_fondo72 ,
            f_liquida      ,
            subcuenta      ,
            movimiento     ,
            folio_liquida  ,
            id_referencia  ,
            importe        ,
            estado_pago    ,
            f_registro     ,
            h_registro     ,
            origen          
            )
         VALUES (
            v_id_afi_fondo72            ,
            TODAY                       ,
            v_subcuenta                 ,
            v_movimiento_tanto_adicional,
            v_folio_liquida             ,
            v_id_solicitud              ,
            v_importe                   ,
            NULL                        ,
            TODAY                       ,
            CURRENT HOUR TO SECOND      ,
            v_origen              
         );
      
      END IF-- tanto adicional del monto exacto            
        
      -- cambia el estatus de solicitud a preliquidada
      UPDATE  ret_fondo_ahorro 
      SET     estado_solicitud   = 50,
              folio              = v_folio_liquida
      WHERE   id_solicitud       = v_id_solicitud;

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


