






CREATE FUNCTION "safreviv".fn_insert_preliquidacion_retiro_fondo_ahorro_cont_manual(v_folio_liquida    DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              DECIMAL(9,0))
                                                 RETURNING SMALLINT

   DEFINE  v_b_paso               SMALLINT;
   DEFINE  v_id_afi_fondo72       DECIMAL(9,0);
   DEFINE  v_id_derechohabiente   DECIMAL(9,0);
   DEFINE  v_id_solicitud         DECIMAL(9,0);
   DEFINE  r_acc_viv97            DECIMAL(18,6);
   DEFINE  r_pes_viv72            DECIMAL(14,2);
   DEFINE  v_movimiento           SMALLINT;
   DEFINE  v_movimiento_apo       SMALLINT;
   DEFINE  v_movimiento_sgiro     SMALLINT;
   DEFINE  v_valor_mov            SMALLINT;
   DEFINE  v_origen               CHAR(20);
   DEFINE  v_subcuenta            SMALLINT;
   DEFINE  v_saldo_97_aivs        DECIMAL(18,6);
   DEFINE  v_saldo_97_pesos       DECIMAL(14,2);
   DEFINE  v_resultado_consulta   SMALLINT;
   DEFINE  v_count                SMALLINT;

   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_fondo_ahorro    INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_bnd_preli             SMALLINT;
   DEFINE v_nss                   CHAR(12);
   DEFINE r_tanto_adicional       decimal(14,2);
   define r_saldo_sobregiro       decimal(14,2);


   -- se inician las variables para marca
   LET v_marca_fondo_ahorro = 802; -- marca para disposicion de fondo ahorro
   LET v_i_estado_marca     = 0;
   LET v_count              = 0;
   LET v_bnd_preli          = 0;

   LET v_b_paso             = 0;
   LET v_id_afi_fondo72     = 0;
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;
   LET r_acc_viv97          = 0;
   LET r_pes_viv72          = 0;
   LET v_movimiento         = 182;
   LET v_movimiento_apo     = 422;
   LET v_movimiento_sgiro   = 752;
   LET v_origen             = "RETIRO W";
   LET v_subcuenta          = 40;
   LET v_resultado_consulta = 0;
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_97_pesos     = 0;



  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_fondo_ahorro.log';

   FOREACH cu_ret_fondo FOR SELECT id_solicitud
                              INTO v_id_solicitud
                              FROM ret_fondo_ahorro_manual
                             WHERE estado_solicitud = 18


   --busca registros en estatus de capturado
   FOREACH cu_ret_det FOR SELECT det.saldo_viv72, det.tanto_adicional , det.saldo_sobregiro,det.id_afi_fondo72
                                     INTO r_pes_viv72
                                         ,r_tanto_adicional
                                         ,r_saldo_sobregiro
                                         ,v_id_afi_fondo72
                                     FROM ret_det_fondo72_manual  det
                                    WHERE id_solicitud = v_id_solicitud
                                      AND estado_detalle   = 1

          LET v_saldo_97_pesos = 0;
          LET v_nss            = "";
          Let v_count = v_count + 1;

               --TRACE ('v_nss antes de asignar');
               --TRACE (v_nss);
               --TRACE ('v_id_derechohabiente antes de asignar');
               --TRACE (v_id_derechohabiente);

          --SELECT nss
          --  INTO v_nss
          --  FROM afi_derechohabiente
          -- WHERE id_derechohabiente = v_id_derechohabiente;


         SELECT nss, id_derechohabiente
           INTO v_nss , v_id_derechohabiente
           FROM afi_fondo72
          WHERE id_afi_fondo72 = v_id_afi_fondo72;

         SELECT tipo
           INTO v_valor_mov
           FROM cat_movimiento
          WHERE movimiento = v_movimiento ;

         --TRACE ('v_nss despues de asignar');
         --TRACE (v_nss);
         --TRACE ('v_id_afi_fondo72 despues de asignar');
         --TRACE (v_id_afi_fondo72);

        --Let v_count = v_count + 1;

         --TRACE ('Total monto en pesos a preliquidar');
         --TRACE (r_pes_viv72);
         --TRACE ('Valor antes de hacer el cambio');
         --TRACE (r_pes_viv72);
         --TRACE ('Tipo de movimiento');
         --TRACE (v_valor_mov);
         --TRACE (v_saldo_97_pesos);
         --TRACE (r_pes_viv72);

         LET r_pes_viv72 = r_pes_viv72 * v_valor_mov;

         --TRACE ('v_id_afi_fondo72 despues');
         --TRACE (v_id_afi_fondo72);
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         --TRACE (v_id_afi_fondo72   );
         --TRACE (TODAY                 );
         --TRACE (v_subcuenta           );
         --TRACE (v_movimiento          );
         --TRACE (v_folio_liquida       );
         --TRACE (v_id_solicitud        );
         --TRACE ('null'                );
         --TRACE (v_nss                 );
         --TRACE ('null'                );
         --TRACE (r_pes_viv72           );
         --TRACE ('null'                );
         --TRACE ('null'                );
         --TRACE ('null'                );
         --TRACE (TODAY                 );
         --TRACE (CURRENT HOUR TO SECOND);
         --TRACE (v_origen              );
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');

         INSERT INTO ret_preliquida72 values( v_id_afi_fondo72,                 --id_ret_fondo72
                                              TODAY,                            --f_liquida
                                              v_subcuenta,                      --subcuenta
                                              v_movimiento,                     --movimiento    182
                                              v_folio_liquida,                  --folio_liquida
                                              v_id_solicitud,                   --id_referencia
                                              r_pes_viv72,                      --importe
                                              NULL,                             --estado_pago
                                              TODAY,                            --f_registro
                                              CURRENT HOUR TO SECOND,           --h_registro
                                              v_origen);                        --origen

         IF r_tanto_adicional > 0 THEN       
         SELECT tipo
           INTO v_valor_mov
           FROM cat_movimiento
          WHERE movimiento = v_movimiento_apo ;
          
            LET r_tanto_adicional = r_tanto_adicional * v_valor_mov;
            INSERT INTO ret_preliquida72 VALUES(   v_id_afi_fondo72 ,           --id_ret_fondo72
                                                   TODAY,                       --f_liquida
                                                   v_subcuenta,                 --subcuenta
                                                   v_movimiento_apo, --422      --movimiento
                                                   v_folio_liquida,             --folio_liquida
                                                   v_id_solicitud,              --id_referencia
                                                   r_tanto_adicional,           --importe
                                                   NULL,                        --estado_pago
                                                   TODAY,                       --f_registro
                                                   CURRENT HOUR TO SECOND,      --h_registro
                                                   v_origen);                   --origen

         END IF;

        IF r_saldo_sobregiro > 0 THEN    
         SELECT tipo
           INTO v_valor_mov
           FROM cat_movimiento
          WHERE movimiento = v_movimiento_sgiro ;
          
            LET r_saldo_sobregiro = r_saldo_sobregiro * v_valor_mov;
            INSERT INTO ret_preliquida72 VALUES(   v_id_afi_fondo72 ,           --id_ret_fondo72
                                                   TODAY,                       --f_liquida
                                                   v_subcuenta,                 --subcuenta
                                                   v_movimiento_sgiro, --422      --movimiento
                                                   v_folio_liquida,             --folio_liquida
                                                   v_id_solicitud,              --id_referencia
                                                   r_saldo_sobregiro,           --importe
                                                   NULL,                        --estado_pago
                                                   TODAY,                       --f_registro
                                                   CURRENT HOUR TO SECOND,      --h_registro
                                                   v_origen);                   --origen

         END IF;
          IF v_id_derechohabiente IS NOT NULL then
            UPDATE sfr_marca_activa
                   SET folio                = v_folio_liquida
                 WHERE id_derechohabiente   = v_id_derechohabiente
                   AND marca                = v_marca_fondo_ahorro
                   AND n_referencia         = v_id_solicitud;

            UPDATE sfr_marca_historica
                   SET folio                = v_folio_liquida
                 WHERE id_derechohabiente   = v_id_derechohabiente
                   AND marca                = v_marca_fondo_ahorro
                   AND n_referencia         = v_id_solicitud;
          END IF
         END FOREACH;
            --cambia estatus de campo ingresado a  preliquidacion
            --IF sqlcode <> 0 THEN
            UPDATE  ret_fondo_ahorro_manual SET estado_solicitud   = 50,
                                         folio              = v_folio_liquida
                                  WHERE id_solicitud        = v_id_solicitud;

            --Coloca preliquidacion como proceso listo
            UPDATE glo_folio
                    SET status =  1
                  WHERE proceso_cod = v_proceso_cod
                    AND  opera_cod  = v_opera_cod
                    AND folio       = v_folio_liquida
                    AND status      = 0;

            --actualiza folio en la operacion (preliquidacion)
            UPDATE bat_ctr_operacion
                    SET folio        = v_folio_liquida
                  WHERE pid          = v_pid
                    AND proceso_cod  = v_proceso_cod
                    AND opera_cod    = 1;

           LET v_bnd_preli = 1;     -- busca si se preliquido por lo menos 1 para pasar a liquidar

           --TRACE (v_id_afi_fondo72);
           --TRACE (v_marca_fondo_ahorro);
           --TRACE (v_id_solicitud);
           --TRACE (v_folio_liquida);
           --TRACE ('0');
           --TRACE ('0');
           --TRACE ('0');
           --TRACE ("NULL");
           --TRACE (v_usuario_cod);


     END FOREACH;


   IF v_bnd_preli = 0 then
        --Coloca preliquidacion como proceso listo
        UPDATE glo_folio
                  SET status =  10
                WHERE proceso_cod = v_proceso_cod
                  AND opera_cod   = v_opera_cod
                  AND folio       = v_folio_liquida
                  AND status      = 0;
   END IF;

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida72;

   RETURN v_b_paso;
END FUNCTION;


