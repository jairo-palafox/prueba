






CREATE FUNCTION "safreviv".fn_insert_preliquidacion_retiro_ley73(v_folio_liquida           DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              SMALLINT)
                                                 RETURNING SMALLINT

   DEFINE  v_b_paso               SMALLINT;
   DEFINE  v_id_derechohabiente   DECIMAL(9,0);
   DEFINE  v_id_solicitud         DECIMAL(9,0);
   DEFINE  v_movimiento           SMALLINT;
   DEFINE  v_movimiento_apo       SMALLINT;
   DEFINE  v_valor_mov            SMALLINT;
   DEFINE  v_origen               char(20);
   DEFINE  v_subcuenta_92         SMALLINT;
   DEFINE  v_subcuenta_97         SMALLINT;
   DEFINE  v_resultado_consulta   SMALLINT;
   --DEFINE  v_count                SMALLINT;

   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_ley73           INTEGER; -- 803 de acuerdo a catalogo
   DEFINE v_bnd_preli             SMALLINT;

   DEFINE v_nss                   CHAR(12);
   
   DEFINE r_pes_viv97             DECIMAL(14,2);
   DEFINE r_pes_viv92             DECIMAL(14,2);
   DEFINE r_pes_viv_total         DECIMAL(14,2);
   
   DEFINE r_aivs_viv92            DECIMAL(18,6);
   DEFINE r_aivs_viv97            DECIMAL(18,6);
   DEFINE r_aivs_viv_total        DECIMAL(18,6);


   -- se inician las variables para marca
   LET v_marca_ley73        = 803; -- marca para disposicion de recursos
   LET v_i_estado_marca     = 0;
   --LET v_count              = 0;
   LET v_bnd_preli          = 0;

   LET v_b_paso             = 0; 
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;

   LEt v_movimiento         = 192;
   LEt v_movimiento_apo     = 422;
   LET v_origen             = "RETIRO U";
   LET v_subcuenta_92       = 8;
   LET v_subcuenta_97       = 4;
   LET v_resultado_consulta = 0;
   
   LET r_pes_viv97          = 0;
   LET r_pes_viv92          = 0;
   
   LET r_aivs_viv92         = 0;
   LET r_aivs_viv97         = 0;
   
   LET r_pes_viv_total      = 0;
   LET r_aivs_viv_total     = 0;



  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_ley73.log';

   --busca registros en estatus de capturado
   FOREACH cu_ret_ley73 FOR SELECT id_derechohabiente, id_solicitud,importe_viv92, importe_viv97,aivs_viv92,aivs_viv97
                                     INTO v_id_derechohabiente
                                          ,v_id_solicitud
                                          ,r_pes_viv92
                                          ,r_pes_viv97
                                          ,r_aivs_viv92
                                          ,r_aivs_viv97
                                     FROM ret_ley73
                                    WHERE estado_solicitud in (15)

    
    LET v_nss            = "";
    --Let v_count = v_count + 1;

         --TRACE ('v_nss antes de asignar');
         --TRACE (v_nss);
         --TRACE ('v_id_derechohabiente antes de asignar');
         --TRACE (v_id_derechohabiente);

     SELECT nss
       INTO  v_nss
       FROM afi_derechohabiente
      WHERE id_derechohabiente  =  v_id_derechohabiente;

         --TRACE ('v_nss despues de asignar');
         --TRACE (v_nss);
         --TRACE ('v_id_derechohabiente despues de asignar');
         --TRACE (v_id_derechohabiente);

         SELECT tipo
           INTO v_valor_mov
           FROM cat_movimiento
          WHERE movimiento = v_movimiento ;


         LET r_pes_viv_total  = (r_pes_viv92 + r_pes_viv97)  * v_valor_mov;
         LET r_aivs_viv_total = (r_aivs_viv92 + r_aivs_viv97)* v_valor_mov;

         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');

         --TRACE (TODAY                 );
         --TRACE (v_subcuenta_92        );
         --TRACE (v_movimiento          );
         --TRACE (v_folio_liquida       );
         --TRACE (v_id_solicitud        );
         --TRACE ('null'                );
         --TRACE (v_nss                 );
         --TRACE ('null'                );
         --TRACE (r_pes_viv92           );
         --TRACE ('null'                );
         --TRACE ('null'                );
         --TRACE ('null'                );
         --TRACE (TODAY                 );
         --TRACE (CURRENT HOUR TO SECOND);
         --TRACE (v_origen              );
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         --TRACE ('""""""""""""""""""""""""""""""""""""""""""""');
         IF r_pes_viv92 > 0 THEN          
            LET r_pes_viv92 = r_pes_viv92    * v_valor_mov;
            LET r_aivs_viv92 = r_aivs_viv92  * v_valor_mov;         
            INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                   v_id_derechohabiente,
                                                   v_subcuenta_92,
                                                   11,
                                                   v_movimiento,             
                                                   v_folio_liquida,
                                                   v_id_solicitud,
                                                   r_aivs_viv92, --r_aivs_viv_total,
                                                   r_pes_viv92,  --r_pes_viv_total,
                                                   TODAY,
                                                   TODAY,
                                                   CURRENT HOUR TO SECOND,
                                                   v_origen
                                                   );
         END IF; 
         IF r_pes_viv97 > 0 THEN
            LET r_pes_viv97 = r_pes_viv97    * v_valor_mov;
            LET r_aivs_viv97 = r_aivs_viv97  * v_valor_mov; 
            INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                   v_id_derechohabiente,
                                                   v_subcuenta_97,
                                                   11,
                                                   v_movimiento,             
                                                   v_folio_liquida,
                                                   v_id_solicitud,
                                                   r_aivs_viv97, --r_aivs_viv_total,
                                                   r_pes_viv97,  --r_pes_viv_total,
                                                   TODAY,
                                                   TODAY,
                                                   CURRENT HOUR TO SECOND,
                                                   v_origen
                                                   );
        END IF; 



            --cambia estatus de campo ingresado a  preliquidacion
            --IF sqlcode <> 0 THEN
            UPDATE  ret_ley73 SET estado_solicitud   = 50,
                                         folio             = v_folio_liquida
                                  WHERE id_derechohabiente = v_id_derechohabiente
                                    AND id_solicitud       = v_id_solicitud;

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


       --TRACE (v_id_derechohabiente);
       --TRACE (v_marca_ley73);
       --TRACE (v_id_solicitud);
       --TRACE (v_folio_liquida);
       --TRACE ('0');
       --TRACE ('0');
       --TRACE ('0');
       --TRACE ("NULL");
       --TRACE (v_usuario_cod);

            UPDATE sfr_marca_activa
                   SET folio                = v_folio_liquida
                 WHERE id_derechohabiente   = v_id_derechohabiente
                   AND marca                = v_marca_ley73
                   AND n_referencia = v_id_solicitud;


            UPDATE sfr_marca_historica
                   SET folio                = v_folio_liquida
                 WHERE id_derechohabiente   = v_id_derechohabiente
                   AND marca  = v_marca_ley73
                   AND n_referencia = v_id_solicitud;

   END FOREACH;

   IF v_bnd_preli = 0 then
        --Coloca preliquidacion como proceso listo
        UPDATE glo_folio
                  SET status =  10
                WHERE proceso_cod = v_proceso_cod
                  AND opera_cod  = v_opera_cod
                  AND folio       = v_folio_liquida
                  AND status      = 0;
   END IF;

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida;

   RETURN v_b_paso;
END FUNCTION;


