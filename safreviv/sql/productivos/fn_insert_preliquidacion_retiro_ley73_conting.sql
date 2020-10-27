






CREATE FUNCTION "safreviv".fn_insert_preliquidacion_retiro_ley73_conting(v_folio_liquida   DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              SMALLINT)
      RETURNING INTEGER, INTEGER, VARCHAR(250)

   DEFINE  v_b_paso               SMALLINT;
   DEFINE  v_id_derechohabiente   DECIMAL(9,0);
   DEFINE  v_id_solicitud         DECIMAL(9,0);
   DEFINE  v_movimiento           SMALLINT;
   DEFINE  v_valor_mov            SMALLINT;
   DEFINE  v_origen               CHAR(20);
   DEFINE  v_subcuenta_92         SMALLINT;
   DEFINE  v_subcuenta_97         SMALLINT;
   DEFINE  v_resultado_consulta   SMALLINT;
   
   DEFINE  v_saldo_92_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
   DEFINE  v_saldo_92_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92

   DEFINE v_sobregiro_aivs97     DECIMAL(18,6);
   DEFINE v_sobregiro_viv97      DECIMAL(20,2);

   DEFINE v_sobregiro_aivs92     DECIMAL(18,6);
   DEFINE v_sobregiro_viv92      DECIMAL(20,2); 

   DEFINE v_saldo_original        DECIMAL(20,2); 
   
   DEFINE  v_saldo_97_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
   DEFINE  v_saldo_97_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92

   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_ley73           INTEGER; -- 803 de acuerdo a catalogo
   DEFINE v_bnd_preli             SMALLINT;

   DEFINE v_nss                   CHAR(12);
   
   DEFINE r_solicitado_pes_viv97             DECIMAL(14,2);
   DEFINE r_solicitado_pes_viv92             DECIMAL(14,2);
   DEFINE r_pes_viv_total         DECIMAL(14,2);
   
   DEFINE r_solicitado_aivs_viv92            DECIMAL(18,6);
   DEFINE r_solicitado_aivs_viv97            DECIMAL(18,6);
   DEFINE r_aivs_viv_total        DECIMAL(18,6);
   DEFINE v_fondo                 SMALLINT;
   
   -- Control de Excepciones
   DEFINE v_si_resultado          SMALLINT;
   DEFINE sql_err                 INTEGER;
   DEFINE isam_err                INTEGER;
   DEFINE err_txt                 VARCHAR(250);
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_movimiento_sobregiro  SMALLINT;


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


   -- se inician las variables para marca
   LET v_marca_ley73        = 803; -- marca para disposicion de recursos
   LET v_i_estado_marca     = 0;
   --LET v_count            = 0;
   LET v_bnd_preli          = 2;

   LET v_b_paso             = 0; 
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;

   LET v_movimiento           = 192;
   LET v_movimiento_sobregiro = 912 ;
   LET v_origen               = "RETIRO U";
   LET v_subcuenta_92         = 8;
   LET v_subcuenta_97         = 4;
   LET v_resultado_consulta   = 0;
   
   LET r_solicitado_pes_viv97          = 0;
   LET r_solicitado_pes_viv92          = 0;
   
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_97_pesos     = 0;
   LET v_saldo_92_aivs      = 0;
   LET v_saldo_92_pesos     = 0;

   LET v_sobregiro_aivs97  = 0 ;
   LET v_sobregiro_viv97   = 0 ;
   LET v_sobregiro_aivs92  = 0 ;
   LET v_sobregiro_viv92   = 0 ;
   
   LET r_solicitado_aivs_viv92         = 0;
   LET r_solicitado_aivs_viv97         = 0;
   
   LET r_pes_viv_total      = 0;
   LET r_aivs_viv_total     = 0;
   LET v_fondo              = 11;
   LET v_saldo_original     = 0 ;
   
   -- se asume que el proceso termina bien
   
   LET v_si_resultado       = 0;
   LET isam_err             = 0;
   LET v_c_msj              = 'El proceso finalizó exitosamente.';

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_ley73_conting.log';
   --trace("ganacia");
   --busca registros en estatus de capturado
   FOREACH cu_ret_ley73 FOR 
   SELECT 
      id_derechohabiente ,
      id_solicitud       ,
      importe_viv92      ,
      importe_viv97      ,
      aivs_viv92         ,
      aivs_viv97
   INTO 
      v_id_derechohabiente,
      v_id_solicitud      ,
      r_solicitado_pes_viv92         ,
      r_solicitado_pes_viv97         ,
      r_solicitado_aivs_viv92        ,
      r_solicitado_aivs_viv97        
   FROM  ret_ley73
   WHERE estado_solicitud = 15
      
      --trace("foreach"); 
      LET v_bnd_preli = 0;           
       
      -- se obtiene el saldo de vivienda 97
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_97,
                                                NULL,
                                                v_fondo)
                        INTO v_saldo_97_pesos,v_saldo_97_aivs,v_resultado_consulta;
                        --trace ("v_saldo_97_pesos");
                        --trace (v_saldo_97_pesos||" "||v_saldo_97_aivs||" "||v_resultado_consulta);

      -- se obtiene el saldo de vivienda 92
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_92,
                                                NULL,
                                                v_fondo)
                        INTO v_saldo_92_pesos,v_saldo_92_aivs,v_resultado_consulta;
                        
        --trace ("v_saldo_92_pesos");
        --trace (v_saldo_92_pesos||" "||v_saldo_92_aivs||" "||v_resultado_consulta);                         
        --trace (v_saldo_97_pesos );
        --trace (r_solicitado_pes_viv97);
        --trace (v_saldo_92_pesos);
        --trace (r_solicitado_pes_viv92);
        ----trace (((v_saldo_97_pesos  >= r_solicitado_pes_viv97) OR r_solicitado_pes_viv97 = 0  ) AND ((v_saldo_92_pesos  >= r_solicitado_pes_viv92) OR r_solicitado_pes_viv92 = 0));
                    
        /*IF ((v_saldo_97_aivs >= r_solicitado_aivs_viv97) OR r_solicitado_aivs_viv97 = 0 ) AND ((v_saldo_92_aivs >= r_solicitado_aivs_viv92) OR r_solicitado_aivs_viv92 = 0) 
            AND NOT (r_solicitado_aivs_viv97 = 0 and r_solicitado_aivs_viv92 = 0 )THEN */
            
           LET v_nss = "";
           --Let v_count = v_count + 1;           
           --trace ('v_nss antes de asignar');
           --trace (v_nss);
           --trace ('v_id_derechohabiente antes de asignar');
           --trace (v_id_derechohabiente);
           
           SELECT nss
           INTO   v_nss
           FROM   afi_derechohabiente
           WHERE  id_derechohabiente  =  v_id_derechohabiente;
           
            --trace ('v_nss despues de asignar');
            --trace (v_nss);
            --trace ('v_id_derechohabiente despues de asignar');
            --trace (v_id_derechohabiente);
           
           SELECT tipo
             INTO v_valor_mov
             FROM cat_movimiento
            WHERE movimiento = v_movimiento ;
           
           
                --LET r_pes_viv_total  = (r_solicitado_pes_viv92 + r_solicitado_pes_viv97)  * v_valor_mov;
                LET r_pes_viv_total  = (r_solicitado_pes_viv92 + r_solicitado_pes_viv97)  * -1;
                LET r_aivs_viv_total = (r_solicitado_aivs_viv92 + r_solicitado_aivs_viv97)* -1;
                --LET r_pes_viv_total  = (r_solicitado_pes_viv92 + r_solicitado_pes_viv97)  * -1;
           
                --trace ('""""""""""""""""""""""""""""""""""""""""""""');
                --trace ('""""""""""""""""r_pes_92    """""""""""""""""');
                --trace ('""""""""""""""""""""""""""""""""""""""""""""');
           
                --trace (TODAY);
                --trace(v_id_derechohabiente);
                --trace(v_subcuenta_92);
                --trace(v_fondo);
                --trace(v_movimiento);
                --trace(v_folio_liquida);
                --trace(v_id_solicitud);
                --trace(r_solicitado_aivs_viv92);
                --trace(r_solicitado_pes_viv92);
                --trace(TODAY);
                --trace(TODAY);
                --trace(CURRENT HOUR TO SECOND);
                --trace(v_origen);
                
                --trace ('""""""""""""""""""""""""""""""""""""""""""""');
                --trace ('""""""""""""""""r_pes_92    """""""""""""""""');
                --trace ('""""""""""""""""""""""""""""""""""""""""""""');

                --IF r_solicitado_pes_viv92 > 0 THEN  
                --saldo > solicitado
                IF v_saldo_92_aivs  >= r_solicitado_aivs_viv92 THEN
                   --LET r_solicitado_pes_viv92 = r_solicitado_pes_viv92    * v_valor_mov;
                   --LET r_solicitado_aivs_viv92 = r_solicitado_aivs_viv92  * v_valor_mov;
                   LET r_solicitado_pes_viv92  = r_solicitado_pes_viv92    * -1;
                   LET r_solicitado_aivs_viv92 = r_solicitado_aivs_viv92   * -1;         
                   
                   INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                          v_id_derechohabiente,
                                                          v_subcuenta_92,
                                                          v_fondo,
                                                          v_movimiento,             
                                                          v_folio_liquida,
                                                          v_id_solicitud,
                                                          r_solicitado_aivs_viv92, --r_aivs_viv_total,
                                                          r_solicitado_pes_viv92,  --r_pes_viv_total,
                                                          TODAY,
                                                          TODAY,
                                                          CURRENT HOUR TO SECOND,
                                                          v_origen
                                                          );
               ELSE 
                   --LET r_solicitado_pes_viv92 = r_solicitado_pes_viv92    * v_valor_mov;
                   --LET r_solicitado_aivs_viv92 = r_solicitado_aivs_viv92  * v_valor_mov;
                   --r_solicitado_aivs_viv92 > v_saldo_92_aivs
                   IF v_saldo_92_aivs > 0 THEN 
                     LET v_sobregiro_aivs92 = r_solicitado_aivs_viv92 - v_saldo_92_aivs;
                     LET v_saldo_original   = v_saldo_92_aivs;
                     LET v_sobregiro_aivs92 = v_sobregiro_aivs92   * -1;
                     LET v_saldo_92_aivs    = v_saldo_92_aivs      * -1;
                     
                   ELSE 
                     LET v_sobregiro_aivs92 = r_solicitado_aivs_viv92 ; --+ (v_saldo_92_aivs * 1 );
                     LET v_saldo_original   = v_saldo_92_aivs;
                     LET v_sobregiro_aivs92 = v_sobregiro_aivs92   * -1;                     
                     --LET v_saldo_92_aivs  = v_saldo_92_aivs        * -1;                     
                   END IF
                   IF v_saldo_92_pesos > 0 THEN 
                     LET v_sobregiro_viv92  = r_solicitado_pes_viv92  - v_saldo_92_pesos  ;
                     LET v_sobregiro_viv92  = v_sobregiro_viv92    * -1;
                     LET v_saldo_92_pesos   = v_saldo_92_pesos     * -1;  
                   ELSE 
                     LET v_sobregiro_viv92 = r_solicitado_pes_viv92 ; --+ (v_saldo_92_pesos * 1 );
                     LET v_sobregiro_viv92 = v_sobregiro_viv92   * -1;
                     --LET v_saldo_92_pesos  = v_saldo_92_pesos        * -1;                     
                   END IF
                   INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                               v_id_derechohabiente,
                                                               v_subcuenta_92,
                                                               v_fondo,
                                                               v_movimiento_sobregiro,             
                                                               v_folio_liquida,
                                                               v_id_solicitud,
                                                               v_sobregiro_aivs92, --r_aivs_viv_total,  v_saldo_92_pesos,v_saldo_92_aivs
                                                               v_sobregiro_viv92,  --r_pes_viv_total,
                                                               TODAY,
                                                               TODAY,
                                                               CURRENT HOUR TO SECOND,
                                                               v_origen
                                                              );         
                   IF v_saldo_original > 0 then 
                      INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                                  v_id_derechohabiente,
                                                                  v_subcuenta_92,
                                                                  v_fondo,
                                                                  v_movimiento,             
                                                                  v_folio_liquida,
                                                                  v_id_solicitud,
                                                                  v_saldo_92_aivs,
                                                                  v_saldo_92_pesos,                                                                                                                  
                                                                  TODAY,
                                                                  TODAY,
                                                                  CURRENT HOUR TO SECOND,
                                                                  v_origen
                                                                  );
                   END IF;
                END IF; 
                
                --IF r_solicitado_pes_viv97 > 0 THEN
                IF v_saldo_97_aivs  >= r_solicitado_aivs_viv97 THEN
                   --LET r_solicitado_pes_viv97  = r_solicitado_pes_viv97    * v_valor_mov;
                   --LET r_solicitado_aivs_viv97 = r_solicitado_aivs_viv97  * v_valor_mov; 
                   LET r_solicitado_pes_viv97  = r_solicitado_pes_viv97    * -1;
                   LET r_solicitado_aivs_viv97 = r_solicitado_aivs_viv97   * -1; 
                   
                   INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                          v_id_derechohabiente,
                                                          v_subcuenta_97,
                                                          v_fondo,
                                                          v_movimiento,             
                                                          v_folio_liquida,
                                                          v_id_solicitud,
                                                          r_solicitado_aivs_viv97, --r_aivs_viv_total,
                                                          r_solicitado_pes_viv97,  --r_pes_viv_total,
                                                          TODAY,
                                                          TODAY,
                                                          CURRENT HOUR TO SECOND,
                                                          v_origen
                                                          );
               ELSE 
                   --LET r_solicitado_pes_viv97  = r_solicitado_pes_viv97    * v_valor_mov;
                   --LET r_solicitado_aivs_viv97 = r_solicitado_aivs_viv97  * v_valor_mov;
                   IF v_saldo_97_aivs > 0 THEN 
                     LET v_sobregiro_aivs97 = r_solicitado_aivs_viv97 - v_saldo_97_aivs;
                     LET v_saldo_original    = v_saldo_97_aivs;
                     LET v_sobregiro_aivs97 = v_sobregiro_aivs97   * -1;
                     LET v_saldo_97_aivs    = v_saldo_97_aivs        * -1;
                     
                   ELSE 
                     LET v_sobregiro_aivs97 = r_solicitado_aivs_viv97; -- + (v_saldo_97_aivs * 1 );
                     LET v_saldo_original   = v_saldo_97_aivs;
                     LET v_sobregiro_aivs97 = v_sobregiro_aivs97   * -1;                     
                     --LET v_saldo_97_aivs  = v_saldo_97_aivs        * -1;                     
                   END IF
                   IF v_saldo_97_pesos > 0 THEN 
                     LET v_sobregiro_viv97  = r_solicitado_pes_viv97  - v_saldo_97_pesos ;                     
                     LET v_sobregiro_viv97  = v_sobregiro_viv97    * -1;
                     LET v_saldo_97_pesos = v_saldo_97_pesos       * -1;
                  ELSE 
                     LET v_sobregiro_viv97 = r_solicitado_pes_viv97;  --+ (v_saldo_97_pesos * 1 );
                     LET v_sobregiro_viv97 = v_sobregiro_viv97   * -1;
                     --LET v_saldo_97_pesos  = v_saldo_97_pesos        * -1;                     
                  END IF
                   
                   INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                          v_id_derechohabiente,
                                                          v_subcuenta_97,
                                                          v_fondo,
                                                          v_movimiento_sobregiro,             
                                                          v_folio_liquida,
                                                          v_id_solicitud,
                                                          v_sobregiro_aivs97, --r_aivs_viv_total,
                                                          v_sobregiro_viv97,  --r_pes_viv_total,
                                                          TODAY,
                                                          TODAY,
                                                          CURRENT HOUR TO SECOND,
                                                          v_origen
                                                          );
                   IF v_saldo_original > 0 THEN 
                      INSERT INTO safre_viv:ret_preliquida VALUES(TODAY,
                                                                  v_id_derechohabiente,
                                                                  v_subcuenta_97,
                                                                  v_fondo,
                                                                  v_movimiento,             
                                                                  v_folio_liquida,
                                                                  v_id_solicitud,
                                                                  v_saldo_97_aivs, 
                                                                  v_saldo_97_pesos,                                                                    
                                                                  TODAY,
                                                                  TODAY,
                                                                  CURRENT HOUR TO SECOND,
                                                                  v_origen
                                                                 );
                  END IF;
               END IF;
               
                   --cambia estatus de campo ingresado a  preliquidacion
                   --IF sqlcode <> 0 THEN
                   UPDATE  ret_ley73 SET estado_solicitud         = 50,
                                                folio             = v_folio_liquida
                                         WHERE id_derechohabiente = v_id_derechohabiente
                                           AND id_solicitud       = v_id_solicitud;
           
                   --Coloca preliquidacion como proceso listo
                   UPDATE glo_folio
                           SET status =  1
                         WHERE proceso_cod = v_proceso_cod
                           AND opera_cod   = 2
                           AND folio       = v_folio_liquida
                           AND status      = 0;
           
                   --actualiza folio en la operacion (preliquidacion)
                   UPDATE bat_ctr_operacion
                           SET folio        = v_folio_liquida
                         WHERE pid          = v_pid
                           AND proceso_cod  = v_proceso_cod
                           AND opera_cod    = 3;
           
                   LET v_bnd_preli = 1;     -- busca si se preliquido por lo menos 1 para pasar a liquidar
           
                   UPDATE sfr_marca_activa
                          SET folio                = v_folio_liquida
                        WHERE id_derechohabiente   = v_id_derechohabiente
                          AND marca                = v_marca_ley73
                          AND n_referencia = v_id_solicitud;
           
           
                   UPDATE sfr_marca_historica
                          SET folio                = v_folio_liquida
                        WHERE id_derechohabiente   = v_id_derechohabiente
                          AND marca                = v_marca_ley73
                          AND n_referencia         = v_id_solicitud;
/*        ELSE
          UPDATE ret_ley73
              SET cod_rechazo           = 10,                  --cod_rechazo
                   estado_solicitud     = 100                  --estatus de solicitud
              WHERE id_derechohabiente  = v_id_derechohabiente --id_derechohabiente
                AND id_solicitud        = v_id_solicitud;
                                  
           -- se desmarca la cuenta
           EXECUTE FUNCTION safre_viv:fn_desmarca_cuenta(
                   v_id_derechohabiente
                  ,v_marca_ley73 -- desmarca de solo infonavit
                  ,v_id_solicitud -- identificador de registro de archivo o lote
                  ,40 -- estado marca
                  ,0 -- marca de la causa
                  ,v_usuario_cod
                  ,v_proceso_cod)
              INTO v_i_estado_marca;                
    
        END IF   */

        if v_bnd_preli   = 1 then
           LET  v_b_paso = 1; 
        end if 
         
   END FOREACH;

  
   IF v_b_paso = 1 then
        --Coloca preliquidacion como proceso listo
       UPDATE glo_folio
          SET status =  1
        WHERE proceso_cod = v_proceso_cod
          AND opera_cod   = 2
          AND folio       = v_folio_liquida
          AND status      = 0; 

           LET v_c_msj   = 'El proceso finalizó exitosamente.';
    ELSE 
            LET v_si_resultado = 2000;
            LET v_c_msj   = 'El proceso no preliquidó registros. Verifique en la consulta general de retiros.';
                --Coloca preliquidacion como proceso rechazado
        UPDATE glo_folio
                  SET status =  10
                WHERE proceso_cod = v_proceso_cod
                  AND opera_cod   = v_opera_cod
                  AND folio       = v_folio_liquida
                  AND status      = 0;
   END IF;
   
   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida;

   --RETURN v_b_paso;
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


