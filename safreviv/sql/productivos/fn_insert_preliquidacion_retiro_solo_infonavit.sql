






CREATE FUNCTION "safreviv".fn_insert_preliquidacion_retiro_solo_infonavit(v_folio_liquida  DECIMAL(10,0),
                                                          v_proceso_cod         SMALLINT,
                                                          v_opera_cod           SMALLINT,
                                                          v_usuario_cod         VARCHAR(20),
                                                          v_pid                 DECIMAL(9,0))
   RETURNING INTEGER, INTEGER, VARCHAR(250)
                                                 
   DEFINE  v_id_derechohabiente   DECIMAL(9,0); -- identificador de derechohabiente
   DEFINE  v_id_solicitud         DECIMAL(9,0); -- identificador solicitud de retiro
   DEFINE  v_acc_viv97            DECIMAL(18,6);
   DEFINE  v_pes_viv97            DECIMAL(20,2);
   DEFINE  v_movimiento           INTEGER;      -- falta que se asigne el numero de movimiento que se le colocara
   DEFINE  v_valor_mov            INTEGER;
   DEFINE  v_origen               CHAR(20);
   DEFINE  v_subcuenta            INTEGER;
   DEFINE  v_fondo                INTEGER;
   DEFINE  v_saldo_97_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97
   DEFINE  v_saldo_97_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv97
   DEFINE  v_resultado_consulta   INTEGER;
   DEFINE  v_bnd_preli            INTEGER;

   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_solo_infonavit  INTEGER; -- 801 de acuerdo a catalogo
   
   -- variables para recepcion de errores
   DEFINE v_sql_error  INTEGER     ;
   DEFINE v_isam_error INTEGER     ;
   DEFINE v_mensaje    VARCHAR(250);
   
   -- en caso de error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje
      
      RETURN v_sql_error, v_isam_error, v_mensaje;
   END EXCEPTION
   
   -- se inician las variables para marca
   LET v_marca_solo_infonavit = 801; -- marca para disposicion de recursos
   LET v_i_estado_marca       = 0;
   
   -- se asume que no hay error
   LET v_sql_error          = 0;
   LET v_isam_error         = 0;
   LET v_mensaje            = "Preliquidación finalizada correctamente.";
   
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;
   LET v_acc_viv97          = 0; 
   LET v_pes_viv97          = 0;
   LET v_valor_mov          = 1;
   LET v_movimiento         = 172;
   LET v_origen             = "RETIRO Z";
   LET v_subcuenta          = 44;
   LET v_resultado_consulta = 0;
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_97_pesos     = 0;
   LET v_bnd_preli          = 0;
   LEt v_fondo              = 11;
   --LEt v_proceso_cod        = 1501;
     
   --SET DEBUG FILE TO '/safreviv/retlog.dir/fn_insert_preliquidacion_retiro_solo_info.trace';

--trace "antes de foreach";

    --busca registros en estatus de capturado
    FOREACH cu_ret_solo_infonavit FOR 
    SELECT 
       id_derechohabiente ,
       id_solicitud       ,
       aivs_viv97         ,
       importe_viv97
    INTO   
       v_id_derechohabiente,
       v_id_solicitud      ,
       v_acc_viv97         ,
       v_pes_viv97
    FROM  ret_solo_infonavit
    WHERE estado_solicitud = 10
    
    --trace "recupera saldo valuado";
       EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                 v_id_derechohabiente,
                                                 v_subcuenta,
                                                 NULL,
                                                 v_fondo)
                       INTO v_saldo_97_pesos,v_saldo_97_aivs,v_resultado_consulta;
       
       --TRACE ('Total monto en pesos a preliquidar');
       --TRACE (v_pes_viv97);
       
       --TRACE ('Tipo de movimiento');
       --TRACE (v_valor_mov);

       --TRACE ('Saldo en pesos recuperado');
       --TRACE (v_saldo_97_pesos);

       --TRACE ('Saldo en acciones recuperado');
       --TRACE (v_saldo_97_aivs);
        
       --corregir la validacion de  saldo colocando la cuenta correcta en RETW01
       --no muestra datos ya que no tiene una subcuenta validada
       IF ( v_saldo_97_pesos  >= v_pes_viv97 ) THEN
       --trace "select tipo de movimiento";
       
          SELECT tipo 
            INTO v_valor_mov
            FROM cat_movimiento
           WHERE movimiento = v_movimiento ;
       
       
          LET v_pes_viv97 = v_pes_viv97 * v_valor_mov;
          LET v_acc_viv97 = v_acc_viv97 * v_valor_mov;
          
          --TRACE ('-------------------------------------');
          --TRACE ('Total monto en pesos a preliquidar');
          --TRACE (v_pes_viv97);
          --TRACE ('Valor antes de hacer el cambio');
          --TRACE (v_pes_viv97);
          --TRACE ('Tipo de movimiento');
          --TRACE (v_valor_mov);
          --TRACE (v_saldo_97_pesos);
          --TRACE (v_pes_viv97);    
       
       --trace "insert a ret_preliquida";
       
          --inserta registros en estatus de capturado a la tabla de preliquidacion
          INSERT INTO ret_preliquida VALUES(TODAY,
                                           v_id_derechohabiente,
                                           v_subcuenta,
                                           v_fondo,
                                           v_movimiento,             
                                           v_folio_liquida,
                                           v_id_solicitud,
                                           v_acc_viv97,
                                           v_pes_viv97,
                                           TODAY,
                                           TODAY,
                                           CURRENT HOUR TO SECOND,
                                           v_origen
                                           );
                                           
          --cambia estatus de campo ingresado a  preliquidacion
          
          --trace "update de ret_solo_infonavit";
          
          UPDATE  ret_solo_infonavit SET estado_solicitud   = 50,
                                                   folio              = v_folio_liquida,
                                                   f_captura          = TODAY 
                                             WHERE id_derechohabiente = v_id_derechohabiente
                                               AND id_solicitud       = v_id_solicitud;
       
       
           LET v_bnd_preli = 1;     -- busca si se preliquido por lo menos 1 para pasar a liquidar
       
          --TRACE (v_id_derechohabiente);
          --TRACE (v_marca_solo_infonavit);
          --TRACE (v_id_solicitud);
          --TRACE (v_folio_liquida);
          --TRACE ('0');
          --TRACE ('0');
          --TRACE ('0');
          --TRACE ("NULL");
          --TRACE (v_usuario_cod);    
       
       --trace "update de marcas activas e historicas";
       
               UPDATE sfr_marca_activa
                      SET folio                = v_folio_liquida
                    WHERE id_derechohabiente   = v_id_derechohabiente
                      AND marca                = v_marca_solo_infonavit
                      AND n_referencia = v_id_solicitud;
       
               UPDATE sfr_marca_historica
                      SET folio                = v_folio_liquida
                    WHERE id_derechohabiente   = v_id_derechohabiente
                      AND marca                = v_marca_solo_infonavit
                      AND n_referencia         = v_id_solicitud;
               
       ELSE

--trace "execute de insert en solicitud_Retiro SINF";
          -- se rechaza la solicitud por falta de saldo
          UPDATE  ret_solo_infonavit 
          SET     estado_solicitud = 101, -- rechazado
                  cod_rechazo      = 10 -- falta de saldo
          WHERE id_solicitud       = v_id_solicitud;

       
       --trace "desmarca la cuenta";
               -- se desmarca la cuenta
               EXECUTE FUNCTION fn_desmarca_cuenta(
                       v_id_derechohabiente
                      ,v_marca_solo_infonavit -- desmarca de solo infonavit
                      ,v_id_solicitud -- identificador de registro de archivo o lote
                      ,40 -- estado marca
                      ,v_marca_solo_infonavit -- marca de la causa
                      ,v_usuario_cod
                      ,v_proceso_cod) --proceso_cod)
                  INTO v_i_estado_marca;
                     
       END IF;
    END FOREACH;
    
    IF ( v_bnd_preli = 0 ) THEN
    
    --trace "update de glo_folio";
       -- se marca el folio como error
       UPDATE glo_folio 
          SET status =  10
        WHERE folio = v_folio_liquida;
    ELSE
      --trace "finalizando proceso";
       -- se marca el folio como correcto
       UPDATE glo_folio 
                 SET status =  1 
               WHERE folio  = v_folio_liquida;
               
       --actualiza folio en la operacion (preliquidacion)
       UPDATE bat_ctr_operacion 
                  SET folio        = v_folio_liquida
                WHERE pid          = v_pid
                  AND proceso_cod  = v_proceso_cod
                  AND opera_cod    = v_opera_cod;

    END IF; 
    
    UPDATE STATISTICS FOR TABLE ret_preliquida;
   RETURN v_sql_error, v_isam_error, v_mensaje;
END FUNCTION;


