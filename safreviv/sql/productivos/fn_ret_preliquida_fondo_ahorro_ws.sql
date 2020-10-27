






CREATE FUNCTION "safreviv".fn_ret_preliquida_fondo_ahorro_ws(v_folio_liquida    DECIMAL(10,0),
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
   LET v_c_msj        = 'El proceso finaliz� exitosamente.';

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_preliquida_fondo_ahorro_ws.trace';

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
   
   --actualiza folio en el proceso
   UPDATE bat_ctr_proceso
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod;
   
   -- se obtiene el signo del movimiento de retiro fondo de ahorro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_fondo_ahorro ;

   -- se leen las solicitudes aprobadas para su preliquidacion
   FOREACH
   SELECT a.id_solicitud      ,
          a.id_derechohabiente,
          a.id_afi_fondo72    ,
          a.saldo_viv72       ,
          a.tanto_adicional
   INTO   v_id_solicitud             , -- id de la solicitud
          v_id_derechohabiente       , -- id_derechohabiente para marca
          v_id_afi_fondo72           , -- derechohabiente con fondo de ahorro
          v_saldo_viv72_solicitado   , -- saldo solicitado
          v_tanto_adicional_solicitud  -- tanto adicional
   FROM   ret_fondo_ahorro_generico a,
          ret_solicitud_generico b
   WHERE  a.estado_solicitud = v_estado_solicitud
   AND    a.id_solicitud     = b.id_solicitud -- que sean del retiro generico
   
      -- se asigna cuanto se solicita originalmente
      LET v_saldo_viv72_solicitado_orig = v_saldo_viv72_solicitado;          
      LET v_nss                         = NULL ;
               
      -- se obtiene el saldo del trabajador
	  SELECT SUM(importe)
	  INTO   v_saldo_del_trabajador
	  FROM   cta_fondo72
	  WHERE  id_afi_fondo72 = v_id_afi_fondo72
	  AND    subcuenta = v_subcuenta;
	  
	  -- si el saldo no existe o esta en negativos, se considera cero
	  IF ( v_saldo_del_trabajador IS NULL OR v_saldo_del_trabajador < 0 ) THEN
	     LET v_saldo_del_trabajador = 0;
	  END IF
      -- =================================================================
      --  PRELIQUIDA MONTO POSEIDO         
      -- =================================================================
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
        
      -- cambia el estatus de solicitud a preliquidada y se le asigna el folio
      UPDATE  ret_fondo_ahorro_generico
      SET     estado_solicitud = 50,
              folio            = v_folio_liquida
      WHERE   id_solicitud     = v_id_solicitud;
      
      -- se actualiza el estado de la solicitud en retiro generico
      UPDATE  ret_solicitud_generico
      SET     estado_solicitud   = 50,
              folio              = v_folio_liquida
      WHERE   id_solicitud       = v_id_solicitud;

	  -- se asigna el folio a la marca activa e historica
	  UPDATE sfr_marca_activa
	  SET    folio = p_folio
	  WHERE  id_derechohabiente = v_id_derechohabiente
	  AND    n_referencia       = v_id_solicitud
	  AND    proceso_marca      = 1503
	  AND    marca              = v_marca_fondo_ahorro
	  AND    folio              = 0;
	  
	  UPDATE sfr_marca_historica
	  SET    folio = p_folio
	  WHERE  id_derechohabiente = v_id_derechohabiente
	  AND    n_referencia       = v_id_solicitud
	  AND    proceso_marca      = 1503
	  AND    marca              = v_marca_fondo_ahorro
	  AND    folio              = 0;
	  
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


