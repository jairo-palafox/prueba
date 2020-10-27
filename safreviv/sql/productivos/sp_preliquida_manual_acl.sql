






CREATE PROCEDURE "safreviv".sp_preliquida_manual_acl(p_folio DECIMAL(9,0), 
                                   p_id_derechohabiente DECIMAL(9,0), 
                                   p_id_referencia DECIMAL(9,0), 
                                   p_nrp CHAR(11),                     
                                   p_periodo_pago CHAR(6),             
                                   p_folio_sua DECIMAL(6,0),           
                                   p_imp_ap_pat DECIMAL(12,2),         
                                   p_aivs DECIMAL(18,6),               
                                   p_imp_am_cre DECIMAL(12,2),         
                                   p_f_pago DATE,                      
                                   p_imp_ren_viv_pgo DECIMAL(12,2),    
                                   p_int_gen_pgo_ext DECIMAL(12,2),    
                                   p_aiv_gen_pgo_ext DECIMAL(18,6),    
                                   p_tpo_aclaracion          SMALLINT,
                                   p_tpo_archivo             SMALLINT,
                                   p_tpo_afiliacion          SMALLINT,
                                   p_origen_pago             SMALLINT,
                                   p_usuario CHAR(20))

   --acl_preliquida
   DEFINE v_preliq_f_liquida          DATE;                       --f_liquida            date                   
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);               --id_derechohabiente   decimal(9,0)           
   DEFINE v_preliq_subcuenta          SMALLINT;                   --subcuenta            smallint               
   DEFINE v_preliq_fondo_inversion    SMALLINT;                   --fondo_inversion      smallint               
   DEFINE v_preliq_movimiento         SMALLINT;                   --movimiento           smallint               
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);               --folio_liquida        decimal(9,0)           
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);               --id_referencia        decimal(9,0)           
   DEFINE v_preliq_monto_acciones     DECIMAL(22,2);              --monto_acciones       decimal(22,2)          
   DEFINE v_preliq_monto_pesos        DECIMAL(22,2);              --monto_pesos          decimal(22,2)          
   DEFINE v_preliq_f_valor            DATE;                       --f_valor              date                   
   DEFINE v_preliq_f_registro         DATE;                       --f_registro           date                   
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;    --h_registro           datetime hour to second
   DEFINE v_preliq_usuario            CHAR(20);                   --usuario              char(20)               
   DEFINE v_origen_pago_aux           SMALLINT;
   DEFINE v_bandera_nrp               SMALLINT;
   --SET DEBUG FILE TO 'sp_preliquida_manual_acl.trace';
   --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;
   
   --Constantes para todos los derechohabintes que pertenecen al folio
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_folio_liquida   = p_folio;
   LET v_preliq_usuario         = p_usuario;
   LET v_origen_pago_aux = p_origen_pago;
   LET p_origen_pago = 14;
   LET v_bandera_nrp = 0;
   
   IF (p_nrp[1,2] = "99") THEN
			LET v_bandera_nrp = 1;
	 ELSE
	 		LET v_bandera_nrp = 0;
   END IF
      --TRACE "Paso 1: Preliquidacion";
      --SI PASO LAS VALIDACIONES ANTERIORES ***  INICIA LA PRELIQUIDACIÓN  ****
      
      LET v_preliq_id_referencia   = p_id_referencia; 
      LET v_preliq_f_valor         = p_f_pago;
      LET v_preliq_f_liquida      = TODAY;
      LET v_preliq_f_registro     = TODAY;
      LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
      
      --TRACE "Paso 2: Primer Registro";
      --Primer registro
      LET v_preliq_id_derechohabiente = p_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta          = 44;
      ELSE
      	LET v_preliq_subcuenta          = 4;
      END IF
      LET v_preliq_movimiento         = 101;
      LET v_preliq_monto_pesos        = p_imp_ap_pat;
      LET v_preliq_monto_acciones     = p_aivs;
      
      
      IF v_preliq_monto_pesos > 0 THEN
         INSERT INTO acl_preliquida
                    (
                      f_liquida, 
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
                      origen
                     )
              VALUES(
                      v_preliq_f_liquida,
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
                      p_nrp
                     );
      ELSE
        INSERT INTO pag_excep_preliquida   
                   (
                     folio               ,
                     id_referencia       ,
                     id_derechohabiente  ,
                     nrp                 ,
                     periodo_pago        ,
                     folio_sua           
                   )        
           VALUES  (
                     p_folio              ,
                     p_id_referencia      ,
                     p_id_derechohabiente ,
                     p_nrp                ,
                     p_periodo_pago       ,
                     p_folio_sua
                   );
      END IF
      
      --TRACE "Paso 3: Segundo Registro";
      --Segundo registro
      LET v_preliq_id_derechohabiente = p_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta          = 44;
      ELSE
      	LET v_preliq_subcuenta          = 41;
      	-- HCRG 16-04-2012 para subcuenta 41 y movimiento 101, siefore = 0
      	LET v_preliq_fondo_inversion = 0;
      END IF
      LET v_preliq_movimiento         = 101;
      LET v_preliq_monto_pesos        = p_imp_am_cre;
      LET v_preliq_monto_acciones     = 0;
      
      IF v_preliq_monto_pesos > 0 THEN
         INSERT INTO acl_preliquida
                    (
                      f_liquida, 
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
                      origen
                    )
              VALUES(
                      v_preliq_f_liquida,
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
                      p_nrp
                    );
      ELSE
        INSERT INTO pag_excep_preliquida   
                   (
                     folio               ,
                     id_referencia       ,
                     id_derechohabiente  ,
                     nrp                 ,
                     periodo_pago        ,
                     folio_sua           
                   )        
           VALUES  (
                     p_folio              ,
                     p_id_referencia      ,
                     p_id_derechohabiente ,
                     p_nrp                ,
                     p_periodo_pago       ,
                     p_folio_sua
                   );
      END IF
      -- HCRG 16-04-2012 si subcuenta fué 43 y movimiento 81, siefore regresa a 11
      LET v_preliq_fondo_inversion = 11;
      
      --TRACE "Paso 4: Tercer Registro";
      --Tercer registro
      LET v_preliq_id_derechohabiente = p_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta          = 44;
      ELSE
      	LET v_preliq_subcuenta          = 4;
      END IF
      LET v_preliq_movimiento         = 44;
      LET v_preliq_monto_pesos        = p_imp_ren_viv_pgo;
      LET v_preliq_monto_acciones     = 0;
      
      IF v_preliq_monto_pesos > 0 THEN
         INSERT INTO acl_preliquida
                    (
                      f_liquida, 
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
                      origen
                     )
              VALUES(
                      v_preliq_f_liquida,
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
                      p_nrp
                     );
      ELSE
     INSERT INTO pag_excep_preliquida   
                   (
                     folio               ,
                     id_referencia       ,
                     id_derechohabiente  ,
                     nrp                 ,
                     periodo_pago        ,
                     folio_sua           
                   )        
           VALUES  (
                     p_folio              ,
                     p_id_referencia      ,
                     p_id_derechohabiente ,
                     p_nrp                ,
                     p_periodo_pago       ,
                     p_folio_sua
                   );
      END IF
      
      --TRACE "Paso 5: Cuarto Registro";
      --Cuarto registro
      LET v_preliq_id_derechohabiente = p_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta          = 44;
      ELSE
      	LET v_preliq_subcuenta          = 4;
      END IF
      LET v_preliq_movimiento         = 43;
      LET v_preliq_monto_pesos        = p_int_gen_pgo_ext;
      LET v_preliq_monto_acciones     = p_aiv_gen_pgo_ext;
      
      IF v_preliq_monto_pesos > 0 THEN
         INSERT INTO acl_preliquida
                     (
                       f_liquida, 
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
                       origen
                     )
              VALUES(
                       v_preliq_f_liquida,
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
                       p_nrp
                     );
      ELSE
       INSERT INTO pag_excep_preliquida   
                   (
                     folio               ,
                     id_referencia       ,
                     id_derechohabiente  ,
                     nrp                 ,
                     periodo_pago        ,
                     folio_sua           
                   )        
           VALUES  (
                     p_folio              ,
                     p_id_referencia      ,
                     p_id_derechohabiente ,
                     p_nrp                ,
                     p_periodo_pago       ,
                     p_folio_sua
                   );
      END IF
   
   --TRACE "Paso 5: update glo_folio y glo_ctr_archivo";
   --Se actualiza la tabla que lleva el control de los folios
   {UPDATE safre_viv:glo_folio
     SET status = 1
   WHERE folio = p_folio;
   
   --Se actualiza el estatus del archivo
   UPDATE safre_viv:glo_ctr_archivo
      SET estado = 3
    WHERE folio = p_folio;}
    
   
   UPDATE pag_ctr_pago
   SET    estado_pago = 80
   WHERE  folio = p_folio
   AND    id_referencia = p_id_referencia;
   
   UPDATE cta_his_pagos
   SET    ind_liquidacion = 6
   WHERE  folio = p_folio
   AND    id_referencia = p_id_referencia; 
   
END PROCEDURE;


