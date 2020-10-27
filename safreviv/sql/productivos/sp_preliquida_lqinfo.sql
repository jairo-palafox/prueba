






CREATE PROCEDURE "safreviv".sp_preliquida_lqinfo(p_folio DECIMAL(9,0), p_usuario CHAR(20))

RETURNING SMALLINT, INTEGER, VARCHAR(255), DECIMAL(9,0)

   --pag_cza_pag_patronal
   DEFINE v_cza_folio                 DECIMAL(9,0);
   DEFINE v_cza_folio_sua             CHAR(6);
   DEFINE v_cza_f_valor_4seguros      DATE;
   --pag_det_trabajador
   DEFINE v_det_folio                 DECIMAL(9,0);
   DEFINE v_det_id_derechohabiente    DECIMAL(9);
   DEFINE v_det_imp_ap_pat            DECIMAL(12,2);
   DEFINE v_det_imp_am_cre            DECIMAL(12,2);
   DEFINE v_det_imp_ren_viv_pgo_ext   DECIMAL(12,2);
   DEFINE v_det_aiv_ap_pat            DECIMAL(18,6);
   DEFINE v_det_int_gen_pgo_ext       DECIMAL(12,2);
   DEFINE v_det_aiv_gen_pgo_ext       DECIMAL(18,6);
   --pag_preliquida
   DEFINE v_preliq_f_liquida          DATE;
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);
   DEFINE v_preliq_subcuenta          SMALLINT;
   DEFINE v_preliq_fondo_inversion    SMALLINT;
   DEFINE v_preliq_movimiento         SMALLINT;
   DEFINE v_preliq_movimiento_ren     SMALLINT;
   DEFINE v_preliq_movimiento_int     SMALLINT;
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);
   DEFINE v_preliq_monto_acciones     DECIMAL(20,2);
   DEFINE v_preliq_monto_pesos        DECIMAL(20,2);
   DEFINE v_preliq_f_valor            DATE;
   DEFINE v_preliq_f_registro         DATE;
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;

   DEFINE v_consecutivo               INTEGER;
   DEFINE v_localiza_trabajador       char(1);
   DEFINE v_tpo_aclaracion            char(2);
   DEFINE v_nrp                       char(11);
   DEFINE v_id_referencia             decimal(9,0);
   DEFINE v_periodo_pago              char(6);
   DEFINE v_bandera_nrp               SMALLINT;
   DEFINE v_tpo_patron                CHAR(2);
   DEFINE v_ind_liquidacion           SMALLINT;
   DEFINE v_f_pago                    DATE;
   DEFINE v_f_valor                   DATE;
   DEFINE v_importe_valida            DECIMAL(12,2);
   DEFINE v_fn_existe_pgo_cta         SMALLINT;
   DEFINE v_tpo_afiliacion            SMALLINT;
   DEFINE v_destino_ap_viv            CHAR(01);

   DEFINE v_indices_deshabilitados    SMALLINT;      -- 0=No se deshabilitaron. 1=Se deshabilitaron
   DEFINE v_conteo                    DECIMAL(9,0);
   DEFINE v_cont_nrp_riss             SMALLINT;

   --	 Variables de manejor de excepcion
	 DEFINE v_error                     SMALLINT;
	 DEFINE isam_err                    INTEGER;
   DEFINE err_txt                     VARCHAR(255);

   ON EXCEPTION
      SET v_error, isam_err, err_txt
      --TRACE 'Ocurrio el error:'||v_error;
      --TRACE isam_err;
      --TRACE err_txt;
      RETURN v_error, isam_err, err_txt, v_det_id_derechohabiente;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/preliquida_pag.trace';
   --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;

   LET v_consecutivo = 0;
   LET v_bandera_nrp = 0;
   LET v_det_id_derechohabiente = 0;

   --TRACE 'borra la tabla :pag_lqinfo_preliquida';
   DROP TABLE  if exists  pag_lqinfo_preliquida ;

    --TRACE 'crea la tabla :pag_lqinfo_preliquida';
    CREATE TABLE pag_lqinfo_preliquida
      (
        f_liquida          DATE NOT NULL ,
        id_derechohabiente DECIMAL(9,0) NOT NULL ,
        subcuenta          SMALLINT NOT NULL ,
        fondo_inversion    SMALLINT NOT NULL ,
        movimiento         SMALLINT NOT NULL ,
        folio_liquida      DECIMAL(9,0) NOT NULL ,
        id_referencia      DECIMAL(9,0) NOT NULL ,
        monto_acciones     DECIMAL(22,2),
        monto_pesos        DECIMAL(22,2),
        f_valor            DATE,
        f_registro         DATE,
        h_registro         DATETIME HOUR TO SECOND,
        origen             char(20)
      ) fragment by round robin in pag_1_dbs, pag_2_dbs;

   -- se asume que no se deshabilitaran los indices
   LET v_indices_deshabilitados = 0;
   LET v_conteo = 0;

   -- se cuentan los registros
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   tmp_det_trabajador;

   -- si el conteo es igual o mayor a 1 millon de registros, se deshabilitan los indices
   IF ( v_conteo > 1999999 ) THEN 
   
       -- se activa la bandera de indices deshabilitados
       LET v_indices_deshabilitados = 1;
   
       -- se deshabilitan los indices
       SET INDEXES xpkpag_ctr_pago  DISABLED;
       SET INDEXES xie1pag_ctr_pago DISABLED;
   END IF

   FOREACH SELECT id_derechohabiente        ,
                  imp_ap_pat                ,
                  imp_am_cre                ,
                  --se modifica el día 7 de agosto de 2012 por instrucción de Gerardo Vega  y por regla de negocio
                   0                        ,
                  --imp_ren_viv_pgo_ext       ,
                  aiv_ap_pat                ,
                  int_gen_pgo_ext           ,
                  aiv_gen_pgo_ext           ,
                  localiza_trabajador       ,
                  tpo_aclaracion            ,
                  nrp                       ,
                  id_referencia             ,
                  periodo_pago              ,
                  folio_sua                 ,
                  tpo_patron                ,
                  ind_liquidacion           ,
                  f_valor                   ,
                  destino_ap_viv
             INTO v_det_id_derechohabiente  ,
                  v_det_imp_ap_pat          ,
                  v_det_imp_am_cre          ,
                  v_det_imp_ren_viv_pgo_ext ,
                  v_det_aiv_ap_pat          ,
                  v_det_int_gen_pgo_ext     ,
                  v_det_aiv_gen_pgo_ext     ,
                  v_localiza_trabajador     ,
                  v_tpo_aclaracion          ,
                  v_nrp                     ,
                  v_id_referencia           ,
                  v_periodo_pago            ,
                  v_cza_folio_sua           ,
                  v_tpo_patron              ,
                  v_ind_liquidacion         ,
                  v_cza_f_valor_4seguros    ,
                  v_destino_ap_viv
            FROM  cta_his_pagos
           WHERE folio = p_folio

      LET v_consecutivo = v_consecutivo + 1;


-- SE COMENTA SIGUIENTE FUNCIONALIDAD DE LUGAR DE TRABAJADOR YA
-- QUE NUNCA SE USO ESTA INFORMACIÓN Y ASI ACELARAR LA PRELIQUIDACION
-- 30-09-2014
{
      --Se ejecuta el store PROCEDURE en BD para la insertar el lugar del trabajador
      EXECUTE PROCEDURE sp_inserta_lugar_trabajador(v_det_id_derechohabiente
                                                   ,v_localiza_trabajador
                                                   ,TODAY
                                                   ,1
                                                   ,1  --El uno indica que proviene de registro de pagos
                                                   );
}


      --Se inicia las validaciones
      IF v_cza_f_valor_4seguros IS NULL THEN
         --Si no se encuentra el registro no continua con la preliquidacion pero se genera una excepcion en la tabla: pag_excep_preliquida
         --TRACE 'Sin f_valor_4seguros:' || v_consecutivo;
         INSERT INTO pag_excep_preliquida
                (folio,
                 id_referencia,
                 id_derechohabiente,
                 nrp,
                 periodo_pago,
                 folio_sua
                )
         VALUES (p_folio,
                 v_id_referencia,
                 v_det_id_derechohabiente,
                 v_nrp,
                 v_periodo_pago,
                 v_cza_folio_sua);

         CONTINUE FOREACH;
      END IF


         LET v_cont_nrp_riss = 0;
         
         IF EXISTS (SELECT nrp
	                  FROM   cat_riss_nrp 
	                  WHERE  nrp = v_nrp
	                  AND    id_nrp = 0) THEN
            LET v_cont_nrp_riss = 1;
         
         END IF   -- NRP ACTIVO
	       
         --- FUCIONALIDAD EFIRISS VOLUNTARIO ----	       
         IF v_cont_nrp_riss = 1 OR v_nrp = "B0799994105"  THEN -- EFIRESS VOLUNTARIO
         
            LET v_preliq_fondo_inversion = 11;
            LET v_preliq_folio_liquida   = p_folio;
            LET v_preliq_f_valor         = v_cza_f_valor_4seguros;
            LET v_preliq_id_referencia   = v_id_referencia;
            LET v_tpo_afiliacion         = 0;    --NO REGISTRADA
            
            
            --============================= INICIO REGLA DE NOGICIO 25-JUN-2012 GAVP =============================
            
            LET v_importe_valida = v_det_imp_ap_pat;  --Revisar si se utiliza con amortización
            
            IF v_localiza_trabajador = "3" THEN     -- 3 = Aclaraciones
            
               IF v_tpo_aclaracion = "13" OR v_tpo_aclaracion = "17" THEN
                  LET v_bandera_nrp = 0;     -- PAGO IMSS
                  LET v_ind_liquidacion = 3; -- ACL ADELANTADA FUSION(13) O TRASPASO(17)
                  LET v_tpo_afiliacion  = 1;    -- AFORE
            
                  -- Actualiza estado del pago
                  EXECUTE PROCEDURE sp_actualiza_estado_pago
                          (p_folio,                  --folio
                           v_id_referencia,          --id_referencia
                           v_det_id_derechohabiente, --id_derechohabiente
                           30,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA ADELANTADA
                           v_tpo_aclaracion,         --tpo_aclaracion
                           TODAY                     --f_actualiza
                          );
            
               ELSE
                  -- CONTINUA CON EL SIGUIENTE REGISTRO SIN PRELIQUIDAR
                  -- YA QUE ES UNA ACLARACION QUE NO SE LE DA SALIDA
                  INSERT INTO pag_excep_preliq_pag
                  VALUES (p_folio,v_id_referencia,v_det_id_derechohabiente,1);
            
                  -- Actualiza estado del pago
                  EXECUTE PROCEDURE sp_actualiza_estado_pago
                          (p_folio,                  --folio
                           v_id_referencia,          --id_referencia
                           v_det_id_derechohabiente, --id_derechohabiente
                           20,                       --estado_pago = ACLARATORIO SIN LIQUIDAR
                           v_tpo_aclaracion,         --tpo_aclaracion
                           TODAY                     --f_actualiza
                          );
            
                  CONTINUE FOREACH;
               END IF
               
               IF v_nrp = "B0799994105" THEN
                  LET v_preliq_movimiento     = 711;   --ABONO REG PAG ADE VOL RISS
                  LET v_preliq_movimiento_ren = 84;    --RENDIMIENTO ADE VOL RISS
                  LET v_preliq_movimiento_int = 143;   --INTERESES REG PAG LQ ADE VOL RISS
               ELSE
               	  LET v_preliq_movimiento     = 771;   --ABONO REG PAG ADE VOL RISS
               	  LET v_preliq_movimiento_int = 193;   --INTERESES REG PAG LQ ADE VOL RISS
               END IF
            
            ELSE --- Movimientos para aportes normales
            
               -- Actualiza estado del pago
               EXECUTE PROCEDURE sp_actualiza_estado_pago
                       (p_folio,                  --folio
                        v_id_referencia,          --id_referencia
                        v_det_id_derechohabiente, --id_derechohabiente
                        10,                       --estado_pago = PAGO NORMAL LIQUIDADO
                        v_tpo_aclaracion,         --tpo_aclaracion
                        TODAY                     --f_actualiza
                       );
                       
               IF v_nrp = "B0799994105" THEN
                  LET v_preliq_movimiento     = 701;   --ABONO REG PAG VOL RISS
                  LET v_preliq_movimiento_ren = 74;    --RENDIMIENTO VOL RISS
                  LET v_preliq_movimiento_int = 133;   --INTERESES PAG LQ VOL RISS
               ELSE
                  LET v_preliq_movimiento     = 761;   --ABONO REG PAG VOL RISS
                  LET v_preliq_movimiento_int = 183;   --INTERESES PAG LQ VOL RISS         	
               END IF
            
            END IF
            
            --Primer registro -- Aportación a subcuenta de Vivienda
            
            LET v_preliq_f_liquida          = TODAY;
            LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;
            
            --- SE AGREGA IF PARA PORTABILIDAD
            IF v_destino_ap_viv <> "3" THEN
               LET v_preliq_subcuenta = 55;    -- viv97
--               LET v_preliq_monto_acciones = v_det_aiv_ap_pat;   -- nvo port  efiriss vol
                 LET v_preliq_fondo_inversion = 11;
            ELSE
               LET v_preliq_subcuenta = 60;    -- PORTABILIDAD     
               LET v_preliq_fondo_inversion = 10;      	            	
--            	 IF v_localiza_trabajador = "3" THEN   --- Aclaracion
--                  LET v_preliq_monto_acciones = v_det_aiv_ap_pat;   -- nvo port  efiriss vol                          
--               ELSE
--                  LET v_preliq_monto_acciones = v_det_imp_ap_pat;   -- nvo port  efiriss vol           
--               END IF
            END IF   
            
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_aiv_ap_pat;

            LET v_preliq_f_registro = TODAY; --en caso de realizar la preliquidacion con muchos registros, actualiza cada registro
            LET v_preliq_h_registro = CURRENT HOUR TO SECOND; --actualiza tiempo para cada registro
            
            --Solo se inserta si el registro tiene importe mayor a CERO
            IF v_preliq_monto_pesos > 0 THEN
               INSERT INTO pag_lqinfo_preliquida
                            (f_liquida,
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
                             v_nrp);
            END IF
            
            --segundo registro - Intereses subcuenta de vivienda
            
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;        -- nvo port    	 
            
--            IF v_preliq_subcuenta = 60 THEN          -- nvo port
--            	 IF v_localiza_trabajador = "3" THEN   --- Aclaracion
--                  LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;
--               ELSE
--                  LET v_preliq_monto_acciones = v_det_int_gen_pgo_ext;               
--               END IF
--            ELSE 
--               LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;            	
--            END IF
            
            LET v_preliq_f_registro     = TODAY;
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
            
            --Solo se inserta si el registro tiene importe mayo a CERO
            IF v_preliq_monto_pesos > 0 THEN
               INSERT INTO pag_lqinfo_preliquida
                            (f_liquida,
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
                      VALUES(v_preliq_f_liquida            --f_liquida
                            ,v_preliq_id_derechohabiente   --id_derechohabiente
                            ,v_preliq_subcuenta            --subcuenta
                            ,v_preliq_fondo_inversion      --fondo_inversion
                            ,v_preliq_movimiento_int       --movimiento
                            ,v_preliq_folio_liquida        --folio_liquida
                            ,v_preliq_id_referencia        --id_referencia
                            ,v_preliq_monto_acciones       --monto_acciones
                            ,v_preliq_monto_pesos          --monto_pesos
                            ,v_preliq_f_valor              --f_valor
                            ,v_preliq_f_registro           --f_registro
                            ,v_preliq_h_registro           --h_registro
                            ,v_nrp);
            END IF
            ----------------- FIN NRP VOL RISS -----------------------------
            ----------------------------------------------------------------
            
         ELSE     
            ---------------------------------------------------------------
            --------------- FUNCIONALIDAD NORMAL --------------------------
            LET v_preliq_fondo_inversion = 11;
            LET v_preliq_folio_liquida   = p_folio;
            LET v_preliq_f_valor         = v_cza_f_valor_4seguros;
            LET v_preliq_id_referencia   = v_id_referencia;
            LET v_tpo_afiliacion         = 0;    --NO REGISTRADA
            
            
            --============================= INICIO REGLA DE NOGICIO 25-JUN-2012 GAVP =============================
            
            LET v_importe_valida = v_det_imp_ap_pat;  --Revisar si se utiliza con amortización
            
            IF v_localiza_trabajador = "3" THEN
            
               IF v_tpo_patron = "99" THEN
                  LET v_bandera_nrp = 1;        -- PAGO SOLO INFONAVIT
                  LET v_ind_liquidacion = 2;    -- ACL ADELANTADA SOLO INFONAVIT
                  LET v_tpo_afiliacion  = 2;    -- SOLO INFONAVIT
            
                  -- Actualiza estado del pago
                  EXECUTE PROCEDURE sp_actualiza_estado_pago
                          (p_folio,                  --folio
                           v_id_referencia,          --id_referencia
                           v_det_id_derechohabiente, --id_derechohabiente
                           30,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA ADELANTADA
                           v_tpo_aclaracion,         --tpo_aclaracion
                           TODAY                     --f_actualiza
                          );
            
               ELSE
                  IF v_tpo_aclaracion = "13" OR v_tpo_aclaracion = "17" THEN
                     LET v_bandera_nrp = 0;     -- PAGO IMSS
                     LET v_ind_liquidacion = 3; -- ACL ADELANTADA FUSION(13) O TRASPASO(17)
                     LET v_tpo_afiliacion  = 1;    -- AFORE
            
                     -- Actualiza estado del pago
                     EXECUTE PROCEDURE sp_actualiza_estado_pago
                             (p_folio,                  --folio
                              v_id_referencia,          --id_referencia
                              v_det_id_derechohabiente, --id_derechohabiente
                              30,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA ADELANTADA
                              v_tpo_aclaracion,         --tpo_aclaracion
                              TODAY                     --f_actualiza
                             );
            
                  ELSE
                     -- CONTINUA CON EL SIGUIENTE REGISTRO SIN PRELIQUIDAR
                     -- YA QUE ES UNA ACLARACION QUE NO SE LE DA SALIDA
                     INSERT INTO pag_excep_preliq_pag
                     VALUES (p_folio,v_id_referencia,v_det_id_derechohabiente,1);
            
                     -- Actualiza estado del pago
                     EXECUTE PROCEDURE sp_actualiza_estado_pago
                             (p_folio,                  --folio
                              v_id_referencia,          --id_referencia
                              v_det_id_derechohabiente, --id_derechohabiente
                              20,                       --estado_pago = ACLARATORIO SIN LIQUIDAR
                              v_tpo_aclaracion,         --tpo_aclaracion
                              TODAY                     --f_actualiza
                             );
            
                     CONTINUE FOREACH;
                  END IF
               END IF
            
               --TRACE 'v_ind_liquidacion:'||v_ind_liquidacion;
               --TRACE 'v_bandera_nrp:'||v_bandera_nrp;
            
               --- Movimientos para aclaraciones
               -- SE AGREGA ESTE IF POR J312 20-JUN-2014 --                   
               IF v_nrp = "B0799991101" THEN   -- RISS SCHP OBLIGATORIO o RISS SHCP INFONAVIT
                  LET v_preliq_movimiento     = 691;   --ABONO REG PAG LQ-ADELANTO RISS
                  LET v_preliq_movimiento_ren = 54;    --RENDIMIENTO ACL RISS
                  LET v_preliq_movimiento_int = 103;   --INTERESES REG PAG LQ-ADELANTO RISS
               ELSE
                  LET v_preliq_movimiento     = 451;   --ABONO REGISTRO DE PAGOS LQ-ADELANTO
                  LET v_preliq_movimiento_ren = 14;    --RENDIMIENTO ACL
                  LET v_preliq_movimiento_int = 83;    --INTERESES REGISTRO DE PAGOS LQ-ADELANTO
               END IF
            ELSE --- Movimientos para aportes normales
            
               -- Actualiza estado del pago
               EXECUTE PROCEDURE sp_actualiza_estado_pago
                       (p_folio,                  --folio
                        v_id_referencia,          --id_referencia
                        v_det_id_derechohabiente, --id_derechohabiente
                        10,                       --estado_pago = PAGO NORMAL LIQUIDADO
                        v_tpo_aclaracion,         --tpo_aclaracion
                        TODAY                     --f_actualiza
                       );
            
               -- SE AGREGA ESTE IF POR J312 20-JUN-2014 -- 
               IF v_nrp = "B0799991101" THEN  -- RISS SCHP OBLIGATORIO o RISS SHCP INFONAVIT
                  LET v_preliq_movimiento     = 681;   --ABONO SUBSIDIO RISS GOBIERNO FEDERAL
                  LET v_preliq_movimiento_ren = 64;    --RENDIMIENTO RISS
                  LET v_preliq_movimiento_int = 123;   --INTERESES RISS
               ELSE
                  LET v_preliq_movimiento     = 1;     --ABONO POR REGISTRO DE PAGOS
                  LET v_preliq_movimiento_ren = 4;     --RENDIMIENTO
                  LET v_preliq_movimiento_int = 3;     --INTERESES     
               END IF
            
            END IF
            
            --#### aqui inicia el proceso de preliquidación afectando a 4 tablas
            --TRACE 'Inicia Preliquidacion';
            
            --Primer registro -- Aportación a subcuenta de Vivienda
            
            LET v_preliq_f_liquida          = TODAY;
            LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;

            -- nvo port
            --- SE AGREGA IF PARA PORTABILIDAD            
            IF v_destino_ap_viv <> "3" THEN            
               IF v_bandera_nrp = 1 THEN
                 LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
               ELSE
                 LET v_preliq_subcuenta = 4;    -- viv97
               END IF
               LET v_preliq_fondo_inversion = 11;
--               LET v_preliq_monto_acciones = v_det_aiv_ap_pat;     -- nvo port  aport normal
            ELSE
               LET v_preliq_subcuenta = 60;   -- Portabilidad
               LET v_preliq_fondo_inversion = 10;
--            	 IF v_localiza_trabajador = "3" THEN   --- Aclaracion
--                  LET v_preliq_monto_acciones = v_det_aiv_ap_pat;     -- nvo port  aport normal
--               ELSE
--                  LET v_preliq_monto_acciones = v_det_imp_ap_pat;     -- nvo port  aport normal               	
--               END IF
            END IF            
            
            -- #Movimiento ya definido según localiza_trabajador LET v_preliq_movimiento     = 1;
            
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_aiv_ap_pat;    -- nvo port
            LET v_preliq_f_registro     = TODAY; --en caso de realizar la preliquidacion con muchos registros, actualiza cada registro
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND; --actualiza tiempo para cada registro
            
            --Solo se inserta si el registro tiene importe mayo a CERO
            IF v_preliq_monto_pesos > 0 THEN
               INSERT INTO pag_lqinfo_preliquida
                            (f_liquida,
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
                             v_nrp);
            END IF
            
            
            --segundo registro Abono a la subcuenta de amortización
            
            --LET v_preliq_f_liquida = TODAY;
            
            LET v_preliq_fondo_inversion = 10; -- Las amortizaciones van en pesos
            
            IF v_bandera_nrp = 1 THEN
              LET v_preliq_subcuenta = 43;  -- amort viv97 solo infonavit
            ELSE
              LET v_preliq_subcuenta = 41;  -- amort viv97
            END IF
            
            --# Movimiento ya predefinido según localiza_trabajador LLET v_preliq_movimiento     = 1;
            
            LET v_preliq_monto_pesos    = v_det_imp_am_cre;
            LET v_preliq_monto_acciones = v_det_imp_am_cre;  -- El valor es al 1x1
            LET v_preliq_f_registro     = TODAY;
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
            
            --Solo se inserta si el registro tiene importe mayo a CERO
            IF v_preliq_monto_pesos > 0 THEN
               INSERT INTO pag_lqinfo_preliquida
                             (f_liquida,
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
                             v_nrp);
            END IF
            -- HCRG 16-04-2012 si subcuenta fué 41 y moviemiento 1, siefore regresa a 11
            
            --3er Registro - Rendimiento subcuenta de vivienda

            LET v_preliq_fondo_inversion = 11;
            
            --LET v_preliq_f_liquida = TODAY;
            
            -- nvo port
            --- SE AGREGA IF PARA PORTABILIDAD            
            IF v_destino_ap_viv <> "3" THEN            
               IF v_bandera_nrp = 1 THEN
                 LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
               ELSE
                 LET v_preliq_subcuenta = 4;    -- viv97
               END IF
               LET v_preliq_fondo_inversion = 11;
--               LET v_preliq_monto_acciones = v_det_aiv_ap_pat;     -- nvo port  aport normal
            ELSE
               LET v_preliq_subcuenta = 60;   -- Portabilidad
               LET v_preliq_fondo_inversion = 10;
--            	 IF v_localiza_trabajador = "3" THEN   --- Aclaracion
--                  LET v_preliq_monto_acciones = v_det_aiv_ap_pat;     -- nvo port  aport normal
--               ELSE
--                  LET v_preliq_monto_acciones = v_det_imp_ap_pat;     -- nvo port  aport normal               	
--               END IF
            END IF            
            
            --# Movimiento ya predefinido según localiza_trabajador LET v_preliq_movimiento     = 4;
            LET v_preliq_monto_pesos    = v_det_imp_ren_viv_pgo_ext;
            LET v_preliq_monto_acciones = 0;
            LET v_preliq_f_registro     = TODAY;
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
            
            --Solo se inserta si el registro tiene importe mayor a CERO
            IF v_preliq_monto_pesos > 0 THEN
            
            	--se modifica el día 15 de junio para el calculo de aivs
            	IF v_preliq_subcuenta = 4 AND v_preliq_movimiento = 4 THEN
            		 --se calcual el mon to de acciones
            	   LET v_preliq_monto_acciones = fn_consulta_precio_fondo (v_preliq_monto_pesos ,
            	                                                         TODAY ) ;
            	END IF
                --se insertan los datos a la tabla de preliquidación
               INSERT INTO pag_lqinfo_preliquida
                            (f_liquida,
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
                      VALUES(v_preliq_f_liquida,
                             v_preliq_id_derechohabiente,
                             v_preliq_subcuenta,
                             v_preliq_fondo_inversion,
                             v_preliq_movimiento_ren,
                             v_preliq_folio_liquida,
                             v_preliq_id_referencia,
                             v_preliq_monto_acciones,
                             v_preliq_monto_pesos,
                             v_preliq_f_valor,
                             v_preliq_f_registro,
                             v_preliq_h_registro,
                             v_nrp);
            END IF
            
            --cuarto registro - Intereses subcuenta de vivienda
            
            --LET v_preliq_f_liquida = TODAY;

            -- nvo port
            --- SE AGREGA IF PARA PORTABILIDAD            
            IF v_destino_ap_viv <> "3" THEN            
               IF v_bandera_nrp = 1 THEN
                 LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
               ELSE
                 LET v_preliq_subcuenta = 4;    -- viv97
               END IF
               LET v_preliq_fondo_inversion = 11;               
--               LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;   -- nvo port   aport normal int_gen_pgo_ext            
            ELSE
               LET v_preliq_subcuenta = 60;   -- Portabilidad
               LET v_preliq_fondo_inversion = 10;               
--            	 IF v_localiza_trabajador = "3" THEN   --- Aclaracion
--                  LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;   -- nvo port   aport normal int_gen_pgo_ext         
--               ELSE
--                  LET v_preliq_monto_acciones = v_det_int_gen_pgo_ext;   -- nvo port   aport normal int_gen_pgo_ext                        	
--              END IF
            END IF
           
            --# Movimiento ya predefinido según localiza_trabajador LET v_preliq_movimiento     = 3;
            
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;   -- nvo port
            LET v_preliq_f_registro     = TODAY;
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
            
            --Solo se inserta si el registro tiene importe mayo a CERO
            IF v_preliq_monto_pesos > 0 THEN
               INSERT INTO pag_lqinfo_preliquida
                            (f_liquida,
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
                      VALUES(v_preliq_f_liquida            --f_liquida
                            ,v_preliq_id_derechohabiente   --id_derechohabiente
                            ,v_preliq_subcuenta            --subcuenta
                            ,v_preliq_fondo_inversion      --fondo_inversion
                            ,v_preliq_movimiento_int       --movimiento
                            ,v_preliq_folio_liquida        --folio_liquida
                            ,v_preliq_id_referencia        --id_referencia
                            ,v_preliq_monto_acciones       --monto_acciones
                            ,v_preliq_monto_pesos          --monto_pesos
                            ,v_preliq_f_valor              --f_valor
                            ,v_preliq_f_registro           --f_registro
                            ,v_preliq_h_registro           --h_registro
                            ,v_nrp);
            END IF
            --------------- FIN PROCESO NORMAL -------------------- 
         
         END IF




      LET v_bandera_nrp = 0;

   END FOREACH

   -- se verifica si se deshabilitaon los indices para rehabilitarlos
   IF ( v_indices_deshabilitados = 1 ) THEN   

      -- se desactiva la bandera
      LET v_indices_deshabilitados = 0;

      -- se reactivan los indices
      SET INDEXES xpkpag_ctr_pago  ENABLED;
      SET INDEXES xie1pag_ctr_pago ENABLED;

   END IF
   
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio;

   create index xpkpag_lqinfo_preliquida on pag_lqinfo_preliquida
        (folio_liquida) in afi_3_dbs;
   UPDATE STATISTICS FOR table pag_lqinfo_preliquida;
   UPDATE STATISTICS FOR table pag_ctr_pago;

    -- el proceso termino correctamente
   LET v_error        = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de Preliquidación finalizó correctamente.";

   RETURN v_error, isam_err, err_txt, v_det_id_derechohabiente;

END PROCEDURE;


