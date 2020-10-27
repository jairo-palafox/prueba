






CREATE procedure "safreviv".sp_reversa_solicitud_teso(g_nss          CHAR(11),  -- NSS DERECHOHABIENTE
                                           g_res_op       SMALLINT,  -- RESPUESTA DE OPERACIÛN.  ACEPTADO / RECHAZADO
                                           g_cod_rechazo  SMALLINT   -- CÛDIGO DE RECHAZO
                                         )


DEFINE v_importe    LIKE  cta_fondo72.importe          ;
DEFINE v_movimiento SMALLINT                           ;
DEFINE v_origen     CHAR(20)                           ;


DEFINE g_ar_id_afi_fondo72       DECIMAL(9,0)             ;
DEFINE g_ar_f_liquida            DATE                     ;
DEFINE g_ar_subcuenta            SMALLINT                 ;
DEFINE g_ar_movimiento           SMALLINT                 ;
DEFINE g_ar_folio_liquida        DECIMAL(9,0)             ;
DEFINE g_ar_id_referencia        DECIMAL(9,0)             ;
DEFINE g_ar_rfc                  CHAR(13)                 ;
DEFINE g_ar_nss                  CHAR(11)                 ;
DEFINE g_ar_nombre               CHAR(40)                 ;
DEFINE g_ar_importe              DECIMAL(22,2)            ;
DEFINE g_ar_estado_pago          CHAR(1)                  ;
DEFINE g_ar_tipo_pension         CHAR(1)                  ;
DEFINE g_ar_prescripcion         CHAR(1)                  ;
DEFINE g_ar_f_registro           DATE                     ;
DEFINE g_ar_h_registro           DATETIME HOUR TO SECOND  ;
DEFINE g_ar_origen               CHAR(20)                 ;
DEFINE v_id_afi_fondo72          LIKE cta_fondo72.id_afi_fondo72;

   LET v_movimiento = 141;
   LET v_origen     = "REVERSO W";

   LET g_ar_id_afi_fondo72 = 0;
   LET g_ar_f_liquida      = TODAY;
   LET g_ar_subcuenta      = 0;
   LET g_ar_movimiento     = 0;
   LET g_ar_folio_liquida  = 0;
   LET g_ar_id_referencia  = 0;
   LET g_ar_rfc            = NULL;
   LET g_ar_nss            = NULL;
   LET g_ar_nombre         = NULL;
   LET g_ar_importe        = 0;
   LET g_ar_estado_pago    = NULL;
   LET g_ar_tipo_pension   = NULL;
   LET g_ar_prescripcion   = NULL;
   LET g_ar_f_registro     = today;
   LET g_ar_h_registro     = CURRENT HOUR TO SECOND;
   LET g_ar_origen         = NULL;
   LET v_id_afi_fondo72    = 0;

    --SET DEBUG FILE TO '/ds/safreviv_int/BD/trace:sp_reversa_solicitud_teso.log';

         --TRACE ('antes del foreach');
         --TRACE (v_id_afi_fondo72);

         FOREACH SELECT
                    afi.id_afi_fondo72          --  id_afi_fondo72
                    ,f_liquida              --  f_liquida
                    ,subcuenta              --  subcuenta
                    ,movimiento             --  movimiento
                    ,folio_liquida          --  folio_liquida
                    ,id_referencia          --  id_referencia
                    ,afi.rfc                  
                    ,afi.nss                  
                    ,afi.nombre               
                    ,importe                --  importe
                    ,estado_pago            --  estado_pago
                    --,tipo_pension         --
                    --,prescripcion         --
                    ,f_registro             --  f_registro
                    ,h_registro             --  h_registro
                    ,origen                 --  origen
         INTO  g_ar_id_afi_fondo72,         --  id_afi_fondo72
               g_ar_f_liquida     ,         --  f_liquida
               g_ar_subcuenta     ,         --  subcuenta
               g_ar_movimiento    ,         --  movimiento
               g_ar_folio_liquida ,         --  folio_liquida
               g_ar_id_referencia ,         --  id_referencia
               g_ar_rfc           ,
               g_ar_nss           ,
               g_ar_nombre        ,
               g_ar_importe       ,         --  importe
               g_ar_estado_pago   ,         --  estado_pago
               --g_ar_tipo_pension  ,
               --g_ar_prescripcion  ,
               g_ar_f_registro    ,         --  f_registro
               g_ar_h_registro    ,         --  h_registro
               g_ar_origen                  --  origen
           FROM cta_fondo72 cta , 
                afi_fondo72 afi
          WHERE movimiento       in( 182,422)
            AND afi.id_afi_fondo72 = cta.id_afi_fondo72
            AND afi.nss               = g_nss

         --TRACE ('Total monto v_id_afi_fondo72');
         --TRACE (v_id_afi_fondo72);


  -- SELECT NVL(MAX(id_afi_fondo72),0)
  --   INTO v_id_afi_fondo72
  --   FROM cta_fondo72;


         --TRACE ('Total monto v_id_afi_fondo72 despues ');
         --TRACE (v_id_afi_fondo72);

      IF g_ar_importe < 0 THEN
        LET  g_ar_importe       = g_ar_importe * -1;
        LET g_ar_f_liquida      = TODAY                       ;
        LET g_ar_f_registro     = TODAY                       ;
        LET g_ar_h_registro     = CURRENT HOUR TO SECOND      ;
        LET g_ar_origen         = v_origen                    ;
        LET g_ar_movimiento     = v_movimiento                ;
        --LET v_id_afi_fondo72    = v_id_afi_fondo72 + 1        ;
        --LET g_ar_id_afi_fondo72 = v_id_afi_fondo72            ;

        IF g_res_op = 0 THEN
           INSERT INTO cta_fondo72 VALUES(g_ar_id_afi_fondo72,        --  id_afi_fondo72  
                                          g_ar_f_liquida     ,        --  f_liquida       
                                          g_ar_subcuenta     ,        --  subcuenta       
                                          g_ar_movimiento    ,        --  movimiento      
                                          g_ar_folio_liquida ,        --  folio_liquida   
                                          g_ar_id_referencia ,        --  id_referencia   
                                          g_ar_rfc           ,                            
                                          g_ar_nss           ,                            
                                          g_ar_nombre        ,                            
                                          g_ar_importe       ,        --  importe         
                                          g_ar_estado_pago   ,        --  estado_pago     
                                          --g_ar_tipo_pension  ,        --                  
                                          --g_ar_prescripcion  ,        --                  
                                          g_ar_f_registro    ,        --  f_registro      
                                          g_ar_h_registro    ,        --  h_registro      
                                          g_ar_origen                 --  origen          
                                          );

           INSERT INTO ret_preliquida72 VALUES(g_ar_id_afi_fondo72,       --  id_afi_fondo72  
                                               g_ar_f_liquida     ,       --  f_liquida       
                                               g_ar_subcuenta     ,       --  subcuenta       
                                               g_ar_movimiento    ,       --  movimiento      
                                               g_ar_folio_liquida ,       --  folio_liquida   
                                               g_ar_id_referencia ,       --  id_referencia   
                                               g_ar_rfc           ,                           
                                               g_ar_nss           ,                           
                                               g_ar_nombre        ,                           
                                               g_ar_importe       ,       --  importe         
                                               g_ar_estado_pago   ,       --  estado_pago     
                                               --g_ar_tipo_pension  ,       --                  
                                               --g_ar_prescripcion  ,       --                  
                                               g_ar_f_registro    ,       --  f_registro      
                                               g_ar_h_registro    ,       --  h_registro      
                                               g_ar_origen                --  origen          
                                               );

           UPDATE  ret_fondo_ahorro
              SET  estado_solicitud = 90
                  ,cod_rechazo      = g_cod_rechazo
            WHERE id_solicitud      = g_ar_id_referencia
              AND estado_solicitud  = 70;
        ELSE
            UPDATE  ret_fondo_ahorro
              SET  estado_solicitud = 80
                  ,cod_rechazo      = 0
            WHERE id_solicitud      = g_ar_id_referencia
              AND estado_solicitud  = 70;
  
        END IF
      END IF
   END FOREACH;
END procedure
;


