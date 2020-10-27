






CREATE procedure "safreviv".sp_reversa_solicitud_teso_info(g_nss          CHAR(11),  -- NSS DERECHOHABIENTE
                                                g_res_op       SMALLINT,  -- RESPUESTA DE OPERACIÛN.  ACEPTADO / RECHAZADO
                                                g_cod_rechazo  SMALLINT   -- CÛDIGO DE RECHAZO
                                               )


 DEFINE v_importe           LIKE  cta_movimiento.monto_pesos;
 DEFINE v_movimiento_reverso      SMALLINT                ;
 DEFINE v_movimiento_ant          SMALLINT                ;
 DEFINE v_origen                  CHAR(20)                ;
 DEFINE g_id_derechohabiente      DECIMAL(9,0)            ;
 --DEFINE v_id_cta_movimiento smallint   ;

 DEFINE g_ar_f_liquida            DATE                    ;
 DEFINE g_ar_id_derechohabiente   DECIMAL(9,0)            ;
 DEFINE g_ar_subcuenta            SMALLINT                ;
 DEFINE g_ar_fondo_inversion      SMALLINT                ;
 DEFINE g_ar_movimiento           SMALLINT                ;
 DEFINE g_ar_folio_liquida        DECIMAL(10,0)           ;
 DEFINE g_ar_id_referencia        DECIMAL(9,0)            ;
 DEFINE g_ar_monto_acciones       DECIMAL(20,2)           ;
 DEFINE g_ar_monto_pesos          DECIMAL(20,2)           ;
 DEFINE g_ar_f_valor              DATE                    ;
 DEFINE g_ar_f_registro           DATE                    ;
 DEFINE g_ar_h_registro           DATETIME HOUR TO SECOND ;
 DEFINE g_ar_origen               CHAR(20)                ;

 LET v_movimiento_reverso = 131;
 LET v_movimiento_ant     = 172;
 LET v_origen             = "REVERSO W";

 LET g_ar_f_liquida            = TODAY;
 LET g_ar_id_derechohabiente   = 0;
 LET g_ar_subcuenta            = 0;
 LET g_ar_fondo_inversion      = 0;
 LET g_ar_movimiento           = 0;
 LET g_ar_folio_liquida        = 0;
 LET g_ar_id_referencia        = 0;
 LET g_ar_monto_acciones       = 0;
 LET g_ar_monto_pesos          = 0;
 LET g_ar_f_valor              = TODAY;
 LET g_ar_f_registro           = TODAY;
 LET g_ar_h_registro           = CURRENT HOUR TO SECOND;
 LET g_ar_origen               = NULL;
 
 SELECT id_derechohabiente 
   INTO g_id_derechohabiente
   FROM afi_derechohabiente
   WHERE nss = g_nss;


    --SET DEBUG FILE TO '/ds/safreviv_int/BD/trace:sp_reversa_solicitud_teso_info.log';

         --TRACE ('antes del foreach');
         

      FOREACH SELECT *   INTO       g_ar_f_liquida          ,
                                    g_ar_id_derechohabiente ,
                                    g_ar_subcuenta          ,
                                    g_ar_fondo_inversion    ,
                                    g_ar_movimiento         ,
                                    g_ar_folio_liquida      ,
                                    g_ar_id_referencia      ,
                                    g_ar_monto_acciones     ,
                                    g_ar_monto_pesos        ,
                                    g_ar_f_valor            ,
                                    g_ar_f_registro         ,
                                    g_ar_h_registro         ,
                                    g_ar_origen
                                        FROM cta_movimiento
                                         WHERE movimiento           = v_movimiento_ant
                                           AND id_derechohabiente   = g_id_derechohabiente

      IF g_ar_monto_pesos < 0 THEN
        LET g_ar_monto_pesos    = g_ar_monto_pesos    * -1;
        LET g_ar_monto_acciones = g_ar_monto_acciones * -1;
        LET g_ar_f_liquida      = TODAY                       ;
        LET g_ar_f_registro     = TODAY                       ;
        LET g_ar_h_registro     = CURRENT HOUR TO SECOND      ;
        LET g_ar_origen         = v_origen                    ;
        LET g_ar_movimiento     = v_movimiento_reverso        ;


        IF g_res_op = 0 THEN
           INSERT INTO cta_movimiento VALUES( g_ar_f_liquida          ,
                                                        g_ar_id_derechohabiente ,
                                                        g_ar_subcuenta          ,
                                                        g_ar_fondo_inversion    ,
                                                        g_ar_movimiento         ,
                                                        g_ar_folio_liquida      ,
                                                        g_ar_id_referencia      ,
                                                        g_ar_monto_acciones     ,
                                                        g_ar_monto_pesos        ,
                                                        g_ar_f_valor            ,
                                                        g_ar_f_registro         ,
                                                        g_ar_h_registro         ,
                                                        g_ar_origen
                                                         );

           INSERT INTO ret_preliquida VALUES(g_ar_f_liquida          ,
                                                       g_ar_id_derechohabiente ,
                                                       g_ar_subcuenta          ,
                                                       g_ar_fondo_inversion    ,
                                                       g_ar_movimiento         ,
                                                       g_ar_folio_liquida      ,
                                                       g_ar_id_referencia      ,
                                                       g_ar_monto_acciones     ,
                                                       g_ar_monto_pesos        ,
                                                       g_ar_f_valor            ,
                                                       g_ar_f_registro         ,
                                                       g_ar_h_registro         ,
                                                       g_ar_origen
                                                         );

           UPDATE  ret_solo_infonavit
              SET  estado_solicitud = 90
                  ,cod_rechazo      = g_cod_rechazo
            WHERE id_solicitud = g_ar_id_referencia
              AND estado_solicitud = 70;
        ELSE
        IF g_res_op = 1 THEN
           UPDATE  ret_solo_infonavit
              SET  estado_solicitud = 80
                  ,cod_rechazo      = 0
            WHERE id_solicitud = g_ar_id_referencia
              AND estado_solicitud = 70;

         ELSE
             UPDATE  ret_solo_infonavit
              SET  estado_solicitud  = 70
                  ,cod_rechazo       = 0
            WHERE id_solicitud       = g_ar_id_referencia
              AND estado_solicitud   = 60
              AND id_derechohabiente = g_ar_id_derechohabiente;
         END IF
        END IF
      END IF
   END FOREACH;
END procedure
;


