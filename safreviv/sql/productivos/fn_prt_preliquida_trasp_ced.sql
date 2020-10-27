






create procedure "safreviv".fn_prt_preliquida_trasp_ced(p_folio DECIMAL(9,0),
                                             p_usuario CHAR(20))

RETURNING SMALLINT         , --v_error_sql
          SMALLINT         , --v_error_isam
          CHAR(255)        , --v_msq_sql
          INTEGER          , --v_tot_registros
          DECIMAL(22,2)    , --v_tot_aivs
          DECIMAL(22,2)    ; --v_tot_pesos

-- registro traspaso cedente

DEFINE v_t_id_prt_traspaso_cedente         DECIMAL(9,0)   ;
DEFINE v_t_id_prt_solicitud_cedente        DECIMAL(9,0)   ;
--DEFINE v_t_folio_liquida                   DECIMAL(9,0)   ;
--DEFINE v_t_folio_procesar                  CHAR(50)       ;
DEFINE v_t_id_derechohabiente              DECIMAL(9,0)   ;
--DEFINE v_t_nss                             CHAR(11)       ;
--DEFINE v_t_precio_aiv_infonavit97_cedido   DECIMAL(16,6)  ;
DEFINE v_t_f_precio_aiv_infonavit92_cedido DATE           ;
DEFINE v_t_f_precio_aiv_infonavit97_cedido DATE           ;

DEFINE v_t_mto_aivs_infonavit97_cedido     DECIMAL(22,2)  ;
DEFINE v_t_mto_pesos_infonavit97_cedido    DECIMAL(22,2)  ;
DEFINE v_t_mto_aivs_infonavit92_cedido     DECIMAL(22,2)  ;
DEFINE v_t_mto_pesos_infonavit92_cedido    DECIMAL(22,2)  ;

DEFINE v_t_f_valor_transferencia           DATE           ;
DEFINE v_t_estado                          SMALLINT       ;
DEFINE v_t_usuario                         CHAR(20)       ;

-- registro preliquidacion

DEFINE v_p_f_liquida                       DATE           ;
DEFINE v_p_id_derechohabiente              DECIMAL(9,0)   ;
DEFINE v_p_subcuenta                       SMALLINT       ;
DEFINE v_p_subcuenta92                     SMALLINT       ;
DEFINE v_p_fondo_inversion                 SMALLINT       ;
DEFINE v_p_movimiento                      SMALLINT       ;
DEFINE v_p_subcuenta_60                    SMALLINT       ;
DEFINE v_p_fondo_inversion_10              SMALLINT       ;
DEFINE v_p_movimiento_cargo                SMALLINT       ;
DEFINE v_p_movimiento_abono                SMALLINT       ;
DEFINE v_p_folio_liquida                   DECIMAL(9,0)   ;
DEFINE v_p_id_referencia                   DECIMAL(9,0)   ;
DEFINE v_p_monto_acciones_97               DECIMAL(22,2)  ;
DEFINE v_p_monto_pesos_97                  DECIMAL(22,2)  ;
DEFINE v_p_monto_acciones_92               DECIMAL(22,2)  ;
DEFINE v_p_monto_pesos_92                  DECIMAL(22,2)  ;
DEFINE v_p_monto_acciones_inv10            DECIMAL(22,2)  ; -- En fondo de inversión 10
DEFINE v_p_monto_pesos_inv10               DECIMAL(22,2)  ; -- En fondo de inversión 10
DEFINE v_p_f_valor                         DATE           ;
DEFINE v_p_f_registro                      DATE           ;
DEFINE v_p_h_registro                      DATETIME HOUR TO SECOND;
DEFINE v_p_origen                          CHAR(20)       ;

DEFINE v_tipo_movimiento_cargo  SMALLINT;
DEFINE v_tipo_movimiento_abono  SMALLINT;

-- variables de control de excepciones y maq estado

DEFINE v_ind                               SMALLINT       ;
DEFINE v_diag                              CHAR(255)      ;
DEFINE v_error_sql                         SMALLINT       ;
DEFINE v_error_isam                        SMALLINT       ;
DEFINE v_msg_sql                           CHAR(255)      ;
DEFINE v_estado_destino                    SMALLINT       ;

-- variables de trabajo

DEFINE v_tot_registros                     INTEGER        ;
DEFINE v_tot_aivs                          DECIMAL(22,2)  ;
DEFINE v_tot_pesos                         DECIMAL(22,2)  ;

   ON EXCEPTION SET v_error_sql   , -- cod error sql
                    v_error_isam  , -- cod error isam
                    v_msg_sql       -- descripcion error

      RETURN v_error_sql,v_error_isam,v_msg_sql,v_tot_registros,v_tot_aivs,v_tot_pesos;

   END EXCEPTION;

   --SET DEBUG FILE TO '/tmp/fn_prt_preliquida_trasp_ced.trace';
   --TRACE ON;


   LET v_ind        = 0  ;
   LET v_diag       = "" ;
   LET v_error_sql  = 0  ;
   LET v_error_isam = 0  ;
   LET v_msg_sql    = "" ;

   LET v_tot_registros = 0 ;
   LET v_tot_aivs      = 0 ;
   LET v_tot_pesos     = 0 ;
   LET v_p_subcuenta       = 4      ; -- viv97
   LET v_p_subcuenta92     = 8      ; -- viv97
   LET v_p_fondo_inversion = 11     ; -- aivs
   LET v_p_movimiento      = 1608   ;
   LET v_p_folio_liquida   = p_folio;
   LET v_p_subcuenta_60       = 60;    -- PORTABILIDAD
   LET v_p_fondo_inversion_10 = 10;    -- PESOS 
   LET v_p_movimiento_cargo   = 1610;  -- Cargo a portabilidad 
   LET v_p_movimiento_abono   = 1611;  -- Abono a portabilidad

   LET v_p_monto_acciones_97  = 0;
   LET v_p_monto_pesos_97     = 0;
   LET v_p_monto_acciones_92  = 0;
   LET v_p_monto_pesos_92     = 0;

   SELECT tipo
     INTO v_tipo_movimiento_cargo
     FROM cat_movimiento
    WHERE movimiento = v_p_movimiento_cargo;

   SELECT tipo
     INTO v_tipo_movimiento_abono
     FROM cat_movimiento
    WHERE movimiento = v_p_movimiento_abono;

   -- se actualiza el folio a preliquidado desde antes para facilitar el reverso
   -- en caso de error

   UPDATE glo_folio
   SET    status = 1
   WHERE  folio  = p_folio ;


   -- Se identifican los traspasos pendientes de ser aplicados

   FOREACH cur_solicitud  FOR

        SELECT b.id_prt_traspaso_cedente                ,
               b.id_prt_solicitud_cedente               ,
               b.id_derechohabiente                     ,
               b.f_precio_aiv_infonavit97_cedido        ,
               b.mto_aivs_infonavit97_cedido            ,
               b.mto_pesos_infonavit97_cedido           ,
               b.f_precio_aiv_infonavit92_cedido        ,
               b.mto_aivs_infonavit92_cedido            ,
               b.mto_pesos_infonavit92_cedido           ,
               b.f_valor_transferencia                  ,
               b.usuario
          INTO v_t_id_prt_traspaso_cedente              ,
               v_t_id_prt_solicitud_cedente             ,
               v_t_id_derechohabiente                   ,
               v_t_f_precio_aiv_infonavit97_cedido      ,
               v_t_mto_aivs_infonavit97_cedido          ,
               v_t_mto_pesos_infonavit97_cedido         ,
               v_t_f_precio_aiv_infonavit92_cedido      ,
               v_t_mto_aivs_infonavit92_cedido          ,
               v_t_mto_pesos_infonavit92_cedido         ,
               v_t_f_valor_transferencia                ,
               v_t_usuario
          FROM prt_solicitud_cedente a  ,
               prt_traspaso_cedente  b
         WHERE a.estado                   = 60 -- saldo notificado procesar
           AND a.id_prt_solicitud_cedente = b.id_prt_solicitud_cedente
           AND b.estado                   = 20 -- saldo notificado procesar

        UPDATE prt_traspaso_cedente
           SET estado = 30,  -- preliquidada
               folio_liquida = p_folio
         WHERE id_prt_traspaso_cedente = v_t_id_prt_traspaso_cedente;

        EXECUTE FUNCTION fn_glo_maq_individual(2                             , -- maq portabilidad
                                               v_t_id_prt_solicitud_cedente  , -- id de la solicitud
                                               60                            , -- recibir diagnostico procesar aceptado
                                               p_usuario )                     -- usuario
                    INTO v_ind            ,
                         v_diag           ,
                         v_error_sql      ,
                         v_error_isam     ,
                         v_msg_sql        ,
                         v_estado_destino;



        LET v_p_monto_acciones_inv10 = 0;
        LET v_p_monto_pesos_inv10    = 0;
        LET v_p_monto_acciones_97    = 0;
        LET v_p_monto_pesos_97       = 0;
        LET v_p_monto_acciones_92    = 0;
        LET v_p_monto_pesos_92       = 0;

        -- Se provisiona el cargo por portabilidad en la subcuenta 4 se obtienen pesos al primer dia natural
        -- del mes de liquidacion (mes siguiente a la recepcion de la notificacion de procesar)

        IF v_t_mto_pesos_infonavit97_cedido > 0 THEN

        LET v_p_f_liquida           = v_t_f_precio_aiv_infonavit97_cedido;
        LET v_p_id_derechohabiente  = v_t_id_derechohabiente             ;
        LET v_p_id_referencia       = v_t_id_prt_traspaso_cedente        ;
        LET v_p_monto_acciones_97   = - v_t_mto_aivs_infonavit97_cedido  ;
        LET v_p_monto_pesos_97      = - v_t_mto_pesos_infonavit97_cedido ;
        LET v_p_f_valor             = MDY(MONTH(v_t_f_precio_aiv_infonavit97_cedido),"01",YEAR(v_t_f_precio_aiv_infonavit97_cedido)) ;
        LET v_p_f_registro          = TODAY                              ;
        LET v_p_h_registro          = CURRENT HOUR TO SECOND             ;
        LET v_p_origen              = "TRASP PORTABILIDAD"               ;

        -- Movimiento de cargo por traspaso en vivienda 97


           INSERT INTO prt_preliquida ( f_liquida               ,
                                        id_derechohabiente      ,
                                        subcuenta               ,
                                        fondo_inversion         ,
                                        movimiento              ,
                                        folio_liquida           ,
                                        id_referencia           ,
                                        monto_acciones          ,
                                        monto_pesos             ,
                                        f_valor                 ,
                                        f_registro              ,
                                        h_registro              ,
                                        origen                   )
                               VALUES ( v_p_f_liquida           ,
                                        v_p_id_derechohabiente  ,
                                        v_p_subcuenta           ,
                                        v_p_fondo_inversion     ,
                                        v_p_movimiento          ,
                                        v_p_folio_liquida       ,
                                        v_p_id_referencia       ,
                                        v_p_monto_acciones_97   ,
                                        v_p_monto_pesos_97      ,
                                        v_p_f_valor             ,
                                        v_p_f_registro          ,
                                        v_p_h_registro          ,
                                        v_p_origen               );

           EXECUTE FUNCTION fn_consulta_precio_fondo(v_t_mto_pesos_infonavit97_cedido,
                                                     v_t_f_precio_aiv_infonavit97_cedido,
                                                     v_p_fondo_inversion_10) INTO v_t_mto_aivs_infonavit97_cedido;

           LET v_p_monto_acciones_inv10 = v_p_monto_acciones_inv10 + v_t_mto_aivs_infonavit97_cedido;
           LET v_p_monto_pesos_inv10    = v_p_monto_pesos_inv10    + v_t_mto_pesos_infonavit97_cedido;

        END IF

        IF v_t_mto_pesos_infonavit92_cedido > 0 THEN

        -- Se provisiona el cargo por portabilidad en la subcuenta 8 se obtienen pesos al primer dia natural
        -- del mes de liquidacion (mes siguiente a la recepcion de la notificacion de procesar)

        LET v_p_f_liquida           = v_t_f_precio_aiv_infonavit92_cedido;
        LET v_p_id_derechohabiente  = v_t_id_derechohabiente             ;
        LET v_p_id_referencia       = v_t_id_prt_traspaso_cedente        ;
        LET v_p_monto_acciones_92   = - v_t_mto_aivs_infonavit92_cedido  ;
        LET v_p_monto_pesos_92      = - v_t_mto_pesos_infonavit92_cedido ;
        LET v_p_f_valor             = v_t_f_precio_aiv_infonavit92_cedido;
        LET v_p_f_registro          = TODAY                              ;
        LET v_p_h_registro          = CURRENT HOUR TO SECOND             ;
        LET v_p_origen              = "TRASP PORTABILIDAD"               ;

        -- Movimiento de cargo por traspaso en vivienda 97
        INSERT INTO prt_preliquida ( f_liquida               ,
                                     id_derechohabiente      ,
                                     subcuenta               ,
                                     fondo_inversion         ,
                                     movimiento              ,
                                     folio_liquida           ,
                                     id_referencia           ,
                                     monto_acciones          ,
                                     monto_pesos             ,
                                     f_valor                 ,
                                     f_registro              ,
                                     h_registro              ,
                                     origen                   )
                            VALUES ( v_p_f_liquida           ,
                                     v_p_id_derechohabiente  ,
                                     v_p_subcuenta92         ,
                                     v_p_fondo_inversion     ,
                                     v_p_movimiento          ,
                                     v_p_folio_liquida       ,
                                     v_p_id_referencia       ,
                                     v_p_monto_acciones_92   ,
                                     v_p_monto_pesos_92      ,
                                     v_p_f_valor             ,
                                     v_p_f_registro          ,
                                     v_p_h_registro          ,
                                     v_p_origen               );

        EXECUTE FUNCTION fn_consulta_precio_fondo(v_t_mto_pesos_infonavit92_cedido,
                                                  v_t_f_precio_aiv_infonavit92_cedido,
                                                  v_p_fondo_inversion_10) INTO v_t_mto_aivs_infonavit92_cedido;

           LET v_p_monto_acciones_inv10 = v_p_monto_acciones_inv10 + v_t_mto_aivs_infonavit92_cedido;
           LET v_p_monto_pesos_inv10    = v_p_monto_pesos_inv10    + v_t_mto_pesos_infonavit92_cedido;

        END IF
                                              
        LET v_p_monto_acciones_inv10  = v_p_monto_acciones_inv10 * v_tipo_movimiento_abono;
        LET v_p_monto_pesos_inv10     = v_p_monto_pesos_inv10    * v_tipo_movimiento_abono;

        -- Movimiento de abono en portabilidad
        INSERT INTO prt_preliquida ( f_liquida         ,
                                     id_derechohabiente,
                                     subcuenta         ,
                                     fondo_inversion   ,
                                     movimiento        ,
                                     folio_liquida     ,
                                     id_referencia     ,
                                     monto_acciones    ,
                                     monto_pesos       ,
                                     f_valor           ,
                                     f_registro        ,
                                     h_registro        ,
                                     origen )
                            VALUES ( v_p_f_liquida         ,
                                     v_p_id_derechohabiente,
                                     v_p_subcuenta_60      ,
                                     v_p_fondo_inversion_10,
                                     v_p_movimiento_abono  ,
                                     v_p_folio_liquida     ,
                                     v_p_id_referencia     ,
                                     v_p_monto_acciones_inv10,
                                     v_p_monto_pesos_inv10   ,
                                     v_p_f_valor           ,
                                     v_p_f_registro        ,
                                     v_p_h_registro        ,
                                     v_p_origen);
                              
        LET v_p_monto_acciones_inv10  = v_p_monto_acciones_inv10 * v_tipo_movimiento_cargo;
        LET v_p_monto_pesos_inv10     = v_p_monto_pesos_inv10    * v_tipo_movimiento_cargo;

        -- Movimiento de cargo en portabilidad
        INSERT INTO prt_preliquida ( f_liquida         ,
                                     id_derechohabiente,
                                     subcuenta         ,
                                     fondo_inversion   ,
                                     movimiento        ,
                                     folio_liquida     ,
                                     id_referencia     ,
                                     monto_acciones    ,
                                     monto_pesos       ,
                                     f_valor           ,
                                     f_registro        ,
                                     h_registro        ,
                                     origen )
                            VALUES ( v_p_f_liquida           ,
                                     v_p_id_derechohabiente  ,
                                     v_p_subcuenta_60        ,
                                     v_p_fondo_inversion_10  ,
                                     v_p_movimiento_cargo    ,
                                     v_p_folio_liquida       ,
                                     v_p_id_referencia       ,
                                     v_p_monto_acciones_inv10,
                                     v_p_monto_pesos_inv10   ,
                                     v_p_f_valor             ,
                                     v_p_f_registro          ,
                                     v_p_h_registro          ,
                                     v_p_origen);

        LET v_tot_registros = v_tot_registros + 1            ;
        LET v_tot_aivs      = v_tot_aivs      + v_p_monto_acciones_92 + v_p_monto_acciones_97;
        LET v_tot_pesos     = v_tot_pesos     + v_p_monto_pesos_92 + v_p_monto_pesos_97;

   END FOREACH;

RETURN v_error_sql,v_error_isam,v_msg_sql,v_tot_registros,v_tot_aivs,v_tot_pesos;

END PROCEDURE;


