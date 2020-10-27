






create procedure "safreviv".fn_prt_preliquida_subs_ced(p_folio DECIMAL(9,0),
                                             p_usuario CHAR(20))

RETURNING SMALLINT         , --v_error_sql
          SMALLINT         , --v_error_isam
          CHAR(255)        , --v_msq_sql
          INTEGER          , --v_tot_registros
          DECIMAL(22,2)    , --v_tot_aivs
          DECIMAL(22,2)    ; --v_tot_pesos


--------------------------------------------------------------------
-- ultima modificacion: 4 abr 2015
--
-- funcion que identifica las aportaciones subsecuentes a enviar
-- a fovissste por concepto de portabilidad.
-- se identifican las aportaciones subsecuentes puras, es decir las
-- que provienen de creditos preexistentes y tambien las que 
-- de manera previa tuvieron una transferencia de saldo por  
-- portabilidad.
--
--
-- funciones que utiliza : fn_saldo_dia                 
--                         sp_prt_consulta_subsecuentes  
--                         fn_glo_maq_individual 
--                         fn_ultimo_dia_mes  
--------------------------------------------------------------------


-- registro solicitud cedente

DEFINE v_s_id_prt_solicitud_cedente           DECIMAL(9,0)    ;
DEFINE v_s_nss                                CHAR(11)        ;
DEFINE v_s_curp                               CHAR(18)        ;
DEFINE v_s_paterno                            CHAR(40)        ;
DEFINE v_s_materno                            CHAR(40)        ;
DEFINE v_s_nombre                             CHAR(40)        ;
DEFINE v_s_id_credito_fovissste               DECIMAL(10,0)    ;
DEFINE v_s_correo_e                           CHAR(100)       ;
DEFINE v_s_telefono                           CHAR(100)       ;
DEFINE v_s_saldo_insoluto_credito_fovissste   DECIMAL(22,2)   ;
DEFINE v_s_aivs_saldo_viv97_infonavit         DECIMAL(22,2)   ;
DEFINE v_s_pesos_saldo_viv97_infonavit        DECIMAL(22,2)   ;
DEFINE v_s_aivs_saldo_viv97_afore             DECIMAL(22,2)   ;
DEFINE v_s_pesos_saldo_viv97_afore            DECIMAL(22,2)   ;
DEFINE v_s_aivs_viv97_cedido                  DECIMAL(22,2)   ;
DEFINE v_s_pesos_viv97_cedido                 DECIMAL(22,2)   ;
DEFINE v_s_f_originacion_fovissste            DATE            ;
DEFINE v_s_f_fin_credito                      DATE;
DEFINE v_s_f_consulta_credito                 DATE            ;
DEFINE v_s_f_vigencia                         DATE            ;
DEFINE v_s_f_ini_tramite                      DATE            ;
DEFINE v_s_tipo_portabilidad                  SMALLINT        ;
DEFINE v_s_n_caso                             VARCHAR(20)     ;
DEFINE v_s_estado                             SMALLINT        ;
DEFINE v_s_resultado_operacion                CHAR(2)         ;
DEFINE v_s_diagnostico_interno                CHAR(3)         ;
DEFINE v_s_folio_procesar                     CHAR(50)        ;


-- registro traspaso cedente


DEFINE v_t_id_prt_traspaso_cedente                   DECIMAL(9,0)     ;
DEFINE v_t_id_prt_solicitud_cedente                  DECIMAL(9,0)     ;
DEFINE v_t_folio_liquida                             DECIMAL(9,0)      ;
DEFINE v_t_folio_procesar                            CHAR(50)         ;
DEFINE v_t_instituto_origen                          CHAR(3)          ;
DEFINE v_t_tpo_movimiento                            CHAR(2)          ;
DEFINE v_t_id_derechohabiente                        DECIMAL(9,0)     ;
DEFINE v_t_nss                                       CHAR(11)         ;
DEFINE v_t_ap_paterno                                CHAR(40)          ;
DEFINE v_t_ap_materno                                CHAR(40)          ;
DEFINE v_t_nombre                                    CHAR(40)          ;
DEFINE v_t_curp                                      CHAR(18)         ;
DEFINE v_t_tpo_operacion                             CHAR(2)          ;
DEFINE v_t_id_credito_fovissste                      DECIMAL(10,0)    ;
DEFINE v_t_sdo_insoluto_fovissste                    DECIMAL(22,2)    ;
DEFINE v_t_f_originacion_fovissste                   DATE             ;
DEFINE v_t_precio_aiv_infonavit97                    DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97                  DATE             ;
DEFINE v_t_mto_aivs_infonavit97                      DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97                     DECIMAL(22,2)    ;
DEFINE v_t_precio_aiv_infonavit97_afo                DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97_afo              DATE             ;
DEFINE v_t_mto_aivs_infonavit97_afo                  DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_afo                 DECIMAL(22,2)    ;
DEFINE v_t_ind_origen_sdo_traspaso                   SMALLINT         ;
DEFINE v_t_f_precio_aiv_infonavit97_recalculado      DATE             ;
DEFINE v_t_precio_aiv_infonavit97_recalculado        DECIMAL(16,6)    ;
DEFINE v_t_mto_aivs_infonavit97_recalculado          DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_recalculado         DECIMAL(22,2)    ;
DEFINE v_t_precio_aiv_infonavit97_cedido             DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97_cedido           DATE             ;
DEFINE v_t_mto_aivs_infonavit97_cedido               DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_cedido              DECIMAL(22,2)    ;
DEFINE v_t_mto_aivs_infonavit97_diferencia           DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_diferencia          DECIMAL(22,2)    ;
DEFINE v_t_f_valor_transferencia                     DATE             ;
DEFINE v_t_diag_procesar                             CHAR(3)          ;
DEFINE v_t_estado                                    SMALLINT         ;
DEFINE v_t_usuario                                   CHAR(20)         ;

-- registro preliquidacion

DEFINE v_p_f_liquida                       DATE           ;
DEFINE v_p_id_derechohabiente              DECIMAL(9,0)   ;
DEFINE v_p_subcuenta                       SMALLINT       ;
DEFINE v_p_fondo_inversion                 SMALLINT       ;
DEFINE v_p_movimiento                      SMALLINT       ;
DEFINE v_p_folio_liquida                   DECIMAL(9,0)   ;
DEFINE v_p_id_referencia                   DECIMAL(9,0)   ;
DEFINE v_p_monto_acciones                  DECIMAL(22,2)  ;
DEFINE v_p_monto_pesos                     DECIMAL(22,2)  ;
DEFINE v_p_f_valor                         DATE           ;
DEFINE v_p_f_registro                      DATE           ;
DEFINE v_p_h_registro                      DATETIME HOUR TO SECOND;
DEFINE v_p_origen                          CHAR(20)       ;

DEFINE v_precio_fondo                      DECIMAL(19,14) ;

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
--DEFINE v_nss                               CHAR(011)      ;
DEFINE v_id_derechohabiente                DECIMAL(9,0)  ;
DEFINE v_ultimo_dia                        SMALLINT       ;
--DEFINE v_saldo_aivs_portabilidad           DECIMAL(22,2)   ;
--DEFINE v_saldo_pesos_portabilidad          DECIMAL(22,2)   ;


   ON EXCEPTION SET v_error_sql   , -- cod error sql
                    v_error_isam  , -- cod error isam
                    v_msg_sql       -- descripcion error

      RETURN v_error_sql,v_error_isam,v_msg_sql,v_tot_registros,v_tot_aivs,v_tot_pesos;

   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_prt_preliquida_subs_ced.trace';
   --TRACE ON;


     LET v_ind           = 0  ;
     LET v_diag          = "" ;
     LET v_error_sql     = 0  ;
     LET v_error_isam    = 0  ;
     LET v_msg_sql       = "" ;

     LET v_tot_registros = 0  ;
     LET v_tot_aivs      = 0  ;
     LET v_tot_pesos     = 0  ;

     -- se actualiza el folio a preliquidado desde antes para facilitar el reverso
     -- en caso de error

     UPDATE glo_folio
     SET    status = 1   
     WHERE  folio  = p_folio ;

     EXECUTE FUNCTION fn_ultimo_dia_mes(today) INTO v_ultimo_dia;

     LET v_t_f_valor_transferencia = (MDY(MONTH(TODAY),v_ultimo_dia,YEAR(today))) + 1 UNITS DAY;

    -- Se identifican las cuentas a ser revisadas para verificar si existen
    -- aportaciones subsecuentes a enviar a fovissste

   FOREACH EXECUTE PROCEDURE sp_prt_consulta_subsecuentes()
           INTO v_s_id_prt_solicitud_cedente           ,
                v_id_derechohabiente                   ,
                v_s_nss                                ,
                v_s_curp                               ,
                v_s_paterno                            ,
                v_s_materno                            ,
                v_s_nombre                             ,
                v_s_id_credito_fovissste               ,
                v_s_correo_e                           ,
                v_s_telefono                           ,
                v_s_saldo_insoluto_credito_fovissste   ,
                v_s_aivs_saldo_viv97_infonavit         ,
                v_s_pesos_saldo_viv97_infonavit        ,
                v_s_aivs_saldo_viv97_afore             ,
                v_s_pesos_saldo_viv97_afore            ,
                v_s_aivs_viv97_cedido                  ,
                v_s_pesos_viv97_cedido                 ,
                v_s_f_originacion_fovissste            ,
                v_s_f_fin_credito                      ,
                v_s_f_consulta_credito                 ,
                v_s_f_vigencia                         ,
                v_s_f_ini_tramite                      ,
                v_s_tipo_portabilidad                  ,
                v_s_n_caso                             ,
                v_s_estado                             ,
                v_s_resultado_operacion                ,
                v_s_diagnostico_interno                ,
                v_s_folio_procesar                     
   
        SELECT NVL(SUM(monto_acciones),0),
               NVL(SUM(monto_pesos),0)
          INTO v_t_mto_aivs_infonavit97_cedido,
               v_t_mto_pesos_infonavit97_cedido 
          FROM TABLE(MULTISET(
        SELECT monto_acciones,
               monto_pesos
          FROM cta_movimiento 
         WHERE id_derechohabiente = v_id_derechohabiente
           AND subcuenta          = 60        -- todo llega a la subcuenta de portabilidad
           AND movimiento NOT BETWEEN 1600 AND 1699
           -- Todos los movimientos de subsecuentes que no sean por parte de portabilidad
           -- Provienen de Liquidación de pagos, pero pueden tener diferentes movimientos      
        UNION ALL
        SELECT monto_acciones,
               monto_pesos
          FROM cta_movimiento 
         WHERE id_derechohabiente = v_id_derechohabiente
           AND subcuenta          = 60        -- portabilidad
           AND movimiento = 1604 ));
           -- Los movimientos de subsecuentes portabilidad cedente (1604), matan los saldos que ya se hayan procesado

        -- al sumar todos lo movimientos debe resultar el saldo pendiente
 
      -- Si hay aportaciones de portabilidad se ingresa el registro
      IF( v_t_mto_aivs_infonavit97_cedido > 0 )THEN 
           -- Se ingresa el registro de la transferencia de aportaciones subsecuentes en la tabla historica
           
           LET v_t_id_prt_traspaso_cedente  = seq_prt_traspaso_cedente.NEXTVAL    ;
           LET v_t_id_prt_solicitud_cedente = v_s_id_prt_solicitud_cedente        ;
           LET v_t_folio_liquida            = p_folio                             ;
           LET v_t_instituto_origen         = "001"                               ;  -- infonavit
           LET v_t_tpo_movimiento           = "02"                                ;   -- cedente
           LET v_t_id_derechohabiente       = v_id_derechohabiente                ;
           LET v_t_nss                      = v_s_nss                             ;
           LET v_t_ap_paterno               = v_s_paterno                         ;
           LET v_t_ap_materno               = v_s_materno                         ;
           LET v_t_nombre                   = v_s_nombre                          ;
           LET v_t_curp                     = v_s_curp                            ;
           LET v_t_tpo_operacion            = "02";           LET v_t_id_credito_fovissste     = v_s_id_credito_fovissste            ;
           LET v_t_f_originacion_fovissste  = v_s_f_originacion_fovissste         ;
           LET v_t_estado                   = 30                                  ;
           LET v_t_usuario                  = p_usuario                           ;
           
           INSERT INTO prt_traspaso_cedente ( id_prt_traspaso_cedente             ,
                                              id_prt_solicitud_cedente            ,
                                              folio_liquida                       ,
                                              instituto_origen                    ,
                                              tpo_movimiento                      ,
                                              id_derechohabiente                  ,
                                              nss                                 ,
                                              ap_paterno                          ,
                                              ap_materno                          ,
                                              nombre                              ,
                                              curp                                ,
                                              tpo_operacion                       ,
                                              id_credito_fovissste                ,
                                              f_originacion_fovissste             ,
                                              mto_aivs_infonavit97_cedido         ,
                                              mto_pesos_infonavit97_cedido        ,
                                              f_valor_transferencia                ,
                                              estado                              ,
                                              usuario                               )     
                                     VALUES ( v_t_id_prt_traspaso_cedente         ,
                                              v_t_id_prt_solicitud_cedente        ,
                                              v_t_folio_liquida                   ,
                                              v_t_instituto_origen                ,
                                              v_t_tpo_movimiento                  ,
                                              v_t_id_derechohabiente              ,
                                              v_t_nss                             ,
                                              v_t_ap_paterno                      ,
                                              v_t_ap_materno                      ,
                                              v_t_nombre                          ,
                                              v_t_curp                            ,
                                              v_t_tpo_operacion                   ,
                                              v_t_id_credito_fovissste            ,
                                              v_t_f_originacion_fovissste         ,
                                              v_t_mto_aivs_infonavit97_cedido   ,
                                              v_t_mto_pesos_infonavit97_cedido  ,
                                              v_t_f_valor_transferencia            ,
                                              v_t_estado                          ,
                                              v_t_usuario                          ) ;
           
           -- Se provisiona el cargo por portabilidad en la subcuenta 4 se obtienen pesos al primer dia natural
           -- del mes de liquidacion (mes siguiente a la recepcion de la notificacion de procesar)
           
           
           --LET v_p_f_liquida           = v_t_f_valor_transferencia          ;
           LET v_p_f_liquida           = TODAY;
           LET v_p_id_derechohabiente  = v_t_id_derechohabiente             ;
           LET v_p_subcuenta           = 60                                 ; -- portabilidad
           LET v_p_fondo_inversion     = 10                                 ; -- pesos
           LET v_p_movimiento          = 1604                               ; -- cargo por trasp portabilidad scta 60
           LET v_p_folio_liquida       = p_folio                            ;
           LET v_p_id_referencia       = v_t_id_prt_traspaso_cedente        ;
           LET v_p_monto_acciones      = - v_t_mto_aivs_infonavit97_cedido  ;
           LET v_p_monto_pesos         = - v_t_mto_pesos_infonavit97_cedido ;
           LET v_p_f_valor             = v_t_f_valor_transferencia          ;
           LET v_p_f_registro          = TODAY                              ;
           LET v_p_h_registro          = CURRENT HOUR TO SECOND             ;
           LET v_p_origen              = "TRASP SUBS PORTABILIDAD"               ;
           
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
                                        v_p_monto_acciones      ,
                                        v_p_monto_pesos         ,
                                        v_p_f_valor             ,
                                        v_p_f_registro          ,
                                        v_p_h_registro          ,
                                        v_p_origen               );
           
           
           LET v_tot_registros = v_tot_registros + 1            ;
           LET v_tot_aivs      = v_tot_aivs      + v_p_monto_acciones   ;
           LET v_tot_pesos     = v_tot_pesos     + v_p_monto_pesos  ;
      END IF
   END FOREACH;

RETURN v_error_sql,v_error_isam,v_msg_sql,v_tot_registros,v_tot_aivs,v_tot_pesos;

END PROCEDURE;


