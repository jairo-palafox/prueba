






CREATE PROCEDURE "safreviv".sp_prt_recibe_notif_traspaso_ced(p_id_bus_solicitud_tramite DECIMAL(9,0) ,
                                                  p_folio_procesar           CHAR(050)     )

--==================================================================================
-- recibe notificacion y diagnoico de solictud de traspaso de saldo de portabilidad
-- esta funcion es llamada por bus una vez que este recibe la notificacion de 
-- procesar.
-- Funciones Utilizadas : fn_ultimo_dia_mes 
--                        fn_saldo_dia
--                        fn_glo_maq_individual
--                        sp_prt_error_bus
--==================================================================================


-- variables de tabla temporal del bus prt_bus_traspaso


DEFINE v_b_id_bus_solicitud_tramite               DECIMAL(9,0)     ;
--DEFINE v_b_folio_procesar                         CHAR(50)         ;
DEFINE v_b_bus_proceso_cod                        CHAR(3)          ;
DEFINE v_b_bus_operacion_cod                      CHAR(4)          ;
DEFINE v_b_precio_aiv_infonavit97                 DECIMAL(15,6)    ;
DEFINE v_b_precio_aiv_infonavit92                 DECIMAL(15,6)    ;
DEFINE v_b_f_precio_aiv_infonavit97               DATE             ;
DEFINE v_b_f_precio_aiv_infonavit92               DATE             ;

DEFINE v_b_mto_aivs_infonavit97                   DECIMAL(22,2)    ;
DEFINE v_b_mto_pesos_infonavit97                  DECIMAL(22,2)    ;
DEFINE v_b_mto_aivs_infonavit92                   DECIMAL(22,2)    ;
DEFINE v_b_mto_pesos_infonavit92                  DECIMAL(22,2)    ;

--DEFINE v_b_f_valor_transferencia                  DATE             ;
DEFINE v_b_diag_procesar                          CHAR(3)          ;

-- variables de solicitud registrada prt_solicitud_cedente

DEFINE v_s_aivs_saldo_viv97_afore                 DECIMAL(22,2)    ;
DEFINE v_s_pesos_saldo_viv97_afore                DECIMAL(22,2)    ;
DEFINE v_s_aivs_saldo_viv92_afore                 DECIMAL(22,2)    ;
DEFINE v_s_pesos_saldo_viv92_afore                DECIMAL(22,2)    ;


-- variables de solicitud de traspaso prt_traspaso_cedente


DEFINE v_t_id_prt_traspaso_cedente                DECIMAL(9,0)     ;
DEFINE v_t_id_prt_solicitud_cedente               DECIMAL(9,0)     ;

DEFINE v_t_folio_procesar                         CHAR(50)         ;
DEFINE v_t_id_derechohabiente                     DECIMAL(9,0)     ;
DEFINE v_t_nss                                    CHAR(11)         ; 
DEFINE v_t_sdo_insoluto_fovissste                 DECIMAL(22,2)    ;

DEFINE v_t_precio_aiv_infonavit97                 DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97               DATE             ;
DEFINE v_t_mto_aivs_infonavit97                   DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97                  DECIMAL(22,2)    ;
DEFINE v_t_precio_aiv_infonavit92                 DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit92               DATE             ;
DEFINE v_t_mto_aivs_infonavit92                   DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit92                  DECIMAL(22,2)    ;

DEFINE v_t_precio_aiv_infonavit97_afo             DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97_afo           DATE             ;
DEFINE v_t_mto_aivs_infonavit97_afo               DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_afo              DECIMAL(22,2)         ;
DEFINE v_t_precio_aiv_infonavit92_afo             DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit92_afo           DATE             ;
DEFINE v_t_mto_aivs_infonavit92_afo               DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit92_afo              DECIMAL(22,2)         ;

DEFINE v_t_precio_aiv_infonavit97_recalculado     DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97_recalculado   DATE             ;
DEFINE v_t_mto_aivs_infonavit97_recalculado       DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_recalculado      DECIMAL(22,2)         ;
DEFINE v_t_precio_aiv_infonavit92_recalculado     DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit92_recalculado   DATE             ;
DEFINE v_t_mto_aivs_infonavit92_recalculado       DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit92_recalculado      DECIMAL(22,2)         ;

DEFINE v_t_precio_aiv_infonavit97_cedido          DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit97_cedido        DATE             ;
DEFINE v_t_mto_aivs_infonavit97_cedido            DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit97_cedido           DECIMAL(22,2)         ;
DEFINE v_t_precio_aiv_infonavit92_cedido          DECIMAL(16,6)    ;
DEFINE v_t_f_precio_aiv_infonavit92_cedido        DATE             ;
DEFINE v_t_mto_aivs_infonavit92_cedido            DECIMAL(22,2)    ;
DEFINE v_t_mto_pesos_infonavit92_cedido           DECIMAL(22,2)         ;

DEFINE v_t_mto_aivs_infonavit97_diferencia        DECIMAL(22,2)     ;
DEFINE v_t_mto_pesos_infonavit97_diferencia       DECIMAL(22,2)     ;
DEFINE v_t_mto_aivs_infonavit92_diferencia        DECIMAL(22,2)     ;
DEFINE v_t_mto_pesos_infonavit92_diferencia       DECIMAL(22,2)     ;

DEFINE v_t_ind_origen_sdo_traspaso                SMALLINT         ;
DEFINE v_t_f_valor_transferncia                   DATE             ;
DEFINE v_t_diag_procesar                          CHAR(3)          ;
DEFINE v_t_estado                                 SMALLINT         ;
DEFINE v_t_usuario                                CHAR(20)         ;

-- varibales de control de errores

DEFINE v_error_sql                                INTEGER          ;
DEFINE v_error_isam                               INTEGER          ;
DEFINE v_msg_sql                                  CHAR(255)        ;
DEFINE v_origen                                   CHAR(255)        ;
DEFINE v_ind                                      CHAR(005)        ;
DEFINE v_diag                                     CHAR(255)        ;
DEFINE v_proceso_cod                              CHAR(003)        ;
DEFINE v_operacion_cod                            CHAR(004)        ;
DEFINE v_folio_cliente                            CHAR(050)        ;
DEFINE v_f_error                                  DATETIME YEAR TO SECOND;
DEFINE v_estado_destino                           SMALLINT         ;

-- varibales de trabajo 

DEFINE v_f_valor                                  DATE             ;
DEFINE v_ultimo_dia                               SMALLINT         ;
DEFINE v_precio_fondo                             DECIMAL(19,14)   ;



   ON EXCEPTION SET v_error_sql   , -- cod error sql
                    v_error_isam  , -- cod error isam
                    v_msg_sql       -- descripcion error

       LET  v_f_error = CURRENT;

       EXECUTE PROCEDURE sp_prt_error_bus(p_id_bus_solicitud_tramite   ,
                             v_b_bus_proceso_cod     ,
                             v_b_bus_operacion_cod   ,
                             "safreviv"              ,
                             v_f_error               ,
                             v_error_sql             ,
                             v_error_isam            ,
                             v_msg_sql               ,
                             v_origen                , -- funcion de donde proviene el error
                             v_ind                   ,
                             v_diag                   );

   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_recibe_notif_traspaso_ced.trace';
   --TRACE ON;

   -- inicializacion de variables de error

   LET v_f_error    = CURRENT  ;
   LET v_error_sql  = 0        ;
   LET v_error_isam = 0        ; 
   LET v_msg_sql    = ""       ;
   LET v_origen     = "sp_prt_recibe_notif_traspaso_ced";
   LET v_ind        = 0        ; 
   LET v_diag       = ""       ;


   -- se rescata la notificacion del bus
   SELECT a.id_bus_solicitud_tramite        ,
          a.bus_proceso_cod                 ,
          a.bus_operacion_cod               ,
          a.precio_aiv_infonavit97          ,
          a.f_precio_aiv_infonavit97[5,6]||'/'||f_precio_aiv_infonavit97[7,8]||'/'||f_precio_aiv_infonavit97[1,4],
          a.mto_aivs_infonavit97            ,
          a.mto_pesos_infonavit97           ,
          a.precio_aiv_infonavit92          ,
          a.f_precio_aiv_infonavit92[5,6]||'/'||f_precio_aiv_infonavit92[7,8]||'/'||f_precio_aiv_infonavit92[1,4],
          a.mto_aivs_infonavit92            ,
          a.mto_pesos_infonavit92           ,
          a.diag_procesar                   
   INTO   v_b_id_bus_solicitud_tramite      ,
          v_b_bus_proceso_cod               ,
          v_b_bus_operacion_cod             ,
          v_b_precio_aiv_infonavit97        ,
          v_b_f_precio_aiv_infonavit97      ,
          v_b_mto_aivs_infonavit97          ,
          v_b_mto_pesos_infonavit97         ,
          v_b_precio_aiv_infonavit92        ,
          v_b_f_precio_aiv_infonavit92      ,
          v_b_mto_aivs_infonavit92          ,
          v_b_mto_pesos_infonavit92         ,
          v_b_diag_procesar                 
   FROM   prt_bus_traspaso a
   WHERE  a.id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;

   -- se rescata la solicitud del traspaso relacionada a la notificacion
   SELECT id_prt_traspaso_cedente           ,          
          id_prt_solicitud_cedente          ,  
          folio_procesar                    ,
          id_derechohabiente                ,
          nss                               ,
          precio_aiv_infonavit97            , 
          f_precio_aiv_infonavit97          ,
          mto_aivs_infonavit97              ,
          mto_pesos_infonavit97             ,
          mto_aivs_infonavit92              ,
          mto_pesos_infonavit92             ,
          sdo_insoluto_fovissste            ,
          ind_origen_sdo_traspaso           ,
          f_valor_transferencia              ,
          diag_procesar                     ,
          estado                            ,
          usuario                         
     INTO v_t_id_prt_traspaso_cedente       , 
          v_t_id_prt_solicitud_cedente      , 
          v_t_folio_procesar                , 
          v_t_id_derechohabiente            ,
          v_t_nss                           ,
          v_t_precio_aiv_infonavit97        , 
          v_t_f_precio_aiv_infonavit97      , 
          v_t_mto_aivs_infonavit97          , 
          v_t_mto_pesos_infonavit97         , 
          v_t_mto_aivs_infonavit92          , 
          v_t_mto_pesos_infonavit92         , 
          v_t_sdo_insoluto_fovissste        ,
          v_t_ind_origen_sdo_traspaso       , 
          v_t_f_valor_transferncia          , 
          v_t_diag_procesar                 , 
          v_t_estado                        , 
          v_t_usuario                                  
     FROM prt_traspaso_cedente 
    WHERE folio_procesar = p_folio_procesar 
    AND   estado = 10;


   LET v_s_aivs_saldo_viv97_afore  = v_b_mto_aivs_infonavit97   ;
   LET v_s_pesos_saldo_viv97_afore = v_b_mto_pesos_infonavit97  ;
   LET v_s_aivs_saldo_viv92_afore  = v_b_mto_aivs_infonavit92   ;
   LET v_s_pesos_saldo_viv92_afore = v_b_mto_pesos_infonavit92  ;


   -- se elige accion en caso de ser acetpado o rechazado

   IF( v_b_diag_procesar = "100" OR v_b_diag_procesar = "101" )THEN --  diagnóstico aceptado

      -- sacamos el saldo actual de viv97 de la cuenta en infonavit

      EXECUTE FUNCTION fn_ultimo_dia_mes(TODAY) INTO v_ultimo_dia;

      LET v_f_valor = (MDY(MONTH(TODAY),v_ultimo_dia,YEAR(TODAY))) + 1 UNITS DAY;

      LET v_t_f_precio_aiv_infonavit97_recalculado = v_f_valor;
      LET v_t_f_precio_aiv_infonavit92_recalculado = v_f_valor;
      
      -- se obtiene el saldo de viv97 actualizado a la fecha de recepcion de la 
      -- notificacion de transferencia de viv97 por  portabilidad

      SELECT FIRST 1 precio_fondo
        INTO v_t_precio_aiv_infonavit97_recalculado
        FROM glo_valor_fondo
       WHERE f_valuacion = v_t_f_precio_aiv_infonavit97_recalculado
         AND fondo       = 11;
     
      EXECUTE FUNCTION fn_saldo_dia(v_t_nss,v_t_id_derechohabiente,4,v_t_f_precio_aiv_infonavit97_recalculado)
                  INTO v_ind               ,
                       v_t_mto_aivs_infonavit97_recalculado  ,
                       v_t_mto_pesos_infonavit97_recalculado ;

      LET v_t_mto_pesos_infonavit97_recalculado = v_t_mto_aivs_infonavit97_recalculado * v_t_precio_aiv_infonavit97_recalculado;

      SELECT FIRST 1 precio_fondo
        INTO v_t_precio_aiv_infonavit92_recalculado
        FROM glo_valor_fondo
       WHERE f_valuacion = v_t_f_precio_aiv_infonavit92_recalculado
         AND fondo       = 11;
     
      EXECUTE FUNCTION fn_saldo_dia(v_t_nss,v_t_id_derechohabiente,8,v_t_f_precio_aiv_infonavit92_recalculado)
                  INTO v_ind               ,
                       v_t_mto_aivs_infonavit92_recalculado  ,
                       v_t_mto_pesos_infonavit92_recalculado ;

      LET v_t_mto_pesos_infonavit92_recalculado = v_t_mto_aivs_infonavit92_recalculado * v_t_precio_aiv_infonavit92_recalculado;


      -- regreso con error en el saldo

      IF v_ind <> 0 THEN
   
   
      END IF

      -- comparamos los saldos infonavit y afore para registrar diferencia de saldos a favor o en contra

      --LET v_t_mto_aivs_infonavit97_diferencia = v_t_mto_aivs_infonavit97_recalculado - v_t_mto_aivs_infonavit97_afo;

      LET v_t_mto_aivs_infonavit97_diferencia = v_t_mto_aivs_infonavit97_recalculado - v_s_aivs_saldo_viv97_afore;
      LET v_t_mto_pesos_infonavit97_diferencia = v_t_mto_pesos_infonavit97_recalculado - v_s_pesos_saldo_viv97_afore;

      LET v_t_mto_aivs_infonavit92_diferencia = v_t_mto_aivs_infonavit92_recalculado - v_s_aivs_saldo_viv92_afore;
      LET v_t_mto_pesos_infonavit92_diferencia = v_t_mto_pesos_infonavit92_recalculado - v_s_pesos_saldo_viv92_afore;
     

 
      IF  v_t_mto_aivs_infonavit97_diferencia < 0 THEN  
          -- si el infonavit tiene menos se envia lo que tenga infonavit
          LET v_t_precio_aiv_infonavit97_cedido = v_t_precio_aiv_infonavit97_recalculado;
          LET v_t_f_precio_aiv_infonavit97_cedido = v_t_f_precio_aiv_infonavit97_recalculado;
          LET v_t_mto_aivs_infonavit97_cedido  = v_t_mto_aivs_infonavit97_recalculado;  
          LET v_t_mto_pesos_infonavit97_cedido = v_t_mto_pesos_infonavit97_recalculado;  
      ELSE
          
          -- si el infonavit tiene mas saldo o igual saldo se envia lo que reporta la afore 
          --LET v_t_precio_aiv_infonavit97_cedido = v_t_precio_aiv_infonavit97_afo;
          --LET v_t_f_precio_aiv_infonavit97_cedido = v_t_f_precio_aiv_infonavit97_afo;
          --LET v_t_mto_aivs_infonavit97_cedido  = v_t_mto_aivs_infonavit97_afo; 
          --LET v_t_mto_pesos_infonavit97_cedido = v_t_mto_pesos_infonavit97_afo; 
          
          -- Los datos de prt_bus_traspaso son considerados como los que están en la afore
          IF( v_t_mto_pesos_infonavit97_recalculado > v_t_sdo_insoluto_fovissste )THEN
             -- si Infonavit tiene más saldo, envía como tope el crédito             
             LET v_t_precio_aiv_infonavit97_cedido = v_b_precio_aiv_infonavit97;
             LET v_t_f_precio_aiv_infonavit97_cedido = v_b_f_precio_aiv_infonavit97;
             --LET v_t_mto_aivs_infonavit97_cedido  = v_t_sdo_insoluto_fovissste; 
             -- Ree calcula aivs para los aivs del crédito
             EXECUTE FUNCTION fn_consulta_precio_fondo(v_t_sdo_insoluto_fovissste,v_b_f_precio_aiv_infonavit97,11) 
               INTO v_t_mto_aivs_infonavit97_cedido;
             LET v_t_mto_pesos_infonavit97_cedido = v_t_sdo_insoluto_fovissste; 
             
          ELSE
             -- Si el crédito fovissste es mayor, envia los saldos de infonavit
             LET v_t_precio_aiv_infonavit97_cedido = v_b_precio_aiv_infonavit97;
             LET v_t_f_precio_aiv_infonavit97_cedido = v_b_f_precio_aiv_infonavit97;
             LET v_t_mto_aivs_infonavit97_cedido  = v_b_mto_aivs_infonavit97; 
             LET v_t_mto_pesos_infonavit97_cedido = v_b_mto_pesos_infonavit97; 
          END IF
      END IF

----
----
             LET v_t_precio_aiv_infonavit97_cedido   = v_b_precio_aiv_infonavit97;
             LET v_t_f_precio_aiv_infonavit97_cedido = v_b_f_precio_aiv_infonavit97;
             LET v_t_mto_aivs_infonavit97_cedido     = v_b_mto_aivs_infonavit97; 
             LET v_t_mto_pesos_infonavit97_cedido    = v_b_mto_pesos_infonavit97; 
----
----

      IF  v_t_mto_aivs_infonavit92_diferencia < 0 THEN  
          -- si el infonavit tiene menos se envia lo que tenga infonavit
          LET v_t_precio_aiv_infonavit92_cedido = v_t_precio_aiv_infonavit92_recalculado;
          LET v_t_f_precio_aiv_infonavit92_cedido = v_t_f_precio_aiv_infonavit92_recalculado;
          LET v_t_mto_aivs_infonavit92_cedido  = v_t_mto_aivs_infonavit92_recalculado;  
          LET v_t_mto_pesos_infonavit92_cedido = v_t_mto_pesos_infonavit92_recalculado;  
      ELSE
          
          --LET v_t_precio_aiv_infonavit97_cedido = v_t_precio_aiv_infonavit97_afo;
          --LET v_t_f_precio_aiv_infonavit97_cedido = v_t_f_precio_aiv_infonavit97_afo;
          --LET v_t_mto_aivs_infonavit97_cedido  = v_t_mto_aivs_infonavit97_afo; 
          --LET v_t_mto_pesos_infonavit97_cedido = v_t_mto_pesos_infonavit97_afo; 
          
          -- Los datos de prt_bus_traspaso son considerados como los que están en la afore
          IF( v_t_mto_pesos_infonavit92_recalculado > v_t_sdo_insoluto_fovissste )THEN
             -- si Infonavit tiene más saldo, envía como tope el crédito             
             LET v_t_precio_aiv_infonavit92_cedido = v_b_precio_aiv_infonavit92;
             LET v_t_f_precio_aiv_infonavit92_cedido = v_b_f_precio_aiv_infonavit92;
             -- Ree calcula aivs para los aivs del crédito
             EXECUTE FUNCTION fn_consulta_precio_fondo(v_t_sdo_insoluto_fovissste,v_b_f_precio_aiv_infonavit92,11) 
               INTO v_t_mto_aivs_infonavit92_cedido;
             LET v_t_mto_pesos_infonavit92_cedido = v_t_sdo_insoluto_fovissste; 
             
          ELSE
             -- Si el crédito fovissste es mayor, envia los saldos de infonavit
             LET v_t_precio_aiv_infonavit92_cedido = v_b_precio_aiv_infonavit92;
             LET v_t_f_precio_aiv_infonavit92_cedido = v_b_f_precio_aiv_infonavit92;
             LET v_t_mto_aivs_infonavit92_cedido  = v_b_mto_aivs_infonavit92; 
             LET v_t_mto_pesos_infonavit92_cedido = v_b_mto_pesos_infonavit92; 
          END IF
      END IF
---
---
             LET v_t_precio_aiv_infonavit92_cedido   = v_b_precio_aiv_infonavit92;
             LET v_t_f_precio_aiv_infonavit92_cedido = v_b_f_precio_aiv_infonavit92;
             LET v_t_mto_aivs_infonavit92_cedido     = v_b_mto_aivs_infonavit92; 
             LET v_t_mto_pesos_infonavit92_cedido    = v_b_mto_pesos_infonavit92; 
---
---

      -- se actualiza la solicitud de traspaso de acuerdo a lo recibido en la
      -- notificacion recibida en el bus

      UPDATE prt_solicitud_cedente
         SET aivs_saldo_viv97_afore               = v_s_aivs_saldo_viv97_afore                ,
             pesos_saldo_viv97_afore              = v_s_pesos_saldo_viv97_afore               ,
             aivs_viv97_cedido                    = v_t_mto_aivs_infonavit97_cedido           , 
             pesos_viv97_cedido                   = v_t_mto_pesos_infonavit97_cedido          , 
             aivs_saldo_viv92_afore               = v_s_aivs_saldo_viv92_afore                ,
             pesos_saldo_viv92_afore              = v_s_pesos_saldo_viv92_afore               ,
             aivs_viv92_cedido                    = v_t_mto_aivs_infonavit92_cedido           , 
             pesos_viv92_cedido                   = v_t_mto_pesos_infonavit92_cedido          
       WHERE id_prt_solicitud_cedente             = v_t_id_prt_solicitud_cedente ;
     
      -- se actualiza en la solicitud el saldo recibido de la afore en la 
      -- notificacion de la transferencia
     
      UPDATE prt_traspaso_cedente
         SET diag_procesar                         = v_b_diag_procesar                        ,
             precio_aiv_infonavit97_afo            = v_b_precio_aiv_infonavit97               ,
             f_precio_aiv_infonavit97_afo          = v_b_f_precio_aiv_infonavit97             ,
             mto_aivs_infonavit97_afo              = v_b_mto_aivs_infonavit97                 ,
             mto_pesos_infonavit97_afo             = v_b_mto_pesos_infonavit97                ,
             precio_aiv_infonavit97_recalculado    = v_t_precio_aiv_infonavit97_recalculado   ,
             f_precio_aiv_infonavit97_recalculado  = v_t_f_precio_aiv_infonavit97_recalculado ,
             mto_aivs_infonavit97_recalculado      = v_t_mto_aivs_infonavit97_recalculado     ,
             mto_pesos_infonavit97_recalculado     = v_t_mto_pesos_infonavit97_recalculado    ,
             precio_aiv_infonavit97_cedido         = v_t_precio_aiv_infonavit97_cedido        ,
             f_precio_aiv_infonavit97_cedido       = v_t_f_precio_aiv_infonavit97_cedido      ,
             mto_aivs_infonavit97_cedido           = v_t_mto_aivs_infonavit97_cedido          ,
             mto_pesos_infonavit97_cedido          = v_t_mto_pesos_infonavit97_cedido         ,
             mto_aivs_infonavit97_diferencia       = v_t_mto_aivs_infonavit97_diferencia      ,
             mto_pesos_infonavit97_diferencia      = v_t_mto_pesos_infonavit97_diferencia     ,
             precio_aiv_infonavit92_afo            = v_b_precio_aiv_infonavit92               ,
             f_precio_aiv_infonavit92_afo          = v_b_f_precio_aiv_infonavit92             ,
             mto_aivs_infonavit92_afo              = v_b_mto_aivs_infonavit92                 ,
             mto_pesos_infonavit92_afo             = v_b_mto_pesos_infonavit92                ,
             precio_aiv_infonavit92_recalculado    = v_t_precio_aiv_infonavit92_recalculado   ,
             f_precio_aiv_infonavit92_recalculado  = v_t_f_precio_aiv_infonavit92_recalculado ,
             mto_aivs_infonavit92_recalculado      = v_t_mto_aivs_infonavit92_recalculado     ,
             mto_pesos_infonavit92_recalculado     = v_t_mto_pesos_infonavit92_recalculado    ,
             precio_aiv_infonavit92_cedido         = v_t_precio_aiv_infonavit92_cedido        ,
             f_precio_aiv_infonavit92_cedido       = v_t_f_precio_aiv_infonavit92_cedido      ,
             mto_aivs_infonavit92_cedido           = v_t_mto_aivs_infonavit92_cedido          ,
             mto_pesos_infonavit92_cedido          = v_t_mto_pesos_infonavit92_cedido         ,
             mto_aivs_infonavit92_diferencia       = v_t_mto_aivs_infonavit92_diferencia      ,
             mto_pesos_infonavit92_diferencia      = v_t_mto_pesos_infonavit92_diferencia     ,
             estado                                = 20 -- traspaso notificado aceptado
       WHERE folio_procesar                        = p_folio_procesar ;

       -- se avanza maquinaria de estado de la soliciud de traspaso cedente por portbilidad 

       EXECUTE FUNCTION fn_glo_maq_individual(2                             , -- maq portabilidad
                                              v_t_id_prt_solicitud_cedente  , -- id de la solicitud
                                              50                            , -- recibir diagnostico procesar aceptado
                                              v_t_usuario )                 -- usuario
                  INTO v_ind            ,
                       v_diag           ,
                       v_error_sql      ,
                       v_error_isam     ,
                       v_msg_sql        ,
                       v_estado_destino  ;


   ELSE  -- diagnostico procesar rechazado

      UPDATE prt_solicitud_cedente
         SET aivs_saldo_viv97_afore                = v_s_aivs_saldo_viv97_afore               ,
             pesos_saldo_viv97_afore               = v_s_pesos_saldo_viv97_afore              ,
             aivs_saldo_viv92_afore                = v_s_aivs_saldo_viv92_afore               ,
             pesos_saldo_viv92_afore               = v_s_pesos_saldo_viv92_afore              
       WHERE id_prt_solicitud_cedente              = v_t_id_prt_solicitud_cedente             ;

      UPDATE prt_traspaso_cedente
         SET diag_procesar                         = v_b_diag_procesar                        ,
             precio_aiv_infonavit97_afo            = v_b_precio_aiv_infonavit97               ,
             f_precio_aiv_infonavit97_afo          = v_b_f_precio_aiv_infonavit97             ,
             mto_aivs_infonavit97_afo              = v_b_mto_aivs_infonavit97                 ,
             mto_pesos_infonavit97_afo             = v_b_mto_pesos_infonavit97                ,
             precio_aiv_infonavit92_afo            = v_b_precio_aiv_infonavit92               ,
             f_precio_aiv_infonavit92_afo          = v_b_f_precio_aiv_infonavit92             ,
             mto_aivs_infonavit92_afo              = v_b_mto_aivs_infonavit92                 ,
             mto_pesos_infonavit92_afo             = v_b_mto_pesos_infonavit92                ,
             --precio_aiv_infonavit97_recalculado    = v_t_precio_aiv_infonavit97_recalculado   ,
             --f_precio_aiv_infonavit97_recalculado  = v_t_f_precio_aiv_infonavit97_recalculado ,
             --mto_aivs_infonavit97_recalculado      = v_t_mto_aivs_infonavit97_recalculado     ,
             --mto_pesos_infonavit97_recalculado     = v_t_mto_pesos_infonavit97_recalculado    
             estado                                = 5 -- traspaso rechazado
       WHERE folio_procesar                        = p_folio_procesar                          ;

       -- se avanza maquinaria de estado de la soliciud de traspaso cedente por portbilidad 
       EXECUTE FUNCTION fn_glo_maq_individual(2                             , -- maq portabilidad
                                              v_t_id_prt_solicitud_cedente  , -- id de la solicitud
                                              45                            , -- recibir diagnostico procesar rechazado
                                              v_t_usuario )                 -- usuario
                  INTO v_ind            ,
                       v_diag           ,
                       v_error_sql      ,
                       v_error_isam     ,
                       v_msg_sql        ,
                       v_estado_destino  ;
   END IF
    
   -- Elimina el registro temporal del bus para el traspaso 
     DELETE 
     FROM prt_bus_traspaso
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;

END PROCEDURE;


