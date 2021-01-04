
DBSCHEMA Schema Utility       INFORMIX-SQL Version 12.10.FC14AEE







CREATE PROCEDURE "safreviv".sp_prt_solicita_traspaso_cedente_reenv(p_id_prt_sol_ced VARCHAR(10),
                                                  p_proceso_cod    CHAR(3),
                                                  p_operacion_cod  CHAR(4),
                                                  p_usuario_cod    CHAR(20),
                                                  p_id_prt_traspaso_ced_orig dec(10,0))
RETURNING SMALLINT,
          CHAR(3),
          INTEGER,
          INTEGER,
          CHAR(254),
          SMALLINT;
          
--=====================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:01/04/2015
-- funciones utilizadas :  fn_convierte_mensaje    -- bus
--                         sp_prt_error            -- rutina error
--                         fn_glo_maq_individual   -- maquinaria                      
-- Descripcion : Este procedimiento gestiona la solicitud de traspaso
--               por portabilidad como instituto cedente, invoca el 
--               clinente iniciador para solicitud de transferencia por 
--               portabilidad y avanza la maquinara de estado para  
--               dejarla como transferencia solicitada. Para efectos de 
--               revision de errores verificar la tabla prt_error_bus 
--               con la fecha y el id de la solicitud de transferencia.
--======================================================================

-- Datos del BUS

DEFINE v_sequencia_bus DECIMAL(9,0);
DEFINE v_campo         CHAR(40);
DEFINE v_cad_error     LVARCHAR(1000);

-- variables solicitud de marca cedente

DEFINE v_s_id_prt_solicitud_cedente             DECIMAL(9,0)  ;
DEFINE v_s_nss                                  CHAR(11)      ;
DEFINE v_s_curp                                 CHAR(18)      ;
DEFINE v_s_paterno                              CHAR(40)      ;
DEFINE v_s_materno                              CHAR(40)      ;
DEFINE v_s_nombre                               CHAR(40)      ;
DEFINE v_s_id_credito_fovissste                 DECIMAL(10,0)  ;
DEFINE v_s_correo_e                             CHAR(100)     ;
DEFINE v_s_telefono                             CHAR(100)     ;
DEFINE v_s_saldo_insoluto_credito_fovissste     DECIMAL(22,2) ;
DEFINE v_s_aivs_saldo_viv97_infonavit           DECIMAL(22,2) ;
DEFINE v_s_pesos_saldo_viv97_infonavit          DECIMAL(22,2) ;
DEFINE v_s_aivs_saldo_viv97_afore               DECIMAL(22,2) ;
DEFINE v_s_pesos_saldo_viv97_afore              DECIMAL(22,2) ;
DEFINE v_s_aivs_viv97_cedido                    DECIMAL(22,2) ;
DEFINE v_s_pesos_viv97_cedido                   DECIMAL(22,2) ;
DEFINE v_s_f_originacion_fovissste              DATE          ;
DEFINE v_s_f_fin_credito                        DATE      ;
DEFINE v_s_f_consulta_credito                   DATE          ;
DEFINE v_s_f_vigencia                           DATE          ;
DEFINE v_s_f_ini_tramite                        DATE          ;
DEFINE v_s_tipo_portabilidad                    SMALLINT      ;
DEFINE v_s_n_caso                               VARCHAR(20)   ;
DEFINE v_s_estado                               SMALLINT      ;
DEFINE v_s_resultado_operacion                  CHAR(2)       ;
DEFINE v_s_diagnostico_interno                  CHAR(5)       ;
DEFINE v_s_folio_procesar                       CHAR(50)      ;

-- Variables de solicitud de Traspaso Cedente

DEFINE v_t_id_prt_traspaso_cedente              DECIMAL(9,0)  ;
DEFINE v_t_id_prt_solicitud_cedente             DECIMAL(9,0)  ;
DEFINE v_t_folio_liquida                        DECIMAL(10)   ;
DEFINE v_t_folio_procesar                       CHAR(50)      ;
DEFINE v_t_instituto_origen                     CHAR(3)       ;
DEFINE v_t_tpo_movimiento                       CHAR(2)       ;
DEFINE v_t_id_derechohabiente                   DECIMAL(9,0)  ;
DEFINE v_t_nss                                  CHAR(11)      ;
DEFINE v_t_ap_paterno                           CHAR(40)       ;
DEFINE v_t_ap_materno                           CHAR(40)       ;
DEFINE v_t_nombre                               CHAR(40)       ;
DEFINE v_t_curp                                 CHAR(18)      ;
DEFINE v_t_tpo_operacion                        CHAR(2)       ;
DEFINE v_t_id_credito_fovissste                 DECIMAL(10,0) ;
DEFINE v_t_sdo_insoluto_fovissste               DECIMAL(22,2) ;
DEFINE v_t_f_originacion_fovissste              DATE          ;
DEFINE v_t_f_originacion_fovissste_bus          CHAR(8);
DEFINE v_t_precio_aiv_infonavit97               DECIMAL(16,6) ;
DEFINE v_t_f_precio_aiv_infonavit97             DATE          ;
DEFINE v_t_f_precio_aiv_infonavit97_bus         CHAR(8);
DEFINE v_t_mto_aivs_infonavit97                 DECIMAL(22,2) ;
DEFINE v_t_mto_pesos_infonavit97                DECIMAL(22,2) ;
DEFINE v_t_ind_origen_sdo_traspaso              SMALLINT      ;
DEFINE v_t_precio_aiv_infonavit97_afo           DECIMAL(16,6) ;
DEFINE v_t_f_precio_aiv_infonavit97_afo         DATE          ;
DEFINE v_t_mto_aivs_infonavit97_afo             DECIMAL(22,2) ;
DEFINE v_t_mto_pesos_infonavit97_afo            CHAR(18)      ;
DEFINE v_t_f_valor_transferencia                DATE          ;
DEFINE v_t_f_valor_transferencia_bus            CHAR(8)       ;
DEFINE v_t_diag_procesar                        CHAR(3)       ;
DEFINE v_t_estado                               SMALLINT      ;
DEFINE v_t_usuario                              CHAR(20)      ;


-- variables de trabajo 
DEFINE v_ultimo_dia                             SMALLINT      ;
DEFINE v_precio_fondo                           DECIMAL(19,14) ;
DEFINE v_id_derechohabiente                     DECIMAL(9,0)  ;
DEFINE v_saldo_aivs_viv97                       DECIMAL(22,2) ;
DEFINE v_saldo_pesos_viv97                      DECIMAL(22,2) ;
DEFINE v_folio_procesar                         CHAR(50)      ;

-- variables  de control de errores

DEFINE v_error_sql                              INTEGER       ;
DEFINE v_error_isam                             INTEGER       ;
DEFINE v_msg_sql                                CHAR(254)     ;
DEFINE v_ind                                    SMALLINT      ;
DEFINE v_diag                                   CHAR(3)       ;
DEFINE v_origen                                 CHAR(255)     ;
DEFINE v_hora_error                             DATETIME HOUR TO SECOND;

-- Variables para avance de manquinaria
DEFINE v_estado           SMALLINT;
DEFINE v_id_maquinaria    SMALLINT;
DEFINE v_senial_aceptada  SMALLINT;
DEFINE v_senial_rechazada SMALLINT;
DEFINE v_usuario          CHAR(20);

DEFINE v_comando       LVARCHAR(1000);

-- Variables de estatus de avance de maquinaria

DEFINE v_estado_destino SMALLINT;


   ON EXCEPTION SET v_error_sql   , -- cod error sql
                    v_error_isam  , -- cod error isam
                    v_msg_sql       -- descripcion error
  

             CALL sp_prt_error_bus(p_id_prt_sol_ced    ,
                                   p_proceso_cod       ,
                                   p_operacion_cod     , 
                                   p_usuario_cod       ,  
                                   TODAY               ,
                                   v_error_sql         ,
                                   v_error_isam        ,
                                   v_msg_sql           ,
                                   v_origen            , -- funcion de donde proviene el error
                                   v_ind               ,
                                   v_diag               );
      RETURN v_ind            ,
             v_diag           ,
             v_error_sql      ,
             v_error_isam     ,
             v_msg_sql        ,
             v_estado_destino  ;

   END EXCEPTION;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_solicita_traspaso_cedente.trace';
   --SET DEBUG FILE TO '/tmp/sp_prt_solicita_traspaso_cedente.trace';
   --TRACE ON;


   -- asignacion inicial de las varibales de control de errores

   LET v_error_sql  = 0                                   ;
   LET v_error_isam = 0                                   ;
   LET v_msg_sql    = " "                                 ;
   LET v_origen     = "fn_prt_solicita_traspaso_cedente"  ;
   LET v_ind        = ""                                  ;
   LET v_diag       = ""                                  ;
   LET v_hora_error = CURRENT                             ;


   -- Recupera solicitud de portabilidad cedente
   -- p_id_prt_sol_ced = folio_cliente para efectos del bus


SELECT id_prt_solicitud_cedente              ,
       nss                                   ,
       curp                                  ,
       paterno                               ,
       materno                               ,
       nombre                                ,
       id_credito_fovissste                  ,
       correo_e                              ,
       telefono                              ,
       saldo_insoluto_credito_fovissste      ,
       aivs_saldo_viv97_infonavit            ,
       pesos_saldo_viv97_infonavit           ,
       aivs_saldo_viv97_afore                ,
       pesos_saldo_viv97_afore               ,
       aivs_viv97_cedido                     ,
       pesos_viv97_cedido                    ,
       f_originacion_fovissste               ,
       f_fin_credito                         ,
       f_consulta_credito                    ,
       f_vigencia                            ,
       f_ini_tramite                         ,
       tipo_portabilidad                     ,
       n_caso                                ,
       estado                                ,
       resultado_operacion                   ,
       diagnostico_interno                   ,
       folio_procesar                    
  INTO v_s_id_prt_solicitud_cedente          ,
       v_s_nss                               ,
       v_s_curp                              ,
       v_s_paterno                           ,
       v_s_materno                           ,
       v_s_nombre                            ,
       v_s_id_credito_fovissste              ,
       v_s_correo_e                          ,
       v_s_telefono                          ,
       v_s_saldo_insoluto_credito_fovissste  ,
       v_s_aivs_saldo_viv97_infonavit        ,
       v_s_pesos_saldo_viv97_infonavit       ,
       v_s_aivs_saldo_viv97_afore            ,
       v_s_pesos_saldo_viv97_afore           ,
       v_s_aivs_viv97_cedido                 ,
       v_s_pesos_viv97_cedido                ,
       v_s_f_originacion_fovissste           ,
       v_s_f_fin_credito                     ,
       v_s_f_consulta_credito                ,
       v_s_f_vigencia                        ,
       v_s_f_ini_tramite                     ,
       v_s_tipo_portabilidad                 ,
       v_s_n_caso                            ,
       v_s_estado                            ,
       v_s_resultado_operacion               ,
       v_s_diagnostico_interno               ,
       v_s_folio_procesar                    
  FROM prt_solicitud_cedente
 WHERE id_prt_solicitud_cedente = p_id_prt_sol_ced;

 EXECUTE FUNCTION fn_ultimo_dia_mes(today) INTO v_ultimo_dia;

 LET v_t_f_valor_transferencia = (MDY(MONTH(TODAY),v_ultimo_dia,YEAR(today))) + 1 UNITS DAY;


-- se obtiene el saldo de viv97 al primer dia natural del mes siguiente

   SELECT FIRST 1 precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_t_f_valor_transferencia
      AND fondo       = 11;
      
   SELECT FIRST 1 id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = v_s_nss;

 EXECUTE FUNCTION fn_saldo_dia(v_s_nss,v_id_derechohabiente,4,v_t_f_valor_transferencia)
    INTO v_ind               ,
         v_saldo_aivs_viv97  ,
         v_saldo_pesos_viv97;

 LET v_saldo_pesos_viv97 = v_saldo_aivs_viv97 * v_precio_fondo;
 -- regreso con error en el saldo
 IF v_ind <> 0 THEN 


 END IF

 -- se envia como maximo el saldo de Infonavit para no quebrantar la cuenta si el crédito de fovissste es mayor
 IF v_saldo_pesos_viv97 >  v_s_saldo_insoluto_credito_fovissste THEN 
    LET v_s_pesos_saldo_viv97_infonavit = v_s_saldo_insoluto_credito_fovissste; 
    -- Reecalcula las avis según los pesos del crédito a la fecha de transferencia
    EXECUTE FUNCTION fn_consulta_precio_fondo(v_s_pesos_saldo_viv97_infonavit,v_t_f_valor_transferencia,10) 
       INTO v_s_aivs_saldo_viv97_infonavit; 
    LET v_t_tpo_operacion               = "02";
 ELSE
    LET v_s_pesos_saldo_viv97_infonavit = v_saldo_pesos_viv97;
    LET v_s_aivs_saldo_viv97_infonavit  = v_saldo_aivs_viv97;
    LET v_t_tpo_operacion               = "01";
 END IF


LET v_t_id_prt_traspaso_cedente    = seq_prt_traspaso_cedente.NEXTVAL ;
LET v_t_id_prt_solicitud_cedente   = v_s_id_prt_solicitud_cedente        ;
LET v_t_instituto_origen           = "001"                            ;  -- infonavit
LET v_t_tpo_movimiento             = "02"                             ;  -- cedente
LET v_t_id_derechohabiente         = v_id_derechohabiente             ;
LET v_t_nss                        = v_s_nss                          ;
LET v_t_ap_paterno                 = v_s_paterno                      ;
LET v_t_ap_materno                 = v_s_materno                      ;
LET v_t_nombre                     = v_s_nombre                       ;
LET v_t_curp                       = v_s_curp                         ;
LET v_t_id_credito_fovissste       = v_s_id_credito_fovissste         ;
LET v_t_sdo_insoluto_fovissste     = v_s_saldo_insoluto_credito_fovissste     ;
LET v_t_f_originacion_fovissste    = v_s_f_originacion_fovissste      ; 
LET v_t_precio_aiv_infonavit97     = v_precio_fondo                   ;
LET v_t_f_precio_aiv_infonavit97   = v_t_f_valor_transferencia        ;
LET v_t_mto_aivs_infonavit97       = v_s_aivs_saldo_viv97_infonavit   ;
LET v_t_mto_pesos_infonavit97      = v_s_pesos_saldo_viv97_infonavit  ;
LET v_t_estado                     = 10                               ; --solicitud trasp registrada
LET v_t_usuario                    = p_usuario_cod                    ;
LET v_t_ind_origen_sdo_traspaso    = 1                                ; -- traspaso de saldos

   -- Se inserta en tabla de traspasos cedente

   INSERT INTO prt_traspaso_cedente (id_prt_traspaso_cedente        ,
                                     id_prt_solicitud_cedente       ,
                                     instituto_origen               ,
                                     tpo_movimiento                 ,
                                     id_derechohabiente             ,
                                     nss                            ,
                                     ap_paterno                     ,
                                     ap_materno                     ,
                                     nombre                         ,
                                     curp                           ,
                                     tpo_operacion                  ,
                                     id_credito_fovissste           ,
                                     sdo_insoluto_fovissste         ,
                                     f_originacion_fovissste        ,
                                     precio_aiv_infonavit97         ,
                                     f_precio_aiv_infonavit97       ,
                                     mto_aivs_infonavit97           ,
                                     mto_pesos_infonavit97          ,
                                     ind_origen_sdo_traspaso        ,
                                     f_valor_transferencia          ,
                                     estado                         ,
                                     usuario                         )
                            VALUES ( v_t_id_prt_traspaso_cedente    ,
                                     v_t_id_prt_solicitud_cedente   ,
                                     v_t_instituto_origen           ,
                                     v_t_tpo_movimiento             ,
                                     v_t_id_derechohabiente         ,
                                     v_t_nss                        ,
                                     v_t_ap_paterno                 ,
                                     v_t_ap_materno                 ,
                                     v_t_nombre                     ,
                                     v_t_curp                       ,
                                     v_t_tpo_operacion              ,
                                     v_t_id_credito_fovissste       ,
                                     v_t_sdo_insoluto_fovissste     ,
                                     v_t_f_originacion_fovissste    ,
                                     v_t_precio_aiv_infonavit97     ,
                                     v_t_f_precio_aiv_infonavit97   ,
                                     v_t_mto_aivs_infonavit97       ,
                                     v_t_mto_pesos_infonavit97      ,
                                     v_t_ind_origen_sdo_traspaso    ,
                                     v_t_f_valor_transferencia      ,
                                     v_t_estado                     ,
                                     v_t_usuario                     );


   -- si inserta en el historico de id's para el traspaso

   INSERT INTO prt_his_id_folio 
   VALUES (p_id_prt_traspaso_ced_orig     ,
           v_t_id_prt_traspaso_cedente    ,
           ""                             , 
           2                              ,
           today );

   -- Se inserta en tabla temporal del bus

   LET v_folio_procesar = v_t_id_prt_traspaso_cedente;
   LET v_sequencia_bus    = seq_bus_solicitud_tramite.NEXTVAL;    -- Secuencia única del BUS
   
   LET v_t_f_valor_transferencia_bus = TO_CHAR(v_t_f_valor_transferencia,'%Y%m%d');   
   LET v_t_f_originacion_fovissste_bus =  TO_CHAR(v_t_f_originacion_fovissste,'%Y%m%d');
   LET v_t_f_precio_aiv_infonavit97_bus =  TO_CHAR(v_t_f_precio_aiv_infonavit97,'%Y%m%d');
   
   INSERT INTO prt_bus_traspaso (id_bus_solicitud_tramite           ,
                                 bus_proceso_cod                    ,
                                 bus_operacion_cod                  ,
                                 folio_cliente                      ,
                                 folio_procesar                     ,
                                 instituto_origen                   ,
                                 tpo_movimiento                     ,
                                 nss                                ,
                                 ap_paterno                         ,
                                 ap_materno                         ,
                                 nombre                             ,
                                 curp                               ,
                                 tpo_operacion                      ,
                                 id_credito_fovissste               ,
                                 sdo_insoluto_fovissste             ,
                                 f_originacion                      ,
                                 precio_aiv_infonavit97             ,
                                 f_precio_aiv_infonavit97           ,
                                 mto_aivs_infonavit97               ,
                                 mto_pesos_infonavit97              ,
                                 f_valor_transferencia                )
                         VALUES (v_sequencia_bus                    ,
                                 p_proceso_cod                      ,
                                 p_operacion_cod                    ,
                                 v_t_id_prt_traspaso_cedente        , -- folio cliente
                                 v_folio_procesar                   ,
                                 v_t_instituto_origen               ,
                                 v_t_tpo_movimiento                 ,
                                 v_t_nss                            ,
                                 v_t_ap_paterno                     ,
                                 v_t_ap_materno                     ,
                                 v_t_nombre                         ,
                                 v_t_curp                           ,
                                 v_t_tpo_operacion                  ,
                                 v_t_id_credito_fovissste           ,
                                 v_t_sdo_insoluto_fovissste         ,
                                 v_t_f_originacion_fovissste_bus,
                                 v_t_precio_aiv_infonavit97         ,
                                 v_t_f_precio_aiv_infonavit97_bus,
                                 v_t_mto_aivs_infonavit97           ,
                                 v_t_mto_pesos_infonavit97          , 
                                 v_t_f_valor_transferencia_bus      );
   

   -- Se actualiza el saldo solicitado en la tabla de solicitud

   UPDATE prt_solicitud_cedente 
   SET    aivs_saldo_viv97_infonavit  = v_t_mto_aivs_infonavit97, 
          pesos_saldo_viv97_infonavit = v_t_mto_pesos_infonavit97
   WHERE  id_prt_solicitud_cedente    = v_s_id_prt_solicitud_cedente;
 
   -- Llama funcion para invocar WS del BUS para solicitar marca a procesar

   
   LET v_campo            = "id_bus_solicitud_tramite"; -- Columna ID de tabla temporal para envio de la solicitud
   LET v_cad_error        = "";


   -- se envia la solicitud a traves del bus, invocando al servicio iniciador de la solicitud de transferencia

   LET v_origen = "fn_bus_convierte_mensaje";

   EXECUTE PROCEDURE sp_bus_convierte_mensaje(v_folio_procesar   ,
                                              v_sequencia_bus    ,
                                              p_proceso_cod      ,
                                              p_operacion_cod    ,
                                              v_campo            ,
                                              v_cad_error         );
                                              


   -- una vez que se mando la solicitud se avanza la maquinara de estado  

   LET v_origen = "fn_glo_maq_individual";

   EXECUTE FUNCTION fn_glo_maq_individual(2                             , -- maq portabilidad
                                          v_s_id_prt_solicitud_cedente  , -- id de la solicitud
                                          40                            , -- solicitar transferencia de saldo
                                          p_usuario_cod )                 -- usuario  
              INTO v_ind            ,
                   v_diag           ,
                   v_error_sql      ,
                   v_error_isam     ,
                   v_msg_sql        ,
                   v_estado_destino  ;
                                      
   RETURN v_ind            ,
          v_diag           ,
          v_error_sql      ,
          v_error_isam     ,
          v_msg_sql        ,
          v_estado_destino  ;
END PROCEDURE;


