






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
--Temporal proceso portabilidad contingente sar92 deshabilitar la      
--solicitud autompatica solicitado por Gabriel Silva                   
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
   LET v_estado_destino = "";

                                      
   RETURN v_ind            ,
          v_diag           ,
          v_error_sql      ,
          v_error_isam     ,
          v_msg_sql        ,
          v_estado_destino  ;
END PROCEDURE;


