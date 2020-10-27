






CREATE PROCEDURE "safreviv".sp_prt_recibe_folio_procesar_trasp_ced(p_id_bus_solicitud_tramite DECIMAL(9,0),
                                                        p_folio_procesar           CHAR(050)   )

-- varibales de control de errores
DEFINE v_error_sql               INTEGER  ;
DEFINE v_error_isam              INTEGER  ;
DEFINE v_msg_sql                 CHAR(255);
DEFINE v_origen                  CHAR(255);
DEFINE v_ind                     CHAR(005);
DEFINE v_diag                    CHAR(255);
DEFINE v_proceso_cod             CHAR(003);
DEFINE v_operacion_cod           CHAR(004);
DEFINE v_folio_cliente           CHAR(050);
DEFINE v_f_error                 DATETIME YEAR TO SECOND;
DEFINE v_usuario_cod             CHAR(20);
DEFINE v_estado_destino          SMALLINT;


-- variables de prt_traspaso_cednete
DEFINE v_t_id_prt_solicitud_cedente   DECIMAL(9,0);


   ON EXCEPTION SET v_error_sql   , -- cod error sql
                    v_error_isam  , -- cod error isam
                    v_msg_sql       -- descripcion error

       LET  v_f_error = CURRENT;

       EXECUTE PROCEDURE sp_prt_error_bus(p_id_bus_solicitud_tramite   ,
                             v_proceso_cod                ,
                             v_operacion_cod              ,
                             "safreviv"                   ,
                             v_f_error                    ,
                             v_error_sql                  ,
                             v_error_isam                 ,
                             v_msg_sql                    ,
                             v_origen                     , -- funcion de donde proviene el error
                             v_ind                        ,
                             v_diag                        );

   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_recibe_folio_procesar_trasp_ced.trace';
   --TRACE ON;

     LET v_error_sql      = 0;
     LET v_error_isam     = 0;
     LET v_msg_sql        = "";
     LET v_proceso_cod    = "";
     LET v_operacion_cod  = "";
     LET v_folio_cliente  = "";
     LET v_origen         = "sp_prt_recibe_folio_procesar_trasp_ced"; 
     LET v_ind            = "";
     LET v_diag           = "";
     LET v_usuario_cod    = "safreviv";



     SELECT  a.bus_proceso_cod      ,
             a.bus_operacion_cod    ,
             a.folio_cliente       
     INTO    v_proceso_cod,
             v_operacion_cod        ,
             v_folio_cliente
     FROM    prt_bus_traspaso a 
     WHERE   a.id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;

     IF DBINFO('SQLCA.SQLERRD2') = 0 THEN

        LET v_error_sql  = -746     ;
        LET v_error_isam = 0        ;
        LET v_msg_sql    = "folio_procesar no actualizado id: "||p_id_bus_solicitud_tramite;
        LET v_ind        = 1        ;
        LET v_diag       = "folio_procesar no actualizado";
        LET v_f_error    = CURRENT  ;
        
        EXECUTE PROCEDURE sp_prt_error_bus(p_id_bus_solicitud_tramite    ,
                              v_proceso_cod                 ,
                              v_operacion_cod               ,
                              v_usuario_cod                 , -- usuario
                              v_f_error                     ,
                              v_error_sql                   ,
                              v_error_isam                  ,
                              v_msg_sql                     ,
                              v_origen                      , -- funcion de donde proviene el error
                              v_ind                         ,
                              v_diag                         );

     END IF

     -- se actualiza el folio_procesar regresado por el bus (respuesta del servicio inciador) 
     UPDATE prt_traspaso_cedente 
     SET    folio_procesar          = p_folio_procesar
     WHERE  id_prt_traspaso_cedente = v_folio_cliente;

     UPDATE prt_his_id_folio 
     SET    folio_procesar = p_folio_procesar
     WHERE  id_prt_nuevo   = v_folio_cliente;

     SELECT id_prt_solicitud_cedente
     INTO   v_t_id_prt_solicitud_cedente
     FROM   prt_traspaso_cedente 
     WHERE  id_prt_traspaso_cedente = v_folio_cliente;

     -- se avanza maquinaria a solicitud 

     EXECUTE FUNCTION fn_glo_maq_individual(2                            , -- portabilidad cedente
                                            v_t_id_prt_solicitud_cedente , -- id tabla solicitud
                                            44                           , -- recibir folio procesar
                                            v_usuario_cod)
                 INTO v_ind,
                      v_diag,
                      v_error_sql,
                      v_error_isam,
                      v_msg_sql,
                      v_estado_destino;


END PROCEDURE;


