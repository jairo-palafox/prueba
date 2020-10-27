






CREATE PROCEDURE "safreviv".sp_prt_recibe_notif_marca_receptora(p_id_bus_solicitud_tramite DECIMAL(9,0),
                                                     p_folio_procesar VARCHAR(50))
-- Resultado de solicitud de marca
DEFINE v_bus_proceso_cod  CHAR(3);
DEFINE v_bus_opera_cod    CHAR(4);
DEFINE v_origen_solicitud CHAR(3);
DEFINE v_tipo_solicitud   CHAR(2);
DEFINE v_nss              CHAR(11);
DEFINE v_nombre           VARCHAR(40);
DEFINE v_paterno          VARCHAR(40);
DEFINE v_materno          VARCHAR(40);
DEFINE v_curp             CHAR(18);
DEFINE v_diag_procesar    CHAR(3);

DEFINE v_estado_sol       SMALLINT;
DEFINE v_id_prt_solicitud_receptora DECIMAL(9,0);
DEFINE v_usuario_cod      CHAR(20);
DEFINE v_proceso_marca    SMALLINT;
DEFINE v_marca_prt        SMALLINT;

DEFINE v_id_maquinaria    SMALLINT;
DEFINE v_senial_mrca_acp  SMALLINT;
DEFINE v_senial_mrca_rch  SMALLINT;

-- Resultado de maquinaria
DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);
DEFINE v_error_sql      INTEGER;
DEFINE v_error_isam     INTEGER;
DEFINE v_msg_sql        CHAR(254);
DEFINE v_estado_destino SMALLINT;

DEFINE v_id_derechohabiente DECIMAL(9,0);

DEFINE v_existe  SMALLINT;

DEFINE r_cod_rch      SMALLINT;
DEFINE v_f_error_bus  DATETIME YEAR TO SECOND;
DEFINE v_origen_error CHAR(40);
DEFINE v_diag_bus     CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      
             
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_recibe_notif_marca_receptora.trace';
   --TRACE ON;
   
   LET v_estado_sol    = 10;  -- Solicitud consultada
   LET v_usuario_cod   = 'SAFREVIV';
   LET v_marca_prt     = 704; -- Marca portabilidad receptora
   LET v_id_maquinaria = 3;
   LET v_proceso_marca = 2803;
   
   LET v_senial_mrca_acp = 20; -- Marca aceptada
   LET v_senial_mrca_rch = 25; -- Marca rechazada
   
   LET v_existe = 0;
   
   -- Recupera la respuesta de solicitud de marca portabilidad
   SELECT bus_proceso_cod,
          bus_operacion_cod,
          origen_solicitud,
          tipo_solicitud,
          nss,
          nombre,
          ap_paterno,
          ap_materno,
          curp,
          diagnostico_procesar
     INTO v_bus_proceso_cod,
          v_bus_opera_cod,
          v_origen_solicitud,
          v_tipo_solicitud,
          v_nss,
          v_nombre,
          v_paterno,
          v_materno,
          v_curp,
          v_diag_procesar
     FROM prt_bus_notifica_marca
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
   
   LET v_id_prt_solicitud_receptora = NULL;
   
   -- Recupera la última consultada
   SELECT MAX(id_prt_solicitud_receptora)
     INTO v_id_prt_solicitud_receptora
     FROM prt_solicitud_receptora rcp
    WHERE nss = v_nss
      AND estado = v_estado_sol;

   IF( v_id_prt_solicitud_receptora IS NOT NULL )THEN
      
      -- 000 registro aceptado
      IF( v_diag_procesar = '000' )THEN
         SELECT id_derechohabiente
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         -- En caso de que no se haya marcado la cuenta, la marca esta considerada para convivir con 
         -- las demás marca, por lo que debe ser posible continuar el proceso sin la marca       
         EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                          v_marca_prt,
                                          v_id_prt_solicitud_receptora, -- id_referencia
                                          p_id_bus_solicitud_tramite, -- en lugar de folio, el identificador de ejecución del bus
                                          0,    -- Estado de marca
                                          0,    -- Código de rechazo
                                          NULL, -- Marca causa
                                          NULL, -- Fecha causa
                                          v_usuario_cod,
                                          v_proceso_marca) INTO r_cod_rch;

         EXECUTE FUNCTION fn_glo_maq_individual(v_id_maquinaria,
                                                v_id_prt_solicitud_receptora,
                                                v_senial_mrca_acp,
                                                v_usuario_cod) INTO v_ind,
                                                                    v_diag,
                                                                    v_error_sql,
                                                                    v_error_isam,
                                                                    v_msg_sql,
                                                                    v_estado_destino;
         UPDATE prt_solicitud_receptora
            SET folio_procesar = p_folio_procesar
          WHERE id_prt_solicitud_receptora = v_id_prt_solicitud_receptora;
      ELSE
         -- Rechaza 
         EXECUTE FUNCTION fn_glo_maq_individual(v_id_maquinaria,
                                                v_id_prt_solicitud_receptora,
                                                v_senial_mrca_rch,
                                                v_usuario_cod) INTO v_ind,
                                                                    v_diag,
                                                                    v_error_sql,
                                                                    v_error_isam,
                                                                    v_msg_sql,
                                                                    v_estado_destino;
         UPDATE prt_solicitud_receptora
            SET folio_procesar = p_folio_procesar
          WHERE id_prt_solicitud_receptora = v_id_prt_solicitud_receptora;
      END IF
      -- Elimina registro temporal de bus
      DELETE
        FROM prt_bus_notifica_marca
       WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
   ELSE
      LET v_f_error_bus  = CURRENT YEAR TO SECOND;
      LET v_error_sql    = 100; -- No hay registro
      LET v_error_isam   = NULL;
      LET v_msg_sql      = "Cuenta no procesada, no existe solicitud para NSS " || v_nss;
      LET v_origen_error = "sp_prt_recibe_notif_marca_receptora";
      LET v_ind          = 0;
      LET v_diag_bus     = "NSS " || v_nss || " con diagnóstico procesar " || v_diag_procesar;
      EXECUTE PROCEDURE sp_prt_error_bus(p_id_bus_solicitud_tramite,
                                         v_bus_proceso_cod,
                                         v_bus_opera_cod,
                                         v_usuario_cod,
                                         v_f_error_bus,
                                         v_error_sql,
                                         v_error_isam,
                                         v_msg_sql,
                                         v_origen_error,
                                         v_ind,
                                         v_diag_bus);
      -- EL registro temporal del bus se conserva para revisión
   END IF
                                       
END PROCEDURE;


