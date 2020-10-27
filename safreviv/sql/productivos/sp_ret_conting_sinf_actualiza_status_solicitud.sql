






CREATE PROCEDURE "safreviv".sp_ret_conting_sinf_actualiza_status_solicitud(v_causa_rec   SMALLINT,  --causa de rechazo
                                                 v_estatus     SMALLINT,  --estatus de solicitud
                                                 v_usuario_cod VARCHAR(20),
                                                 v_mod_marca   CHAR(1),
                                                 v_folio DECIMAL(9,0)
                                                 )
DEFINE v_id_solicitud         DECIMAL(9,0);
DEFINE v_i_estado_marca       INTEGER;
DEFINE v_id_derechohabiente   DECIMAL(9,0);
DEFINE v_marca_solo_infonavit INTEGER; -- 801 de acuerdo a catalogo
DEFINE v_proceso_cod          SMALLINT;

LET v_marca_solo_infonavit = 801; -- marca para solo infonavit
LET v_i_estado_marca       = 0;
LET v_id_derechohabiente   = 0;

LET v_i_estado_marca       = 0;
LET v_id_solicitud         = 0;
LET v_proceso_cod         = 1517; 
 
--SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_status_retiro_solo_info_acti.trace';

   --se busca solicitud para realizar cambio e estatus
   FOREACH 
   SELECT id_solicitud, id_derechohabiente
   INTO   v_id_solicitud ,v_id_derechohabiente
   FROM   ret_solo_infonavit
   WHERE  folio = v_folio
   AND    estado_solicitud = 50

      --cambio de estatus tabla ret_solo_infonavit
      UPDATE  ret_solo_infonavit 
      SET     estado_solicitud   = v_estatus,
              cod_rechazo        = v_causa_rec
      WHERE   id_solicitud       = v_id_solicitud
      AND     folio              = v_folio;

      --TRACE ('------------------------------------\n');
      --TRACE ('v_id_derechohabiente');
      --TRACE (v_id_derechohabiente);
      --TRACE ('v_marca_solo_infonavit');
      --TRACE (v_marca_solo_infonavit);
      --TRACE ('v_id_solicitud');
      --TRACE (v_id_solicitud);
      --TRACE ('v_usuario_cod');
      --TRACE (v_usuario_cod);

      -- si es desmarca
      IF ( v_mod_marca = 'D' ) THEN
         -- se desmarca la cuenta
         EXECUTE FUNCTION fn_desmarca_cuenta(
                         v_id_derechohabiente
                        ,v_marca_solo_infonavit -- desmarca de solo infonavit
                        ,v_id_solicitud         -- identificador de registro de archivo o lote
                        ,0                      -- estado marca
                        ,0                      -- marca de la causa
                        ,v_usuario_cod
                        ,v_proceso_cod) --proceso_cod)
                    INTO v_i_estado_marca;
      END IF
      
      -- si es reverso de la desmarca
      IF ( v_mod_marca = 'R' ) THEN
         -- se ejecuta el reverso de la desmarca
         EXECUTE PROCEDURE sp_reversa_desmarca(
                                v_id_derechohabiente
                               ,v_marca_solo_infonavit    -- marca de disposicion
                               ,v_id_solicitud
                               ,v_folio);
      END IF
  END FOREACH;

END PROCEDURE;


