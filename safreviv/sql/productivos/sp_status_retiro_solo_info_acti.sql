






CREATE PROCEDURE "safreviv".sp_status_retiro_solo_info_acti(v_causa_rec   SMALLINT,  --causa de rechazo
                                                 v_estatus     SMALLINT,  --estatus de solicitud
                                                 v_usuario_cod VARCHAR(20),
                                                 v_mod_marca   CHAR(1)
                                                 )
   DEFINE v_id_solicitud         DECIMAL(9,0);
   DEFINE v_folio                SMALLINT;
   DEFINE v_i_estado_marca       INTEGER;
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_marca_solo_infonavit INTEGER; -- 801 de acuerdo a catalogo
   DEFINE v_proceso_cod          SMALLINT;

   LET v_marca_solo_infonavit = 801; -- marca para solo infonavit
   LET v_i_estado_marca       = 0;
   LET v_id_derechohabiente   = 0;

   LET v_i_estado_marca       = 0;
   LET v_id_solicitud         = 0;
   LET v_folio                = 0;
   LET v_proceso_cod         = 1501; 
 
--SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_status_retiro_solo_info_acti.trace';

  IF v_mod_marca = 'D' THEN
   --toma el ultimo folio activo y modifica su estatus
   SELECT max(folio_liquida)
     INTO v_folio
     FROM ret_preliquida  a ,
          glo_folio b
    WHERE a.folio_liquida = b.folio
      AND b.proceso_cod   = 1501 --50
      AND b.opera_cod     = 1
      AND b.status        = 2;
   END IF

  IF v_mod_marca = 'R' THEN

   --toma el ultimo folio activo y modifica su estatus
   SELECT max(folio_liquida)
     INTO v_folio
     FROM ret_preliquida  a ,
          glo_folio b
    WHERE a.folio_liquida = b.folio
      AND b.proceso_cod   = 1501  --solo infonavit
      AND b.opera_cod     = 1
      AND b.status        = 1;
   END IF

       --TRACE ('------------------------------------\n');
       --TRACE ('v_folio');
       --TRACE (v_folio);
       --TRACE ('v_marca_solo_infonavit');
       --TRACE (v_marca_solo_infonavit);
       --TRACE ('v_id_solicitud');
       --TRACE (v_id_solicitud);
       --TRACE ('v_usuario_cod');
       --TRACE (v_usuario_cod);

   --se busca solicitud para realizar cambio e estatus
  FOREACH cu_status_solo_infonavit FOR SELECT id_referencia, id_derechohabiente
                                              INTO v_id_solicitud ,v_id_derechohabiente
                                              FROM ret_preliquida
                                             WHERE folio_liquida = v_folio

   --cambio de estatus tabla ret_solo_infonavit
   UPDATE  ret_solo_infonavit SET estado_solicitud   = v_estatus,
                                            cod_rechazo        = v_causa_rec,
                                            f_captura          = TODAY
                                      WHERE id_solicitud       = v_id_solicitud;

       --TRACE ('------------------------------------\n');
       --TRACE ('v_id_derechohabiente');
       --TRACE (v_id_derechohabiente);
       --TRACE ('v_marca_solo_infonavit');
       --TRACE (v_marca_solo_infonavit);
       --TRACE ('v_id_solicitud');
       --TRACE (v_id_solicitud);
       --TRACE ('v_usuario_cod');
       --TRACE (v_usuario_cod);

    IF v_mod_marca = 'D' THEN
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


     IF v_mod_marca = 'R' THEN

      EXECUTE PROCEDURE sp_reversa_desmarca(
                              v_id_derechohabiente
                             ,v_marca_solo_infonavit    -- marca de disposicion
                             ,v_id_solicitud
                             ,v_folio);

      END IF
  END FOREACH;

END PROCEDURE;


