






CREATE FUNCTION "safreviv".fn_reverso_ret_viv97_preli(v_folio_liquida      DECIMAL(10,0),
                                           v_proceso_cod        smallint,
                                           v_opera_cod          smallint,
                                           p_usuario_cod        char(20))
                                        returning smallint

   DEFINE v_id_solicitud         DECIMAL(9,0);
   DEFINE v_pid                  SMALLINT;
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_marca_entra          SMALLINT;

   LET v_marca_entra = 801;
   LET v_id_derechohabiente = 0;

   LET v_id_solicitud = 0;
   LET v_pid = 0;

   --busca todos las solicitudes con el folio dado
   FOREACH cu_reverso_ret_solo_infonavit FOR SELECT id_referencia, id_derechohabiente
                                               INTO v_id_solicitud,v_id_derechohabiente
                                               FROM ret_preliquida
                                              WHERE folio_liquida = v_folio_liquida
      --IF sqlcode != 0 THEN
      --regresa a estatus capturado solicitud
      UPDATE  ret_solo_infonavit SET estado_solicitud   = 10,
                                               f_captura          = TODAY
                                         WHERE id_solicitud       = v_id_solicitud
                                           AND estado_solicitud   = 50;

      --se invoca SP que reversa la marca de la cuenta consultada
     /* EXECUTE PROCEDURE  sp_reversa_marca ( v_id_derechohabiente,
                                            v_marca_entra  ,
                                            v_id_solicitud ,
                                            v_folio_liquida );*/
      --end if
   END FOREACH;

   --coloca el folio como reversado
      UPDATE glo_folio SET status         = 10
                               WHERE proceso_cod    = v_proceso_cod
                                 AND opera_cod      = v_opera_cod
                                 AND folio          = v_folio_liquida;

     UPDATE bat_ctr_operacion SET folio        = v_folio_liquida,
                                             estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod
                                         AND opera_cod    = 0;

      --coloca operacion 1 como reversado
      UPDATE bat_ctr_operacion SET folio        = v_folio_liquida,
                                             estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod
                                         AND opera_cod    = 1;

      --coloca operacion 2 como reversado
      UPDATE bat_ctr_operacion SET estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod
                                         AND opera_cod    = 2;

      --coloca operacion 3 como reversado
      UPDATE bat_ctr_operacion SET estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod
                                         AND opera_cod    = 3;

     UPDATE bat_ctr_proceso SET  estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod;

END FUNCTION;


