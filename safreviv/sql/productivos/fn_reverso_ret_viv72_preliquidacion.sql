






CREATE FUNCTION "safreviv".fn_reverso_ret_viv72_preliquidacion(v_folio_liquida      DECIMAL(10,0),
                                                    v_proceso_cod        smallint,
                                                    v_opera_cod          smallint)

                                                 RETURNING SMALLINT
   DEFINE v_id_solicitud         DECIMAL(9,0);
   DEFINE v_pid                  DECIMAL(9,0);
   DEFINE v_nss                  CHAR(12);
   DEFINE v_marca_entra          SMALLINT;
   DEFINE v_id_afi_fondo72       DECIMAL(9,0);

   LET v_marca_entra    = 802;
   LET v_nss            = 0;
   LET v_id_solicitud   = 0.0;
   LET v_id_afi_fondo72 = 0;
   LET v_pid            = 0;

   --busca todos las solicitudes con el folio dado
   FOREACH cu_reverso_ret_fondo_ahorro FOR SELECT id_referencia ,id_afi_fondo72 --, nss
                                       INTO v_id_solicitud,v_id_afi_fondo72  --, v_nss
                                       FROM ret_preliquida72
                                      WHERE folio_liquida = v_folio_liquida
    --IF sqlcode != 0 THEN
    --regresa a estatus capturado solicitud
    SELECT nss
      INTO v_nss
      FROM afi_fondo72
     WHERE id_afi_fondo72 = v_id_afi_fondo72;

      UPDATE  ret_fondo_ahorro_generico    SET estado_solicitud   = 15
                                         WHERE id_solicitud       = v_id_solicitud
                                           AND estado_solicitud   = 50;

      UPDATE  ret_solicitud_generico       SET estado_solicitud   = 15
                                         WHERE id_solicitud       = v_id_solicitud
                                           AND estado_solicitud   = 50;

      --se invoca SP que reversa la marca de la cuenta consultada
      --se reversa hasta qe la solicitd sea reversada por no pago
      --EXECUTE PROCEDURE  sp_reversa_marca ( v_id_derechohabiente,
                              --v_marca_entra  ,
                              --v_id_solicitud ,
                              --v_folio_liquida );

  END FOREACH ;

      --coloca el folio como reversado
      UPDATE glo_folio SET status         = 10
                               WHERE proceso_cod    = v_proceso_cod
                                 AND opera_cod      = v_opera_cod
                                 AND folio          = v_folio_liquida;


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

      --coloca operacion 2 como reversado
      UPDATE bat_ctr_operacion SET estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod
                                         AND opera_cod    = 3;

     UPDATE bat_ctr_proceso SET  estado_cod   = 10
                                       WHERE folio        = v_folio_liquida
                                         AND proceso_cod  = v_proceso_cod;

END FUNCTION;


