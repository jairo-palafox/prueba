






CREATE FUNCTION "safreviv".fn_grt_preliquida(p_folio_liq DECIMAL(9,0),
                                  p_folio_acr DECIMAL(9,0),
                                  p_id_cre_acreditado DECIMAL(9,0),
                                  p_id_derechohabiente DECIMAL(9,0),
                                  p_valor_fondo DECIMAL(19,14),
                                  p_tpo_trabajador CHAR(1),
                                  p_tpo_origina SMALLINT)

RETURNING SMALLINT

   ----variable para status de ejecución
   DEFINE v_status          SMALLINT;
   DEFINE v_fc_status       SMALLINT;
   DEFINE v_av_status       SMALLINT;    

   -- Variables para la ejecución de la función de preliquidación de vivienda voluntaria RISS
   DEFINE v_proceso         SMALLINT;
   DEFINE v_edo_volriss     SMALLINT;
   DEFINE v_err_volriss     SMALLINT;

   --SET DEBUG FILE TO 'preliquidaGrt.trace';
   --TRACE ON;

   LET v_status   = 0;
   LET v_proceso  = 1227;

   ---Valor aivs al día de la preliquidación
   IF p_valor_fondo IS NULL THEN
      LET v_status = 1;
   ELSE

      EXECUTE FUNCTION fn_cre_preliq_volriss(p_folio_liq         ,
                                             p_id_cre_acreditado ,
                                             p_id_derechohabiente,
                                             p_valor_fondo       ,
                                             p_tpo_trabajador    ,
                                             v_proceso           )
                                        INTO v_edo_volriss,v_err_volriss;

      ----Verificación de aportaciones para fortalecimiento del crédito
      EXECUTE FUNCTION fn_cre_fort_cred(p_folio_liq,
                                        p_id_cre_acreditado,
                                        p_id_derechohabiente,
                                        p_valor_fondo,
                                        p_tpo_trabajador,
                                        p_tpo_origina)
                                   INTO v_fc_status;

      ----Verificación de aportaciones para aportaciones voluntarias
      EXECUTE FUNCTION fn_cre_ap_vol(p_folio_liq,
                                     p_id_cre_acreditado,
                                     p_id_derechohabiente,
                                     p_valor_fondo,
                                     p_tpo_trabajador,
                                     p_tpo_origina)
                                   INTO v_av_status;
   END IF

   RETURN v_status;

   --Finaliza la función de preliquidación de solicitud de saldo en garantía 43 bis
END FUNCTION;


