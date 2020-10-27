






CREATE FUNCTION "safreviv".fn_agr_obtiene_mov_liq(v_tpo_orig SMALLINT, v_tpo_transf CHAR(2))

   RETURNING SMALLINT

   DEFINE v_folio_liquida  DECIMAL(9,0);
   DEFINE v_criterio       SMALLINT;
   DEFINE v_obtiene        SMALLINT;
   DEFINE v_tt             VARCHAR(5);
   DEFINE v_sqry           CHAR(500);
   DEFINE v_sqry_tb        CHAR(500);
   DEFINE v_tabla          VARCHAR(20);
   DEFINE v_f_liquida      DATE;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrObtieneMov.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_obtiene   = 0;
   LET v_criterio  = 0;
   LET v_f_liquida = "12/31/1899";

   IF v_tpo_transf = "18" THEN
      LET v_tt = "18','48";
   ELSE
      LET v_tt = v_tpo_transf;
   END IF

   -- se obtienen folios liquidados
   INSERT INTO safre_tmp:tmp_folio_liq_agr
   SELECT DISTINCT folio_liquida
     FROM cre_acreditado cre
    WHERE cre.folio_liquida <> 0
      AND cre.estado IN(140, 145)
      AND cre.edo_procesar IN (60, 70)
      AND cre.tpo_originacion = v_tpo_orig;

   IF v_tpo_transf = "18" THEN
      INSERT INTO safre_tmp:tmp_folio_liq_agr
      SELECT DISTINCT folio_liquida
        FROM cre_uso_garantia uso
       WHERE uso.folio_liquida <> 0
         AND uso.estado = 140
         AND uso.edo_procesar IN (10, 70)
         AND tpo_transferencia IN('18','48')
         AND folio_liquida NOT IN(
             SELECT fol_lq.folio_liquida
               FROM safre_tmp:tmp_folio_liq_agr fol_lq);
   ELSE
      INSERT INTO safre_tmp:tmp_folio_liq_agr
      SELECT DISTINCT folio_liquida
        FROM cre_uso_garantia uso
       WHERE uso.folio_liquida <> 0
         AND uso.estado = 140
         AND uso.edo_procesar IN (10, 70)
         AND tpo_transferencia IN('43')
         AND folio_liquida NOT IN(
             SELECT fol_lq.folio_liquida
               FROM safre_tmp:tmp_folio_liq_agr fol_lq);
   END IF

   --se obtienen movimientos
   FOREACH
      SELECT folio_liquida
        INTO v_folio_liquida
        FROM safre_tmp:tmp_folio_liq_agr

      CALL fn_tab_movimiento (v_criterio,v_folio_liquida,v_f_liquida)
      RETURNING v_tabla;

      LET v_sqry = " INSERT INTO safre_tmp:tmp_cta_mov_agrs04 "||
                   " SELECT cta.id_derechohabiente, "||
                   "        cta.subcuenta, "||
                   "        cta.folio_liquida, "||
                   "        cta.monto_acciones "||
                   "   FROM "||v_tabla ||" cta WHERE cta.folio_liquida = "||v_folio_liquida;

      EXECUTE IMMEDIATE v_sqry;
   END FOREACH;

   RETURN v_obtiene;

END FUNCTION;


