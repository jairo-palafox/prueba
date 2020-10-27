






CREATE PROCEDURE "safreviv".fn_uni_identifica_reg_extractor()

DEFINE v_tmp_nss_ado CHAR(11);
DEFINE v_nss_ado CHAR(11);

FOREACH
   SELECT count (c.nsscta1)
   INTO   v_nss_ado
   FROM   uni_det_unificador a,
          uni_det_unificado c
   WHERE  a.id_unificador = c.id_unificador
   AND    a.folio_unificacion = c.folio_unificacion
   AND    a.folio_unificacion <> 42406
   AND    c.nsscta1 in (SELECT nss_ado
                        FROM   safre_tmp:tmp_ados_marca150)
   AND    a.estado_familia IN (1,5)

   --Si NSS es valido se actualiza estado a encontrado folio <> 42406
   IF v_nss_ado IS NOT NULL THEN
      UPDATE safre_tmp:tmp_ados_marca150
      SET    estado = 1
      WHERE  nss_ado = v_nss_ado;

      LET v_nss_ado = "";
   END IF
END FOREACH;

FOREACH
   SELECT c.nsscta1
   INTO   v_nss_ado
   FROM   uni_det_unificador a,
          uni_det_unificado c
   WHERE  a.id_unificador = c.id_unificador
   AND    a.folio_unificacion = c.folio_unificacion
   AND    a.folio_unificacion = 42406
   AND    c.nsscta1 in (SELECT nss_ado
                        FROM   safre_tmp:tmp_ados_marca150)
   AND    a.estado_familia IN (1,5)
   --Si NSS es valido se actualiza estado a encontrado folio 42406
   IF v_nss_ado IS NOT NULL THEN
      UPDATE safre_tmp:tmp_ados_marca150
      SET    estado = 2
      WHERE  nss_ado = v_nss_ado;

      LET v_nss_ado = "";
   END IF
END FOREACH;

END PROCEDURE
;


