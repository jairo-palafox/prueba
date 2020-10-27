






CREATE FUNCTION "safreviv".fn_genera_folio(
                p_proceso     SMALLINT,
                p_operacion   SMALLINT,
                p_usuario     CHAR(20))
       RETURNING DECIMAL(9,0);

   DEFINE r_folio     DECIMAL(9,0);

   INSERT INTO glo_folio VALUES(seq_glo_folio.NEXTVAL,
                            p_proceso,
                            p_operacion,
                            0,
                            '',
                            TODAY,
                            p_usuario);

   SELECT seq_glo_folio.CURRVAL
     INTO r_folio
     FROM cat_proceso
    WHERE proceso_cod = p_proceso ;

   RETURN r_folio;

END FUNCTION
;

CREATE FUNCTION "safreviv".fn_genera_folio(
                p_proceso     SMALLINT,
                p_operacion   SMALLINT,
                p_folio_ref   DECIMAL(9,0),
                p_usuario     CHAR(20))
       RETURNING DECIMAL(9,0);

   DEFINE r_folio     DECIMAL(9,0);

   INSERT INTO glo_folio VALUES(seq_glo_folio.NEXTVAL,
                            p_proceso,
                            p_operacion,
                            0,
                            p_folio_ref,
                            TODAY,
                            p_usuario);

   SELECT seq_glo_folio.CURRVAL
     INTO r_folio
     FROM cat_proceso
    WHERE proceso_cod = p_proceso ;

   RETURN r_folio;

END FUNCTION
;


