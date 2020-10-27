






CREATE FUNCTION "safreviv".fn_busca_info_cre_hana(p_nss CHAR(11))
RETURNING SMALLINT, SMALLINT, SMALLINT, SMALLINT, SMALLINT;


DEFINE v_rch_cod            SMALLINT;
DEFINE v_credito_vigente    SMALLINT;
DEFINE v_credito_cancelado  SMALLINT;
DEFINE v_credito_liquidado  SMALLINT;
DEFINE v_sin_tramite        SMALLINT;
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_entidad            SMALLINT;
DEFINE v_entidad_desc       CHAR(30);
DEFINE dumy                 CHAR(1);

LET v_credito_vigente   = 0 ;
LET v_credito_cancelado = 0 ;
LET v_credito_liquidado = 0 ;
LET v_sin_tramite       = 0 ;
LET v_rch_cod           = 0 ;


PREPARE c_stmt FROM "SELECT DISTINCT c.id_derechohabiente, m.entidad " ||
                    "FROM cre_acreditado c, cat_tipo_credito d, cat_maq_credito m, " ||
                    "cat_cre_entidad e, afi_derechohabiente a " ||
                    "WHERE a.nss = ? " ||
                    "AND c.id_derechohabiente = a.id_derechohabiente " ||
                    "AND c.tpo_originacion = d.tpo_originacion " ||
                    "AND c.tpo_credito = d.tpo_credito " ||
                    "AND c.estado = m.estado " ||
                    "AND m.entidad = e.entidad ; " ;

DECLARE cur_credito CURSOR FOR c_stmt ;
OPEN cur_credito USING p_nss;

WHILE ( 1 = 1 )
   FETCH cur_credito INTO v_id_derechohabiente, v_entidad ;
   IF (SQLCODE != 100) THEN
       IF v_id_derechohabiente IS NOT NULL THEN

          IF v_entidad = 1 THEN
             LET v_credito_vigente   = 1 ;
          ELSE 
             IF v_entidad = 2 THEN
                LET v_credito_liquidado = 1 ;
                ELSE
                   IF v_entidad = 3 THEN
                      LET v_sin_tramite       = 1 ;
                   ELSE
                      IF v_entidad = 5 THEN
                         LET v_credito_cancelado = 1 ;
                      END IF
                END IF
             END IF
          END IF

       ELSE
          LET v_rch_cod = 1 ;
       END IF
   ELSE
      EXIT;
   END IF
END WHILE ;

RETURN v_rch_cod, v_credito_vigente, v_credito_cancelado, v_credito_liquidado, v_sin_tramite;

END FUNCTION
;


