






CREATE PROCEDURE "safreviv".sp_liquida_manual_acl (p_folio       DECIMAL(9,0),
                                  p_usuario     CHAR(20)    ,
                                  p_pid         DECIMAL(9,0),
                                  p_proceso_cod SMALLINT    ,
                                  p_opera_cod   SMALLINT    ,
                                  p_tabla       CHAR(30)    ,
                                  p_id_referencia DECIMAL(9,0))
RETURNING SMALLINT;

DEFINE v_cadena     CHAR(300);
DEFINE v_status     SMALLINT;
DEFINE  sql_err          INTEGER ;
DEFINE  isam_err         INTEGER ;
DEFINE  error_info       CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
      IF sql_err = -746 THEN
         RAISE EXCEPTION sql_err, 0, error_info;
      ELSE
         LET v_status = sql_err;
         RETURN v_status;
      END IF
END EXCEPTION

   SET PDQPRIORITY HIGH;


   LET v_cadena = "INSERT INTO cta_movimiento "||
                  "SELECT * FROM "||TRIM(p_tabla)||
                  " WHERE folio_liquida = "||p_folio||
                  " AND   id_referencia = "||p_id_referencia;

   EXECUTE IMMEDIATE v_cadena;

{
   UPDATE glo_folio
      SET status = 2
    WHERE folio = p_folio;

   UPDATE bat_ctr_proceso
      SET folio       = p_folio
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod;
}
   RETURN 0;
END PROCEDURE;


