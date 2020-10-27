






CREATE FUNCTION "safreviv".fn_liquida_decreto (p_folio       DECIMAL(9,0),
                                    p_usuario     CHAR(20)    ,
                                    p_pid         DECIMAL(9,0),
                                    p_proceso_cod SMALLINT    ,
                                    p_opera_cod   SMALLINT    ,
                                    p_tabla       CHAR(30)    )
RETURNING SMALLINT, INTEGER, VARCHAR

DEFINE v_cadena     CHAR(300);
DEFINE v_status     SMALLINT;
DEFINE  sql_err          INTEGER ;
DEFINE  isam_err         INTEGER ;
DEFINE  error_info       CHAR(70);

   DEFINE v_error_isam INTEGER;
   DEFINE v_mensaje    VARCHAR(250);
  
 
   -- en caso de excepcion
   ON EXCEPTION SET v_status, v_error_isam, v_mensaje
               
      RETURN v_status, v_error_isam, v_mensaje;
   END EXCEPTION 


--ON EXCEPTION
--   SET sql_err, isam_err, error_info
--      IF sql_err = -746 THEN
--         RAISE EXCEPTION sql_err, 0, error_info;
--      ELSE
--         LET v_status = sql_err;
--         RETURN v_status;
--      END IF
--END EXCEPTION

   SET PDQPRIORITY HIGH;

   -- se asume que no hay error
   LET v_status     = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Proceso de liquidación finalizado correctamente";


   LET v_cadena = "INSERT INTO cta_decreto SELECT * FROM "||TRIM(p_tabla)||
               " WHERE folio_liquida = "||p_folio;

   EXECUTE IMMEDIATE v_cadena;

   UPDATE glo_folio
      SET status = 2
    WHERE folio = p_folio;

   UPDATE bat_ctr_proceso
      SET folio       = p_folio
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod;

   RETURN v_status, v_error_isam, v_mensaje;
END FUNCTION;


