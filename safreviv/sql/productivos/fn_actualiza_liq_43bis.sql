






CREATE FUNCTION "selefp".fn_actualiza_liq_43bis(p_clave INTEGER)
-----------------------------------------------------------
   RETURNING SMALLINT, SMALLINT

   DEFINE v1_id_derechohabiente  DECIMAL(9,0) ;
   DEFINE v1_nss                 CHAR(11)     ;
   DEFINE v_error                SMALLINT     ;
   DEFINE cont_1                 SMALLINT     ;
   DEFINE v_error_1              SMALLINT     ;


   ON EXCEPTION SET v_error
      LET cont_1 = 0 ;
      RETURN v_error, cont_1 ;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_actualiza_liq_43bis.trace';
   TRACE ON;

   LET v_error = 0 ;
   LET cont_1  = 0 ;

   IF p_clave = 18 THEN
       FOREACH
           SELECT a.nss   ,
                  b.id_derechohabiente
           INTO   v1_nss  ,
                  v1_id_derechohabiente
           FROM   safre_tmp:ocg_tmp_liq1_old a, afi_derechohabiente b
           WHERE  a.nss = b.nss

           LET cont_1 = cont_1 + 1 ;

           UPDATE safre_tmp:ocg_tmp_liq1_old
           SET    id_derechohabiente = v1_id_derechohabiente
           WHERE  nss = v1_nss ;
       END FOREACH
   END IF
   RETURN v_error, cont_1 ;
END FUNCTION ;


