GLOBALS

   DEFINE request RECORD 
      nss             CHAR(11)
   END RECORD
   DEFINE response RECORD
      CodResp         STRING,
      nss             CHAR(11),
      sms             STRING,
      correo          STRING 
   END RECORD
   DEFINE a           SMALLINT
   DEFINE bnd_dato    SMALLINT
   DEFINE v_dato      CHAR(1)
   DEFINE v_s_qry     STRING 

END GLOBALS 