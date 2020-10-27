GLOBALS

   DEFINE v_rec_in RECORD 
      nss_in          CHAR(11)
   END RECORD 
   DEFINE resp_out RECORD
      CodResp             STRING,
      nss                 CHAR (11),
      afore               STRING
   END RECORD
   DEFINE a               SMALLINT
   DEFINE bnd_dato        SMALLINT
   DEFINE v_dato          CHAR(1)
   DEFINE v_s_qry         STRING 

END GLOBALS 