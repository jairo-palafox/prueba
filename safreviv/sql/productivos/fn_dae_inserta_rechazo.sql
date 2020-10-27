






CREATE FUNCTION "safreviv".fn_dae_inserta_rechazo(p_rch_folio  DECIMAL(9,0),
                                       p_nss        CHAR(11),
                                       p_dae_id_ref DECIMAL(9,0))

   RETURNING INTEGER,
             INTEGER,
             CHAR(200),
             SMALLINT

   -- Control de Excepciones
   DEFINE sql_err  INTEGER;
   DEFINE isam_err INTEGER;
   DEFINE v_isam_err  INTEGER;
   DEFINE err_txt  CHAR(200);
   DEFINE v_correcto_integra SMALLINT;          
   DEFINE v_i_resultado SMALLINT;

   ON EXCEPTION SET sql_err, isam_err, err_txt

      LET v_i_resultado = sql_err;
      LET v_correcto_integra = 1;
      LET v_isam_err = isam_err;

      RETURN v_i_resultado,
             isam_err, 
             err_txt,
             v_correcto_integra;
   END EXCEPTION
            
--   SET DEBUG FILE TO "/ds/safreviv_int/BD/dae_inserta_rch.txt";
--   TRACE ON;
            
   LET v_correcto_integra    = 0;
   LET v_i_resultado         = 0;
   LET err_txt               = "Ok";
   LET v_isam_err            = 0;            

   INSERT INTO dae_rch_archivo         
   VALUES      (seq_dae_rch_archivo.NEXTVAL,
                p_rch_folio,
                1,
                p_dae_id_ref,
                "02",
                8,
                "RECHAZO POR DICTAMEN");
                      
 RETURN v_i_resultado,
        v_isam_err,
        err_txt,
        v_correcto_integra;
END FUNCTION;


