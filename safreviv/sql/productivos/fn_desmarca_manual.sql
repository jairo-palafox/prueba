






CREATE FUNCTION "safreviv".fn_desmarca_manual(p_marca SMALLINT, 
                                   p_proceso_cod SMALLINT)

RETURNING INTEGER,
          INTEGER,
          VARCHAR(250),
          SMALLINT,
          INTEGER;

DEFINE sql_err              INTEGER;
DEFINE isam_err             INTEGER;
DEFINE err_txt              VARCHAR(250);
DEFINE v_resultado          SMALLINT;
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_n_referencia       DECIMAL(9,0);
DEFINE v_error              INTEGER;
DEFINE v_bnd_marca          SMALLINT;
DEFINE v_tot_desmarca       INTEGER;

ON EXCEPTION SET sql_err, isam_err, err_txt
   RETURN sql_err,
          isam_err,
          err_txt,
          v_resultado,
          v_tot_desmarca;
END EXCEPTION

LET v_id_derechohabiente  = 0;
LET v_n_referencia        = 0;
LET v_resultado           = 0;
LET v_tot_desmarca        = 0;
LET sql_err               = 0;
LET isam_err              = 0;
LET err_txt               = "Desmarca realizada exitosamente";

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_desmarca_manual.trace';
   --TRACE ON;

   FOREACH 
      SELECT id_derechohabiente, 
             n_referencia
      INTO   v_id_derechohabiente, 
             v_n_referencia
      FROM   sfr_marca_activa
      WHERE  marca = p_marca
      
      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,p_marca,v_n_referencia,40,'',"safreviv",p_proceso_cod)
      INTO v_bnd_marca;
      
      IF v_bnd_marca <> 0 THEN 
         LET v_resultado = 1;
         LET err_txt     = "Error en desmarca de derechohabiente " || v_id_derechohabiente ;
      ELSE
         LET v_tot_desmarca = v_tot_desmarca+1;
      END IF
   END FOREACH;

   IF v_tot_desmarca = 0 THEN 
      LET err_txt = "No se desmarcó ninguna cuenta";
   END IF 

   RETURN sql_err,
          isam_err,
          err_txt,
          v_resultado,
          v_tot_desmarca; 
END FUNCTION;


