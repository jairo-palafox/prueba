






CREATE FUNCTION "safreviv".fn_desmarcar_501_prodinf675()

RETURNING SMALLINT,
          INTEGER ;


DEFINE v_id_derechohabiente       DECIMAL(9,0);
DEFINE v_id_derechohabiente_marca DECIMAL(9,0);
DEFINE v_nss_501                  CHAR(11);
DEFINE v_tot_desmarca             INTEGER;
DEFINE v_n_referencia             INTEGER;
DEFINE v_error                    INTEGER;
DEFINE v_bnd_marca                SMALLINT;
DEFINE v_folio                    DECIMAL(9,0);

   ON EXCEPTION SET v_error
      RETURN v_error,
             v_tot_desmarca; -- ERROR
   END EXCEPTION WITH RESUME;

LET v_tot_desmarca       = 0;
LET v_id_derechohabiente = 0;
LET v_nss_501            = "";
LET v_n_referencia       = 0;
LET v_error              = 0;
LET v_folio              = 0;
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_desmarcar_501_prodinf675.trace';
   --TRACE ON;

   FOREACH
      SELECT nss_501
      INTO   v_nss_501
      FROM   safre_tmp:desmarcar_501
         
      SELECT id_derechohabiente 
      INTO   v_id_derechohabiente 
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_501;
  
      SELECT id_derechohabiente, 
             n_referencia,
             folio
      INTO   v_id_derechohabiente_marca, 
             v_n_referencia,
             v_folio
      FROM   sfr_marca_activa
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    marca = 501;

      IF v_id_derechohabiente_marca IS NOT NULL THEN 
         EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_marca,501,v_n_referencia,40,0,"safreviv",2301)
         INTO v_bnd_marca;
         
         UPDATE uni_pre_unificador 
         SET    estado_familia = 2,
                estado         = 2,
                diagnostico    = 2
         WHERE  folio_lote = v_folio
         AND    id_derechohabiente = v_id_derechohabiente_marca;

         LET v_tot_desmarca = v_tot_desmarca + 1;
      END IF
   END FOREACH;
      
   RETURN v_error,
          v_tot_desmarca;

END FUNCTION;


