






CREATE FUNCTION "safreviv".fn_reversa_desmarca_prodinf675()

RETURNING SMALLINT,
          INTEGER ;


DEFINE v_id_derechohabiente       DECIMAL(9,0);
DEFINE v_id_derechohabiente_marca DECIMAL(9,0);
DEFINE v_nss_501                  CHAR(11);
DEFINE v_nss_502                  CHAR(11);
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

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reversa_desmarca_prodinf675.trace';
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
      FROM   sfr_marca_historica
      WHERE  marca = 501
      AND    id_derechohabiente = v_id_derechohabiente;

      IF v_id_derechohabiente_marca IS NOT NULL THEN 
         EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_marca, 501, v_n_referencia, v_folio)
         INTO v_bnd_marca;
         
         UPDATE uni_pre_unificador 
         SET    estado_familia = 1,
                estado         = 1,
                diagnostico    = 1
         WHERE  id_derechohabiente = v_id_derechohabiente_marca;

         LET v_tot_desmarca = v_tot_desmarca + 1;
      END IF
   END FOREACH;
   
   FOREACH
      SELECT nss_502
      INTO   v_nss_502
      FROM   safre_tmp:desmarcar_502
         
      SELECT id_derechohabiente 
      INTO   v_id_derechohabiente 
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_502;

      SELECT id_derechohabiente, 
             n_referencia, 
             folio
      INTO   v_id_derechohabiente_marca, 
             v_n_referencia, 
             v_folio
      FROM   sfr_marca_historica
      WHERE  marca = 502
      AND    id_derechohabiente = v_id_derechohabiente;

      IF v_id_derechohabiente_marca IS NOT NULL THEN 
         EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_marca, 502, v_n_referencia, v_folio)
         INTO v_bnd_marca;
         
         UPDATE uni_pre_unificado
         SET    estado         = 1,
                diagnostico    = 1
         WHERE  id_derechohabiente = v_id_derechohabiente_marca;

         LET v_tot_desmarca = v_tot_desmarca + 1;
      END IF
   END FOREACH;
   
      
   RETURN v_error,
          v_tot_desmarca;

END FUNCTION;


