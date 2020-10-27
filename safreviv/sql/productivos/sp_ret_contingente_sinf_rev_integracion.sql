






CREATE PROCEDURE "safreviv".sp_ret_contingente_sinf_rev_integracion(p_folio DECIMAL(9,0))

DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_marca_entra        SMALLINT    ;
DEFINE v_id_solicitud       DECIMAL(9,0);

   -- se define la marca solo infonavit
   LET v_marca_entra = 801;

   -- se reversa la marca de los registros
   FOREACH
   SELECT
      id_derechohabiente,
      id_solicitud
   INTO
      v_id_derechohabiente,
      v_id_solicitud
   FROM 
      ret_solo_infonavit
   WHERE folio = p_folio
   AND   estado_solicitud = 10
      
      -- se invoca SP que reversa la marca de la cuenta consultada
      EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                                           v_marca_entra  ,
                                           v_id_solicitud ,
                                           p_folio        );
   END FOREACH;

   -- se borran los datos de contingente solo infonavit
   DELETE FROM ret_solo_infonavit 
   WHERE folio = p_folio;
   
   -- se actualiza el estatus del archivo cargado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;
   
   -- se le quita el folio al archivo cargado
   UPDATE glo_ctr_archivo
   SET    folio = NULL,
          estado = 1
   WHERE  folio = p_folio;

END PROCEDURE;


