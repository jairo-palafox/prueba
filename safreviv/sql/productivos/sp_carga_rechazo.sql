






CREATE PROCEDURE "safreviv".sp_carga_rechazo(p_folio DECIMAL(9,0))

   DEFINE v_folio   DECIMAL(9,0);
   DEFINE v_ref     DECIMAL(9,0);
   DEFINE v_derecho DECIMAL(9,0);
   DEFINE v_sua     DECIMAL(6,0);
   DEFINE v_pp      CHAR(06);
   DEFINE v_f_pago  DATE;
   DEFINE v_nrp     CHAR(11);
   DEFINE v_cve     CHAR(03);
   DEFINE v_viv     DECIMAL(16,2);
   DEFINE v_amo     DECIMAL(16,2);

 FOREACH
   SELECT folio,
          id_referencia,
          id_derechohabiente,
          folio_sua,
          periodo_pago,
          f_pago,
          nrp,
          cve_ent_receptora,
          imp_ap_pat,
          imp_am_cre
    INTO  v_folio,
          v_ref,
          v_derecho,
          v_sua,
          v_pp,
          v_f_pago,
          v_nrp,
          v_cve,
          v_viv,
          v_amo
    FROM  cta_his_pagos
    WHERE folio = p_folio
    AND   result_operacion = "02"

    EXECUTE PROCEDURE sp_reg_rechazo_acl02(p_folio,v_ref,v_derecho,v_sua,v_pp,v_f_pago,v_nrp,v_cve,v_viv,v_amo); 

 END FOREACH



END PROCEDURE
;


