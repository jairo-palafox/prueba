






CREATE FUNCTION "safreviv".fn_genera_folio_bus(p_id_proceso VARCHAR(3), p_id_operacion VARCHAR(3))
RETURNING VARCHAR(50);

   DEFINE v_folio       VARCHAR(50);
   DEFINE v_cve_afore   CHAR(6);
   DEFINE v_secuencia   DECIMAL(9,0);

   --LET v_cve_afore = '578001';   --Este valor se tiene que consultar de alguna tabla

   --LET v_folio = v_cve_afore || LPAD(p_id_proceso,3,'0') || LPAD(p_id_operacion,4,'0') || REPLACE(TO_CHAR(CURRENT YEAR TO FRACTION(3) , '%Y%m%d%H%M%S%F3'),'.', '' );
   LET v_folio = LPAD(p_id_proceso,3,'0') || LPAD(p_id_operacion,4,'0') || REPLACE(TO_CHAR(CURRENT YEAR TO FRACTION(3) , '%Y%m%d%H%M%S%F3'),'.', '' );
   --LET v_secuencia = seq_bus_folio.NEXTVAL;
   --LET v_folio = TO_CHAR(CURRENT YEAR TO SECOND , '%Y%m%d%H%M%S') || v_cve_afore || LPAD(v_secuencia,6,'0');

   RETURN v_folio;
END FUNCTION;


