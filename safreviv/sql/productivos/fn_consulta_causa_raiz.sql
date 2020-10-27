






CREATE FUNCTION "safreviv".fn_consulta_causa_raiz(p_nss CHAR(11))
RETURNING CHAR(13), CHAR(18), CHAR(40), CHAR(40), CHAR(40)

   DEFINE v_rfc                     CHAR(13);
   DEFINE v_curp                    CHAR(18);
   DEFINE v_primer_ap               CHAR(40);
   DEFINE v_segundo_ap              CHAR(40);
   DEFINE v_nombre                  CHAR(40);

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_verifica_fallecido.trace';
   ---TRACE ON;

   LET v_rfc        = "";
   LET v_curp       = "";
   LET v_primer_ap  = "";
   LET v_segundo_ap = "";
   LET v_nombre     = "";

   IF p_nss = "" THEN
     LET p_nss = NULL;
   END IF

   -- se asume que existe derechohabiente

   IF EXISTS (SELECT nss
                FROM afi_derechohabiente
                WHERE nss = p_nss) THEN

      SELECT rfc, curp, ap_paterno_af, ap_materno_af, nombre_af
        INTO v_rfc, v_curp, v_primer_ap, v_segundo_ap, v_nombre
        FROM afi_derechohabiente
       WHERE nss = p_nss;

   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_rfc, v_curp, v_primer_ap, v_segundo_ap, v_nombre;

END FUNCTION
;


