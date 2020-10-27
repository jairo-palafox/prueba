






CREATE FUNCTION "safreviv".fn_busca_nss_unificador1(p_nss CHAR(11))
   RETURNING CHAR(11), DECIMAL(9,0)

   DEFINE v_nss_unificador    CHAR(11);
   DEFINE v_id_dh_unificador  DECIMAL(9,0);
   DEFINE v_diag              SMALLINT;

   LET v_diag = 0;

   SELECT a.nss,
          a.id_derechohabiente
     INTO v_nss_unificador,
          v_id_dh_unificador
     FROM afi_derechohabiente a
    WHERE a.nss = p_nss;

   -- Se abre ciclo debido a que en algunos registros el query regresa más de un                                                                                                                                                              resultado
   FOREACH
      SELECT a.nss,
             a.id_derechohabiente
        INTO v_nss_unificador,
             v_id_dh_unificador
        FROM uni_det_unificador u,
             afi_derechohabiente a
       WHERE id_unificador IN (
             SELECT id_unificador
               FROM uni_det_unificado
              WHERE nsscta1 = p_nss )
         AND u.id_derechohabiente = a.id_derechohabiente
         AND u.estado_familia = 1
         AND u.diagnostico >= 5

      EXIT FOREACH;
   END FOREACH;

   IF p_nss <> v_nss_unificador THEN
      LET v_diag = 1;
   END IF

   --y debe regresar el nss e id_derechohabiente del unificador
  RETURN v_nss_unificador, v_id_dh_unificador;

END FUNCTION
;


