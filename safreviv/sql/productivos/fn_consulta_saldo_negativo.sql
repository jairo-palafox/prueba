






CREATE FUNCTION "safreviv".fn_consulta_saldo_negativo()
RETURNING SMALLINT;
DEFINE v_nss                CHAR (11);
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_sdo_pesos          DECIMAL(12,2);
DEFINE v_sdo_aivs           DECIMAL(12,6);
DEFINE v_sdo_res            SMALLINT;
DEFINE v_resultado          SMALLINT;

FOREACH
   SELECT nss,
          id_derechohabiente
   INTO   v_nss,
          v_id_derechohabiente
   FROM   dae_det_solicitud
   GROUP BY 1,2

   EXECUTE FUNCTION fn_saldo_dia(v_nss, v_id_derechohabiente,46,TODAY)
   INTO v_sdo_res, v_sdo_aivs, v_sdo_pesos;

   IF v_sdo_aivs < 0 THEN
      INSERT INTO safre_tmp:tmp_dae_id_dh(id_derechohabiente, nss, monto_pesos, monto_acciones)
      VALUES (v_id_derechohabiente, v_nss,v_sdo_pesos, v_sdo_aivs);
   END IF
   LET v_resultado= 1;
END FOREACH

RETURN V_resultado ;
END FUNCTION;


