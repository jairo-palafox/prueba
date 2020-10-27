






CREATE FUNCTION "safreviv".fn_act_diag_tramite(p_f_diag DATE)
   RETURNING SMALLINT

   DEFINE v_id_derechohabiente             DECIMAL(9,0);
   DEFINE v_diagnostico                    CHAR(4);
   DEFINE v_result                         SMALLINT;
   DEFINE v_diag_prc                       CHAR(3);
   DEFINE v_cod_error                      SMALLINT;
   DEFINE v_id_cre_tramite                 DECIMAL(9,0);
   DEFINE v_id_origen                      DECIMAL(9,0); 

   ON EXCEPTION SET v_cod_error
      LET v_result = 1;
      RETURN v_result;
   END EXCEPTION;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/act_diag_tramite.trace";
   --SET DEBUG FILE TO "/safreviv_int/archivos/act_diag_tramite.trace";
   --TRACE ON;

   -- Inicialización de variables
   LET v_result = 0;

   FOREACH
      SELECT c.id_derechohabiente,
             c.diagnostico,
             c.id_origen
        INTO v_id_derechohabiente,
             v_diag_prc,
             v_id_origen 
        FROM cta_his_marca_ws c
       WHERE c.f_actualiza = p_f_diag
         AND c.situacion   = 2
         AND c.marca       = 234

      LET v_diagnostico = "3"||v_diag_prc;

      IF v_diagnostico = "3   " THEN
         LET v_diagnostico = "3000";

         IF EXISTS (SELECT id_derechohabiente
                     FROM cta_sol_marca_cambio_afore
                    WHERE id_origen = v_id_origen
                      AND estado    = 10) THEN
           
            UPDATE cre_acreditado
               SET edo_procesar = 60
             WHERE id_cre_acreditado = v_id_origen;

            UPDATE cta_sol_marca_cambio_afore
               SET estado = 20
             WHERE id_origen = v_id_origen;

         END IF
      END IF

      IF v_diagnostico = "3528" OR v_diagnostico = "3960" THEN
         LET v_diagnostico = "3000";

         --inserta en tabla de solicitud de marca para registros con cambio de afore
         INSERT INTO cta_sol_marca_cambio_afore(
                            id_derechohabiente,
                            id_origen,
                            diagnostico,
                            f_respuesta,
                            estado)
                     VALUES(v_id_derechohabiente,
                            v_id_origen,
                            v_diagnostico,
                            today,
                            10); 
      END IF

      IF v_diagnostico = "3049" THEN
         LET v_diagnostico = "3000";
      END IF

      IF EXISTS (  ---Se verifica si está ya inciado el trámite
                 SELECT id_cre_tramite
                   FROM cre_tramite
                  WHERE id_derechohabiente = v_id_derechohabiente
                    AND estado             = 18) THEN

         SELECT id_cre_tramite
           INTO v_id_cre_tramite
           FROM cre_tramite
          WHERE id_derechohabiente = v_id_derechohabiente
            AND estado             = 18;

         UPDATE cre_his_tramite
            SET diagnostico    = v_diagnostico
          WHERE id_cre_tramite = v_id_cre_tramite
            AND estado         = 18;
      END IF
   END FOREACH;

   RETURN v_result;

END FUNCTION;


