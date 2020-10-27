






CREATE FUNCTION "safreviv".fn_act_liq_cofi()

   RETURNING SMALLINT
   
   DEFINE v_id_ocg_referencia  DECIMAL(9,0);
   DEFINE v_id_ocg_tramite     DECIMAL(9,0);
   DEFINE v_f_liquida_cofi     DATE;
   DEFINE v_error              SMALLINT;
   
   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;
   
   --SET DEBUG FILE TO '/safreviv_int/BD/fn_act_liq_cofi.trace';
   --TRACE ON;
   
   LET v_id_ocg_referencia = NULL;
   LET v_id_ocg_tramite    = NULL;
   LET v_f_liquida_cofi    = NULL;
   LET v_error             = 0;
 
   FOREACH
      -- Recupera registros con f_liquida_cofi nulas
      SELECT mg.id_ocg_referencia
        INTO v_id_ocg_referencia
        FROM ocg_fecha_mig mg,
             ocg_formalizacion fz
       WHERE mg.id_ocg_referencia = fz.id_ocg_formalizacion
         AND mg.subproceso = 2
         AND mg.f_liquida_cofi IS NULL
         AND fz.id_ocg_tramite IS NOT NULL

      -- Recupera el trámite del sp002
      SELECT id_ocg_tramite
        INTO v_id_ocg_tramite
        FROM ocg_formalizacion
        WHERE id_ocg_formalizacion = v_id_ocg_referencia;

      -- Recupera f_liquida_cofi
      IF EXISTS (SELECT id_ocg_referencia
                   FROM ocg_fecha_mig
                  WHERE id_ocg_referencia = v_id_ocg_tramite
                    AND subproceso = 1
                    AND f_liquida_cofi IS NOT NULL) THEN 

         SELECT f_liquida_cofi
           INTO v_f_liquida_cofi
           FROM ocg_fecha_mig
          WHERE id_ocg_referencia = v_id_ocg_tramite
            AND subproceso = 1;

         -- Actualiza f_liquida_cofi para sp002
         UPDATE ocg_fecha_mig
            SET f_liquida_cofi = v_f_liquida_cofi
          WHERE id_ocg_referencia = v_id_ocg_referencia
            AND subproceso = 2;
      END IF    
         
   END FOREACH

   RETURN v_error;
END FUNCTION
;


