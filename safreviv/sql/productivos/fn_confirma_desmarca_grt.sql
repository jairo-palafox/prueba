






CREATE FUNCTION "safreviv".fn_confirma_desmarca_grt(p_fecha DATE)

   RETURNING SMALLINT

   DEFINE v_error       SMALLINT;
   DEFINE v_id_dh       DECIMAL(9,0);
   DEFINE v_id_ocg_lq   DECIMAL(9,0);
   DEFINE v_id_ocg_dt   DECIMAL(9,0);
   DEFINE v_id_ocg_fz   DECIMAL(9,0);
   DEFINE v_id_ocg_tmt  DECIMAL(9,0);


   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_confirma_desmarca_grt.trace';
   ---TRACE ON;

   -- Inicializa variables
   LET v_error      = 0;
   LET v_id_dh      = NULL;
   LET v_id_ocg_lq  = NULL;
   LET v_id_ocg_dt  = NULL;
   LET v_id_ocg_fz  = NULL;
   LET v_id_ocg_tmt = NULL;

   FOREACH

      SELECT id_derechohabiente
        INTO v_id_dh
        FROM cta_his_marca_ws
       WHERE f_actualiza = p_fecha
         AND situacion = 0
         AND marca_procesar = '02'
         AND diagnostico = '   '

      IF (v_id_dh IS NOT NULL) THEN

         -- Busca en ocg_liquidacion con situación 140 y 150
         IF EXISTS (SELECT id_derechohabiente
                      FROM ocg_liquidacion
                     WHERE id_derechohabiente = v_id_dh
                       AND situacion IN (140,150)) THEN

            -- Recupera ID´S de liquidación
            SELECT id_ocg_liquidacion,
                   id_ocg_detalle,
                   id_ocg_formalizacion,
                   id_ocg_tramite
              INTO v_id_ocg_lq,
                   v_id_ocg_dt,
                   v_id_ocg_fz,
                   v_id_ocg_tmt
              FROM ocg_liquidacion
             WHERE id_derechohabiente = v_id_dh
               AND situacion IN (140,150);

            -- En caso de contar con el trámite 
            IF (v_id_ocg_tmt IS NOT NULL) THEN
               UPDATE ocg_tramite
                  SET situacion = 158
                WHERE id_ocg_tramite     = v_id_ocg_tmt
                  AND id_derechohabiente = v_id_dh;
            END IF

            -- Actualiza en ocg_acreditado y ocg_formalizacion
            IF (v_id_ocg_fz IS NOT NULL) THEN
               UPDATE ocg_acreditado
                  SET situacion = 158,
                      f_conf_desmarca_prcr = p_fecha
                WHERE id_ocg_formalizacion = v_id_ocg_fz;

               UPDATE ocg_formalizacion
                  SET situacion = 158
                WHERE id_ocg_formalizacion = v_id_ocg_fz
                  AND id_derechohabiente   = v_id_dh;
            END IF

            -- Por último actualiza liquidacion
            UPDATE ocg_liquidacion
               SET situacion = 158
             WHERE id_ocg_liquidacion = v_id_ocg_lq
               AND id_derechohabiente = v_id_dh;

         END IF
      END IF

   END FOREACH;

   RETURN v_error;

END FUNCTION;


