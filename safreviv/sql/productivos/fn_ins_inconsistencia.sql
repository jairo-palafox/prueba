






CREATE FUNCTION "safreviv".fn_ins_inconsistencia(p_id_ocg_solicitud_ug DECIMAL(9,0),
                                      p_inconsistencia      SMALLINT)
RETURNING SMALLINT;

   DEFINE v_resultado               SMALLINT;
   DEFINE v_situacioN               SMALLINT;
   DEFINE v_subproceso              SMALLINT;
   DEFINE v_diag                    SMALLINT;
   DEFINE v_f_proceso               DATE;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      LET v_resultado = 1;

      -- se devuelve el resultado de la operación indicando que ocurrió un error
      RETURN v_resultado;
   END EXCEPTION

   SET DEBUG FILE TO '/safreviv_int/archivos/insInconsConv.trace';
   TRACE ON;

   LET v_resultado  = 0;
   LET v_situacion  = 20;
   LET v_subproceso = 3;
   LET v_diag       = 2;
   LET v_f_proceso  = today;

   UPDATE ocg_solicitud_uso_garantia
      SET situacion   = v_situacion,
          diagnostico = v_diag
    WHERE id_ocg_solicitud_ug = p_id_ocg_solicitud_ug;

   INSERT INTO ocg_inconsistencia
               (id_ocg_referencia,
                subproceso,
                inconsistencia,
                f_proceso)
        VALUES (p_id_ocg_solicitud_ug,
                v_subproceso,
                p_inconsistencia,
                v_f_proceso);

   RETURN v_resultado;

END FUNCTION;


