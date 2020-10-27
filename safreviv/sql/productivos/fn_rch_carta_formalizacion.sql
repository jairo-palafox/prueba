






CREATE FUNCTION "safreviv".fn_rch_carta_formalizacion( p_id_ocg_formalizacion DECIMAL(9,0) )
   RETURNING SMALLINT

   DEFINE v_inconsistencia CHAR(2);
   DEFINE v_f_proceso      DATE;
   DEFINE v_id_ocg_tramite DECIMAL(9,0);
   DEFINE v_subproceso     SMALLINT;
   DEFINE v_bnd_ok         SMALLINT;
   DEFINE v_id_ocg_detalle DECIMAL(9,0);

   ON EXCEPTION SET v_bnd_ok
      RETURN v_bnd_ok;
   END EXCEPTION;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_rch_carta_formalizacion.trace';
   --TRACE ON;

   LET v_bnd_ok         = 0;
   LET v_subproceso     = 2;
   LET v_inconsistencia = 42;
   LET v_f_proceso      = TODAY;

   -- Se obtiene el id de tr▒mite
   SELECT id_ocg_tramite
     INTO v_id_ocg_tramite
     FROM ocg_formalizacion
    WHERE id_ocg_formalizacion = p_id_ocg_formalizacion;

   -- Se obtiene el id de detalle
   SELECT id_ocg_detalle
     INTO v_id_ocg_detalle
     FROM ocg_formalizacion
    WHERE id_ocg_formalizacion = p_id_ocg_formalizacion;

   -- Se actaliza la fecha de proceso
   UPDATE ocg_detalle
      SET f_proceso = TODAY
    WHERE id_ocg_detalle = v_id_ocg_detalle;

   -- Se inserta la inconsistencia
   INSERT INTO ocg_inconsistencia VALUES( p_id_ocg_formalizacion,
                                          v_subproceso,
                                          v_inconsistencia,
                                          v_f_proceso
                                          );

   -- Se actualiza la situaci▒n de la formalizaci▒n
   UPDATE ocg_formalizacion
      SET situacion             = 20,
          estado                = 60,
          diagnostico           = 2,
          f_registro_carta      = TODAY
     WHERE id_ocg_formalizacion = p_id_ocg_formalizacion;

   -- Se actualiza la situaci▒n del tr▒mite
   UPDATE ocg_tramite
      SET situacion = 50
    WHERE id_ocg_tramite = v_id_ocg_tramite;

  RETURN v_bnd_ok;
END FUNCTION;


