






CREATE FUNCTION "safreviv".fn_act_ocg_tramite()
RETURNING SMALLINT, SMALLINT, INTEGER

   DEFINE v_ax_diagnostico          CHAR(4);
   DEFINE v_usuario_marca           CHAR(20);
   DEFINE v_ax_ususario             CHAR(20);
   DEFINE v_f_vigencia              DATE;
   DEFINE v_ax_f_vigencia           DATE;
   DEFINE v_ax_f_proceso            DATETIME HOUR TO SECOND;
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_ax_num_credito          DECIMAL(10,0);
   DEFINE v_n_referencia            INTEGER;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_marca                   SMALLINT;
   DEFINE v_proceso_marca           SMALLINT;
   DEFINE v_ax_estado               SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_situacion_fin           SMALLINT;
   DEFINE v_estado_fin              SMALLINT;
   DEFINE v_folio                   DECIMAL(10,0);
   DEFINE v_vencidos                INTEGER;

   -- Variables para la inserción en la tabla cta_marca_ws
   DEFINE v_tpo_originacion         CHAR(2);
   DEFINE v_tpo_credito             SMALLINT;

   -- Variables en caso de error
   DEFINE v_cod_error               SMALLINT;
   DEFINE v_si_error                SMALLINT;

   ON EXCEPTION SET v_cod_error
      LET v_si_error = 1;
      RETURN v_si_error, v_cod_error, v_vencidos;
   END EXCEPTION;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_act_cre_tramite.trace";
   --SET DEBUG FILE TO "/safreviv_int/archivos/fn_act_cre_tramite.trace";
   --TRACE ON;

   -- Inicialización de variables
   LET v_marca           = 206;
   LET v_si_error        = 0;
   LET v_cod_error       = 0;
   LET v_tpo_originacion = "02";
   LET v_tpo_credito     = 2;
   LET v_estado_marca    = 40;
   LET v_ax_ususario     = "infonavit";
   LET v_situacion_fin   = 120;
   LET v_estado_fin      = 40;
   LET v_vencidos        = 0;

   FOREACH
      SELECT ocg.id_derechohabiente,
             ocg.id_ocg_tramite
        INTO v_id_derechohabiente,
             v_id_ocg_tramite
        FROM ocg_tramite ocg
       WHERE ocg.situacion  = 50
         AND ocg.f_vigencia < TODAY
         AND id_ocg_tramite NOT IN(
                SELECT t.id_ocg_tramite
                  FROM ocg_tramite t, ocg_formalizacion f
                 WHERE t.situacion      = 50
                   AND t.f_vigencia     < today
                   AND t.id_ocg_tramite = f.id_ocg_tramite
                   AND f.situacion      >= 50)


      FOREACH
         SELECT sfr.n_referencia,
                sfr.folio,
                sfr.proceso_marca
           INTO v_n_referencia,
                v_folio,
                v_proceso_marca
           FROM sfr_marca_activa sfr
          WHERE sfr.id_derechohabiente = v_id_derechohabiente
            AND sfr.marca              = v_marca
            AND sfr.n_referencia       = v_id_ocg_tramite

         -- Se invoca a la función que desmarca la cuenta
         EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                              v_marca,         -- 206
                                              v_n_referencia,
                                              v_estado_marca,   --40
                                              v_marca,
                                              v_ax_ususario,
                                              v_proceso_marca )
                                              INTO v_ax_cod_error;
      END FOREACH

      -- Se actualiza la situacion de ocg_tramite a 120
      UPDATE ocg_tramite
         SET situacion = v_situacion_fin,
             estado    = v_estado_fin
       WHERE id_ocg_tramite     = v_id_ocg_tramite
         AND id_derechohabiente = v_id_derechohabiente;

      LET v_vencidos = v_vencidos + 1;

   END FOREACH;

   RETURN v_si_error, v_cod_error, v_vencidos;

END FUNCTION;


