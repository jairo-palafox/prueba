






CREATE FUNCTION "safreviv".fn_verifica_id_archivo()
RETURNING DECIMAL(9,0), DECIMAL(9,0)

   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0);
   DEFINE v_folio_archivo           DECIMAL(9,0);
   DEFINE v_lote                    SMALLINT;
   DEFINE v_f_lote                  DATE;
   DEFINE v_id_proceso              SMALLINT;
   DEFINE v_operacion               SMALLINT;
   DEFINE v_nom_archivo             CHAR(40);
   DEFINE v_tot_registros           SMALLINT;
   DEFINE v_tot_aceptados           SMALLINT;
   DEFINE v_tot_rechazados          SMALLINT;
   DEFINE v_tot_sin_origen          SMALLINT;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_op                      SMALLINT;
   DEFINE v_fecha                   CHAR(10);

   LET v_id_proceso         = 301;
   LET v_op                 = 1;
   LET v_id_cre_ctr_archivo = 0;
   LET v_usuario            = "infonavit";
   LET v_lote               = 1;
   LET v_f_lote             = TODAY;
   LET v_operacion          = 10;
   LET v_tot_registros      = 1;
   LET v_tot_aceptados      = 1;
   LET v_tot_rechazados     = 0;
   LET v_tot_sin_origen     = 0;
   LET v_estado             = 20;
   LET v_f_proceso          = TODAY;
   LET v_fecha              = TODAY;
   LET v_nom_archivo        = "ORIGINA_MF_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".rca";

   SELECT r.id_cre_ctr_archivo, folio_archivo
     INTO v_id_cre_ctr_archivo, v_folio_archivo
     FROM cre_ctr_archivo r
    WHERE r.id_proceso = v_id_proceso
      AND r.operacion  = v_operacion
      AND r.f_proceso  = v_f_proceso;

   IF v_id_cre_ctr_archivo IS NULL OR
      v_id_cre_ctr_archivo = 0 THEN

      CALL fn_genera_folio (v_id_proceso, v_op, v_usuario)
      RETURNING v_folio_archivo;

      LET v_id_cre_ctr_archivo = seq_cre_archivo.NEXTVAL;

      INSERT INTO cre_ctr_archivo (
                  id_cre_ctr_archivo,
                  folio_archivo,
                  lote,
                  f_lote,
                  id_proceso,
                  operacion,
                  nom_archivo,
                  tot_registros,
                  tot_aceptados,
                  tot_rechazados,
                  tot_sin_origen,
                  estado,
                  f_proceso,
                  usuario)
          VALUES (v_id_cre_ctr_archivo,
                  v_folio_archivo,
                  v_lote,
                  v_f_lote,
                  v_id_proceso,
                  v_operacion,
                  v_nom_archivo,
                  v_tot_registros,
                  v_tot_aceptados,
                  v_tot_rechazados,
                  v_tot_sin_origen,
                  v_estado,
                  v_f_proceso,
                  v_usuario);
   END IF

   RETURN v_id_cre_ctr_archivo, v_folio_archivo;

END FUNCTION;


