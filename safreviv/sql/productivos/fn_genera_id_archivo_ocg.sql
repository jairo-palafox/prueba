






CREATE FUNCTION "safreviv".fn_genera_id_archivo_ocg(p_subproc SMALLINT)
RETURNING DECIMAL(9,0), DECIMAL(9,0)

   DEFINE v_id_ocg_ctr_archivo      DECIMAL(9,0);
   DEFINE v_folio_archivo           DECIMAL(9,0);
   DEFINE v_f_lote                  DATE;
   DEFINE v_id_proceso              SMALLINT;
   DEFINE v_operacion               SMALLINT;
   DEFINE v_nom_archivo             CHAR(40);
   DEFINE v_tot_registros           SMALLINT;
   DEFINE v_tot_sp1                 DECIMAL(10,0);
   DEFINE v_tot_sp2                 DECIMAL(10,0);
   DEFINE v_tot_sp3                 DECIMAL(10,0);
   DEFINE v_tot_sp4                 DECIMAL(10,0);
   DEFINE v_tot_sp5                 DECIMAL(10,0);
   DEFINE v_estado                  SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_op                      SMALLINT;
   DEFINE v_fecha                   CHAR(10);

   LET v_op                 = 1;
   LET v_id_ocg_ctr_archivo = 0;
   LET v_usuario            = "infonavit";
   LET v_f_lote             = TODAY;
   LET v_tot_registros      = 1;
   LET v_tot_sp1            = 0;
   LET v_tot_sp2            = 0;
   LET v_tot_sp3            = 0;
   LET v_tot_sp4            = 0;
   LET v_tot_sp5            = 0;
   LET v_estado             = 20;
   LET v_f_proceso          = TODAY;
   LET v_fecha              = TODAY;

   IF p_subproc = 4 THEN
      LET v_id_proceso  = 3904;
      LET v_nom_archivo = "APSUBSEC_43BIS"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".trx";
      LET v_operacion   = 10;
   ELSE
   END IF

   SELECT r.id_ocg_ctr_archivo, folio_archivo
     INTO v_id_ocg_ctr_archivo, v_folio_archivo
     FROM ocg_ctr_archivo r
    WHERE r.id_proceso = v_id_proceso
      AND r.operacion  = v_operacion
      AND r.f_proceso  = v_f_proceso;

   IF v_id_ocg_ctr_archivo IS NULL OR
      v_id_ocg_ctr_archivo = 0 THEN

      CALL fn_genera_folio (v_id_proceso, v_op, v_usuario)
      RETURNING v_folio_archivo;

      LET v_id_ocg_ctr_archivo = seq_cre_archivo.NEXTVAL;

      INSERT INTO ocg_ctr_archivo (
                  id_ocg_ctr_archivo,
                  folio_archivo,
                  f_lote,
                  id_proceso,
                  operacion,
                  nom_archivo,
                  tot_registros,
                  tot_sp1,
                  tot_sp2,
                  tot_sp3,
                  tot_sp4,
                  tot_sp5,
                  estatus,
                  f_proceso,
                  usuario)
          VALUES (v_id_ocg_ctr_archivo,
                  v_folio_archivo,
                  v_f_lote,
                  v_id_proceso,
                  v_operacion,
                  v_nom_archivo,
                  v_tot_registros,
                  v_tot_sp1,
                  v_tot_sp2,
                  v_tot_sp3,
                  v_tot_sp4,
                  v_tot_sp5,
                  v_estado,
                  v_f_proceso,
                  v_usuario);
   END IF

   RETURN v_id_ocg_ctr_archivo, v_folio_archivo;

END FUNCTION;


