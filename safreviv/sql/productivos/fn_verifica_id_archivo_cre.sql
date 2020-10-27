






CREATE FUNCTION "safreviv".fn_verifica_id_archivo_cre(p_proceso SMALLINT)
RETURNING DECIMAL(9,0), DECIMAL(9,0)

   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0);
   DEFINE v_folio_archivo           DECIMAL(9,0);
   DEFINE v_lote                    SMALLINT;
   DEFINE v_f_lote                  DATE;
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
   DEFINE v_nom_arh                 CHAR(40);
   DEFINE v_fecha                   CHAR(10);
   DEFINE v_tpo_transferencia       CHAR(2);
   DEFINE v_extension               CHAR(5);

   ---SET DEBUG FILE TO '/safreviv_int/archivos/idCtrArhCre.trace';
   ---TRACE ON;

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
   LET v_fecha              = v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10];

   IF p_proceso = 202 THEN
      LET v_tpo_transferencia  = "15";
   END IF

   IF p_proceso = 1203 OR p_proceso = 1232 THEN
      LET v_tpo_transferencia  = "19";
   END IF

   SELECT operacion,
          nom_archivo,
          extension
     INTO v_operacion,
          v_nom_arh,
          v_extension
     FROM cat_archivo_proceso
    WHERE proceso_cod = p_proceso;

   LET v_nom_archivo = TRIM(v_nom_arh)||TRIM(v_fecha)||"."||TRIM(v_extension);

   CALL fn_genera_folio (p_proceso, v_op, v_usuario)
   RETURNING v_folio_archivo;

   LET v_id_cre_ctr_archivo = seq_cre_archivo.NEXTVAL;

   IF p_proceso <> 1203 AND
      p_proceso <> 1232 AND 
      p_proceso <> 202  THEN
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
                  p_proceso,
                  v_operacion,
                  v_nom_archivo,
                  v_tot_registros,
                  v_tot_aceptados,
                  v_tot_rechazados,
                  v_tot_sin_origen,
                  v_estado,
                  v_f_proceso,
                  v_usuario);
   ELSE
      INSERT INTO dse_ctr_archivo (
                  tpo_transferencia,
                  lote,
                  f_lote,
                  tot_registros,
                  tot_aceptados,
                  tot_rechazados,
                  estado,
                  f_proceso,
                  usuario,
                  folio,
                  nom_archivo)
          VALUES (v_tpo_transferencia,
                  v_lote,
                  v_f_lote,
                  v_tot_registros,
                  v_tot_aceptados,
                  v_tot_rechazados,
                  v_estado,   
                  v_f_proceso,
                  v_usuario, 
                  v_folio_archivo,
                  v_nom_archivo);
   END IF

   RETURN v_id_cre_ctr_archivo, v_folio_archivo;

END FUNCTION
;


