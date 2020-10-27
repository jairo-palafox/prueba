






CREATE FUNCTION "safreviv".fn_verifica_id_archivo_grt(p_proceso SMALLINT)
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
   DEFINE v_fecha                   CHAR(10);
   DEFINE v_tpo_transferencia       CHAR(2);

   ---SET DEBUG FILE TO '/safreviv_int/archivos/idCtrArh.trace';
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
   LET v_tpo_transferencia  = "19";

   IF p_proceso = 1201 THEN
      LET v_operacion   = 21;
      LET v_nom_archivo = "ORIG_43bis_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".rcg";
   END IF

   IF p_proceso = 1229 THEN
      LET v_operacion   = 21;
      LET v_nom_archivo = "ORIG_43bis_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".rcg"; 
   END IF

   IF p_proceso = 1208 THEN
      LET v_operacion   = 30;
      LET v_nom_archivo = "DESMARCA_43bis_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".dmg";
   END IF

   IF p_proceso = 1230 THEN
      LET v_operacion   = 30;
      LET v_nom_archivo = "LIQ_GTIA_43bis_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".dmg";
   END IF

   IF p_proceso = 1202 THEN
      LET v_operacion   = 18;
      LET v_nom_archivo = "SOL_UG_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".rcu";
   END IF

   IF p_proceso = 1231 THEN
      LET v_operacion   = 18;
      LET v_nom_archivo = "SOL_UG_43bis"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".rcu";
   END IF
   
   IF p_proceso = 1203 THEN
      LET v_nom_archivo = "DEV_SDOS_"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".dcg";
   END IF

   IF p_proceso = 1232 THEN
      LET v_nom_archivo = "DSE_43bis"||v_fecha[4,5]||v_fecha[1,2]||v_fecha[7,10]||".dcg";
   END IF

   CALL fn_genera_folio (p_proceso, v_op, v_usuario)
   RETURNING v_folio_archivo;

   LET v_id_cre_ctr_archivo = seq_cre_archivo.NEXTVAL;

   IF p_proceso <> 1203 AND
      p_proceso <> 1232 THEN
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


