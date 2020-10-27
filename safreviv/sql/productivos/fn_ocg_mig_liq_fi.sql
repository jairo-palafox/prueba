






CREATE FUNCTION "safreviv".fn_ocg_mig_liq_fi()
   RETURNING SMALLINT

DEFINE v_f_carga          DATE;
DEFINE v_f_carga2         CHAR(8);
DEFINE v_f_respuesta      DATE;
DEFINE v_f_respuesta2     CHAR(8);
DEFINE v_situacion        CHAR(1);
DEFINE v_situacion_ocg    SMALLINT;
DEFINE v_subproceso       CHAR(3);
DEFINE v_f_envio          DATE;
DEFINE v_f_envio2         CHAR(8);
DEFINE v_ent_financiera   SMALLINT;
DEFINE v_nss              CHAR(11);
DEFINE v_num_ctr_int      CHAR(18);
DEFINE v_diagnostico      CHAR(1);
DEFINE v_bimestre         CHAR(6);
DEFINE v_impt_ap_devuelto DECIMAL(15,2);
DEFINE v_f_liberacion     DATE;
DEFINE v_f_liberacion2    CHAR(8);
DEFINE v_f_proceso        DATE;
DEFINE v_f_proceso2       CHAR(8);
DEFINE v_f_dev_pago       CHAR(8);
DEFINE v_f_dev_pago2      DATE;
DEFINE v_seq_devolucion   INTEGER;
DEFINE v_seq_archivo      INTEGER;
DEFINE v_seq_detalle      INTEGER;
DEFINE v_id_dh            DECIMAL(9,0);
DEFINE v_edo_registro     SMALLINT;
DEFINE v_estado           SMALLINT;
DEFINE v_error            SMALLINT;
DEFINE v_today            DATE;
DEFINE v_tpo_credito      CHAR(1);
DEFINE v_impt_gtia_devuelto DECIMAL(15,2);
DEFINE v_concepto         SMALLINT;
DEFINE v_seq_transaccion  INTEGER;
DEFINE v_bnd_1            SMALLINT;
DEFINE v_bnd_2            SMALLINT;
DEFINE v_bnd_3            SMALLINT;
DEFINE v_bnd_4            SMALLINT;
DEFINE v_bnd_5            SMALLINT;
DEFINE v_impt_gtia_devuelto2 decimal(15,2);
DEFINE v_impt_ap_devuelto2   decimal(15,2);
DEFINE v_causa_liquida    CHAR(2);
DEFINE v_f_vigencia       DATE;
DEFINE v_foja             DATE;
DEFINE v_seq_liquidacion  DECIMAL(9,0);
DEFINE v_importe          DECIMAL(15,2);

ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_mig_liq_fi.trace';
   TRACE ON;

   LET v_error      = 0;
   LET v_f_proceso  = today;
   LET v_f_proceso  = "12/13/2016";
   LET v_foja       = NULL;

   FOREACH
      SELECT f_carga,
             f_respuesta,
             situacion,
             subproceso,
             f_envio,
             ent_financiera,
             nss,
             num_ctr_int,
             diagnostico,
             bimestre,
             impt_ap_devuelto,
             impt_gtia_devuelto,
             f_liberacion,
             f_proceso,
             f_dev_pago,
             tpo_credito,
             causa_liquida,
             f_vigencia
        INTO v_f_carga2,
             v_f_respuesta2,
             v_situacion,
             v_subproceso,
             v_f_envio2,
             v_ent_financiera,
             v_nss,
             v_num_ctr_int,
             v_diagnostico,
             v_bimestre,
             v_impt_ap_devuelto,
             v_impt_gtia_devuelto,
             v_f_liberacion2,
             v_f_proceso2,
             v_f_dev_pago,
             v_tpo_credito,
             v_causa_liquida,
             v_f_vigencia
        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 5
         AND situacion in ("F","I")
         AND nss <> "51917110291"

      LET v_bnd_1 = 0;
      LET v_bnd_2 = 0;
      LET v_bnd_3 = 0;
      LET v_bnd_4 = 0;
      LET v_bnd_5 = 0;

      IF (v_tpo_credito is null) OR
         (v_tpo_credito = ' ' )  THEN
         LET v_tpo_credito = "A";
      END IF

      IF (v_f_carga2[1,2] <= "12") AND
         (v_f_carga2[1,2] >= "01")  AND
         (v_f_carga2[3,4] <= "31") AND
         (v_f_carga2[3,4] >= "01")  THEN
         LET v_f_carga = v_f_carga2;
         LET v_bnd_1 = 1;
      END IF

      IF (v_f_respuesta2[1,2] <= "12") AND
         (v_f_respuesta2[1,2] >= "01")  AND
         (v_f_respuesta2[3,4] <= "31") AND
         (v_f_respuesta2[3,4] >= "01")  THEN
         LET v_f_respuesta = v_f_respuesta2;
         LET v_bnd_2 = 1;
      END IF

      IF (v_f_envio2[1,2] <= "12") AND
         (v_f_envio2[1,2] >= "01")  AND
         (v_f_envio2[3,4] <= "31") AND
         (v_f_envio2[3,4] >= "01")  THEN
         LET v_f_envio = v_f_envio2;
         LET v_bnd_3 = 1;
      END IF
{
      IF (v_f_liberacion2[1,2] <= "12") AND
         (v_f_liberacion2[1,2] >= "01")  AND
         (v_f_liberacion2[3,4] <= "31") AND
         (v_f_liberacion2[3,4] >= "01")  THEN
         LET v_f_liberacion = v_f_liberacion2;
         LET v_bnd_4 = 1;
      END IF
}
 LET v_bnd_4 = 1;

      IF (v_f_proceso2[1,2] <= "12") AND
         (v_f_proceso2[1,2] >= "01")  AND
         (v_f_proceso2[3,4] <= "31") AND
         (v_f_proceso2[3,4] >= "01")  THEN
         LET v_f_proceso = v_f_proceso2;
         LET v_bnd_5 = 1;
      END IF

      IF v_bnd_1 = 1 AND
         v_bnd_2 = 1 AND
         v_bnd_3 = 1 AND
         v_bnd_4 = 1 AND
         v_bnd_5 = 1 THEN

      LET v_f_dev_pago2 = v_f_dev_pago[5,6]||v_f_dev_pago[7,8]||v_f_dev_pago[1,4];

      SELECT id_derechohabiente
        INTO v_id_dh
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      LET v_seq_detalle     = seq_ocg_detalle.NEXTVAL;
      LET v_seq_archivo     = seq_ocg_archivo.NEXTVAL;
      LET v_seq_devolucion  = seq_ocg_devolucion.NEXTVAL;
      LET v_seq_liquidacion = seq_ocg_liquidacion.NEXTVAL; 
      LET v_estado          = 30;
    --  LET v_situacion       = 70;
      LET v_concepto = 0;

      IF v_situacion = "F" THEN
         LET v_edo_registro  = 190;
         LET v_situacion_ocg = 110;
      END IF

      IF v_situacion = "I" THEN
         LET v_edo_registro  = 190;
         LET v_situacion_ocg = 110;
      END IF

      LET v_importe = null ;

      IF --(v_impt_gtia_devuelto < 0) OR
         --(v_impt_gtia_devuelto IS NULL) AND
         (v_impt_ap_devuelto > 0) THEN
         LET v_concepto = 508;
         LET v_impt_gtia_devuelto = NULL;
         LET v_importe = v_impt_ap_devuelto;
      END IF

      IF (v_impt_gtia_devuelto > 0) THEN --AND
         --(v_impt_ap_devuelto < 0) OR
         --(v_impt_ap_devuelto IS NULL) THEN
         LET v_concepto = 608;
         LET v_impt_ap_devuelto = NULL;
         LET v_importe = v_impt_gtia_devuelto;
      END IF

      IF v_concepto not in (508,608) THEN
         let v_concepto = 700;
      END IF

      INSERT INTO ocg_detalle
           VALUES(v_seq_detalle,
                  v_seq_archivo,
                  v_id_dh,
                  v_subproceso,
                  v_f_proceso,
                  v_ent_financiera,
                  v_nss);

      INSERT INTO ocg_fecha_mig
          VALUES (v_seq_liquidacion,
                  v_seq_detalle,
                  v_id_dh,
                  v_f_envio,
                  v_f_carga,
                  v_f_respuesta,
                  v_foja,
                  5,
                  v_f_proceso);

      INSERT INTO ocg_liquidacion
           VALUES (v_seq_liquidacion,
                   v_seq_detalle,
                   "",
                   "",
                   v_id_dh,
                   v_ent_financiera,
                   v_num_ctr_int, 
                   v_bimestre,
                   v_impt_ap_devuelto,
                   '', --v_f_liberacion,
                   v_impt_gtia_devuelto,
                   v_causa_liquida,
                   v_f_dev_pago2,
                   v_tpo_credito,
                   1,
                   30,
                   160);
                  
      INSERT INTO ocg_devolucion
           VALUES(v_seq_devolucion,
                  v_seq_detalle,
                  '',
                  '',
                  v_id_dh,
                  v_ent_financiera,
                  v_num_ctr_int,
                  v_bimestre,
                  v_impt_ap_devuelto,
                  v_impt_gtia_devuelto,
                  0,
                  v_f_dev_pago2,
                  v_tpo_credito,
                  v_diagnostico,
                  v_estado,
                  v_situacion_ocg,
                  v_edo_registro,
                  v_f_proceso);

         LET v_seq_transaccion = seq_ocg_trx_pago.NEXTVAL;

         INSERT INTO ocg_ctr_transaccion
             VALUES (v_seq_transaccion,
                     '', --id_ocg_formalizacion
                     v_seq_detalle,
                     v_id_dh,
                     '', --id_referencia_cta
                     '', --folio_referencia
                     3992,
                     v_ent_financiera,
                     v_num_ctr_int,
                     '', --folio
                     v_f_dev_pago2, --f_transaccion
                     v_nss,
                     '', --curp
                     v_importe,
                     v_bimestre,-- periodo
                     v_f_respuesta,
                     v_concepto,
                     today,
                     80);
      ELSE

         INSERT INTO tmp_migra_fi
              VALUES(v_f_carga2,
                     v_f_respuesta2,
                     v_situacion,
                     v_subproceso,
                     v_f_envio2,
                     v_ent_financiera,
                     v_nss,
                     v_num_ctr_int,
                     v_diagnostico,
                     v_bimestre,
                     v_impt_ap_devuelto,
                     v_impt_gtia_devuelto,
                     v_f_liberacion2,
                     v_f_proceso2,
                     v_f_dev_pago,
                     v_tpo_credito);

         CONTINUE FOREACH;
      END IF
                  
   END FOREACH

 RETURN v_error;
END FUNCTION;


