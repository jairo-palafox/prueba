






CREATE FUNCTION "safreviv".fn_agr_saldos_remanentes(p_v_usuario CHAR(20),
                                         p_i_tpo_originacion SMALLINT)
   RETURNING SMALLINT
   -- registro de cre acreditado
   DEFINE v_cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE v_cre_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE v_cre_id_derechohabiente   DECIMAL(9,0);
   -- registro de cre his acreditado
   DEFINE v_his_id_cre_acreditado    DECIMAL(9,0);
   DEFINE v_his_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE v_his_tpo_transferencia    CHAR(2);
   DEFINE v_his_edo_procesar         SMALLINT;
   DEFINE v_his_diagnostico          CHAR(3);
   DEFINE v_his_estado               SMALLINT;
   DEFINE v_his_nss_afore            CHAR(11);
   DEFINE v_his_rfc_afore            CHAR(13);
   DEFINE v_his_paterno_afore        CHAR(40);
   DEFINE v_his_materno_afore        CHAR(40);
   DEFINE v_his_nombre_afore         CHAR(40);
   DEFINE v_his_nom_imss             CHAR(50);
   DEFINE v_his_f_proceso            DATE;
   -- variables auxiliares
   DEFINE v_ax_monto_acciones        DECIMAL(28,2);
   DEFINE v_error                    SMALLINT; -- codigo de error en caso de excepción
   DEFINE v_edo_procesar             SMALLINT;

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrSaldoReman.trace';
   --TRACE ON;

   -- se inicializa las variables
   LET v_error = 0;

   -- se obtienen los registros de cre acreditado con estado Liquidado (140) y para
   -- las subcuentas correspondientes a vivienda 92 (8, 42)
   FOREACH
      SELECT a.id_cre_acreditado,
             a.id_cre_ctr_archivo,
             a.id_derechohabiente,
             a.edo_procesar,
             SUM(b.monto_acciones)
        INTO v_cre_id_cre_acreditado,
             v_cre_id_cre_ctr_archivo,
             v_cre_id_derechohabiente,
             v_edo_procesar,
             v_ax_monto_acciones
        FROM safre_viv:cre_acreditado a, safre_viv:cta_movimiento b
       WHERE a.estado IN (140,900)
         AND a.id_derechohabiente = b.id_derechohabiente
         AND a.tpo_originacion = p_i_tpo_originacion
         AND a.edo_procesar IN (120,5)
         AND b.subcuenta IN (8,42)
       GROUP BY 1,2,3,4
      HAVING SUM(b.monto_acciones) > 0

      IF v_edo_procesar = 5 THEN
         LET v_his_edo_procesar = v_edo_procesar;
      ELSE
         LET v_his_edo_procesar = 70;
      END IF

      -- se asignan los valores del registro a insertar
      LET v_his_id_cre_acreditado  = v_cre_id_cre_acreditado;
      LET v_his_id_cre_ctr_archivo = v_cre_id_cre_ctr_archivo;
      LET v_his_tpo_transferencia  = NULL;
      LET v_his_diagnostico        = NULL;
      LET v_his_estado             = 25;
      LET v_his_nss_afore          = NULL;
      LET v_his_rfc_afore          = NULL;
      LET v_his_paterno_afore      = NULL;
      LET v_his_materno_afore      = NULL;
      LET v_his_nombre_afore       = NULL;
      LET v_his_nom_imss           = NULL;
      LET v_his_f_proceso          = TODAY;

      -- se inserta en la tabla cre his acreditado
      INSERT INTO safre_viv:cre_his_acreditado(
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  tpo_transferencia,
                  edo_procesar,
                  diagnostico,
                  estado,
                  nss_afore,
                  rfc_afore,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  f_proceso)
          VALUES (v_his_id_cre_acreditado,
                  v_his_id_cre_ctr_archivo,
                  v_his_tpo_transferencia,
                  v_his_edo_procesar,
                  v_his_diagnostico,
                  v_his_estado,
                  v_his_nss_afore,
                  v_his_rfc_afore,
                  v_his_paterno_afore,
                  v_his_materno_afore,
                  v_his_nombre_afore,
                  v_his_nom_imss,
                  v_his_f_proceso);

      -- se actualiza el registro leído de cre acreditado a estado 70-Por Reenviar
      IF v_edo_procesar = 5 THEN
         -- se actualiza unicamente el estado a 25
         UPDATE safre_viv:cre_acreditado
            SET estado = 25
          WHERE id_cre_acreditado = v_cre_id_cre_acreditado
            AND estado IN(140,900)
            AND tpo_originacion = p_i_tpo_originacion
            AND edo_procesar = 5;
      ELSE
         -- se actualiza estado a 25 y estado procesar a 70
         UPDATE safre_viv:cre_acreditado
            SET edo_procesar = 70,
                estado = 25
          WHERE id_cre_acreditado = v_cre_id_cre_acreditado
            AND estado IN(140,900)
            AND tpo_originacion = p_i_tpo_originacion
            AND edo_procesar = 120;
      END IF
   END FOREACH

   RETURN v_error;
END FUNCTION
;


