






CREATE FUNCTION "safreviv".fn_grt_insrt_dsectrarch_devsdos(p_c_nom_archivo CHAR(40),
                                                p_d_pid         DECIMAL(9,0),
                                                p_v_usuario     CHAR(20),
                                                p_i_estado      SMALLINT)
   RETURNING SMALLINT, SMALLINT, INTEGER
   -- Registro de dse ctr archivo
   DEFINE v_ra_tpo_transferencia SMALLINT; -- operacion del proceso (campo registro)
   DEFINE v_ra_lote              SMALLINT; -- lote (campo registro)
   DEFINE v_ra_f_lote            DATE; -- fecha de lote (campo registro)
   DEFINE v_ra_tot_registros     DECIMAL(10,0); -- total de registro insertados (campo registro)
   DEFINE v_ra_tot_aceptados     DECIMAL(10,0); -- total de registro aceptados (campo registro)
   DEFINE v_ra_tot_rechazados    DECIMAL(10,0); -- total de registro rechazados (campo registro)
   DEFINE v_ra_estado            SMALLINT; -- estado de la insercion (campo registro)
   DEFINE v_ra_f_proceso         DATE; -- fecha de proceso (campo registro)
   DEFINE v_ra_usuario           CHAR(20); -- usuario (campo registro)
   DEFINE v_ra_folio             DECIMAL(9);
   DEFINE v_ra_nom_archivo       CHAR(40);
   -- Variables auxiliares
   DEFINE v_ax_lote              SMALLINT; -- lote
   DEFINE v_ax_f_lote            DATE; -- fecha de lote auxiliar
   DEFINE v_ax_tot_registros     INTEGER; -- total de registros insertados
   DEFINE v_ax_cuenta_reg        INTEGER; -- contador de registros
   DEFINE v_ax_nss               CHAR(11);
   DEFINE v_ax_id_operacion      CHAR(2); -- identificador de operacion
   DEFINE v_ax_operacion         SMALLINT; -- operacion del proceso
   DEFINE v_b_estatus_proc       SMALLINT; -- contiene el estatus a regresar segun el proceso
   DEFINE v_ax_error             SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_cnt_nss              SMALLINT;

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtDseCtrArch_dse_grt.trace';
   --TRACE ON;

   -- se asigna la operacion del proceso (19 - Devolucion De Saldos 43bis)
   LET v_ax_operacion = 19;
   LET v_ax_error = 0;
   LET v_b_estatus_proc = 0;
   LET v_ax_tot_registros = 0;
   LET v_ax_f_lote = NULL;
   LET v_ax_lote = NULL;

   -- se busca numero de lote correspondiente al archivo
   SELECT f_presentacion, consecutivo_lote
     INTO v_ax_f_lote, v_ax_lote
     FROM safre_tmp:tmp_dse_devol_enc_grt;

   -- si no se encuentra la fecha de lote en la sentencia se asigna la fecha de hoy
   IF v_ax_f_lote IS NULL THEN
      LET v_ax_f_lote = TODAY;
   END IF;

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_ax_lote IS NULL THEN
      LET v_ax_lote = 1;
   END IF;

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
      SELECT COUNT(*)
        INTO v_ax_tot_registros
        FROM safre_tmp:tmp_dse_devol_det_grt;

      -- se valida el total de registros insertados en detalle
      IF v_ax_tot_registros = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca como rechazado el archivo ya que éste debe contener al menos un registro detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del encabezado
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_dse_devol_enc_grt;

      -- se verifica el total de registros insertados como encabezado
      IF v_ax_cuenta_reg <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN ENCABEZADO O CON MÁS DE UNO");
         -- se marca como rechazado el archivo ya que éste debe contener un encabezado
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 3;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_dse_devol_sum_grt;

      -- se verifica el total de registros insertados como sumario
      IF v_ax_cuenta_reg <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN SUMARIO O CON MÁS DE UNO");
         -- se marca como rechazado el archivo ya que éste debe contener un sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del id operacion
   IF p_i_estado = 10 THEN
      SELECT id_operacion
        INTO v_ax_id_operacion
        FROM safre_tmp:tmp_dse_devol_enc_grt;

      -- se verifica el identificador de operacion
      IF v_ax_id_operacion <> "15" THEN
         --TRACE("ERROR: OPERACIÓN EN ARCHIVO INVALIDA");
         -- se marca como rechazado el archivo ya que el id operacion debe ser '15'
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 6;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del total en sumario
   IF p_i_estado = 10 THEN
      SELECT cant_registros
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_dse_devol_sum_grt;

      -- se verifica el identificador de operacion
      IF v_ax_cuenta_reg <> v_ax_tot_registros THEN
         --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 4;
      END IF
   END IF

      -- en caso de ser valido el estado se hace la validación del NSS
   IF p_i_estado = 10 THEN
            -- se busca el nss obtenido en el catálogo
      INSERT INTO safre_tmp:tmp_nss_no_catalogados_grt
      SELECT nss,
             saldo_vivienda97
        FROM safre_tmp:tmp_dse_devol_det_grt
       WHERE nss NOT IN (
         SELECT nss
           FROM afi_derechohabiente)
         OR nss = "00000000000";

      SELECT COUNT(*)
        INTO v_cnt_nss
        FROM safre_tmp:tmp_nss_no_catalogados_grt;

      DELETE
        FROM safre_tmp:tmp_dse_devol_det_grt
       WHERE nss IN(
         SELECT nss
           FROM safre_tmp:tmp_nss_no_catalogados_grt);

      IF v_cnt_nss >= 0 THEN
         --LET p_i_estado = 30;
         LET v_b_estatus_proc = 5;
      END IF
   END IF;

   -- se asignan los valores a las variables del registro a insertar
   LET v_ra_tpo_transferencia = v_ax_operacion;
   LET v_ra_lote = v_ax_lote;
   LET v_ra_f_lote = v_ax_f_lote;
   LET v_ra_tot_registros = v_ax_tot_registros;
   LET v_ra_tot_aceptados = 0;
   LET v_ra_tot_rechazados = 0;
   LET v_ra_estado = p_i_estado;
   LET v_ra_f_proceso = TODAY;
   LET v_ra_usuario = p_v_usuario;
   LET v_ra_folio = 0;
   LET v_ra_nom_archivo = p_c_nom_archivo;

   -- se inserta el registro en la tabla de control
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
       VALUES (v_ra_tpo_transferencia,
               v_ra_lote,
               v_ra_f_lote,
               v_ra_tot_registros,
               v_ra_tot_aceptados,
               v_ra_tot_rechazados,
               v_ra_estado,
               v_ra_f_proceso,
               v_ra_usuario,
               v_ra_folio,
               v_ra_nom_archivo);

   RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


