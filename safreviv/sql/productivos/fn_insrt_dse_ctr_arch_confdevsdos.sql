






CREATE FUNCTION "safreviv".fn_insrt_dse_ctr_arch_confdevsdos(p_c_nom_archivo CHAR(40),
                                                  p_d_pid DECIMAL(9,0),
                                                  p_v_usuario CHAR(20),
                                                  p_i_estado SMALLINT)
   RETURNING SMALLINT, INTEGER
   -- Registro de dse ctr archivo
   DEFINE ctr_tpo_transferencia CHAR(2); -- tipo de transferencia
   DEFINE ctr_lote              SMALLINT; -- lote
   DEFINE ctr_f_lote            DATE; -- fecha de lote
   DEFINE ctr_tot_registros     DECIMAL(10,0); -- total de registro insertados
   DEFINE ctr_tot_aceptados     DECIMAL(10,0); -- total de registro aceptados
   DEFINE ctr_tot_rechazados    DECIMAL(10,0); -- total de registro rechazados
   DEFINE ctr_estado            SMALLINT; -- estado de la insercion
   DEFINE ctr_f_proceso         DATE; -- fecha de proceso
   DEFINE ctr_usuario           CHAR(20); -- usuario
   DEFINE ctr_folio             DECIMAL(9);
   DEFINE ctr_nom_archivo       CHAR(40);
   -- Variables auxiliares
   DEFINE v_ax_lote           SMALLINT; -- lote
   DEFINE v_ax_f_lote         DATE; -- fecha de lote auxiliar
   DEFINE v_ax_tot_registros  INTEGER; -- total de registros insertados
   DEFINE v_ax_cuenta_reg     INTEGER; -- contador de registros
   DEFINE v_ax_id_operacion   CHAR(2); -- identificador de operacion
   DEFINE v_ax_nss            CHAR(11);
   DEFINE v_ax_tpo_transf     CHAR(2); -- tipo de transferencia
   DEFINE v_b_estatus_proc    SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO 'dbg_dseCtrArch_confDSE.log';

   -- se inicializan variables
   LET v_ax_tpo_transf = "59";
   LET v_b_estatus_proc = 0;

   -- se busca numero de lote correspondiente al archivo
   SELECT f_presentacion, consecutivo_lote
     INTO v_ax_f_lote, v_ax_lote
     FROM safre_tmp:tmp_confirmacion_saldos_enc;

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_ax_lote IS NULL THEN
      LET v_ax_lote = 1;
   END IF;

   -- si no se encuentra la fecha de lote en la sentencia se asigna la fecha de hoy
   IF v_ax_f_lote IS NULL THEN
      LET v_ax_f_lote = TODAY;
   END IF;

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
      SELECT COUNT(*)
        INTO v_ax_tot_registros
        FROM safre_tmp:tmp_confirmacion_saldos;

      -- se valida el total de registros insertados en detalle
      IF v_ax_tot_registros = 0 THEN
         -- se marca como rechazado el archivo ya que éste debe contener al menos un registro detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- en caso de ser valido el estado se hace una ultima validacion
   IF p_i_estado = 10 THEN
      FOREACH
      -- se leen todos los nss unicos de la tabla temporal
      SELECT UNIQUE nss
        INTO v_ax_nss
        FROM safre_tmp:tmp_confirmacion_saldos
   
         -- se buscar el nss obtenido en la tabla maestro
         IF NOT EXISTS (
         SELECT nss
           FROM safre_viv:afi_derechohabiente
          WHERE nss = v_ax_nss) THEN
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 5;
            EXIT FOREACH;
         END IF;
      END FOREACH;
   END IF;

   -- en caso de ser valido el estado se hace la validacion del encabezado
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_confirmacion_saldos_enc;

      -- se verifica el total de registros insertados como encabezado
      IF v_ax_cuenta_reg <> 1 THEN
         -- se marca como rechazado el archivo ya que éste debe contener un encabezado
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 3;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_confirmacion_saldos_sum;

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
        FROM safre_tmp:tmp_confirmacion_saldos_enc;

      -- se verifica el identificador de operacion
      IF v_ax_id_operacion <> "15" THEN
         -- se marca como rechazado el archivo ya que el id operacion debe ser '15'
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 6;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del total en sumario
   IF p_i_estado = 10 THEN
      SELECT cant_registros
        INTO v_ax_cuenta_reg
        FROM safre_tmp:tmp_confirmacion_saldos_sum;

      -- se verifica el identificador de operacion
      IF v_ax_cuenta_reg <> v_ax_tot_registros THEN
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 4;
      END IF
   END IF

   -- se asignan los valores a las variables del registro a insertar
   --LET v_ra_pid = p_d_pid;
   LET ctr_tpo_transferencia = v_ax_tpo_transf;
   LET ctr_lote = v_ax_lote;
   LET ctr_f_lote = v_ax_f_lote;
   LET ctr_tot_registros = v_ax_tot_registros;
   LET ctr_tot_aceptados = 0;
   LET ctr_tot_rechazados = 0;
   LET ctr_estado = p_i_estado;
   LET ctr_f_proceso = TODAY;
   LET ctr_usuario = p_v_usuario;
   LET ctr_folio = 0;
   LET ctr_nom_archivo = p_c_nom_archivo;

   -- se inserta el registro en la tabla de control
   INSERT INTO safre_viv:dse_ctr_archivo (
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
       VALUES (ctr_tpo_transferencia,
               ctr_lote,
               ctr_f_lote,               
               ctr_tot_registros,
               ctr_tot_aceptados,
               ctr_tot_rechazados,
               ctr_estado,
               ctr_f_proceso,
               ctr_usuario,
               ctr_folio,
               ctr_nom_archivo);

   RETURN v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


