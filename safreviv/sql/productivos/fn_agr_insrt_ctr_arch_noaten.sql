






CREATE FUNCTION "safreviv".fn_agr_insrt_ctr_arch_noaten(p_c_nom_archivo CHAR(40),
                                             p_i_estado SMALLINT,
                                             p_v_usuario CHAR(20))
   RETURNING SMALLINT, INTEGER
   -- Registro de cre ctr archivo
   DEFINE v_ra_id_cre_ctr_arch DECIMAL(9,0); -- identificador de la tabla de control
   DEFINE v_ra_folio_archivo   DECIMAL(9,0); -- numero de folio
   DEFINE v_ra_lote            SMALLINT; -- lote
   DEFINE v_ra_f_lote          DATE; -- fecha de lote
   DEFINE v_ra_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_ra_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_ra_nom_archivo     CHAR(40); -- nombre del archivo
   DEFINE v_ra_tot_registros   DECIMAL(10,0); -- total de registro insertados
   DEFINE v_ra_tot_aceptados   DECIMAL(10,0); -- total de registro aceptados
   DEFINE v_ra_tot_rechazados  DECIMAL(10,0); -- total de registro rechazados
   DEFINE v_r_tot_sin_origen   DECIMAL(10,0); -- total de registro sin origen
   DEFINE v_ra_estado          SMALLINT; -- estado de la insercion
   DEFINE v_ra_f_proceso       DATE; -- fecha de proceso
   DEFINE v_ra_usuario         CHAR(20); -- usuario
   -- Variables auxiliares
   DEFINE v_ax_tot_registros   INTEGER; -- numero total de registros
   DEFINE v_ax_tot_regs_aux    INTEGER; -- total de registros en encabezado
   DEFINE v_ax_id_operacion    CHAR(2); -- identificador de operacion
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_f_lote          DATE; -- fecha de lote
   DEFINE v_ax_nss_infonavit   CHAR(11); -- NSS Infonavit
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrCtrArch_noaten.trace';
   --TRACE ON;

   -- se asigna la operacion del proceso (14 - No Antendidas)
   LET v_ax_operacion = 14;
   LET v_b_estatus_proc = 0;
   LET v_ax_id_proceso = 301; -- 301-Anualidades Garantizadas

   -- se busca numero de lote correspondiente al archivo
   SELECT f_presentacion, consecutivo_lote
     INTO v_ax_f_lote, v_ax_lote
     FROM safre_tmp:tmp_no_atend_enc_agr;

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_ax_lote IS NULL THEN
      LET v_ax_lote = 1;
   END IF;

   -- si no se encuentra la fecha de lote se asigna la fecha de hoy
   IF v_ax_f_lote IS NULL THEN
      LET v_ax_f_lote = TODAY;
   END IF;

   -- si el archivo est� marcado como Valido se realiza la validaci�n el detalle
   IF p_i_estado = 10 THEN
      -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
      SELECT COUNT(*)
        INTO v_ax_tot_registros
       FROM safre_tmp:tmp_no_atend_det_agr;

      -- se valida el total de registros insertados en detalle
      IF v_ax_tot_registros = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca como rechazado el archivo ya que �ste debe contener al menos un registro detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- en caso de ser valido el estado se hace una ultima validacion
   IF p_i_estado = 10 THEN
      FOREACH
      -- se leen todos los nss unicos de la tabla temporal
      SELECT UNIQUE nss_infonavit
        INTO v_ax_nss_infonavit
        FROM safre_tmp:tmp_no_atend_det_agr

         -- se buscar el nss obtenido en la tabla maestro
         IF NOT EXISTS (
         SELECT nss
           FROM safre_viv:afi_derechohabiente
          WHERE nss = v_ax_nss_infonavit) THEN
            -- se marca como rechazado el archivo ya que todos los nss deben estar catalogados
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 5;
            EXIT FOREACH;
         END IF;
      END FOREACH;
   END IF;

   -- en caso de ser valido el estado se hace la validacion del encabezado
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_no_atend_enc_agr;

      -- se verifica el total de registros insertados como encabezado
      IF v_ax_tot_regs_aux <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN ENCABEZADO O CON M�S DE UNO");
         -- se marca como rechazado el archivo ya que �ste debe contener un encabezado
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 3;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_no_atend_sum_agr;

      -- se verifica el total de registros insertados como sumario
      IF v_ax_tot_regs_aux <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN SUMARIO O CON M�S DE UNO");
         -- se marca como rechazado el archivo ya que �ste debe contener un sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del id operacion
   IF p_i_estado = 10 THEN
      SELECT id_operacion
        INTO v_ax_id_operacion
        FROM safre_tmp:tmp_no_atend_enc_agr;

      -- se verifica el identificador de operacion
      IF v_ax_id_operacion <> "14" THEN
         --TRACE("ERROR: OPERACI�N EN ARCHIVO INVALIDA");
         -- se marca como rechazado el archivo ya que el id operacion debe ser '14'
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 6;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del tipo de transferencia
   IF p_i_estado = 10 THEN
      IF EXISTS (
      SELECT tpo_transf
        FROM safre_tmp:tmp_no_atend_det_agr
       WHERE tpo_transf NOT IN (
             SELECT tpo_transferencia
               FROM safre_viv:cat_tipo_transferencia)) THEN
         -- se marca como rechazado el archivo ya que existen tipos transferencia direfente al requierido
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 7;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del total en sumario
   IF p_i_estado = 10 THEN
      SELECT cant_registros
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_no_atend_sum_agr;

      -- se verifica el identificador de operacion
      IF v_ax_tot_regs_aux <> v_ax_tot_registros THEN
         --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 4;
      END IF
   END IF

   -- se asignan los valores del registro a insetar
   LET v_ra_id_cre_ctr_arch = seq_cre_archivo.NEXTVAL;
   LET v_ra_folio_archivo = 0;
   LET v_ra_lote = v_ax_lote;
   LET v_ra_f_lote = v_ax_f_lote;
   LET v_ra_id_proceso = v_ax_id_proceso;
   LET v_ra_operacion = v_ax_operacion;
   LET v_ra_nom_archivo = p_c_nom_archivo;
   LET v_ra_tot_registros = v_ax_tot_registros;
   LET v_ra_tot_aceptados = 0;
   LET v_ra_tot_rechazados = 0;
   LET v_r_tot_sin_origen = NULL;
   LET v_ra_estado = p_i_estado;
   LET v_ra_f_proceso = TODAY;
   LET v_ra_usuario = p_v_usuario;

   -- se inserta el registro en la tabla de control
   INSERT INTO safre_viv:cre_ctr_archivo (
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
       VALUES (v_ra_id_cre_ctr_arch,
               v_ra_folio_archivo,
               v_ra_lote,
               v_ra_f_lote,
               v_ra_id_proceso,
               v_ra_operacion,
               v_ra_nom_archivo,
               v_ra_tot_registros,
               v_ra_tot_aceptados,
               v_ra_tot_rechazados,
               v_r_tot_sin_origen,
               v_ra_estado,
               v_ra_f_proceso,
               v_ra_usuario);

   RETURN v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


