






CREATE FUNCTION "safreviv".fn_uso_insrt_ctr_arch_usogar(p_c_nom_archivo CHAR(40),
                                             p_i_estado SMALLINT,
                                             p_v_usuario char(20))
   RETURNING SMALLINT, INTEGER
   -- Registro de cre ctr archivo
   DEFINE v_r_id_cre_ctr_arch  DECIMAL(9,0); -- identificador de la tabla de control
   DEFINE v_r_folio_archivo    DECIMAL(9,0); -- folio
   DEFINE v_r_lote             SMALLINT; -- lote
   DEFINE v_r_f_lote           DATE; -- fecha de lote
   DEFINE v_r_id_proceso       SMALLINT; -- identificador del proceso
   DEFINE v_r_operacion        SMALLINT; -- operacion del proceso
   DEFINE v_r_nom_archivo      CHAR(40); -- nombre del archivo
   DEFINE v_r_tot_registros    DECIMAL(10,0); -- total de registro insertados
   DEFINE v_r_tot_aceptados    DECIMAL(10,0); -- total de registro aceptados
   DEFINE v_r_tot_rechazados   DECIMAL(10,0); -- total de registro rechazados
   DEFINE v_r_tot_sin_origen   DECIMAL(10,0); -- total de registro sin origen
   DEFINE v_r_estado           SMALLINT; -- estado de la insercion
   DEFINE v_r_f_proceso        DATE; -- fecha de proceso
   DEFINE v_r_usuario          CHAR(20); -- usuario
   -- Registros axiliares
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_f_lote          DATE; -- fecha de lote
   DEFINE v_ax_nss_infonavit   CHAR(11); -- nss infonavit
   DEFINE v_ax_cont_regs       INTEGER; -- total de registros auxiliar
   DEFINE v_ax_cont_regs_01    INTEGER; -- total de registros insertados en nuevos acreditados 01
   DEFINE v_ax_cont_regs_20    INTEGER; -- total de registros insertados en cambio estatus 20
   DEFINE v_ax_sum_regs_01     INTEGER; -- total de registros en sumario de nuevos acreditados 01
   DEFINE v_ax_sum_regs_20     INTEGER; -- total de registros en sumario de cambio estatus 20
   DEFINE v_ax_tot_regs_det    INTEGER; -- total de registros insertados en detalle
   DEFINE v_ax_tot_regs_aux    INTEGER; -- total de registros en encabezado
   DEFINE v_ax_saldo_viv_sum   DECIMAL(15,0); -- total saldo de vivienda del sumario
   DEFINE v_ax_saldo_viv_det   DECIMAL(15,0); -- total saldo de vivienda del detalle
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso

   SET DEBUG FILE TO '/safreviv_int/BD/usoCtrArch_usoGar.trace';
   TRACE ON;

   -- se inicializan variables
   LET v_ax_operacion = 18;
   LET v_b_estatus_proc = 0;
   LET v_ax_id_proceso = 1202; -- 1202-Uso de Garantía
   LET v_ax_f_lote = NULL;
   LET v_ax_lote = NULL;

   -- se consulta la fecha de lote
   FOREACH
   SELECT FIRST 1 fec_presentacion
     INTO v_ax_f_lote
     FROM safre_tmp:tmp_uso_garantia_det_uso
    WHERE fec_presentacion IS NOT NULL
   END FOREACH;

   -- se valida la fecha de lote
   IF v_ax_f_lote IS NULL THEN
      LET v_ax_f_lote = TODAY;
   END IF

   -- se busca numero de lote correspondiente al archivo
   SELECT MAX(lote)
     INTO v_ax_lote
     FROM safre_viv:cre_ctr_archivo
    WHERE f_lote = v_ax_f_lote
      AND operacion = v_ax_operacion;

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_ax_lote IS NULL THEN
      LET v_ax_lote = 1;
   ELSE
      LET v_ax_lote = v_ax_lote + 1;
   END IF;

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
      SELECT COUNT(*), SUM(saldo_viv)
        INTO v_ax_tot_regs_det, v_ax_saldo_viv_det
        FROM safre_tmp:tmp_uso_garantia_det_uso;

      -- se valida el total de registros insertados en detalle
      IF v_ax_tot_regs_det = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca como rechazado el archivo ya que éste debe contener al menos un registro detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- si el archivo está marcado como Valido se realiza la validación del NSS
   IF p_i_estado = 10 THEN
      FOREACH
      -- se leen todos los nss unicos de la tabla temporal
      SELECT UNIQUE nss_trab
        INTO v_ax_nss_infonavit
        FROM safre_tmp:tmp_uso_garantia_det_uso

         -- se buscar el nss obtenido en la tabla maestro
         IF NOT EXISTS (
         SELECT nss
           FROM safre_viv:afi_derechohabiente
          WHERE nss = v_ax_nss_infonavit) THEN
            -- se asigna estatus de error y se sale del ciclo
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 5;
            EXIT FOREACH;
         END IF;
      END FOREACH;
   END IF;

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_uso_garantia_sum_uso;

      -- se verifica el total de registros insertados como sumario
      IF v_ax_tot_regs_aux <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN SUMARIO O CON MÁS DE UNO");
         -- se marca como rechazado el archivo ya que éste debe contener un sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del tipo de transferencia
   IF p_i_estado = 10 THEN
      IF EXISTS (
      SELECT origen_transf
        FROM safre_tmp:tmp_uso_garantia_det_uso
       WHERE origen_transf NOT IN ('18','48')) THEN
         -- se marca como rechazado el archivo ya que existen tipos transferencia direfente al requierido
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 7;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del total en sumario
   IF p_i_estado = 10 THEN
      SELECT cant_reg_detalle, sum_saldo_viv
        INTO v_ax_tot_regs_aux, v_ax_saldo_viv_sum
        FROM safre_tmp:tmp_uso_garantia_sum_uso;

      -- se verifica el identificador de operacion
      IF v_ax_tot_regs_aux <> v_ax_tot_regs_det THEN
         --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 4;
      ELIF v_ax_saldo_viv_sum <> v_ax_saldo_viv_det THEN
         --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 8;
      END IF
   END IF

   -- se asignan los valores del registro a insetar
   LET v_r_id_cre_ctr_arch = seq_cre_archivo.NEXTVAL;
   LET v_r_folio_archivo   = 0;
   LET v_r_lote            = v_ax_lote;
   LET v_r_f_lote          = v_ax_f_lote;
   LET v_r_id_proceso      = v_ax_id_proceso;
   LET v_r_operacion       = v_ax_operacion;
   LET v_r_nom_archivo     = p_c_nom_archivo;
   LET v_r_tot_registros   = v_ax_tot_regs_det;
   LET v_r_tot_aceptados   = 0;
   LET v_r_tot_rechazados  = 0;
   LET v_r_tot_sin_origen  = 0;
   LET v_r_estado          = p_i_estado;
   LET v_r_f_proceso       = TODAY;
   LET v_r_usuario         = p_v_usuario;
   
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
       VALUES (v_r_id_cre_ctr_arch,
               v_r_folio_archivo,
               v_r_lote,
               v_r_f_lote,
               v_r_id_proceso,
               v_r_operacion,
               v_r_nom_archivo,
               v_r_tot_registros,
               v_r_tot_aceptados,
               v_r_tot_rechazados,
               v_r_tot_sin_origen,
               v_r_estado,
               v_r_f_proceso,
               v_r_usuario);

   RETURN v_b_estatus_proc, v_ax_tot_regs_det;
END FUNCTION;


