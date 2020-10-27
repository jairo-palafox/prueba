






CREATE FUNCTION "safreviv".fn_agr_insrt_ctr_arch_solicsdo(p_c_nom_archivo CHAR(40),
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
   DEFINE v_ax_tot_regs_det    INTEGER; -- total de registros insertados
   DEFINE v_ax_tot_regs_aux    INTEGER; -- total de registros en encabezado
   DEFINE v_ax_f_lote          DATE; -- fecha de lote
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_id_operacion    CHAR(2); -- identificador de operacion
   DEFINE v_ax_nss             CHAR(11); -- nss infonavit
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrCtrArch_rechSdos.trace';
   --TRACE ON;
   
   -- se inicializan variables
   LET v_ax_operacion = 24; -- 24 - Solicitud de Saldo
   LET v_b_estatus_proc = 0;
   LET v_ax_id_proceso = 301; -- 301 - Anualidades garantizadas
   LET v_ax_f_lote = TODAY;
   LET v_ax_lote = NULL;

   -- se busca numero de lote correspondiente al archivo
   SELECT MAX(lote)
     INTO v_ax_lote
     FROM safre_viv:cre_ctr_archivo
    WHERE f_lote = v_ax_f_lote;

   -- si no se encuentra lote en la sentencia se asume que es la primera del dia
   IF v_ax_lote IS NULL THEN
      LET v_ax_lote = 1;
   ELSE
      -- se incrementa el lote del dia
      LET v_ax_lote = v_ax_lote + 1;
   END IF;

   -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
   SELECT COUNT(*)
     INTO v_ax_tot_regs_det
     FROM safre_tmp:tmp_solic_sdo_det_agr;

   -- se valida el total de registros insertados en detalle
   IF v_ax_tot_regs_det = 0 THEN
      --TRACE("ERROR: NO HAY REGISTROS DETALLE");
      -- se marca como rechazado el archivo ya que éste debe contener al menos un registro detalle
      LET p_i_estado = 30;
      LET v_b_estatus_proc = 1;

      RETURN v_b_estatus_proc, v_ax_tot_regs_det;
   END IF

   -- se cuenta el numero de registros insertados como sumario
   SELECT COUNT(*)
     INTO v_ax_tot_regs_aux
     FROM safre_tmp:tmp_solic_sdo_sum_agr;

   -- se verifica el total de registros insertados como sumario
   IF v_ax_tot_regs_aux <> 1 THEN
      --TRACE("ERROR: ARCHIVO SIN SUMARIO O CON MÁS DE UNO");
      -- se marca como rechazado el archivo ya que éste debe contener un sumario
      LET p_i_estado = 30;
      LET v_b_estatus_proc = 2;

      RETURN v_b_estatus_proc, v_ax_tot_regs_det;
   END IF
   
   -- se obtiene el numero de registros marcados en el sumario
   SELECT tot_registros
     INTO v_ax_tot_regs_aux
     FROM safre_tmp:tmp_solic_sdo_sum_agr;

   -- se valida que la cantidad de registros en detalle coincida con el sumario
   IF v_ax_tot_regs_aux <> v_ax_tot_regs_det THEN
      --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
      -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
      LET p_i_estado = 30;
      LET v_b_estatus_proc = 4;

      RETURN v_b_estatus_proc, v_ax_tot_regs_det;
   END IF

   -- se asignan los valores del registro a insetar
   LET v_ra_id_cre_ctr_arch = seq_cre_archivo.NEXTVAL;
   LET v_ra_folio_archivo = 0;
   LET v_ra_lote = v_ax_lote;
   LET v_ra_f_lote = v_ax_f_lote;
   LET v_ra_id_proceso = v_ax_id_proceso;
   LET v_ra_operacion = v_ax_operacion;
   LET v_ra_nom_archivo = p_c_nom_archivo;
   LET v_ra_tot_registros = v_ax_tot_regs_det;
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

   RETURN v_b_estatus_proc, v_ax_tot_regs_det;
END FUNCTION;


