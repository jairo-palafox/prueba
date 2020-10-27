






CREATE FUNCTION "safreviv".fn_grt_insrt_ctr_arch_desmarca(p_c_nom_archivo CHAR(40),
                                               p_i_estado SMALLINT,
                                               p_v_usuario CHAR(20))
   RETURNING SMALLINT, INTEGER
   -- Registro de cre ctr archivo
   DEFINE v_ra_id_cre_ctr_arch DECIMAL(9,0); -- identificador de la tabla de control
   DEFINE v_ra_folio_archivo   DECIMAL(9,0); -- folio
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
   DEFINE v_ax_tot_regs_det    INTEGER; -- numero total de registros en archivo
   DEFINE v_ax_tot_regs_sum    INTEGER; -- numero total de registros registrados en sumario
   DEFINE v_ax_regs_det_02     INTEGER; -- total de registros insertados como tipo 02
   DEFINE v_ax_regs_det_11     INTEGER; -- total de registros insertados como tipo 11
   DEFINE v_ax_regs_sum_11     INTEGER; -- total de registros en sumario como tipo 11
   DEFINE v_ax_regs_sum_tot    INTEGER; -- contador auxiliar de registro
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_f_lote          DATE; -- fecha del lote
   DEFINE v_ax_nss_infonavit   CHAR(11);
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtCtrArch_desmarca.trace';
   --TRACE ON;

   -- se asigna la operacion del proceso (30 - Desmarca)
   LET v_ax_operacion = 30;
   LET v_b_estatus_proc = 0;
   LET v_ax_id_proceso = 1201; -- 1201-Solicitud de Saldo en Garantía 43 bis
   LET v_ax_f_lote = NULL; -- se inicializa con nulo la fecha lote
   LET v_ax_lote = NULL;

   -- se agrupan los registros de rechazo en la tabla 02
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_01;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_03;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_04;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_05;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_06;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_07;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_10;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_13;
   INSERT INTO safre_tmp:tmp_desmarca_det_grt_02 SELECT * FROM safre_tmp:tmp_desmarca_det_grt_20;

   -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla detalle 2
   SELECT COUNT(*)
     INTO v_ax_regs_det_02
     FROM safre_tmp:tmp_desmarca_det_grt_02;

   -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla de paso
   SELECT COUNT(*)
     INTO v_ax_regs_det_11
     FROM safre_tmp:tmp_desmarca_det_grt;

   -- se crea la sentencia sql que obtiene el numero registros insertados en la tabla sumario
   -- ya no envían sumario, se asume que ésta validación es procedente
   --SELECT COUNT(*)
     --INTO v_ax_regs_sum_tot
     --FROM safre_tmp:tmp_desmarca_sum_grt;

   LET v_ax_regs_sum_tot = 1;

   -- se acumula el número de registros detalle (11 y 02) y sumario
   LET v_ax_tot_regs_det = v_ax_regs_det_02 + v_ax_regs_det_11; --- + v_ax_regs_sum_tot;

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se verifica si existieron registros en ambas tablas y en sumario
      IF v_ax_regs_det_11 = 0 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- si el archivo está marcado como Valido se realiza la validación el sumario
   -- ya no envían sumario, se asume que ésta validación es procedente
   IF p_i_estado = 10 THEN
      -- se valida la existencia en sumario
      IF v_ax_regs_sum_tot <> 1 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros en sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      ELSE
         -- se consulta el total de regsitros en la tabla de sumario
         -- ya no envían sumario, se asume que ésta validación es procedente
         --SELECT tot_regs --, tot_regs_11
           --INTO v_ax_tot_regs_sum --, v_ax_regs_sum_11
           --FROM safre_tmp:tmp_desmarca_sum_grt;

         LET v_ax_tot_regs_sum = v_ax_tot_regs_det;

         -- se verifican los totales
         IF v_ax_tot_regs_sum <> v_ax_tot_regs_det THEN           
            -- se asigna estatus erroneo ya que no coincide el total de registros en sumario
            -- con los contados en detalle
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 4;
         --ELIF v_ax_regs_sum_11 <> v_ax_regs_det_11 THEN
         --   -- se asigna estatus erroneo ya que no coincide el total de registros en sumario
         --   -- con los contados en detalle
         --   LET p_i_estado = 30;
         --   LET v_b_estatus_proc = 4;
         END IF
      END IF
   END IF

   -- se consulta la fecha de lote
   FOREACH
   SELECT FIRST 1 fec_proceso
     INTO v_ax_f_lote
     FROM safre_tmp:tmp_desmarca_det_grt
   END FOREACH;

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

   -- se resta el registro sumario a la variable de total de registro en detalle
   --LET v_ax_tot_regs_det = v_ax_tot_regs_det - 1;

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


