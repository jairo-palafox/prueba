






CREATE FUNCTION "safreviv".fn_agr_insrt_ctr_arch_desmarca(p_c_nom_archivo CHAR(40),
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
   DEFINE v_ax_regs_det_orig   INTEGER; -- total de registros insertados como Originación (01, 20)
   DEFINE v_ax_regs_det_sinor  INTEGER; -- total de registros insertados como Sin Originación
   DEFINE v_ax_regs_det_react  INTEGER; -- total de registros insertados como Reactivación (04, 08)
   DEFINE v_ax_regs_det_desm   INTEGER; -- total de registros insertados como Desmarca (11 y 05)
   DEFINE v_ax_regs_sum        INTEGER; -- contador auxiliar de registro
   DEFINE v_ax_regs_tot_sum    INTEGER; -- total de registros registrados en sumario
   DEFINE v_ax_regs_11_det     INTEGER; -- total de registros tipo 11 insertados en detalle
   DEFINE v_ax_regs_11_sum     INTEGER; -- total de registros tipo 11 registrados en sumario
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_f_lote          DATE; -- fecha del lote
   DEFINE v_ax_nss_infonavit   CHAR(11);
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrCtrArch_desmarca.trace';
   --TRACE ON;

   -- se asigna la operacion del proceso (30 - Desmarca)
   LET v_ax_operacion = 30; -- Operación del proceso
   LET v_b_estatus_proc = 0; -- regresa el estatus por el cual no se puede realizar la integración
   LET v_ax_id_proceso = 301; -- 301-Anualidades Garantizadas
   LET v_ax_f_lote = NULL; -- se inicializa con nulo la fecha lote
   LET v_ax_lote = NULL;

   -- se crea la sentencia sql que obtiene el número registros insertados como tipo 11
   SELECT COUNT(*)
     INTO v_ax_regs_11_det
     FROM safre_tmp:tmp_desmarca_det_agr;

   -- se agrupan los registros de liquidación de crédito en la tabla 11
   INSERT INTO safre_tmp:tmp_desmarca_det_agr SELECT * FROM safre_tmp:tmp_desmarca_det_agr_05;

   -- se agrupan los registros de rechazo en la tabla 02
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_03;
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_06;
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_07;
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_10;
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_13;
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_02 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_15;

   -- se agrupan los registros de Recurrente Originación en la tabla 01
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_01 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_20;
   
   -- se agrupan los registros de Reactivación en la tabla 04
   INSERT INTO safre_tmp:tmp_desmarca_det_agr_04 SELECT * FROM safre_tmp:tmp_desmarca_det_agr_08;

   -- se crea la sentencia sql que obtiene el número registros insertados en la tabla detalle (Originación)
   SELECT COUNT(*)
     INTO v_ax_regs_det_orig
     FROM safre_tmp:tmp_desmarca_det_agr_01;

   -- se crea la sentencia sql que obtiene el número registros insertados en la tabla detalle (Sin originación)
   SELECT COUNT(*)
     INTO v_ax_regs_det_sinor
     FROM safre_tmp:tmp_desmarca_det_agr_02;

   -- se crea la sentencia sql que obtiene el número registros insertados en la tabla detalle (Reactivación)
   SELECT COUNT(*)
     INTO v_ax_regs_det_react
     FROM safre_tmp:tmp_desmarca_det_agr_04;

   -- se crea la sentencia sql que obtiene el número registros insertados en la tabla detalla (Desmarca)
   SELECT COUNT(*)
     INTO v_ax_regs_det_desm
     FROM safre_tmp:tmp_desmarca_det_agr;

   -- se crea la sentencia sql que obtiene el número registros insertados en la tabla sumario
   -- ya no envían sumario, se asume que ésta validación es procedente
   --SELECT COUNT(*)
     --INTO v_ax_regs_sum
     --FROM safre_tmp:tmp_desmarca_sum_agr;

   -- se acumula el número de registros detalle y sumario
   LET v_ax_tot_regs_det = v_ax_regs_det_orig + v_ax_regs_det_sinor + v_ax_regs_det_react + v_ax_regs_det_desm; --- + v_ax_regs_sum;

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se verifica si existieron registros en ambas tablas y en sumario
      IF v_ax_regs_det_desm = 0 AND
         v_ax_regs_det_orig = 0 AND
         v_ax_regs_det_react = 0 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- si el archivo está marcado como válido se realiza la validación el sumario
   -- ya no envían sumario, se asume que ésta validación es procedente
   IF p_i_estado = 10 THEN
      -- se valida la existencia en sumario

      LET v_ax_regs_sum = 1;

      IF v_ax_regs_sum <> 1 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros en sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      ELSE
         -- se consulta el total de regsitros en la tabla de sumario
         --SELECT tot_regs, tot_regs_11
           --INTO v_ax_regs_tot_sum, v_ax_regs_11_sum
           --FROM safre_tmp:tmp_desmarca_sum_agr;

         LET v_ax_regs_tot_sum = v_ax_tot_regs_det;
         LET v_ax_regs_11_sum  = v_ax_regs_11_det;

         -- se verifican los totales
         IF v_ax_regs_tot_sum <> v_ax_tot_regs_det THEN           
            -- se asigna estatus erroneo ya que no coincide el total de registros en sumario
            -- con los contados en detalle
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 4;
         ELIF v_ax_regs_11_sum <> v_ax_regs_11_det THEN
            -- se asigna estatus erroneo ya que no coincide el total de registros en sumario
            -- con los contados en detalle
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 4;
         END IF
      END IF
   END IF

{
   -- Ya no se valida que el NSS exista en catálogo, en la integración se rechaza
   -- el registro para este caso. 23/11/2012 12:28:58 p.m.
   -- si el archivo está marcado como Valido se realiza la validación del NSS
   IF p_i_estado = 10 THEN
      FOREACH
      -- se leen todos los nss unicos de la tabla temporal
      SELECT UNIQUE nss
        INTO v_ax_nss_infonavit
        FROM safre_tmp:tmp_desmarca_det_agr

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
}

   -- se consulta la fecha de lote
   FOREACH
   SELECT FIRST 1 fec_proceso
     INTO v_ax_f_lote
     FROM safre_tmp:tmp_desmarca_det_agr
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
   LET v_r_tot_sin_origen = 0;
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


