






CREATE FUNCTION "safreviv".fn_agr_insrt_ctr_arch_recurr(p_c_nom_archivo CHAR(40),
                                             p_i_estado SMALLINT,
                                             p_v_usuario char(20))
                                             
   RETURNING SMALLINT, SMALLINT, INTEGER, INTEGER, VARCHAR(250)
   
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
   DEFINE v_ax_cont_regs       INTEGER; -- total de registros auxiliar
   DEFINE v_ax_cont_regs_01    INTEGER; -- total de registros insertados en nuevos acreditados 01
   DEFINE v_ax_cont_regs_20    INTEGER; -- total de registros insertados en cambio estatus 20
   DEFINE v_ax_sum_regs_01     INTEGER; -- total de registros en sumario de nuevos acreditados 01
   DEFINE v_ax_sum_regs_20     INTEGER; -- total de registros en sumario de cambio estatus 20
   DEFINE v_ax_tot_registros   INTEGER; -- total de registros insertados
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_ax_operacion       SMALLINT; -- operacion del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso
   DEFINE v_ax_error           SMALLINT; -- contiene el c�digo de error en caso de ocurrir
   DEFINE v_isam_err           INTEGER;
   DEFINE v_c_msj              VARCHAR(250);

   ON EXCEPTION SET v_ax_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrCtrArch_recurr.trace';
   --TRACE ON;

   -- se asigna la operacion del proceso (21 - Recurrente)
   LET v_ax_operacion = 21;
   LET v_ax_id_proceso = 301; -- 301-Anualidades Garantizadas
   LET v_ax_error = 0;
   LET v_isam_err = 0;
   LET v_c_msj = 'El proceso finaliz� correctamente';
   LET v_b_estatus_proc = 0;
   LET v_ax_tot_registros = 0;
   LET v_ax_f_lote = NULL;
   LET v_ax_lote = NULL;

   -- se cuenta el numero de registros insertados en la temporal (NUEVO ACREDITADO)
   SELECT COUNT(*)
     INTO v_ax_cont_regs_01
     FROM safre_tmp:tmp_cre_acred_agr_01;

   -- se cuenta el numero de registros insertados en la temporal (CAMBIO DE ESTATUS)
   SELECT COUNT(*)
     INTO v_ax_cont_regs_20
     FROM safre_tmp:tmp_cre_acred_agr_20;

   -- si el archivo est� marcado como Valido se realiza la validaci�n el sumario
   IF p_i_estado = 10 THEN
      -- se cuenta el numero de registros insertados en la tabla de sumario
      -- ya no env�an sumario, se asume que �sta validaci�n es procedente
      --SELECT COUNT(*)
        --INTO v_ax_cont_regs
        --FROM safre_tmp:tmp_cre_acred_agr_sum;

      LET v_ax_cont_regs = 1;

      -- se valida la existencia en sumario
      IF v_ax_cont_regs <> 1 THEN
         --TRACE("ERROR: NO EXISTE SUMARIO");
         -- se asigna estatus erroneo ya que no se encontraron registros en sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      ELSE
         -- se consulta el total de regsitros en la tabla de sumario
         -- ya no env�an sumario, se asume que �sta validaci�n es procedente
         --SELECT tot_regs_01, tot_regs_20
           --INTO v_ax_sum_regs_01, v_ax_sum_regs_20
           --FROM safre_tmp:tmp_cre_acred_agr_sum;

         LET v_ax_sum_regs_01 = v_ax_cont_regs_01;

         -- se verifican los totales de nuevos acreditados
         IF v_ax_sum_regs_01 <> v_ax_cont_regs_01 THEN
            --TRACE("ERROR: NO COINCIDE EL TOTAL EN SUMARIO CON EL DEL ARCHIVO (NUEVOS ACREDITADOS)");
            -- se asigna estatus erroneo ya que no se encontraron registros detalle
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 4;
         END IF

         LET v_ax_sum_regs_20 = v_ax_cont_regs_20;

         -- si el archivo est� marcado como Valido, continua con validaci�n en cambio estatus
         IF p_i_estado = 10 THEN
            -- se verifican los totales de cambio de estatus
            IF v_ax_sum_regs_20 <> v_ax_cont_regs_20 THEN
               --TRACE("ERROR: NO COINCIDE EL TOTAL EN SUMARIO CON EL DEL ARCHIVO (CAMBIO ESTATUS)");
               -- se asigna estatus erroneo ya que no se encontraron registros detalle
               LET p_i_estado = 30;
               LET v_b_estatus_proc = 4;
            END IF
         END IF
      END IF
   END IF

   -- se insertan los registro tipo 20 en el tipo de registro 01
   INSERT INTO safre_tmp:tmp_cre_acred_agr_01 SELECT * FROM safre_tmp:tmp_cre_acred_agr_20;

   -- se agrupan los registros sin originacion en la tabla 02
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_03;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_04;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_05;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_06;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_07;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_10;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_11;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_13;
   INSERT INTO safre_tmp:tmp_cre_acred_agr_02 SELECT * FROM safre_tmp:tmp_cre_acred_agr_15;

   -- si el archivo est� marcado como V�lido, continua con el conteo de regs sin originaci�n
   IF p_i_estado = 10 THEN
      -- se cuenta el numero de registros insertados en la temporal (01-NUEVO ACREDITADO)
      SELECT COUNT(*)
        INTO v_ax_cont_regs
        FROM safre_tmp:tmp_cre_acred_agr_01;

      -- se acumula el total en la variable del registro
      LET v_ax_tot_registros = v_ax_cont_regs;

      -- se cuenta el numero de registros insertados en la temporal (02-REESTRUCTURA)
      SELECT COUNT(*)
        INTO v_ax_cont_regs
        FROM safre_tmp:tmp_cre_acred_agr_02;

      -- se acumula el total en la variable del registro
      LET v_ax_tot_registros = v_ax_tot_registros + v_ax_cont_regs;

      -- se valida los registros en detalle
      IF v_ax_tot_registros = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se asigna estatus erroneo ya que no se encontraron registros detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- si el archivo est� marcado como V�lido, se busca la fecha y lote
   IF p_i_estado = 10 THEN
      -- se consulta la fecha de lote
      FOREACH
      SELECT FIRST 1 fec_proceso
        INTO v_ax_f_lote
        FROM safre_tmp:tmp_cre_acred_agr_01
       WHERE fec_proceso IS NOT NULL
      END FOREACH;

      -- se valida la fecha proceso
      IF v_ax_f_lote IS NULL OR v_ax_f_lote = "12/31/1899" THEN
         -- se consulta la fecha de lote de los registros sin originacion
         FOREACH
         SELECT FIRST 1 fec_proceso
           INTO v_ax_f_lote
           FROM safre_tmp:tmp_cre_acred_agr_02
          WHERE fec_proceso IS NOT NULL
         END FOREACH;

         -- se valida la fecha proceso
         IF v_ax_f_lote IS NULL OR v_ax_f_lote = "12/31/1899" THEN
            -- se asigna estatus erroneo ya que el archivo no contiene fecha proceso
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 8;
         END IF
      END IF;
   END IF

   -- si el archivo est� marcado como V�lido, se busca el lote
   IF p_i_estado = 10 THEN
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
   END IF;

   -- si el archivo est� marcado como V�lido, se inserta registro en la tabla de control
   IF p_i_estado = 10 THEN
      -- se asignan los valores del registro a insetar
      LET v_r_id_cre_ctr_arch = seq_cre_archivo.NEXTVAL;
      LET v_r_folio_archivo   = 0;
      LET v_r_lote            = v_ax_lote;
      LET v_r_f_lote          = v_ax_f_lote;
      LET v_r_id_proceso      = v_ax_id_proceso;
      LET v_r_operacion       = v_ax_operacion;
      LET v_r_nom_archivo     = p_c_nom_archivo;
      LET v_r_tot_registros   = v_ax_tot_registros;
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
   END IF

   RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros, v_isam_err, v_c_msj;

END FUNCTION
;


