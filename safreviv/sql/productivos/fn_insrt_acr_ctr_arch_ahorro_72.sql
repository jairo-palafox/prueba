






CREATE FUNCTION "safreviv".fn_insrt_acr_ctr_arch_ahorro_72(p_c_nom_archivo CHAR(40),
                                                p_i_estado SMALLINT,
                                                p_v_usuario CHAR(20))
   RETURNING SMALLINT, SMALLINT, INTEGER
   -- Registro de cre ctr archivo
   DEFINE v_r_folio_archivo    DECIMAL(9,0); -- folio
   DEFINE v_r_trasferencia     CHAR(2);
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
   DEFINE v_ax_cont_regs_20    DECIMAL(12,2); -- total de registros insertados en cambio estatus 20
   DEFINE v_ax_sum_regs_01     DECIMAL(12,2); -- total de registros en sumario de nuevos acreditados 01
   DEFINE v_ax_sum_regs_20     INTEGER; -- total de registros en sumario de cambio estatus 20
   DEFINE v_ax_tot_registros   INTEGER; -- total de registros insertados
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso
   DEFINE v_ax_error           SMALLINT; -- contiene el c�digo de error en caso de ocurrir

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrFondo_ahorro_72.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_b_estatus_proc = 0;
   LET v_ax_tot_registros = 0;
   LET v_ax_error = 0;

   -- si el archivo est� marcado como Valido, continua con el conteo de regs sin originaci�n
   IF p_i_estado = 10 THEN

         LET v_ax_f_lote = TODAY;
         
         -- se busca numero de lote correspondiente al archivo
         SELECT MAX(lote)
           INTO v_ax_lote
           FROM safre_viv:dse_ctr_archivo
          WHERE f_lote = v_ax_f_lote;

         -- si no se encuentra lote en la sentencia se asume que es la primera del dia
         IF v_ax_lote IS NULL THEN
            LET v_ax_lote = 1;
         ELSE
            LET v_ax_lote = v_ax_lote + 1;
         END IF;
   END IF

   -- si el archivo est� marcado como Valido se realiza la validaci�n el detalle
   IF p_i_estado = 10 THEN
      -- se cuenta el numero de registros insertados en la temporal (NUEVO ACREDITADO)
      SELECT COUNT(*)
        INTO v_ax_tot_registros
        FROM safre_tmp:tmp_det_restituye_fondo;

      -- se valida los registros en detalle
      IF v_ax_tot_registros = 0 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- si el archivo est� marcado como Valido se realiza la validaci�n el sumario
   IF p_i_estado = 10 THEN
      -- se cuenta el numero de registros insertados en la tabla de sumario
      SELECT COUNT(*)
        INTO v_ax_cont_regs
        FROM safre_tmp:tmp_sum_restituye_fondo;

      -- se valida la existencia en sumario
      IF v_ax_cont_regs <> 1 THEN
         -- se asigna estatus erroneo ya que no se encontraron registros en sumario
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 2;
      ELSE
         -- se consulta el total de regsitros en la tabla de sumario
         -- sum detalle aportacion
         SELECT SUM(tot_aportaciones)
           INTO v_ax_sum_regs_01
           FROM safre_tmp:tmp_det_restituye_fondo;
           
         SELECT tot_aportaciones
           INTO v_ax_cont_regs_20
           FROM safre_tmp:tmp_sum_restituye_fondo;

            -- se verifican los totales de cambio de estatus
         IF v_ax_sum_regs_01 <> v_ax_cont_regs_20 THEN
            -- se asigna estatus erroneo ya que no se encontraron registros detalle
            LET p_i_estado = 30;
            LET v_b_estatus_proc = 4;
         ELSE
            SELECT COUNT(*)
              INTO v_ax_cont_regs
              FROM safre_tmp:tmp_det_restituye_fondo;
            
            SELECT num_registros
              INTO v_ax_cont_regs_01
              FROM safre_tmp:tmp_sum_restituye_fondo;
            
            -- se verifican los totales de registros
            IF v_ax_cont_regs <> v_ax_cont_regs_01 THEN
               -- se asigna estatus erroneo ya que no se encontraron registros detalle
               LET p_i_estado = 30;
               LET v_b_estatus_proc = 9;
            END IF     
         END IF
      END IF
   END IF

   IF p_i_estado = 10 THEN
      -- se asignan los valores del registro a insetar
      LET v_r_folio_archivo   = 0;
      LET v_r_trasferencia    = 15;
      LET v_r_lote            = v_ax_lote;
      LET v_r_f_lote          = v_ax_f_lote;
      LET v_r_nom_archivo     = p_c_nom_archivo;
      LET v_r_tot_registros   = v_ax_tot_registros;
      LET v_r_tot_aceptados   = 0;
      LET v_r_tot_rechazados  = 0;
      LET v_r_estado          = p_i_estado;
      LET v_r_f_proceso       = TODAY;
      LET v_r_usuario         = p_v_usuario;

      -- se inserta el registro en la tabla de control
      
         INSERT INTO safre_viv:dse_ctr_archivo(
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
            VALUES(v_r_trasferencia,
                   v_r_lote,
                   v_r_f_lote,
                   v_r_tot_registros,
                   v_r_tot_aceptados,
                   v_r_tot_rechazados,
                   v_r_estado,
                   v_r_f_proceso,
                   v_r_usuario,
                   v_r_folio_archivo,
                   v_r_nom_archivo);
   END IF
   RETURN v_ax_error, v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


