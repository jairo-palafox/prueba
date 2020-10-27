






CREATE FUNCTION "safreviv".fn_insrt_grt_ctr_arch_devol(p_c_nom_archivo CHAR(40),
                                            p_i_estado SMALLINT,
                                            p_c_usuario CHAR(20))
   RETURNING SMALLINT, INTEGER
   -- Registro de acr ctr archivo
   DEFINE v_r_id_cre_ctr_arch  DECIMAL(9,0); -- identificador de la tabla de control
   DEFINE v_r_folio            DECIMAL(9,0);
   DEFINE v_r_lote             SMALLINT;
   DEFINE v_r_f_lote           DATE;
   DEFINE v_r_id_proceso       SMALLINT;
   DEFINE v_r_operacion        SMALLINT;
   DEFINE v_r_nom_archivo      CHAR(40);
   DEFINE v_r_tot_registros    DECIMAL(10,0);
   DEFINE v_r_tot_aceptados    DECIMAL(10,0);
   DEFINE v_r_tot_rechazados   DECIMAL(10,0);
   DEFINE v_r_tot_sin_origen   DECIMAL(10,0);
   DEFINE v_r_estado           SMALLINT;
   DEFINE v_r_f_proceso        DATE;
   DEFINE v_r_usuario          CHAR(20);
   -- Variables auxiliares
   DEFINE v_ax_tot_registros   INTEGER; -- total de registros insertados
   DEFINE v_ax_tot_regs_aux    INTEGER; -- total de registros en encabezado
   DEFINE v_ax_id_operacion    CHAR(2); -- identificador de operacion
   DEFINE v_ax_nss_infonavit   CHAR(11); -- nss infonavit
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_f_lote          DATE; -- fecha de lote
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtCtrArch_devol.trace';
   --TRACE ON;
   
   -- se inicializan variables
   LET v_b_estatus_proc = 0;
   LET v_ax_id_proceso = 1201; -- 1201-Solicitud de Saldo en Garantía 43 bis

   -- se obtiene el lote y la fecha
   SELECT f_presentacion, cons_lote
   INTO v_ax_f_lote, v_ax_lote
   FROM safre_tmp:tmp_devoluc_enc_grt;
    
   IF ( v_ax_lote IS NULL ) THEN
      -- se trata del primer lote del dia
      LET v_ax_lote = 1;
   END IF

   IF ( v_ax_f_lote IS NULL ) THEN
      -- se asigna la fecha de hoy
      LET v_ax_f_lote = TODAY;
   END IF

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se cuentan los registros cargados a la tabla temporal de detalle de devoluciones
      SELECT COUNT(*)
      INTO v_ax_tot_registros
      FROM safre_tmp:tmp_devoluc_det_grt;

      -- verifica si existen registros en detalle
      IF v_ax_tot_registros = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca que hubo un error ya que no hay registros
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del nss
   IF p_i_estado = 10 THEN
      FOREACH
      -- se leen todos los nss unicos de la tabla temporal
      SELECT UNIQUE nss
        INTO v_ax_nss_infonavit
        FROM safre_tmp:tmp_devoluc_det_grt

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

   -- en caso de ser valido el estado se hace la validacion del encabezado
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_devoluc_enc_grt;

      -- se verifica el total de registros insertados como encabezado
      IF v_ax_tot_regs_aux <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN ENCABEZADO O CON MÁS DE UNO");
         -- se marca como rechazado el archivo ya que éste debe contener un encabezado
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 3;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_devoluc_sum_grt;

      -- se verifica el total de registros insertados como sumario
      IF v_ax_tot_regs_aux <> 1 THEN
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
        FROM safre_tmp:tmp_devoluc_enc_grt;

      -- se verifica el identificador de operacion
      IF v_ax_id_operacion <> "06" THEN
         --TRACE("ERROR: OPERACIÓN EN ARCHIVO INVALIDA");
         -- se marca como rechazado el archivo ya que el id operacion debe ser '06'
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 6;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del tipo de transferencia
   IF p_i_estado = 10 THEN
      IF EXISTS (
      SELECT nss
        FROM safre_tmp:tmp_devoluc_det_grt
       WHERE tpo_transferencia NOT IN (
             SELECT tpo_transferencia
               FROM safre_viv:cat_tipo_transferencia)) THEN
         -- se marca como rechazado el archivo ya que existen tipos transferencia direfente al requierido
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 7;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del total en sumario
   IF p_i_estado = 10 THEN
      SELECT num_registros
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_devoluc_sum_grt;

      -- se verifica el identificador de operacion
      IF v_ax_tot_regs_aux <> v_ax_tot_registros THEN
         --TRACE("ERROR: TOTAL EN SUMARIO NO IGUAL AL TOTAL DE REGISTROS");
         -- se marca como rechazado el archivo ya coincide el total del sumario con el insertado en detalle
         LET p_i_estado = 30;
         LET v_b_estatus_proc = 4;
      END IF
   END IF

   -- se asignan los valores del registro a insetar
   LET v_r_id_cre_ctr_arch  = seq_cre_archivo.NEXTVAL;
   LET v_r_folio            = 0; -- va en cero, este campo se actualiza en la integracion
   LET v_r_lote             = v_ax_lote;
   LET v_r_f_lote           = v_ax_f_lote;
   LET v_r_id_proceso       = v_ax_id_proceso;
   LET v_r_operacion        = 6; -- 6-Devoluciones de solicitudes
   LET v_r_nom_archivo      = p_c_nom_archivo;
   LET v_r_tot_registros    = v_ax_tot_registros;
   LET v_r_tot_aceptados    = 0; -- va en cero, este campo se actualiza en la integracion
   LET v_r_tot_rechazados   = 0; -- va en cero, este campo se actualiza en la integracion
   LET v_r_tot_sin_origen   = NULL;
   LET v_r_estado           = p_i_estado;
   LET v_r_f_proceso        = TODAY; -- fecha del dia
   LET v_r_usuario          = p_c_usuario; -- usuario firmado en el sistema

   -- se insertan los datos en la tabla cre_ctr_archivo
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
               v_r_folio,
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
   RETURN v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


