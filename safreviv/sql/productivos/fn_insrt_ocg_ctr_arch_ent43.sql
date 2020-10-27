






CREATE FUNCTION "safreviv".fn_insrt_ocg_ctr_arch_ent43(p_c_nom_archivo CHAR(40),
                                            p_i_estado      SMALLINT,
                                            p_c_usuario     CHAR(20))
   RETURNING SMALLINT, INTEGER

   -- Registro de acr ctr archivo
   DEFINE v_r_usuario          CHAR(20);
   DEFINE v_r_nom_archivo      CHAR(40);
   DEFINE v_r_f_proceso        DATE;
   DEFINE v_r_id_ocg_ctr_arch  DECIMAL(9,0); -- identificador de la tabla de control
   DEFINE v_r_folio            DECIMAL(9,0);
   DEFINE v_r_tot_registros    DECIMAL(10,0);
   DEFINE v_r_lote             DATE;
   DEFINE v_r_id_proceso       SMALLINT;
   DEFINE v_r_operacion        SMALLINT;
   DEFINE v_r_estado           SMALLINT;

   -- Variables auxiliares
   DEFINE v_ax_id_operacion    CHAR(2);  -- identificador de operacion
   DEFINE v_ax_nss_infonavit   CHAR(11); -- nss infonavit
   DEFINE v_ax_f_lote          DATE;     -- fecha de lote
   DEFINE v_ax_tot_registros   INTEGER;  -- total de registros insertados
   DEFINE v_ax_tot_regs_aux    INTEGER;  -- total de registros en encabezado
   DEFINE v_ax_lote            SMALLINT; -- lote
   DEFINE v_ax_id_proceso      SMALLINT; -- identificador del proceso
   DEFINE v_b_estatus_proc     SMALLINT; -- contiene el estatus a regresar segun el proceso
   DEFINE v_ax_f_transferencia DATE;
   DEFINE v_cnt_enc            SMALLINT;
   DEFINE v_cnt_sum            SMALLINT;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_insrt_ocg_ctr_arch_ent43.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_b_estatus_proc     = 0;
   LET v_ax_id_proceso      = 3901; -- Recepción de Entidades financieras 43 Bis
   LET v_ax_f_lote          = NULL;
   LET v_ax_lote            = NULL;
   LET v_ax_f_transferencia = TODAY;
   LET v_cnt_enc            = 0;
   LET v_cnt_sum            = 0;

   IF v_ax_f_lote IS NULL THEN
      -- se asigna la fecha de hoy
      LET v_ax_f_lote = TODAY;
   END IF

   IF v_ax_lote IS NULL THEN
      -- se trata del primer lote del dia
      LET v_ax_lote = 1;
   END IF 

   -- si el archivo está marcado como Valido se realiza la validación el detalle
   IF p_i_estado = 10 THEN
      -- se cuentan los registros cargados a la tabla temporal de detalle 
      SELECT COUNT(*)
        INTO v_ax_tot_registros
        FROM safre_tmp:tmp_rec_det_ocg43;

      -- verifica si existen registros en detalle
      IF v_ax_tot_registros = 0 THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca que hubo un error ya que no hay registros
         LET p_i_estado       = 30;
         LET v_b_estatus_proc = 1;
      END IF
   END IF

   -- en caso de ser valido el estado se hace la validacion del sumario
   IF p_i_estado = 10 THEN
      SELECT COUNT(*)
        INTO v_ax_tot_regs_aux
        FROM safre_tmp:tmp_rec_sum_ocg43;

      -- se verifica el total de registros insertados como sumario
      IF v_ax_tot_regs_aux <> 1 THEN
         --TRACE("ERROR: ARCHIVO SIN SUMARIO O CON MÁS DE UNO");
         -- se marca como rechazado el archivo ya que éste debe contener un sumario
         LET p_i_estado       = 30;
         LET v_b_estatus_proc = 2;
      END IF
   END IF

   SELECT COUNT(*)
     INTO v_cnt_enc
     FROM safre_tmp:tmp_rec_enc_ocg43;

   IF v_cnt_enc > 1 THEN
      LET v_b_estatus_proc = 10;
   ELSE
      SELECT f_transferencia 
        INTO v_ax_f_transferencia 
        FROM safre_tmp:tmp_rec_enc_ocg43;

      IF EXISTS( SELECT f_lote
                   FROM ocg_ctr_archivo
                  WHERE f_lote = v_ax_f_transferencia 
                    AND id_proceso = 3901) THEN 
         LET v_b_estatus_proc = 0;
      END IF;
   END IF;

   SELECT COUNT(*)
     INTO v_cnt_sum
     FROM safre_tmp:tmp_rec_sum_ocg43;

   IF v_cnt_sum > 1 THEN 
      LET v_b_estatus_proc = 11;
   END IF;

   -- se asignan los valores del registro a insetar
   LET v_r_id_ocg_ctr_arch  = seq_ocg_archivo.NEXTVAL;
   LET v_r_folio            = 0;                       -- va en cero, este campo se actualiza en la integracion
   LET v_r_lote             = TODAY;
   LET v_r_id_proceso       = v_ax_id_proceso;
   LET v_r_operacion        = 1;
   LET v_r_nom_archivo      = p_c_nom_archivo;
   LET v_r_tot_registros    = v_ax_tot_registros;
   LET v_r_estado           = p_i_estado;
   LET v_r_f_proceso        = TODAY;                   -- fecha del dia
   LET v_r_usuario          = p_c_usuario;             -- usuario firmado en el sistema

   -- Se insertan en la tabla de control de archivos de ocg
   INSERT INTO ocg_ctr_archivo (
               id_ocg_ctr_archivo   ,
               folio_archivo        ,
               f_lote               ,
               id_proceso           ,
               operacion            ,
               nom_archivo          ,
               tot_registros        ,
               tot_sp1              ,
               tot_sp2              ,
               tot_sp3              ,
               tot_sp4              ,
               tot_sp5              ,
               estatus              ,
               f_proceso            ,
               usuario)
       VALUES (v_r_id_ocg_ctr_arch  ,
               v_r_folio            ,
               v_ax_f_transferencia ,
               v_r_id_proceso       ,
               v_r_operacion        ,
               v_r_nom_archivo      ,
               v_r_tot_registros    ,
               0                    ,
               0                    ,
               0                    ,
               0                    ,
               0                    ,
               v_r_estado           ,
               v_r_f_proceso        ,
               v_r_usuario);

   INSERT INTO ocg_ctr_proceso
        VALUES(v_r_id_ocg_ctr_arch,
               0,     -- Se actualizan cuando se ejecutan los subprocesos
               0,     -- Se actualizan cuando se ejecutan los subprocesos
               0,     -- Se actualizan cuando se ejecutan los subprocesos
               0,     -- Se actualizan cuando se ejecutan los subprocesos
               0,
               TODAY );    -- Se actualizan cuando se ejecutan los subprocesos;

   --TRACE 'PARAMETROS QUE DEVUELVE LA FUNCIÓN :'; 
   --TRACE 'v_estatus_proc' || v_b_estatus_proc ;
   --TRACE 'tot_registros' || v_ax_tot_registros;

   RETURN v_b_estatus_proc, v_ax_tot_registros;
END FUNCTION;


