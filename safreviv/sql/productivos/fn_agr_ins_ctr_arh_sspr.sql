






CREATE FUNCTION "safreviv".fn_agr_ins_ctr_arh_sspr(p_nom_archivo CHAR(40),
                                        p_estado  SMALLINT,
                                        p_usuario CHAR(20))

   RETURNING SMALLINT,INTEGER

   DEFINE v_estado_carga         SMALLINT;
   DEFINE v_ax_tot_registros     INTEGER ;
   -- Variables insert ctr_archivo
   DEFINE v_id_cre_ctr_arh       DECIMAL(9,0);

   --SET DEBUG FILE TO '/safreviv_int/archivos/fn_agr_ins_ctr_arh_sspr.trace';
   --TRACE ON;

   -- Inicializa variables
   LET v_estado_carga     = 0;  -- Correcto
   LET v_ax_tot_registros = 0;

   -- Si el archivo está marcado como Valido se realiza la validación el detalle
   IF (p_estado = 10) THEN

      SELECT COUNT(*)
        INTO v_ax_tot_registros
        FROM safre_tmp:tmp_det_marca_sspr;

      -- se valida el total de registros insertados en detalle
      IF (v_ax_tot_registros = 0) THEN
         --TRACE("ERROR: NO HAY REGISTROS DETALLE");
         -- se marca como rechazado el archivo ya que éste debe contener al menos un registro detalle
         LET p_estado       = 30;
         LET v_estado_carga = 1;
      END IF
   END IF

   -- Asigna valores
   LET v_id_cre_ctr_arh = seq_cre_archivo.NEXTVAL;

   -- Inserta en tabla control de archivos
   INSERT INTO cre_ctr_archivo (
               id_cre_ctr_archivo ,
               folio_archivo      ,
               lote               ,
               f_lote             ,
               id_proceso         ,
               operacion          ,
               nom_archivo        ,
               tot_registros      ,
               tot_aceptados      ,
               tot_rechazados     ,
               tot_sin_origen     ,
               estado             ,
               f_proceso          ,
               usuario)
       VALUES (v_id_cre_ctr_arh   ,
               0                  ,
               NULL               , -- No necesario lote
               NULL               , -- No necesario f_lote
               350                , -- ACTUALIZACION MARCA SSPR
               1                  , -- VALIDA ARCHIVO SSPR
               p_nom_archivo      ,
               v_ax_tot_registros ,
               0                  ,
               0                  ,
               0                  ,
               p_estado           ,
               TODAY              ,
               p_usuario);


   RETURN v_estado_carga,v_ax_tot_registros;

END FUNCTION;


