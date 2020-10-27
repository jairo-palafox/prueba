






CREATE FUNCTION "safreviv".fn_integra_fondo_ahorro_72(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_pid DECIMAL(9,0),
                                           p_d_folio DECIMAL(9,0),
                                           p_i_transferencia CHAR(2))
   RETURNING SMALLINT, INTEGER, INTEGER;
   -- REGISTRO tmp_det_restituye_fondo
   DEFINE v_tmp_consecutivo      INTEGER;
   DEFINE v_tmp_tipo_registro    CHAR(2);
   DEFINE v_tmp_nss              CHAR(11);
   DEFINE v_tmp_credito          CHAR(10);
   DEFINE v_tmp_per_pago         CHAR(6);
   DEFINE v_tmp_fecha_pago       DATE;
   DEFINE v_tmp_tipo_pago        CHAR(4);
   DEFINE v_tmp_rechazo          CHAR(2);
   DEFINE v_tmp_desc_rechazo     CHAR(40);
   DEFINE v_tmp_tot_aportaciones DECIMAL(12,2);
   DEFINE v_tmp_num_cuenta       DECIMAL(10,0);
   DEFINE v_tmp_fecha            DATE;
   -- REGISTRO dse_restitucion_fondo72
   DEFINE v_dse_id_restitucion_fondo72 DECIMAL(9,0);
   DEFINE v_dse_folio                  DECIMAL(9,0);
   DEFINE v_dse_nss                    CHAR(11);
   DEFINE v_dse_importe                DECIMAL(12,2);
   DEFINE v_dse_num_cta                DECIMAL(10,0);
   DEFINE v_dse_f_restitucion          DATE;
   DEFINE v_dse_estado                 INTEGER;
   DEFINE v_dse_f_proceso              DATE;
   -- Campos auxiliares
   DEFINE v_ax_cuenta_acpt        INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech        INTEGER; -- contador de registros rechazados
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_i_estado              SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE r_ax_bandera            SMALLINT; -- valor de regreso de la actualización

   ON EXCEPTION SET v_error
        -- Devolvera el codigo de error cuando ocurra una excepción diferente a -239
        RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegFondoAhorro72.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt = 0;
   LET v_ax_cuenta_rech = 0;
   LET v_error          = 0;
   LET v_i_estado       = 2; -- estado Integrado

   -----------------------------------------------------------------------------------
                  -- SE PROCESAN LOS REGISTROS FONDO DE AHORRO 72 --
   -----------------------------------------------------------------------------------
   FOREACH
      -- se obtienen los datos de la tabla temporal del proceso de fondo de ahorro 72
      SELECT *
      INTO v_tmp_consecutivo,
           v_tmp_tipo_registro,
           v_tmp_nss,
           v_tmp_credito,
           v_tmp_per_pago,
           v_tmp_fecha_pago,
           v_tmp_tipo_pago,
           v_tmp_rechazo,
           v_tmp_desc_rechazo,
           v_tmp_tot_aportaciones,
           v_tmp_num_cuenta,
           v_tmp_fecha
      FROM safre_tmp:tmp_det_restituye_fondo

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET v_dse_id_restitucion_fondo72 = seq_dse_restitucion_fondo72.NEXTVAL;
      LET v_dse_folio                  = p_d_folio;
      LET v_dse_nss                    = v_tmp_nss;
      LET v_dse_importe                = v_tmp_tot_aportaciones;
      LET v_dse_num_cta                = v_tmp_num_cuenta;
      LET v_dse_f_restitucion          = v_tmp_fecha;
      LET v_dse_estado                 = 10;
      LET v_dse_f_proceso              = TODAY;

      -- se inserta registro en la tabla maestro
      INSERT INTO safre_viv:dse_restitucion_fondo72(
                            id_restitucion_fondo72,
                            folio,
                            nss,
                            importe,
                            num_cta,
                            f_restitucion,
                            estado,
                            f_proceso)
                    VALUES (v_dse_id_restitucion_fondo72,
                            v_dse_folio,
                            v_dse_nss,
                            v_dse_importe,
                            v_dse_num_cta,
                            v_dse_f_restitucion,
                            v_dse_estado,
                            v_dse_f_proceso);

      LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;
   END FOREACH;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:dse_restitucion_fondo72;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso,
                                                 p_d_folio,
                                                 v_i_estado,
                                                 p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE fn_act_dse_ctr_archivo(p_d_folio,
                                            p_d_pid,
                                            p_i_transferencia,
                                            v_ax_cuenta_acpt,
                                            v_ax_cuenta_rech);

   RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
END FUNCTION
;


