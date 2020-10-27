






CREATE FUNCTION  "safreviv".fn_grt_solicitud_desmarca(p_v_usuario          CHAR(20),     -- N/U
                                           p_d_folio            DECIMAL(9,0), -- N/U
                                           p_id_derechohabiente DECIMAL(9,0),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                           p_f_liquida_credito  DATE,          --N/U
                                           p_id_ocg_liquidacion DECIMAL(9,0))

   RETURNING SMALLINT, SMALLINT, SMALLINT, DECIMAL;

   -- REGISTRO tmp_desmarca_det_grt
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10);
   DEFINE v_rt_ssv_92_97            DECIMAL(8);
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_credito          DECIMAL(3,0);
   DEFINE v_rt_sts_credito          DECIMAL(3,0);
   DEFINE v_rt_tpo_descuento        DECIMAL(1,0);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(8,0);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2,0);

   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE rch_nss                   CHAR(11);
   DEFINE rch_tpo_originacion       SMALLINT;
   DEFINE rch_tpo_credito           SMALLINT;
   DEFINE rch_tpo_registro          CHAR(2);
   DEFINE rch_num_credito           DECIMAL(10,0);
   DEFINE rch_sdo_deudor            DECIMAL(12,2);
   DEFINE rch_f_otorga              DATE;
   DEFINE rch_f_culmina             DATE;
   DEFINE rch_edo_credito           SMALLINT;
   DEFINE rch_tpo_dscto             SMALLINT;
   DEFINE rch_valor_dscto           DECIMAL(8,4);
   DEFINE rch_nrp                   CHAR(11);
   DEFINE rch_f_ini_dscto           DATE;
   DEFINE rch_nss_liberado          CHAR(11);
   DEFINE rch_f_gen_arh             DATE;
   DEFINE rch_sdo_credito           DECIMAL(12,2);
   DEFINE rch_f_prox_liq            DATE;
   DEFINE rch_f_desde               DATE;
   DEFINE rch_f_hasta               DATE;
   DEFINE rch_tpo_rch               SMALLINT;
   DEFINE rch_estado                SMALLINT;

   -- REGISTRO cre acreditado
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);

   -- Campos auxiliares
   DEFINE v_ax_operacion            SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_lote_acpt         INTEGER;  -- total de registros aceptados
   DEFINE v_ax_id_lote_rech         INTEGER;  -- total de registros rechazados
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_estado               SMALLINT; -- estado
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar
   DEFINE v_ax_tpo_transferencia    CHAR(2);  -- tipo de transferencia
   DEFINE v_i_glo_estado            SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_b_desmarcar             SMALLINT; -- booleana que indica si el registro se debe o no desmarcar
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_ax_f_proceso            DATE;     --fecha de proceso
   DEFINE r_ax_bandera              SMALLINT; -- valor de regreso de la actualización
   DEFINE p_v_arch_proceso          CHAR(100);

   DEFINE v_rechazo                 SMALLINT;
   DEFINE v_aceptado                SMALLINT;

   -- Campos desmarca 223
   DEFINE v_dm_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_dm_marca_entra          SMALLINT;
   DEFINE v_dm_n_referencia         INTEGER;
   DEFINE v_dm_estado_marca         SMALLINT;
   DEFINE v_dm_marca_causa          SMALLINT;
   DEFINE v_dm_usuario              CHAR(12);
   DEFINE v_dm_proceso_cod          SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;

   ON EXCEPTION SET v_ax_error
      LET v_rechazo  = 0;
      LET v_aceptado = 0;

      RETURN v_ax_error, v_aceptado, v_rechazo, v_id_cre_acreditado;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/grtIntegDesm.trace';
   ---TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt        =  0;
   LET v_ax_id_lote_rech        =  0;
   LET v_ax_operacion           =  30; -- Desmarca
   LET v_i_glo_estado           =  2; -- estado Integrado para glo ctr archivo
   LET v_ax_error               =  0;
   LET v_ax_f_proceso           =  TODAY;
   LET v_ax_tpo_transferencia   =  "18"; -- Solicitud de Saldos en Garantía 43 bis
   LET v_rechazo                =  0;
   LET v_aceptado               =  0;

   -- se inicializan variables
   LET v_id_cre_acreditado      = NULL;
   LET p_v_arch_proceso         = NULL;
   LET rch_f_otorga             = NULL;
 --  LET rch_f_culmina           = NULL;
   LET rch_edo_credito          = NULL;
   LET rch_tpo_dscto            = NULL;
   LET rch_nrp                  = NULL;
   LET rch_f_ini_dscto          = NULL;
   LET rch_nss_liberado         = NULL;
   LET rch_f_gen_arh            = NULL;
   LET rch_sdo_credito          = NULL;
   LET rch_f_prox_liq           = NULL;
   LET rch_f_desde              = NULL;
   LET rch_f_hasta              = NULL;
   LET rch_tpo_rch              = NULL;
   LET rch_estado               = NULL;
   LET v_ax_edo_procesar        = NULL;
   --LET v_ax_tpo_transferencia   = NULL; 
   LET v_ax_sts_registro        = " ";
   LET v_rt_tpo_registro        = NULL;
   LET v_rt_nss                 = NULL;
   LET v_rt_num_credito         =  0;
   LET v_rt_ssv_92_97           =  0;
   LET v_rt_fec_culminacion     =  TODAY;
   LET v_rt_tpo_credito         =  2;
   LET v_rt_sts_credito         =  11;
   LET v_rt_tpo_descuento       =  0;
   LET v_rt_nrp                  = NULL;
   LET v_rt_fec_ini_oblig_patron = NULL;
   LET v_rt_nss_liberado         = NULL;
   LET v_rt_fec_proceso          = TODAY;
   LET v_rt_sdo_credito          = 0;
   LET v_rt_fec_prox_liquidar    = NULL;
   LET v_rt_fec_dsd_avis_desast  = NULL;
   LET v_rt_fec_hst_avis_desast  = NULL;
   LET v_rt_tpo_rechazo          = 0;

   LET v_dm_estado_marca      = 0;
   LET v_dm_marca_causa       = 0;
   LET v_dm_usuario           = p_v_usuario;
   LET v_dm_proceso_cod       = 1208;

   -- se obtiene el identificador de cre acreditado con el id_derechohabiente
   FOREACH
      SELECT FIRST 1 c.id_cre_acreditado, c.edo_procesar
        INTO v_id_cre_acreditado, v_ax_edo_procesar
        FROM cre_acreditado c,
             cat_maq_credito m
       WHERE c.id_derechohabiente = p_id_derechohabiente
         AND c.tpo_originacion    = 2
         AND c.tpo_credito        = v_rt_tpo_credito
         AND c.estado             = m.estado
         AND m.entidad            = 1
       ORDER BY c.f_otorga DESC, c.estado
   END FOREACH;

    -- se valida si fue posible identificar el registro correspondiente en la tabla maestro
   IF v_id_cre_acreditado IS NULL THEN
      -- se asigna el estado de rechazo por No Existe Marca Crédito Vigente
      LET v_ax_sts_registro = 13; 

      ---se asigna el valor al registro de rechazo
      LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET rch_nss                = v_rt_nss;
      LET rch_tpo_originacion    = 2; -- Créditos en Garantía 43 bis
      LET rch_tpo_credito        = v_rt_tpo_credito;
      LET rch_tpo_registro       = v_rt_tpo_registro;
      LET rch_num_credito        = v_rt_num_credito;
      LET rch_sdo_deudor         = v_rt_ssv_92_97/100;
      LET rch_f_otorga           = NULL;
      LET rch_f_culmina          = v_rt_fec_culminacion;
      LET rch_edo_credito        = v_rt_sts_credito;
      LET rch_tpo_dscto          = NULL;
      LET rch_valor_dscto        = 0;
      LET rch_nrp                = v_rt_nrp;
      LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
      LET rch_nss_liberado       = v_rt_nss_liberado;
      LET rch_f_gen_arh          = v_rt_fec_proceso;
      LET rch_sdo_credito        = v_rt_sdo_credito/100;
      LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
      LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
      LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
      LET rch_tpo_rch            = v_rt_tpo_rechazo;
      LET rch_estado             = v_ax_sts_registro;

      -- se inserta en la tabla de rechazos
      INSERT INTO cre_rch_acreditado (
                  id_cre_ctr_archivo,
                  nss,
                  tpo_originacion,
                  tpo_credito,
                  tpo_registro,
                  num_credito,
                  sdo_deudor,
                  f_otorga,
                  f_culmina,
                  edo_credito,
                  tpo_dscto,
                  valor_dscto,
                  nrp,
                  f_ini_dscto,
                  nss_liberado,
                  f_gen_arh,
                  sdo_credito,
                  f_prox_liq,
                  f_desde,
                  f_hasta,
                  tpo_rch,
                  estado)
          VALUES (rch_id_cre_ctr_archivo,
                  rch_nss,
                  rch_tpo_originacion,
                  rch_tpo_credito,
                  rch_tpo_registro,
                  rch_num_credito,
                  rch_sdo_deudor,
                  rch_f_otorga,
                  rch_f_culmina,
                  rch_edo_credito,
                  rch_tpo_dscto,
                  rch_valor_dscto,
                  rch_nrp,
                  rch_f_ini_dscto,
                  rch_nss_liberado,
                  rch_f_gen_arh,
                  rch_sdo_credito,
                  rch_f_prox_liq,
                  rch_f_desde,
                  rch_f_hasta,
                  rch_tpo_rch,
                  rch_estado);

           -- se incrementa el número de registros rechazados
           LET v_rechazo = 1;
   ELSE  ---si existe marca de crédito vigente
      -- se valida que el estado procesar sea mayor o igual a 120 - SALDO TRANSFERIDO
      IF v_ax_edo_procesar < 55 OR v_ax_edo_procesar > 120 THEN
         -- se asignan los valores en las variables que se usarÃ¡n para insertar el registro
         LET v_ax_sts_registro = 160;

         -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
         INSERT INTO safre_tmp:tmp_nss_desmarcados_grt (
                     nss,
                     tpo_credito)
             VALUES (v_rt_nss,
                     v_rt_tpo_credito);
      ELSE
         -- se inicializa la variable
         LET v_ax_estado = NULL;
         LET v_b_desmarcar = 1;

         -- se obtienen los estados de la tabla de Uso de Garantía
         FOREACH
            SELECT estado, edo_procesar
              INTO v_ax_estado, v_ax_edo_procesar
              FROM cre_uso_garantia
             WHERE tpo_transferencia  = v_ax_tpo_transferencia
               AND id_derechohabiente = p_id_derechohabiente
               AND id_cre_ctr_archivo IN (
                   SELECT id_cre_ctr_archivo
                     FROM cre_ctr_archivo
                    WHERE operacion = 18)

            IF v_ax_estado = 140 OR v_ax_estado = 20 THEN
               IF v_ax_edo_procesar >= 10 AND v_ax_edo_procesar <= 85 THEN
                  -- existen registro(s) en Uso de Garantía que no cumplen con la condición. No desmarcar
                  LET v_b_desmarcar = 0;

                  EXIT FOREACH;
               ELSE
                  CONTINUE FOREACH;
               END IF
            ELSE
               CONTINUE FOREACH;
            END IF
         END FOREACH

         IF v_b_desmarcar = 1 THEN
            -- se asignan los valores en las variables que se usaran para insertar el registro
            LET v_ax_sts_registro = 160;

            -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
            INSERT INTO safre_tmp:tmp_nss_desmarcados_agr (
                        nss,
                        tpo_credito)
                VALUES (v_rt_nss,
                        v_rt_tpo_credito);
         ELSE
            -- se asigna rechazo de desmarca
            LET v_ax_sts_registro = 280;
         END IF
      END IF

      -- se inserta en cre his 
      INSERT INTO cre_his_acreditado (
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  tpo_transferencia,
                  edo_procesar,
                  estado,
                  f_proceso)
          VALUES (v_id_cre_acreditado,
                  p_ax_id_cre_ctr_arch,
                  v_ax_tpo_transferencia,
                  v_ax_edo_procesar,
                  v_ax_sts_registro,
                  v_ax_f_proceso);
                  
      --inserta ocg_transaccion_cre
      INSERT INTO ocg_transaccion_cre
                  (subproceso,
                   id_referencia_ocg,
                   id_referencia_cre,
                   periodo_pago,
                   f_proceso)
          VALUES ( 5,
                   p_id_ocg_liquidacion,
                   v_id_cre_acreditado,
                   NULL,
                   TODAY );

      -- se actualizan estados en cre acreditado

      UPDATE cre_acreditado
         SET estado = v_ax_sts_registro
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND id_derechohabiente = p_id_derechohabiente;

     -- se incrementa el contador de registros aceptados
      LET v_aceptado = 1;
   END IF  

   -- desmarca 223 registros liquidados pendientes de conciliación
   FOREACH
      SELECT id_derechohabiente,
             marca,
             n_referencia
        INTO v_dm_id_derechohabiente,
             v_dm_marca_entra,
             v_dm_n_referencia
        FROM sfr_marca_activa
       WHERE id_derechohabiente = p_id_derechohabiente
         AND marca              = 223

      -- se invoca la función de desmarca
      EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                          v_dm_marca_entra,
                                          v_dm_n_referencia,
                                          v_dm_estado_marca,
                                          v_dm_marca_causa,
                                          v_dm_usuario,
                                          v_dm_proceso_cod)
                                     INTO v_ax_cod_error;
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_acreditado;
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;
   UPDATE STATISTICS FOR TABLE ocg_transaccion_cre;

   RETURN v_ax_error, v_aceptado, v_rechazo, v_id_cre_acreditado;

END FUNCTION;


