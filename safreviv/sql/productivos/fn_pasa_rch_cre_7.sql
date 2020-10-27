






CREATE FUNCTION "safreviv".fn_pasa_rch_cre_7()
   RETURNING INTEGER, INTEGER, VARCHAR(255)
   -- Registro de rechazos
   DEFINE rch_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE rch_nss                CHAR(11);
   DEFINE rch_tpo_originacion    SMALLINT;
   DEFINE rch_tpo_credito        SMALLINT;
   DEFINE rch_tpo_registro       CHAR(2);
   DEFINE rch_num_credito        DECIMAL(10,0);
   DEFINE rch_sdo_deudor         DECIMAL(12,2);
   DEFINE rch_f_otorga           DATE;
   DEFINE rch_f_culmina          DATE;
   DEFINE rch_edo_credito        SMALLINT;
   DEFINE rch_tpo_dscto          SMALLINT;
   DEFINE rch_valor_dscto        DECIMAL(8,4);
   DEFINE rch_nrp                CHAR(11);
   DEFINE rch_f_ini_dscto        DATE;
   DEFINE rch_nss_liberado       CHAR(11);
   DEFINE rch_f_gen_arh          DATE;
   DEFINE rch_sdo_credito        DECIMAL(12,2);
   DEFINE rch_f_prox_liq         DATE;
   DEFINE rch_f_desde            DATE;
   DEFINE rch_f_hasta            DATE;
   DEFINE rch_tpo_rch            SMALLINT;
   DEFINE rch_estado             SMALLINT;
   -- Registro maestro
   DEFINE cre_id_cre_acreditado  DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE cre_folio_liquida      DECIMAL(9,0);
   DEFINE cre_id_derechohabiente DECIMAL(9,0);
   DEFINE cre_tpo_originacion    SMALLINT;
   DEFINE cre_tpo_credito        SMALLINT;
   DEFINE cre_tpo_registro       CHAR(2);
   DEFINE cre_num_credito        DECIMAL(10,0);
   DEFINE cre_sdo_deudor         DECIMAL(12,2);
   DEFINE cre_f_otorga           DATE;
   DEFINE cre_f_culmina          DATE;
   DEFINE cre_edo_credito        SMALLINT;
   DEFINE cre_tpo_dscto          SMALLINT;
   DEFINE cre_valor_dscto        DECIMAL(8,4);
   DEFINE cre_nrp                CHAR(11);
   DEFINE cre_f_ini_dscto        DATE;
   DEFINE cre_nss_liberado       CHAR(11);
   DEFINE cre_f_gen_arh          DATE;
   DEFINE cre_sdo_credito        DECIMAL(12,2);
   DEFINE cre_f_prox_liq         DATE;
   DEFINE cre_f_desde            DATE;
   DEFINE cre_f_hasta            DATE;
   DEFINE cre_tpo_rch            SMALLINT;
   DEFINE cre_edo_procesar       SMALLINT;
   DEFINE cre_estado             SMALLINT;
   -- Registro de historicos
   DEFINE his_id_cre_acreditado  DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE his_tpo_transferencia  CHAR(2);
   DEFINE his_edo_procesar       SMALLINT;
   DEFINE his_diagnostico        CHAR(3);
   DEFINE his_estado             SMALLINT;
   DEFINE his_nss_afore          CHAR(11);
   DEFINE his_rfc_afore          CHAR(13);
   DEFINE his_paterno_afore      CHAR(40);
   DEFINE his_materno_afore      CHAR(40);
   DEFINE his_nombre_afore       CHAR(40);
   DEFINE his_nom_imss           CHAR(50);
   DEFINE his_f_proceso          DATE;
   -- Registro de saldo deudor
   DEFINE sdo_id_cre_acreditado  DECIMAL(9,0);
   DEFINE sdo_folio_referencia   DECIMAL(9,0);
   DEFINE sdo_f_movimiento       DATE;
   DEFINE sdo_movimiento         SMALLINT;
   DEFINE sdo_id_referencia      DECIMAL(9,0);
   DEFINE sdo_monto_aivs         DECIMAL(16,6);
   DEFINE sdo_monto_pesos        DECIMAL(12,2);
   DEFINE sdo_f_proceso          DATE;
   -- REGISTRO de sfr marca historica
   DEFINE sfrh_id_derechohabiente  DECIMAL(9,0);
   DEFINE sfrh_marca               SMALLINT;
   DEFINE sfrh_n_referencia        DECIMAL(9,0);
   DEFINE sfrh_f_inicio            DATE;
   DEFINE sfrh_h_inicio            DATETIME HOUR TO SECOND;
   DEFINE sfrh_f_fin               DATE;
   DEFINE sfrh_folio               DECIMAL(9,0);
   DEFINE sfrh_estado_marca        SMALLINT;
   DEFINE sfrh_rch_cod             SMALLINT;
   DEFINE sfrh_marca_causa         SMALLINT;
   DEFINE sfrh_f_marca_causa       DATE;
   DEFINE sfrh_f_vigencia          DATE;
   DEFINE sfrh_usuario_marca       CHAR(20);
   DEFINE sfrh_usuario_desmarca    CHAR(20);
   -- REGISTRO de sfr marca activa
   DEFINE sfra_id_derechohabiente  DECIMAL(9,0);
   DEFINE sfra_marca               SMALLINT;
   DEFINE sfra_n_referencia        DECIMAL(9,0);
   DEFINE sfra_f_inicio            DATE;
   DEFINE sfra_h_inicio            DATETIME HOUR TO SECOND;
   DEFINE sfra_folio               DECIMAL(9,0);
   DEFINE sfra_marca_causa         SMALLINT;
   DEFINE sfra_f_marca_causa       DATE;
   DEFINE sfra_f_vigencia          DATE;
   DEFINE sfra_usuario_marca       CHAR(20);
   -- Variables auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_ax_tpo_transferencia  CHAR(2);
   DEFINE v_ax_id_deudor          SMALLINT;
   DEFINE v_ax_marca_inf          SMALLINT;
   DEFINE v_ax_folio_archivo      DECIMAL(9,0);
   -- control de excepciones 
   DEFINE v_ax_error              INTEGER; -- codigo de error SQL
   DEFINE v_error_isam            INTEGER; -- codigo de error ISAM
   DEFINE v_mensaje               VARCHAR(255); -- mensaje de error

   ON EXCEPTION SET v_ax_error, v_error_isam, v_mensaje
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_ax_error, v_error_isam, v_mensaje;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrCtrArch_devol.trace';
   --TRACE ON;

   -- se incializan variables
   LET v_ax_error = 0;
   LET v_error_isam  = 0;
   LET v_mensaje     = "El proceso de rechazos termino satisfactoriamente";

   -- se procesan los registros de rechazos de acreditado
   FOREACH
   SELECT *
   INTO rch_id_cre_ctr_archivo,
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
        rch_estado
   FROM safre_viv:cre_rch_acreditado
   WHERE nss IN ("55048100303", "13866652889", "55007303856", "32805403535",
                 "33785905796", "11664727192", "65725601515")
   AND estado = 14
      -- se busca el id derechohabiente del nss en proceso
      SELECT id_derechohabiente
      INTO v_ax_id_derechohabiente
      FROM afi_derechohabiente
      WHERE nss = rch_nss;

      -- se asignan los valores del registro a insertar
      LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
      LET cre_id_cre_ctr_archivo = rch_id_cre_ctr_archivo;
      LET cre_folio_liquida      = 0;
      LET cre_id_derechohabiente = v_ax_id_derechohabiente;
      LET cre_tpo_originacion    = rch_tpo_originacion;
      LET cre_tpo_credito        = rch_tpo_credito;
      LET cre_tpo_registro       = rch_tpo_registro;
      LET cre_num_credito        = rch_num_credito;
      LET cre_sdo_deudor         = rch_sdo_deudor;
      LET cre_f_otorga           = rch_f_otorga;
      LET cre_f_culmina          = rch_f_culmina;
      LET cre_edo_credito        = rch_edo_credito;
      LET cre_tpo_dscto          = rch_tpo_dscto;
      LET cre_valor_dscto        = rch_valor_dscto;
      LET cre_nrp                = rch_nrp;
      LET cre_f_ini_dscto        = rch_f_ini_dscto;
      LET cre_nss_liberado       = rch_nss_liberado;
      LET cre_f_gen_arh          = rch_f_gen_arh;
      LET cre_sdo_credito        = rch_sdo_credito;
      LET cre_f_prox_liq         = rch_f_prox_liq;
      LET cre_f_desde            = rch_f_desde;
      LET cre_f_hasta            = rch_f_hasta;
      LET cre_tpo_rch            = rch_tpo_rch;
      LET cre_edo_procesar       = 60;
      LET cre_estado             = 140;

      -- se inserta registro en la tabla maestro
      INSERT INTO safre_viv:cre_acreditado (
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  folio_liquida,
                  id_derechohabiente,
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
                  edo_procesar,
                  estado)
          VALUES (cre_id_cre_acreditado,
                  cre_id_cre_ctr_archivo,
                  cre_folio_liquida,
                  cre_id_derechohabiente,
                  cre_tpo_originacion,
                  cre_tpo_credito,
                  cre_tpo_registro,
                  cre_num_credito,
                  cre_sdo_deudor,
                  cre_f_otorga,
                  cre_f_culmina,
                  cre_edo_credito,
                  cre_tpo_dscto,
                  cre_valor_dscto,
                  cre_nrp,
                  cre_f_ini_dscto,
                  cre_nss_liberado,
                  cre_f_gen_arh,
                  cre_sdo_credito,
                  cre_f_prox_liq,
                  cre_f_desde,
                  cre_f_hasta,
                  cre_tpo_rch,
                  cre_edo_procesar,
                  cre_estado);

      -- se hace la consulta del tipo de transferencia y el id deudor
      SELECT DECODE(id_proceso,201,"03",301,"43"), id_deudor, marca_inf
        INTO v_ax_tpo_transferencia, v_ax_id_deudor, v_ax_marca_inf
        FROM safre_viv:cat_tipo_credito
       WHERE tpo_credito = cre_tpo_credito;

      -- se asignan los valores del registro historico
      LET his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = cre_id_cre_ctr_archivo;
      LET his_tpo_transferencia  = v_ax_tpo_transferencia;
      LET his_edo_procesar       = 60;
      LET his_diagnostico        = 0;
      LET his_estado             = 140;
      LET his_nss_afore          = "";
      LET his_rfc_afore          = "";
      LET his_paterno_afore      = "";
      LET his_materno_afore      = "";
      LET his_nombre_afore       = "";
      LET his_nom_imss           = "";
      LET his_f_proceso          = TODAY;
      
      -- se insert el registo historico
      INSERT INTO safre_viv:cre_his_acreditado (
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  tpo_transferencia,
                  edo_procesar,
                  diagnostico,
                  estado,
                  nss_afore,
                  rfc_afore,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  f_proceso)
          VALUES (his_id_cre_acreditado,
                  his_id_cre_ctr_archivo,
                  his_tpo_transferencia,
                  his_edo_procesar,
                  his_diagnostico,
                  his_estado,
                  his_nss_afore,
                  his_rfc_afore,
                  his_paterno_afore,
                  his_materno_afore,
                  his_nombre_afore,
                  his_nom_imss,
                  his_f_proceso);

      -- se inicializa el folio del archivo
      LET v_ax_folio_archivo = NULL;

      -- se obtiene el folio del archivo para el identificador en proceso
      SELECT folio_archivo
      INTO v_ax_folio_archivo
      FROM cre_ctr_archivo
      WHERE id_cre_ctr_archivo = rch_id_cre_ctr_archivo;

      -- se valida el folio del archivo
      IF v_ax_folio_archivo IS NULL THEN
         LET v_ax_folio_archivo = 0;
      END IF

      -- si el id deudor es 1 indica que se inserta en la tabla de saldo deudor
      IF v_ax_id_deudor = 1 THEN
         -- se asignan valores al registro a insertar en saldo deudor
         LET sdo_id_cre_acreditado = cre_id_cre_acreditado;
         LET sdo_folio_referencia  = v_ax_folio_archivo;
         LET sdo_f_movimiento      = TODAY;
         LET sdo_movimiento        = 181;
         LET sdo_id_referencia     = cre_id_cre_acreditado;
         LET sdo_monto_aivs        = 0;
         LET sdo_monto_pesos       = cre_sdo_deudor;
         LET sdo_f_proceso         = TODAY;

         -- se inserta en la tabla cre deudor
         INSERT INTO safre_viv:cre_saldo_deudor(
                     id_cre_acreditado,
                     folio_referencia,
                     f_movimiento,
                     movimiento,
                     id_referencia,
                     monto_aivs,
                     monto_pesos,
                     f_proceso)
             VALUES (sdo_id_cre_acreditado,
                     sdo_folio_referencia,
                     sdo_f_movimiento,
                     sdo_movimiento,
                     sdo_id_referencia,
                     sdo_monto_aivs,
                     sdo_monto_pesos,
                     sdo_f_proceso);
      END IF

      -- verifica si el crédito está vigente (001-VIGENTE)
      IF cre_edo_credito = 1 THEN
         -- se asgina los valores al registro de sfr marca historica a insertar
         LET sfrh_id_derechohabiente = cre_id_derechohabiente;
         LET sfrh_marca              = v_ax_marca_inf;
         LET sfrh_n_referencia       = cre_id_cre_acreditado;
         LET sfrh_f_inicio           = cre_f_otorga;
         LET sfrh_h_inicio           = "12:00:00";
         LET sfrh_f_fin              = cre_f_culmina;
         LET sfrh_folio              = v_ax_folio_archivo;
         LET sfrh_estado_marca       = 0;
         LET sfrh_rch_cod            = 0;
         LET sfrh_marca_causa        = NULL;
         LET sfrh_f_marca_causa      = NULL;
         LET sfrh_f_vigencia         = cre_f_otorga;
         LET sfrh_usuario_marca      = "infonavit";
         LET sfrh_usuario_desmarca   = NULL;

         INSERT INTO safre_viv:sfr_marca_historica(
                     id_derechohabiente,
                     marca,
                     n_referencia,
                     f_inicio,
                     h_inicio,
                     f_fin,
                     folio,
                     estado_marca,
                     rch_cod,
                     marca_causa,
                     f_marca_causa,
                     f_vigencia,
                     usuario_marca,
                     usuario_desmarca)
              VALUES(sfrh_id_derechohabiente,
                     sfrh_marca,
                     sfrh_n_referencia,
                     sfrh_f_inicio,
                     sfrh_h_inicio,
                     sfrh_f_fin,
                     sfrh_folio,
                     sfrh_estado_marca,
                     sfrh_rch_cod,
                     sfrh_marca_causa,
                     sfrh_f_marca_causa,
                     sfrh_f_vigencia,
                     sfrh_usuario_marca,
                     sfrh_usuario_desmarca);

         -- se asgina los valores al registro de sfr marca activa a insertar
         LET sfra_id_derechohabiente = cre_id_derechohabiente;
         LET sfra_marca              = v_ax_marca_inf;
         LET sfra_n_referencia       = cre_id_cre_acreditado;
         LET sfra_f_inicio           = cre_f_otorga;
         LET sfra_h_inicio           = "12:00:00";
         LET sfra_folio              = v_ax_folio_archivo;
         LET sfra_marca_causa        = NULL;
         LET sfra_f_marca_causa      = NULL;
         LET sfra_f_vigencia         = cre_f_otorga;
         LET sfra_usuario_marca      = "infonavit";

         INSERT INTO safre_viv:sfr_marca_activa(
                     id_derechohabiente,
                     marca,
                     n_referencia,
                     f_inicio,
                     h_inicio,
                     folio,
                     marca_causa,
                     f_marca_causa,
                     f_vigencia,
                     usuario_marca)
              VALUES(sfra_id_derechohabiente,
                     sfra_marca,
                     sfra_n_referencia,
                     sfra_f_inicio,
                     sfra_h_inicio,
                     sfra_folio,
                     sfra_marca_causa,
                     sfra_f_marca_causa,
                     sfra_f_vigencia,
                     sfra_usuario_marca);
      END IF
   END FOREACH

   RETURN v_ax_error, v_error_isam, v_mensaje;
END FUNCTION;


