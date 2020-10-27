






CREATE FUNCTION "safreviv".fn_integra_recurr_his_marca(p_d_folio DECIMAL(9,0),
                                            p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT;
   -- Registro de cre acreditado
   DEFINE cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE cre_id_derechohabiente   DECIMAL(9,0);
   DEFINE cre_tpo_credito          SMALLINT;
   DEFINE cre_num_credito          DECIMAL(10,0);
   DEFINE cre_f_otorga             DATE;
   DEFINE cre_f_culmina            DATE;
   DEFINE cre_edo_credito          SMALLINT;
   DEFINE cre_edo_procesar         SMALLINT;
   DEFINE cre_estado               SMALLINT;
   -- Registro de cre his acreditado
   DEFINE v_his_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_his_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE v_his_tpo_transferencia  CHAR(2);
   DEFINE v_his_edo_procesar       SMALLINT;
   DEFINE v_his_diagnostico        CHAR(3);
   DEFINE v_his_estado             SMALLINT;
   DEFINE v_his_nss_afore          CHAR(11);
   DEFINE v_his_rfc_afore          CHAR(13);
   DEFINE v_his_paterno_afore      CHAR(40);
   DEFINE v_his_materno_afore      CHAR(40);
   DEFINE v_his_nombre_afore       CHAR(40);
   DEFINE v_his_nom_imss           CHAR(50);
   DEFINE v_his_f_proceso          DATE;
   -- REGISTRO de cta credito
   DEFINE cta_id_derechohabiente   DECIMAL(9,0);
   DEFINE cta_proceso_cod          SMALLINT;
   DEFINE cta_tpo_credito          SMALLINT;
   DEFINE cta_num_credito          DECIMAL(10,0);
   DEFINE cta_f_credito            DATE;
   -- REGISTRO de cta his credito
   DEFINE ctah_id_derechohabiente  DECIMAL(9,0);
   DEFINE ctah_proceso_cod         SMALLINT;
   DEFINE ctah_tpo_credito         SMALLINT;
   DEFINE ctah_num_credito         DECIMAL(10,0);
   DEFINE ctah_f_credito           DATE;
   DEFINE ctah_estado              SMALLINT;
   DEFINE ctah_f_actualiza         DATE;
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
   DEFINE v_ax_tpo_transferencia   CHAR(2);
   DEFINE v_ax_id_proceso          SMALLINT;
   DEFINE v_ax_marca_inf           SMALLINT;
   DEFINE v_ax_excep_error         SMALLINT;

   ON EXCEPTION SET v_ax_excep_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_excep_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrProcMarca.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_excep_error = 0;

   FOREACH
   SELECT id_cre_acreditado,
          id_derechohabiente,
          tpo_credito,
          num_credito,
          f_otorga,
          f_culmina,
          edo_credito,
          edo_procesar,
          estado
     INTO cre_id_cre_acreditado,
          cre_id_derechohabiente,
          cre_tpo_credito,
          cre_num_credito,
          cre_f_otorga,
          cre_f_culmina,
          cre_edo_credito,
          cre_edo_procesar,
          cre_estado
     FROM safre_viv:cre_acreditado
    WHERE id_cre_ctr_archivo = p_ax_id_cre_ctr_arch
      -- se hace la consulta del tipo de transferencia y el id deudor
      SELECT DECODE(id_proceso,201,"03",1201,"16",301,"43"), id_proceso, marca_inf
        INTO v_ax_tpo_transferencia, v_ax_id_proceso, v_ax_marca_inf
        FROM safre_viv:cat_tipo_credito
       WHERE tpo_credito = cre_tpo_credito;

      -- se asignan valores al registro a insertar en his acreditados
      LET v_his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET v_his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET v_his_tpo_transferencia  = v_ax_tpo_transferencia;
      LET v_his_edo_procesar       = cre_edo_procesar;
      LET v_his_diagnostico        = 0;
      LET v_his_estado             = cre_estado;
      LET v_his_nss_afore          = "";
      LET v_his_rfc_afore          = "";
      LET v_his_paterno_afore      = "";
      LET v_his_materno_afore      = "";
      LET v_his_nombre_afore       = "";
      LET v_his_nom_imss           = "";
      LET v_his_f_proceso          = TODAY;

      --se inserta en la tabla his acreditado
      INSERT INTO safre_viv:cre_his_acreditado(
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
          VALUES (v_his_id_cre_acreditado,
                  v_his_id_cre_ctr_archivo,
                  v_his_tpo_transferencia,
                  v_his_edo_procesar,
                  v_his_diagnostico,
                  v_his_estado,
                  v_his_nss_afore,
                  v_his_rfc_afore,
                  v_his_paterno_afore,
                  v_his_materno_afore,
                  v_his_nombre_afore,
                  v_his_nom_imss,
                  v_his_f_proceso);

      -- verifica si el crédito está vigente (001-VIGENTE)
      IF cre_edo_credito = 1 THEN
         -- se asgina los valores al registro de cta credito a insertar
         LET cta_id_derechohabiente = cre_id_derechohabiente;
         LET cta_proceso_cod        = v_ax_id_proceso;
         LET cta_tpo_credito        = cre_tpo_credito;
         LET cta_num_credito        = cre_num_credito;
         LET cta_f_credito          = cre_f_otorga;

         INSERT INTO safre_viv:cta_credito(
                     id_derechohabiente,
                     proceso_cod,
                     tpo_credito,
                     num_credito,
                     f_credito)
              VALUES(cta_id_derechohabiente,
                     cta_proceso_cod,
                     cta_tpo_credito,
                     cta_num_credito,
                     cta_f_credito);
{
         -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
         IF v_ax_tpo_transferencia = "03" THEN
            -- corresponde a Transferencia de Acreditados
            --LET v_ax_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
            LET v_ax_marca_inf = 221; -- Transferencia de Acreditados
         ELIF v_ax_tpo_transferencia = "16" THEN
            -- corresponde a Créditos en Garantía 43 bis (Solicitud de Saldo en Garantía)
            --LET v_ax_marca_procesar = "02"; -- 'grt' => 02 (Apoyo Infornavit)
            LET v_ax_marca_inf = 223; -- para uso de garantía 43 bis
         ELSE
            -- corresponde a Anualidades Garantizadas
            --LET v_ax_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
            LET v_ax_marca_inf = 225; -- Anualidades Garantizadas
         END IF
}
         -- se asgina los valores al registro de sfr marca historica a insertar
         LET sfrh_id_derechohabiente = cre_id_derechohabiente;
         LET sfrh_marca              = v_ax_marca_inf;
         LET sfrh_n_referencia       = cre_id_cre_acreditado;
         LET sfrh_f_inicio           = cre_f_otorga;
         LET sfrh_h_inicio           = "12:00:00";
         LET sfrh_f_fin              = cre_f_culmina;
         LET sfrh_folio              = p_d_folio;
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
         LET sfra_folio              = cre_f_culmina;
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
      END IF;

      -- verifica si el crédito está liquidado (002-LIQUIDADO)
      IF cre_edo_credito = 2 THEN
         -- se asgina los valores al registro de cta credito a insertar
         LET ctah_id_derechohabiente = cre_id_derechohabiente;
         LET ctah_proceso_cod        = v_ax_id_proceso;
         LET ctah_tpo_credito        = cre_tpo_credito;
         LET ctah_num_credito        = cre_num_credito;
         LET ctah_f_credito          = cre_f_otorga;
         LET ctah_estado             = cre_edo_credito;
         LET ctah_f_actualiza        = cre_f_culmina;

         INSERT INTO safre_viv:cta_his_credito(
                     id_derechohabiente,
                     proceso_cod,
                     tpo_credito,
                     num_credito,
                     f_credito,
                     estado,
                     f_actualiza)
              VALUES(ctah_id_derechohabiente,
                     ctah_proceso_cod,
                     ctah_tpo_credito,
                     ctah_num_credito,
                     ctah_f_credito,
                     ctah_estado,
                     ctah_f_actualiza);
      END IF;
   END FOREACH;

   RETURN v_ax_excep_error;
END FUNCTION;


