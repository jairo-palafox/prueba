






CREATE FUNCTION "safreviv".fn_tmp_analsista()
   RETURNING INTEGER, INTEGER, VARCHAR(255)
   -- Registro de rechazos
   DEFINE tmp_nss                CHAR(11);
   DEFINE tmp_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_id_cre_acreditado  DECIMAL(9,0);
   DEFINE tmp_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE tmp_folio_liquida      DECIMAL(9,0);
   DEFINE tmp_tpo_originacion    SMALLINT;
   DEFINE tmp_tpo_credito        SMALLINT;
   DEFINE tmp_tpo_registro       CHAR(2);
   DEFINE tmp_num_credito        DECIMAL(10,0);
   --DEFINE tmp_num97              DECIMAL(14,2);
   --DEFINE tmp_sdo97              DECIMAL(14,2);
   --DEFINE tmp_num92              DECIMAL(14,2);
   --DEFINE tmp_sdo92              DECIMAL(14,2);
   DEFINE tmp_sdo_deudor         DECIMAL(12,2);
   DEFINE tmp_f_otorga           DATE;
   DEFINE tmp_edo_credito        SMALLINT;
   DEFINE tmp_edo_procesar       SMALLINT;
   DEFINE tmp_estado             SMALLINT;
   DEFINE tmp_estatus            SMALLINT;
   -- Registro maestro
   DEFINE cre_id_cre_acreditado  DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE cre_folio_liquida      DECIMAL(9,0);
   DEFINE cre_tpo_originacion    SMALLINT;
   DEFINE cre_tpo_credito        SMALLINT;
   DEFINE cre_tpo_registro       CHAR(2);
   DEFINE cre_num_credito        DECIMAL(10,0);
   DEFINE cre_sdo_deudor         DECIMAL(12,2);
   DEFINE cre_f_otorga           DATE;
   DEFINE cre_edo_credito        SMALLINT;
   DEFINE cre_edo_procesar       SMALLINT;
   DEFINE cre_estado             SMALLINT;
   -- Variables auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_ax_num_registros      SMALLINT;
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
   LET v_ax_error    = 0;
   LET v_error_isam  = 0;
   LET v_mensaje     = "El proceso de temporal de rechazos termino satisfactoriamente";

   -- se procesan los registros de rechazos de acreditado
   FOREACH
   SELECT nss
   INTO tmp_nss
   FROM safre_tmp:tmp_analsista
      -- se busca el id derechohabiente del nss en proceso
      SELECT id_derechohabiente
      INTO v_ax_id_derechohabiente
      FROM safre_viv:afi_derechohabiente
      WHERE nss = tmp_nss;

      SELECT COUNT(*)
      INTO v_ax_num_registros
      FROM safre_viv:cre_acreditado
      WHERE tpo_originacion = 1
      AND id_derechohabiente = v_ax_id_derechohabiente;

      IF v_ax_num_registros > 1 THEN
         LET tmp_id_derechohabiente = v_ax_id_derechohabiente;
         LET tmp_id_cre_acreditado  = NULL;
         LET tmp_id_cre_ctr_archivo = NULL;
         LET tmp_folio_liquida      = NULL;
         LET tmp_tpo_originacion    = NULL;
         LET tmp_tpo_credito        = NULL;
         LET tmp_tpo_registro       = NULL;
         LET tmp_num_credito        = NULL;
         LET tmp_sdo_deudor         = NULL;
         LET tmp_f_otorga           = NULL;
         LET tmp_edo_credito        = NULL;
         LET tmp_edo_procesar       = NULL;
         LET tmp_estado             = NULL;
         LET tmp_estatus            = v_ax_num_registros;
      ELIF v_ax_num_registros = 1 THEN
         SELECT id_cre_acreditado,
                id_cre_ctr_archivo,
                folio_liquida,
                tpo_credito,
                tpo_registro,
                num_credito,
                sdo_deudor,
                f_otorga,
                edo_credito,
                edo_procesar,
                estado
         INTO cre_id_cre_acreditado,
              cre_id_cre_ctr_archivo,
              cre_folio_liquida,
              cre_tpo_credito,
              cre_tpo_registro,
              cre_num_credito,
              cre_sdo_deudor,
              cre_f_otorga,
              cre_edo_credito,
              cre_edo_procesar,
              cre_estado
         FROM safre_viv:cre_acreditado
         WHERE tpo_originacion = 1
         AND id_derechohabiente = v_ax_id_derechohabiente;

         LET tmp_id_derechohabiente = v_ax_id_derechohabiente;
         LET tmp_id_cre_acreditado  = cre_id_cre_acreditado;
         LET tmp_id_cre_ctr_archivo = cre_id_cre_ctr_archivo;
         LET tmp_folio_liquida      = cre_folio_liquida;
         LET tmp_tpo_originacion    = 1;
         LET tmp_tpo_credito        = cre_tpo_credito;
         LET tmp_tpo_registro       = cre_tpo_registro;
         LET tmp_num_credito        = cre_num_credito;
         LET tmp_sdo_deudor         = cre_sdo_deudor;
         LET tmp_f_otorga           = cre_f_otorga;
         LET tmp_edo_credito        = cre_edo_credito;
         LET tmp_edo_procesar       = cre_edo_procesar;
         LET tmp_estado             = cre_estado;
         LET tmp_estatus            = v_ax_num_registros;
      ELSE
         SELECT COUNT(*)
         INTO v_ax_num_registros
         FROM safre_viv:cre_acreditado
         WHERE id_derechohabiente = v_ax_id_derechohabiente;

         IF v_ax_num_registros <> 1 THEN
            LET tmp_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmp_id_cre_acreditado  = NULL;
            LET tmp_id_cre_ctr_archivo = NULL;
            LET tmp_folio_liquida      = NULL;
            LET tmp_tpo_originacion    = NULL;
            LET tmp_tpo_credito        = NULL;
            LET tmp_tpo_registro       = NULL;
            LET tmp_num_credito        = NULL;
            LET tmp_sdo_deudor         = NULL;
            LET tmp_f_otorga           = NULL;
            LET tmp_edo_credito        = NULL;
            LET tmp_edo_procesar       = NULL;
            LET tmp_estado             = 99;
            LET tmp_estatus            = v_ax_num_registros;
         ELSE
            SELECT id_cre_acreditado,
                   id_cre_ctr_archivo,
                   folio_liquida,
                   tpo_originacion,
                   tpo_credito,
                   tpo_registro,
                   num_credito,
                   sdo_deudor,
                   f_otorga,
                   edo_credito,
                   edo_procesar,
                   estado
            INTO cre_id_cre_acreditado,
                 cre_id_cre_ctr_archivo,
                 cre_folio_liquida,
                 cre_tpo_originacion,
                 cre_tpo_credito,
                 cre_tpo_registro,
                 cre_num_credito,
                 cre_sdo_deudor,
                 cre_f_otorga,
                 cre_edo_credito,
                 cre_edo_procesar,
                 cre_estado
            FROM safre_viv:cre_acreditado
            WHERE id_derechohabiente = v_ax_id_derechohabiente;

            LET tmp_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmp_id_cre_acreditado  = cre_id_cre_acreditado;
            LET tmp_id_cre_ctr_archivo = cre_id_cre_ctr_archivo;
            LET tmp_folio_liquida      = cre_folio_liquida;
            LET tmp_tpo_originacion    = cre_tpo_originacion;
            LET tmp_tpo_credito        = cre_tpo_credito;
            LET tmp_tpo_registro       = cre_tpo_registro;
            LET tmp_num_credito        = cre_num_credito;
            LET tmp_sdo_deudor         = cre_sdo_deudor;
            LET tmp_f_otorga           = cre_f_otorga;
            LET tmp_edo_credito        = cre_edo_credito;
            LET tmp_edo_procesar       = cre_edo_procesar;
            LET tmp_estado             = cre_estado;
            LET tmp_estatus            = v_ax_num_registros;
         END IF
      END IF

      -- se inserta registro en la tabla maestro
      UPDATE safre_tmp:tmp_analsista
      SET id_derechohabiente = tmp_id_derechohabiente,
          id_cre_acreditado  = tmp_id_cre_acreditado,
          id_cre_ctr_archivo = tmp_id_cre_ctr_archivo,
          folio_liquida      = tmp_folio_liquida,
          tpo_originacion    = tmp_tpo_originacion,
          tpo_credito        = tmp_tpo_credito,
          tpo_registro       = tmp_tpo_registro,
          num_credito        = tmp_num_credito,
          sdo_deudor         = tmp_sdo_deudor,
          f_otorga           = tmp_f_otorga,
          edo_credito        = tmp_edo_credito,
          edo_procesar       = tmp_edo_procesar,
          estado             = tmp_estado,
          estatus            = tmp_estatus
      WHERE nss = tmp_nss;
   END FOREACH

   RETURN v_ax_error, v_error_isam, v_mensaje;
END FUNCTION;


