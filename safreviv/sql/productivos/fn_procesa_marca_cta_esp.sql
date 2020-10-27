






CREATE FUNCTION "safreviv".fn_procesa_marca_cta_esp(p_v_usuario         CHAR(20),
                                        p_d_folio           DECIMAL(9,0),
                                        p_d_id_cre_ctr_arch DECIMAL(9,0),
                                        p_si_proceso_cod    SMALLINT)
   RETURNING SMALLINT;

   -- REGISTRO de cre acreditado
   DEFINE cre_id_cre_acreditado  DECIMAL(9,0);
   DEFINE cre_id_derechohabiente DECIMAL(9,0);
   DEFINE cre_tpo_credito        SMALLINT;
   DEFINE cre_num_credito        DECIMAL(10,0);
   DEFINE cre_f_otorga           DATE;
   DEFINE cre_edo_procesar       SMALLINT;
   DEFINE cre_tpo_originacion    SMALLINT;
   DEFINE cre_edo_credito        SMALLINT;
   DEFINE cre_nvo_edo            SMALLINT;
   DEFINE cre_tpo_registro       CHAR(2);

   -- Variables auxiliares
   DEFINE v_ax_marca_inf         SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc         SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_transferencia CHAR(2);  -- tipo de transferencia
   DEFINE v_ax_id_credito        SMALLINT; -- identificador del crédito
   DEFINE v_ax_cod_error         SMALLINT; -- contiene código error (retorno de la función externa)
   DEFINE v_ax_intento           SMALLINT; -- intento
   DEFINE v_ax_situacion         SMALLINT; -- situación
   DEFINE v_ax_marca_procesar    CHAR(2); -- marca procesar
   DEFINE v_ax_f_solicita        DATE; -- fecha solicita
   DEFINE v_ax_fecha_causa       DATE; -- fecha causa
   DEFINE v_ax_codigo_rechazo    SMALLINT; -- código de rechazo
   DEFINE v_ax_estado_marca      SMALLINT; -- estado marca
   DEFINE v_ax_marca_activa      SMALLINT; -- marca activa
   DEFINE v_ax_marca_causa       SMALLINT; -- marca causa
   DEFINE v_ax_marca_entra       SMALLINT; -- marca entra
   DEFINE v_ax_marca_ws          SMALLINT; -- marca ws
   DEFINE v_ax_sts_marcaje       SMALLINT; -- estatus de retorno de la función
   DEFINE v_ax_marcaje           SMALLINT;

   --DEFINE v_ax_marca_entra_ext   SMALLINT; -- marca entra extra que se inserta (223 GRT)
   DEFINE v_ax_excep_error       SMALLINT; -- contiene código error (ocurrido en el proceso)}
   DEFINE v_ax_nss               CHAR(11);

   ON EXCEPTION SET v_ax_excep_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_excep_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrProcMarca.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_excep_error    = 0;
   LET v_ax_situacion      = 2;
   LET v_ax_intento        = 1;
   LET v_ax_f_solicita     = TODAY;
   LET v_ax_fecha_causa    = "";
   LET v_ax_codigo_rechazo = 0;
   LET v_ax_estado_marca   = 0;
   LET v_ax_marca_causa    = NULL;
   LET cre_edo_credito     = NULL;
   LET cre_nvo_edo         = NULL;
   LET cre_tpo_registro    = NULL;
   LET v_ax_marcaje        = 0;

   --se consultan los registros de cre acreditado
   FOREACH
    SELECT id_cre_acreditado,
           id_derechohabiente,
           tpo_credito,
           num_credito,
           f_otorga,
           edo_procesar,
           edo_credito,
           tpo_registro,
           tpo_originacion
      INTO cre_id_cre_acreditado,
           cre_id_derechohabiente,
           cre_tpo_credito,
           cre_num_credito,
           cre_f_otorga,
           cre_edo_procesar,
           cre_edo_credito,
           cre_tpo_registro,
           cre_tpo_originacion
      FROM cre_acreditado
     WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch
       AND estado = 10
       AND edo_credito = 1

      -- se obtiene las marcas y el tipo de transferencia para el tipo de credito en proceso
      SELECT FIRST 1 marca_inf, marca_prc, DECODE(id_proceso,201,"03",1201, "16", 301,"43")
        INTO v_ax_marca_entra, v_ax_marca_ws, v_ax_tpo_transferencia
        FROM safre_viv:cat_tipo_credito
       WHERE tpo_originacion = cre_tpo_originacion
         AND tpo_credito = cre_tpo_credito;

      -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
      IF v_ax_tpo_transferencia = "03" THEN
         -- corresponde a Transferencia de Acreditados
         LET v_ax_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
         --LET v_ax_marca_entra_ext = 221; -- Transferencia de Acreditados
      ELIF v_ax_tpo_transferencia = "16" THEN
         -- corresponde a Créditos en Garantía 43 bis (Solicitud de Saldo en Garantía)
         LET v_ax_marca_procesar = "02"; -- 'grt' => 02 (Apoyo Infornavit)
         --LET v_ax_marca_entra_ext = 223; -- para uso de garantía 43 bis
      ELSE
         -- corresponde a Anualidades Garantizadas
         LET v_ax_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
         --LET v_ax_marca_entra_ext = 225; -- Anualidades Garantizadas
      END IF

      IF (cre_tpo_registro = "20" AND
         NOT EXISTS ( SELECT id_derechohabiente
                        FROM sfr_marca_activa
                       WHERE id_derechohabiente = cre_id_derechohabiente
                         AND marca = v_ax_marca_entra
                    )) OR cre_tpo_registro = "01" THEN
         -- se ejecuta la función de marcaje
         EXECUTE FUNCTION fn_marca_cuenta(cre_id_derechohabiente,
                                          v_ax_marca_entra,
                                          cre_id_cre_acreditado,
                                          p_d_folio,
                                          v_ax_estado_marca,
                                          v_ax_codigo_rechazo,
                                          v_ax_marca_causa,
                                          v_ax_fecha_causa,
                                          p_v_usuario,
                                          p_si_proceso_cod)
                                     INTO v_ax_sts_marcaje;

         -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
         IF v_ax_sts_marcaje = 0 THEN
            --nuevo estado de acuerdo al estado del credito
            IF cre_edo_credito = 1 THEN
               LET cre_nvo_edo = 20;
            ELIF cre_edo_credito = 3 THEN
               LET cre_nvo_edo = 18;
            END IF

            -- se actualiza el estado en cre acreditado y cre his acreditado
            UPDATE cre_acreditado
               SET estado = cre_nvo_edo
            WHERE id_cre_acreditado = cre_id_cre_acreditado;

            UPDATE cre_his_acreditado
               SET estado = cre_nvo_edo
            WHERE id_cre_acreditado = cre_id_cre_acreditado;
         ELSE
            -- si el estado de retorno de marcaje es nulo se asigna cero
            IF v_ax_sts_marcaje IS NULL THEN
               LET v_ax_sts_marcaje = 0;
            ELIF v_ax_sts_marcaje < 0 THEN
               FOREACH
               SELECT FIRST 1 c.marca_activa,
                      c.rch_cod
                 INTO v_ax_marca_activa,
                      v_ax_sts_marcaje
                 FROM sfr_convivencia c, 
                      sfr_marca_activa a
                WHERE a.id_derechohabiente = cre_id_derechohabiente
                  AND a.marca        = c.marca_activa
                  AND c.marca_entra  = v_ax_marca_entra
                  AND c.rch_cod  > 0  
                ORDER BY a.f_inicio desc
               END FOREACH;
               --LET v_ax_sts_marcaje = v_ax_sts_marcaje * -1;
            END IF

            -- en caso de ser mayor a cero
            IF v_ax_sts_marcaje > 999 THEN
               LET v_ax_sts_marcaje = 150;
            END IF

            --nuevo estado de acuerdo al estado del crédito
            IF cre_edo_credito = 1 THEN
               LET cre_nvo_edo = 150;
            ELIF cre_edo_credito = 3 THEN
               LET cre_nvo_edo = 155;
            END IF

            --se actualiza el estado en cre acreditado y cre his acreditado
            UPDATE cre_acreditado
               SET estado = cre_nvo_edo
             WHERE id_cre_acreditado = cre_id_cre_acreditado;

             --Obtiene el valor del nss
             SELECT nss
               INTO v_ax_nss
               FROM afi_derechohabiente
              WHERE id_derechohabiente = cre_id_derechohabiente;

            --Se integra inserción en la tabla de rechazos de marca
            INSERT INTO safre_tmp:tmp_marca_rechazo(nss,
                         tpo_credito,
                         estado)
                 VALUES (v_ax_nss,
                         cre_tpo_credito,
                         2);

            UPDATE cre_his_acreditado
               SET estado = cre_nvo_edo,
                   diagnostico = v_ax_sts_marcaje
             WHERE id_cre_acreditado = cre_id_cre_acreditado;
         END IF
      END IF

      LET cre_edo_credito     = NULL;
      LET cre_nvo_edo         = NULL;
      LET cre_tpo_registro    = NULL;
      LET v_ax_marcaje        = 0;

   END FOREACH

   RETURN v_ax_excep_error;

END FUNCTION;


