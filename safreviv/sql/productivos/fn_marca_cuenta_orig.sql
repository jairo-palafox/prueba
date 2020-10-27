






CREATE FUNCTION "safreviv".fn_marca_cuenta_orig(p_v_usuario         CHAR(20),
                                     p_d_folio           DECIMAL(9,0),
                                     p_d_id_cre_ctr_arch DECIMAL(9,0),
                                     p_si_proceso_cod    SMALLINT,
                                     p_id_cre_acreditado DECIMAL(9,0),
                                     p_id_ocg_formaliza  DECIMAL(9,0),
                                     p_id_ocg_tramite    DECIMAL(9,0))
   RETURNING SMALLINT;

   -- REGISTRO de cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_tpo_credito           SMALLINT;
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_f_otorga              DATE;
   DEFINE cre_edo_procesar          SMALLINT;
   DEFINE cre_tpo_originacion       SMALLINT;
   DEFINE cre_edo_credito           SMALLINT;
   DEFINE cre_nvo_edo               SMALLINT;
   DEFINE cre_tpo_registro          CHAR(2);
   DEFINE cre_estado                SMALLINT;
   DEFINE v_marca                   SMALLINT;  -- variable para marca  

   -- Variables auxiliares
   DEFINE v_ax_marca_inf            SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc            SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_transferencia    CHAR(2);  -- tipo de transferencia
   DEFINE v_ax_id_credito           SMALLINT; -- identificador del crédito
   DEFINE v_ax_cod_error            SMALLINT; -- contiene código error (retorno de la función externa)
   DEFINE v_ax_intento              SMALLINT; -- intento
   DEFINE v_ax_situacion            SMALLINT; -- situación
   DEFINE v_ax_marca_procesar       CHAR(2);  -- marca procesar
   DEFINE v_ax_f_marca_info         DATE;     -- fecha marca 
   DEFINE v_ax_f_solicita           DATE;     -- fecha solicita
   DEFINE v_ax_fecha_causa          DATE;     -- fecha causa
   DEFINE v_ax_codigo_rechazo       SMALLINT; -- código de rechazo
   DEFINE v_ax_estado_marca         SMALLINT; -- estado marca
   DEFINE v_ax_marca_activa         SMALLINT; -- marca activa
   DEFINE v_ax_marca_causa          SMALLINT; -- marca causa
   DEFINE v_ax_marca_entra          SMALLINT; -- marca entra
   DEFINE v_ax_marca_ws             SMALLINT; -- marca ws
   DEFINE v_ax_sts_marcaje          SMALLINT; -- estatus de retorno de la función
   DEFINE v_ax_marcaje              SMALLINT;
   DEFINE v_situacion               SMALLINT;
   DEFINE r_cod_rechazo             INTEGER; -- estatus de retorno de la función
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0);        
	 DEFINE	v_ax_marca                SMALLINT;
	 DEFINE	v_ax_n_referencia         DECIMAL(9,0);
   DEFINE v_ax_excep_error          SMALLINT; -- contiene código error (ocurrido en el proceso)}
   DEFINE v_ax_nss                  CHAR(11);
   --variables desmarca 221,223,225
   DEFINE v_marca_peticion          SMALLINT;
   DEFINE v_n_referencia            DECIMAL(9,0);
   DEFINE v_marca_origen            SMALLINT;
   DEFINE v_folio                   DECIMAL(9,0);

   ON EXCEPTION SET v_ax_excep_error
      -- Devolverá el código de error que ocasione la excepción
      RETURN v_ax_excep_error;
   END EXCEPTION

--   SET DEBUG FILE TO '/safreviv_int/archivos/grtProcMarca.trace';
--   TRACE ON;

   -- se inicializan variables
   LET v_ax_excep_error    = 0;
   LET v_ax_situacion      = 2;
   LET v_ax_intento        = 1;
   LET v_ax_f_marca_info   = TODAY;
   LET v_ax_f_solicita     = TODAY;
   LET v_ax_fecha_causa    = "";
   LET v_ax_codigo_rechazo = 0;
   LET v_ax_estado_marca   = 0;
   LET v_ax_marca_causa    = NULL;
   LET cre_edo_credito     = NULL;
   LET cre_nvo_edo         = NULL;
   LET cre_tpo_registro    = NULL;
   LET v_ax_marcaje        = 0;
   LET v_situacion         = 70;
   LET v_n_referencia      = NULL;
   LET v_marca_origen      = NULL;
   LET v_folio             = NULL;
   LET r_cod_rechazo       = 0;

  --se consultan los registros de cre acreditado
   FOREACH
    SELECT c.id_cre_acreditado,
           c.id_derechohabiente,
           c.tpo_credito,
           c.num_credito,
           c.f_otorga,
           c.edo_procesar,
           c.edo_credito,
           c.tpo_registro,
           c.tpo_originacion,
           c.estado,
           a.nss
      INTO cre_id_cre_acreditado,
           cre_id_derechohabiente,
           cre_tpo_credito,
           cre_num_credito,
           cre_f_otorga,
           cre_edo_procesar,
           cre_edo_credito,
           cre_tpo_registro,
           cre_tpo_originacion,
           cre_estado,
           v_ax_nss
      FROM cre_acreditado c,
           afi_derechohabiente a
     WHERE c.id_cre_acreditado  = p_id_cre_acreditado
       AND c.id_derechohabiente = a.id_derechohabiente
       AND c.id_cre_ctr_archivo = p_d_id_cre_ctr_arch
       AND c.edo_credito        = 1

      -- se obtiene las marcas y el tipo de transferencia para el tipo de crédito en proceso
      SELECT FIRST 1 marca_inf, marca_prc, DECODE(id_proceso,201,"03",1201, "16",301,"43")
        INTO v_ax_marca_entra, v_ax_marca_ws, v_ax_tpo_transferencia
        FROM cat_tipo_credito
       WHERE tpo_originacion = cre_tpo_originacion
         AND tpo_credito     = cre_tpo_credito;

      -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
      IF v_ax_tpo_transferencia = "03" THEN
         -- corresponde a Transferencia de Acreditados
         LET v_ax_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
      ELIF v_ax_tpo_transferencia = "16" THEN
         -- corresponde a Créditos en Garantía 43 bis (Solicitud de Saldo en Garantía)
         LET v_ax_marca_procesar = "02"; -- 'grt' => 02 (Apoyo Infornavit)
      ELSE
         -- corresponde a Anualidades Garantizadas
         LET v_ax_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
      END IF

       -- Desmarca las marcas 206
        FOREACH 
           SELECT s.id_derechohabiente,
                  s.marca,
                  s.n_referencia
             INTO v_ax_id_derechohabiente,
                  v_ax_marca,             
                  v_ax_n_referencia       
             FROM sfr_marca_activa s
            WHERE s.id_derechohabiente = cre_id_derechohabiente
              AND s.marca = 206

           EXECUTE FUNCTION fn_desmarca_cuenta (cre_id_derechohabiente,
                                                v_ax_marca,
                                                v_ax_n_referencia,
                                                0,
                                                0,
                                                p_v_usuario,
                                                p_si_proceso_cod)
                                           INTO r_cod_rechazo;
         END FOREACH;

      IF (cre_tpo_registro = "20" AND
         NOT EXISTS ( SELECT id_derechohabiente
                        FROM sfr_marca_activa
                       WHERE id_derechohabiente = cre_id_derechohabiente
                         AND marca = v_ax_marca_entra
                    )) OR cre_tpo_registro = "01" THEN

         -- Adecuación EAS
         -- Antes de marcar se desmarcan la 221, 233, 223 (Marcas de petición de saldos)
         -- Ya que estas marcas impiden la nueva originación por convivencia.
         LET v_marca_peticion = NULL;

         -- Evalúa la petición a conciliar dependiendo del tipo de originación
         -- APOYO INFONAVIT
         IF(cre_tpo_originacion = 2) THEN
            LET v_marca_peticion = 223;
         ELSE
            -- CREDITOS COFINANCIADOS
            IF(cre_tpo_originacion = 4) THEN
               LET v_marca_peticion = 225;
            ELSE
               -- CREDITOS TRADICIONALES
               IF(cre_tpo_originacion = 1) THEN
                  LET v_marca_peticion = NULL;   --221 (No se puede hacer esta solicitud, hasta nuevo aviso)
               END IF
            END IF
         END IF

         FOREACH
            SELECT n_referencia,
                   marca,
                   folio
              INTO v_n_referencia,
                   v_marca_origen,
                   v_folio
              FROM sfr_marca_activa
             WHERE id_derechohabiente = cre_id_derechohabiente
               AND marca IN (221,223,225)

            -- Desmarca la petición de saldo
            EXECUTE FUNCTION fn_desmarca_cuenta(cre_id_derechohabiente,
                                                v_marca_origen,
                                                v_n_referencia,
                                                0,
                                                0,
                                                p_v_usuario,
                                                p_si_proceso_cod) -- 1229 Generación de marca crédito 43Bis
                                           INTO r_cod_rechazo;

            IF(r_cod_rechazo = 0) THEN
               INSERT INTO cre_marca_conciliacion(
                           id_referencia     ,
                           id_derechohabiente,
                           id_proceso        ,
                           marca_ini         ,
                           folio_marca       ,
                           tpo_originar      ,
                           marca_fin         ,
                           estado            ,
                           usuario           ,
                           f_proceso)
                    VALUES(v_n_referencia         ,
                           cre_id_derechohabiente ,
                           p_si_proceso_cod       ,
                           v_marca_origen         ,
                           v_folio                ,
                           cre_tpo_originacion    ,
                           v_marca_peticion       ,
                           NULL                   ,   -- El estado se actualiza
                           p_v_usuario            ,
                           TODAY);
            END IF
         END FOREACH

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

         -- verifica el resultado de la marca interna
         IF v_ax_sts_marcaje = 0 THEN
            --nuevo estado de acuerdo al estado del credito
            IF cre_edo_credito = 1 THEN
               LET cre_nvo_edo = 20;
            ELIF cre_edo_credito = 3 THEN
               LET cre_nvo_edo = 18;
            END IF

            -- si el estado es 140 no se actualiza  HFJ
            IF cre_nvo_edo = 20 THEN
               -- se actualiza el estado en cre acreditado y cre his acreditado
               UPDATE cre_acreditado
                  SET estado           = cre_nvo_edo
               WHERE id_cre_acreditado = cre_id_cre_acreditado;

               UPDATE cre_his_acreditado
                  SET estado           = cre_nvo_edo
               WHERE id_cre_acreditado = cre_id_cre_acreditado;

               UPDATE cre_marca_conciliacion
                  SET estado = cre_nvo_edo
                WHERE id_derechohabiente = cre_id_derechohabiente
                  AND id_proceso = p_si_proceso_cod
                  AND f_proceso  = TODAY;

               --Si es un registro 43 bis se actualiza información de OCG
               IF cre_tpo_credito = 2 THEN
                  UPDATE ocg_acreditado
                     SET f_marca_infonavit    = v_ax_f_marca_info,
                         f_solic_marca_prcr   = v_ax_f_solicita,
                         situacion            = v_situacion
                   WHERE id_ocg_formalizacion = p_id_ocg_formaliza;

                  UPDATE ocg_formalizacion
                     SET situacion            = v_situacion
                   WHERE id_ocg_formalizacion = p_id_ocg_formaliza;

                  UPDATE ocg_tramite
                     SET situacion      = v_situacion
                   WHERE id_ocg_tramite = p_id_ocg_tramite;
               END IF 
            END IF
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
                  AND a.marca              = c.marca_activa
                  AND c.marca_entra        = v_ax_marca_entra
                  AND c.rch_cod            > 0
                ORDER BY a.f_inicio
               END FOREACH;
            ELSE
               LET v_ax_marca_activa = v_ax_sts_marcaje;
            END IF

            IF EXISTS (SELECT marca_inf
                         FROM cat_tipo_credito
                        WHERE marca_inf = v_ax_marca_activa) THEN
               IF cre_edo_credito = 1 THEN
                  LET cre_nvo_edo = 152;
               ELIF cre_edo_credito = 3 THEN
                  LET cre_nvo_edo = 18;
               END IF

               -- si el estado es 140 no se actualiza  HFJ
               IF cre_estado <> 140 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE cre_acreditado
                     SET estado = cre_nvo_edo
                  WHERE id_cre_acreditado = cre_id_cre_acreditado;

                  UPDATE cre_his_acreditado
                     SET estado = cre_nvo_edo
                  WHERE id_cre_acreditado = cre_id_cre_acreditado;

                  UPDATE cre_marca_conciliacion
                     SET estado = 150  -- Marca infonavit rechazada proceso operativo
                   WHERE id_derechohabiente = cre_id_derechohabiente
                    AND id_proceso = p_si_proceso_cod
                    AND f_proceso  = TODAY;
               END IF

               CONTINUE FOREACH;
            END IF

            -- en caso de ser mayor a cero
            IF v_ax_sts_marcaje > 999 THEN
               LET v_ax_sts_marcaje = 150;
            END IF

            --nuevo estado de acuerdo al estado del crédito
            IF cre_edo_credito = 1 THEN
               IF v_ax_sts_marcaje IN(590,591,592) THEN
                  LET cre_nvo_edo = 159;
               ELSE
                  LET cre_nvo_edo = 150;
               END IF
            ELIF cre_edo_credito = 3 THEN
               LET cre_nvo_edo = 155;
            END IF

            --se actualiza el estado en cre acreditado y cre his acreditado
            UPDATE cre_acreditado
               SET estado            = cre_nvo_edo
             WHERE id_cre_acreditado = cre_id_cre_acreditado;

            UPDATE cre_his_acreditado
               SET estado            = cre_nvo_edo,
                   diagnostico       = v_ax_sts_marcaje
             WHERE id_cre_acreditado = cre_id_cre_acreditado;

            --Se integra inserción en la tabla de rechazos de marca
            INSERT INTO safre_tmp:tmp_marca_rechazo(nss,
                         tpo_credito,
                         estado)
                 VALUES (v_ax_nss,
                         cre_tpo_credito,
                         2);

            UPDATE cre_marca_conciliacion
               SET estado = 150  -- Marca infonavit rechazada proceso operativo
             WHERE id_derechohabiente = cre_id_derechohabiente
               AND id_proceso = p_si_proceso_cod
               AND f_proceso  = TODAY;

         END IF
      END IF

      LET cre_edo_credito     = NULL;
      LET cre_nvo_edo         = NULL;
      LET cre_tpo_registro    = NULL;
      LET v_ax_marcaje        = 0;

   END FOREACH;

   RETURN v_ax_excep_error;

END FUNCTION;


