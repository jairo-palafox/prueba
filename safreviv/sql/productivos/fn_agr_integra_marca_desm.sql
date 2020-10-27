






CREATE FUNCTION "safreviv".fn_agr_integra_marca_desm(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                          p_v_arch_proceso    CHAR(100),
                                          p_v_usuario         CHAR(20),
                                          p_d_folio           DECIMAL(9,0),
                                          p_si_proceso_cod    SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250),
             INTEGER, INTEGER, INTEGER, INTEGER
   -- REGISTRO de la temporal
   DEFINE tmp_tpo_registro          CHAR(2);
   DEFINE tmp_cont_servicio         DECIMAL(10,0);
   DEFINE tmp_nss                   CHAR(11);
   DEFINE tmp_num_credito           CHAR(10);
   DEFINE tmp_tpo_originacion       CHAR(2);
   DEFINE tmp_tpo_orig              SMALLINT;
   DEFINE tmp_tpo_credito           CHAR(3);
   DEFINE tmp_sts_credito           CHAR(3);
   DEFINE tmp_sit_credito           CHAR(1);
   DEFINE tmp_f_inicio_marca        DATE;
   DEFINE tmp_f_fin_marca           DATE;
   DEFINE tmp_edo_solicitud         CHAR(2);
   DEFINE tmp_diagnostico           CHAR(3);
   -- REGISTRO de la temporal
   DEFINE his_id_cre_acreditado    DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE his_tpo_transferencia    CHAR(2);
   DEFINE his_edo_procesar         SMALLINT;
   DEFINE his_diagnostico          CHAR(3);
   DEFINE his_estado               SMALLINT;
   DEFINE his_nss_afore            CHAR(11);
   DEFINE his_rfc_afore            CHAR(13);
   DEFINE his_paterno_afore        CHAR(40);
   DEFINE his_materno_afore        CHAR(40);
   DEFINE his_nombre_afore         CHAR(40);
   DEFINE his_nom_imss             CHAR(50);
   DEFINE his_f_proceso            DATE;
   -- REGISTRO de rechazos
   DEFINE rch_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE rch_nss                  CHAR(11);
   DEFINE rch_tpo_originacion      SMALLINT;
   DEFINE rch_tpo_credito          SMALLINT;
   DEFINE rch_num_credito          DECIMAL(10,0);
   DEFINE rch_sdo_deudor           DECIMAL(12,2);
   DEFINE rch_edo_credito          SMALLINT;
   DEFINE rch_valor_dscto          DECIMAL(8,4);
   DEFINE rch_estado               SMALLINT;
   -- Registo de tabla maestro
   DEFINE cre_id_cre_acreditado   DECIMAL(9,0); -- identificador del acreditado
   DEFINE cre_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE cre_tpo_credito         SMALLINT; -- tipo de crédito
   DEFINE cre_edo_procesar        SMALLINT; -- estado procesar
   -- Registro de la tabla historica para el reintento de Marca/Desmarca
   DEFINE v_his_id_cre_acreditado  DECIMAL(9,0); -- identificador del acreditado
   DEFINE v_his_id_cre_ctr_archivo DECIMAL(9,0); -- identificador del archivo
   DEFINE v_his_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_his_folio_archivo      DECIMAL(9,0); -- folio del archivo
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_cuenta_acept       INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech        INTEGER; -- contador de registros rechazados
   DEFINE v_ax_marca_acept        INTEGER; -- contador de marcas aceptadas
   DEFINE v_ax_marca_rech         INTEGER; -- contador de marcas rechazadas
   DEFINE v_ax_desmarca_acept     INTEGER; -- contador de desmarcas aceptadas
   DEFINE v_ax_desmarca_rech      INTEGER; -- contador de desmarcas rechazadas
   DEFINE v_ax_tpo_originacion    SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_transferencia  CHAR(2); -- tipo de transferencia
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_rch_estado         SMALLINT; -- estado procesar
   DEFINE v_ax_marca_prc          SMALLINT; -- marca procesar
   DEFINE v_ax_actualiza_edos     SMALLINT; -- marca procesar
   DEFINE v_i_estado_glo          SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE r_ax_existe_marca_prc   SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_edo_retorno           SMALLINT; -- estado retorno de alguna funcion

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
     -- Devolvera el codigo de error cuando ocurra una excepción
     RETURN v_error, v_isam_err, v_c_msj, v_ax_marca_acept, v_ax_marca_rech, v_ax_desmarca_acept, v_ax_desmarca_rech;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRecurr.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acept      = 0;
   LET v_ax_cuenta_rech       = 0;
   LET v_ax_marca_acept       = 0;
   LET v_ax_marca_rech        = 0;
   LET v_ax_desmarca_acept    = 0;
   LET v_ax_desmarca_rech     = 0;
   LET v_i_estado_glo         = 2; -- estado Integrado
   --LET v_ax_tpo_transferencia = "43"; -- Anualidades Garantizadas
   LET v_ax_edo_procesar      = 20;
   LET v_error                = 0;
   LET v_isam_err             = 0;
   LET v_c_msj                = 'El proceso finalizó correctamente';

   ------------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE LA CARGA MARCA / DESMARCA --
   ------------------------------------------------------------
   -- se obtienen los datos de la tabla temporal del proceso de Recurrente
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servicio,
           tmp_nss,
           tmp_num_credito,
           tmp_tpo_originacion,
           tmp_tpo_credito,
           tmp_sts_credito,
           tmp_sit_credito,
           tmp_f_inicio_marca,
           tmp_f_fin_marca,
           tmp_edo_solicitud,
           tmp_diagnostico
      FROM safre_tmp:tmp_marca_desmarca_det

      -- se inicializan variables
      LET cre_id_cre_acreditado = NULL;
      LET v_ax_id_derechohabiente = NULL;
      LET v_ax_tpo_originacion = NULL;
      LET r_edo_retorno = 0;

      -- se valida el número de crédito
      IF tmp_num_credito IS NULL THEN
         LET tmp_num_credito = 0;
      END IF

      LET tmp_tpo_orig = tmp_tpo_originacion;

      IF tmp_tpo_orig IS NULL THEN
         LET tmp_tpo_orig = 0;
      END IF

      -- se obtiene el id cre_acreditado
      FOREACH
       SELECT FIRST 1 cre.id_cre_acreditado, cre.edo_procesar, tpo.marca_prc, tpo.tpo_originacion, afi.id_derechohabiente
         INTO cre_id_cre_acreditado, cre_edo_procesar, v_ax_marca_prc, v_ax_tpo_originacion, v_ax_id_derechohabiente
         FROM safre_viv:cre_acreditado cre, safre_viv:afi_derechohabiente afi, safre_viv:cat_tipo_credito tpo
        WHERE afi.nss = tmp_nss
          AND afi.id_derechohabiente = cre.id_derechohabiente
          AND cre.tpo_originacion = tmp_tpo_orig
          AND cre.tpo_credito = tpo.tpo_credito
          AND tpo.tpo_credito = tmp_tpo_credito
          AND tpo.f_actualiza <= cre.f_otorga
          AND tpo.tpo_originacion = cre.tpo_originacion
          AND cre.edo_credito = 1
        ORDER BY cre.f_otorga DESC, cre.estado
      END FOREACH;

      IF v_ax_tpo_originacion IS NULL THEN
         -- se asiga el estado indicando el motivo del error
         LET v_ax_rch_estado = 26; -- 26-REGISTRO SIN ORIGINACIÓN DE CRÉDITO

         -- se asinan los valores en el registro a insertar
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = tmp_tpo_orig;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = 0;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_valor_dscto        = 0;
         LET rch_estado             = v_ax_rch_estado;

         -- se inserta registro
         INSERT INTO safre_viv:cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     num_credito,
                     sdo_deudor,
                     edo_credito,
                     valor_dscto,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_edo_credito,
                     rch_valor_dscto,
                     rch_estado);

         -- se incrementa los registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         CONTINUE FOREACH;
      END IF

      IF v_ax_id_derechohabiente IS NULL THEN
         -- se asiga el estado indicando el motivo del error
         LET v_ax_rch_estado = 11; -- 11-TRABAJADOR NO EXISTE

         -- se asinan los valores en el registro a insertar
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = tmp_tpo_orig;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = 0;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_valor_dscto        = 0;
         LET rch_estado             = v_ax_rch_estado;

         -- se inserta registro
         INSERT INTO safre_viv:cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     num_credito,
                     sdo_deudor,
                     edo_credito,
                     valor_dscto,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_edo_credito,
                     rch_valor_dscto,
                     rch_estado);

         -- se incrementa los registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se verifica si fue posible obtener el id acreditado
      IF cre_id_cre_acreditado IS NULL OR cre_edo_procesar = 5 THEN
         IF cre_id_cre_acreditado IS NULL THEN
            -- se asiga el estado indicando el motivo del error
            LET v_ax_rch_estado = 13; -- 13-NO EXISTE MARCA CRÉDITO VIGENTE
         ELSE
            -- se asiga el estado indicando el motivo del error
            LET v_ax_rch_estado = 24; -- 24-REGISTRO CORRESPONDE A SOLO INFONAVIT
         END IF

         -- se asinan los valores en el registro a insertar
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = tmp_tpo_orig;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = 0;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_valor_dscto        = 0;
         LET rch_estado             = v_ax_rch_estado;

         -- se inserta registro
         INSERT INTO safre_viv:cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     num_credito,
                     sdo_deudor,
                     edo_credito,
                     valor_dscto,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_edo_credito,
                     rch_valor_dscto,
                     rch_estado);

         -- se incrementa los registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         CONTINUE FOREACH;
      END IF;

      -- se asigna el tipo de transferencia dependiendo del tipo de originacion
      IF v_ax_tpo_originacion = 1 THEN
         -- se asigna el tipo de transferencia correspondiente a "TA"
         LET v_ax_tpo_transferencia = "03";
      ELIF v_ax_tpo_originacion = 4 THEN
         -- se asigna el tipo de transferencia correspondiente a "AG"
         LET v_ax_tpo_transferencia = "43";
      ELSE
         -- se asume que el tipo de originación es 2 por lo que el tipo de transferencia pertenece a "CG"
         LET v_ax_tpo_transferencia = "16";
      END IF

      -- se asigna la información en el registro a insertar
      LET his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
      LET his_tpo_transferencia  = v_ax_tpo_transferencia;
      LET his_diagnostico        = tmp_diagnostico;
      LET his_estado             = 20;
      LET his_nss_afore          = NULL;
      LET his_rfc_afore          = NULL;
      LET his_paterno_afore      = NULL;
      LET his_materno_afore      = NULL;
      LET his_nombre_afore       = NULL;
      LET his_nom_imss           = NULL;
      LET his_f_proceso          = TODAY;

      IF tmp_edo_solicitud <> "01" THEN
         IF tmp_sit_credito = "M" THEN
            LET v_ax_edo_procesar = 50;
         ELSE
            LET v_ax_edo_procesar = 200;
         END IF

         -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
         IF v_ax_tpo_originacion = 2 THEN
            -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
            IF cre_edo_procesar < 55 THEN
               LET v_ax_actualiza_edos = 1;
            ELSE
               LET v_ax_actualiza_edos = 0;
            END IF
         ELSE
            -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
            IF cre_edo_procesar < 60 THEN
               LET v_ax_actualiza_edos = 1;
            ELSE
               LET v_ax_actualiza_edos = 0;
            END IF
         END IF

         IF v_ax_actualiza_edos = 1 THEN
            -- se actualiza el estado en cre acreditado y cre his acreditado
            UPDATE safre_viv:cre_acreditado
               SET edo_procesar = v_ax_edo_procesar
             WHERE id_cre_acreditado = cre_id_cre_acreditado;

            -- se asigna el estado procesar
            LET his_edo_procesar = v_ax_edo_procesar;

            -- se inserta registro en tabla cre his acreditado
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
         END IF

         -- se incrementa el contador de registros rechazados
         LET v_ax_marca_rech = v_ax_marca_rech + 1;

         CONTINUE FOREACH;
      END IF

      IF tmp_sit_credito = "M" THEN
         IF v_ax_tpo_originacion <> 2 THEN
            -- se invoca la función que verifica si ya existe la marca de procesar
            EXECUTE FUNCTION safre_viv:fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                                               v_ax_marca_prc)
                                                          INTO r_ax_existe_marca_prc;

            -- en caso de no existir la marca se ejecuta
            IF r_ax_existe_marca_prc = 0 THEN
               -- se ejecuta la función de marcaje
               EXECUTE FUNCTION safre_viv:fn_marca_cuenta(v_ax_id_derechohabiente,
                                                          v_ax_marca_prc,
                                                          cre_id_cre_acreditado, -- referencia
                                                          p_d_folio,
                                                          0, -- estado marca
                                                          0, -- codigo rechazo
                                                          NULL, -- marca causa
                                                          "", -- fecha causa
                                                          p_v_usuario,
                                                          p_si_proceso_cod)
                                                    INTO  r_edo_retorno;
            ELSE
               -- se indica que no se ha ejecutado la marca y continua con el siguiente registro
               CONTINUE FOREACH;
            END IF
         ELSE
            -- se asigna estus correcto para que actulice a 55 los etados
            LET r_edo_retorno = 0;
         END IF

         -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
         IF r_edo_retorno = 0 THEN
            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- se asigna el estado procesar a 55 - Marca Aceptada Procesar
               LET v_ax_edo_procesar = 55;

               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            ELSE
               -- se asigna el estado procesar a 60 - Marcada Procesar
               LET v_ax_edo_procesar = 60;

               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            END IF

            IF v_ax_actualiza_edos = 1 THEN
               -- se actualiza el estado en cre acreditado y cre his acreditado
               UPDATE safre_viv:cre_acreditado
                  SET edo_procesar = v_ax_edo_procesar
                WHERE id_cre_acreditado = cre_id_cre_acreditado;

               -- se asigna el estado procesar al campo del registro
               LET his_edo_procesar = v_ax_edo_procesar;

               -- se inserta registro en tabla cre his acreditado
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
            END IF

            -- se incrementa el contador de registros aceptados
            LET v_ax_marca_acept = v_ax_marca_acept + 1;
         ELSE
            -- se asigna el estado procesar a 30 - Error en Marca
            LET v_ax_edo_procesar = 30;

            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            ELSE
               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            END IF

            IF v_ax_actualiza_edos = 1 THEN
               -- se actualiza el estado en cre acreditado y cre his acreditado
               UPDATE safre_viv:cre_acreditado
                  SET edo_procesar = v_ax_edo_procesar
                WHERE id_cre_acreditado = cre_id_cre_acreditado;

               -- se asigna el estado procesar al campo del registro
               LET his_edo_procesar = v_ax_edo_procesar;

               -- se inserta registro en tabla cre his acreditado
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
            END IF

            -- se incrementa el contador de registros rechazados
            LET v_ax_marca_rech = v_ax_marca_rech + 1;
         END IF
      ELSE
         -- se asuma que se ejecutará la desmarca
         EXECUTE FUNCTION safre_viv:fn_desmarca_cuenta(v_ax_id_derechohabiente,
                                                       v_ax_marca_prc,
                                                       cre_id_cre_acreditado, -- referencia
                                                       0, -- estado marca
                                                       0, -- marca causa
                                                       p_v_usuario,
                                                       p_si_proceso_cod)
                                                  INTO r_edo_retorno;

         -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
         IF r_edo_retorno = 0 THEN
            -- se asigna el estado procesar a 210 - Desmarca Aceptada
            LET v_ax_edo_procesar = 210;

            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            ELSE
               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            END IF

            IF v_ax_actualiza_edos = 1 THEN
               -- se actualiza el estado en cre acreditado y cre his acreditado
               UPDATE safre_viv:cre_acreditado
                  SET edo_procesar = v_ax_edo_procesar
                WHERE id_cre_acreditado = cre_id_cre_acreditado;

               -- se asigna el estado procesar al campo del registro
               LET his_edo_procesar = v_ax_edo_procesar;

               -- se inserta registro en tabla cre his acreditado
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
            END IF

            -- se incrementa el contador de registros aceptados
            LET v_ax_desmarca_acept = v_ax_desmarca_acept + 1;
         ELSE
            -- se asigna el estado procesar a 35 - Error en desmarca
            LET v_ax_edo_procesar = 35;

            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            ELSE
               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  LET v_ax_actualiza_edos = 1;
               ELSE
                  LET v_ax_actualiza_edos = 0;
               END IF
            END IF

            IF v_ax_actualiza_edos = 1 THEN
               -- se actualiza el estado en cre acreditado y cre his acreditado
               UPDATE safre_viv:cre_acreditado
                  SET edo_procesar = v_ax_edo_procesar
                WHERE id_cre_acreditado = cre_id_cre_acreditado;

               -- se asigna el estado procesar al campo del registro
               LET his_edo_procesar = v_ax_edo_procesar;

               -- se inserta registro en tabla cre his acreditado
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
            END IF

            -- se incrementa el contador de registros rechazados
            LET v_ax_desmarca_rech = v_ax_desmarca_rech + 1;
         END IF
      END IF
   END FOREACH;

   -----------------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS PENDIENTES POR MARCAR / DESMARCAR --
   -----------------------------------------------------------------
   FOREACH
      -- se obtienen los datos de la tabla temporal del proceso de Recurrente
      SELECT his.id_cre_acreditado,
             his.id_cre_ctr_archivo,
             his.edo_procesar,
             ctr.folio_archivo
        INTO v_his_id_cre_acreditado,
             v_his_id_cre_ctr_archivo,
             v_his_edo_procesar,
             v_his_folio_archivo
        FROM safre_viv:cre_his_acreditado his,
             safre_viv:cre_ctr_archivo ctr
       WHERE his.edo_procesar IN (30,35)
         AND his.id_cre_ctr_archivo <> p_d_id_cre_ctr_arch
         AND his.id_cre_ctr_archivo = ctr.id_cre_ctr_archivo

      -- se inicializan variables
      LET cre_id_derechohabiente = NULL;
      LET cre_tpo_credito = NULL;

      -- se obtiene el id cre_acreditado
      FOREACH
         SELECT FIRST 1 id_derechohabiente, tpo_credito, edo_procesar
           INTO cre_id_derechohabiente, cre_tpo_credito, cre_edo_procesar
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_his_id_cre_acreditado
            AND edo_credito = 1
          ORDER BY f_otorga DESC

         EXIT FOREACH;
      END FOREACH;

      -- en caso de no encontrar el registro correspondiente en la tabla maestro continua con el siguiente registro
      IF cre_id_derechohabiente IS NULL OR cre_tpo_credito IS NULL THEN
         CONTINUE FOREACH;
      END IF

      -- se obtiene la marca y tipo originacion para el tipo de credito en proceso
      FOREACH
       SELECT FIRST 1 marca_prc, tpo_originacion
         INTO v_ax_marca_prc, v_ax_tpo_originacion
         FROM safre_viv:cat_tipo_credito
        WHERE tpo_credito = cre_tpo_credito
      END FOREACH;

      IF v_his_edo_procesar = 30 THEN
         -- se invoca la función que verifica si ya existe la marca de procesar
         EXECUTE FUNCTION safre_viv:fn_cre_existe_marca_prc(cre_id_derechohabiente,
                                                            v_ax_marca_prc)
                                                       INTO r_ax_existe_marca_prc;

         -- en caso de no existir la marca se ejecuta
         IF r_ax_existe_marca_prc = 0 THEN
            -- se ejecuta la función de marcaje
            EXECUTE FUNCTION safre_viv:fn_marca_cuenta(cre_id_derechohabiente,
                                                       v_ax_marca_prc,
                                                       v_his_id_cre_acreditado, -- referencia
                                                       v_his_folio_archivo,
                                                       0, -- estado marca
                                                       0, -- codigo rechazo
                                                       NULL, -- marca causa
                                                       "", -- fecha causa
                                                       p_v_usuario,
                                                       p_si_proceso_cod)
                                                 INTO  r_edo_retorno;
         ELSE
            -- se indica que no se ha ejecutado la marca y continua con el siguiente registro
            CONTINUE FOREACH;
         END IF

         -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
         IF r_edo_retorno = 0 THEN
            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- se asigna el estado procesar a 55 - Marca Aceptada Procesar
               LET v_ax_edo_procesar = 55;

               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE safre_viv:cre_acreditado
                     SET edo_procesar = v_ax_edo_procesar
                   WHERE id_cre_acreditado = v_his_id_cre_acreditado;
               END IF
            ELSE
               -- se asigna el estado procesar a 60 - Marcada Procesar
               LET v_ax_edo_procesar = 60;

               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE safre_viv:cre_acreditado
                     SET edo_procesar = v_ax_edo_procesar
                   WHERE id_cre_acreditado = v_his_id_cre_acreditado;
               END IF
            END IF

            -- se actualiza el estado en cre his acreditado
            UPDATE safre_viv:cre_his_acreditado
               SET edo_procesar = v_ax_edo_procesar
             WHERE id_cre_acreditado = v_his_id_cre_acreditado
               AND id_cre_ctr_archivo = v_his_id_cre_ctr_archivo;

            -- se incrementa el contador de registros aceptados
            LET v_ax_marca_acept = v_ax_marca_acept + 1;
         END IF
      ELSE
         -- se asuma que se ejecutará la desmarca
         EXECUTE FUNCTION safre_viv:fn_desmarca_cuenta(cre_id_derechohabiente,
                                                       v_ax_marca_prc,
                                                       v_his_id_cre_acreditado, -- referencia
                                                       0, -- estado marca
                                                       0, -- marca causa
                                                       p_v_usuario,
                                                       p_si_proceso_cod)
                                                  INTO r_edo_retorno;

         -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
         IF r_edo_retorno = 0 THEN
            -- se asigna el estado procesar a 210 - Desmarca Aceptada
            LET v_ax_edo_procesar = 210;

            -- se verifica si el tipo de originación corresponde a Créditos en Garantía 43 bis (2)
            IF v_ax_tpo_originacion = 2 THEN
               -- si el estado procesar es menor que 55 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 55 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE safre_viv:cre_acreditado
                     SET edo_procesar = v_ax_edo_procesar
                   WHERE id_cre_acreditado = v_his_id_cre_acreditado;
               END IF
            ELSE
               -- si el estado procesar es menor que 60 se habilita la bandera de actualización de estados
               IF cre_edo_procesar < 60 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE safre_viv:cre_acreditado
                     SET edo_procesar = v_ax_edo_procesar
                   WHERE id_cre_acreditado = v_his_id_cre_acreditado;
               END IF
           END IF

            -- se actualiza el estado en cre his acreditado
            UPDATE safre_viv:cre_his_acreditado
               SET edo_procesar = v_ax_edo_procesar
             WHERE id_cre_acreditado = v_his_id_cre_acreditado
               AND id_cre_ctr_archivo = v_his_id_cre_ctr_archivo;

            -- se incrementa el contador de registros aceptados
            LET v_ax_desmarca_acept = v_ax_desmarca_acept + 1;
         END IF
      END IF
   END FOREACH;

   -- se suman las marcas y desmarcas aceptadas
   LET v_ax_cuenta_acept = v_ax_cuenta_acept + v_ax_marca_acept + v_ax_desmarca_acept;
   LET v_ax_cuenta_rech = v_ax_cuenta_rech + v_ax_marca_rech + v_ax_desmarca_rech;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cre_his_acreditado;
   UPDATE STATISTICS FOR TABLE safre_viv:cre_rch_acreditado;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado_glo, p_v_usuario) INTO r_edo_retorno;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, v_ax_cuenta_acept, v_ax_cuenta_rech, 0, p_d_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_ax_marca_acept, v_ax_marca_rech, v_ax_desmarca_acept, v_ax_desmarca_rech;
END FUNCTION
;


