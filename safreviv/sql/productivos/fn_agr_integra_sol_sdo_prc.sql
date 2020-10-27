






CREATE FUNCTION "safreviv".fn_agr_integra_sol_sdo_prc(p_usuario         CHAR(20),
                                           p_arch_proceso    CHAR(100),
                                           p_folio           DECIMAL(10,0),
                                           p_id_cre_ctr_arch DECIMAL(9,0) )

   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   DEFINE v_error                  SMALLINT;
   DEFINE v_isam_err               INTEGER;
   DEFINE v_c_msj                  VARCHAR(250);
   DEFINE v_c_nss                  CHAR(11);
   -- Variables rec. inf. temporal
   DEFINE v_tmp_nss                CHAR(11);
   DEFINE v_tmp_aivs_92            DECIMAL(12,6);
   DEFINE v_tmp_aivs_97            DECIMAL(12,6);
   DEFINE v_tmp_periodo_pago       CHAR(6);
   DEFINE v_tmp_tpo_solicitud      CHAR(2);
   DEFINE v_tmp_marca_origen       SMALLINT;
   DEFINE v_tmp_marca_prcr         SMALLINT;
   DEFINE v_id_derechohabiente     DECIMAL(9,0);
   DEFINE v_diagnostico            SMALLINT;
   DEFINE v_id_cre_acreditado      DECIMAL(9,0);
   DEFINE v_aux_tpo_orig           SMALLINT;
   DEFINE v_total_aceptados        INTEGER;
   DEFINE v_total_rechazados       INTEGER;
   DEFINE v_n_referencia           DECIMAL(9,0);
   DEFINE v_importe_97             DECIMAL(12,2);
   DEFINE v_err_desm               SMALLINT;
   DEFINE v_num_credito            DECIMAL(10,0);
   DEFINE v_marca_entra            SMALLINT;
   DEFINE v_aux_id_cre_ug          DECIMAL(9,0);
   DEFINE v_ax_sts_marcaje         SMALLINT;
   DEFINE v_estado_arh_glo         SMALLINT;
   DEFINE r_ax_bandera             SMALLINT;
   DEFINE v_aux_tpo_transferencia  CHAR(2);
   DEFINE v_aux_tpo_uso            SMALLINT;
   DEFINE v_f_movimiento           DATE;
   DEFINE v_precio_fondo           DECIMAL(19,14);
   DEFINE v_c_id_cre_uso_garantia  DECIMAL(9,0);
   DEFINE v_tmp_aux_periodo        CHAR(6);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_int/archivos/fn_agr_integra_sol_sdo_prc.trace';
   --TRACE ON;

   -- Inicializa variables
   LET v_error                 = 0;
   LET v_isam_err              = 0;
   LET v_c_msj                 = 'El proceso SSPR finalizó correctamente';
   LET v_c_nss                 = "0"; -- Valor del NSS antes de entrar al ciclo
   LET v_estado_arh_glo        = 2;   -- Estado integrado
   LET v_tmp_nss               = NULL;
   LET v_tmp_aivs_92           = 0;
   LET v_tmp_aivs_97           = 0;
   LET v_tmp_periodo_pago      = NULL;
   LET v_tmp_tpo_solicitud     = NULL;
   LET v_tmp_marca_origen      = NULL;
   LET v_tmp_marca_prcr        = NULL;
   LET v_id_derechohabiente    = NULL;
   LET v_diagnostico           = 0;  -- Diagnostico procedente
   LET v_id_cre_acreditado     = NULL;
   LET v_aux_tpo_orig          = NULL;
   LET v_total_aceptados       = 0;
   LET v_total_rechazados      = 0;
   LET v_err_desm              = 0;
   LET v_marca_entra           = NULL;
   LET v_importe_97            = 0;
   LET v_num_credito           = 0;
   LET v_c_id_cre_uso_garantia = NULL;
   LET v_tmp_aux_periodo       = NULL;

   -- Calcula fecha de movimiento
   LET v_f_movimiento = MDY(MONTH(TODAY),1,YEAR(TODAY));   -- Obtiene primer día del mes
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH;

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_movimiento
      AND fondo = 11;

   FOREACH
      -- se obtienen inf. de la temporal
      SELECT nss             ,
             aivs_92 /1000000,
             aivs_97 /1000000,
             periodo_pago    ,
             tpo_solicitud   ,
             marca_origen    ,
             marca_prcr      ,
             YEAR(TODAY)||LPAD(MONTH(TODAY),2,0) AS aux_periodo
        INTO v_tmp_nss           ,
             v_tmp_aivs_92       ,
             v_tmp_aivs_97       ,
             v_tmp_periodo_pago  ,
             v_tmp_tpo_solicitud ,
             v_tmp_marca_origen  ,
             v_tmp_marca_prcr    ,
             v_tmp_aux_periodo
        FROM safre_tmp:tmp_det_marca_sspr

      -- Valida que sea un derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_tmp_nss;

      -- Marca a actualizar
      IF(v_tmp_marca_prcr = 1) THEN
         LET v_marca_entra = 221;
      END IF
      IF(v_tmp_marca_prcr = 2) THEN
         LET v_marca_entra = 223;
         -- Verifica periodo
         IF(v_tmp_periodo_pago IS NULL) OR (v_tmp_periodo_pago = "      ") OR (v_tmp_periodo_pago = "000000") THEN
            LET v_tmp_periodo_pago = v_tmp_aux_periodo;
         END IF
      END IF
      IF(v_tmp_marca_prcr = 4) THEN
         LET v_marca_entra = 225;
         -- Verifica periodo
         IF(v_tmp_periodo_pago = "      ") OR (v_tmp_periodo_pago = "000000") THEN
            LET v_tmp_periodo_pago = NULL;
         END IF
      END IF

      IF(v_id_derechohabiente IS NULL) THEN
         LET v_diagnostico = 11; --> Trabajador no existe
         LET v_total_rechazados = v_total_rechazados + 1;
      ELSE
         -- Valida si es un registro repetido
         IF EXISTS(SELECT id_derechohabiente
                     FROM cre_act_marca_sspr
                    WHERE id_derechohabiente = v_id_derechohabiente
                      AND folio = p_folio) THEN
            LET v_diagnostico = 17;  --> Registro repetido
            LET v_total_rechazados = v_total_rechazados + 1;
         ELSE
            {==== Valida Información de la temporal ====}
            -- Valida Marca origen
            IF(v_tmp_marca_origen <> 221) AND (v_tmp_marca_origen <> 223) AND (v_tmp_marca_origen <> 225) THEN
               LET v_diagnostico = 14;  --> Tipo de registro no válido
               LET v_total_rechazados = v_total_rechazados + 1;
            ELSE
               -- Valida Marca Procesar
               -- 2-Archivo 43Bis, 4-Archivo AG
               IF(v_tmp_marca_prcr <> 1) AND (v_tmp_marca_prcr <> 2) AND (v_tmp_marca_prcr <> 4) THEN
                  LET v_diagnostico = 14;  --> Tipo de registro no válido
                  LET v_total_rechazados = v_total_rechazados + 1;
               ELSE
                  -- Evalúa el tipo de originación de acuerdo a la Marca original
                  IF(v_tmp_marca_origen = 221) THEN
                    LET v_aux_tpo_orig = 1;        --CREDITOS TRADICIONALES
                  ELSE
                     IF(v_tmp_marca_origen = 223) THEN
                        LET v_aux_tpo_orig = 2;     --CRÉDITOS EN GARANTÍA 43BIS
                     ELSE
                        IF(v_tmp_marca_origen = 225) THEN
                           LET v_aux_tpo_orig = 4;  -- CREDITOS COFINANCIADOS AG
                        END IF
                     END IF
                  END IF

                  -- Verifica que exista el registro en la tabla maestra, independientemente si está vigente
                  SELECT MAX(c.id_cre_acreditado)
                    INTO v_id_cre_acreditado
                    FROM cre_acreditado  c,
                         cat_maq_credito m
                   WHERE c.id_derechohabiente = v_id_derechohabiente
                     AND c.estado  = m.estado;

                  IF(v_id_cre_acreditado IS NULL) THEN
                     LET v_diagnostico = 26;   --> Registro sin originación de crédito
                     LET v_total_rechazados = v_total_rechazados + 1;
                  ELSE
                     -- Calcula importe_97
                     LET v_importe_97 = ((v_tmp_aivs_92 + v_tmp_aivs_97) * v_precio_fondo);

                     {==== Solicitud de saldo a Procesar ====}

                     LET v_n_referencia  = NULL;
                     LET v_num_credito   = 0;
                     LET v_aux_tpo_transferencia = NULL;
                     LET v_aux_tpo_uso   = NULL;

                      -- Si tiene la marca 221 - TRANSFERENCIA SALDO ACREDITADO
                     IF(v_tmp_marca_origen = 221) THEN
                        --Verifica que la marca a conciliar sea una 223 o 225
                        IF(v_tmp_marca_prcr <> 2) AND (v_tmp_marca_prcr <> 4) THEN 
                           LET v_diagnostico = 14;   --> Tipo registro no válido
                           LET v_total_rechazados = v_total_rechazados + 1;
                        ELSE
                           -- Búsca la marca activa 221 a barrer
                           SELECT MAX(n_referencia)
                             INTO v_n_referencia
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_derechohabiente
                              AND marca = v_tmp_marca_origen;

                           IF(v_n_referencia IS NULL) THEN
                              LET v_diagnostico = 18;   --> Registro rechazado
                              LET v_total_rechazados = v_total_rechazados + 1;
                           ELSE
                              -- Verifica que no se realice la solicitud el mismo periodo
                              IF(v_marca_entra = 223) THEN
                                 SELECT MAX(id_cre_uso_garantia)
                                   INTO v_c_id_cre_uso_garantia
                                   FROM cre_uso_garantia
                                  WHERE id_derechohabiente = v_id_derechohabiente
                                    AND tpo_transferencia IN("18","48")
                                    AND periodo_pago = v_tmp_periodo_pago
                                    AND estado IN(20,140,142)
                                    AND edo_procesar IN(10,70,80,85,120);
                              ELSE
                                IF(v_marca_entra = 225) THEN
                                   SELECT MAX(id_cre_uso_garantia)
                                     INTO v_c_id_cre_uso_garantia
                                     FROM cre_uso_garantia
                                    WHERE id_derechohabiente = v_id_derechohabiente
                                      AND tpo_transferencia  = "43"
                                      AND estado IN(20,140,142)
                                      AND edo_procesar IN(10,70,80,85);
                                END IF
                              END IF

                              IF(v_c_id_cre_uso_garantia IS NOT NULL) THEN
                                 LET v_diagnostico = 17;   --> Registro repetido
                                 LET v_total_rechazados = v_total_rechazados + 1;
                              ELSE
                                 -- Recupera la inf. del acreditado
                                 SELECT num_credito
                                   INTO v_num_credito
                                   FROM cre_acreditado
                                  WHERE id_cre_acreditado = v_n_referencia;

                                 -- Desmarca cuenta anterior
                                 EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                                     v_tmp_marca_origen, -- Marca original
                                                                     v_n_referencia,
                                                                     0,
                                                                     0,
                                                                     p_usuario,
                                                                     350)                 -- Actualización marca SSPR
                                                                INTO v_err_desm;

                                 -- Realiza petición de saldo
                                 LET v_aux_id_cre_ug = seq_cre_uso.NEXTVAL;

                                 IF(v_marca_entra = 223) THEN
                                    LET v_aux_tpo_transferencia = "18";
                                    LET v_aux_tpo_uso = 3;
                                    LET v_num_credito = 0;
                                 ELSE
                                    IF(v_marca_entra = 225) THEN
                                       LET v_aux_tpo_transferencia = "43";
                                       LET v_aux_tpo_uso = 2;
                                    END IF
                                 END IF

                                 -- Crea solicitud
                                 INSERT INTO cre_uso_garantia (id_cre_uso_garantia,
                                                               id_cre_ctr_archivo ,
                                                               folio_liquida      ,
                                                               id_derechohabiente ,
                                                               tpo_transferencia  ,
                                                               tpo_uso            ,
                                                               num_credito        ,
                                                               f_presentacion     ,
                                                               f_movimiento       ,
                                                               periodo_pago       ,
                                                               importe_v97        ,
                                                               nss_afore          ,
                                                               rfc_afore          ,
                                                               paterno_afore      ,
                                                               materno_afore      ,
                                                               nombre_afore       ,
                                                               nom_imss           ,
                                                               edo_procesar       ,
                                                               diagnostico        ,
                                                               estado             ,
                                                               f_proceso)
                                                       VALUES (v_aux_id_cre_ug        ,
                                                               p_id_cre_ctr_arch      ,
                                                               0                      ,
                                                               v_id_derechohabiente   ,
                                                               v_aux_tpo_transferencia,
                                                               v_aux_tpo_uso          ,
                                                               v_num_credito          ,
                                                               TODAY                  ,
                                                               v_f_movimiento         ,
                                                               v_tmp_periodo_pago     ,
                                                               v_importe_97           ,
                                                               ""                     ,
                                                               ""                     ,
                                                               ""                     ,
                                                               ""                     ,
                                                               ""                     ,
                                                               ""                     ,
                                                               70                     ,
                                                               ""                     ,
                                                               142                    ,
                                                               TODAY);

                                 -- Marca la cuenta
                                 EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                                  v_marca_entra,
                                                                  v_aux_id_cre_ug,
                                                                  p_folio,
                                                                  0,
                                                                  0,                 -- Codigo de rechazo
                                                                  "",
                                                                  "",                -- Fecha causa
                                                                  p_usuario,
                                                                  350)
                                                             INTO v_ax_sts_marcaje;

                                 -- Incrementa contador aceptados
                                 LET v_total_aceptados = v_total_aceptados + 1;
                              END IF
                           END IF
                        END IF
                     END IF   -- Marca origen 221

                     -- Si tiene la marca 223 - USO DE GARANTÍA 43BIS
                     IF(v_tmp_marca_origen = 223) THEN
                        --Verifica que la marca a conciliar sea una 225
                        IF(v_tmp_marca_prcr <> 4) THEN 
                           LET v_diagnostico = 14;   --> Tipo de registro no válido
                           LET v_total_rechazados = v_total_rechazados + 1;
                        ELSE
                           -- Búsca la marca activa 223
                           SELECT MAX(n_referencia)
                             INTO v_n_referencia
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_derechohabiente
                              AND marca = v_tmp_marca_origen;

                           IF(v_n_referencia IS NULL) THEN
                              LET v_diagnostico = 18;   --> Registro rechazado
                              LET v_total_rechazados = v_total_rechazados + 1;
                           ELSE
                              -- Verifica que no exista otra solicitud 225 con el mismo periodo
                              SELECT MAX(id_cre_uso_garantia)
                                INTO v_c_id_cre_uso_garantia
                                FROM cre_uso_garantia
                               WHERE id_derechohabiente = v_id_derechohabiente
                                 AND tpo_transferencia  = "43"
                                 AND estado IN(20,140,142)
                                 AND edo_procesar IN(10,70,80,85);

                              IF(v_c_id_cre_uso_garantia IS NOT NULL) THEN
                                 LET v_diagnostico = 17;   --> Registro repetido
                                 LET v_total_rechazados = v_total_rechazados + 1;
                              ELSE
                                 -- Recupera el monto anterior para solicitarlo a Procesar con la nueva marca de conciliación
                                 SELECT num_credito
                                   INTO v_num_credito
                                   FROM cre_uso_garantia
                                  WHERE id_cre_uso_garantia = v_n_referencia;

                                 -- Desmarca cuenta anterior
                                 EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                                     v_tmp_marca_origen,   -- Marca original
                                                                     v_n_referencia,
                                                                     0,
                                                                     0,
                                                                     p_usuario,
                                                                     350)                  -- Actualización marca SSPR
                                                                INTO v_err_desm;

                                 -- Realiza petición
                                 LET v_aux_id_cre_ug = seq_cre_uso.NEXTVAL;

                                 -- Crea solicitud
                                 INSERT INTO cre_uso_garantia (id_cre_uso_garantia,
                                                               id_cre_ctr_archivo ,
                                                               folio_liquida      ,
                                                               id_derechohabiente ,
                                                               tpo_transferencia  ,
                                                               tpo_uso            ,
                                                               num_credito        ,
                                                               f_presentacion     ,
                                                               f_movimiento       ,
                                                               periodo_pago       ,
                                                               importe_v97        ,
                                                               nss_afore          ,
                                                               rfc_afore          ,
                                                               paterno_afore      ,
                                                               materno_afore      ,
                                                               nombre_afore       ,
                                                               nom_imss           ,
                                                               edo_procesar       ,
                                                               diagnostico        ,
                                                               estado             ,
                                                               f_proceso)
                                                       VALUES (v_aux_id_cre_ug     ,
                                                               p_id_cre_ctr_arch   ,
                                                               0                   ,
                                                               v_id_derechohabiente,
                                                               "43"                ,          -- Cuenta crédito anualidades garantizadas
                                                               2                   ,
                                                               v_num_credito       ,
                                                               TODAY               ,
                                                               v_f_movimiento      ,
                                                               v_tmp_periodo_pago  ,
                                                               v_importe_97        ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               70                  ,
                                                               ""                  ,
                                                               142                 ,
                                                               TODAY);

                                 -- Marca la cuenta
                                 EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                                  v_marca_entra,
                                                                  v_aux_id_cre_ug,
                                                                  p_folio,
                                                                  0,
                                                                  0,             -- Codigo de rechazo
                                                                  "",
                                                                  "",            -- Fecha causa
                                                                  p_usuario,
                                                                  350)
                                                             INTO v_ax_sts_marcaje;

                                 -- Incrementa contador aceptados
                                 LET v_total_aceptados = v_total_aceptados + 1;
                              END IF
                           END IF
                        END IF
                     END IF   -- Marca origen 223

                     -- Si tiene la marca 225 - USO DE ANUALIDAD
                     IF(v_tmp_marca_origen = 225) THEN
                        --Verifica que la marca a conciliar sea una 223
                        IF(v_tmp_marca_prcr <> 2) THEN 
                           LET v_diagnostico = 14;   --> Tipo de registro no válido
                           LET v_total_rechazados = v_total_rechazados + 1;
                        ELSE
                           -- Búsca la marca activa 225
                           SELECT MAX(n_referencia)
                             INTO v_n_referencia
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_derechohabiente
                              AND marca = v_tmp_marca_origen;

                           IF(v_n_referencia IS NULL) THEN
                              LET v_diagnostico = 18;   --> Registro rechazado
                              LET v_total_rechazados = v_total_rechazados + 1;
                           ELSE
                              -- Verifica que no exista otra solicitud 223 con el mismo periodo
                              SELECT MAX(id_cre_uso_garantia)
                                INTO v_c_id_cre_uso_garantia
                                FROM cre_uso_garantia
                               WHERE id_derechohabiente = v_id_derechohabiente
                                 AND tpo_transferencia IN("18","48")
                                 AND periodo_pago = v_tmp_periodo_pago
                                 AND estado IN(20,140,142)
                                 AND edo_procesar IN(10,70,80,85,120);

                              IF(v_c_id_cre_uso_garantia IS NOT NULL) THEN
                                 LET v_diagnostico = 17;   --> Registro repetido
                                 LET v_total_rechazados = v_total_rechazados + 1;
                              ELSE
                                 -- Recupera el monto anterior para solicitarlo a Procesar con la nueva marca de conciliación
                                 SELECT num_credito
                                   INTO v_num_credito
                                   FROM cre_uso_garantia
                                  WHERE id_cre_uso_garantia = v_n_referencia;

                                 -- Desmarca cuenta anterior
                                 EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                                     v_tmp_marca_origen,   -- Marca original
                                                                     v_n_referencia,
                                                                     0,
                                                                     0,
                                                                     p_usuario,
                                                                     350)                  -- Actualización marca SSPR
                                                                INTO v_err_desm;

                                 -- Realiza petición
                                 LET v_aux_id_cre_ug = seq_cre_uso.NEXTVAL;

                                 -- Crea solicitud
                                 INSERT INTO cre_uso_garantia (id_cre_uso_garantia,
                                                               id_cre_ctr_archivo ,
                                                               folio_liquida      ,
                                                               id_derechohabiente ,
                                                               tpo_transferencia  ,
                                                               tpo_uso            ,
                                                               num_credito        ,
                                                               f_presentacion     ,
                                                               f_movimiento       ,
                                                               periodo_pago       ,
                                                               importe_v97        ,
                                                               nss_afore          ,
                                                               rfc_afore          ,
                                                               paterno_afore      ,
                                                               materno_afore      ,
                                                               nombre_afore       ,
                                                               nom_imss           ,
                                                               edo_procesar       ,
                                                               diagnostico        ,
                                                               estado             ,
                                                               f_proceso)
                                                       VALUES (v_aux_id_cre_ug     ,
                                                               p_id_cre_ctr_arch   ,
                                                               0                   ,
                                                               v_id_derechohabiente,
                                                               "18"                ,          -- Uso de la garantía 43Bis
                                                               3                   ,
                                                               0                   ,
                                                               TODAY               ,
                                                               v_f_movimiento      ,
                                                               v_tmp_periodo_pago  ,
                                                               v_importe_97        ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               ""                  ,
                                                               70                  ,
                                                               ""                  ,
                                                               142                 ,
                                                               TODAY);

                                 -- Marca la cuenta
                                 EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                                  v_marca_entra,
                                                                  v_aux_id_cre_ug,
                                                                  p_folio,
                                                                  0,
                                                                  0,                             -- Codigo de rechazo
                                                                  "",
                                                                  "",                            -- Fecha causa
                                                                  p_usuario,
                                                                  350)
                                                             INTO v_ax_sts_marcaje;

                                 -- Incrementa contador aceptados
                                 LET v_total_aceptados = v_total_aceptados + 1;
                              END IF
                           END IF
                        END IF
                     END IF   -- Marca origen 225
                  END IF   -- Valida Crédito vigente
               END IF   -- Valida Marca Procesar
            END IF   -- Valida Marca origen
         END IF   -- Valida registro repetido
      END IF   -- Valida derechohabiente

      -- Guarda diagnóstico del registro
      INSERT INTO cre_act_marca_sspr(nss               ,
                                     id_derechohabiente,
                                     folio             ,
                                     aivs_92           ,
                                     aivs_97           ,
                                     periodo_pago      ,
                                     tpo_solicitud     ,
                                     marca_orig        ,
                                     marca_prc         ,
                                     marca_act         ,
                                     diagnostico       ,
                                     f_proceso)
                              VALUES(v_tmp_nss           ,
                                     v_id_derechohabiente,
                                     p_folio             ,
                                     v_tmp_aivs_92       ,
                                     v_tmp_aivs_97       ,
                                     v_tmp_periodo_pago  ,
                                     v_tmp_tpo_solicitud ,
                                     v_tmp_marca_origen  ,
                                     v_tmp_marca_prcr    ,
                                     v_marca_entra       ,
                                     v_diagnostico       ,
                                     TODAY);
      -- Limpia variables
      LET v_marca_entra   = NULL;
      LET v_diagnostico   = 0;
      LET v_num_credito   = 0;
                                                                         
   END FOREACH;

   -- Valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   -- Ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_arch_proceso, p_folio, v_estado_arh_glo, p_usuario) INTO r_ax_bandera;

   -- Ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_folio, v_total_aceptados, v_total_rechazados, 0, p_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION;


