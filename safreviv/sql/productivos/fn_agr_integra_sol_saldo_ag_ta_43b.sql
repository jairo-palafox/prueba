






CREATE FUNCTION "safreviv".fn_agr_integra_sol_saldo_ag_ta_43b(p_usuario_cod    CHAR(20),
                                                   p_proceso_cod    SMALLINT,
                                                   p_nombre_archivo CHAR(40),
                                                   p_folio          DECIMAL(9,0),
                                                   p_pid            DECIMAL(9,0) )
   RETURNING INTEGER, INTEGER, CHAR(200), INTEGER,  INTEGER, INTEGER

   --Variables de tabla temporal
   DEFINE v_tmp_tipo_registro        CHAR(02);
   DEFINE v_tmp_nss                  CHAR(11);
   DEFINE v_tmp_aivs_92              DECIMAL(12,6);
   DEFINE v_tmp_aivs_97              DECIMAL(12,6);
   DEFINE v_tmp_periodo              CHAR(6);
   DEFINE v_tmp_tipo                 CHAR(2);
   DEFINE v_tmp_marca_prc            SMALLINT;
   DEFINE v_tmp_fec_sol_saldo        CHAR(8);

   --Variables de tabla de detalles
   DEFINE v_tipo_registro            CHAR(02);
   DEFINE v_nss                      CHAR(11);
   DEFINE v_aivs_92                  DECIMAL(12,6);
   DEFINE v_aivs_97                  DECIMAL(12,6);
   DEFINE v_periodo                  CHAR(4);
   DEFINE v_tipo                     CHAR(4);
   DEFINE v_marca_prc                SMALLINT;
   DEFINE v_precio_fondo             DECIMAL(19,14);

   DEFINE v_id_cre_uso_garantia      DECIMAL(9,0);
   DEFINE v_id_cre_uso_garantia_ag   DECIMAL(9,0);
   DEFINE v_folio_liquida            DECIMAL(9,0);
   DEFINE v_id_derechohabiente       DECIMAL(9,0);
   DEFINE v_id_derechohabiente_afi   DECIMAL(9,0);
   DEFINE v_tpo_transferencia        CHAR(2);
   DEFINE v_tpo_uso                  SMALLINT;
   DEFINE v_num_credito              DECIMAL(10,0);
   DEFINE v_f_presentacion           DATE;
   DEFINE v_f_movimiento             DATE;
   DEFINE v_periodo_pago             CHAR(6);
   DEFINE v_importe_v97              DECIMAL(12,2);
   DEFINE v_nss_afore                CHAR(11);
   DEFINE v_edo_procesar             SMALLINT;
   DEFINE v_estado                   SMALLINT;
   DEFINE v_f_proceso                DATE;
   DEFINE v_cod_resultado            SMALLINT;
   DEFINE v_diagnostico              SMALLINT;
   DEFINE v_tot_tpo_1                INTEGER;
   DEFINE v_tot_tpo_2                INTEGER;
   DEFINE v_id_cre_acreditado        DECIMAL(9,0);
   DEFINE v_c_id_cre_uso_garantia    DECIMAL(9,0);  
   DEFINE v_id_cre_acreditado_ag_rch DECIMAL(9,0);
   DEFINE v_proceso_cod_43b          SMALLINT;
   DEFINE v_id_cre_ctr_archivo_43b   DECIMAL(9,0);
   DEFINE v_folio_archivo_43b        DECIMAL(9,0);
   DEFINE v_proceso_cod_ag           SMALLINT;
   DEFINE v_id_cre_ctr_archivo_ag    DECIMAL(9,0);
   DEFINE v_folio_archivo_ag         DECIMAL(9,0);
   DEFINE v_c_id_cre_acreditado_ag   DECIMAL(9,0);
   DEFINE v_bnd_fecha                SMALLINT;

   -- Control de Excepciones
   DEFINE sql_err                    INTEGER;
   DEFINE isam_err                   INTEGER;
   DEFINE v_isam_err                 INTEGER;
   DEFINE err_txt                    CHAR(200);
   DEFINE v_resultado                SMALLINT;
   DEFINE v_tot_regs_insertados      INTEGER;
   DEFINE v_total_rechazados         INTEGER;
   DEFINE v_total_aceptados          INTEGER;

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      LET v_isam_err = isam_err;
      LET err_txt = v_tmp_nss;

      RETURN v_resultado,
             isam_err,
             err_txt,
             v_tot_regs_insertados,
             v_total_rechazados,
             v_total_aceptados;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/archivos/agr_integra_sol_saldo.trace";
   --TRACE ON;

   --Iniciar variables
   LET v_resultado                = 0;
   LET v_cod_resultado            = 1;
   LET sql_err                    = 0; 
   LET isam_err                   = 0; 
   LET v_isam_err                 = 0; 
   LET err_txt                    = "Integración finalizada exitosamente";
   LET v_tot_regs_insertados      = 0; 
   LET v_total_rechazados         = 0; 
   LET v_total_aceptados          = 0; 
   LET v_proceso_cod_43b          = 1202;
   LET v_proceso_cod_ag           = 302;
   LET v_aivs_92                  = 0;
   LET v_aivs_97                  = 0;
   LET v_periodo_pago             = 0;
   LET v_tmp_tipo_registro        = "";
   LET v_tmp_nss                  = "";
   LET v_tmp_aivs_92              = "";
   LET v_tmp_aivs_97              = "";
   LET v_tmp_periodo              = "";
   LET v_tmp_tipo                 = "";
   LET v_tmp_marca_prc            = "";
   LET v_tmp_fec_sol_saldo        = "";
   LET v_diagnostico              = 0;
   LET v_id_cre_ctr_archivo_43b   = 0;
   LET v_folio_archivo_43b        = 0;
   LET v_id_cre_ctr_archivo_ag    = 0;
   LET v_folio_archivo_ag         = 0;
   LET v_id_derechohabiente_afi   = "";
   LET v_c_id_cre_uso_garantia    = "";
   LET v_c_id_cre_acreditado_ag   = "";
   LET v_id_cre_uso_garantia_ag   = "";
   LET v_id_cre_acreditado        = "";
   LET v_id_cre_acreditado_ag_rch = "";
   LET v_f_proceso                = "";
   LET v_bnd_fecha                = 0;
   LET v_tot_tpo_1                = 0;
   LET v_tot_tpo_2                = 0;

   ---Identificadores para registros 43 bis
   EXECUTE FUNCTION fn_genera_id_archivo(v_proceso_cod_43b)
   INTO v_id_cre_ctr_archivo_43b, v_folio_archivo_43b;

   ---Identificadores para registros uso de anualidad
   EXECUTE FUNCTION fn_genera_id_archivo(v_proceso_cod_ag)
   INTO v_id_cre_ctr_archivo_ag, v_folio_archivo_ag;

   FOREACH
      SELECT tpo_registro,
             nss,
             aivs_92 /1000000,
             aivs_97 /1000000,
             periodo,
             tipo,
             marca_procesar,
             fec_sol_saldo
        INTO v_tmp_tipo_registro,
             v_tmp_nss,
             v_tmp_aivs_92,
             v_tmp_aivs_97,
             v_tmp_periodo,
             v_tmp_tipo,
             v_tmp_marca_prc,
             v_tmp_fec_sol_saldo
        FROM safre_tmp:tmp_det_sol_saldo

      --Se valida que el tipo de registro sea válido
      IF v_tmp_tipo_registro IS NULL OR v_tmp_tipo_registro = "" OR v_tmp_tipo_registro <> "02" THEN
         LET v_diagnostico      = 14; -- Rechazo tipo de registro
         LET v_cod_resultado    = 2;
         LET v_total_rechazados = v_total_rechazados + 1;
      ELSE
         --Se valida que el tipo de marca sea válido
         IF (v_tmp_marca_prc IS NULL OR v_tmp_marca_prc = "") OR 
            (v_tmp_marca_prc <> 1 AND v_tmp_marca_prc <> 2 AND v_tmp_marca_prc <> 4) THEN
            LET v_diagnostico      = 16; -- Rechazo tipo de registro
            LET v_cod_resultado    = 2;
            LET v_total_rechazados = v_total_rechazados + 1;
         ELSE
            IF v_tmp_tipo IS NULL OR v_tmp_tipo = "" OR
               NOT EXISTS (SELECT tpo_solicitud
                             FROM cat_tipo_solic_sdo
                            WHERE tpo_solicitud = v_tmp_tipo) THEN
               LET v_diagnostico      = 14; -- Rechazo tipo de registro
               LET v_cod_resultado    = 2;
               LET v_total_rechazados = v_total_rechazados + 1;
            ELSE
               -- Se valida que nss exista derechohabiente
               IF v_tmp_nss IS NULL OR v_tmp_nss = "" OR v_tmp_nss = "           " THEN
                  LET v_diagnostico      = 11; -- Rechazo tipo de registro
                  LET v_cod_resultado    = 2;
                  LET v_total_rechazados = v_total_rechazados + 1;
               ELSE
                  --Obtiene el id_derechohabiente de SAFRE
                  SELECT id_derechohabiente
                    INTO v_id_derechohabiente_afi
                    FROM afi_derechohabiente 
                   WHERE nss = v_tmp_nss;

                  --Si el Derechohabiente no existe se RECHAZA
                  IF v_id_derechohabiente_afi IS NULL THEN
                     LET v_diagnostico      = 11; --Trabajador no existe en base de derechohabientes
                     LET v_cod_resultado    = 2;
                     LET v_total_rechazados = v_total_rechazados + 1;
                  ELSE
                     EXECUTE FUNCTION fn_verifica_fecha(v_tmp_fec_sol_saldo,"mmddyyyy")
                     INTO v_bnd_fecha, v_f_proceso;

                     IF v_bnd_fecha = 0 THEN  -- Se valida que la fecha de solicitud de saldo sea válida
                        LET v_diagnostico      = 23; -- Rechazo tipo de registro
                        LET v_cod_resultado    = 2;
                        LET v_total_rechazados = v_total_rechazados + 1;
                     ELSE
                       --Se valida que exista el registro en cre_Acreditado, independientemente de la vigencia del crédito
                       SELECT MAX (id_cre_acreditado)
                         INTO v_id_cre_acreditado
                         FROM cre_acreditado c, cat_maq_credito m
                        WHERE id_derechohabiente = v_id_derechohabiente_afi
                          AND c.estado = m.estado
                      --  AND m.entidad = 1
                          AND c.tpo_originacion = v_tmp_marca_prc;

                       IF v_id_cre_acreditado IS NULL THEN
                          LET v_diagnostico      = 13; --No existe marca crédito vigente
                          LET v_cod_resultado    = 2;
                          LET v_total_rechazados = v_total_rechazados + 1;
                       ELSE
                          -- Valida registro TA
                          IF (v_tmp_marca_prc = 1) THEN
                             SELECT count(*)
                               INTO v_tot_tpo_1
                               FROM cre_solic_sdo
                              WHERE nss = v_tmp_nss
                                AND folio = p_folio;

                             -- Valida registros duplicados TA
                             IF v_tot_tpo_1 = 1 THEN
                                LET v_diagnostico      = 17; --Registro repetido
                                LET v_cod_resultado    = 2;
                                LET v_total_rechazados = v_total_rechazados + 1;
                             ELSE
                                IF EXISTS(SELECT id_derechohabiente
                                            FROM cre_acreditado c, cat_maq_credito m
                                           WHERE id_derechohabiente = v_id_derechohabiente_afi
                                             AND c.estado = m.estado
                                             AND c.edo_procesar IN (60,70,80,85)
                                             AND m.entidad = 1
                                             AND c.tpo_originacion = v_tmp_marca_prc) THEN
                                  {
                                   LET v_diagnostico      = 12;  --Existe crédito anterior
                                   LET v_cod_resultado    = 2;
                                   LET v_total_rechazados = v_total_rechazados + 1;
                                  }
                                END IF
                             END IF
                          ELSE
                             -- Valida registros UA
                             IF (v_tmp_marca_prc = 4) THEN
                                -- Validación monto solicitado UA
                                IF (v_tmp_aivs_92 IS NULL OR v_tmp_aivs_92 = 0) AND
                                   (v_tmp_aivs_97 IS NULL OR v_tmp_aivs_97 = 0) THEN
                                   LET v_diagnostico      = 33; -- Rechazo tipo de registro
                                   LET v_cod_resultado    = 2;
                                   LET v_total_rechazados = v_total_rechazados + 1;
                                ELSE
                                   SELECT COUNT (*)
                                     INTO v_tot_tpo_1
                                     FROM cre_solic_sdo
                                    WHERE nss = v_tmp_nss
                                      AND folio = p_folio;

                                   -- Valida registros duplicados UA
                                   IF v_tot_tpo_1 = 1 THEN
                                      LET v_diagnostico      = 17; --Registro repetido
                                      LET v_cod_resultado    = 2;
                                      LET v_total_rechazados = v_total_rechazados + 1;
                                   ELSE
                                      IF EXISTS(SELECT id_derechohabiente
                                                  FROM cre_acreditado c, cat_maq_credito m
                                                 WHERE id_derechohabiente = v_id_derechohabiente_afi
                                                   AND c.estado = m.estado
                                                   AND c.edo_procesar in(60,70,80,85)
                                                   AND m.entidad = 1
                                                   AND c.tpo_originacion = v_tmp_marca_prc) THEN
                                        {
                                         LET v_diagnostico      = 12;  --Existe crédito anterior
                                         LET v_cod_resultado    = 2;
                                         LET v_total_rechazados = v_total_rechazados + 1;
                                        }
                                      ELSE
                                         -- Valida que no haya un registro activo mismo periodo
                                         SELECT MAX(id_cre_uso_garantia)
                                           INTO v_c_id_cre_uso_garantia
                                           FROM cre_uso_garantia
                                          WHERE id_derechohabiente = v_id_derechohabiente_afi
                                           AND tpo_transferencia = "43"
                                           AND estado IN(20,140,142)
                                           AND edo_procesar IN(10,70,80,85);

                                         IF v_c_id_cre_uso_garantia IS NOT NULL THEN
                                           {
                                            LET v_diagnostico      = 17;  --Registro repetido
                                            LET v_cod_resultado    = 2;
                                            LET v_total_rechazados = v_total_rechazados + 1;
                                           }
                                         END IF
                                      END IF
                                   END IF
                                END IF  --Fin de validación monto solicitado UA
                             ELSE
                                -- Validación monto solicitado 43bis
                                IF (v_tmp_aivs_97 IS NULL OR v_tmp_aivs_97 = 0) THEN
                                   LET v_diagnostico      = 33; -- Rechazo tipo de registro
                                   LET v_cod_resultado    = 2;
                                   LET v_total_rechazados = v_total_rechazados + 1;
                                ELSE
                                   -- Validación periodo pago
                                   IF v_tmp_periodo IS NULL OR v_tmp_periodo = "" OR v_tmp_periodo = "000000" OR
                                      v_tmp_periodo[1,4] < "2012" OR v_tmp_periodo[5,6] > "12" OR v_tmp_periodo[5,6] < "01" THEN
                                      LET v_diagnostico      = 23; -- Rechazo tipo de registro
                                      LET v_cod_resultado    = 2;
                                      LET v_total_rechazados = v_total_rechazados + 1;
                                   ELSE
                                      --Se valida que no existan registros duplicados 43 bis
                                      SELECT COUNT (*)
                                        INTO v_tot_tpo_2
                                        FROM cre_solic_sdo
                                       WHERE nss = v_tmp_nss
                                         AND periodo_pago = v_tmp_periodo
                                         AND folio = p_folio;

                                      IF v_tot_tpo_2 > 1 THEN
                                         LET v_diagnostico      = 17; --Registro repetido
                                         LET v_cod_resultado    = 2;
                                         LET v_total_rechazados = v_total_rechazados + 1;
                                      ELSE
                                         -- Valida que no haya un registro activo mismo periodo
                                         SELECT MAX(id_cre_uso_garantia)
                                           INTO v_c_id_cre_uso_garantia
                                           FROM cre_uso_garantia
                                          WHERE id_derechohabiente = v_id_derechohabiente_afi
                                           AND tpo_transferencia IN("18","48")
                                           AND periodo_pago = v_tmp_periodo
                                           AND estado IN(20,140,142)
                                           AND edo_procesar IN(10,70,80,85,120);

                                         IF v_c_id_cre_uso_garantia IS NOT NULL THEN
                                           {
                                            LET v_diagnostico      = 17;  --Registro repetido
                                            LET v_cod_resultado    = 2;
                                            LET v_total_rechazados = v_total_rechazados + 1;
                                           }
                                         END IF  --Fin de validación que no haya un registro activo mismo periodo
                                      END IF  --Fin de validación que no existan registros duplicados 43 bis
                                   END IF  --Fin de validación de periodo pago
                                END IF  --Fin de validación monto solicitado 43bis
                             END IF  --Fin de validación registros UA
                          END IF  --Fin de validación registros TA
                       END IF  --Fin validación que cada registro tenga crédito vigente
                     END IF  --Fin de validación que la fecha de solicitud de saldo sea válida
                  END IF  --Fin Derechohabiente no existe se RECHAZA
               END IF  --Fin de validación que exista derechohabiente
            END IF  --Fin de validación que exista tipo de solicitud de saldo
         END IF  --Fin de validación que el tipo de marca sea válido
      END IF   --Fin de validación que el tipo de registro sea válido

      -- Realiza asignación de las aivs sea rechazado o aceptado el registro
      LET v_aivs_92 = v_tmp_aivs_92;
      LET v_aivs_97 = v_tmp_aivs_97;

      IF v_diagnostico = 0 THEN
         -- Valida si la marca 1 Archivo TA
         IF v_tmp_marca_prc = 1 THEN
            IF v_id_cre_acreditado IS NOT NULL THEN
               UPDATE cre_acreditado
                  SET estado = 142,
                      edo_procesar = 70
               WHERE  id_cre_acreditado = v_id_cre_acreditado;

               LET v_total_aceptados = v_total_aceptados + 1 ;
            ELSE
               LET v_diagnostico      = 26; --Registro sin originación de crédito
               LET v_cod_resultado    = 2;
               LET v_total_rechazados = v_total_rechazados + 1;
            END IF
         ELSE
            LET v_f_movimiento = MDY(month(v_tmp_fec_sol_saldo),1,year(v_tmp_fec_sol_saldo));
            LET v_f_movimiento = v_f_movimiento + 1 units month;
            LET v_periodo_pago = v_tmp_periodo;

            SELECT precio_fondo
              INTO v_precio_fondo
              FROM glo_valor_fondo
             WHERE f_valuacion = v_f_movimiento
               AND fondo = 11;

            LET v_importe_v97 = ((v_aivs_92 + v_aivs_97) * v_precio_fondo);

            -- Valida si la marca 2 Archivo 43Bis
            IF v_tmp_marca_prc = 2 THEN
               INSERT INTO cre_uso_garantia (id_cre_uso_garantia,
                                             id_cre_ctr_archivo,
                                             folio_liquida,
                                             id_derechohabiente,
                                             tpo_transferencia,
                                             tpo_uso,
                                             num_credito,
                                             f_presentacion,
                                             f_movimiento,
                                             periodo_pago,
                                             importe_v97,
                                             nss_afore,
                                             rfc_afore,
                                             paterno_afore,
                                             materno_afore,
                                             nombre_afore,
                                             nom_imss,
                                             edo_procesar,
                                             diagnostico,
                                             estado,
                                             f_proceso)
                                     VALUES (seq_cre_uso.NEXTVAL,
                                             v_id_cre_ctr_archivo_43b,
                                             0,
                                             v_id_derechohabiente_afi,
                                             "18",
                                             2,
                                             0,
                                             v_f_proceso,
                                             v_f_movimiento, --1er día natural del mes siguente a today,
                                             v_periodo_pago,
                                             v_importe_v97,
                                             v_tmp_nss,
                                             "",
                                             "",
                                             "",
                                             "",
                                             "",
                                             70,
                                             "",
                                             142,
                                             TODAY);

               LET v_total_aceptados = v_total_aceptados + 1;
            ELSE
               SELECT MAX(id_cre_acreditado)
                 INTO v_c_id_cre_acreditado_ag
                 FROM cre_acreditado c, cat_maq_credito m, cat_tipo_credito t
                WHERE id_derechohabiente = v_id_derechohabiente_afi
                  AND c.estado = m.estado
               -- AND m.entidad = 1
                  AND c.tpo_originacion = v_tmp_marca_prc
                  AND c.tpo_originacion = t.tpo_originacion
                  AND c.tpo_credito     = t.tpo_credito;

               -- Valida si el acreditado vigente AG
               IF v_c_id_cre_acreditado_ag IS NULL THEN
                  LET v_diagnostico      = 26; --Registro sin originación de crédito
                  LET v_cod_resultado    = 2;
                  LET v_total_rechazados = v_total_rechazados + 1;
               ELSE
                  SELECT MAX(id_cre_uso_garantia)
                    INTO v_id_cre_uso_garantia_ag
                    FROM cre_uso_garantia
                   WHERE id_derechohabiente = v_id_derechohabiente_afi
                     AND tpo_transferencia = '43'
                     AND estado IN (20,140,142)
                     AND edo_procesar IN (10,70,80,85);

                  -- Valida si hay un registro pendiente de UA
                  IF v_id_cre_uso_garantia_ag IS NULL OR v_id_cre_uso_garantia_ag = "" THEN
                     SELECT num_credito
                       INTO v_num_credito
                       FROM cre_acreditado
                      WHERE id_cre_acreditado = v_c_id_cre_acreditado_ag;

                     IF v_num_credito = 0 OR v_num_credito IS NULL OR v_num_credito = "" THEN
                        LET v_num_credito = 1;
                     END IF

                     INSERT INTO cre_uso_garantia (
                                 id_cre_uso_garantia,
                                 id_cre_ctr_archivo,
                                 folio_liquida,
                                 id_derechohabiente,
                                 tpo_transferencia,
                                 tpo_uso,
                                 num_credito,
                                 f_presentacion,
                                 f_movimiento,
                                 periodo_pago,
                                 importe_v97,
                                 nss_afore,
                                 rfc_afore,
                                 paterno_afore,
                                 materno_afore,
                                 nombre_afore,
                                 nom_imss,
                                 edo_procesar,
                                 diagnostico,
                                 estado,
                                 f_proceso)
                         VALUES (seq_cre_uso.NEXTVAL,
                                 v_id_cre_ctr_archivo_ag,
                                 0,
                                 v_id_derechohabiente_afi,
                                 "43",
                                 2,
                                 v_num_credito,
                                 v_f_proceso,
                                 v_f_movimiento, --1er día natural del mes siguente a today,
                                 "",
                                 v_importe_v97,
                                 v_tmp_nss,
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 70,
                                 "",
                                 142,
                                 TODAY);

                     LET v_total_aceptados = v_total_aceptados + 1 ;
                  ELSE
                     LET v_diagnostico = 26;  --Registro sin originación de crédito
                     LET v_cod_resultado = 2;
                     LET v_total_rechazados = v_total_rechazados + 1;
                  END IF -- Fin de validación si hay un registro pendiente de UA
               END IF  -- Fin de validación si el acreditado vigente AG
            END IF -- Fin de validación si la marca 2 Archivo 43Bis
         END IF -- Fin de validación si la marca 1 Archivo TA,
      END IF -- Fin de validación de diagnóstico

      INSERT INTO cre_solic_sdo 
          VALUES (p_folio,
                  v_id_derechohabiente_afi,
                  v_tmp_nss,
                  v_aivs_92,
                  v_aivs_97,
                  v_tmp_periodo,
                  v_tmp_tipo,
                  v_tmp_marca_prc,
                  v_cod_resultado,
                  v_diagnostico,
                  p_usuario_cod,
                  TODAY);

      LET v_tot_regs_insertados = v_tot_regs_insertados + 1;

      LET v_tmp_tipo_registro        = "";
      LET v_tmp_nss                  = "";
      LET v_tmp_aivs_92              = "";
      LET v_tmp_aivs_97              = "";
      LET v_tmp_periodo              = "";
      LET v_tmp_tipo                 = "";
      LET v_tmp_marca_prc            = "";
      LET v_tmp_fec_sol_saldo        = "";
      LET v_id_derechohabiente_afi   = "";
      LET v_id_cre_acreditado        = "";
      LET v_c_id_cre_uso_garantia    = "";
      LET v_diagnostico              = 0;
      LET v_cod_resultado            = 1;
      LET v_c_id_cre_uso_garantia    = "";
      LET v_c_id_cre_acreditado_ag   = "";
      LET v_id_cre_uso_garantia_ag   = "";
      LET v_id_cre_acreditado        = "";
      LET v_id_cre_acreditado_ag_rch = "";
      LET v_aivs_92                  = 0;
      LET v_aivs_97                  = 0;
      LET v_periodo_pago             = 0;
      LET v_f_proceso                = "";
      LET v_bnd_fecha                = 0;
      LET v_tot_tpo_1                = 0;
      LET v_tot_tpo_2                = 0;


   END FOREACH;

   IF v_resultado < 0 THEN
      UPDATE bat_ctr_operacion
         SET folio       = p_folio,
             nom_archivo = p_nombre_archivo
       WHERE proceso_cod = p_proceso_cod
        AND opera_cod   = 2
        AND pid         = p_pid;
   END IF

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
     SET folio     = p_folio,
          estado    = 2 -- integrado
    WHERE proceso_cod = p_proceso_cod
     AND  opera_cod   = 1   -- etapa de carga
     AND  estado      = 1;  -- archivo cargado

   SELECT nom_archivo
     INTO p_nombre_archivo
     FROM bat_ctr_operacion
    WHERE pid = p_pid
      AND opera_cod = 1;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 2
      AND pid         = p_pid;

   UPDATE statistics FOR TABLE cre_acreditado;
   UPDATE statistics FOR TABLE cre_uso_garantia;

   RETURN v_resultado,
          v_isam_err,
          err_txt,
          v_tot_regs_insertados, -- Totales
          v_total_rechazados,    -- Rechazadas
          v_total_aceptados;     -- Aceptados

END FUNCTION;


