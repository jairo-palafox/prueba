






CREATE FUNCTION "safreviv".fn_agr_act_deudor(p_usuario     CHAR(20),
                                  p_proceso_cod SMALLINT)

   RETURNING SMALLINT,
             INTEGER,
             DECIMAL(12,2),
             INTEGER,
             DECIMAL(12,2),
             INTEGER,
             DECIMAL(12,2)

   DEFINE v_error                   SMALLINT;
   DEFINE v_lote_procesa            INTEGER;
   DEFINE v_lote_acept              INTEGER;
   DEFINE v_lote_rch                INTEGER;
   DEFINE v_lote_sum_monto_procesa  DECIMAL(12,2);
   DEFINE v_lote_sum_monto_acep     DECIMAL(12,2);
   DEFINE v_lote_sum_monto_rch      DECIMAL(12,2);
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_sdo_deudor              DECIMAL(12,2);
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_f_otorga                DATE;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_entidad                 SMALLINT;
   DEFINE v_tot_vigente             INTEGER;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_folio_archivo           DECIMAL(9,0);
   DEFINE v_marca_inf               SMALLINT;
   DEFINE v_resp_marca              SMALLINT;
   DEFINE v_ind_vigente             SMALLINT;

   ON EXCEPTION SET v_error
      RETURN v_error,
             v_lote_procesa,
             v_lote_sum_monto_procesa,
             v_lote_acept,
             v_lote_sum_monto_acep,
             v_lote_rch,
             v_lote_sum_monto_rch;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_agr_act_deudor.trace';
   ---TRACE ON;

   --Inicializa valores
   LET v_error                  = 0;
   LET v_lote_procesa           = 0;
   LET v_lote_sum_monto_procesa = 0;
   LET v_lote_acept             = 0;
   LET v_lote_sum_monto_acep    = 0;
   LET v_lote_rch               = 0;
   LET v_lote_sum_monto_rch     = 0;
   LET v_num_credito            = NULL;
   LET v_sdo_deudor             = 0;
   LET v_id_cre_acreditado      = NULL;
   LET v_id_derechohabiente     = NULL;
   LET v_f_otorga               = NULL;
   LET v_estado                 = NULL;
   LET v_entidad                = NULL;
   LET v_tot_vigente            = 0;
   LET v_tpo_credito            = NULL;
   LET v_folio_archivo          = 0;
   LET v_marca_inf              = NULL;
   LET v_resp_marca             = 0;
   LET v_ind_vigente            = 0;

   FOREACH
      --En la tmp puede presentar más de un registro por número de crédito por lo
      --cual se agrupa y sumariza todos los montos qe correspondan a un número de crédito.
      SELECT num_credito,
         SUM (monto_deudor)
        INTO v_num_credito,
             v_sdo_deudor
        FROM safre_tmp:tmp_deudor_sap_fico
       WHERE monto_deudor > 0
       GROUP BY num_credito

      -- Incrementa total registros procesados
      LET v_lote_procesa = v_lote_procesa + 1;
      LET v_lote_sum_monto_procesa = v_lote_sum_monto_procesa + v_sdo_deudor;

      -- Recupera inf. del acreditado
      /* 18  (TRÁMITE)
         20  (VIGENTE MARCADA)
         140 (VIGENTE DEUDOR LIQUIDADO)
         145 (VIGENTE DEUDOR LIQUIDADO)
         148 (LIQUIDADA MINISTRACIÓN)
         170 (CRÉDITO LIQUIDADO)
         174 (CRÉDITO CANCELADO)
         290 (TRÁMITE/CRÉDITO VENCIDO)
      */

      FOREACH
         SELECT FIRST 1 
                acr.id_cre_acreditado,
                acr.id_derechohabiente,
                acr.tpo_credito,
                acr.f_otorga,
                acr.estado,
                maq.entidad,
                arh.folio_archivo
           INTO v_id_cre_acreditado,
                v_id_derechohabiente,
                v_tpo_credito,
                v_f_otorga,
                v_estado,
                v_entidad,
                v_folio_archivo
           FROM cre_acreditado acr,
                cat_maq_credito maq,
                cre_ctr_archivo arh
          WHERE acr.num_credito     = v_num_credito
            AND acr.tpo_originacion = 4
            AND acr.estado IN (18,20,140,145,148,170,174,290)
            AND acr.estado = maq.estado
            AND acr.id_cre_ctr_archivo = arh.id_cre_ctr_archivo
          --AND acr.edo_procesar <= 120
          ORDER BY maq.entidad, acr.f_otorga
      END FOREACH;

      IF(v_id_cre_acreditado IS NOT NULL) THEN

         -- Verifica que no exista más de un crédito vigente con el mismo número de crédito
         SELECT COUNT(*)
           INTO v_tot_vigente
           FROM cre_acreditado c,
                cat_maq_credito q
          WHERE c.id_derechohabiente <> v_id_derechohabiente
            AND c.num_credito = v_num_credito
            AND c.estado      = q.estado
            AND q.entidad     = 1;  -- Crédito vigente

         IF (v_tot_vigente > 0) THEN
            INSERT INTO safre_tmp:tmp_sin_nss_deudor(
                                     num_credito,
                                     diagnostico)
                              VALUES(v_num_credito,
                                     21); -- Número de crédito ya existe

            LET v_lote_rch = v_lote_rch + 1;
            LET v_lote_sum_monto_rch = v_lote_sum_monto_rch + v_sdo_deudor;
            CONTINUE FOREACH;
         END IF

         -- Obtiene marca infonavit
         IF(v_estado = 18) THEN
            LET v_marca_inf = 213;   -- CRÉDITO EN TRÁMITE
         ELSE
            SELECT marca_inf
              INTO v_marca_inf
              FROM cat_tipo_credito
             WHERE tpo_credito     = v_tpo_credito
               AND tpo_originacion = 4;
         END IF

         -- Actualiza solo los creditos en trámite, vigentes y liquidados.
         IF(v_estado = 18)  OR
           (v_estado = 20)  OR
           (v_estado = 140) OR
           (v_estado = 145) OR
           (v_estado = 148) OR
           (v_estado = 170) THEN

           -- Si el crédito está liquidado verifica si existe un crédito vigente
           -- para ese derechohabiente con otro número de crédito
           IF(v_estado = 170) THEN
              IF EXISTS(SELECT acr.id_derechohabiente
                          FROM cre_acreditado acr,
                               cat_maq_credito maq
                         WHERE acr.id_derechohabiente = v_id_derechohabiente
                           AND acr.num_credito <> v_num_credito
                           AND acr.estado  = maq.estado
                           AND maq.entidad = 1) THEN
                 LET v_ind_vigente = 1; -- Levanta bandera
              END IF
           END IF

            -- Verifica existencia de marca activa
            IF NOT EXISTS(SELECT id_derechohabiente
                            FROM sfr_marca_activa
                           WHERE id_derechohabiente = v_id_derechohabiente
                             AND marca = v_marca_inf) THEN

               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,-- id_derechohabiente
                                                v_marca_inf,         -- Marca a generar
                                                v_id_cre_acreditado, -- Referencia id_cre_acreditado
                                                v_folio_archivo,     -- Folio
                                                0,                   -- Estado de la marca 
                                                0,                   -- Código de rechazo
                                                NULL,                -- Marca causa
                                                "",                  -- Fecha causa
                                                p_usuario,           -- Usuario
                                                p_proceso_cod)       -- Proceso
                                           INTO v_resp_marca;
            END IF

            -- Actualiza registro para petición de saldo
            IF(v_estado = 18) THEN
               UPDATE cre_acreditado
                  SET estado       = 18,
                      edo_procesar = 70,
                      sdo_deudor   = v_sdo_deudor
                WHERE id_cre_acreditado = v_id_cre_acreditado;
            ELSE
               UPDATE cre_acreditado
                  SET estado       = 20,
                      edo_procesar = 70,
                      sdo_deudor   = v_sdo_deudor
                WHERE id_cre_acreditado = v_id_cre_acreditado;
            END IF

            -- Verifica que no exista un movimiento para el mismo deudor
            IF NOT EXISTS(SELECT id_cre_acreditado
                            FROM cre_saldo_deudor
                           WHERE id_cre_acreditado = v_id_cre_acreditado
                             AND movimiento  = 1781) THEN

               INSERT INTO cre_saldo_deudor
                  VALUES(v_id_cre_acreditado,
                         v_folio_archivo    ,
                         today              ,
                         1781               ,
                         v_id_cre_acreditado,
                         0                  ,
                         v_sdo_deudor       ,
                         today);
            ELSE
               INSERT INTO cre_saldo_deudor
                  SELECT id_cre_acreditado,
                        folio_referencia ,
                        f_movimiento     ,
                        1781             ,
                        id_referencia    ,
                        monto_aivs       ,
                        monto_pesos      ,
                        f_proceso
                   FROM cre_saldo_deudor
                  WHERE id_cre_acreditado = v_id_cre_acreditado
                    AND movimiento = 181;

               UPDATE cre_saldo_deudor
                  SET monto_pesos = v_sdo_deudor
                WHERE id_cre_acreditado = v_id_cre_acreditado
                  AND movimiento = 181;
            END IF

            LET v_lote_sum_monto_acep = v_lote_sum_monto_acep + v_sdo_deudor;
            LET v_lote_acept = v_lote_acept + 1;

         END IF -- ESTADOS ACTUALIZA

         --Inserta los registros aceptados, se incluyen los cancelados y trámites vencidos(Pero no se actualiza el deudor)
         INSERT INTO safre_tmp:tmp_deudor_agr(
                                  id_cre_acreditado ,
                                  id_derechohabiente,
                                  num_credito_arh   ,   -- Número de crédito cargado en el archivo
                                  ind_vigente       ,   -- Indicador de que está liquidado y tiene otro crédito vigente
                                  sdo_deudor_act    ,
                                  estado            ,
                                  f_actualiza)
                          VALUES (v_id_cre_acreditado ,
                                  v_id_derechohabiente,
                                  v_num_credito       ,
                                  v_ind_vigente       ,
                                  v_sdo_deudor        ,
                                  v_estado            ,
                                  TODAY);
      ELSE
         INSERT INTO safre_tmp:tmp_sin_nss_deudor(
                                  num_credito,
                                  diagnostico)
                           VALUES(v_num_credito,
                                  13);  -- No existe marca crédito vigente

         LET v_lote_rch = v_lote_rch + 1;
         LET v_lote_sum_monto_rch = v_lote_sum_monto_rch + v_sdo_deudor;
      END IF

      -- Resetea variables
      LET v_id_cre_acreditado = NULL;
      LET v_tpo_credito       = NULL;
      LET v_estado            = NULL;
      LET v_marca_inf         = NULL;
      LET v_folio_archivo     = 0;
      LET v_ind_vigente       = 0;

   END FOREACH

   RETURN v_error,
          v_lote_procesa,
          v_lote_sum_monto_procesa,
          v_lote_acept,
          v_lote_sum_monto_acep,
          v_lote_rch,
          v_lote_sum_monto_rch;

END FUNCTION
;


