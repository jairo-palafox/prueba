






CREATE FUNCTION "safreviv".fn_agr_obtiene_remanente(p_monto_inicio  DECIMAL(13,2),
                                         p_monto_fin     DECIMAL(13,2),
                                         p_f_origina_ini DATE,
                                         p_f_origina_fin DATE)

   RETURNING SMALLINT,
             INTEGER,
             VARCHAR(250);

   DEFINE v_error                  SMALLINT;
   DEFINE v_isam_err               INTEGER;
   DEFINE v_c_msj                  VARCHAR(250);
   DEFINE v_busca_credito          INTEGER;
   DEFINE v_sqry_credito           CHAR(90);
   DEFINE v_sqry_f_otorga          CHAR(60);
   DEFINE v_sqry_monto             CHAR(90);
   DEFINE v_sqry_global            CHAR(1000);
   DEFINE v_qry_no_prospecto       CHAR(200);
   DEFINE v_precio_fondo           DECIMAL(19,14);
   DEFINE v_tmp_nss                CHAR(11);
   DEFINE v_tmp_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_tmp_id_derechohabiente DECIMAL(9,0);
   DEFINE v_tmp_tpo_originacion    SMALLINT;
   DEFINE v_tmp_marca_prcr         SMALLINT;
   DEFINE v_tmp_tpo_credito        SMALLINT;
   DEFINE v_tmp_marca_ifv          SMALLINT;
   DEFINE v_tmp_num_credito        DECIMAL(10,0);
   DEFINE v_tmp_sdo_deudor         DECIMAL(12,2);
   DEFINE v_tmp_f_otorga           DATE;
   DEFINE v_tmp_estado             SMALLINT;
   DEFINE v_tmp_edo_procesar       SMALLINT;
   DEFINE v_diagnostico            SMALLINT;
   DEFINE v_aux_marca_ifv          SMALLINT;
   DEFINE v_aux_marca_prc          SMALLINT;
   DEFINE v_pesos_92               DECIMAL(16,2);
   DEFINE v_aivs_92                DECIMAL(16,2);
   DEFINE v_pesos_97               DECIMAL(16,2);
   DEFINE v_aivs_97                DECIMAL(16,2);
   DEFINE v_total_saldo            DECIMAL(18,2);
   DEFINE v_total_aivs             DECIMAL(18,2);
   DEFINE v_marca_impide           SMALLINT;
   DEFINE v_ind_convive            SMALLINT;
   DEFINE v_rch_cod                SMALLINT;
   DEFINE v_bnd_monto              SMALLINT;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj;
   END EXCEPTION

   /*
   --SET DEBUG FILE TO '/safreviv_int/archivos/obtiene_remanentes_agr.trace';
   --TRACE ON;
   */

   -- Inicializa variables
   LET v_error         = 0;
   LET v_isam_err      = 0;
   LET v_c_msj         = 'El proceso finalizó correctamente';
   LET v_busca_credito = 0;
   LET v_sqry_monto    = NULL;
   LET v_sqry_f_otorga = NULL;
   LET v_diagnostico   = 0; -- Diagnóstico aceptado
   LET v_aux_marca_ifv = NULL;
   LET v_aux_marca_prc = NULL;
   LET v_pesos_92      = 0;
   LET v_aivs_92       = 0;
   LET v_pesos_97      = 0;
   LET v_aivs_97       = 0;
   LET v_total_saldo   = 0;
   LET v_total_aivs    = 0;
   LET v_marca_impide  = NULL;
   LET v_ind_convive   = 0;
   LET v_rch_cod       = 0;
   LET v_bnd_monto     = 0;

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
      AND fondo       = 11;

   DROP TABLE IF EXISTS tmp_no_prospecto;

   -- Crea cadena en caso de recibir fecha de originación
   IF(p_f_origina_ini IS NOT NULL) AND (p_f_origina_fin IS NULL) THEN
      LET v_sqry_f_otorga = " AND c.f_otorga >= '"||p_f_origina_ini||"'";
   ELIF (p_f_origina_ini IS NULL) AND (p_f_origina_fin IS NOT NULL) THEN
      LET v_sqry_f_otorga = " AND c.f_otorga <= '"||p_f_origina_fin||"'";
   ELIF (p_f_origina_ini IS NOT NULL) AND (p_f_origina_fin IS NOT NULL) THEN
      LET v_sqry_f_otorga = " AND c.f_otorga BETWEEN '"||p_f_origina_ini||"'"||" AND '"||p_f_origina_fin||"'";
   END IF

   -- Crea cadena en caso de recibir el tipo de crédito.
   SELECT COUNT(*)
     INTO v_busca_credito
     FROM safre_tmp:tmp_parametro_credito;

   IF (v_busca_credito > 0) THEN
      LET v_sqry_credito = " AND c.tpo_credito IN (SELECT tpo_credito FROM safre_tmp:tmp_parametro_credito) ";
   ELSE
      LET v_sqry_credito = " AND c.tpo_credito IN (1,3,4,5,10,11,15,29,30) ";
   END IF

   -- Establece prioridad
   SET PDQPRIORITY HIGH;

   LET v_sqry_global = " INSERT INTO tmp_remanente_agr"||
                       " SELECT a.nss, c.id_cre_acreditado, c.id_derechohabiente, c.tpo_originacion, DECODE(c.tpo_originacion,1,231,4,234), c.tpo_credito,"||
                       " DECODE(c.tpo_credito,1,201,3,203,4,204,5,205,10,210,11,211,15,215,29,229,30,230), c.num_credito, c.sdo_deudor, c.f_otorga, c.estado, c.edo_procesar"||
                       " FROM cre_acreditado c, afi_derechohabiente a WHERE c.id_derechohabiente = a.id_derechohabiente "||v_sqry_credito||v_sqry_f_otorga||
                       " AND c.estado IN (140,145,220,900) AND c.edo_procesar = 120";

   -- TRACE 'v_squery_global = ' || v_sqry_global;
   EXECUTE IMMEDIATE v_sqry_global;

   CREATE INDEX xpk_nss_remanente ON tmp_remanente_agr(nss);
   CREATE INDEX xpk_id_dh_remanente ON tmp_remanente_agr(id_derechohabiente);
   CREATE INDEX xpk_id_acre_remanente ON tmp_remanente_agr(id_cre_acreditado);

   UPDATE STATISTICS FOR TABLE tmp_remanente_agr;

   -- Recupera saldo remanente de acreditados
   INSERT INTO tmp_saldo_remanente
   SELECT id_derechohabiente,
          subcuenta,
          ROUND(SUM(monto_acciones) * v_precio_fondo,2),
          SUM(monto_acciones)
     FROM cta_movimiento
    WHERE id_derechohabiente IN (SELECT id_derechohabiente FROM tmp_remanente_agr)
      AND subcuenta IN (4,8)
      AND fondo_inversion = 11
      GROUP BY 1,2;

   CREATE INDEX xpk_id_dh_sdo_rem ON tmp_saldo_remanente(id_derechohabiente);
   UPDATE STATISTICS FOR TABLE tmp_saldo_remanente;

   -- En caso de recibir un rango de montos, se crea cadena para omitir aquellos registros que no cumplen
   IF(p_monto_inicio IS NOT NULL) OR (p_monto_fin IS NOT NULL) THEN

      LET v_bnd_monto = 1;

      IF(p_monto_inicio IS NOT NULL) AND (p_monto_fin IS NULL) THEN
         LET v_sqry_monto = " HAVING SUM(pesos) < "||p_monto_inicio;
      ELIF(p_monto_inicio IS NULL) AND (p_monto_fin IS NOT NULL) THEN
         LET v_sqry_monto = " HAVING SUM(pesos) > "||p_monto_fin;
      ELIF (p_monto_inicio IS NOT NULL) AND (p_monto_fin IS NOT NULL) THEN
         LET v_sqry_monto = " HAVING SUM(pesos) NOT BETWEEN "||p_monto_inicio||" AND "||p_monto_fin;
      END IF

      LET v_qry_no_prospecto = " SELECT id_derechohabiente, SUM(pesos) AS pesos FROM tmp_saldo_remanente GROUP BY 1"||v_sqry_monto||
                               " INTO TEMP tmp_no_prospecto";

      --TRACE 'v_qry_no_prospecto '|| v_qry_no_prospecto;
      EXECUTE IMMEDIATE v_qry_no_prospecto;

      -- Elimina registros que no se consideran dentro del rango de montos
      DELETE FROM tmp_remanente_agr
         WHERE id_derechohabiente IN (SELECT id_derechohabiente FROM tmp_no_prospecto);

      DELETE FROM tmp_saldo_remanente
         WHERE id_derechohabiente IN (SELECT id_derechohabiente FROM tmp_no_prospecto);
   END IF

   -- Validaciones de negocio
   FOREACH
      SELECT nss               ,
             id_cre_acreditado ,
             id_derechohabiente,
             tpo_originacion   ,
             marca_prcr        ,
             tpo_credito       ,
             marca_ifv         ,
             num_credito       ,
             sdo_deudor        ,
             f_otorga          ,
             estado            ,
             edo_procesar
        INTO v_tmp_nss               ,
             v_tmp_id_cre_acreditado ,
             v_tmp_id_derechohabiente,
             v_tmp_tpo_originacion   ,
             v_tmp_marca_prcr        ,
             v_tmp_tpo_credito       ,
             v_tmp_marca_ifv         ,
             v_tmp_num_credito       ,
             v_tmp_sdo_deudor        ,
             v_tmp_f_otorga          ,
             v_tmp_estado            ,
             v_tmp_edo_procesar
        FROM tmp_remanente_agr

      -- Valida marca infonavit
      SELECT MAX(marca)
        INTO v_aux_marca_ifv
        FROM sfr_marca_activa
       WHERE id_derechohabiente = v_tmp_id_derechohabiente
         AND marca = v_tmp_marca_ifv;

      IF(v_aux_marca_ifv IS NULL) THEN
         LET v_diagnostico = 13;   --> No existe marca de crédito vigente
      ELSE
         --- Valida marca interna Procesar
         SELECT MAX(marca)
           INTO v_aux_marca_prc
           FROM sfr_marca_activa
          WHERE id_derechohabiente = v_tmp_id_derechohabiente
            AND marca = v_tmp_marca_prcr;

         IF(v_aux_marca_prc IS NULL) THEN
            LET v_diagnostico = 30;   --> Registro no marcado en Procesar
         ELSE
            -- Valida marca operativa que impida la petición
            FOREACH
               SELECT marca
                 INTO v_marca_impide
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = v_tmp_id_derechohabiente
                  AND marca NOT IN (v_aux_marca_ifv,v_aux_marca_prc)

               SELECT ind_convivencia,
                      rch_cod
                 INTO v_ind_convive,
                      v_rch_cod
                 FROM sfr_convivencia
                WHERE marca_activa = v_marca_impide
                  AND marca_entra  = v_aux_marca_ifv;

               IF(v_ind_convive = 20) THEN
                  EXIT FOREACH;
               END IF
            END FOREACH

            IF(v_ind_convive = 20) THEN
               LET v_diagnostico = 32;   --> Marca operativa en proceso
            ELSE
               -- Valida ssv 92.
               SELECT pesos,
                      acciones
                 INTO v_pesos_92,
                      v_aivs_92
                 FROM tmp_saldo_remanente
                WHERE id_derechohabiente = v_tmp_id_derechohabiente
                  AND subcuenta = 8;

               -- Valida ssv 97.
               SELECT pesos,
                      acciones
                 INTO v_pesos_97,
                      v_aivs_97
                 FROM tmp_saldo_remanente
                WHERE id_derechohabiente = v_tmp_id_derechohabiente
                  AND subcuenta = 4;

               IF(v_pesos_92 IS NULL) THEN
                  LET v_pesos_92 = 0;
               END IF
               IF(v_aivs_92 IS NULL) THEN
                  LET v_aivs_92 = 0;
               END IF
               IF(v_pesos_97 IS NULL) THEN
                  LET v_pesos_97 = 0;
               END IF
               IF(v_aivs_97 IS NULL) THEN
                  LET v_aivs_97 = 0;
               END IF

               -- Saldo total ssv
               LET v_total_saldo = v_pesos_92 + v_pesos_97;
               LET v_total_aivs  = v_aivs_92 + v_aivs_97;

               IF(v_bnd_monto = 1) THEN
                  IF(v_total_saldo <= 0) THEN
                     LET v_diagnostico   = 0;
                     LET v_aux_marca_ifv = NULL;
                     LET v_aux_marca_prc = NULL;
                     LET v_pesos_92      = 0;
                     LET v_aivs_92       = 0;
                     LET v_pesos_97      = 0;
                     LET v_aivs_97       = 0;
                     LET v_total_saldo   = 0;
                     LET v_total_aivs    = 0;
                     LET v_marca_impide  = NULL;
                     LET v_ind_convive   = 0;
                     LET v_rch_cod       = 0;
                     CONTINUE FOREACH;
                  END IF
               ELSE
                  IF(v_total_saldo = 0) THEN
                     LET v_diagnostico = 8;   --> Cuenta con saldo cero
                  ELSE
                     IF(v_total_saldo < 0) THEN
                        LET v_diagnostico = 34;   --> Cuenta con saldo negativo
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

      -- Registros Aceptados
      IF(v_diagnostico = 0) THEN
            INSERT INTO safre_tmp:tmp_sel_parametro_aceptado(
                               nss               ,
                               id_cre_acreditado ,
                               id_derechohabiente,
                               tpo_credito       ,
                               num_credito       ,
                               tpo_originacion   ,
                               f_otorga          ,
                               marca_operativa   ,
                               marca_procesar    ,
                               sdo92             ,
                               sdo97             ,
                               suma_saldo        ,
                               aivs92            ,
                               aivs97            ,
                               suma_aivs         ,
                               estado)
                        VALUES(v_tmp_nss               ,
                               v_tmp_id_cre_acreditado ,
                               v_tmp_id_derechohabiente,
                               v_tmp_tpo_credito       ,
                               v_tmp_num_credito       ,
                               v_tmp_tpo_originacion   ,
                               v_tmp_f_otorga          ,
                               v_aux_marca_ifv         ,
                               v_aux_marca_prc         ,
                               v_pesos_92              ,
                               v_pesos_97              ,
                               v_total_saldo           ,
                               v_aivs_92               ,
                               v_aivs_97               ,
                               v_total_aivs            ,
                               v_tmp_estado);
      ELSE
         -- Registros Rechazados
         INSERT INTO safre_tmp:tmp_rch_sel_parametro(
                            nss            ,
                            sdo92          ,
                            sdo97          ,
                            suma_saldo     ,
                            aivs92         ,
                            aivs97         ,
                            suma_aivs      ,
                            tpo_credito    ,
                            num_credito    ,
                            f_otorga       ,
                            marca_operativa,
                            marca_procesar ,
                            rch_cod_marca  ,
                            tpo_originacion,
                            causal_rch)
                     VALUES(v_tmp_nss            ,
                            v_pesos_92           ,
                            v_pesos_97           ,
                            v_total_saldo        ,
                            v_aivs_92            ,
                            v_aivs_97            ,
                            v_total_aivs         ,
                            v_tmp_tpo_credito    ,
                            v_tmp_num_credito    ,
                            v_tmp_f_otorga       ,
                            v_aux_marca_ifv      ,
                            v_aux_marca_prc      ,
                            v_rch_cod            ,
                            v_tmp_tpo_originacion,
                            v_diagnostico);
      END IF

      -- Resetea variables
      LET v_diagnostico   = 0;
      LET v_aux_marca_ifv = NULL;
      LET v_aux_marca_prc = NULL;
      LET v_pesos_92      = 0;
      LET v_aivs_92       = 0;
      LET v_pesos_97      = 0;
      LET v_aivs_97       = 0;
      LET v_total_saldo   = 0;
      LET v_total_aivs    = 0;
      LET v_marca_impide  = NULL;
      LET v_ind_convive   = 0;
      LET v_rch_cod       = 0;

   END FOREACH

   RETURN v_error, v_isam_err, v_c_msj;

END FUNCTION
;


