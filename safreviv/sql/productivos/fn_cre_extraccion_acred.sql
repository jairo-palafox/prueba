






CREATE FUNCTION "safreviv".fn_cre_extraccion_acred()

   RETURNING SMALLINT, INTEGER, VARCHAR(250);

   -- Registro del acreditado
   DEFINE cre_nss                CHAR(11);
   DEFINE cre_rfc                CHAR(13);
   DEFINE cre_curp               CHAR(18);
   DEFINE cre_paterno            CHAR(40);
   DEFINE cre_materno            CHAR(40);
   DEFINE cre_nombre             CHAR(40);
   DEFINE cre_id_derechohabiente DECIMAL(9,0);
   DEFINE cre_id_cre_acreditado  DECIMAL(9,0);
   DEFINE cre_tpo_originacion    SMALLINT;
   DEFINE cre_tpo_credito        SMALLINT;
   DEFINE cre_num_credito        DECIMAL(10,0);
   DEFINE cre_f_otorga           DATE;
   DEFINE cre_edo_procesar       SMALLINT;
   DEFINE cre_f_actualiza        DATE;
   DEFINE edo_estado             SMALLINT;

   -- Registro del saldo diario
   DEFINE sdo_subcuenta          SMALLINT;
   DEFINE sdo_monto_pesos        DECIMAL(13,2);

   -- Registro de la tabla temporal
   DEFINE tmp_nss                CHAR(11);
   DEFINE tmp_rfc                CHAR(13);
   DEFINE tmp_curp               CHAR(18);
   DEFINE tmp_paterno            CHAR(40);
   DEFINE tmp_materno            CHAR(40);
   DEFINE tmp_nombre             CHAR(40);

   DEFINE tmp_tpo_credito        SMALLINT;
   DEFINE tmp_num_credito        DECIMAL(10,0);
   DEFINE tmp_f_ini_credito      DATE;
   DEFINE tmp_f_liq_credito      DATE;
   DEFINE tmp_marca_safre_saci   CHAR(1);
   DEFINE tmp_marca_procesar     CHAR(1);
   DEFINE tmp_saldo_ssv97        DECIMAL(18,2);
   DEFINE tmp_saldo_ssv92        DECIMAL(18,2);
   DEFINE tmp_estado_cred        SMALLINT;

   -- variables auxiliares
   DEFINE v_c_marca_proc_aux     CHAR(1);
   DEFINE v_d_saldo_ssv92        DECIMAL(13,2);
   DEFINE v_d_saldo_ssv97        DECIMAL(13,2);

   DEFINE v_tabla                VARCHAR(25);
   DEFINE v_cod_error            SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err             INTEGER;
   DEFINE v_c_msj                VARCHAR(250);
   DEFINE v_precio               DECIMAL(15,6);

   ON EXCEPTION SET v_cod_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_cod_error, v_isam_err, v_c_msj;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/extracAcred.trace';
   ---TRACE ON;

   -- se establece la prioridad
   SET PDQPRIORITY HIGH;

   -- se inicializa el contador de registros
   LET v_cod_error = 0;
   LET v_isam_err  = 0;
   LET v_c_msj     = 'El proceso finalizó correctamente';

   DROP TABLE IF EXISTS tmp_id_dh;
   DROP TABLE IF EXISTS tmp_saldo_acred;

   SELECT UNIQUE id_derechohabiente
     FROM safre_tmp:tmp_acred
     INTO TEMP tmp_id_dh;

   CREATE INDEX tmp_id_dh1 ON tmp_id_dh(id_derechohabiente);

   UPDATE STATISTICS FOR TABLE tmp_id_dh;

   -- se busca cual es la tabla activa de saldos
   SELECT tabla_saldo
     INTO v_tabla
     FROM safre_sdo@vivws_tcp:glo_saldo
    WHERE ind_saldo = 1;

   IF (v_tabla = 'cta_saldo_diario') THEN
        SELECT sdo.id_derechohabiente, cat.id_subcuenta, sum(sdo.monto_pesos) monto_pesos
          FROM tmp_id_dh t,
               safre_sdo@vivws_tcp:cta_saldo_diario sdo,
               cat_subcuenta_preca cat
         WHERE t.id_derechohabiente = sdo.id_derechohabiente
           AND sdo.subcuenta IN (4,8,42,44,55)
           AND sdo.fondo_inversion = 11
           AND cat.subcuenta = sdo.subcuenta
         GROUP BY 1,2
         INTO temp tmp_saldo_acred;
   ELSE
        SELECT sdo.id_derechohabiente,cat.id_subcuenta,sum(sdo.monto_pesos) monto_pesos
          FROM tmp_id_dh t,
               safre_sdo@vivws_tcp:cta_saldo_diario_bis sdo,
               cat_subcuenta_preca cat
         WHERE t.id_derechohabiente = sdo.id_derechohabiente
           AND sdo.subcuenta IN (4,8,42,44,55)
           AND sdo.fondo_inversion = 11
           AND cat.subcuenta = sdo.subcuenta
         GROUP BY 1,2
         INTO temp tmp_saldo_acred;
   END IF

   CREATE INDEX tmp_saldo_acred1 ON tmp_saldo_acred(id_derechohabiente);

   UPDATE STATISTICS FOR TABLE tmp_saldo_acred;

   -- se regresa la prioridad
   SET PDQPRIORITY DEFAULT;

   LOCK TABLE safre_tmp:tmp_extrac_acred IN EXCLUSIVE MODE;

   -- se lee cada uno de los derechohabientes vigentes
   FOREACH
      SELECT t.nss,
             NVL(t.rfc," "),
             NVL(t.curp," "),
             t.paterno,
             NVL(t.materno," "),
             t.nombre,
             t.id_derechohabiente,
             t.tpo_originacion,
             t.tpo_credito,
             t.num_credito,
             t.f_otorga,
             t.edo_procesar,
             t.entidad,
             t.id_cre_acreditado
        INTO cre_nss,
             cre_rfc,
             cre_curp,
             cre_paterno,
             cre_materno,
             cre_nombre,
             cre_id_derechohabiente,
             cre_tpo_originacion,
             cre_tpo_credito,
             cre_num_credito,
             cre_f_otorga,
             cre_edo_procesar,
             edo_estado,
             cre_id_cre_acreditado
        FROM safre_tmp:tmp_acred t

      -- se incializan los montos
      LET v_d_saldo_ssv92 = 0;
      LET v_d_saldo_ssv97 = 0;

      SELECT sdo.monto_pesos
        INTO v_d_saldo_ssv97
        FROM tmp_saldo_acred sdo
       WHERE sdo.id_derechohabiente = cre_id_derechohabiente
         AND sdo.id_subcuenta = 4;

      SELECT sdo.monto_pesos
        INTO v_d_saldo_ssv92
        FROM tmp_saldo_acred sdo
       WHERE sdo.id_derechohabiente = cre_id_derechohabiente
         AND sdo.id_subcuenta = 8;

      IF v_d_saldo_ssv97 IS NULL OR v_d_saldo_ssv97 < 0 THEN
         LET v_d_saldo_ssv97 = 0;
      END IF

      IF v_d_saldo_ssv92 IS NULL OR v_d_saldo_ssv92 < 0 THEN
         LET v_d_saldo_ssv92 = 0;
      END IF

      -- dependiendo del estado Procesar se asigna la marca Procesar
      IF cre_edo_procesar >= 55 AND cre_edo_procesar <= 200 THEN
         LET v_c_marca_proc_aux = cre_tpo_originacion;
      ELSE
         LET v_c_marca_proc_aux = 0;
      END IF

      -- se asigna el registro a insertar
      LET tmp_nss              = cre_nss;
      LET tmp_rfc              = cre_rfc;
      LET tmp_curp             = cre_curp;
      LET tmp_paterno          = cre_paterno;
      LET tmp_materno          = cre_materno;
      LET tmp_nombre           = cre_nombre;
      LET tmp_tpo_credito      = cre_tpo_credito;
      LET tmp_num_credito      = cre_num_credito;
      LET tmp_f_ini_credito    = cre_f_otorga;
      LET tmp_marca_procesar   = v_c_marca_proc_aux;
      LET tmp_saldo_ssv97      = v_d_saldo_ssv97; -- * v_precio;
      LET tmp_saldo_ssv92      = v_d_saldo_ssv92; -- * v_precio;
      LET tmp_estado_cred      = edo_estado;      -- 1 Créditos vigentes, 2 liquidados, 5 cancelados

      IF edo_estado = 1 THEN
         LET tmp_f_liq_credito    = NULL;
         LET tmp_marca_safre_saci = cre_tpo_originacion;
      ELSE
         SELECT FIRST 1 cta.f_actualiza
           INTO cre_f_actualiza
           FROM cta_his_credito cta
          WHERE cta.id_derechohabiente = cre_id_derechohabiente
            AND cta.num_credito = cre_num_credito;

         IF cre_f_actualiza IS NULL OR cre_f_actualiza = "" THEN
            SELECT MAX(f_fin)
              INTO cre_f_actualiza
              FROM sfr_marca_historica
             WHERE id_derechohabiente = cre_id_derechohabiente
               AND marca IN(SELECT marca_inf
                              FROM cat_tipo_credito
                             WHERE marca_inf IS NOT NULL)
               AND f_fin IS NOT NULL
               AND f_fin > cre_f_otorga;
         END IF

         IF cre_f_actualiza IS NULL OR cre_f_actualiza = "" THEN
            IF cre_tpo_originacion = 2 THEN
               SELECT MAX(f_fin)
                 INTO cre_f_actualiza
                 FROM sfr_marca_historica
                WHERE id_derechohabiente = cre_id_derechohabiente
                  AND marca = 232
                  AND f_fin IS NOT NULL
                  AND f_fin > cre_f_otorga;
            ELSE
               SELECT MAX(f_fin)
                 INTO cre_f_actualiza
                 FROM sfr_marca_historica
                WHERE id_derechohabiente = cre_id_derechohabiente
                  AND marca IN(231,234)
                  AND f_fin IS NOT NULL
                  AND f_fin > cre_f_otorga;
            END IF
         END IF 

         IF cre_f_actualiza IS NULL OR cre_f_actualiza = "" THEN
            LET cre_f_actualiza = TODAY;
         END IF

         LET tmp_f_liq_credito = cre_f_actualiza;

         IF edo_estado = 2 THEN
            LET tmp_marca_safre_saci = 0; --Crédito liquidado
         ELSE
            LET tmp_marca_safre_saci = 9; --Crédito cancelado
         END IF
      END IF

      IF tmp_marca_safre_saci IS NULL OR
         tmp_marca_safre_saci = "" THEN
         LET tmp_marca_safre_saci = cre_tpo_originacion;
      END IF

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_extrac_acred (
                  nss,
                  rfc,
                  curp,
                  paterno,
                  materno,
                  nombre,
                  tpo_credito,
                  num_credito,
                  f_ini_credito,
                  f_liq_credito,
                  marca_safre_saci,
                  marca_procesar,
                  saldo_ssv97,
                  saldo_ssv92,
                  estado_cred,
                  id_cre_acreditado)
          VALUES (tmp_nss,
                  tmp_rfc,
                  tmp_curp,
                  tmp_paterno,
                  tmp_materno,
                  tmp_nombre,
                  tmp_tpo_credito,
                  tmp_num_credito,
                  tmp_f_ini_credito,
                  tmp_f_liq_credito,
                  tmp_marca_safre_saci,
                  tmp_marca_procesar,
                  tmp_saldo_ssv97,
                  tmp_saldo_ssv92,
                  tmp_estado_cred,
                  cre_id_cre_acreditado);
   END FOREACH;

   UNLOCK TABLE safre_tmp:tmp_extrac_acred;

   RETURN v_cod_error, v_isam_err, v_c_msj;

END FUNCTION;


