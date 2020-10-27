






CREATE FUNCTION "safreviv".fn_dse_agrupa_devolucion(p_folio_agrupa DECIMAL(9,0),
                                         p_usuario_cod  CHAR(20),
                                         p_tpo_transf   CHAR(2))
   RETURNING SMALLINT

   -- registro tabla origen para DSE devolucion
   DEFINE dse_id_dse_devolucion        DECIMAL(9,0);
   DEFINE dse_id_derechohabiente       DECIMAL(9,0) ;
   DEFINE dse_num_credito              DECIMAL(10,0);
   DEFINE dse_tpo_transferencia        CHAR(2);
   DEFINE dse_origen_devolucion        CHAR(2);
   DEFINE dse_subcuenta92              SMALLINT;
   DEFINE dse_subcuenta97              SMALLINT;
   DEFINE dse_monto_aivs92             DECIMAL(16,6);
   DEFINE dse_monto_pesos92            DECIMAL(12,2);
   DEFINE dse_monto_aivs97             DECIMAL(16,6);
   DEFINE dse_monto_pesos97            DECIMAL(12,2);
   DEFINE dse_monto_aportacion         DECIMAL(12,2);
   DEFINE dse_aivs_aportacion          DECIMAL(16,6);
   DEFINE dse_nss_separacion           CHAR(11);
   DEFINE dse_estado                   SMALLINT;

   -- REGISTRO tabla (destino) de DSE agrupado
   DEFINE dsea_id_dse_grp_devolucion   DECIMAL(9,0);
   DEFINE dsea_id_derechohabiente      DECIMAL(9,0);
   DEFINE dsea_num_credito             DECIMAL(10,0);
   DEFINE dsea_tpo_transferencia       CHAR(2);
   DEFINE dsea_origen_devolucion       CHAR(2);
   DEFINE dsea_f_movimiento            DATE;
   DEFINE dsea_folio_liquida           DECIMAL(9,0);
   DEFINE dsea_aivs97                  DECIMAL(16,6);
   DEFINE dsea_pesos97                 DECIMAL(12,2);
   DEFINE dsea_aivs92                  DECIMAL(16,6);
   DEFINE dsea_pesos92                 DECIMAL(12,2);
   DEFINE dsea_monto_aportacion        DECIMAL(12,2);
   DEFINE dsea_aivs_aportacion         DECIMAL(16,6);
   DEFINE dsea_nss_separacion          CHAR(11);
   DEFINE dsea_edo_procesar            SMALLINT;
   DEFINE dsea_estado                  SMALLINT;

   -- REGISTRO de la tabla his_devolucion
   DEFINE dseh_id_dse_grp_devolucion   DECIMAL(9,0);
   DEFINE dseh_folio                   DECIMAL(9,0);
   DEFINE dseh_tpo_transferencia       CHAR(2);
   DEFINE dseh_lote                    SMALLINT;
   DEFINE dseh_id_lote                 DECIMAL(9,0);
   DEFINE dseh_f_presentacion          DATE;
   DEFINE dseh_paterno_afore           CHAR(40);
   DEFINE dseh_materno_afore           CHAR(40);
   DEFINE dseh_nombre_afore            CHAR(40);
   DEFINE dseh_nom_imss                CHAR(50);
   DEFINE dseh_aivs97                  DECIMAL(16,6);
   DEFINE dseh_pesos97                 DECIMAL(12,2);
   DEFINE dseh_aivs92                  DECIMAL(16,6);
   DEFINE dseh_pesos92                 DECIMAL(12,2);
   DEFINE dseh_monto_aportacion        DECIMAL(12,2);
   DEFINE dseh_aivs_aportacion         DECIMAL(16,6);
   DEFINE dseh_nss_separacion          CHAR(11);
   DEFINE dseh_edo_procesar            SMALLINT;
   DEFINE dseh_diagnostico             CHAR(3);
   DEFINE dseh_estado                  SMALLINT;
   DEFINE dseh_f_proceso               DATE;

   -- Campos auxiliares
   DEFINE v_ax_edo_procesar            SMALLINT; -- estado procesar
   DEFINE v_ax_c_tipo_trabajador       CHAR(1); -- tipo de trabajador
   DEFINE v_ax_nombre_imss             CHAR(50); -- nombre IMMS
   DEFINE v_precio_fondo               DECIMAL(19,14); -- valor del fondo en determinada fecha
   DEFINE v_resultado                  SMALLINT; -- resultado del proceso-> 0: correcto, <> 0: error
   DEFINE v_id_lote                    DECIMAL(9,0); -- identificador del lote
   DEFINE v_lote                       SMALLINT; -- lote
   DEFINE v_subcta                     SMALLINT; -- subcuenta foreach gral
   DEFINE v_monto_pesos                DECIMAL(12,2); -- monto pesos foreach gral
   DEFINE v_monto_aivs                 DECIMAL(16,6); -- monto acciones foreach gral
   DEFINE v_monto_aportacion           DECIMAL(12,2); -- monto aportación foreach gral
   DEFINE v_aivs_aportacion            DECIMAL(16,6); -- aivs aportación foreach gral
   DEFINE v_id_dse_devolucion          DECIMAL(9,0);  -- id_dse_Dev para separación

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado;
   END EXCEPTION

   ---SET DEBUG FILE TO 'dseAgrupaDSE.log';
   ---TRACE ON;

   SET PDQPRIORITY HIGH;

   -- se inicializan variables
   LET v_resultado = 0; -- se asume que no ocurrirá ningun error
   LET v_id_lote   = 0; -- id lote

   LET dse_subcuenta92      = 8;
   LET dse_subcuenta97      = 4;
   LET dse_monto_aivs92     = 0;
   LET dse_monto_pesos92    = 0;
   LET dse_monto_aivs97     = 0;
   LET dse_monto_pesos97    = 0;
   LET dse_monto_aportacion = 0;
   LET dse_aivs_aportacion  = 0;

   LET dse_tpo_transferencia = p_tpo_transf;
   LET dse_num_credito       = "";
   LET dse_nss_separacion    = "";

   -- se obtiene el valor del fondo de la fecha del dia
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
      AND fondo = 11;

   -- si no existe el valor del fondo para la fecha dada
   IF ( v_precio_fondo IS NULL ) THEN
      -- generar un error
      LET v_resultado = -1; -- no hay valor de fondo para la fecha

      RETURN v_resultado;
   END IF

   -- se obtiene el lote para el día de hoy
   SELECT MAX(lote)
     INTO v_lote
     FROM dse_his_devolucion
    WHERE f_proceso = TODAY
      AND tpo_transferencia = p_tpo_transf;

   -- se valida el lote
   IF v_lote IS NULL THEN
      -- se asume que es el primer lote del día
      LET v_lote = 0;
   ELSE
      -- se incrementa el lote del día
      LET v_lote = v_lote + 1;
   END IF

   UPDATE STATISTICS FOR TABLE dse_devolucion;

   -- verifica NSS de Separación de Cuentas con nss_separacion diferentes
   UPDATE dse_devolucion
      SET estado = 10
    WHERE estado = 0
      AND tpo_transferencia = p_tpo_transf
      AND modulo_cod = "sep";

   SELECT UNIQUE d.id_derechohabiente, d.nss_separacion
     FROM dse_devolucion d
    WHERE d.estado = 10
      AND d.tpo_transferencia = p_tpo_transf
      AND d.modulo_cod = "sep"
   INTO TEMP reg_nss_sep;

   SELECT s.id_derechohabiente, count(*) tot
     FROM reg_nss_sep s
   GROUP BY 1
   HAVING COUNT(*) > 1
   INTO TEMP reg_sep_dup;

   FOREACH
       SELECT MAX(d.id_dse_devolucion)
         INTO v_id_dse_devolucion
         FROM dse_devolucion d, reg_sep_dup t
        WHERE d.estado = 10
          AND d.tpo_transferencia = p_tpo_transf
          AND d.modulo_cod = "sep"
          AND d.id_derechohabiente = t.id_derechohabiente

       UPDATE dse_devolucion
          SET estado = 0
        WHERE id_dse_devolucion = v_id_dse_devolucion
          AND estado = 10;
   END FOREACH

   -- se consulta la información de devolucion agrupadamente para las subcuentas (4,8)
   FOREACH
      -- se consulta el tipo de trabajador
      SELECT UNIQUE d.id_derechohabiente, a.tipo_trabajador, a.nombre_imss
        INTO dse_id_derechohabiente, v_ax_c_tipo_trabajador, v_ax_nombre_imss
        FROM dse_devolucion d, afi_derechohabiente a
       WHERE d.estado = 10
         AND d.tpo_transferencia = p_tpo_transf
         AND d.id_derechohabiente = a.id_derechohabiente
         AND d.subcuenta IN(4,8)

      FOREACH
         SELECT ds.subcuenta,
                sum(ds.monto_pesos),
                sum(ds.monto_aivs),
                sum(ds.monto_aportacion),
                sum(ds.aivs_aportacion)
           INTO v_subcta,
                v_monto_pesos,
                v_monto_aivs,
                v_monto_aportacion,
                v_aivs_aportacion
           FROM dse_devolucion ds
          WHERE ds.estado = 10
            AND ds.tpo_transferencia = p_tpo_transf
            AND ds.id_derechohabiente = dse_id_derechohabiente
            AND ds.subcuenta IN(4,8)
         GROUP BY 1

         IF v_subcta = 4 THEN
            IF v_monto_pesos IS NULL OR v_monto_pesos < 0 THEN
               LET v_monto_pesos = 0;
            END IF

            IF v_monto_aivs IS NULL OR v_monto_aivs < 0 THEN
               LET v_monto_aivs = 0;
            END IF

            LET dse_monto_aivs97  = dse_monto_aivs97 + v_monto_aivs;
            LET dse_monto_pesos97 = dse_monto_pesos97 + v_monto_pesos;
         END IF

         IF v_subcta = 8 THEN
            IF v_monto_pesos IS NULL OR v_monto_pesos < 0 THEN
               LET v_monto_pesos = 0;
            END IF

            IF v_monto_aivs IS NULL OR v_monto_aivs < 0 THEN
               LET v_monto_aivs = 0;
            END IF

            LET dse_monto_aivs92  = dse_monto_aivs92 + v_monto_aivs;
            LET dse_monto_pesos92 = dse_monto_pesos92 + v_monto_pesos;
         END IF

         IF v_monto_aportacion IS NULL OR v_monto_aportacion < 0 THEN
             LET v_monto_aportacion = 0;
         END IF

         IF v_aivs_aportacion IS NULL OR v_aivs_aportacion < 0 THEN
             LET v_aivs_aportacion = 0;
         END IF

         LET dse_monto_aportacion = dse_monto_aportacion + v_monto_aportacion;
         LET dse_aivs_aportacion  = dse_aivs_aportacion + v_aivs_aportacion;
      END FOREACH

      FOREACH
         SELECT FIRST 1 dn.num_credito
           INTO dse_num_credito
           FROM dse_devolucion dn
          WHERE dn.estado = 10
            AND dn.tpo_transferencia = p_tpo_transf
            AND dn.id_derechohabiente = dse_id_derechohabiente
            AND dn.num_credito <> 0
            AND dn.num_credito IS NOT NULL
            AND dn.subcuenta IN(4,8)
         ORDER BY  modulo_cod
      END FOREACH

      FOREACH
         SELECT FIRST 1 dn.nss_separacion
           INTO dse_nss_separacion
           FROM dse_devolucion dn
          WHERE dn.estado = 10
            AND dn.tpo_transferencia = p_tpo_transf
            AND dn.id_derechohabiente = dse_id_derechohabiente
            AND dn.subcuenta IN(4,8)
         ORDER BY  modulo_cod DESC
      END FOREACH

      -- se incrementa el identificador del lote
      LET v_id_lote = v_id_lote +1;

      -- se valida el tipo de trabajador
      IF v_ax_c_tipo_trabajador = "I" THEN
         LET v_ax_edo_procesar = 20;
      ELSE
         LET v_ax_edo_procesar = 7;
      END IF

      -- se obtiene el origen devolución
      IF dse_monto_pesos92 IS NOT NULL AND dse_monto_pesos92 <> 0 AND
         dse_monto_pesos97 IS NOT NULL AND dse_monto_pesos97 <> 0 THEN
         LET dse_origen_devolucion = "05";
      ELIF dse_monto_pesos92 IS NOT NULL AND dse_monto_pesos92 <> 0 THEN
         LET dse_origen_devolucion = "01";
      ELSE
         LET dse_origen_devolucion = "02";
      END IF

      -- se calcula el valor de las AIVs
      LET dse_monto_aivs92    = dse_monto_pesos92 / v_precio_fondo;
      LET dse_monto_aivs97    = dse_monto_pesos97 / v_precio_fondo;
      LET dse_aivs_aportacion = dse_monto_aportacion / v_precio_fondo;

      -- se asignan los datos comunes a los posibles registros que se crearán (1 por viv97 y 1 por viv92)
      LET dsea_id_dse_grp_devolucion = seq_dse_grp_devolucion.NEXTVAL;
      LET dsea_id_derechohabiente    = dse_id_derechohabiente;
      LET dsea_num_credito           = dse_num_credito;
      LET dsea_tpo_transferencia     = dse_tpo_transferencia;
      LET dsea_origen_devolucion     = dse_origen_devolucion;
      LET dsea_f_movimiento          = TODAY - DAY(TODAY) + 1; --dse_f_movimiento;
      LET dsea_folio_liquida         = 0;
      LET dsea_aivs97                = dse_monto_aivs97;
      LET dsea_pesos97               = dse_monto_pesos97;
      LET dsea_aivs92                = dse_monto_aivs92;
      LET dsea_pesos92               = dse_monto_pesos92;
      LET dsea_monto_aportacion      = dse_monto_aportacion;
      LET dsea_aivs_aportacion       = dse_aivs_aportacion;
      LET dsea_nss_separacion        = dse_nss_separacion;
      LET dsea_edo_procesar          = v_ax_edo_procesar;
      LET dsea_estado                = 20;

      -- se inserta el registro en la tabla de agrupacion de DSE
      INSERT INTO dse_agrupa_devolucion(
                  id_dse_grp_devolucion,
                  id_derechohabiente,
                  num_credito,
                  tpo_transferencia,
                  origen_devolucion,
                  f_movimiento,
                  folio_liquida,
                  aivs97,
                  pesos97,
                  aivs92,
                  pesos92,
                  monto_aportacion,
                  aivs_aportacion,
                  nss_separacion,
                  edo_procesar,
                  estado)
          VALUES (dsea_id_dse_grp_devolucion,
                  dsea_id_derechohabiente,
                  dsea_num_credito,
                  dsea_tpo_transferencia,
                  dsea_origen_devolucion,
                  dsea_f_movimiento,
                  dsea_folio_liquida,
                  dsea_aivs97,
                  dsea_pesos97,
                  dsea_aivs92,
                  dsea_pesos92,
                  dsea_monto_aportacion,
                  dsea_aivs_aportacion,
                  dsea_nss_separacion,
                  dsea_edo_procesar,
                  dsea_estado);

      -- ============================================================
      -- se crea un registro para his_devolucion
      LET dseh_id_dse_grp_devolucion = dsea_id_dse_grp_devolucion;
      LET dseh_folio                 = p_folio_agrupa;
      LET dseh_tpo_transferencia     = p_tpo_transf;
      LET dseh_lote                  = v_lote;
      LET dseh_id_lote               = v_id_lote;
      LET dseh_f_presentacion        = TODAY;
      LET dseh_paterno_afore         = NULL;
      LET dseh_materno_afore         = NULL;
      LET dseh_nombre_afore          = NULL;
      LET dseh_nom_imss              = v_ax_nombre_imss;
      LET dseh_aivs97                = dsea_aivs97;
      LET dseh_pesos97               = dsea_pesos97;
      LET dseh_aivs92                = dsea_aivs92;
      LET dseh_pesos92               = dsea_pesos92;
      LET dseh_monto_aportacion      = dsea_monto_aportacion;
      LET dseh_aivs_aportacion       = dsea_aivs_aportacion;
      LET dseh_nss_separacion        = dsea_nss_separacion;
      LET dseh_edo_procesar          = dsea_edo_procesar;
      LET dseh_diagnostico           = 0;
      LET dseh_estado                = dsea_estado;
      LET dseh_f_proceso             = TODAY;

      -- se inserta el registro en la tabla de his devolucion
      INSERT INTO dse_his_devolucion (
                  id_dse_grp_devolucion,
                  folio,
                  tpo_transferencia,
                  lote,
                  id_lote,
                  f_presentacion,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  aivs97,
                  pesos97,
                  aivs92,
                  pesos92,
                  monto_aportacion,
                  aivs_aportacion,
                  nss_separacion,
                  edo_procesar,
                  diagnostico,
                  estado,
                  f_proceso)
          VALUES (dseh_id_dse_grp_devolucion,
                  dseh_folio,
                  dseh_tpo_transferencia,
                  dseh_lote,
                  dseh_id_lote,
                  dseh_f_presentacion,
                  dseh_paterno_afore,
                  dseh_materno_afore,
                  dseh_nombre_afore,
                  dseh_nom_imss,
                  dseh_aivs97,
                  dseh_pesos97,
                  dseh_aivs92,
                  dseh_pesos92,
                  dseh_monto_aportacion,
                  dseh_aivs_aportacion,
                  dseh_nss_separacion,
                  dseh_edo_procesar,
                  dseh_diagnostico,
                  dseh_estado,
                  dseh_f_proceso);

      -- se actualiza el estado del registro en dse_devolucion a 80
      UPDATE dse_devolucion
         SET estado = 15,
             folio = p_folio_agrupa
       WHERE id_derechohabiente = dse_id_derechohabiente
         ---AND num_credito = dse_num_credito
         AND estado = 10
         AND tpo_transferencia = p_tpo_transf
         AND subcuenta IN (4,8);

      LET dse_monto_aivs92     = 0;
      LET dse_monto_pesos92    = 0;
      LET dse_monto_aivs97     = 0;
      LET dse_monto_pesos97    = 0;
      LET dse_monto_aportacion = 0;
      LET dse_aivs_aportacion  = 0;

      LET dse_num_credito      = "";
      LET dse_nss_separacion   = "";
   END FOREACH;

   -- se consulta la información de devolución agrupadamente para las subcuentas (42,44)
   FOREACH
      -- se consulta el tipo de trabajador
      SELECT UNIQUE d.id_derechohabiente, a.tipo_trabajador, a.nombre_imss
        INTO dse_id_derechohabiente, v_ax_c_tipo_trabajador, v_ax_nombre_imss
        FROM dse_devolucion d, afi_derechohabiente a
       WHERE d.estado = 10
         AND d.tpo_transferencia = p_tpo_transf
         AND d.id_derechohabiente = a.id_derechohabiente
         AND d.subcuenta IN(42,44)

      FOREACH
         SELECT ds.subcuenta,
                sum(ds.monto_pesos),
                sum(ds.monto_aivs),
                sum(ds.monto_aportacion),
                sum(ds.aivs_aportacion)
           INTO v_subcta,
                v_monto_pesos,
                v_monto_aivs,
                v_monto_aportacion,
                v_aivs_aportacion
           FROM dse_devolucion ds
          WHERE ds.estado = 10
            AND ds.tpo_transferencia = p_tpo_transf
            AND ds.id_derechohabiente = dse_id_derechohabiente
            AND ds.subcuenta IN(42,44)
         GROUP BY 1

         IF v_subcta = 44 THEN
            IF v_monto_pesos IS NULL OR v_monto_pesos < 0 THEN
               LET v_monto_pesos = 0;
            END IF

            IF v_monto_aivs IS NULL OR v_monto_aivs < 0 THEN
               LET v_monto_aivs = 0;
            END IF

            LET dse_monto_aivs97  = dse_monto_aivs97 + v_monto_aivs;
            LET dse_monto_pesos97 = dse_monto_pesos97 + v_monto_pesos;
         END IF

         IF v_subcta = 42 THEN
            IF v_monto_pesos IS NULL OR v_monto_pesos < 0 THEN
               LET v_monto_pesos = 0;
            END IF

            IF v_monto_aivs IS NULL OR v_monto_aivs < 0 THEN
               LET v_monto_aivs = 0;
            END IF

            LET dse_monto_aivs92  = dse_monto_aivs92 + v_monto_aivs;
            LET dse_monto_pesos92 = dse_monto_pesos92 + v_monto_pesos;
         END IF

         IF v_monto_aportacion IS NULL OR v_monto_aportacion < 0 THEN
             LET v_monto_aportacion = 0;
         END IF

         IF v_aivs_aportacion IS NULL OR v_aivs_aportacion < 0 THEN
             LET v_aivs_aportacion = 0;
         END IF

         LET dse_monto_aportacion = dse_monto_aportacion + v_monto_aportacion;
         LET dse_aivs_aportacion  = dse_aivs_aportacion + v_aivs_aportacion;
      END FOREACH

      FOREACH
         SELECT FIRST 1 dn.num_credito
           INTO dse_num_credito
           FROM dse_devolucion dn
          WHERE dn.estado = 10
            AND dn.tpo_transferencia = p_tpo_transf
            AND dn.id_derechohabiente = dse_id_derechohabiente
            AND dn.num_credito <> 0
            AND dn.num_credito IS NOT NULL
            AND dn.subcuenta IN(42,44)
         ORDER BY  modulo_cod
      END FOREACH

      FOREACH
         SELECT FIRST 1 dn.nss_separacion
           INTO dse_nss_separacion
           FROM dse_devolucion dn
          WHERE dn.estado = 10
            AND dn.tpo_transferencia = p_tpo_transf
            AND dn.id_derechohabiente = dse_id_derechohabiente
            AND dn.subcuenta IN(42,44)
         ORDER BY  modulo_cod DESC
      END FOREACH

      -- se incrementa el identificador del lote
      LET v_id_lote = v_id_lote +1;

      -- se valida el tipo de trabajador
      IF v_ax_c_tipo_trabajador = "I" THEN
         LET v_ax_edo_procesar = 20;
      ELSE
         LET v_ax_edo_procesar = 7;
      END IF

      -- se obtiene el origen devolución
      IF dse_monto_pesos92 IS NOT NULL AND dse_monto_pesos92 <> 0 AND
         dse_monto_pesos97 IS NOT NULL AND dse_monto_pesos97 <> 0 THEN
         LET dse_origen_devolucion = "05";
      ELIF dse_monto_pesos92 IS NOT NULL AND dse_monto_pesos92 <> 0 THEN
         LET dse_origen_devolucion = "01";
      ELSE
         LET dse_origen_devolucion = "02";
      END IF

      -- se calcula el valor de las AIVs
      LET dse_monto_aivs92    = dse_monto_pesos92 / v_precio_fondo;
      LET dse_monto_aivs97    = dse_monto_pesos97 / v_precio_fondo;
      LET dse_aivs_aportacion = dse_monto_aportacion / v_precio_fondo;

      -- se asignan los datos comunes a los posibles registros que se crearan (1 por viv97 y 1 por viv92)
      LET dsea_id_dse_grp_devolucion = seq_dse_grp_devolucion.NEXTVAL;
      LET dsea_id_derechohabiente    = dse_id_derechohabiente;
      LET dsea_num_credito           = dse_num_credito;
      LET dsea_tpo_transferencia     = dse_tpo_transferencia;
      LET dsea_origen_devolucion     = dse_origen_devolucion;
      LET dsea_f_movimiento          = TODAY - DAY(TODAY) + 1; --dse_f_movimiento;
      LET dsea_folio_liquida         = 0;
      LET dsea_aivs97                = dse_monto_aivs97;
      LET dsea_pesos97               = dse_monto_pesos97;
      LET dsea_aivs92                = dse_monto_aivs92;
      LET dsea_pesos92               = dse_monto_pesos92;
      LET dsea_monto_aportacion      = dse_monto_aportacion;
      LET dsea_aivs_aportacion       = dse_aivs_aportacion;
      LET dsea_nss_separacion        = dse_nss_separacion;
      LET dsea_edo_procesar          = v_ax_edo_procesar;
      LET dsea_estado                = 20;

      -- se inserta el registro en la tabla de agrupacion de DSE
      INSERT INTO dse_agrupa_devolucion(
                  id_dse_grp_devolucion,
                  id_derechohabiente,
                  num_credito,
                  tpo_transferencia,
                  origen_devolucion,
                  f_movimiento,
                  folio_liquida,
                  aivs97,
                  pesos97,
                  aivs92,
                  pesos92,
                  monto_aportacion,
                  aivs_aportacion,
                  nss_separacion,
                  edo_procesar,
                  estado)
          VALUES (dsea_id_dse_grp_devolucion,
                  dsea_id_derechohabiente,
                  dsea_num_credito,
                  dsea_tpo_transferencia,
                  dsea_origen_devolucion,
                  dsea_f_movimiento,
                  dsea_folio_liquida,
                  dsea_aivs97,
                  dsea_pesos97,
                  dsea_aivs92,
                  dsea_pesos92,
                  dsea_monto_aportacion,
                  dsea_aivs_aportacion,
                  dsea_nss_separacion,
                  dsea_edo_procesar,
                  dsea_estado);

      -- ============================================================
      -- se crea un registro para his_devolucion
      LET dseh_id_dse_grp_devolucion = dsea_id_dse_grp_devolucion;
      LET dseh_folio                 = p_folio_agrupa;
      LET dseh_tpo_transferencia     = p_tpo_transf;
      LET dseh_lote                  = v_lote;
      LET dseh_id_lote               = v_id_lote;
      LET dseh_f_presentacion        = TODAY;
      LET dseh_paterno_afore         = NULL;
      LET dseh_materno_afore         = NULL;
      LET dseh_nombre_afore          = NULL;
      LET dseh_nom_imss              = v_ax_nombre_imss;
      LET dseh_aivs97                = dsea_aivs97;
      LET dseh_pesos97               = dsea_pesos97;
      LET dseh_aivs92                = dsea_aivs92;
      LET dseh_pesos92               = dsea_pesos92;
      LET dseh_monto_aportacion      = dsea_monto_aportacion;
      LET dseh_aivs_aportacion       = dsea_aivs_aportacion;
      LET dseh_nss_separacion        = dsea_nss_separacion;
      LET dseh_edo_procesar          = dsea_edo_procesar;
      LET dseh_diagnostico           = 0;
      LET dseh_estado                = dsea_estado;
      LET dseh_f_proceso             = TODAY;

      -- se inserta el registro en la tabla de his devolucion
      INSERT INTO dse_his_devolucion (
                  id_dse_grp_devolucion,
                  folio,
                  tpo_transferencia,
                  lote,
                  id_lote,
                  f_presentacion,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  aivs97,
                  pesos97,
                  aivs92,
                  pesos92,
                  monto_aportacion,
                  aivs_aportacion,
                  nss_separacion,
                  edo_procesar,
                  diagnostico,
                  estado,
                  f_proceso)
          VALUES (dseh_id_dse_grp_devolucion,
                  dseh_folio,
                  dseh_tpo_transferencia,
                  dseh_lote,
                  dseh_id_lote,
                  dseh_f_presentacion,
                  dseh_paterno_afore,
                  dseh_materno_afore,
                  dseh_nombre_afore,
                  dseh_nom_imss,
                  dseh_aivs97,
                  dseh_pesos97,
                  dseh_aivs92,
                  dseh_pesos92,
                  dseh_monto_aportacion,
                  dseh_aivs_aportacion,
                  dseh_nss_separacion,
                  dseh_edo_procesar,
                  dseh_diagnostico,
                  dseh_estado,
                  dseh_f_proceso);

      -- se actualiza el estado del registro en dse_devolucion a 80
      UPDATE dse_devolucion
         SET estado = 15,
             folio = p_folio_agrupa
       WHERE id_derechohabiente = dse_id_derechohabiente
         ---AND num_credito = dse_num_credito
         AND estado = 10
         AND tpo_transferencia = p_tpo_transf
         AND subcuenta IN (42,44);

      LET dse_monto_aivs92     = 0;
      LET dse_monto_pesos92    = 0;
      LET dse_monto_aivs97     = 0;
      LET dse_monto_pesos97    = 0;
      LET dse_monto_aportacion = 0;
      LET dse_aivs_aportacion  = 0;
   END FOREACH;

   -- actualiza estadisticas a la tabla de agrupación y de historicos
   UPDATE STATISTICS FOR TABLE dse_agrupa_devolucion;
   UPDATE STATISTICS FOR TABLE dse_his_devolucion;

   SET PDQPRIORITY DEFAULT;

   -- se devuelve el resultado al termino del proceso
   RETURN v_resultado;

END FUNCTION;


