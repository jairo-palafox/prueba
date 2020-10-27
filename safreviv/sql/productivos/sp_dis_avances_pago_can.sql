






CREATE PROCEDURE "safreviv".sp_dis_avances_pago_can(p_folio DECIMAL(9,0),p_usuario CHAR(30))

  --Definición de variables
  DEFINE v_tpo_registro         CHAR(01); -- Tipo de registro
  DEFINE v_num_credito          CHAR(10); -- Número de credito
  DEFINE v_f_pago               DATE; -- Fecha de pago
  DEFINE v_monto_aportacion     DECIMAL(12,2); -- Monto de la aportacion
  DEFINE v_monto_amortizacion   DECIMAL(12,2); -- Monto de la amortización
  DEFINE v_f_presentacion       DATE; -- Fecha de presentacion
  DEFINE v_estado               SMALLINT; -- Tipo de estado de rechazo
  DEFINE v_subcuenta            SMALLINT; -- Subcuenta para amortización y aportación
  DEFINE v_fdo_inversion        SMALLINT; -- Fondo de inversión
  DEFINE v_movimiento           SMALLINT; -- Tipo de movimiento
  DEFINE v_monto_pesos          DECIMAL(12,2); -- Valor monto aportación y/o amortización
  DEFINE v_hora_proceso         DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
  DEFINE v_origen               VARCHAR(20);
  DEFINE v_nss                  CHAR(11); -- No. de seguro social para obtener id_derechohabiente
  DEFINE v_nrp                  CHAR(11); -- Numero de registro patronal
  DEFINE v_periodo_pago         VARCHAR(06); -- Perido de pago en formato AABB (año-bimestre)
  DEFINE v_causal               CHAR(02); -- Perido de pago en formato AABB (año-bimestre)
  DEFINE v_id_derechohabiente   DECIMAL(9,0); -- Id derechohabiente segun nss
  DEFINE v_tpo_avance           SMALLINT;
  DEFINE v_tpo_patron           CHAR(2); -- Tipo de Patron
  DEFINE v_cuenta_cancelado     INTEGER;
  DEFINE v_estado_ava           SMALLINT;

  --#Inicialización de variables
  LET v_hora_proceso       = CURRENT HOUR TO SECOND;
  LET v_id_derechohabiente = 0;
  LET v_num_credito        = "";
  LET v_f_pago             = TODAY;
  LET v_monto_aportacion   = 0.00;
  LET v_monto_amortizacion = 0.00;
  LET v_f_presentacion     = TODAY;
  LET v_subcuenta          = 0;
  LET v_fdo_inversion      = 0;
  LET v_movimiento         = 0; --Validar
  LET v_monto_pesos        = 0;
  LET v_origen             = ""; --nrp y periodo de pago
  LET v_nss                = "";
  LET v_nrp                = "";
  LET v_periodo_pago       = "";
  LET v_causal             = 0;
  LET v_tpo_registro       = "80"; --80 por el catálogo de layout
  LET v_tpo_avance         = 1811;
  LET v_cuenta_cancelado   = 0;
  LET v_estado             = 100;       --Estado 100 cuando no se rechaza el registro; 101 cuando no existe en el maestro de derechohabientes
                                        --y 102 cuando valida que exista por los 3 campos
  LET v_estado_ava         = 0;

  FOREACH
    --Obtiene registros de la tabla temporal
    SELECT nss,
           nrp,
           periodo_pago,
           causal
    INTO   v_nss,
           v_nrp,
           v_periodo_pago,
           v_causal
    FROM   safre_tmp:tmp_dis_can_ava_pag0

        --Valida si el periodo es anterior a 2000
        IF v_periodo_pago > "9500" AND v_periodo_pago <= "9999" THEN
                LET v_periodo_pago = "19"||v_periodo_pago; --Se concatena el inicio del año al periodo de pago para que quede en formato yyyybb
        ELSE
                LET v_periodo_pago = "20"||v_periodo_pago; --Se concatena el inicio del año al periodo de pago para que quede en formato yyyybb
        END IF

    --#Obtenemos id_derechohabiente según número seguro social
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = v_nss;

        LET v_estado = 100; --Se asume que no se rechaza

    --#Asigna id_derechohabiente si no se encuentra en tabla
    IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
       LET v_id_derechohabiente = "999999999";
       --#Rechazo por no existir en el maestro de derechohabientes
       LET v_estado = 101;
    ELSE
       -- ##### Se agrega validación para sólo almacenar un registro de cancelación ##### --
       SELECT count(*)
       INTO   v_cuenta_cancelado
       FROM   dis_det_avance_pago
       WHERE  id_derechohabiente = v_id_derechohabiente
       AND    periodo_pago       = v_periodo_pago
       AND    nrp                = v_nrp
       AND    estado             = 100;

       --Si enuentra un registro duplicado, lo inserta en tabla de rechazos para poder llevar control
       IF v_cuenta_cancelado > 0 THEN
          --Rechazo por estar duplicado el registro
                  LET v_estado = 103;

                  --Valida que exista el registro con el nss, el periodo de pago y el nrp
                  SELECT FIRST 1 num_credito,
                                 f_pago,
                                 monto_aportacion,
                                 monto_amortizacion
                  INTO   v_num_credito,
                                 v_f_pago,
                                 v_monto_aportacion,
                                 v_monto_amortizacion
                  FROM   dis_det_avance_pago
                  WHERE  id_derechohabiente = v_id_derechohabiente
                  AND    periodo_pago       = v_periodo_pago
                  AND    nrp                = v_nrp;


                  INSERT INTO dis_rch_avance_pago
              VALUES (seq_dis_rch_avance.NEXTVAL,
                       p_folio,
                       v_tpo_avance,
                       v_tpo_registro,
                       v_nss,
                       v_id_derechohabiente,
                       v_num_credito,
                       v_periodo_pago,
                       v_f_pago,
                       v_nrp,
                       v_monto_aportacion * -1,
                       v_monto_amortizacion * -1,
                       v_f_presentacion,
                       v_estado);

                   CONTINUE FOREACH;
       END IF;

       --Valida que exista el registro con el nss, el periodo de pago y el nrp
       SELECT FIRST 1 A.id_derechohabiente,
              D.f_pago,
              D.monto_aportacion,
              D.monto_amortizacion,
              D.num_credito,
              D.estado
       INTO   v_id_derechohabiente,
              v_f_pago,
              v_monto_aportacion,
              v_monto_amortizacion,
              v_num_credito,
              v_estado_ava
       FROM   safre_viv:afi_derechohabiente A,
              safre_viv:dis_det_avance_pago D
       WHERE  A.id_derechohabiente = D.id_derechohabiente
       AND    D.periodo_pago       = v_periodo_pago
       AND    A.nss                = v_nss
       AND    D.nrp                = v_nrp;

       IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
          LET v_id_derechohabiente = "999999999";
          --#Rechazo por no existir registro
          LET v_estado = 102;
       END IF;

       IF v_estado_ava <> 30 THEN
          LET v_estado = 104;
       END IF;
    END IF

    --#Registro en catálogo de movimientos solo si no fue rechazado
    IF (v_estado = 100) THEN
        LET v_origen = v_nrp||'-'||v_periodo_pago;

        --#Inserción en la tabla detalle de avance de pago
        INSERT INTO safre_viv:dis_det_avance_pago
                    (id_dis_det_avance_pago,
                     folio,
                     periodo_pago,
                     id_derechohabiente,
                     tpo_avance,
                     tpo_registro,
                     num_credito,
                     f_pago,
                     nrp,
                     monto_aportacion,
                     monto_amortizacion,
                     f_presentacion,
                     estado)
        VALUES      (seq_dis_avance.NEXTVAL,
                     p_folio,
                     v_periodo_pago,
             v_id_derechohabiente,
                     v_tpo_avance, --1811
                     v_tpo_registro, --80
             v_num_credito,
             v_f_pago,
                     v_nrp,
             v_monto_aportacion * -1,
             v_monto_amortizacion * -1,
             v_f_presentacion,
                     v_estado);

        --#Duplicar registro si aportación > 0
        IF v_monto_aportacion > 0.00 THEN
           --LET v_movimiento = 522; --Cargo cancelación avance de pago

           LET v_subcuenta = 4;

           --Insertamos en cat_movimiento con monto de aportación
           --ABONO CANCELACIÓN DE AVANCES
           INSERT INTO safre_viv:cta_movimiento
                       (f_liquida,
                                id_derechohabiente,
                                subcuenta,
                                fondo_inversion,
                                movimiento,
                                folio_liquida,
                                id_referencia,
                                monto_acciones,
                                monto_pesos,
                                f_valor,
                                f_registro,
                                h_registro,
                                origen)
               VALUES  (TODAY,
                        v_id_derechohabiente,
                        v_subcuenta,
                        v_fdo_inversion,
                        431,
                        p_folio,
                        seq_dis_avance.CURRVAL,
                        0.00,
                        v_monto_aportacion,
                        v_f_pago,
                        TODAY,
                        v_hora_proceso,
                        v_origen);

           --CARGO CANCELACIÓN DE AVANCES
           INSERT INTO safre_viv:cta_movimiento
                       (f_liquida,
                                id_derechohabiente,
                                subcuenta,
                                fondo_inversion,
                                movimiento,
                                folio_liquida,
                                id_referencia,
                        monto_acciones,
                                monto_pesos,
                                f_valor,
                                f_registro,
                                h_registro,
                                origen)
           VALUES      (TODAY,
                        v_id_derechohabiente,
                        v_subcuenta,
                        v_fdo_inversion,
                        522,
                        p_folio,
                        seq_dis_avance.CURRVAL,
                        0.00,
                        v_monto_aportacion * -1,
                        v_f_pago,
                        TODAY,
                        v_hora_proceso,
                        v_origen);
        END IF

        IF v_monto_amortizacion > 0.00 THEN

           LET v_subcuenta = 41;

           --Insertamos en cat_movimiento con monto de amortización
           --ABONO CANCELACIÓN DE AVANCES
           INSERT INTO safre_viv:cta_movimiento
                       (f_liquida,
                                id_derechohabiente,
                                subcuenta,
                                fondo_inversion,
                                movimiento,
                                folio_liquida,
                                id_referencia,
                        monto_acciones,
                                monto_pesos,
                                f_valor,
                                f_registro,
                                h_registro,
                                origen)
           VALUES      (TODAY,
                        v_id_derechohabiente,
                        v_subcuenta,
                        v_fdo_inversion,
                        431,
                        p_folio,
                        seq_dis_avance.CURRVAL,
                        0.00,
                        v_monto_amortizacion,
                        v_f_pago,
                        TODAY,
                        v_hora_proceso,
                        v_origen);

           --CARGO CANCELACIÓN DE AVANCES
           INSERT INTO safre_viv:cta_movimiento
                       (f_liquida,
                                id_derechohabiente,
                                subcuenta,
                                fondo_inversion,
                                movimiento,
                                folio_liquida,
                                id_referencia,
                        monto_acciones,
                                monto_pesos,
                                f_valor,
                                f_registro,
                                h_registro,
                                origen)
           VALUES      (TODAY,
                        v_id_derechohabiente,
                        v_subcuenta,
                        v_fdo_inversion,
                        522,
                        p_folio,
                        seq_dis_avance.CURRVAL,
                        0.00,
                        v_monto_amortizacion * -1,
                        v_f_pago,
                        TODAY,
                        v_hora_proceso,
                        v_origen);
        END IF

        --Cancelación Avances de Pago
        UPDATE safre_viv:dis_det_avance_pago
                SET    estado             = 70  -- Para cancelación ------
                WHERE  id_derechohabiente = v_id_derechohabiente
                AND    periodo_pago       = v_periodo_pago
                AND    tpo_avance         = 181
                AND    nrp                = v_nrp
        AND    estado             = 30;
    ELSE
        INSERT INTO safre_viv:dis_rch_avance_pago
        VALUES      (seq_dis_rch_avance.NEXTVAL,
                     p_folio,
                     v_tpo_avance,
                     v_tpo_registro,
                     v_nss,
                     v_id_derechohabiente,
                     v_num_credito,
                     v_periodo_pago,
                     v_f_pago,
                     v_nrp,
                     v_monto_aportacion * -1,
                     v_monto_amortizacion * -1,
                     v_f_presentacion,
                     v_estado);
    END IF
  END FOREACH;

  UPDATE statistics FOR TABLE dis_det_avance_pago;

END PROCEDURE;


