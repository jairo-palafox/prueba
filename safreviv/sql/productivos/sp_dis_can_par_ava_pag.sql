






CREATE PROCEDURE "safreviv".sp_dis_can_par_ava_pag(p_folio DECIMAL(9,0),p_usuario CHAR(30))

--Última modificación 13112018
--Declaración de variables
DEFINE v_nss                 CHAR(11);      -- No. de seguro social para obtener id_derechohabiente
DEFINE v_num_credito         CHAR(10);      -- Número de credito
DEFINE v_nrp                 CHAR(11);      -- Numero de registro patronal
DEFINE v_periodo_pago        VARCHAR(06);   -- Perido de pago en formato AABB (año-bimestre)
DEFINE v_aportacion          DECIMAL(9,2);  -- Monto aportación
DEFINE v_amortizacion        DECIMAL(9,2);  -- Monto amortización
DEFINE v_monto_apo_fin       DECIMAL(12,2); -- Monto de la aportacion final
DEFINE v_monto_amo_fin       DECIMAL(12,2); -- Monto de la amortización final
  
DEFINE v_tpo_registro        CHAR(01);      -- Tipo de registro
DEFINE v_f_pago              DATE;          -- Fecha de pago
DEFINE v_monto_aportacion    DECIMAL(12,2); -- Monto de la aportacion
DEFINE v_monto_amortizacion  DECIMAL(12,2); -- Monto de la amortización
DEFINE v_f_presentacion      DATE;          -- Fecha de presentacion
DEFINE v_estado              SMALLINT;      -- Tipo de estado de rechazo
DEFINE v_subcuenta           SMALLINT;      -- Subcuenta para amortización y aportación
DEFINE v_fdo_inversion       SMALLINT;      -- Fondo de inversión 
DEFINE v_movimiento          SMALLINT;      -- Tipo de movimiento 
DEFINE v_monto_pesos         DECIMAL(12,2); -- Valor monto aportación y/o amortización
DEFINE v_hora_proceso        DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
DEFINE v_origen              VARCHAR(20); 
DEFINE v_causal              CHAR(02);      -- Perido de pago en formato AABB (año-bimestre)
DEFINE v_id_derechohabiente  DECIMAL(9,0);  -- Id derechohabiente segun nss
DEFINE v_tpo_avance          SMALLINT;
DEFINE v_tpo_avance_fin      SMALLINT; 
DEFINE v_tpo_patron          CHAR(2);       -- Tipo de Patron
DEFINE v_cuenta_cancelado    INTEGER;
DEFINE v_estado_ava          SMALLINT;
DEFINE v_edo_can_par         SMALLINT;
DEFINE v_band_canc_may       SMALLINT;
DEFINE v_edo_ava_fin         SMALLINT;
DEFINE v_id_dis_det_ap_orig  DECIMAL(9,0);

  --#Inicialización de variables
  LET v_nss                = "";
  LET v_num_credito        = "";
  LET v_nrp                = "";
  LET v_periodo_pago       = "";
  LET v_aportacion         = 0;
  LET v_amortizacion       = 0;

  LET v_monto_apo_fin      = 0;
  LET v_monto_amo_fin      = 0;

  LET v_hora_proceso       = CURRENT HOUR TO SECOND;
  LET v_id_derechohabiente = 0;
  LET v_f_pago             = TODAY;
  LET v_monto_aportacion   = 0.00;
  LET v_monto_amortizacion = 0.00;
  LET v_f_presentacion     = TODAY;
  LET v_subcuenta          = 0;
  LET v_fdo_inversion      = 0;
  LET v_movimiento         = 0;    --Validar
  LET v_monto_pesos        = 0;
  LET v_origen             = "";   --nrp y periodo de pago  
  LET v_causal             = 0;
  LET v_tpo_registro       = "80"; --80 por el catálogo de layout
  LET v_tpo_avance_fin     = 181;
  LET v_tpo_avance         = 1815;
  LET v_cuenta_cancelado   = 0;
  LET v_estado             = 100;  --Estado 100 cuando no se rechaza el registro; 101 cuando no existe en el maestro de derechohabientes
		                           --y 102 cuando valida que exista por los 3 campos
  LET v_estado_ava         = 0;

  LET v_edo_can_par        = 0;    --Rechazado
  LET v_band_canc_may      = 0;
  LET v_edo_ava_fin        = 30;
  LET v_id_dis_det_ap_orig = 0;
   
  FOREACH
    --Obtiene registros de la tabla temporal
    SELECT nss,
           num_credito, 
           nrp, 
           bimestre,
           aportacion,
           amortizacion 
    INTO   v_nss,
           v_num_credito, 
           v_nrp, 
           v_periodo_pago, 
           v_aportacion,
           v_amortizacion
    FROM   safre_tmp:tmp_dis_can_parc_ava0

    ----#División campos de importes
    LET v_aportacion   = v_aportacion/100 ;
    LET v_amortizacion = v_amortizacion/100 ;

    --#Obtenemos id_derechohabiente según número seguro social
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = v_nss;

    LET v_estado        = 100; --Se asume que no se rechaza
    LET v_edo_can_par   = 0;   --Rechazado
    LET v_band_canc_may = 0;   --Sin Rechazo
    
    --#Asigna id_derechohabiente si no se encuentra en tabla
    IF (v_id_derechohabiente IS NULL) OR 
       (v_id_derechohabiente  = 0)    THEN
       LET v_id_derechohabiente = "999999999";
       --#Rechazo por no existir en el maestro de derechohabientes
       LET v_estado = 101;
    ELSE
       -- ##### Se agrega validación para sólo almacenar un registro de cancelación ##### --
       SELECT COUNT(*)
       INTO   v_cuenta_cancelado
       FROM   dis_det_avance_pago a
       WHERE  a.id_derechohabiente = v_id_derechohabiente
       AND    a.num_credito        = v_num_credito
       AND    a.periodo_pago       = v_periodo_pago
       AND    a.nrp                = v_nrp
       AND    a.estado             = 100;
				
       --Si enuentra un registro duplicado, lo inserta en tabla de rechazos para poder llevar control
       IF v_cuenta_cancelado > 0 THEN
          --Rechazo por estar duplicado el registro
          LET v_estado = 103;
					
          --Valida que exista el registro con el nss, el periodo de pago y el nrp
          SELECT FIRST 1 a.num_credito, 
                 a.f_pago,
                 a.monto_aportacion, 
                 a.monto_amortizacion,
                 a.id_dis_det_avance_pago
          INTO   v_num_credito, 
                 v_f_pago, 
                 v_monto_aportacion, 
                 v_monto_amortizacion,
                 v_id_dis_det_ap_orig
          FROM   dis_det_avance_pago a
          WHERE  a.id_derechohabiente = v_id_derechohabiente
          AND    a.periodo_pago       = v_periodo_pago
          AND    a.num_credito        = v_num_credito
          AND    a.nrp                = v_nrp;
		  
          INSERT INTO dis_rch_avance_pago VALUES (seq_dis_rch_avance.NEXTVAL,
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

          INSERT INTO dis_canc_par_ava_pag VALUES (seq_dis_canc_par_ap.NEXTVAL,
                                                   p_folio,
                                                   v_periodo_pago,
                                                   v_id_derechohabiente,
                                                   v_tpo_avance,
                                                   v_nss,
                                                   v_num_credito,
                                                   v_nrp,
                                                   v_aportacion,
                                                   v_amortizacion,
                                                   v_id_dis_det_ap_orig,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   v_edo_can_par);
          CONTINUE FOREACH;
       END IF;

       --Valida que exista el registro con el nss, el periodo de pago y el nrp
       SELECT FIRST 1 A.id_derechohabiente,
              D.f_pago,
              D.monto_aportacion, 
              D.monto_amortizacion,
              D.num_credito,
              D.estado,
              D.id_dis_det_avance_pago
       INTO   v_id_derechohabiente,
              v_f_pago,
              v_monto_aportacion,
              v_monto_amortizacion,
              v_num_credito,
              v_estado_ava,
              v_id_dis_det_ap_orig
       FROM   safre_viv:afi_derechohabiente A, 
              safre_viv:dis_det_avance_pago D
       WHERE  A.id_derechohabiente = D.id_derechohabiente
       AND    D.num_credito        = v_num_credito
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

       IF v_estado_ava = 30 THEN
          IF v_monto_aportacion > 0.00 THEN            
             IF v_aportacion > v_monto_aportacion THEN
                LET v_estado        = 105;
                LET v_band_canc_may = 1;   --Con Rechazo
             END IF;
          ELSE
             IF v_monto_aportacion <= 0.00 AND 
                v_aportacion        > 0.00 THEN
                LET v_estado        = 105;
                LET v_band_canc_may = 1;   --Con Rechazo
             END IF;
          END IF;

          IF v_monto_amortizacion > 0.00 THEN            
             IF v_amortizacion > v_monto_amortizacion THEN
                LET v_estado        = 105;
                LET v_band_canc_may = 1;   --Con Rechazo
             END IF;              
          ELSE
             IF v_monto_amortizacion <= 0.00 AND 
                v_amortizacion        > 0.00 THEN
                LET v_estado        = 105;
                LET v_band_canc_may = 1;   --Con Rechazo
             END IF;
          END IF;

          IF v_band_canc_may = 1 THEN
             INSERT INTO dis_rch_avance_pago VALUES (seq_dis_rch_avance.NEXTVAL,
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

             INSERT INTO dis_canc_par_ava_pag VALUES (seq_dis_canc_par_ap.NEXTVAL,
                                                      p_folio,
                                                      v_periodo_pago,
                                                      v_id_derechohabiente,
                                                      v_tpo_avance,
                                                      v_nss,
                                                      v_num_credito,
                                                      v_nrp,
                                                      v_aportacion,
                                                      v_amortizacion,
                                                      v_id_dis_det_ap_orig,
                                                      v_monto_aportacion,
                                                      v_monto_amortizacion,
                                                      0,
                                                      0,
                                                      v_edo_can_par);
             CONTINUE FOREACH;
          END IF;     
       END IF;
    END IF

    --#Registro en catálogo de movimientos solo si no fue rechazado
    IF (v_estado = 100) THEN 
       --SE INSERTAN LOS MONTOS DEL ARCHIVO
       LET v_monto_apo_fin = 0;
       LET v_monto_amo_fin = 0;

       LET v_edo_can_par   = 1;   --Aceptado

       --#Inserción en la tabla detalle de avance de pago
       INSERT INTO safre_viv:dis_det_avance_pago (id_dis_det_avance_pago,
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
                                                  monto_dif_apo,
                                                  monto_dif_amo,
                                                  f_presentacion,
                                                  estado)
                                          VALUES (seq_dis_avance.NEXTVAL,
                                                  p_folio,
                                                  v_periodo_pago,
                                                  v_id_derechohabiente,
                                                  v_tpo_avance, --1815
                                                  v_tpo_registro, --80
                                                  v_num_credito,
                                                  v_f_pago,
                                                  v_nrp,
                                                  v_aportacion * -1,
                                                  v_amortizacion * -1,
                                                  v_monto_aportacion,
                                                  v_monto_amortizacion,                                                 
                                                  v_f_presentacion,
                                                  v_estado);

       LET v_monto_apo_fin = v_monto_aportacion   - v_aportacion;
       LET v_monto_amo_fin = v_monto_amortizacion - v_amortizacion;

       IF (v_monto_apo_fin = 0.00 AND v_monto_amo_fin = 0.00) THEN
          --Cancelación Avances de Pago
          UPDATE safre_viv:dis_det_avance_pago
          SET    estado             = 70  -- Para cancelación ------ 
          WHERE  id_derechohabiente = v_id_derechohabiente
          AND    num_credito        = v_num_credito
          AND    periodo_pago       = v_periodo_pago
          AND    tpo_avance         = 181 
          AND    nrp                = v_nrp
          AND    estado             = 30;
       ELSE
          IF (v_monto_apo_fin > 0.00 OR v_monto_amo_fin > 0.00) THEN
             --#Inserción en la tabla detalle de avance de pago (final)
             UPDATE safre_viv:dis_det_avance_pago
             SET    monto_aportacion   = v_monto_apo_fin,
                    monto_amortizacion = v_monto_amo_fin,
                    monto_dif_apo      = v_monto_apo_fin,
                    monto_dif_amo      = v_monto_amo_fin
             WHERE  id_derechohabiente = v_id_derechohabiente
             AND    num_credito        = v_num_credito
             AND    periodo_pago       = v_periodo_pago
             AND    tpo_avance         = 181 
             AND    nrp                = v_nrp
             AND    estado             = 30;
             
             {INSERT INTO safre_viv:dis_det_avance_pago (id_dis_det_avance_pago,
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
                                                        monto_dif_apo,
                                                        monto_dif_amo,
                                                        f_presentacion,
                                                        estado)
                                                VALUES (seq_dis_avance.NEXTVAL,
                                                        p_folio,
                                                        v_periodo_pago,
                                                        v_id_derechohabiente,
                                                        v_tpo_avance_fin, --181
                                                        v_tpo_registro,   --80
                                                        v_num_credito,
                                                        v_f_pago,
                                                        v_nrp,
                                                        v_monto_apo_fin,
                                                        v_monto_amo_fin,
                                                        v_monto_apo_fin,
                                                        v_monto_amo_fin,                                                        
                                                        v_f_presentacion,
                                                        v_edo_ava_fin);}
          END IF
       END IF

       INSERT INTO dis_canc_par_ava_pag VALUES (seq_dis_canc_par_ap.NEXTVAL,
                                                p_folio,
                                                v_periodo_pago,
                                                v_id_derechohabiente,
                                                v_tpo_avance,
                                                v_nss,
                                                v_num_credito,
                                                v_nrp,
                                                v_aportacion,
                                                v_amortizacion,
                                                seq_dis_avance.CURRVAL,
                                                v_monto_aportacion,
                                                v_monto_amortizacion,
                                                v_monto_apo_fin,
                                                v_monto_amo_fin,
                                                v_edo_can_par);
    ELSE
       INSERT INTO safre_viv:dis_rch_avance_pago VALUES (seq_dis_rch_avance.NEXTVAL,
                                                         p_folio,
                                                         v_tpo_avance,
                                                         v_tpo_registro,
                                                         v_nss,
                                                         v_id_derechohabiente,
                                                         v_num_credito,
                                                         v_periodo_pago,
                                                         v_f_pago,
                                                         v_nrp,
                                                         v_aportacion * -1,
                                                         v_amortizacion * -1,
                                                         v_f_presentacion,
                                                         v_estado);

       INSERT INTO safre_viv:dis_canc_par_ava_pag VALUES (seq_dis_canc_par_ap.NEXTVAL,
                                                          p_folio,
                                                          v_periodo_pago,
                                                          v_id_derechohabiente,
                                                          v_tpo_avance,
                                                          v_nss,
                                                          v_num_credito,
                                                          v_nrp,
                                                          v_aportacion,
                                                          v_amortizacion,
                                                          0,
                                                          0,
                                                          0,
                                                          0,
                                                          0,
                                                          v_edo_can_par);
    END IF
  END FOREACH;

  SET INDEXES FOR dis_rch_avance_pago ENABLED;  
  SET INDEXES FOR dis_canc_par_ava_pag ENABLED;
  
  UPDATE statistics FOR TABLE dis_det_avance_pago;
  UPDATE statistics FOR TABLE dis_rch_avance_pago;
  UPDATE statistics FOR TABLE dis_canc_par_ava_pag;

END PROCEDURE;


