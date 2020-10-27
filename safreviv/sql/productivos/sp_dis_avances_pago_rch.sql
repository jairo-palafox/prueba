






CREATE PROCEDURE "safreviv".sp_dis_avances_pago_rch(p_folio DECIMAL(10,0),p_usuario CHAR(30))

DEFINE v_tpo_registro        CHAR(01); -- Tipo de registro
DEFINE v_id_derechohabiente  DECIMAL(9,0); -- Id derechohabiente segun nss
DEFINE v_nss                 CHAR(11); -- No. de seguro social para obtener id_derechohabiente
DEFINE v_num_credito         CHAR(10); -- Numero de credito
DEFINE v_periodo_pago        CHAR(06); -- Perido de pago
DEFINE v_f_pago              DATE; -- Fecha de pago
DEFINE v_nrp                 CHAR(11); -- Numero de registro patronal
DEFINE v_monto_aportacion    DECIMAL(12,2); -- Monto de la aportacion
DEFINE v_monto_amortizacion  DECIMAL(12,2); -- Monto de la amortización
DEFINE v_f_presentacion      DATE; -- Fecha de presentacion
DEFINE v_estado              SMALLINT; -- Tipo de estado de rechazo
DEFINE v_f_actualiza         DATE; -- Fecha de ejecución del proceso
DEFINE v_existe_registro40   DECIMAL (10,0);
DEFINE v_existe_registro30   DECIMAL (10,0); -- Contador para validar exista id avance de pago
DEFINE v_tpo_patron          CHAR(2); -- Tipo de Patron
       ------------------
DEFINE v_tot_registros       DECIMAL(10,0);
DEFINE v_tot_registros_3     DECIMAL(10,0);
DEFINE v_tot_aportacion      DECIMAL(22,2); -- Tolata de aportaciones
DEFINE v_tot_amortizacion    DECIMAL(22,2); -- Total de amortizaciones
DEFINE v_tot_detalles_archivo DECIMAL(10,0); --Total de registros de detalle
       ------------------
DEFINE v_subcuenta           SMALLINT; -- Subcuenta para amortización y aportación
DEFINE v_fdo_inversion       SMALLINT; -- Fondo de inversión 11
DEFINE v_movimiento          SMALLINT; -- Tipo de movimiento 11
DEFINE v_monto_acciones      DECIMAL(12,2); -- Monto acciones valor 0
DEFINE v_monto_pesos         DECIMAL(12,2); -- Valor monto aportación y/o amortización
DEFINE v_hora_proceso        DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
       ------------------
DEFINE v_sumario_aportacion  DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_sumario_amortizacion DECIMAL(22,2); -- Monto de la amortización
DEFINE v_dif_apo             DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_dif_amo             DECIMAL(22,2); -- Monto de la amortización
       ------------------
DEFINE v_seq_dis_avance      DECIMAL(9,0);
DEFINE v_id_det_avance_pago  DECIMAL(9,0);
DEFINE v_tpo_avance          SMALLINT; 
       ------------------
DEFINE v_origen              VARCHAR(20); 

   --#Inicialización de variables
   LET v_f_actualiza          = TODAY;
   LET v_hora_proceso         = CURRENT HOUR TO SECOND;
   LET v_tpo_registro         = "2";
   LET v_id_derechohabiente   = 0;
   LET v_nss                  = "";
   LET v_num_credito          = "";
   LET v_periodo_pago         = "";
   LET v_f_pago               = TODAY;
   LET v_nrp                  = "";
   LET v_monto_aportacion     = 0.00;
   LET v_monto_amortizacion   = 0.00;
   LET v_f_presentacion       = TODAY;
   LET v_estado               = 40;
   LET v_tot_registros        = 0;
   LET v_tot_registros_3      = 0;
   LET v_tot_aportacion       = 0.00;
   LET v_tot_amortizacion     = 0.00;
   LET v_subcuenta            = 0;
   LET v_fdo_inversion        = 0;
   LET v_movimiento           = 11;
   LET v_monto_acciones       = 0;
   LET v_monto_pesos          = 0;
   LET v_existe_registro40    = 0;
   LET v_existe_registro30    = 0;
   LET v_sumario_aportacion   = 0.00;
   LET v_sumario_amortizacion = 0.00;
   LET v_dif_apo              = 0.00;
   LET v_dif_amo              = 0.00;
   LET v_origen               = "Rech Avance";
   LET v_seq_dis_avance       = 0;
   LET v_id_det_avance_pago   = 0;
   LET v_tot_detalles_archivo = 0;
   LET v_tpo_avance           = 1812; --Valor del tipo de avance para rechazo de registro de pago

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_dis_avances_pago_rch.--TRACE';
   --TRACE 'Folio '||p_folio;
   --TRACE 'Usuario '||p_usuario;

   --#Obtener fecha de presentación
   SELECT f_presentacion
   INTO   v_f_presentacion
   FROM   safre_tmp:tmp_dis_ravances_pago0
   WHERE  tpo_registro = 0;

   --#Consulta para obtener sumatorias de aportaciones y amortizaciones en temporal
   SELECT sum(monto_aportacion), sum(monto_amortizacion)
   INTO   v_sumario_aportacion, v_sumario_amortizacion
   FROM   safre_tmp:tmp_dis_ravances_pago2;
   ----#División campos de importes
   LET v_sumario_aportacion   = v_sumario_aportacion/100 ;
   LET v_sumario_amortizacion = v_sumario_amortizacion/100 ;

   --#Consulta para sumario de avance de pago en temporal de registro
   SELECT tpo_registro, tot_registros, tot_aportacion,
          tot_amortizacion
   INTO   v_tpo_registro, v_tot_registros, v_tot_aportacion,
          v_tot_amortizacion
   FROM   safre_tmp:tmp_dis_ravances_pago4;
   ----#División campos de importes
   LET v_tot_aportacion   = v_tot_aportacion/100;
   LET v_tot_amortizacion = v_tot_amortizacion/100;
   

   --#Obtener diferiencias de aportaciones y amortizaciones
   LET v_dif_apo = v_sumario_aportacion - v_tot_aportacion;
   LET v_dif_amo = v_sumario_amortizacion - v_tot_amortizacion;

   IF v_dif_apo <> 0.00 OR v_dif_amo <> 0.00 THEN
      LET v_estado = 20;
   END IF;
   
   --Obtiene total de registros del registro tipo 3 para validación
   SELECT tot_registros
   INTO   v_tot_registros_3
   FROM   safre_tmp:tmp_dis_ravances_pago3;
   
   --Consulta total de regstros del archivo
   SELECT COUNT(*)
   INTO   v_tot_detalles_archivo
   FROM   safre_tmp:tmp_dis_ravances_pago2;
   
   
   --### Se agrega validación para rechazo por diferencia en total de registros detalle vs sumario ###--
   IF v_tot_detalles_archivo <> v_tot_registros_3 THEN
      LET v_estado = 28; 
   END IF

   --#Inserción sumario de avance de pago
   INSERT INTO safre_viv:dis_sum_avance_pago
   VALUES (p_folio, 
           v_tpo_registro, 
           v_tot_registros, 
           v_tot_aportacion,
	   v_tot_amortizacion, 
           v_f_presentacion, 
           v_estado);
			   
   --##############	   AGREGADO      ###############
   --Si hay rechazo por diferencia de totales detalle vs sumario
   IF v_estado = 28 THEN
      FOREACH
        SELECT tpo_registro, nss, num_credito, periodo_pago,
               fecha_pago, nrp, monto_aportacion, monto_amortizacion
        INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
               v_f_pago, v_nrp, v_monto_aportacion, v_monto_amortizacion
        FROM   safre_tmp:tmp_dis_ravances_pago2
			
        ----#División campos de importes
        LET v_monto_aportacion   = v_monto_aportacion/100 ;
        LET v_monto_amortizacion = v_monto_amortizacion/100 ;

        --#Obtenemos id_derechohabiente según número seguro social
        SELECT id_derechohabiente
        INTO   v_id_derechohabiente
        FROM   safre_viv:afi_derechohabiente
        WHERE  nss = v_nss;

        --#Inserción en la tabla rechazos de avance de pago
        --TRACE 'Estado '||v_estado;
        --TRACE 'Derechohabiente '||v_id_derechohabiente;
        --TRACE 'Tabla >>dis_rch_avance_pago';
        INSERT INTO safre_viv:dis_rch_avance_pago
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
      END FOREACH;
   END IF
			   
   --#Valida para insertar registros con rechazos por sumas
   IF v_estado = 20 THEN
   
      IF v_dif_apo <> 0 THEN

         LET v_estado = 25;

         FOREACH
           SELECT tpo_registro, nss, num_credito, periodo_pago,
                  fecha_pago, nrp, monto_aportacion, monto_amortizacion
           INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
                  v_f_pago, v_nrp, v_monto_aportacion, v_monto_amortizacion
           FROM safre_tmp:tmp_dis_ravances_pago2
            

           --#Obtenemos id_derechohabiente según número seguro social
           SELECT id_derechohabiente
           INTO   v_id_derechohabiente
           FROM   safre_viv:afi_derechohabiente
           WHERE  nss = v_nss;

           --#Inserción en la tabla rechazos de avance de pago
	   --TRACE 'Realiza insert cuando las diferencias de las sumatorias con aportacion no son 0';
           --TRACE 'Estado '||v_estado;
           --TRACE 'Derechohabiente '||v_id_derechohabiente;
           --TRACE 'Tabla >>dis_rch_avance_pago';
           INSERT INTO safre_viv:dis_rch_avance_pago
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

         END FOREACH;
      END IF
	  
      IF v_dif_amo <> 0 THEN

         LET v_estado = 26;

         FOREACH
           SELECT tpo_registro, nss, num_credito, periodo_pago,
                  fecha_pago, nrp, monto_aportacion, monto_amortizacion
           INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
                  v_f_pago, v_nrp, v_monto_aportacion, v_monto_amortizacion
           FROM   safre_tmp:tmp_dis_ravances_pago2

           --#Obtenemos id_derechohabiente según número seguro social
           SELECT id_derechohabiente
           INTO   v_id_derechohabiente
           FROM   safre_viv:afi_derechohabiente
           WHERE  nss = v_nss;

           --#Inserción en la tabla rechazos de avance de pago
	   --TRACE 'Realiza insert cuando las diferencias de las sumatorias con amortizacion no son 0';
           --TRACE 'Estado '||v_estado;
           --TRACE 'Derechohabiente '||v_id_derechohabiente;
           --TRACE 'Tabla >>dis_rch_avance_pago';
           INSERT INTO safre_viv:dis_rch_avance_pago
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

         END FOREACH;
      END IF
	  
      --LET v_estado = 20; --Reasigna estado = 20 solo para validacion posterior
   END IF

   --#Valida para insertar registros sin rechazo por sumas
   IF v_estado = 40  THEN
      FOREACH
        SELECT tpo_registro, nss, num_credito, periodo_pago,
               fecha_pago, nrp, monto_aportacion, monto_amortizacion
        INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
               v_f_pago, v_nrp, v_monto_aportacion, v_monto_amortizacion
        FROM   safre_tmp:tmp_dis_ravances_pago2
        
        ----#División campos de importes
        LET v_monto_aportacion   = v_monto_aportacion/100;
        LET v_monto_amortizacion = v_monto_amortizacion/100 ;
         
        --#Valor de registros no rechazados
        LET v_estado = 40;
     
	--TRACE 'Monto de aportacion -- '||v_monto_aportacion;
	--TRACE 'Monto de amortizacion -- '||v_monto_amortizacion;
        --#Obtenemos id_derechohabiente según número seguro social
        SELECT id_derechohabiente
        INTO   v_id_derechohabiente
        FROM   safre_viv:afi_derechohabiente
        WHERE  nss = v_nss;

        --#Asigna id_derechohabiente si no se encuentra en tabla
        IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
           LET v_id_derechohabiente = "999999999";
           --#Rechazo por no existir en el maestro de derechohabientes
           LET v_estado = 20;
        END IF

	--Consulta para validar que exista registro con estado 40
	SELECT count(*) 
        INTO   v_existe_registro40
        FROM   safre_viv:dis_det_avance_pago
        WHERE  periodo_pago       = v_periodo_pago
        AND    nrp                = v_nrp
        AND    id_derechohabiente = v_id_derechohabiente
        AND    num_credito        = v_num_credito
	AND    estado             = 40; --Avances de pago aplicados a la cuenta individual
		   
	IF v_existe_registro40 > 0 THEN 
	   --#Rechazo por duplicidad de registro
            LET v_estado = 21;
	END IF 
		
        IF v_monto_aportacion <= 0.00 AND v_monto_amortizacion <= 0.00 THEN
           --#Rechazo por no incluir montos
           LET v_estado = 22;
        END IF
		 
		 
	IF v_periodo_pago IS NULL OR v_periodo_pago = " " THEN
           --#Rechazo por no contar con el periodo de pago
           LET v_estado = 23;
        END IF

        IF v_f_pago IS NULL OR v_f_pago = " " THEN
           --#Rechazo por no contar con fecha de pago
           LET v_estado = 24;
        END IF
		 
	--NOTA: Se comenta momentáneamente, llave de la dispersión con NRP -- Se agrega validación 24/08/2012
	IF v_num_credito = "0000000000" OR v_num_credito IS NULL THEN
           --#Rechazo por contar con número de crédito en ceros
           LET v_estado = 27;
        END IF

        {IF v_tot_detalles_archivo <> v_tot_registros THEN
	   LET v_estado = 28; --Rechazo por diferencia total registros detalle vs sumario
        END IF}

        IF v_monto_aportacion > 0.00 THEN
           --LET v_subcuenta = 4;
           LET v_monto_pesos = v_monto_aportacion;
        END IF

        IF v_monto_amortizacion > 0.00 THEN
           --LET v_subcuenta = 41;
           LET v_monto_pesos = v_monto_amortizacion;
        END IF

        IF v_subcuenta = 4 OR v_subcuenta = 41 THEN
           LET v_fdo_inversion = 0;
        END IF

	--Consulta para validar que exista registro con estado 30
	SELECT COUNT(*), id_dis_det_avance_pago
	INTO   v_existe_registro30, v_id_det_avance_pago
        FROM   safre_viv:dis_det_avance_pago
        WHERE  periodo_pago       = v_periodo_pago
        AND    nrp                = v_nrp
        AND    id_derechohabiente = v_id_derechohabiente
        AND    num_credito        = v_num_credito
	AND    estado             = 30 --Avances de pago aplicados a la cuenta individual
	GROUP BY id_dis_det_avance_pago;
		      
        IF v_existe_registro30 = 0 THEN 
	   --#Rechazo por no existir un registro de avances de pago
           --LET v_estado = 28;
	   LET v_estado = 60; --Se cambia a estado 60 para poder utilizar el estado 28 como otro tipo de rechazo
	END IF 

        --#Registro en catalogo de movimientos solo si no fue rechazado
        --IF v_estado < 20 OR v_estado > 28 THEN --Son los estado de rechazo, del 20 al 28
	IF (v_estado < 20 OR v_estado > 28) AND (v_estado <> 60) THEN --Son los estado de rechazo, del 20 al 28 y agregando el estado 60
	   LET v_estado = 40;
		
           --#Inserción en la tabla detalle de avance de pago
           --TRACE 'Estado '||v_estado;
           --TRACE 'Derechohabiente '||v_id_derechohabiente;
           --TRACE 'Tabla >>dis_det_avance_pago';
           INSERT INTO safre_viv:dis_det_avance_pago
           VALUES (seq_dis_avance.NEXTVAL,
                   p_folio,
                   v_periodo_pago,
                   v_id_derechohabiente,
                   v_tpo_avance,
                   v_tpo_registro,
                   v_num_credito,
                   v_f_pago,
                   v_nrp,
                   v_monto_aportacion * -1,
                   v_monto_amortizacion * -1,
		   0.00,--v_dif_apo
		   0.00,--v_dif_amo,
                   v_f_presentacion,
                   v_estado);

           --#Realizar el Carogo y Abono por el Rechazo del Avance de Pagos
           -- APORTACION
           IF v_monto_aportacion > 0.00 THEN
              --TRACE 'Estado '||v_estado;
              --TRACE 'Derechohabiente '||v_id_derechohabiente;
              --TRACE 'Aportaciones';
              --TRACE 'Tabla >>cta_movimiento';
			   
	      --Se le añade el periodo de pago al origen
	      LET v_origen     = v_origen||'-'||v_periodo_pago;

              {LET v_tpo_patron = v_nrp[1,2];

              IF v_tpo_patron = '99' THEN
                 LET v_subcuenta = 44;
              ELSE
                 LET v_subcuenta = 4;
              END IF}

              LET v_subcuenta = 4;

              --ABONO RECHAZO DE AVANCE DE PAGO
              INSERT INTO safre_viv:cta_movimiento
              VALUES (TODAY,
                      v_id_derechohabiente,
                      v_subcuenta,
                      v_fdo_inversion,
                      421,
                      p_folio,
                      seq_dis_avance.CURRVAL,
                      v_monto_acciones,
                      v_monto_aportacion,
                      v_f_pago,
                      TODAY,
                      v_hora_proceso,
                      v_origen);

              --CARGO RECHAZO DE AVANCE DE PAGO
              INSERT INTO safre_viv:cta_movimiento
              VALUES (TODAY,
                      v_id_derechohabiente,
                      v_subcuenta,
                      v_fdo_inversion,
                      442,
                      p_folio,
                      seq_dis_avance.CURRVAL,
                      v_monto_acciones,
                      v_monto_aportacion * -1,
                      v_f_pago,
                      TODAY,
                      v_hora_proceso,
                      v_origen);
           END IF 

           -- AMORTIZACION
           IF v_monto_amortizacion > 0.00 THEN
              --TRACE 'Estado '||v_estado;
              --TRACE 'Derechohabiente '||v_id_derechohabiente;
              --TRACE 'Amortizaciones';
              --TRACE 'Tabla >>cta_movimiento';
			   
	      --Se le añade el periodo de pago al origen
	      LET v_origen     = v_origen||'-'||v_periodo_pago;

              {LET v_tpo_patron = v_nrp[1,2];

              IF v_tpo_patron = '99' THEN
                 LET v_subcuenta = 43;
              ELSE
                 LET v_subcuenta = 41;
              END IF}

              LET v_subcuenta = 41;

              --ABONO RECHAZO DE AVANCE DE PAGO
              INSERT INTO safre_viv:cta_movimiento
              VALUES (TODAY,
                      v_id_derechohabiente,
                      v_subcuenta,
                      v_fdo_inversion,
                      421,
                      p_folio,
                      seq_dis_avance.CURRVAL,
                      v_monto_acciones,
                      v_monto_amortizacion,
                      v_f_pago,
                      TODAY,
                      v_hora_proceso,
                      v_origen);

              --CARGO RECHAZO DE AVANCE DE PAGO
              INSERT INTO safre_viv:cta_movimiento
              VALUES (TODAY,
                      v_id_derechohabiente,
                      v_subcuenta,
                      v_fdo_inversion,
                      442,
                      p_folio,
                      seq_dis_avance.CURRVAL,
                      v_monto_acciones,
                      v_monto_amortizacion * -1,
                      v_f_pago,
                      TODAY,
                      v_hora_proceso,
                      v_origen);
           END IF 

           --Cancelación Avances de Pago por Rechazo
           UPDATE safre_viv:dis_det_avance_pago
           SET    estado                 = 41  --Cancelación Avances de Pago por Rechazo
           WHERE  id_dis_det_avance_pago = v_id_det_avance_pago
           AND    estado                 = 30;  --Avances de Pago aplicados a la cuenta individual

         ELSE
           --#Inserción en la tabla rechazos de avance de pago
           --TRACE 'Estado '||v_estado;
           --TRACE 'Derechohabiente '||v_id_derechohabiente;
           --TRACE 'Tabla >>dis_rch_avance_pago';
           INSERT INTO safre_viv:dis_rch_avance_pago
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
         END IF
      END FOREACH;
   END IF

   UPDATE statistics FOR TABLE dis_det_avance_pago;

END PROCEDURE;


