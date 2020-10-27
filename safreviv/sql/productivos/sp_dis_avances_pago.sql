






CREATE PROCEDURE "safreviv".sp_dis_avances_pago(p_folio DECIMAL(10,0),p_usuario CHAR(30))
DEFINE v_tpo_registro           CHAR(01); -- Tipo de registro
DEFINE v_id_derechohabiente     DECIMAL(9,0); -- Id derechohabiente segun nss
DEFINE v_nss                    CHAR(11); -- No. de seguro social para obtener id_derechohabiente
DEFINE v_num_credito            CHAR(10); -- Numero de credito
DEFINE v_i_num_credito          SMALLINT; --Numero de credito en entero para la validaci�n
DEFINE v_periodo_pago           CHAR(06); -- Perido de pago
DEFINE v_periodo_pago_valida    INTEGER; -- Cuenta de per�odos de pago
DEFINE v_f_pago                 DATE; -- Fecha de pago
DEFINE v_nrp                    CHAR(11); -- Numero de registro patronal
DEFINE v_monto_aportacion       DECIMAL(12,2); -- Monto de la aportacion
DEFINE v_monto_amortizacion     DECIMAL(12,2); -- Monto de la amortizaci�n

DEFINE v_monto_aportacion_ref   DECIMAL(12,2); -- Monto de la aportaci�n para sumatoria de registros duplicados
DEFINE v_monto_amortizacion_ref DECIMAL(12,2); -- Monto de la amortizaci�n para sumatoria de registros duplicados

DEFINE v_monto_aportacion_dup   DECIMAL(12,2); -- Monto de la aportaci�n duplicados
DEFINE v_monto_amortizacion_dup DECIMAL(12,2); -- Monto de la amortizaci�n duplicados

DEFINE v_f_presentacion         DATE; -- Fecha de presentacion
DEFINE v_estado                 SMALLINT; -- Tipo de estado de rechazo
DEFINE v_f_actualiza            DATE; -- Fecha de ejecuci�n del proceso
DEFINE v_existe_registro        INTEGER; -- Contador para validar si el registro es rechazado
       ------------------
DEFINE v_tot_registros          DECIMAL(10,0); -- Total de registros del sumario
DEFINE v_tot_registros_3        DECIMAL(10,0);
DEFINE v_tot_aportacion         DECIMAL(22,2); -- Tolata de aportaciones
DEFINE v_tot_amortizacion       DECIMAL(22,2); -- Total de amortizaciones
DEFINE v_tot_detalles_archivo   DECIMAL(10,0); --Total de registros de detalle
       ------------------
DEFINE v_subcuenta              SMALLINT; -- Subcuenta para amortizaci�n y aportaci�n
DEFINE v_fdo_inversion          SMALLINT; -- Fondo de inversi�n 11
DEFINE v_movimiento             SMALLINT; -- Tipo de movimiento 11
DEFINE v_monto_acciones         DECIMAL(12,2); -- Monto acciones valor 0
DEFINE v_monto_pesos            DECIMAL(12,2); -- Valor monto aportaci�n y/o amortizaci�n
DEFINE v_hora_proceso           DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
       ------------------
DEFINE v_sumario_aportacion     DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_sumario_amortizacion   DECIMAL(22,2); -- Monto de la amortizaci�n
DEFINE v_dif_apo                DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_dif_amo                DECIMAL(22,2); -- Monto de la amortizaci�n
DEFINE v_tpo_avance             SMALLINT; 
       ------------------
DEFINE v_origen                 VARCHAR(20); --Campo nuevo cta_movimiento

DEFINE v_tpo_patron             CHAR(2); --Tipo Patron

   --#Inicializaci�n de variables
   LET v_f_actualiza            = TODAY;
   LET v_hora_proceso           = CURRENT HOUR TO SECOND;
   LET v_tpo_registro           = "2";
   LET v_id_derechohabiente     = 0;
   LET v_nss                    = "";
   LET v_num_credito            = "";
   LET v_i_num_credito          = 0;
   LET v_periodo_pago           = "";
   LET v_periodo_pago_valida    = 0;
   LET v_f_pago                 = TODAY;
   LET v_nrp                    = "";
   LET v_monto_aportacion       = 0.00;
   LET v_monto_amortizacion     = 0.00;
   LET v_f_presentacion         = TODAY;
   LET v_estado                 = 10;   -- Para carga recurrente
   LET v_tot_registros          = 0;
   LET v_tot_registros_3        = 0;
   LET v_tot_aportacion         = 0.00;
   LET v_tot_amortizacion       = 0.00;
   LET v_subcuenta              = 0;
   LET v_fdo_inversion          = 0;
   LET v_movimiento             = 11;
   LET v_monto_acciones         = 0;
   LET v_monto_pesos            = 0;
   LET v_existe_registro        = 0;
   LET v_sumario_aportacion     = 0.00;
   LET v_sumario_amortizacion   = 0.00;
   LET v_dif_apo                = 0.00;
   LET v_dif_amo                = 0.00;
   LET v_origen                 = "Concentradora";
   LET v_tot_detalles_archivo   = 0;
   LET v_tpo_avance             = 181;
   LET v_monto_aportacion_ref   = 0.00;
   LET v_monto_amortizacion_ref = 0.00;
   LET v_monto_aportacion_dup   = 0.00;
   LET v_monto_amortizacion_dup = 0.00;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/VIVAvancePago1.--TRACE';
   --TRACE 'Folio '||p_folio;
   --TRACE 'Usuario '||p_usuario;

   --#Obtener fecha de presentaci�n
   SELECT f_presentacion
   INTO   v_f_presentacion
   FROM   safre_tmp:tmp_dis_avances_pago0
   WHERE  tpo_registro = 0;

   --#Consulta para obtener sumatorias de aportaciones y amortizaciones en temporal
   SELECT sum(monto_aportacion),
          sum(monto_amortizacion)
   INTO   v_sumario_aportacion,
          v_sumario_amortizacion
   FROM   safre_tmp:tmp_dis_avances_pago2;

   ----#Divisi�n campos de importes
   LET v_sumario_aportacion   = v_sumario_aportacion/100 ;
   LET v_sumario_amortizacion = v_sumario_amortizacion/100 ;

   --#Consulta para sumario de avance de pago en temporal de registro
   SELECT tpo_registro,
          tot_registros,
          tot_aportacion,
          tot_amortizacion
   INTO   v_tpo_registro,
          v_tot_registros,
          v_tot_aportacion,
          v_tot_amortizacion
   FROM   safre_tmp:tmp_dis_avances_pago4;

   ----#Divisi�n campos de importes
   LET v_tot_aportacion   = v_tot_aportacion/100;
   LET v_tot_amortizacion = v_tot_amortizacion/100;

   --#Obtener diferiencias de aportaciones y amortizaciones
   LET v_dif_apo = v_sumario_aportacion   - v_tot_aportacion;
   LET v_dif_amo = v_sumario_amortizacion - v_tot_amortizacion;

   IF v_dif_apo <> 0 OR v_dif_amo <> 0 THEN
      LET v_estado = 20;
   END IF;
   
   --Consulta total de regstros del archivo
   SELECT COUNT(*)
   INTO   v_tot_detalles_archivo
   FROM   safre_tmp:tmp_dis_avances_pago2;
   
   --Obtiene total de registros del registro tipo 3 para validaci�n
   SELECT tot_registros
   INTO   v_tot_registros_3
   FROM   safre_tmp:tmp_dis_avances_pago3;
   
   --### Se agrega validaci�n para rechazo por diferencia en total de registros detalle vs sumario ###--
   IF v_tot_detalles_archivo <> v_tot_registros_3 THEN
      LET v_estado = 28; 
   END IF
	
   SELECT count (DISTINCT periodo_pago)
   INTO   v_periodo_pago_valida
   FROM   safre_tmp:tmp_dis_avances_pago2;
	
   --### Se agrega validaci�n para rechazo por diferencia en periodos de pago y se le asigna rechazo general###--
   IF v_periodo_pago_valida > 1 THEN
      LET v_estado = 40; 
   END IF
	
   --#Inserci�n sumario de avance de pago
   INSERT INTO dis_sum_avance_pago
   VALUES (p_folio,
           v_tpo_registro,
           v_tot_registros,
           v_tot_aportacion,
           v_tot_amortizacion,
           v_f_presentacion,
           v_estado);
		   
   --##############	   AGREGADO      ###############
   --Si hay rechazo por diferencia de totales detalle vs sumario o rechazo por periodos de pago
   IF v_estado = 28 OR v_estado = 40 THEN
      FOREACH
        SELECT tpo_registro,
               nss,
               num_credito,
               periodo_pago,
               fecha_pago,
               nrp,
               monto_aportacion,
               monto_amortizacion
        INTO   v_tpo_registro,
               v_nss,
               v_num_credito,
               v_periodo_pago,
               v_f_pago,
               v_nrp,
               v_monto_aportacion,
               v_monto_amortizacion
        FROM   safre_tmp:tmp_dis_avances_pago2
			
	----#Divisi�n campos de importes
        LET v_monto_aportacion   = v_monto_aportacion/100 ;
        LET v_monto_amortizacion = v_monto_amortizacion/100 ;

        --#Obtenemos id_derechohabiente seg�n n�mero seguro social
        SELECT id_derechohabiente
        INTO   v_id_derechohabiente
        FROM   afi_derechohabiente
        WHERE  nss = v_nss;

        --#Inserci�n en la tabla rechazos de avance de pago
        --TRACE 'Estado '||v_estado;
        --TRACE 'Derechohabiente '||v_id_derechohabiente;
        --TRACE 'Tabla >>dis_rch_avance_pago';
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
                     v_monto_aportacion,
                     v_monto_amortizacion,
                     v_f_presentacion,
                     v_estado);
      END FOREACH;
   END IF 

   --#Valida para insertar registros con rechazos por sumas
   IF v_estado = 20 THEN
      IF v_dif_apo <> 0 THEN
         LET v_estado = 25;
         FOREACH
           SELECT tpo_registro,
                  nss,
                  num_credito,
                  periodo_pago,
                  fecha_pago,
                  nrp,
                  monto_aportacion,
                  monto_amortizacion
             INTO v_tpo_registro,
                  v_nss,
                  v_num_credito,
                  v_periodo_pago,
                  v_f_pago,
                  v_nrp,
                  v_monto_aportacion,
                  v_monto_amortizacion
             FROM safre_tmp:tmp_dis_avances_pago2

           ----#Divisi�n campos de importes
           LET v_monto_aportacion   = v_monto_aportacion/100 ;
           LET v_monto_amortizacion = v_monto_amortizacion/100 ;

           --#Obtenemos id_derechohabiente seg�n n�mero seguro social
           SELECT id_derechohabiente
             INTO v_id_derechohabiente
             FROM afi_derechohabiente
            WHERE nss = v_nss;

           --#Inserci�n en la tabla rechazos de avance de pago
           --TRACE 'Estado '||v_estado;
           --TRACE 'Derechohabiente '||v_id_derechohabiente;
           --TRACE 'Tabla >>dis_rch_avance_pago';
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
                        v_monto_aportacion,
                        v_monto_amortizacion,
                        v_f_presentacion,
                        v_estado);

         END FOREACH;
      END IF

      IF v_dif_amo <> 0 THEN
         LET v_estado = 26;
         FOREACH
            SELECT tpo_registro,
                   nss,
                   num_credito,
                   periodo_pago,
                   fecha_pago,
                   nrp,
                   monto_aportacion,
                   monto_amortizacion
              INTO v_tpo_registro,
                   v_nss,
                   v_num_credito,
                   v_periodo_pago,
                   v_f_pago,
                   v_nrp,
                   v_monto_aportacion,
                   v_monto_amortizacion
              FROM safre_tmp:tmp_dis_avances_pago2

            ----#Divisi�n campos de importes
            LET v_monto_aportacion   = v_monto_aportacion/100;
            LET v_monto_amortizacion = v_monto_amortizacion/100 ;

            --#Obtenemos id_derechohabiente seg�n n�mero seguro social
            SELECT id_derechohabiente
              INTO v_id_derechohabiente
              FROM afi_derechohabiente
             WHERE nss = v_nss;

            --#Inserci�n en la tabla rechazos de avance de pago
            --TRACE 'Estado '||v_estado;
            --TRACE 'Derechohabiente '||v_id_derechohabiente;
            --TRACE 'Tabla >>dis_rch_avance_pago';
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
                         v_monto_aportacion,
                         v_monto_amortizacion,
                         v_f_presentacion,
                         v_estado);

         END FOREACH;
      END IF
   END IF

   --#Valida para insertar registros sin rechazo por sumas
   IF v_estado = 10 THEN
      FOREACH
         SELECT tpo_registro,
                nss,
                num_credito,
                periodo_pago,
                fecha_pago,
                nrp,
                monto_aportacion,
                monto_amortizacion
           INTO v_tpo_registro,
                v_nss,
                v_num_credito,
                v_periodo_pago,
                v_f_pago,
                v_nrp,
                v_monto_aportacion,
                v_monto_amortizacion
           FROM safre_tmp:tmp_dis_avances_pago2
         
         ----#Divisi�n campos de importes
         LET v_monto_aportacion   = v_monto_aportacion/100;
         LET v_monto_amortizacion = v_monto_amortizacion/100 ;

         --#Valor de registros no rechazados
         LET v_estado = 10;

         --#Obtenemos id_derechohabiente seg�n n�mero seguro social
         SELECT id_derechohabiente
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         --#Asigna id_derechohabiente si no se encuentra en tabla
         IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
            LET v_id_derechohabiente = "999999999";
            --#Rechazo por no existir en el maestro de derechohabientes
            LET v_estado             = 20;
         END IF

         IF v_monto_aportacion <= 0.00 AND v_monto_amortizacion <= 0.00 THEN
            --#Rechazo por no incluir montos
            LET v_estado = 22;
         END IF

         IF v_monto_aportacion > 0.00 THEN
            --LET v_subcuenta   = 4;
            LET v_monto_pesos = v_monto_aportacion;
         END IF

         IF v_monto_amortizacion > 0.00 THEN
            --LET v_subcuenta   = 41;
            LET v_monto_pesos = v_monto_amortizacion;
         END IF

         IF v_subcuenta = 4 OR v_subcuenta = 41 THEN
            LET v_fdo_inversion = 0;
         END IF

         IF v_periodo_pago IS NULL OR v_periodo_pago = "      " THEN
            --#Rechazo por no contar con el periodo de pago
            LET v_estado = 23;
         END IF

         IF v_f_pago IS NULL OR v_f_pago = "          " THEN
            --#Rechazo por no contar con fecha de pago
            LET v_estado = 24;
         END IF

         --Nota: En espera de validaci�n "0000000000"  -- Se agrega la validaci�n 24/08/2012
         --LET v_i_num_credito = v_num_credito;
         IF v_num_credito = "0000000000" OR v_num_credito IS NULL THEN
            --#Rechazo por contar con n�mero de cr�dito en ceros
            LET v_estado = 27;
         END IF
		 
	 IF v_tot_detalles_archivo <> v_tot_registros_3 THEN
	    LET v_estado = 28; --Rechazo por diferencia total registros detalle vs sumario
	 END IF

         IF v_nrp = "00000000000" OR
            v_nrp IS NULL OR
            v_nrp = "" OR 
            v_nrp = "           " THEN
            --#Rechazo por NO contar con el nrp
            LET v_estado = 29;
         END IF

         --#Consulta si existe duplicidad
         --TRACE 'v_id_derechohabiente '||v_id_derechohabiente;
         --TRACE 'v_num_credito '||v_num_credito;
         --TRACE 'v_periodo_pago '||v_periodo_pago;
         IF EXISTS(
                   SELECT dis.id_derechohabiente
                     FROM dis_det_avance_pago dis
                    WHERE dis.folio              = p_folio
                      AND dis.periodo_pago       = v_periodo_pago
                      AND dis.nrp                = v_nrp
                      AND dis.id_derechohabiente = v_id_derechohabiente
                      AND dis.num_credito        = v_num_credito
                      AND dis.estado            IN (30,10)
                   ) THEN
				   
	    --------######  Se agrega la condici�n de que el rechazo por duplicidad de registro desaparece 
            --------######  y en cambio se debe realizar la actualizaci�n al mismo    ######------	   
		
	    SELECT dis.monto_aportacion, dis.monto_amortizacion
	    INTO   v_monto_aportacion_ref, v_monto_amortizacion_ref
	    FROM   dis_det_avance_pago dis
	    WHERE  dis.periodo_pago       = v_periodo_pago
	    AND    dis.nrp                = v_nrp
	    AND    dis.id_derechohabiente = v_id_derechohabiente
	    AND    dis.num_credito        = v_num_credito
	    AND    dis.estado            IN (30,10);
                           
	    --Realiza suma de importes 
	    LET v_monto_aportacion_dup   = v_monto_aportacion   + v_monto_aportacion_ref;
	    LET v_monto_amortizacion_dup = v_monto_amortizacion + v_monto_amortizacion_ref;
				     	
            --Realiza actualizaci�n de registro
            UPDATE dis_det_avance_pago
            SET    monto_aportacion   = v_monto_aportacion_dup,
                   monto_amortizacion = v_monto_amortizacion_dup,
                   monto_dif_apo      = v_monto_aportacion_dup,
                   monto_dif_amo      = v_monto_amortizacion_dup
            WHERE  periodo_pago       = v_periodo_pago
            AND    nrp                = v_nrp
            AND    id_derechohabiente = v_id_derechohabiente
            AND    num_credito        = v_num_credito
            AND    estado            IN (30,10);
		
            --- APORTACION 
            IF v_monto_aportacion > 0.00 THEN
	       --Se le a�ade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
               {LET v_tpo_patron = v_nrp[1,2];

               IF v_tpo_patron = '99' tHEN
                  LET v_subcuenta = 44;
               ELSE
                  LET v_subcuenta = 4;
               END IF}
                        
                LET v_subcuenta = 4;

               --ABONO AVANCE DE PAGO
               INSERT INTO cta_movimiento
                    VALUES (TODAY,
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            11,
                            p_folio,
                            seq_dis_avance.CURRVAL,
                            v_monto_acciones,
                            v_monto_aportacion,
                            v_f_pago,
                            TODAY,
                            v_hora_proceso,
                            v_origen);
            END IF;
            --- APORTACION 
					
            --- AMORTIZACION
            IF v_monto_amortizacion > 0.00 THEN
	       --Se le a�ade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
               {LET v_tpo_patron = v_nrp[1,2];

               IF v_tpo_patron = '99' tHEN
                  LET v_subcuenta = 43;
               ELSE
                  LET v_subcuenta = 41;
               END IF}
                        
               LET v_subcuenta = 41;

               --ABONO AVANCE DE PAGO
               INSERT INTO cta_movimiento
                    VALUES (TODAY,
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            11,
                            p_folio,
                            seq_dis_avance.CURRVAL,
                            v_monto_acciones,
                            v_monto_amortizacion,
                            v_f_pago,
                            TODAY,
                            v_hora_proceso,
                            v_origen);
               --ABONO AVANCE DE PAGO
            END IF;
            --- AMORTIZACION
					
            --Limpia variables
            LET v_monto_aportacion_ref   = 0.00;
            LET v_monto_amortizacion_ref = 0.00;
            LET v_monto_aportacion_dup   = 0.00;
            LET v_monto_amortizacion_dup = 0.00;

            CONTINUE FOREACH;
				   
            --#Rechazo por duplicidad de registro
            --LET v_estado = 21;
            
         ELSE
            --TRACE 'Registro no duplicado '||v_estado;
            --TRACE 'v_id_derechohabiente '||v_id_derechohabiente;
         END IF

         --TRACE 'Inserta>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>estado '||v_estado;

         --#Registro en catalogo de movimientos solo si no fue rechazado
         IF v_estado = 10 THEN
            --#Inserci�n en la tabla detalle de avance de pago
            --TRACE 'Estado '||v_estado;
            --TRACE 'Derechohabiente '||v_id_derechohabiente;
            --TRACE 'Tabla >>dis_det_avance_pago';
            INSERT INTO dis_det_avance_pago
                 VALUES (seq_dis_avance.NEXTVAL,
                         p_folio,
                         v_periodo_pago,
                         v_id_derechohabiente,
			 v_tpo_avance,
                         v_tpo_registro,
                         v_num_credito,
                         v_f_pago,
                         v_nrp,
                         v_monto_aportacion,
                         v_monto_amortizacion,
                         v_monto_aportacion,    ---v_dif_apo,
                         v_monto_amortizacion,  ---v_dif_amo,
                         v_f_presentacion,
                         v_estado);
					
	    ---- ##########################################################################################3  -----
	    IF v_monto_aportacion > 0.00 THEN
				
	       --Se le a�ade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
	       {LET v_tpo_patron = v_nrp[1,2];

	       IF v_tpo_patron = '99' THEN
	          LET v_subcuenta = 44;
	       ELSE
	          LET v_subcuenta = 4;
	       END IF}
				
	        LET v_subcuenta = 4;
 
	       --ABONO AVANCE DE PAGO
	       INSERT INTO safre_viv:cta_movimiento
	            VALUES (TODAY,
	                    v_id_derechohabiente,
	                    v_subcuenta,
	                    v_fdo_inversion,
	                    11,
	                    p_folio,
	                    seq_dis_avance.CURRVAL,
	                    v_monto_acciones,
	                    v_monto_aportacion,
	                    v_f_pago,
	                    TODAY,
	                    v_hora_proceso,
	                    v_origen);
	    END IF;
					
	    -- AMORTIZACION
	    IF v_monto_amortizacion > 0.00 THEN

	       --Se le a�ade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
	       {LET v_tpo_patron = v_nrp[1,2];

	       IF v_tpo_patron = '99' THEN
		  LET v_subcuenta = 43;
	       ELSE
		  LET v_subcuenta = 41;
               END IF}

	       LET v_subcuenta = 41;

               --ABONO AVANCE DE PAGO
	       INSERT INTO safre_viv:cta_movimiento
		    VALUES (TODAY,
		            v_id_derechohabiente,
		            v_subcuenta,
		            v_fdo_inversion,
		            11,
		            p_folio,
		            seq_dis_avance.CURRVAL,
		            v_monto_acciones,
		            v_monto_amortizacion,
		            v_f_pago,
		            TODAY,
		            v_hora_proceso,
		            v_origen);
            END IF 
         ELSE
            --#Inserci�n en la tabla rechazos de avance de pago
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
                         v_monto_aportacion,
                         v_monto_amortizacion,
                         v_f_presentacion,
                         v_estado);
         END IF
      END FOREACH;
   END IF

   update statistics for table dis_det_avance_pago;

END PROCEDURE;


