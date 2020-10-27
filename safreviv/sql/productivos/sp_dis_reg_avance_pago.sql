






CREATE PROCEDURE "safreviv".sp_dis_reg_avance_pago(p_folio DECIMAL(10,0),p_usuario CHAR(30))
DEFINE v_tpo_registro           CHAR(01); -- Tipo de registro
DEFINE v_id_derechohabiente     DECIMAL(9,0); -- Id derechohabiente segun nss
DEFINE v_nss                    CHAR(11); -- No. de seguro social para obtener id_derechohabiente
DEFINE v_num_credito            CHAR(10); -- Numero de credito
DEFINE v_i_num_credito          SMALLINT; --Numero de credito en entero para la validación
DEFINE v_periodo_pago           CHAR(06); -- Perido de pago
DEFINE v_periodo_pago_valida    INTEGER; -- Cuenta de períodos de pago
DEFINE v_f_pago                 DATE; -- Fecha de pago
DEFINE v_nrp                    CHAR(11); -- Numero de registro patronal
DEFINE v_monto_aportacion       DECIMAL(12,2); -- Monto de la aportacion
DEFINE v_monto_amortizacion     DECIMAL(12,2); -- Monto de la amortización

DEFINE v_monto_aportacion_ref   DECIMAL(12,2); -- Monto de la aportación para sumatoria de registros duplicados
DEFINE v_monto_amortizacion_ref DECIMAL(12,2); -- Monto de la amortización para sumatoria de registros duplicados

DEFINE v_monto_aportacion_dup   DECIMAL(12,2); -- Monto de la aportación duplicados
DEFINE v_monto_amortizacion_dup DECIMAL(12,2); -- Monto de la amortización duplicados

DEFINE v_f_presentacion         DATE; -- Fecha de presentacion
DEFINE v_estado                 SMALLINT; -- Tipo de estado de rechazo
DEFINE v_f_actualiza            DATE; -- Fecha de ejecución del proceso
DEFINE v_existe_registro        INTEGER; -- Contador para validar si el registro es rechazado
       ------------------
DEFINE v_tot_registros          DECIMAL(10,0); -- Total de registros del sumario
DEFINE v_tot_registros_3        DECIMAL(10,0);
DEFINE v_tot_aportacion         DECIMAL(22,2); -- Tolata de aportaciones
DEFINE v_tot_amortizacion       DECIMAL(22,2); -- Total de amortizaciones
DEFINE v_tot_detalles_archivo   DECIMAL(10,0); --Total de registros de detalle
       ------------------
DEFINE v_subcuenta              SMALLINT; -- Subcuenta para amortización y aportación
DEFINE v_fdo_inversion          SMALLINT; -- Fondo de inversión 11
DEFINE v_movimiento             SMALLINT; -- Tipo de movimiento 11
DEFINE v_monto_acciones         DECIMAL(12,2); -- Monto acciones valor 0
DEFINE v_monto_pesos            DECIMAL(12,2); -- Valor monto aportación y/o amortización
DEFINE v_hora_proceso           DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
       ------------------
DEFINE v_sumario_aportacion     DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_sumario_amortizacion   DECIMAL(22,2); -- Monto de la amortización
DEFINE v_dif_apo                DECIMAL(22,2); -- Monto de la aportacion
DEFINE v_dif_amo                DECIMAL(22,2); -- Monto de la amortización
DEFINE v_tpo_avance             SMALLINT; 
       ------------------
DEFINE v_origen                 VARCHAR(20); --Campo nuevo cta_movimiento

DEFINE v_tpo_patron             CHAR(2); --Tipo Patron

DEFINE v_id_referencia          DECIMAL(9,0);

   --#Inicialización de variables
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
   LET v_id_referencia          = 0;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/VIVAvancePago1.--TRACE';
   --TRACE 'Folio '||p_folio;
   --TRACE 'Usuario '||p_usuario;


   --#Valida para insertar registros sin rechazo por sumas
      FOREACH
         SELECT id_derechohabiente,
                monto_aportacion,
                monto_amortizacion,
                f_pago,
                periodo_pago,
                id_dis_det_avance_pago
           INTO v_id_derechohabiente,
                v_monto_aportacion,
                v_monto_amortizacion,
                v_f_pago,
                v_periodo_pago,
                v_id_referencia
           FROM dis_det_avance_pago
          WHERE folio = p_folio
         
            --- APORTACION 
            IF v_monto_aportacion > 0.00 THEN
	       --Se le añade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
                        
                LET v_subcuenta = 4;

               --ABONO AVANCE DE PAGO
               --INSERT INTO safre_tmp:tmp_dis_movimiento
               INSERT INTO cta_movimiento
                    VALUES ('01292013',
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            11,
                            p_folio,
                            --seq_dis_avance.CURRVAL,
                            v_id_referencia,
                            v_monto_acciones,
                            v_monto_aportacion,
                            v_f_pago,
                            '01292013',
                            v_hora_proceso,
                            v_origen);

               --INSERT INTO safre_tmp:tmp_dis_movimiento
               INSERT INTO cta_movimiento
                    VALUES ('01292013',
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            502,
                            p_folio,
                            --seq_dis_avance.CURRVAL,
                            v_id_referencia,
                            v_monto_acciones,
                            v_monto_aportacion * -1,
                            v_f_pago,
                            '01292013',
                            v_hora_proceso,
                            v_origen);
            END IF;
            --- APORTACION 
					
            --- AMORTIZACION
            IF v_monto_amortizacion > 0.00 THEN
	       --Se le añade el periodo de pago al origen
	       LET v_origen     = v_origen||'-'||v_periodo_pago;
                        
               LET v_subcuenta = 41;

               --ABONO AVANCE DE PAGO
               --INSERT INTO safre_tmp:tmp_dis_movimiento
               INSERT INTO cta_movimiento
                    VALUES ('01292013',
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            11,
                            p_folio,
                            --seq_dis_avance.CURRVAL,
                            v_id_referencia,
                            v_monto_acciones,
                            v_monto_amortizacion,
                            v_f_pago,
                            '01292013',
                            v_hora_proceso,
                            v_origen);

               --INSERT INTO safre_tmp:tmp_dis_movimiento
               INSERT INTO cta_movimiento
                    VALUES ('01292013',
                            v_id_derechohabiente,
                            v_subcuenta,
                            v_fdo_inversion,
                            502,
                            p_folio,
                            --seq_dis_avance.CURRVAL,
                            v_id_referencia,
                            v_monto_acciones,
                            v_monto_amortizacion * -1,
                            v_f_pago,
                            '01292013',
                            v_hora_proceso,
                            v_origen);
               --ABONO AVANCE DE PAGO
            END IF;
            --- AMORTIZACION
					
            --Limpia variables
           LET v_id_derechohabiente = 0;
           LET v_monto_aportacion   = 0.0;
           LET v_monto_amortizacion = 0.0;
           LET v_f_pago             = '';
           LET v_periodo_pago       = '';
           LET v_id_referencia      = 0;

      END FOREACH;
END PROCEDURE;


