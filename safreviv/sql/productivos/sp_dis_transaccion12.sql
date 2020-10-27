






CREATE PROCEDURE "safreviv".sp_dis_transaccion12(p_id_derechohabiente  DECIMAL(9,0),   --id_derechohabiente
                                     p_num_crd_ifv          DECIMAL(10,0),  --Numero de credito
                                     p_periodo_bimestre     CHAR(6),        --Bimestre de pago
                                     p_fecha_pago_val       DATE,           --Fecha de pago
                                     p_imp_ap_pat           DECIMAL(12,2),  --Importe de aportaciones
				     p_monto_pesos	    DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                     p_precio_fec_pag       DECIMAL(19,14), --Precio del fondo de la fecha de pago
                                     p_precio_fec_hoy       DECIMAL(19,14), --Precio del fondo del día
                                     p_fecha_01mm           DATE,           --Fecha de movimiento
                                     p_proceso_cod          SMALLINT,       --Proceso operacion
                                     p_folio_referencia     DECIMAL(9,0),   --Folio Dis
                                     p_tpo_patron           CHAR(02),       --Tipo Patron, viene de NRP[1,2]
				     p_localiza_trabajador  CHAR(1),        --Viene Folio Dispersion
				     p_aiv_ap_pat 	    DECIMAL(18,6),  --Valor de AIVS
				     p_cve_ent_receptora    CHAR(3),		--Clave de la entidad receptora
				     p_monto_pesos_real	    DECIMAL(12,2),  --imp_am_crecc
				     p_nss		    CHAR(11),
				     p_periodo_pago	    CHAR(6))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 21062013
--Declaración de variables
DEFINE v_tpo_transferencia   CHAR(2);
DEFINE v_origen_devolucion   CHAR(2);
DEFINE v_aivs_aportacion     DECIMAL(16,6);
DEFINE v_nss_separacion      CHAR(11);
DEFINE v_estado              SMALLINT;
DEFINE v_folio 		     DECIMAL(9,0);
DEFINe v_modulo_cod	     CHAR(3);
DEFINE v_subcuenta           SMALLINT;
DEFINE v_monto_aivs          DECIMAL(22,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_proceso_cod_RP      SMALLINT;

DEFINE v_f_liquida           DATE;
--DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_id_ref_seq          DECIMAL(9,0); --Secuencia de interfaz

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;     --Estatus del proceso
DEFINE v_monto_acciones      DECIMAL(12,2);
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);
DEFINE v_movimiento          SMALLINT;
DEFINE v_periodo_pago_dae    CHAR(4);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

LET v_tpo_transferencia = 19;    --Asigna crédito 43Bis
LET v_origen_devolucion = 02;    --Asigna valor SAR97
LET v_aivs_aportacion   = 0.00;
LET v_nss_separacion    = "";
LET v_estado            = 10;    --Asigna estado reenviada
LET v_folio             = 0;
LET v_subcuenta         = 46; --Amortización excedente
LET v_monto_aivs        = 0.00;
LET v_monto_pesos       = 0.00;
LET v_proceso_cod_RP    = 1401;
LET v_bnd_transaccion   = 0;
LET v_bnd_proceso       = 0;     --Estado correcto
LET v_f_liquida         = TODAY;
LET v_f_registro        = TODAY;
LET v_fondo_inversion   = 11;
LET v_id_ref_seq        = 0;
LET v_monto_acciones    = 0.00;
LET v_h_registro        = CURRENT HOUR TO SECOND;
LET v_origen            = "DIS DAE-"||p_periodo_bimestre;
LET v_movimiento        = 501; --ABONO AMORTIZACIÓN EXCEDENTE DISPERSIÓN
LET v_modulo_cod        = "dis";
LET v_periodo_pago_dae  = substr(p_periodo_pago,3,6);


  --Se agrega conversión de AIVS - Verificar
  LET v_monto_acciones  = (p_monto_pesos_real / p_precio_fec_hoy );
  LET v_monto_pesos     = v_monto_acciones;

  IF p_monto_pesos_real > 0 THEN

     LET v_id_ref_seq   = seq_dae_det_solicitud.NEXTVAL;

     INSERT INTO dis_preliquida
          VALUES (v_f_liquida, 
                  p_id_derechohabiente, 
                  v_subcuenta,
                  v_fondo_inversion, 
                  v_movimiento, 
                  p_folio_referencia,
                  v_id_ref_seq, 
                  v_monto_acciones, 
                  p_monto_pesos_real,
                  p_fecha_pago_val, 
                  v_f_registro, 
                  v_h_registro, 
                  v_origen);
			
     INSERT INTO dae_det_solicitud
		 (id_dae_referencia,
		  id_derechohabiente,
		  folio,
		  num_credito,
		  fecha_pago,
		  periodo_pago,
		  importe_amort,
		  nss,
		  entidad_receptora,
		  folio_liquida,
		  fecha_liquida,
		  estado,
		  resul_opera,
		  motivo_rechazo)
	  VALUES
		 (v_id_ref_seq,
		  p_id_derechohabiente,
		  p_folio_referencia,
		  p_num_crd_ifv,
		  p_fecha_pago_val,
		  v_periodo_pago_dae,
		  p_monto_pesos_real,
		  p_nss,
		  p_cve_ent_receptora,
		  p_folio_referencia,
		  v_f_liquida,
		  6, --Se actualiza a 6: Integrado - Dispersión
		  "01",
		  0);
  END IF  
       
  LET v_char = "Terminada transacción 12 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;
	   
END PROCEDURE;


