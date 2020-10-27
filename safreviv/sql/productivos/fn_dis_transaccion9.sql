






create procedure "safreviv".fn_dis_transaccion9(p_edo_credito          SMALLINT,       --Tipo de credito del derechohabiente
                                     p_derechohabiente_pag  DECIMAL(9,0),   --Id derechohabiente
                                     p_num_credito          DECIMAL(10,0),  --Numero de credito
                                     p_periodo_bimestre     CHAR(6),        --Periodo de Pago Bimestre
                                     p_f_pago               DATE,           --Fecha de pago patronal
                                     p_imp_ap_pat           DECIMAL(12,2),  --Importe aportaciones patronales
                                     p_monto_pesos          DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                     p_precio_f_pag         DECIMAL(19,14), --Precio de fecha de pago
                                     p_precio_fec_hoy       DECIMAL(19,14), --Precio de fondo del dia
                                     p_fecha_01mm           DATE,           --Fecha de movimiento, proceso
                                     p_proceso_cod_reg_pago SMALLINT,       --Codigo proceso de registro de pagos
                                     p_folio_disp           DECIMAL(9,0),   --Folio de dispersión
                                     p_tpo_patron           CHAR(02),       --Tipo de patron
                                     p_localiza_trabajador  CHAR(01),       --Localización del trabajador
                                     p_monto_pesos_real     DECIMAL(12,2),  --Amortización
                                     p_id_referencia        DECIMAL(9,0),   --ID de referencia
                                     p_nrp                  CHAR(11),       --Registro patronal
                                     p_folio_sua            DECIMAL(6),     --Folio detalle trabajador 
                                     p_folio_reg_pag        DECIMAL(9,0),   --Folio de registro de pagos
                                     p_tipo_trabajador      SMALLINT,       --Codigo de proceso del tipo de trabajador
									 p_aiv_ap_pat 	    	DECIMAL(18,6),  --Valor de AIVS 
									 p_cve_ent_receptora    CHAR(3),	    --Clave de la entidad receptora
									 p_nss		    		CHAR(11),
									 p_periodo_pago	    	CHAR(6),
									 p_destino_ap_viv       CHAR(1),        --Se agrega destino
									 p_tipo_credito			SMALLINT)
					 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 15062016
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE r_bnd_epera_error     SMALLINT;       --Bandera operacion error

DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN v_status, isam_err, error_info;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_bnd_transaccion      = 0;

  --Si derechohabiente no tiene AVANCE ABIERTO
  --DEVOLUCION DE IMPORTES EXCEDENTES para APORTACIONES
  EXECUTE PROCEDURE sp_dis_Transaccion14(p_derechohabiente_pag,
                                         p_num_credito,
                                         p_periodo_bimestre,
                                         p_f_pago,
                                         p_imp_ap_pat,
                                         p_monto_pesos,
                                         p_precio_f_pag,
                                         p_precio_fec_hoy,
                                         p_fecha_01mm,
                                         p_proceso_cod_reg_pago,
                                         p_folio_disp,
                                         p_tpo_patron,
                                         p_localiza_trabajador,
										 p_aiv_ap_pat, --Se agrega valor de AIVS
										 p_tipo_credito)
               INTO v_bnd_transaccion, v_status, error_info;

  --AMORTIZACION Pago a Cartera para AMORTIZACIONES
  EXECUTE PROCEDURE sp_dis_Transaccion5(p_derechohabiente_pag,
                                        0,
                                        p_monto_pesos_real,
                                        p_folio_disp,
                                        p_id_referencia,
                                        p_precio_fec_hoy,
                                        p_nrp,
                                        p_periodo_bimestre,
                                        p_folio_sua,
                                        p_num_credito,
                                        p_folio_reg_pag,
                                        p_proceso_cod_reg_pago,
                                        p_tpo_patron,
                                        p_f_pago,
                                        p_tipo_trabajador,
                                        p_localiza_trabajador,
                                        0, --Se agrega valor de AIVS
                                        p_destino_ap_viv, --Se agrega destino
                                        p_tipo_credito)
               INTO v_bnd_transaccion, v_status, error_info;	

  LET v_char = "  Terminada lanzador de transacciones Créditos MEJORAVIT";
  RETURN v_bnd_transaccion, v_status, error_info;

END PROCEDURE;


