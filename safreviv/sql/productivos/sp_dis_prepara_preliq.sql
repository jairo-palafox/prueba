






CREATE PROCEDURE "safreviv".sp_dis_prepara_preliq(p_folio_pag     DECIMAL(9,0), 
                                       p_folio_dis     DECIMAL(9,0),
                                       p_usuario       CHAR(20),
                                       p_pid           DECIMAL(9,0),  --Pid genrado para el proceso
                                       p_proceso_cod   SMALLINT,      --Proceso dispersión preliquidación
                                       p_opera_cod     SMALLINT,      --Operacion preliquidacion
                                       p_tpo_ejecucion SMALLINT)
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 05112018
--Declaración de variables
DEFINE r_instruccion_mdt     SMALLINT;       --Bandera instruccion mandato
DEFINE r_bnd_epera_error     SMALLINT;       --Bandera operacion error
DEFINE v_derechohabiente_real DECIMAL(9,0);  --Derechohabiente del proceso de mandatos
DEFINE v_referencia_real     DECIMAL(9,0);   --Identificador de referencia del proceso de mandatos
DEFINE v_monto_pesos_real    DECIMAL(12,2);

DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_nss                 CHAR(11);
DEFINE v_folio_sua           DECIMAL(6,0);
DEFINE v_periodo_pago        CHAR(6);
DEFINE v_f_pago              DATE;
DEFINE v_reg_pat_imss        CHAR(11);
DEFINE v_tot_apo_subs        INTEGER;

DEFINE ef_id_derechohabiente DECIMAL(9,0);
DEFINE ef_nss                CHAR(11);
DEFINE ef_folio_sua          DECIMAL(6,0);
DEFINE ef_periodo_pago       CHAR(6);
DEFINE ef_f_pago             DATE;
DEFINE ef_nrp                CHAR(11);
DEFINE ef_ind_liquidacion    SMALLINT;
DEFINE ef_tot_apo_subs       INTEGER;
DEFINE ef_id_dis_interface   DECIMAL(9,0);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(70);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_tot_imp_apo_aivs    DECIMAL(26,6);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET r_instruccion_mdt      = 0;
LET r_bnd_epera_error      = 0;
LET v_derechohabiente_real = 0;
LET v_referencia_real      = 0;
LET v_monto_pesos_real     = 0.00;

  --Tipo ejecucion 0
  --Crea tabla dis_preliquida
  --Ejecuta Mandatos (Pendiente)
  IF p_tpo_ejecucion = 0 THEN
     --Ejecuta stored para generar las instrucciones de mandatos
     CREATE TABLE dis_preliquida
                 (f_liquida          DATE NOT NULL,
                  id_derechohabiente DECIMAL(9,0) NOT NULL ,
                  subcuenta          SMALLINT NOT NULL ,
                  fondo_inversion    SMALLINT NOT NULL ,
                  movimiento         SMALLINT NOT NULL ,
                  folio_liquida      DECIMAL(9,0) NOT NULL ,
                  id_referencia      DECIMAL(9,0) NOT NULL ,
                  monto_acciones     DECIMAL(22,2),
                  monto_pesos        DECIMAL(22,2),
                  f_valor            DATE,
                  f_registro         DATE,
                  h_registro         DATETIME HOUR TO SECOND,
                  origen             CHAR(20))
     FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;
   
     SET PDQPRIORITY HIGH;
   
  END IF

  --Tipo ejecucion 1
  --Crea índices
  --Actualiza estadísticas
  IF p_tpo_ejecucion = 1 THEN
     CREATE INDEX xdis_preliquida2 ON dis_preliquida
     (folio_liquida) IN dis_ix_dbs;
	 
     SET INDEXES FOR dis_interface_ef_ad ENABLED;
     SET INDEXES FOR dis_interface_hs ENABLED;
     SET INDEXES FOR dis_info_inconsistente ENABLED;
     SET INDEXES FOR dis_compensa_avance ENABLED;
     SET INDEXES FOR dis_his_transaccion ENABLED;
     SET INDEXES FOR dis_crd_ceros ENABLED;

     UPDATE STATISTICS FOR TABLE dis_preliquida;
     UPDATE STATISTICS FOR TABLE dis_interface_hs;
     UPDATE STATISTICS FOR TABLE dis_his_hs;
     UPDATE STATISTICS FOR TABLE dis_interface_ef_ad;
     UPDATE STATISTICS FOR TABLE dis_compensa_avance;
     UPDATE STATISTICS FOR TABLE dis_info_inconsistente;
     UPDATE STATISTICS FOR TABLE dis_his_transaccion;
     UPDATE STATISTICS FOR TABLE dis_crd_ceros;
  END IF

  LET v_char = "Terminado Prepara Preliquidacion ";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


