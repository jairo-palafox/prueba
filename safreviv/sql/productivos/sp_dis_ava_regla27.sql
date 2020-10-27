






CREATE PROCEDURE "safreviv".sp_dis_ava_regla27(p_folio          DECIMAL(9,0),
                                    p_nss            CHAR(11),
                                    p_nrp            CHAR(11),
                                    p_numero_credito CHAR(10),
                                    p_periodo_pago   CHAR(6),
                                    p_monto_amo      DECIMAL(12,2),
                                    p_usuario        VARCHAR(30))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 12082014
--Declaración de variables
DEFINE v_id_ref_seq          DECIMAL(9,0); --Secuencia
DEFINE v_nss 		     CHAR(11);
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_periodo_pago        CHAR(6);
DEFINE v_f_pago              DATE;
DEFINE v_nrp                 CHAR(11);
DEFINE v_amortizacion        DECIMAL(8,2);

DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_estado 	     SMALLINT;
DEFINE v_desc_resultado      CHAR(40);

DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT; --Estatus del proceso
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

  --#Inicialización de variables
  LET v_id_ref_seq           = 0;
  LET v_nss 		     = "";
  LET v_num_credito          = 0;
  LET v_periodo_pago         = "";
  LET v_f_pago               = "";
  LET v_nrp                  = "";
  LET v_amortizacion         = 0.00;

  LET v_id_derechohabiente   = 0;
  LET v_estado               = 0;
  LET v_desc_resultado       = "";

  LET v_bnd_proceso          = 0; --Estado correcto
  LET v_bnd_transaccion      = 0;

--SET DEBUG FILE TO '/ds/safreviv/dis/sql/sp_dis_ava_regla27.TRACE';
--SET DEBUG FILE TO '/home/safreviv/sp_dis_ava_regla27.TRACE';
-- SET DEBUG FILE TO 'sp_dis_ava_regla27.TRACE';
-- TRACE ON;

  SET PDQPRIORITY HIGH;

  LET v_estado = 0; --registro integrado
  
  --Validación de Negocio NSS 
  IF p_nss IS NULL THEN
     LET v_estado         = 10; --nss nulo
     --LET v_desc_resultado = 'NSS NULO';
  ELSE
     IF LENGTH(p_nss) <> 11 THEN
        LET v_estado = 20; --nss diferente a 11 posiciones
     END IF
  END IF

  --Validación de Negocio NRP
  IF v_estado = 0 THEN
     IF p_nrp         IS NULL OR 
        LENGTH(p_nrp) <> 11 THEN
        LET v_estado = 80; --nrp nulo
     END IF
  END IF

  --Validación de Negocio Numero de Crédito
  IF v_estado = 0 THEN
     IF p_numero_credito IS NULL THEN
        LET v_estado = 40; --numero de crédito nulo
     END IF
  END IF

  --Validación de Negocio Período de Pago
  IF v_estado = 0 THEN
     IF p_periodo_pago      IS NULL OR
        LENGTH(p_periodo_pago) <> 6 THEN
        LET v_estado = 50; --periodo de pago nulo
     END IF
  END IF

  --Validación de Negocio Fecha de Pago
  {IF v_estado = 0 THEN
     IF p_f_pago IS NULL THEN
        LET v_estado = 60; --fecha de pago nula
     END IF
  END IF}

  --Validación de Negocio Monto Amortización
  IF v_estado = 0 THEN
     IF p_monto_amo <= 0.00 THEN
        LET v_estado = 100; --monto amortización menor - igual a 0
     END IF
  END IF

  IF v_estado = 0 THEN
     LET v_id_ref_seq = seq_dis_interface_ce.NEXTVAL;

     --Integra en la tabla los casos de excepción a dispersar
     INSERT INTO dis_ava_regla_27 VALUES (v_id_ref_seq,
                                          p_folio,
                                          p_periodo_pago,
                                          p_nss,
                                          p_numero_credito,
                                          p_nrp,
                                          p_monto_amo,
                                          TODAY,
                                          p_usuario);
  END IF
   
  UPDATE STATISTICS FOR TABLE dis_ava_regla_27;

--TRACE 'Finaliza sp_dis_ava_regla27 con valor '||v_bnd_proceso;   
  LET v_char = "Terminado Ava_regla27_SPL";
  RETURN v_bnd_proceso , v_estado, v_char;
  
END PROCEDURE;


