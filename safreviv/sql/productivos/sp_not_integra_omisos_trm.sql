






CREATE PROCEDURE "safreviv".sp_not_integra_omisos_trm(p_folio DECIMAL(9,0))
RETURNING SMALLINT

	DEFINE v_error                SMALLINT;
   DEFINE v_cza_fecha            DATE;
   DEFINE v_det_total            INTEGER;
   DEFINE v_sum_fecha            DATE;
   DEFINE v_sum_total            INTEGER;
   DEFINE v_int_total            INTEGER;
	
ON EXCEPTION SET v_error     --por si hay error lo cacha en la variable definida
	RETURN v_error;          --devuelve el error  -- aqui se cachan todos los errores que puedan existir cuando se definen las variables
END EXCEPTION

   SET PDQPRIORITY HIGH;

   SELECT fecha_op 
   INTO v_cza_fecha
   FROM safre_tmp:tmp_cza_omisos_trm;

   SELECT COUNT(*) 
   INTO v_det_total
   FROM safre_tmp:tmp_det_omisos_trm;

   SELECT fecha_op, tot_registros
   INTO v_sum_fecha, v_sum_total
   FROM safre_tmp:tmp_sum_omisos_trm;

   IF (v_cza_fecha <> v_sum_fecha)THEN
      RETURN 1;   --Fechas de operación no coinciden
   END IF

   IF (v_det_total <> v_sum_total)THEN
      RETURN 2;   --Numero de detalles no corresponde con sumario
   END IF
   
   INSERT INTO not_det_omisos_trm 
   SELECT p_folio,
         afi.nss,
         afi.id_derechohabiente,
         det.nrp,
         (det.aportacion/100),
         (det.amortizacion/100),
         det.mes_periodo_pago,
         det.ano_periodo_pago
   FROM safre_tmp:tmp_det_omisos_trm det
   INNER JOIN afi_derechohabiente afi ON afi.nss = det.nss;

	UPDATE STATISTICS FOR TABLE not_det_omisos_trm;

   SELECT COUNT(*) 
   INTO v_int_total
   FROM not_det_omisos_trm
   WHERE folio = p_folio;

   IF (v_int_total <> v_sum_total)THEN
      RETURN 3;   --Numero de detalles integrados no corresponde con sumario
   END IF

   SET PDQPRIORITY DEFAULT;	
	
	RETURN 0;
END PROCEDURE;


