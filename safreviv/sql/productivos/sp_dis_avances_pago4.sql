






CREATE PROCEDURE "safreviv".sp_dis_avances_pago4(p_folio DECIMAL(10,0))

DEFINE v_id_dis_det_ava_pag  DECIMAL(9,0); --Id detalle avance pago

--Última modificación 01082017
--#Inicialización de variables
  LET v_id_dis_det_ava_pag = 0;

  SET PDQPRIORITY HIGH;
  
  -- Elimina los folios que cuentan con rechazos de la tabla detalle
  DELETE 
  FROM   safre_viv:dis_det_avance_pago
  WHERE  folio = p_folio;
   
  -- Elimina los folios que cuentan con rechazos de la tabla sumario
  DELETE 
  FROM   safre_viv:dis_sum_avance_pago
  WHERE  folio = p_folio;

  -- Elimina los folios que se integraron a la tabla de referencia 
  -- de los pagos previos
  {FOREACH
    SELECT ap.id_dis_det_avance_pago
    INTO   v_id_dis_det_ava_pag
    FROM   safre_viv:dis_ava_pago_previo ap
    WHERE  ap.folio = p_folio 

    DELETE 
    FROM   safre_viv:dis_ref_pago_previo
    WHERE  id_dis_det_avance_pago = v_id_dis_det_ava_pag;
  END FOREACH;

  -- Elimina los folios que se integraron a la tabla pago previo
  DELETE 
  FROM   safre_viv:dis_ava_pago_previo
  WHERE  folio = p_folio;}

  DELETE 
  FROM   safre_viv:dis_his_transaccion
  WHERE  folio_liquida = p_folio;

  -- Elimina los folios que cuentan con rechazos de la tabla de movimientos
  DELETE 
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio;
   
  --Actualiza estado de archivo a REVERSADO 
  UPDATE glo_ctr_archivo
  SET    estado      = 3
  WHERE  proceso_cod = 902
  AND    folio       = p_folio;

END PROCEDURE;


