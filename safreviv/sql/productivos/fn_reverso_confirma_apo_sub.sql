






CREATE FUNCTION  "safreviv".fn_reverso_confirma_apo_sub(p_folio_transaccion DECIMAL(9,0))
  RETURNING SMALLINT;

--Última modificación 10102016
--Declaración de variables
  DEFINE v_error               SMALLINT;
  DEFINE v_id_ocg_detalle      DECIMAL(9,0);
   
  ON EXCEPTION SET v_error
     --Ocurrio un error al realizar el reverso de la Confirmación de
	 -- Aportaciones Subsecuentes.
     --Se regresa el número de error que ocurrio
     RETURN v_error;
  END EXCEPTION
   
  --Se inicia el error en 0 para indicar que por default no ocurrio un error
  LET v_error = 0;
   
  UPDATE dis_ctr_aps_tns
  SET    cve_ent_financiera = 0,
         num_ctr_int_ef     = 0,
         concepto           = 0,
         id_ctr_transaccion = 0,
         folio_transaccion  = 0,
         f_transaccion      = "",
         estado             = 10,
         tpo_credito        = 2
  WHERE  folio_transaccion  = p_folio_transaccion;

  {DELETE 
  FROM   ocg_his_transaccion
  WHERE  folio_referencia = p_folio_transaccion;}

  FOREACH 
    SELECT trn.id_ocg_detalle
    INTO   v_id_ocg_detalle
    FROM   ocg_ctr_transaccion trn
    WHERE  trn.folio = p_folio_transaccion

    DELETE 
    FROM   ocg_detalle
    WHERE  id_ocg_detalle = v_id_ocg_detalle;
  END FOREACH

  DELETE
  FROM   ocg_ctr_transaccion
  WHERE  folio = p_folio_transaccion;

  RETURN v_error;

END FUNCTION;


