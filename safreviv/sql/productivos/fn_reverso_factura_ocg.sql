






CREATE FUNCTION  "safreviv".fn_reverso_factura_ocg(p_folio_factura DECIMAL(9,0))
  RETURNING SMALLINT;

--Última modificación 01092016
--Declaración de variables
  DEFINE v_error             SMALLINT;
  DEFINE v_referencia        CHAR(16);
   
  ON EXCEPTION SET v_error
     --Ocurrio un error al realizar el reverso de la Facturación de OCG
     --Se regresa el número de error que ocurrio
     RETURN v_error;
  END EXCEPTION
   
  --Se inicia el error en 0 para indicar que por default no ocurrio un error
  LET v_error = 0;

  {FOREACH
    SELECT a.referencia
    INTO   v_referencia
    FROM   dis_ctr_factura_aps a
    WHERE  a.folio_factura = p_folio_factura

    DELETE 
    FROM   dis_ctr_edo_factura_aps
    WHERE  referencia = v_referencia;
  END FOREACH;}

  UPDATE dis_ctr_aps_tns
  SET    concepto             = 107,
         folio_factura        = 0,
         f_factura            =  "",
         estado               = 20
  WHERE  folio_factura        = p_folio_factura
  AND    concepto             = 117;

  UPDATE dis_ctr_aps_tns
  SET    concepto             = 307,
         folio_factura        = 0,
         f_factura            =  "",
         estado               = 20
  WHERE  folio_factura        = p_folio_factura
  AND    concepto             = 317;

  UPDATE dis_ctr_aps_tns
  SET    concepto             = 407,
         folio_factura        = 0,
         f_factura            =  "",
         estado               = 20
  WHERE  folio_factura        = p_folio_factura
  AND    concepto             = 417;

  UPDATE dis_ctr_aps_tns
  SET    concepto             = 807,
         folio_factura        = 0,
         f_factura            =  "",
         estado               = 20
  WHERE  folio_factura        = p_folio_factura
  AND    concepto             = 817;

  RETURN v_error;

END FUNCTION;


