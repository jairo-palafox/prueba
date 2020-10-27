






CREATE PROCEDURE "safreviv".sp_pag_sar92_rev_integracion(p_folio DECIMAL(9,0))

 RETURNING SMALLINT, INTEGER, VARCHAR(255)
  
  -- Control de Excepciones
  DEFINE v_si_resultado              SMALLINT;
  DEFINE err_txt                     VARCHAR(255);
  DEFINE v_error                     SMALLINT;
  DEFINE isam_err                    INTEGER ;  
   
  ON EXCEPTION
     SET v_error, isam_err, err_txt
     --TRACE 'Ocurrio el error:'||v_error;
     --TRACE isam_err;
     --TRACE error_info;
     RETURN v_error, isam_err, err_txt ;
  END EXCEPTION --WITH RESUME
    
  -- se borran los datos de las tablas historicas de SAR92
  DELETE FROM pag_cza_sar92    WHERE folio = p_folio;
  DELETE FROM pag_det_sar92    WHERE folio = p_folio;
  DELETE FROM pag_sum_sar92    WHERE folio = p_folio;
  
  UPDATE glo_folio
     SET status = -1
   WHERE folio = p_folio;
    
	LET v_si_resultado = 0;
  LET isam_err       = 0;
  LET err_txt        = "El proceso de carga/integración finalizó correctamente.";  
  
END PROCEDURE;


