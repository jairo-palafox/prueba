






CREATE PROCEDURE "safreviv".sp_pag_vol_rev_integracion(p_folio DECIMAL(9,0),
                                            p_proceso_cod SMALLINT)

RETURNING SMALLINT, 
          INTEGER, 
          VARCHAR(255)
          
-- Control de Excepciones
DEFINE v_error_sql            INTEGER;
DEFINE v_isam_error           INTEGER;
DEFINE v_mensaje_error        CHAR(200);


   --manejo de excepciones
   ON EXCEPTION SET v_error_sql, 
                    v_isam_error, 
                    v_mensaje_error
      
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error;
   END EXCEPTION
   
   LET v_error_sql     = 0;
   LET v_isam_error    = 0;
   LET v_mensaje_error = "El reverso de integración finalizó correctamente.";
   
  -- se borran los datos de las tablas historicas de aportaciones voluntarias
  DELETE FROM pag_cza_apvol  WHERE folio = p_folio;
  DELETE FROM pag_det_apvol  WHERE folio = p_folio;
  DELETE FROM pag_sum_apvol  WHERE folio = p_folio;
  
  -- se actualiza el estatus del archivo cargado
  UPDATE glo_folio
     SET status = -1
   WHERE folio = p_folio;
   
  --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 3 
   UPDATE glo_ctr_archivo 
      SET estado = 1
    WHERE proceso_cod = p_proceso_cod
      AND folio = p_folio;
      
      
   RETURN v_error_sql, 
          v_isam_error, 
          v_mensaje_error;
  
END PROCEDURE;


