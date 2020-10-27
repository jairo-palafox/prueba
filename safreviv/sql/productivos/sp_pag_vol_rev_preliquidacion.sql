






CREATE PROCEDURE "safreviv".sp_pag_vol_rev_preliquidacion(p_folio       DECIMAL(9,0),
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
   LET v_mensaje_error = "El reverso de preliquidación finalizó correctamente.";

   
   DELETE
     FROM pag_preliquida
    WHERE folio_liquida = p_folio;
    
   --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 0
   UPDATE glo_ctr_archivo
      SET estado = 1
    WHERE proceso_cod = p_proceso_cod
      AND estado = 2
      AND folio = p_folio;
   
    --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 0
   UPDATE glo_folio
      SET status = 0
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio = p_folio;
   
   RETURN v_error_sql, 
          v_isam_error, 
          v_mensaje_error;
          
END PROCEDURE;


