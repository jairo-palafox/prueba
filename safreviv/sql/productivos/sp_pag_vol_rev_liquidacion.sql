






CREATE PROCEDURE "safreviv".sp_pag_vol_rev_liquidacion(p_folio       DECIMAL(9,0),
                                            p_proceso_cod SMALLINT)
                                            
RETURNING INTEGER, 
          INTEGER, 
          VARCHAR(255)

DEFINE v_error_sql     INTEGER;
DEFINE v_isam_error    INTEGER;
DEFINE v_mensaje_error CHAR(200);

   --manejo de excepciones
   ON EXCEPTION SET v_error_sql, 
                    v_isam_error, 
                    v_mensaje_error
      
      
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error;
   END EXCEPTION

   -- se asume que el programa termina sin error
   LET v_error_sql     = 0;
   LET v_isam_error    = 0;
   LET v_mensaje_error = "El reverso de liquidación finalizó correctamente";

   -- se elimina la liquidacion con funcion general de reverso de liquidacion
   EXECUTE FUNCTION fn_reverso_liquidacion (p_folio ) INTO v_error_sql ;
   
   IF( v_error_sql <> 0 )THEN
      LET v_mensaje_error = "Error al eliminar movimientos";
   
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error;
   END IF

   --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 1
   UPDATE glo_folio
      SET status = 1
    WHERE proceso_cod = p_proceso_cod
      AND folio = p_folio;

   RETURN v_error_sql, 
          v_isam_error, 
          v_mensaje_error;
   
END PROCEDURE;


