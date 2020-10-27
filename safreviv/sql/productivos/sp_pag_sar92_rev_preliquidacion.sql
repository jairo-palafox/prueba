






CREATE PROCEDURE "safreviv".sp_pag_sar92_rev_preliquidacion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT                 
                 );
                   -- Control de Excepciones
  DEFINE sql_err                INTEGER;
  DEFINE isam_err               INTEGER;
  DEFINE err_txt                CHAR(200);
  DEFINE v_c_msj                CHAR(200);
  DEFINE v_si_resultado         SMALLINT;
  DEFINE v_id_derechohabiente   DECIMAL(9,0); 
  DEFINE v_id_solicitud         DECIMAL(9,0);
  DEFINE v_marca                SMALLINT;

   -- no se borran los datos de la tabla de preliquidacion
   -- porque la tabla se regenera en cada ejecucion
   DELETE 
     FROM pag_sar92_preliquida
    WHERE folio_liquida = p_folio;
    
    --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 0
   UPDATE glo_folio
      SET status = 0
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio = p_folio;
   
   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 2
   UPDATE glo_ctr_archivo 
      SET estado = 2  
    WHERE proceso_cod = p_proceso_cod
      AND estado = 3
      AND folio = p_folio;
      
 --RETURN v_si_resultado, err_txt;
END PROCEDURE;


