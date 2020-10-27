






CREATE PROCEDURE "safreviv".sp_ajuste_tia (p_folio       DECIMAL(9,0),
                                p_proceso_cod SMALLINT,
                                p_usuario     CHAR(20))

   RETURNING SMALLINT, VARCHAR(255)

   DEFINE v_codigo_respuesta SMALLINT;
   DEFINE v_descripcion      VARCHAR(255);
   DEFINE sql_err            INTEGER;
   DEFINE isam_err           INTEGER;
   DEFINE err_txt            CHAR(200);

   --manejo de excepciones 
   ON EXCEPTION SET sql_err, isam_err, err_txt
      RETURN sql_err,  err_txt;
   END EXCEPTION

   -- REVERSAR LIQUIDACION
   UPDATE glo_folio
      SET status = 1 
    WHERE proceso_cod = p_proceso_cod
      AND status = 2
      AND folio  = p_folio;

   UPDATE glo_ctr_archivo 
      SET estado = 3
    WHERE proceso_cod = p_proceso_cod
      AND estado = 4
      AND folio = p_folio;

   -- REVERSO PRELIQUIDACION
   UPDATE glo_folio
      SET status = 0 -- integrado
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio  = p_folio;                     
   
   UPDATE glo_ctr_archivo 
      SET estado = 2  
    WHERE proceso_cod = p_proceso_cod
      AND estado = 3
      AND folio = p_folio;
      
   UPDATE tia_det_traspaso
      SET result_operacion = '01'
    WHERE folio = p_folio
      AND result_operacion in ('04','05','09','08','10');
   
   DELETE FROM tia_preliquida 
   WHERE folio_liquida =  p_folio ;

   -- REVERSO INTEGRACION
   --se actualiza el status del archivo segun folio
   UPDATE glo_folio
      SET status = -1 --3 --REVERSADO
    WHERE folio        = p_folio
      AND proceso_cod  = p_proceso_cod ;

END PROCEDURE
;


