






CREATE PROCEDURE "safreviv".sp_tia_reverso_preliquidacion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT
                 );
   -- Control de Excepciones
   DEFINE sql_err                      INTEGER    ;
   DEFINE isam_err                     INTEGER    ;
   DEFINE err_txt                      CHAR(200)  ;
   DEFINE v_c_msj                      CHAR(200)  ;
   DEFINE v_si_resultado               SMALLINT   ;
   DEFINE v_i_contador_tia_movi        INTEGER    ;
   DEFINE v_i_contador_cta_decreto     INTEGER    ;
   DEFINE v_i_contador_tia_preliqudia  INTEGER    ;
   --manejo de excepciones
  --ON EXCEPTION SET sql_err, isam_err, err_txt
  --    LET v_si_resultado = sql_err;
  --    
  --    RETURN v_si_resultado, isam_err, err_txt;
  --END EXCEPTION
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_tia_reverso_preliquidacion.txt';
   --TRACE 'Inicia Reverso PRELIQUIDACIONintegracion con Folio:' || p_folio ||" - Fecha:" || TODAY;
  
  LET  v_i_contador_tia_movi = 0 ;
  LET  v_i_contador_cta_decreto = 0 ;
  LET  v_i_contador_tia_preliqudia = 0 ;
  
  
  -- se actualiza el estatus del folio a integrado
   UPDATE glo_folio
      SET status = 0 -- integrado
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio  = p_folio;                     
   
   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 2
   UPDATE glo_ctr_archivo 
      SET estado = 2  
    WHERE proceso_cod = p_proceso_cod
      AND estado = 3
      AND folio = p_folio;
      
   --se actualiza el result operacion a 03
   UPDATE tia_det_traspaso
      SET result_operacion = '01'
    WHERE folio = p_folio
      AND result_operacion = '04';
   
   	DELETE FROM tia_preliquida 
   	 WHERE folio_liquida =  p_folio ;

END PROCEDURE ;


