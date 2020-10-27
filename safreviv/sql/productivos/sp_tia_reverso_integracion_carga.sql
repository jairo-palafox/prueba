






CREATE PROCEDURE "safreviv".sp_tia_reverso_integracion_carga( 
                 p_folio               DECIMAL(9,0),
                 p_pid                 DECIMAL(9,0),
                 p_proceso_cod         SMALLINT, 
                 p_opera_cod_carga     SMALLINT,
                 p_usuario_cod         CHAR(20)                                             
                 );
   -- Control de Excepciones
   DEFINE sql_err                INTEGER;
   DEFINE isam_err               INTEGER;
   DEFINE err_txt                CHAR(200);
   DEFINE v_c_msj                CHAR(200);
   DEFINE v_si_resultado         SMALLINT;
   DEFINE v_nom_archivo          VARCHAR(100);
   DEFINE v_estatus              SMALLINT ;
   DEFINE v_contador_excepcion   INTEGER ;

  -- LET v_contador_excepcion = 0 ;
   --manejo de excepciones
  --ON EXCEPTION SET sql_err, isam_err, err_txt
  --    LET v_si_resultado = sql_err;
  --    
  --    RETURN v_si_resultado, isam_err, err_txt;
  --END EXCEPTION
  
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_tia_reverso_integracion_carga.txt';
   --TRACE 'Inicia Reverso integracion con Folio:' || p_folio ||" - Fecha:" || TODAY;

   --se actualiza el status del archivo segun folio
   UPDATE glo_folio
      SET status = -1 --3 --REVERSADO
    WHERE folio        = p_folio
      AND proceso_cod  = p_proceso_cod ;

   --se eliminan los registros del encabezado de la tabla tia_cza_traspaso
   DELETE FROM tia_cza_traspaso
    WHERE folio = p_folio;

   -- se eliminan los registros de la tabal tia_det_traspaso
   DELETE FROM tia_det_traspaso
    WHERE folio = p_folio;

    --se eliminan los registros del sumario de la tbala tia_sum_traspaso 
    DELETE FROM tia_sum_traspaso
    WHERE folio = p_folio;

    -- se eliminan los registros en tia_excep_traspaso
    DELETE FROM tia_excep_traspaso
    WHERE folio = p_folio;

END PROCEDURE ;


