






CREATE FUNCTION "safreviv".fn_finaliza_fc(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT) 
       RETURNING SMALLINT, INTEGER, VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE ret_fc_id_solicitud           DECIMAL(9,0) ;
 DEFINE ret_fc_id_derechohabiente     DECIMAL(9,0) ;
 
 -- para marcar las cuentas
 DEFINE v_i_estado_marca              INTEGER;
 DEFINE v_marca_fc                    INTEGER; -- 808 de acuerdo a catalogo
 
 -- Control de Excepciones
 DEFINE v_si_resultado                SMALLINT;
 DEFINE sql_err  INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);


   -- se establece el comportamiento al aparecer alguna excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_fc.txt";
 
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso finalizó correctamente';

 --TRACE("Desmarcando cuentas liquidadas");
 
   LET v_marca_fc       = 807; -- marca para fortalecimiento al credito
   LET v_i_estado_marca = 0;

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_fortalecimiento_credito
      SET estado_solicitud = 60 -- liquidada
    WHERE estado_solicitud = 50 -- preliquidada
      AND folio            = p_folio;

   -- se desmarcan todas las cuentas
   FOREACH
   SELECT
      id_solicitud       ,
      id_derechohabiente    
   INTO
      ret_fc_id_solicitud      ,
      ret_fc_id_derechohabiente    
   FROM  ret_fortalecimiento_credito
   WHERE folio = p_folio
     AND estado_solicitud = 60 -- liquidadas
  
      -- se desmarca la cuenta en cuestion
      LET v_i_estado_marca = 0;
      EXECUTE FUNCTION fn_desmarca_cuenta(
              ret_fc_id_derechohabiente
             ,v_marca_fc -- marca de retiro fortalecimiento al credito
             ,ret_fc_id_solicitud -- identificador de registro de archivo o lote
             ,0 -- estado marca
             ,0 -- marca de la causa
             ,p_usuario_cod
             ,p_proceso_cod) -- retiro fortalecimiento al credito
         INTO v_i_estado_marca;
      
   END FOREACH;
 
 --TRACE("termina proceso");
 LET v_c_msj = 'El proceso finalizó correctamente';
 
 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION 
;


