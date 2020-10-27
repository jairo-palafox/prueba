






CREATE FUNCTION "safreviv".fn_finaliza_ret_amort_excedente(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT,p_marca SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE ret_amort_excedente_id_solicitud           DECIMAL(9,0) ;
 DEFINE ret_amort_excedente_id_derechohabiente     DECIMAL(9,0) ;

 DEFINE v_act_id_dpe_referencia         DECIMAL(9,0);
 DEFINE v_act_folio                     DECIMAL(9,0);
 DEFINE v_act_reg_patronal_imss         CHAR(11);
 DEFINE v_act_periodo_pago              CHAR(6);
 DEFINE v_act_id_derechohabiente        DECIMAL(9,0);
 DEFINE v_dpe_diagnostico               SMALLINT;
 DEFINE v_act_total_avis_viv_dev        DECIMAL(16,6);
 DEFINE v_act_total_imp_viv_dev         DECIMAL(16,6);
 DEFINE v_act_acumula_avis_viv_dev      DECIMAL(16,6);
 DEFINE v_act_acumula_imp_viv_dev       DECIMAL(16,6);

 DEFINE v_si_resultado                     SMALLINT;

 -- para marcar las cuentas
 DEFINE v_i_estado_marca                          INTEGER;
 DEFINE v_marca_amort_excedente              INTEGER; -- 805 de acuerdo a catalogo


 -- Control de Excepciones
 DEFINE sql_err  INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_amort_excedente.txt";

   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = '1';

   --TRACE("Desmarcando cuentas liquidadas");

   LET v_marca_amort_excedente = p_marca; --805; -- marca para amort_excedente de recursos
   LET v_i_estado_marca    = 0;

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_amort_excedente
   SET    estado_solicitud = 60 -- liquidada
   WHERE  estado_solicitud = 50 -- preliquidada
   AND    folio            = p_folio;
   
   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 60
   WHERE  estado_solicitud = 50
   AND    folio            = p_folio
   AND    modalidad_retiro = 9;

   -- actualizar la tabla dae_det_solicitud
   -- 28NOV2013
      -- Se comenta esta actualizacion porque posteriormente se llama a la funcion 
      -- fn_dae_actualiza_status_retiro
--   FOREACH
--   SELECT id_derechohabiente
--   INTO   ret_amort_excedente_id_derechohabiente
--   FROM   ret_solicitud_generico
--   WHERE  folio = p_folio
--   AND    estado_solicitud = 60
--   AND    modalidad_retiro = 9
   
      -- se actualiza el registro a saldo retirado

--      UPDATE dae_det_solicitud
--      SET    status_retiro      = 1
--      WHERE  id_derechohabiente = ret_amort_excedente_id_derechohabiente;
   
--   END FOREACH;

   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso terminado exitosamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


