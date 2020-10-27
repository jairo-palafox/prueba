






CREATE FUNCTION "safreviv".fn_finaliza_disposicion(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT,p_marca SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE ret_disposicion_id_solicitud           DECIMAL(9,0) ;
 DEFINE ret_disposicion_id_derechohabiente     DECIMAL(9,0) ;

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
 DEFINE v_marca_disposicion              INTEGER; -- 805 de acuerdo a catalogo


 -- Control de Excepciones
 DEFINE sql_err INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);

     ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

 --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_disposicion.txt";

 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = '1';

 --TRACE("Desmarcando cuentas liquidadas");

   LET v_marca_disposicion = p_marca; --805; -- marca para disposicion de recursos
   LET v_i_estado_marca    = 0;

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_disposicion
      SET estado_solicitud = 60 -- liquidada
    WHERE estado_solicitud = 50 -- preliquidada
      AND folio            = p_folio;



      -- se desmarcan todas las cuentas porque el archivo sera rechazado
      FOREACH
      SELECT
         id_solicitud          ,
         id_derechohabiente
      INTO
         ret_disposicion_id_solicitud      ,
         ret_disposicion_id_derechohabiente
      FROM  ret_disposicion
      WHERE folio = p_folio
        AND estado_solicitud = 60 -- liquidadas

         -- se desmarca la cuenta en cuestion
      -- se marca la cuenta
      LET v_i_estado_marca = 0;
      EXECUTE FUNCTION fn_desmarca_cuenta(
              ret_disposicion_id_derechohabiente
             ,v_marca_disposicion -- marca de disposicion
             ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
             ,0 -- estado marca
             ,0 -- marca de la causa
             ,p_usuario_cod
             ,p_proceso_cod) -- disposicion
         INTO v_i_estado_marca;

      END FOREACH;


 --TRACE("termina proceso");
 LET v_c_msj = 'proceso terminado exitosamente';

 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


