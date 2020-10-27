






CREATE FUNCTION "safreviv".fn_finaliza_ret_ley73_ws(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT,p_marca SMALLINT)
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
DEFINE v_marca_ley73              INTEGER; -- 803 marca de ley 73


-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_ret_ley73_ws.txt";

   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = '1';

   LET v_marca_ley73 = p_marca; -- 803 ley 73
   LET v_i_estado_marca    = 0;

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_ley73_generico
   SET    estado_solicitud = 60 -- liquidada
   WHERE  estado_solicitud = 50 -- preliquidada
   AND    folio            = p_folio;

   -- Actualiza el estado a los beneficiarios
   UPDATE ret_beneficiario_juridico
   SET    estado_solicitud = 60
   WHERE  estado_solicitud = 50
   AND    id_solicitud IN (SELECT id_solicitud 
                           FROM   ret_solicitud_generico 
                           WHERE  folio            = p_folio
                           AND    modalidad_retiro = 3
                           AND    estado_solicitud = 50);

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 60
   WHERE  estado_solicitud = 50
   AND    folio            = p_folio
   AND    modalidad_retiro = 3;


   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso terminado exitosamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


