






CREATE FUNCTION "safreviv".fn_dpe_posliquida(p_usuario_cod CHAR(20),
                                  p_folio_liquida DECIMAL(9,0),
                                  p_proceso_cod SMALLINT)
   RETURNING SMALLINT, INTEGER, CHAR(200)

 -- Tipos status de pagos
 DEFINE c_pago_total_nss                SMALLINT;
 DEFINE c_pago_parcial_nss              SMALLINT;
 DEFINE c_pago_por_preliquidar_total    SMALLINT;
 DEFINE c_pago_por_preliquidar_parcial  SMALLINT;
 DEFINE c_pago_preliquidado_total       SMALLINT;
 DEFINE c_pago_preliquidado_parcial     SMALLINT;
 DEFINE c_pago_liquidado_total          SMALLINT;
 DEFINE c_pago_liquidado_parcial        SMALLINT;
 DEFINE c_pago_enviado_procesar_total   SMALLINT;
 DEFINE c_pago_enviado_procesar_parcial SMALLINT;

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

 DEFINE v_si_marca_imsss                SMALLINT;
 DEFINE v_si_resultado                  SMALLINT;
 DEFINE v_folio_lote                    DECIMAL(9,0);

 -- Control de Excepciones
 DEFINE sql_err INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  CHAR(200);

    ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = -206;
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

 --SET DEBUG FILE TO "/safreviv_int/BD/trace.dpe.posliquida.txt";
 --trace on;

 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = '1';

 LET c_pago_total_nss                = 0;
 LET c_pago_parcial_nss              = 1;
 LET c_pago_por_preliquidar_total    = 2;
 LET c_pago_por_preliquidar_parcial  = 3;
 LET c_pago_preliquidado_total       = 4;
 LET c_pago_preliquidado_parcial     = 5;
 LET c_pago_liquidado_total          = 6;
 LET c_pago_liquidado_parcial        = 7;
 LET c_pago_enviado_procesar_total   = 8;
 LET c_pago_enviado_procesar_parcial = 9;
 LET v_folio_lote                    = 0;

 --trace("Verifica pagos devoluciones completas");

 LET v_c_msj = 'Previo a foreach para buscar datos con folio'||p_folio_liquida;
 
 FOREACH
    SELECT a.id_dpe_referencia
          ,a.id_derechohabiente
    INTO  v_act_id_dpe_referencia
          ,v_act_id_derechohabiente
    FROM  dpe_sol_trabajador a, 
          dpe_resp_procesar p
    WHERE a.estado_solicitud = 1
    AND   a.id_dpe_referencia = p.id_dpe_referencia
    AND   a.folio_respuesta   = p.folio
    AND   a.reg_patronal_imss = p.reg_patronal_imss
    AND   a.periodo_pago      = p.periodo_pago
    AND   a.folio_respuesta   IS NOT NULL
    AND   a.folio_liquida = p_folio_liquida
    AND   a.diagnostico = 4
    AND   a.resul_op = 1
    
    
    LET v_c_msj = 'Dentro de proceso foreach';
    LET v_c_msj = 'Se verifica si se pago completamente la solicitud';
    LET v_c_msj = 'Se pago completamente, se procede a desmarcar';

    -- # [indica que ya se cubri¦ el importe total a devolver]
    -- # [ y se requiere quitar la marca de la cuenta        ]
    LET v_si_marca_imsss = 401;

    EXECUTE FUNCTION fn_desmarca_cuenta( v_act_id_derechohabiente
                                        ,v_si_marca_imsss -- marca de imss
                                        ,v_act_id_dpe_referencia
                                        ,0
                                        ,0
                                        ,p_usuario_cod
                                        ,p_proceso_cod)
    INTO v_si_resultado;

    LET v_c_msj = 'Solicitud desmarcada';

    -- Pone a estado pagado de un parcial
    UPDATE dpe_sol_trabajador
    SET    diagnostico = c_pago_liquidado_total
    WHERE  id_dpe_referencia = v_act_id_dpe_referencia;

 END FOREACH;

 UPDATE glo_folio
 SET    status = 2
 WHERE  folio = p_folio_liquida;

 --trace("termina proceso");
 LET v_c_msj = 'proceso terminado exitosamente';

 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


