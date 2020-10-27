






CREATE FUNCTION "safreviv".fn_dpe_preliquida(p_usuario_cod CHAR(20),
                                  p_folio       DECIMAL(9,0),
                                  p_pid         DECIMAL (9,0))
   RETURNING INTEGER,
             INTEGER,
             CHAR(200),
             INTEGER

-- tabla de preliquidacion
DEFINE v_preliq_f_liquida            DATE                   ;
DEFINE v_preliq_id_derechohabiente   DECIMAL(9)             ;
DEFINE v_preliq_subcuenta            SMALLINT               ;
DEFINE v_preliq_fondo_inversion      SMALLINT               ;
DEFINE v_preliq_movimiento           SMALLINT               ;
DEFINE v_preliq_folio_liquida        DECIMAL(9,0)           ;
DEFINE v_preliq_id_referencia        DECIMAL(9,0)           ;
DEFINE v_preliq_monto_acciones       DECIMAL(16,6)          ;
DEFINE v_preliq_monto_pesos          DECIMAL(16,6)          ;
DEFINE v_preliq_f_valor              DATE                   ;
DEFINE v_preliq_f_registro           DATE                   ;
DEFINE v_preliq_h_registro           DATETIME HOUR TO SECOND;
DEFINE v_origen_preliquida           CHAR(18)               ;
-- detalle en safre_viv
 DEFINE v_dpe_id_dpe_referencia     DECIMAL(9,0) ;
 DEFINE v_dpe_folio                 DECIMAL(9,0) ;
 DEFINE v_dpe_id_derechohabiente    DECIMAL(9,0)   ;
 DEFINE v_dpe_nombre_trabajador     CHAR(50)     ;
 DEFINE v_dpe_total_pagar_avis_viv_dev          DECIMAL(16,6);
 DEFINE v_dpe_imp_viv_dev           DECIMAL(12,2);
 DEFINE v_dpe_reg_patronal_imss     CHAR(11);
 DEFINE v_dpe_periodo_pago          CHAR(6);
 DEFINE v_dpe_diagnostico           SMALLINT;

 DEFINE v_pagado_imp_viv_dev        DECIMAL(16,6);
 DEFINE v_total_pagado_avis_viv_dev DECIMAL(16,6);
 DEFINE v_d_saldo_disponible_aivs   DECIMAL(16,6);
 DEFINE v_d_saldo_calculado_aivs    DECIMAL(16,6);
 DEFINE v_id_dpe_patron             DECIMAL(16,6);
 -- variables de soporte al proceso
 DEFINE v_b_exito                       SMALLINT; -- booleana para indicar si el proceso termino bien
 DEFINE v_id_derechohabiente            DECIMAL(9,0); -- ID de derechohabiente asociado a un NSS

 -- subcuenta y
 DEFINE v_subcuenta_tmp                 SMALLINT; -- subcuenta temporal
 DEFINE v_fondo_inversion_tmp           SMALLINT; -- fondo temporal
 DEFINE v_movimiento_tmp                SMALLINT; -- clave de movimiento temporal
 DEFINE v_si_procesa_insercion          SMALLINT;
 --
 DEFINE v_dte_fecha_hoy                 DATE;
 DEFINE v_si_estado                     SMALLINT;
 DEFINE v_f_valor_viv                   DATE;

 DEFINE v_act_total_avis_viv_dev        DECIMAL(16,6);
 DEFINE v_act_total_imp_viv_dev         DECIMAL(16,6);
 DEFINE v_act_acumula_avis_viv_dev      DECIMAL(16,6);
 DEFINE v_act_acumula_imp_viv_dev       DECIMAL(16,6);
 DEFINE v_si_marca_imsss                SMALLINT;
 DEFINE v_resultado                     SMALLINT;

 DEFINE v_si_total_pagar                SMALLINT;

 -- Control de Excepciones
 DEFINE sql_err                  INTEGER;
 DEFINE v_i_resultado            INTEGER;
 DEFINE isam_err                 INTEGER;
 DEFINE v_isam_err               INTEGER;
 DEFINE err_txt                  CHAR(200);
 DEFINE v_i_registros_insertados INTEGER;

 ON EXCEPTION SET sql_err, isam_err, err_txt
    LET v_i_resultado = sql_err;
    LET v_isam_err = isam_err;

    RETURN v_i_resultado,
           v_isam_err,
           err_txt,
           v_i_registros_insertados;
 END EXCEPTION

 --SET DEBUG FILE TO "/safreviv_int/dpe/envio/trace.dpe.preliquida.txt";
 --TRACE ON;

 LET v_i_resultado            = 0;
 LET v_si_procesa_insercion   = 0;
 LET v_i_registros_insertados = 0;
 LET v_preliq_monto_acciones  = 0;
 LET v_preliq_monto_pesos     = 0;
 LET err_txt                  = "Ejecución exitosa";
 LET v_isam_err               = 0;
 LET v_dte_fecha_hoy = TODAY;

   -- se obtienen todos los registros de la tabla temporal
   FOREACH
      SELECT a.id_dpe_referencia
            ,a.folio
            ,a.id_derechohabiente
            ,p.imp_viv_dev
            ,p.aivs_viv_dev
            ,a.reg_patronal_imss
            ,a.periodo_pago
            ,a.id_dpe_patron
      INTO  v_dpe_id_dpe_referencia
            ,v_dpe_folio
            ,v_dpe_id_derechohabiente
            ,v_dpe_imp_viv_dev
            ,v_dpe_total_pagar_avis_viv_dev
            ,v_dpe_reg_patronal_imss
            ,v_dpe_periodo_pago
            ,v_id_dpe_patron
      FROM  dpe_sol_trabajador a,
            dpe_resp_procesar p
      WHERE a.estado_solicitud = 1
      AND   p.resul_op IN (1,4)
      AND   a.id_dpe_referencia= p.id_dpe_referencia
      AND   a.folio_respuesta  = p.folio
      AND   a.reg_patronal_imss= p.reg_patronal_imss
      AND   a.periodo_pago     = p.periodo_pago
      AND   p.folio            = p_folio
      AND   a.folio_respuesta  IS NOT NULL
      AND   a.folio_liquida    IS NULL


      LET v_subcuenta_tmp       =  4;
      LET v_fondo_inversion_tmp = 11;

      -- El movimiento es:
      -- 342	ABONO POR DEVOLUCIÓN DE PAGOS DE INDEBIDOS O EN EXCESO
      LET v_movimiento_tmp = 342;

      -- Se transfieren los datos al registro de
      LET v_preliq_f_liquida          = TODAY;
      LET v_preliq_id_derechohabiente = v_dpe_id_derechohabiente;
      LET v_preliq_subcuenta          = v_subcuenta_tmp         ;
      LET v_preliq_fondo_inversion    = v_fondo_inversion_tmp   ;
      LET v_preliq_movimiento         = v_movimiento_tmp        ;
      LET v_preliq_folio_liquida      = v_dpe_folio             ;
      LET v_preliq_id_referencia      = v_dpe_id_dpe_referencia ;

      -- Total calculado
      LET v_preliq_monto_acciones     = v_dpe_total_pagar_avis_viv_dev*-1;
      LET v_preliq_monto_pesos        = v_dpe_imp_viv_dev *-1;

      SELECT f_valor_viv
      INTO   v_f_valor_viv
      FROM   dpe_patron
      WHERE  id_dpe_referencia = v_id_dpe_patron;

      LET v_preliq_f_valor            = v_f_valor_viv ;
      LET v_preliq_f_registro         = TODAY                   ;
      LET v_preliq_h_registro         = CURRENT HOUR TO SECOND  ;
      -- Se agrega el origen concatenando periodo de pago - NRP
      LET v_origen_preliquida         = v_dpe_periodo_pago||'-'||v_dpe_reg_patronal_imss;

      -- se insertan en la tabla de preliquidacion
      INSERT INTO dpe_preliquida (
                                  f_liquida         ,
                                  id_derechohabiente,
                                  subcuenta         ,
                                  fondo_inversion   ,
                                  movimiento        ,
                                  folio_liquida     ,
                                  id_referencia     ,
                                  monto_acciones    ,
                                  monto_pesos       ,
                                  f_valor           ,
                                  f_registro        ,
                                  h_registro        ,
                                  origen
                                  )
      VALUES( v_preliq_f_liquida         ,
              v_preliq_id_derechohabiente,
              v_preliq_subcuenta         ,
              v_preliq_fondo_inversion   ,
              v_preliq_movimiento        ,
              p_folio            ,
              v_preliq_id_referencia     ,
              v_preliq_monto_acciones    ,
              v_preliq_monto_pesos       ,
              v_preliq_f_valor           ,
              v_preliq_f_registro        ,
              v_preliq_h_registro        ,
              v_origen_preliquida
             );

      UPDATE dpe_sol_trabajador
      SET    diagnostico        = 4,
             folio_liquida      = p_folio
      WHERE  id_dpe_referencia  = v_dpe_id_dpe_referencia;

      LET v_f_valor_viv = NULL;
      -- [Actualiza total de registros insertados mayor a cero]
      LET v_i_registros_insertados = v_i_registros_insertados + 1;
   END FOREACH;

   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio
   AND    proceso_cod = 1006;


   UPDATE bat_ctr_operacion
   SET    folio       = p_folio
   WHERE  pid         = p_pid
   AND    proceso_cod = 1006
   AND    opera_cod   = 3;

   UPDATE STATISTICS FOR TABLE dpe_preliquida;

    RETURN v_i_resultado,
           v_isam_err,
           err_txt,
           v_i_registros_insertados;
END FUNCTION;


