






CREATE FUNCTION "safreviv".fn_ret_preliquida_his_ley73(v_folio_liquida    DECIMAL(10,0),
                                            v_proceso_cod      SMALLINT,
                                            v_opera_cod        SMALLINT,
                                            v_usuario_cod      VARCHAR(20),
                                            v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

DEFINE  v_b_paso               SMALLINT;
DEFINE  v_id_derechohabiente   DECIMAL(9,0);
DEFINE  v_id_solicitud         DECIMAL(9,0);
DEFINE  v_movimiento_abono     SMALLINT;
DEFINE  v_movimiento_cargo     SMALLINT;
DEFINE  v_valor_mov            SMALLINT;
DEFINE  v_origen               char(20);
DEFINE  v_subcuenta            SMALLINT;
DEFINE  v_subcuenta_97         SMALLINT;

-- para calcular el saldo del trabajador
DEFINE  v_resultado_consulta   SMALLINT;
DEFINE  v_saldo_92_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_92_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92
DEFINE  v_saldo_97_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_97_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92
DEFINE  v_fondo                SMALLINT; -- fondo de inversion
DEFINE  v_monto_acciones       DECIMAL(18,6); -- acciones
DEFINE  v_monto_pesos          DECIMAL(20,2); -- pesos
DEFINE  v_diferencia_pesos     DECIMAL(14,2);
DEFINE  v_diferencia_aivs      DECIMAL(18,6);

-- ret_his_anexo1
DEFINE ret_his_anexo1_id_solicitud              decimal(9,0) ;
DEFINE ret_his_anexo1_estado_solicitud          smallint     ;
DEFINE ret_his_anexo1_cod_rechazo               smallint     ;
DEFINE ret_his_anexo1_estado_transferencia      CHAR(1)      ;
DEFINE ret_his_anexo1_tpo_pensionado            CHAR(1)      ;
DEFINE ret_his_anexo1_estado_demanda_amparo     CHAR(1)      ;
DEFINE ret_his_anexo1_origen_entrega            CHAR(1)      ;
DEFINE ret_his_anexo1_mecanismo_entrega         CHAR(1)      ;
DEFINE ret_his_anexo1_folio                     decimal(9,0) ;
DEFINE ret_his_anexo1_id_derechohabiente        decimal(9,0) ;
DEFINE ret_his_anexo1_nss                       char(11)     ;
DEFINE ret_his_anexo1_curp                      char(18)     ;
DEFINE ret_his_anexo1_ap_paterno_af             char(40)     ;
DEFINE ret_his_anexo1_ap_materno_af             char(40)     ;
DEFINE ret_his_anexo1_nombre_af                 char(40)     ;
DEFINE ret_his_anexo1_folio_pago                CHAR(4)      ;
DEFINE ret_his_anexo1_pesos_viv97_transf        decimal(13,2);
DEFINE ret_his_anexo1_pesos_viv97_no_transf     decimal(13,2);
DEFINE ret_his_anexo1_f_transferencia           date         ;
DEFINE ret_his_anexo1_num_juicio                CHAR(5)      ;
DEFINE ret_his_anexo1_ano_juicio                CHAR(4)      ;
DEFINE ret_his_anexo1_juzgado                   char(100)    ;
DEFINE ret_his_anexo1_num_expediente            CHAR(7)      ;
DEFINE ret_his_anexo1_ap_paterno_benef          char(40)     ;
DEFINE ret_his_anexo1_ap_materno_benef          char(40)     ;
DEFINE ret_his_anexo1_nombre_benef              char(40)     ;
DEFINE ret_his_anexo1_num_oficio_infonavit      char(25)     ;
DEFINE ret_his_anexo1_f_entrega_recursos_tesofe date         ;
DEFINE ret_his_anexo1_f_entrega_recursos_af     date         ;
DEFINE ret_his_anexo1_saldo_pagado_viv97        decimal(13,2);
DEFINE ret_his_anexo1_banco_receptor            CHAR(2)      ;
DEFINE ret_his_anexo1_num_cuenta                CHAR(16)     ;
DEFINE ret_his_anexo1_clabe                     CHAR(18)     ;

DEFINE v_i_estado_marca        INTEGER;
DEFINE v_marca_ley73           INTEGER; -- 803 de acuerdo a catalogo
DEFINE v_bnd_preli             SMALLINT;

DEFINE r_pes_viv97             DECIMAL(14,2);
DEFINE r_pes_viv92             DECIMAL(14,2);

DEFINE r_aivs_viv92            DECIMAL(18,6);
DEFINE r_aivs_viv97            DECIMAL(18,6);

-- Control de Excepciones
DEFINE v_si_resultado          SMALLINT;
DEFINE sql_err                 INTEGER;
DEFINE isam_err                INTEGER;
DEFINE err_txt                 VARCHAR(250);
DEFINE v_c_msj                 VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, ret_his_anexo1_id_solicitud;
   END EXCEPTION

   -- se actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status      =  1
   WHERE  folio       = v_folio_liquida;

   -- actualiza folio en la operacion y proceso
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;

   -- se inician las variables para marca
   LET v_marca_ley73               = 803; -- marca de retiro ley73
   LET v_i_estado_marca            = 0;
   LET v_bnd_preli                 = 0;
   LET v_b_paso                    = 0; 
   LET v_id_derechohabiente        = 0;
   LET ret_his_anexo1_id_solicitud = 0;
   LET v_movimiento_abono          = 651; -- CARGA INICIAL LEY 73 ANEXO 1
   LET v_origen                    = "ANEXO 1";
   LET v_subcuenta                 = 47; -- TESOFE
   LET v_subcuenta_97              = 4;
   LET r_pes_viv97                 = 0;
   LET r_pes_viv92                 = 0;
   LET r_aivs_viv92                = 0;
   LET r_aivs_viv97                = 0;
   LET v_c_msj                     = "El proceso de preliquidación finalizó correctamente.";
   LET isam_err                    = 0;
   LET v_si_resultado              = 0;
   
   -- se inician las variables para calculo de saldo
   LET v_resultado_consulta = 0;  
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_97_pesos     = 0;
   LET v_saldo_92_aivs      = 0;
   LET v_saldo_92_pesos     = 0;
   LET v_fondo              = 10; -- FONDO cuya aiv cuesta 1 peso siempre
   LET v_monto_acciones     = 0;
   LET v_monto_pesos        = 0;
   LET v_diferencia_pesos   = 0;
   LET v_diferencia_aivs    = 0;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_ley73.log';
   
   -- se obtiene el signo del movimiento de retiro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono;

   -- busca registros en estatus de capturado
   FOREACH 
   SELECT 
      id_solicitud              ,
      estado_solicitud          ,
      cod_rechazo               ,
      estado_transferencia      ,
      tpo_pensionado            ,
      estado_demanda_amparo     ,
      origen_entrega            ,
      mecanismo_entrega         ,
      folio                     ,
      id_derechohabiente        ,
      nss                       ,
      curp                      ,
      ap_paterno_af             ,
      ap_materno_af             ,
      nombre_af                 ,
      folio_pago                ,
      pesos_viv97_transf        ,
      pesos_viv97_no_transf     ,
      f_transferencia           ,
      num_juicio                ,
      ano_juicio                ,
      juzgado                   ,
      num_expediente            ,
      ap_paterno_benef          ,
      ap_materno_benef          ,
      nombre_benef              ,
      num_oficio_infonavit      ,
      f_entrega_recursos_tesofe ,
      f_entrega_recursos_af     ,
      saldo_pagado_viv97        ,
      banco_receptor            ,
      num_cuenta                ,
      clabe                     
   INTO 
       ret_his_anexo1_id_solicitud              ,
       ret_his_anexo1_estado_solicitud          ,
       ret_his_anexo1_cod_rechazo               ,
       ret_his_anexo1_estado_transferencia      ,
       ret_his_anexo1_tpo_pensionado            ,
       ret_his_anexo1_estado_demanda_amparo     ,
       ret_his_anexo1_origen_entrega            ,
       ret_his_anexo1_mecanismo_entrega         ,
       ret_his_anexo1_folio                     ,
       ret_his_anexo1_id_derechohabiente        ,
       ret_his_anexo1_nss                       ,
       ret_his_anexo1_curp                      ,
       ret_his_anexo1_ap_paterno_af             ,
       ret_his_anexo1_ap_materno_af             ,
       ret_his_anexo1_nombre_af                 ,
       ret_his_anexo1_folio_pago                ,
       ret_his_anexo1_pesos_viv97_transf        ,
       ret_his_anexo1_pesos_viv97_no_transf     ,
       ret_his_anexo1_f_transferencia           ,
       ret_his_anexo1_num_juicio                ,
       ret_his_anexo1_ano_juicio                ,
       ret_his_anexo1_juzgado                   ,
       ret_his_anexo1_num_expediente            ,
       ret_his_anexo1_ap_paterno_benef          ,
       ret_his_anexo1_ap_materno_benef          ,
       ret_his_anexo1_nombre_benef              ,
       ret_his_anexo1_num_oficio_infonavit      ,
       ret_his_anexo1_f_entrega_recursos_tesofe ,
       ret_his_anexo1_f_entrega_recursos_af     ,
       ret_his_anexo1_saldo_pagado_viv97        ,
       ret_his_anexo1_banco_receptor            ,
       ret_his_anexo1_num_cuenta                ,
       ret_his_anexo1_clabe                     
   FROM  ret_his_anexo1
   WHERE estado_solicitud = 10 -- aceptados
   AND   folio            = v_folio_liquida

      -- si tiene saldo a favor
	  IF ( ret_his_anexo1_pesos_viv97_transf IS NOT NULL AND ret_his_anexo1_pesos_viv97_transf > 0 ) OR
         ( ret_his_anexo1_pesos_viv97_no_transf IS NOT NULL AND ret_his_anexo1_pesos_viv97_no_transf > 0 ) THEN
   
         -- se calcula el saldo no transferido a la tesofe para que entre como abono
         LET v_monto_acciones     = (ret_his_anexo1_pesos_viv97_transf + ret_his_anexo1_pesos_viv97_no_transf) * v_valor_mov;
         LET v_monto_pesos        = (ret_his_anexo1_pesos_viv97_transf + ret_his_anexo1_pesos_viv97_no_transf) * v_valor_mov;
         
         -- se inserta el monto en la tabla de preliquidacion
         INSERT INTO ret_preliquida (
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
            origen            )
         VALUES (
            TODAY                            ,
            ret_his_anexo1_id_derechohabiente,
            v_subcuenta                      ,
            v_fondo                          ,
            v_movimiento_abono               ,
            v_folio_liquida                  ,
            ret_his_anexo1_id_solicitud      ,
            v_monto_acciones                 ,
            v_monto_pesos                    ,
            TODAY                            ,
            TODAY                            ,
            CURRENT HOUR TO SECOND           ,
            v_origen
         );
		 
         -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
         LET v_bnd_preli = 1;
	  END IF

      -- se actualica la solicitud a estado liquidado y se le asigna el folio
      UPDATE ret_his_anexo1
      SET    estado_solicitud   = 50
      WHERE  id_derechohabiente = ret_his_anexo1_id_derechohabiente
      AND    id_solicitud       = ret_his_anexo1_id_solicitud
      AND    folio              = v_folio_liquida
      AND    estado_solicitud   = 10;

   END FOREACH;

   -- si no se preliquidaron registros
   IF ( v_bnd_preli = 0 ) THEN
      -- se marca el procesoe en error
      LET v_si_resultado = 1000;
      LET isam_err       = 0;
      LET v_c_msj        = "Error. No se preliquidaron solicitudes para el folio.";
   END IF;

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, ret_his_anexo1_id_solicitud;
END FUNCTION;


