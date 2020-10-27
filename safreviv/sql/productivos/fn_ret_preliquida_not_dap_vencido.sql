






CREATE FUNCTION "safreviv".fn_ret_preliquida_not_dap_vencido(v_folio_liquida    DECIMAL(10,0),
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
DEFINE  v_importe_actualizado  DECIMAL(20,2); -- pesos
DEFINE  v_acciones_actualizadas DECIMAL(18,6); -- pesos
DEFINE  v_diferencia_pesos     DECIMAL(14,2);
DEFINE  v_diferencia_aivs      DECIMAL(18,6);
DEFINE  v_precio_dia           DECIMAL(18,6);
DEFINE  v_precio_dia_liquidacion DECIMAL(18,6);

-- ret_not_dap_vencido
DEFINE ret_not_dap_vencido_id_solicitud               DECIMAL(9,0)     ;
DEFINE ret_not_dap_vencido_folio                      DECIMAL(9,0)    ;
DEFINE ret_not_dap_vencido_num_docto                  CHAR(10)      ;
DEFINE ret_not_dap_vencido_posicion                   CHAR(3)     ;
DEFINE ret_not_dap_vencido_anio                       CHAR(4)     ;
DEFINE ret_not_dap_vencido_division                   CHAR(4)     ;
DEFINE ret_not_dap_vencido_descripcion                CHAR(8)     ;
DEFINE ret_not_dap_vencido_f_liquidacion              DATE     ;
DEFINE ret_not_dap_vencido_usuario                    CHAR(8)      ;
DEFINE ret_not_dap_vencido_referencia                 CHAR(11)      ;
DEFINE ret_not_dap_vencido_txt_docto                  CHAR(25);
DEFINE ret_not_dap_vencido_acreedor                   CHAR(8);
DEFINE ret_not_dap_vencido_nombre                     CHAR(33)         ;
DEFINE ret_not_dap_vencido_vp                         CHAR(2)      ;
DEFINE ret_not_dap_vencido_bp                         CHAR(2)      ;
DEFINE ret_not_dap_vencido_importe                    DECIMAL(10,2)      ;
DEFINE ret_not_dap_vencido_moneda                     CHAR(4)      ;
DEFINE ret_not_dap_vencido_anulacion                  CHAR(10)    ;
DEFINE ret_not_dap_vencido_anio_dos                   CHAR(4)      ;
DEFINE ret_not_dap_vencido_doc_comp                   CHAR(10)     ;
DEFINE ret_not_dap_vencido_anio_tres                  CHAR(4)     ;
DEFINE ret_not_dap_vencido_estatus                    CHAR(7)     ;
DEFINE ret_not_dap_vencido_num_cta                    CHAR(16)     ;
DEFINE ret_not_dap_vencido_f_conta1                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta2                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta3                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta4                   DATE         ;
DEFINE ret_not_dap_vencido_estado_solicitud           SMALLINT     ;

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
      
      RETURN v_si_resultado, isam_err, err_txt, ret_not_dap_vencido_id_solicitud;
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
   LET v_i_estado_marca            = 0;
   LET v_bnd_preli                 = 0;
   LET v_b_paso                    = 0; 
   LET v_id_derechohabiente        = 0;
   LET ret_not_dap_vencido_id_solicitud = 0;
   LET v_movimiento_abono          = 131; -- CARGA INICIAL LEY 73 ANEXO 1
   LET v_origen                    = "DAP Vencido";
   LET v_subcuenta                 = 44; -- Vivienda 97 solo Infonavit
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
   LET v_fondo              = 11; -- FONDO cuya aiv cuesta 1 peso siempre
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

   -- se obtiene el precio del dia 
   SELECT precio_fondo
   INTO   v_precio_dia
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = today;

   IF (v_precio_dia IS NULL OR v_precio_dia = 0) THEN
       LET v_si_resultado = 1002;
       LET isam_err       = 0;
       LET v_c_msj        = "Error. No hay precio para liquidar.";
       RETURN v_si_resultado, isam_err, v_c_msj, 0;
   END IF  
   -- busca registros en estatus de capturado
   FOREACH 
   SELECT 
       id_solicitud              ,
       estado_solicitud          ,
       folio                     ,
       referencia                ,
       f_liquidacion             ,
       importe
   INTO 
       ret_not_dap_vencido_id_solicitud              ,
       ret_not_dap_vencido_estado_solicitud          ,
       ret_not_dap_vencido_folio                     ,
       ret_not_dap_vencido_referencia                ,
       ret_not_dap_vencido_f_liquidacion             ,
       ret_not_dap_vencido_importe
   FROM  ret_not_dap_vencido
   WHERE estado_solicitud =  20 -- aceptados
   AND   estatus          in (3, 4, 23, 25, 26)
   AND   folio            =  v_folio_liquida

   
      -- si tiene saldo a favor
	  IF ( ret_not_dap_vencido_importe IS NOT NULL AND ret_not_dap_vencido_importe > 0 ) THEN
           -- se obtiene el precio del dia de la liquidacion 
         SELECT precio_fondo
         INTO   v_precio_dia_liquidacion
         FROM   glo_valor_fondo
         WHERE  fondo = 11
         AND    f_valuacion = ret_not_dap_vencido_f_liquidacion;

         LET v_id_derechohabiente = 0;
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = ret_not_dap_vencido_referencia;

         IF (v_id_derechohabiente = 0) THEN
             LET v_si_resultado = 1001;
             LET isam_err       = 0;
             LET v_c_msj        = "Error. No se encontro el id derechohabiente para el NSS. >", ret_not_dap_vencido_referencia, "<";
             RETURN v_si_resultado, isam_err, v_c_msj, ret_not_dap_vencido_referencia; 
         END IF 
         -- se calcula el saldo no transferido a la tesofe para que entre como abono
         
         LET v_monto_acciones      = ret_not_dap_vencido_importe / v_precio_dia_liquidacion;
         LET v_importe_actualizado = v_monto_acciones * v_precio_dia;
         
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
            TODAY                             ,
            v_id_derechohabiente              ,
            v_subcuenta                       ,
            v_fondo                           ,
            v_movimiento_abono                ,
            v_folio_liquida                   ,
            ret_not_dap_vencido_id_solicitud  ,
            v_monto_acciones                  ,
            ret_not_dap_vencido_importe       ,
            ret_not_dap_vencido_f_liquidacion ,
            TODAY                             ,
            CURRENT HOUR TO SECOND            ,
            v_origen
         );
		 
         -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
         LET v_bnd_preli = 1;
	  END IF

      -- se actualica la solicitud a estado liquidado y se le asigna el folio
      UPDATE ret_not_dap_vencido
      SET    estado_solicitud   = 50
      WHERE  id_solicitud       = ret_not_dap_vencido_id_solicitud
      AND    folio              = v_folio_liquida
      AND    estado_solicitud   = 20;

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
   RETURN v_si_resultado, isam_err, v_c_msj, ret_not_dap_vencido_id_solicitud;
END FUNCTION;


