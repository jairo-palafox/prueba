






CREATE FUNCTION "safreviv".fn_deo_preliq_op98(p_folio DECIMAL(10), p_usuario_cod CHAR(20), p_pid DECIMAL(9,0)) RETURNING INTEGER, INTEGER, VARCHAR(255)
-- tabla de preliquidacion de op98
DEFINE v_preliq_f_liquida            DATE                   ;
DEFINE v_preliq_id_derechohabiente   DECIMAL(9)             ;
DEFINE v_preliq_subcuenta            SMALLINT               ;
DEFINE v_preliq_fondo_inversion      SMALLINT               ;
DEFINE v_preliq_movimiento           SMALLINT               ;
DEFINE v_preliq_folio_liquida        DECIMAL(10)            ;
DEFINE v_preliq_id_referencia        DECIMAL(10)            ;
DEFINE v_preliq_monto_acciones       DECIMAL(16,6)          ; -- cambio de precision
DEFINE v_preliq_monto_pesos          DECIMAL(12,2)          ; -- cambio de precision
DEFINE v_preliq_f_valor              DATE                   ;
DEFINE v_preliq_f_registro           DATE                   ;
DEFINE v_preliq_h_registro           DATETIME HOUR TO SECOND;
DEFINE v_preliq_usuario              CHAR(20)               ;
-- detalle Op98 en safre_viv
 DEFINE v_detop98_id_detalle            DECIMAL(9,0) ;
 DEFINE v_detop98_folio                 DECIMAL(9,0) ;
 DEFINE v_detop98_tipo_autocorreccion   SMALLINT     ;
 DEFINE v_detop98_cve_afore             SMALLINT     ;
 DEFINE v_detop98_id_derechohabiente    DECIMAL(9,0)   ;
 DEFINE v_detop98_rfc                   CHAR(13)     ;
 DEFINE v_detop98_curp                  CHAR(18)     ;
 DEFINE v_detop98_nombre_trabajador     CHAR(50)     ;
 DEFINE v_detop98_acc_devol_viv97       DECIMAL(16,6); -- cambio de precision
 DEFINE v_detop98_acc_devol_viv92       DECIMAL(16,6); -- cambio de precision
 DEFINE v_detop98_pes_devol_viv97       DECIMAL(12,2); -- cambio de precision
 DEFINE v_detop98_pes_devol_viv92       DECIMAL(12,2); -- cambio de precision
 DEFINE v_detop98_pes_int_devol_viv97   DECIMAL(12,2); -- cambio de precision
 DEFINE v_detop98_pes_int_devol_viv92   DECIMAL(12,2); -- cambio de precision
 DEFINE v_detop98_remanente             SMALLINT     ;
 DEFINE v_detop98_f_afectacion_contable DATE         ;
 DEFINE v_detop98_f_valor_recep_afore   DATE         ;
 DEFINE v_detop98_f_valor_devol_inf     DATE         ;
 DEFINE v_detop98_estado_devolucion     SMALLINT     ;
 DEFINE v_fecha_primer_dia_mes          DATE         ;
 DEFINE v_mes_valuacion                 SMALLINT     ;
 DEFINE v_texto_fecha_primer_dia_mes    VARCHAR(10)  ;

-- variables de soporte al proceso
DEFINE v_b_exito                       SMALLINT; -- booleana para indicar si el proceso termino bien
DEFINE v_id_derechohabiente            decimal(9); -- ID de derechohabiente asociado a un NSS
DEFINE v_i_resultado                   INTEGER; -- resultado de la operacion
-- subcuenta y
DEFINE v_subcuenta_tmp                 SMALLINT; -- subcuenta temporal
DEFINE v_fondo_inversion_tmp           SMALLINT; -- fondo temporal
DEFINE v_movimiento_tmp                SMALLINT; -- clave de movimiento temporal

-- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 
 -- si no se encuentra la tabla tmp_detalle_op98 entonces levantamos una excepcion
 -- The specified table (safre_tmp:tmp_detalle_op98) is not in the database
 ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_i_resultado = sql_err;
   
   RETURN v_i_resultado, isam_err, err_txt;
 END EXCEPTION

 -- se asume que el proceso termina correctamente
 LET v_i_resultado = 0;
 LET isam_err = 0;
 LET err_txt = "El proceso finalizó correctamente.";
 
 -- se calcula el mes de la valuacion
 IF ( MONTH(TODAY) = 12 ) THEN
    LET v_mes_valuacion = 1;
 ELSE
    LET v_mes_valuacion = MONTH(TODAY) + 1;
 END IF
 
 -- se construye la fecha del primer dia del mes
 IF ( v_mes_valuacion < 10 ) THEN
    LET v_texto_fecha_primer_dia_mes = "0" || v_mes_valuacion || "01" || YEAR(TODAY);
 ELSE
    LET v_texto_fecha_primer_dia_mes = v_mes_valuacion || "01" || YEAR(TODAY);
 END IF
 
 LET v_fecha_primer_dia_mes       = DATE(v_texto_fecha_primer_dia_mes);

 -- se obtienen todos los registros de la tabla temporal
 FOREACH --cur_tmp_detalle FOR
   SELECT
          a.id_detalle
         ,a.folio
         ,a.tpo_autocorreccion
         ,a.cve_afore
         ,a.id_derechohabiente
         ,a.rfc
         ,a.curp
         ,a.nombre_trabajador
         ,a.acc_devol_viv97
         ,a.acc_devol_viv92
         ,a.pes_devol_viv97
         ,a.pes_devol_viv92
         ,a.pes_int_devol_viv97
         ,a.pes_int_devol_viv92
         ,a.remanente
         ,a.f_afectacion_contable
         ,a.f_valor_recep_afore
         ,a.f_valor_devol_inf
         ,a.estado_devolucion
     INTO
         v_detop98_id_detalle            ,
         v_detop98_folio                 ,
         v_detop98_tipo_autocorreccion   ,
         v_detop98_cve_afore             ,
         v_detop98_id_derechohabiente    ,
         v_detop98_rfc                   ,
         v_detop98_curp                  ,
         v_detop98_nombre_trabajador     ,
         v_detop98_acc_devol_viv97       ,
         v_detop98_acc_devol_viv92       ,
         v_detop98_pes_devol_viv97       ,
         v_detop98_pes_devol_viv92       ,
         v_detop98_pes_int_devol_viv97   ,
         v_detop98_pes_int_devol_viv92   ,
         v_detop98_remanente             ,
         v_detop98_f_afectacion_contable ,
         v_detop98_f_valor_recep_afore   ,
         v_detop98_f_valor_devol_inf     ,
         v_detop98_estado_devolucion

   FROM safre_viv:deo_det_op98 a
   WHERE
    a.folio = p_folio

   -- =====================================================================
   --               IMPORTES DE VIVIENDA 92
   -- =====================================================================
   -- los montos de vivienda 92 van a la subcuenta 8
   IF ( v_detop98_pes_devol_viv92 > 0 ) THEN
      LET v_subcuenta_tmp       =  8;
      LET v_fondo_inversion_tmp = 11;

      -- el movimiento es:
      -- 31	ABONO POR DEVOLUCIÓN DE APLICACIONES INDEBIDAS
      LET v_movimiento_tmp = 31;

      -- se transfieren los datos al registro de
      --LET v_preliq_f_liquida          = v_fecha_primer_dia_mes      ;
      LET v_preliq_f_liquida          = TODAY                       ;
      
      LET v_preliq_id_derechohabiente = v_detop98_id_derechohabiente;
      LET v_preliq_subcuenta          = v_subcuenta_tmp             ;
      LET v_preliq_fondo_inversion    = v_fondo_inversion_tmp       ;
      LET v_preliq_movimiento         = v_movimiento_tmp            ;
      LET v_preliq_folio_liquida      = p_folio                     ;
      LET v_preliq_id_referencia      = v_detop98_id_detalle        ;
      LET v_preliq_monto_acciones     = v_detop98_acc_devol_viv92;
      LET v_preliq_monto_pesos        = v_detop98_pes_devol_viv92;
      --LET v_preliq_f_valor            = v_fecha_primer_dia_mes      ;
      LET v_preliq_f_valor            = v_detop98_f_valor_devol_inf      ;
      
      
      LET v_preliq_f_registro         = TODAY                       ;
      LET v_preliq_h_registro         = CURRENT HOUR TO SECOND      ;
      LET v_preliq_usuario            = p_usuario_cod               ;

      -- se insertan en la tabla de preliquidacion
      INSERT INTO safre_viv:deo_preliquida
             (f_liquida         ,
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
              h_registro         
              )
      VALUES (v_preliq_f_liquida         ,
              v_preliq_id_derechohabiente,
              v_preliq_subcuenta         ,
              v_preliq_fondo_inversion   ,
              v_preliq_movimiento        ,
              v_preliq_folio_liquida     ,
              v_preliq_id_referencia     ,
              v_preliq_monto_acciones    ,
              v_preliq_monto_pesos       ,
              v_preliq_f_valor           ,
              v_preliq_f_registro        ,
              v_preliq_h_registro        
             );
      -- NOTA
      -- Los intereses no se insertar porque ya estan contemplados en el
      -- monto en pesos
   END IF

   -- =====================================================================
   --               IMPORTES DE VIVIENDA 97
   -- =====================================================================

   -- los montos de vivienda 97 van a la subcuenta 4
   IF ( v_detop98_pes_devol_viv97 > 0 ) THEN
      LET v_subcuenta_tmp       =  4;
      LET v_fondo_inversion_tmp = 11;

      -- el movimiento es:
      -- 31	ABONO POR DEVOLUCIÓN DE APLICACIONES INDEBIDAS
      LET v_movimiento_tmp = 31;

      -- se transfieren los datos al registro de
      --LET v_preliq_f_liquida          = v_fecha_primer_dia_mes      ;
      LET v_preliq_f_liquida          = TODAY                       ;
      
      LET v_preliq_id_derechohabiente = v_detop98_id_derechohabiente;
      LET v_preliq_subcuenta          = v_subcuenta_tmp             ;
      LET v_preliq_fondo_inversion    = v_fondo_inversion_tmp       ;
      LET v_preliq_movimiento         = v_movimiento_tmp            ;
      LET v_preliq_folio_liquida      = p_folio                     ;
      LET v_preliq_id_referencia      = v_detop98_id_detalle        ;
      LET v_preliq_monto_acciones     = v_detop98_acc_devol_viv97;
      LET v_preliq_monto_pesos        = v_detop98_pes_devol_viv97;
      --LET v_preliq_f_valor            = v_fecha_primer_dia_mes      ;
      LET v_preliq_f_valor            = v_detop98_f_valor_devol_inf ;
      
      LET v_preliq_f_registro         = TODAY                       ;
      LET v_preliq_h_registro         = CURRENT HOUR TO SECOND      ;
      LET v_preliq_usuario            = p_usuario_cod               ;

      -- se insertan en la tabla de preliquidacion
      INSERT INTO safre_viv:deo_preliquida
       (f_liquida         ,
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
        h_registro         
        )
      VALUES (v_preliq_f_liquida         ,
              v_preliq_id_derechohabiente,
              v_preliq_subcuenta         ,
              v_preliq_fondo_inversion   ,
              v_preliq_movimiento        ,
              v_preliq_folio_liquida     ,
              v_preliq_id_referencia     ,
              v_preliq_monto_acciones    ,
              v_preliq_monto_pesos       ,
              v_preliq_f_valor           ,
              v_preliq_f_registro        ,
              v_preliq_h_registro        
             );
      -- NOTA
      -- Los intereses no se insertar porque ya estan contemplados en el
      -- monto en pesos

   END IF


 END FOREACH

 {
 glo_folio
 con estatus 1
 para el proceso y todo eso
 }
   
   -- Se comenta porque el estado 3 de archivo es reversado
   UPDATE glo_folio
      SET status       = 1 -- preliquidado
    WHERE 
      folio = p_folio;
   
   -- se actualiza el folio en la tabla de control de operacion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = 801
      AND opera_cod   = 3
      AND pid         = p_pid;

   
   -- ##[Error]
   -- ##-- Agregar folio a operacion de integracion
   -- ##UPDATE safre_viv:bat_ctr_operacion
   -- ##   SET folio       = p_folio
   -- ## WHERE proceso_cod = 801
   -- ##   AND opera_cod   = 3
   -- ##   AND pid         = p_pid;

 IF ( v_i_resultado = 0 ) THEN
 		LET err_txt = 'El proceso finalizó correctamente.';
 END IF 
 
 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado, isam_err, err_txt;
END FUNCTION;


