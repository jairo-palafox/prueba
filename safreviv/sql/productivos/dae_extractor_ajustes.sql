






CREATE FUNCTION "safreviv".dae_extractor_ajustes()

RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE v_fecha_pago        DATE;
DEFINE v_periodo_pago      CHAR(4);
DEFINE v_registro_pago     CHAR(8);
DEFINE v_origen_sol        CHAR(1);
DEFINE v_delegacion        CHAR(2);
DEFINE v_importe_amort     DECIMAL(16,6);
DEFINE v_total_importe     DECIMAL(16,6);
DEFINE v_tipo_pago         CHAR(3);
DEFINE v_nss               CHAR(11);
DEFINE v_entidad_receptora CHAR(3);
DEFINE v_folio_liquida_sol DECIMAL(9,0);
DEFINE v_fecha_ingreso     DATE;
DEFINE v_f_liquida         DATE;
DEFINE v_movimiento        SMALLINT;
DEFINE v_movimiento_desc   CHAR(40);
DEFINE v_fondo_inversion   SMALLINT;
DEFINE v_monto_pesos       DECIMAL(12,2);
DEFINE v_monto_acciones    DECIMAL(16,6);
DEFINE v_f_valor           DATE;
DEFINE v_folio_liquida     DECIMAL(9,0);
DEFINE v_origen            CHAR(20);
DEFINE v_estado            SMALLINT;

DEFINE sql_err                  INTEGER  ;
DEFINE v_resultado              INTEGER  ;
DEFINE isam_err                 INTEGER  ;
DEFINE err_txt                  CHAR(200);
DEFINE v_msj                    CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      
      RETURN v_resultado, isam_err, err_txt;
   END EXCEPTION
   
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/safreviv_int/dae/envio/extractor_ajuste.txt';
   --TRACE ON;


LET v_fecha_pago        = "";
LET v_periodo_pago      = "";
LET v_registro_pago     = "";
LET v_origen_sol        = "";
LET v_delegacion        = "";
LET v_importe_amort     = 0;
LET v_total_importe     = 0;
LET v_tipo_pago         = "";
LET v_nss               = "";
LET v_entidad_receptora = "";
LET v_folio_liquida_sol = 0;
LET v_fecha_ingreso     = "";
LET v_f_liquida         = "";
LET v_movimiento        = 0;
LET v_movimiento_desc   = "";
LET v_fondo_inversion   = 0;
LET v_monto_pesos       = 0;
LET v_monto_acciones    = 0;
LET v_f_valor           = "";
LET v_folio_liquida     = 0;
LET v_origen            = "";
LET v_estado            = 0;

LET v_resultado         = 0 ;
LET isam_err            = 0 ;
LET v_msj               = "Ejecución exitosa";

   FOREACH
      SELECT b.fecha_pago,
             b.periodo_pago,
             b.registro_pago,
             b.origen,
             b.delegacion,
             b.importe_amort,
             b.total_importe,
             b.tipo_pago,
             b.nss,
             b.entidad_receptora,
             b.folio_liquida,
             b.fecha_liquida,
             a.f_liquida,
             a.movimiento,
             c.movimiento_desc,
             a.fondo_inversion,
             a.monto_pesos,
             a.monto_acciones,
             a.f_valor,
             a.folio_liquida,
             a.origen,
             b.estado
        INTO v_fecha_pago,
             v_periodo_pago,
             v_registro_pago,
             v_origen_sol,
             v_delegacion,
             v_importe_amort,
             v_total_importe,
             v_tipo_pago,
             v_nss,
             v_entidad_receptora,
             v_folio_liquida_sol,
             v_fecha_ingreso,
             v_f_liquida,
             v_movimiento,
             v_movimiento_desc,
             v_fondo_inversion,
             v_monto_pesos,
             v_monto_acciones,
             v_f_valor,
             v_folio_liquida,
             v_origen,
             v_estado
      FROM   cta_movimiento14 a, 
             dae_det_solicitud b,
             cat_movimiento c
      WHERE  b.id_derechohabiente = a.id_derechohabiente
      AND    b.id_dae_referencia =  a.id_referencia
      AND    a.subcuenta = 46
      AND    a.movimiento = 511
      AND    c.movimiento = a.movimiento

      INSERT INTO safre_tmp:dae_tmp_prodinf856 (fecha_pago,       
                                                periodo_pago,
                                                registro_pago,
                                                origen_sol,
                                                delegacion,
                                                importe_amort,
                                                total_importe,
                                                tipo_pago,
                                                nss,
                                                entidad_receptora,
                                                folio_liquida_sol,
                                                fecha_ingreso,
                                                f_liquida,
                                                movimiento,
                                                movimiento_desc,
                                                fondo_inversion,
                                                monto_pesos,
                                                monto_acciones,
                                                f_valor,
                                                folio_liquida,
                                                origen,
                                                estado)
             VALUES (v_fecha_pago,
                     v_periodo_pago,
                     v_registro_pago,
                     v_origen_sol,
                     v_delegacion,
                     v_importe_amort,
                     v_total_importe,
                     v_tipo_pago,
                     v_nss,
                     v_entidad_receptora,
                     v_folio_liquida_sol,
                     v_fecha_ingreso,
                     v_f_liquida,
                     v_movimiento,
                     v_movimiento_desc,
                     v_fondo_inversion,
                     v_monto_pesos,
                     v_monto_acciones,
                     v_f_valor,
                     v_folio_liquida,
                     v_origen,
                     v_estado);
   END FOREACH
   
   FOREACH
      SELECT b.nss,
             a.f_liquida,
             a.movimiento,
             c.movimiento_desc,
             a.fondo_inversion,
             a.monto_pesos,
             a.monto_acciones,
             a.f_valor,
             a.folio_liquida,
             a.origen
      INTO   v_nss,            
             v_f_liquida,      
             v_movimiento,     
             v_movimiento_desc,
             v_fondo_inversion,
             v_monto_pesos,    
             v_monto_acciones, 
             v_f_valor,        
             v_folio_liquida,  
             v_origen
      FROM   cta_movimiento14  a,
             dae_det_solicitud b,
             cat_movimiento    c
      WHERE  b.id_derechohabiente = a.id_derechohabiente
      AND    c.movimiento = a.movimiento
      AND    a.subcuenta = 46
      AND    a.movimiento <> 511 

      INSERT INTO safre_tmp:dae_tmp_prodinf856 (nss,
                                                f_liquida,
                                                movimiento,
                                                movimiento_desc,
                                                fondo_inversion,
                                                monto_pesos,
                                                monto_acciones,
                                                f_valor,
                                                folio_liquida,
                                                origen)
             VALUES (v_nss,
                     v_f_liquida,
                     v_movimiento,
                     v_movimiento_desc,
                     v_fondo_inversion,
                     v_monto_pesos,
                     v_monto_acciones,
                     v_f_valor,
                     v_folio_liquida,
                     v_origen);
   END FOREACH
-------   
   FOREACH
      SELECT b.fecha_pago,
             b.periodo_pago,
             b.registro_pago,
             b.origen,
             b.delegacion,
             b.importe_amort,
             b.total_importe,
             b.tipo_pago,
             b.nss,
             b.entidad_receptora,
             b.folio_liquida,
             b.fecha_liquida,
             a.f_liquida,
             a.movimiento,
             c.movimiento_desc,
             a.fondo_inversion,
             a.monto_pesos,
             a.monto_acciones,
             a.f_valor,
             a.folio_liquida,
             a.origen,
             b.estado
        INTO v_fecha_pago,
             v_periodo_pago,
             v_registro_pago,
             v_origen_sol,
             v_delegacion,
             v_importe_amort,
             v_total_importe,
             v_tipo_pago,
             v_nss,
             v_entidad_receptora,
             v_folio_liquida_sol,
             v_fecha_ingreso,
             v_f_liquida,
             v_movimiento,
             v_movimiento_desc,
             v_fondo_inversion,
             v_monto_pesos,
             v_monto_acciones,
             v_f_valor,
             v_folio_liquida,
             v_origen,
             v_estado
      FROM   cta_movimiento a, 
             dae_det_solicitud b,
             cat_movimiento c
      WHERE  b.id_derechohabiente = a.id_derechohabiente
      AND    b.id_dae_referencia =  a.id_referencia
      AND    a.subcuenta = 46
      AND    a.movimiento = 511
      AND    c.movimiento = a.movimiento


      INSERT INTO safre_tmp:dae_tmp_prodinf856 (fecha_pago,       
                                                periodo_pago,
                                                registro_pago,
                                                origen_sol,
                                                delegacion,
                                                importe_amort,
                                                total_importe,
                                                tipo_pago,
                                                nss,
                                                entidad_receptora,
                                                folio_liquida_sol,
                                                fecha_ingreso,
                                                f_liquida,
                                                movimiento,
                                                movimiento_desc,
                                                fondo_inversion,
                                                monto_pesos,
                                                monto_acciones,
                                                f_valor,
                                                folio_liquida,
                                                origen,
                                                estado)
             VALUES (v_fecha_pago,
                     v_periodo_pago,
                     v_registro_pago,
                     v_origen_sol,
                     v_delegacion,
                     v_importe_amort,
                     v_total_importe,
                     v_tipo_pago,
                     v_nss,
                     v_entidad_receptora,
                     v_folio_liquida_sol,
                     v_fecha_ingreso,
                     v_f_liquida,
                     v_movimiento,
                     v_movimiento_desc,
                     v_fondo_inversion,
                     v_monto_pesos,
                     v_monto_acciones,
                     v_f_valor,
                     v_folio_liquida,
                     v_origen,
                     v_estado);
   END FOREACH
   
   FOREACH
      SELECT b.nss,
             a.f_liquida,
             a.movimiento,
             c.movimiento_desc,
             a.fondo_inversion,
             a.monto_pesos,
             a.monto_acciones,
             a.f_valor,
             a.folio_liquida,
             a.origen
      INTO   v_nss,            
             v_f_liquida,      
             v_movimiento,     
             v_movimiento_desc,
             v_fondo_inversion,
             v_monto_pesos,    
             v_monto_acciones, 
             v_f_valor,        
             v_folio_liquida,  
             v_origen
      FROM   cta_movimiento  a,
             dae_det_solicitud b,
             cat_movimiento    c
      WHERE  b.id_derechohabiente = a.id_derechohabiente
      AND    c.movimiento = a.movimiento
      AND    a.subcuenta = 46
      AND    a.movimiento <> 511 

      INSERT INTO safre_tmp:dae_tmp_prodinf856 (nss,
                                                f_liquida,
                                                movimiento,
                                                movimiento_desc,
                                                fondo_inversion,
                                                monto_pesos,
                                                monto_acciones,
                                                f_valor,
                                                folio_liquida,
                                                origen)
             VALUES (v_nss,
                     v_f_liquida,
                     v_movimiento,
                     v_movimiento_desc,
                     v_fondo_inversion,
                     v_monto_pesos,
                     v_monto_acciones,
                     v_f_valor,
                     v_folio_liquida,
                     v_origen);
   END FOREACH

RETURN v_resultado, isam_err, v_msj;

END FUNCTION;


