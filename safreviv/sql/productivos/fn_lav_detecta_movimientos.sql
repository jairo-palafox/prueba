






CREATE FUNCTION "safreviv".fn_lav_detecta_movimientos(p_usuario_cod  CHAR(20), 
                                           p_pid          INTEGER, 
                                           p_proceso_cod  SMALLINT,
                                           p_opera_cod    SMALLINT,
                                           p_folio        DECIMAL (9,0),
                                           p_fecha_ini    DATE,
                                           p_fecha_fin    DATE,
                                           p_periodo      CHAR(6)
                                           )

   RETURNING INTEGER, 
             INTEGER, 
             CHAR(200),
             INTEGER

   -- Control de Excepciones
   DEFINE sql_err                  INTEGER;
   DEFINE isam_err                 INTEGER;
   DEFINE err_txt                  CHAR(200);
   DEFINE v_resultado              INTEGER;
   DEFINE v_monto_valida           DECIMAL(16,4);
   DEFINE v_id_tipo_cambio         DECIMAL(9,0); 
   DEFINE v_valor_dolar            DECIMAL(16,4);
   DEFINE v_monto_op_relevantes    DECIMAL(5,0);
   DEFINE v_monto_op_importantes   DECIMAL(5,0);
   DEFINE v_monto_op_preocupantes  DECIMAL(5,0);  
   DEFINE v_f_valor              DATE;
   DEFINE v_id_derechohabiente     DECIMAL(9,0);
   DEFINE v_monto_pesos            DECIMAL(16,4);
   DEFINE v_id_referencia          DECIMAL(9,0);
   DEFINE v_nss                    CHAR(11);
   DEFINE v_tot_regs_detectados    INTEGER;
   DEFINE v_id_tipo_reporte        SMALLINT;
   DEFINE v_id_lav_tipo_validacion SMALLINT;
   DEFINE v_id_lav_tipo_condicion  SMALLINT;
   DEFINE v_tipo_reporte           SMALLINT;    
   DEFINE v_monto_desde              DECIMAL(16,6);
   DEFINE v_monto_hasta              DECIMAL(16,6);
      
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado           = sql_err;
      RETURN v_resultado, 
             isam_err, 
             err_txt,
             v_tot_regs_detectados;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/detecta_operaciones.lav";
   --TRACE ON;
   
   LET v_resultado              = 0;
   LET sql_err                  = 0;
   LET isam_err                 = 0;
   LET err_txt                  = "Validación exitosa";
   LET v_monto_op_relevantes    = 10000;
   LET v_monto_op_importantes   = 0;
   LET v_monto_op_preocupantes  = 0;
   LET v_tot_regs_detectados    = 1;
   LET v_id_tipo_reporte        = 0;
   LET v_id_lav_tipo_validacion = 0;
   LET v_id_lav_tipo_condicion  = 0;
   LET v_monto_valida           = 0;
   LET v_tipo_reporte           = 1;

   FOREACH
      SELECT a.f_valor, 
             a.id_derechohabiente,
             a.monto_pesos,
             a.id_referencia,
             b.nss
      INTO   v_f_valor,
             v_id_derechohabiente,
             v_monto_pesos,
             v_id_referencia,
             v_nss
      FROM   cta_movimiento a,
             afi_derechohabiente b
      WHERE  b.id_derechohabiente = a.id_derechohabiente
      AND    a.subcuenta = 45
      AND    a.movimiento = 491
      AND    a.f_liquida BETWEEN p_fecha_ini
      AND    p_fecha_fin

      SELECT id_tipo_cambio,
             valor
      INTO   v_id_tipo_cambio,
             v_valor_dolar
      FROM   lav_tipo_cambio
      WHERE  f_valor = v_f_valor;

      LET v_monto_valida =  ( (v_monto_pesos) / (v_valor_dolar) );
      
      IF v_monto_valida IS NOT NULL THEN
         SELECT a.id_tipo_reporte,
                b.id_lav_tipo_validacion,
                b.id_lav_tipo_condicion,
                c.monto_desde,
                c.monto_hasta
         INTO   v_id_tipo_reporte,       
                v_id_lav_tipo_validacion,
                v_id_lav_tipo_condicion, 
                v_monto_desde,           
                v_monto_hasta
         FROM   lav_tipo_reporte      a,
                lav_tipo_validaciones b,
                lav_tipo_condicion    c
         WHERE  b.id_tipo_reporte       = a.id_tipo_reporte
         AND    b.id_lav_tipo_condicion = c.id_lav_tipo_condicion
         AND    a.id_tipo_reporte       = v_tipo_reporte;

         IF v_monto_valida >= v_monto_desde AND v_monto_valida <= v_monto_hasta THEN
            INSERT INTO lav_det_lavado (
                                        id_lav_det_lavado ,
                                        folio             ,
                                        id_derechohabiente,
                                        nss               ,
                                        id_tipo_reporte   ,
                                        id_tipo_validacion,
                                        consecutivo       ,
                                        periodo_reporte   ,
                                        tipo_operacion    ,
                                        inst_monetario    ,
                                        pesos             ,
                                        id_tipo_cambio    ,
                                        f_operacion       ,
                                        f_deteccion       ,
                                        f_registro        ,
                                        estado            ,
                                        usuario_captura
                                       )
                               VALUES (
                                       v_id_referencia         ,
                                       p_folio                 ,
                                       v_id_derechohabiente    ,
                                       v_nss                   ,
                                       v_id_tipo_reporte       ,
                                       v_id_lav_tipo_validacion,
                                       v_tot_regs_detectados   ,
                                       p_periodo               ,
                                       1                       , --Detección de abonos
                                       2                       , --Dólares
                                       v_monto_pesos           ,
                                       v_id_tipo_cambio        ,
                                       v_f_valor               ,
                                       TODAY                   ,
                                       TODAY                   ,
                                       1                       ,
                                       p_usuario_cod
                                      );
            LET v_tot_regs_detectados = v_tot_regs_detectados + 1;
         END IF   
      ELSE
         LET err_txt = "No existe valor del dólar al día " || v_f_valor ||" para el NSS " || v_nss ;
      END IF    
   END FOREACH;

   UPDATE statistics FOR TABLE lav_det_lavado ;
   
   UPDATE bat_ctr_operacion 
   SET    folio = p_folio
   WHERE  pid = p_pid;
   
   UPDATE glo_folio
   SET    status = 2
   WHERE  folio = p_folio;

   RETURN v_resultado, 
          isam_err, 
          err_txt,
          v_tot_regs_detectados;
END FUNCTION;


