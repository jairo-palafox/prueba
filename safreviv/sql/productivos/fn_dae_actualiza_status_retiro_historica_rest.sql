






CREATE FUNCTION "safreviv".fn_dae_actualiza_status_retiro_historica_rest()

RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE v_id_derechohabiente_sol  DECIMAL(9,0);
DEFINE v_id_derechohabiente      DECIMAL(9,0);
DEFINE v_id_referencia           DECIMAL(9,0);
DEFINE v_folio_liquida           DECIMAL(9,0);
DEFINE v_f_liquida               DATE;
DEFINE v_monto_acciones          DECIMAL(16,6);
DEFINE v_monto_acciones_aj       DECIMAL(16,6);
DEFINE v_sum_acciones            DECIMAL(16,6);
DEFINE sql_err                   INTEGER  ;
DEFINE v_resultado               INTEGER  ;
DEFINE isam_err                  INTEGER  ;
DEFINE err_txt                   CHAR(200);
DEFINE v_msj                     CHAR(200);
DEFINE v_f_liquida_ret           DATE;
DEFINE v_monto_acciones_ret      DECIMAL(16,6);
DEFINE v_existe                  SMALLINT;

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      
      RETURN v_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/BD/act_st_rest.dae";
   --TRACE ON;

LET v_id_derechohabiente      = 0;
LET v_id_derechohabiente_sol  = 0;
LET v_id_referencia           = 0;
LET v_folio_liquida           = 0;
LET v_f_liquida               = 0;
LET v_existe                  = 0;
LET v_monto_acciones          = 0;
LET v_monto_acciones_aj       = 0;
LET v_sum_acciones            = 0;
LET v_resultado               = 0;
LET isam_err                  = 0;
LET v_msj                     = "Actualización Exitosa";

   --Se buscan cuentas con movimientos de abono por restitución
   -- ABONO RETIRO AMORT EXCEDENTE NO PAGADO - 251

   FOREACH
      --Recupera datos de las solicitudes de Devolución
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_sol
      FROM   dae_det_solicitud
      WHERE  resul_opera = "01"
      AND    status_retiro = 4
      GROUP BY 1

      LET  err_txt = "Dentro del Foreach -" || v_id_derechohabiente_sol ;
      FOREACH
         --Recupera la fecha del último movimiento de restitución
         SELECT MAX(f_liquida),
                monto_acciones
         INTO   v_f_liquida,
                v_monto_acciones
         FROM   cta_movimiento
         WHERE  id_derechohabiente = v_id_derechohabiente_sol
         AND    subcuenta = 46
         AND    movimiento = 251
         GROUP BY 2

         IF v_f_liquida IS NOT NULL THEN
            LET  err_txt = "Dentro del IF -" || v_id_derechohabiente_sol || "-" || v_monto_acciones || "-" || v_f_liquida;            
            LET v_sum_acciones = 0;
            FOREACH              
               --Consulta los abonos de am-ex por derechohabiente
               SELECT id_referencia,
                      id_derechohabiente,
                      monto_acciones
               INTO   v_id_referencia,
                      v_id_derechohabiente, 
                      v_monto_acciones_aj
               FROM   cta_movimiento
               WHERE  id_derechohabiente = v_id_derechohabiente_sol
               AND    subcuenta  = 46
               AND    movimiento = 511
               AND    f_liquida <= v_f_liquida
               ORDER BY f_liquida DESC

               SELECT COUNT(*)
               INTO   v_existe 
               FROM   dae_det_solicitud 
               WHERE  id_derechohabiente = v_id_derechohabiente
               AND    id_dae_referencia  = v_id_referencia
               AND    status_retiro      = 1;

               IF v_existe > 0 THEN 
                  CONTINUE FOREACH;
               END IF

               LET v_sum_acciones = v_sum_acciones + v_monto_acciones_aj;
               --Actualiza status del retiro a 3 (Tiene restitución)
               UPDATE dae_det_solicitud 
                  SET status_retiro      = 7
               WHERE  id_derechohabiente = v_id_derechohabiente
               AND    id_dae_referencia  = v_id_referencia
               AND    status_retiro      = 4;
               
               IF v_sum_acciones = v_monto_acciones THEN
                  --Actualiza status del retiro a 3 (Tiene restitución)
                  UPDATE dae_det_solicitud    
                  SET    status_retiro = 3    
                  WHERE  id_derechohabiente = v_id_derechohabiente
                  AND    status_retiro = 7;
                  
                  EXIT FOREACH;
               END IF

            END FOREACH  
            
         END IF
      END FOREACH
   END FOREACH
   
RETURN v_resultado, isam_err, v_msj;

END FUNCTION;


