






CREATE FUNCTION "safreviv".fn_dae_actualiza_status_retiro_historica_ajuste()

RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE v_id_derechohabiente_sol DECIMAL(9,0);
DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_id_referencia          DECIMAL(9,0);
DEFINE v_folio_liquida          DECIMAL(9,0);
DEFINE v_f_liquida              DATE;

DEFINE sql_err                  INTEGER  ;
DEFINE v_resultado              INTEGER  ;
DEFINE isam_err                 INTEGER  ;
DEFINE err_txt                  CHAR(200);
DEFINE v_msj                    CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      
      RETURN v_resultado, isam_err, err_txt;
   END EXCEPTION

LET v_id_derechohabiente     = 0;
let v_id_derechohabiente_sol = 0;
LET v_id_referencia          = 0;
LET v_folio_liquida          = 0;
LET v_f_liquida              = 0;

LET v_resultado              = 0 ;
LET isam_err                 = 0 ;
LET v_msj                    = "Actualización Exitosa";

   FOREACH
      --Recupera datos de las solicitudes de Devolución      
      SELECT a.id_derechohabiente
      INTO   v_id_derechohabiente_sol
      FROM   dae_det_solicitud a,
             dae_det_ajuste b
      WHERE  a.id_derechohabiente = b.id_derechohabiente
      AND    a.resul_opera = b.resul_operacion
      AND    a.status_retiro = 1
      GROUP  BY 1
    
      --Recupera la fecha del último movimiento de abono
      SELECT MAX(f_liquida)
      INTO   v_f_liquida
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente_sol
      AND    subcuenta = 46
      AND    movimiento = 1422;

      FOREACH
         --Consulta los abonos de am-ex por derechohabiente
         SELECT folio_liquida,
                id_referencia,
                id_derechohabiente
         INTO   v_folio_liquida,
                v_id_referencia,
                v_id_derechohabiente
         FROM   cta_movimiento
         WHERE  f_liquida <= v_f_liquida
         AND    id_derechohabiente = v_id_derechohabiente_sol
         AND    subcuenta = 46
         AND    movimiento = 511

         IF v_id_referencia IS NOT NULL THEN 
            --Actualiza status del retiro a 2 (Tiene ajuste)
            UPDATE dae_det_solicitud 
               SET status_retiro      = 2
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    folio_liquida      = v_folio_liquida 
            AND    resul_opera        = "01"
            AND    id_dae_referencia  = v_id_referencia            
            AND    status_retiro      = 1;
         END IF   
      END FOREACH
   END FOREACH

RETURN v_resultado, isam_err, v_msj;

END FUNCTION;


