






CREATE FUNCTION "safreviv".fn_dae_actualiza_status_retiro_historica_retiro()

RETURNING SMALLINT, INTEGER, INTEGER, CHAR(50)

DEFINE v_id_derechohabiente_sol DECIMAL(9,0);
DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_id_referencia          DECIMAL(9,0);
DEFINE v_folio_liquida_retiro   DECIMAL(9,0);
DEFINE v_f_liquida              DATE;

DEFINE v_folio_liquida_aj       DECIMAL(9,0);
DEFINE v_id_referencia_aj       DECIMAL(9,0);
DEFINE v_id_derechohabiente_aj  CHAR(11);
DEFINE v_tot_actualizados       INTEGER;

DEFINE sql_err                  INTEGER  ;
DEFINE v_resultado              INTEGER  ;
DEFINE isam_err                 INTEGER  ;
DEFINE err_txt                  CHAR(200);
DEFINE v_msj                    CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      LET v_tot_actualizados = 0;
      RETURN v_resultado, isam_err, v_tot_actualizados, err_txt;
   END EXCEPTION
   
   SET DEBUG FILE TO "/safreviv_int/BD/actualiza_retiro.trace";
   TRACE ON;

LET v_id_derechohabiente     = 0;
LET v_id_derechohabiente_sol = 0;
LET v_id_referencia          = 0;
LET v_folio_liquida_retiro   = 0;
LET v_f_liquida              = 0;

LET v_folio_liquida_aj       = 0;
LET v_id_referencia_aj       = 0;
LET v_id_derechohabiente_aj  = 0;

LET v_resultado              = 0;
LET isam_err                 = 0;
LET v_msj                    = "Actualización Exitosa";
LET v_tot_actualizados       = 0;

   FOREACH
      --Recupera datos de las solicitudes de Devolución
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_sol
      FROM   dae_det_solicitud
      WHERE  resul_opera = "01"
         LET  err_txt = "Dentro del Foreach -" || v_id_derechohabiente_sol ;
      FOREACH

         --Recupera último movimiento de retiro
         SELECT folio_liquida,
                id_referencia,
                MAX(f_liquida)
         INTO   v_folio_liquida_retiro,
                v_id_referencia,
                v_f_liquida
         FROM   cta_movimiento
         WHERE  id_derechohabiente = v_id_derechohabiente_sol
         AND    subcuenta = 46
         AND    movimiento = 1402
         GROUP BY 1,2

--         IF v_id_referencia IS NOT NULL THEN
               LET  err_txt = "Dentro del IF -" || v_id_derechohabiente_sol || "-" || v_id_referencia || "-" || v_f_liquida;
            FOREACH

               SELECT id_referencia
               INTO   v_id_referencia_aj
               FROM   cta_movimiento
               WHERE  id_derechohabiente = v_id_derechohabiente_sol
               AND    subcuenta = 46
               AND    movimiento = 511
               AND    f_liquida <= v_f_liquida
               
               LET  err_txt = "Antes del update " || v_id_derechohabiente_sol || "-" || v_id_referencia_aj;
               
               --Actualiza status_retiro 4 (Tiene retiro)
               UPDATE dae_det_solicitud 
                  SET status_retiro = 4
               WHERE  id_derechohabiente = v_id_derechohabiente_sol
               AND    id_dae_referencia  = v_id_referencia_aj;
               
               LET v_tot_actualizados = v_tot_actualizados + 1;
            END FOREACH   
  --       END IF
      END FOREACH   
   END FOREACH

RETURN v_resultado, isam_err, v_tot_actualizados, v_msj ;

END FUNCTION;


