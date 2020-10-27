






CREATE FUNCTION "safreviv".fn_uni_ajuste_operativo()

RETURNING INTEGER,
          INTEGER,
          VARCHAR(250),
          SMALLINT;


DEFINE sql_err             INTEGER;     
DEFINE isam_err            INTEGER;     
DEFINE err_txt             VARCHAR(250);
DEFINE v_resultado         SMALLINT;
DEFINE v_valor_fondo       DECIMAL(19,14);
DEFINE v_monto_pesos_cargo DECIMAL(12,2);
DEFINE v_monto_pesos_abono DECIMAL(12,2);

ON EXCEPTION SET sql_err, isam_err, err_txt
   RETURN sql_err, 
          isam_err, 
          err_txt,
          v_resultado;
END EXCEPTION

LET v_valor_fondo       = 0;
LET v_monto_pesos_cargo = 0;
LET v_monto_pesos_abono = 0;
LET v_resultado         = 0;
LET sql_err             = 0;
LET isam_err            = 0;
LET err_txt             = "Ajuste realizado exitosamente";

   SELECT precio_fondo
   INTO   v_valor_fondo  
   FROM   glo_valor_fondo
   WHERE  f_valuacion = TODAY 
   AND    fondo       = 11;

   IF v_valor_fondo  IS NOT NULL THEN    
      LET v_monto_pesos_cargo = ((-13761.54) * v_valor_fondo);
      LET v_monto_pesos_abono = v_monto_pesos_cargo * -1;

      INSERT INTO cta_movimiento VALUES (TODAY, 33970784,  4, 11,1212, 36321, 34718,-13761.540000,v_monto_pesos_cargo,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --CARGO
      INSERT INTO cta_movimiento VALUES (TODAY, 33970784, 44, 11, 521, 36321, 34718, 13761.540000,v_monto_pesos_abono,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      
      INSERT INTO cta_movimiento VALUES (TODAY, 40018145, 44, 11,521, 36321, 34718,10095.120000, 17024.51,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      INSERT INTO cta_movimiento VALUES (TODAY, 16554119, 44, 11,521, 36321, 34718, 9650.640000, 16274.94,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      INSERT INTO cta_movimiento VALUES (TODAY, 28649173, 44, 11,521, 36321, 34718,42105.480000, 71007.10,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      INSERT INTO cta_movimiento VALUES (TODAY, 16798993, 44, 11,521, 36321, 34718,17844.730000, 30093.53,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      INSERT INTO cta_movimiento VALUES (TODAY,  6720711, 44, 11,521, 36321, 34718, 8224.100000, 13869.20,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
      INSERT INTO cta_movimiento VALUES (TODAY, 45884176, 44, 11,521, 36321, 34718, 1126.140000,  1963.65,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-34718"); --ABONO
   ELSE 
      LET v_resultado = 1;
      LET err_txt     = "No existe precio fondo para el día de hoy";
   END IF

   RETURN sql_err,   
          isam_err,  
          err_txt,   
          v_resultado;

END FUNCTION;


