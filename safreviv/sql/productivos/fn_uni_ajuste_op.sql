






CREATE FUNCTION "safreviv".fn_uni_ajuste_op()

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
DEFINE v_bnd_marca         SMALLINT;

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

      LET v_monto_pesos_cargo = ((-28439.99) * v_valor_fondo);  -- -58086.4043758
      LET v_monto_pesos_abono = v_monto_pesos_cargo * -1; --58086.4043758

      INSERT INTO cta_movimiento VALUES (TODAY,  9221948, 4, 11,1212, 48409, 1730795,-28439.990000,v_monto_pesos_cargo,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-48409"); --CARGO
      INSERT INTO cta_movimiento VALUES (TODAY, 11469873, 4, 11, 521, 48409, 1730795, 28439.990000,v_monto_pesos_abono,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-48409"); --ABONO

      LET v_monto_pesos_cargo = ((-1308.58) * v_valor_fondo);
      LET v_monto_pesos_abono = v_monto_pesos_cargo * -1;

      INSERT INTO cta_movimiento VALUES (TODAY,  9221948, 8, 11,1212, 48409, 1730795,-1308.580000,v_monto_pesos_cargo,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-48409"); --CARGO
      INSERT INTO cta_movimiento VALUES (TODAY, 11469873, 8, 11, 521, 48409, 1730795, 1308.580000,v_monto_pesos_abono,TODAY, TODAY, CURRENT HOUR TO SECOND, "ROP-FOLIO-48409"); --ABONO
      
      EXECUTE FUNCTION fn_desmarca_cuenta(9221948,550, 1730795,0,0,"safreviv", 2318)
      INTO v_bnd_marca;
      
      IF v_bnd_marca <> 0 THEN 
         LET v_resultado = 1;
         LET err_txt     = "Error en desmarca " || v_bnd_marca ;
      END IF
   ELSE 
      LET v_resultado = 1;
      LET err_txt     = "No existe precio fondo para el día de hoy";
   END IF

   RETURN sql_err,   
          isam_err,  
          err_txt,   
          v_resultado;

END FUNCTION;


