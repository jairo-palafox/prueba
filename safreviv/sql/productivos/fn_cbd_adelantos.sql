






CREATE FUNCTION "safreviv".fn_cbd_adelantos(p_finicio DATE, p_fcorte DATE)
RETURNING SMALLINT;
   DEFINE v_resultado         SMALLINT;

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/bdnsviv/debug_fn_cbd_adelantos_" || YEAR(p_fcorte) || "_" || MONTH(p_fcorte) || ".txt");

   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo pag");
   CALL fn_cbd_adelantos_pag(p_finicio, p_fcorte) RETURNING v_resultado;

   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo acl");
   CALL fn_cbd_adelantos_acl(p_finicio, p_fcorte) RETURNING v_resultado;
   
   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo dis");
   CALL fn_cbd_adelantos_dis(p_finicio, p_fcorte) RETURNING v_resultado;

   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo acr");
   CALL fn_cbd_adelantos_acr(p_finicio, p_fcorte) RETURNING v_resultado;

   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo agr");
   CALL fn_cbd_adelantos_agr(p_finicio, p_fcorte) RETURNING v_resultado;

   --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo grt");
   CALL fn_cbd_adelantos_grt(p_finicio, p_fcorte) RETURNING v_resultado;

  --TRACE("Se ejecuta la funcion para los movimientos adelantados del modulo dpe");
   CALL fn_cbd_adelantos_dpe(p_finicio, p_fcorte) RETURNING v_resultado;

   RETURN v_resultado;
END FUNCTION;


