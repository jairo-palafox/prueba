






CREATE FUNCTION "safreviv".fn_cbd_adelantos_dpe(p_finicio DATE, p_fcorte DATE, p_tabla VARCHAR(60))
RETURNING VARCHAR(255);

   DEFINE v_consulta           LVARCHAR(1000);
   DEFINE  sql_err             INTEGER ;
   DEFINE  isam_err            INTEGER ;
   DEFINE  error_info          VARCHAR(200);
   DEFINE v_mensaje            VARCHAR(80);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_mensaje = "Erro en el modulo dpe -> "|| sql_err || " ; " || isam_err || " ; "|| TRIM(error_info);
      UPDATE cbd_control_adelanto set estado = 9 where modulo = 'dpe';
      RETURN TRIM(v_mensaje);
   END EXCEPTION WITH RESUME
   
   SET PDQPRIORITY HIGH;

   --Se cargan los movimientos adelantados del periodo
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'dpe', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "1 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN dpe_sol_trab_parcial dpe ON (dpe.id_dpe_referencia = mov.id_referencia " ||
                                                      "AND dpe.folio_liquida = mov.folio_liquida " ||
                                                      "AND dpe.diagnostico NOT IN (12,13)) " ||
                     "INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = mov.id_derechohabiente " ||
                     "WHERE mov.f_liquida >= '" || p_finicio || "' " ||
                     "AND mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   --Se cargan los movimientos adelantados historicos
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'dpe', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "0 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN dpe_sol_trab_parcial dpe ON (dpe.id_dpe_referencia = mov.id_referencia " ||
                                                      "AND dpe.folio_liquida = mov.folio_liquida " ||
                                                      "AND dpe.diagnostico NOT IN (12,13)) " ||
                     "INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = mov.id_derechohabiente " ||
                     "WHERE mov.f_liquida >= MDY(11,1,2012) " ||
                     "AND mov.f_liquida < '" || p_finicio || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   LET v_mensaje = "Los adelantos dpe del periodo " || TO_CHAR(p_finicio,'%d-%m-%Y') || " al " || TO_CHAR(p_fcorte,'%d-%m-%Y') ||" se calcularon correctamente en la tabla " || p_tabla;

   UPDATE cbd_control_adelanto set estado = 2 where modulo = 'dpe';
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_mensaje;

END FUNCTION;


