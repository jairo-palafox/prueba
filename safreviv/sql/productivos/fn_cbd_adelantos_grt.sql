






CREATE FUNCTION "safreviv".fn_cbd_adelantos_grt(p_finicio DATE, p_fcorte DATE, p_tabla VARCHAR(60))
RETURNING VARCHAR(255);

   DEFINE v_consulta           LVARCHAR(1000);
   DEFINE  sql_err             INTEGER ;
   DEFINE  isam_err            INTEGER ;
   DEFINE  error_info          VARCHAR(200);
   DEFINE v_mensaje            VARCHAR(255);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_mensaje = "Erro en el modulo grt -> "|| sql_err || " ; " || isam_err || " ; "|| TRIM(error_info);
      UPDATE cbd_control_adelanto set estado = 9 where modulo = 'grt';
      RETURN TRIM(v_mensaje);
   END EXCEPTION WITH RESUME

   LET v_consulta = "DELETE FROM cbd_movimiento_adelanto WHERE modulo = 'grt'";

   SET PDQPRIORITY HIGH;

   EXECUTE IMMEDIATE v_consulta ;

   --Se cargan los movimientos adelantados historicos
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "m.id_derechohabiente, " ||
                        "m.nss, " ||
                        "m.f_liquida, " ||
                        "'grt', " ||
                        "m.subcuenta, " ||
                        "62, " ||
                        "11, " ||
                        "69237, " ||
                        "m.id_derechohabiente, " ||
                        "m.monto_acciones, " ||
                        "round(m.monto_acciones*f.precio_fondo,2), " ||
                        "0 " ||
                     "FROM safre_tmp:tmp_adelanto m, glo_valor_fondo f " ||
                     "WHERE m.modulo = 'grt' " ||
                     "AND m.f_liquida = f.f_valuacion " ||
                     "AND f.fondo = 11 ";
   EXECUTE IMMEDIATE v_consulta ;

{
   --Movimientos de devolucion
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'dse', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "1 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN dse_agrupa_devolucion dev_acr ON ( " ||
                                          "dev_acr.folio_liquida = mov.folio_liquida " ||
                                          "AND dev_acr.id_dse_grp_devolucion = mov.id_referencia " ||
                                          "AND dev_acr.id_derechohabiente = mov.id_derechohabiente " ||
                                          "AND dev_acr.tpo_transferencia = 19 " ||		--43 bis
                                          "AND dev_acr.estado IN (140,145) " ||
                                          "AND dev_acr.edo_procesar NOT IN (5,120) " ||
                                                   ") " ||
                     "LEFT JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = dev_acr.id_derechohabiente " ||
                                                      " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.f_liquida >= '" || p_finicio || "' " ||
                     "AND mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   --Movimientos de devolucion Historicos
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'dse', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "0 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN dse_agrupa_devolucion dev_acr ON ( " ||
                                          "dev_acr.folio_liquida = mov.folio_liquida " ||
                                          "AND dev_acr.id_dse_grp_devolucion = mov.id_referencia " ||
                                          "AND dev_acr.id_derechohabiente = mov.id_derechohabiente " ||
                                          "AND dev_acr.tpo_transferencia = 19 " ||		--43 bis
                                          "AND dev_acr.estado IN (140,145) " ||
                                          "AND dev_acr.edo_procesar NOT IN (5,120) " ||
                                                   ") " ||
                     "LEFT JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = dev_acr.id_derechohabiente " ||
                                                      " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.f_liquida >= MDY(11,1,2012) " ||
                     "AND mov.f_liquida < '" || p_finicio || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;
}

   LET v_mensaje = "Los adelantos grt del periodo " || TO_CHAR(p_finicio,'%d-%m-%Y') || " al " || TO_CHAR(p_fcorte,'%d-%m-%Y') ||" se calcularon correctamente en la tabla " || p_tabla;

   UPDATE cbd_control_adelanto set estado = 2 where modulo = 'grt';

   SET PDQPRIORITY DEFAULT;
   RETURN v_mensaje;
END FUNCTION;


