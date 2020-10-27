






CREATE FUNCTION "safreviv".fn_cbd_adelantos_agr2(p_finicio DATE, p_fcorte DATE, p_tabla VARCHAR(60))
RETURNING VARCHAR(255);

   DEFINE v_consulta           LVARCHAR(1500);
   DEFINE  sql_err             INTEGER ;
   DEFINE  isam_err            INTEGER ;
   DEFINE  error_info          VARCHAR(200);
   DEFINE v_mensaje            VARCHAR(255);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_mensaje = "Erro en el modulo agr -> "|| sql_err || " ; " || isam_err || " ; "|| TRIM(error_info);
      UPDATE cbd_control_adelanto set estado = 9 where modulo = 'agr';
      RETURN TRIM(v_mensaje);
   END EXCEPTION WITH RESUME

   SET DEBUG FILE TO '/safreviv_int/archivos/cbdAdelantoAgr.trace';
   TRACE ON;

   SET PDQPRIORITY HIGH;

   --Se cargan los movimientos adelantados del periodo
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'agr', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "1 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cre_acreditado acr ON ( " ||
                        "acr.id_cre_acreditado = mov.id_referencia " ||
                        "AND acr.id_derechohabiente = mov.id_derechohabiente " ||
                        "AND acr.folio_liquida = mov.folio_liquida " ||
                        "AND acr.tpo_originacion = 4 " ||   --Anualidades Garantizadas
                           "AND acr.estado = 140 " ||
                           "AND acr.edo_procesar < 120 " ||
                           "AND acr.edo_procesar <> 7 " ||
                     ") " ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = acr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.id_derechohabiente NOT IN (SELECT tpm.id_derechohabiente " ||
                                       "FROM safre_tmp:tmp_cre_confirmado tpm " ||
                                       "WHERE tpm.modulo_cod = 'agr' " ||
                                       ") " ||
                     "AND mov.folio_liquida < 48856 " ||
                     "AND mov.subcuenta IN (4,8,55) " ||
                     "AND mov.f_liquida >= '" || p_finicio || "' " ||
                     "AND mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.f_liquida < '01/31/2016'" ||
                     "AND mov.monto_acciones < 0 " ||
                     "AND mov.movimiento IN(492,82)";
   EXECUTE IMMEDIATE v_consulta ;

   --Se cargan los movimientos historicos
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'agr', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "0 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cre_acreditado acr ON ( " ||
                        "acr.id_cre_acreditado = mov.id_referencia " ||
                        "AND acr.id_derechohabiente = mov.id_derechohabiente " ||
                        "AND acr.folio_liquida = mov.folio_liquida " ||
                        "AND acr.tpo_originacion = 4 " ||   --Anualidades Garantizadas
                        "AND acr.estado = 140 " ||
                        "AND acr.edo_procesar < 120 " ||
                        "AND acr.edo_procesar <> 7 " ||
                     ") " ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = acr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.id_derechohabiente NOT IN ( " ||
                                     "SELECT tpm.id_derechohabiente " ||
                                       "FROM safre_tmp:tmp_cre_confirmado tpm " ||
                                       "WHERE tpm.modulo_cod = 'agr' " ||
                                       ") " ||
                     "AND mov.folio_liquida < 48856 " ||
                     "AND mov.subcuenta IN (4,8,55) " ||
                     "AND mov.f_liquida >= MDY(11,1,2012) " ||
                     "AND mov.f_liquida < '" || p_finicio || "' " ||
                     "AND mov.f_liquida < '01/31/2016'" ||
                     "AND mov.monto_acciones < 0 " ||
                     "AND mov.movimiento IN(492,82)";
   EXECUTE IMMEDIATE v_consulta ;

   --Se cargan los movimientos adelantados del periodo de uso de anualidad
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'agr', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "1 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cre_uso_garantia uso_agr ON ( " ||
                                          "uso_agr.folio_liquida = mov.folio_liquida " ||
                                          "AND uso_agr.id_cre_uso_garantia = mov.id_referencia " ||
                                          "AND uso_agr.id_derechohabiente = mov.id_derechohabiente " ||
                                          "AND uso_agr.tpo_transferencia = 43 " ||    --Anualidades Garantizadas
                                          "AND uso_agr.estado = 140 " ||
                                          "AND uso_agr.edo_procesar IN (10,55,70,75,80,85) " ||
                                                   ") " ||
                     "INNER JOIN cre_ctr_archivo cre_arc ON (cre_arc.id_cre_ctr_archivo = uso_agr.id_cre_ctr_archivo " ||
                                                        "AND cre_arc.operacion = 43 )" ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = uso_agr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.folio_liquida < 48856 " ||
                     "AND mov.f_liquida >= '" || p_finicio || "' " ||
                     "AND mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.f_liquida < '01/31/2016'" ||
                     "AND mov.subcuenta IN (4,8,55) " ||
                     "AND mov.monto_acciones < 0 " ||
                     "AND mov.movimiento IN(242,542)";

   EXECUTE IMMEDIATE v_consulta ;

   --Historicos de Uso de anualidad
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'agr', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "0 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cre_uso_garantia uso_agr ON ( " ||
                                          "uso_agr.folio_liquida = mov.folio_liquida " ||
                                          "AND uso_agr.id_cre_uso_garantia = mov.id_referencia " ||
                                          "AND uso_agr.id_derechohabiente = mov.id_derechohabiente " ||
                                          "AND uso_agr.tpo_transferencia = 43 " ||    --Anualidades Garantizadas
                                          "AND uso_agr.estado = 140 " ||
                                          "AND uso_agr.edo_procesar IN (55,70,75,80,85) " ||
                                                   ") " ||
                     "INNER JOIN cre_ctr_archivo cre_arc ON (cre_arc.id_cre_ctr_archivo = uso_agr.id_cre_ctr_archivo " ||
                                                        "AND cre_arc.operacion = 43 )" ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = uso_agr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.folio_liquida < 48856 " ||
                     "AND mov.f_liquida >= MDY(11,1,2012) " ||
                     "AND mov.f_liquida < '" || p_finicio || "' " ||
                     "AND mov.f_liquida < '01/31/2016'" ||
                     "AND mov.subcuenta IN (4,8,55) " ||
                     "AND mov.monto_acciones < 0 " ||
                     "AND mov.movimiento IN(242,542)";

   EXECUTE IMMEDIATE v_consulta;

   --Movimientos de devolucion
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
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
                                          "AND dev_acr.tpo_transferencia = 43 " ||    --Anualidades Garantizadas
                                          "AND dev_acr.estado IN (140,145) " ||
                                          "AND dev_acr.edo_procesar NOT IN (5,120) " ||
                                                   ") " ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = dev_acr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.f_liquida >= '" || p_finicio || "' " ||
                     "AND mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   --Historico de Movimientos de devolucion
   LET v_consulta =  "INSERT INTO safre_tmp:tmp_cbd_movimiento_adelanto " ||
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
                                          "AND dev_acr.tpo_transferencia = 43 " ||    --Anualidades Garantizadas
                                          "AND dev_acr.estado IN (140,145) " ||
                                          "AND dev_acr.edo_procesar NOT IN (5,120) " ||
                                                   ") " ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = dev_acr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.f_liquida >= MDY(11,1,2012) " ||
                     "AND mov.f_liquida < '" || p_finicio || "' " || 
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   LET v_mensaje = "Los adelantos agr del periodo " || TO_CHAR(p_finicio,'%d-%m-%Y') || " al " || TO_CHAR(p_fcorte,'%d-%m-%Y') ||" se calcularon correctamente en la tabla " || p_tabla;

   UPDATE cbd_control_adelanto set estado = 2 where modulo = 'agr';

   SET PDQPRIORITY DEFAULT;
   RETURN v_mensaje;
END FUNCTION;


