






CREATE FUNCTION "safreviv".fn_cbd_adelantos_crt_neg(p_fcorte DATE, p_tabla VARCHAR(60))
RETURNING VARCHAR(255);

DEFINE v_consulta           LVARCHAR(1000);
   DEFINE  sql_err             INTEGER ;
   DEFINE  isam_err            INTEGER ;
   DEFINE  error_info          VARCHAR(200);
   DEFINE v_mensaje            VARCHAR(150);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_mensaje = "Erro en el modulo grt -> "|| sql_err || " ; " || isam_err || " ; "|| TRIM(error_info);
      RETURN TRIM(v_mensaje);
   END EXCEPTION WITH RESUME

   SET PDQPRIORITY HIGH;


    --Se cargan los movimientos adelantados del periodo
   LET v_consulta =  "INSERT INTO tmp_adelanto_grt_neg " ||
                     "SELECT " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'grt', " ||
                        "mov.subcuenta, " ||
                        "mov.monto_acciones, " ||
                        "uso_agr.periodo_pago " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cre_uso_garantia uso_agr ON ( " ||
                                          "uso_agr.folio_liquida = mov.folio_liquida " ||
                                          "AND uso_agr.periodo_pago = mov.id_referencia " ||
                                          "AND uso_agr.id_derechohabiente = mov.id_derechohabiente " ||
                                          "AND uso_agr.tpo_transferencia = 18 " ||		--43 bis
                                          "AND uso_agr.estado IN (140,145) " ||
                                          "AND uso_agr.edo_procesar IN (70,75,80,85) " ||
                                                   ") " ||
                     "INNER JOIN afi_derechohabiente afi ON (afi.id_derechohabiente = uso_agr.id_derechohabiente " ||
                                                       " AND afi.tipo_trabajador = 'I') " ||
                     "WHERE mov.f_liquida <= '" || p_fcorte || "' " ||
                     "AND mov.subcuenta IN (4,8,55) " ||
                     "AND mov.monto_acciones < 0 ";
   EXECUTE IMMEDIATE v_consulta ;

   LET v_mensaje = "Los adelantos grt con fecha de liquidación menor o igual a " || TO_CHAR(p_fcorte,'%d-%m-%Y') ||" se calcularon correctamente en la tabla " || p_tabla;
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_mensaje;
END FUNCTION;


