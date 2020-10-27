






CREATE FUNCTION "safreviv".fn_cbd_adelantos_acl(p_finicio DATE, p_fcorte DATE, p_tabla VARCHAR(60))
RETURNING VARCHAR(255);

   DEFINE v_consulta           LVARCHAR(1000);
   DEFINE  sql_err             INTEGER ;
   DEFINE  isam_err            INTEGER ;
   DEFINE  error_info          VARCHAR(200);
   DEFINE v_mensaje            VARCHAR(80);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
      LET v_mensaje = "Erro en el modulo acl -> "|| sql_err || " ; " || isam_err || " ; "|| TRIM(error_info);
      UPDATE cbd_control_adelanto set estado = 9 where modulo = 'acl';
      RETURN TRIM(v_mensaje);
   END EXCEPTION WITH RESUME
   
   SET PDQPRIORITY HIGH;

   --Se cargan los movimientos adelantados del periodo
   LET v_consulta =  "INSERT INTO cbd_movimiento_adelanto " ||
                     "SELECT " ||
                        "afi.id_derechohabiente, " ||
                        "afi.nss, " ||
                        "mov.f_liquida, " ||
                        "'acl', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "1 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cta_his_pagos pag ON ( " ||
                              "pag.folio = mov.folio_liquida " ||
                              "AND pag.id_referencia = mov.id_referencia " ||
                              "AND pag.origen_archivo <> 1 " ||
                              "AND pag.ind_liquidacion = 3 " ||      --APORTACION ACLARATORIO FUSI�N O TRASPASO AF-AF (TPO_ACL 13 o 17) ADELANTAMIENTO
                     ") " ||
                     "INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = pag.id_derechohabiente " ||
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
                        "'acl', " ||
                        "mov.subcuenta, " ||
                        "mov.movimiento, " ||
                        "mov.fondo_inversion, " ||
                        "mov.folio_liquida, " ||
                        "mov.id_referencia, " ||
                        "mov.monto_acciones, " ||
                        "mov.monto_pesos, " ||
                        "0 " ||
                     "FROM " || p_tabla || " mov " ||
                     "INNER JOIN cta_his_pagos pag ON ( " ||
                              "pag.folio = mov.folio_liquida " ||
                              "AND pag.id_referencia = mov.id_referencia " ||
                              "AND pag.origen_archivo <> 1 " ||
                              "AND pag.ind_liquidacion = 3 " ||      --APORTACION ACLARATORIO FUSI�N O TRASPASO AF-AF (TPO_ACL 13 o 17) ADELANTAMIENTO
                     ") " ||
                     "INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = pag.id_derechohabiente " ||
                     "WHERE mov.f_liquida < '" || p_finicio || "' " ||
                     "AND mov.subcuenta IN (4,8,55) ";
   EXECUTE IMMEDIATE v_consulta ;

   LET v_mensaje = "Los adelantos acl del periodo " || TO_CHAR(p_finicio,'%d-%m-%Y') || " al " || TO_CHAR(p_fcorte,'%d-%m-%Y') ||" se calcularon correctamente en la tabla " || p_tabla;

   UPDATE cbd_control_adelanto set estado = 2 where modulo = 'acl';
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_mensaje;

END FUNCTION;


