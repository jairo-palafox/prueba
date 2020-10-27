






CREATE FUNCTION "safreviv".fn_dae_historico_movimientos (p_condicion LVARCHAR(1000))

RETURNING SMALLINT,
          INTEGER,
          CHAR(200);

   DEFINE v_sel_his           LVARCHAR(1000);
   DEFINE v_sel_act           LVARCHAR(1000);
   DEFINE v_nombre_tabla      VARCHAR(20)  ;
   DEFINE v_existe_his        SMALLINT;
   DEFINE v_resultado         SMALLINT;
   -- Control de Excepciones
   DEFINE sql_err             INTEGER;
   DEFINE isam_err            INTEGER;
   DEFINE v_isam_err          INTEGER;
   DEFINE err_txt             CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt

      LET v_resultado = sql_err;
      LET v_isam_err = isam_err;

      RETURN v_resultado,
             isam_err,
             err_txt;
   END EXCEPTION

   SET DEBUG FILE TO "/safreviv_int/BD/fn_dae_historico_movimientos.trace";

   DROP TABLE IF EXISTS tmp_movimiento;

   LET v_existe_his = 0 ;
   LET v_sel_his = " ";

   LET v_resultado = 0;
   LET isam_err = 0;
   LET v_isam_err = 0;
   LET err_txt = "Ejecución terminada correctamente";

   FOREACH
      SELECT tabla
        INTO v_nombre_tabla
        FROM cat_tab_movimiento
        ORDER BY tabla

      TRACE "tabla:"||v_nombre_tabla;

      LET v_sel_his = " SELECT a.id_derechohabiente,"
                      ||"      a.num_credito,"
                      ||"      a.fecha_pago,"
                      ||"      a.periodo_pago,"
                      ||"      a.registro_pago,"
                      ||"      a.delegacion,"
                      ||"      a.importe_amort,"
                      ||"      a.monto_aivs,"
                      ||"      a.tipo_pago,"
                      ||"      a.nss,"
                      ||"      a.entidad_receptora,"
                      ||"      a.folio_liquida,"
                      ||"      a.fecha_liquida"
                      ||" FROM   dae_det_solicitud a," || v_nombre_tabla || " b "
                      ||" WHERE  b.id_derechohabiente = a.id_derechohabiente "
                      ||" AND    b.subcuenta = 46 "
                      ||" AND    b.movimiento = 1402 "
                      ||" AND    a.id_derechohabiente IS NOT NULL "
                      ||" AND    " || p_condicion
                      || " UNION ";

       LET v_existe_his = 1 ;

   END FOREACH

   LET v_sel_act = " SELECT a.id_derechohabiente,"
                      ||"      a.num_credito,"
                      ||"      a.fecha_pago,"
                      ||"      a.periodo_pago,"
                      ||"      a.registro_pago,"
                      ||"      a.delegacion,"
                      ||"      a.importe_amort,"
                      ||"      a.monto_aivs,"
                      ||"      a.tipo_pago,"
                      ||"      a.nss,"
                      ||"      a.entidad_receptora,"
                      ||"      a.folio_liquida,"
                      ||"      a.fecha_liquida"
                      ||" FROM   dae_det_solicitud a, cta_movimiento b "
                      ||" WHERE  b.id_derechohabiente = a.id_derechohabiente "
                      ||" AND    b.subcuenta = 46 "
                      ||" AND    b.movimiento = 1402 "
                      ||" AND    a.id_derechohabiente IS NOT NULL "
                      ||" AND    " || p_condicion
                      ||"  INTO TEMP tmp_movimiento " ;

   IF v_existe_his = 1 THEN
      LET v_sel_his = v_sel_his || v_sel_act ;
   ELSE
      LET v_sel_his = v_sel_act ;
   END IF

   EXECUTE IMMEDIATE v_sel_his ;

   CREATE INDEX tmp_movimiento_ix1 ON tmp_movimiento
   (id_derechohabiente
    );

    LET v_resultado = 0;

    UPDATE STATISTICS FOR TABLE tmp_movimiento ;

 RETURN v_resultado,
        v_isam_err,
        err_txt;
END FUNCTION
;


