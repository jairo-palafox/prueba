






CREATE FUNCTION "safreviv".fn_saldo_actual(p_nss        CHAR(11),
                                p_subcuenta  SMALLINT,
                                p_f_saldo DATE)
   RETURNING SMALLINT, SMALLINT, DECIMAL(20,2), DECIMAL(20,2);

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_subcuenta            SMALLINT;
   DEFINE v_acciones             DECIMAL(20,2);
   DEFINE v_fondo_inversion      SMALLINT;

   DEFINE r_subcuenta            SMALLINT;
   DEFINE r_fondo_inversion      SMALLINT;
   DEFINE r_acciones             DECIMAL(20,2);
   DEFINE r_pesos                DECIMAL(20,2);

   DEFINE v_f_inicio             DATE;

   DROP TABLE IF EXISTS tmp_saldo_dia;

   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss       = p_nss;

   IF p_subcuenta = 0 THEN ----SALDO DE TODAS LAS SUBCUENTAS


         SELECT id_derechohabiente     ,
                subcuenta subcuenta_dia,
                fondo_inversion        ,
                SUM(monto_acciones) acciones
           FROM cta_movimiento
          WHERE f_liquida          <=  p_f_saldo
            AND id_derechohabiente = v_id_derechohabiente
          GROUP BY 1,2,3
           INTO TEMP tmp_saldo_dia;

   ELSE  ---SALDO POR SUBCUENTA
         SELECT id_derechohabiente     ,
                subcuenta subcuenta_dia,
                fondo_inversion        ,
                SUM(monto_acciones) acciones
           FROM cta_movimiento
          WHERE f_liquida          <=  p_f_saldo
            AND id_derechohabiente = v_id_derechohabiente
            AND subcuenta          = p_subcuenta
          GROUP BY 1,2,3
           INTO TEMP tmp_saldo_dia;
   END IF

   FOREACH SELECT a.subcuenta_dia,
                  a.fondo_inversion,
                  sum(a.acciones),
                  sum(a.acciones*gf.precio_fondo)
             INTO r_subcuenta,
                  r_fondo_inversion,
                  r_acciones,
                  r_pesos
             FROM tmp_saldo_dia a,
                  glo_valor_fondo gf
            WHERE gf.fondo              = a.fondo_inversion
              AND gf.f_valuacion        = TODAY
            GROUP BY 1,2

      RETURN r_subcuenta,
             r_fondo_inversion,
             r_acciones,
             r_pesos WITH RESUME;
   END FOREACH;
END FUNCTION;


