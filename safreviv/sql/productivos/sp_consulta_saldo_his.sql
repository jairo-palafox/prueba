






CREATE PROCEDURE "safreviv".sp_consulta_saldo_his(p_id_derechohabiente  DECIMAL(9,0), p_fcorte DATE)
   DEFINE v_hoy      DATE;
   DEFINE v_tabla    VARCHAR(20);
   DEFINE v_query    CHAR(500);

   CALL fn_tab_movimiento(1,null,p_fcorte) returning v_tabla;

   LET v_query =  "INSERT INTO tmp_movimientos_his " ||
                  "SELECT mov.id_derechohabiente, " ||
                    "mov.subcuenta, " ||
                    "mov.fondo_inversion, " ||
                    "mov.f_liquida, " ||
                    "mov.movimiento, " ||
                    "mov.monto_pesos, " ||
                    "mov.monto_acciones, " ||
                    "mov.f_valor, " ||
                    "mov.folio_liquida, " ||
                    "mov.origen, " ||
                    "mov.id_referencia, " ||
                    "cat.modulo_cod " ||
                  "FROM  " || v_tabla || " mov " ||
                  "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento " ||
                  "WHERE mov.id_derechohabiente =  " || p_id_derechohabiente || " " ||
                  "AND mov.f_liquida <= '" || p_fcorte || "'";

   EXECUTE IMMEDIATE v_query;

   INSERT INTO tmp_arbol_saldo_his
   SELECT cgr.grupo_regimen ||'-'||cgr.desc_larga, 
          tms.fondo_inversion,
          CASE tms.fondo_inversion 
             WHEN 0 THEN sum(tms.monto_pesos)
             ELSE sum(tms.monto_acciones * gf.precio_fondo)
          END CASE,
          sum(tms.monto_acciones),
          cgr.grupo_regimen,
          "",
          "",
          cgr.grupo_regimen||"."||tms.fondo_inversion,
          1
     FROM tmp_movimientos_his tms,
          cat_grp_subcta_regimen cgsr,
          cat_grupo_regimen cgr,
          glo_valor_fondo gf
    WHERE cgsr.subcuenta       = tms.subcuenta
      AND cgr.grupo_regimen    = cgsr.grupo_regimen
      AND gf.fondo             = tms.fondo_inversion
      AND gf.f_valuacion       = p_fcorte
    GROUP BY 1,2,5,6,7,8;

   INSERT INTO tmp_arbol_saldo_his
   SELECT cs.subcuenta||'-'||cs.subcuenta_desc, 
          tms.fondo_inversion, 
          CASE tms.fondo_inversion 
             WHEN 0 THEN sum(tms.monto_pesos)
             ELSE sum(tms.monto_acciones * gf.precio_fondo)
          END CASE,
          sum(tms.monto_acciones),
          cs.subcuenta,
          "",
          cgsr.grupo_regimen||"."||tms.fondo_inversion,
          cgsr.grupo_regimen||"."||tms.fondo_inversion||"."||cs.subcuenta,
          2
     FROM tmp_movimientos_his tms,
          cat_grp_subcta_regimen cgsr,
          cat_subcuenta cs,
          glo_valor_fondo gf
    WHERE cs.subcuenta    = tms.subcuenta
      AND cgsr.subcuenta  = tms.subcuenta 
      AND gf.fondo        = tms.fondo_inversion
      AND gf.f_valuacion  = p_fcorte
    GROUP BY 1,2,5,6,7,8 ;

   CREATE INDEX xi_tmp_movimientos_his ON tmp_movimientos_his(subcuenta, fondo_inversion) using btree;
END PROCEDURE;


