






CREATE PROCEDURE "safreviv".sp_montos_folio(p_folio       DECIMAL(9,0),
                                 p_tabla       CHAR(30))

    DEFINE qry_string     CHAR(500);

    SET PDQPRIORITY HIGH;

    DROP TABLE IF EXISTS tmp_montos_liq;
    DROP TABLE IF EXISTS tmp_agrupa_saldo_liq;

    CREATE TEMP TABLE tmp_montos_liq(
          subcuenta          SMALLINT     ,
          subcuenta_desc     CHAR(50)     ,
          fondo_inversion    SMALLINT     ,
          padre_id           CHAR(30)     ,
          id                 CHAR(30)     ,
          nivel              SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6));



    IF p_tabla = "cta_movimiento" THEN
       LET p_tabla = fn_tab_movimiento(0,p_folio,'');
    END IF

    LET qry_string = " SELECT dp.subcuenta, "||
                            " dp.fondo_inversion, "||
                            " dp.movimiento, "||
                            " sum(dp.monto_pesos) monto_pesos, "||
                            " sum(dp.monto_acciones) monto_acciones "||
                       " FROM "||p_tabla ||" dp "||
                      " WHERE dp.folio_liquida = "|| p_folio||
                      " GROUP BY 1,2,3 "||
                      " INTO TEMP tmp_agrupa_saldo_liq ";

    EXECUTE IMMEDIATE qry_string;

----INSERTA GRUPOS
    INSERT INTO tmp_montos_liq
    SELECT tgr.grupo_regimen,
           tgr.desc_larga,
           tmp.fondo_inversion,
           "",
           tgr.grupo_regimen||"."||tmp.fondo_inversion,
           1,
           sum(tmp.monto_pesos),
           sum(tmp.monto_acciones)
      FROM tmp_agrupa_saldo_liq tmp,
           cat_grp_subcta_regimen tasr,
           cat_grupo_regimen tgr
     WHERE tasr.subcuenta    = tmp.subcuenta
       AND tgr.grupo_regimen = tasr.grupo_regimen
     GROUP BY 1,2,3,4,5,6;

----INSERTA SUBCUENTAS
    INSERT INTO tmp_montos_liq
    SELECT cs.subcuenta,
           cs.subcuenta||'-'||cs.subcuenta_desc,
           tmp.fondo_inversion,
           cg.grupo_regimen||"."||tmp.fondo_inversion,
           cg.grupo_regimen||"."||tmp.fondo_inversion||"."||cs.subcuenta,
           2,
           sum(tmp.monto_pesos),
           sum(tmp.monto_acciones)
      FROM tmp_agrupa_saldo_liq tmp,
           cat_grp_subcta_regimen cg,
           cat_subcuenta cs
     WHERE cg.subcuenta   = tmp.subcuenta
       AND cs.subcuenta   = cg.subcuenta
     GROUP BY 1,2,3,4,5,6;

----INSERTA MOVIMIENTOS
    INSERT INTO tmp_montos_liq
    SELECT cm.movimiento,
           cm.movimiento||'-'||cm.movimiento_desc,
           tmp.fondo_inversion,
           cg.grupo_regimen||"."||tmp.fondo_inversion||"."||cg.subcuenta,
           cg.grupo_regimen||"."||tmp.fondo_inversion||"."||cg.subcuenta||"."||cm.movimiento,
           3,
           sum(tmp.monto_pesos),
           sum(tmp.monto_acciones)
      FROM tmp_agrupa_saldo_liq tmp,
           cat_grp_subcta_regimen cg,
           cat_movimiento cm
     WHERE cm.movimiento = tmp.movimiento
       AND cg.subcuenta  = tmp.subcuenta
     GROUP BY 1,2,3,4,5,6;

    SET PDQPRIORITY DEFAULT;
END PROCEDURE;


