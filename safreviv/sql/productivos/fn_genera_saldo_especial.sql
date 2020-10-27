






CREATE FUNCTION "safreviv".fn_genera_saldo_especial(p_fcorte DATE)
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_fcorte               DATE;
   DEFINE v_estado_ant           SMALLINT;
   DEFINE v_fecha_text           CHAR(6);
   DEFINE v_mensaje              VARCHAR(100);
   DEFINE v_valor_fondo          DECIMAL(19,14);
   DEFINE v_precio               DECIMAL(12,0);
   DEFINE v_tabla                VARCHAR(50);
   DEFINE v_query                CHAR(900);

   --SET DEBUG FILE TO '/safreviv_int/cbd/envio/fn_genera_saldo_especial.trace';
   --TRACE ON;

   LET v_fcorte = p_fcorte;
   LET v_fecha_text = TO_CHAR(v_fcorte,'%y%m%d');

   LET v_mensaje = "El saldo fue generado con fecha de corte = " || v_fcorte;

   --Se eliminan las tablas temporales
   DROP TABLE IF EXISTS tmp_cbd_saldo_mensual;
   DROP TABLE IF EXISTS tmp_saldo97_imss;
   DROP TABLE IF EXISTS tmp_saldo92_imss;
   DROP TABLE IF EXISTS tmp_saldo97_inf;
   DROP TABLE IF EXISTS tmp_saldo92_inf;

   --Se eliminna la tabla de saldo mensual
   DROP TABLE IF EXISTS cbd_saldo_especial_imss CASCADE ;
   DROP TABLE IF EXISTS cbd_saldo_especial_inf CASCADE ;
   
   CREATE TABLE cbd_saldo_especial_imss 
     (
      id_derechohabiente DECIMAL(9,0),
      nss CHAR(11),
      rfc CHAR(13),
      curp CHAR(18),
      ape_paterno  CHAR(40),
      ape_materno CHAR(40),
      nombre CHAR(40),
      nombre_imss CHAR(50),
      f_credito CHAR(6),
      id_credito SMALLINT,
      aport_viv97 CHAR(13),
      aivs_viv97 CHAR(15),
      f_ssv97 CHAR(6),
      aport_viv92 CHAR(11),
      aivs_viv92 CHAR(15),
      f_ssv92 CHAR(6),
      valor_paivs CHAR(12),
      f_paivs CHAR(6),
      f_ultimo_movimiento CHAR(6),
      ind_inhabilita CHAR(1)
     )
  fragment by round robin in cbd_13_dbs , cbd_14_dbs , cbd_15_dbs , cbd_16_dbs
  LOCK MODE ROW;

  CREATE TABLE cbd_saldo_especial_inf 
     (
      id_derechohabiente DECIMAL(9,0),
      nss CHAR(11),
      rfc CHAR(13),
      curp CHAR(18),
      ape_paterno  CHAR(40),
      ape_materno CHAR(40),
      nombre CHAR(40),
      nombre_imss CHAR(50),
      f_credito CHAR(6),
      id_credito SMALLINT,
      aport_viv97 CHAR(13),
      aivs_viv97 CHAR(15),
      f_ssv97 CHAR(6),
      aport_viv92 CHAR(11),
      aivs_viv92 CHAR(15),
      f_ssv92 CHAR(6),
      valor_paivs CHAR(12),
      f_paivs CHAR(6),
      f_ultimo_movimiento CHAR(6),
      ind_inhabilita CHAR(1)
     )
  fragment by round robin in cbd_17_dbs, cbd_18_dbs
  LOCK MODE ROW;

  SET PDQPRIORITY HIGH;

  SELECT
      precio_fondo
   INTO
      v_valor_fondo
   FROM glo_valor_fondo
   WHERE fondo = 11
   AND f_valuacion = v_fcorte;

   LET v_precio = TO_CHAR((v_valor_fondo * 1000000),'&&&&&&&&&&&&');

   --Se busca la tabla donde se encuentran los movimientos de la fecha solicitada
   LET v_tabla = NULL;
   SELECT
      TRIM(tabla)
   INTO
      v_tabla
   FROM cat_tab_movimiento
   WHERE f_inicial <= v_fcorte
   AND f_final >= v_fcorte;

   IF v_tabla IS NULL THEN
      LET v_tabla = 'cta_movimiento';
   END IF
   
  --Se genera una tabla temporal con los saldos para cada derechohabiente a la fecha de corte
  LET v_query =   'SELECT cta.id_derechohabiente, ' ||
                     'cta.subcuenta, ' ||
                     'cta.fondo_inversion, ' ||
                     'SUM(cta.monto_acciones) monto_acciones, ' ||
                     'SUM(cta.monto_acciones * gf.precio_fondo) monto_pesos ' ||
                  'FROM ' || v_tabla || ' cta ' ||
                  'INNER JOIN glo_valor_fondo gf ON (gf.fondo = cta.fondo_inversion AND gf.f_valuacion = "' || v_fcorte || '") ' || 
                  'WHERE cta.f_liquida <= "' || v_fcorte || '" ' ||
                  'AND cta.subcuenta IN (4,8,42,44,55) ' ||
                  'AND cta.fondo_inversion = 11 ' ||
                  'GROUP BY cta.id_derechohabiente, ' ||
                         'cta.subcuenta, ' ||
                         'cta.fondo_inversion ' ||
                  'INTO TEMP tmp_cbd_saldo_mensual ';
   EXECUTE IMMEDIATE v_query;
   UPDATE STATISTICS FOR TABLE tmp_cbd_saldo_mensual;

   --Se calcula el saldo para viv97 imss
   SELECT sdo.id_derechohabiente, SUM(sdo.monto_acciones) monto_acciones, SUM(sdo.monto_pesos) monto_pesos
   FROM  tmp_cbd_saldo_mensual sdo
   WHERE sdo.subcuenta IN (4,55)
   GROUP BY sdo.id_derechohabiente
   INTO TEMP tmp_saldo97_imss;
   UPDATE STATISTICS FOR TABLE tmp_saldo97_imss;

   --Se calcula el saldo para viv92 imss
   SELECT sdo.id_derechohabiente, sdo.monto_acciones, sdo.monto_pesos
   FROM tmp_cbd_saldo_mensual sdo
   WHERE sdo.subcuenta = 8
   --GROUP BY sdo.id_derechohabiente
   INTO TEMP tmp_saldo92_imss;
   UPDATE STATISTICS FOR TABLE tmp_saldo92_imss;

      --Se calcula el saldo para viv97 inf
   SELECT sdo.id_derechohabiente, sdo.monto_acciones, sdo.monto_pesos
   FROM tmp_cbd_saldo_mensual sdo
   WHERE sdo.subcuenta = 44
   --GROUP BY sdo.id_derechohabiente
   INTO TEMP tmp_saldo97_inf;
   UPDATE STATISTICS FOR TABLE tmp_saldo97_inf;

   --Se calcula el saldo para viv92 inf
   SELECT sdo.id_derechohabiente, sdo.monto_acciones, sdo.monto_pesos
   FROM tmp_cbd_saldo_mensual sdo
   WHERE sdo.subcuenta = 42
   --GROUP BY sdo.id_derechohabiente
   INTO TEMP tmp_saldo92_inf;
   UPDATE STATISTICS FOR TABLE tmp_saldo92_inf;

   --Se llena la tabla para generar el archivo imss
   LOCK TABLE cbd_saldo_especial_imss IN EXCLUSIVE MODE;
   
   INSERT INTO cbd_saldo_especial_imss
   SELECT
      afi.id_derechohabiente,
      afi.nss,
      afi.rfc,
      afi.curp,
      afi.ap_paterno_af,
      afi.ap_materno_af,
      afi.nombre_af,
      afi.nombre_imss,
      TO_CHAR(afi.f_credito,'%y%m%d'),
      afi.id_credito,
      TO_CHAR((NVL(sdo97.monto_pesos,0)* 100),'-&&&&&&&&&&&&'),
      TO_CHAR((NVL(sdo97.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv97 
      TO_CHAR((NVL(sdo92.monto_pesos,0)* 100),'-&&&&&&&&&&'),
      TO_CHAR((NVL(sdo92.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv92
      v_precio,
      v_fecha_text,  --f_paivs 
      v_fecha_text,   --f_ultimo_movimiento
      '0' -- ind_inhabilita
   FROM afi_derechohabiente afi 
   LEFT JOIN tmp_saldo97_imss sdo97 ON sdo97.id_derechohabiente = afi.id_derechohabiente
   LEFT JOIN tmp_saldo92_imss sdo92 ON sdo92.id_derechohabiente = afi.id_derechohabiente
   WHERE afi.id_derechohabiente NOT IN (SELECT sfr.id_derechohabiente FROM sfr_marca_activa sfr WHERE sfr.marca IN (150,151,160))
   AND afi.f_apertura <= v_fcorte
   ;

   INSERT INTO cbd_saldo_especial_imss
   SELECT
      afi.id_derechohabiente,
      afi.nss,
      afi.rfc,
      afi.curp,
      afi.ap_paterno_af,
      afi.ap_materno_af,
      afi.nombre_af,
      afi.nombre_imss,
      TO_CHAR(afi.f_credito,'%y%m%d'),
      afi.id_credito,
      TO_CHAR((NVL(sdo97.monto_pesos,0)* 100),'-&&&&&&&&&&&&'),
      TO_CHAR((NVL(sdo97.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv97 
      TO_CHAR((NVL(sdo92.monto_pesos,0)* 100),'-&&&&&&&&&&'),
      TO_CHAR((NVL(sdo92.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv92
      v_precio,
      v_fecha_text,  --f_paivs 
      v_fecha_text,   --f_ultimo_movimiento
      '1' -- ind_inhabilita
   FROM afi_derechohabiente afi 
   LEFT JOIN tmp_saldo97_imss sdo97 ON sdo97.id_derechohabiente = afi.id_derechohabiente
   LEFT JOIN tmp_saldo92_imss sdo92 ON sdo92.id_derechohabiente = afi.id_derechohabiente
   WHERE afi.id_derechohabiente IN (SELECT sfr.id_derechohabiente FROM sfr_marca_activa sfr WHERE sfr.marca IN (150,151,160))
   AND afi.f_apertura <= v_fcorte
   ;

   UNLOCK TABLE cbd_saldo_especial_imss;
   UPDATE STATISTICS FOR TABLE cbd_saldo_especial_imss;

   DELETE FROM cbd_saldo_especial_imss WHERE TO_NUMBER(aivs_viv97) = 0 AND TO_NUMBER(aivs_viv92) = 0 AND id_derechohabiente IN(SELECT afi2.id_derechohabiente 
                                                                                                                                  FROM afi_derechohabiente afi2
                                                                                                                                  WHERE afi2.tipo_trabajador = 'S');

   --Se llena la tabla para generar el archivo solo infonavit
   LOCK TABLE cbd_saldo_especial_inf IN EXCLUSIVE MODE;
   
   INSERT INTO cbd_saldo_especial_inf
   SELECT
      afi.id_derechohabiente,
      afi.nss,
      afi.rfc,
      afi.curp,
      afi.ap_paterno_af,
      afi.ap_materno_af,
      afi.nombre_af,
      afi.nombre_imss,
      TO_CHAR(afi.f_credito,'%y%m%d'),
      afi.id_credito,
      TO_CHAR((sdo97.monto_pesos * 100),'-&&&&&&&&&&&&'),
      TO_CHAR((sdo97.monto_acciones * 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv97 
      TO_CHAR((NVL(sdo92.monto_pesos,0)* 100),'-&&&&&&&&&&'),
      TO_CHAR((NVL(sdo92.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv92
      v_precio,
      v_fecha_text,  --f_paivs 
      v_fecha_text,   --f_ultimo_movimiento
      '0' -- ind_inhabilita 
   FROM afi_derechohabiente afi 
   INNER JOIN tmp_saldo97_inf sdo97 ON sdo97.id_derechohabiente = afi.id_derechohabiente
   LEFT JOIN tmp_saldo92_inf sdo92 ON sdo92.id_derechohabiente = afi.id_derechohabiente
   WHERE afi.id_derechohabiente NOT IN (SELECT sfr.id_derechohabiente FROM sfr_marca_activa sfr WHERE sfr.marca IN (150,151,160))
   AND afi.f_apertura <= v_fcorte
   ;

   --Se llenan los registros solo infonavit que no tienen saldo
   INSERT INTO cbd_saldo_especial_inf
   SELECT
      afi.id_derechohabiente,
      afi.nss,
      afi.rfc,
      afi.curp,
      afi.ap_paterno_af,
      afi.ap_materno_af,
      afi.nombre_af,
      afi.nombre_imss,
      TO_CHAR(afi.f_credito,'%y%m%d'),
      afi.id_credito,
      ' 000000000000',
      ' 00000000000000',
      v_fecha_text,  --f_ssv97 
      ' 0000000000',
      ' 00000000000000',
      v_fecha_text,  --f_ssv92
      v_precio,
      v_fecha_text,  --f_paivs 
      v_fecha_text,   --f_ultimo_movimiento
      '0'   -- ind_inhabilita
   FROM afi_derechohabiente afi 
   WHERE afi.tipo_trabajador = 'S'
   AND afi.id_derechohabiente NOT IN (SELECT sdo97.id_derechohabiente FROM tmp_saldo97_inf sdo97)
   AND afi.id_derechohabiente NOT IN (SELECT sfr.id_derechohabiente FROM sfr_marca_activa sfr WHERE sfr.marca IN (150,151,160))
   AND afi.f_apertura <= v_fcorte
   ;

   --Se llenan los registros para cuentas inhabilitadas
   INSERT INTO cbd_saldo_especial_inf
   SELECT
      afi.id_derechohabiente,
      afi.nss,
      afi.rfc,
      afi.curp,
      afi.ap_paterno_af,
      afi.ap_materno_af,
      afi.nombre_af,
      afi.nombre_imss,
      TO_CHAR(afi.f_credito,'%y%m%d'),
      afi.id_credito,
      TO_CHAR((sdo97.monto_pesos * 100),'-&&&&&&&&&&&&'),
      TO_CHAR((sdo97.monto_acciones * 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv97 
      TO_CHAR((NVL(sdo92.monto_pesos,0)* 100),'-&&&&&&&&&&'),
      TO_CHAR((NVL(sdo92.monto_acciones,0)* 1000000),'-&&&&&&&&&&&&&&'),
      v_fecha_text,  --f_ssv92
      v_precio,
      v_fecha_text,  --f_paivs 
      v_fecha_text,   --f_ultimo_movimiento
      '1' -- ind_inhabilita 
   FROM afi_derechohabiente afi 
   INNER JOIN tmp_saldo97_inf sdo97 ON sdo97.id_derechohabiente = afi.id_derechohabiente
   LEFT JOIN tmp_saldo92_inf sdo92 ON sdo92.id_derechohabiente = afi.id_derechohabiente
   WHERE afi.id_derechohabiente IN (SELECT sfr.id_derechohabiente FROM sfr_marca_activa sfr WHERE sfr.marca IN (150,151,160))
   AND afi.f_apertura <= v_fcorte
   ;

   UNLOCK TABLE cbd_saldo_especial_inf;
   UPDATE STATISTICS FOR TABLE cbd_saldo_especial_inf;

   UPDATE cbd_saldo_especial_inf SET f_credito = '      ' WHERE f_credito IS NULL;
   UPDATE cbd_saldo_especial_inf SET rfc = ' ' WHERE rfc IS NULL;
   UPDATE cbd_saldo_especial_inf SET curp = ' ' WHERE curp IS NULL;
   UPDATE cbd_saldo_especial_inf SET ape_paterno = ' ' WHERE ape_paterno IS NULL;
   UPDATE cbd_saldo_especial_inf SET ape_materno = ' ' WHERE ape_materno IS NULL;
   UPDATE cbd_saldo_especial_inf SET nombre = ' ' WHERE nombre IS NULL;
   UPDATE cbd_saldo_especial_inf SET nombre_imss = ' ' WHERE nombre_imss IS NULL;

   UPDATE cbd_saldo_especial_imss SET f_credito = '      ' WHERE f_credito IS NULL;
   UPDATE cbd_saldo_especial_imss SET rfc = ' ' WHERE rfc IS NULL;
   UPDATE cbd_saldo_especial_imss SET curp = ' ' WHERE curp IS NULL;
   UPDATE cbd_saldo_especial_imss SET ape_paterno = ' ' WHERE ape_paterno IS NULL;
   UPDATE cbd_saldo_especial_imss SET ape_materno = ' ' WHERE ape_materno IS NULL;
   UPDATE cbd_saldo_especial_imss SET nombre = ' ' WHERE nombre IS NULL;
   UPDATE cbd_saldo_especial_imss SET nombre_imss = ' ' WHERE nombre_imss IS NULL;


   SET PDQPRIORITY DEFAULT;

   DROP TABLE IF EXISTS tmp_cbd_saldo_mensual;
   DROP TABLE IF EXISTS tmp_saldo97_imss;
   DROP TABLE IF EXISTS tmp_saldo92_imss;
   DROP TABLE IF EXISTS tmp_saldo97_inf;
   DROP TABLE IF EXISTS tmp_saldo92_inf;

   LET v_resultado = 1;
   
   RETURN v_resultado, v_mensaje;      --uno significa que la funcion se ejecuto correctamente

END FUNCTION;


