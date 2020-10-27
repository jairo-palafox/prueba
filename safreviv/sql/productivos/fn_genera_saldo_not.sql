






CREATE FUNCTION "safreviv".fn_genera_saldo_not()
RETURNING SMALLINT;

   DEFINE v_resultado            SMALLINT;

   --Se eliminna la tabla de saldo preca
   DROP TABLE IF EXISTS not_saldo_diario CASCADE ;

   CREATE TABLE not_saldo_diario 
     (
       id_derechohabiente DECIMAL(9,0) NOT NULL,
       sdo_viv92 decimal(13,2),
       sdo_viv97 decimal(13,2)
     )
  FRAGMENT BY round robin in not_sdo_1_dbs, not_sdo_2_dbs
  LOCK MODE ROW;

  SET PDQPRIORITY HIGH;

  IF EXISTS ( SELECT tabla_saldo
                 FROM safre_sdo@vivws_tcp:glo_saldo
                WHERE tabla_saldo = "cta_saldo_diario"
                  AND ind_saldo = 1
             ) THEN

      --Se calcula el saldo para viv97
      DROP TABLE IF EXISTS tmp_saldo97;
      SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM safre_sdo@vivws_tcp:cta_saldo_diario 
      WHERE subcuenta IN (4,44,55)
      AND fondo_inversion = 11 
      AND id_derechohabiente IN (SELECT afi_ind.id_derechohabiente FROM afi_ind_notifica afi_ind WHERE afi_ind.tpo_notificacion IN (1,2))
      GROUP BY id_derechohabiente
      INTO TEMP tmp_saldo97;
      UPDATE STATISTICS FOR TABLE tmp_saldo97;

      --Se calcula el saldo para viv92
      DROP TABLE IF EXISTS tmp_saldo92;
      SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM safre_sdo@vivws_tcp:cta_saldo_diario 
      WHERE subcuenta IN (8,42)
      AND fondo_inversion = 11 
      AND id_derechohabiente IN (SELECT afi_ind.id_derechohabiente FROM afi_ind_notifica afi_ind WHERE afi_ind.tpo_notificacion IN (1,2))
      GROUP BY id_derechohabiente
      INTO TEMP tmp_saldo92;
      UPDATE STATISTICS FOR TABLE tmp_saldo92;
   ELSE
      --Se calcula el saldo para viv97
      DROP TABLE IF EXISTS tmp_saldo97;
      SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM safre_sdo@vivws_tcp:cta_saldo_diario_bis 
      WHERE subcuenta IN (4,44,55)
      AND fondo_inversion = 11 
      AND id_derechohabiente IN (SELECT afi_ind.id_derechohabiente FROM afi_ind_notifica afi_ind WHERE afi_ind.tpo_notificacion IN (1,2))
      GROUP BY id_derechohabiente
      INTO TEMP tmp_saldo97;
      UPDATE STATISTICS FOR TABLE tmp_saldo97;

      --Se calcula el saldo para viv92
      DROP TABLE IF EXISTS tmp_saldo92;
      SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM safre_sdo@vivws_tcp:cta_saldo_diario_bis 
      WHERE subcuenta IN (8,42)
      AND fondo_inversion = 11 
      AND id_derechohabiente IN (SELECT afi_ind.id_derechohabiente FROM afi_ind_notifica afi_ind WHERE afi_ind.tpo_notificacion IN (1,2))
      GROUP BY id_derechohabiente
      INTO TEMP tmp_saldo92;
      UPDATE STATISTICS FOR TABLE tmp_saldo92;
   END IF

   LOCK TABLE not_saldo_diario IN EXCLUSIVE MODE;
   
   INSERT INTO not_saldo_diario
   SELECT --DISTINCT
   ind.id_derechohabiente,
   NVL(sdo92.monto_pesos,0),
   NVL(sdo97.monto_pesos,0)
   FROM afi_ind_notifica ind
   LEFT JOIN tmp_saldo97 sdo97 ON sdo97.id_derechohabiente = ind.id_derechohabiente 
   LEFT JOIN tmp_saldo92 sdo92 ON sdo92.id_derechohabiente = ind.id_derechohabiente 
   WHERE ind.tpo_notificacion IN (1,2);

   {INSERT INTO not_saldo_diario
   SELECT DISTINCT
   sdo92.id_derechohabiente,
   NVL(sdo92.monto_pesos,0),
   NVL(sdo97.monto_pesos,0)
   FROM tmp_saldo97 sdo97, tmp_saldo92 sdo92
   where sdo97.id_derechohabiente=sdo92.id_derechohabiente;}

   

   UNLOCK TABLE not_saldo_diario;
   CREATE INDEX xpk_not_saldo_diario ON not_saldo_diario (id_derechohabiente) USING btree IN not_sdo_ix_dbs;

   UPDATE STATISTICS FOR TABLE not_saldo_diario;

   {DELETE FROM not_saldo_diario WHERE (sdo_viv92 + sdo_viv97) < 100;}

   SET PDQPRIORITY DEFAULT;

   LET v_resultado = 1;
   
   RETURN v_resultado;      --uno significa que la funcion se ejecuto correctamente
END FUNCTION;


