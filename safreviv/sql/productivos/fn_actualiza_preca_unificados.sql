






CREATE FUNCTION "safreviv".fn_actualiza_preca_unificados()
RETURNING VARCHAR(200);

   DEFINE v_mensaje              VARCHAR(100);

   DEFINE v_unificados           SMALLINT;
   DEFINE v_unificadores         SMALLINT;

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_pesos97              DECIMAL(13,2);
   DEFINE v_acciones97           DECIMAL(18,6);
   DEFINE v_pesos92              DECIMAL(13,2);
   DEFINE v_acciones92           DECIMAL(18,6);
   
   SET PDQPRIORITY HIGH;

   LET v_unificados = 0;
   LET v_unificadores = 0;

   --Primero se ponen a cero los unificados
   FOREACH
      SELECT id_derechohabiente,
             nss
      INTO   v_id_derechohabiente,
             v_nss
      FROM nss_unificado
      WHERE id_excepcion = 1

      LET v_unificados = v_unificados + 1;
      UPDATE cta_saldo_preca SET pesos_92 = 0, aivs_92 = 0, pesos_97 = 0, aivs_97 = 0 WHERE nss = v_nss;
   END FOREACH;

   --Se calcula el saldo de los uificadores
   FOREACH
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM nss_unificador
      WHERE id_excepcion = 1

      LET v_unificadores = v_unificadores + 1;
   
      SELECT FIRST 1
         uni.nss,
         sdo97.monto_pesos,
         sdo97.monto_acciones,
         sdo92.monto_pesos,
         sdo92.monto_acciones
      INTO
         v_nss,
         v_pesos97,
         v_acciones97,
         v_pesos92,
         v_acciones92
      FROM nss_unificador uni
      LEFT OUTER JOIN(
         SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos, SUM(monto_acciones) AS monto_acciones 
         FROM safre_sdo@vivws_tcp:cta_saldo_diario_bis WHERE subcuenta IN (4,44)
         AND fondo_inversion = 11
         AND id_derechohabiente  = v_id_derechohabiente
      GROUP BY id_derechohabiente
      ) sdo97 ON uni.id_derechohabiente = sdo97.id_derechohabiente
      LEFT OUTER JOIN(
         SELECT id_derechohabiente, monto_pesos, monto_acciones 
         FROM safre_sdo@vivws_tcp:cta_saldo_diario_bis WHERE subcuenta = 8
         AND fondo_inversion = 11 
         AND id_derechohabiente = v_id_derechohabiente
      ) sdo92 ON uni.id_derechohabiente = sdo92.id_derechohabiente
      WHERE uni.id_derechohabiente = v_id_derechohabiente;

      IF v_acciones97 IS NULL OR v_acciones97 < 0 THEN
         LET v_acciones97 = 0;
         LET v_pesos97 = 0;
      END IF
      IF v_acciones92 IS NULL OR v_acciones92 < 0 THEN
         LET v_acciones92 = 0;
         LET v_pesos92 = 0;
      END IF

      UPDATE cta_saldo_preca SET pesos_92 = v_pesos92, 
                                 aivs_92 = v_acciones92, 
                                 pesos_97 = v_pesos97, 
                                 aivs_97 = v_acciones97
      WHERE nss = v_nss;
      
   END FOREACH;

   DROP TABLE IF EXISTS tmp_saldos;
   LET v_mensaje = "Proceso ejecutado, unidicados = " || v_unificados || ", unificadores = " || v_unificadores;
   
	RETURN v_mensaje;
END FUNCTION;


