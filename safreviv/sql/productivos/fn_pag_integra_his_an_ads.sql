






CREATE FUNCTION "safreviv".fn_pag_integra_his_an_ads(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT,VARCHAR(200);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   SET PDQPRIORITY HIGH;

   INSERT INTO safre_his@vivht_tcp:his_pag_anual (nss,
                              id_derechohabiente,
                              rfc,
                              nombre,
                              ano_pago,
                              clave,
                              patron,
                              bimestres,
                              importe,
                              folio,
                              usuario)
   SELECT
      tmp.nss,
      afi.id_derechohabiente,
      tmp.rfc,
      tmp.nombre,
      tmp.ano,
      tmp.clave,
      tmp.nombre_patron,
      bimestres,
      (importe)/100,
      p_folio,
      p_usuario
   FROM safre_tmp:tmp_his_an_ads tmp
   INNER JOIN afi_derechohabiente afi ON afi.nss = tmp.nss;

   INSERT INTO safre_his@vivht_tcp:his_pag_anual (nss,
                              id_derechohabiente,
                              rfc,
                              nombre,
                              ano_pago,
                              clave,
                              patron,
                              bimestres,
                              importe,
                              folio,
                              usuario)
   SELECT
      tmp.nss,
      '',
      tmp.rfc,
      tmp.nombre,
      tmp.ano,
      tmp.clave,
      tmp.nombre_patron,
      bimestres,
      (importe)/100,
      p_folio,
      p_usuario
   FROM safre_tmp:tmp_his_an_ads tmp
   WHERE tmp.nss NOT IN (SELECT afi.nss FROM afi_derechohabiente afi);

   RETURN v_resultado, v_mensaje;
END FUNCTION;


