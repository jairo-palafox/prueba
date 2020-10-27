






CREATE FUNCTION "safreviv".fn_genera_saldo_preca()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_fcorte               DATE;
   DEFINE v_estado_ant           SMALLINT;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(80);

   DEFINE v_id_derechohabiente   DECIMAL(9,0);

   DEFINE v_subcuenta            SMALLINT;
   DEFINE v_monto                DECIMAL(18,6);
   DEFINE v_pesos                DECIMAL(13,2);
   DEFINE v_precio               DECIMAL(18,6);

   DEFINE v_nss                  CHAR(11);
   DEFINE v_f_proceso            DATE;
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_ap_paterno           CHAR(50);
   DEFINE v_ap_materno           CHAR(50);
   DEFINE v_nombre               CHAR(50);
   DEFINE v_tipo_trabajador      CHAR(1);

   DEFINE v_pesos_92             DECIMAL(13,2);
   DEFINE v_pesos_97             DECIMAL(13,2);
   DEFINE v_aivs_92              DECIMAL(22,6);
   DEFINE v_aivs_97              DECIMAL(22,6);
   DEFINE v_afore                CHAR(3);

   --Se establece la fecha de corte como el dia natural inmediato anterior
   LET v_fcorte = TODAY - 1;

   LET v_resultado = 1;    --Significa que el proceso se ejecuto correctamente
   LET v_mensaje = "El saldo con fecha de corte " || v_fcorte || " se genero correctamente";

   --Se valida si no se ha generado el saldo a la fecha de corte
   SELECT
      estado_genera
   INTO
      v_estado_ant
   FROM safre_sdo@vivws_tcp:glo_ctr_saldo
   WHERE tpo_saldo = 3                    --tpo_saldo = 3 significa que es saldo para precalificacion
   AND f_saldo = v_fcorte
   ;

   --Proceso en ejecucion
   IF(v_estado_ant = 1) THEN
      LET v_resultado = 2;
      LET v_mensaje = "El proceso de generacion del saldo ya esta en proceso";
      RETURN v_resultado, v_mensaje;
   END IF

   --El saldo ya fue generado
   IF(v_estado_ant = 2) THEN
      LET v_resultado = 2;
      LET v_mensaje = "El saldo con fecha de corte " || v_fcorte || " ya fue generado";
      RETURN v_resultado, v_mensaje;
   END IF

    --El proceso anterior genero algun error
   IF(v_estado_ant = 3) THEN
      DELETE FROM safre_sdo@vivws_tcp:glo_ctr_saldo WHERE tpo_saldo = 3 AND f_saldo = v_fcorte;
   END IF

   --Se inserta el registro inicial en el control de saldo
   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_saldo VALUES(v_fcorte, 1, 3);

	--Se eliminna la tabla de saldo preca
   DROP TABLE IF EXISTS cta_saldo_preca CASCADE ;

   CREATE TABLE cta_saldo_preca 
     (
       nss char(11) not null ,
       f_proceso date,
       rfc char(13),
       curp char(18),
       ap_paterno char(50),
       ap_materno char(50),
       nombre char(50),
       subcuenta smallint,
       importe decimal(13,2),
       f_valor date,
       afore char(3),
       tipo_trabajador CHAR(1)
     ) 
  FRAGMENT BY round robin in saldop_1_dbs, saldop_2_dbs, saldop_3_dbs, saldop_4_dbs
  EXTENT SIZE 8000 NEXT SIZE 2000 LOCK MODE ROW;

  --Se elimina la tabla que contiene los deltas para trabajadores del estado
  DROP TABLE IF EXISTS afi_nuevo_trabajador CASCADE ;
   CREATE TABLE afi_nuevo_trabajador
     (
         id_derechohabiente     DECIMAL(9,0) NOT NULL,
         nss                    CHAR(11) NOT NULL ,
         rfc                    CHAR(13) NOT NULL,
         curp                   CHAR(18),
         ap_paterno             CHAR(50) NOT NULL,
         ap_materno             CHAR(50),
         nombre                 CHAR(50) NOT NULL,
         f_apertura             DATE NOT NULL,
         tipo_trabajador        CHAR(1) NOT NULL
     )fragment by round robin in afi_1_dbs , afi_2_dbs;

  SET PDQPRIORITY HIGH;


  DROP TABLE IF EXISTS tmp_saldo_previo;
  DROP TABLE IF EXISTS tmp_saldo_preca;
  DROP TABLE IF EXISTS sin_saldo;
  DROP TABLE IF EXISTS tmp_portabilidad;

  --Se calcula el saldo para omitir el movimiento 481

  --Se genera una tabla temporal con los saldos para cada derechohabiente a la fecha de corte
   SELECT cta.id_derechohabiente,
          cta.subcuenta,
          SUM(cta.monto_acciones * gf.precio_fondo) monto_pesos
   FROM cta_movimiento cta
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = cta.fondo_inversion AND gf.f_valuacion = v_fcorte) 
   WHERE cta.f_liquida <= v_fcorte
   AND cta.subcuenta IN (4,8,42,44,55)
   AND cta.fondo_inversion = 11
   AND cta.movimiento NOT IN (481)
   GROUP BY cta.id_derechohabiente,
          cta.subcuenta
   INTO TEMP tmp_saldo_previo;
   UPDATE STATISTICS FOR TABLE tmp_saldo_previo;

   --Se calcula el saldo omitiendo las cuentas inactivas
   SELECT 
      sdo.id_derechohabiente,
      cat.id_subcuenta,
      sum(sdo.monto_pesos) monto_pesos
   FROM tmp_saldo_previo sdo
   INNER JOIN cat_subcuenta_preca cat ON cat.subcuenta = sdo.subcuenta
   WHERE sdo.id_derechohabiente NOT IN (  SELECT id_derechohabiente 
                                          FROM sfr_marca_activa 
                                          WHERE marca IN (150,151,160)
                                       )
   GROUP BY 1,2
   INTO temp tmp_saldo_preca;
        
  UPDATE STATISTICS FOR TABLE tmp_saldo_preca;

  --Se obtienen las cuentas con marca de portabilidad
   SELECT id_derechohabiente FROM sfr_marca_activa
   WHERE marca IN (704,705)
   INTO TEMP tmp_portabilidad;

   UPDATE STATISTICS FOR TABLE tmp_portabilidad;

   LOCK TABLE cta_saldo_preca IN EXCLUSIVE MODE;
    
   INSERT INTO cta_saldo_preca
   SELECT afi.nss, today, afi.rfc, afi.curp, afi.ap_paterno_af, afi.ap_materno_af,afi.nombre_af,sdo.id_subcuenta,
          sdo.monto_pesos, today -1 , afo.afore_cod,afi.tipo_trabajador
   FROM  tmp_saldo_preca sdo
   INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = sdo.id_derechohabiente
   LEFT JOIN afi_afore afo ON afo.id_derechohabiente = sdo.id_derechohabiente
   WHERE sdo.id_derechohabiente NOT IN (SELECT por.id_derechohabiente FROM tmp_portabilidad por)
   AND   sdo.monto_pesos >= 0
    ;

   INSERT INTO cta_saldo_preca
   SELECT afi.nss, today, afi.rfc, afi.curp, afi.ap_paterno_af, afi.ap_materno_af,afi.nombre_af,sdo.id_subcuenta,
          sdo.monto_pesos, today -1 , "999" afore, afi.tipo_trabajador
   FROM   tmp_saldo_preca sdo
   INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = sdo.id_derechohabiente
   WHERE sdo.id_derechohabiente IN (SELECT por.id_derechohabiente FROM tmp_portabilidad por)
   AND sdo.monto_pesos >= 0
   ;
    
   SELECT 
      UNIQUE (afi.id_derechohabiente) id_derechohabiente
   FROM afi_derechohabiente afi
   WHERE afi.id_derechohabiente NOT IN (  SELECT  sdo.id_derechohabiente
                                          FROM  tmp_saldo_preca sdo
                                       )
	 AND afi.id_derechohabiente NOT IN (   SELECT id_derechohabiente 
                                          FROM sfr_marca_activa
                                          WHERE marca  IN (150,151,160)
                                       )
    INTO TEMP sin_saldo;
    
    UPDATE STATISTICS FOR TABLE sin_saldo;
    
    INSERT INTO cta_saldo_preca
    SELECT afi.nss, today, afi.rfc, afi.curp, afi.ap_paterno_af, afi.ap_materno_af,afi.nombre_af,"4",
           0, today -1 , afo.afore_cod, afi.tipo_trabajador
     FROM sin_saldo sdo 
     INNER JOIN afi_derechohabiente afi ON sdo.id_derechohabiente = afi.id_derechohabiente
     LEFT JOIN afi_afore afo ON afo.id_derechohabiente = sdo.id_derechohabiente
     WHERE sdo.id_derechohabiente NOT IN (SELECT por.id_derechohabiente FROM tmp_portabilidad por)
    ;

    INSERT INTO cta_saldo_preca
    SELECT afi.nss, today, afi.rfc, afi.curp, afi.ap_paterno_af, afi.ap_materno_af,afi.nombre_af,"4",
           0, today -1 , "999" afore, afi.tipo_trabajador
     FROM sin_saldo sdo 
     INNER JOIN afi_derechohabiente afi ON sdo.id_derechohabiente = afi.id_derechohabiente
     WHERE sdo.id_derechohabiente IN (SELECT por.id_derechohabiente FROM tmp_portabilidad por)
    ;
    
   UNLOCK TABLE cta_saldo_preca;
   UPDATE STATISTICS FOR TABLE cta_saldo_preca;
    
   UPDATE cta_saldo_preca SET rfc = ' ' WHERE rfc IS NULL;
   UPDATE cta_saldo_preca SET curp = ' ' WHERE curp IS NULL;
   UPDATE cta_saldo_preca SET ap_paterno = ' ' WHERE ap_paterno IS NULL;
   UPDATE cta_saldo_preca SET ap_materno = ' ' WHERE ap_materno IS NULL;
   UPDATE cta_saldo_preca SET nombre = ' ' WHERE nombre IS NULL;
   UPDATE cta_saldo_preca SET afore = '000' WHERE afore IS NULL;

   DROP TABLE IF EXISTS tmp_saldo_previo;
   DROP TABLE IF EXISTS tmp_saldo_preca;
   DROP TABLE IF EXISTS sin_saldo;
   DROP TABLE IF EXISTS tmp_portabilidad;
        
   --Finaliza la operacion
   UPDATE safre_sdo@vivws_tcp:glo_ctr_saldo SET estado_genera = 2 WHERE tpo_saldo = 3 AND f_saldo = v_fcorte;

   RETURN v_resultado, v_mensaje;      --uno significa que la funcion se ejecuto correctamente

END FUNCTION;


