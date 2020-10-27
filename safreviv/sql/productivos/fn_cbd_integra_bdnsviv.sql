






CREATE FUNCTION "safreviv".fn_cbd_integra_bdnsviv(p_folio DECIMAL(9,0))
RETURNING SMALLINT,VARCHAR(200);

   DEFINE v_folio_ant         DECIMAL(9,0);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   --Variables para el manejo del encabezado
   DEFINE v_cza_f_operacion               DATE;
   DEFINE v_f_corte                       DATE;

   --Variables para el manejo del sumario
   DEFINE v_sum_tpo_registro              CHAR(2);
   DEFINE v_sum_f_operacion               DATE;
   DEFINE v_sum_total_registros           DECIMAL(15,0);
   DEFINE v_sum_total_saldo_viv97         DECIMAL(22,2);
   DEFINE v_sum_total_saldo_viv92         DECIMAL(22,2);
   DEFINE v_sum_total_fondo72             DECIMAL(22,2);
   DEFINE v_sum_total_aivs_viv97          DECIMAL(26,6);
   DEFINE v_sum_total_aivs_viv92          DECIMAL(26,6);

   --Variables para el manejo de cifras globales
   DEFINE v_det_total_viv97               DECIMAL(22,2);
   DEFINE v_det_total_viv92               DECIMAL(22,2);
   DEFINE v_det_total_fondo72             DECIMAL(22,2);
   DEFINE v_det_total_aivs97              DECIMAL(26,6);
   DEFINE v_det_total_aivs92              DECIMAL(26,6);
   
   DEFINE v_diferencia                    DECIMAL(26,6);
   DEFINE v_tolera                        DECIMAL(26,6);
   DEFINE v_total_detalle_temp            INTEGER;
   
   DROP TABLE IF EXISTS cbd_saldo_bdnsviv;
   DROP TABLE IF EXISTS cbd_detalle_bdnsviv;
   DROP TABLE IF EXISTS cbd_duplicado_bdnsviv;
   DROP TABLE IF EXISTS tmp_duplicado;
   DROP TABLE IF EXISTS tmp_diagnostico;
   
   CREATE TABLE cbd_detalle_bdnsviv
   (
      nss                   char(11)   NOT NULL,
      id_derechohabiente    decimal(9,0)  ,
      rfc                   char(13)  ,
      curp                  char(18)  ,
      ap_paterno            char(40)  ,
      ap_materno            char(40)  ,
      nombre                char(40)  ,
      acciones92            decimal(22,6)  ,
      acciones97            decimal(22,6)  ,
      cve_afore             char(3)  
   )FRAGMENT BY ROUND ROBIN IN bdnsv_1_dbs,bdnsv_2_dbs,bdnsv_3_dbs,bdnsv_4_dbs
   extent size 256000 next size 32000 lock mode row;

   CREATE TABLE cbd_saldo_bdnsviv
   (
      nss                   char(11),
      id_derechohabiente    decimal(9,0),
      subcuenta             smallint,
      monto_acciones        decimal(22,6),
      monto_pesos           decimal(18,2)  
   )FRAGMENT BY ROUND ROBIN IN bdnsv_1_dbs,bdnsv_2_dbs,bdnsv_3_dbs,bdnsv_4_dbs
   extent size 128000 next size 16000 lock mode row;

   CREATE TABLE cbd_duplicado_bdnsviv
   (
      nss                   char(11)   NOT NULL,
      id_derechohabiente    decimal(9,0)  ,
      rfc                   char(13)  ,
      curp                  char(18)  ,
      ap_paterno            char(40)  ,
      ap_materno            char(40)  ,
      nombre                char(40)  ,
      acciones92            decimal(22,6)  ,
      acciones97            decimal(22,6)  ,
      cve_afore             char(3)   
   )FRAGMENT BY ROUND ROBIN IN bdnsv_1_dbs,bdnsv_2_dbs,bdnsv_3_dbs,bdnsv_4_dbs
   lock mode row;


   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   SET PDQPRIORITY HIGH;

   --Se actualizan estadisticas de la tabla de detalles
   UPDATE statistics FOR TABLE safre_tmp:tmp_cbd_bdnsviv2;

   --Se obtiene la fecha de operación del encabezado (SIEMPRE SE TENDRA UN SOLO REGISTRO PARA EL ENCABEZADO)
   SELECT
      f_operacion
   INTO
      v_cza_f_operacion
   FROM safre_tmp:tmp_cbd_bdnsviv1;

   --Se establece la fecha de corte a partir de la fecha de operacion del archivo
   LET v_f_corte = v_cza_f_operacion;
   IF (MONTH(v_f_corte) = MONTH(v_f_corte + 1)) THEN
      --Significa que la fecha de corte no es fin de mes por lo que se establece la fecha de corte como el ultimo dia natural del mes anterior
      LET v_f_corte = MDY(MONTH(v_cza_f_operacion),1,YEAR(v_cza_f_operacion)) - 1;
   END IF

   --Se valida que el no exista un archivo con la misma fecha de corte
   SELECT folio
   INTO v_folio_ant
   FROM cbd_cza_bdnsviv
   WHERE f_corte = v_f_corte AND estado <> 9;
   
   IF(v_folio_ant IS NOT NULL) THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Ya se cargo un archivo BDNSVIV con fecha de corte " || 
                       TO_CHAR(v_f_corte,'%d-%m-%Y') || ", El folio asignado para el proceso anterior es: " || v_folio_ant;

      RETURN v_resultado, v_mensaje;
   END IF

   INSERT INTO cbd_cza_bdnsviv VALUES(p_folio,
                                     v_cza_f_operacion,
                                     v_f_corte,
                                     1);

   --SALDOS
   SELECT
      (SUM(saldo_viv97)/100),
      (SUM(saldo_viv92)/100),
      (SUM(aivs_viv97)/1000000),
      (SUM(aivs_viv92)/1000000)
   INTO
      v_det_total_viv97,
      v_det_total_viv92,
      v_det_total_aivs97,
      v_det_total_aivs92
   FROM safre_tmp:tmp_cbd_bdnsviv2;

   --- se cuentan los registros
   SELECT COUNT(nss)
   INTO v_total_detalle_temp
   FROM safre_tmp:tmp_cbd_bdnsviv2;

   SELECT
      total_registros,
      (total_saldo_viv97/100),
      (total_saldo_viv92/100),
      (total_aivs_viv97/1000000),
      (total_aivs_viv92/1000000)
   INTO
      v_sum_total_registros,
      v_sum_total_saldo_viv97,
      v_sum_total_saldo_viv92,
      v_sum_total_aivs_viv97,
      v_sum_total_aivs_viv92
   FROM safre_tmp:tmp_cbd_bdnsviv9;

   IF (v_sum_total_registros <> v_total_detalle_temp) THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR en el Sumario: El total de registros en el sumario no corresponde al numero de detalles";

      --Se actualiza el encabezado con estado de error
      UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
      
      RETURN v_resultado, v_mensaje;
   END IF

   --Validacion de viv92
   IF (v_sum_total_saldo_viv92 <> v_det_total_viv92) THEN
      --Se saca la tolerancia equivalente al 1%
      LET v_diferencia = v_sum_total_saldo_viv92 - v_det_total_viv92;
      IF v_diferencia < 1 THEN
         LET v_diferencia = v_diferencia * -1;
      END IF
      LET v_tolera = v_sum_total_saldo_viv92 * 0.01;

      IF v_diferencia > v_tolera THEN
         --LET v_resultado = 1;
         --LET v_mensaje = "ERROR en el Sumario: El monto del sumario en vivienda 92 no corresponde a la suma de los detalles, deferencia = " || v_diferencia;

         --UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
         --RETURN v_resultado, v_mensaje;
      END IF
      --LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario en vivienda 92";
   END IF

   --Validacion de viv97
   IF (v_sum_total_saldo_viv97 <> v_det_total_viv97) THEN
      --Se saca la tolerancia equivalente al 1%
      LET v_diferencia = v_sum_total_saldo_viv97 - v_det_total_viv97;
      IF v_diferencia < 1 THEN
         LET v_diferencia = v_diferencia * -1;
      END IF
      LET v_tolera = v_sum_total_saldo_viv97 * 0.01;

      IF v_diferencia > v_tolera THEN
         --LET v_resultado = 1;
         --LET v_mensaje = "ERROR en el Sumario: El monto del sumario en vivienda 97 no corresponde a la suma de los detalles, deferencia = " || v_diferencia;

         --UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
         --RETURN v_resultado, v_mensaje;
      END IF
      --LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario de vivienda 97";
   END IF

   --Validacion de aivs92
   IF (v_sum_total_aivs_viv92 <> v_det_total_aivs92) THEN
      --Se saca la tolerancia equivalente al 1%
      LET v_diferencia = v_sum_total_aivs_viv92 - v_det_total_aivs92;
      IF v_diferencia < 1 THEN
         LET v_diferencia = v_diferencia * -1;
      END IF
      LET v_tolera = v_sum_total_aivs_viv92 * 0.01;

      IF v_diferencia > v_tolera THEN
         LET v_resultado = 1;
         LET v_mensaje = "ERROR en el Sumario: El monto del sumario en AIVS 92 no corresponde a la suma de los detalles, deferencia = " || v_diferencia;

         UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
         RETURN v_resultado, v_mensaje;
      END IF
      LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario de AIVS 92";
   END IF

   --Validacion de aivs97
   IF (v_sum_total_aivs_viv97 <> v_det_total_aivs97) THEN
      --Se saca la tolerancia equivalente al 1%
      LET v_diferencia = v_sum_total_aivs_viv97 - v_det_total_aivs97;
      IF v_diferencia < 1 THEN
         LET v_diferencia = v_diferencia * -1;
      END IF
      LET v_tolera = v_sum_total_aivs_viv97 * 0.01;

      IF v_diferencia > v_tolera THEN
         LET v_resultado = 1;
         LET v_mensaje = "ERROR en el Sumario: El monto del sumario en AIVS 97 no corresponde a la suma de los detalles, deferencia = " || v_diferencia;

         UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
         RETURN v_resultado, v_mensaje;
      END IF
      LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario de AIVS 97";
   END IF

   --Se procesa la informacion de los detalles
   CREATE UNIQUE INDEX xpkcbd_detalle_bdnsviv ON cbd_detalle_bdnsviv (nss) USING btree IN bdnsv_1ix_dbs;
   START VIOLATIONS TABLE FOR cbd_detalle_bdnsviv USING tmp_duplicado, tmp_diagnostico;
   SET INDEXES FOR cbd_detalle_bdnsviv filtering;

   LOCK TABLE cbd_detalle_bdnsviv IN EXCLUSIVE MODE;

   --Primero se insertan los registros validos en SAFRE
   INSERT INTO cbd_detalle_bdnsviv
   SELECT
      det.nss,
      afi.id_derechohabiente,
      det.rfc,
      det.curp,
      det.ap_paterno,
      det.ap_materno,
      det.nombre,
      det.aivs_viv92/1000000,
      det.aivs_viv97/1000000,
      det.cve_afore
   FROM safre_tmp:tmp_cbd_bdnsviv2 det
   INNER JOIN afi_derechohabiente afi ON afi.nss = det.nss
   WHERE det.nss NOT IN ('           ','00000000000');

   --Se insertan los registros con nss's en blanco o con ceros
   INSERT INTO cbd_detalle_bdnsviv
   SELECT
      det.nss,
      '',
      det.rfc,
      det.curp,
      det.ap_paterno,
      det.ap_materno,
      det.nombre,
      det.aivs_viv92/1000000,
      det.aivs_viv97/1000000,
      det.cve_afore
   FROM safre_tmp:tmp_cbd_bdnsviv2 det
   WHERE det.nss IN ('           ','00000000000');

   --Se insertan los registros sin cuenta en safre
   INSERT INTO cbd_detalle_bdnsviv
   SELECT
      det.nss,
      '',
      det.rfc,
      det.curp,
      det.ap_paterno,
      det.ap_materno,
      det.nombre,
      det.aivs_viv92/1000000,
      det.aivs_viv97/1000000,
      det.cve_afore
   FROM safre_tmp:tmp_cbd_bdnsviv2 det
   WHERE det.nss NOT IN ('           ','00000000000')
   AND det.nss NOT IN (SELECT nss FROM afi_derechohabiente);

   UNLOCK TABLE cbd_detalle_bdnsviv;
   
   SET INDEXES FOR cbd_detalle_bdnsviv enabled;
   STOP VIOLATIONS TABLE FOR cbd_detalle_bdnsviv;
   CREATE INDEX IX_1_cbd_detalle_bdnsviv ON cbd_detalle_bdnsviv(id_derechohabiente)IN bdnsv_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_detalle_bdnsviv;

   --Se separan los registros duplicados
   INSERT INTO cbd_duplicado_bdnsviv
   SELECT
      det.nss,
      det.id_derechohabiente,
      det.rfc,
      det.curp,
      det.ap_paterno,
      det.ap_materno,
      det.nombre,
      det.acciones92,
      det.acciones97,
      det.cve_afore
   FROM cbd_detalle_bdnsviv det
   WHERE det.nss IN (SELECT dup.nss FROM tmp_duplicado dup);
   
   INSERT INTO cbd_duplicado_bdnsviv
   SELECT
      nss,
      id_derechohabiente,
      rfc,
      curp,
      ap_paterno,
      ap_materno,
      nombre,
      acciones92,
      acciones97,
      cve_afore
   FROM tmp_duplicado;

   DELETE FROM cbd_detalle_bdnsviv WHERE nss IN (SELECT nss FROM tmp_duplicado);

   CREATE INDEX ix_cbd_duplicado_bdnsviv ON cbd_duplicado_bdnsviv (nss) USING btree IN bdnsv_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_duplicado_bdnsviv;

   DROP TABLE IF EXISTS tmp_duplicado;
   DROP TABLE IF EXISTS tmp_diagnostico;

   --Se insertan los saldos
   INSERT INTO cbd_saldo_bdnsviv
   SELECT
      det.nss,
      cbd.id_derechohabiente,
      4,
      det.aivs_viv97/1000000,
      det.saldo_viv97/100
   FROM safre_tmp:tmp_cbd_bdnsviv2 det
   INNER JOIN cbd_detalle_bdnsviv cbd ON cbd.nss = det.nss;
   
   INSERT INTO cbd_saldo_bdnsviv
   SELECT
      det.nss,
      cbd.id_derechohabiente,
      8,
      det.aivs_viv92/1000000,
      det.saldo_viv92/100
   FROM safre_tmp:tmp_cbd_bdnsviv2 det
   INNER JOIN cbd_detalle_bdnsviv cbd ON cbd.nss = det.nss;

   CREATE INDEX IX_2_cbd_saldo_bdnsviv ON cbd_saldo_bdnsviv(id_derechohabiente)IN bdnsv_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_saldo_bdnsviv;

   --Se procesa la informacion del sumario (SIEMPRE SE TENDRA UN SOLO REGISTRO PARA EL SUMARIO)
   --TRACE("Recuperando la informacion del sumario...");
   SELECT
      f_operacion,
      total_registros,
      (total_saldo_viv97/100),
      (total_saldo_viv92/100),
      (total_fondo72/100),
      (total_aivs_viv97/1000000),
      (total_aivs_viv92/1000000)
   INTO
      v_sum_f_operacion,
      v_sum_total_registros,
      v_sum_total_saldo_viv97,
      v_sum_total_saldo_viv92,
      v_sum_total_fondo72,
      v_sum_total_aivs_viv97,
      v_sum_total_aivs_viv92
   FROM safre_tmp:tmp_cbd_bdnsviv9;

   
   --Aqui se inserta el registro del archivo
   INSERT INTO cbd_sum_bdnsviv VALUES(p_folio,
                                       v_sum_total_saldo_viv97,
                                       v_sum_total_saldo_viv92,
                                       v_sum_total_aivs_viv97,
                                       v_sum_total_aivs_viv92,
                                       v_sum_total_registros);

   IF (v_resultado = 0) THEN
      UPDATE cbd_cza_bdnsviv SET estado = 2 WHERE folio = p_folio;
   ELSE
      UPDATE cbd_cza_bdnsviv SET estado = 9 WHERE folio = p_folio;
   END IF

   --Se llena la tabla con el resumen de saldos a conciliar
   INSERT INTO cbd_saldo_conciliacion
   SELECT 
   v_f_corte,
   subcuenta,
   SUM(monto_acciones),
   2
   FROM cbd_saldo_bdnsviv
   WHERE id_derechohabiente is not null
   GROUP BY 2;
   
   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


