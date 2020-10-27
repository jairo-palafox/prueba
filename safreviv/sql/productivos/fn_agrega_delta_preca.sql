






CREATE FUNCTION "safreviv".fn_agrega_delta_preca()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_fcorte               DATE;
   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);
   DEFINE v_estado_ant           SMALLINT;
   DEFINE v_inserta              SMALLINT;
   DEFINE v_fvalida              DATE;

   DEFINE f_preca                DATE;
   DEFINE v_fproceso_preca       DATE;
   DEFINE v_estado_preca         SMALLINT;

   DEFINE v_nss                  CHAR(11);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_ap_paterno           CHAR(50);
   DEFINE v_ap_materno           CHAR(50);
   DEFINE v_nombre               CHAR(50);
   DEFINE v_f_apertura           DATE;
   DEFINE v_tipo_trabajador      CHAR(1);
   
   --Se establece la fecha de corte como el dia natural inmediato anterior
   LET v_fcorte = TODAY - 1;

   LET v_resultado = 1;    --Significa que el proceso se ejecuto correctamente
   LET v_mensaje = "Las cuentas de trabajadores del estado aperturadas el dia " || v_fcorte || " se agregaron correctamente";

   SET PDQPRIORITY HIGH;

    --Se valida si no se ha generado el saldo a la fecha de corte
   SELECT
      estado_genera
   INTO
      v_estado_ant
   FROM safre_sdo@vivws_tcp:glo_ctr_saldo
   WHERE tpo_saldo = 4                    --tpo_saldo = 4 significa que se agregan nuevas cuentas a la tabla de saldos para precalificacion
   AND f_saldo = v_fcorte
   ;

   --Proceso en ejecucion
   IF(v_estado_ant = 1) THEN
      LET v_resultado = 2;
      LET v_mensaje = "El proceso que agrega las nuevas cuentas ya esta en proceso";
      RETURN v_resultado, v_mensaje;
   END IF

   --El saldo ya fue generado
   IF(v_estado_ant = 2) THEN
      LET v_resultado = 2;
      LET v_mensaje = "Las cuentas aperturadas el dia " || v_fcorte || " ya fueron agregadas";
      RETURN v_resultado, v_mensaje;
   END IF

    --El proceso anterior genero algun error
   IF(v_estado_ant = 3) THEN
      DELETE FROM safre_sdo@vivws_tcp:glo_ctr_saldo WHERE tpo_saldo = 4 AND f_saldo = v_fcorte;
      DELETE FROM cta_saldo_preca WHERE f_valor = v_fcorte;
   END IF

   --Se busca la ultima fecha de corte para el proceso masivo de precalificacion
   SELECT
   MAX(f_saldo)
   INTO f_preca
   FROM safre_sdo@vivws_tcp:glo_ctr_saldo
   WHERE tpo_saldo = 3;

   IF(f_preca = v_fcorte) THEN
      LET v_resultado = 3;
      LET v_mensaje = "El proceso no se puede ejecutar porque la fecha de corte es la misma que el proceso masivo que prepara el archivo de precalificacion";
      RETURN v_resultado, v_mensaje;
   END IF

   SELECT
      estado_genera
   INTO
      v_estado_preca
   FROM safre_sdo@vivws_tcp:glo_ctr_saldo
   WHERE tpo_saldo = 3                    --tpo_saldo = 3 Corresponde al proceso masivo de precalificacion
   AND f_saldo = f_preca
   ;

    --Proceso en ejecucion
   IF(v_estado_preca = 1) THEN
      LET v_resultado = 4;
      LET v_mensaje = "El proceso no se puede ejecutar porque existe un proceso masivo para precalificación en ejecución";
      RETURN v_resultado, v_mensaje;
   END IF

   --Proceso masivo con error
   IF(v_estado_preca = 3) THEN
      LET v_resultado = 5;
      LET v_mensaje = "El proceso no se puede ejecutar porque el ultimo proceso masivo para precalificación termino con error";
      RETURN v_resultado, v_mensaje;
   END IF

   LET v_fproceso_preca = f_preca + 1;

   --Se inserta el registro inicial en el control de saldo diario
   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_saldo VALUES(v_fcorte, 1, 4);

   --Inserta cada registro nuevo en la tabla preparada para precalificacion
   FOREACH
      SELECT 
         nss,
         rfc,
         curp,
         ap_paterno,
         ap_materno,
         nombre,
         f_apertura,
         tipo_trabajador
      INTO
         v_nss,
         v_rfc,
         v_curp,
         v_ap_paterno,
         v_ap_materno,
         v_nombre,
         v_f_apertura,
         v_tipo_trabajador
      FROM afi_nuevo_trabajador

      IF (v_f_apertura = v_fproceso_preca) THEN
         --Si la fecha de apertura es el mismo dia que la fecha de ejecucion del proceso masivo debemos validar que el NSS no alla sido agregado
         SELECT FIRST 1
            f_proceso
         INTO
            v_fvalida
         FROM cta_saldo_preca
         WHERE nss = v_nss;

         IF (v_fvalida IS NULL) THEN
            --Si la fecha es nula se inserta el registro porque el nss aun no esta en la tabla de preparacion
            LET v_inserta = 1;
         ELSE
            --Si la fecha no es nula NO se inserta porque el nss ya fue agregado a la tabla de preparacion en el proceso masivo
            LET v_inserta = 0;
         END IF
      ELSE
         --Para fechas de apertura distintas a la fecha del proceso masivo siempre se agregaran los NSS's
         LET v_inserta = 1;
      END IF

      IF (v_inserta = 1) THEN
         IF (v_curp IS NULL) THEN
            LET v_curp = ' ';
         END IF
         IF (v_ap_materno IS NULL) THEN
            LET v_ap_materno = ' ';
         END IF
         INSERT INTO cta_saldo_preca VALUES (v_nss,
                                             TODAY,
                                             v_rfc,
                                             v_curp,
                                             v_ap_paterno,
                                             v_ap_materno,
                                             v_nombre,
                                             4,
                                             0,
                                             v_f_apertura,
                                             '000',
                                             v_tipo_trabajador);
         INSERT INTO cta_saldo_preca VALUES (v_nss,
                                             TODAY,
                                             v_rfc,
                                             v_curp,
                                             v_ap_paterno,
                                             v_ap_materno,
                                             v_nombre,
                                             8,
                                             0,
                                             v_f_apertura,
                                             '000',
                                             v_tipo_trabajador);
      END IF
   END FOREACH;

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

      --Finaliza la operacion
   UPDATE safre_sdo@vivws_tcp:glo_ctr_saldo SET estado_genera = 2 WHERE tpo_saldo = 4 AND f_saldo = v_fcorte;

RETURN v_resultado, v_mensaje;
END FUNCTION;


