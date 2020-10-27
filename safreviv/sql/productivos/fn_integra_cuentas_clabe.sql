






CREATE FUNCTION "selefp".fn_integra_cuentas_clabe( p_folio           DECIMAL(9,0),
                                          p_nombre_archivo  VARCHAR(40), 
                                          p_pid             DECIMAL(9,0), 
                                          p_usuario_cod     CHAR(20),
                                          p_script          VARCHAR(60))
RETURNING SMALLINT,VARCHAR(200);
 
   --Variables de salida
   DEFINE v_respuesta         SMALLINT; 
   DEFINE v_mensaje           VARCHAR(200);

   DEFINE v_valida_masivo     DECIMAL(9,0);

   --Variables para manejr el negocio
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_id_afi_fondo72    DECIMAL(9,0);
   DEFINE v_nss               CHAR(11);
   DEFINE v_clabe             CHAR(18);
   DEFINE v_nombre            CHAR(60);
   DEFINE v_monto             DECIMAL(14,2);
   DEFINE v_h_registro        DATETIME HOUR TO SECOND;
   DEFINE v_estado            SMALLINT;
   DEFINE v_rechazo           SMALLINT;

   --Variables para las validaciones
   DEFINE v_unico             SMALLINT;
   DEFINE v_valida_indice     VARCHAR(30);
   DEFINE v_saldo_tmp         DECIMAL(14,2);
   DEFINE v_id_afi_tmp        DECIMAL(9,0);
   DEFINE v_nombre_tmp        CHAR(60);

   --Validacion datamart
   DEFINE v_max_sec_pension   SMALLINT;
   DEFINE v_diag_registro     SMALLINT;
   DEFINE v_tpo_prestacion    CHAR(2);
   DEFINE v_tpo_pension       CHAR(2);
   DEFINE v_porcentaje        DECIMAL(5,2);

   --Validacion Credito
   DEFINE v_resultado         SMALLINT; 
   DEFINE v_tpo_originacion   SMALLINT;
   DEFINE v_tpo_credito       SMALLINT;
   DEFINE v_num_credito       DECIMAL(10,0); 
   DEFINE v_f_otorga          DATE; 
   DEFINE v_f_liquida         DATE;

   --Se inicializan las variables de respuesta
   LET v_respuesta = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   DROP TABLE IF EXISTS ret_cta_clabe_dia;
   DROP TABLE IF EXISTS ret_cta_clabe_vio;
   DROP TABLE IF EXISTS ret_cta_clabe;
   CREATE TABLE ret_cta_clabe
   (
      nss char(11) not null ,
      clabe char(18),
      nom_titular char(40),
      banco char(3)
   ) in ret_dbs  extent size 16 next size 16 lock mode page;
   create unique index xpkret_cta_clabe on ret_cta_clabe  (nss) using btree  in ret_ix_dbs;
   create unique index xak1ret_cta_clabe on ret_cta_clabe (clabe) using btree  in ret_ix_dbs;
   --create unique index xak2ret_cta_clabe on ret_cta_clabe (nss,clabe) using btree  in ret_ix_dbs;

   SET PDQPRIORITY HIGH;
   
   --Se activa el filtro por indice
   START VIOLATIONS TABLE FOR ret_cta_clabe;
   SET INDEXES FOR ret_cta_clabe filtering;

   INSERT INTO ret_cta_clabe
   SELECT
   nss,
   cta_clabe,
   nombre,
   cta_clabe[1,3]
   FROM safre_tmp:tmp_ret_cta_clabe;

   SET INDEXES FOR ret_cta_clabe enabled;
   UPDATE statistics FOR TABLE ret_cta_clabe;
   STOP VIOLATIONS TABLE FOR ret_cta_clabe;

   --Despues de esto se tiene la tabla de cuentas clave llena
   --los registros que se duplican quedan en la tabla ret_cta_clabe_vio
   --el tipo de indice que no paso esta en la tabla ret_cta_clabe_dia

   --se genera un indice por nss para la tabla de duplicados
   create index xpkret_cta_clabe_vio on ret_cta_clabe_vio  (nss) using btree  in ret_ix_dbs;
   create index xpkret_cta_clabe_dia on ret_cta_clabe_dia (informix_tupleid, objtype, objowner) using btree  in ret_ix_dbs;
   UPDATE statistics FOR TABLE ret_cta_clabe_vio;
   UPDATE statistics FOR TABLE ret_cta_clabe_dia;

   SELECT FIRST 1 
   id_solicitud
   INTO v_valida_masivo
   FROM ret_fondo_ahorro_masivo;

   IF (v_valida_masivo IS NULL OR v_valida_masivo = 0) THEN
      --Si no se encuentran registros en la tabla del masivo se calcula todo el universo
      FOREACH
         SELECT
            nss,
            clabe
         INTO
            v_nss,
            v_clabe
         FROM ret_cta_clabe

         --Primero se busca el NSS en afi_fondo72 y se valida que sea unico
         LET v_monto = 0;
         LET v_unico = 0;
         FOREACH
            SELECT
               id_afi_fondo72,
               nombre
            INTO
               v_id_afi_fondo72,
               v_nombre
            FROM afi_fondo72
            WHERE nss = v_nss

            LET v_unico = v_unico + 1;
         END FOREACH;

         IF (v_unico > 1) THEN
            --Se agrega la regla de negocio de ignorar las cuentas con saldo <= 0
            LET v_unico = 0;
            LET v_saldo_tmp = 0;
            FOREACH
               SELECT
                  afi.id_afi_fondo72,
                  afi.nombre,
                  SUM(cta.importe)
               INTO
                  v_id_afi_tmp,
                  v_nombre_tmp,
                  v_saldo_tmp
               FROM afi_fondo72 afi
               INNER JOIN cta_fondo72 cta ON cta.id_afi_fondo72 = afi.id_afi_fondo72
               WHERE afi.nss = v_nss
               GROUP BY afi.id_afi_fondo72, afi.nombre

               IF v_saldo_tmp > 0 THEN
                  LET v_id_afi_fondo72 = v_id_afi_tmp;
                  LET v_nombre = v_nombre_tmp;
                  LET v_unico = v_unico + 1;
               END IF
            END FOREACH;
         END IF

         IF (v_unico = 1) THEN
            --Si v_unico = 1 significa que solo se encontró un registro para el NSS
            --Primero validamos que solo exista un NSS para cada cuenta clabe y que las cuentas clabes sean unicas
            SELECT FIRST 1
               ind.objname
            INTO
               v_valida_indice
            FROM ret_cta_clabe_vio dup
            INNER JOIN ret_cta_clabe_dia ind ON (ind.informix_tupleid = dup.informix_tupleid 
                                   AND ind.objtype = dup.informix_optype
                                   AND ind.objowner = dup.informix_recowner)
            WHERE dup.nss = v_nss;
            IF (v_valida_indice IS NOT NULL)THEN
               LET v_estado = 100;  --Solicitud Rechazada
               IF (v_valida_indice = 'xpkret_cta_clabe') THEN
                  --En este caso el NSS tiene mas de una cuenta clabe
                  LET v_rechazo = 150;
               ELSE 
                  --En este caso la cuenta clabe no es unica
                  LET v_rechazo = 151;
               END IF
            ELSE  --Fin v_valida_indice IS NOT NULL
               --Si v_valida_indice es nulo entonces el registro paso la validacion de cuenta clabe

               --Se valida en el datamart
               SELECT
                  MAX(sec_pension)
               INTO 
                  v_max_sec_pension
               FROM ret_datamart
               WHERE nss = v_nss;

               SELECT FIRST 1
                  afi.id_derechohabiente,
                  ret.diag_registro,
                  ret.tpo_prestacion,
                  ret.tpo_pension,
                  ret.porcentaje_valuacion
               INTO
                  v_id_derechohabiente,
                  v_diag_registro,
                  v_tpo_prestacion,
                  v_tpo_pension,
                  v_porcentaje
               FROM ret_datamart ret
               LEFT JOIN afi_derechohabiente afi ON afi.nss = ret.nss
               WHERE  ret.nss = v_nss
               AND ret.sec_pension = v_max_sec_pension;

               IF (v_tpo_prestacion IS NULL OR v_tpo_prestacion = '03') THEN
                  --No cuenta con resolucon valida en datamart
                  LET v_estado = 100;  --Solicitud Rechazada
                  LET v_rechazo = 90;  --NO EXISTE RESOLUCION EN SPESS
               ELSE     --FIN no cuenta con resolucion de pension valida
                  --Se encontro una resolucion de pension
                  --Se valida el tipo
                  IF (v_tpo_pension = 'IP' AND v_porcentaje < 50) THEN
                     --Por el tipo de pension se rechaza por porcentaje menor a 50%
                     LET v_estado = 100;  --Solicitud Rechazada
                     LET v_rechazo = 140; --PORCENTAJE DE VALUACION MENOR AL 50% 
                  ELSE  --FIN incapacidad con porcentaje menor a 50%
                     --En este punto ya se valido que el NSS cuenta con una resulucion en spess corecta

                     --En caso de que el NSS no se encuentre en afi_derechohabiente se omite la validacion de credito
                     IF (v_id_derechohabiente IS NULL) THEN
                        LET v_resultado = 1;
                     ELSE
                        --Se valida el credito
                        CALL fn_credito_vivienda (v_id_derechohabiente, 0)
                        RETURNING v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida;
                     END IF

                     IF (v_resultado = 0) THEN     -- 0 significa que tiene credito vigente
                        LET v_estado = 100;  --Solicitud Rechazada
                        LET v_rechazo = 20; --DH EN PROCESO DE ACREDITADO
                     ELSE     --FIN NSS con credito
                        --Aqui los NSS ya pasaron las validaciones de pension y credito
                        
                        --Ahora se busca el saldo
                        SELECT
                           SUM(importe)
                        INTO
                           v_monto
                        FROM cta_fondo72
                        WHERE id_afi_fondo72 = v_id_afi_fondo72;

                        IF (v_monto IS NULL OR v_monto <= 0) THEN    --SALDO <= 0
                           LET v_monto = 0; 
                           LET v_estado = 100;  --Solicitud Rechazada
                           LET v_rechazo = 10;  --DH CON SALDO INSUFICIENTE
                        ELSE  --FIN saldo <=0
                           --Estos registros son los que entran en el programa de pago masivo
                           LET v_estado = 65;  --Solicitud Aceptada (SOLICITUD PENDIENTE DE PAGO)
                           LET v_rechazo = NULL;
                        END IF      --Fin v_monto > 0
                     END IF   --FIN NSS SIN credito
                  END IF   --FIN registro con resolicion en SPESS correcta
               END IF   --FIN Resolucion de datamart valida para pension 
            END IF   --FIN Cuenta clabe valida
            --INSERTAR LA SOLICITUD
            LET v_h_registro = CURRENT HOUR TO SECOND;
            INSERT INTO ret_fondo_ahorro_masivo VALUES (seq_ret_solicitud.NEXTVAL,        --id_solicitud
                                                        v_id_afi_fondo72,                 --id_afi_fondo72
                                                        v_nss,                            --nss
                                                        v_clabe,                          --clabe
                                                        v_monto,                          --importe_viv72
                                                        TODAY,                            --f_proceso
                                                        v_h_registro,                     --h_proceso
                                                        p_usuario_cod,                    --usuario
                                                        v_estado,                         --estado_solicitud
                                                        v_rechazo);                       --cod_rechazo
         END IF      --FIN NSS Unico en SAFRE
      END FOREACH;

      --Despues de generar el universo se crea un archivo de salida con la siguiente estructura
      --NSS				      CHAR(11)			Número de Seguridad Social									
      --RFC				      CHAR(13)			Registro Federal del Contribuyente     									
      --Nombre				   CHAR(60)			Nombre del trabajador									
      --Monto del pago		DECIMAL(14,2)	Monto a pagar									
      --Estatus de Pago		NUMÉRICO			"EFP lo genera con estatus de pago igual a 0.
      system "dbaccess safre_viv " || p_script;

      --Se actualiza el estado del primer pago para que se active el pago
      UPDATE ret_ctr_pago_masivo SET estado_pago = 1 WHERE num_pago = 1;
      UPDATE ret_ctr_pago_masivo SET estado_pago = 0 WHERE num_pago <> 1;

   ELSE     --FIN Tabla de masivo vacia
      --Si ya existen registros en la tabla del masivo solo se tiene que actualizar las clabes de los rechazos por banco
      LET v_mensaje = "El archivo se integro correctamente sin modificar el universo";
   END IF   --FIN Tabla de masivo llena

   SET PDQPRIORITY DEFAULT;
    
   RETURN v_respuesta, v_mensaje;
END FUNCTION;


