






CREATE FUNCTION "safreviv".fn_reevalua_masivo_fondo72()
RETURNING SMALLINT,VARCHAR(200);
 
   --Variables de salida
   DEFINE v_respuesta         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   DEFINE v_valida_masivo     DECIMAL(9,0);

   --Variables para manejr el negocio
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_id_afi_fondo72    DECIMAL(9,0);
   DEFINE v_id_solicitud      DECIMAL(9,0);
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
   DEFINE v_num_pago          SMALLINT;
   DEFINE v_pago_anterior     SMALLINT;

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
   

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de respuesta
   LET v_respuesta = 0;
   LET v_mensaje = "El proceso se ejecuto correctamente...";

   SELECT 
      MAX(num_pago)
   INTO 
      v_pago_anterior
   FROM ret_ctr_pago_masivo
   WHERE estado_pago = 2;

   IF (v_pago_anterior = 3) THEN
      LET v_respuesta = 1;
      LET v_mensaje = "El programa de pago masivo ya concluyo porque ya se realizaron los 3 pagos programados";
   END IF

   --Se validan todos los registros que fueron rechazados por SAFRE
   FOREACH
      SELECT
         ret.id_solicitud,
         ret.id_afi_fondo72,
         ret.nss,
         cve.clabe
      INTO
         v_id_solicitud,
         v_id_afi_fondo72,
         v_nss,
         v_clabe
      FROM ret_fondo_ahorro_masivo ret
      INNER JOIN ret_cta_clabe cve ON cve.nss = ret.nss
      WHERE ret.estado_solicitud <> 71                                  --se omiten los registros pagados
      --AND cod_rechazo NOT BETWEEN 300 and 399                           --se omiten los rechazos del banco
      --WHERE ret.estado_solicitud NOT IN (65,66,71)                                  --pendiente, reenvio, pagado
      --AND ret.cod_rechazo NOT IN (152,153,301,302,303,305,306,313,314,315,316,317,318,319)  --rechazo de ADS o Rechazos del banco
      --AND ret.nss[11] <= v_pago_anterior

      LET v_monto = 0;
      
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
               --En este punto ya se valido que el NSS cuenta con una resulucion en spess correcta

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
                     LET v_estado = 66;  --Solicitud Aceptada (SOLICITUD PENDIENTE DE REENVIO PARA PAGO MASIVO)
                     LET v_rechazo = NULL;
                  END IF      --Fin v_monto > 0
               END IF   --FIN NSS SIN credito
            END IF   --FIN registro con resolicion en SPESS correcta
         END IF   --FIN Resolucion de datamart valida para pension 
      END IF   --FIN Cuenta clabe valida
      --ACTUALIZA LA SOLICITUD
      LET v_h_registro = CURRENT HOUR TO SECOND;
      UPDATE ret_fondo_ahorro_masivo
      SET   clabe = v_clabe,
            importe_viv72 = v_monto,
            f_proceso = TODAY,
            h_proceso = v_h_registro,
            estado_solicitud = v_estado,
            cod_rechazo = v_rechazo
      WHERE id_solicitud = v_id_solicitud;
   END FOREACH;

   --Se activa el siguiente pago para SIAFF
   SELECT
      MIN(num_pago)
   INTO
      v_num_pago
   FROM ret_ctr_pago_masivo
   WHERE estado_pago = 0;

   UPDATE ret_ctr_pago_masivo SET estado_pago = 1 WHERE num_pago = v_num_pago;

   SET PDQPRIORITY DEFAULT;
    
   RETURN v_respuesta, v_mensaje;
END FUNCTION;


