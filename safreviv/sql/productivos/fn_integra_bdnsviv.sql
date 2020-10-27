






CREATE FUNCTION "safreviv".fn_integra_bdnsviv(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0))
RETURNING SMALLINT,VARCHAR(200),INTEGER,INTEGER;

   DEFINE v_folio_ant         DECIMAL(9,0);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);
   DEFINE v_total_rechazos    INTEGER;
   DEFINE v_total_registros   INTEGER;

   --Variables para el manejo del encabezado
   DEFINE v_cza_tpo_registro              CHAR(2);
   DEFINE v_cza_id_servicio               CHAR(2);
   DEFINE v_cza_id_operacion              CHAR(2);
   DEFINE v_cza_tpo_ent_origen            CHAR(2);
   DEFINE v_cza_cve_ent_origen            CHAR(3);
   DEFINE v_cza_tpo_ent_destino           CHAR(2);
   DEFINE v_cza_cve_ent_destino           CHAR(3);
   DEFINE v_cza_f_operacion               DATE;

   --Variables para el manejo del detalle
   DEFINE v_det_tpo_registro              CHAR(2);
   DEFINE v_det_id_operacion              CHAR(2);
   DEFINE v_det_nss                       CHAR(11);
   DEFINE v_det_saldo_viv97               DECIMAL(22,2);
   DEFINE v_det_saldo_viv92               DECIMAL(22,2);
   DEFINE v_det_saldo_fondo72             DECIMAL(22,2);
   DEFINE v_det_aivs_viv97                DECIMAL(26,6);
   DEFINE v_det_aivs_viv92                DECIMAL(26,6);
   DEFINE v_det_tipo_trabajador       		CHAR(1);
   DEFINE v_det_cve_afore             		CHAR(3);
   DEFINE v_det_dif_afore_procanase   		CHAR(1);
   DEFINE v_det_ind_credito_vivienda  		CHAR(1);
   DEFINE v_det_ind_credito_43bis     		CHAR(1);
   DEFINE v_det_ind_unificacion       		CHAR(1);
   DEFINE v_det_ind_separacion        		CHAR(1);
   DEFINE v_det_ind_retiro            		CHAR(1);
   DEFINE v_det_ind_traspaso_9297     		CHAR(1);
   DEFINE v_det_ind_afore_afore       		CHAR(1);
   DEFINE v_det_ind_dev_pagos         		CHAR(1);
   DEFINE v_det_ind_trans_acreditados 		CHAR(1);
   DEFINE v_det_ind_acr_43bis_prev    		CHAR(1);
   DEFINE v_det_ind_acr_43bis_gar     		CHAR(1);
   DEFINE v_det_ind_acr_43bis_sub     		CHAR(1);

   DEFINE v_det_error                     INTEGER;
   DEFINE v_total_detalle_temp            INTEGER;
   
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
   DEFINE v_det_registros97               INTEGER;
   DEFINE v_det_registros92               INTEGER;
   DEFINE v_det_registros72               INTEGER;

   
   DEFINE v_diferencia                    DECIMAL(26,6);
   DEFINE v_tolera                        DECIMAL(26,6);

   DEFINE v_inf_total_viv97               DECIMAL(22,2);
   DEFINE v_inf_total_viv92               DECIMAL(22,2);
   DEFINE v_inf_total_fondo72             DECIMAL(22,2);
   DEFINE v_inf_total_aivs97              DECIMAL(26,6);
   DEFINE v_inf_total_aivs92              DECIMAL(26,6);
   DEFINE v_inf_registros97               INTEGER;
   DEFINE v_inf_registros92               INTEGER;
   DEFINE v_inf_registros72               INTEGER;

   DEFINE v_f_corte                       DATE;

   SET PDQPRIORITY HIGH;
   
   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/bdnsviv/debug_bdnsviv_"|| p_pid || ".txt");

   --TRACE("Se inicia la carga del archivo de saldos de vivienda");
   --TRACE("Usuario que ejecuta: "|| p_usuario_cod);
   --TRACE("Folio: " ||  p_folio);
   --TRACE("PID: " || p_pid);
   --TRACE("Nombre del Archivo: " || p_nombre_archivo);

   --TRACE("Creando la tabla de detalle...");
   DROP TABLE IF EXISTS cbd_detalle_bdnsviv CASCADE ;

   CREATE TABLE cbd_detalle_bdnsviv
   (
      nss                   char(11)   NOT NULL,
      saldo_viv97           decimal(22,2)  ,
      saldo_viv92           decimal(22,2)  ,
      saldo_fondo72         decimal(22,2)  ,
      aivs_97               decimal(26,6)  ,
      aivs_92               decimal(26,6)  ,
      tipo_trabajador       char(1)  ,
      cve_afore             char(3)  ,
      dif_afore_procanase   char(1)  ,
      ind_credito_vivienda  char(1)  ,
      ind_credito_43bis     char(1)  ,
      ind_unificacion       char(1)  ,
      ind_separacion        char(1)  ,
      ind_retiro            char(1)  ,
      ind_traspaso_9297     char(1)  ,
      ind_afore_afore       char(1)  ,
      ind_dev_pagos         char(1)  ,
      ind_trans_acreditados char(1)  ,
      ind_acr_43bis_prev    char(1)  ,
      ind_acr_43bis_gar     char(1)  ,
      ind_acr_43bis_sub     char(1)  
   ) 
   FRAGMENT BY ROUND ROBIN IN bdnsv_1_dbs,bdnsv_2_dbs,bdnsv_3_dbs,bdnsv_4_dbs;


   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   --Se inicializa el conteo de rechazos
   LET v_total_rechazos = 0;

   --Se inicializa el conteo de registros aceptados
   LET v_total_registros = 0;
   LET v_det_registros97 = 0;
   LET v_det_registros92 = 0;
   LET v_det_registros72 = 0;

   --Se inicializan los sumarios de saldos
   LET v_det_total_viv97 = 0;
   LET v_det_total_viv92 = 0;
   LET v_det_total_fondo72 = 0;
   LET v_det_total_aivs97 = 0;
   LET v_det_total_aivs92 = 0;

   --Primero se validan las cifras globales

   --SALDOS
   SELECT
      (SUM(saldo_viv97))/100,
      (SUM(saldo_viv92))/100,
      (SUM(aivs_viv97))/1000000,
      (SUM(aivs_viv92))/1000000
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
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;

      --TRACE(v_mensaje);
      --TRACE("v_sum_total_registros = " || v_sum_total_registros);
      --TRACE("v_total_detalle_temp = " || v_total_detalle_temp);

      DELETE FROM  cbd_cza_bdnsviv WHERE folio = p_folio;
      DROP TABLE IF EXISTS cbd_detalle_bdnsviv CASCADE ;
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
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
         LET v_resultado = 1;
         LET v_mensaje = "ERROR en el Sumario: El monto del sumario en vivienda 92 no corresponde a la suma de los detalles";
         LET v_total_rechazos = 1;
         LET v_total_registros = 0;

         RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
      END IF
      LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario en vivienda 92";
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
         LET v_resultado = 1;
         LET v_mensaje = "ERROR en el Sumario: El monto del sumario en vivienda 97 no corresponde a la suma de los detalles";
         LET v_total_rechazos = 1;
         LET v_total_registros = 0;
         
         RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
      END IF
      LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario de vivienda 92";
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
         LET v_total_rechazos = 1;
         LET v_total_registros = 0;
         
         RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
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
         LET v_total_rechazos = 1;
         LET v_total_registros = 0;
         
         RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
      END IF
      LET v_mensaje = "El archivo se integro correctamente pero existe una diferencia de " || v_diferencia || " en el sumario de AIVS 92";
   END IF

   --Se inicializa el conteo de registros aceptados
   LET v_total_registros = 0;
   LET v_det_registros97 = 0;
   LET v_det_registros92 = 0;
   LET v_det_registros72 = 0;

   --Se inicializan los sumarios de saldos
   LET v_det_total_viv97 = 0;
   LET v_det_total_viv92 = 0;
   LET v_det_total_fondo72 = 0;
   LET v_det_total_aivs97 = 0;
   LET v_det_total_aivs92 = 0;

   --Se procesa la informacion del encabezado (SIEMPRE SE TENDRA UN SOLO REGISTRO PARA EL ENCABEZADO)
   --TRACE("Recuperando la informacion del encabezado...");
   SELECT
      tpo_registro, 
      id_servicio, 
      id_operacion,
      tpo_ent_origen,
      cve_ent_origen, 
      tpo_ent_destino, 
      cve_ent_destino, 
      f_operacion
   INTO
      v_cza_tpo_registro,
      v_cza_id_servicio,
      v_cza_id_operacion,
      v_cza_tpo_ent_origen,
      v_cza_cve_ent_origen,
      v_cza_tpo_ent_destino,
      v_cza_cve_ent_destino,
      v_cza_f_operacion
   FROM safre_tmp:tmp_cbd_bdnsviv1;

   --TRACE("Inicia la validación del encabezado...");
   
   IF(v_cza_tpo_registro <> "01") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Tipo de Registro incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_id_servicio <> "12") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Identificador de Servicio incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_id_operacion <> "02") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Identificador de Operación incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_tpo_ent_origen <> "03") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Tipo Entidad Origen incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_cve_ent_origen <> "001") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Clave Entidad Origen incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_tpo_ent_destino <> "04") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Tipo Entidad Destino incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   IF(v_cza_cve_ent_destino <> "002") THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Clave Entidad Destino incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF

   --Se establece la fecha de corte a partir de la fecha de operacion del archivo
   LET v_f_corte = v_cza_f_operacion;
   IF (MONTH(v_f_corte) = MONTH(v_f_corte + 1)) THEN
      --Significa que la fecha de corte no es fin de mes por lo que se establece la fecha de corte como el ultimo dia natural del mes anterior
      LET v_cza_f_operacion = MDY(MONTH(v_f_corte),1,YEAR(v_f_corte)) - 1;
   END IF

   --Se valida que el no exista un archivo con la misma fecha de corte
   SELECT folio
   INTO v_folio_ant
   FROM cbd_cza_bdnsviv
   WHERE f_operacion = v_cza_f_operacion
   ;
   
   IF(v_folio_ant IS NOT NULL) THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Encabezado: Ya se cargo un archivo BDNSVIV con fecha de corte " || 
                       v_cza_f_operacion || ", El folio asignado para el proceso anterior es: " || v_folio_ant;
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);

      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF
   
   --TRACE("Finaliza la validación del encabezado");

   INSERT INTO cbd_cza_bdnsviv VALUES(p_folio,
                                     v_cza_f_operacion);

   --Se procesa la informacion de los detalles
   --TRACE("Inicia la carga de los detalles...");
   FOREACH
      SELECT
         tpo_registro,
         id_operacion,
         nss,
         (saldo_viv97/100),
         (saldo_viv92/100),
         (saldo_fondo72/100),
         (aivs_viv97/1000000),
         (aivs_viv92/1000000),
         tipo_trabajador,
         cve_afore,
         dif_afore_procanase,
         ind_credito_vivienda,
         ind_credito_43bis,
         ind_unificacion,
         ind_separacion,
         ind_retiro,
         ind_traspaso_9297,
         ind_afore_afore,
         ind_dev_pagos,
         ind_trans_acreditados,
         ind_acr_43bis_prev,
         ind_acr_43bis_gar,
         ind_acr_43bis_sub
      INTO
         v_det_tpo_registro,
         v_det_id_operacion,
         v_det_nss,
         v_det_saldo_viv97,
         v_det_saldo_viv92,
         v_det_saldo_fondo72,
         v_det_aivs_viv97,
         v_det_aivs_viv92,
         v_det_tipo_trabajador,
         v_det_cve_afore,
         v_det_dif_afore_procanase,
         v_det_ind_credito_vivienda,
         v_det_ind_credito_43bis,
         v_det_ind_unificacion,
         v_det_ind_separacion,
         v_det_ind_retiro,
         v_det_ind_traspaso_9297,
         v_det_ind_afore_afore,
         v_det_ind_dev_pagos,
         v_det_ind_trans_acreditados,
         v_det_ind_acr_43bis_prev,
         v_det_ind_acr_43bis_gar,
         v_det_ind_acr_43bis_sub
      FROM safre_tmp:tmp_cbd_bdnsviv2

      LET v_det_error = 0;

      IF(v_det_tpo_registro <> "02")THEN
         LET v_det_error = 1;
         --TRACE("Se rechazara el registro de detalle del nss " || v_det_nss || " por ERROR de validación: Tipo Registro incorrecto");
      END IF
      
      IF(v_det_error = 0 AND v_det_id_operacion <> "02")THEN
         LET v_det_error = 1;
         --TRACE("Se rechazara el registro de detalle del nss " || v_det_nss || " por ERROR de validación: Identificador de Operación incorrecto");
      END IF

      IF(v_det_error = 0 AND LENGTH(TRIM(v_det_nss)) <> 11)THEN
         LET v_det_error = 1;
         --TRACE("Se rechazara el registro de detalle del nss " || v_det_nss || " por ERROR de validación: NSS incorrecto");
      END IF

      IF(v_det_error = 0 AND v_det_tipo_trabajador NOT IN (1,2))THEN
         LET v_det_error = 1;
         --TRACE("Se rechazara el registro de detalle del nss " || v_det_nss || " por ERROR de validación: Tipo de Trabajador incorrecto");
      END IF

      IF(v_det_error = 1)THEN
         --Significa que el registro no paso las validaciones y se rechazara
         LET v_total_rechazos = v_total_rechazos + 1;
      ELSE
         INSERT INTO cbd_detalle_bdnsviv VALUES(  v_det_nss,
                                                            v_det_saldo_viv97,
                                                            v_det_saldo_viv92,
                                                            v_det_saldo_fondo72,
                                                            v_det_aivs_viv97,
                                                            v_det_aivs_viv92,
                                                            v_det_tipo_trabajador,
                                                            v_det_cve_afore,
                                                            v_det_dif_afore_procanase,
                                                            v_det_ind_credito_vivienda,
                                                            v_det_ind_credito_43bis,
                                                            v_det_ind_unificacion,
                                                            v_det_ind_separacion,
                                                            v_det_ind_retiro,
                                                            v_det_ind_traspaso_9297,
                                                            v_det_ind_afore_afore,
                                                            v_det_ind_dev_pagos,
                                                            v_det_ind_trans_acreditados,
                                                            v_det_ind_acr_43bis_prev,
                                                            v_det_ind_acr_43bis_gar,
                                                            v_det_ind_acr_43bis_sub);
         LET v_total_registros = v_total_registros + 1;
         IF v_det_aivs_viv97 > 0 THEN
            LET v_det_registros97 = v_det_registros97 + 1;
         END IF
         IF v_det_aivs_viv92 > 0 THEN
            LET v_det_registros92 = v_det_registros92 + 1;
         END IF
         IF v_det_saldo_fondo72 > 0 THEN
            LET v_det_registros72 = v_det_registros72 + 1;
         END IF
        
         IF v_det_saldo_viv97 is not null  THEN 
            LET v_det_total_viv97 = v_det_total_viv97 + v_det_saldo_viv97;
         END IF
         IF v_det_saldo_viv92 is not null  THEN 
            LET v_det_total_viv92 = v_det_total_viv92 + v_det_saldo_viv92;
         END IF
         IF v_det_saldo_fondo72 is not null  THEN 
            LET v_det_total_fondo72 = v_det_total_fondo72 + v_det_saldo_fondo72;
         END IF
         IF v_det_aivs_viv97 is not null  THEN 
            LET v_det_total_aivs97 = v_det_total_aivs97 + v_det_aivs_viv97;
         END IF
         IF v_det_aivs_viv92 is not null  THEN 
            LET v_det_total_aivs92 = v_det_total_aivs92 + v_det_aivs_viv92;
         END IF
      END IF
      
   END FOREACH;

   CREATE UNIQUE INDEX XPKcbd_detalle_bdnsviv ON cbd_detalle_bdnsviv(nss ASC)IN bdnsv_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_detalle_bdnsviv;


   --TRACE("Termina la carga de los detalles");
   --TRACE("Total de registros integrados: " || v_total_registros);
   --TRACE("Total de rechazos: " || v_total_rechazos);

   IF(v_total_rechazos > 0)THEN
      LET v_resultado = 2;
      LET v_mensaje = "Se termino la carga de los detalles pero se rechazaron registros por errores de validacion";
      --TRACE(v_mensaje);
   END IF

   --Se procesa la informacion del sumario (SIEMPRE SE TENDRA UN SOLO REGISTRO PARA EL SUMARIO)
   --TRACE("Recuperando la informacion del sumario...");
   SELECT
      tpo_registro,
      f_operacion,
      total_registros,
      (total_saldo_viv97/100),
      (total_saldo_viv92/100),
      (total_fondo72/100),
      (total_aivs_viv97/1000000),
      (total_aivs_viv92/1000000)
   INTO
      v_sum_tpo_registro,
      v_sum_f_operacion,
      v_sum_total_registros,
      v_sum_total_saldo_viv97,
      v_sum_total_saldo_viv92,
      v_sum_total_fondo72,
      v_sum_total_aivs_viv97,
      v_sum_total_aivs_viv92
   FROM safre_tmp:tmp_cbd_bdnsviv9;

   --TRACE("Se inicia la validacion del sumario...");
   IF(v_sum_tpo_registro <> "09")THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR de validación en el Sumario: Tipo de Registro incorrecto";
      LET v_total_rechazos = 1;
      LET v_total_registros = 0;
      --TRACE(v_mensaje);

      DELETE FROM  cbd_cza_bdnsviv WHERE folio = p_folio;
      DROP TABLE IF EXISTS cbd_detalle_bdnsviv CASCADE ;
      
      RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
   END IF
   
   --Aqui se inserta el registro del archivo
   INSERT INTO cbd_sum_bdnsviv VALUES(p_folio,
                                                v_sum_total_saldo_viv97,
                                                v_sum_total_saldo_viv92,
                                                v_sum_total_aivs_viv97,
                                                v_sum_total_aivs_viv92,
                                                v_sum_total_registros);

   --Se insertan los registros de las cifras de conciliacion globales
   --Vivienda 97
   SELECT
      monto_acciones,
      monto_pesos
   INTO
      v_inf_total_aivs97,
      v_inf_total_viv97
   FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global
   WHERE subcuenta = 4
   AND fondo_inversion = 11
   AND f_saldo = v_cza_f_operacion;
   
   INSERT INTO cbd_cifras_concilia_global VALUES(p_folio,
                                                            4,
                                                            v_inf_total_aivs97,
                                                            v_inf_total_viv97,
                                                            0,
                                                            v_det_total_aivs97,
                                                            v_det_total_viv97,
                                                            v_det_registros97);

   --Vivienda 92
   SELECT
      monto_acciones,
      monto_pesos
   INTO
      v_inf_total_aivs92,
      v_inf_total_viv92
   FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global
   WHERE subcuenta = 8
   AND fondo_inversion = 11
   AND f_saldo = v_cza_f_operacion;
   
   INSERT INTO cbd_cifras_concilia_global VALUES(p_folio,
                                                            8,
                                                            v_inf_total_aivs92,
                                                            v_inf_total_viv92,
                                                            0,
                                                            v_det_total_aivs92,
                                                            v_det_total_viv92,
                                                            v_det_registros92);

   --Fondo 72
   SELECT
      monto_pesos
   INTO
      v_inf_total_fondo72
   FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global
   WHERE subcuenta = 40
   AND fondo_inversion = 11
   AND f_saldo = v_cza_f_operacion;

   --SELECT
   --  COUNT(DISTINCT id_derechohabiente)
   --INTO
   --   v_inf_registros72
   --FROM safre_sdo@vivws_tcp:cta_saldo_mensual
   --WHERE subcuenta = 40
   --AND fondo_inversion = 11;
   INSERT INTO cbd_cifras_concilia_global VALUES(p_folio,
                                                            40,
                                                            0,
                                                            v_inf_total_fondo72,
                                                            0,
                                                            0,
                                                            v_det_total_fondo72,
                                                            v_det_registros72);

   --TRACE("Se finaliza la carga del archivo...");
   --TRACE("Resultado: " || v_resultado);
   --TRACE("Mensaje de respuesta: " || v_mensaje);
   --TRACE("Total de rechazos: " || v_total_rechazos);
   --TRACE("Total de registros integrados: " || v_total_registros);

   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje, v_total_rechazos, v_total_registros;
END FUNCTION;


