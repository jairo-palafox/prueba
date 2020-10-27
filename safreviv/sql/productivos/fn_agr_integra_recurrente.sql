






CREATE FUNCTION "safreviv".fn_agr_integra_recurrente(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                          p_folio             DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER,INTEGER, CHAR(11);

   -- REGISTRO de la temporal
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10);
   DEFINE v_rt_ssv_92_97            DECIMAL(8);
   DEFINE v_rt_fec_otorgamiento     DATE;
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_credito          DECIMAL(3);
   DEFINE v_rt_sts_credito          DECIMAL(3);
   DEFINE v_rt_tpo_descuento        DECIMAL(1);
   DEFINE v_rt_val_descuento        DECIMAL(8);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(8);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2);

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE cre_folio_liquida         DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_tpo_originacion       SMALLINT;
   DEFINE cre_tpo_credito           SMALLINT;
   DEFINE cre_tpo_registro          CHAR(2);
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_sdo_deudor            DECIMAL(22,2);
   DEFINE cre_f_otorga              DATE;
   DEFINE cre_f_culmina             DATE;
   DEFINE cre_edo_credito           SMALLINT;
   DEFINE cre_tpo_dscto             SMALLINT;
   DEFINE cre_valor_dscto           DECIMAL(8,4);
   DEFINE cre_nrp                   CHAR(11);
   DEFINE cre_f_ini_dscto           DATE;
   DEFINE cre_nss_liberado          CHAR(11);
   DEFINE cre_f_gen_arh             DATE;
   DEFINE cre_sdo_credito           DECIMAL(22,2);
   DEFINE cre_f_prox_liq            DATE;
   DEFINE cre_f_desde               DATE;
   DEFINE cre_f_hasta               DATE;
   DEFINE cre_tpo_rch               SMALLINT;
   DEFINE cre_edo_procesar          SMALLINT;
   DEFINE cre_estado                SMALLINT;

   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE rch_nss                   CHAR(11);
   DEFINE rch_tpo_originacion       SMALLINT;
   DEFINE rch_tpo_credito           SMALLINT;
   DEFINE rch_tpo_registro          CHAR(2);
   DEFINE rch_num_credito           DECIMAL(10,0);
   DEFINE rch_sdo_deudor            DECIMAL(22,2);
   DEFINE rch_f_otorga              DATE;
   DEFINE rch_f_culmina             DATE;
   DEFINE rch_edo_credito           SMALLINT;
   DEFINE rch_tpo_dscto             SMALLINT;
   DEFINE rch_valor_dscto           DECIMAL(8,4);
   DEFINE rch_nrp                   CHAR(11);
   DEFINE rch_f_ini_dscto           DATE;
   DEFINE rch_nss_liberado          CHAR(11);
   DEFINE rch_f_gen_arh             DATE;
   DEFINE rch_sdo_credito           DECIMAL(22,2);
   DEFINE rch_f_prox_liq            DATE;
   DEFINE rch_f_desde               DATE;
   DEFINE rch_f_hasta               DATE;
   DEFINE rch_tpo_rch               SMALLINT;
   DEFINE rch_estado                SMALLINT;

   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador      CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt          INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech          INTEGER; -- contador de registros rechazados
   DEFINE v_ax_exist_derech_vig     SMALLINT; -- indica si el derechohabiente existe como vigente
   DEFINE v_ax_exist_numcred        INTEGER; -- contador de registros auxiliar
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar
   DEFINE v_ax_sdo_cred_aux         DECIMAL(22,2); -- saldo del crédito
   DEFINE v_ax_ssv_92_97            DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto          DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion      SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_credito          SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito           SMALLINT; -- identificador del crédito
   DEFINE v_error                   SMALLINT; -- en caso de error contiene el código
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_edo_nci                 SMALLINT;
   DEFINE v_entidad                 SMALLINT;
   DEFINE v_ax_nss                  CHAR(11);
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE v_f_proceso               DATE;
   DEFINE v_tpo_operacion           CHAR(3);
   DEFINE verror                    SMALLINT; -- código de error en caso de excepción

   -- parámetros de la función de desmarca
   DEFINE des_id_derechohabiente    DECIMAL(9,0);
   DEFINE des_marca_entra           SMALLINT;
   DEFINE des_n_referencia          INTEGER;
   DEFINE des_estado_marca          SMALLINT;
   DEFINE des_marca_causa           SMALLINT;
   DEFINE des_usuario               CHAR(20);
   DEFINE v_ax_marca_inf            SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc            SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_transferencia    CHAR(2);  -- tipo de transferencia
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_ax_cod_error            SMALLINT;

    ON EXCEPTION SET v_error
      -- verifica si el error se debe a:
      -- 239 Could not insert new row - duplicate value in a UNIQUE INDEX column
      --Inicializa valor de la variable auxiliar
      --LET v_ax_cuenta_rech = 0;

      IF v_error = -239 THEN
         -- se inserta el registro en la tabla de rechazos con estado = 17
         LET v_ax_sts_registro = 17;

         -- se incrementa el número de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usarán para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_f_otorga,
                     rch_f_culmina,
                     rch_edo_credito,
                     rch_tpo_dscto,
                     rch_valor_dscto,
                     rch_nrp,
                     rch_f_ini_dscto,
                     rch_nss_liberado,
                     rch_f_gen_arh,
                     rch_sdo_credito,
                     rch_f_prox_liq,
                     rch_f_desde,
                     rch_f_hasta,
                     rch_tpo_rch,
                     rch_estado);

         --Se integra tabla de rechazos
         INSERT INTO safre_tmp:tmp_marca_rechazo(nss,
                     tpo_credito,
                     estado)
             VALUES (rch_nss,
                     rch_tpo_credito,
                     3);
      ELSE
         -- Devolverá el código de error cuando ocurra una excepción diferente a -239
         RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech, v_rt_nss;
      END IF
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRecurr.trace';
   --SET DEBUG FILE TO '/safreviv_int/archivos/agrIntegRecurr.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt           = 0;
   LET v_ax_cuenta_rech           = 0;
   LET v_error                    = 0;
   LET v_ax_exist_derech_vig      = 0;
   LET v_ax_exist_numcred         = 0;
   LET rch_id_cre_ctr_archivo     = NULL;
   LET rch_nss                    = NULL; 
   LET rch_tpo_originacion        = NULL; 
   LET rch_tpo_credito            = NULL; 
   LET rch_tpo_registro           = NULL; 
   LET rch_num_credito            = NULL; 
   LET rch_sdo_deudor             = NULL; 
   LET rch_f_otorga               = NULL; 
   LET rch_f_culmina              = NULL; 
   LET rch_edo_credito            = NULL; 
   LET rch_tpo_dscto              = NULL; 
   LET rch_valor_dscto            = NULL; 
   LET rch_nrp                    = NULL; 
   LET rch_f_ini_dscto            = NULL; 
   LET rch_nss_liberado           = NULL; 
   LET rch_f_gen_arh              = NULL; 
   LET rch_sdo_credito            = NULL; 
   LET rch_f_prox_liq             = NULL; 
   LET rch_f_desde                = NULL; 
   LET rch_f_hasta                = NULL; 
   LET rch_tpo_rch                = NULL; 
   LET rch_estado                 = NULL; 
   LET v_rt_nss                   = NULL;
   LET v_ax_tpo_originacion       = NULL;
   LET v_rt_tpo_credito           = NULL;
   LET v_rt_tpo_registro          = NULL;
   LET v_rt_num_credito           = NULL;
   LET v_ax_ssv_92_97             = NULL;
   LET v_rt_fec_otorgamiento      = NULL;
   LET v_rt_fec_culminacion       = NULL;
   LET v_rt_sts_credito           = NULL;
   LET v_rt_tpo_descuento         = NULL;
   LET v_ax_valor_dscto           = NULL;
   LET v_rt_nrp                   = NULL;
   LET v_rt_fec_ini_oblig_patron  = NULL;
   LET v_rt_nss_liberado          = NULL;
   LET v_rt_fec_proceso           = NULL;
   LET v_ax_sdo_cred_aux          = NULL;
   LET v_rt_fec_prox_liquidar     = NULL;
   LET v_rt_fec_dsd_avis_desast   = NULL;
   LET v_rt_fec_hst_avis_desast   = NULL;
   LET v_rt_tpo_rechazo           = NULL;
   LET v_ax_sts_registro          = 10;
   LET des_id_derechohabiente     = NULL;
   LET des_marca_entra            = NULL;
   LET des_n_referencia           = NULL;
   LET des_estado_marca           = NULL;
   LET des_marca_causa            = NULL;
   LET des_usuario                = NULL;
   LET v_ax_marca_inf             = NULL;
   LET v_ax_marca_prc             = NULL;
   LET v_ax_tpo_transferencia     = NULL;
   LET v_edo_nci                  = 0;
   LET v_entidad                  = 0;
   LET v_ax_edo_procesar          = 10;
   LET v_f_proceso                = TODAY;
   LET v_tpo_operacion            = "EJ";

   --------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS INEXISTENTES EN CATÁLOGO --
   --------------------------------------------------------
   FOREACH
    SELECT *
      INTO v_rt_tpo_registro,
           v_rt_nss,
           v_rt_num_credito,
           v_rt_ssv_92_97,
           v_rt_fec_otorgamiento,
           v_rt_fec_culminacion,
           v_rt_tpo_credito,
           v_rt_sts_credito,
           v_rt_tpo_descuento,
           v_rt_val_descuento,
           v_rt_nrp,
           v_rt_fec_ini_oblig_patron,
           v_rt_nss_liberado,
           v_rt_fec_proceso,
           v_rt_sdo_credito,
           v_rt_fec_prox_liquidar,
           v_rt_fec_dsd_avis_desast,
           v_rt_fec_hst_avis_desast,
           v_rt_tpo_rechazo
      FROM safre_tmp:tmp_cre_acred_agr_01
     WHERE nss NOT IN (
           SELECT afi.nss
             FROM afi_derechohabiente afi
           INNER JOIN safre_tmp:tmp_cre_acred_agr_01 t
                   ON afi.nss = t.nss)

      -- se asigna el estatud de error
      LET v_ax_sts_registro = 11; -- 11 Derechohabiente no existe en maestro

      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      FOREACH
       SELECT FIRST 1 tpo_originacion
         INTO v_ax_tpo_originacion
         FROM cat_tipo_credito
        WHERE tpo_credito = v_rt_tpo_credito
          AND f_actualiza <= v_rt_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se incrementa el numero de registros rechazados
      LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = v_rt_sdo_credito / 100;
      LET v_ax_ssv_92_97    = v_rt_ssv_92_97 / 100;
      LET v_ax_valor_dscto  = v_rt_val_descuento / 10000;

      -- se asignan los valores en las variables que se usarán para insertar el registro
      LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
      LET rch_nss                = v_rt_nss;
      LET rch_tpo_originacion    = v_ax_tpo_originacion;
      LET rch_tpo_credito        = v_rt_tpo_credito;
      LET rch_tpo_registro       = v_rt_tpo_registro;
      LET rch_num_credito        = v_rt_num_credito;
      LET rch_sdo_deudor         = v_ax_ssv_92_97;
      LET rch_f_otorga           = v_rt_fec_otorgamiento;
      LET rch_f_culmina          = v_rt_fec_culminacion;
      LET rch_edo_credito        = v_rt_sts_credito;
      LET rch_tpo_dscto          = v_rt_tpo_descuento;
      LET rch_valor_dscto        = v_ax_valor_dscto;
      LET rch_nrp                = v_rt_nrp;
      LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
      LET rch_nss_liberado       = v_rt_nss_liberado;
      LET rch_f_gen_arh          = v_rt_fec_proceso;
      LET rch_sdo_credito        = v_ax_sdo_cred_aux;
      LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
      LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
      LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
      LET rch_tpo_rch            = v_rt_tpo_rechazo;
      LET rch_estado             = v_ax_sts_registro;

      -- se inserta registro
      INSERT INTO cre_rch_acreditado (
                  id_cre_ctr_archivo,
                  nss,
                  tpo_originacion,
                  tpo_credito,
                  tpo_registro,
                  num_credito,
                  sdo_deudor,
                  f_otorga,
                  f_culmina,
                  edo_credito,
                  tpo_dscto,
                  valor_dscto,
                  nrp,
                  f_ini_dscto,
                  nss_liberado,
                  f_gen_arh,
                  sdo_credito,
                  f_prox_liq,
                  f_desde,
                  f_hasta,
                  tpo_rch,
                  estado)
          VALUES (rch_id_cre_ctr_archivo,
                  rch_nss,
                  rch_tpo_originacion,
                  rch_tpo_credito,
                  rch_tpo_registro,
                  rch_num_credito,
                  rch_sdo_deudor,
                  rch_f_otorga,
                  rch_f_culmina,
                  rch_edo_credito,
                  rch_tpo_dscto,
                  rch_valor_dscto,
                  rch_nrp,
                  rch_f_ini_dscto,
                  rch_nss_liberado,
                  rch_f_gen_arh,
                  rch_sdo_credito,
                  rch_f_prox_liq,
                  rch_f_desde,
                  rch_f_hasta,
                  rch_tpo_rch,
                  rch_estado);

          --Se agrega la tabla de rechazos
          INSERT INTO safre_tmp:tmp_marca_rechazo
                     (nss,
                      tpo_credito,
                      estado)
              VALUES (rch_nss,
                      rch_tpo_credito,
                      3);
   END FOREACH;

   -----------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE NUEVOS ACREDITADOS --
   -----------------------------------------------------
   -- se obtienen los datos de la tabla temporal del proceso de Recurrente
   FOREACH
    SELECT t.*,
           a.id_derechohabiente,
           a.tipo_trabajador,
           a.nss
      INTO v_rt_tpo_registro,
           v_rt_nss,
           v_rt_num_credito,
           v_rt_ssv_92_97,
           v_rt_fec_otorgamiento,
           v_rt_fec_culminacion,
           v_rt_tpo_credito,
           v_rt_sts_credito,
           v_rt_tpo_descuento,
           v_rt_val_descuento,
           v_rt_nrp,
           v_rt_fec_ini_oblig_patron,
           v_rt_nss_liberado,
           v_rt_fec_proceso,
           v_rt_sdo_credito,
           v_rt_fec_prox_liquidar,
           v_rt_fec_dsd_avis_desast,
           v_rt_fec_hst_avis_desast,
           v_rt_tpo_rechazo,
           v_ax_id_derechohabiente,
           v_ax_tipo_trabajador,
           v_ax_nss
      FROM safre_tmp:tmp_cre_acred_agr_01 t,
           afi_derechohabiente a
     WHERE t.nss = a.nss
       AND t.sts_credito = 1

      -- se inicializan variables
      LET v_ax_tpo_credito = NULL;

      -- se consulta el tipo de crédito y el tipo originación para el tipo crédito
      FOREACH
       SELECT FIRST 1 tpo_credito, tpo_originacion
         INTO v_ax_tpo_credito, v_ax_tpo_originacion
         FROM cat_tipo_credito
        WHERE tpo_credito = v_rt_tpo_credito
          AND f_actualiza <= v_rt_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;

      CALL fn_consulta_marca_tramite(v_ax_id_derechohabiente, v_ax_nss)
      RETURNING verror, v_ax_id_derechohabiente, vcodResp, vdescResp;

      IF vcodResp[1] = "3" THEN
         IF vcodResp = "3000" THEN
            CALL fn_credito_ejercido_recurr(v_ax_id_derechohabiente,
                                            v_ax_nss,
                                            v_ax_tpo_credito,
                                            v_rt_num_credito,
                                            v_rt_fec_otorgamiento,
                                            v_rt_tpo_descuento,
                                            v_rt_val_descuento,
                                            v_rt_ssv_92_97,
                                            v_f_proceso,
                                            v_rt_nrp,
                                            v_tpo_operacion,
                                            p_d_id_cre_ctr_arch,
                                            p_folio,
                                            v_ax_tipo_trabajador)
            RETURNING verror, v_ax_nss, vcodResp, vdescResp, v_rt_num_credito, v_ax_tpo_credito;

            LET v_ax_edo_procesar     = 10;
            LET v_ax_exist_derech_vig = 0;
            LET v_ax_exist_numcred    = 0;

            CONTINUE FOREACH;
         ELSE
            LET v_ax_edo_procesar = 40;
         END IF
      ELSE
         IF vcodResp <> "2000" AND vcodResp <> "2213" THEN
            LET v_ax_edo_procesar = 40;
         ELSE
            IF vcodResp = "2213" THEN
               LET v_ax_edo_procesar = 50;
            END IF
         END IF
      END IF

      -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
      IF EXISTS (SELECT UNIQUE c.id_derechohabiente
                   FROM cre_acreditado c,
                        cat_maq_credito m
                  WHERE c.id_derechohabiente = v_ax_id_derechohabiente
                    AND c.estado = m.estado
                    AND m.entidad = 1) THEN
         -- se prende la bandera
         LET v_ax_exist_derech_vig = 1;
      ELSE
         -- se mantiene la bandera apagada
         LET v_ax_exist_derech_vig = 0;
      END IF

      -- se valida que el num crédito no exista en la tabla maestro para el mismo tipo de originación
      FOREACH
         SELECT c1.estado, c2.entidad
           INTO v_edo_nci, v_entidad
           FROM cre_acreditado c1, cat_maq_credito c2
          WHERE c1.tpo_originacion = v_ax_tpo_originacion
            AND c1.num_credito     = v_rt_num_credito
            AND c1.estado          = c2.estado
            AND c2.entidad         = 1
      END FOREACH;

      IF v_entidad = 1 THEN
         -- se prende la bandera
         LET v_ax_exist_numcred = 1; --existe nci vigente
      --ELSE
         --IF v_entidad = 2 THEN
            --LET v_ax_exist_numcred = 2; --existe nci liquidado
         --ELSE
         ---- se desactiva la bandera
            --LET v_ax_exist_numcred = 0; --no existe nsi
         --END IF
      END IF

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = v_rt_sdo_credito / 100;
      LET v_ax_ssv_92_97    = v_rt_ssv_92_97 / 100;
      LET v_ax_valor_dscto  = v_rt_val_descuento / 10000;

      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente no debe estar en catálogo (no debe ser nulo)
      -- * el estatus del crédito debe ser igual a 1
      -- * el tipo de crédito debe ser de originación 1 o 4 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL AND
         v_ax_tpo_credito IS NOT NULL AND
         v_ax_exist_derech_vig = 0 AND
         v_ax_exist_numcred = 0 AND
         (v_rt_tpo_registro IN ("01","20") AND  v_rt_sts_credito = 1) THEN

         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;

         -- se valida el tipo de trabajador
         IF v_ax_tipo_trabajador = "I" THEN
            LET v_ax_edo_procesar = 10;
         ELSE
            LET v_ax_edo_procesar = 7;
         END IF

         -- se asigna que el registro no fue rechazado
         IF v_rt_sts_credito = 3 THEN
            LET v_ax_sts_registro = 18;
         ELSE
            LET v_ax_sts_registro = 10;
         END IF

         --IF v_ax_exist_numcred = 2 THEN 
            ---- vigente deudor liquidado 
            --LET v_ax_sts_registro = 140;
         --END IF

         IF v_ax_edo_procesar = 40 THEN
             LET v_ax_sts_registro = 240;
         END IF

         -- se asignan los valores en las variables que se usarán para insertar el registro
         LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
         LET cre_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET cre_folio_liquida      = 0;
         LET cre_id_derechohabiente = v_ax_id_derechohabiente;
         LET cre_tpo_originacion    = v_ax_tpo_originacion;
         LET cre_tpo_credito        = v_rt_tpo_credito;
         LET cre_tpo_registro       = v_rt_tpo_registro;
         LET cre_num_credito        = v_rt_num_credito;
         LET cre_sdo_deudor         = v_ax_ssv_92_97;
         LET cre_f_otorga           = v_rt_fec_otorgamiento;
         LET cre_f_culmina          = v_rt_fec_culminacion;
         LET cre_edo_credito        = v_rt_sts_credito;
         LET cre_tpo_dscto          = v_rt_tpo_descuento;
         LET cre_valor_dscto        = v_ax_valor_dscto;
         LET cre_nrp                = v_rt_nrp;
         LET cre_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET cre_nss_liberado       = v_rt_nss_liberado;
         LET cre_f_gen_arh          = v_rt_fec_proceso;
         LET cre_sdo_credito        = v_ax_sdo_cred_aux;
         LET cre_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET cre_f_desde            = v_rt_fec_dsd_avis_desast;
         LET cre_f_hasta            = v_rt_fec_hst_avis_desast;
         LET cre_tpo_rch            = v_rt_tpo_rechazo;
         LET cre_edo_procesar       = v_ax_edo_procesar;
         LET cre_estado             = v_ax_sts_registro;

         -- se inserta registro en la tabla maestro
         INSERT INTO cre_acreditado (
                     id_cre_acreditado,
                     id_cre_ctr_archivo,
                     folio_liquida,
                     id_derechohabiente,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     edo_procesar,
                     estado)
             VALUES (cre_id_cre_acreditado,
                     cre_id_cre_ctr_archivo,
                     cre_folio_liquida,
                     cre_id_derechohabiente,
                     cre_tpo_originacion,
                     cre_tpo_credito,
                     cre_tpo_registro,
                     cre_num_credito,
                     cre_sdo_deudor,
                     cre_f_otorga,
                     cre_f_culmina,
                     cre_edo_credito,
                     cre_tpo_dscto,
                     cre_valor_dscto,
                     cre_nrp,
                     cre_f_ini_dscto,
                     cre_nss_liberado,
                     cre_f_gen_arh,
                     cre_sdo_credito,
                     cre_f_prox_liq,
                     cre_f_desde,
                     cre_f_hasta,
                     cre_tpo_rch,
                     cre_edo_procesar,
                     cre_estado);

         -- 06/07/2012 12:33:49 p.m. Todos se actualizarán con id credito igual a 1
         LET v_ax_id_credito = 1;

         -- se actualiza el registro de afi derechohabiente
         UPDATE afi_derechohabiente
            SET id_credito = v_ax_id_credito,
                f_credito = cre_f_otorga 
          WHERE id_derechohabiente = cre_id_derechohabiente;
      ELSE
         -- se valida la razón del rechazo
         IF v_ax_id_derechohabiente IS NULL THEN
            -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
            LET v_ax_sts_registro = 11;
         ELIF NOT v_rt_sts_credito IN (1,3) THEN
            -- se asigna status 15 en el registro "Estatus del crédito no válido"
            LET v_ax_sts_registro = 15;
         ELIF v_ax_tpo_credito IS NULL THEN
            -- se asigna status 16 en el registro "Tipo credito no válido"
            LET v_ax_sts_registro = 16;
         ELIF v_ax_exist_derech_vig > 0 THEN
            -- se asigna status 20 en el registro "Derechohabiente vigente"
            LET v_ax_sts_registro = 20;
         ELSE
            -- se asigna status 21 en el registro "Número de crédito vigente"
            LET v_ax_sts_registro = 21;
         END IF

         -- se incrementa el número de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usarán para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_f_otorga,
                     rch_f_culmina,
                     rch_edo_credito,
                     rch_tpo_dscto,
                     rch_valor_dscto,
                     rch_nrp,
                     rch_f_ini_dscto,
                     rch_nss_liberado,
                     rch_f_gen_arh,
                     rch_sdo_credito,
                     rch_f_prox_liq,
                     rch_f_desde,
                     rch_f_hasta,
                     rch_tpo_rch,
                     rch_estado);
                     
        --se agrega la tabla de rechazos
        INSERT INTO safre_tmp:tmp_marca_rechazo(nss,
                      tpo_credito,
                      estado)
              VALUES (rch_nss,
                      rch_tpo_credito,
                      3);
      END IF;

      -- cambio de estatus para los tipos 18
      IF v_rt_tpo_registro = "20" AND v_rt_sts_credito IN (1, 9) THEN
         IF EXISTS ( SELECT id_derechohabiente
                       FROM cre_acreditado
                      WHERE id_derechohabiente = v_ax_id_derechohabiente
                        AND tpo_credito = v_rt_tpo_credito
                        AND num_credito = v_rt_num_credito
                        AND tpo_registro = 1
                        AND edo_credito = 3
                        AND estado = 18
                   ) THEN

            FOREACH
               SELECT FIRST 1 id_cre_acreditado
                 INTO v_id_cre_acreditado
                 FROM cre_acreditado
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_credito = v_rt_tpo_credito
                  AND num_credito = v_rt_num_credito
                  AND tpo_registro = 1
                  AND edo_credito = 3
                  AND estado = 18
            END FOREACH;

            UPDATE cre_acreditado
               SET estado = 19
             WHERE id_cre_acreditado = v_id_cre_acreditado;

            IF v_rt_sts_credito = 9 THEN
               -- se obtiene las marcas y el tipo de transferencia para el tipo de credito en proceso
               SELECT marca_inf, marca_prc, DECODE(id_proceso,201,"03",1201, "16", 301,"43")
                 INTO v_ax_marca_inf, v_ax_marca_prc, v_ax_tpo_transferencia
                 FROM cat_tipo_credito
                WHERE tpo_credito = v_rt_tpo_credito
                  AND tpo_originacion = v_ax_tpo_originacion;

               SELECT usuario
                 INTO v_usuario
                 FROM cre_ctr_archivo
                WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;

               -- se asignan los valores para la función de desmarca
               LET des_id_derechohabiente = v_ax_id_derechohabiente;
               LET des_marca_entra        = v_ax_marca_inf;
               LET des_n_referencia       = v_id_cre_acreditado;
               LET des_estado_marca       = 0;
               LET des_marca_causa        = 0;
               LET des_usuario            = v_usuario;

               EXECUTE FUNCTION fn_desmarca_cuenta(v_ax_id_derechohabiente,
                                                   des_marca_entra,
                                                   des_n_referencia,
                                                   des_estado_marca,
                                                   des_marca_causa,
                                                   des_usuario,
                                                   301)--parametro de entrada
                                              INTO v_ax_cod_error;
            END IF
         END IF
      END IF

      -- se inicializan variables
      LET v_ax_ssv_92_97          = 0;
      LET v_rt_ssv_92_97          = 0;
      LET des_id_derechohabiente  = NULL;
      LET des_marca_entra         = NULL;
      LET des_n_referencia        = NULL;
      LET des_estado_marca        = NULL;
      LET des_marca_causa         = NULL;
      LET des_usuario             = NULL;
      LET v_ax_marca_inf          = NULL;
      LET v_ax_marca_prc          = NULL;
      LET v_ax_tpo_transferencia  = NULL;
      LET v_id_cre_acreditado     = NULL;
      LET v_ax_exist_derech_vig   = 0;
      LET v_ax_exist_numcred      = 0;
      LET v_edo_nci               = 0;
      LET v_entidad               = 0;

   END FOREACH;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_acreditado;
   UPDATE STATISTICS FOR TABLE cre_rch_acreditado;

   RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech, v_rt_nss;

END FUNCTION
;


