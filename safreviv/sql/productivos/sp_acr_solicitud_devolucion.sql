






CREATE PROCEDURE "safreviv".sp_acr_solicitud_devolucion(p_id_lote        DECIMAL(10,0),
                                             p_lote           SMALLINT,
                                             p_f_lote         DATE,
                                             p_f_presentacion DATE,
                                             p_f_movimiento   DATE,
                                             p_id_lote_dev    CHAR(16),
                                             g_usuario_cod    CHAR(20))

   --Table: dse_agrupa_devolucion
   DEFINE v_agrupa_id_dse_grp_devolucion  DECIMAL(9,0);
   DEFINE v_agrupa_id_derechohabiente     DECIMAL(9,0);
   DEFINE v_agrupa_num_credito            DECIMAL(10,0);
   DEFINE v_agrupa_tpo_transferencia      CHAR(2);
   DEFINE v_agrupa_origen_devolucion      CHAR(2);
   DEFINE v_agrupa_f_movimiento           DATE;
   DEFINE v_agrupa_folio_liquida          DECIMAL(9,0);
   DEFINE v_agrupa_aivs97                 DECIMAL(16,6);
   DEFINE v_agrupa_pesos97                DECIMAL(12,2);
   DEFINE v_agrupa_aivs92                 DECIMAL(16,6);
   DEFINE v_agrupa_pesos92                DECIMAL(12,2);
   DEFINE v_agrupa_monto_aportacion       DECIMAL(12,2);
   DEFINE v_agrupa_aivs_aportacion        DECIMAL(16,6);
   DEFINE v_agrupa_nss_separacion         CHAR(11);
   DEFINE v_agrupa_edo_procesar           SMALLINT;
   DEFINE v_agrupa_estado                 SMALLINT;

   --Variables para la tabla: afi_derechohabiente
   DEFINE v_afi_curp                      CHAR(18);
   DEFINE v_afi_nss                       CHAR(11);
   DEFINE v_afi_rfc                       CHAR(13);
   DEFINE v_afi_ap_paterno_af             CHAR(40);
   DEFINE v_afi_ap_materno_af             CHAR(40);
   DEFINE v_afi_nombre_af                 CHAR(40);
   DEFINE v_afi_nombre_imss               CHAR(50);

   --Variables para la tabla: tmp_rep_devolucion y tmp_ctr_devolucion
   DEFINE v_tmp_curp                      CHAR(18);
   DEFINE v_tmp_nss                       CHAR(11);
   DEFINE v_tmp_rfc                       CHAR(13);
   DEFINE v_tmp_ap_paterno_af             CHAR(40);
   DEFINE v_tmp_ap_materno_af             CHAR(40);
   DEFINE v_tmp_nombre_af                 CHAR(40);
   DEFINE v_tmp_aivs97                    DECIMAL(16,6);
   DEFINE v_tmp_pesos97                   DECIMAL(12,2);
   DEFINE v_tmp_nss_separacion            CHAR(11);
   DEFINE v_tmp_origen_devolucion         CHAR(2);
   DEFINE v_tmp_monto_aportacion          DECIMAL(12,2);
   DEFINE v_tmp_aivs_aportacion           DECIMAL(16,6);
   DEFINE v_tmp_aivs92                    DECIMAL(16,6);
   DEFINE v_tmp_pesos92                   DECIMAL(12,2);
   DEFINE v_tmp_nom_imss                  CHAR(50);
   DEFINE v_tmp_num_credito               DECIMAL(10,0);
   DEFINE v_tmp_edo_procesar              SMALLINT;

   --Variables auxiliares
   DEFINE v_valor_fondo                   DECIMAL(19,14);
   DEFINE v_edo_procesar_aux              SMALLINT;
   DEFINE v_f_solic_saldos_aux            DATE;

   --Variables provenientes de la tabla: tmp_rep_devolucion
   DEFINE v_tmp_curp_r                   CHAR(18);
   DEFINE v_tmp_nss_r                    CHAR(11);
   DEFINE v_tmp_rfc_r                    CHAR(13);
   DEFINE v_tmp_ap_paterno_af_r          CHAR(40);
   DEFINE v_tmp_ap_materno_af_r          CHAR(40);
   DEFINE v_tmp_nombre_af_r              CHAR(40);
   DEFINE v_tmp_aivs97_r                 DECIMAL(16,6);
   DEFINE v_tmp_pesos97_r                DECIMAL(12,2);
   DEFINE v_tmp_nss_separacion_r         CHAR(11);
   DEFINE v_tmp_origen_devolucion_r      CHAR(2);
   DEFINE v_tmp_monto_aportacion_r       DECIMAL(12,2);
   DEFINE v_tmp_aivs_aportacion_r        DECIMAL(16,6);
   DEFINE v_tmp_aivs92_r                 DECIMAL(16,6);
   DEFINE v_tmp_pesos92_r                DECIMAL(12,2);
   DEFINE v_tmp_nom_imss_r               CHAR(50);
   DEFINE v_tmp_num_credito_r            DECIMAL(10,0);
   DEFINE v_tmp_edo_procesar_r           SMALLINT;

   --Variables agregadas despues del cambio para validacion de marca 150 23/10/2015
   DEFINE v_nss_unificador               CHAR(11);
   DEFINE v_id_dh_unificador             DECIMAL(9,0);
   DEFINE p_nss                          CHAR(11);
   DEFINE vcodigo_op                     CHAR(1);
   DEFINE vfecha_tramite                 DATE;
   DEFINE p_marca                        DECIMAL;
   DEFINE p_id_dh                        DECIMAL(9,0);
   DEFINE v_id_grp_dev_unificador        DECIMAL(9,0);
   DEFINE v_bnd                          SMALLINT;
   DEFINE v_diag                         SMALLINT;

   --Variables para tabla tmp_rpt_devolucion
   DEFINE v_rpt_curp                      CHAR(18);
   DEFINE v_rpt_nss                       CHAR(11);
   DEFINE v_rpt_rfc                       CHAR(13);
   DEFINE v_rpt_ap_paterno_af             CHAR(40);
   DEFINE v_rpt_ap_materno_af             CHAR(40);
   DEFINE v_rpt_nombre_af                 CHAR(40);
   DEFINE v_rpt_aivs97                    DECIMAL(16,6);
   DEFINE v_rpt_pesos97                   DECIMAL(12,2);
   DEFINE v_rpt_nss_separacion            CHAR(11);
   DEFINE v_rpt_origen_devolucion         CHAR(2);
   DEFINE v_rpt_monto_aportacion          DECIMAL(12,2);
   DEFINE v_rpt_aivs_aportacion           DECIMAL(16,6);
   DEFINE v_rpt_aivs92                    DECIMAL(16,6);
   DEFINE v_rpt_pesos92                   DECIMAL(12,2);
   DEFINE v_rpt_nom_imss                  CHAR(50);
   DEFINE v_rpt_num_credito               DECIMAL(10,0);
   DEFINE v_rpt_edo_procesar              SMALLINT;
   DEFINE qry_string                      CHAR(200);

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseSolicDevol.trace';
   --SET DEBUG FILE TO '/safreviv/acr/sql/dseSolicDevol.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_bnd                          = 0 ;
   LET v_agrupa_id_dse_grp_devolucion = 0;
   LET v_agrupa_id_derechohabiente    = 0;
   LET v_agrupa_num_credito           = 0;
   LET v_agrupa_tpo_transferencia     = "15";
   LET v_agrupa_origen_devolucion     = 2;
   LET v_agrupa_f_movimiento          = "";
   LET v_agrupa_folio_liquida         = 0;
   LET v_agrupa_aivs97                = 0;
   LET v_agrupa_pesos97               = 0;
   LET v_agrupa_aivs92                = 0;
   LET v_agrupa_pesos92               = 0;
   LET v_agrupa_monto_aportacion      = 0;
   LET v_agrupa_aivs_aportacion       = 0;
   LET v_agrupa_nss_separacion        = "";
   LET v_agrupa_edo_procesar          = "";
   LET v_agrupa_estado                = "";
   LET v_afi_curp                     = "";
   LET v_afi_nss                      = "";
   LET v_afi_rfc                      = "";
   LET v_afi_ap_paterno_af            = "";
   LET v_afi_ap_materno_af            = "";
   LET v_afi_nombre_af                = "";
   LET v_afi_nombre_imss              = "";
   LET v_tmp_curp                     = "";
   LET v_tmp_nss                      = "";
   LET v_tmp_rfc                      = "";
   LET v_tmp_ap_paterno_af            = "";
   LET v_tmp_ap_materno_af            = "";
   LET v_tmp_nombre_af                = "";
   LET v_tmp_aivs97                   = 0;
   LET v_tmp_pesos97                  = 0;
   LET v_tmp_nss_separacion           = "";
   LET v_tmp_origen_devolucion        = 2;
   LET v_tmp_monto_aportacion         = 0;
   LET v_tmp_aivs_aportacion          = 0;
   LET v_tmp_aivs92                   = 0;
   LET v_tmp_pesos92                  = 0;
   LET v_tmp_nom_imss                 = "";
   LET v_tmp_num_credito              = "";
   LET v_tmp_edo_procesar             = "";
   LET v_valor_fondo                  = 0;
   LET v_edo_procesar_aux             = "";
   LET v_f_solic_saldos_aux           = "";
   LET v_tmp_curp_r                   = "";
   LET v_tmp_nss_r                    = "";
   LET v_tmp_rfc_r                    = "";
   LET v_tmp_ap_paterno_af_r          = "";
   LET v_tmp_ap_materno_af_r          = "";
   LET v_tmp_nombre_af_r              = "";
   LET v_tmp_aivs97_r                 = 0;
   LET v_tmp_pesos97_r                = 0;
   LET v_tmp_nss_separacion_r         = "";
   LET v_tmp_origen_devolucion_r      = 2;
   LET v_tmp_monto_aportacion_r       = 0;
   LET v_tmp_aivs_aportacion_r        = 0;
   LET v_tmp_aivs92_r                 = 0;
   LET v_tmp_pesos92_r                = 0;
   LET v_tmp_nom_imss_r               = "";
   LET v_tmp_num_credito_r            = 0;
   LET v_tmp_edo_procesar_r           = "";
   LET v_nss_unificador               = "";
   LET v_id_dh_unificador             = 0;
   LET p_nss                          = "";
   LET vcodigo_op                     = "";
   LET vfecha_tramite                 = "";
   LET p_marca                        = "";
   LET p_id_dh                        = "";
   LET v_id_grp_dev_unificador        = "";
   LET v_diag                         = 0;

   -- se obtiene la fecha del último día del mes anterior de la fecha de presentación
   LET v_f_solic_saldos_aux = p_f_presentacion - DAY(p_f_presentacion);
   LET p_f_movimiento = p_f_movimiento - 1 UNITS MONTH;

   --se consulta el valor del fondo para recalcular los pesos
   SELECT precio_fondo
     INTO v_valor_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = p_f_movimiento;

   --Se inicia el foreach para la lectura de los registros
   FOREACH
      SELECT d1.*
        INTO v_agrupa_id_dse_grp_devolucion,
             v_agrupa_id_derechohabiente,
             v_agrupa_num_credito,
             v_agrupa_tpo_transferencia,
             v_agrupa_origen_devolucion,
             v_agrupa_f_movimiento,
             v_agrupa_folio_liquida,
             v_agrupa_aivs97,
             v_agrupa_pesos97,
             v_agrupa_aivs92,
             v_agrupa_pesos92,
             v_agrupa_monto_aportacion,
             v_agrupa_aivs_aportacion,
             v_agrupa_nss_separacion,
             v_agrupa_edo_procesar,
             v_agrupa_estado
        FROM dse_agrupa_devolucion d1
       WHERE d1.tpo_transferencia = '15'
         --AND d1.estado            IN(140,142)
         AND d1.estado            IN(140,142,20)
         AND d1.edo_procesar      IN(20,70)

      IF v_agrupa_pesos97 IS NULL THEN
         LET v_agrupa_pesos97 = 0;
         LET v_agrupa_aivs97  = 0;
      END IF

      IF v_agrupa_pesos92 IS NULL THEN
         LET v_agrupa_pesos92 = 0;
         LET v_agrupa_aivs92  = 0;
      END IF

      IF (v_agrupa_pesos97 = 0) AND (v_agrupa_pesos92 = 0) THEN
         CONTINUE FOREACH;
      END IF

      SELECT curp,
             nss,
             rfc,
             ap_paterno_af,
             ap_materno_af,
             nombre_af,
             nombre_imss
        INTO v_afi_curp,
             v_afi_nss,
             v_afi_rfc,
             v_afi_ap_paterno_af,
             v_afi_ap_materno_af,
             v_afi_nombre_af,
             v_afi_nombre_imss
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_agrupa_id_derechohabiente;

      IF v_afi_nombre_imss IS NULL OR v_afi_nombre_imss[1] = " " THEN
         LET v_afi_nombre_imss = TRIM(v_afi_ap_paterno_af)||"$"||TRIM(v_afi_ap_materno_af)||"$"||TRIM(v_afi_nombre_af);
      END IF

      -- se ejecuta función que verifica marca
      EXECUTE FUNCTION fn_verifica_marca_43 (v_afi_nss,150,v_agrupa_id_derechohabiente)
                  INTO p_nss, vcodigo_op, vfecha_tramite, p_marca, p_id_dh;

      -- En caso de que la función regrese valor 1
      IF vcodigo_op = '1' THEN
         --se ejecuta la función que busca el nss unificador
         EXECUTE PROCEDURE fn_busca_nss_unificador (v_afi_nss)
                      INTO v_nss_unificador, v_id_dh_unificador, v_diag;

         IF v_diag = 1 THEN
            SELECT curp,
                   nss,
                   rfc,
                   ap_paterno_af,
                   ap_materno_af,
                   nombre_af,
                   nombre_imss
              INTO v_afi_curp,
                   v_afi_nss,
                   v_afi_rfc,
                   v_afi_ap_paterno_af,
                   v_afi_ap_materno_af,
                   v_afi_nombre_af,
                   v_afi_nombre_imss
              FROM afi_derechohabiente
             WHERE id_derechohabiente = v_id_dh_unificador;

            IF v_afi_nombre_imss IS NULL OR v_afi_nombre_imss[1] = " " THEN
               LET v_afi_nombre_imss = TRIM(v_afi_ap_paterno_af)||"$"||TRIM(v_afi_ap_materno_af)||"$"||TRIM(v_afi_nombre_af);
            END IF

            LET v_id_grp_dev_unificador = seq_dse_grp_devolucion.NEXTVAL;

            --se insertan en tabla datos del unificador haciendo referencia a sus unificados
            INSERT INTO dse_grp_unificacion VALUES ( v_agrupa_id_derechohabiente,    --id_dh de unificado
                                                     v_agrupa_id_dse_grp_devolucion, --id_dse_de unificado
                                                     v_id_dh_unificador,             -- id_dh de unificador
                                                     v_id_grp_dev_unificador,        --id_dse de unificador
                                                     TODAY,
                                                     g_usuario_cod);                 -- falta agregar usuario en parámetros de entrada de función

            --se actualiza el estado en dse_agrupa_devolución para registro actual
            UPDATE dse_agrupa_devolucion
               SET estado = 230
             WHERE id_dse_grp_devolucion = v_agrupa_id_dse_grp_devolucion;

            --se inserta registro con datos originales en tabla histórica
            INSERT INTO dse_his_devolucion VALUES ( v_agrupa_id_dse_grp_devolucion,
                                                    v_agrupa_folio_liquida,
                                                    15,
                                                    p_lote,--lote
                                                    p_id_lote,--id_lote
                                                    0,--f_presentacion
                                                    v_afi_ap_paterno_af,
                                                    v_afi_ap_materno_af,
                                                    v_afi_nombre_af,
                                                    v_afi_nombre_imss,
                                                    v_agrupa_aivs97,
                                                    v_agrupa_pesos97,
                                                    v_agrupa_aivs92,
                                                    v_agrupa_pesos92,
                                                    v_agrupa_monto_aportacion,
                                                    v_agrupa_aivs_aportacion,
                                                    v_agrupa_nss_separacion,
                                                    v_agrupa_edo_procesar,
                                                    0,--diagnostico
                                                    v_agrupa_estado,
                                                    TODAY );

            INSERT INTO dse_his_devolucion VALUES ( v_agrupa_id_dse_grp_devolucion,
                                                    v_agrupa_folio_liquida,
                                                    15,
                                                    p_lote,--lote
                                                    p_id_lote,--id_lote
                                                    0,--f_presentacion
                                                    v_afi_ap_paterno_af,
                                                    v_afi_ap_materno_af,
                                                    v_afi_nombre_af,
                                                    v_afi_nombre_imss,
                                                    v_agrupa_aivs97,
                                                    v_agrupa_pesos97,
                                                    v_agrupa_aivs92,
                                                    v_agrupa_pesos92,
                                                    v_agrupa_monto_aportacion,
                                                    v_agrupa_aivs_aportacion,
                                                    v_agrupa_nss_separacion,
                                                    v_agrupa_edo_procesar,
                                                    0,--diagnostico
                                                    230,
                                                    TODAY );

            INSERT INTO dse_agrupa_devolucion VALUES ( v_id_grp_dev_unificador, -- consecutivo para id_dse de unificador
                                                       v_id_dh_unificador,
                                                       v_agrupa_num_credito,
                                                       v_agrupa_tpo_transferencia,
                                                       v_agrupa_origen_devolucion,
                                                       v_agrupa_f_movimiento,
                                                       v_agrupa_folio_liquida,
                                                       v_agrupa_aivs97,
                                                       v_agrupa_pesos97,
                                                       v_agrupa_aivs92,
                                                       v_agrupa_pesos92,
                                                       v_agrupa_monto_aportacion,
                                                       v_agrupa_aivs_aportacion,
                                                       v_agrupa_nss_separacion,
                                                       v_agrupa_edo_procesar,
                                                       v_agrupa_estado);

            INSERT INTO dse_his_devolucion VALUES ( v_id_grp_dev_unificador,
                                                    v_agrupa_folio_liquida,
                                                    15,
                                                    p_lote,--lote
                                                    p_id_lote,--id_lote
                                                    0,--f_presentacion
                                                    v_afi_ap_paterno_af,
                                                    v_afi_ap_materno_af,
                                                    v_afi_nombre_af,
                                                    v_afi_nombre_imss,
                                                    v_agrupa_aivs97,
                                                    v_agrupa_pesos97,
                                                    v_agrupa_aivs92,
                                                    v_agrupa_pesos92,
                                                    v_agrupa_monto_aportacion,
                                                    v_agrupa_aivs_aportacion,
                                                    v_agrupa_nss_separacion,
                                                    v_agrupa_edo_procesar,
                                                    0,--diagnostico
                                                    140,
                                                    TODAY );

            LET v_agrupa_id_dse_grp_devolucion = v_id_grp_dev_unificador;
            LET v_tmp_nss                      = v_nss_unificador;
         END IF
      END IF

      --Se asignan los campos para la tabla temporal
      LET v_tmp_curp              = v_afi_curp;
      LET v_tmp_nss               = v_afi_nss;
      LET v_tmp_rfc               = v_afi_rfc;
      LET v_tmp_ap_paterno_af     = v_afi_ap_paterno_af;
      LET v_tmp_ap_materno_af     = v_afi_ap_materno_af;
      LET v_tmp_nombre_af         = v_afi_nombre_af;
      LET v_tmp_aivs97            = v_agrupa_aivs97;
      LET v_tmp_nss_separacion    = v_agrupa_nss_separacion;
      LET v_tmp_monto_aportacion  = v_agrupa_monto_aportacion;
      LET v_tmp_aivs_aportacion   = v_agrupa_aivs_aportacion;
      LET v_tmp_aivs92            = v_agrupa_aivs92;
      LET v_tmp_nom_imss          = v_afi_nombre_imss;
      LET v_tmp_num_credito       = v_agrupa_num_credito;
      LET v_tmp_edo_procesar      = v_agrupa_edo_procesar;

      --se calcula el valor en pesos correspondiente a numero de aivs
      LET v_tmp_pesos97 = v_agrupa_aivs97 * v_valor_fondo;
      LET v_tmp_pesos92 = v_agrupa_aivs92 * v_valor_fondo;

      IF v_tmp_pesos97 > 0 THEN
         IF v_tmp_pesos92 > 0 THEN
            LET v_tmp_origen_devolucion = 5;
         ELSE
            LET v_tmp_origen_devolucion = 2;
         END IF
      ELSE
         LET v_tmp_origen_devolucion = 1;
      END IF

      IF v_tmp_monto_aportacion IS NULL THEN
         LET v_tmp_monto_aportacion = 0;
      END IF

      --Se inserta el detalle individual del registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_ctr_devolucion VALUES ( v_agrupa_id_dse_grp_devolucion,
                                                        v_tmp_curp                  ,
                                                        v_tmp_nss                   ,
                                                        v_tmp_rfc                   ,
                                                        v_tmp_ap_paterno_af         ,
                                                        v_tmp_ap_materno_af         ,
                                                        v_tmp_nombre_af             ,
                                                        v_tmp_aivs97                ,
                                                        v_tmp_pesos97               ,
                                                        v_tmp_nss_separacion        ,
                                                        v_tmp_origen_devolucion     ,
                                                        v_tmp_monto_aportacion      ,
                                                        v_tmp_aivs_aportacion       ,
                                                        v_tmp_aivs92                ,
                                                        v_tmp_pesos92               ,
                                                        v_tmp_nom_imss              ,
                                                        v_tmp_num_credito           ,
                                                        v_tmp_edo_procesar );

      IF v_agrupa_edo_procesar = 70 THEN
         LET v_edo_procesar_aux = 85;
      ELSE
         LET v_edo_procesar_aux = 80;
      END IF

     -- se actualiza el registro de la tabla de agrupación
      UPDATE dse_agrupa_devolucion
         SET edo_procesar          = v_edo_procesar_aux
       WHERE id_dse_grp_devolucion = v_agrupa_id_dse_grp_devolucion;

      -- se actualiza el registro de la tabla "dse_his_devolucion"
      UPDATE dse_his_devolucion
         SET id_lote        = p_id_lote,
             lote           = p_lote,
             f_presentacion = p_f_presentacion,
             edo_procesar   = v_edo_procesar_aux
       WHERE id_dse_grp_devolucion = v_agrupa_id_dse_grp_devolucion
         AND estado                = v_agrupa_estado
         AND edo_procesar IN (20,70);

      -- se inicializan variables
      LET v_bnd                          = 0 ;
      LET v_agrupa_id_dse_grp_devolucion = 0;
      LET v_agrupa_id_derechohabiente    = 0;
      LET v_agrupa_num_credito           = 0;
      LET v_agrupa_tpo_transferencia     = "15";
      LET v_agrupa_origen_devolucion     = 2;
      LET v_agrupa_f_movimiento          = "";
      LET v_agrupa_folio_liquida         = 0;
      LET v_agrupa_aivs97                = 0;
      LET v_agrupa_pesos97               = 0;
      LET v_agrupa_aivs92                = 0;
      LET v_agrupa_pesos92               = 0;
      LET v_agrupa_monto_aportacion      = 0;
      LET v_agrupa_aivs_aportacion       = 0;
      LET v_agrupa_nss_separacion        = "";
      LET v_agrupa_edo_procesar          = "";
      LET v_agrupa_estado                = "";
      LET v_afi_curp                     = "";
      LET v_afi_nss                      = "";
      LET v_afi_rfc                      = "";
      LET v_afi_ap_paterno_af            = "";
      LET v_afi_ap_materno_af            = "";
      LET v_afi_nombre_af                = "";
      LET v_afi_nombre_imss              = "";
      LET v_tmp_curp                     = "";
      LET v_tmp_nss                      = "";
      LET v_tmp_rfc                      = "";
      LET v_tmp_ap_paterno_af            = "";
      LET v_tmp_ap_materno_af            = "";
      LET v_tmp_nombre_af                = "";
      LET v_tmp_aivs97                   = 0;
      LET v_tmp_pesos97                  = 0;
      LET v_tmp_nss_separacion           = "";
      LET v_tmp_origen_devolucion        = 2;
      LET v_tmp_monto_aportacion         = 0;
      LET v_tmp_aivs_aportacion          = 0;
      LET v_tmp_aivs92                   = 0;
      LET v_tmp_pesos92                  = 0;
      LET v_tmp_nom_imss                 = "";
      LET v_tmp_num_credito              = "";
      LET v_edo_procesar_aux             = "";
      LET v_f_solic_saldos_aux           = "";
      LET v_tmp_curp_r                   = "";
      LET v_tmp_nss_r                    = "";
      LET v_tmp_rfc_r                    = "";
      LET v_tmp_ap_paterno_af_r          = "";
      LET v_tmp_ap_materno_af_r          = "";
      LET v_tmp_nombre_af_r              = "";
      LET v_tmp_aivs97_r                 = 0;
      LET v_tmp_pesos97_r                = 0;
      LET v_tmp_nss_separacion_r         = "";
      LET v_tmp_origen_devolucion_r      = 2;
      LET v_tmp_monto_aportacion_r       = 0;
      LET v_tmp_aivs_aportacion_r        = 0;
      LET v_tmp_aivs92_r                 = 0;
      LET v_tmp_pesos92_r                = 0;
      LET v_tmp_nom_imss_r               = "";
      LET v_tmp_num_credito_r            = 0;
      LET v_tmp_edo_procesar_r           = "";
      LET v_nss_unificador               = "";
      LET v_id_dh_unificador             = 0;
      LET p_nss                          = "";
      LET vcodigo_op                     = "";
      LET vfecha_tramite                 = "";
      LET p_marca                        = "";
      LET p_id_dh                        = "";
      LET v_id_grp_dev_unificador        = "";

   END FOREACH;

END PROCEDURE;


