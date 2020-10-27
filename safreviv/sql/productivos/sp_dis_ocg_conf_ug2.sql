






CREATE PROCEDURE "safreviv".sp_dis_ocg_conf_ug2(v_folio    DECIMAL(9,0),
                                    v_folio_ug DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 10102016
--Declaración de variables
DEFINE v_id_dis_interface_ef       DECIMAL(9,0);
DEFINE v_id_derechohabiente        DECIMAL(9,0);
DEFINE v_folio_sua                 DECIMAL(6,0);
DEFINE v_periodo_pago              CHAR(6);
DEFINE v_f_pago                    DATE;
DEFINE v_nrp                       CHAR(11);
DEFINE v_ind_liquidacion           SMALLINT;
DEFINE v_folio_liquida             DECIMAL(9,0);
DEFINE v_f_liquida                 DATE;
DEFINE v_num_crd_ifv               DECIMAL(10,0);
DEFINE v_imp_ap_pat                DECIMAL(12,2);
DEFINE v_aiv_ap_pat                DECIMAL(18,6);
DEFINE v_tpo_credito               SMALLINT;
DEFINE v_cve_ent_financiera        SMALLINT;
DEFINE v_num_ctr_int_ef            CHAR(18);
DEFINE v_concepto                  SMALLINT;
DEFINE v_concepto_c                CHAR(3);
DEFINE v_id_ctr_transaccion        DECIMAL(9,0);
DEFINE v_id_ocg_detalle            DECIMAL(9,0);
DEFINE v_folio_transaccion         DECIMAL(9,0);
DEFINE v_f_transaccion             DATE;
DEFINE v_folio_factura             DECIMAL(9,0);
DEFINE v_f_factura                 DATE;
DEFINE v_estado                    SMALLINT;
DEFINE v_nss                       CHAR(11);
DEFINE v_curp                      CHAR(18);
DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER ;
DEFINE isam_err                    INTEGER ;
DEFINE error_info                  CHAR(70);
DEFINE v_char                      CHAR(70);
DEFINE v_bnd_proceso               SMALLINT;       --Estatus del proceso
DEFINE v_fecha_carga               DATE;
DEFINE v_bnd_existe_acreditado     SMALLINT;
DEFINE v_bnd_existe_transacciones  SMALLINT;
DEFINE v_bnd_acred_vigente         SMALLINT;
DEFINE v_tot_existe_acr            SMALLINT;
DEFINE v_tot_existe_trans          SMALLINT;
DEFINE v_edo_ent_financiera        SMALLINT;
DEFINE v_id_ocg_formalizacion      DECIMAL(9,0);

DEFINE v_tpo_credito_ocg           CHAR(1);
DEFINE v_resultado                 SMALLINT;
DEFINE v_id_der_credito            DECIMAL(9,0);
DEFINE v_f_otorgamiento            DATE;
DEFINE v_f_liq_credito             DATE;

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

  SET DEBUG FILE TO '/safreviv_int/archivos/sp_dis_ocg_conf_ug.TRACE';
  TRACE ON;

  --#Inicialización de variables
  LET v_bnd_proceso              = 0; --Estado correcto
  LET v_id_dis_interface_ef      = 0;
  LET v_id_derechohabiente       = 0;
  LET v_folio_sua                = 0;
  LET v_periodo_pago             = "";
  LET v_f_pago                   = "";
  LET v_nrp                      = "";
  LET v_ind_liquidacion          = 0;
  LET v_folio_liquida            = 0;
  LET v_f_liquida                = "";
  LET v_num_crd_ifv              = 0;
  LET v_imp_ap_pat               = 0;
  LET v_aiv_ap_pat               = 0;
  LET v_tpo_credito              = 0;
  LET v_cve_ent_financiera       = 0;
  LET v_num_ctr_int_ef           = "";
  LET v_concepto                 = 0;
  LET v_concepto_c               = 0;
  LET v_id_ctr_transaccion       = 0;
  LET v_id_ocg_detalle           = 0;
  LET v_folio_transaccion        = 0;
  LET v_f_transaccion            = "";
  LET v_folio_factura            = 0;
  LET v_f_factura                = "";
  LET v_estado                   = 0;
  LET v_nss                      = "";
  LET v_curp                     = "";
  LET v_bnd_existe_acreditado    = 0;
  LET v_bnd_existe_transacciones = 0;
  LET v_bnd_acred_vigente        = 0;
  LET v_tot_existe_acr           = 0;
  LET v_tot_existe_trans         = 0;
  LET v_edo_ent_financiera       = 0;
  LET v_id_ocg_formalizacion     = 0;

  LET v_tpo_credito_ocg          = "";
  LET v_resultado                = 0;
  LET v_id_der_credito           = 0;
  LET v_f_otorgamiento           = "";
  LET v_f_liq_credito            = "";

  {DROP TABLE IF EXISTS tmp_dis_ocg_conf_ug;

  CREATE TABLE tmp_dis_ocg_conf_ug (id_dis_interface_ef DECIMAL(9,0),
                                    id_derechohabiente  DECIMAL(9,0),
                                    nss                 CHAR(11),
                                    curp                CHAR(18),
                                    nombre_af           CHAR(40),
                                    ap_paterno_af       CHAR(40),
                                    ap_materno_af       CHAR(40))

  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;}

  SET PDQPRIORITY HIGH;

  SELECT apo.id_dis_interface_ef,
         afi.id_derechohabiente,
         afi.nss,
         afi.nombre_af,
         afi.ap_paterno_af,
         afi.ap_materno_af,
         afi.curp
  FROM   dis_ctr_aps_tns apo,
         afi_derechohabiente afi
  WHERE  apo.folio_liquida      = v_folio_ug
  AND    apo.id_derechohabiente = afi.id_derechohabiente
and apo.estado =  10
  INTO TEMP tmp_dis_ocg_conf_ug;

  UPDATE STATISTICS FOR TABLE tmp_dis_ocg_conf_ug;

  --Obtener detalle de los registros a confirmar
  FOREACH
    SELECT apo.id_dis_interface_ef,
           apo.id_derechohabiente,
           apo.folio_sua,
           apo.periodo_pago,
           apo.f_pago,
           apo.nrp,
           apo.ind_liquidacion,
           apo.folio_liquida,
           apo.f_liquida,
           apo.num_crd_ifv,
           apo.imp_ap_pat,
           apo.aiv_ap_pat,
           apo.tpo_credito,
           apo.cve_ent_financiera,
           apo.num_ctr_int_ef,
           apo.concepto,
           apo.id_ctr_transaccion,
           apo.folio_transaccion,
           apo.f_transaccion,
           apo.folio_factura,
           apo.f_factura,
           apo.estado,
           afi.nss,
           afi.curp
    INTO   v_id_dis_interface_ef,
           v_id_derechohabiente,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp,
           v_ind_liquidacion,
           v_folio_liquida,
           v_f_liquida,
           v_num_crd_ifv,
           v_imp_ap_pat,
           v_aiv_ap_pat,
           v_tpo_credito,
           v_cve_ent_financiera,
           v_num_ctr_int_ef,
           v_concepto,
           v_id_ctr_transaccion,
           v_folio_transaccion,
           v_f_transaccion,
           v_folio_factura,
           v_f_factura,
           v_estado,
           v_nss,
           v_curp
    FROM   dis_ctr_aps_tns apo,
           tmp_dis_ocg_conf_ug afi
    --WHERE  apo.id_derechohabiente = afi.id_derechohabiente
    WHERE  apo.folio_liquida       = v_folio_ug
    AND    apo.id_dis_interface_ef = afi.id_dis_interface_ef
    AND    apo.id_derechohabiente  = afi.id_derechohabiente
    ORDER BY apo.tpo_credito

    LET v_estado             = 23;
    LET v_tot_existe_acr     = 0;
    LET v_tot_existe_trans   = 0;
    ---LET v_concepto           = 0;
    LET v_concepto_c         = 0;
    LET v_cve_ent_financiera = 0;
    LET v_num_ctr_int_ef     = '';
    LET v_fecha_carga        = '';
    LET v_edo_ent_financiera = 0;

    LET v_tot_existe_acr        = 0;
    LET v_bnd_existe_acreditado = 0; --NO EXISTE

    LET v_id_ctr_transaccion    = seq_dis_ctr_aps_tns.NEXTVAL;

    --########## Se agrega función para identificar el crédito ###########
    --Valor resultado = 0:  Sin Crédito
    --Valor resultado = 1:  Crédito Vigente
    --Valor resultado = 2:  Crédito Liquidado
    --Valor tpo_credito = A o C: Apoyo INFONAVIT
    --Valor tpo_credito = 7 u 8: COFINANCIADOS
    LET v_tpo_credito_ocg          = "";
    LET v_resultado                = 0;
    LET v_id_der_credito           = 0;
    LET v_f_otorgamiento           = "";
    LET v_f_liq_credito            = "";
    LET v_cve_ent_financiera       = 0;
    LET v_num_ctr_int_ef           = '';
    LET v_id_ocg_formalizacion     = 0;

    EXECUTE FUNCTION fn_credito_43bis(v_id_derechohabiente)
                INTO v_resultado,
                     v_id_der_credito,
                     v_tpo_credito_ocg,
                     v_f_otorgamiento,
                     v_f_liq_credito,
                     v_cve_ent_financiera,
                     v_num_ctr_int_ef,
                     v_id_ocg_formalizacion;

    LET v_tpo_credito = 3;

    --Verificar si existe en BD ACREDITADOS
    IF (v_resultado  = 0     OR
        v_resultado IS NULL) THEN
       LET v_estado   = 21; -- 21 – Confirmado No Acreditado
       --LET v_concepto = 508;  -- 508 - Devolución No Acreditados

       IF v_concepto = 307 THEN
          LET v_concepto = 308;  -- 508 - Devolución No Acreditados aportaciones
       END IF
       IF v_concepto = 407 THEN
          LET v_concepto = 408;  -- 508 - Devolución No Acreditados usos
       END IF
    ELSE ------------ACREDITADO existe en BD
       LET v_tot_existe_trans         = 0;
       LET v_bnd_existe_transacciones = 0;

       SELECT COUNT(*)
       INTO   v_tot_existe_trans
       FROM   ocg_ctr_transaccion
       WHERE  id_derechohabiente = v_id_derechohabiente;
       IF v_tot_existe_trans >= 1 THEN
          LET v_bnd_existe_transacciones = 1;
       END IF

       --Validar existe en BD TRANSACCIONES
       IF v_bnd_existe_transacciones = 1 THEN --------------SI existe(NSS) en BD TRANSACCIONES
          LET v_tot_existe_acr    = 0;
          LET v_bnd_acred_vigente = 0;
          LET v_estado            = 20;  -- 20 – Confirmado

          {SELECT FIRST 1 tr.concepto, tr.f_proceso
          INTO   v_concepto, v_fecha_carga
          FROM   ocg_ctr_transaccion tr
          WHERE  tr.id_derechohabiente = v_id_derechohabiente
          AND    tr.f_proceso         <> TODAY
          GROUP BY 2,1;

          LET v_concepto_c = v_concepto;}

          --Validar si Acreditado = VIGENTE
          IF v_resultado = 1 THEN -----------------------SI esta VIGENTE
             {IF v_tpo_credito = 3 THEN ------------- Uso de Garantía
                LET v_concepto = 407; -- 407 – Recurrente por Facturar
             END IF}
          ELSE--------------------------------------------------Estado DIFERENTE A VIGENTE
             IF v_tpo_credito = 3 THEN   ------------- Uso de Garantía
                IF v_concepto = 307 THEN
                   LET v_concepto = 308; -- 308 – Devolución
                   --TRANSACCIÓN DEVOLUCIÓN
                END IF
                IF v_concepto = 407 THEN ------------- Uso de Garantía
                   LET v_concepto = 408; -- 408 – Devolución
                   --TRANSACCIÓN DEVOLUCIÓN
                END IF
             END IF
          END IF
       ELSE ------------------------------------------------ NO existe en BD TRANSACCIONES
          LET v_estado            = 20;  -- 20 – Confirmado

          --Validar si Acreditado = VIGENTE
          IF v_resultado = 1 THEN -----------------------SI esta VIGENTE
             {IF v_tpo_credito = 3 THEN ------------- Uso de Garantía
                LET v_concepto = 407; -- 407 – Recurrente por Facturar
             END IF}
          ELSE--------------------------------------------------Estado DIFERENTE A VIGENTE
             IF v_tpo_credito = 3 THEN   ------------- Uso de Garantía
                IF v_concepto = 307 THEN
                   LET v_concepto = 308; -- 308 – Devolución
                   --TRANSACCIÓN DEVOLUCIÓN
                END IF
                IF v_concepto = 407 THEN
                   LET v_concepto = 408; -- 408 – Devolución
                   --TRANSACCIÓN DEVOLUCIÓN
                END IF
             END IF
          END IF
       END IF
    END IF

    --2.4) A todos los registros que cumplieron las validaciones para asignarles concepto,
    --     se identificara y asignara de forma previa a la facturación la entidad financiera
    --     y el número de control interno de la entidad financiera.
    IF v_concepto = 0 THEN
       UPDATE dis_ctr_aps_tns
       SET    id_ctr_transaccion  = v_id_ctr_transaccion,
              folio_transaccion   = v_folio,
              f_transaccion       = TODAY,
              estado              = v_estado,
              tpo_credito         = v_tpo_credito
       WHERE  id_dis_interface_ef = v_id_dis_interface_ef;
    ELSE
       IF v_concepto <> 508 THEN
          SELECT cat.cve_bloqueo
          INTO   v_edo_ent_financiera
          FROM   cat_cta_cnt_ocg cat
          WHERE  cat.cve_ent_financiera = v_cve_ent_financiera
          AND    cat.tpo_credito        = v_tpo_credito;

          IF v_edo_ent_financiera = 1 THEN -- Entidad Financiera Bloqueada
             LET v_estado = 40; -- 40  - Entidad Financiera Bloqueada
          ELSE
             LET v_id_ocg_detalle = seq_ocg_detalle.NEXTVAL;

             INSERT INTO ocg_ctr_transaccion VALUES (v_id_ctr_transaccion,  --seq_dis_ctr_aps_tns
                                                     v_id_ocg_formalizacion,
                                                     v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                                     v_id_derechohabiente,
                                                     v_id_dis_interface_ef, --id_dis_interface_ef
                                                     v_folio_liquida,       --Folio Liquidación Aportaciones Subsecuentes
                                                     3904,                  --Confirma Aportaciones Subsecuentes
                                                     v_cve_ent_financiera,
                                                     v_num_ctr_int_ef,
                                                     v_folio,
                                                     TODAY,
                                                     v_nss,
                                                     v_curp,
                                                     v_imp_ap_pat,
                                                     v_periodo_pago,
                                                     v_f_pago,
                                                     v_concepto,
                                                     TODAY,
                                                     30);

             INSERT INTO ocg_detalle VALUES (v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                             0,                    -- se envía siempre un valor “0” (cero)
                                             v_id_derechohabiente, -- se envía el id_derechohabiente del registro
                                             4,                    -- se envía siempre como subproceso valor “4”
                                             TODAY,                -- se envía fecha en que se inserta el registro
                                             v_cve_ent_financiera, -- se envía entidad financiera del registro a insertar
                                             v_nss);               -- se envía el NSS del registro a insertar

             INSERT INTO ocg_fecha_mig VALUES (v_id_ctr_transaccion, --seq_dis_ctr_aps_tns
                                               v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                               v_id_derechohabiente, -- se envía el id_derechohabiente del registro
                                               TODAY,                -- f_envio = today
                                               "",                   -- f_carga, se deja nulo
                                               TODAY,                -- f_respuesta = today
                                               "",                   -- f_liquida_cofi siempre nulo
                                               4,                    -- subproceso
                                               TODAY);               -- fecha de alta del registro

          END IF
       ELSE
          LET v_id_ocg_detalle = seq_ocg_detalle.NEXTVAL;

          INSERT INTO ocg_ctr_transaccion VALUES (v_id_ctr_transaccion,  --seq_dis_ctr_aps_tns
                                                  v_id_ocg_formalizacion,
                                                  v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                                  v_id_derechohabiente,
                                                  v_id_dis_interface_ef, --id_dis_interface_ef
                                                  v_folio_liquida,       --Folio Liquidación Aportaciones Subsecuentes
                                                  3904,                  --Confirma Aportaciones Subsecuentes
                                                  v_cve_ent_financiera,
                                                  v_num_ctr_int_ef,
                                                  v_folio,
                                                  TODAY,
                                                  v_nss,
                                                  v_curp,
                                                  v_imp_ap_pat,
                                                  v_periodo_pago,
                                                  v_f_pago,
                                                  v_concepto,
                                                  TODAY,
                                                  30);

         INSERT INTO ocg_detalle VALUES (v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                         0,                    -- se envía siempre un valor “0” (cero)
                                         v_id_derechohabiente, -- se envía el id_derechohabiente del registro
                                         4,                    -- se envía siempre como subproceso valor “4”
                                         TODAY,                -- se envía fecha en que se inserta el registro
                                         v_cve_ent_financiera, -- se envía entidad financiera del registro a insertar
                                         v_nss);               -- se envía el NSS del registro a insertar

         INSERT INTO ocg_fecha_mig VALUES (v_id_ctr_transaccion, --seq_dis_ctr_aps_tns
                                           v_id_ocg_detalle,     -- secuencia para id_ocg_detalle
                                           v_id_derechohabiente, -- se envía el id_derechohabiente del registro
                                           TODAY,                -- f_envio = today
                                           "",                   -- f_carga, se deja nulo
                                           TODAY,                -- f_respuesta = today
                                           "",                   -- f_liquida_cofi siempre nulo
                                           4,                    -- subproceso
                                           TODAY);               -- fecha de alta del registro

       END IF

       UPDATE dis_ctr_aps_tns
       SET    id_ctr_transaccion  = v_id_ctr_transaccion,
              folio_transaccion   = v_folio,
              f_transaccion       = TODAY,
              cve_ent_financiera  = v_cve_ent_financiera,
              num_ctr_int_ef      = v_num_ctr_int_ef,
              tpo_credito         = v_tpo_credito,
              concepto            = v_concepto,
              estado              = v_estado
       WHERE  id_dis_interface_ef = v_id_dis_interface_ef;
    END IF

  END FOREACH;

  UPDATE glo_folio
     SET status = 1
   WHERE folio = v_folio;

  --TRACE 'Finaliza sp_dis_cargar_tablas_tmp '||v_bnd_proceso;
  LET v_char = "Termina Confirmación del proceso de OCG";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


