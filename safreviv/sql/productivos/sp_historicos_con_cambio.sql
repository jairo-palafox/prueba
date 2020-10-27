






CREATE PROCEDURE "safreviv".sp_historicos_con_cambio(p_folio DECIMAL(9,0),
                                          p_pid DECIMAL(9,0),
                                          p_proceso_cod SMALLINT,
                                          p_origen_archivo SMALLINT )
RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(11)

   --TABLA tmp_det_cc_nss
   DEFINE tmp_det_cc_tpo_registro        CHAR(1);
   DEFINE tmp_det_cc_cve_ent_receptora   CHAR(3);
   DEFINE tmp_det_cc_nrp                 CHAR(11);
   DEFINE tmp_det_cc_rfc_patron          CHAR(13);
   DEFINE tmp_det_cc_periodo_pago        CHAR(6);
   DEFINE tmp_det_cc_f_pago_patron       DATE;
   DEFINE tmp_det_cc_folio_sua           DECIMAL(6);
   DEFINE tmp_det_cc_nss                 CHAR(11);
   DEFINE tmp_det_cc_rfc                 CHAR(13);
   DEFINE tmp_det_cc_curp                CHAR(18);
   DEFINE tmp_det_cc_num_crd_ifv         CHAR(10);
   DEFINE tmp_det_cc_f_ini_desc_crd_ifv  DATE;
   DEFINE tmp_det_cc_num_mov_periodo     DECIMAL(2);
   DEFINE tmp_det_cc_nombre_trabajador   CHAR(50);
   DEFINE tmp_det_cc_ult_sdi             CHAR(9);
   DEFINE tmp_det_cc_tpo_trabajador      CHAR(1);
   DEFINE tmp_det_cc_jornada             CHAR(1);
   DEFINE tmp_det_cc_localiza_trabajador CHAR(1);
   DEFINE tmp_det_cc_destino_ap_viv      CHAR(1);
   DEFINE tmp_det_cc_dias_cot_bim        DECIMAL(2);
   DEFINE tmp_det_cc_dias_incap_bim      DECIMAL(2);
   DEFINE tmp_det_cc_dias_ausent_bim     DECIMAL(2);
   DEFINE tmp_det_cc_imp_ap_pat          DECIMAL(14);
   DEFINE tmp_det_cc_imp_am_cre          DECIMAL(14);
   DEFINE tmp_det_cc_imp_ren_viv         DECIMAL(14);
   DEFINE tmp_det_cc_marca_sua           CHAR(2);
   DEFINE tmp_det_cc_marca_bdnsar        CHAR(1);
   DEFINE tmp_det_cc_diag_aclaracion     CHAR(2);
   DEFINE tmp_det_cc_f_proceso           DATE;
   DEFINE tmp_det_cc_nss_dispersion      CHAR(11);
   DEFINE tmp_det_cc_paterno_afore       CHAR(40);
   DEFINE tmp_det_cc_materno_afore       CHAR(40);
   DEFINE tmp_det_cc_nombre_afore        CHAR(40);
   DEFINE tmp_det_cc_aivs                DECIMAL(24);
   DEFINE tmp_det_cc_valor_aiv           DECIMAL(24);
   DEFINE tmp_det_cc_int_gen_pgo_ext     DECIMAL(14);
   DEFINE tmp_det_cc_aiv_gen_pgo_ext     DECIMAL(24);

   -- variables para dejar registro del cambio de nombre
   DEFINE v_paterno_afore CHAR(40);
   DEFINE v_materno_afore CHAR(40);
   DEFINE v_nombre_afore  CHAR(40);


   --TABLA acl_det_cc_nss
   DEFINE acl_det_cc_ult_sdi         DECIMAL(7,2);
   DEFINE acl_det_cc_imp_ap_pat      DECIMAL(12,2);
   DEFINE acl_det_cc_imp_am_cre      DECIMAL(12,2);
   DEFINE acl_det_cc_imp_ren_viv     DECIMAL(12,2);
   DEFINE acl_det_cc_aivs            DECIMAL(18,6);
   DEFINE acl_det_cc_valor_aiv       DECIMAL(18,6);
   DEFINE acl_det_cc_int_gen_pgo_ext DECIMAL(12,2);
   DEFINE acl_det_cc_aiv_gen_pgo_ext DECIMAL(18,6);

   --TABLA tmp_sum_cc_nss
   DEFINE tmp_sum_cc_tpo_registro         CHAR(1);
   DEFINE tmp_sum_cc_tot_ap               DECIMAL(9);
   DEFINE tmp_sum_cc_suma_ap_pat          DECIMAL(20);
   DEFINE tmp_sum_cc_suma_am              DECIMAL(20);
   DEFINE tmp_sum_cc_suma_aivs            DECIMAL(20);
   DEFINE tmp_sum_cc_suma_int_viv_pgo_ext DECIMAL(20);
   DEFINE tmp_sum_cc_suma_aiv_pgo_ext     DECIMAL(20);

   --TABLA acl_sum_cc_nss
   DEFINE acl_sum_cc_folio                DECIMAL(9,0);
   DEFINE acl_sum_cc_tpo_registro         CHAR(2);
   DEFINE acl_sum_cc_tot_ap               INTEGER;
   DEFINE acl_sum_cc_suma_ap_pat          DECIMAL(18,2);
   DEFINE acl_sum_cc_suma_am              DECIMAL(18,2);
   DEFINE acl_sum_cc_suma_aivs            DECIMAL(18,6);
   DEFINE acl_sum_cc_suma_int_viv_pgo_ext DECIMAL(18,2);
   DEFINE acl_sum_cc_suma_aiv_pgo_ext     DECIMAL(18,6);
   DEFINE v_ide_derechohabiente           DECIMAL(9,0);
   
   DEFINE contador          INTEGER;
   DEFINE v_tipo_trabajador CHAR(1);

	 -- variables para registro de duplicados en pagos encontrados
	 DEFINE v_id_referencia       DECIMAL(9,0);
	 DEFINE v_folio_duplicado     DECIMAL(9,0);
	 DEFINE v_registro_usado      CHAR(1); -- indica que registro duplicado se uso para integrar
	 DEFINE v_contador_duplicados SMALLINT; -- para contar cuantos registros de pago en aclara se encuentran duplicados
	 DEFINE v_origen_archivo      SMALLINT;
	 DEFINE v_ind_cta             SMALLINT; -- 1 pago que se toma para aclaracion. 0 - pago duplicado

   -- Control de Excepciones
   DEFINE sql_err        INTEGER;
   DEFINE isam_err       INTEGER;
   DEFINE err_txt        VARCHAR(255);
   DEFINE v_c_msj        VARCHAR(255);
   DEFINE v_si_resultado SMALLINT;

   -- verificacion de registros recibidos                         
   DEFINE v_num_registros_detalle     DECIMAL (9,0); -- registros detalle
   DEFINE v_num_registros_detalle_sum DECIMAL (9,0); -- registros de detalle indicados en sumario

   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;
   DEFINE v_err_suma_intereses_vivienda_no_coincide     SMALLINT;
   DEFINE v_err_suma_AIVS_pago_extemporaneo_no_coincide SMALLINT;
   
   DEFINE v_ind_liquidacion  SMALLINT;
   DEFINE v_tpo_patron       CHAR(02);   
   DEFINE v_error_en_sumario SMALLINT; -- booleana para saber si hubo error en sumario
   DEFINE v_result_operacion SMALLINT; -- se asigna el valor original que es 1 = "aceptado"
   
   -- para el cambio de NSS
   DEFINE v_id_derechohabiente_nuevo DECIMAL(9,0);
   DEFINE v_tpo_aclaracion           CHAR (2);
   
   DEFINE v_folio_reg           DECIMAL(9,0);
   DEFINE v_id_referencia_reg   DECIMAL(12,2);
   DEFINE v_imp_ap_pat          DECIMAL(12,2);
   DEFINE v_imp_am_cre          DECIMAL(12,2);
   DEFINE v_imp_ren_viv_pgo_ext DECIMAL(12,2);
   DEFINE v_aiv_ap_pat          DECIMAL(18,6);
   DEFINE v_int_gen_pgo_ext     DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext     DECIMAL(18,6);   

   DEFINE v_result_opera SMALLINT;
   
   --g-   
   DEFINE v_cont SMALLINT;
   DEFINE v_cont_referencia DECIMAL(9,0);
   DEFINE v_rechazo SMALLINT;
   
   -- se comenta por que marca error de tmp_det_cc_nss con valor indefinido 27052014   
   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt, tmp_det_cc_nss;
   END EXCEPTION

   --g- ALTER SEQUENCE seq_cta_his_pagos RESTART 1;

   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   LET v_err_suma_intereses_vivienda_no_coincide     = 5;
   LET v_err_suma_AIVS_pago_extemporaneo_no_coincide = 6;
   
   LET v_tpo_patron = "";
   
   -- se inicia el contador
   LET contador = 1;
   
   LET v_result_operacion = 1; -- "aceptado"
   LET tmp_det_cc_nss = NULL;
   
   
   --SET DEBUG FILE TO '/safreviv_req/inc1389710/tra/sp_historicos_con_cambio_nss.trace'; 
   --TRACE 'Inicia el stored procedure de registro historicos de CON CAMBIO EN NSS';

   -- se agrega filtro de lectura de tablas para cambio de nss y cambio de nombre
   IF p_origen_archivo = 6 THEN
       -- se obtienen las sumas de montos de la tabla del sumario
       SELECT
          SUM(suma_ap_pat         ) / 100    , -- aportaciones patronales
          SUM(suma_am             ) / 100    , -- amortizacion
          SUM(suma_aivs           ) / 1000000, -- aplicacion de intereses de vivienda
          SUM(suma_int_viv_pgo_ext) / 100    , -- intereses de vivienda
          SUM(suma_aiv_pgo_ext    ) / 1000000 -- aplicacion de intereses de vivienda pagos extemporaneos
       INTO
          acl_sum_cc_suma_ap_pat          ,
          acl_sum_cc_suma_am              ,
          acl_sum_cc_suma_aivs            ,
          acl_sum_cc_suma_int_viv_pgo_ext ,
          acl_sum_cc_suma_aiv_pgo_ext
       FROM safre_tmp:tmp_sum_cc_nss;
      
      
       -- se obtiene la suma de los monto de la tabla de detalle
       SELECT
          SUM(imp_ap_pat     ) / 100    ,
          SUM(imp_am_cre     ) / 100    ,
          SUM(aivs           ) / 1000000,
          SUM(int_gen_pgo_ext) / 100    ,
          SUM(aiv_gen_pgo_ext) / 1000000
       INTO
          acl_det_cc_imp_ap_pat      , -- aportaciones patronales
          acl_det_cc_imp_am_cre      , -- amortizacion
          acl_det_cc_aivs            , -- aplicacion de intereses de vivienda
          acl_det_cc_int_gen_pgo_ext , -- intereses de vivienda
          acl_det_cc_aiv_gen_pgo_ext   -- aplicacion de intereses de vivienda pagos extemporaneos
       FROM safre_tmp:tmp_det_cc_nss;
      
      
       -- se verifica que el numero de registros en la tabla de detalle sea el mismo
       -- que el encontrado en el sumario
       SELECT COUNT(*)
       INTO v_num_registros_detalle
       FROM
          safre_tmp:tmp_det_cc_nss;
      
       SELECT tot_ap
       INTO v_num_registros_detalle_sum
       FROM safre_tmp:tmp_sum_cc_nss;      
   ELSE
       SELECT
          SUM(suma_ap_pat         ) / 100    , -- aportaciones patronales
          SUM(suma_am             ) / 100    , -- amortizacion
          SUM(suma_aivs           ) / 1000000, -- aplicacion de intereses de vivienda
          SUM(suma_int_viv_pgo_ext) / 100    , -- intereses de vivienda
          SUM(suma_aiv_pgo_ext    ) / 1000000 -- aplicacion de intereses de vivienda pagos extemporaneos
       INTO
          acl_sum_cc_suma_ap_pat          ,
          acl_sum_cc_suma_am              ,
          acl_sum_cc_suma_aivs            ,
          acl_sum_cc_suma_int_viv_pgo_ext ,
          acl_sum_cc_suma_aiv_pgo_ext
       FROM safre_tmp:tmp_sum_cc_nom;
      
      
       -- se obtiene la suma de los monto de la tabla de detalle
       SELECT
          SUM(imp_ap_pat     ) / 100    ,
          SUM(imp_am_cre     ) / 100    ,
          SUM(aivs           ) / 1000000,
          SUM(int_gen_pgo_ext) / 100    ,
          SUM(aiv_gen_pgo_ext) / 1000000
       INTO
          acl_det_cc_imp_ap_pat      , -- aportaciones patronales
          acl_det_cc_imp_am_cre      , -- amortizacion
          acl_det_cc_aivs            , -- aplicacion de intereses de vivienda
          acl_det_cc_int_gen_pgo_ext , -- intereses de vivienda
          acl_det_cc_aiv_gen_pgo_ext   -- aplicacion de intereses de vivienda pagos extemporaneos
       FROM safre_tmp:tmp_det_cc_nom;
      
       -- se verifica que el numero de registros en la tabla de detalle sea el mismo
       -- que el encontrado en el sumario
       SELECT COUNT(*)
       INTO v_num_registros_detalle
       FROM
          safre_tmp:tmp_det_cc_nom;
      
       SELECT tot_ap
       INTO v_num_registros_detalle_sum
       FROM safre_tmp:tmp_sum_cc_nom;  
   	
   END IF
   
   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;
   
   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- ========================================================================
   -- se verifican las cifras de montos totales recibidos
   -- ========================================================================

   -- aportaciones patronales
   IF ( acl_sum_cc_suma_ap_pat <> acl_det_cc_imp_ap_pat ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aportaciones patronales de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- amortizacion del credito
   IF ( acl_sum_cc_suma_am <> acl_det_cc_imp_am_cre ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- aplicacion de intereses de vivienda
   IF ( acl_sum_cc_suma_aivs <> acl_det_cc_aivs ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- intereses de vivienda pago extemporaneo
   IF ( acl_sum_cc_suma_int_viv_pgo_ext <> acl_det_cc_int_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_intereses_vivienda_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- aplicacion de intereses de vivienda pago extemporaneo
   IF ( acl_sum_cc_suma_aiv_pgo_ext <> acl_det_cc_aiv_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_pago_extemporaneo_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de pago extemporáneo de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
   END IF

   -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
   IF v_error_en_sumario = 1 THEN
      -- se actualiza el folio en turno como erroneo
      UPDATE glo_folio
      SET    status = -1
      WHERE  folio = p_folio;
      
      -- se deja el archivo en estatus de error
      UPDATE glo_ctr_archivo
      SET    folio = p_folio,
             estado = 2 -- integrado
      WHERE proceso_cod    = p_proceso_cod
      AND   opera_cod      = 1 -- archivo cargado
      AND   estado         = 1; -- etapa de carga
      
      -- se devuelve el error
      RETURN v_si_resultado, isam_err, err_txt, tmp_det_cc_nss;
   END IF

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    folio = p_folio;
       
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- EJECUTA DETALLE --
   
   EXECUTE PROCEDURE sp_registros_sin_cuenta_acl_cc(P_folio, p_origen_archivo)
   INTO v_si_resultado, isam_err, err_txt, tmp_det_cc_nss; 

   IF v_si_resultado <> 0 THEN
      RETURN v_si_resultado, isam_err, err_txt, tmp_det_cc_nss; 
   END IF;

   --TRACE 'inicia el foreach de la tabla tmp_det_cc_nss';
   
   --g- 
   LET v_cont_referencia = 0;
   
   FOREACH EXECUTE PROCEDURE sp_historicos_con_cambio_lee_temporales (p_origen_archivo) -- SELECT
      INTO v_ide_derechohabiente        ,  
           tmp_det_cc_tpo_registro       ,
           tmp_det_cc_cve_ent_receptora  ,
           tmp_det_cc_nrp                ,
           tmp_det_cc_rfc_patron         ,
           tmp_det_cc_periodo_pago       ,
           tmp_det_cc_f_pago_patron      ,
           tmp_det_cc_folio_sua          ,
           tmp_det_cc_nss                ,
           tmp_det_cc_rfc                ,
           tmp_det_cc_curp               ,
           tmp_det_cc_num_crd_ifv        ,
           tmp_det_cc_f_ini_desc_crd_ifv ,
           tmp_det_cc_num_mov_periodo    ,
           tmp_det_cc_nombre_trabajador  ,
           tmp_det_cc_ult_sdi            ,
           tmp_det_cc_tpo_trabajador     ,
           tmp_det_cc_jornada            ,
           tmp_det_cc_localiza_trabajador,
           tmp_det_cc_destino_ap_viv     ,
           tmp_det_cc_dias_cot_bim       ,
           tmp_det_cc_dias_incap_bim     ,
           tmp_det_cc_dias_ausent_bim    ,
           tmp_det_cc_imp_ap_pat         ,
           tmp_det_cc_imp_am_cre         ,
           tmp_det_cc_imp_ren_viv        ,
           tmp_det_cc_marca_sua          ,
           tmp_det_cc_marca_bdnsar       ,
           tmp_det_cc_diag_aclaracion    ,
           tmp_det_cc_f_proceso          ,
           tmp_det_cc_nss_dispersion     ,
           tmp_det_cc_paterno_afore      ,
           tmp_det_cc_materno_afore      ,
           tmp_det_cc_nombre_afore       ,
           tmp_det_cc_aivs               ,
           tmp_det_cc_valor_aiv          ,
           tmp_det_cc_int_gen_pgo_ext    ,
           tmp_det_cc_aiv_gen_pgo_ext

      --g-
      LET v_cont_referencia = v_cont_referencia + 1;

      -- Actualiza la precisión de los importes correspondientes
      LET acl_det_cc_ult_sdi         = tmp_det_cc_ult_sdi        ;
      LET acl_det_cc_imp_ap_pat      = tmp_det_cc_imp_ap_pat     ;
      LET acl_det_cc_imp_am_cre      = tmp_det_cc_imp_am_cre     ;
      LET acl_det_cc_imp_ren_viv     = tmp_det_cc_imp_ren_viv    ;
      LET acl_det_cc_aivs            = tmp_det_cc_aivs           ;
      LET acl_det_cc_valor_aiv       = tmp_det_cc_valor_aiv      ;
      LET acl_det_cc_int_gen_pgo_ext = tmp_det_cc_int_gen_pgo_ext;
      LET acl_det_cc_aiv_gen_pgo_ext = tmp_det_cc_aiv_gen_pgo_ext;
      
      -- asignación para cuando trae valores en nulo 12-feb-2012
      IF acl_det_cc_int_gen_pgo_ext IS NULL THEN
         LET acl_det_cc_aiv_gen_pgo_ext = 0;
      END IF
      
      IF acl_det_cc_aiv_gen_pgo_ext IS NULL THEN
         LET acl_det_cc_aiv_gen_pgo_ext = 0;
      END IF

      -- Inicializacion de variables
      LET v_tpo_patron = tmp_det_cc_nrp;
      
      LET v_tpo_aclaracion =" ";

--    --g- SE QUITA VALIDACIÓN DE DUPLICADOS DE ACUERDO A REUNIÓN DEL 18-NOV-2015
--    --g- EN LA OFICIAN DE LUIS FLORES
      { --g-
      -- nva funcionalidad para que no ingrese  --g-
      -- duplicados al reprocesar rechazados 5-nov-2015
      LET v_cont = 0;
      SELECT count(*)
      INTO   v_cont
      FROM   cta_his_pagos
      WHERE  id_derechohabiente = v_ide_derechohabiente
      AND    folio_sua          = tmp_det_cc_folio_sua
      AND    periodo_pago       = tmp_det_cc_periodo_pago
      AND    f_pago             = tmp_det_cc_f_pago_patron
      AND    nrp                = tmp_det_cc_nrp
      AND    cve_ent_receptora  = tmp_det_cc_cve_ent_receptora
      AND    imp_ap_pat         = tmp_det_cc_imp_ap_pat 
      AND    imp_am_cre         = tmp_det_cc_imp_am_cre;

      IF v_cont > 1 THEN   -- encontro duplicado y se genera excepcion
      	 INSERT INTO acl_rech_duplicados VALUES (p_folio,v_cont_referencia,p_origen_archivo);
      ELSE  
      } --g-

      IF v_ide_derechohabiente <> 52112212 THEN --xvi-141
         --======================================================================
         -- se coloca este query por orden de Hamir ya que dice que en estos 
         -- archivos no viene la aclaración y hay que obteneral de historico
         
         -- Hamir reafirma que el tpo_aclaracion lo obtenga del historico
         -- fecha reafirmacion 7-nov-2012 a las 12:23 dia del temblor
         -- de Chiapas de 7.5 y Oaxaca de 6.3
      
         -- 28Ago2014. Esta consulta se agrupa en un foreach para encontrar posibles duplicados
         -- se inicia el contador de duplicados
         LET v_contador_duplicados = 0;  

         FOREACH
            SELECT tpo_aclaracion,
                   folio,
                   id_referencia,
                   origen_archivo,
                   folio,
                   id_referencia,             
                   imp_ap_pat,
                   imp_am_cre,
                   imp_ren_viv_pgo_ext,
                   aiv_ap_pat,
                   int_gen_pgo_ext,
                   aiv_gen_pgo_ext
            INTO   v_tpo_aclaracion,
                   v_folio_duplicado,
                   v_id_referencia,
                   v_origen_archivo, 
                   v_folio_reg,
                   v_id_referencia_reg,
                   v_imp_ap_pat,
                   v_imp_am_cre,
                   v_imp_ren_viv_pgo_ext,
                   v_aiv_ap_pat,
                   v_int_gen_pgo_ext,
                   v_aiv_gen_pgo_ext            
            FROM   cta_his_pagos
            WHERE  id_derechohabiente = v_ide_derechohabiente
            AND    folio_sua          = tmp_det_cc_folio_sua
            AND    periodo_pago       = tmp_det_cc_periodo_pago
            AND    nrp                = tmp_det_cc_nrp
            AND    imp_ap_pat         = tmp_det_cc_imp_ap_pat 
            AND    imp_am_cre         = tmp_det_cc_imp_am_cre
            AND    origen_archivo     in (1,4)  -- buscar el tipo de aclaracion en los folios de LQINFO

--          SE QUITA VALIDACIÓN DEL CAMPO f_pago DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--          el campo iba después de periodo_pago
--                     AND    f_pago             = tmp_det_cc_f_pago_patron
--          SE QUITA VALIDACIÓN DEL CAMPO cve_ent_receptora DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--          el campo iba después de nrp
--                     AND    cve_ent_receptora  = tmp_det_cc_cve_ent_receptora
-----------------------------------------------------------------------------------------------------------------------------------------------------
--           AND    int_gen_pgo_ext    = tmp_det_cc_int_gen_pgo_ext  -- se cambia validación de pesos por aivs por solicitud de Hamir 5-feb-2013
--          SE QUITA VALIDACIÓN DEL CAMPO aiv_gen_pgo_ext DE ACUERDO
--          A REQUERIMIENTO JIRA PRODINF-241  22-ABR-2014
--           AND    aiv_gen_pgo_ext    = tmp_det_cc_aiv_gen_pgo_ext  -- se cambia validación de pesos por aivs por solicitud de Hamir 5-feb-2013
-----------------------------------------------------------------------------------------------------------------------------------------------------
         
            -- se cuenta un registro
            LET v_contador_duplicados = v_contador_duplicados + 1;
            
            -- se toma como el registro correcto el primero que se encuentre
            IF ( v_contador_duplicados = 1 ) THEN 
               LET v_ind_cta = 1; -- se marca el registro como el tomado para aclaracion
            ELSE
               LET v_ind_cta = 0; -- registro duplicado
            END IF
            
            -- se inserta el registro duplicado
            EXECUTE PROCEDURE sp_inserta_acl_his_duplicados(v_folio_duplicado,
                                                            v_origen_archivo,
                                                            v_ide_derechohabiente,
                                                            v_id_referencia,
                                                            tmp_det_cc_folio_sua,
                                                            tmp_det_cc_periodo_pago,
                                                            tmp_det_cc_f_pago_patron,
                                                            tmp_det_cc_nrp,
                                                            tmp_det_cc_cve_ent_receptora,
                                                            tmp_det_cc_imp_ap_pat,
                                                            tmp_det_cc_imp_am_cre,
                                                            tmp_det_cc_int_gen_pgo_ext,
                                                            v_ind_cta,
                                                            p_folio                             
                                                            );
         
         END FOREACH;
         
         -- si no hay duplicados, se borra la tabla de duplicados
         IF v_contador_duplicados = 1 THEN
            DELETE FROM acl_his_duplicados
            WHERE  id_derechohabiente = v_ide_derechohabiente
            AND    folio_sua          = tmp_det_cc_folio_sua
            AND    periodo_pago       = tmp_det_cc_periodo_pago
            AND    f_pago             = tmp_det_cc_f_pago_patron
            AND    nrp                = tmp_det_cc_nrp
            AND    cve_ent_receptora  = tmp_det_cc_cve_ent_receptora
            AND    imp_ap_pat         = tmp_det_cc_imp_ap_pat 
            AND    imp_am_cre         = tmp_det_cc_imp_am_cre
--            AND    int_gen_pgo_ext    = tmp_det_cc_int_gen_pgo_ext
            AND    folio_integracion  = p_folio;
         END IF
         
------   ---------------------------------------------------------------------------
------   ---------------------------------------------------------------------------
      END IF  --xvi-141

      IF v_ide_derechohabiente = 52112212 THEN
     	   INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,1,TODAY);  --xvi-141 código 1 Diferente nss en LQ o CI
      	 LET v_result_operacion = 2;
         LET v_tpo_aclaracion   = " ";
      ELSE
         IF (v_tpo_aclaracion IS NOT NULL OR v_tpo_aclaracion<>'') AND
         	  (tmp_det_cc_nss_dispersion ="" OR tmp_det_cc_nss_dispersion IS NULL) AND p_origen_archivo = 6 THEN
            LET v_result_operacion = 3;                                                -- se rechaza x inconsistencia x que nss disper viene en blancos
            INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,11,TODAY);   --xvi-141 código 11 Diferente nss nuevo en LQ o CI
         ELSE
         	  IF v_tpo_aclaracion = " " OR v_tpo_aclaracion IS NULL THEN
               LET v_result_operacion = 2;       -- se rechaza x inconsistencia x no tener reg historico anterior
               LET v_tpo_aclaracion   = " ";
               EXECUTE PROCEDURE sp_reg_rechazo_acl(p_folio,v_cont_referencia,v_ide_derechohabiente,tmp_det_cc_folio_sua,tmp_det_cc_periodo_pago,tmp_det_cc_f_pago_patron,tmp_det_cc_nrp,tmp_det_cc_cve_ent_receptora,tmp_det_cc_imp_ap_pat,tmp_det_cc_imp_am_cre) INTO v_rechazo; --xvi-141
            ELSE
               LET v_result_operacion = 1;
            END IF
         END IF
      END IF                

{
         IF v_tpo_aclaracion = " " OR v_tpo_aclaracion IS NULL THEN   --xvi-141
            IF tmp_det_cc_nss_dispersion ="" OR tmp_det_cc_nss_dispersion IS NULL THEN
               LET v_result_operacion = 3;       -- se rechaza x inconsistencia x que nss disper viene en blancos
               INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,11,TODAY);  --xvi-141 código 11 Diferente nss nuevo en LQ o CI
            ELSE
               LET v_result_operacion = 2;       -- se rechaza x inconsistencia x no tener reg historico anterior
               LET v_tpo_aclaracion   = " ";         	
            	
     	         IF v_ide_derechohabiente = 52112212 THEN   --xvi-141 nss en blanco
                  INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,1,TODAY);  --xvi-141 código 1 Diferente nss en LQ o CI            
               ELSE
                  EXECUTE PROCEDURE sp_reg_rechazo_acl(p_folio,v_cont_referencia,v_ide_derechohabiente,tmp_det_cc_folio_sua,tmp_det_cc_periodo_pago,tmp_det_cc_f_pago_patron,tmp_det_cc_nrp,tmp_det_cc_cve_ent_receptora,tmp_det_cc_imp_ap_pat,tmp_det_cc_imp_am_cre); --xvi-141
               END IF    --xvi-141
            END IF  

         ELSE
            LET v_result_operacion = 1;
         END IF
}

--====   ==================================================================
         
         -- Regla Negocio de Adelantamientos--
         
         IF v_tpo_patron = "99" THEN
         
            LET v_tipo_trabajador = "S";
            LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI
         
         ELSE
         
            LET v_tipo_trabajador = "I";
         
            IF (v_tpo_aclaracion = "13" OR
                v_tpo_aclaracion = "17") THEN
         
               LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS
         
            ELSE
               
--               IF v_tpo_aclaracion = " " THEN
               IF v_tpo_aclaracion = " " OR ((v_tpo_aclaracion IS NOT NULL OR v_tpo_aclaracion <> '') AND v_result_operacion = 3) THEN	

                  LET v_ind_liquidacion = 1;  --significa que no existe tpo_aclaracion
                              
               ELSE
               
                 LET v_ind_liquidacion = 5; --ACL normal liquidada
               
               END IF
            END IF
         END IF
         
--====   ==================================================================
         
         -- ============================================================
         -- SOLO PARA CAMBIO DE NSS
         IF p_origen_archivo = 6  THEN --AND tmp_det_cc_nss_dispersion <> "" THEN -- se agregó validación nss_dispersion en blanco 19-ago-2015 por incidente 849963 
         
             -- Obtiene el identificador del derechohabiente
            SELECT FIRST 1 id_derechohabiente
            INTO   v_id_derechohabiente_nuevo
            FROM   afi_derechohabiente
            WHERE  nss = tmp_det_cc_nss_dispersion;
            
--            IF v_c_nss IS NULL THEN
--               LET v_c_nss = "00000000000"
--            END IF     
           
         ELSE
            -- no se trata de cambio de NSS, no se abre la cuenta
            LET v_id_derechohabiente_nuevo = NULL;
            
            -- se obtiene el nombre original del trabajador para dejar registro
            -- del cambio de nombre
            SELECT nombre_af     ,
                   ap_paterno_af ,
                   ap_materno_af 
            INTO   v_nombre_afore ,
                   v_paterno_afore,
                   v_materno_afore
            FROM   afi_derechohabiente
            WHERE  id_derechohabiente = v_ide_derechohabiente;
            
            -- se guarda en el historico
            INSERT INTO afi_his_derechohabiente
               ( 
                id_derechohabiente  ,  
                f_modifica          ,
                folio_lote_modifica ,
                ind_modifica        ,
                curp                ,
                rfc                 ,
                ind_nrp             ,
                f_nacimiento        ,
                nombre_imss         ,
                nombre_af           ,
                ap_paterno_af       ,
                ap_materno_af       
               )
            VALUES 
               (
                v_ide_derechohabiente ,
                TODAY                 ,
                p_folio               ,
                6                     ,  --cambio nombre afore
                NULL                  ,
                NULL                  ,
                NULL                  ,  
                NULL                  ,
                NULL                  ,
                v_nombre_afore        ,
                v_paterno_afore       ,
                v_materno_afore
               );       
            
            -- se cambia el nombre del derechohabiente
            UPDATE afi_derechohabiente
            SET    nombre_af     = tmp_det_cc_nombre_afore,
                   ap_paterno_af = tmp_det_cc_paterno_afore,
                   ap_materno_af = tmp_det_cc_materno_afore
            WHERE  id_derechohabiente = v_ide_derechohabiente;
         
         END IF

	
         IF (v_result_operacion = 2  OR v_result_operacion = 3) THEN  --saci2018-67

            IF tmp_det_cc_destino_ap_viv = "1" AND tmp_det_cc_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
               INSERT INTO cta_especial_acl (
                  folio,
                  origen_archivo,
                  id_referencia ,
                  cve_ent_receptora,
                  nrp,
                  periodo_pago,
                  folio_sua,
                  f_pago,
                  id_derechohabiente,
                  localiza_trabajador,
                  tpo_aclaracion,
                  imp_ap_pat,
                  imp_am_cre,
                  imp_ren_viv_pgo_ext,
                  aiv_ap_pat,
                  valor_aiv,
                  int_gen_pgo_ext,
                  aiv_gen_pgo_ext,
                  result_operacion,
                  ind_liquidacion,
                  num_crd_ifv,
                  f_proceso,
                  tpo_patron,
                  folio_referencia,
                  destino_ap_viv
                  )
               VALUES (
                  p_folio,
                  p_origen_archivo,  -- origen_archivo=6 para acl_det_cc_nss
                  v_cont_referencia,
                  tmp_det_cc_cve_ent_receptora,
                  tmp_det_cc_nrp,
                  tmp_det_cc_periodo_pago,
                  tmp_det_cc_folio_sua,
                  tmp_det_cc_f_pago_patron,
                  v_ide_derechohabiente,
                  tmp_det_cc_localiza_trabajador,
                  v_tpo_aclaracion,
                  acl_det_cc_imp_ap_pat,
                  acl_det_cc_imp_am_cre,
                  acl_det_cc_imp_ren_viv,
                  acl_det_cc_aivs,
                  acl_det_cc_valor_aiv,
                  acl_det_cc_int_gen_pgo_ext,
                  acl_det_cc_aiv_gen_pgo_ext,
                  2,    --result_operacion,
                  v_ind_liquidacion,
                  tmp_det_cc_num_crd_ifv,
                  TODAY,
                  v_tpo_patron,
                  0,
                  tmp_det_cc_destino_ap_viv
                  );  --saci2018-67

               DELETE FROM cta_his_pagos                              --inc1389710
               WHERE  id_derechohabiente = v_ide_derechohabiente      --inc1389710
               AND    folio_sua          = tmp_det_cc_folio_sua       --inc1389710
               AND    periodo_pago       = tmp_det_cc_periodo_pago    --inc1389710
               AND    nrp                = tmp_det_cc_nrp             --inc1389710
               AND    imp_ap_pat         = acl_det_cc_imp_ap_pat      --inc1389710
               AND    imp_am_cre         = acl_det_cc_imp_am_cre;     --inc1389710

            ELSE                                        --saci2018-67-02
               INSERT INTO cta_rechazos_acl (
                  folio,
                  origen_archivo,
                  id_referencia ,
                  cve_ent_receptora,
                  nrp,
                  periodo_pago,
                  folio_sua,
                  f_pago,
                  id_derechohabiente,
                  localiza_trabajador,
                  tpo_aclaracion,
                  imp_ap_pat,
                  imp_am_cre,
                  imp_ren_viv_pgo_ext,
                  aiv_ap_pat,
                  valor_aiv,
                  int_gen_pgo_ext,
                  aiv_gen_pgo_ext,
                  result_operacion,
                  ind_liquidacion,
                  num_crd_ifv,
                  f_proceso,
                  tpo_patron,
                  folio_referencia,
                  destino_ap_viv
                  )
               VALUES (
                  p_folio,
                  p_origen_archivo,  -- origen_archivo=6 para acl_det_cc_nss
                  v_cont_referencia,
                  tmp_det_cc_cve_ent_receptora,
                  tmp_det_cc_nrp,
                  tmp_det_cc_periodo_pago,
                  tmp_det_cc_folio_sua,
                  tmp_det_cc_f_pago_patron,
                  v_ide_derechohabiente,
                  tmp_det_cc_localiza_trabajador,
                  v_tpo_aclaracion,
                  acl_det_cc_imp_ap_pat,
                  acl_det_cc_imp_am_cre,
                  acl_det_cc_imp_ren_viv,
                  acl_det_cc_aivs,
                  acl_det_cc_valor_aiv,
                  acl_det_cc_int_gen_pgo_ext,
                  acl_det_cc_aiv_gen_pgo_ext,
                  v_result_operacion,
                  v_ind_liquidacion,
                  tmp_det_cc_num_crd_ifv,
                  TODAY,
                  v_tpo_patron,
                  0,
                  tmp_det_cc_destino_ap_viv
                  );  --saci2018-67

            END IF                                      --saci2018-67-02
            
         ELSE     --   A C E P T A D O S  --

            IF tmp_det_cc_destino_ap_viv = "1" AND tmp_det_cc_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02

               INSERT INTO cta_especial_acl (
                  folio,
                  origen_archivo,
                  id_referencia ,
                  cve_ent_receptora,
                  nrp,
                  periodo_pago,
                  folio_sua,
                  f_pago,
                  id_derechohabiente,
                  localiza_trabajador,
                  tpo_aclaracion,
                  imp_ap_pat,
                  imp_am_cre,
                  imp_ren_viv_pgo_ext,
                  aiv_ap_pat,
                  valor_aiv,
                  int_gen_pgo_ext,
                  aiv_gen_pgo_ext,
                  result_operacion,
                  ind_liquidacion,
                  num_crd_ifv,
                  f_proceso,
                  tpo_patron,
                  folio_referencia,
                  destino_ap_viv
                  )
               VALUES (
                  p_folio,
                  p_origen_archivo,  -- origen_archivo=6 para acl_det_cc_nss
                  v_cont_referencia,
                  tmp_det_cc_cve_ent_receptora,
                  tmp_det_cc_nrp,
                  tmp_det_cc_periodo_pago,
                  tmp_det_cc_folio_sua,
                  tmp_det_cc_f_pago_patron,
                  v_ide_derechohabiente,
                  tmp_det_cc_localiza_trabajador,
                  v_tpo_aclaracion,
                  acl_det_cc_imp_ap_pat,
                  acl_det_cc_imp_am_cre,
                  acl_det_cc_imp_ren_viv,
                  acl_det_cc_aivs,
                  acl_det_cc_valor_aiv,
                  acl_det_cc_int_gen_pgo_ext,
                  acl_det_cc_aiv_gen_pgo_ext,
                  1,    --result_operacion,
                  v_ind_liquidacion,
                  tmp_det_cc_num_crd_ifv,
                  TODAY,
                  v_tpo_patron,
                  0,
                  tmp_det_cc_destino_ap_viv
                  );

               INSERT INTO cta_his_pagos (
                  folio,
                  origen_archivo,
                  id_referencia ,
                  cve_ent_receptora,
                  nrp,
                  periodo_pago,
                  folio_sua,
                  f_pago,
                  id_derechohabiente,
                  localiza_trabajador,
                  tpo_aclaracion,
                  imp_ap_pat,
                  imp_am_cre,
                  imp_ren_viv_pgo_ext,
                  aiv_ap_pat,
                  valor_aiv,
                  int_gen_pgo_ext,
                  aiv_gen_pgo_ext,
                  result_operacion,
                  ind_liquidacion,
                  num_crd_ifv,
                  f_proceso,
                  tpo_patron,
                  folio_referencia,
                  destino_ap_viv
                  )
               VALUES (
                  p_folio,
                  p_origen_archivo,  -- origen_archivo=6 para acl_det_cc_nss
                  v_cont_referencia,
                  tmp_det_cc_cve_ent_receptora,
                  tmp_det_cc_nrp,
                  tmp_det_cc_periodo_pago,
                  tmp_det_cc_folio_sua,
                  tmp_det_cc_f_pago_patron,
                  v_ide_derechohabiente,
                  tmp_det_cc_localiza_trabajador,
                  v_tpo_aclaracion,
                  acl_det_cc_imp_ap_pat,
                  acl_det_cc_imp_am_cre,
                  acl_det_cc_imp_ren_viv,
                  acl_det_cc_aivs,
                  acl_det_cc_valor_aiv,
                  acl_det_cc_int_gen_pgo_ext,
                  acl_det_cc_aiv_gen_pgo_ext,
                  v_result_operacion,
                  v_ind_liquidacion,
                  tmp_det_cc_num_crd_ifv,
                  TODAY,
                  v_tpo_patron,
                  0,
                  tmp_det_cc_destino_ap_viv
                  );

{ Quitar de aquí y colocar en sp_preliquida_sin_cambio_nss
               DELETE FROM cta_his_pagos                              --inc1389710
               WHERE  id_derechohabiente = v_ide_derechohabiente      --inc1389710
               AND    folio_sua          = tmp_det_cc_folio_sua       --inc1389710
               AND    periodo_pago       = tmp_det_cc_periodo_pago    --inc1389710
               AND    nrp                = tmp_det_cc_nrp             --inc1389710
               AND    imp_ap_pat         = acl_det_cc_imp_ap_pat      --inc1389710
               AND    imp_am_cre         = acl_det_cc_imp_am_cre      --inc1389710
               AND    folio              < p_folio;                   --inc1389710   
}

            ELSE                                        --saci2018-67-02
               -- Inserta a tabla cta_his_pagos para control de historicos
               INSERT INTO cta_his_pagos (
                  folio,
                  origen_archivo,
                  id_referencia ,
                  cve_ent_receptora,
                  nrp,
                  periodo_pago,
                  folio_sua,
                  f_pago,
                  id_derechohabiente,
                  localiza_trabajador,
                  tpo_aclaracion,
                  imp_ap_pat,
                  imp_am_cre,
                  imp_ren_viv_pgo_ext,
                  aiv_ap_pat,
                  valor_aiv,
                  int_gen_pgo_ext,
                  aiv_gen_pgo_ext,
                  result_operacion,
                  ind_liquidacion,
                  num_crd_ifv,
                  f_proceso,
                  tpo_patron,
                  folio_referencia,
                  destino_ap_viv
                  )
               VALUES (
                  p_folio,
                  p_origen_archivo,  -- origen_archivo=6 para acl_det_cc_nss
                  v_cont_referencia,
                  tmp_det_cc_cve_ent_receptora,
                  tmp_det_cc_nrp,
                  tmp_det_cc_periodo_pago,
                  tmp_det_cc_folio_sua,
                  tmp_det_cc_f_pago_patron,
                  v_ide_derechohabiente,
                  tmp_det_cc_localiza_trabajador,
                  v_tpo_aclaracion,
                  acl_det_cc_imp_ap_pat,
                  acl_det_cc_imp_am_cre,
                  acl_det_cc_imp_ren_viv,
                  acl_det_cc_aivs,
                  acl_det_cc_valor_aiv,
                  acl_det_cc_int_gen_pgo_ext,
                  acl_det_cc_aiv_gen_pgo_ext,
                  v_result_operacion,
                  v_ind_liquidacion,
                  tmp_det_cc_num_crd_ifv,
                  TODAY,
                  v_tpo_patron,
                  0,
                  tmp_det_cc_destino_ap_viv
                  );

            END IF                                      --saci2018-67-02
            
         END IF --saci2018-67
                     
         INSERT INTO cta_pag_complemento(
            folio,                --decimal(9,0)
            origen_archivo,       --smallint
            id_referencia,        --decimal(9,0)
            id_derechohabiente,   --decimal(9,0)
            rfc_patron,           --char(13)
            rfc,                  --char(13)
            curp,                 --char(18)
            num_mov_periodo,      --smallint
            f_ini_desc_crd_ifv,   --date
            ult_sdi,              --decimal(7,2)
            tpo_trabajador,       --char(1)
            jornada,              --char(1)
            destino_ap_viv,       --char(1)
            dias_cot_bim,         --smallint
            dias_incap_bim,       --smallint
            dias_ausent_bim,      --smallint
            marca_sua,            --char(2)
            marca_bdnsar,         --char(1)
            id_derhab_nuevo
            )
         VALUES (p_folio,
            p_origen_archivo,
            --g- seq_cta_his_pagos.CURRVAL,
            v_cont_referencia,
            v_ide_derechohabiente,
            tmp_det_cc_rfc_patron,
            tmp_det_cc_rfc,
            tmp_det_cc_curp,
            tmp_det_cc_num_mov_periodo,
            tmp_det_cc_f_ini_desc_crd_ifv,
            acl_det_cc_ult_sdi,
            tmp_det_cc_tpo_trabajador,
            tmp_det_cc_jornada,
            tmp_det_cc_destino_ap_viv,
            tmp_det_cc_dias_cot_bim,
            tmp_det_cc_dias_incap_bim,
            tmp_det_cc_dias_ausent_bim,
            tmp_det_cc_marca_sua,
            tmp_det_cc_marca_bdnsar,
            v_id_derechohabiente_nuevo
            );
                         
         IF v_result_operacion = 1 THEN
         		INSERT INTO acl_pag_registrado VALUES (
               p_folio,
               --g- seq_cta_his_pagos.CURRVAL,
               v_cont_referencia,
               v_imp_ap_pat,         
               v_imp_am_cre,         
               v_imp_ren_viv_pgo_ext,
               v_aiv_ap_pat,         
               v_int_gen_pgo_ext,    
               v_aiv_gen_pgo_ext,
               v_folio_reg,          
               v_id_referencia_reg,
               p_origen_archivo
               );              
         END IF
                                
         LET contador = contador + 1;

   --g-   END IF -- Se quita IF de validación de duplicados   
   END FOREACH

   --TRACE 'inicia el foreach de la tabla tmp_sum_sc_nss';
   -- ============================================================
   -- SOLO PARA CAMBIO DE NSS
   IF p_origen_archivo = 6 THEN

      FOREACH 
         SELECT tpo_registro,
                tot_ap,
                suma_ap_pat,
                suma_am,
                suma_aivs,
                suma_int_viv_pgo_ext,
                suma_aiv_pgo_ext
         INTO   tmp_sum_cc_tpo_registro,
                tmp_sum_cc_tot_ap,
                tmp_sum_cc_suma_ap_pat,
                tmp_sum_cc_suma_am,
                tmp_sum_cc_suma_aivs,
                tmp_sum_cc_suma_int_viv_pgo_ext,
                tmp_sum_cc_suma_aiv_pgo_ext
         FROM   safre_tmp:tmp_sum_cc_nss
      
         --TRACE li_contador;  
      
         --Actualizar la precision para los campos correspondientes
      
         LET acl_sum_cc_suma_ap_pat          = tmp_sum_cc_suma_ap_pat          / 100;
         LET acl_sum_cc_suma_am              = tmp_sum_cc_suma_am              / 100;
         LET acl_sum_cc_suma_aivs            = tmp_sum_cc_suma_aivs            / 1000000;
         LET acl_sum_cc_suma_int_viv_pgo_ext = tmp_sum_cc_suma_int_viv_pgo_ext / 100;
         LET acl_sum_cc_suma_aiv_pgo_ext     = tmp_sum_cc_suma_aiv_pgo_ext     / 1000000;
      
         INSERT INTO acl_sum_cc_nss (
            folio,
            tpo_registro,
            tot_ap,
            suma_ap_pat,
            suma_am,
            suma_aivs,
            suma_int_viv_pgo_ext,
            suma_aiv_pgo_ext
            )
         VALUES
            (
            p_folio,
            tmp_sum_cc_tpo_registro,
            tmp_sum_cc_tot_ap,
            acl_sum_cc_suma_ap_pat,
            acl_sum_cc_suma_am,
            acl_sum_cc_suma_aivs,
            acl_sum_cc_suma_int_viv_pgo_ext,
            acl_sum_cc_suma_aiv_pgo_ext
            );
      END FOREACH
   ELSE
      -- CAMBIO DE NOMBRE (ORIGEN ARCHIVO 7)
      FOREACH 
         SELECT tpo_registro,
                tot_ap,
                suma_ap_pat ,
                suma_am,
                suma_aivs,
                suma_int_viv_pgo_ext,
                suma_aiv_pgo_ext
         INTO   tmp_sum_cc_tpo_registro,
                tmp_sum_cc_tot_ap,
                tmp_sum_cc_suma_ap_pat ,
                tmp_sum_cc_suma_am,
                tmp_sum_cc_suma_aivs,
                tmp_sum_cc_suma_int_viv_pgo_ext,
                tmp_sum_cc_suma_aiv_pgo_ext
         FROM   safre_tmp:tmp_sum_cc_nom
      
         --TRACE li_contador;
      
         --Actualizar la precision para los campos correspondientes
      
         LET acl_sum_cc_suma_ap_pat          = tmp_sum_cc_suma_ap_pat          / 100;
         LET acl_sum_cc_suma_am              = tmp_sum_cc_suma_am              / 100;
         LET acl_sum_cc_suma_aivs            = tmp_sum_cc_suma_aivs            / 1000000;
         LET acl_sum_cc_suma_int_viv_pgo_ext = tmp_sum_cc_suma_int_viv_pgo_ext / 100;
         LET acl_sum_cc_suma_aiv_pgo_ext     = tmp_sum_cc_suma_aiv_pgo_ext     / 1000000;
      
         INSERT INTO acl_sum_cc_nss (
            folio,
            tpo_registro,
            tot_ap,
            suma_ap_pat,
            suma_am,
            suma_aivs,
            suma_int_viv_pgo_ext,
            suma_aiv_pgo_ext
            )
         VALUES
            (
            p_folio,
            tmp_sum_cc_tpo_registro,
            tmp_sum_cc_tot_ap,
            acl_sum_cc_suma_ap_pat,
            acl_sum_cc_suma_am,
            acl_sum_cc_suma_aivs,
            acl_sum_cc_suma_int_viv_pgo_ext,
            acl_sum_cc_suma_aiv_pgo_ext
            );
      END FOREACH
      
   END IF

   UPDATE STATISTICS FOR TABLE cta_his_pagos ;
   UPDATE STATISTICS FOR TABLE cta_pag_complemento;
   UPDATE STATISTICS FOR TABLE cta_especial_acl;
   UPDATE STATISTICS FOR TABLE cta_rechazos_acl;   	
   UPDATE STATISTICS FOR TABLE acl_sum_cc_nss  ;

   --TRACE 'Finaliza el store procedure de registro historicos de SIN CAMBIO EN NSS'; 

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";


   RETURN v_si_resultado, isam_err, err_txt, tmp_det_cc_nss;
   
END PROCEDURE;


