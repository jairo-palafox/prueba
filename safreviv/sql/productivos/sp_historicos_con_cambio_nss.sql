






CREATE PROCEDURE "safreviv".sp_historicos_con_cambio_nss(p_folio DECIMAL(9,0),
                                              p_pid DECIMAL(9,0),
                                              p_proceso_cod SMALLINT )
RETURNING SMALLINT, INTEGER, VARCHAR(255)

   --TABLA tmp_det_cc_nss
   DEFINE tmp_det_cc_tpo_registro         CHAR(1);
   DEFINE tmp_det_cc_cve_ent_receptora    CHAR(3);
   DEFINE tmp_det_cc_nrp                  CHAR(11);
   DEFINE tmp_det_cc_rfc_patron           CHAR(13);
   DEFINE tmp_det_cc_periodo_pago         CHAR(6);
   DEFINE tmp_det_cc_f_pago_patron        DATE;
   DEFINE tmp_det_cc_folio_sua            DECIMAL(6);
   DEFINE tmp_det_cc_nss                  CHAR(11);
   DEFINE tmp_det_cc_rfc                  CHAR(13);
   DEFINE tmp_det_cc_curp                 CHAR(18);
   DEFINE tmp_det_cc_num_crd_ifv          CHAR(10);
   DEFINE tmp_det_cc_f_ini_desc_crd_ifv   DATE;
   DEFINE tmp_det_cc_num_mov_periodo      DECIMAL(2);
   DEFINE tmp_det_cc_nombre_trabajador    CHAR(50);
   DEFINE tmp_det_cc_ult_sdi              CHAR(9);
   DEFINE tmp_det_cc_tpo_trabajador       CHAR(1);
   DEFINE tmp_det_cc_jornada              CHAR(1);
   DEFINE tmp_det_cc_localiza_trabajador  CHAR(1);
   DEFINE tmp_det_cc_destino_ap_viv       CHAR(1);
   DEFINE tmp_det_cc_dias_cot_bim         DECIMAL(2);
   DEFINE tmp_det_cc_dias_incap_bim       DECIMAL(2);
   DEFINE tmp_det_cc_dias_ausent_bim      DECIMAL(2);
   DEFINE tmp_det_cc_imp_ap_pat           DECIMAL(14);
   DEFINE tmp_det_cc_imp_am_cre           DECIMAL(14);
   DEFINE tmp_det_cc_imp_ren_viv          DECIMAL(14);
   DEFINE tmp_det_cc_marca_sua            CHAR(2);
   DEFINE tmp_det_cc_marca_bdnsar         CHAR(1);
   DEFINE tmp_det_cc_diag_aclaracion      CHAR(2);
   DEFINE tmp_det_cc_f_proceso            DATE;
   DEFINE tmp_det_cc_nss_dispersion       CHAR(11);
   DEFINE tmp_det_cc_paterno_afore        CHAR(40);
   DEFINE tmp_det_cc_materno_afore        CHAR(40);
   DEFINE tmp_det_cc_nombre_afore         CHAR(40);
   DEFINE tmp_det_cc_aivs                 DECIMAL(24);
   DEFINE tmp_det_cc_valor_aiv            DECIMAL(24);
   DEFINE tmp_det_cc_int_gen_pgo_ext      DECIMAL(14);
   DEFINE tmp_det_cc_aiv_gen_pgo_ext      DECIMAL(24);

   --TABLA acl_det_cc_nss
   DEFINE acl_det_cc_ult_sdi              DECIMAL(7,2);
   DEFINE acl_det_cc_imp_ap_pat           DECIMAL(12,2);
   DEFINE acl_det_cc_imp_am_cre           DECIMAL(12,2);
   DEFINE acl_det_cc_imp_ren_viv          DECIMAL(12,2);
   DEFINE acl_det_cc_aivs                 DECIMAL(18,6);
   DEFINE acl_det_cc_valor_aiv            DECIMAL(18,6);
   DEFINE acl_det_cc_int_gen_pgo_ext      DECIMAL(12,2);
   DEFINE acl_det_cc_aiv_gen_pgo_ext      DECIMAL(18,6);

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

   DEFINE ld_ide_derechohabiente          DECIMAL(9);
   DEFINE contador                        INTEGER;
   DEFINE v_tipo_trabajador               CHAR(1);

   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

   -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         SMALLINT; -- registros detalle
   DEFINE v_num_registros_detalle_sum     SMALLINT; -- registros de detalle indicados en sumario

   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;
   DEFINE v_err_suma_intereses_vivienda_no_coincide     SMALLINT;
   DEFINE v_err_suma_AIVS_pago_extemporaneo_no_coincide SMALLINT;
   
   DEFINE v_ind_liquidacion SMALLINT;
   DEFINE v_tpo_patron      CHAR(02);   

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


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
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET 
        folio = p_folio,
       estado = 2 -- integrado
     WHERE proceso_cod    = 103 -- en aclaracion
       AND folio = p_folio;
       
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;
         
   

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_historicos_con_cambio_nss.trace';
   --TRACE 'Inicia el stored procedure de registro historicos de CON CAMBIO EN NSS';
  
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
   

   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";

      RETURN v_si_resultado, isam_err, err_txt;
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

      RETURN v_si_resultado, isam_err, err_txt;
   END IF

   -- amortizacion del credito
   IF ( acl_sum_cc_suma_am <> acl_det_cc_imp_am_cre ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo";

      RETURN v_si_resultado, isam_err, err_txt;
   END IF

   -- aplicacion de intereses de vivienda
   IF ( acl_sum_cc_suma_aivs <> acl_det_cc_aivs ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";

      RETURN v_si_resultado, isam_err, err_txt;
   END IF

   -- intereses de vivienda pago extemporaneo
   IF ( acl_sum_cc_suma_int_viv_pgo_ext <> acl_det_cc_int_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_intereses_vivienda_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";

      RETURN v_si_resultado, isam_err, err_txt;
   END IF

   -- aplicacion de intereses de vivienda pago extemporaneo
   IF ( acl_sum_cc_suma_aiv_pgo_ext <> acl_det_cc_aiv_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_pago_extemporaneo_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de pago extemporáneo de detalle cargado no corresponde con el reportado en archivo";

      RETURN v_si_resultado, isam_err, err_txt;
   END IF
   



   --TRACE 'inicia el foreach de la tabla tmp_det_cc_nss';
   FOREACH SELECT
   	       tpo_registro         ,
           cve_ent_receptora    ,
           nrp                  ,
           rfc_patron           ,
           periodo_pago         ,
           f_pago_patron        ,
           folio_sua            ,
           nss                  ,
           rfc                  ,
           curp                 ,
           num_crd_ifv          ,
           f_ini_desc_crd_ifv   ,
           num_mov_periodo      ,
           nombre_trabajador    ,
           ult_sdi/100          ,
           tpo_trabajador       ,
           jornada              ,
           localiza_trabajador  ,
           destino_ap_viv       ,
           dias_cot_bim         ,
           dias_incap_bim       ,
           dias_ausent_bim      ,
           imp_ap_pat/100       ,
           imp_am_cre/100       ,
           imp_ren_viv/100      ,
           marca_sua            ,
           marca_bdnsar         ,
           diag_aclaracion      ,
           f_proceso            ,
           nss_dispersion       ,
           paterno_afore        ,
           materno_afore        ,
           nombre_afore         ,
           aivs/1000000         ,
           valor_aiv/1000000    ,
           int_gen_pgo_ext/100  ,
           aiv_gen_pgo_ext/1000000
      INTO tmp_det_cc_tpo_registro,
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
      FROM safre_tmp:tmp_det_cc_nss

      --TRACE li_contador;

--      IF ( tmp_det_cc_nrp[1,2] = "99" ) THEN
--      	 LET v_tipo_trabajador = "S";
--      ELSE
--         LET v_tipo_trabajador = "I";
--      END IF

      -- Actualiza la precisión de los importes correspondientes
      LET acl_det_cc_ult_sdi         = tmp_det_cc_ult_sdi        ;
      LET acl_det_cc_imp_ap_pat      = tmp_det_cc_imp_ap_pat     ;
      LET acl_det_cc_imp_am_cre      = tmp_det_cc_imp_am_cre     ;
      LET acl_det_cc_imp_ren_viv     = tmp_det_cc_imp_ren_viv    ;
      LET acl_det_cc_aivs            = tmp_det_cc_aivs           ;
      LET acl_det_cc_valor_aiv       = tmp_det_cc_valor_aiv      ;
      LET acl_det_cc_int_gen_pgo_ext = tmp_det_cc_int_gen_pgo_ext;
      LET acl_det_cc_aiv_gen_pgo_ext = tmp_det_cc_aiv_gen_pgo_ext;

      -- Inicializacion de variables
      LET ld_ide_derechohabiente = NULL;
      LET v_tpo_patron = tmp_det_cc_nrp;
      
--======================================================================
      -- Regla Negocio de Adelantamientos--

      IF v_tpo_patron = "99" THEN

         LET v_tipo_trabajador = "S";
         LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI

      ELSE

         LET v_tipo_trabajador = "I";

         IF (tmp_det_cc_diag_aclaracion = "13" OR
             tmp_det_cc_diag_aclaracion = "17") THEN

            LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

         ELSE

            LET v_ind_liquidacion = 5; --ACL normal liquidada

         END IF
      END IF

--======================================================================
      
      
      
      -- Obtiene el identificador del derechohabiente
      SELECT FIRST 1 id_derechohabiente
      INTO   ld_ide_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_det_cc_nss;

      -- Si no existe informacion en la tabla "afi_derechohabiente",
      -- entonces se inserta el registro en dicha tabla (aperturar una cuenta)
      IF ( ld_ide_derechohabiente IS NULL ) THEN

         --TRACE "tmp_det_cc_nss:"||tmp_det_cc_nss;
         --TRACE "tmp_det_cc_curp:"||tmp_det_cc_curp;
         --TRACE "tmp_det_cc_rfc:"||tmp_det_cc_rfc;
         --TRACE "tmp_det_cc_nombre_trabajador:"||tmp_det_cc_nombre_trabajador;

         LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(tmp_det_cc_nss ,
                                                             tmp_det_cc_curp,
                                                             tmp_det_cc_rfc ,
                                                             1,
                                                             tmp_det_cc_nombre_trabajador,
                                                             v_tipo_trabajador,
                                                             0, -- Credito. se da por omision
                                                             6,  -- 5 = ACLARATORIO CON CAMBIO NSS
                                                             p_folio, -- folio del lote
                                                             "R" -- origen afiliacion
                                                             );
      END IF

      -- Inserta a tabla cta_his_pagos para control de historicos
      INSERT INTO cta_his_pagos (
                  folio               ,
                  origen_archivo      ,
                  id_referencia       ,
                  cve_ent_receptora   ,
                  nrp                 ,
                  periodo_pago        ,
                  folio_sua           ,
                  f_pago              ,
                  id_derechohabiente  ,
                  localiza_trabajador ,
                  tpo_aclaracion      ,
                  imp_ap_pat          ,
                  imp_am_cre          ,
                  imp_ren_viv_pgo_ext ,
                  aiv_ap_pat          ,
                  valor_aiv           ,
                  int_gen_pgo_ext     ,
                  aiv_gen_pgo_ext     ,
                  result_operacion    ,
                  ind_liquidacion     ,
                  num_crd_ifv         ,
                  f_proceso           ,
                  tpo_patron          ,
                  folio_referencia
                 )
         VALUES (
                 p_folio          ,
                 6                ,  -- origen_archivo=6 para acl_det_cc_nss
                 seq_cta_his_pagos.NEXTVAL ,
                 tmp_det_cc_cve_ent_receptora  ,
                 tmp_det_cc_nrp                ,
                 tmp_det_cc_periodo_pago       ,
                 tmp_det_cc_folio_sua          ,
                 tmp_det_cc_f_pago_patron      ,
                 ld_ide_derechohabiente        ,
                 tmp_det_cc_localiza_trabajador ,
                 tmp_det_cc_diag_aclaracion    ,
                 acl_det_cc_imp_ap_pat         ,
                 acl_det_cc_imp_am_cre         ,
                 acl_det_cc_imp_ren_viv        ,
                 acl_det_cc_aivs               ,
                 acl_det_cc_valor_aiv          ,
                 acl_det_cc_int_gen_pgo_ext    ,
                 acl_det_cc_aiv_gen_pgo_ext    ,
                 NULL                          ,
                 v_ind_liquidacion             ,
                 tmp_det_cc_num_crd_ifv        ,
                 TODAY                         ,
                 v_tpo_patron                  ,
                 0);

     INSERT INTO cta_pag_complemento(
                              folio                --decimal(9,0)
                              ,origen_archivo       --smallint
                              ,id_referencia        --decimal(9,0)
                              ,id_derechohabiente   --decimal(9,0)
                              ,rfc_patron           --char(13)
                              ,rfc                  --char(13)
                              ,curp                 --char(18)
                              ,num_mov_periodo      --smallint
                              ,f_ini_desc_crd_ifv   --date
                              ,ult_sdi              --decimal(7,2)
                              ,tpo_trabajador       --char(1)
                              ,jornada              --char(1)
                              ,destino_ap_viv       --char(1)
                              ,dias_cot_bim         --smallint
                              ,dias_incap_bim       --smallint
                              ,dias_ausent_bim      --smallint
                              ,marca_sua            --char(2)
                              ,marca_bdnsar         --char(1)
                              )
                       VALUES (p_folio
                              ,6
                              ,seq_cta_his_pagos.CURRVAL
                              ,ld_ide_derechohabiente
                              ,tmp_det_cc_rfc_patron
                              ,tmp_det_cc_rfc
                              ,tmp_det_cc_curp
                              ,tmp_det_cc_num_mov_periodo
                              ,tmp_det_cc_f_ini_desc_crd_ifv
                              ,acl_det_cc_ult_sdi
                              ,tmp_det_cc_tpo_trabajador
                              ,tmp_det_cc_jornada
                              ,tmp_det_cc_destino_ap_viv
                              ,tmp_det_cc_dias_cot_bim
                              ,tmp_det_cc_dias_incap_bim
                              ,tmp_det_cc_dias_ausent_bim
                              ,tmp_det_cc_marca_sua
                              ,tmp_det_cc_marca_bdnsar
                              );
      LET contador = contador + 1;
   END FOREACH

   --TRACE 'inicia el foreach de la tabla tmp_sum_sc_nss';
   FOREACH SELECT
           tpo_registro         ,
           tot_ap               ,
           suma_ap_pat          ,
           suma_am              ,
           suma_aivs            ,
           suma_int_viv_pgo_ext ,
           suma_aiv_pgo_ext

      INTO tmp_sum_cc_tpo_registro         ,
           tmp_sum_cc_tot_ap               ,
           tmp_sum_cc_suma_ap_pat          ,
           tmp_sum_cc_suma_am              ,
           tmp_sum_cc_suma_aivs            ,
           tmp_sum_cc_suma_int_viv_pgo_ext ,
           tmp_sum_cc_suma_aiv_pgo_ext
      FROM safre_tmp:tmp_sum_cc_nss

      --TRACE li_contador;

      --Actualizar la precision para los campos correspondientes

      LET acl_sum_cc_suma_ap_pat          = tmp_sum_cc_suma_ap_pat          /100;
      LET acl_sum_cc_suma_am              = tmp_sum_cc_suma_am              /100;
      LET acl_sum_cc_suma_aivs            = tmp_sum_cc_suma_aivs            /1000000;
      LET acl_sum_cc_suma_int_viv_pgo_ext = tmp_sum_cc_suma_int_viv_pgo_ext /100;
      LET acl_sum_cc_suma_aiv_pgo_ext     = tmp_sum_cc_suma_aiv_pgo_ext     /1000000;

      INSERT INTO acl_sum_cc_nss (
            folio                 ,
            tpo_registro          ,
            tot_ap                ,
            suma_ap_pat           ,
            suma_am               ,
            suma_aivs             ,
            suma_int_viv_pgo_ext  ,
            suma_aiv_pgo_ext
           )
      VALUES
           (
            p_folio ,
            tmp_sum_cc_tpo_registro        ,
            tmp_sum_cc_tot_ap              ,
            acl_sum_cc_suma_ap_pat         ,
            acl_sum_cc_suma_am             ,
            acl_sum_cc_suma_aivs           ,
            acl_sum_cc_suma_int_viv_pgo_ext,
            acl_sum_cc_suma_aiv_pgo_ext
           );
   END FOREACH
   {
    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET 
        folio = p_folio,
       estado = 2 -- integrado
     WHERE proceso_cod    = 103 -- en aclaracion
       AND folio = p_folio;
       
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;
    }

   --UPDATE STATISTICS FOR TABLE acl_det_cc_nss  ;
   UPDATE STATISTICS FOR TABLE cta_his_pagos       ;
   UPDATE STATISTICS FOR TABLE acl_sum_cc_nss  ;

   --TRACE 'Finaliza el store procedure de registro historicos de SIN CAMBIO EN NSS';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt;
END PROCEDURE;


