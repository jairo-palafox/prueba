






CREATE PROCEDURE "safreviv".sp_dwh_extractor_retiros()
RETURNING INTEGER, INTEGER, CHAR(200);


   DEFINE v_total_registros   INTEGER;

   DEFINE r_sql_err        INTEGER;
   DEFINE r_isam_err       INTEGER;
   DEFINE r_err_txt        CHAR(200);

   DEFINE v_f_aiv               DATE; 
   DEFINE v_d_fvalor           CHAR(2);
   DEFINE v_m_fvalor           CHAR(2);
   DEFINE v_an_fvalor          CHAR(4);
   
   DEFINE v_d_liquida           CHAR(2);
   DEFINE v_m_liquida           CHAR(2);
   DEFINE v_an_liquida          CHAR(4);
   
   DEFINE v_monto_aivs_viv92    DECIMAL (20,2);
   DEFINE v_monto_pesos_viv92   DECIMAL (20,2);
   DEFINE v_monto_aivs_viv97    DECIMAL (20,2);
   DEFINE v_monto_pesos_viv97   DECIMAL (20,2);
   DEFINE v_id_derechohabiente  DECIMAL(10,0);
   DEFINE v_folio               DATE;
   DEFINE v_subcuenta           SMALLINT;
   DEFINE v_fondo               SMALLINT;
   DEFINE v_acciones            DECIMAL (20,2);
   DEFINE v_pesos               DECIMAL (20,2);
   DEFINE v_subcuenta_asoc      SMALLINT;
   DEFINE v_fecha_aiv           DATE; 
   DEFINE v_dia_fvalor           CHAR(2);
   DEFINE v_mes_fvalor           CHAR(2);
   DEFINE v_anio_fvalor          CHAR(4);
   
   DEFINE v_dia_liquida           CHAR(2);
   DEFINE v_mes_liquida           CHAR(2);
   DEFINE v_anio_liquida          CHAR(4);
   
   DEFINE v_tabla               CHAR(30);
   DEFINE v_sentencia           CHAR(1500);

   DEFINE v_nss                   CHAR(11)          ;
   DEFINE v_tpo_retiro            CHAR(1)           ;
   DEFINE v_tpo_seguro            CHAR(2)           ;
   DEFINE v_tpo_pension           CHAR(2)           ;
   DEFINE v_regimen               SMALLINT          ;
   DEFINE v_tpo_prestacion        CHAR(2)           ;
   DEFINE v_sec_pension           CHAR(2)           ;
   DEFINE v_tpo_movimiento        CHAR(3)           ;
   DEFINE v_anio_solicitud        CHAR(4)           ;
   DEFINE v_mes_solicitud         CHAR(2)           ;
   DEFINE v_anio_inicio_pension   CHAR(4)           ;
   DEFINE v_mes_inicio_pension    CHAR(2)           ;
   DEFINE v_anio_resolucion       CHAR(4)           ;
   DEFINE v_mes_resolucion        CHAR(2)           ;
   DEFINE v_anio_carga_datamart   CHAR(4)           ;
   DEFINE v_mes_carga_datamart    CHAR(2)           ;
   DEFINE v_porcentaje_valuacion  DECIMAL(5,2)      ;
   DEFINE v_semanas_cotizadas     SMALLINT          ;
   DEFINE v_cve_afore             SMALLINT          ;
   DEFINE v_f_aivs_sol            DATE              ;
   DEFINE v_monto_ssv92_sol       DECIMAL(20,2)     ;
   DEFINE v_aivs97_sol            DECIMAL(20,6)     ;
   DEFINE v_monto_ssv97_sol       DECIMAL(20,2)     ;
   DEFINE v_aivs92_sol            DECIMAL(20,6)     ;
   DEFINE v_estado_solicitud      SMALLINT          ;
   DEFINE v_cod_rechazo           SMALLINT          ;

   ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt
      RETURN r_sql_err, r_isam_err, r_err_txt;
   END EXCEPTION;

   LET r_sql_err  = 0;
   LET r_isam_err = 0;
   LET r_err_txt  = "";

   SET PDQPRIORITY HIGH;
 -- Extraccion de ret_disposicion -----

   SELECT id_derechohabiente,
          MAX(sec_pension) as sec_pension,
          MAX(id_solicitud) as id_solicitud
     FROM ret_disposicion
    GROUP BY id_derechohabiente
     INTO TEMP tmp_derechohabientes;

   CREATE TEMP TABLE tmp_pagadas (id_derechohabiente DECIMAL(10,0),
                                  monto_acciones     DECIMAL(16,6),
                                  monto_pesos        DECIMAL(12,2),
                                  f_valor            DATE         ,
                                  f_liquida          DATE         , 
                                  subcuenta          SMALLINT     ,
                                  folio_liquida      DECIMAL(9,0) ); 

   FOREACH SELECT tabla 
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_sentencia = " INSERT INTO tmp_pagadas " ||
                        " SELECT cta.id_derechohabiente, " ||
                               " cta.monto_acciones, " ||
                               " cta.monto_pesos, " ||
                               " cta.f_valor, " ||
                               " cta.f_liquida, " ||
                               " cta.subcuenta , " ||
                               " cta.folio_liquida  " ||
                          " FROM " || TRIM(v_tabla) || " cta, "||
                               " ret_disposicion ret "||
                         " WHERE ret.folio = cta.folio_liquida "||
                         "   AND ret.id_derechohabiente = cta.id_derechohabiente "||
                         "   AND ret.id_solicitud = cta.id_referencia "||
                         "   AND subcuenta IN (8,4) "||
                         "   AND fondo_inversion = 11 ";
      EXECUTE IMMEDIATE v_sentencia;

   END FOREACH
   CREATE INDEX xe1_tmp_pag_folio ON tmp_pagadas (folio_liquida);
   UPDATE STATISTICS FOR TABLE tmp_pagadas;

   SELECT tmpd.id_derechohabiente,
          rmt.tpo_retiro,
          '' subcta_asoc ,
          rmt.tpo_seguro ,
          rmt.tpo_pension ,
          rmt.regimen ,
          rmt.tpo_prestacion,
          tmpd.sec_pension ,
          '' tpo_movimiento, -- rtr.tpo_movimiento  ,
          YEAR(ret.f_solicitud) as anio_solicitud ,
          MONTH(ret.f_solicitud) as mes_solicitud,
          YEAR (ret.f_inicio_pension) as anio_inicio_pension,
          MONTH (ret.f_inicio_pension) as mes_inicio_pension ,
          YEAR (ret.f_resolucion) as anio_resolucion ,
          MONTH (ret.f_resolucion) as mes_resolucion ,
          '' anio_carga_datamart,  --YEAR(rtr.f_carga_datamart) as anio_carga_datamart,
          ''  mes_carga_datamart,  --MONTH (rtr.f_carga_datamart) as mes_carga_datamart ,
          ret.porcentaje_valuacion ,
          ret.semanas_cotizadas  ,
          ret.cve_afore  ,
          ret.f_solicitud as f_aivs_sol ,
          ret.aivs_viv92 as aivs92_sol ,
          '' monto_ssv92_sol ,
          ret.aivs_viv97 as aivs97_sol ,
          '' monto_ssv97_sol,
          '' f_aivs_pag ,
          '' aivs92_pag ,
          '' monto_ssv92_pag,
          '' aivs97_pag ,
          '' monto_ssv97_pag ,
          ret.estado_solicitud ,
          ret.cod_rechazo,
          ret.folio
     FROM tmp_derechohabientes tmpd ,
          ret_disposicion ret ,
          ret_matriz_derecho rmt
    WHERE tmpd.id_solicitud = ret.id_solicitud AND
          ret.id_ret_matriz_derecho = rmt.id_ret_matriz_derecho
     INTO TEMP tmp_retiros;

   CREATE INDEX xpk_tmp_ret_dwh ON tmp_retiros (id_derechohabiente);
   UPDATE STATISTICS FOR TABLE tmp_retiros;

   FOREACH SELECT tmp.id_derechohabiente,
             tmp.folio,
             afi.nss,
             tmp.tpo_retiro,
             tmp.tpo_seguro ,
             tmp.tpo_pension ,
             tmp.regimen ,
             tmp.tpo_prestacion,
             tmp.sec_pension ,
             tmp.tpo_movimiento  ,
             tmp.anio_solicitud,
             tmp.mes_solicitud,
             tmp.anio_inicio_pension,
             tmp.mes_inicio_pension ,
             tmp.anio_resolucion ,
             tmp.mes_resolucion ,
             tmp.anio_carga_datamart,
             tmp.mes_carga_datamart ,
             tmp.porcentaje_valuacion ,
             tmp.semanas_cotizadas  ,
             tmp.cve_afore  ,
             tmp.f_aivs_sol ,
             tmp.aivs92_sol ,
             tmp.monto_ssv92_sol ,
             tmp.aivs97_sol ,
             tmp.monto_ssv97_sol,
             tmp.estado_solicitud ,
             tmp.cod_rechazo
        INTO v_id_derechohabiente   ,
             v_folio                ,
             v_nss                  ,
             v_tpo_retiro           ,
             v_tpo_seguro           ,
             v_tpo_pension          ,
             v_regimen              ,
             v_tpo_prestacion       ,
             v_sec_pension          ,
             v_tpo_movimiento       ,
             v_anio_solicitud       ,
             v_mes_solicitud        ,
             v_anio_inicio_pension  ,
             v_mes_inicio_pension   ,
             v_anio_resolucion      ,
             v_mes_resolucion       ,
             v_anio_carga_datamart  ,
             v_mes_carga_datamart   ,
             v_porcentaje_valuacion ,
             v_semanas_cotizadas    ,
             v_cve_afore            ,
             v_f_aivs_sol           ,
             v_aivs92_sol           ,
             v_monto_ssv92_sol      ,
             v_aivs97_sol           ,
             v_monto_ssv97_sol      ,
             v_estado_solicitud     ,
             v_cod_rechazo          
        FROM tmp_retiros tmp,
             afi_derechohabiente afi
       WHERE afi.id_derechohabiente = tmp.id_derechohabiente


      LET v_monto_aivs_viv92   = 0;
      LET v_monto_pesos_viv92  = 0;
      LET v_monto_aivs_viv97   = 0;
      LET v_monto_pesos_viv97  = 0;
      LET v_subcuenta_asoc    = 0;
      LET v_fecha_aiv         = NULL ;
      LET v_dia_fvalor        = NULL ;  
			LET v_mes_fvalor        = NULL ;  
			LET v_anio_fvalor       = NULL ;  
			LET v_dia_liquida       = NULL ;  
			LET v_mes_liquida       = NULL ;  
			LET v_anio_liquida      = NULL ;   

      FOREACH SELECT tmpg.monto_acciones,
                     tmpg.monto_pesos,
                     DAY(tmpg.f_valor) ,
                     MONTH(tmpg.f_valor),
                     YEAR (tmpg.f_valor) ,
                     DAY(tmpg.f_liquida) ,
                     MONTH(tmpg.f_liquida),
                     YEAR (tmpg.f_liquida) ,
                     tmpg.f_valor ,
                     tmpg.subcuenta
                INTO v_acciones,
                     v_pesos,
                     v_d_fvalor, 
                     v_m_fvalor,
                     v_an_fvalor,
                     v_d_liquida, 
                     v_m_liquida ,
                     v_an_liquida,
                     v_f_aiv, 
                     v_subcuenta
                FROM tmp_pagadas tmpg
               WHERE tmpg.folio_liquida = v_folio
                 AND tmpg.id_derechohabiente = v_id_derechohabiente
                 
                 
               



         IF(v_subcuenta = 4 ) THEN
            LET v_monto_pesos_viv97 = v_pesos;
            LET v_monto_aivs_viv97  = v_acciones;
            LET v_dia_fvalor   = v_d_fvalor ;
            LET v_mes_fvalor   = v_m_fvalor ;
            LET v_anio_fvalor  = v_an_fvalor ;
            LET v_dia_liquida  = v_d_liquida ;
            LET v_mes_liquida  = v_m_liquida ;
            LET v_anio_liquida = v_an_liquida ;
            LET v_fecha_aiv = v_f_aiv ;
            LET v_subcuenta_asoc = v_subcuenta ;
         ELIF(v_subcuenta = 8) THEN
            LET v_monto_pesos_viv92 = v_pesos;
            LET v_monto_aivs_viv92  = v_acciones;
            LET v_dia_fvalor   = v_d_fvalor ;
            LET v_mes_fvalor   = v_m_fvalor ;
            LET v_anio_fvalor  = v_an_fvalor ;
            LET v_dia_liquida  = v_d_liquida ;
            LET v_mes_liquida  = v_m_liquida ;
            LET v_anio_liquida = v_an_liquida ;
            LET v_fecha_aiv = v_f_aiv ;
            LET v_subcuenta_asoc = v_subcuenta ;
         END IF
      END FOREACH

      INSERT INTO safre_sdo@vivws_tcp:saci_retiros VALUES (v_nss                  , 
                                                           v_tpo_retiro           , 
                                                           v_subcuenta_asoc       , 
                                                           v_tpo_seguro           , 
                                                           v_tpo_pension          , 
                                                           v_regimen              , 
                                                           v_tpo_prestacion       , 
                                                           v_sec_pension          , 
                                                           v_tpo_movimiento       , 
                                                           v_anio_solicitud       , 
                                                           v_mes_solicitud        , 
                                                           v_anio_inicio_pension  , 
                                                           v_mes_inicio_pension   , 
                                                           v_anio_resolucion      , 
                                                           v_mes_resolucion       , 
                                                           v_anio_carga_datamart  , 
                                                           v_mes_carga_datamart   , 
                                                           v_porcentaje_valuacion , 
                                                           v_semanas_cotizadas    , 
                                                           v_cve_afore            , 
                                                           v_f_aivs_sol           , 
                                                           v_aivs92_sol           , 
                                                           v_monto_ssv92_sol      , 
                                                           v_aivs97_sol           , 
                                                           v_monto_ssv97_sol      , 
                                                           v_anio_liquida         ,
                   																				 v_mes_liquida 					,
                   																				 v_dia_liquida					,
                   																				 v_anio_fvalor          ,
                   																				 v_mes_fvalor           ,
                   																				 v_dia_fvalor           ,
                   																				 v_fecha_aiv            ,
                                                           v_monto_aivs_viv92     ,      
                                                           v_monto_pesos_viv92    ,     
                                                           v_monto_aivs_viv97     ,      
                                                           v_monto_pesos_viv97    ,     
                                                           v_estado_solicitud     , 
                                                           v_cod_rechazo)         ;
   END FOREACH

-- Extraccion de ret_transferencia -----

   DROP TABLE IF EXISTS tmp_derechohabientes;
   DROP TABLE IF EXISTS tmp_pagadas;
   DROP TABLE IF EXISTS tmp_retiros;

   SELECT id_derechohabiente,
          MAX(sec_pension) as sec_pension,
          MAX(id_solicitud) as id_solicitud
     FROM ret_transferencia
    GROUP BY id_derechohabiente
     INTO TEMP tmp_derechohabientes;

   CREATE TEMP TABLE tmp_pagadas (id_derechohabiente DECIMAL(10,0),
                                  monto_acciones     DECIMAL(16,6),
                                  monto_pesos        DECIMAL(12,2),
                                  f_valor            DATE         ,
                                  f_liquida          DATE         , 
                                  subcuenta          SMALLINT     ,
                                  folio_liquida      DECIMAL(9,0) ); 

   FOREACH SELECT tabla 
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_sentencia = " INSERT INTO tmp_pagadas " ||
                        " SELECT cta.id_derechohabiente, " ||
                               " cta.monto_acciones, " ||
                               " cta.monto_pesos, " ||
                               " cta.f_valor, " ||
                               " cta.f_liquida, " ||
                               " cta.subcuenta , " ||
                               " cta.folio_liquida  " ||
                          " FROM " || TRIM(v_tabla) || " cta, "||
                               " ret_transferencia ret "||
                         " WHERE ret.folio = cta.folio_liquida "||
                         "   AND ret.id_derechohabiente = cta.id_derechohabiente "||
                         "   AND ret.id_solicitud = cta.id_referencia "||
                         "   AND subcuenta IN (8,4) "||
                         "   AND fondo_inversion = 11 ;" ;
 
      EXECUTE IMMEDIATE v_sentencia;

   END FOREACH

   CREATE INDEX xe1_tmp_pag_folio ON tmp_pagadas (folio_liquida);
   UPDATE STATISTICS FOR TABLE tmp_pagadas;

   SELECT tmpd.id_derechohabiente,
          rmt.tpo_retiro,
          '' subcta_asoc ,
          rmt.tpo_seguro ,
          rmt.tpo_pension ,
          rmt.regimen ,
          rmt.tpo_prestacion,
          tmpd.sec_pension ,
          ret.tpo_movimiento  ,
          '' anio_solicitud , --YEAR(ret.f_solicitud) as anio_solicitud ,
          '' mes_solicitud , --MONTH(ret.f_solicitud) as mes_solicitud,
          YEAR (ret.f_inicio_pension) as anio_inicio_pension,
          MONTH (ret.f_inicio_pension) as mes_inicio_pension ,
          YEAR (ret.f_resolucion) as anio_resolucion ,
          MONTH (ret.f_resolucion) as mes_resolucion ,
          YEAR(ret.f_carga_datamart) as anio_carga_datamart,
          MONTH (ret.f_carga_datamart) as mes_carga_datamart ,
          ret.porcentaje_valuacion ,
          ret.semanas_cotizadas  ,
          ret.cve_afore  ,
          '' f_aivs_sol , --ret.f_solicitud as f_aivs_sol ,
          '' aivs92_sol , --ret.aivs_viv92 as aivs92_sol ,
          '' monto_ssv92_sol ,
          ret.aivs_viv97 as aivs97_sol ,
          '' monto_ssv97_sol,
          '' f_aivs_pag ,
          '' aivs92_pag ,
          '' monto_ssv92_pag,
          '' aivs97_pag ,
          '' monto_ssv97_pag ,
          ret.estado_solicitud ,
          ret.cod_rechazo,
          ret.folio
     FROM tmp_derechohabientes tmpd ,
          ret_transferencia ret ,
          ret_matriz_derecho rmt
    WHERE tmpd.id_solicitud = ret.id_solicitud
      AND ret.id_ret_matriz_derecho = rmt.id_ret_matriz_derecho
     INTO TEMP tmp_retiros;

   CREATE INDEX xpk_tmp_ret_dwh ON tmp_retiros (id_derechohabiente);
   UPDATE STATISTICS FOR TABLE tmp_retiros;

   FOREACH SELECT tmp.id_derechohabiente,
             tmp.folio,
             afi.nss,
             tmp.tpo_retiro,
             tmp.tpo_seguro ,
             tmp.tpo_pension ,
             tmp.regimen ,
             tmp.tpo_prestacion,
             tmp.sec_pension ,
             tmp.tpo_movimiento  ,
             tmp.anio_solicitud,
             tmp.mes_solicitud,
             tmp.anio_inicio_pension,
             tmp.mes_inicio_pension ,
             tmp.anio_resolucion ,
             tmp.mes_resolucion ,
             tmp.anio_carga_datamart,
             tmp.mes_carga_datamart ,
             tmp.porcentaje_valuacion ,
             tmp.semanas_cotizadas  ,
             tmp.cve_afore  ,
             tmp.f_aivs_sol ,
             tmp.aivs92_sol ,
             tmp.monto_ssv92_sol ,
             tmp.aivs97_sol ,
             tmp.monto_ssv97_sol,
             tmp.estado_solicitud ,
             tmp.cod_rechazo
        INTO v_id_derechohabiente   ,
             v_folio                ,
             v_nss                  ,
             v_tpo_retiro           ,
             v_tpo_seguro           ,
             v_tpo_pension          ,
             v_regimen              ,
             v_tpo_prestacion       ,
             v_sec_pension          ,
             v_tpo_movimiento       ,
             v_anio_solicitud       ,
             v_mes_solicitud        ,
             v_anio_inicio_pension  ,
             v_mes_inicio_pension   ,
             v_anio_resolucion      ,
             v_mes_resolucion       ,
             v_anio_carga_datamart  ,
             v_mes_carga_datamart   ,
             v_porcentaje_valuacion ,
             v_semanas_cotizadas    ,
             v_cve_afore            ,
             v_f_aivs_sol           ,
             v_aivs92_sol           ,
             v_monto_ssv92_sol      ,
             v_aivs97_sol           ,
             v_monto_ssv97_sol      ,
             v_estado_solicitud     ,
             v_cod_rechazo          
        FROM tmp_retiros tmp,
             afi_derechohabiente afi
       WHERE afi.id_derechohabiente = tmp.id_derechohabiente
       
       LET v_monto_aivs_viv92   = 0;
      LET v_monto_pesos_viv92  = 0;
      LET v_monto_aivs_viv97   = 0;
      LET v_monto_pesos_viv97  = 0;
      LET v_subcuenta_asoc    = 0;
      LET v_fecha_aiv         = NULL ;
      LET v_dia_fvalor        = NULL ;  
			LET v_mes_fvalor        = NULL ;  
			LET v_anio_fvalor       = NULL ;  
			LET v_dia_liquida       = NULL ;  
			LET v_mes_liquida       = NULL ;  
			LET v_anio_liquida      = NULL ;  


      FOREACH SELECT tmpg.monto_acciones,
                     tmpg.monto_pesos,
                     DAY(tmpg.f_valor) ,
                     MONTH(tmpg.f_valor),
                     YEAR (tmpg.f_valor) ,
                     DAY(tmpg.f_liquida) ,
                     MONTH(tmpg.f_liquida),
                     YEAR (tmpg.f_liquida) ,
                     tmpg.f_valor ,
                     tmpg.subcuenta
                INTO v_acciones,
                     v_pesos,
                     v_d_fvalor, 
                     v_m_fvalor,
                     v_an_fvalor,
                     v_d_liquida, 
                     v_m_liquida ,
                     v_an_liquida,
                     v_f_aiv ,
                     v_subcuenta
                FROM tmp_pagadas tmpg
               WHERE tmpg.folio_liquida = v_folio
                 AND tmpg.id_derechohabiente = v_id_derechohabiente
                 
                 
               



         IF(v_subcuenta = 4 ) THEN
            LET v_monto_pesos_viv97 = v_pesos;
            LET v_monto_aivs_viv97  = v_acciones;
            LET v_dia_fvalor   = v_d_fvalor ;
            LET v_mes_fvalor   = v_m_fvalor ;
            LET v_anio_fvalor  = v_an_fvalor ;
            LET v_dia_liquida  = v_d_liquida ;
            LET v_mes_liquida  = v_m_liquida ;
            LET v_anio_liquida = v_an_liquida ;
            LET v_fecha_aiv = v_f_aiv ;
            LET v_subcuenta_asoc = v_subcuenta ;
         ELIF(v_subcuenta = 8) THEN
            LET v_monto_pesos_viv92 = v_pesos;
            LET v_monto_aivs_viv92  = v_acciones;
            LET v_dia_fvalor   = v_d_fvalor ;
            LET v_mes_fvalor   = v_m_fvalor ;
            LET v_anio_fvalor  = v_an_fvalor ;
            LET v_dia_liquida  = v_d_liquida ;
            LET v_mes_liquida  = v_m_liquida ;
            LET v_anio_liquida = v_an_liquida ;
            LET v_fecha_aiv = v_f_aiv ;
            LET v_subcuenta_asoc = v_subcuenta ;
         END IF
      END FOREACH

      INSERT INTO safre_sdo@vivws_tcp:saci_retiros VALUES (v_nss                  , 
                                                           v_tpo_retiro           , 
                                                           v_subcuenta_asoc       , 
                                                           v_tpo_seguro           , 
                                                           v_tpo_pension          , 
                                                           v_regimen              , 
                                                           v_tpo_prestacion       , 
                                                           v_sec_pension          , 
                                                           v_tpo_movimiento       , 
                                                           v_anio_solicitud       , 
                                                           v_mes_solicitud        , 
                                                           v_anio_inicio_pension  , 
                                                           v_mes_inicio_pension   , 
                                                           v_anio_resolucion      , 
                                                           v_mes_resolucion       , 
                                                           v_anio_carga_datamart  , 
                                                           v_mes_carga_datamart   , 
                                                           v_porcentaje_valuacion , 
                                                           v_semanas_cotizadas    , 
                                                           v_cve_afore            , 
                                                           v_f_aivs_sol           , 
                                                           v_aivs92_sol           , 
                                                           v_monto_ssv92_sol      , 
                                                           v_aivs97_sol           , 
                                                           v_monto_ssv97_sol      , 
                                                           v_anio_liquida         ,
                   																				 v_mes_liquida 					,
                   																				 v_dia_liquida					,
                   																				 v_anio_fvalor          ,
                   																				 v_mes_fvalor           ,
                   																				 v_dia_fvalor           ,
                   																				 v_fecha_aiv            ,
                                                           v_monto_aivs_viv92     ,      
                                                           v_monto_pesos_viv92    ,     
                                                           v_monto_aivs_viv97     ,      
                                                           v_monto_pesos_viv97    ,     
                                                           v_estado_solicitud     , 
                                                           v_cod_rechazo)         ;
   END FOREACH

     
   SET PDQPRIORITY DEFAULT;

   SELECT count(*)
     INTO v_total_registros
     FROM safre_sdo@vivws_tcp:saci_retiros;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh VALUES (TODAY,
                                                       "saci_retiros",
                                                       TODAY,
                                                       v_total_registros,
                                                       1);

   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE;


