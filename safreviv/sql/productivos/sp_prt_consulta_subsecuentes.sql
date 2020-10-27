






CREATE PROCEDURE "safreviv".sp_prt_consulta_subsecuentes()
RETURNING DECIMAL(9,0)    ,
          DECIMAL(9,0)    ,
          CHAR(11)        ,
          CHAR(18)        ,
          CHAR(40)        ,
          CHAR(40)        ,
          CHAR(40)        ,
          DECIMAL(10,0)    ,
          CHAR(100)       ,
          CHAR(100)       ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DECIMAL(22,2)   ,
          DATE            ,
          DATE,
          DATE            ,
          DATE            ,
          DATE            ,
          SMALLINT        ,
          VARCHAR(20)     ,
          SMALLINT        ,
          CHAR(2)         ,
          CHAR(3)         ,
          CHAR(50)        ;


DEFINE v_s_id_prt_solicitud_cedente           DECIMAL(9,0)    ;
DEFINE v_s_nss                                CHAR(11)        ;
DEFINE v_s_curp                               CHAR(18)        ;
DEFINE v_s_paterno                            CHAR(40)        ;
DEFINE v_s_materno                            CHAR(40)        ;
DEFINE v_s_nombre                             CHAR(40)        ;
DEFINE v_s_id_credito_fovissste               DECIMAL(10,0)    ;
DEFINE v_s_correo_e                           CHAR(100)       ;
DEFINE v_s_telefono                           CHAR(100)       ;
DEFINE v_s_saldo_insoluto_credito_fovissste   DECIMAL(22,2)   ;
DEFINE v_s_aivs_saldo_viv97_infonavit         DECIMAL(22,2)   ;
DEFINE v_s_pesos_saldo_viv97_infonavit        DECIMAL(22,2)   ;
DEFINE v_s_aivs_saldo_viv97_afore             DECIMAL(22,2)   ;
DEFINE v_s_pesos_saldo_viv97_afore            DECIMAL(22,2)   ;
DEFINE v_s_aivs_viv97_cedido                  DECIMAL(22,2)   ;
DEFINE v_s_pesos_viv97_cedido                 DECIMAL(22,2)   ;
DEFINE v_s_f_originacion_fovissste            DATE            ;
DEFINE v_s_f_fin_credito                      DATE;
DEFINE v_s_f_consulta_credito                 DATE            ;
DEFINE v_s_f_vigencia                         DATE            ;
DEFINE v_s_f_ini_tramite                      DATE            ;
DEFINE v_s_tipo_portabilidad                  SMALLINT        ;
DEFINE v_s_n_caso                             VARCHAR(20)     ;
DEFINE v_s_estado                             SMALLINT        ;
DEFINE v_s_resultado_operacion                CHAR(2)         ;
DEFINE v_s_diagnostico_interno                CHAR(3)         ;
DEFINE v_s_folio_procesar                     CHAR(50)        ;
DEFINE v_id                                   decimal(9,0)    ;
DEFINE v_nss                                  char(11)        ;


DEFINE v_id_derechohabiente                   DECIMAL(9,0)    ;

drop table if exists prt_consulta_solicitud_cedente;
create temp table prt_consulta_solicitud_cedente
  (
    id_prt_solicitud_cedente decimal(9,0) ,
    id_derechohabiente decimal(9,0),
    nss char(11),
    curp char(18),
    paterno char(40),
    materno char(40),
    nombre char(40),
    id_credito_fovissste decimal(10,0),
    correo_e char(100),
    telefono char(100),
    saldo_insoluto_credito_fovissste decimal(22,2),
    aivs_saldo_viv97_infonavit decimal(22,2),
    pesos_saldo_viv97_infonavit decimal(22,2),
    aivs_saldo_viv97_afore decimal(22,2),
    pesos_saldo_viv97_afore decimal(22,2),
    aivs_viv97_cedido decimal(22,2),
    pesos_viv97_cedido decimal(22,2),
    f_originacion_fovissste date,
    f_fin_credito date,
    f_consulta_credito date,
    f_vigencia date,
    f_ini_tramite date,
    tipo_portabilidad smallint ,
    n_caso varchar(20),
    estado smallint,
    resultado_operacion char(2),
    diagnostico_interno char(5) ,
    folio_procesar char(50)
  );

       INSERT INTO prt_consulta_solicitud_cedente
       SELECT a.id_prt_solicitud_cedente       ,
              b.id_derechohabiente                   ,
              a.nss                                  ,
              a.curp                                 ,
              a.paterno                              ,
              a.materno                              ,
              a.nombre                               ,
              a.id_credito_fovissste                 ,
              a.correo_e                             ,
              a.telefono                             ,
              a.saldo_insoluto_credito_fovissste     ,
              a.aivs_saldo_viv97_infonavit           ,
              a.pesos_saldo_viv97_infonavit          ,
              a.aivs_saldo_viv97_afore               ,
              a.pesos_saldo_viv97_afore              ,
              a.aivs_viv97_cedido                    ,
              a.pesos_viv97_cedido                   ,
              a.f_originacion_fovissste              ,
              a.f_fin_credito                        ,
              a.f_consulta_credito                   ,
              a.f_vigencia                           ,
              a.f_ini_tramite                        ,
              a.tipo_portabilidad                    ,
              a.n_caso                               ,
              a.estado                               ,
              a.resultado_operacion                  ,
              a.diagnostico_interno                  ,
              a.folio_procesar
         FROM prt_solicitud_cedente              a   ,
              afi_derechohabiente                b
        WHERE a.nss                = b.nss
          AND a.estado             = 40             -- marcado procesar
          AND a.tipo_portabilidad  = 2              -- solo subsecuentes 
;
       INSERT INTO prt_consulta_solicitud_cedente
       SELECT a.id_prt_solicitud_cedente             ,
              b.id_derechohabiente                   ,
              a.nss                                  ,
              a.curp                                 ,
              a.paterno                              ,
              a.materno                              ,
              a.nombre                               ,
              a.id_credito_fovissste                 ,
              a.correo_e                             ,
              a.telefono                             ,
              a.saldo_insoluto_credito_fovissste     ,
              a.aivs_saldo_viv97_infonavit           ,
              a.pesos_saldo_viv97_infonavit          ,
              a.aivs_saldo_viv97_afore               ,
              a.pesos_saldo_viv97_afore              ,
              a.aivs_viv97_cedido                    ,
              a.pesos_viv97_cedido                   ,
              a.f_originacion_fovissste              ,
              a.f_fin_credito                        ,
              a.f_consulta_credito                   ,
              a.f_vigencia                           ,
              a.f_ini_tramite                        ,
              a.tipo_portabilidad                    ,
              a.n_caso                               ,
              a.estado                               ,
              a.resultado_operacion                  ,
              a.diagnostico_interno                  ,
              a.folio_procesar
         FROM prt_solicitud_cedente   a  ,
              afi_derechohabiente     b
        WHERE a.nss                =    b.nss
          AND a.estado             = 80              -- marcado procesar
          AND a.tipo_portabilidad  = 1               -- transferencia de saldo y ap vol
;
       FOREACH cur_subsecuentes  FOR
       SELECT min(id_prt_solicitud_cedente) ,
              nss 
       INTO   v_id ,
              v_nss
       FROM prt_consulta_solicitud_cedente 
       GROUP BY 2

       SELECT a.id_prt_solicitud_cedente             ,
              a.id_derechohabiente                   ,
              a.nss                                  ,
              a.curp                                 ,
              a.paterno                              ,
              a.materno                              ,
              a.nombre                               ,
              a.id_credito_fovissste                 ,
              a.correo_e                             ,
              a.telefono                             ,
              a.saldo_insoluto_credito_fovissste     ,
              a.aivs_saldo_viv97_infonavit           ,
              a.pesos_saldo_viv97_infonavit          ,
              a.aivs_saldo_viv97_afore               ,
              a.pesos_saldo_viv97_afore              ,
              a.aivs_viv97_cedido                    ,
              a.pesos_viv97_cedido                   ,
              a.f_originacion_fovissste              ,
              a.f_fin_credito                        ,
              a.f_consulta_credito                   ,
              a.f_vigencia                           ,
              a.f_ini_tramite                        ,
              a.tipo_portabilidad                    ,
              a.n_caso                               ,
              a.estado                               ,
              a.resultado_operacion                  ,
              a.diagnostico_interno                  ,
              a.folio_procesar
         INTO v_s_id_prt_solicitud_cedente           ,
              v_id_derechohabiente                   ,
              v_s_nss                                ,
              v_s_curp                               ,
              v_s_paterno                            ,
              v_s_materno                            ,
              v_s_nombre                             ,
              v_s_id_credito_fovissste               ,
              v_s_correo_e                           ,
              v_s_telefono                           ,
              v_s_saldo_insoluto_credito_fovissste   ,
              v_s_aivs_saldo_viv97_infonavit         ,
              v_s_pesos_saldo_viv97_infonavit        ,
              v_s_aivs_saldo_viv97_afore             ,
              v_s_pesos_saldo_viv97_afore            ,
              v_s_aivs_viv97_cedido                  ,
              v_s_pesos_viv97_cedido                 ,
              v_s_f_originacion_fovissste            ,
              v_s_f_fin_credito                      ,
              v_s_f_consulta_credito                 ,
              v_s_f_vigencia                         ,
              v_s_f_ini_tramite                      ,
              v_s_tipo_portabilidad                  ,
              v_s_n_caso                             ,
              v_s_estado                             ,
              v_s_resultado_operacion                ,
              v_s_diagnostico_interno                ,
              v_s_folio_procesar
              FROM prt_consulta_solicitud_cedente a
              WHERE a.id_prt_solicitud_cedente = v_id 
              AND   a.nss                      = v_nss;

RETURN v_s_id_prt_solicitud_cedente           ,
       v_id_derechohabiente                   ,
       v_s_nss                                ,
       v_s_curp                               ,
       v_s_paterno                            ,
       v_s_materno                            ,
       v_s_nombre                             ,
       v_s_id_credito_fovissste               ,
       v_s_correo_e                           ,
       v_s_telefono                           ,
       v_s_saldo_insoluto_credito_fovissste   ,
       v_s_aivs_saldo_viv97_infonavit         ,
       v_s_pesos_saldo_viv97_infonavit        ,
       v_s_aivs_saldo_viv97_afore             ,
       v_s_pesos_saldo_viv97_afore            ,
       v_s_aivs_viv97_cedido                  ,
       v_s_pesos_viv97_cedido                 ,
       v_s_f_originacion_fovissste            ,
       v_s_f_fin_credito                      ,
       v_s_f_consulta_credito                 ,
       v_s_f_vigencia                         ,
       v_s_f_ini_tramite                      ,
       v_s_tipo_portabilidad                  ,
       v_s_n_caso                             ,
       v_s_estado                             ,
       v_s_resultado_operacion                ,
       v_s_diagnostico_interno                ,
       v_s_folio_procesar                     WITH RESUME;

END FOREACH;

END PROCEDURE;


