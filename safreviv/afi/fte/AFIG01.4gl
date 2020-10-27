###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Módulo            => AFILIACIÓN                                              #
#Programa          => AFIG01                                                  #
#Objetivo          => Declaración de variables y constantes globales          #
###############################################################################

GLOBALS

   DEFINE g_tipo_criterio           SMALLINT
   DEFINE i                         SMALLINT
   DEFINE ii                        SMALLINT
   DEFINE iii                       SMALLINT

   DEFINE j                         SMALLINT
   DEFINE jj                        SMALLINT
   DEFINE jjj                       SMALLINT

   DEFINE cont                      INTEGER

   DEFINE g_id_derechohabiente      DECIMAL(9,0)

   DEFINE g_criterio                CHAR(50)
   DEFINE g_valor                   VARCHAR(50)

   DEFINE g_consulta                STRING
   DEFINE g_domiclio                STRING
   DEFINE g_telefono                STRING
   DEFINE g_correo                  STRING
   DEFINE g_credito                 STRING
   DEFINE v_s_qryTxt                STRING

   -- registro de derechohabientes
   DEFINE reg_derechohabiente RECORD
      nss                           CHAR(11),
      rfc                           CHAR(13),
      curp                          CHAR(18),
      f_nacimiento                  DATE,
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombre_af                     CHAR(40),
      nombre_imss                   CHAR(50),
      desc_tipo_trab                CHAR(30),
      desc_origen                   CHAR(20),
      desc_orig_credito             CHAR(30),
      num_credito                   DECIMAL(10,0),
      edo_credito                   CHAR(20),
      f_otorga                      DATE,
      f_liquida                     DATE,
      desc_tpo_credito              CHAR(30),
      f_apertura_inf                DATE,
      f_apertura                    DATE,
      ind_estado_cuenta             SMALLINT,
      f_estado_cuenta               DATE,
      desc_riss_imss                CHAR(40),
      f_alta_imss                   DATE,
      desc_riss_inf                 CHAR(40),
      f_alta_inf                    DATE,
      ind_not_sms                   SMALLINT,
      ind_not_correo                SMALLINT,
      bloquea_sms                   SMALLINT,
      bloquea_correo                SMALLINT,
      tpo_dscto                     CHAR(20), -- agregado
      sexo                          CHAR(1)
   END RECORD

   DEFINE arr_busqueda DYNAMIC ARRAY OF RECORD
       nss                          CHAR(11),
       rfc                          CHAR(13),
       curp                         CHAR(18)
   END RECORD

   DEFINE lc_qry                    STRING
   DEFINE lc_condicion              STRING

    DEFINE pos                      SMALLINT

    DEFINE g_enter                  CHAR(1)
    DEFINE g_usuario                CHAR(20)

    DEFINE g_hoy                    DATE -- fecha del dÍa

    DEFINE v_resultado              SMALLINT
    DEFINE v_tpo_originacion        SMALLINT
    DEFINE v_tpo_credito            SMALLINT
    DEFINE v_f_otorga               DATE
    DEFINE v_f_liquida              DATE

    DEFINE g_reg_modulo RECORD
        ruta_exp                    CHAR(40),
        ruta_rescate                CHAR(40),
        ruta_listados               CHAR(40)
    END RECORD

    DEFINE g_mensaje                STRING
    DEFINE g_titulo                 STRING
    DEFINE g_imagen                 STRING
    DEFINE sel_where                STRING
    DEFINE cla_where                STRING
    DEFINE comma                    STRING

    DEFINE w_criterio               ui.window
    DEFINE f_criterio               ui.form

    DEFINE v_ind_nss                BOOLEAN
    DEFINE v_ind_rfc                BOOLEAN
    DEFINE v_ind_curp               BOOLEAN
    DEFINE v_ind_folio              BOOLEAN

    DEFINE domicilio RECORD
      calle                         CHAR(60),
      num_ext                       CHAR(25),
      num_int                       CHAR(25),
      colonia_desc                  CHAR(50),
      cp                            CHAR(5),
      delegacion_desc               CHAR(50),
      ciudad_desc                   CHAR(50),
      estado_desc                   CHAR(50)
   END RECORD

   DEFINE domicilio_1 DYNAMIC ARRAY OF RECORD
      dom_cod                       SMALLINT,
      dom_desc                      CHAR(15),
      envio_desc                    CHAR(15),
      f_proceso_dom                 DATE
   END RECORD

   DEFINE domicilio_2 DYNAMIC ARRAY OF RECORD
      id_domicilio                  INTEGER,
      ind_envio                     CHAR(1)
   END RECORD

   -- arreglo de números telefónicos de un derechohabiente
   DEFINE arr_telefono DYNAMIC ARRAY OF RECORD
      tel_cod                       SMALLINT, -- tipo de teléfono: particular, oficina, etc
      tel_desc                      CHAR(15), -- PARTICULAR, OFICINA, CELULAR, ETC
      cve_lada                      CHAR(3) , -- Larga distancia
      telefono                      CHAR(40), -- número telefónico
      extension                     CHAR(5) , -- extensión telefónica
      pais_cod                      CHAR(3) , -- código de paÍs. se asume siempre 52 (México)
      f_proceso_tel                 DATE
   END RECORD

   DEFINE correo_1 DYNAMIC ARRAY OF RECORD
      correo_elect                  CHAR(50),
      f_proceso_ce                  DATE
   END RECORD

   DEFINE arr_relacion_derech DYNAMIC ARRAY OF RECORD
       v_id_derechohabiente         DECIMAL(9,0) , -- id_derechohabiente
       v_nrp                        CHAR(11)     , -- NRP del derechohabiente
       v_f_alta_nrp                 DATE         , -- fecha de alta
       v_ind_relacion               SMALLINT     , -- indicador de relación
       v_folio_lote                 DECIMAL(9,0) , -- folio del lote en que se generó el registro
       v_f_actualiza                DATE         , -- fecha en que se registró
       v_usuario                    CHAR(20)       -- usuario que ejecutó el proceso
   END RECORD

   DEFINE arr_riss DYNAMIC ARRAY OF RECORD
       v_nrp_riss                   CHAR(11)     , -- NRP del derechohabiente
       v_desc_riss                  CHAR(40)     , -- descripción indicador riss
       v_f_mov_riss                 DATE         , -- fecha_movimiento
       v_f_alta_riss                DATE         , -- fecha de alta
       v_desc_riss_rl               CHAR(40)     ,
       id_riss                      SMALLINT
   END RECORD

   DEFINE credito_1 DYNAMIC ARRAY OF RECORD
      desc_originacion              CHAR(40),
      desc_credito                  CHAR(40),
      v_tpo_dscto                   CHAR (40),
      v_num_credito                 DECIMAL(10,0),
      v_edo_cred                    CHAR(40),
      v_infonavit                   CHAR(30),
      estado                        SMALLINT,
      v_procesar                    CHAR(30),
      v_f_otorga                    DATE,
      v_f_liquida                   DATE,
      v_f_homologa                  DATE
   END RECORD

   DEFINE rec_crm_datos RECORD
        crm_paterno                 STRING,
        crm_nombre                  STRING,
        crm_curp                    STRING,
        crm_calle                   STRING,
        crm_colonia                 STRING,
        crm_municipio               STRING,
        crm_celular                 STRING,
        crm_correo_elect            STRING,
        crm_materno                 STRING,
        crm_f_nacimiento            STRING,
        crm_rfc                     STRING,
        crm_num_exterior            STRING,
        crm_cod_postal              STRING,
        crm_estado                  STRING,
        crm_telefono                STRING,
        crm_num_interior            STRING,
        crm_not_sms                 SMALLINT,
        crm_not_correo_e            STRING,
        crm_bloq_sms                SMALLINT,
        crm_bloq_correo_e           SMALLINT
   END RECORD

   DEFINE unifica_1 DYNAMIC ARRAY OF RECORD
      nss_unificador                CHAR(11),
      nss_unificado                 CHAR(11)
   END RECORD

   DEFINE total_dom                 SMALLINT
   DEFINE total_tel                 SMALLINT
   DEFINE total_correo              SMALLINT

   DEFINE cur_row                   SMALLINT
   DEFINE scr_row                   SMALLINT

   -- constantes para los procesos afiliatorios
   CONSTANT  g_proceso_cod_afi_movimientos_imss      SMALLINT = 1801 -- movimientos afiliatorios IMSS
   CONSTANT  g_proceso_cod_afi_movimientos_sinf      SMALLINT = 1802 -- movimientos afiliatorios Solo Infonavit
   CONSTANT  g_proceso_cod_afi_movimientos_opt75     SMALLINT = 1803 -- movimientos afiliatorios Opt 75

   -- procesos recurrentes de afiliacion
   CONSTANT  g_proceso_cod_afi_domicilio             SMALLINT = 1804 -- recurrente de domicilio
   CONSTANT  g_proceso_cod_afi_telefono              SMALLINT = 1805 -- recurrente de telefonos
   CONSTANT  g_proceso_cod_afi_correoelectronico     SMALLINT = 1806 -- recurrente de correo electronico

   -- procesos de actualizacion de datos
   CONSTANT  g_proceso_cod_afi_actualiza_bdnsviv     SMALLINT = 1807 -- Actualizacion de datos de la bdnsviv
   CONSTANT  g_proceso_cod_afi_actualiza_RFC         SMALLINT = 1808 -- Actualizacion de RFC de Afiliacion

   -- aclaraciones fondo 72
   CONSTANT  g_proceso_cod_afi_aclara_fondo72        SMALLINT = 1810 -- Aclaracion de datos de fondo 72
 
   -- carga de NSS para TRM
   CONSTANT  g_proceso_cod_afi_carga_nss_trm         SMALLINT = 1812-- Carga de nuevos NSS para/por TRM

   -- etapas de los procesos IMSS
   CONSTANT  g_opera_cod_afi_movimientos_carga_imss         SMALLINT = 1, -- carga de archivo afi IMSS
             g_opera_cod_afi_movimientos_integracion_imss   SMALLINT = 2 -- integracion afi IMSS

   -- etapas de los procesos SOLO INFONAVIT
   CONSTANT  g_opera_cod_afi_movimientos_carga_sinf         SMALLINT = 1, -- carga de archivo afi Solo infonavit
             g_opera_cod_afi_movimientos_integracion_sinf   SMALLINT = 2 -- integracion afi Solo Infonavit

   -- etapas de los procesos Opt 75
   CONSTANT  g_opera_cod_afi_movimientos_carga_opt75        SMALLINT = 1, -- carga de archivo afi Opt 75
             g_opera_cod_afi_movimientos_integracion_opt75  SMALLINT = 2 -- integracion afi Opt 75

   -- etapas de los procesos recurrente de domicilio
   CONSTANT  g_opera_cod_afi_carga_domicilio                SMALLINT = 1, -- carga de archivo afi domicilio
             g_opera_cod_afi_integracion_domicilio          SMALLINT = 2 -- integracion afi domicilio

   -- etapas de los procesos recurrente de telefono
   CONSTANT  g_opera_cod_afi_carga_telefono                 SMALLINT = 1, -- carga de archivo afi telefono
             g_opera_cod_afi_integracion_telefono           SMALLINT = 2 -- integracion afi telefono

   -- etapas de los procesos recurrente de correo electronico
   CONSTANT  g_opera_cod_afi_carga_correoelectronico        SMALLINT = 1, -- carga de archivo afi correo electronico
             g_opera_cod_afi_integracion_correoelectronico  SMALLINT = 2 -- integracion afi correo electronico

   -- etapas del proceso de actualizacion de datos de la bdnsviv
   CONSTANT  g_opera_cod_afi_actualiza_bdnsviv              SMALLINT = 1 -- lectura y actualizacion de datos de la bdnsviv

   -- etapas del proceso de AFI actualizacion de RFC
   CONSTANT  g_opera_cod_afi_act_rfc_carga                  SMALLINT = 1, -- carga de archivo afi actualiza RFC
             g_opera_cod_afi_act_rfc_integracion            SMALLINT = 2 -- integracion afi afi actualiza RFC

   -- aclaraciones fondo72 es en linea y solo se liquida             
   CONSTANT  g_opera_cod_afi_acl_fondo72_liquidacion        SMALLINT = 1 -- liquidacion de movimientos de aclaracion de fondo 72

   -- etapas del proceso de carga de NSS de TRM
   CONSTANT  g_opera_cod_afi_carga_nss_trm                  SMALLINT = 1, -- carga de archivo nuevos NSS de TRM
             g_opera_cod_afi_integracion_nss_trm            SMALLINT = 2 -- integracion de archivo nuevos NSS de TRM

   -- parámetro para ejecución de función de créditos de vivienda
   CONSTANT v_valida                                        SMALLINT = 1 -- verifica estado del último crédito otorgado

   CONSTANT PERFIL_CRM                                      CHAR(5) = 'COCRM'
   CONSTANT PERFIL_CONSUL                                   CHAR(6) = "CONSUL"

END GLOBALS

#Objetivo: 
#
FUNCTION fn_inicializa() 

   LET g_consulta = " SELECT FIRST 1 afi.id_derechohabiente,",
                           " afi.nss,",
                           " afi.rfc,",
                           " afi.curp,",
                           " afi.f_nacimiento,",
                           " afi.ap_paterno_af,",
                           " afi.ap_materno_af,",
                           " afi.nombre_af,",
                           " afi.nombre_imss,",
                           " DECODE(afi.tipo_trabajador,'S','SOLO INFONAVIT','I','IMSS','E','EDO-MCPIO SOLO INFONAVIT'),",
                           " cds.desc_origen_afi,",
                           " afi.f_apertura_inf,",
                           " afi.f_apertura,",
                           " afi.ind_estado_cuenta,",
                           " afi.f_estado_cuenta,",
                           " afi.sexo",
                      " FROM afi_derechohabiente afi,",
                      "      cat_origen_afi cds ",
                     " WHERE afi.nss = '", reg_derechohabiente.nss CLIPPED,"'",
                       " AND afi.origen_afiliacion = cds.origen_afiliacion"

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_edo_cred_viv(?,?)"

END FUNCTION

