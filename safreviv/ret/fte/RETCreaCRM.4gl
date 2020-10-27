--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETCreaCRM                                              #
#OBJETIVO          => Programa de Apoyo para dar de alta los casos en crm     #
###############################################################################

 
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "ret_datos_contacto_crm.inc"

GLOBALS "RETG01.4gl"
GLOBALS
-- registro de entrada para la consulta por nss
DEFINE ws_ret_cons_nss_in RECORD
         nss                 CHAR(11) --numero de seguridad social
      END RECORD,
       -- registro de respuesta para la consulta por nss
       ws_ret_cons_nss_out  RECORD
         codRet              CHAR(2)  , -- Código de retorno
         mensaje             STRING-- CHAR(30) -- Descripción de código de retorno
       END RECORD,
      -- registro de entrada para la actualización de datos
       ws_ret_actualiza_in RECORD
         nss                 CHAR(11) , -- Numero de Seguridad Social
         beneficiario        STRING   , -- Indicador de beneficiario
         claveAfore          CHAR(3)  , -- Clave la Afore a la que pertenece el pensionado
         nombre_s            STRING   , -- Nombre del beneficiario
         paterno_s           STRING   , -- Apellido Paterno del beneficiario
         materno_s           STRING   , -- Apellido Materno del beneficiario
         rfc_s               CHAR(13) , -- RFC del beneficiario
         curp_s              CHAR(18) , -- CURP del beneficiario
         tipo_doc            STRING   , -- El valor de este campo es determinado por portal
         folio_ife           STRING   , -- Cadena que continen el número de folio de ife
         clave_ife           STRING   , -- Clave de la credencial del IFE 
         street              STRING   , -- Calle del domicilio del beneficiario
         numExt              STRING   , -- Numero exterior del domicilio
         numInt              STRING   , -- Numero interior del domicilio
         mcStreet            STRING   , -- Colonia
         transpZone          STRING   , -- Municipio
         postCodel           STRING   , -- Codigo
         telLada             STRING   , -- Lada del telefono
         telNumber           STRING   , -- Telefono del beneficiario
         telCelular          STRING   , -- Celular del beneficiario
         poBoxLobby          STRING   , -- correo
         f_resolucion        STRING   , -- Fecha de resolución de pension
         horaContact         STRING   , -- Horario de contacto
         cuenta_clabe        STRING   , -- Cuenta CLABE del banco que se está almacenando por parte de la afore
         cuenta_bancaria     STRING   , -- Cuenta Bancaria que se está almacenando por parte de la Afore
         datos_bancarios     STRING   , -- Descripción de la cuenta bancaria que se está almacenando
         importe_devolver    STRING   , -- Importe que se le depositará al afiliado
         fecha_creacion      CHAR(8)  , -- fecha de creación de la solicitud
         lugar_emision       STRING   -- lugar de emisión del folio
      END RECORD,
       -- registro de respuesta para la actualización de datos
       ws_ret_actualiza_out  RECORD
         codRet              STRING   , -- Codigo de retorno
         mensaje             STRING   , -- Descripcion de código de retorno
         folio               STRING   , -- Folio TRM de la solicitud
         nss                 CHAR(11) , -- Numero de Seguridad Social
         rfc                 CHAR(13) , -- RFC del beneficiario
         curp                CHAR(18) , -- CURP del beneficiario
         tipo_doc            STRING   , -- El valor de este campo es determinado por portal
         folio_ife           STRING   , -- Cadena que continen el número de folio de ife
         clave_ife           STRING   , -- Clave de la credencial del IFE          
         nombre              STRING   , -- Nombre del Beneficiario
         aPaterno            STRING   , -- Apellido Paterno del beneficiario
         aMaterno            STRING   , -- Apellido Materno Beneficiario
         desAfore            STRING   , -- Nombre de la Afore
         e_sha1              STRING   -- Cadena generada para garantizar que la transacción haya sido segura
       END RECORD,

      -- registro de entrada para la reimpresión de datos
       ws_ret_re_imprime_in RECORD
         nss                 CHAR(11) , -- Numero de Seguridad Social
         folio               STRING     -- Folio de la solicitud
      END RECORD,
       -- registro de respuesta para la reimpresión de datos
       ws_ret_re_imprime_out  RECORD
         codRet              STRING   , -- Codigo de retorno
         mensaje             STRING   , -- Descripcion de código de retorno
         folio               STRING   , -- Folio TRM de la solicitud
         nss                 CHAR(11) , -- Numero de Seguridad Social
         rfc                 CHAR(13) , -- RFC del beneficiario
         curp                CHAR(18) , -- CURP del beneficiario
         folio_ife           STRING   , -- Cadena que continen el número de folio de ife
         clave_ife           STRING   , -- Clave de la credencial del IFE          
         nombre              STRING   , -- Nombre del Beneficiario
         aPaterno            STRING   , -- Apellido Paterno del beneficiario
         aMaterno            STRING   , -- Apellido Materno Beneficiario
         desAfore            STRING   , -- Nombre de la Afore
         tipo_doc            STRING   , -- El valor de este campo es determinado por portal
         e_sha1              STRING   , -- Cadena generada para garantizar que la transacción haya sido segura
         cuenta_clabe        STRING   , -- Cuenta CLABE del banco que se está almacenando por parte de la afore
         cuenta_bancaria     STRING   , -- Cuenta Bancaria que se está almacenando por parte de la Afore
         datos_bancarios     STRING   , -- Descripción de la cuenta bancaria que se está almacenando
         importe_devolver    STRING   , -- Importe que se le depositará al afiliado
         fecha_creacion      CHAR(8)  , -- fecha de creación de la solicitud
         lugar_emision       STRING   -- lugar de emisión del folio
       END RECORD
       
DEFINE g_indice_retiro      SMALLINT, -- indice del tipo de retiro consultado
       g_id_derechohabiente DECIMAL(9,0) ,
       g_id_fondo72         DECIMAL(9,0) ,
       g_causal_ref         SMALLINT     ,
       g_nss                CHAR(11)     ,
       g_rfc                CHAR(13)     , -- rfc del trabajador
       g_acc_acciones       DECIMAL(14,6),
       g_acc_pesos          DECIMAL(14,6),
       g_tanto_adicional    DECIMAL(14,6),
       g_id_solicitud       DECIMAL(9,0) ,
       g_refer              CHAR(18)     ,
       g_id_beneficiario    SMALLINT     , -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18)     , -- Nombre del beneficiario 
       g_ape_pat            CHAR(18)     , -- Apellido paterno 
       g_ape_mat            CHAR(18)     , -- Apellido materno           
       g_causal_adai        SMALLINT     , -- Clave de Adai 
       g_entidad            SMALLINT     , -- Entidad federativa
       g_id_datamart        DECIMAL(9,0) , -- Identificador datamart
       g_causal_retiro      SMALLINT     ,
       g_bnd_uso_seq        SMALLINT     ,
       g_sq_ret_solicitud   DECIMAL(9,0) -- id de solicitud nueva 

DEFINE g_r_tmp_id_fondo72   RECORD
        nss                  CHAR(11)     ,
        id_derechohabiente   DECIMAL(9,0) ,
        id_afi_fondo72       DECIMAL(9,0) ,
        importe              DECIMAL(12,2),
        rfc                  CHAR(13)     ,
        estatus              SMALLINT     ,
        rechazo_cod          SMALLINT
       END RECORD

DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws
       
-- =======================================================
-- constantes para la evaluacion del resultado de la ejecucion del webservice
CONSTANT  g_res_procesada                    SMALLINT = 0  ,
          g_res_sin_solicitud                SMALLINT = -1 ,
          g_res_desconectado_del_servidor    SMALLINT = -2 ,
          g_res_conexion_con_cliente_perdida SMALLINT = -3 ,
          g_res_servidor_interrumpido_ctrl_c SMALLINT = -4 ,
          g_res_error_interno                SMALLINT = -10,
          g_msg_procesada                    STRING = "Solicitud procesada"                  ,
          g_msg_sin_solicitud                STRING = "Sin solicitud"                        ,
          g_msg_desconectado_del_servidor    STRING = "Desconectado del servidor"            ,
          g_msg_conexion_con_cliente_perdida STRING = "Se perdió la conexión con el cliente" ,
          g_msg_servidor_interrumpido_ctrl_c STRING = "Se interrumpió el servidor con CTRL-C",
          g_msg_error_interno                STRING = "Ocurrió un error interno"
         
DEFINE serverURL STRING -- URL del servidor
DEFINE v_pantalla    SMALLINT

END GLOBALS

#
# MAIN
#
MAIN
DEFINE v_contador  INTEGER 

   DISPLAY "*************************************************************************************************"
   DISPLAY "**"
   DISPLAY "**"
   DISPLAY "**"
   DISPLAY CURRENT YEAR TO MINUTE, " Inicia proceso de Creacion Id en CRM "  
   DISPLAY "**"
   DISPLAY "**"
   CALL fn_crea_temporal()
   DISPLAY CURRENT YEAR TO MINUTE, " Tablas temporales creadas "
   CALL fn_carga_archivo()

   DATABASE safre_viv

   SELECT COUNT(*) 
   INTO   v_contador
   FROM   safre_tmp:tmp_solicitud_va_dev_auto
   
   IF v_contador > 0 THEN 
      DISPLAY CURRENT YEAR TO MINUTE, " Información cargada correctamente se procesarán ", v_contador, " solicitudes "
      CALL fn_procesa_informacion()
      DISPLAY CURRENT YEAR TO MINUTE, " Termina proceso "
   ELSE 
      DISPLAY CURRENT YEAR TO MINUTE, " No existe información por procesar "
   END IF 

END MAIN 

FUNCTION fn_procesa_informacion()       
DEFINE v_resultado           INTEGER -- recibe el resultado de la ejecucion del servicio 
DEFINE v_ruta_log            STRING
DEFINE v_cadena              STRING
DEFINE v_ruta_ejecutable     VARCHAR(40)
DEFINE v_id_derechohabiente  DECIMAL(10,0)
DEFINE v_id_solicitud        DECIMAL(10,0)
DEFINE v_id_peticion         DECIMAL(10,0)
DEFINE v_contador            INTEGER 

DEFINE v_tmp_actualiza_datos RECORD 
            nss                  CHAR(11),
            cuentaclabe          CHAR(18),
            estado_solicitud     SMALLINT,
            cod_rechazo          SMALLINT
   END RECORD 

DEFINE v_marca_entra    SMALLINT 
DEFINE v_proceso_cod    SMALLINT
DEFINE v_folio          DECIMAL(9,0)
DEFINE v_estado_marca   SMALLINT
DEFINE v_codigo_rechazo SMALLINT
DEFINE v_marca_causa    SMALLINT
DEFINE v_fecha_causa    DATE
DEFINE v_usuario        CHAR(20)
DEFINE v_aivs92         DECIMAL(22,2)
DEFINE v_aivs97         DECIMAL(22,2)
DEFINE v_pesos92        DECIMAL(22,2)
DEFINE v_pesos97        DECIMAL(22,2)
DEFINE v_tesofe         DECIMAL(22,2)
DEFINE v_p_tesofe       DECIMAL(22,2)
DEFINE v_precio_fondo   DECIMAL(10,5)
DEFINE v_entidad_federativa SMALLINT
DEFINE v_respuesta        SMALLINT
DEFINE v_existe_solicitud SMALLINT
DEFINE v_n_referencia     INTEGER
DEFINE v_respuesta_servicio INTEGER 


DEFINE v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud

    LET v_marca_entra    = 803
    LET v_proceso_cod    = g_proceso_cod_ret_ley73_ws
    LET v_folio          = "0"
    LET v_estado_marca   = "0"
    LET v_codigo_rechazo = "0"
    LET v_marca_causa    = "0"
    LET v_fecha_causa    = NULL
    LET v_usuario        = "saci_va_da"

   DECLARE cur_actualiza_datos CURSOR FOR
   SELECT * 
   FROM   safre_tmp:tmp_solicitud_va_dev_auto

   -- se leen las solicitudes de estos casos
   FOREACH cur_actualiza_datos INTO v_tmp_actualiza_datos.*
      LET v_contador = v_contador + 1
      LET v_id_solicitud = 0
      IF v_contador MOD 100 = 0 THEN 
         DISPLAY "Registros procesados : ", v_contador
      END IF 

      -- Invocacion al servicio de Creacion ID
      LET consultarDatosContacto.nss = v_tmp_actualiza_datos.nss
      LET consultarDatosContacto.idAplicacion = 1
      DISPLAY "Se procesa el NSS: ", v_tmp_actualiza_datos.nss, " Cuenta CLABE: ", v_tmp_actualiza_datos.cuentaclabe
      CALL consultarDatosContacto_g() RETURNING v_resultado
      IF v_resultado = 0 THEN 
         DISPLAY "El llamado al Servicio de Creacion ID :", v_resultado 
         LET v_respuesta_servicio = consultarDatosContactoResponse.return.estatus
         LET v_respuesta_servicio = consultarDatosContactoResponse.return.descError
         
      ELSE
         DISPLAY "No se puedo llamar al servicio de Creación del ID en CRM :", v_resultado
         DISPLAY "NSS :", v_tmp_actualiza_datos.nss
      END IF 
              
   END FOREACH 

END FUNCTION 


FUNCTION fn_crea_temporal()

DATABASE safre_tmp;

--DROP TABLE IF EXISTS tmp_solicitud_va_dev_auto;
--CREATE TABLE tmp_solicitud_va_dev_auto (
--            nss                  CHAR(11),
--            cuentaclabe          CHAR(18),
--            estado_solicitud     SMALLINT,
--            cod_rechazo          SMALLINT,
--            id_solicitud         DECIMAL(10,0));
   TRUNCATE TABLE  tmp_solicitud_va_dev_auto;
END FUNCTION

FUNCTION fn_carga_archivo()
DEFINE v_sql      STRING 
DEFINE v_archivo  CHAR(55) 

   LET v_archivo = "sol_vent_afore_dev_auto.unl"

   --      PREPARE sid_inserta_archivo FROM v_sql
   --      EXECUTE sid_inserta_archivo
   
   LOAD FROM v_archivo
   INSERT INTO tmp_solicitud_va_dev_auto
   

   
END FUNCTION


