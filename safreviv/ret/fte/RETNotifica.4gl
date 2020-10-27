--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETNotifica                                             #
#OBJETIVO          => Programa de Apoyo para dar de alta solicitudes          #
#                     de ventanilla Afore                                     #
###############################################################################

 
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS "RETG04.4gl"
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
   DISPLAY CURRENT YEAR TO MINUTE, " Inicia proceso de carga de información de Solicitudes de Tuberia"
   DISPLAY "**"
   DISPLAY "**"
   CALL fn_crea_temporal()
   DISPLAY CURRENT YEAR TO MINUTE, " Tablas temporales creadas "
   CALL fn_carga_archivo()
   SELECT COUNT(*) 
   INTO   v_contador
   FROM   tmp_actualiza_datos_v_a
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
            folio                CHAR(14),
            nss                  CHAR(11),
            beneficiario         CHAR(1),
            claveafore           CHAR(3),
            nombre_s             CHAR(40),
            paterno_s            CHAR(40),
            materno_s            CHAR(40),
            rfc_s                CHAR(13),
            curp_s               CHAR(18),
            tipodoc              CHAR(2),
            folioife             CHAR(13),
            claveife             CHAR(18),
            street               CHAR(60),
            numext               CHAR(10),
            numint               CHAR(10),
            mcstreet             CHAR(25),
            transpzone           CHAR(10),
            postcodel            CHAR(10),
            tellada              CHAR(2),
            telnumber            CHAR(8),
            telcelular           CHAR(10),
            poboxlobby           CHAR(40),
            fresolucion          CHAR(8),
            horacontact          CHAR(2),
            cuentaclabe          CHAR(18),
            cuentabancaria       CHAR(20),
            datosbancarios       CHAR(40),
            importedevolver      CHAR(20),
            fechacreacion        CHAR(8),
            lugaremision         CHAR(50),
            desafore             CHAR(40),
            e_sha1               CHAR(50)
   END RECORD 
DEFINE v_rec_actualia_datos RECORD LIKE ret_ws_actualiza_datos_v_a.*
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


DEFINE v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud

    LET v_marca_entra    = 815
    LET v_proceso_cod    = g_proceso_cod_ret_ley73_ws
    LET v_folio          = "0"
    LET v_estado_marca   = "0"
    LET v_codigo_rechazo = "0"
    LET v_marca_causa    = "0"
    LET v_fecha_causa    = NULL
    LET v_usuario        = "safreviv"

   DECLARE cur_actualiza_datos CURSOR FOR
   SELECT * 
   FROM   tmp_actualiza_datos_v_a

   -- se leen las solicitudes de estos casos
   FOREACH cur_actualiza_datos INTO v_tmp_actualiza_datos.*
      --Consulta si existe  otra solicitud para el derechohabiente
      LET v_contador = v_contador + 1
      IF v_contador MOD 100 = 0 THEN 
         DISPLAY "Registros procesados : ", v_contador
      END IF 
 
      SELECT id_derechohabiente 
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_tmp_actualiza_datos.nss

      IF v_id_derechohabiente IS NULL THEN
         DISPLAY CURRENT YEAR TO MINUTE, "No existe NSS en la base de datos del Infonavit, NSS :",v_tmp_actualiza_datos.nss
         CONTINUE FOREACH
      END IF 
      CALL fn_verifica_solicitud_generico(v_id_derechohabiente,3,1)
      RETURNING v_existe_solicitud, v_n_referencia

      IF NOT v_existe_solicitud THEN
         --Se obtiene el número de solicitud
         SELECT seq_ret_solicitud.NEXTVAL
         INTO   v_id_solicitud
         FROM   systables 
         WHERE  tabid = 1

         SELECT seq_ret_ws_generico.nextVal
         INTO   v_id_peticion
         FROM   systables
         WHERE  tabid = 1


         --se obtiene la descripción del afore
         SELECT afore_desc
         INTO v_tmp_actualiza_datos.desafore
         FROM cat_afore
         WHERE afore_cod = v_tmp_actualiza_datos.claveafore
         
         INSERT INTO ret_ws_actualiza_datos_v_a VALUES (v_id_peticion,v_id_solicitud, v_tmp_actualiza_datos.*);

         -- Arma el query INSERCIÓN
         LET v_r_ret_solicitud_generico.id_solicitud           = v_id_solicitud
         LET v_r_ret_solicitud_generico.id_derechohabiente     = v_id_derechohabiente
         LET v_r_ret_solicitud_generico.nss                    = v_tmp_actualiza_datos.nss
         LET v_r_ret_solicitud_generico.rfc                    = v_tmp_actualiza_datos.rfc_s
         LET v_r_ret_solicitud_generico.modalidad_retiro       = 3
         LET v_r_ret_solicitud_generico.folio                  = 0
         LET v_r_ret_solicitud_generico.caso_adai              = NULL
         LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
         LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
         LET v_r_ret_solicitud_generico.folio_restitucion      = 0
         LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
         LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
         LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
         LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
         LET v_r_ret_solicitud_generico.estado_solicitud       = 10
         LET v_r_ret_solicitud_generico.cod_rechazo            = 0

         INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )

         -- Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            DISPLAY CURRENT YEAR TO MINUTE, " No se pudo insertar el registro en ret_solicitud_generico, NSS:", v_tmp_actualiza_datos.nss  
         ELSE
            DISPLAY CURRENT YEAR TO MINUTE, " Se ha insertado con éxito el registro en retiro genérico, NSS:", v_tmp_actualiza_datos.nss  
            -- Inserta en ret_ley73_generico
            CALL fn_calcula_saldo_ley73(v_tmp_actualiza_datos.nss, 8, TODAY) RETURNING v_resultado, v_aivs92, v_pesos92
            -- se obtiene el saldo de viv97
            CALL fn_calcula_saldo_ley73(v_tmp_actualiza_datos.nss, 4, TODAY) RETURNING v_resultado, v_aivs97, v_pesos97
            -- se obtiene el saldo de aportaciones voluntarias
            CALL fn_calcula_saldo_ley73(v_tmp_actualiza_datos.nss, 47, TODAY) RETURNING v_resultado, v_tesofe, v_p_tesofe

            SELECT precio_fondo
            INTO   v_precio_fondo
            FROM   glo_valor_fondo
            WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                  FROM   (SELECT LIMIT 1 1 
                                          FROM   systables))
            AND    fondo = 11                 
            LET v_pesos92 = v_aivs92 * v_precio_fondo
            LET v_pesos97 = v_aivs97 * v_precio_fondo
            LET v_p_tesofe  = v_tesofe
                
            INSERT INTO ret_ley73_generico (id_solicitud,id_derechohabiente, folio, gpo_ley73,subgrupo,
                                          f_solicitud, f_valuacion,aivs_viv92,aivs_viv97,importe_viv92,
                                          importe_viv97, importe_viv97_anexo1,f_captura,h_captura,
                                          usuario, estado_solicitud,cod_rechazo)
                 VALUES (v_id_solicitud, v_id_derechohabiente, 0, 1, 114,v_r_ret_solicitud_generico.f_solicitud,
                         v_r_ret_solicitud_generico.f_solicitud,v_aivs92,v_aivs97,v_pesos92,v_pesos97,v_tesofe,
                         v_r_ret_solicitud_generico.f_solicitud,v_r_ret_solicitud_generico.h_solicitud,'vafore',
                         10,0);
            -- Se inserta en ret_sol_medio_entrega para saber que se registro por Ventanilla Afore
            INSERT INTO ret_sol_medio_entrega (id_solicitud, grupo, medio_entrega,f_registro)
            VALUES (v_id_solicitud, 1, 5, CURRENT YEAR TO MINUTE);               
            -- Se busca la entidad federativa a partir del código postal
            LET v_entidad_federativa = v_tmp_actualiza_datos.transpzone[3,4]
            CALL fn_registra_beneficiario_retiro_generico(
               v_id_solicitud,
               1, -- Titular
               1, -- SPEI
               1, -- FALTA PARENTESCO
               v_tmp_actualiza_datos.paterno_s,
               v_tmp_actualiza_datos.materno_s,
               v_tmp_actualiza_datos.nombre_s,
               v_tmp_actualiza_datos.telnumber,
               v_tmp_actualiza_datos.poboxlobby,
               100,
               v_aivs92 + v_aivs97,
               v_pesos92 + v_pesos97 + v_tesofe,
               v_tmp_actualiza_datos.cuentaclabe,
               "",
               v_entidad_federativa)            
         END IF

         ---Se ejecuta la función de marcaje para la nueva solicitud
         CALL fn_ret_generico_marca_cuenta( v_id_derechohabiente ,
                                             v_marca_entra       ,
                                             v_id_solicitud      ,
                                             v_folio             ,
                                             v_estado_marca      ,
                                             v_codigo_rechazo    ,
                                             v_marca_causa       ,
                                             v_fecha_causa       ,
                                             v_usuario           ,
                                             v_proceso_cod )
         RETURNING v_respuesta
      ELSE
         DISPLAY CURRENT YEAR TO MINUTE, " Ya existe una solicitud previamente cargada, NSS:", v_tmp_actualiza_datos.nss 
      END IF 
   END FOREACH 

END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_generico
Fecha creacion: Septiembre 27, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Verifica que no exista una solicitud en la tabla de retiro genérico

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     26 Nov 2013             - La discriminacion del registro se hace
                                        por grupo de ventanilla infonavit
======================================================================
}
FUNCTION fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad_retiro,p_accion)
DEFINE v_sql STRING,
       p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_modalidad_retiro   SMALLINT,
       p_accion             SMALLINT,
       v_ban_existe         SMALLINT,
       v_num_solicitud      DECIMAL(9,0)
       
       
    -- Inicializa valores
    LET v_ban_existe= FALSE    

    -- Marcar 		  
    LET v_sql = "\n SELECT id_solicitud         ",
               "\n FROM ret_solicitud_generico ",
               "\n WHERE id_derechohabiente =  ", p_id_derechohabiente,
               "\n AND modalidad_retiro     =  ", p_modalidad_retiro,
               --"\n AND grupo_ventanilla     =  ", gi_ventanilla_afore,
               "\n AND estado_solicitud IN (   ",
               "\n 8, 10, 15, 50, 60, 70, 700, 71,  ", -- precaptura, captura, aprobacion, preliq., liquid., enviada fico, conf. pago
               "\n 90, 91, 209, 210, 211, 212, ", -- rch fico, rechazo banco, cancelacion CxP, 
               "\n 213, 214 )                  " -- restitucion

    PREPARE stm_existe_solicitud FROM v_sql
    DECLARE cur_existe_solicitud CURSOR FOR stm_existe_solicitud

    -- Itera resultados
    FOREACH cur_existe_solicitud INTO v_num_solicitud
        --Asigna valor
        IF ( v_num_solicitud IS NOT NULL ) THEN
            LET v_ban_existe = TRUE 
        END IF
    END FOREACH

    --DISPLAY "Solicitud encontrada: "
    --DISPLAY "Bandera: ", v_ban_existe
    --DISPLAY "Solicitud: ", v_num_solicitud

    -- se devuelve el resultado de la creacion
    RETURN v_ban_existe, v_num_solicitud
END FUNCTION                 


FUNCTION fn_crea_temporal()

DROP TABLE IF EXISTS tmp_actualiza_datos_v_a;
CREATE TEMP TABLE tmp_actualiza_datos_v_a (
            folio                CHAR(14),
            nss                  CHAR(11),
            beneficiario         CHAR(1),
            claveafore           CHAR(3),
            nombre_s             CHAR(40),
            paterno_s            CHAR(40),
            materno_s            CHAR(40),
            rfc_s                CHAR(13),
            curp_s               CHAR(18),
            tipodoc              CHAR(2),
            folioife             CHAR(13),
            claveife             CHAR(18),
            street               CHAR(60),
            numext               CHAR(10),
            numint               CHAR(10),
            mcstreet             CHAR(25),
            transpzone           CHAR(10),
            postcodel            CHAR(10),
            tellada              CHAR(2),
            telnumber            CHAR(8),
            telcelular           CHAR(10),
            poboxlobby           CHAR(40),
            fresolucion          CHAR(8),
            horacontact          CHAR(2),
            cuentaclabe          CHAR(18),
            cuentabancaria       CHAR(20),
            datosbancarios       CHAR(40),
            importedevolver      CHAR(20),
            fechacreacion        CHAR(8),
            lugaremision         CHAR(50),
            desafore             CHAR(40),
            e_sha1               CHAR(50));

END FUNCTION
FUNCTION fn_carga_archivo()
DEFINE v_sql      STRING 
DEFINE v_archivo  CHAR(55) 

   LET v_archivo = "solicitudes_en_tuberia.unl"

   --      PREPARE sid_inserta_archivo FROM v_sql
   --      EXECUTE sid_inserta_archivo
   
   LOAD FROM v_archivo
   INSERT INTO tmp_actualiza_datos_v_a
   

   
END FUNCTION


