--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => ENEAS ADAN ARMAS OSORIO E.F.P.                          #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW40                                                  #
#OBJETIVO          => WS CONSULTA DE ESTADO POR NSS                           #
#                     RETIRO GENERICO                                         #
#FECHA INICIO      => 02-DIC-2013                                             #
# Autor           Fecha      Modificación                                                    #
# Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
#                 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
###############################################################################

IMPORT FGL WSHelper
IMPORT com
  
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
DEFINE v_resultado       INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log        STRING,
       v_cadena          STRING,
       v_ruta_ejecutable VARCHAR(40)
      
    DEFER INTERRUPT

    -- se obtiene la ruta ejecutable
    SELECT ruta_bin
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW40."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_retiro_generico(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro 72-92 service") --, STYLE="naked")
            --display_status("Retiro Service Startup")
        ELSE
            IF num_args() <> 0 THEN
                CALL exitHelp()
                EXIT PROGRAM
            END IF
        END IF
    END IF
  
    -- se crea el servicio
    CALL ERRORLOG("invoca creacion de servicio Retiro")
    CALL fn_crea_servicio_retiro_generico(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor...")

    -- se inicia el motor de WS
    CALL com.WebServiceEngine.Start()
    CALL ERRORLOG("Servidor en escucha")

    -- si se tiene pantalla
    IF ( v_pantalla ) THEN
        MENU
            -- en espera del servidor
            ON IDLE 1
            -- se invoca la ejecucion del servicio
            LET v_resultado = com.WebServiceEngine.ProcessServices(-1) -- [sin timeout -1]

            -- se verifica el resultado
            CASE v_resultado
                WHEN g_res_procesada
                    DISPLAY g_msg_procesada TO msg

                WHEN g_res_sin_solicitud
                    DISPLAY g_msg_sin_solicitud TO msg

                WHEN g_res_desconectado_del_servidor
                    DISPLAY g_msg_desconectado_del_servidor TO msg
                    EXIT PROGRAM   # The Application server has closed the connection

                WHEN g_res_conexion_con_cliente_perdida
                    DISPLAY g_msg_conexion_con_cliente_perdida TO msg

                WHEN g_res_servidor_interrumpido_ctrl_c
                    DISPLAY g_msg_servidor_interrumpido_ctrl_c TO msg

                WHEN g_msg_error_interno
                    DISPLAY g_msg_error_interno TO msg

                OTHERWISE 
                    -- se recibio algun otro codigo de retorno
                    DISPLAY "Se recibió otro código de retorno" TO msg
            END CASE

            -- si se elige cerrar
            ON ACTION CLOSE
                EXIT PROGRAM
        END MENU

    ELSE  -- no se tiene pantalla
        WHILE ( TRUE )
            LET v_resultado = com.WebServiceEngine.ProcessServices(-1) -- [sin timeout -1]
            CALL ERRORLOG("Regresa de procesar el servicio: ")
            CALL ERRORLOG(v_resultado)

            -- se verifica el resultado
            CASE v_resultado
                WHEN g_res_procesada
                    DISPLAY g_msg_procesada
                    CALL ERRORLOG(g_msg_procesada)

                WHEN g_res_sin_solicitud
                    DISPLAY g_msg_sin_solicitud
                    CALL ERRORLOG(g_msg_procesada)

                WHEN g_res_desconectado_del_servidor
                    DISPLAY g_msg_desconectado_del_servidor
                    CALL ERRORLOG(g_msg_procesada)
                    EXIT PROGRAM   # The Application server has closed the connection

                WHEN g_res_conexion_con_cliente_perdida
                    CALL ERRORLOG(g_msg_procesada)
                    DISPLAY g_msg_conexion_con_cliente_perdida

                WHEN g_res_servidor_interrumpido_ctrl_c
                    CALL ERRORLOG(g_msg_procesada)
                    DISPLAY g_msg_servidor_interrumpido_ctrl_c

                WHEN g_msg_error_interno
                    CALL ERRORLOG(g_msg_procesada)
                    DISPLAY g_msg_error_interno

                OTHERWISE 
                    -- se recibio algun otro codigo de retorno
                    DISPLAY "Se recibio otro codigo de retorno"
                    CALL ERRORLOG(g_msg_procesada)
            END CASE

            IF ( INT_FLAG <> 0 ) THEN
                LET INT_FLAG = 0
                EXIT WHILE
            END IF

        END WHILE

        CALL ERRORLOG("El servidor se detuvo")
    END IF
END MAIN

{
======================================================================
Clave: 
Nombre: fn_crea_servicio_retiro_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_retiro_generico(p_generar_WSDL)
DEFINE v_webservice          com.WebService       # WebService
DEFINE op                    com.WebOperation     # Operation of a WebService
DEFINE op2                   com.WebOperation     # Operation of a WebService
DEFINE op3                   com.WebOperation     # Operation of a WebService
DEFINE v_service_NameSpace   STRING -- namespace del servicio
DEFINE p_generar_WSDL        SMALLINT -- booleana que indica si se solicito enviar el WSDL
DEFINE v_resultado           INTEGER
DEFINE v_urn                 STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("UrlAforesConfig", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        LET op = com.WebOperation.CreateDOCStyle("ConsultarRetiroNss","ConsultarRetiroNss",ws_ret_cons_nss_in,ws_ret_cons_nss_out)
        CALL v_webservice.publishOperation(op, "ConsultarRetiroNss")

        --declaración de la segunda función
        LET op2 = com.WebOperation.CreateDOCStyle("actualizarDatosGenerales","actualizarDatosGenerales",ws_ret_actualiza_in,ws_ret_actualiza_out)
        CALL v_webservice.publishOperation(op2, "actualizarDatosGenerales")

        --declaración de la tercera función
        LET op3 = com.WebOperation.CreateDOCStyle("reimpresionDatosGenerales","reimpresionDatosGenerales",ws_ret_re_imprime_in,ws_ret_re_imprime_out)
        CALL v_webservice.publishOperation(op3, "reimpresionDatosGenerales")

        -- si se hace generacion del WSDL
        IF ( p_generar_WSDL ) THEN
            -- Generar el WSDL
            LET v_resultado = v_webservice.saveWSDL(serverURL)

            -- si se genero el WSDL sin errores
            IF ( v_resultado = 0 ) THEN
                DISPLAY "WSDL creado exitosamente"
            ELSE
                DISPLAY "ERROR: No se pudo crear el WSDL"
            END IF
        ELSE
            -- =========================
            -- REgistro del servicio
            CALL com.WebServiceEngine.RegisterService(v_webservice)  
            --display_status("Retiro 72-92 Service registrado")
            CALL ERRORLOG("Se registro el servicio consulta de estado por nss para retiro")
        END IF

        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de estado por nss para retiro': " || STATUS)
            EXIT PROGRAM
    END TRY

END FUNCTION

FUNCTION exitHelp()
    DISPLAY "Usage: "
    DISPLAY "  ", arg_val(0)
    DISPLAY "    Start the server on port defined by FGLAPPSERVER"
    DISPLAY "  ", arg_val(0), " -W serverurl"
    DISPLAY "    Generate the WSDL file for the given url"
    DISPLAY "  ", arg_val(0), " -S port"
    DISPLAY "    Start service in graphical mode and on given port"
    EXIT PROGRAM
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_ret_consulta_nss
Fecha creacion: diciembre 02, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta general por nss
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de vivienda segun ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION ConsultarRetiroNss()
DEFINE v_nss                  CHAR(11),      -- NSS
       p_es_consulta          SMALLINT,      -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess          SMALLINT,      -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart          LIKE ret_datamart.id_datamart,
       v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
       v_f_inicio_pension     DATE,          -- fecha de inicio de pension en el SPESS
       v_validacion_nss       SMALLINT,      -- Resultado de la validacion del NSS 0 = validacion exitosa, 1 = nss invalido
       v_validacion_marca     SMALLINT,      -- Resultado de la validacion de marca 0 = sin marca, 1 = con marca que impide el tramite
       v_validacion_solicitud SMALLINT,      -- Resultado de la validacion de solicitud previa
       v_validacion_spess     SMALLINT,      -- Resultado de la validacion del SPESS
       v_cod_ret              CHAR(2),       -- Codigo de rechazo regresado por la funcion
       v_desc_cod_ret         CHAR(100),     -- Descripcion del codigo de rechazo regresado por la funcion
       
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       p_grupo_ley73          SMALLINT,
       v_tiene_credito        SMALLINT,      -- booleana que indica si se tiene un credito vigente
       v_tipo_credito         SMALLINT,       -- clave del tipo de credito
       v_resultado            SMALLINT,
       v_saldo                DECIMAL(22,2)

    LET v_nss = ws_ret_cons_nss_in.nss
    ----  Validar primero si el nss es correcto
    CALL fn_valida_estructura_nss(v_nss) RETURNING v_validacion_nss
    DISPLAY "Regresa de validar estructura del nss >",v_validacion_nss, "<"
    IF v_validacion_nss = 1 THEN
        CALL fn_respuesta_ws_cod(v_nss,g_url_disp_cod_ret_22,g_url_disp_desc_cod_ret_22)
        RETURN
    END IF 

    -- se obtiene el id_derechohabiente 
    SELECT   id_derechohabiente--,  id_afi_fondo72
    INTO   v_id_derechohabiente--,v_id_afi_fondo72
    FROM   afi_derechohabiente
    WHERE  nss = ws_ret_cons_nss_in.nss

    -- se valida que exista el id_derechohabiente
    DISPLAY "Regresa de buscar el nss, id_derechohabiente >",v_id_derechohabiente, "<"
    IF v_id_derechohabiente IS NULL THEN
        CALL fn_respuesta_ws_cod(v_nss, g_url_disp_cod_ret_23,g_url_disp_desc_cod_ret_23)
        RETURN 
    END IF 
    -- Valida que no tenga marca para proceder con la solicitud
    CALL fn_valida_marcas(v_id_derechohabiente) RETURNING v_validacion_marca, v_cod_ret, v_desc_cod_ret
    DISPLAY "Regresa de validar las marcas, v_validacion_marca >",v_validacion_marca, "< v_cod_ret >", v_cod_ret, "<" 
    IF v_validacion_marca = 1 THEN
        CALL fn_respuesta_ws_cod(v_nss, v_cod_ret,v_desc_cod_ret)
        RETURN 
    END IF 
    CALL fn_valida_saldo(v_nss) RETURNING v_resultado, v_saldo
    IF v_resultado = 0 AND v_saldo = 0 THEN --- Se rechaza por saldo cero 
        CALL fn_respuesta_ws_cod(v_nss, g_url_disp_cod_ret_74,g_url_disp_desc_cod_ret_74)
        RETURN 
    END IF 
--    CALL fn_ret_ley73_credito_vigente(v_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
--    IF ( v_tiene_credito ) THEN
--        -- se verifica si el credito es de tipo 43BIS
--        IF ( NOT  (fn_verifica_tipo_credito_43bis(v_tipo_credito)) ) THEN 
--            -- se rechaza por tener credigo vigente
--            CALL fn_respuesta_ws_cod(v_nss, g_url_disp_cod_ret_80,g_url_disp_desc_cod_ret_80)--no  tiene
--            RETURN 
--        END IF
--    END IF   

    -- Valida si hay una solicitud en tramite
    CALL fn_valida_solicitud_previa(v_id_derechohabiente) RETURNING v_validacion_solicitud, v_cod_ret, v_desc_cod_ret
    IF v_validacion_solicitud = 1 THEN
        CALL fn_respuesta_ws_cod(v_nss, v_cod_ret,v_desc_cod_ret)
        RETURN 
    END IF 

    CALL fn_valida_spess(v_nss) RETURNING v_validacion_spess, v_cod_ret, v_desc_cod_ret
    IF v_validacion_spess = 1 THEN
        CALL fn_respuesta_ws_cod(v_nss, v_cod_ret,v_desc_cod_ret)
        RETURN 
    END IF 

    -- se valida la solicitud para un grupo 1
    --CALL fn_retl73_valida_grupo1(v_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
    CALL fn_respuesta_ws_cod(v_nss, "99","PROCESO EXITOSO")--si tien
   
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo1
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de un credito por amortaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss              CHAR(11), -- NSS
       p_grupo_ley73      SMALLINT, -- grupo de retiro segun Ley73
       p_es_consulta      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess      SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart      LIKE ret_datamart.id_datamart,
       v_aivs_viv92       DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97       DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92      DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97      DECIMAL(22,2), -- saldo pesos de viv97
       v_resultado        SMALLINT, -- resultado de la consulta
       v_tiene_credito    SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tipo_credito     SMALLINT, -- clave del tipo de credito
       v_f_inicio_pension DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total      DECIMAL(24,6) -- saldo total (viv92 + viv97)
   
    -- se verifica si tuvo/tiene un retiro de devolucion
    IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
        -- se rechaza por insuficiencia de saldo
        CALL fn_respuesta_ws_cod(p_nss, "21","PARA EL NSS SOLICITADO NO HAY MONTO DE RETIRO")--no tiene
    ELSE
        -- se verifica si el derechohabiente tiene un credito vigente
        CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
        IF ( v_tiene_credito ) THEN
            -- se verifica si el credito es de tipo 43BIS
            IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
                -- el saldo de vivienda92 es mayor a cero
                CALL fn_respuesta_ws_cod(p_nss, "99","PROCESO EXITOSO")--si tien
            ELSE
                -- se rechaza por tener credigo vigente
                CALL fn_respuesta_ws_cod(p_nss, "80","NSS CON CREDITO VIGENTE")--no  tiene
            END IF
        ELSE
            -- el saldo es retirable
            CALL fn_respuesta_ws_cod(p_nss, "99","PROCESO EXITOSO")--si tiene
        END IF
    END IF
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_cod
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado de un estado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
--FUNCTION fn_respuesta_ws_aport_voluntarias(p_nss, p_estado_solicitud, p_cod_rechazo, p_fecha_valuacion, p_aivs, p_pesos)
FUNCTION fn_respuesta_ws_cod(p_nss, p_codRet,p_mensaje)
DEFINE   p_nss         CHAR(11),
         p_codRet      CHAR(2), -- Código de retorno
         p_mensaje     CHAR(100) -- Descripción de código de retorno

    LET ws_ret_cons_nss_out.codRet  = p_codRet
    LET ws_ret_cons_nss_out.mensaje = p_mensaje

    INSERT INTO  ret_ws_disponibilidad_v_a 
         VALUES (seq_ret_ws_disponibilidad_v_a.nextval, p_nss,
                 TODAY, CURRENT HOUR TO SECOND, p_codRet, p_mensaje);
   

END FUNCTION

{
======================================================================
Clave: 
Nombre: actualizarDatosGenerales
Fecha creacion: diciembre 03, 2013
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Actualización de datos generales

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION actualizarDatosGenerales()
--753 fn_aplica_marca_modalidad(
DEFINE v_marca_entra           SMALLINT,
       v_proceso_cod           SMALLINT,
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente,
       p_modalidad             SMALLINT,
       v_existe_solicitud      SMALLINT,
       v_n_referencia          INTEGER ,
       v_estatus_marca         SMALLINT,
       v_res_actualizacion     SMALLINT,
       v_respuesta_marcaje     SMALLINT,
       v_folio                 DECIMAL(9,0),
       v_estado_marca          SMALLINT,
       v_codigo_rechazo        SMALLINT,
       v_marca_causa           SMALLINT,
       v_fecha_causa           DATE,
       v_usuario               CHAR(20),
       v_cod_inconsistencia    SMALLINT,
       v_afore_desc            LIKE cat_afore.afore_desc,
       v_afore_cod             LIKE cat_afore.afore_cod,
       v_indice_retiro         SMALLINT,
       v_nss                   LIKE afi_fondo72.nss,
       v_rfc                   LIKE afi_fondo72.rfc,
       v_indice_modalidad      SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario   SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario   SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_datos_validos         SMALLINT, -- booleana que indica si los datos de entrada son correctos
       v_disponibilidad_valida SMALLINT, -- Bolleana que indica el resultado de la consulta de disponibilidad
       v_cod_retorno           SMALLINT, -- codigo de retorno
       v_desc_retorno          STRING,   -- descripcion del codigo de retorno
       v_cta_clabe_correcta    SMALLINT,  -- booleana que indica si la cuenta clabe tiene estructura correcta
       v_respuesta_marca       SMALLINT,
       v_id_solicitud          DECIMAL(9,0),
       v_info_genera_sha       STRING     -- Cadena que contiene los valores concatenados para calcular el SHA

    --se inicia en blanco la respuesta
    LET ws_ret_actualiza_out.codRet   = ""
    LET ws_ret_actualiza_out.mensaje  = ""
    LET ws_ret_actualiza_out.folio    = ""
    LET ws_ret_actualiza_out.nss      = ""
    LET ws_ret_actualiza_out.rfc      = ""
    LET ws_ret_actualiza_out.nombre   = ""
    LET ws_ret_actualiza_out.aPaterno = ""
    LET ws_ret_actualiza_out.aMaterno = ""
    LET ws_ret_actualiza_out.desAfore = ""

--ws_ret_actualiza_in.nss
--ws_ret_actualiza_in.beneficiario
--ws_ret_actualiza_in.claveAfore
--ws_ret_actualiza_in.nombre_s
--ws_ret_actualiza_in.paterno_s
--ws_ret_actualiza_in.materno_s
--ws_ret_actualiza_in.rfc_s
--ws_ret_actualiza_in.curp_s
--ws_ret_actualiza_in.street
--ws_ret_actualiza_in.numExt
--ws_ret_actualiza_in.numInt
--ws_ret_actualiza_in.mcStreet
--ws_ret_actualiza_in.transpZone
--ws_ret_actualiza_in.postCode1
--ws_ret_actualiza_in.telLada
--ws_ret_actualiza_in.telNumber
--ws_ret_actualiza_in.telCelular
--ws_ret_actualiza_in.poBoxLobby
--ws_ret_actualiza_in.f_resolucion
--ws_ret_actualiza_in.horaContact


    -- se crea el registro de la bitacora de la peticion ws registro de solicitud
    CALL fn_registra_actualiza_datos(1, NULL, NULL, NULL, NULL)
    RETURNING g_id_peticion

    -- se obtiene el id_derechohabiente 
    SELECT   id_derechohabiente--,  id_afi_fondo72
    INTO   v_id_derechohabiente--,v_id_afi_fondo72
    FROM   afi_derechohabiente
    WHERE  nss = ws_ret_actualiza_in.nss

    -- se valida que exista el id_derechohabiente
    IF v_id_derechohabiente IS NULL THEN
        CALL actualizarCodResp("101","NSS no se encuentra registrado en la base de datos Infonavit")
        RETURN
    END IF 

    --Validaciones a los datos de entrada
    IF ws_ret_actualiza_in.nombre_s IS NULL OR ws_ret_actualiza_in.nombre_s = "" THEN
        CALL actualizarCodResp("102","Nombre del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.paterno_s IS NULL OR ws_ret_actualiza_in.paterno_s = "" THEN
        CALL actualizarCodResp("103","Apellido paterno del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.rfc_s IS NULL OR ws_ret_actualiza_in.rfc_s = "" THEN
        CALL actualizarCodResp("105","RFC del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.curp_s IS NULL OR ws_ret_actualiza_in.curp_s = "" THEN
        CALL actualizarCodResp("106","CURP del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.street IS NULL OR ws_ret_actualiza_in.street = "" THEN
        CALL actualizarCodResp("107","Calle del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.numExt IS NULL OR ws_ret_actualiza_in.numExt = "" THEN
        CALL actualizarCodResp("108","Numero exterior del calle obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.mcStreet IS NULL OR ws_ret_actualiza_in.mcStreet = "" THEN
        CALL actualizarCodResp("110","Colonia del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.transpZone IS NULL OR ws_ret_actualiza_in.transpZone = "" THEN
        CALL actualizarCodResp("111","Municipio del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.postCodel IS NULL OR ws_ret_actualiza_in.postCodel = "" THEN
        CALL actualizarCodResp("112","Código postal del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.telLada IS NULL OR ws_ret_actualiza_in.telLada = "" THEN
        CALL actualizarCodResp("113","Lada del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.telNumber IS NULL OR ws_ret_actualiza_in.telNumber = "" THEN
        CALL actualizarCodResp("114","Telefono del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.telCelular IS NULL OR ws_ret_actualiza_in.telCelular = "" THEN
        CALL actualizarCodResp("115","Celular del solicitante obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.poBoxLobby IS NULL OR ws_ret_actualiza_in.poBoxLobby = "" THEN
        CALL actualizarCodResp("116","Correo electrónico del solicitante obligatorio")
        RETURN
    END IF

    IF Length(ws_ret_actualiza_in.rfc_s CLIPPED ) < 13 THEN
        CALL actualizarCodResp("117","RFC del solicitante < 13 posiciones")
        RETURN
    END IF

    IF Length(ws_ret_actualiza_in.curp_s CLIPPED ) < 18 THEN
        CALL actualizarCodResp("118","CURP del solicitante < 18 posiciones")
        RETURN
    END IF

    IF ws_ret_actualiza_in.beneficiario IS NULL OR ws_ret_actualiza_in.beneficiario = "" THEN
        CALL actualizarCodResp("122","Beneficiario obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.claveAfore IS NULL OR ws_ret_actualiza_in.claveAfore = "" THEN
        CALL actualizarCodResp("123","Clave Afore obligatorio")
        RETURN
    END IF

    IF ws_ret_actualiza_in.f_resolucion IS NULL OR ws_ret_actualiza_in.f_resolucion = "" THEN
        CALL actualizarCodResp("124","Fecha de resolución obligatoria")
        RETURN
    END IF

    IF ws_ret_actualiza_in.horaContact IS NULL OR ws_ret_actualiza_in.horaContact = "" THEN
        CALL actualizarCodResp("125","Hora de contacto obligatoria")
        RETURN
    END IF

    --validacion que sea numerico el código del afore
    TRY
        LET v_afore_cod = ws_ret_actualiza_in.claveAfore
        CATCH
            CALL actualizarCodResp("126","No existe Clave Afore en catalogo")
            RETURN
    END TRY

    --se obtiene la descripción del afore
    SELECT afore_desc
    INTO v_afore_desc
    FROM cat_afore
    WHERE afore_cod = v_afore_cod

    -- se valida que exista el afore
    IF v_afore_desc IS NULL THEN
        CALL actualizarCodResp("126","No existe Clave Afore en catalogo")
        RETURN
    END IF 

    --terminan validaciones e inicia el proceso
    -- Asigna variables para la ejecución del store
    LET v_marca_entra    = 815
    LET v_proceso_cod    = g_proceso_cod_ret_ley73_ws
    LET v_folio          = "0"
    LET v_estado_marca   = "0"
    LET v_codigo_rechazo = "0"
    LET v_marca_causa    = "0"
    LET v_fecha_causa    = NULL
    LET v_usuario        = "safreviv"

    --Solicitud de marcaje
    --Consulta si existe  otra solicitud para el derechohabiente 
    CALL fn_verifica_solicitud_generico(v_id_derechohabiente,3,1)
    RETURNING v_existe_solicitud, v_n_referencia

    -- Valida que no exista otra solicitud 
    IF NOT v_existe_solicitud THEN
        --Asigna estatus de la marca
        LET v_estatus_marca = 10

        --Se obtiene el número de solicitud
        SELECT seq_ret_solicitud.NEXTVAL
        INTO   v_n_referencia
        FROM   systables 
        WHERE  tabid = 1

        -- Actualiza tabla de retiro genérico   
        CALL fn_actualiza_retiro_generico(v_id_derechohabiente,ws_ret_actualiza_in.nss,ws_ret_actualiza_in.rfc_s
                                          ,3,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,1)
             RETURNING v_res_actualizacion

        -- Verifica que no haya existido un error en la actualización de la tabla
        IF v_res_actualizacion THEN

            ---Se ejecuta la función de marcaje para la nueva solicitud
            CALL fn_ret_generico_marca_cuenta( v_id_derechohabiente ,
                                                v_marca_entra       ,
                                                v_n_referencia      ,
                                                v_folio             ,
                                                v_estado_marca      ,
                                                v_codigo_rechazo    ,
                                                v_marca_causa       ,
                                                v_fecha_causa       ,
                                                v_usuario           ,
                                                v_proceso_cod )
            RETURNING v_respuesta_marcaje

            -- Valida que la respuesta sea correcta
            IF ( v_respuesta_marcaje = 0 ) THEN
                CALL actualizarCodResp("301","Solicitud aceptada")

                -- Se ejecutó correctamente el store, se indica que se marcó la cuenta
                --LET v_estatus_marca = gi_estatus_marca_existoso

                --Se indica código de rechazo 0, para mostrar que la ejecución fué correcta
                --LET v_cod_inconsistencia = 0
            ELSE
                -- Sucedió un error al ejecutar la función de marcado
                -- Se elimina el registro creado
                CALL fn_actualiza_retiro_generico(v_id_derechohabiente, ws_ret_actualiza_in.nss,ws_ret_actualiza_in.rfc_s
                                                 ,p_modalidad, v_n_referencia,v_folio, v_estatus_marca, v_codigo_rechazo, 3)
                     RETURNING v_res_actualizacion

                -- se indica que no se pudo marcar
                LET v_estatus_marca = gi_estatus_marca_no_exitoso

                -- se verifica si es por inconvivencia
                IF ( v_respuesta_marcaje > 0 ) THEN
                    LET v_cod_inconsistencia = gi_error_marca_no_convive
                ELSE
                    -- Error al marcar/desmarcar la cuenta
                    LET v_cod_inconsistencia = gi_error_interno_marca
                END IF
                CALL actualizarCodResp("304","Error al insertar el registro")

            END IF   	             
        ELSE
            CALL actualizarCodResp("304","Error al insertar el registro")

            -- Existió un error en la integración de retiro genérico
            --LET v_estatus_marca = gi_estatus_marca_no_exitoso

            -- error al generar la solicitud en ret_solicitud_generico
            --LET v_cod_inconsistencia = gi_error_marca_generar_solicitud
        END IF
                
    ELSE
        CALL actualizarCodResp("302","Ya existe el NSS con los datos del solicitante")
        LET v_n_referencia = NULL
        --Se solicitó marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
        --LET v_estatus_marca = gi_estatus_marca_no_exitoso

        --Se envía código de rechazo indicando que existe otra solicitud en marcha
        --LET v_cod_inconsistencia = gi_solicitud_en_tramite

    END IF     

    LET ws_ret_actualiza_out.folio = ws_ret_actualiza_in.nss,ws_ret_actualiza_in.claveAfore
    LET ws_ret_actualiza_out.nss = ws_ret_actualiza_in.nss
    LET ws_ret_actualiza_out.rfc = ws_ret_actualiza_in.rfc_s
    LET ws_ret_actualiza_out.curp = ws_ret_actualiza_in.curp_s
    LET ws_ret_actualiza_out.tipo_doc = ws_ret_actualiza_in.tipo_doc
    LET ws_ret_actualiza_out.folio_ife = ws_ret_actualiza_in.folio_ife
    LET ws_ret_actualiza_out.clave_ife = ws_ret_actualiza_in.clave_ife
    LET ws_ret_actualiza_out.nombre = ws_ret_actualiza_in.nombre_s
    LET ws_ret_actualiza_out.aPaterno = ws_ret_actualiza_in.paterno_s
    LET ws_ret_actualiza_out.aMaterno = ws_ret_actualiza_in.materno_s
    LET ws_ret_actualiza_out.desAfore = v_afore_desc
    LET v_info_genera_sha = ws_ret_actualiza_out.folio CLIPPED, ws_ret_actualiza_out.nss CLIPPED,
                            ws_ret_actualiza_out.rfc CLIPPED, ws_ret_actualiza_out.curp CLIPPED,
                            ws_ret_actualiza_out.tipo_doc CLIPPED, ws_ret_actualiza_out.folio_ife CLIPPED,
                            ws_ret_actualiza_out.clave_ife CLIPPED, ws_ret_actualiza_out.nombre CLIPPED,
                            ws_ret_actualiza_out.aPaterno CLIPPED, ws_ret_actualiza_out.aMaterno CLIPPED,
                            ws_ret_actualiza_out.desAfore CLIPPED 
    CALL fn_hash(v_info_genera_sha CLIPPED, "SHA1") RETURNING ws_ret_actualiza_out.e_sha1
    --LET ws_ret_actualiza_out.e_sha1 = "kj8557)(&/%&%q&#$q#uHIWUGEWIEFWEOWIH473"
    CALL fn_registra_actualiza_datos(2, v_n_referencia, g_id_peticion, v_afore_desc, ws_ret_actualiza_out.e_sha1)
    RETURNING g_id_peticion


END FUNCTION

FUNCTION actualizarCodResp(p_cod,p_mensaje)
DEFINE p_cod STRING
      ,p_mensaje STRING
    LET ws_ret_actualiza_out.codRet = p_cod
    LET ws_ret_actualiza_out.mensaje = p_mensaje
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
               "\n 213)                  " -- restitucion

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
{
======================================================================
Clave: 
Nombre: fn_actualiza_retiro_generico
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico


Registro de modificaciones:
Autor           Fecha        Descrip. cambio
Ivan Vega     26 Nov 2013  - La solicitud se crea con grupo de ventanilla
                             infonavit
Eneas Armas   20140122     Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
              20140122     Se cambia la tabla ret_ley73 por ret_ley73_generico
======================================================================
}
FUNCTION fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad_retiro, p_id_solicitud,
                                      p_folio, p_estado_solicitud, p_cod_rechazo, p_tipo_actualizacion)
DEFINE p_id_derechohabiente       DECIMAL(9,0),
       p_nss                      CHAR(11),
       p_rfc                      CHAR(13),
       p_modalidad_retiro         SMALLINT,
       p_id_solicitud             DECIMAL(9,0),
       p_folio                    DECIMAL(9,0),
       p_estado_solicitud         SMALLINT,
       p_cod_rechazo              SMALLINT,
       p_tipo_actualizacion       SMALLINT, --Variable que recibe el tipo de actualización(INSERT,UPDATE,DELETE)
       v_sql                      STRING,
       v_respuesta                SMALLINT,
       v_aivs92                   DECIMAL(22,2),
       v_aivs97                   DECIMAL(22,2),
       v_pesos92                  DECIMAL(22,2),
       v_pesos97                  DECIMAL(22,2),
       v_tesofe                   DECIMAL(22,2),
       v_p_tesofe                 DECIMAL(22,2),
       v_precio_fondo             DECIMAL(10,5),
       v_resultado                SMALLINT,
       v_existe_entidad           SMALLINT,
       v_entidad_federativa       SMALLINT,
       v_codigo_postal            CHAR(5),
       v_cantidad_marcas          SMALLINT,
       v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud
   
    -- Valida el tipo de actualización 1-Inserción, 2-Actualización
    CASE p_tipo_actualizacion

        WHEN 1
            -- Arma el query INSERCIÓN
            LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
            LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
            LET v_r_ret_solicitud_generico.nss                    = p_nss
            LET v_r_ret_solicitud_generico.rfc                    = p_rfc
            LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
            LET v_r_ret_solicitud_generico.folio                  = p_folio
            LET v_r_ret_solicitud_generico.caso_adai              = NULL
            LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
            LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
            LET v_r_ret_solicitud_generico.folio_restitucion      = 0
            LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
            LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
--            LET v_r_ret_solicitud_generico.folio_afore            = p_nss,ws_ret_actualiza_in.claveAfore -- se usa en ventanilla afore
--            LET v_r_ret_solicitud_generico.grupo_ventanilla       = gi_ventanilla_afore -- solicitud iniciada en afore
            LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
            LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
            LET v_r_ret_solicitud_generico.estado_solicitud       = p_estado_solicitud
            LET v_r_ret_solicitud_generico.cod_rechazo            = p_cod_rechazo

            -- se inserta el registro
            INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )
                                    
            -- Valida que la ejecución del insert haya sido correcta
            IF ( SQLCA.SQLCODE < 0 ) THEN
                -- Asigna respuesta negativa
                LET v_respuesta = FALSE
                CALL ERRORLOG("Se ha producido un error al insertar el registro en retiro genérico")
            ELSE
                -- Asigna respuesta afirmativa
                LET v_respuesta = TRUE
                CALL ERRORLOG("Se ha insertado con éxito el registro en retiro genérico")
                -- Inserta en ret_ley73_generico
                CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs92, v_pesos92
                -- se obtiene el saldo de viv97
                CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs97, v_pesos97
                -- se obtiene el saldo de aportaciones voluntarias
                CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_tesofe, v_p_tesofe
                -- *****************************************************************************
                --- Valida si tiene crédito para saber si se paga vivienda 97 o no SACI2019-156
                -- *****************************************************************************
                LET v_cantidad_marcas = 0
                SELECT COUNT(*)  
                INTO   v_cantidad_marcas
                FROM   sfr_marca_activa 
                WHERE  id_derechohabiente = p_id_derechohabiente
                AND    marca IN (202,212,218,220,222,223,224,226,227,232,233)                 

                IF v_cantidad_marcas > 0 THEN 
                  LET v_aivs97 = 0
                  LET v_tesofe = 0
                END IF 
                -- ******************************************************************************
                
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
                       VALUES (p_id_solicitud, p_id_derechohabiente, p_folio, 1, 114,v_r_ret_solicitud_generico.f_solicitud,
                               v_r_ret_solicitud_generico.f_solicitud,v_aivs92,v_aivs97,v_pesos92,v_pesos97,v_tesofe,
                               v_r_ret_solicitud_generico.f_solicitud,v_r_ret_solicitud_generico.h_solicitud,'vafore',
                               p_estado_solicitud,p_cod_rechazo);
                -- Se inserta en ret_sol_medio_entrega para saber que se registro por Ventanilla Afore
                INSERT INTO ret_sol_medio_entrega (id_solicitud, grupo, medio_entrega,f_registro)
                VALUES (p_id_solicitud, 1, 5, CURRENT YEAR TO MINUTE);               
                -- Se busca la entidad federativa a partir del código postal
                LET v_entidad_federativa = ws_ret_actualiza_in.transpZone.subString(3,4)
                CALL fn_registra_beneficiario_retiro_generico(
                     p_id_solicitud,
                     1, -- Titular
                     1, -- SPEI
                     1, -- FALTA PARENTESCO
                     ws_ret_actualiza_in.paterno_s,
                     ws_ret_actualiza_in.materno_s,
                     ws_ret_actualiza_in.nombre_s,
                     ws_ret_actualiza_in.telNumber,
                     ws_ret_actualiza_in.poBoxLobby,
                     100,
                     v_aivs92 + v_aivs97,
                     v_pesos92 + v_pesos97 + v_tesofe,
                     ws_ret_actualiza_in.cuenta_clabe,
                     "",
                     v_entidad_federativa)            
            END IF
                                
        WHEN 2

            -- si no llego el codigo de rechazo, se define el generico
            IF ( p_cod_rechazo IS NULL ) THEN
                LET p_cod_rechazo = 54 -- rechazo generico. CAMBIAR POR CONSTANTE GLOBAL
            END IF

            -- Arma el query ACTUALIZACION
            LET v_sql = "\nUPDATE ret_solicitud_generico",
                         "\nSET    estado_solicitud   =  ", p_estado_solicitud, ",",
                         "\n       cod_rechazo        =  ", p_cod_rechazo,
                         "\nWHERE  id_derechohabiente =  ", p_id_derechohabiente,
                         "\nAND    modalidad_retiro   =  ", p_modalidad_retiro,
                         "\nAND    id_solicitud       =  ", p_id_solicitud
              
            PREPARE stm_update_retiro FROM v_sql
            EXECUTE stm_update_retiro 
                                   
            -- Valida que la ejecución del insert haya sido correcta
            IF ( SQLCA.SQLCODE < 0 ) THEN
                -- Asigna respuesta negativa
                LET v_respuesta = FALSE
                CALL ERRORLOG("Se ha producido un error al actualizar el registro en retiro genérico")
            ELSE
                -- Asigna respuesta afirmativa
                LET v_respuesta = TRUE
                CALL ERRORLOG("Se ha actualizado con éxito el registro en retiro genérico")

                -- se actualiza la tabla de historicos con el estado de solicitud y cogido de rechazo segun la modalidad
                CASE p_modalidad_retiro
                    WHEN 2 -- fondo de ahorro               
                        UPDATE ret_fondo_ahorro_generico
                        SET    estado_solicitud = p_estado_solicitud,
                               cod_rechazo      = p_cod_rechazo
                        WHERE  id_solicitud     = p_id_solicitud

                        -- si se trata de un rechazo, se desmarca
                        IF ( p_estado_solicitud = 100 ) THEN
                            CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 802, 
                                                          p_id_solicitud, 802,
                                                          "safreviv", g_proceso_cod_ret_fondo_ahorro)
                        END IF

                    WHEN 3 -- ley 73
                        UPDATE ret_ley73_generico
                        SET    estado_solicitud = p_estado_solicitud,
                               cod_rechazo      = p_cod_rechazo
                        WHERE  id_solicitud     = p_id_solicitud

                        -- si se trata de un rechazo, se desmarca
                        IF ( p_estado_solicitud = 100 ) THEN
                            CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 815,
                                                              p_id_solicitud, 815,
                                                              "safreviv", g_proceso_cod_ret_amort_excedentes)
                        END IF

                    WHEN 9 -- amortizaciones excedentes
                        UPDATE ret_amort_excedente
                        SET    estado_solicitud = p_estado_solicitud,
                               cod_rechazo      = p_cod_rechazo
                        WHERE  id_solicitud     = p_id_solicitud

                        -- si se trata de un rechazo, se desmarca
                        IF ( p_estado_solicitud = 100 ) THEN
                            CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 810, 
                                                          p_id_solicitud, 810,
                                                          "safreviv", g_proceso_cod_ret_amort_excedentes)
                        END IF

                    WHEN 10 -- aportaciones voluntarias
                        UPDATE ret_voluntaria
                        SET    estado_solicitud = p_estado_solicitud,
                               cod_rechazo      = p_cod_rechazo
                        WHERE  id_solicitud     = p_id_solicitud

                        -- si se trata de un rechazo, se desmarca
                        IF ( p_estado_solicitud = 100 ) THEN
                            CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 809, 
                                                              p_id_solicitud, 809,
                                                              "safreviv", g_proceso_cod_ret_amort_excedentes)
                        END IF
                END CASE
            END IF
        WHEN 3

            -- Arma el query ELIMINACIÓN
            LET v_sql = "\n DELETE FROM ret_solicitud_generico",
            "\n WHERE  id_derechohabiente = ", p_id_derechohabiente,
            "\n AND    modalidad_retiro   = ", p_modalidad_retiro,
            "\n AND    id_solicitud       = ", p_id_solicitud

            PREPARE stm_delete_retiro FROM v_sql
            EXECUTE stm_delete_retiro 

            --Valida que la ejecución del insert haya sido correcta
            IF ( SQLCA.SQLCODE < 0 ) THEN
                -- Asigna respuesta negativa
                LET v_respuesta=FALSE
                CALL ERRORLOG("Se ha producido un error al eliminar el registro en retiro genérico")
            ELSE

                -- Asigna respuesta afirmativa
                LET v_respuesta = TRUE
                CALL ERRORLOG("Se ha eliminado con éxito el registro en retiro genérico")
            END IF              
    END CASE

    -- Regresa respuesta
    RETURN v_respuesta
END FUNCTION                  

{
======================================================================
Clave: 
Nombre: fn_valida_estructura_nss
Fecha creacion: Junio 05, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Valida la estructura del NSS

======================================================================
}
FUNCTION fn_valida_estructura_nss(p_nss)
DEFINE p_nss                CHAR(11),
       v_ban_valido         SMALLINT,
       v_encontrado         SMALLINT,
       i                    SMALLINT,
       v_caracter           CHAR(1)
       
    LET v_ban_valido = 0
    LET v_encontrado = 0
    LET v_caracter   = ""
    LET i            = 0
    
    -- Se eliminan los espacios   
    LET p_nss = p_nss CLIPPED 
    -- Se le agrega un cero a la izquierda si el nss es enviado es a 10 posiciones
    IF LENGTH(p_nss) = 10 THEN 
        LET p_nss = "0", p_nss
    END IF 
    -- Se valida que no contenga espacios el NSS
    -- Se valida que solo se conforme de numeros
    FOR i = 1 TO 11 
        LET v_caracter = p_nss[i,i]
        CASE 
            WHEN (v_caracter = 0 OR 
                  v_caracter = 1 OR
                  v_caracter = 2 OR
                  v_caracter = 3 OR
                  v_caracter = 4 OR
                  v_caracter = 5 OR
                  v_caracter = 6 OR
                  v_caracter = 7 OR
                  v_caracter = 8 OR
                  v_caracter = 9 )
                LET v_ban_valido = 0
            OTHERWISE
                LET v_ban_valido = 1
                EXIT FOR
        END CASE 
    END FOR
    RETURN v_ban_valido
END FUNCTION                 
{
======================================================================
Clave: 
Nombre: fn_valida_marcas
Fecha creacion: Junio 05, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Valida la estructura del NSS

======================================================================
}
FUNCTION fn_valida_marcas(p_id_derechohabiente)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_ban_valido         SMALLINT,
       v_cod_rechazo        CHAR(2),
       v_desc_cod_rechazo   CHAR(100),
       v_marca_paso         SMALLINT,
       v_caracter           CHAR(1),
       v_resultado          SMALLINT,
       v_saldo_aivs         DECIMAL(22,2),
       v_saldo_pesos        DECIMAL(22,2)
       
LET v_ban_valido       = 0
LET v_caracter         = ""
LET v_cod_rechazo      = ""
LET v_desc_cod_rechazo = ""
LET v_resultado        = 0
LET v_saldo_aivs       = 0.00
LET v_saldo_pesos      = 0.00

    DECLARE cur_marca CURSOR FOR SELECT marca  
                                 FROM   sfr_marca_activa 
                                 WHERE  id_derechohabiente = p_id_derechohabiente
                                 --AND    marca IN (593, 594, 590, 595, 596, 597, 280, 701, 702, 833, 213, 815,805,806,808, 814,
                                 --                 201,202,203,204,205,210,211,212,214,215,216,217,218,221,223,232,233)
                                 AND    marca IN (150,151,160,161,201,203,204,205,206,210,211,213,214,215,216,217,
                                                  221,223,225,231,233,234,235,280,401,501,502,503,504,551,590,592,
                                                  593,594,595,596,597,701,702,705,706,803,805,806,808,814,815,819,
                                                  820,822,202,212,218,220,222,224,226,227,232)                 
    
    FOREACH cur_marca INTO v_marca_paso
        IF v_marca_paso = 150 OR  v_marca_paso = 151 OR
           v_marca_paso = 160 OR  v_marca_paso = 161 OR
           v_marca_paso = 501 OR  v_marca_paso = 502 OR
           v_marca_paso = 503 OR  v_marca_paso = 504 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_22
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_22
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 803 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_58
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_58
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 590 OR  v_marca_paso = 595 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_63
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_63
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 592 OR  v_marca_paso = 596 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_64
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_64
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 593 OR  v_marca_paso = 594 OR
           v_marca_paso = 597 OR  v_marca_paso = 814 OR
           v_marca_paso = 819 OR  v_marca_paso = 820 OR
           v_marca_paso = 822 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_65
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_65
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 280 OR  v_marca_paso = 551 OR
           v_marca_paso = 701 OR  v_marca_paso = 702 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_73
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_73
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 201 OR  v_marca_paso = 203 OR
           v_marca_paso = 204 OR  v_marca_paso = 205 OR
           v_marca_paso = 210 OR  v_marca_paso = 211 OR
           v_marca_paso = 214 OR  v_marca_paso = 215 OR
           v_marca_paso = 216 OR  v_marca_paso = 217 OR
           v_marca_paso = 221 OR  
           v_marca_paso = 225 OR  v_marca_paso = 231 OR
           v_marca_paso = 234 OR
           v_marca_paso = 235 OR  v_marca_paso = 401 OR
           v_marca_paso = 705 OR  v_marca_paso = 706 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_80
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_80
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 206 OR  v_marca_paso = 213 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_81
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_81
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 805 OR  v_marca_paso = 806 OR
           v_marca_paso = 808 OR  v_marca_paso = 815 THEN
            LET v_cod_rechazo      = g_url_disp_cod_ret_88
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_88
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 202 OR  v_marca_paso = 212 OR
           v_marca_paso = 218 OR  v_marca_paso = 220 OR
           v_marca_paso = 222 OR  v_marca_paso = 224 OR
           v_marca_paso = 223 OR  v_marca_paso = 233 OR
           v_marca_paso = 226 OR  v_marca_paso = 227 OR
           v_marca_paso = 232 THEN
           -- Buscamos el saldo de vivienda 92, si no tiene se regresa con código 80
           CALL fn_calcula_saldo_ley73(ws_ret_cons_nss_in.nss,8,TODAY) RETURNING v_resultado, v_saldo_aivs, v_saldo_pesos
           IF v_resultado = 0 AND v_saldo_aivs <= 0 THEN 
              LET v_cod_rechazo      = g_url_disp_cod_ret_80
              LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_80
              LET v_ban_valido = 1
              EXIT FOREACH
           END IF 
        END IF 
        -- Validaciones Anteriores
--        IF v_marca_paso = 593 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_40
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_40
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 594 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_41
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_41
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 590 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_63
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_63
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 595 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_63
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_63
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 596 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_64
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_64
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 597 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_65
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_65
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 280 OR v_marca_paso = 701 OR v_marca_paso = 702 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_73
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_73
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 805 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_90
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_90
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 806 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_92
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_92
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 808 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_91
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_91
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 814 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_65
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_65
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF v_marca_paso = 815 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_88
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_88
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF (v_marca_paso = 213) THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_81
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_81
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
--        IF (v_marca_paso = 201 OR v_marca_paso = 203 OR v_marca_paso = 204 OR
--            v_marca_paso = 205 OR v_marca_paso = 210 OR v_marca_paso = 211 OR 
--            v_marca_paso = 214 OR v_marca_paso = 215 OR v_marca_paso = 216 OR 
--            v_marca_paso = 217 OR v_marca_paso = 221 ) THEN 
--        LET v_cod_rechazo      = g_url_disp_cod_ret_80
--        LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_80
--        LET v_ban_valido = 1
--        EXIT FOREACH        
--        END IF     
    END FOREACH


    -- Se eliminan los espacios   
    RETURN v_ban_valido, v_cod_rechazo, v_desc_cod_rechazo
END FUNCTION                 

{
======================================================================
Clave: 
Nombre: fn_valida_solicitud_previa
Fecha creacion: Junio 05, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Valida la estructura del NSS

======================================================================
}
FUNCTION fn_valida_solicitud_previa(p_id_derechohabiente)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_ban_valido         SMALLINT,
       v_cod_rechazo        CHAR(2),
       v_desc_cod_rechazo   CHAR(100),
       v_marca_paso         SMALLINT,
       v_estado_solicitud   SMALLINT,
       v_cod_rech_sol       SMALLINT,
       v_caracter           CHAR(1)

       
    LET v_ban_valido       = 0
    LET v_caracter         = ""
    LET v_cod_rechazo      = ""
    LET v_desc_cod_rechazo = ""
    LET v_estado_solicitud = 0
    LET v_cod_rech_sol     = 0

    DECLARE cur_solicitud CURSOR FOR SELECT estado_solicitud, cod_rechazo  
                                     FROM   ret_solicitud_generico 
                                     WHERE  id_derechohabiente = p_id_derechohabiente 
                                     AND    id_solicitud IN (SELECT MAX(id_solicitud)
                                                             FROM   ret_solicitud_generico
                                                             WHERE  id_derechohabiente = p_id_derechohabiente
                                                             AND    modalidad_retiro = 3)
    
    FOREACH cur_solicitud INTO v_estado_solicitud, v_cod_rech_sol
        IF v_estado_solicitud = 8 OR v_estado_solicitud = 10 THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_58
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_58
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_estado_solicitud = 15 THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_59
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_59
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_estado_solicitud = 50 OR v_estado_solicitud = 60 THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_60
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_60
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_estado_solicitud = 72 THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_61
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_61
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF ((v_estado_solicitud = 209 OR v_estado_solicitud = 210 OR 
             v_estado_solicitud = 211 OR v_estado_solicitud = 212 ) AND 
             (v_cod_rech_sol = 65)) THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_62
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_62
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF v_marca_paso = 208 OR v_marca_paso = 701 OR v_marca_paso = 702 THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_73
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_73
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        -- Se elimina esta validación para permitir pagos complementarios, de solicitudes ya pagadas y rechazadas por Banco, SACI2019-12 
--        IF v_estado_solicitud = 73 THEN 
--            LET v_cod_rechazo      = g_url_disp_cod_ret_74
--            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_74
--            LET v_ban_valido = 1
--            EXIT FOREACH        
--        END IF 
        IF (v_estado_solicitud = 215) THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_75
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_75
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF (v_estado_solicitud = 71 ) THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_76
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_76
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF (v_estado_solicitud = 90 ) AND (
             v_cod_rech_sol = 65) THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_77
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_77
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
        IF (v_estado_solicitud = 70 OR v_estado_solicitud = 700 ) THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_78
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_78
            LET v_ban_valido = 1
            EXIT FOREACH        
        END IF 
    END FOREACH
    -- Se eliminan los espacios   
    RETURN v_ban_valido, v_cod_rechazo, v_desc_cod_rechazo
END FUNCTION                 

{
======================================================================
Clave: 
Nombre: fn_valida_solicitud_previa
Fecha creacion: Junio 05, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Valida la estructura del NSS

======================================================================
}
FUNCTION fn_valida_spess(p_nss)
DEFINE p_nss                CHAR(11),
       v_ban_valido         SMALLINT,
       v_cod_rechazo        CHAR(2),
       v_desc_cod_rechazo   CHAR(100),
       v_regimen            SMALLINT,
       v_tpo_prestacion     CHAR(2),
       v_f_resolucion       DATE,
       v_id_datamart        DECIMAL(9,0),
       v_sql                STRING

       
    LET v_ban_valido       = 0
    LET v_cod_rechazo      = ""
    LET v_desc_cod_rechazo = ""
    LET v_regimen          = 0
    LET v_tpo_prestacion   = 0

    LET v_sql = "\nSELECT FIRST 1 a.regimen, a.f_resolucion, a.tpo_prestacion, a.id_datamart ",
                "\nFROM   ret_datamart a                              ",
                "\nWHERE  a.nss            = '", p_nss,            "' ", -- el trabajador
                "\nAND    a.sec_pension in (SELECT MAX(sec_pension)   ", -- el regimen es forzoso que sea Ley73
                "\n                         FROM   ret_datamart       ", -- retiro avalado por el imss
                "\n                         WHERE  nss = '", p_nss,"')"                      

    PREPARE sid_ley73 FROM v_sql
    EXECUTE sid_ley73 INTO v_regimen, v_f_resolucion, v_tpo_prestacion, v_id_datamart

    -- si se encontro un datamart
    IF ( v_id_datamart IS NULL ) THEN
        LET v_cod_rechazo      = g_url_disp_cod_ret_82
        LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_82
        LET v_ban_valido = 1
    ELSE 
        IF v_f_resolucion < "01/13/2012" THEN 
            LET v_cod_rechazo      = g_url_disp_cod_ret_83
            LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_83
            LET v_ban_valido = 1
        ELSE 
            IF v_regimen <> 73 THEN
                LET v_cod_rechazo      = g_url_disp_cod_ret_84
                LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_84
                LET v_ban_valido = 1
            ELSE 
                IF v_tpo_prestacion <> "00" THEN 
                    LET v_cod_rechazo      = g_url_disp_cod_ret_85
                    LET v_desc_cod_rechazo = g_url_disp_desc_cod_ret_85
                    LET v_ban_valido = 1
                END IF  
            END IF 
        END IF
    END IF 
    
    RETURN v_ban_valido, v_cod_rechazo, v_desc_cod_rechazo
END FUNCTION                 


{
======================================================================
Nombre: fn_registra_actualiza_datos
Fecha creacion: Febrero 02, 2016
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para actualización de datos de retiro de ventanilla
afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_actualiza_datos(p_ind_operacion, p_id_solicitud,p_id_peticion, p_desafore, p_e_sha)
DEFINE v_id_peticion                    DECIMAL(9,0), -- id de la peticion
       p_ind_operacion                  SMALLINT,     -- 1 inserta, 2 actualiza
       p_id_solicitud                   DECIMAL(9,0), -- id asignado a la solicitud
       p_id_peticion                    DECIMAL(9,0), -- id de la petición
       p_nss                            CHAR(11),     -- NSS del trabajador
       p_desafore                       CHAR(50),     -- Nombre de la Afore
       p_e_sha                          CHAR(50),     -- Cadena de integridad de la respuesta
       v_folio                          CHAR(14),     -- Variable de trabajo para el folio
       v_r_ret_ws_actualiza_datos_v_a   RECORD LIKE ret_ws_actualiza_datos_v_a.*

       
       
		
    -- si se solicita crear la bitacora de la peticion
    IF ( p_ind_operacion = 1 ) THEN
        -- se obtiene el id de peticion nuevo
        SELECT seq_ret_ws_generico.nextVal
        INTO   v_id_peticion
        FROM   systables
        WHERE  tabid = 1
      
        -- se asignan los datos
        LET v_r_ret_ws_actualiza_datos_v_a.id_peticion             = v_id_peticion
        LET v_r_ret_ws_actualiza_datos_v_a.id_solicitud            = NULL
        LET v_r_ret_ws_actualiza_datos_v_a.folio                   = NULL 
        LET v_r_ret_ws_actualiza_datos_v_a.nss                     = ws_ret_actualiza_in.nss                    
        LET v_r_ret_ws_actualiza_datos_v_a.beneficiario            = ws_ret_actualiza_in.beneficiario       
        LET v_r_ret_ws_actualiza_datos_v_a.claveafore              = ws_ret_actualiza_in.claveAfore
        LET v_r_ret_ws_actualiza_datos_v_a.nombre_s                = ws_ret_actualiza_in.nombre_s
        LET v_r_ret_ws_actualiza_datos_v_a.paterno_s               = ws_ret_actualiza_in.paterno_s
        LET v_r_ret_ws_actualiza_datos_v_a.materno_s               = ws_ret_actualiza_in.materno_s
        LET v_r_ret_ws_actualiza_datos_v_a.rfc_s                   = ws_ret_actualiza_in.rfc_s
        LET v_r_ret_ws_actualiza_datos_v_a.curp_s                  = ws_ret_actualiza_in.curp_s                   
        LET v_r_ret_ws_actualiza_datos_v_a.tipodoc                 = ws_ret_actualiza_in.tipo_doc
        LET v_r_ret_ws_actualiza_datos_v_a.folioife                = ws_ret_actualiza_in.folio_ife
        LET v_r_ret_ws_actualiza_datos_v_a.claveife                = ws_ret_actualiza_in.clave_ife
        LET v_r_ret_ws_actualiza_datos_v_a.street                  = ws_ret_actualiza_in.street
        LET v_r_ret_ws_actualiza_datos_v_a.numext                  = ws_ret_actualiza_in.numExt
        LET v_r_ret_ws_actualiza_datos_v_a.numint                  = ws_ret_actualiza_in.numInt
        LET v_r_ret_ws_actualiza_datos_v_a.mcstreet                = ws_ret_actualiza_in.mcStreet
        LET v_r_ret_ws_actualiza_datos_v_a.transpzone              = ws_ret_actualiza_in.transpZone
        LET v_r_ret_ws_actualiza_datos_v_a.postcodel               = ws_ret_actualiza_in.postCodel
        LET v_r_ret_ws_actualiza_datos_v_a.tellada                 = ws_ret_actualiza_in.telLada
        LET v_r_ret_ws_actualiza_datos_v_a.telnumber               = ws_ret_actualiza_in.telNumber
        LET v_r_ret_ws_actualiza_datos_v_a.telcelular              = ws_ret_actualiza_in.telCelular
        LET v_r_ret_ws_actualiza_datos_v_a.poboxlobby              = ws_ret_actualiza_in.poBoxLobby
        LET v_r_ret_ws_actualiza_datos_v_a.fresolucion             = ws_ret_actualiza_in.f_resolucion
        LET v_r_ret_ws_actualiza_datos_v_a.horacontact             = ws_ret_actualiza_in.horaContact
        LET v_r_ret_ws_actualiza_datos_v_a.cuentaclabe             = ws_ret_actualiza_in.cuenta_clabe
        LET v_r_ret_ws_actualiza_datos_v_a.cuentabancaria          = ws_ret_actualiza_in.cuenta_bancaria
        LET v_r_ret_ws_actualiza_datos_v_a.datosbancarios          = ws_ret_actualiza_in.datos_bancarios
        LET v_r_ret_ws_actualiza_datos_v_a.importedevolver         = ws_ret_actualiza_in.importe_devolver
        LET v_r_ret_ws_actualiza_datos_v_a.fechacreacion           = ws_ret_actualiza_in.fecha_creacion
        LET v_r_ret_ws_actualiza_datos_v_a.lugaremision            = ws_ret_actualiza_in.lugar_emision
        LET v_r_ret_ws_actualiza_datos_v_a.desafore                = NULL 
        LET v_r_ret_ws_actualiza_datos_v_a.e_sha1                  = NULL 
        
      
        -- se inserta el registro de peticion
        INSERT INTO ret_ws_actualiza_datos_v_a VALUES ( v_r_ret_ws_actualiza_datos_v_a.* )
    ELSE
        -- se actualiza el id_solicitud en la peticion
        LET v_folio = ws_ret_actualiza_in.nss, ws_ret_actualiza_in.claveAfore
        UPDATE ret_ws_actualiza_datos_v_a
        SET    id_solicitud = p_id_solicitud,
               folio        = v_folio,
               desafore     = p_desafore,
               e_sha1       = p_e_sha
        WHERE  id_peticion  = p_id_peticion

        -- se devuelve la misma id de peticion
        LET v_id_peticion = g_id_peticion
    END IF

    -- se devuelve el id de la peticion
    RETURN v_id_peticion
END FUNCTION

{
======================================================================
Clave: 
Nombre: reimpresionDatosGenerales
Fecha creacion: diciembre 02, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta general por nss
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de vivienda segun ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION reimpresionDatosGenerales()
DEFINE p_nss                  CHAR(11),      -- NSS
       p_folio                char(14),      -- El folio se compone del NSS y la clave de Afore
       p_es_consulta          SMALLINT,      -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess          SMALLINT,      -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart          LIKE ret_datamart.id_datamart,
       v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
       v_f_inicio_pension     DATE,          -- fecha de inicio de pension en el SPESS
       v_validacion_nss       SMALLINT,      -- Resultado de la validacion del NSS 0 = validacion exitosa, 1 = nss invalido
       v_validacion_marca     SMALLINT,      -- Resultado de la validacion de marca 0 = sin marca, 1 = con marca que impide el tramite
       v_validacion_solicitud SMALLINT,      -- Resultado de la validacion de solicitud previa
       v_validacion_spess     SMALLINT,      -- Resultado de la validacion del SPESS
       v_cod_ret              CHAR(2),       -- Codigo de rechazo regresado por la funcion
       v_desc_cod_ret         CHAR(100),     -- Descripcion del codigo de rechazo regresado por la funcion
       v_folio_ife            LIKE ret_ws_actualiza_datos_v_a.folioife,
       v_clave_ife            LIKE ret_ws_actualiza_datos_v_a.claveife,
       v_nombre               LIKE ret_ws_actualiza_datos_v_a.nombre_s,
       v_aPaterno             LIKE ret_ws_actualiza_datos_v_a.paterno_s,
       v_aMaterno             LIKE ret_ws_actualiza_datos_v_a.materno_s,
       v_desAfore             LIKE ret_ws_actualiza_datos_v_a.desafore,
       v_tipo_doc             LIKE ret_ws_actualiza_datos_v_a.tipodoc,
       v_e_sha1               LIKE ret_ws_actualiza_datos_v_a.e_sha1,
       v_cuenta_clabe         LIKE ret_ws_actualiza_datos_v_a.cuentaclabe,
       v_cuenta_bancaria      LIKE ret_ws_actualiza_datos_v_a.cuentabancaria,
       v_datos_bancarios      LIKE ret_ws_actualiza_datos_v_a.datosbancarios,
       v_importe_devolver     LIKE ret_ws_actualiza_datos_v_a.importedevolver,
       v_fecha_creacion       LIKE ret_ws_actualiza_datos_v_a.fechacreacion,
       v_lugar_emision        LIKE ret_ws_actualiza_datos_v_a.lugaremision,
       
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       v_regs_encontrados     SMALLINT, 
       p_grupo_ley73          SMALLINT,
       v_tiene_credito        SMALLINT,      -- booleana que indica si se tiene un credito vigente
       v_tipo_credito         SMALLINT       -- clave del tipo de credito

    --se inicia en blanco la respuesta
    LET ws_ret_re_imprime_out.codRet           = ""
    LET ws_ret_re_imprime_out.mensaje          = ""
    LET ws_ret_re_imprime_out.folio            = ""
    LET ws_ret_re_imprime_out.nss              = ""
    LET ws_ret_re_imprime_out.rfc              = ""
    LET ws_ret_re_imprime_out.curp             = ""
    LET ws_ret_re_imprime_out.folio_ife        = ""
    LET ws_ret_re_imprime_out.clave_ife        = ""
    LET ws_ret_re_imprime_out.nombre           = ""
    LET ws_ret_re_imprime_out.aPaterno         = ""
    LET ws_ret_re_imprime_out.aMaterno         = ""
    LET ws_ret_re_imprime_out.desAfore         = ""
    LET ws_ret_re_imprime_out.tipo_doc         = ""
    LET ws_ret_re_imprime_out.e_sha1           = ""
    LET ws_ret_re_imprime_out.cuenta_clabe     = ""
    LET ws_ret_re_imprime_out.cuenta_bancaria  = ""
    LET ws_ret_re_imprime_out.datos_bancarios  = ""
    LET ws_ret_re_imprime_out.importe_devolver = ""
    LET ws_ret_re_imprime_out.fecha_creacion   = ""
    LET ws_ret_re_imprime_out.lugar_emision    = ""
    LET v_folio_ife                            = ""
    LET v_clave_ife                            = ""
    LET v_nombre                               = ""
    LET v_aPaterno                             = ""
    LET v_aMaterno                             = ""
    LET v_desAfore                             = ""
    LET v_tipo_doc                             = ""
    LET v_e_sha1                               = ""
    LET v_cuenta_clabe                         = ""
    LET v_cuenta_bancaria                      = ""
    LET v_datos_bancarios                      = ""
    LET v_importe_devolver                     = ""
    LET v_fecha_creacion                       = ""
    LET v_lugar_emision                        = ""
    LET v_regs_encontrados                     = 0


    IF ws_ret_re_imprime_in.nss IS NOT NULL AND ws_ret_re_imprime_in.folio IS NOT NULL THEN 
        LET p_folio = ws_ret_re_imprime_in.folio
        LET p_nss   = ws_ret_re_imprime_in.nss
        SELECT COUNT(*)
        INTO   v_regs_encontrados
        FROM   ret_ws_actualiza_datos_v_a
        WHERE  nss = p_nss
        AND    folio = p_folio
        AND    id_solicitud IS NOT NULL
        AND    id_peticion = (SELECT MAX(id_peticion)
                              FROM   ret_ws_actualiza_datos_v_a
                              WHERE  nss = p_nss
                              AND    folio = p_folio
                              AND    id_solicitud IS NOT NULL)


        IF v_regs_encontrados = 1 THEN 
            -- se obtienen los datos de regreso  
            SELECT rfc_s, curp_s, folioife, claveife, 
                   nombre_s, paterno_s, materno_s, 
                   desafore, tipodoc, e_sha1, cuentaclabe, 
                   cuentabancaria, datosbancarios, 
                   importedevolver, fechacreacion, lugaremision
            INTO   ws_ret_re_imprime_out.rfc,
                   ws_ret_re_imprime_out.curp,
                   v_folio_ife,
                   v_clave_ife,
                   v_nombre,
                   v_aPaterno,
                   v_aMaterno,
                   v_desAfore,
                   v_tipo_doc,
                   v_e_sha1,
                   v_cuenta_clabe,
                   v_cuenta_bancaria,
                   v_datos_bancarios,
                   v_importe_devolver,
                   v_fecha_creacion,
                   v_lugar_emision
            FROM   ret_ws_actualiza_datos_v_a
            WHERE  nss = p_nss
            AND    folio = p_folio
            AND    id_solicitud IS NOT NULL
            AND    id_peticion = (SELECT MAX(id_peticion)
                                  FROM   ret_ws_actualiza_datos_v_a
                                  WHERE  nss = p_nss
                                  AND    folio = p_folio
                                  AND    id_solicitud IS NOT NULL)

            LET ws_ret_re_imprime_out.codRet           = "305"
            LET ws_ret_re_imprime_out.mensaje          = "Solicitud aceptada"
            LET ws_ret_re_imprime_out.folio            = p_folio
            LET ws_ret_re_imprime_out.nss              = p_nss
            LET ws_ret_re_imprime_out.folio_ife        = v_folio_ife
            LET ws_ret_re_imprime_out.clave_ife        = v_clave_ife
            LET ws_ret_re_imprime_out.nombre           = v_nombre
            LET ws_ret_re_imprime_out.aPaterno         = v_aPaterno
            LET ws_ret_re_imprime_out.aMaterno         = v_aMaterno
            LET ws_ret_re_imprime_out.desAfore         = v_desAfore
            LET ws_ret_re_imprime_out.tipo_doc         = v_tipo_doc
            LET ws_ret_re_imprime_out.e_sha1           = v_e_sha1
            LET ws_ret_re_imprime_out.cuenta_clabe     = v_cuenta_clabe
            LET ws_ret_re_imprime_out.cuenta_bancaria  = v_cuenta_bancaria
            LET ws_ret_re_imprime_out.datos_bancarios  = v_datos_bancarios
            LET ws_ret_re_imprime_out.importe_devolver = v_importe_devolver
            LET ws_ret_re_imprime_out.fecha_creacion   = v_fecha_creacion
            LET ws_ret_re_imprime_out.lugar_emision    = v_lugar_emision
        ELSE
            IF v_regs_encontrados > 1 THEN 
                LET ws_ret_re_imprime_out.codRet           = "-999"
                LET ws_ret_re_imprime_out.mensaje          = "Se encontraron multiples registros conincidentes"
            ELSE 
                LET ws_ret_re_imprime_out.codRet           = "-998"
                LET ws_ret_re_imprime_out.mensaje          = "No se encontraron registros para los parámetros proporcionados"
            END IF
        END IF
    ELSE
        LET ws_ret_re_imprime_out.codRet           = "-997"
        LET ws_ret_re_imprime_out.mensaje          = "Faltan parámetros, favor de proporcionarlos completos"
    END IF
    
   
END FUNCTION

FUNCTION fn_valida_saldo(p_nss)
DEFINE p_nss                CHAR(11)
DEFINE v_resultado          SMALLINT 
DEFINE v_aivs92             DECIMAL(22,2)
DEFINE v_aivs97             DECIMAL(22,2)
DEFINE v_tesofe             DECIMAL(22,2)
DEFINE v_saldo              DECIMAL(22,2)
DEFINE v_pesos              DECIMAL(22,2) 

    -- Inserta en ret_ley73_generico
    CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs92, v_pesos
    -- se obtiene el saldo de viv97
    CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs97, v_pesos
    -- se obtiene el saldo de aportaciones voluntarias
    CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_tesofe, v_pesos

    LET v_saldo =  v_aivs92 + v_aivs97 + v_tesofe
    LET v_resultado = 0
      
RETURN v_resultado, v_saldo
END FUNCTION 

