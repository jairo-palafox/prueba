--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW42                                                  #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO GENERICO VENTANILLA AFORE                        #
#FECHA INICIO      => 02-DIC-2013                                             #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_ret_generico_solicitud_in RECORD
         ind_beneficiario        smallint     ,
         entidad_federativa      smallint     ,
         nss                     char(11)     ,
         rfc                     char(13)     ,
         curp                    char(18)     ,
         clabe                   char(18)     ,
         gpo_trabajador          char(4)      ,
         sec_pension             char(2)      ,
         regimen                 char(2)      ,
         tpo_retiro              char(1)      ,
         tpo_seguro              char(2)      ,
         tpo_pension             char(2)      ,
         tpo_prestacion          char(2)      ,
         sem_cotizadas           smallint     ,
         nombre_pensionado       char(40)     ,
         ap_paterno_pensionado   char(40)     ,
         ap_materno_pensionado   char(40)     ,
         rfc_beneficiario        char(13)     ,
         curp_beneficiario       char(18)     ,
         cve1_siefore            char(2)      ,
         ret92_cve1_siefore      decimal(13,2),
         ret97_cve1_siefore      decimal(13,2),
         otros_cve1_siefore      decimal(13,2),
         imp_neto_cve1_siefore   decimal(13,0),
         cve2_siefore            char(2)      ,
         ret92_cve2_siefore      decimal(13,2),
         ret97_cve2_siefore      decimal(13,2),
         otros_cve2_siefore      decimal(13,2),
         imp_neto_cve2_siefore   decimal(13,0),
         imp_neto_total_siefores decimal(13,0),
         aivs_viv97              decimal(13,2),
         aivs_viv92              decimal(13,2),
         f_valor                 char(8)      ,
         pesos_viv92             decimal(13,2),
         pesos_viv97             decimal(13,2),
         otros_vivienda          decimal(13,0),
         imp_neto_dep_vivienda   decimal(13,2),
         imp_aplicados_afore     char(127)    ,
         f_pago                  char(8)      ,
         referencia_pago         char(20)     ,
         observaciones           char(127)    ,
         folio_notificacion      char(29)     ,
         folio_operacion         char(10)     ,
         aivs_viv92_aplicados    decimal(13,2),
         aivs_viv97_aplicados    decimal(13,2),
         viv92_aplicados         decimal(13,2),
         viv97_aplicados         decimal(13,2),
         estatus_vivienda        char(1)      ,
         diag_recepcion          char(3)      ,
         desc_diagnostico        char(20)     ,
         result_operacion        char(2)      ,
         det_resultado           char(200)    ,
         cve_afore               smallint     ,
         nom_trabajador          char(40)     ,
         ap_paterno_trabajador   char(40)     ,
         ap_materno_trabajador   char(40)     
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
         cod_retorno         SMALLINT, -- codigo de retorno
         desc_retorno        STRING    -- descripcion del codigo de retorno
       END RECORD
         
DEFINE g_indice_retiro  SMALLINT -- indice del tipo de retiro consultado
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
         
-- constantes para codigos de retorno
CONSTANT gi_solicitud_aceptada_afore            SMALLINT = 10 
CONSTANT gi_error_id_beneficiario_vacio                 SMALLINT = 201,
         gi_error_nss_vacio                             SMALLINT = 202,
         gi_error_nss_no_encontrado                     SMALLINT = 102,
         gi_error_rfc_vacio                             SMALLINT = 206,
         gi_error_rfc_invalido                          SMALLINT = 233,
         gi_error_curp_vacio                            SMALLINT = 207,
         gi_error_curp_invalido                         SMALLINT = 234,
         gi_error_clabe_vacio                           SMALLINT = 209,
         gi_error_clabe_estructura_invalida             SMALLINT = 237, -- longitud invalida
         gi_error_clabe_invalido                        SMALLINT = 258, -- algo esta mal en la clabe
         gi_error_gpo_trabajador_vacio                  SMALLINT = 210,
         gi_error_sec_pension_vacio                     SMALLINT = 211,
         gi_error_regimen_vacio                         SMALLINT = 212,
         gi_error_regimen_invalido                      SMALLINT = 239, -- cuando es diferente a 73
         gi_error_tpo_retiro_vacio                      SMALLINT = 213,
         gi_error_tpo_seguro_vacio                      SMALLINT = 214,
         gi_error_tpo_pension_vacio                     SMALLINT = 215,
         gi_error_tpo_prestacion_vacio                  SMALLINT = 216,
         gi_error_sem_cotizadas_vacio                   SMALLINT = 217,
         gi_error_nombre_pensionado_vacio               SMALLINT = 218,
         gi_error_ap_paterno_pensionado_vacio           SMALLINT = 204,
         gi_error_ap_pat_beneficiario_vacio             SMALLINT = 407,
         gi_error_ap_paterno_pensionado_invalido        SMALLINT = 470,
         gi_error_rfc_beneficiario_vacio                SMALLINT = 221,
         gi_error_rfc_beneficiario_invalido             SMALLINT = 240,
         gi_error_curp_beneficiario_vacio               SMALLINT = 241,
         gi_error_aivs_viv92_viv97_vacio                SMALLINT = 242,
         gi_error_aivs_viv97_vacio                      SMALLINT = 242,
         gi_error_fec_valor_vivienda_vacio              SMALLINT = 244,
         gi_error_fec_valor_vivienda_invalido           SMALLINT = 256,
         gi_error_pesos_viv92_vacio                     SMALLINT = 246,
         gi_error_pesos_viv92_viv97_vacio               SMALLINT = 246,
         gi_error_pesos_viv92_negativo                  SMALLINT = 260,
         gi_error_pesos_viv92_viv97_negativo            SMALLINT = 260,
         gi_error_pesos_viv97_vacio                     SMALLINT = 246,
         gi_error_pesos_viv97_negativo                  SMALLINT = 260,
         gi_error_imp_neto_vivienda_vacio               SMALLINT = -999 -- no viene codigo pero si que es obligatorio
{
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201
}         
         
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW42."
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
    DISPLAY "Iniciando servidor de WS de generacion de solicitud de retiro Ley 73 Ventanilla Afore..."

    -- se inicia el motor de WS
    CALL com.WebServiceEngine.Start()
    DISPLAY "Servidor en escucha..."

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
DEFINE v_webservice         com.WebService       # WebService
DEFINE op                   com.WebOperation     # Operation of a WebService
DEFINE v_service_NameSpace  STRING -- namespace del servicio
DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
DEFINE v_resultado          INTEGER

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://www.infonavit.org.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroGenericoSolicitudVAfore", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_generico_solicitud","fn_ret_generico_solicitud",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        CALL v_webservice.publishOperation(op, "fn_ret_generico_solicitud")

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
            -- Registro del servicio
            CALL com.WebServiceEngine.RegisterService(v_webservice)  
            --display_status("Retiro 72-92 Service registrado")
            CALL ERRORLOG("Se registro el servicio consulta de saldos disponibles para retiro")
        END IF

        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de saldos disponibles para retiro': " || STATUS)
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
Nombre: fn_ret_generico_solicitud
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_generico_solicitud()
DEFINE v_indice_retiro         SMALLINT,
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
       v_id_derechohabiente    DECIMAL(9,0),
       v_respuesta_marca       SMALLINT,
       v_id_solicitud          DECIMAL(9,0),
       v_folio                 DECIMAL(9,0),
       v_estado_marca          SMALLINT,
       v_codigo_rechazo        SMALLINT,--Variable de rechazo del store
       v_marca_causa           SMALLINT,
       v_fecha_causa           DATE,
       v_usuario               CHAR(20),
       v_proceso_cod           SMALLINT
       
    -- se verifica si se esta solicitando eco
    IF ( UPSHIFT(ws_ret_generico_solicitud_in.nss) = "ECO" ) THEN
        -- se devuelve ECO
        LET ws_ret_generico_solicitud_out.cod_retorno  = 0
        LET ws_ret_generico_solicitud_out.desc_retorno = "ECO"
        DISPLAY "Se recibio ECO"
    ELSE
        -- se crea el registro de la bitacora de la peticion ws registro de solicitud
        CALL fn_registra_peticion_registro_solicitud_va(1, NULL)
        RETURNING g_id_peticion

        -- se valida que los datos se hayan recibido
        CALL fn_valida_datos_entrada() RETURNING v_datos_validos, v_cod_retorno, v_desc_retorno

        -- si los datos son incorrectos
        IF ( NOT v_datos_validos ) THEN
            -- se rechaza la solicitud
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        -- se obtiene el NSS
        LET v_nss = ws_ret_generico_solicitud_in.nss

        -- se valida que los datos se hayan recibido
        CALL fn_valida_disponibilidad(v_nss) RETURNING v_disponibilidad_valida, v_cod_retorno, v_desc_retorno
        IF v_cod_retorno <> 99 THEN 
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        CALL fn_busca_derechohabiente(v_nss) RETURNING v_id_derechohabiente
        IF v_id_derechohabiente = 0 THEN 
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_nss_no_encontrado)
            RETURN
        END IF
        -- Guarda la Solicitud
        CALL fn_genera_solicitud_ret_ley73(v_nss, v_id_derechohabiente, gi_solicitud_aceptada_afore, 0,
                                     ws_ret_generico_solicitud_in.aivs_viv92, 
                                     ws_ret_generico_solicitud_in.aivs_viv97,
                                     ws_ret_generico_solicitud_in.pesos_viv92,
                                     ws_ret_generico_solicitud_in.pesos_viv97) RETURNING v_id_solicitud
        IF v_id_solicitud = 0 THEN 
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, -999)
            RETURN
        END IF 

        LET v_folio          = "0"
        LET v_estado_marca   = "0"
        LET v_codigo_rechazo = "0"
        LET v_marca_causa    = "0"
        LET v_fecha_causa    = NULL
        LET v_usuario        = "safreviv"
        LET v_proceso_cod    = g_proceso_cod_ret_ley73_ws

        -- Marca la cuenta
        CALL fn_marca_cuenta(v_id_derechohabiente, 815,v_id_solicitud,v_folio,v_estado_marca,
                           v_codigo_rechazo,v_marca_causa,v_fecha_causa,v_usuario,v_proceso_cod) RETURNING v_respuesta_marca
        IF v_respuesta_marca <> 0 THEN 
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_nss_no_encontrado)
            RETURN
        END IF

        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada_afore, v_cod_retorno)

    END IF 
   
END FUNCTION

{
======================================================================
Nombre: fn_valida_datos_entrada
Fecha creacion: Diciembre 04, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida los datos de entrada que se recibieron en la solicitud de generacion
de retiro por ventanilla afore. Utiliza el registro global y devuelve
una booleana indicando si los datos son correctos, ademas de un codigo de
retorno y su descripcion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_datos_entrada()
DEFINE v_datos_correctos SMALLINT,
       v_cod_retorno     SMALLINT,
       v_desc_retorno    STRING,
       ls_cadena         STRING -- cadena auxiliar para 

    -- se asume que los datos son correctos
    LET v_datos_correctos = TRUE
    LET v_cod_retorno     = 99 -- solicitud correcta
    LET v_desc_retorno    = "SOLICITUD ACEPTADA"


    -- se verifica cada campo
    -- Validacion ID 1 Indicador del Beneficiario Punto 1.51 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.ind_beneficiario) ) OR 
       ( ws_ret_generico_solicitud_in.ind_beneficiario <> 1 AND 
         ws_ret_generico_solicitud_in.ind_beneficiario <> 2 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_id_beneficiario_vacio  -- 201
        LET v_desc_retorno    = "Indicador de beneficiario obligatorio"
    END IF

    -- Validacion ID 3 NSS Punto 1.52 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.nss) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_nss_vacio              -- 202
        LET v_desc_retorno    = "NSS obligatorio"
    END IF

    ----  Validaciones que aplican solo para el Indicador de Beneficiario 2 Trabajador
    IF ws_ret_generico_solicitud_in.ind_beneficiario = 2 THEN 
        -- Validacion ID 4 RFC del Trabajador Punto 1.54 G. O.
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.rfc) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_rfc_vacio          -- 206
            LET v_desc_retorno    = "RFC obligatorio"

        ELSE
            LET ls_cadena = ws_ret_generico_solicitud_in.rfc CLIPPED
            -- el RFC debe tener 13 caracteres
            IF ( ls_cadena.getLength() <> 13 ) THEN
                LET v_datos_correctos = FALSE
                LET v_cod_retorno     = gi_error_rfc_invalido    -- 233
                LET v_desc_retorno    = "RFC menor a 13 caracteres"
            END IF
        END IF 
        -- Validacion ID 16 Apellido Paterno del Pensionado Punto 1.56 G. O.
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.ap_paterno_pensionado) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_ap_paterno_pensionado_vacio   -- 204
            LET v_desc_retorno    = "Apellido paterno pensionado obligatorio"
        END IF
        -- Validacion ID 5 CURP del trabajdor Punto 1.57 G. O. 
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.curp) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_curp_vacio                    -- 207
            LET v_desc_retorno    = "CURP obligatorio"
        ELSE
            -- Validacion ID 5 CURP del trabajdor Punto 1.58 G. O.
            LET ls_cadena = ws_ret_generico_solicitud_in.curp CLIPPED
            -- el curp debe tener 18 caracteres
            IF ( ls_cadena.getLength() <> 18 ) THEN
                LET v_datos_correctos = FALSE
                LET v_cod_retorno     = gi_error_curp_invalido              -- 234
                LET v_desc_retorno    = "CURP menor a 18 caracteres"
            END IF
        END IF
    END IF 
    -- Validacion ID 6 CLABE Punto 1.59 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.clabe) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_clabe_vacio                       -- 209
        LET v_desc_retorno    = "CLABE obligatorio"
    ELSE
        -- debe tener 18 caracteres
        -- Validacion ID 6 CLABE Punto 1.60 G. O.
        IF ( NOT fn_verifica_estructura_clabe(ws_ret_generico_solicitud_in.clabe) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_clabe_estructura_invalida      -- 237
            LET v_desc_retorno    = "CLABE menor a 18 caracteres"
            --ELSE
            -- se valida que la CLABE este correcta conforme a componentes

            --FALTA
        END IF
    END IF
    -- Validacion ID 7 Grupo del Trabajador Punto 1.62 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.gpo_trabajador) ) THEN
    LET v_datos_correctos = FALSE
    LET v_cod_retorno     = gi_error_gpo_trabajador_vacio             -- 210
    LET v_desc_retorno    = "Grupo Trabajador obligatorio"
    END IF
    -- Validacion ID 8 Secuencia de Pensión Punto 1.63 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.sec_pension) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_sec_pension_vacio                -- 211
        LET v_desc_retorno    = "Secuencia pensión obligatorio"
    END IF
    -- Validacion ID 9 Régimen Punto 1.64 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.regimen) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_regimen_vacio                    -- 212
        LET v_desc_retorno    = "Regimen obligatorio"
    ELSE
        -- debe ser 73
        -- Validacion ID 9 Régimen Punto 1.65 G. O.
        IF ( ws_ret_generico_solicitud_in.regimen <> "73" ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_regimen_invalido              -- 239
            LET v_desc_retorno    = "Regimen no es 73"
        END IF
    END IF
    -- Validacion ID 10 Tipo de Retiro Punto 1.66 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.tpo_retiro) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_retiro_vacio                 -- 213
        LET v_desc_retorno    = "Tipo de retiro obligatorio"
    END IF
    -- Validacion ID 11 Tipo de Seguro Punto 1.67 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.tpo_seguro) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_seguro_vacio                 -- 214
        LET v_desc_retorno    = "Tipo de seguro obligatorio"
    END IF
    -- Validacion ID 12 Tipo de Pensión Punto 1.68 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.tpo_pension) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_pension_vacio                -- 215
        LET v_desc_retorno    = "Tipo de pensión obligatorio"
    END IF
    -- Validacion ID 13 Tipo de Prestación Punto 1.69 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.tpo_prestacion) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_prestacion_vacio             -- 216
        LET v_desc_retorno    = "Tipo de prestación obligatorio"
    END IF
    -- Validacion ID 14 Semanas Cotizadas Punto 1.70 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.sem_cotizadas) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_sem_cotizadas_vacio              -- 217
        LET v_desc_retorno    = "Semanas cotizadas obligatorio"
    END IF
    ----  Validaciones que aplican solo para el Indicador de Beneficiario 1 Beneficiario
    IF ws_ret_generico_solicitud_in.ind_beneficiario = 1 THEN 
        -- Validacion ID 15 Nombre del Beneficiario Punto 1.71 G. O.
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.nombre_pensionado) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_nombre_pensionado_vacio     -- 218
            LET v_desc_retorno    = "Nombre de beneficiario obligatorio"
        END IF
        -- Validacion ID 16 Apellido Paterno del Beneficiario Punto 1.72 G. O.
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.ap_paterno_pensionado) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_ap_pat_beneficiario_vacio   -- 407
            LET v_desc_retorno    = "Apellido paterno pensionado obligatorio"
        END IF
        -- Validacion ID 18 RFC del Beneficiario Punto 1.73 G. O.
        IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.rfc_beneficiario) ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_rfc_beneficiario_vacio      -- 221
            LET v_desc_retorno    = "RFC beneficiario obligatorio"
        ELSE
            -- el rfc debe tener 13 caracteres
            -- Validacion ID 18 RFC del Beneficiario Punto 1.74 G. O.
            LET ls_cadena = ws_ret_generico_solicitud_in.rfc_beneficiario CLIPPED
            IF ( ls_cadena.getLength() <> 13 ) THEN
                LET v_datos_correctos = FALSE
                LET v_cod_retorno     = gi_error_rfc_beneficiario_invalido  -- 240
                LET v_desc_retorno    = "RFC del beneficiario menor a 13 caracteres"
            END IF
        END IF
        -- Validacion ID 19 CURP del Beneficiario Punto 1.75 G. O.
        LET ls_cadena = ws_ret_generico_solicitud_in.curp_beneficiario CLIPPED
        IF ( ls_cadena.getLength() <> 18 ) THEN
            LET v_datos_correctos = FALSE
            LET v_cod_retorno     = gi_error_curp_beneficiario_vacio        -- 241
            LET v_desc_retorno    = "CURP beneficiario menor a 18 posiciones"
        END IF
    END IF 
    -- Validacion ID 31 y ID 32 Mayores a cero cuando menos uno de los dos Punto 1.76 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.aivs_viv97) OR
         ws_ret_generico_solicitud_in.aivs_viv97 = 0) AND 
       ( fn_dato_vacio(ws_ret_generico_solicitud_in.aivs_viv92) OR 
         ws_ret_generico_solicitud_in.aivs_viv92 = 0) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_aivs_viv92_viv97_vacio  -- 242
        LET v_desc_retorno    = "Vivienda 92 AIVS y Vivienda 97 AIVS están con importe en ceros"
    END IF
    -- Validacion ID 33 Fecha Valor Vivienda Punto 1.77 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.f_valor) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_fec_valor_vivienda_vacio   -- 244
        LET v_desc_retorno    = "Fecha fecha valor vivienda"
    END IF
    -- Validacion ID 34 y ID 35 Mayores a cero cuando menos uno de los dos Punto 1.79 G. O.
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.pesos_viv92) OR 
         ws_ret_generico_solicitud_in.pesos_viv92 = 0) AND 
       ( fn_dato_vacio(ws_ret_generico_solicitud_in.pesos_viv97) OR 
         ws_ret_generico_solicitud_in.pesos_viv97 = 0)  THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_pesos_viv92_viv97_vacio    -- 246
        LET v_desc_retorno    = "Vivienda 97 MXN y Vivienda 92 MNX están con importe en cero"
    END IF 
    -- Validacion ID 34 y ID 35 no contengan valores negativos ninguno de ellos Punto 1.80 G. O.
    IF ( ws_ret_generico_solicitud_in.pesos_viv92 < 0 OR 
         ws_ret_generico_solicitud_in.pesos_viv97 < 0) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_pesos_viv92_viv97_negativo   -- 260
        LET v_desc_retorno    = "Vivienda 97 MXN y Vivienda 92 MXN están con importes negativos"
    END IF

    -- se devuelve el resultado de la validacion
    RETURN v_datos_correctos, v_cod_retorno, v_desc_retorno
END FUNCTION
{
======================================================================
Nombre: fn_valida_disponibilidad
Fecha creacion: Diciembre 04, 2013
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Vaida si previamente existe un registro con resultado exitoso para 
continuar con la captura de la solicitud
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_disponibilidad(p_nss)
DEFINE p_nss                   CHAR(11),
       v_disponibilidad_valida SMALLINT,
       v_cod_retorno           SMALLINT,
       v_desc_retorno          STRING,
       v_id_consulta           DECIMAL(9,0),
       v_mensaje               CHAR(100),
       ls_cadena               STRING -- cadena auxiliar para 

    -- se asume que los datos son correctos
    LET v_disponibilidad_valida = TRUE
    LET v_id_consulta           = 0
    LET v_cod_retorno           = 99 -- solicitud correcta
    LET v_desc_retorno          = "SOLICITUD ACEPTADA"

    SELECT id_consulta, cod_ret, mensaje
    INTO   v_id_consulta, v_cod_retorno, v_mensaje
    FROM   ret_ws_disponibilidad_v_a
    WHERE id_consulta IN (SELECT MAX(id_consulta)
                         FROM   ret_ws_disponibilidad_v_a
                         WHERE  nss = p_nss);

    IF v_id_consulta = 0 OR v_id_consulta IS NULL THEN 
        LET v_cod_retorno  = 102
        LET v_desc_retorno = "NSS no se encuentra registrado en la base de datos del INFONAVIT"
        LET v_disponibilidad_valida = FALSE 
    END IF 
    --RETURN TRUE,0,"CORRECTO"

    -- se devuelve el resultado de la validacion
    RETURN v_disponibilidad_valida, v_cod_retorno, v_desc_retorno
END FUNCTION

{
======================================================================
Nombre: fn_dato_vacio
Fecha creacion: Diciembre 04, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un dato esta vacio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_dato_vacio(p_dato)
DEFINE p_dato STRING

    -- se verifica si el dato esta vacio
    IF ( p_dato.trim() IS NULL ) THEN
        RETURN TRUE
    ELSE
        RETURN FALSE
    END IF
END FUNCTION
       

       

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la validacion de disponibilidad del retiro 
de ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_ley73(p_estado_solicitud, p_cod_rechazo)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         v_des_larga        LIKE ret_rechazo.des_larga, -- descripcion del codigo de rechazo
         p_subcuenta        SMALLINT, -- subcuenta de inversion
         p_importe_aivs     DECIMAL(24,6), -- monto en AIVs
         p_fecha_valuacion  DATE, -- fecha de valuacion
         v_valor_fondo      LIKE glo_valor_fondo.precio_fondo
            
    -- se genera el registro de disponibilidad
    IF ( p_estado_solicitud = gi_solicitud_aceptada_afore ) THEN
        -- solicitud aceptada
        LET ws_ret_generico_solicitud_out.cod_retorno  = 0
        LET ws_ret_generico_solicitud_out.desc_retorno = "Correcto"
    ELSE
        -- peticion rechazada
        -- se obtiene la descripcion del error
        SELECT des_larga
        INTO   v_des_larga
        FROM   ret_rechazo_generico
        WHERE  cod_rechazo = (p_cod_rechazo + 1000)
      
        -- si no se encuentra se verifica si es alguno de los que se tienen en las validaciones
        IF ( v_des_larga IS NULL ) THEN
            CASE p_cod_rechazo
                WHEN gi_error_id_beneficiario_vacio          
                    LET v_des_larga = "ID Beneficiario obligatorio"
                WHEN gi_error_nss_vacio                      
                    LET v_des_larga = "NSS obligatorio"
                WHEN gi_error_nss_no_encontrado              
                    LET v_des_larga = "NSS no encontrado"
                WHEN gi_error_rfc_vacio                      
                    LET v_des_larga = "RFC obligatorio"
                WHEN gi_error_rfc_invalido                   
                    LET v_des_larga = "RFC invalido"
                WHEN gi_error_curp_vacio                     
                    LET v_des_larga = "CURP obligatorio"
                WHEN gi_error_curp_invalido                  
                    LET v_des_larga = "CURP invalido"
                WHEN gi_error_clabe_vacio                    
                    LET v_des_larga = "CLABE obligatorio"
                WHEN gi_error_clabe_estructura_invalida      
                    LET v_des_larga = "CLABE estructura invalida"
                WHEN gi_error_clabe_invalido                 
                    LET v_des_larga = "CLABE invalida"
                WHEN gi_error_gpo_trabajador_vacio           
                    LET v_des_larga = "Grupo trabajador obligatorio"
                WHEN gi_error_sec_pension_vacio              
                    LET v_des_larga = "Secuencia pensión obligatorio"
                WHEN gi_error_regimen_vacio                  
                    LET v_des_larga = "Regimen obligatorio"
                WHEN gi_error_regimen_invalido               
                    LET v_des_larga = "Regimen invalido"
                WHEN gi_error_tpo_retiro_vacio               
                    LET v_des_larga = "Tipo retiro obligatorio"
                WHEN gi_error_tpo_seguro_vacio               
                    LET v_des_larga = "Tipo seguro obligatorio"
                WHEN gi_error_tpo_pension_vacio              
                    LET v_des_larga = "Tipo pensión obligatorio"
                WHEN gi_error_tpo_prestacion_vacio           
                    LET v_des_larga = "Tipo prestación obligatorio"
                WHEN gi_error_sem_cotizadas_vacio            
                    LET v_des_larga = "Semanas cotizadas obligatorio"
                WHEN gi_error_nombre_pensionado_vacio        
                    LET v_des_larga = "Nombre pensionado obligatorio"
                WHEN gi_error_ap_paterno_pensionado_vacio    
                    LET v_des_larga = "Apellido paterno pensionado obligatorio"
                WHEN gi_error_ap_paterno_pensionado_invalido 
                    LET v_des_larga = "Apellido materno pensionado obligatorio"
                WHEN gi_error_rfc_beneficiario_vacio         
                    LET v_des_larga = "RFC beneficiario obligatorio"
                WHEN gi_error_rfc_beneficiario_invalido      
                    LET v_des_larga = "RFC beneficiario invalido"
                WHEN gi_error_curp_beneficiario_vacio        
                    LET v_des_larga = "CURP beneficiario obligatorio"
                WHEN gi_error_aivs_viv92_viv97_vacio               
                    LET v_des_larga = "AIVs Viv92 obligatorio"
                WHEN gi_error_aivs_viv97_vacio               
                    LET v_des_larga = "AIVs Viv97 obligatorio"
                WHEN gi_error_fec_valor_vivienda_vacio       
                    LET v_des_larga = "Fecha valor obligatorio"
                WHEN gi_error_fec_valor_vivienda_invalido    
                    LET v_des_larga = "Fecha valor invalida"
                WHEN gi_error_pesos_viv92_vacio              
                    LET v_des_larga = "Pesos viv92 obligatorio"
                WHEN gi_error_pesos_viv92_negativo           
                    LET v_des_larga = "Pesos viv92 Negativo"
                WHEN gi_error_pesos_viv97_vacio              
                    LET v_des_larga = "Pesos Viv97 obligatorio"
                WHEN gi_error_pesos_viv97_negativo           
                    LET v_des_larga = "Pesos Viv97 negativo"
                WHEN gi_error_imp_neto_vivienda_vacio        
                    LET v_des_larga = "Importe neto vivienda obligatorio"
                OTHERWISE
                    LET v_des_larga = "Codigo no definido."
            END CASE
        END IF
      
        LET ws_ret_generico_solicitud_out.cod_retorno  = p_cod_rechazo
        LET ws_ret_generico_solicitud_out.desc_retorno = v_des_larga
    END IF

    -- se registra la respuesta de la peticion
    {
    CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, 3, p_subcuenta,
                                                         p_estado_solicitud, p_cod_rechazo, p_importe_aivs,
                                                         p_importe_aivs * v_valor_fondo)
    }

END FUNCTION




{
======================================================================
Nombre: fn_genera_solicitud_ret_ley73
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro Ley 73 de vivienda 92/97 para un
NSS dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, p_estado_solicitud, p_rechazo,
                                       p_aivs_viv92, p_aivs_viv97, p_pesos_viv92, p_pesos_viv97)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
       p_nss                 LIKE afi_derechohabiente.nss, 
       p_rfc                 LIKE afi_derechohabiente.rfc,
       p_estado_solicitud    SMALLINT      , -- estatus de la solicitud
       p_rechazo             SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_aivs_viv92          DECIMAL(24,6),
       p_pesos_viv92         DECIMAL(22,2),
       p_aivs_viv97          DECIMAL(24,6),
       p_pesos_viv97         DECIMAL(22,2),
       p_indice_modalidad    SMALLINT, -- indice de la modalidad del retiro
       v_estatus             SMALLINT      ,
       v_resultado           SMALLINT      , -- resultado de la ejecucion
       v_subcuenta           SMALLINT      , -- subcuenta de retiro
       v_id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
       v_marca_ley73         LIKE sfr_marca.marca, -- marca de amortizaciones excedentes
       v_saldo_poseido       LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       v_r_ret_ley73         RECORD LIKE ret_ley73_generico.*, -- registro de retiro de ley73
       v_r_ret_sol_generico  RECORD LIKE ret_solicitud_generico.*, -- reisgros de solicitud generico ley 73
       v_conteo              SMALLINT,
       v_tipo_pago           SMALLINT,
       v_total_aivs          DECIMAL(24,6), -- total de AIVs
       v_total_pesos         DECIMAL(22,2), -- total de pesos
       v_sql                 STRING -- cadena con enunciado SQL
             
    -- se asigna la marca
    LET v_marca_ley73 = 815 -- ley 73 Ventanilla Afore
    LET v_id_solicitud = 0    

    -- si la solicitud fue aceptada
    IF ( p_estado_solicitud = gi_solicitud_aceptada_afore ) THEN
        DISPLAY "Se crea la solicitud ..."      
        -- Se obtiene el numero de solicitud
        SELECT seq_ret_solicitud.NEXTVAL
        INTO   v_id_solicitud
        FROM   systables 
        WHERE  tabid = 1

        -- se asginan los datos al retistro de solicitud
        LET v_r_ret_ley73.id_solicitud         = v_id_solicitud
        LET v_r_ret_ley73.id_derechohabiente   = p_id_derechohabiente
        LET v_r_ret_ley73.folio                = 0
        LET v_r_ret_ley73.gpo_ley73            = 1
        LET v_r_ret_ley73.subgrupo             = 0
        LET v_r_ret_ley73.f_solicitud          = TODAY
        LET v_r_ret_ley73.f_valuacion          = TODAY
        LET v_r_ret_ley73.aivs_viv92           = p_aivs_viv92
        LET v_r_ret_ley73.aivs_viv97           = p_aivs_viv97
        LET v_r_ret_ley73.importe_viv92        = p_pesos_viv92
        LET v_r_ret_ley73.importe_viv97        = p_pesos_viv97
        LET v_r_ret_ley73.importe_viv97_anexo1 = 0
        LET v_r_ret_ley73.f_captura            = TODAY
        LET v_r_ret_ley73.h_captura            = CURRENT DAY TO SECOND -- CAMBIAR
        LET v_r_ret_ley73.usuario              = "safreviv"
        LET v_r_ret_ley73.estado_solicitud     = p_estado_solicitud
        LET v_r_ret_ley73.cod_rechazo          = p_rechazo
       
        -- se inserta el registro de solicitud en la tabla historica
        INSERT INTO ret_ley73_generico VALUES ( v_r_ret_ley73.* )

        LET v_r_ret_sol_generico.id_solicitud       = v_id_solicitud
        LET v_r_ret_sol_generico.id_derechohabiente = p_id_derechohabiente
        LET v_r_ret_sol_generico.nss                = p_nss
        LET v_r_ret_sol_generico.rfc                = ''
        LET v_r_ret_sol_generico.modalidad_retiro   = 3
        LET v_r_ret_sol_generico.folio              = 0
        LET v_r_ret_sol_generico.caso_adai          = 0
        LET v_r_ret_sol_generico.folio_afore        = 0
        LET v_r_ret_sol_generico.grupo_ventanilla   = gi_ventanilla_afore
        LET v_r_ret_sol_generico.f_solicitud        = TODAY 
        LET v_r_ret_sol_generico.h_solicitud        = CURRENT DAY TO SECOND 
        LET v_r_ret_sol_generico.estado_solicitud   = p_estado_solicitud
        LET v_r_ret_sol_generico.cod_rechazo        = p_rechazo

        INSERT INTO ret_solicitud_generico VALUES (v_r_ret_sol_generico.* )

        -- se actualiza el id de la solicitud a la peticion
        CALL fn_registra_peticion_registro_solicitud_va(2, v_id_solicitud)
           RETURNING g_id_peticion

        -- se calculan los montos totales
        LET v_total_aivs  = p_aivs_viv92 + p_aivs_viv97
        LET v_total_pesos = p_pesos_viv92 + p_pesos_viv97

        -- el pago es por SPEI
        LET v_tipo_pago = 1

        -- si el tipo de beneficiario es el beneficiario
        IF ( ws_ret_generico_solicitud_in.ind_beneficiario = 1 ) THEN
            CALL fn_registra_beneficiario_retiro_generico(v_id_solicitud,
                                                       ws_ret_generico_solicitud_in.ind_beneficiario,
                                                       v_tipo_pago, -- FALTA TIPO DE PAGO
                                                       1, -- FALTA PARENTESCO
                                                       ws_ret_generico_solicitud_in.ap_paterno_pensionado,
                                                       ws_ret_generico_solicitud_in.ap_materno_pensionado,
                                                       ws_ret_generico_solicitud_in.nombre_pensionado,
                                                       NULL,
                                                       NULL,
                                                       100,
                                                       v_total_aivs,
                                                       v_total_pesos,
                                                       ws_ret_generico_solicitud_in.clabe,
                                                       "",
                                                       ws_ret_generico_solicitud_in.entidad_federativa)
        ELSE
            -- el beneficiario es el trabajador
            CALL fn_registra_beneficiario_retiro_generico(v_id_solicitud,
                                                       ws_ret_generico_solicitud_in.ind_beneficiario,
                                                       v_tipo_pago, -- FALTA TIPO DE PAGO
                                                       1, -- FALTA PARENTESCO
                                                       ws_ret_generico_solicitud_in.ap_paterno_trabajador,
                                                       ws_ret_generico_solicitud_in.ap_materno_trabajador,
                                                       ws_ret_generico_solicitud_in.nom_trabajador,
                                                       NULL,
                                                       NULL,
                                                       100,
                                                       v_total_aivs,
                                                       v_total_pesos,
                                                       ws_ret_generico_solicitud_in.clabe,
                                                       "",
                                                       ws_ret_generico_solicitud_in.entidad_federativa)
        END IF
    END IF 
    RETURN v_id_solicitud
END FUNCTION 

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud_va
Fecha creacion: Diciembre 03, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para registro de solicitud de retiro de ventanilla
afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud_va(p_ind_operacion, p_id_solicitud)
DEFINE v_id_peticion                    DECIMAL(9,0), -- id de la peticion
       p_ind_operacion                  SMALLINT, -- 1 inserta, 2 actualiza
       p_id_solicitud                   DECIMAL(9,0), -- id de la solicitud generada
       v_r_ret_ws_sol_retiro_vent_afore RECORD LIKE ret_ws_sol_retiro_vent_afore.* -- registro de peticion al ws
		
    -- si se solicita crear la bitacora de la peticion
    IF ( p_ind_operacion = 1 ) THEN
        -- se obtiene el id de peticion nuevo
        SELECT seq_ret_ws_generico.nextVal
        INTO   v_id_peticion
        FROM   systables
        WHERE  tabid = 1
      
        -- se asignan los datos
        LET v_r_ret_ws_sol_retiro_vent_afore.id_peticion             = v_id_peticion
        LET v_r_ret_ws_sol_retiro_vent_afore.id_solicitud            = NULL
        LET v_r_ret_ws_sol_retiro_vent_afore.ind_beneficiario        = ws_ret_generico_solicitud_in.ind_beneficiario       
        LET v_r_ret_ws_sol_retiro_vent_afore.entidad_federativa      = ws_ret_generico_solicitud_in.entidad_federativa     
        LET v_r_ret_ws_sol_retiro_vent_afore.nss                     = ws_ret_generico_solicitud_in.nss                    
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc                     = ws_ret_generico_solicitud_in.rfc                    
        LET v_r_ret_ws_sol_retiro_vent_afore.curp                    = ws_ret_generico_solicitud_in.curp                   
        LET v_r_ret_ws_sol_retiro_vent_afore.clabe                   = ws_ret_generico_solicitud_in.clabe                  
        LET v_r_ret_ws_sol_retiro_vent_afore.gpo_trabajador          = ws_ret_generico_solicitud_in.gpo_trabajador         
        LET v_r_ret_ws_sol_retiro_vent_afore.sec_pension             = ws_ret_generico_solicitud_in.sec_pension            
        LET v_r_ret_ws_sol_retiro_vent_afore.regimen                 = ws_ret_generico_solicitud_in.regimen                
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_retiro              = ws_ret_generico_solicitud_in.tpo_retiro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_seguro              = ws_ret_generico_solicitud_in.tpo_seguro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_pension             = ws_ret_generico_solicitud_in.tpo_pension            
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_prestacion          = ws_ret_generico_solicitud_in.tpo_prestacion         
        LET v_r_ret_ws_sol_retiro_vent_afore.sem_cotizadas           = ws_ret_generico_solicitud_in.sem_cotizadas          
        LET v_r_ret_ws_sol_retiro_vent_afore.nombre_pensionado       = ws_ret_generico_solicitud_in.nombre_pensionado      
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_pensionado   = ws_ret_generico_solicitud_in.ap_paterno_pensionado  
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_pensionado   = ws_ret_generico_solicitud_in.ap_materno_pensionado  
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc_beneficiario        = ws_ret_generico_solicitud_in.rfc_beneficiario       
        LET v_r_ret_ws_sol_retiro_vent_afore.curp_beneficiario       = ws_ret_generico_solicitud_in.curp_beneficiario      
        LET v_r_ret_ws_sol_retiro_vent_afore.cve1_siefore            = ws_ret_generico_solicitud_in.cve1_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve1_siefore      = ws_ret_generico_solicitud_in.ret92_cve1_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve1_siefore      = ws_ret_generico_solicitud_in.ret97_cve1_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve1_siefore      = ws_ret_generico_solicitud_in.otros_cve1_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve1_siefore   = ws_ret_generico_solicitud_in.imp_neto_cve1_siefore  
        LET v_r_ret_ws_sol_retiro_vent_afore.cve2_siefore            = ws_ret_generico_solicitud_in.cve2_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve2_siefore      = ws_ret_generico_solicitud_in.ret92_cve2_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve2_siefore      = ws_ret_generico_solicitud_in.ret97_cve2_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve2_siefore      = ws_ret_generico_solicitud_in.otros_cve2_siefore     
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve2_siefore   = ws_ret_generico_solicitud_in.imp_neto_cve2_siefore  
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_total_siefores = ws_ret_generico_solicitud_in.imp_neto_total_siefores
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97              = ws_ret_generico_solicitud_in.aivs_viv97             
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92              = ws_ret_generico_solicitud_in.aivs_viv92             
        LET v_r_ret_ws_sol_retiro_vent_afore.f_valor                 = ws_ret_generico_solicitud_in.f_valor                
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv92             = ws_ret_generico_solicitud_in.pesos_viv92            
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv97             = ws_ret_generico_solicitud_in.pesos_viv97            
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_vivienda          = ws_ret_generico_solicitud_in.otros_vivienda         
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_dep_vivienda   = ws_ret_generico_solicitud_in.imp_neto_dep_vivienda  
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_aplicados_afore     = ws_ret_generico_solicitud_in.imp_aplicados_afore    
        LET v_r_ret_ws_sol_retiro_vent_afore.f_pago                  = ws_ret_generico_solicitud_in.f_pago                 
        LET v_r_ret_ws_sol_retiro_vent_afore.referencia_pago         = ws_ret_generico_solicitud_in.referencia_pago        
        LET v_r_ret_ws_sol_retiro_vent_afore.observaciones           = ws_ret_generico_solicitud_in.observaciones          
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_notificacion      = ws_ret_generico_solicitud_in.folio_notificacion     
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_operacion         = ws_ret_generico_solicitud_in.folio_operacion        
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92_aplicados    = ws_ret_generico_solicitud_in.aivs_viv92_aplicados   
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97_aplicados    = ws_ret_generico_solicitud_in.aivs_viv97_aplicados   
        LET v_r_ret_ws_sol_retiro_vent_afore.viv92_aplicados         = ws_ret_generico_solicitud_in.viv92_aplicados        
        LET v_r_ret_ws_sol_retiro_vent_afore.viv97_aplicados         = ws_ret_generico_solicitud_in.viv97_aplicados        
        LET v_r_ret_ws_sol_retiro_vent_afore.estatus_vivienda        = ws_ret_generico_solicitud_in.estatus_vivienda       
        LET v_r_ret_ws_sol_retiro_vent_afore.diag_recepcion          = ws_ret_generico_solicitud_in.diag_recepcion         
        LET v_r_ret_ws_sol_retiro_vent_afore.desc_diagnostico        = ws_ret_generico_solicitud_in.desc_diagnostico       
        LET v_r_ret_ws_sol_retiro_vent_afore.result_operacion        = ws_ret_generico_solicitud_in.result_operacion       
        LET v_r_ret_ws_sol_retiro_vent_afore.det_resultado           = ws_ret_generico_solicitud_in.det_resultado          
        LET v_r_ret_ws_sol_retiro_vent_afore.cve_afore               = ws_ret_generico_solicitud_in.cve_afore              
        LET v_r_ret_ws_sol_retiro_vent_afore.nom_trabajador          = ws_ret_generico_solicitud_in.nom_trabajador         
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_trabajador   = ws_ret_generico_solicitud_in.ap_paterno_trabajador  
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_trabajador   = ws_ret_generico_solicitud_in.ap_materno_trabajador  
      
        -- se inserta el registro de peticion
        INSERT INTO ret_ws_sol_retiro_vent_afore VALUES ( v_r_ret_ws_sol_retiro_vent_afore.* )
    ELSE
        -- se actualiza el id_solicitud en la peticion
        UPDATE ret_ws_sol_retiro_vent_afore
        SET    id_solicitud = p_id_solicitud
        WHERE  id_peticion  = g_id_peticion

        -- se devuelve la misma id de peticion
        LET v_id_peticion = g_id_peticion
    END IF

    -- se devuelve el id de la peticion
    RETURN v_id_peticion
END FUNCTION

{
======================================================================
Nombre: fn_registra_det_peticion_registro_solicitud
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra el detalle de los datos enviados como encabezado de modalidad de retiro
en el servicio de registro de solicitud de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud(p_id_peticion, p_modalidad_retiro, p_causal_retiro,
                                                     p_nrp, p_f_inicio_pension, p_grupo_ley73, p_num_credito)
DEFINE p_id_peticion                          DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro                     SMALLINT, -- modalidad de retiro
       p_causal_retiro                        SMALLINT, -- causal de retiro en fondo72
       p_nrp                                  CHAR(18), -- NRP del trabajador
       p_f_inicio_pension                     CHAR(8), -- fecha de inicio de pension
       p_grupo_ley73                          SMALLINT, -- grupo de ley73
       p_num_credito                          CHAR(20), -- numero de credito para Amort Excedentes
       v_r_ret_ws_det_peticion_crea_solicitud RECORD LIKE ret_ws_det_peticion_crea_solicitud.* -- registro de detalle de peticion al ws

    -- se asignan los datos
    LET v_r_ret_ws_det_peticion_crea_solicitud.id_peticion      = p_id_peticion
    LET v_r_ret_ws_det_peticion_crea_solicitud.modalidad_retiro = p_modalidad_retiro
    LET v_r_ret_ws_det_peticion_crea_solicitud.causal_retiro    = p_causal_retiro
    LET v_r_ret_ws_det_peticion_crea_solicitud.nrp              = p_nrp
    LET v_r_ret_ws_det_peticion_crea_solicitud.f_inicio_pension = p_f_inicio_pension
    LET v_r_ret_ws_det_peticion_crea_solicitud.grupo_ley73      = p_grupo_ley73
    LET v_r_ret_ws_det_peticion_crea_solicitud.num_credito      = p_num_credito
         
    -- se inserta el registro de detalle de peticion
    INSERT INTO ret_ws_det_peticion_crea_solicitud VALUES ( v_r_ret_ws_det_peticion_crea_solicitud.* )
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_det_peticion_registro_solicitud_resp
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra la respuesta enviada al cliente de una modalidad de retiro
recibida como solicitud de registro de solicitud de retiro generico

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud_resp(p_id_peticion, p_modalidad_retiro, p_subcuenta,
                                                          p_estado_solicitud, p_cod_rechazo, p_monto_aivs,
                                                          p_monto_pesos)
DEFINE p_id_peticion       DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro  SMALLINT, -- modalidad de retiro
       p_subcuenta         SMALLINT, -- subcuenta de inversion
       p_estado_solicitud  SMALLINT, -- estado de la solicitud
       p_cod_rechazo       SMALLINT, -- codigo de rechazo
       p_monto_aivs        DECIMAL(22,6), -- saldo en AIVs
       p_monto_pesos       DECIMAL(22,2), -- saldo en pesos equivalente a AIVs por valor accion
       v_r_ret_ws_det_peticion_crea_solicitud_resp RECORD LIKE ret_ws_det_peticion_crea_solicitud_resp.* -- registro de respuesta detalle de peticion al ws

    -- se asignan los datos
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.id_peticion            = p_id_peticion
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.modalidad_retiro       = p_modalidad_retiro
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_subcuenta         = p_subcuenta
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_estado_solicitud  = p_estado_solicitud
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_cod_rechazo       = p_cod_rechazo
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_avis        = p_monto_aivs
    LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_pesos       = p_monto_pesos
         
    -- se inserta el registro de detalle de peticion
    INSERT INTO ret_ws_det_peticion_crea_solicitud_resp VALUES ( v_r_ret_ws_det_peticion_crea_solicitud_resp.* )
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud_benef
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de un beneficiario dado de alta en una solicitud de retiro generico
por modalidad de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud_benef(p_id_peticion, p_modalidad_retiro, p_consec_benef, p_tipo_beneficiario,
                                                       p_clabe_bancaria, p_rfc, p_email, p_telefono, p_tel_movil,
                                                       p_nombre, p_ap_paterno, p_ap_materno, p_entidad_federativa)
DEFINE p_id_peticion        DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro   SMALLINT, -- modalidad de retiro
       p_consec_benef       SMALLINT, -- consecutivo de beneficiario
       p_tipo_beneficiario  SMALLINT, -- tipo de beneficiario
       p_clabe_bancaria     CHAR(18), -- CLABE de transferencia interbancaria
       p_rfc                char(13), -- RFC del beneficiario
       p_email              CHAR(50), -- email
       p_telefono           CHAR(10), -- telefono fijo
       p_tel_movil          CHAR(10), -- telefono movil
       p_nombre             char(40), -- nombre de pila
       p_ap_paterno         char(40), -- apellido paterno
       p_ap_materno         char(40), -- apellido materno
       p_entidad_federativa CHAR(2), -- entidad federativa
       v_r_ret_ws_peticion_crea_sol_benef RECORD LIKE ret_ws_peticion_crea_sol_benef.* -- registro de respuesta detalle de peticion al ws

    -- se asignan los datos
    LET v_r_ret_ws_peticion_crea_sol_benef.id_peticion        = p_id_peticion
    LET v_r_ret_ws_peticion_crea_sol_benef.modalidad_retiro   = p_modalidad_retiro  
    LET v_r_ret_ws_peticion_crea_sol_benef.consec_benef       = p_consec_benef      
    LET v_r_ret_ws_peticion_crea_sol_benef.tipo_beneficiario  = p_tipo_beneficiario 
    LET v_r_ret_ws_peticion_crea_sol_benef.clabe_bancaria     = p_clabe_bancaria    
    LET v_r_ret_ws_peticion_crea_sol_benef.rfc                = p_rfc               
    LET v_r_ret_ws_peticion_crea_sol_benef.email              = p_email             
    LET v_r_ret_ws_peticion_crea_sol_benef.telefono           = p_telefono          
    LET v_r_ret_ws_peticion_crea_sol_benef.tel_movil          = p_tel_movil         
    LET v_r_ret_ws_peticion_crea_sol_benef.nombre             = p_nombre            
    LET v_r_ret_ws_peticion_crea_sol_benef.ap_paterno         = p_ap_paterno        
    LET v_r_ret_ws_peticion_crea_sol_benef.ap_materno         = p_ap_materno        
    LET v_r_ret_ws_peticion_crea_sol_benef.entidad_federativa = p_entidad_federativa
         
    -- se inserta el registro de detalle de beneficiarios
    INSERT INTO ret_ws_peticion_crea_sol_benef VALUES ( v_r_ret_ws_peticion_crea_sol_benef.* )
   
END FUNCTION

FUNCTION fn_marca_cuenta(p_id_derechohabiente,p_marca_entra,p_num_referencia,p_folio,p_estado_marca,
                         p_codigo_rechazo,p_marca_causa,p_fecha_causa,p_usuario,p_proceso_cod)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, 
       p_marca_entra        LIKE sfr_marca.marca,
       p_num_referencia     LIKE ret_solicitud_generico.id_solicitud,
       p_folio              LIKE glo_folio.folio, 
       p_estado_marca       SMALLINT,
       p_codigo_rechazo     SMALLINT,
       p_marca_causa        LIKE sfr_marca.marca,
       p_fecha_causa        DATE,
       p_usuario            LIKE seg_usuario.usuario_cod,
       p_proceso_cod        LIKE cat_proceso.proceso_cod,
       v_sql                STRING, -- cadena con instruccion sql
       v_respuesta_marcaje  SMALLINT -- resultado de la desmarca

    -- se prepara la ejecucion de la desmarca
    LET v_sql = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
                             
    -- se prepara y ejecuta la funcion de marcaje    		                 
    PREPARE stm_marcaje FROM v_sql
    EXECUTE stm_marcaje USING p_id_derechohabiente,
                         p_marca_entra       ,
                         p_num_referencia    ,
                         p_folio             ,
                         p_estado_marca      ,
                         p_codigo_rechazo    ,
                         p_marca_causa       ,
                         p_fecha_causa       ,
                         p_usuario           ,
                         p_proceso_cod 
                   INTO v_respuesta_marcaje

    -- en caso de error se muestra que paso
    IF ( v_respuesta_marcaje <> 0 ) THEN
        DISPLAY "ERROR AL MARCAR: ", v_respuesta_marcaje
    END IF

    -- se devuelve el resultado de la ejecucion
    RETURN v_respuesta_marcaje
END FUNCTION 

{
======================================================================
Nombre: fn_busca_derechohabiente
Fecha creacion: Diciembre 04, 2013
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Busca el Id_derechohabiente en base al NSS 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_derechohabiente(p_nss)
DEFINE p_nss                   CHAR(11),
       v_id_derechohabiente    DECIMAL(9,0),
       v_cod_retorno           SMALLINT,
       v_desc_retorno          STRING,
       v_id_consulta           DECIMAL(9,0),
       v_mensaje               CHAR(100),
       ls_cadena               STRING -- cadena auxiliar para 

    -- se asume que los datos son correctos
    LET v_id_derechohabiente    = 0

    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = p_nss;

    IF v_id_derechohabiente IS NULL OR v_id_derechohabiente = 0 THEN 
        LET v_id_derechohabiente = 0
    END IF 

    -- se devuelve el id_derechohabiente encontrado
    RETURN v_id_derechohabiente
END FUNCTION