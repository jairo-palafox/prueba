################################################################################
#PROYECTO        => VENTANILLA UNICA                                           #
#PROPIETARIO     => OMNISYS                                                    #
#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS42                                                    #
#OBJETIVO        => WS PARA ORQUESTAR LLAMADOS A SERVICIOS PARA MARCAR CUENTAS #
#                => DE FA, LEY73 Y SOLO INFONAVIT                              #
#FECHA CREACION  => 27-AGOSTO-2020                                             #
#VERSION         => 1.0.0                                                      #
#MODIFICACION    =>                                                            #
################################################################################

IMPORT FGL WSHelper
IMPORT com
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "../RETG01.4gl"
GLOBALS "ret_ws_marca_fa.inc"
GLOBALS "ret_ws_marca_ley73.inc"
GLOBALS "ret_ws_marca_si.inc"

GLOBALS
    -- registro de entrada para el servicio
    DEFINE gr_entrada_ws             RECORD
           nss                       LIKE afi_derechohabiente.nss, -- nss del trabajador / REQUERIDO
           RFC                       LIKE afi_derechohabiente.rfc, -- rfc del trabajador / REQUERIDO
           caso_crm                  CHAR(10), -- caso de CRM 10 caracteres / OPCIONAL
           cuenta_clabe              CHAR(18), -- cuenta CLABE /REQUERIDO segun valor de indicador de marca
           ind_marca                 SMALLINT, -- indicador de Marca / OPCIONAL
           cod_rechazo               SMALLINT, -- codigo de rechazo que acompana al marcaje / OPCIONAL 
           medio_entrega             SMALLINT, -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros / REQUERIDO
           usuario                   STRING    -- Usuario que realiza el trámite / OPCIONAL
           END RECORD

    -- registro de respuesta del servicio
    DEFINE gr_salida_ws              RECORD
           nss                       like afi_derechohabiente.nss, -- Numero de seguridad social del trabajador
           estatus_marca             like afi_derechohabiente.rfc,    -- RFC del trabajador
           caso_crm                  CHAR(10), -- caso del CRM
           cod_rechazo               SMALLINT, -- codigo de rechazo     
           des_rechazo               STRING,   -- descripcion del rechazo
           -----------
           id_retiro_fa              LIKE ret_fondo_ahorro.id_solicitud, -- ID solicitrud fondo ahorro
           saldo_pesos_fa            LIKE ret_fondo_ahorro.saldo_viv72, -- saldo en pesos
           tanto_adicional           LIKE ret_fondo_ahorro.tanto_adicional, -- tanto adicional
           -----------
           id_retiro_ley73           LIKE ret_ley73.id_solicitud , -- ID solicitud ley73 
           saldo_aivs_viv92          LIKE ret_ley73.aivs_viv92   , -- saldo en AIVs viv92
           saldo_pesos_viv92         LIKE ret_ley73.importe_viv92, -- saldo en pesos viv92
           saldo_aivs_viv97          LIKE ret_ley73.aivs_viv97   , -- saldo en AIVs viv97
           saldo_pesos_viv97         LIKE ret_ley73.importe_viv97  -- saldo en pesos viv97
           END RECORD
         
    DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado
    -- estatus de marcaje
    CONSTANT GI_ESTATUS_MARCA_EXITO SMALLINT = 1,
             GI_ESTATUS_MARCA_ERROR SMALLINT = 2
             
    -- indicadores de marca
    CONSTANT GI_INDICADOR_MARCA_MARCAR             SMALLINT = 1,
             GI_INDICADOR_MARCA_DESMARCAR          SMALLINT = 2,
             GI_INDICADOR_MARCA_APROBAR_SOLICITUD  SMALLINT = 3,
             GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD SMALLINT = 4

    -- codigos de rechazo del servicio
    CONSTANT GI_COD_RECHAZO_MARCA_EXITO               SMALLINT = 1,
             GI_COD_RECHAZO_MARCA_ERROR               SMALLINT = 2,
             GI_COD_RECHAZO_DATOS_INCOMPLETOS         SMALLINT = 99,
             GI_COD_RECHAZO_CAUSAL_INVALIDA           SMALLINT = 101,
             GI_COD_RECHAZO_INDICADOR_INVALIDO        SMALLINT = 102,
             GI_COD_RECHAZO_ERROR_AL_MARCAR           SMALLINT = 103,
             GI_COD_RECHAZO_ERROR_WS_MARCA            SMALLINT = 104,
             GI_COD_RECHAZO_ERROR_AL_DESMARCAR        SMALLINT = 105,
             GI_COD_RECHAZO_ERROR_INTERNO_MARCA       SMALLINT = 301,
             GI_COD_RECHAZO_MARCA_NO_CONVIVE          SMALLINT = 601,
             GI_COD_RECHAZO_ERROR_GENERICO            SMALLINT = 999

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
             
    DEFINE serverURL     STRING -- URL del servidor
    DEFINE v_pantalla    SMALLINT
    DEFINE gs_desc_error_auxiliar STRING -- cadena de error auxiliar

END GLOBALS

#==============================================================================#
# MAIN                                                                         #
#==============================================================================#
MAIN
DEFINE v_resultado       INTEGER -- recibe el resultado de la ejecucion del servicio 
DEFINE v_ruta_log        STRING
DEFINE v_cadena          STRING
DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
    
    DEFER INTERRUPT

    -- se obtiene la ruta ejecutable
    SELECT ruta_listados
    INTO   v_ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_listados CLIPPED, "/RETWS42."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
    -- se inicia el log del programa
    IF FGL_GETENV("RETWS42LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS42LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS42LOG")
    ELSE
       DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
       CALL STARTLOG(v_ruta_log)
    END IF 

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_ws_marcaje_ventanilla_unica(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN
        ELSE
            IF num_args() <> 0 THEN
                CALL exitHelp()
                EXIT PROGRAM
            END IF
        END IF
    END IF
  
    CALL ERRORLOG("Invoca creacion de servicio de marcaje para Ventanilla Unica")
    
    --se crea el servicio
    CALL fn_crea_ws_marcaje_ventanilla_unica(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Marcaje Ventanilla Unica 1.0 ...")

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
Nombre      : fn_crea_ws_marcaje_ventanilla_unica
Creacion    : agosto -2020
Autor       : Isai Jimenez Rojas -  Omnisys
Objetivo    : Genera el servicio web para llamado de servicios de disponibilidad 
            : para ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_crea_ws_marcaje_ventanilla_unica(p_generar_WSDL)
DEFINE v_webservice         com.WebService       # WebService
DEFINE op                   com.WebOperation     # Operation of a WebService
DEFINE v_service_NameSpace  STRING -- namespace del servicio
DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
DEFINE v_resultado          INTEGER
DEFINE v_urn                STRING -- URN

    DISPLAY "Entra Funcion principal que crea el servicio de marcaje...."  --debug

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("marcaCuentaVentanillaUnica", v_service_NameSpace)
        CALL v_webservice.setFeature("Soap1.1",TRUE)
        DISPLAY "Servicio creado...."
        
        -- =============================
        -- Publicacion de las funciones

        -- declaracion de funcion principal del servicio
        LET op = com.WebOperation.CreateDOCStyle("fn_marca_cuenta_ventanilla_unica","fn_marca_cuenta_ventanilla_unica",gr_entrada_ws, gr_salida_ws)
        --se publica funcion principal del servicio
        CALL v_webservice.publishOperation(op, "fn_marca_cuenta_ventanilla_unica")

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
            --display_status("Retiro Disponibilidad Ventanilla Unica Service registrado")
            CALL ERRORLOG("Se registro el servicio de Marcaje para Ventanilla Unica")
            DISPLAY "Se registro el servicio de Marcaje para Ventanilla Unica"
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Marca cuenta para Ventanilla Unica': " || STATUS)
            EXIT PROGRAM
    END TRY

END FUNCTION

{
======================================================================
Clave: 
Nombre: exitHelp
Fecha creacion: Octubre 07, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Asigna los valores correspondientes al registro de respuesta del ws
de consulta de disponibilidad de ventanilla unica con base en el
parametro p_codigo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
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
Nombre: fn_marca_cuenta_ventanilla_unica
Fecha creacion: Octubre 06, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que ejecuta el proceso de marcaje/desmarcaje de ventanilla unica

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    octubre 14, 2020      cuando se desmarca ley73, el ws de marcaje de este tipo de retiro
                                   invoca un ws de consulta de saldos en ventanilla afore. Si ese servicio no
                                   se puede consumir, se regresa un valor generico "835" que despues es buscado
                                   en un catalogo de errores y que se asocia con la leyenda
                                   "RETIRO POR DISPOSICION DE RECURSOS", ese valor esta definido
                                   en la constante global gi_103_0
                                   Se agrega una clausula para verificar si marcaje de ley73 esta devolviendo
                                   este valor y si es el caso, se envia la respuesta de errores de comunicacion
                                   en ley73
Ivan Vega    Noviembre 17, 2020    Cuando haya un error de comunicacion o de ejecucion de marca segun el indicador y que no se trate de marca
                                   no se debe actualizar el estatus de la tabla de control y se debe devolver el registro al estatus anterior, es decir
                                   si es marca y falla, se desmarca lo ya marcado
                                   si es desmarca y falla, se marca lo desmarcado
                                   si es aprobacion y falla, se devuelve el estatus a pendiente de aprobacion
                                   si es rechazo y falla, se devuelve el estatus a pendiente de aprobacion
                                   
                                   Hay unas solicitudes que nacen en estatus aprobadas, por lo que si cuando se recibe la senal de
                                   aprobacion o rechazo, estas ya estan por encima del estatus 10, y el tipo de retiro estaba disponibles
                                   esta ya no se intentara aprobador o rechazar
Ivan Vega    Noviembre 23, 2020    En la respuesta, sea correcta o erronea, no se debe enviar campos nulos. Las cifras si no estan disponibles,
                                   se enviaran ceros
======================================================================
}
FUNCTION fn_marca_cuenta_ventanilla_unica()
DEFINE v_nss                    LIKE afi_fondo72.nss
DEFINE v_medio_entrega          SMALLINT
DEFINE v_estatus_validacion     SMALLINT
DEFINE v_continuar              SMALLINT
DEFINE lr_ret_control_vu        RECORD LIKE ret_control_vu.*
DEFINE lr_ret_marcaje_fa        tfn_marcaje_faResponse_fn_marcaje_faResponse
DEFINE lr_ret_marcaje_ley73     tfn_marcaje_ley73Response_fn_marcaje_ley73Response
DEFINE lr_ret_marcaje_si        tfn_marcaje_SIResponse_fn_marcaje_SIResponse
DEFINE v_estatus_llamada_fa     SMALLINT
DEFINE v_estatus_llamada_ley73  SMALLINT
DEFINE v_estatus_llamada_SI     SMALLINT
DEFINE v_marcaje_fa_correcto    SMALLINT
DEFINE v_marcaje_ley73_correcto SMALLINT
DEFINE v_marcaje_si_correcto    SMALLINT
DEFINE v_activar_marca_fa       SMALLINT
DEFINE v_activar_marca_ley73    SMALLINT
DEFINE v_activar_marca_si       SMALLINT
DEFINE v_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente
DEFINE v_estatus_solicitud_fa     SMALLINT
DEFINE v_estatus_solicitud_ley73  SMALLINT
DEFINE v_estatus_solicitud_SI     SMALLINT

    DISPLAY "Ingresa a funcion principal que resuelve el servicio..." 
    LET gr_salida_ws.nss = gr_entrada_ws.nss
    
    LET v_nss           = gr_entrada_ws.nss
    LET v_medio_entrega = gr_entrada_ws.medio_entrega
    
    DISPLAY "Parametros recibidos:"
    DISPLAY "====================="
    DISPLAY "NSS               :", gr_entrada_ws.nss           ,":"
    DISPLAY "RFC               :", gr_entrada_ws.RFC           ,":"
    DISPLAY "Caso CRM          :", gr_entrada_ws.caso_crm      ,":"
    DISPLAY "Cuenta Clabe      :", gr_entrada_ws.cuenta_clabe  ,":"
    DISPLAY "Indicador de marca:", gr_entrada_ws.ind_marca     ,":"
    DISPLAY "Codigo Rechazo    :", gr_entrada_ws.cod_rechazo   ,":"
    DISPLAY "Medio de Entrega  :", gr_entrada_ws.medio_entrega ,":"
    DISPLAY "Usuario           :", gr_entrada_ws.usuario       ,":"

    -- se inicia el registro de salida
    LET gr_salida_ws.id_retiro_fa           = 0
    LET gr_salida_ws.saldo_pesos_fa         = 0
    LET gr_salida_ws.tanto_adicional        = 0
    LET gr_salida_ws.id_retiro_ley73        = 0
    LET gr_salida_ws.saldo_aivs_viv92       = 0
    LET gr_salida_ws.saldo_pesos_viv92      = 0
    LET gr_salida_ws.saldo_aivs_viv97       = 0
    LET gr_salida_ws.saldo_pesos_viv97      = 0

    -- se validan los parametros de entrada
    LET v_estatus_validacion = fn_valida_parametros_entrada()
    IF ( v_estatus_validacion = 0 ) THEN
        -- los parametros de entrada son correctos, se procesa la solicitud
        -- se valida que exista una consulta de disponibilidad favorable para el nss y el 
        -- medio de entrega recibido
        CALL fn_obtiene_solicitud_vu(gr_entrada_ws.nss, gr_entrada_ws.medio_entrega)
        RETURNING lr_ret_control_vu.*

        DISPLAY "ID solicitud control vu: ", lr_ret_control_vu.consecutivo

        -- si no se encontro la solicitud
        IF ( lr_ret_control_vu.consecutivo IS NULL ) THEN
            LET gs_desc_error_auxiliar = "No se encontro una consulta de disponibilidad con el NSS y el medio de entrega dados"
            
            CALL fn_genera_respuesta_ws(GI_COD_RECHAZO_ERROR_GENERICO, GI_ESTATUS_MARCA_ERROR, "")
            
        ELSE       
            -- se verifica si se tiene disponibilidad de algun retiro
            IF ( lr_ret_control_vu.bn_disponibilidad_fa = FALSE AND lr_ret_control_vu.bn_disponibilidad_ley73 = FALSE AND lr_ret_control_vu.bn_disponibilidad_si = FALSE) THEN
                -- no se tiene disponibilidad alguna, se rechaza
                LET gs_desc_error_auxiliar = "No se tiene disponibilidad de solicitar ningún retiro"
                CALL fn_genera_respuesta_ws(GI_COD_RECHAZO_ERROR_GENERICO, GI_ESTATUS_MARCA_ERROR, "")            
            ELSE
                -- se intenta realizar el marcado de cada uno de los retiros disponibles
                -- se asume exito
                LET v_estatus_llamada_fa    = 0
                LET v_estatus_llamada_ley73 = 0
                LET v_estatus_llamada_SI    = 0

                LET v_marcaje_fa_correcto    = TRUE
                LET v_marcaje_ley73_correcto = TRUE
                LET v_marcaje_si_correcto    = TRUE

                LET v_activar_marca_fa       = FALSE
                LET v_activar_marca_ley73    = FALSE
                LET v_activar_marca_si       = FALSE

                -- se asume que el flujo completo se realizara
                LET v_continuar = TRUE
                
                -- se obtiene el id_derechohabiente del nss
                SELECT id_derechohabiente
                INTO   v_id_derechohabiente
                FROM   afi_derechohabiente
                WHERE  nss = lr_ret_control_vu.param_nss
                
                -- MARCA DE RETIRO DE FONDO DE AHORRO
                IF ( lr_ret_control_vu.bn_disponibilidad_fa = TRUE ) THEN
                    DISPLAY "invocando marcaje FA"
                    
                    -- si se pide marca/desmarca
                    IF ( gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_MARCAR OR gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_DESMARCAR ) THEN
                        CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                        RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                    ELSE
                        -- 20201117 si la solicitud ya esta aprobada o rechazada, no se intenta operar
                        SELECT estado_solicitud
                        INTO v_estatus_solicitud_fa
                        FROM ret_solicitud_generico
                        WHERE id_solicitud = lr_ret_control_vu.id_solicitud_fa
                        
                        IF ( v_estatus_solicitud_fa IS NOT NULL AND v_estatus_solicitud_fa = 10 ) THEN
                            CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                            RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                        ELSE
                            -- la solicitud ya estaba aprobada
                            DISPLAY "Se detecto solicitud FA ya aprobada/rechazada o procesada con antelacion"                            
                            LET v_estatus_llamada_fa = 0
                            LET lr_ret_marcaje_fa.est_marca   = GI_COD_RECHAZO_MARCA_EXITO
                            LET lr_ret_marcaje_fa.cod_rechazo = 0
                            LET lr_ret_marcaje_fa.des_rechazo = "Solicitud aprobada/rechazada con antelacion"
                            LET lr_ret_marcaje_fa.saldo_pesos = lr_ret_control_vu.saldo_pesos_fa
                            LET lr_ret_marcaje_fa.nss = lr_ret_control_vu.param_nss
                            LET lr_ret_marcaje_fa.rfc = lr_ret_control_vu.param_rfc
                        END IF
                    END IF

                    DISPLAY "Estatus invocacion FA: ", v_estatus_llamada_fa
                    DISPLAY "Estatus marca FA: ", lr_ret_marcaje_fa.est_marca
                    DISPLAY "Cod Rechazo FA: ", lr_ret_marcaje_fa.cod_rechazo
                    DISPLAY "DESC Rechazo FA: ", lr_ret_marcaje_fa.des_rechazo 
                    -- se valida si hubo error en la llamada al ws
                    IF ( v_estatus_llamada_fa <> 0 ) THEN
                        LET lr_ret_marcaje_fa.est_marca = GI_ESTATUS_MARCA_ERROR
                        LET lr_ret_marcaje_fa.cod_rechazo = "Error en llamada a WS de Marcaje de Fondo de Ahorro"
                        LET gr_salida_ws.id_retiro_fa    = 0
                        LET gr_salida_ws.saldo_pesos_fa  = 0
                        LET gr_salida_ws.tanto_adicional = 0
                        LET gr_salida_ws.estatus_marca   = lr_ret_marcaje_fa.est_marca
                        
                        -- CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, FALSE, FALSE, FALSE)
                        LET v_continuar = FALSE
                    ELSE
                        LET gr_salida_ws.id_retiro_fa    = 0
                        LET gr_salida_ws.saldo_pesos_fa  = lr_ret_marcaje_fa.saldo_pesos
                        LET gr_salida_ws.tanto_adicional = lr_ret_control_vu.tanto_adicional_fa
                        LET gr_salida_ws.estatus_marca   = lr_ret_marcaje_fa.est_marca

                        -- si no se pudo ejecutar la operacion
                        IF ( lr_ret_marcaje_fa.cod_rechazo <> 0 ) THEN
                            LET v_continuar = FALSE
                            LET v_marcaje_fa_correcto = FALSE
                            -- CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, FALSE, FALSE, FALSE)
                        ELSE
                            LET v_marcaje_fa_correcto = TRUE
                            LET v_activar_marca_fa    = TRUE
                            -- se actualiza el estatus de marcado en la tabla de control
                            CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si)
                            CALL fn_actualiza_ids_solicitud_retiro(v_id_derechohabiente, lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si, gr_entrada_ws.ind_marca)
                        END IF
                    END IF
                END IF
                
                IF ( v_continuar ) THEN
                    -- MARCA DE RETIRO LEY73
                    IF ( lr_ret_control_vu.bn_disponibilidad_ley73 = TRUE ) THEN
                        DISPLAY "Invocando marcaje Ley 73"

                        -- si se pide marca/desmarca
                        IF ( gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_MARCAR OR gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_DESMARCAR ) THEN
                            CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                  gr_entrada_ws.caso_crm, -- el parametro es el caso adai, es el mismo?
                                                  gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo, 
                                                  lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                  gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                        ELSE
                            -- 20201117 si la solicitud ya esta aprobada o rechazada, no se intenta operar
                            SELECT estado_solicitud
                            INTO v_estatus_solicitud_ley73
                            FROM ret_solicitud_generico
                            WHERE id_solicitud = lr_ret_control_vu.id_solicitud_ley73
                            
                            IF ( v_estatus_solicitud_ley73 IS NOT NULL AND v_estatus_solicitud_ley73 = 10 ) THEN
                                CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                      gr_entrada_ws.caso_crm, -- el parametro es el caso adai, es el mismo?
                                                      gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo, 
                                                      lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                      gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                            ELSE
                                -- la solicitud ya estaba aprobada
                                DISPLAY "Se detecto solicitud Ley73 ya aprobada/rechazada o procesada con antelacion"                            
                                LET v_estatus_llamada_ley73 = 0
                                LET lr_ret_marcaje_ley73.est_marca   = GI_COD_RECHAZO_MARCA_EXITO
                                LET lr_ret_marcaje_ley73.cod_rechazo = 0
                                LET lr_ret_marcaje_ley73.des_rechazo = "Solicitud aprobada/rechazada con antelacion"
                                LET lr_ret_marcaje_ley73.saldo_aivs_viv92 = lr_ret_control_vu.saldo_acciones_viv92
                                LET lr_ret_marcaje_ley73.saldo_aivs_viv97 = lr_ret_control_vu.saldo_acciones_viv97
                                LET lr_ret_marcaje_ley73.saldo_pesos_viv92 = lr_ret_control_vu.saldo_pesos_viv92
                                LET lr_ret_marcaje_ley73.saldo_pesos_viv97 = lr_ret_control_vu.saldo_pesos_viv97
                                LET lr_ret_marcaje_ley73.nss = lr_ret_control_vu.param_nss
                            END IF
                        END IF

                        -- si hubo un error de comunicacion con el servicio
                        IF ( v_estatus_llamada_ley73 <> 0 ) THEN
                            LET v_continuar = FALSE
                            LET lr_ret_marcaje_ley73.est_marca = GI_ESTATUS_MARCA_ERROR
                            LET lr_ret_marcaje_ley73.des_rechazo = "Error en llamada a WS de Marcaje de Ley 73"
                            
                            LET gr_salida_ws.saldo_aivs_viv92   = 0
                            LET gr_salida_ws.saldo_aivs_viv97   = 0
                            LET gr_salida_ws.saldo_pesos_viv92  = 0
                            LET gr_salida_ws.saldo_pesos_viv97  = 0
                            LET gr_salida_ws.id_retiro_ley73    = 0
                            -- si hubo marcaje de fondo de ahorro, se desmarca
                            IF ( lr_ret_control_vu.bn_disponibilidad_fa = TRUE AND v_marcaje_fa_correcto ) THEN
                                DISPLAY "Se reversa el movimiento"
                                -- se regresa el estatus anterior
                                -- 20201117
                                CASE gr_entrada_ws.ind_marca
                                   WHEN GI_INDICADOR_MARCA_MARCAR
                                       -- se desmarca fondo de ahorro
                                       DISPLAY "Se desmcarca FA"
                                       CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                       RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                   
                                   WHEN GI_INDICADOR_MARCA_DESMARCAR
                                       DISPLAY "Se vuelve a marcar FA"
                                       -- se vuelve a marcar
                                       CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                       RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                   
                                   WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                       DISPLAY "Se regresa FA a pendiente de aprobar"
                                       -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                       CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                   
                                   WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                       DISPLAY "Se regresa FA a pendiente de aprobar"
                                       -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                       CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                END CASE
                            END IF
                            -- CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, FALSE, FALSE, FALSE)

                        ELSE
                            -- si la marca no procedio
                            -- 20201014 se verifica si el codigo de rechazo coincide con gi_103_0. Si es el caso, hubo error con la consulta
                            --          de saldo en ventanilla afore. Hay otro proceso que se encarga de reenviar esas desmarcas. Se continua el proceso
                            IF ( lr_ret_marcaje_ley73.cod_rechazo <> 0 AND lr_ret_marcaje_ley73.cod_rechazo <> gi_103_0 ) THEN
                                DISPLAY "Marcaje en Ley 73 no se dio"
                                DISPLAY lr_ret_marcaje_ley73.cod_rechazo
                                DISPLAY lr_ret_marcaje_ley73.des_rechazo
                                LET gr_salida_ws.saldo_aivs_viv92   = 0
                                LET gr_salida_ws.saldo_aivs_viv97   = 0
                                LET gr_salida_ws.saldo_pesos_viv92  = 0
                                LET gr_salida_ws.saldo_pesos_viv97  = 0
                                LET gr_salida_ws.id_retiro_ley73    = 0

                                -- el proceso no continua
                                LET v_continuar = 0
                                LET v_marcaje_ley73_correcto = FALSE

                                -- si hubo marcaje de fondo de ahorro, se desmarca
                                IF ( lr_ret_control_vu.bn_disponibilidad_fa = TRUE AND v_marcaje_fa_correcto = TRUE ) THEN
                                    -- se regresa el estatus anterior
                                    -- 20201117
                                    CASE gr_entrada_ws.ind_marca
                                       WHEN GI_INDICADOR_MARCA_MARCAR
                                           -- se desmarca fondo de ahorro
                                           DISPLAY "Se desmcarca FA"
                                           CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                           RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                       
                                       WHEN GI_INDICADOR_MARCA_DESMARCAR
                                           DISPLAY "Se vuelve a marcar FA"
                                           -- se vuelve a marcar
                                           CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                           RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                       
                                       WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                           DISPLAY "Se regresa FA a pendiente de aprobar"
                                           -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                           CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                       
                                       WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                           DISPLAY "Se regresa FA a pendiente de aprobar"
                                           -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                           CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                    END CASE
                                END IF
                                
                            ELSE
                                DISPLAY "Marcaje Ley73 correcto"
                                DISPLAY "AIVS viv92: ", lr_ret_marcaje_ley73.saldo_aivs_viv92
                                DISPLAY "PESOS viv92: ", lr_ret_marcaje_ley73.saldo_pesos_viv92
                                DISPLAY "AIVS viv97: ", lr_ret_marcaje_ley73.saldo_aivs_viv97
                                DISPLAY "PESOS viv97: ", lr_ret_marcaje_ley73.saldo_pesos_viv97
                                -- el marcado fue exitoso
                                LET v_marcaje_ley73_correcto = TRUE
                                LET v_activar_marca_ley73 = TRUE

                                -- se toman los datos de la respuesta
                                LET gr_salida_ws.saldo_aivs_viv92  = lr_ret_marcaje_ley73.saldo_aivs_viv92
                                LET gr_salida_ws.saldo_aivs_viv97  = lr_ret_marcaje_ley73.saldo_aivs_viv97
                                LET gr_salida_ws.saldo_pesos_viv92 = lr_ret_marcaje_ley73.saldo_pesos_viv92
                                LET gr_salida_ws.saldo_pesos_viv97 = lr_ret_marcaje_ley73.saldo_pesos_viv97

                                CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si)
                                CALL fn_actualiza_ids_solicitud_retiro(v_id_derechohabiente, lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si, gr_entrada_ws.ind_marca)
                            END IF
                        END IF
                    END IF
                    
                    IF ( v_continuar ) THEN
                        -- MARCA DE RETIRO SOLO INFONAVIT
                        IF ( lr_ret_control_vu.bn_disponibilidad_si = TRUE ) THEN
                            DISPLAY "Invocando marcaje retiro solo infonavit"

                            -- si se pide marca/desmarca
                            IF ( gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_MARCAR OR gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_DESMARCAR ) THEN
                                CALL fn_marcaje_SI(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, 
                                                   gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo,
                                                   lr_ret_control_vu.param_grupo_ley73, gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario)
                                RETURNING v_estatus_llamada_SI, lr_ret_marcaje_si.*
                            ELSE
                                -- 20201117 si la solicitud ya esta aprobada o rechazada, no se intenta operar
                                SELECT estado_solicitud
                                INTO v_estatus_solicitud_si
                                FROM ret_solicitud_generico
                                WHERE id_solicitud = lr_ret_control_vu.id_solicitud_si
                                
                                IF ( v_estatus_solicitud_si IS NOT NULL AND v_estatus_solicitud_si = 10 ) THEN
                                    CALL fn_marcaje_SI(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, 
                                                       gr_entrada_ws.cuenta_clabe, gr_entrada_ws.ind_marca, gr_entrada_ws.cod_rechazo,
                                                       lr_ret_control_vu.param_grupo_ley73, gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario)
                                    RETURNING v_estatus_llamada_SI, lr_ret_marcaje_si.*
                                ELSE
                                    -- la solicitud ya estaba aprobada
                                    DISPLAY "Se detecto solicitud SI ya aprobada/rechazada o procesada con antelacion"                            
                                    LET v_estatus_llamada_SI = 0
                                    LET lr_ret_marcaje_si.est_marca   = GI_COD_RECHAZO_MARCA_EXITO
                                    LET lr_ret_marcaje_si.cod_rechazo = 0
                                    LET lr_ret_marcaje_si.des_rechazo = "Solicitud aprobada/rechazada con antelacion"
                                    LET lr_ret_marcaje_si.caso_crm = lr_ret_control_vu.caso_crm
                                    LET lr_ret_marcaje_si.saldo_aivs = lr_ret_control_vu.saldo_acciones_si
                                    LET lr_ret_marcaje_si.monto_pesos = lr_ret_control_vu.saldo_pesos_si
                                    LET lr_ret_marcaje_si.nss = lr_ret_control_vu.param_nss
                                END IF
                            END IF

                            -- si hubo un error de comunicacion con el servicio
                            IF ( v_estatus_llamada_SI <> 0 ) THEN
                                DISPLAY "Error de comunicaciones con servicio de marca SI"
                                LET v_continuar = FALSE
                                LET lr_ret_marcaje_si.est_marca = GI_ESTATUS_MARCA_ERROR
                                LET lr_ret_marcaje_si.des_rechazo = "Error en llamada a WS de Marcaje de Retiro Solo Infonavit"
                            
                                -- si hubo marcaje de fondo de ahorro, se desmarca
                                IF ( lr_ret_control_vu.bn_disponibilidad_fa = TRUE AND v_marcaje_fa_correcto ) THEN
                                    -- se regresa el estatus anterior
                                    -- 20201117
                                    CASE gr_entrada_ws.ind_marca
                                       WHEN GI_INDICADOR_MARCA_MARCAR
                                           -- se desmarca fondo de ahorro
                                           DISPLAY "Se desmcarca FA"
                                           CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                           RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                       
                                       WHEN GI_INDICADOR_MARCA_DESMARCAR
                                           DISPLAY "Se vuelve a marcar FA"
                                           -- se vuelve a marcar
                                           CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                           RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                       
                                       WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                           DISPLAY "Se regresa FA a pendiente de aprobar"
                                           -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                           CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                       
                                       WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                           DISPLAY "Se regresa FA a pendiente de aprobar"
                                           -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                           CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)                                       
                                    END CASE
                                END IF
                                
                                -- si hubo marcaje de retiro ley73, se desmarca
                                IF ( lr_ret_control_vu.bn_disponibilidad_ley73 = TRUE AND v_marcaje_ley73_correcto ) THEN
                                    -- se regresa el estatus anterior
                                    -- 20201117
                                    CASE gr_entrada_ws.ind_marca
                                       WHEN GI_INDICADOR_MARCA_MARCAR
                                           -- se desmarca fondo de ahorro
                                           DISPLAY "Se desmcarca Ley73"
                                           CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                              gr_entrada_ws.caso_crm, -- el parametro es el caso adai
                                                              gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, 
                                                              lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                              gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                                       
                                       WHEN GI_INDICADOR_MARCA_DESMARCAR
                                           DISPLAY "Se vuelve a marcar Ley73"
                                           -- se vuelve a marcar
                                           CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                              gr_entrada_ws.caso_crm, -- el parametro es el caso adai
                                                              gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, 
                                                              lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                              gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                                       
                                       WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                           DISPLAY "Se regresa Ley73 a pendiente de aprobar"
                                           -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                           CALL fn_reversar_aprobacion_rechazo(0,lr_ret_control_vu.id_solicitud_ley73,0)
                                       
                                       WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                           DISPLAY "Se regresa Ley73 a pendiente de aprobar"
                                           -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                           CALL fn_reversar_aprobacion_rechazo(0,lr_ret_control_vu.id_solicitud_ley73,0)                                       
                                    END CASE

                                    -- CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, FALSE, FALSE, FALSE)
                                END IF
                                
                                
                                -- se marca error y no se debe continuar
                                LET v_marcaje_si_correcto = FALSE
                                LET v_continuar = FALSE
                            ELSE
                                -- si la marca no procedio
                                IF ( lr_ret_marcaje_si.cod_rechazo <> 0 ) THEN
                                    DISPLAY "Marcaje en SoloInf no se dio"
                                    DISPLAY lr_ret_marcaje_si.cod_rechazo
                                    DISPLAY lr_ret_marcaje_si.des_rechazo
                                    -- el proceso no continua
                                    LET v_continuar = 0
                                    LET v_marcaje_si_correcto = FALSE
                            
                                    -- si hubo marcaje de fondo de ahorro, se desmarca
                                    IF ( lr_ret_control_vu.bn_disponibilidad_fa = TRUE AND v_marcaje_fa_correcto ) THEN
                                        -- se regresa el estatus anterior
                                        -- 20201117
                                        CASE gr_entrada_ws.ind_marca
                                           WHEN GI_INDICADOR_MARCA_MARCAR
                                               -- se desmarca fondo de ahorro
                                               DISPLAY "Se desmcarca FA"
                                               CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                               RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                           
                                           WHEN GI_INDICADOR_MARCA_DESMARCAR
                                               DISPLAY "Se vuelve a marcar FA"
                                               -- se vuelve a marcar
                                               CALL fn_marcaje_fa(gr_entrada_ws.nss, gr_entrada_ws.RFC, gr_entrada_ws.caso_crm, gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, gr_entrada_ws.medio_entrega)
                                               RETURNING v_estatus_llamada_fa, lr_ret_marcaje_fa.*
                                           
                                           WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                               DISPLAY "Se regresa FA a pendiente de aprobar"
                                               -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                               CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                           
                                           WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                               DISPLAY "Se regresa FA a pendiente de aprobar"
                                               -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                               CALL fn_reversar_aprobacion_rechazo(lr_ret_control_vu.id_solicitud_fa,0,0)
                                        END CASE
                                    END IF
                                    
                                    -- si hubo marcaje de retiro ley73, se desmarca
                                    IF ( lr_ret_control_vu.bn_disponibilidad_ley73 = TRUE AND v_marcaje_ley73_correcto ) THEN
                                        -- se regresa el estatus anterior
                                        -- 20201117
                                        CASE gr_entrada_ws.ind_marca
                                           WHEN GI_INDICADOR_MARCA_MARCAR
                                               -- se desmarca fondo de ahorro
                                               DISPLAY "Se desmcarca Ley73"
                                               CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                                  gr_entrada_ws.caso_crm, -- el parametro es el caso adai
                                                                  gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_DESMARCAR, gr_entrada_ws.cod_rechazo, 
                                                                  lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                                  gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                                           
                                           WHEN GI_INDICADOR_MARCA_DESMARCAR
                                               DISPLAY "Se vuelve a marcar Ley73"
                                               -- se vuelve a marcar
                                               CALL fn_marcaje_ley73(gr_entrada_ws.nss, 
                                                                  gr_entrada_ws.caso_crm, -- el parametro es el caso adai
                                                                  gr_entrada_ws.cuenta_clabe, GI_INDICADOR_MARCA_MARCAR, gr_entrada_ws.cod_rechazo, 
                                                                  lr_ret_control_vu.param_grupo_ley73, -- no viene en el registro de entrada, se saca de control vu
                                                                  gr_entrada_ws.medio_entrega, gr_entrada_ws.usuario) RETURNING v_estatus_llamada_ley73, lr_ret_marcaje_ley73.*
                                           
                                           WHEN GI_INDICADOR_MARCA_APROBAR_SOLICITUD
                                               DISPLAY "Se regresa Ley73 a pendiente de aprobar"
                                               -- no se pudo aprobar, se regresa el estatus a pendiente de aprobacion
                                               CALL fn_reversar_aprobacion_rechazo(0,lr_ret_control_vu.id_solicitud_ley73,0)
                                           
                                           WHEN GI_INDICADOR_MARCA_RECHAZAR_SOLICITUD
                                               DISPLAY "Se regresa Ley73 a pendiente de aprobar"
                                               -- no se pudo rechazar, se regresa el estatus a pendiente de aprobar
                                               CALL fn_reversar_aprobacion_rechazo(0,lr_ret_control_vu.id_solicitud_ley73,0)                                       
                                        END CASE
                                    
                                        -- CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, FALSE, FALSE, FALSE)
                                    END IF
                                ELSE
                                    DISPLAY "Marcaje SoloInf Correcto"
                                    DISPLAY "AIVS SINF: ", lr_ret_marcaje_si.saldo_aivs
                                    DISPLAY "PESOS SINF: ", lr_ret_marcaje_si.monto_pesos
                                    -- el marcado fue exitoso
                                    LET v_marcaje_si_correcto = TRUE
                                    LET v_activar_marca_si = TRUE
                            
                                    -- se toman los datos de la respuesta
                                    LET gr_salida_ws.saldo_aivs_viv97  = gr_salida_ws.saldo_aivs_viv97  + lr_ret_marcaje_si.saldo_aivs
                                    LET gr_salida_ws.saldo_pesos_viv97 = gr_salida_ws.saldo_pesos_viv97 + lr_ret_marcaje_si.monto_pesos
                            
                                    CALL fn_actualiza_control_vu_marcas(lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si)
                                    CALL fn_actualiza_ids_solicitud_retiro(v_id_derechohabiente, lr_ret_control_vu.consecutivo, v_activar_marca_fa, v_activar_marca_ley73, v_activar_marca_si, gr_entrada_ws.ind_marca)
                                END IF
                            END IF 
                        END IF
                        
                        -- si todas las llamadas a los servicios fueron correctas (los servicios respondieron)
                        IF ( v_continuar ) THEN
                            -- todas las llamadas fueron correctas, el marcaje se hizo exitosamente
                            CALL fn_genera_respuesta_ws(0, GI_ESTATUS_MARCA_EXITO, "")
                        ELSE
                            IF ( v_estatus_llamada_SI <> 0) THEN
                                LET gs_desc_error_auxiliar = gs_desc_error_auxiliar, "Ocurrió un error al invocar los WS de marcaje de Solo Infonavit"
                            END IF
                            CALL fn_genera_respuesta_ws(GI_COD_RECHAZO_ERROR_GENERICO, GI_ESTATUS_MARCA_ERROR, "")
                        END IF
                    ELSE
                        -- ocurrio un error en la marca de retiro ley73
                        IF ( v_estatus_llamada_ley73 <> 0 ) THEN
                            LET gs_desc_error_auxiliar = gs_desc_error_auxiliar, "Ocurrió un error al invocar los WS de marcaje de Ley73"
                            CALL fn_genera_respuesta_ws(GI_COD_RECHAZO_ERROR_GENERICO, GI_ESTATUS_MARCA_ERROR, "")
                        ELSE
                            CALL fn_genera_respuesta_ws(lr_ret_marcaje_ley73.cod_rechazo, GI_ESTATUS_MARCA_ERROR, lr_ret_marcaje_ley73.des_rechazo)
                        END IF
                        
                    END IF
                ELSE
                    -- ocurrio un error en la marca de fondo de ahorro
                    -- alguno de los servicios no respondio correctamente, se notifica al cliente
                    IF ( v_estatus_llamada_fa <> 0) THEN
                        LET gs_desc_error_auxiliar = gs_desc_error_auxiliar, "Ocurrió un error al invocar los WS de marcaje de Fondo de Ahorro"
                        CALL fn_genera_respuesta_ws(GI_COD_RECHAZO_ERROR_GENERICO, GI_ESTATUS_MARCA_ERROR, "")
                    ELSE
                        CALL fn_genera_respuesta_ws(lr_ret_marcaje_fa.cod_rechazo, GI_ESTATUS_MARCA_ERROR, lr_ret_marcaje_fa.des_rechazo)
                    END IF
                END IF

            END IF
        END IF
        
    ELSE
        -- errores en parametros de entrada
        CALL fn_genera_respuesta_ws(v_estatus_validacion, GI_ESTATUS_MARCA_ERROR, "")
    END IF   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_reversar_aprobacion_rechazo
Fecha creacion: Noviembre 17, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que regresa el estatus de una solicitud a 10 (creada) cuando
la aprobacion o el rechazo no funcionaron por problemas de comunciacion o de
marcaje en los servicios delegados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_reversar_aprobacion_rechazo(p_id_solicitud_fa, p_id_solicitud_ley73, p_id_solicitud_si)
DEFINE p_id_solicitud_fa    SMALLINT,
       p_id_solicitud_ley73 SMALLINT,
       p_id_solicitud_si    SMALLINT
   
   -- se verifica si se tiene que regresar FA
   IF ( p_id_solicitud_fa <> 0 ) THEN
      UPDATE ret_solicitud_generico
      SET estado_solicitud = 10
      WHERE id_solicitud = p_id_solicitud_fa
      
      UPDATE ret_fondo_ahorro_generico
      SET estado_solicitud = 10
      WHERE id_solicitud = p_id_solicitud_fa
   END IF
   
   -- se verifica si se tiene que regresar Ley73
   IF ( p_id_solicitud_ley73 <> 0 ) THEN
      UPDATE ret_solicitud_generico
      SET estado_solicitud = 10
      WHERE id_solicitud = p_id_solicitud_ley73
      
      UPDATE ret_ley73_generico
      SET estado_solicitud = 10
      WHERE id_solicitud = p_id_solicitud_ley73
   
   END IF
   
   -- se verifica si se tiene que regresar SI
   IF ( p_id_solicitud_si <> 0 ) THEN
      UPDATE ret_solicitud_generico
      SET estado_solicitud = 10
      WHERE id_solicitud = p_id_solicitud_si
      
      -- ret_solo_infonavit no se cambia de estatus porque no requiere aprobacion,
      -- nace ya aprobada en estatus 10
   END IF

END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_obtiene_solicitud_vu
Fecha creacion: Octubre 07, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que obtiene el ultimo registro de consulta disponibilidad de retiro
ventanilla unica por nss y medio de entrega

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtiene_solicitud_vu(p_nss, p_medio_entrega)
DEFINE p_nss             LIKE afi_derechohabiente.nss
DEFINE p_medio_entrega   SMALLINT
DEFINE lr_ret_control_vu RECORD LIKE ret_control_vu.*

    INITIALIZE lr_ret_control_vu.* TO NULL

    -- se busca el ultimo registro que coincida con el nss y el medio de entrega dados
    SELECT FIRST 1 *
    INTO lr_ret_control_vu.*
    FROM ret_control_vu
    WHERE param_nss = p_nss
    AND   param_medio_entrega = p_medio_entrega
    ORDER BY consecutivo DESC

    RETURN lr_ret_control_vu.*
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_valida_parametros_entrada
Fecha creacion: Octubre 07, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que valida la completitud y correctitud de los parametros
de entrada al servicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_parametros_entrada()
    
    -- se validan que existan los parametros minimos
    IF ( (gr_entrada_ws.nss IS NULL OR gr_entrada_ws.nss CLIPPED = "" ) OR
         (gr_entrada_ws.caso_crm IS NULL OR gr_entrada_ws.caso_crm CLIPPED = "" ) OR
         (gr_entrada_ws.medio_entrega IS NULL OR gr_entrada_ws.medio_entrega CLIPPED = "" )
    ) THEN

        RETURN GI_COD_RECHAZO_DATOS_INCOMPLETOS
    ELSE
        -- cuenta clabe es requerida si indicador de marca es APROBAR SOLICITUD
        IF ( gr_entrada_ws.ind_marca IS NOT NULL AND gr_entrada_ws.ind_marca = GI_INDICADOR_MARCA_APROBAR_SOLICITUD AND
                 (gr_entrada_ws.cuenta_clabe IS NULL OR gr_entrada_ws.cuenta_clabe CLIPPED = "")
            ) THEN
            LET gs_desc_error_auxiliar = "La cuenta CLABE es requerida cuando el indicador de marca es MARCA APROBAR SOLICITUD"
            RETURN GI_COD_RECHAZO_ERROR_GENERICO
        END IF

        -- si indicador de marca no cuenta con valores validos
        IF ( gr_entrada_ws.ind_marca < 1 OR gr_entrada_ws.ind_marca > 4 ) THEN
            RETURN GI_COD_RECHAZO_INDICADOR_INVALIDO
        END IF
        
    END IF

    -- si se tiene medio de entrega y no contiene valor valido
    IF ( gr_entrada_ws.medio_entrega < 1 OR gr_entrada_ws.medio_entrega > 7 ) THEN
        LET gs_desc_error_auxiliar = "El medio de entrega es inválido; valores recibidos [1..7]"
        RETURN GI_COD_RECHAZO_ERROR_GENERICO
    END IF

    -- si las validaciones son correctas, se devuelve un cero 
    RETURN 0
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_genera_respuesta_ws
Fecha creacion: Octubre 07, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que pobla el registro de salida del servicio con base en la
clave de estatus de peticion del servicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_respuesta_ws(p_estatus_peticion, p_estatus_marca, p_desc_rechazo)
DEFINE p_estatus_peticion SMALLINT
DEFINE p_estatus_marca    SMALLINT
DEFINE p_desc_rechazo     STRING

DISPLAY "EstatusPeticion,EstatusMarca,DescRechazo a responder: ", p_estatus_peticion, " - ", p_estatus_marca, " - ", p_desc_rechazo
    -- si hubo error
    IF ( p_estatus_peticion <> 0 ) THEN
        LET gr_salida_ws.nss                    = gr_entrada_ws.nss
        LET gr_salida_ws.estatus_marca          = p_estatus_marca
        LET gr_salida_ws.caso_crm               = gr_entrada_ws.caso_crm
        LET gr_salida_ws.cod_rechazo            = p_estatus_peticion
        IF ( NOT p_desc_rechazo.equals("") ) THEN
            LET gr_salida_ws.des_rechazo            = p_desc_rechazo
        ELSE
            LET gr_salida_ws.des_rechazo            = fn_desc_error(p_estatus_peticion)
        END IF
        
        LET gr_salida_ws.id_retiro_fa           = 0
        LET gr_salida_ws.saldo_pesos_fa         = 0
        LET gr_salida_ws.tanto_adicional        = 0
        LET gr_salida_ws.id_retiro_ley73        = 0
        LET gr_salida_ws.saldo_aivs_viv92       = 0
        LET gr_salida_ws.saldo_pesos_viv92      = 0
        LET gr_salida_ws.saldo_aivs_viv97       = 0
        LET gr_salida_ws.saldo_pesos_viv97      = 0
    ELSE
        -- el marcaje fue correcto, los montos son asignados por cada llamada al marcaje
        LET gr_salida_ws.nss                    = gr_entrada_ws.nss
        LET gr_salida_ws.caso_crm               = gr_entrada_ws.caso_crm
        LET gr_salida_ws.estatus_marca          = p_estatus_marca
        LET gr_salida_ws.cod_rechazo            = 0       
        LET gr_salida_ws.des_rechazo            = fn_desc_error(p_estatus_peticion)

    END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_desc_error
Fecha creacion: Octubre 07, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que devuelve la descripcion de codigo de error que recibe como
parametro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_desc_error(p_codigo_error)
DEFINE p_codigo_error SMALLINT
DEFINE v_desc_error STRING

    -- si ya se tiene una descripcion de error, no se sobreescribe
    IF ( gr_salida_ws.des_rechazo IS NOT NULL AND gr_salida_ws.des_rechazo <> "" ) THEN
    DISPLAY "vpd aqui"
       RETURN gr_salida_ws.des_rechazo
    END IF
    
    CASE p_codigo_error
       WHEN 0
           LET v_desc_error = "Correcto"

       WHEN GI_COD_RECHAZO_MARCA_EXITO
           LET v_desc_error = "Estatus marca exitoso"

       WHEN GI_COD_RECHAZO_MARCA_ERROR
           LET v_desc_error = "Estatus marca no exitoso"

       WHEN GI_COD_RECHAZO_DATOS_INCOMPLETOS
           LET v_desc_error = "El NSS, el caso CRM y el medio de entrega son requeridos"

       WHEN GI_COD_RECHAZO_CAUSAL_INVALIDA
           LET v_desc_error = "Causal de retiro inválida"

       WHEN GI_COD_RECHAZO_INDICADOR_INVALIDO
           LET v_desc_error = "Valor enviado como indicador de marca es inválido"

       WHEN GI_COD_RECHAZO_ERROR_AL_MARCAR
           LET v_desc_error = "Error interno al intentar marcar/desmarcar la cuenta"

       WHEN GI_COD_RECHAZO_ERROR_WS_MARCA
           LET v_desc_error = "Error al generar la solicitud en web service de marca"

       WHEN GI_COD_RECHAZO_ERROR_AL_DESMARCAR
           LET v_desc_error = "No existe cuenta para desmarcar"

       WHEN GI_COD_RECHAZO_ERROR_INTERNO_MARCA
           LET v_desc_error = "Error interno en el proceso de marca"

       WHEN GI_COD_RECHAZO_MARCA_NO_CONVIVE
           LET v_desc_error = "La marca no convive"

       WHEN GI_COD_RECHAZO_ERROR_GENERICO
           LET v_desc_error = gs_desc_error_auxiliar
       
       OTHERWISE
           LET v_desc_error = "Error no identificado"
    END CASE

    RETURN v_desc_error
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_actualiza_control_vu_marcas
Fecha creacion: Octubre 08, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Funcion que actualiza los estatus de marcaje por tipo de retiro para un
registro de control de retiro de ventanilla unica

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_actualiza_control_vu_marcas(p_consecutivo, p_marcaje_fa_correcto, p_marcaje_ley73_correcto, p_marcaje_si_correcto)
DEFINE p_consecutivo            LIKE ret_control_vu.consecutivo
DEFINE p_marcaje_fa_correcto    SMALLINT
DEFINE p_marcaje_LEY73_correcto SMALLINT
DEFINE p_marcaje_si_correcto    SMALLINT
    UPDATE ret_control_vu
	SET
    st_marca_fa               = p_marcaje_fa_correcto
   ,st_marca_ley73            = p_marcaje_ley73_correcto
   ,st_marca_si               = p_marcaje_si_correcto
   WHERE consecutivo          = p_consecutivo 
                                                                                                      
END FUNCTION

{
======================================================================
Clave: 
Nombre: 
Fecha creacion: Octubre 29, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Obtiene los identificadores de solicitud de los tipos de retiro que fueron marcados
correctamente y se asignan en la tabla de control de ventanilla unica

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_actualiza_ids_solicitud_retiro(p_id_derechohabiente, p_consecutivo, p_marcaje_fa_correcto, p_marcaje_ley73_correcto, p_marcaje_si_correcto, p_indicador_marca)
DEFINE p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente
DEFINE p_consecutivo            LIKE ret_control_vu.consecutivo
DEFINE p_marcaje_fa_correcto    SMALLINT
DEFINE p_marcaje_LEY73_correcto SMALLINT
DEFINE p_marcaje_si_correcto    SMALLINT
DEFINE v_id_solicitud_fa        LIKE ret_fondo_ahorro.id_solicitud
DEFINE v_id_solicitud_ley73     LIKE ret_ley73_generico.id_solicitud
DEFINE v_id_solicitud_si        LIKE ret_solo_infonavit.id_solicitud
DEFINE p_indicador_marca        SMALLINT

   -- no se reasigna el id de solicitud cuando se aprueba o rechaza
   IF ( p_indicador_marca <> GI_INDICADOR_MARCA_MARCAR ) THEN
      RETURN
   END IF

    -- si la marca de fondo de ahorro fue correctamente
    IF ( p_marcaje_fa_correcto = TRUE ) THEN
       SELECT id_solicitud
       INTO v_id_solicitud_fa
       FROM ret_solicitud_generico
       WHERE id_derechohabiente = p_id_derechohabiente
       AND estado_solicitud = 8
       AND modalidad_retiro = 2 -- fondo de ahorro
    END IF
        
    -- si la marca de retiro ley73 fue correctamente
    IF ( p_marcaje_ley73_correcto = TRUE ) THEN
       SELECT id_solicitud
       INTO v_id_solicitud_ley73
       FROM ret_solicitud_generico
       WHERE id_derechohabiente = p_id_derechohabiente
       AND estado_solicitud = 8
       AND modalidad_retiro = 3 -- ley73
    END IF
    
    -- si la marca de solo infonavit fue correcta
    IF ( p_marcaje_si_correcto = TRUE ) THEN
       SELECT id_solicitud
       INTO v_id_solicitud_si
       FROM ret_solicitud_generico
       WHERE id_derechohabiente = p_id_derechohabiente
       AND estado_solicitud = 8
       AND modalidad_retiro = 1 -- solo infonavit
    END IF

    UPDATE ret_control_vu
	SET
    id_solicitud_fa               = v_id_solicitud_fa
   ,id_solicitud_ley73            = v_id_solicitud_ley73
   ,id_solicitud_si               = v_id_solicitud_si
   WHERE consecutivo          = p_consecutivo 
                                                                                                      
END FUNCTION