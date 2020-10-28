################################################################################
#PROYECTO        => VENTANILLA UNICA                                           #
#PROPIETARIO     => OMNISYS                                                    #
#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS42                                                    #
#OBJETIVO        => WS PARA ORQUESTAR LLAMADOS A SERVICIOS PARA MARCAR CUENTAS #
#                => DE FA, LEY73 Y SOLO INFONAVIT.                             #
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
#GLOBALS "RETG01.4gl"

GLOBALS

    -- registro de entrada para el servicio
    DEFINE gr_entrada_ws             RECORD
           nss                       STRING,    -- nss del trabajador
           RFC                       STRING,    -- rfc del trabajador
           caso_crm                  STRING,
           cuenta_clabe              STRING,
           ind_marca                 STRING,
           cod_rechazo               STRING,
           medio_entrega             STRING,    -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
           usuario                   STRING     -- Usuario que realiza el trámite
           END RECORD

    -- registro de respuesta del servicio
    DEFINE gr_salida_ws              RECORD
           nss                       STRING,    -- Número de seguridad social del trabajador
           estatus_marca             STRING,    -- RFC del trabajador
           caso_crm                  STRING,
           cod_rechazo               STRING,    -- codigo de rechazo     
           des_rechazo               STRING,    -- descripcion del rechazo
           -----------
           id_retiro_fa              STRING,    -- ID solicitrud fondo ahorro
           saldo_pesos_fa            STRING,    -- saldo en pesos
           tanto_adicional           STRING,    -- tanto adicional
           -----------
           id_retiro_ley73           STRING,    -- ID solicitud ley73 
           saldo_aivs_viv92          STRING,    -- saldo en AIVs
           saldo_pesos_viv92         STRING,    -- saldo en pesos
           saldo_aivs_viv97          STRING,    -- saldo en AIVs
           saldo_pesos_viv97         STRING     -- saldo en pesos
           END RECORD
         
    DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado

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

END GLOBALS

#==============================================================================#
# MAIN                                                                         #
#==============================================================================#
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS41."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
    -- se inicia el log del programa
    IF FGL_GETENV("FGLSQLDEBUG") THEN
       CALL STARTLOG("c:/tmp/RETWS42.log")
       DISPLAY "Ruta del log creada del servidor: c:/tmp/RETWS41.log"
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
        CALL fn_crea_ws_disponibilidad_ventanilla_unica(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            --OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Disponibilidad Ley 73 service") --, STYLE="naked")   --pendiente revisar alcance
            --display_status("Retiro Service Startup")
        ELSE
            IF num_args() <> 0 THEN
                CALL exitHelp()
                EXIT PROGRAM
            END IF
        END IF
    END IF
  
    CALL ERRORLOG("Invoca creacion de servicio de marcaje para Ventanilla Unica")
    DISPLAY "INVOCA CREACION DE SERVICIO MARCAJE VENTANILLA UNICA v1.0.01"  --debug
    
    --se crea el servicio
    CALL fn_crea_ws_disponibilidad_ventanilla_unica(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de mARCAJE Ventanilla Unica 1.0 ...")

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
Nombre      : fn_crea_ws_disponibilidad_ventanilla_unica
Creacion    : agosto -2020
Autor       : Isai Jimenez Rojas -  Omnisys
Objetivo    : Genera el servicio web para llamado de servicios de disponibilidad 
            : para ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_crea_ws_disponibilidad_ventanilla_unica(p_generar_WSDL)

    DEFINE v_webservice         com.WebService       # WebService
    DEFINE op                   com.WebOperation     # Operation of a WebService
    DEFINE v_service_NameSpace  STRING -- namespace del servicio
    DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
    DEFINE v_resultado          INTEGER
    DEFINE v_urn                STRING -- URN

DISPLAY "Entra Funcion principal que crea el servicio de marcaje...."  --debug

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
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
        LET op = com.WebOperation.CreateDOCStyle("fn_marca_cuenta_ventanilla_unica","fn_marca_cuenta_ventanilla_unica",gr_entrada_ws,gr_salida_ws)
      --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")   --pendiente detetminar si se elimina esta linea
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

#==============================================================================#
#                                                                              #
#==============================================================================#
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
================================================================================
Nombre      : fn_marca_cuenta_ventanilla_unica
Creacion    : Agosto 2020
Autor       : Isai Jimenez Rojas - Omnisys
Objetivo    : Funcion principal de gestion de disponibilidad para ventanilla unica
Modificacion:
================================================================================
}
FUNCTION fn_marca_cuenta_ventanilla_unica()

   DEFINE v_indice_retiro SMALLINT,
          v_nss             LIKE afi_fondo72.nss,
          v_medio_entrega   SMALLINT,
          v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
          v_ruta_log        STRING,
          v_cadena          STRING

DISPLAY "Ingresa a function principal que resuelve el servicio..." 

   -- se responde el servicio para pruebas

   LET gr_salida_ws.nss = gr_entrada_ws.nss

   LET v_nss           = gr_entrada_ws.nss
   LET v_medio_entrega = gr_entrada_ws.medio_entrega
   
   DISPLAY "Parámetros recibidos:"
   DISPLAY "====================="
   DISPLAY "NSS               :", gr_entrada_ws.nss           ,":"
   DISPLAY "RFC               :", gr_entrada_ws.RFC           ,":"
   DISPLAY "Caso CRM          :", gr_entrada_ws.caso_crm      ,":"
   DISPLAY "Cuenta Clabe      :", gr_entrada_ws.cuenta_clabe  ,":"
   DISPLAY "Indicador de marca:", gr_entrada_ws.ind_marca     ,":"
   DISPLAY "Codigo Rechazo    :", gr_entrada_ws.cod_rechazo   ,":"
   DISPLAY "Medio de Entrega  :", gr_entrada_ws.medio_entrega ,":"
   DISPLAY "Usuario           :", gr_entrada_ws.usuario       ,":"

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS41."
   LET v_cadena   = v_nss
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   --DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   --CALL STARTLOG(v_ruta_log)

   CALL fn_respuesta_dummy()

--CALL fn_respuesta_ws_ventanilla_unica(gi_solicitud_rechazada, gi_datos_incompletos, 8, 0, TODAY,0)

--aqui estara el codigo para gestionar los llamados a los ws de disponibilidad
--            CALL fn_ret_disponibilidad_ley73(v_nss, v_grupo, v_medio_entrega, TRUE)
   
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_respuesta_dummy()

    LET gr_salida_ws.nss                    = gr_entrada_ws.nss
    LET gr_salida_ws.estatus_marca          = "0"
    LET gr_salida_ws.caso_crm               = "1234567890"
    LET gr_salida_ws.cod_rechazo            = "0"
    LET gr_salida_ws.des_rechazo            = "descripcion de prueba"
    
    LET gr_salida_ws.id_retiro_fa           = "123"
    LET gr_salida_ws.saldo_pesos_fa         = "500"
    LET gr_salida_ws.tanto_adicional        = "500"
    
    LET gr_salida_ws.id_retiro_ley73        = "321"
    LET gr_salida_ws.saldo_aivs_viv92       = "500"
    LET gr_salida_ws.saldo_pesos_viv92      = "700"
    LET gr_salida_ws.saldo_aivs_viv97       = "500"
    LET gr_salida_ws.saldo_pesos_viv97      = "700"

END FUNCTION

