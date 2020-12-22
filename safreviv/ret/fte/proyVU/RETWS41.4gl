################################################################################
#PROYECTO        => VENTANILLA UNICA                                           #
#PROPIETARIO     => OMNISYS                                                    #
#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS41                                                    #
#OBJETIVO        => WS PARA ORQUESTAR LLAMADOS A DISPOIBIBILIDADES DE LOS WS   #
#                => FA, LEY73 Y SOLO INFONAVIT                                 #
#FECHA CREACION  => 25-AGOSTO-2020                                             #
#VERSION         => 1.0.0                                                      #
#MODIFICACION    =>                                                            #
#2020-09-29 VHIS se agregan validaciones de completitud y correctitud de datos #
#                de entrada                                                    #
#                se agregan clausulas para validar si los servicios web        #
#                invocados no responden                                        #
#                se complementa insercion/actualizacion de registro de tabla   #
#                de control ret_control_vu                                     #
################################################################################

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
-- Se incluye el archivo inc del cliente que se va a incorporar (consumir)
-- se va a consumir WS de disponibilidad de VU
GLOBALS "ws_ret_disponibilidadFA.inc"
GLOBALS "ret_disponibilidad_ley73.inc"
GLOBALS "ret_disponibilidadSI.inc"
GLOBALS "../RETG01.4gl"

GLOBALS
    -- registro de entrada para el servicio
    DEFINE gr_entrada_vu_ws RECORD
        nss                       LIKE afi_derechohabiente.nss, -- nss del trabajador
        rfc                       LIKE afi_derechohabiente.rfc, -- rfc del trabajador
        causal_retiro             SMALLINT, -- Causal de retiro
        nrp                       LIKE ret_cat_nrp.nrp, -- Numero de registro patronal
        grupo_ley73               SMALLINT, -- num de grupo al que perteneces segun retiro Ley 73
        medio_entrega             SMALLINT  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
    END RECORD

    -- registro de respuesta del servicio
    DEFINE gr_salida_vu_ws RECORD
        nss                       LIKE afi_derechohabiente.nss,      -- Numero de seguridad social del trabajador
        rfc                       LIKE afi_derechohabiente.rfc,      -- RFC del trabajador
        -----------
        estado_solicitud_fa       SMALLINT,      -- estado de la solicitud
        cod_rechazo_fa            SMALLINT,      -- codigo de rechazo     
        des_rechazo_fa            CHAR(100),     -- descripcion del rechazo
        saldo_pesos_fa            DECIMAL(22,2), -- saldo en pesos
        tanto_adicional_fa        DECIMAL(22,2), -- tanto adicional
        -----------
        estado_solicitud_ley73    SMALLINT,      -- estado de la solicitud
        cod_rechazo_ley73         SMALLINT,      -- codigo de rechazo     
        des_rechazo_ley73         CHAR(100),     -- descripcion del rechazo
        saldo_aivs_viv92          DECIMAL(22,6), -- saldo en AIVs
        saldo_pesos_viv92         DECIMAL(22,2), -- saldo en pesos
        saldo_aivs_viv97          DECIMAL(22,6), -- saldo en AIVs
        saldo_pesos_viv97         DECIMAL(22,2), -- saldo en pesos
        -----------
        saldo_total_pesos         DECIMAL(22,2), -- saldo en pesos
        fecha_valuacion           CHAR(8)        -- fecha de valuacion de AIVs en formato AAAAMMDD
    END RECORD

    DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado

    DEFINE reg_salida_vu  RECORD
        estado_solicitud_fa    SMALLINT,
        cod_rechazo_fa         SMALLINT,
        des_rechazo_fa         CHAR(100),
        saldo_pesos_fa         DECIMAL(22,2),
        tanto_adicional        DECIMAL(22,2),
        estado_solicitud_ley73 SMALLINT,
        cod_rechazo_ley73      SMALLINT,
        des_rechazo_ley73      CHAR(100),
        saldo_aivs_viv92       DECIMAL(22,6),
        saldo_pesos_viv92      DECIMAL(22,2),
        saldo_aivs_viv97       DECIMAL(22,6),
        saldo_pesos_viv97      DECIMAL(22,2),
        saldo_pesos_total      DECIMAL(22,2),
        fecha_valuacion        CHAR(8)
    END RECORD
    -- =======================================================
    -- claves de subcuenta de vivienda
    CONSTANT  G_SUBCUENTA_VIV92 SMALLINT = 8,
              G_SUBCUENTA_VIV97 SMALLINT = 4

    -- =======================================================
    -- constantes para la evaluacion del resultado de la ejecucion del webservice
    CONSTANT  G_RES_PROCESADA                    SMALLINT = 0  ,
              G_RES_SIN_SOLICITUD                SMALLINT = -1 ,
              G_RES_DESCONECTADO_DEL_SERVIDOR    SMALLINT = -2 ,
              G_RES_CONEXION_CON_CLIENTE_PERDIDA SMALLINT = -3 ,
              G_RES_SERVIDOR_INTERRUMPIDO_CTRL_C SMALLINT = -4 ,
              G_RES_ERROR_INTERNO                SMALLINT = -10,
              G_MSG_PROCESADA                    STRING = "Solicitud procesada"                  ,
              G_MSG_SIN_SOLICITUD                STRING = "Sin solicitud"                        ,
              G_MSG_DESCONECTADO_DEL_SERVIDOR    STRING = "Desconectado del servidor"            ,
              G_MSG_CONEXION_CON_CLIENTE_PERDIDA STRING = "Se perdió la conexión con el cliente" ,
              G_MSG_SERVIDOR_INTERRUMPIDO_CTRL_C STRING = "Se interrumpió el servidor con CTRL-C",
              G_MSG_ERROR_INTERNO                STRING = "Ocurrió un error interno"
    -- constantes con estatus de solicitud
    CONSTANT  G_SOLICITUD_ACEPTADA SMALLINT  = 10, -- procede
              G_SOLICITUD_RECHAZADA SMALLINT = 100 -- rechazada

    -- valores para validacion de estatus del servicio
    CONSTANT  G_RES_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS SMALLINT = -1, -- parametros de entrada estan incompletos
              G_MSG_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS STRING = "Parametros de entrada incompletos. NSS, Causal de retiro y Medio de entrega son obligatorios",
              G_RES_ERROR_MEDIO_ENTREGA_INEXISTENTE      SMALLINT = -2, -- medio de entrega recibido no existe
              G_MSG_ERROR_MEDIO_ENTREGA_INEXISTENTE      STRING = "El medio de entrega recibido no existe. Valores validos [1,2,3,4,5,6,7]",
              G_RES_ERROR_GRUPO_LEY73_INEXISTENTE        SMALLINT = -3, -- grupo retiro ley73 recibido no existe
              G_MSG_ERROR_GRUPO_LEY73_INEXISTENTE        STRING = "El grupo de retiro Ley 73 recibido no existe. Valores validos [1,2,3,4]",
              G_RES_ERROR_CAUSAL_RETIRO_INEXISTENTE      SMALLINT = -4, -- causal retiro ley73 recibido no existe
              G_MSG_ERROR_CAUSAL_RETIRO_INEXISTENTE      STRING = "La causal de retiro recibida no existe. Valores validos [1,2,3]",
              G_RES_ERROR_CONSUMO_WS                     SMALLINT = -5, -- causal retiro ley73 recibido no existe
              G_MSG_ERROR_CONSUMO_WS                     STRING = "Ocurrio un error al intentar conectar con los servicios de consulta de disponibilidad ",
              G_RES_ERROR_CAUSAL_NO_DISPONIBLE           SMALLINT = -6,
              G_MSG_ERROR_CAUSAL_NO_DISPONIBLE           STRING = "La causal de retiro Defuncion no puede ser tramitada en esta ventanilla", --
              G_RES_CONSUMO_CORRECTO SMALLINT = 0,
              G_MSG_CONSUMO_CORRECTO STRING = "WS Ventanilla Unica Disponibilidad ejecutado correctamente"
              
             
    -- =======================================================
    -- Definicion del registro de control de VU  ---RHV
    DEFINE gr_registro_control_vu  RECORD LIKE ret_control_vu.*
             
    DEFINE serverURL STRING -- URL del servidor
    DEFINE v_pantalla    SMALLINT
    DEFINE acum_saldos_disp_ley73_aivs  DECIMAL(22,6)
    DEFINE acum_saldos_disp_ley73_pesos DECIMAL(22,2)
    DEFINE acum_cod_rechazo_ley73       SMALLINT
    DEFINE acum_des_rechazo_ley73       CHAR(100)
    DEFINE acum_estado_solicitud_ley73  SMALLINT
    DEFINE acum_saldos_disp_SI_aivs     DECIMAL(22,6)
    DEFINE acum_saldos_disp_SI_pesos    DECIMAL(22,2)

END GLOBALS

#==============================================================================#
# MAIN                                                                         #
#==============================================================================#
MAIN
    DEFINE v_resultado       INTEGER -- recibe el resultado de la ejecucion del servicio 
    DEFINE v_ruta_log        STRING
    DEFINE v_cadena          STRING
    DEFINE v_ruta_ejecutable VARCHAR(40)

    DEFER INTERRUPT

    -- se obtiene la ruta ejecutable
    SELECT ruta_bin
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS41_"
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
   
    DISPLAY "Log del servicio: " || FGL_GETENV("RETWS41LOG")
    
    -- se inicia el log del programa
    IF FGL_GETENV("RETWS41LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS41LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS41LOG")
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
  
    CALL ERRORLOG("Invoca creacion de servicio Retiro para Ventanilla Unica")
    DISPLAY "INVOCA CREACION DE SERVICIO DISPONIBILIDAD VENTANILLA UNICA v1.0.02"  --debug

    
    --se crea el servicio
    CALL fn_crea_ws_disponibilidad_ventanilla_unica(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Disponibilidad Ventanilla Unica 1.0 ...")

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
                WHEN G_RES_PROCESADA
                    DISPLAY g_msg_procesada TO msg

                WHEN G_RES_SIN_SOLICITUD
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
                WHEN G_RES_PROCESADA
                    DISPLAY g_msg_procesada
                    CALL ERRORLOG(g_msg_procesada)

                WHEN G_RES_SIN_SOLICITUD
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
    
    -- Se actualiza el archivo control VU
    CALL fn_Actualiza_control_vu()
    
END MAIN

{
======================================================================
Nombre      : fn_crea_ws_disponibilidad_ventanilla_unica
Creacion    : agosto -2020
Autor       : Isai Jimenez Rojas -  Omnisys
Objetivo    : Genera el servicio web para llamado de servicios de disponibilidad 
            : para ventanilla Unica (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_crea_ws_disponibilidad_ventanilla_unica(p_generar_WSDL)

    DEFINE v_webservice         com.WebService       # WebService
    DEFINE op                   com.WebOperation     # Operation of a WebService
    DEFINE v_service_NameSpace  STRING -- namespace del servicio
    DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
    DEFINE v_resultado          INTEGER
--  DEFINE v_urn                STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("ConsultaDisponibilidadVentanillaUnica", v_service_NameSpace)
        CALL v_webservice.setFeature("Soap1.1", TRUE)
        DISPLAY "Entra a crear servicio...."	
        -- =============================
        -- Publicacion de las funciones

        -- declaracion de funcion principal del servicio
        LET op = com.WebOperation.CreateDOCStyle("fn_disponibilidad_ventanilla_unica","fn_disponibilidad_ventanilla_unica",gr_entrada_vu_ws,gr_salida_vu_ws)
        --se publica funcion principal del servicio
        CALL v_webservice.publishOperation(op, "fn_disponibilidad_ventanilla_unica")

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
            --display_status("Retiro Disponibilidad Ventanilla Unica Service registrado")
            CALL ERRORLOG("Se registro el servicio de Disponibilidad para Ventanilla Unica")
            DISPLAY "Se registro el servicio de Disponibilidad para Ventanilla Unica"
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Disponibilidad para Ventanilla Unica': " || STATUS)
            EXIT PROGRAM
    END TRY

END FUNCTION

{
================================================================================
Nombre      : exitHelp
Creacion    : Septiembre 30, 2020
Autor       : Ivan Vega - Omnisys
Objetivo    : Muestra la ayuda del programa cuando se ejecuta con parametros incurrectos
Modificacion:
================================================================================
}
FUNCTION exitHelp()
    DISPLAY "-- WS Consulta de Disponibilidad Ventanilla Unica --"
    DISPLAY "  ", arg_val(0)
    DISPLAY "    Para iniciar el servidor con el puesto definido por la variable FGLAPPSERVER"
    DISPLAY "  ", arg_val(0), " -W serverurl"
    DISPLAY "    Genera la WSDL para la url dada"
    DISPLAY "  ", arg_val(0), " -S port"
    DISPLAY "    Arranca el servicio en modo grafico con el puerto dado"
    EXIT PROGRAM
END FUNCTION

{
================================================================================
Nombre      : fn_disponibilidad_ventanilla_unica
Creacion    : Agosto 2020
Autor       : Isai Jimenez Rojas - Omnisys
Objetivo    : Ejecuta la secuencia de consulta de disponibilidad de fondo de ahorro
              retiro ley 73, y retiro solo infonavit
Modificacion:
Ivan Vega   Noviembre 25, 2020          - si ley73 reporta viv97 como no disponible por un motivo distinto a que no tenga
                                          saldo, no se invoca solo infonavit y se reporta como no disponible
Ivan Vega   Noviembre 26, 2020          - si se recibe causal de retiro 4 (defuncion), se devolvera una respuesta indicando
                                          que el retiro por esa causal no puede ser procesada en ventanilla unica
Ivan Vega   Diciembre 7, 2020           - Se agregan validaciones con base en la matriz de disponibilidad de la ventanilla, que
                                          es controlada por fondo de ahorro
                                            FA      LEY73
                                          Causal 1  No invocar
                                          Causal 2  Invocar
                                          Causal 3  No invocar
                                          Causal 4  No se procesa ningun retiro                                                                  
================================================================================
}
FUNCTION fn_disponibilidad_ventanilla_unica()
DEFINE v_continuar        SMALLINT -- booleana para indicar si se continua con la ejecucion
DEFINE v_estatus_fa       SMALLINT
DEFINE v_estatus_ley73    SMALLINT
DEFINE v_estatus_si       SMALLINT
DEFINE v_invocar_fa_si    SMALLINT -- si FA valida correctamente, 1 invocar SI, 0 no invocarlo
DEFINE v_invocar_ley73_si SMALLINT -- si Ley73 valida correctamente, 1 invocar SI, 0 no invocarlo
DEFINE v_invocar_ley73    SMALLINT -- si la causal de retiro es 2 en FA, se invoca Ley73, si no, no


   DISPLAY "Ingresa a function principal del servicio"
   DISPLAY "Varibles de entorno end-point de los servicios de disponibilidad satelite:"
   DISPLAY "URL_RETWS_DISPONIBILIDAD_FA   : ", FGL_GETENV("URL_RETWS_DISPONIBILIDAD_FA")
   DISPLAY "URL_RETWS_DISPONIBILIDAD_LEY73: ", FGL_GETENV("URL_RETWS_DISPONIBILIDAD_LEY73")
   DISPLAY "URL_RETWS_DISPONIBILIDAD_SI   : ", FGL_GETENV("URL_RETWS_DISPONIBILIDAD_SI")
   -- se responde el servicio para pruebas

   LET gr_salida_vu_ws.nss = gr_entrada_vu_ws.nss
 
   DISPLAY "Parametros recibidos:"
   DISPLAY "NSS          : ", gr_entrada_vu_ws.nss
   DISPLAY "RFC          : ", gr_entrada_vu_ws.rfc
   DISPLAY "CAUSAL RETIRO: ", gr_entrada_vu_ws.causal_retiro
   DISPLAY "NRP          : ", gr_entrada_vu_ws.nrp
   DISPLAY "GRUPO        : ", gr_entrada_vu_ws.grupo_ley73
   DISPLAY "MEDIO ENTREGA: ", gr_entrada_vu_ws.medio_entrega

   -- se asume correctitud de datos
   LET v_continuar        = TRUE
   LET v_estatus_fa       = 0
   LET v_estatus_ley73    = 0
   LET v_estatus_si       = 0
   LET v_invocar_fa_si    = 1 -- se asume que se invocara solo infonavit
   LET v_invocar_ley73_si = 1 -- se asume que se invocara solo infonavit
   LET v_invocar_ley73    = 1 -- se asume que se invocara ley73
   
   -- se valida que se tengan los campos minimos
   IF ( gr_entrada_vu_ws.nss IS NULL OR gr_entrada_vu_ws.medio_entrega IS NULL OR
        gr_entrada_vu_ws.nss = "" OR gr_entrada_vu_ws.medio_entrega = "" OR
        gr_entrada_vu_ws.causal_retiro IS NULL OR gr_entrada_vu_ws.causal_retiro = "" ) THEN
       LET v_continuar = FALSE
       -- campos obligatorios no se recibieron
       CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS, v_estatus_fa, v_estatus_ley73, v_estatus_si)
    END IF

    -- 20201126 si la causal de retiro es 4, no se puede procesar la peticion
    IF ( gr_entrada_vu_ws.causal_retiro = 4 ) THEN
       LET v_continuar = FALSE
       -- campos obligatorios no se recibieron
       CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_CAUSAL_NO_DISPONIBLE, v_estatus_fa, v_estatus_ley73, v_estatus_si)
    END IF


   -- si la causal de retiro no fue 4
   IF ( v_continuar ) THEN
      -- verifica si medio de entrega es correcto
      IF ( gr_entrada_vu_ws.medio_entrega < 1 OR gr_entrada_vu_ws.medio_entrega > 7 ) THEN
         LET v_continuar = FALSE
         -- medio de entrega incorrecto
         CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_MEDIO_ENTREGA_INEXISTENTE, v_estatus_fa, v_estatus_ley73, v_estatus_si)
      END IF
      
      -- verifica si grupo de retiro ley73 es correcto
      IF ( gr_entrada_vu_ws.grupo_ley73 IS NOT NULL AND (gr_entrada_vu_ws.grupo_ley73 < 1 OR gr_entrada_vu_ws.grupo_ley73 > 4) ) THEN
         LET v_continuar = FALSE
         -- medio de entrega incorrecto
         CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_GRUPO_LEY73_INEXISTENTE, v_estatus_fa, v_estatus_ley73, v_estatus_si)
      END IF
      
      -- verifica si la causal de retiro es correcta
      IF ( gr_entrada_vu_ws.causal_retiro IS NOT NULL AND (gr_entrada_vu_ws.causal_retiro < 1 OR gr_entrada_vu_ws.causal_retiro > 3) ) THEN
         LET v_continuar = FALSE
         -- medio de entrega incorrecto
         CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_CONSUMO_WS, v_estatus_fa, v_estatus_ley73, v_estatus_si)
      END IF
   END IF
    
    IF ( v_continuar ) THEN   
       --------------------------------------------------------
       --Inicializa Control de VU ---RHV
       CALL fn_inicializa_campos_control_vu()
    
       --Graba registro de Control VU ---RHV
       CALL fn_graba_registro_control_vu()
    
       -- inicia secuencia de consulta de disponibilidad
       TRY
          --llamar a funcionalidad del servicio retiroSaldosDisponiblesFA            - RETWS20
          CALL fn_ejecutaWS_disponibilidadFA() RETURNING v_estatus_fa, v_invocar_fa_si
   
          -- ley73 solo se invoca cuando la causal es 2
          IF ( gr_entrada_vu_ws.causal_retiro = 2 ) THEN
   
             --llamar a funcionalidad del servicio retiroSaldosDisponiblesLey73         - RETWS07
             CALL fn_ejecutaWS_disponibilidadLey73() RETURNING v_estatus_ley73, v_invocar_ley73_si
          ELSE
             DISPLAY "FA invocado con causal distinta a 2, no se ejecuta ley73"
             LET gr_salida_vu_ws.estado_solicitud_ley73 = 100
             LET gr_salida_vu_ws.cod_rechazo_ley73      = 100
             LET gr_salida_vu_ws.des_rechazo_ley73      = "CAUSAL DE RETIRO NO PERMITE EJECUTAR LEY73"
             LET gr_salida_vu_ws.saldo_aivs_viv92       = 0
             LET gr_salida_vu_ws.saldo_aivs_viv97       = 0
             LET gr_salida_vu_ws.saldo_pesos_viv92      = 0
             LET gr_salida_vu_ws.saldo_pesos_viv97      = 0
          END IF

          -- si se determina que se debe invocar solo infonavit
          IF ( v_invocar_fa_si = 1 AND v_invocar_ley73_si ) THEN
            --llamar a funcionalidad del servicio retiroSaldosDisponiblesSoloInfonavit - RETWS35
            CALL fn_ejecutaWS_disponibilidadSI() RETURNING v_estatus_si --SI = Solo Infonavit
          ELSE
             DISPLAY "FA/Ley73 no procede por validaciones y se determina no invocar solo infonavit"
          END IF

          -- si las tres ejecuciones fueron correctas (0 es correcto)
          IF ( v_estatus_fa = 0 AND v_estatus_ley73 = 0 AND v_estatus_si = 0 ) THEN
              CALL fn_respuesta_ret_ventanilla_unica(G_RES_CONSUMO_CORRECTO, v_estatus_fa, v_estatus_ley73, v_estatus_si)
          ELSE
              -- alguno de los servicios marco un error, se responde como rechazo
              CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_CONSUMO_WS, v_estatus_fa, v_estatus_ley73, v_estatus_si)
          END IF
       CATCH
           -- ocurrio un error al intentar conectar con los web services
           CALL fn_respuesta_ret_ventanilla_unica(G_RES_ERROR_CONSUMO_WS, v_estatus_fa, v_estatus_ley73, v_estatus_si) 
       END TRY

    END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ret_ventanilla_unica
Fecha creacion: Septiembre 29, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Asigna los valores correspondientes al registro de respuesta del ws
de consulta de disponibilidad de ventanilla unica con base en el
parametro p_codigo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Diciembre 21, 2020       - Cuando hay errores de comunicacion en los servicios, el estado de solicitud
                                         se devuelve como rechazado en ambos tipos de retiros
======================================================================
}
FUNCTION fn_respuesta_ret_ventanilla_unica(p_codigo_error, p_estatus_fa, p_estatus_ley73, p_estatus_si)
DEFINE p_codigo_error  SMALLINT
DEFINE p_estatus_fa    SMALLINT
DEFINE p_estatus_ley73 SMALLINT
DEFINE p_estatus_si    SMALLINT
DEFINE v_des_rechazo   STRING
    

    -- se asignan los parametros de respuesta
    LET gr_salida_vu_ws.nss                    = gr_entrada_vu_ws.nss
    LET gr_salida_vu_ws.rfc                    = gr_entrada_vu_ws.rfc
    lET gr_salida_vu_ws.fecha_valuacion        = TODAY USING "YYYYMMDD"

    CASE p_codigo_error
       -- parametros incorrectos
       WHEN G_RES_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS
           LET gr_salida_vu_ws.des_rechazo_fa         = G_MSG_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS
           LET gr_salida_vu_ws.des_rechazo_ley73      = G_MSG_ERROR_PARAMETROS_ENTRADA_INCOMPLETOS

       -- medio de entrega incorrecto
       WHEN G_RES_ERROR_MEDIO_ENTREGA_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_MEDIO_ENTREGA_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_fa         = G_MSG_ERROR_MEDIO_ENTREGA_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_MEDIO_ENTREGA_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_ley73      = G_MSG_ERROR_MEDIO_ENTREGA_INEXISTENTE

       -- grupo retiro ley73 incorrecto
       WHEN G_RES_ERROR_GRUPO_LEY73_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_GRUPO_LEY73_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_fa         = G_MSG_ERROR_GRUPO_LEY73_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_GRUPO_LEY73_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_ley73      = G_MSG_ERROR_GRUPO_LEY73_INEXISTENTE

       -- causal de retiro incorrecto
       WHEN G_RES_ERROR_CAUSAL_RETIRO_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_CAUSAL_RETIRO_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_fa         = G_MSG_ERROR_CAUSAL_RETIRO_INEXISTENTE
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_CAUSAL_RETIRO_INEXISTENTE
           LET gr_salida_vu_ws.des_rechazo_ley73      = G_MSG_ERROR_CAUSAL_RETIRO_INEXISTENTE

       -- error en comunicacion/consumo de los ws
       WHEN G_RES_ERROR_CONSUMO_WS
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_CONSUMO_WS
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_CONSUMO_WS
           LET gr_salida_vu_ws.estado_solicitud_fa    = G_SOLICITUD_RECHAZADA
           LET gr_salida_vu_ws.estado_solicitud_ley73 = G_SOLICITUD_RECHAZADA

           LET v_des_rechazo = G_MSG_ERROR_CONSUMO_WS

           DISPLAY "Error en servicios: ", p_estatus_fa, " ", p_estatus_ley73, " ", p_estatus_si

           IF ( p_estatus_fa <> 0 ) THEN
               LET v_des_rechazo = v_des_rechazo, " FA"
           END IF

           IF ( p_estatus_ley73 <> 0 ) THEN
               LET v_des_rechazo = v_des_rechazo, " LEY73"
           END IF

           IF ( p_estatus_si <> 0 ) THEN
               LET v_des_rechazo = v_des_rechazo, " SI"
           END IF

           LET gr_salida_vu_ws.des_rechazo_fa = v_des_rechazo
           LET gr_salida_vu_ws.des_rechazo_ley73 = v_des_rechazo

       -- causal de retiro no disponible
       WHEN G_RES_ERROR_CAUSAL_NO_DISPONIBLE
           LET gr_salida_vu_ws.estado_solicitud_fa    = G_SOLICITUD_RECHAZADA
           LET gr_salida_vu_ws.estado_solicitud_ley73 = G_SOLICITUD_RECHAZADA
           LET gr_salida_vu_ws.cod_rechazo_fa         = G_RES_ERROR_CAUSAL_NO_DISPONIBLE
           LET gr_salida_vu_ws.des_rechazo_fa         = G_MSG_ERROR_CAUSAL_NO_DISPONIBLE
           LET gr_salida_vu_ws.cod_rechazo_ley73      = G_RES_ERROR_CAUSAL_NO_DISPONIBLE
           LET gr_salida_vu_ws.des_rechazo_ley73      = G_MSG_ERROR_CAUSAL_NO_DISPONIBLE
           
       -- consumo correcto
       WHEN G_RES_CONSUMO_CORRECTO
           -- el saldo total en pesos es la suma de los saldos en pesos de todos los retiros
           LET gr_salida_vu_ws.saldo_total_pesos      = gr_salida_vu_ws.saldo_pesos_fa + gr_salida_vu_ws.tanto_adicional_fa + gr_salida_vu_ws.saldo_pesos_viv92 + gr_salida_vu_ws.saldo_pesos_viv97
           LET gr_salida_vu_ws.fecha_valuacion        = TODAY USING "YYYYMMDD"

    END CASE
END FUNCTION

{
======================================================================
Nombre      : fn_inicializa_campos_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Inicializa los campos para el control de 
            : ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}

FUNCTION fn_inicializa_campos_control_vu() 

    --Inicializa Campos del Archivo de Control de VU
    LET gr_registro_control_vu.fecha_hora              = CURRENT YEAR TO SECOND 
    LET gr_registro_control_vu.param_nss               = gr_entrada_vu_ws.nss
    LET gr_registro_control_vu.param_rfc               = gr_entrada_vu_ws.rfc
    LET gr_registro_control_vu.param_causal_retiro     = gr_entrada_vu_ws.causal_retiro
    LET gr_registro_control_vu.param_NRP               = gr_entrada_vu_ws.nrp
    LET gr_registro_control_vu.param_grupo_ley73       = gr_entrada_vu_ws.grupo_ley73
    LET gr_registro_control_vu.param_medio_entrega     = gr_entrada_vu_ws.medio_entrega
    LET gr_registro_control_vu.bn_disponibilidad_fa    = FALSE
    LET gr_registro_control_vu.bn_disponibilidad_ley73 = FALSE
    LET gr_registro_control_vu.bn_disponibilidad_si	   = FALSE
    LET gr_registro_control_vu.st_marca_fa             = 0
    LET gr_registro_control_vu.st_marca_ley73          = 0
    LET gr_registro_control_vu.st_marca_si             = 0
    LET gr_registro_control_vu.id_solicitud_fa         = 0
    LET gr_registro_control_vu.id_solicitud_ley73      = 0
    LET gr_registro_control_vu.id_solicitud_si         = 0
    LET gr_registro_control_vu.saldo_acciones_si       = 0
    LET gr_registro_control_vu.saldo_acciones_viv92    = 0
    LET gr_registro_control_vu.saldo_acciones_viv97    = 0
    LET gr_registro_control_vu.saldo_pesos_fa          = 0
    LET gr_registro_control_vu.saldo_pesos_si          = 0
    LET gr_registro_control_vu.saldo_pesos_viv92       = 0
    LET gr_registro_control_vu.saldo_pesos_viv97       = 0
    LET gr_registro_control_vu.saldo_total_pesos       = 0
    LET gr_registro_control_vu.tanto_adicional_fa      = 0
    LET gr_registro_control_vu.fecha_valuacion         = NULL

END FUNCTION

{
======================================================================
Nombre      : fn_graba_registro_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Graba registros para el archivo de control de 
            : ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_graba_registro_control_vu()
   -- se obtiene la siguiente secuencia
   SELECT seq_control_vu.NEXTVAL
   INTO gr_registro_control_vu.consecutivo
   FROM systables
   WHERE tabid = 1

   INSERT INTO ret_control_vu
   (consecutivo
   ,fecha_hora
   ,param_nss
   ,param_rfc
   ,param_causal_retiro
   ,param_nrp
   ,param_grupo_ley73
   ,param_medio_entrega
   ,bn_disponibilidad_fa
   ,bn_disponibilidad_ley73
   ,bn_disponibilidad_si
   ,st_marca_fa
   ,st_marca_ley73
   ,st_marca_si
   ,id_solicitud_fa
   ,id_solicitud_ley73
   ,id_solicitud_si
   ,saldo_pesos_fa
   ,tanto_adicional_fa
   ,saldo_acciones_viv92
   ,saldo_pesos_viv92
   ,saldo_acciones_viv97
   ,saldo_pesos_viv97
   ,saldo_acciones_si
   ,saldo_pesos_si
   ,saldo_total_pesos
   ,fecha_valuacion
   )
   VALUES (gr_registro_control_vu.consecutivo
          ,gr_registro_control_vu.fecha_hora
          ,gr_registro_control_vu.param_nss
          ,gr_registro_control_vu.param_rfc
          ,gr_registro_control_vu.param_causal_retiro
          ,gr_registro_control_vu.param_nrp
          ,gr_registro_control_vu.param_grupo_ley73
          ,gr_registro_control_vu.param_medio_entrega
          ,gr_registro_control_vu.bn_disponibilidad_fa
          ,gr_registro_control_vu.bn_disponibilidad_ley73
          ,gr_registro_control_vu.bn_disponibilidad_si
          ,gr_registro_control_vu.st_marca_fa
          ,gr_registro_control_vu.st_marca_ley73
          ,gr_registro_control_vu.st_marca_si
          ,gr_registro_control_vu.id_solicitud_fa
          ,gr_registro_control_vu.id_solicitud_ley73
          ,gr_registro_control_vu.id_solicitud_si
          ,gr_registro_control_vu.saldo_pesos_fa
          ,gr_registro_control_vu.tanto_adicional_fa
          ,gr_registro_control_vu.saldo_acciones_viv92
          ,gr_registro_control_vu.saldo_pesos_viv92
          ,gr_registro_control_vu.saldo_acciones_viv97
          ,gr_registro_control_vu.saldo_pesos_viv97
          ,gr_registro_control_vu.saldo_acciones_si
          ,gr_registro_control_vu.saldo_pesos_si
          ,gr_registro_control_vu.saldo_total_pesos
          ,gr_registro_control_vu.fecha_valuacion
          )
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ejecutaWS_disponibilidadFA
Fecha creacion: Septiembre 28, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Ejecuta el web service de consulta de disponibilidad de fondo de ahorro con el
NSS y RFC proporcionados como parametros del servicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega   Noviembre 26, 2020       - Se agrega un parametro de retorno que indicara
                                       si aunque haya error en FA, se invoca la validacion
                                       de solo infonavit, que sera cuando FA haya sido rechazado
                                       por saldo insuficiente
======================================================================
}
FUNCTION fn_ejecutaWS_disponibilidadFA()
DEFINE estatus_fa   SMALLINT
DEFINE mensaje_fa   STRING
DEFINE v_invocar_si SMALLINT

    -- se asume que se invocara si
    LET v_invocar_si = 1

    DISPLAY "Se llama al servicio de fondo de ahorro"

    LET fn_saldo_disponible_faRequest.nss              = gr_entrada_vu_ws.nss
    LET fn_saldo_disponible_faRequest.rfc              = gr_entrada_vu_ws.rfc
    LET fn_saldo_disponible_faRequest.causal_ret_fa    = gr_entrada_vu_ws.causal_retiro
    LET fn_saldo_disponible_faRequest.nrp              = gr_entrada_vu_ws.nrp
    LET fn_saldo_disponible_faRequest.f_inicio_pension = TODAY
    LET fn_saldo_disponible_faRequest.medio_entrega    = gr_entrada_vu_ws.medio_entrega

    CALL fn_saldo_disponible_fa_g() RETURNING estatus_fa

    DISPLAY "Estatus de la llamada : ", estatus_fa

    IF estatus_fa <> 0 THEN
       --hubo un error
       DISPLAY "SE DETECTO UN ERROR SE SUSPENDE EJECUCION EN FA"
       LET gr_salida_vu_ws.estado_solicitud_fa = G_SOLICITUD_RECHAZADA
       LET gr_salida_vu_ws.cod_rechazo_fa = estatus_fa
       LET gr_salida_vu_ws.des_rechazo_fa = "Error en consumo de WS de FA"
       LET gr_salida_vu_ws.saldo_pesos_fa = 0
       LET gr_salida_vu_ws.tanto_adicional_fa = 0

       LET v_invocar_si = 0
       
       RETURN estatus_fa, v_invocar_si
    END IF

    IF fn_saldo_disponible_faResponse.estado_solicitud = 10 THEN
    -- SOLICITUD ACEPTADA
    -- ACTUALIZAR TABLA DE CONTROL
       LET gr_registro_control_vu.bn_disponibilidad_fa = TRUE
       DISPLAY "SE CAMBIA LA BANDERA DE DISPONIBILIDAD FA A : ",gr_registro_control_vu.bn_disponibilidad_fa
    ELSE
       -- RECHAZO
       DISPLAY "LA BANDERA DE DISPONIBILIDAD FA SE QUEDA EN FALSE : ",gr_registro_control_vu.bn_disponibilidad_fa

       -- si el rechazo es distinto a carencia de saldo, no se invocara solo infonavit
       IF ( fn_saldo_disponible_faResponse.cod_rechazo <> gi_sin_saldo ) THEN
          LET v_invocar_si = 0
       END IF
       
    END IF

    IF fn_saldo_disponible_faResponse.cod_rechazo = 0 THEN
       LET fn_saldo_disponible_faResponse.des_rechazo = "** SIN RECHAZO **"
    END IF
    
    DISPLAY "Salida del WS FA NSS            : ",fn_saldo_disponible_faResponse.nss
    DISPLAY "Salida del WS FA RFC            : ",fn_saldo_disponible_faResponse.rfc
    DISPLAY "Salida del WS FA ESTADO_SOL     : ",fn_saldo_disponible_faResponse.estado_solicitud
    DISPLAY "Salida del WS FA COD_RECHAZO    : ",fn_saldo_disponible_faResponse.cod_rechazo
    DISPLAY "Salida del WS FA DESCRIP_RECHAZO: ",fn_saldo_disponible_faResponse.des_rechazo
    DISPLAY "Salida del WS FA MONTO PESO     : ",fn_saldo_disponible_faResponse.monto_pesos
    DISPLAY "Salida del WS FA MONTO ADICIONAL: ",fn_saldo_disponible_faResponse.monto_adicional
    DISPLAY "Salida del WS FA MONTO TOTAL    : ",fn_saldo_disponible_faResponse.monto_total
    DISPLAY "Salida del WS FA PAGO DAP       : ",fn_saldo_disponible_faResponse.pago_dap
    DISPLAY "Actualizacion de ret_control_vu : ",gr_registro_control_vu.bn_disponibilidad_fa

    -- se actualiza el registro de solicitud
    LET gr_salida_vu_ws.estado_solicitud_fa       = fn_saldo_disponible_faResponse.estado_solicitud
    LET gr_salida_vu_ws.cod_rechazo_fa            = fn_saldo_disponible_faResponse.cod_rechazo
    LET gr_salida_vu_ws.des_rechazo_fa            = fn_saldo_disponible_faResponse.des_rechazo
    LET gr_salida_vu_ws.saldo_pesos_fa            = fn_saldo_disponible_faResponse.monto_pesos
    LET gr_salida_vu_ws.tanto_adicional_fa        = fn_saldo_disponible_faResponse.monto_adicional
    LET gr_registro_control_vu.saldo_pesos_fa     = fn_saldo_disponible_faResponse.monto_pesos
    LET gr_registro_control_vu.tanto_adicional_fa = fn_saldo_disponible_faResponse.monto_adicional
    
    CALL fn_actualiza_control_vu()
    RETURN estatus_fa, v_invocar_si
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ejecutaWS_disponibilidadLey73
Fecha creacion: Septiembre 28, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Ejecuta el web service de consulta de disponibilidad de retiro Ley 73 con el
NSS y RFC proporcionados como parametros del servicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    Noviembre 25, 2020       - se agrega parametro devuelto que indica
                                        si se debe invocar solo infonavit cuando
                                        vivienda 97 esta disponible
Ivan Vega    Noviembre 26, 2020       - Se inhibe la dependencia de solo infonavit con
                                        la disponibilidad de viv97, sin embargo
                                        se deja activa la funcionalidad
======================================================================
}
FUNCTION fn_ejecutaWS_disponibilidadLey73()
DEFINE estatus_ley73                SMALLINT,
       mensaje_ley73                STRING,
       indice_arr                   SMALLINT,
       x                            SMALLINT
DEFINE v_aivs_viv92 DECIMAL(22,6)
DEFINE v_aivs_viv97 DECIMAL(22,6)
DEFINE v_pesos_viv92 DECIMAL(22,6)
DEFINE v_pesos_viv97 DECIMAL(22,6)
DEFINE v_invocar_si  SMALLINT

    DISPLAY "Se llama al servicio de Ley 73"
    LET fn_ret_saldos_disponibles_ley73Request.nss           = gr_entrada_vu_ws.nss
    LET fn_ret_saldos_disponibles_ley73Request.grupo_ley73   = gr_entrada_vu_ws.grupo_ley73
    LET fn_ret_saldos_disponibles_ley73Request.medio_entrega = gr_entrada_vu_ws.medio_entrega

    LET acum_saldos_disp_ley73_aivs  = 0
    LET acum_saldos_disp_ley73_pesos = 0
    LET acum_cod_rechazo_ley73       = " "
    LET acum_des_rechazo_ley73       = " "
    LET v_aivs_viv92  = 0
    LET v_aivs_viv97  = 0
    LET v_pesos_viv92 = 0
    LET v_pesos_viv97 = 0
    LET v_invocar_si  = 1

    CALL fn_ret_saldos_disponibles_ley73_g() RETURNING estatus_ley73

    DISPLAY "Estatus de la llamada : ", estatus_ley73

    IF estatus_ley73 <> 0 THEN
       --hubo un error
       DISPLAY "SE DETECTO UN ERROR SE SUSPENDE EJECUCION EN LEY73"
       RETURN estatus_ley73, v_invocar_si
    END IF

    LET indice_arr = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element.getLength()

    IF indice_arr >= 1 THEN
    
       FOR x = 1 TO indice_arr

          IF fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].estado_solicitud = 10 THEN
          -- SOLICITUD ACEPTADA
          -- ACTUALIZAR TABLA DE CONTROL
             LET gr_registro_control_vu.bn_disponibilidad_ley73 = TRUE
             DISPLAY "SE CAMBIA LA BANDERA DE DISPONIBILIDAD LET73 A : ",gr_registro_control_vu.bn_disponibilidad_ley73
          ELSE
          -- RECHAZO
             DISPLAY "LA BANDERA DE DISPONIBILIDAD LET73 QUEDA EN FALSE : ",gr_registro_control_vu.bn_disponibilidad_ley73
          END IF

          DISPLAY "Salida del WS LEY 73 NSS        : ",fn_ret_saldos_disponibles_ley73Response.nss
          DISPLAY "Salida del WS SUBCUENTA         : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].subcuenta
          DISPLAY "Salida del WS ESTADO SOLIC      : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].estado_solicitud
          DISPLAY "Salida del WS CODIGO RECHA      : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].cod_rechazo
          DISPLAY "Salida del WS DESCRI RECHA      : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].des_rechazo
          DISPLAY "Salida del WS FECHA VALUAC      : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].f_valuacion
          DISPLAY "Salida del WS MONTO AIVS        : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_avis
          DISPLAY "Salida del WS MONTO PESOS       : ",fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_pesos
          DISPLAY "Actualizacion de ret_control_vu : ", gr_registro_control_vu.bn_disponibilidad_fa

          -- acumulados por subcuenta
          IF ( fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].subcuenta = G_SUBCUENTA_VIV92 ) THEN
              LET v_aivs_viv92 = v_aivs_viv92 + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_avis
              LET v_pesos_viv92 = v_pesos_viv92 + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_pesos
          END IF

          IF ( fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].subcuenta = G_SUBCUENTA_VIV97 ) THEN
              LET v_aivs_viv97 = v_aivs_viv97 + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_avis
              LET v_pesos_viv97 = v_pesos_viv97 + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_pesos

              
              -- se valida si no se debe invocar solo infonavit
              {
              IF ( fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].estado_solicitud = 100 ) THEN
                 -- si el rechazo es distinto a Sin saldo
                 IF ( fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].cod_rechazo <> gi_sin_saldo ) THEN
                    -- viv97 no disponible, no se debe invocar solo infonavit
                    LET v_invocar_si = 0
                 END IF
              END IF
              }
          END IF
          
          LET acum_saldos_disp_ley73_aivs  = acum_saldos_disp_ley73_aivs  + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_avis
          LET acum_saldos_disp_ley73_pesos = acum_saldos_disp_ley73_pesos + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_pesos

          LET acum_cod_rechazo_ley73      = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].cod_rechazo
          LET acum_des_rechazo_ley73      = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].des_rechazo
          LET acum_estado_solicitud_ley73 = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].estado_solicitud
          
       END FOR

    END IF


    
    DISPLAY "Salida de montos por subcuenta aivs : ", acum_saldos_disp_ley73_aivs
    DISPLAY "Salida de montos por subcuenta pesos: ", acum_saldos_disp_ley73_pesos

    LET gr_registro_control_vu.saldo_acciones_viv92 = v_aivs_viv92
    LET gr_registro_control_vu.saldo_acciones_viv97 = v_aivs_viv97
    LET gr_registro_control_vu.saldo_pesos_viv92 = v_pesos_viv92
    LET gr_registro_control_vu.saldo_pesos_viv97 = v_pesos_viv97

    LET gr_salida_vu_ws.estado_solicitud_ley73 = acum_estado_solicitud_ley73
    LET gr_salida_vu_ws.cod_rechazo_ley73      = acum_cod_rechazo_ley73
    LET gr_salida_vu_ws.des_rechazo_ley73      = acum_des_rechazo_ley73
    LET gr_salida_vu_ws.saldo_aivs_viv92       = v_aivs_viv92
    LET gr_salida_vu_ws.saldo_aivs_viv97       = v_aivs_viv97
    LET gr_salida_vu_ws.saldo_pesos_viv92      = v_pesos_viv92
    LET gr_salida_vu_ws.saldo_pesos_viv97      = v_pesos_viv97
    
    CALL fn_actualiza_control_vu()
    
    RETURN estatus_ley73, v_invocar_si
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ejecutaWS_disponibilidadSI
Fecha creacion: Septiembre 29, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Ejecuta el web service de consulta de disponibilidad de retiro 
Solo Infonavit con los datos proporcionados como parametros del servicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    Noviembre 17, 2020      - No se estaba asignando el monto de aivs/pesos de solo infonavit
                                       en la tabla de control ret_control_vu
======================================================================
}
FUNCTION fn_ejecutaWS_disponibilidadSI()
DEFINE estatus_si           SMALLINT

    DISPLAY "Se llama al servicio de Solo Infonavit" 

    LET fn_saldo_disponible_SIRequest.nss              = gr_entrada_vu_ws.nss
    LET fn_saldo_disponible_SIRequest.causal_retiro    = gr_entrada_vu_ws.causal_retiro
    LET fn_saldo_disponible_SIRequest.nrp              = gr_entrada_vu_ws.NRP
    LET fn_saldo_disponible_SIRequest.medio_entrega    = gr_entrada_vu_ws.medio_entrega

    CALL fn_saldo_disponible_si_g() RETURNING estatus_si

    DISPLAY "Estatus de la llamada : ", estatus_si

    IF estatus_si <> 0 THEN
       --hubo un error
       DISPLAY "SE DETECTO UN ERROR SE SUSPENDE EJECUCION EN SI"
       RETURN estatus_si
    END IF

    IF fn_saldo_disponible_SIResponse.estado_solicitud = 10 THEN 
    -- SOLICITUD ACEPTADA
    -- ACTUALIZAR TABLA DE CONTROL
       LET gr_registro_control_vu.bn_disponibilidad_si = TRUE
       DISPLAY "SE CAMBIA LA BANDERA DE DISPONIBILIDAD SI A : ",gr_registro_control_vu.bn_disponibilidad_si

       -- SI se reporta como parte de la respuesta de Ley73, se activa el retiro en caso de que
       -- Ley73 haya marcado no disponibilidad
       LET gr_salida_vu_ws.estado_solicitud_ley73 = 10
       LET gr_salida_vu_ws.cod_rechazo_ley73      = 0
       LET gr_salida_vu_ws.des_rechazo_ley73      = "Disponible"
    ELSE
       -- RECHAZO
       DISPLAY "LA BANDERA DE DISPONIBILIDAD SI SE QUEDA EN FALSE : ",gr_registro_control_vu.bn_disponibilidad_si
    END IF

    IF fn_saldo_disponible_SIResponse.cod_rechazo = 0 THEN
       LET fn_saldo_disponible_SIResponse.des_rechazo = "** SIN RECHAZO **"
    END IF
    
    DISPLAY "Salida del WS SI NSS            : ",fn_saldo_disponible_SIResponse.nss
    DISPLAY "Salida del WS SI ESTADO_SOL     : ",fn_saldo_disponible_SIResponse.estado_solicitud
    DISPLAY "Salida del WS SI COD_RECHAZO    : ",fn_saldo_disponible_SIResponse.cod_rechazo
    DISPLAY "Salida del WS SI DES_RECHAZO    : ",fn_saldo_disponible_SIResponse.des_rechazo
    DISPLAY "Salida del WS SI FECHA VAL      : ",fn_saldo_disponible_SIResponse.fecha_valuacion
    DISPLAY "Salida del WS SI SALDO AIVS     : ",fn_saldo_disponible_SIResponse.saldo_aivs
    DISPLAY "Salida del WS SI MONTO PESOS    : ",fn_saldo_disponible_SIResponse.monto_pesos
    DISPLAY "Actualizacion de ret_control_vu : ",gr_registro_control_vu.bn_disponibilidad_si

    -- se acumulan los montos de solo infonavit a vivienda 97
    LET gr_salida_vu_ws.saldo_aivs_viv97  = gr_salida_vu_ws.saldo_aivs_viv97 + fn_saldo_disponible_SIResponse.saldo_aivs
    LET gr_salida_vu_ws.saldo_pesos_viv97 = gr_salida_vu_ws.saldo_pesos_viv97 + fn_saldo_disponible_SIResponse.monto_pesos
    LET gr_registro_control_vu.saldo_acciones_si = fn_saldo_disponible_SIResponse.saldo_aivs
    LET gr_registro_control_vu.saldo_pesos_si    = fn_saldo_disponible_SIResponse.monto_pesos

    -- se realizan los acumulados
    LET gr_registro_control_vu.saldo_total_pesos = gr_registro_control_vu.saldo_pesos_fa + gr_registro_control_vu.tanto_adicional_fa + gr_registro_control_vu.saldo_pesos_viv92 + gr_registro_control_vu.saldo_pesos_viv97 + gr_registro_control_vu.saldo_pesos_si
    LET gr_registro_control_vu.fecha_valuacion = TODAY
    
    CALL fn_actualiza_control_vu()
    RETURN estatus_si
END FUNCTION

{
======================================================================
Nombre      : fn_actualiza_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Actualiza registro para el archivo de control de 
              ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_actualiza_control_vu()

    UPDATE ret_control_vu
	SET
    fecha_hora                = gr_registro_control_vu.fecha_hora
   ,param_nss                 = gr_registro_control_vu.param_nss
   ,param_rfc                 = gr_registro_control_vu.param_rfc
   ,param_causal_retiro       = gr_registro_control_vu.param_causal_retiro
   ,param_nrp                 = gr_registro_control_vu.param_nrp
   ,param_grupo_ley73         = gr_registro_control_vu.param_grupo_ley73
   ,param_medio_entrega       = gr_registro_control_vu.param_medio_entrega
   ,bn_disponibilidad_fa      = gr_registro_control_vu.bn_disponibilidad_fa
   ,bn_disponibilidad_ley73   = gr_registro_control_vu.bn_disponibilidad_ley73
   ,bn_disponibilidad_si      = gr_registro_control_vu.bn_disponibilidad_si
   ,st_marca_fa               = gr_registro_control_vu.st_marca_fa
   ,st_marca_ley73            = gr_registro_control_vu.st_marca_ley73
   ,st_marca_si               = gr_registro_control_vu.st_marca_si
   ,id_solicitud_fa           = gr_registro_control_vu.id_solicitud_fa
   ,id_solicitud_ley73        = gr_registro_control_vu.id_solicitud_ley73
   ,id_solicitud_si           = gr_registro_control_vu.id_solicitud_si
   ,saldo_pesos_fa            = gr_registro_control_vu.saldo_pesos_fa
   ,tanto_adicional_fa        = gr_registro_control_vu.tanto_adicional_fa
   ,saldo_acciones_viv92      = gr_registro_control_vu.saldo_acciones_viv92
   ,saldo_pesos_viv92         = gr_registro_control_vu.saldo_pesos_viv92
   ,saldo_acciones_viv97      = gr_registro_control_vu.saldo_acciones_viv97
   ,saldo_pesos_viv97         = gr_registro_control_vu.saldo_pesos_viv97
   ,saldo_acciones_si         = gr_registro_control_vu.saldo_acciones_si
   ,saldo_pesos_si            = gr_registro_control_vu.saldo_pesos_si
   ,saldo_total_pesos         = gr_registro_control_vu.saldo_total_pesos
   ,fecha_valuacion           = gr_registro_control_vu.fecha_valuacion
   WHERE consecutivo          = gr_registro_control_vu.consecutivo 
                                                                                                      
END FUNCTION
