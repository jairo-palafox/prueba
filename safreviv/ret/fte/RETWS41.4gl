################################################################################
#PROYECTO        => VENTANILLA UNICA                                           #
#PROPIETARIO     => OMNISYS                                                    #
#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS41                                                     #
#OBJETIVO        => WS PARA ORQUESTAR LLAMADOS A DISPOIBIBILIDADES DE LOS WS   #
#                => FA, LEY73 Y SOLO INFONAVIT                                 #
#FECHA CREACION  => 25-AGOSTO-2020                                             #
#VERSION         => 1.0.0                                                      #
#MODIFICACION    =>                                                            #
################################################################################

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"

-- Se incluye el archivo inc del cliente que se va a incorporar (consumir)
-- se va a consumir WS de disponibilidad de VU
GLOBALS "ws_ret_disponibilidadFA.inc"
GLOBALS "ret_disponibilidad_ley73.inc"
GLOBALS "ret_disponibilidadSI.inc"

GLOBALS

    -- registro de entrada para el servicio
    DEFINE gr_entrada_vu_ws             RECORD
           nss                       CHAR(11), -- nss del trabajador
           RFC                       CHAR(13), -- rfc del trabajador
           causal_retiro             SMALLINT, -- Causal de retiro
           NRP                       CHAR(11), -- Numero de registro patronal
           grupo_ley73               SMALLINT, -- num. de grupo al que perteneces segun retiro Ley 73
           medio_entrega             SMALLINT  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
           END RECORD

    -- registro de respuesta del servicio
    DEFINE gr_salida_vu_ws  RECORD
           nss                       CHAR(11),      -- Número de seguridad social del trabajador
           rfc                       CHAR(13),      -- RFC del trabajador
           -----------
           estado_solicitud_fa       SMALLINT,      -- estado de la solicitud
           cod_rechazo_fa            SMALLINT,      -- codigo de rechazo     
           des_rechazo_fa            CHAR(100),     -- descripcion del rechazo
           saldo_pesos_fa            DECIMAL(22,2), -- saldo en pesos
           tanto_adicional           DECIMAL(22,2), -- tanto adicional
           -----------
           estado_solicitud_ley73    SMALLINT,      -- estado de la solicitud
           cod_rechazo_ley73         SMALLINT,      -- codigo de rechazo     
           des_rechazo_ley73         CHAR(100),     -- descripcion del rechazo
           saldo_aivs_viv92          DECIMAL(22,6), -- saldo en AIVs
           saldo_pesos_viv92         DECIMAL(22,2), -- saldo en pesos
           saldo_aivs_viv97          DECIMAL(22,6), -- saldo en AIVs
           saldo_pesos_viv97         DECIMAL(22,2), -- saldo en pesos
           -----------
           saldo_pesos_total         DECIMAL(22,2), -- saldo en pesos
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
             
    -- =======================================================
    -- Definicion del registro de control de VU  ---RHV
             
    DEFINE gr_registro_control_vu  RECORD LIKE ret_control_vu.*
             
    DEFINE serverURL STRING -- URL del servidor
    DEFINE v_pantalla    SMALLINT,
           acum_saldos_disp_ley73_aivs  DECIMAL(22,6),
           acum_saldos_disp_ley73_pesos DECIMAL(22,2),
           acum_cod_rechazo_ley73       SMALLINT,
           acum_des_rechazo_ley73       CHAR(100),
           acum_estado_solicitud_ley73  SMALLINT,
           acum_saldos_disp_SI_aivs     DECIMAL(22,6),
           acum_saldos_disp_SI_pesos    DECIMAL(22,2)

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
       CALL STARTLOG("c:/tmp/RETWS41.log")
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
    
    -- Se actualiza el archivo control VU
    CALL fn_Actualiza_control_vu()
    
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
--  DEFINE v_urn                STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
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
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")   --pendiente detetminar si se elimina esta linea
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
            -- REgistro del servicio
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
Nombre      : fn_disponibilidad_ventanilla_unica
Creacion    : Agosto 2020
Autor       : Isai Jimenez Rojas - Omnisys
Objetivo    : Funcion principal de gestion de disponibilidad para ventanilla unica
Modificacion:
================================================================================
}
FUNCTION fn_disponibilidad_ventanilla_unica()

   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
          v_ruta_log        STRING,
          v_cadena          STRING


   DISPLAY "Ingresa a function principal del servicio" 
   -- se responde el servicio para pruebas

   LET gr_salida_vu_ws.nss = gr_entrada_vu_ws.nss
 
   DISPLAY "Parámetros recibidos:"
   DISPLAY "NSS          : ", gr_entrada_vu_ws.nss
   DISPLAY "RFC          : ", gr_entrada_vu_ws.RFC
   DISPLAY "CAUSAL RETIRO: ", gr_entrada_vu_ws.causal_retiro
   DISPLAY "NRP          : ", gr_entrada_vu_ws.NRP
   DISPLAY "GRUPO        : ", gr_entrada_vu_ws.grupo_ley73
   DISPLAY "MEDIO ENTREGA: ", gr_entrada_vu_ws.medio_entrega

   --------------------------------------------------------
   --Inicializa Control de VU ---RHV
    CALL fn_Inicializa_campos_control_vu()
    
    --Graba registro de Control VU ---RHV
    CALL fn_graba_registro_control_vu()
    
    -------------------------------------------------------
    
    --llamar a funcionalidad del servicio retiroSaldosDisponiblesFA            - RETWS20
    CALL fn_ejecutaWS_disponibilidadFA()
   
    --llamar a funcionalidad del servicio retiroSaldosDisponiblesLey73         - RETWS07
    CALL fn_ejecutaWS_disponibilidadLey73()

    --llamar a funcionalidad del servicio retiroSaldosDisponiblesSoloInfonavit - RETWS35
    CALL fn_ejecutaWS_disponibilidadSI() --SI = Solo Infonavit

    CALL fn_respuesta_ret_ventanilla_unica()

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_respuesta_ret_ventanilla_unica()

    LET gr_salida_vu_ws.nss                    = gr_entrada_vu_ws.nss
    LET gr_salida_vu_ws.rfc                    = gr_entrada_vu_ws.rfc
    
    LET gr_salida_vu_ws.estado_solicitud_fa    = fn_saldo_disponible_faResponse.estado_solicitud
    LET gr_salida_vu_ws.cod_rechazo_fa         = fn_saldo_disponible_faResponse.cod_rechazo
    LET gr_salida_vu_ws.des_rechazo_fa         = fn_saldo_disponible_faResponse.des_rechazo
    LET gr_salida_vu_ws.saldo_pesos_fa         = fn_saldo_disponible_faResponse.monto_pesos
    LET gr_salida_vu_ws.tanto_adicional        = fn_saldo_disponible_faResponse.monto_adicional

    LET gr_salida_vu_ws.estado_solicitud_ley73 = reg_salida_vu.estado_solicitud_ley73
    LET gr_salida_vu_ws.cod_rechazo_ley73      = acum_cod_rechazo_ley73
    LET gr_salida_vu_ws.des_rechazo_ley73      = acum_des_rechazo_ley73
    LET gr_salida_vu_ws.saldo_aivs_viv92       = acum_saldos_disp_ley73_aivs
    LET gr_salida_vu_ws.saldo_pesos_viv92      = acum_saldos_disp_ley73_pesos
    LET gr_salida_vu_ws.saldo_aivs_viv97       = fn_saldo_disponible_SIResponse.saldo_aivs
    LET gr_salida_vu_ws.saldo_pesos_viv97      = fn_saldo_disponible_SIResponse.monto_pesos
    
    LET gr_salida_vu_ws.saldo_pesos_total      = reg_salida_vu.saldo_pesos_total
    LET gr_salida_vu_ws.fecha_valuacion        = TODAY USING "YYYYMMDD"

   RETURN 

END FUNCTION

{
======================================================================
Nombre      : fn_Inicializa_campos_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Inicializa los campos para el control de 
            : ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}

FUNCTION fn_Inicializa_campos_control_vu() 

    --Inicializa Campos del Archivo de Control de VU
      
      LET gr_registro_control_vu.fecha_hora              = CURRENT YEAR TO SECOND 
      LET gr_registro_control_vu.param_nss               = gr_entrada_vu_ws.nss
      LET gr_registro_control_vu.param_rfc               = gr_entrada_vu_ws.rfc
      LET gr_registro_control_vu.param_causal_retiro     = gr_entrada_vu_ws.causal_retiro
      LET gr_registro_control_vu.param_NRP               = gr_entrada_vu_ws.NRP
      LET gr_registro_control_vu.param_grupo_ley73       = gr_entrada_vu_ws.grupo_ley73
      LET gr_registro_control_vu.param_medio_entrega     = gr_entrada_vu_ws.medio_entrega
      LET gr_registro_control_vu.bn_disponibilidad_fa    = FALSE
      LET gr_registro_control_vu.bn_disponibilidad_ley73 = FALSE
      LET gr_registro_control_vu.bn_disponibilidad_si	 = FALSE
      LET gr_registro_control_vu.st_marca_fa             = 0
      LET gr_registro_control_vu.st_marca_ley73          = 0
      LET gr_registro_control_vu.st_marca_si             = 0
      LET gr_registro_control_vu.id_solicitud_fa         = 0
      LET gr_registro_control_vu.id_solicitud_ley73      = 0
      LET gr_registro_control_vu.id_solicitud_si         = 0

END FUNCTION

{
======================================================================
Nombre      : fn_lee_registro_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Lee registro para el archivo de control de 
            : ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_lee_registro_control_vu()

   DEFINE reg_max_ctrl  smallint

   SELECT MAX(consecutivo) INTO reg_max_ctrl
     FROM ret_control_vu
    WHERE param_nss = gr_entrada_vu_ws.nss
      AND param_rfc = gr_entrada_vu_ws.rfc

   SELECT * INTO gr_registro_control_vu.*
     FROM ret_control_vu
    WHERE consecutivo = reg_max_ctrl
      AND param_nss   = gr_entrada_vu_ws.nss
      AND param_rfc   = gr_entrada_vu_ws.rfc

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

   INSERT INTO ret_control_vu 
   VALUES (gr_registro_control_vu.consecutivo,
           gr_registro_control_vu.fecha_hora,
           gr_registro_control_vu.param_nss,
           gr_registro_control_vu.param_rfc,
           gr_registro_control_vu.param_causal_retiro,
           gr_registro_control_vu.param_nrp,
           gr_registro_control_vu.param_grupo_ley73,
           gr_registro_control_vu.param_medio_entrega,
           gr_registro_control_vu.bn_disponibilidad_fa,
           gr_registro_control_vu.bn_disponibilidad_ley73,
           gr_registro_control_vu.bn_disponibilidad_si,
           gr_registro_control_vu.st_marca_fa,
           gr_registro_control_vu.st_marca_ley73,
           gr_registro_control_vu.st_marca_si,
           gr_registro_control_vu.id_solicitud_fa,
           gr_registro_control_vu.id_solicitud_ley73,
           gr_registro_control_vu.id_solicitud_si)
   
END FUNCTION

FUNCTION fn_ejecutaWS_disponibilidadFA()

    DEFINE estatus_fa           SMALLINT,
           mensaje_fa           STRING

    DISPLAY "Se llama al servicio de fondo de ahorro"

    LET fn_saldo_disponible_faRequest.nss              = gr_entrada_vu_ws.nss
    LET fn_saldo_disponible_faRequest.rfc              = gr_entrada_vu_ws.rfc
    LET fn_saldo_disponible_faRequest.causal_ret_fa    = gr_entrada_vu_ws.causal_retiro
    LET fn_saldo_disponible_faRequest.nrp              = gr_entrada_vu_ws.NRP
    LET fn_saldo_disponible_faRequest.f_inicio_pension = TODAY
    LET fn_saldo_disponible_faRequest.medio_entrega    = gr_entrada_vu_ws.medio_entrega

    CALL fn_saldo_disponible_fa_g() RETURNING estatus_fa

    DISPLAY "Estatus de la llamada : ", estatus_fa

    IF estatus_fa <> 0 THEN
       --hubo un error
       DISPLAY "SE DETECTO UN ERROR SE SUSPENDE EJECUCION EN FA"
       RETURN estatus_fa
    END IF

    IF fn_saldo_disponible_faResponse.estado_solicitud = 10 THEN
    -- SOLICITUD ACEPTADA
    -- ACTUALIZAR TABLA DE CONTROL
       LET gr_registro_control_vu.bn_disponibilidad_fa = TRUE
       DISPLAY "SE CAMBIA LA BANDERA DE DISPONIBILIDAD FA A : ",gr_registro_control_vu.bn_disponibilidad_fa
    ELSE
       -- RECHAZO
       DISPLAY "LA BANDERA DE DISPONIBILIDAD FA SE QUEDA EN FALSE : ",gr_registro_control_vu.bn_disponibilidad_fa
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

    CALL fn_Actualiza_control_vu()
    
END FUNCTION

FUNCTION fn_ejecutaWS_disponibilidadLey73()

    DEFINE estatus_ley73                SMALLINT,
           mensaje_ley73                STRING,
           indice_arr                   SMALLINT,
           x                            SMALLINT

    DISPLAY "Se llama al servicio de Ley 73"

    LET fn_ret_saldos_disponibles_ley73Request.nss           = gr_entrada_vu_ws.nss
    LET fn_ret_saldos_disponibles_ley73Request.grupo_ley73   = gr_entrada_vu_ws.grupo_ley73
    LET fn_ret_saldos_disponibles_ley73Request.medio_entrega = gr_entrada_vu_ws.medio_entrega

    LET acum_saldos_disp_ley73_aivs  = 0
    LET acum_saldos_disp_ley73_pesos = 0
    LET acum_cod_rechazo_ley73       = " "
    LET acum_des_rechazo_ley73       = " "

    CALL fn_ret_saldos_disponibles_ley73_g() RETURNING estatus_ley73

    DISPLAY "Estatus de la llamada : ", estatus_ley73

    IF estatus_ley73 <> 0 THEN
       --hubo un error
       DISPLAY "SE DETECTO UN ERROR SE SUSPENDE EJECUCION EN LEY73"
       RETURN estatus_ley73
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

          LET acum_saldos_disp_ley73_aivs  = acum_saldos_disp_ley73_aivs  + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_avis
          LET acum_saldos_disp_ley73_pesos = acum_saldos_disp_ley73_pesos + fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].monto_pesos

       END FOR

    END IF

    LET acum_cod_rechazo_ley73      = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].cod_rechazo
    LET acum_des_rechazo_ley73      = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].des_rechazo
    LET acum_estado_solicitud_ley73 = fn_ret_saldos_disponibles_ley73Response.saldo_x_retiro.element[x].estado_solicitud
    
    DISPLAY "Salida de montos por subcuenta aivs : ", acum_saldos_disp_ley73_aivs
    DISPLAY "Salida de montos por subcuenta pesos: ", acum_saldos_disp_ley73_pesos

    CALL fn_Actualiza_control_vu()
     
END FUNCTION

FUNCTION fn_ejecutaWS_disponibilidadSI()

    DEFINE estatus_si           SMALLINT,
           mensaje_si           STRING

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

    CALL fn_Actualiza_control_vu()

END FUNCTION

{
======================================================================
Nombre      : fn_Actualiza_control_vu
Creacion    : agosto -2020
Autor       : Roberto Hernandez Valerio -  Omnisys
Objetivo    : Actualiza registro para el archivo de control de 
            : ventanilla única (FA, Ley73 y Solo Infonavit)
Modificacion:
======================================================================
}
FUNCTION fn_Actualiza_control_vu()

    UPDATE ret_control_vu SET bn_disponibilidad_fa    = gr_registro_control_vu.bn_disponibilidad_fa,   
                              bn_disponibilidad_ley73 = gr_registro_control_vu.bn_disponibilidad_ley73,
                              bn_disponibilidad_si	  = gr_registro_control_vu.bn_disponibilidad_si
----
     WHERE consecutivo = gr_registro_control_vu.consecutivo 
       AND param_nss = gr_entrada_vu_ws.nss 
       AND param_rfc = gr_entrada_vu_ws.rfc
                                                                                                      
END FUNCTION
