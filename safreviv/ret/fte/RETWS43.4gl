#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS43                                                    #
#OBJETIVO        => WS PARA ORQUESTAR LLAMADOS A CREACION DE SOLICITUDES DE LOS#
#                => WS DE FA, LEY73 Y SOLO INFONAVIT                           #
#FECHA CREACION  => 27-AGOSTO-2020                                             #
#VERSION         => 1.0.0                                                      #
#MODIFICACION    =>                                                            #
################################################################################

--IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
--GLOBALS "RETG01.4gl"

--inclusion de variables de clientes ws
GLOBALS "ret_ws_crea_solicitud_fa.inc"
GLOBALS "ret_ws_crea_solicitud_ley73.inc"
GLOBALS "ret_ws_crea_solicitud_si.inc"

GLOBALS

    ----------------------------------------------------------------------------
    -- registro de entrada para el servicio
    ----------------------------------------------------------------------------
    DEFINE gr_entrada_ws             RECORD
           nss                       STRING, --CHAR(11),      -- nss del trabajador
           RFC                       STRING, --CHAR(13),      -- rfc del trabajador
           caso_crm                  STRING, --CHAR(10),      -- Numero de CRM 
           causal_retiro             STRING, --SMALLINT,      -- Causal de retiro
           NRP                       STRING, --CHAR(11),      -- Numero de registro patronal
           grupo_ley73               STRING, --SMALLINT,      -- num. de grupo al que perteneces segun retiro Ley 73
           medio_entrega             STRING, --SMALLINT,      -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
           ------------------------------------------------------------------------------------------------------------
           tipo_beneficiario         STRING, --SMALLINT,      -- Tipo de beneficiario: 1 - Trabajador
           clabe_bancaria            STRING, --CHAR(18),      -- Clabe bancaria del beneficiario
           rfc_beneficiario          STRING, --CHAR(13),      -- RFC del beneficiario
           email                     STRING, --VARCHAR(50),   -- Correo electrónico del beneficiario
           telefono                  STRING, --VARCHAR(10),   -- Teléfono del beneficiario
           tel_movil                 STRING, --VARCHAR(10),   -- Número de móvil de benficiario
           nombre                    STRING, --CHAR(40),      -- Nombre del beneficiario. Aplica sólo si el tipo de beneficiario es 2 
           ap_paterno                STRING, --CHAR(40),      -- Apellido paterno del beneficiario. Aplica sólo si el tipo de beneficiario es 2
           ap_materno                STRING, --CHAR(40),      -- Apellido materno del beneficiario. Aplica sólo si el tipo de beneficiario es 2
           entidad_federativa        STRING  --CHAR(2)        -- Entidad federativa del domicilio del beneficiario
           END RECORD

    ----------------------------------------------------------------------------
    -- registro de respuesta del servicio
    ----------------------------------------------------------------------------
    DEFINE gr_salida_ws              RECORD
           nss                       CHAR(11),      -- Número de seguridad social del trabajador
           rfc                       CHAR(13),      -- RFC del trabajador
           ---------------------------------------------------------------------
           estado_solicitud_fa       SMALLINT,      -- estado de la solicitud
           cod_rechazo_fa            SMALLINT,      -- codigo de rechazo     
           des_rechazo_fa            CHAR(100),     -- descripcion del rechazo
           saldo_pesos_fa            DECIMAL(22,2), -- saldo en pesos
           tanto_adicional_fa        DECIMAL(22,2), -- tanto adicional
           ---------------------------------------------------------------------
           estado_solicitud_ley73    SMALLINT,      -- estado de la solicitud
           cod_rechazo_ley73         SMALLINT,      -- codigo de rechazo     
           des_rechazo_ley73         CHAR(100),     -- descripcion del rechazo
           ---------------------------------------------------------------------
           saldo_aivs_viv92          DECIMAL(22,6), -- saldo en AIVs
           saldo_pesos_viv92         DECIMAL(22,2), -- saldo en pesos
           saldo_aivs_viv97          DECIMAL(22,6), -- saldo en AIVs
           saldo_pesos_viv97         DECIMAL(22,2), -- saldo en pesos
           ---------------------------------------------------------------------
           saldo_pesos_total         DECIMAL(22,2), -- saldo en pesos
           fecha_valuacion           CHAR(8)      , -- fecha de valuacion de AIVs en formato AAAAMMDD
           sello                     STRING      ,  -- Sello generado por la consulta biometrica (aplica para medio_entrega = 1 tableta)
           pdf                       BYTE           -- Acuse en formato PDF
           END RECORD
         
    DEFINE g_indice_retiro           SMALLINT -- indice del tipo de retiro consultado
    DEFINE g_id_peticion             DECIMAL(9,0) -- id de la peticion al ws

    -- codigos de rechazo del servicio
    CONSTANT GI_COD_RECHAZO_MARCA_EXITO               SMALLINT = 1,
             GI_COD_RECHAZO_MARCA_ERROR               SMALLINT = 2,
             GI_COD_RECHAZO_DATOS_INCOMPLETOS         SMALLINT = 99,
             GI_COD_RECHAZO_CAUSAL_INVALIDA           SMALLINT = 101,
             GI_COD_RECHAZO_ERROR_CREAR_SOLICITUD     SMALLINT = 102,
             GI_COD_RECHAZO_ERROR_GENERICO            SMALLINT = 999
             
    ----------------------------------------------------------------------------
    -- constantes para la evaluacion del resultado de la ejecucion del WS
    ----------------------------------------------------------------------------
    CONSTANT  g_res_procesada                    SMALLINT =  0 ,
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
     
    CONSTANT  GS_MODALIDAD_FA                    SMALLINT = 2,
              GS_MODALIDAD_LEY73                 SMALLINT = 3,
              GS_MODALIDAD_SI                    SMALLINT = 1

    DEFINE gs_desc_error_auxiliar STRING -- cadena de error auxiliar
    
    DEFINE serverURL STRING -- URL del servidor
    DEFINE v_pantalla    SMALLINT

    --VARIABLE AUXILIAR PARA GUARDAR MONTOS DE SOLO INFONAVIT
    DEFINE gr_datos_si        RECORD 
           estado_solicitud   SMALLINT,
           cod_rechazo        SMALLINT,
           des_rechazo        STRING,
           saldo_aivs         DECIMAL(22,6),
           saldo_pesos        DECIMAL(22,2)
           END RECORD


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
    SELECT ruta_listados
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
  
    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS43."
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
    IF fgl_getenv("DEBUGEFP") = "1" THEN
       CALL STARTLOG("RETWS43.log")
    ELSE
       CALL STARTLOG(v_ruta_log)
    END IF 

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_solicitud_ventanilla_unica(TRUE)
        EXIT PROGRAM
    ELSE 
      IF num_args() = 2 AND arg_val(1) = "-S" THEN
        LET v_pantalla = TRUE
        CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
        CLOSE WINDOW SCREEN
        
        -- se abre la ventana monitor del servidor (en consola)
        OPEN WINDOW w WITH FORM "RETWE043" ATTRIBUTES(TEXT = "Retiro 72-92 service") --, STYLE="naked")
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
    CALL fn_crea_solicitud_ventanilla_unica(FALSE)
    
    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor... 20140616")
    
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
              CALL ERRORLOG(g_msg_procesada)
              CALL ERRORLOG(g_msg_procesada)
             
           WHEN g_res_sin_solicitud
              CALL ERRORLOG(g_msg_sin_solicitud)
              CALL ERRORLOG(g_msg_procesada)
             
           WHEN g_res_desconectado_del_servidor
              CALL ERRORLOG(g_msg_desconectado_del_servidor)
              CALL ERRORLOG(g_msg_procesada)
              EXIT PROGRAM   # The Application server has closed the connection
             
           WHEN g_res_conexion_con_cliente_perdida
              CALL ERRORLOG(g_msg_conexion_con_cliente_perdida)
              CALL ERRORLOG(g_msg_procesada)
             
           WHEN g_res_servidor_interrumpido_ctrl_c
              CALL ERRORLOG(g_msg_servidor_interrumpido_ctrl_c)
              CALL ERRORLOG(g_msg_procesada)
             
           WHEN g_msg_error_interno
              CALL ERRORLOG(g_msg_error_interno)
              CALL ERRORLOG(g_msg_procesada)
             
           OTHERWISE 
              -- se recibio algun otro codigo de retorno
              CALL ERRORLOG("Se recibio otro codigo de retorno")
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

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_crea_solicitud_ventanilla_unica(p_generar_WSDL)

    DEFINE v_webservice         com.WebService       # WebService
    
    DEFINE op                   com.WebOperation     # Operation of a WebService
    DEFINE v_service_NameSpace  STRING -- namespace del servicio
    DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
    DEFINE v_resultado          INTEGER
    
    DISPLAY "Entra Funcion principal que crea el servicio de crea solicitud...."  --debug
    
      -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"
    
    TRY
      -- =============================
      -- se crea el servicio
      -- =============================
      LET v_webservice = com.WebService.CreateWebService("creaSolicitudVentanillaUnica", v_service_NameSpace)
      CALL v_webservice.setFeature("Soap1.1",TRUE)
      
      DISPLAY "Servicio creado...."  
    
      -- =============================
      -- Publicacion de las funciones
      -- =============================
      
      LET op = com.WebOperation.CreateDOCStyle("fn_solicitud_ventanilla_unica","fn_solicitud_ventanilla_unica",gr_entrada_ws,gr_salida_ws)
      CALL v_webservice.publishOperation(op, "fn_solicitud_ventanilla_unica")
    
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
         -- =========================
         CALL com.WebServiceEngine.RegisterService(v_webservice)  
         --display_status("Retiro 72-92 Service registrado")
         CALL ERRORLOG("Se registro el servicio Solicitud de ventanilla unica")
      END IF
      
    CATCH -- en caso de error
      DISPLAY("No se pudo crear el servicio 'Solicitud de ventanilla unica': " || STATUS)
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


#==============================================================================#
#Nombre  : fn_solicitud_ventanilla_unica                                       #
#Creacion: octubre 2020                                                        #
#Autor   : Isai Jimenez Rojas                                                  #
#Objetivo: Funcion principal del WS que crea Solicitud de ventanilla unica     #
#                                                                              #
#==============================================================================#
FUNCTION fn_solicitud_ventanilla_unica()

    DEFINE lr_control_vu         RECORD LIKE ret_control_vu.*
    DEFINE ls_estatus            SMALLINT
    DEFINE ls_error              SMALLINT
    DEFINE ls_modalidad          SMALLINT  --1=SI, 2=FA, 3=Ley73, 0=ALL

    DEFINE ls_estatus_fa         SMALLINT  --Estatus de ejecucion de servicio
    DEFINE ls_estatus_ley73      SMALLINT  --Estatus de ejecucion de servicio
    DEFINE ls_estatus_si         SMALLINT  --Estatus de ejecucion de servicio
    DEFINE ls_procesado          SMALLINT 
    
    DEFINE v_bytes_pdf           BYTE
    DEFINE ls_sello              STRING
    DEFINE ls_mensaje_integrado  STRING
    
    DEFINE ls_fecha_hora         STRING 

    DEFINE li_medio_entrega      INTEGER 
    DEFINE lr_cat_medio_entrega  RECORD LIKE ret_cat_medio_entrega.*

    
    ----------------------------------------------------------------------------
    
    DISPLAY "Parámetros recibidos:"
    DISPLAY "    NSS          :", gr_entrada_ws.nss           ,":"
    DISPLAY "    RFC          :", gr_entrada_ws.RFC           ,":"
    DISPLAY "    Caso CRM     :", gr_entrada_ws.caso_crm      ,":"
    DISPLAY "    Causal       :", gr_entrada_ws.causal_retiro ,":"
    DISPLAY "    NRP          :", gr_entrada_ws.NRP           ,":"
    DISPLAY "    Grupo Ley73  :", gr_entrada_ws.grupo_ley73   ,":"
    DISPLAY "    Medio entrega:", gr_entrada_ws.medio_entrega ,":"
   
    DISPLAY "Invoca a funcion para validación de parámetros...."
    
    CALL fn_valida_parametros() RETURNING ls_estatus
    
    IF ls_estatus != 0 THEN
       CALL fn_despliega_debug("Error en parametros recibidos")
       -- errores en parametros de entrada
       CALL fn_genera_respuesta_ws(ls_estatus, GI_COD_RECHAZO_DATOS_INCOMPLETOS, "")
       RETURN
    END IF


    -----------------------------------------------
    --RECUPERA y VALIDA ULTIMO REGISTRO DE CONROL
    -----------------------------------------------
    DISPLAY "Invoca a funcion para obtener ultimo registro de control..."
    
    CALL fn_obtiene_ultimo_control(gr_entrada_ws.nss) RETURNING lr_control_vu.*

    IF lr_control_vu.param_nss IS NULL OR lr_control_vu.param_nss = "" THEN
       --hubo algun error
       LET ls_modalidad = 0   --0=All
       CALL fn_notifica_error(ls_modalidad,"-5","Error al recuperar registro de control")
       RETURN
    END IF
    
    
    --VALIDA EXISTENCIA DE DISPONIBILIDAD
    DISPLAY "Valida existencia de disponibilidad..."
    
    IF lr_control_vu.bn_disponibilidad_FA    = FALSE AND
       lr_control_vu.bn_disponibilidad_Ley73 = FALSE AND
       lr_control_vu.bn_disponibilidad_SI    = FALSE THEN
       LET ls_error     = TRUE
       LET ls_modalidad = 0 --0=ALL
       CALL fn_notifica_error(ls_modalidad,"-5","NO SE IDENTIFICO REGISTRO DE DISPONIBILIDAD")
       RETURN
    END IF 

    --VALIDA ERRORES EN MARCAJE (si hay algun error de marcaje no se crea solicitud)
    DISPLAY "Valida existencia de marcaje...."
    
    IF lr_control_vu.st_marca_FA          < 0 OR
       lr_control_vu.bn_disponibilidad_FA < 0 OR
       lr_control_vu.bn_disponibilidad_SI < 0 THEN
       LET ls_error     = TRUE
       LET ls_modalidad = 0   --0=ALL
       CALL fn_notifica_error(ls_modalidad,"-6","NO SE IDENTIFICO REGISTRO DE MARCAJE")
       RETURN
    END IF

    
    --RECUPERA CATALOGO DE MEDIOS DE ENTREGA

    LET li_medio_entrega = gr_entrada_ws.medio_entrega
    
    SELECT *
    INTO   lr_cat_medio_entrega.* 
    FROM   ret_cat_medio_entrega
    WHERE  medio_entrega = li_medio_entrega
    
    
    --==========================================================================
    --FLUJO PRINCIPAL DE ORQUESTACION
    --==========================================================================

    --Inicializacion
  --LET gr_salida_ws.gr_salida_ws              
  --LET gr_salida_ws.nss                       
  --LET gr_salida_ws.rfc                       
  --LET gr_salida_ws.estado_solicitud_fa       
  --LET gr_salida_ws.cod_rechazo_fa            
  --LET gr_salida_ws.des_rechazo_fa            
  --LET gr_salida_ws.saldo_pesos_fa            
  --LET gr_salida_ws.tanto_adicional_fa        
  --LET gr_salida_ws.estado_solicitud_ley73    
  --LET gr_salida_ws.cod_rechazo_ley73         
  --LET gr_salida_ws.des_rechazo_ley73         
    LET gr_salida_ws.saldo_aivs_viv92  = 0 
    LET gr_salida_ws.saldo_pesos_viv92 = 0 
    LET gr_salida_ws.saldo_aivs_viv97  = 0 
    LET gr_salida_ws.saldo_pesos_viv97 = 0 
    LET gr_salida_ws.saldo_pesos_total = 0 
  --LET gr_salida_ws.fecha_valuacion           
  --LET gr_salida_ws.sello                     
  --LET gr_salida_ws.pdf                       


    LET ls_procesado = FALSE   --para indicar si hubo procesamiento de alguna modalidad
    
    ----------------------------------------------------
    --CONTROLA EJECUCION DE CREACION DE SOLICITUD FA
    ----------------------------------------------------
    DISPLAY "lr_control_vu.bn_disponibilidad_FA = ", lr_control_vu.bn_disponibilidad_FA
    DISPLAY "lr_control_vu.st_marca_FA          = ", lr_control_vu.st_marca_FA
    
    IF lr_control_vu.bn_disponibilidad_FA = TRUE AND lr_control_vu.st_marca_FA = 1 THEN
       --SE SOLICITA CREACION DE SOLICITUD DE FONDO DE AHORRO
       DISPLAY "Se procede creacion de solicitud de FA..."   --debug
       
       --Invoca funcion interna para ejecutar servicio de crea solicitud de fa
       CALL fn_crea_solicitud_FA()  RETURNING ls_estatus_fa

       IF ls_estatus_fa = 0 THEN
          LET ls_procesado = TRUE
       ELSE
          --error
          CALL fn_notifica_error(GS_MODALIDAD_FA,ls_estatus_fa,"ERROR AL CREAR SOLICITUD DE FA")
          RETURN
       END IF
    END IF
    
    ----------------------------------------------------
    --CONTROLA EJECUCION DE CREACION DE SOLICITUD LEY73
    ----------------------------------------------------
    IF lr_control_vu.bn_disponibilidad_Ley73 = TRUE AND lr_control_vu.st_marca_ley73 = 1 THEN
       --SE SOLICITA CREACION DE SOLICITUD DE LEY73
       DISPLAY "Procede creacion de solicitud de Ley73"
       
       --Invoca funcion interna para ejecutar servicio de crea solicitud de fa
       CALL fn_crea_solicitud_Ley73() RETURNING ls_estatus_ley73

       IF ls_estatus_ley73 = 0 THEN
          LET ls_procesado = TRUE
       ELSE
          --error
          CALL fn_notifica_error(GS_MODALIDAD_LEY73,ls_estatus_ley73,"ERROR AL CREAR SOLICITUD LEY73")
          
          --INVOCA MECANISMO PARA REVERSAR LA SOLICITUD DE FA (cambio de estado)
          CALL fn_reversa_crea_solicitud_FA(gr_entrada_ws.nss, lr_control_vu.id_solicitud_fa) RETURNING ls_estatus
          RETURN
       END IF
    END IF 

    ------------------------------------------------------------
    --CONTROLA EJECUCION DE CREACION DE SOLICITUD SOLO INFONAVIT
    ------------------------------------------------------------
    IF lr_control_vu.bn_disponibilidad_SI = TRUE AND lr_control_vu.st_marca_SI = 1 THEN
       --SE SOLICITA CREACION DE SOLICITUD DE SOLO INFONAVIT
       DISPLAY "Procede creacion de solicitud de Solo Infonavit"
       
       --Invoca funcion interna para ejecutar servicio de crea solicitud de fa
       CALL fn_crea_solicitud_SI() RETURNING ls_estatus_si
       
       IF ls_estatus_si = 0 THEN
          LET ls_procesado = TRUE
          --asigna datos de retorno (solo infonavit en ley73
          LET gr_salida_ws.estado_solicitud_ley73 = gr_datos_si.estado_solicitud
          LET gr_salida_ws.cod_rechazo_ley73      = gr_datos_si.cod_rechazo
          LET gr_salida_ws.des_rechazo_ley73      = gr_datos_si.des_rechazo

          --acumula montos obtenidos de solo infonavit
          LET gr_salida_ws.saldo_aivs_viv97  = gr_salida_ws.saldo_aivs_viv97  + gr_datos_si.saldo_aivs 
          LET gr_salida_ws.saldo_pesos_viv97 = gr_salida_ws.saldo_pesos_viv97 + gr_datos_si.saldo_pesos
       ELSE
          --ERROR
          LET ls_modalidad = 1 --0=all
          CALL fn_notifica_error(ls_modalidad,ls_estatus_fa,"Error al crear solicitud solo infonavit")

          --INVOCA MECANISMO PARA REVERSAR LA SOLICITUD DE FA (cambio de estado)
          CALL fn_reversa_crea_solicitud_FA(gr_entrada_ws.nss, lr_control_vu.id_solicitud_fa) RETURNING ls_estatus

          --INVOCA MECANISMO PARA REVERSAR LA SOLICITUD DE LEY73 (cambio de estado)
          CALL fn_reversa_crea_solicitud_Ley73(gr_entrada_ws.nss,3) RETURNING ls_estatus   --3=modalidad 2 (fa)
          
          RETURN
       END IF
    END IF
    
    ------------------------------------------
    --REGISTRA LOS MONTOS TOTALES A INFORMAR
    ------------------------------------------
    
    IF ls_procesado = TRUE THEN 
       --suma monto de SI a lo acumulado para subcuenta 2 de vivienda 97

       LET gr_salida_ws.saldo_pesos_total = gr_salida_ws.saldo_pesos_fa     +
                                            gr_salida_ws.tanto_adicional_fa +
                                            gr_salida_ws.saldo_pesos_viv97
       
       LET gr_salida_ws.fecha_valuacion = TODAY
       
       
       ----------------------------------------------
       --INVOCA FUNCIONALIDAD PARA OBTENCION DE SELLO
       ----------------------------------------------
       IF gr_entrada_ws.medio_entrega = 2 THEN 
            DISPLAY "Invoca a funcion para crear sello...."
       
            CALL fn_genera_sello_cadena_original_solicitud_retiro_vu(gr_entrada_ws.nss               ,
                                                                gr_entrada_ws.medio_entrega     ,
                                                                gr_entrada_ws.caso_crm          ,
                                                                lr_control_vu.id_solicitud_fa   ,
                                                                lr_control_vu.id_solicitud_ley73,
                                                                lr_control_vu.id_solicitud_si   ,
                                                                gr_salida_ws.saldo_pesos_total  )
                    RETURNING gr_salida_ws.sello
       
            LET ls_mensaje_integrado = ""
       
            ---------------------------------------
            --INVOCA A FUNCION PARA GENERAR ACUSE
            ---------------------------------------
            DISPLAY "Invoca a funcion para crear acuse...."
       
            LET ls_fecha_hora = CURRENT YEAR TO SECOND

            CALL fn_genera_acuse_solicitud_retiro_vu(gr_entrada_ws.nss               ,
                                                CURRENT YEAR TO SECOND          ,
                                                gr_entrada_ws.caso_crm          ,
                                                lr_cat_medio_entrega.descripcion,    --gr_entrada_ws.medio_entrega    ,
                                                gr_salida_ws.saldo_pesos_viv92  ,
                                                gr_salida_ws.saldo_pesos_viv97  ,
                                                gr_salida_ws.saldo_pesos_fa     ,
                                                gr_salida_ws.saldo_pesos_total  ,
                                                ls_mensaje_integrado            ,
                                                gr_salida_ws.sello) 
                                                RETURNING v_bytes_pdf
            LET gr_salida_ws.pdf = v_bytes_pdf
        ELSE
            LET gr_salida_ws.sello = null
            LET gr_salida_ws.pdf   = NULL 
        END IF 
       
    END IF

    RETURN

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_valida_parametros()

    DEFINE ls_estatus         SMALLINT
    
    LET ls_estatus = 0   --validacion exitosa

    CALL fn_despliega_debug("Ingresa a validar parametros...")

    --valida campos obligatorios
    IF gr_entrada_ws.nss                IS NULL OR
       gr_entrada_ws.caso_crm           IS NULL OR
       gr_entrada_ws.medio_entrega      IS NULL OR
       gr_entrada_ws.tipo_beneficiario  IS NULL OR
       gr_entrada_ws.clabe_bancaria     IS NULL OR
       gr_entrada_ws.entidad_federativa IS NULL THEN

       LET ls_estatus = GI_COD_RECHAZO_DATOS_INCOMPLETOS

    END IF

    RETURN ls_estatus
    
END FUNCTION 

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_obtiene_ultimo_control(pc_nss)

    DEFINE pc_nss             CHAR(11)
    DEFINE lr_control_vu      RECORD LIKE ret_control_vu.*
    
    TRY
DISPLAY "Busca registro de control para nss: ",pc_nss
       --recupera ultimo registro de control del nss proporcionado
       SELECT FIRST 1 *
       INTO   lr_control_vu.*
       FROM   ret_control_vu
       WHERE  param_nss = pc_nss
       ORDER BY consecutivo DESC
       
       IF FGL_GETENV("DEBUGEFP") = "1" THEN 
          DISPLAY "Registro de control: "
          DISPLAY "ret_control_vu.consecutivo             = ", lr_control_vu.consecutivo            
          DISPLAY "ret_control_vu.fecha_hora              = ", lr_control_vu.fecha_hora             
          DISPLAY "ret_control_vu.param_nss               = ", lr_control_vu.param_nss              
          DISPLAY "ret_control_vu.param_rfc               = ", lr_control_vu.param_rfc              
          DISPLAY "ret_control_vu.param_causal_retiro     = ", lr_control_vu.param_causal_retiro    
          DISPLAY "ret_control_vu.param_nrp               = ", lr_control_vu.param_nrp              
          DISPLAY "ret_control_vu.param_grupo_ley73       = ", lr_control_vu.param_grupo_ley73      
          DISPLAY "ret_control_vu.param_medio_entrega     = ", lr_control_vu.param_medio_entrega    
          DISPLAY "ret_control_vu.bn_disponibilidad_fa    = ", lr_control_vu.bn_disponibilidad_fa   
          DISPLAY "ret_control_vu.bn_disponibilidad_ley73 = ", lr_control_vu.bn_disponibilidad_ley73
          DISPLAY "ret_control_vu.bn_disponibilidad_si    = ", lr_control_vu.bn_disponibilidad_si   
          DISPLAY "ret_control_vu.st_marca_fa             = ", lr_control_vu.st_marca_fa            
          DISPLAY "ret_control_vu.st_marca_ley73          = ", lr_control_vu.st_marca_ley73         
          DISPLAY "ret_control_vu.st_marca_si             = ", lr_control_vu.st_marca_si            
          DISPLAY "ret_control_vu.caso_crm                = ", lr_control_vu.caso_crm               
          DISPLAY "ret_control_vu.id_solicitud_fa         = ", lr_control_vu.id_solicitud_fa        
          DISPLAY "ret_control_vu.id_solicitud_ley73      = ", lr_control_vu.id_solicitud_ley73     
          DISPLAY "ret_control_vu.id_solicitud_si         = ", lr_control_vu.id_solicitud_si        
          DISPLAY "ret_control_vu.saldo_pesos_fa          = ", lr_control_vu.saldo_pesos_fa         
          DISPLAY "ret_control_vu.tanto_adicional_fa      = ", lr_control_vu.tanto_adicional_fa     
          DISPLAY "ret_control_vu.saldo_acciones_viv92    = ", lr_control_vu.saldo_acciones_viv92   
          DISPLAY "ret_control_vu.saldo_pesos_viv92       = ", lr_control_vu.saldo_pesos_viv92      
          DISPLAY "ret_control_vu.saldo_acciones_viv97    = ", lr_control_vu.saldo_acciones_viv97   
          DISPLAY "ret_control_vu.saldo_pesos_viv97       = ", lr_control_vu.saldo_pesos_viv97      
          DISPLAY "ret_control_vu.saldo_acciones_si       = ", lr_control_vu.saldo_acciones_si      
          DISPLAY "ret_control_vu.saldo_pesos_si          = ", lr_control_vu.saldo_pesos_si         
          DISPLAY "ret_control_vu.saldo_total_pesos       = ", lr_control_vu.saldo_total_pesos      
          DISPLAY "ret_control_vu.fecha_valuacion         = ", lr_control_vu.fecha_valuacion        
       END IF 

    CATCH
       --registra los valores de retorno
       LET gr_salida_ws.nss                    = gr_entrada_ws.nss
       LET gr_salida_ws.rfc                    = NULL
       LET gr_salida_ws.estado_solicitud_fa    = NULL
       LET gr_salida_ws.cod_rechazo_fa         = NULL
       LET gr_salida_ws.des_rechazo_fa         = NULL
       LET gr_salida_ws.saldo_pesos_fa         = NULL
       LET gr_salida_ws.tanto_adicional_fa     = NULL
       LET gr_salida_ws.estado_solicitud_ley73 = NULL
       LET gr_salida_ws.cod_rechazo_ley73      = NULL
       LET gr_salida_ws.des_rechazo_ley73      = NULL
       LET gr_salida_ws.saldo_aivs_viv92       = NULL
       LET gr_salida_ws.saldo_pesos_viv92      = NULL
       LET gr_salida_ws.saldo_aivs_viv97       = NULL
       LET gr_salida_ws.saldo_pesos_viv97      = NULL
       LET gr_salida_ws.saldo_pesos_total      = NULL
       LET gr_salida_ws.fecha_valuacion        = NULL
       LET gr_salida_ws.sello                  = NULL
       LET gr_salida_ws.pdf                    = NULL

       DISPLAY "Se presento un error al consultar registro de control "

    END TRY

    RETURN lr_control_vu.*

END FUNCTION 

#==============================================================================#
# Objetivo: ejecutar ws para crear solicitud de FA                             #
#==============================================================================#
FUNCTION fn_crea_solicitud_FA()

    DEFINE ls_resultado     SMALLINT
    
    DISPLAY "Ingresa a generar solicitud de Fondo de Ahorro..."
    
    INITIALIZE fn_solicitud_fondo_ahorroRequest  TO NULL
    INITIALIZE fn_solicitud_fondo_ahorroResponse TO NULL

    -- ASIGNACION DE PARAMETROS DE ENTRADA AL WS  DE FONDO DE AHORRO
    LET fn_solicitud_fondo_ahorroRequest.nss                                            = gr_entrada_ws.nss
    LET fn_solicitud_fondo_ahorroRequest.rfc                                            = gr_entrada_ws.rfc
    LET fn_solicitud_fondo_ahorroRequest.caso_crm                                       = gr_entrada_ws.caso_crm
    LET fn_solicitud_fondo_ahorroRequest.causal_retiro                                  = gr_entrada_ws.causal_retiro
    LET fn_solicitud_fondo_ahorroRequest.nrp                                            = gr_entrada_ws.nrp
    LET fn_solicitud_fondo_ahorroRequest.f_inicio_pension                               = NULL
    LET fn_solicitud_fondo_ahorroRequest.medio_entrega                                  = gr_entrada_ws.medio_entrega
    
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].tipo_beneficiario  = gr_entrada_ws.tipo_beneficiario
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].clabe_bancaria     = gr_entrada_ws.clabe_bancaria
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].rfc                = gr_entrada_ws.rfc_beneficiario
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].email              = gr_entrada_ws.email
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].telefono           = gr_entrada_ws.telefono
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].tel_movil          = gr_entrada_ws.tel_movil
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].nombre             = gr_entrada_ws.nombre
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].ap_paterno         = gr_entrada_ws.ap_paterno
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].ap_materno         = gr_entrada_ws.ap_materno
    LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].entidad_federativa = gr_entrada_ws.entidad_federativa

  
    --SE INVOCA FUNCION PRINCIPAL DE CLIENTE PARA CREAR SOLICIUD DE FONDO DE AHORRO
DISPLAY "Llama a funcion de ws para crear solicitud..."
    CALL fn_solicitud_fondo_ahorro_g() RETURNING ls_resultado
DISPLAY "Regresa de funcion de ws para crear solicitud (ls_resultado:  ",ls_resultado

    IF ls_resultado = 0 THEN
       LET gr_salida_ws.nss                 = gr_entrada_ws.nss
       LET gr_salida_ws.rfc                 = gr_entrada_ws.rfc
       -----------------------------------------------------------------
       LET gr_salida_ws.estado_solicitud_fa = fn_solicitud_fondo_ahorroResponse.estado_solicitud
       LET gr_salida_ws.cod_rechazo_fa      = fn_solicitud_fondo_ahorroResponse.cod_rechazo
       LET gr_salida_ws.des_rechazo_fa      = fn_solicitud_fondo_ahorroResponse.des_rechazo
       LET gr_salida_ws.saldo_pesos_fa      = fn_solicitud_fondo_ahorroResponse.monto_pesos
       LET gr_salida_ws.tanto_adicional_fa  = fn_solicitud_fondo_ahorroResponse.monto_adicional

    END IF

    RETURN ls_resultado

END FUNCTION

#==============================================================================#
# Objetivo: ejecutar ws para crear solicitud de Ley73                          #
#==============================================================================#
FUNCTION fn_crea_solicitud_Ley73()

    DEFINE ls_resultado            SMALLINT
    DEFINE ls_indice               SMALLINT
    DEFINE ld_monto_aivs           DECIMAL(22,6)
    DEFINE ld_monto_pesos          DECIMAL(22,2)
    DEFINE ls_estado_solicitud     SMALLINT
    DEFINE ls_cod_rechazo          SMALLINT
    DEFINE ls_des_rechazo          STRING
    
    DISPLAY "\tIngresa a generar solicitud de Fondo de Ley73..."
    
    INITIALIZE fn_ret_generico_solicitud_ley73Request  TO NULL
    INITIALIZE fn_ret_generico_solicitud_ley73Response TO NULL

    -- ASIGNACION DE DATOS DE ENTRADA AL SERVICIO DE LEY73
    LET fn_ret_generico_solicitud_ley73Request.nss                                            = gr_entrada_ws.nss
    LET fn_ret_generico_solicitud_ley73Request.caso_adai                                      = gr_entrada_ws.caso_crm
    LET fn_ret_generico_solicitud_ley73Request.grupo                                          = gr_entrada_ws.grupo_ley73
    LET fn_ret_generico_solicitud_ley73Request.medio_entrega                                  = gr_entrada_ws.medio_entrega
    LET fn_ret_generico_solicitud_ley73Request.sello                                          = NULL

    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].tipo_beneficiario  = gr_entrada_ws.tipo_beneficiario
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].clabe_bancaria     = gr_entrada_ws.clabe_bancaria
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].rfc                = gr_entrada_ws.rfc_beneficiario
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].email              = gr_entrada_ws.email
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].telefono           = gr_entrada_ws.telefono
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].tel_movil          = gr_entrada_ws.tel_movil
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].nombre             = gr_entrada_ws.nombre
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].ap_paterno         = gr_entrada_ws.ap_paterno
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].ap_materno         = gr_entrada_ws.ap_materno
    LET fn_ret_generico_solicitud_ley73Request.arr_beneficiario.element[1].entidad_federativa = gr_entrada_ws.entidad_federativa

  
    --SE INVOCA FUNCION DE CLIENTE PARA CREAR SOLICIUD DE LEY73
    CALL fn_ret_generico_solicitud_ley73_g() RETURNING ls_resultado

    IF ls_resultado = 0 THEN
       --inicia ciclo para recuperar los datos correspondientes
       FOR ls_indice = 1 TO fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element.getLength()

           --guarda en temporal datos de la modalidad
           LET ld_monto_aivs       = fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].monto_avis
           LET ld_monto_pesos      = fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].monto_pesos
           LET ls_estado_solicitud = fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].estado_solicitud
           LET ls_cod_rechazo      = fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].cod_rechazo
           LET ls_des_rechazo      = fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].des_rechazo
           
           --dependiendo de la subcuenta realiza el acumulado
           CASE fn_ret_generico_solicitud_ley73Response.arr_modalidad_retiro.element[ls_indice].subcuenta
                --acumula vivienda 97
                WHEN 4
                      LET gr_salida_ws.saldo_aivs_viv97  = gr_salida_ws.saldo_aivs_viv97  + ld_monto_aivs
                      LET gr_salida_ws.saldo_pesos_viv97 = gr_salida_ws.saldo_pesos_viv97 + ld_monto_pesos
                --acumula vivienda 92
                WHEN 8
                      LET gr_salida_ws.saldo_aivs_viv92  = gr_salida_ws.saldo_aivs_viv92  + ld_monto_aivs
                      LET gr_salida_ws.saldo_pesos_viv92 = gr_salida_ws.saldo_pesos_viv92 + ld_monto_pesos
           END CASE

       END FOR

       --toma estado, codigo rechazo y desc de rechazo del ultimo registro
       LET gr_salida_ws.estado_solicitud_ley73 = ls_estado_solicitud
       LET gr_salida_ws.cod_rechazo_ley73      = ls_cod_rechazo
       LET gr_salida_ws.des_rechazo_ley73      = ls_des_rechazo

    END IF

    RETURN ls_resultado

END FUNCTION

#==============================================================================#
# Objetivo: ejecutar ws para crear solicitud de Solo Infonavit                 #
#==============================================================================#
FUNCTION fn_crea_solicitud_SI()

    DEFINE ls_resultado     SMALLINT
    
    DISPLAY "Ingresa a generar solicitud de Solo Infonavit..."
    
    
    INITIALIZE fn_ret_solicitud_SIRequest  TO NULL    --variable de entrada cliente
    INITIALIZE fn_ret_solicitud_SIResponse TO NULL    --variable de salida cliente

    -- ASIGNACION DE PARAMETROS DE ENTRADA AL SERVIVIO DE SOLO INFONAVIT
    LET fn_ret_solicitud_SIRequest.nss                                            = gr_entrada_ws.nss
    LET fn_ret_solicitud_SIRequest.caso_crm                                       = gr_entrada_ws.caso_crm
    LET fn_ret_solicitud_SIRequest.causal_retiro                                  = gr_entrada_ws.causal_retiro
    LET fn_ret_solicitud_SIRequest.medio_entrega                                  = gr_entrada_ws.medio_entrega
    
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].tipo_beneficiario  = gr_entrada_ws.tipo_beneficiario
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].clabe_bancaria     = gr_entrada_ws.clabe_bancaria
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].rfc                = gr_entrada_ws.rfc_beneficiario
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].email              = gr_entrada_ws.email
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].telefono           = gr_entrada_ws.telefono
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].tel_movil          = gr_entrada_ws.tel_movil
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].nombre             = gr_entrada_ws.nombre
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].ap_paterno         = gr_entrada_ws.ap_paterno
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].ap_materno         = gr_entrada_ws.ap_materno
    LET fn_ret_solicitud_SIRequest.arr_beneficiario.element[1].entidad_federativa = gr_entrada_ws.entidad_federativa

  
    --SE INVOCA FUNCION PRINCIPAL DE CLIENTE PARA CREAR SOLICIUD SOLO INFONAVIT

DISPLAY "Llama a funcion de ws para crear solicitud SI..."
    CALL fn_ret_solicitud_SI_g() RETURNING ls_resultado
DISPLAY "Regresa de funcion de ws para crear solicitud SI - ls_resultado:  ",ls_resultado

    IF ls_resultado = 0 THEN

       LET gr_datos_si.estado_solicitud = fn_ret_solicitud_SIResponse.estado_solicitud
       LET gr_datos_si.cod_rechazo      = fn_ret_solicitud_SIResponse.cod_rechazo
       LET gr_datos_si.des_rechazo      = fn_ret_solicitud_SIResponse.des_rechazo
       LET gr_datos_si.saldo_aivs       = fn_ret_solicitud_SIResponse.saldo_aivs
       LET gr_datos_si.saldo_pesos      = fn_ret_solicitud_SIResponse.saldo_pesos

    END IF

    RETURN ls_resultado

END FUNCTION


#==============================================================================#
# Objetivo: reversar creacion de solicitud FA                                  #
#==============================================================================#
FUNCTION fn_reversa_crea_solicitud_FA(pc_nss, pi_id_solicitud)

    DEFINE pc_nss                  CHAR(11)
    DEFINE pi_id_solicitud         INTEGER
    DEFINE ls_resultado            SMALLINT

DISPLAY "Ingresa a reversar solicitud FA creada..."

    LET ls_resultado = 0 --retroalimentacion de estado de ejecucion
    
    --recupera el id de la solicitud (susutuir cuando se reciba por parametro)
    SELECT id_solicitud
    INTO   pi_id_solicitud
    FROM   ret_solicitud_generico
    WHERE  nss              = pc_nss
    AND    modalidad_retiro = GS_MODALIDAD_FA
    AND    estado_solicitud = 10  --(10=capturada) checar
   
    --CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud    
    -- INSERT INTO ret_fondo_ahorro_generico VALUES ( r_ret_fondo_ahorro_generico.*)
    -- INSERT INTO ret_ws_peticion_crea_solicitud VALUES ( v_r_ret_ws_peticion_crea_solicitud.* )
    -- INSERT INTO ret_ws_det_peticion_crea_solicitud_resp VALUES ( v_r_ret_ws_det_peticion_crea_solicitud_resp.* )
    -- INSERT INTO ret_ws_peticion_crea_sol_benef VALUES ( v_r_ret_ws_peticion_crea_sol_benef.* )
    -- 
    --se retorna a precapturada
    UPDATE ret_solicitud_generico
    SET    estado_solicitud = 8    --regresa a precapturada
    WHERE  id_solicitud     = pi_id_solicitud
    AND    modalidad_retiro = 2

    DELETE FROM ret_fondo_ahorro_generico
    WHERE  id_solicitud     = pi_id_solicitud
    AND    estado_solicitud = 10

    RETURN ls_resultado

END FUNCTION


#==============================================================================#
# Objetivo: reversar creacion de solicittud Ley73                              #
#==============================================================================#
FUNCTION fn_reversa_crea_solicitud_Ley73(pc_nss, pi_id_solicitud)

    DEFINE pc_nss                  CHAR(11)
    DEFINE pi_id_solicitud         INTEGER
    
    DEFINE ls_resultado            SMALLINT

DISPLAY "Ingresa a reversar solicitud Ley73 creada..."

    LET ls_resultado = 0 --retroalimentacion de estado de ejecucion
    
    --recupera el id de la solicitud (susutuir cuando se reciba por parametro)
    SELECT id_solicitud
    INTO   pi_id_solicitud
    FROM   ret_solicitud_generico
    WHERE  nss              = pc_nss
    AND    modalidad_retiro = GS_MODALIDAD_LEY73
    AND    estado_solicitud = 10  --(10=capturada) checar
    
    --se retorna a precapturada
    UPDATE ret_solicitud_generico
    SET    estado_solicitud = 8   --regresa a precapturada
    WHERE  id_solicitud     = pi_id_solicitud
    AND    modalidad_retiro = GS_MODALIDAD_LEY73

    DELETE FROM ret_ley73_generico
    WHERE  id_solicitud     = pi_id_solicitud
    AND    estado_solicitud = 10 
    
    RETURN ls_resultado

END FUNCTION


#==============================================================================#
# Objetivo: ejecutar reverso de creacion de solicitud de Solo Infonavit        #
# No aplica porque en caso de no poderse crear no habria nada que reversar     #
#==============================================================================#
FUNCTION fn_reversa_crea_solicitud_SI()

    DEFINE ls_resultado     SMALLINT

    RETURN ls_resultado

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_notifica_error(ps_modalidad, ps_codigo_error, ps_mensaje)

    DEFINE ps_modalidad       STRING 
    DEFINE ps_codigo_error    SMALLINT
    DEFINE ps_mensaje         STRING 
   
    LET gr_salida_ws.nss                       = gr_entrada_ws.nss
    LET gr_salida_ws.rfc                       = gr_entrada_ws.rfc
    LET gr_salida_ws.estado_solicitud_fa       = "0"
    LET gr_salida_ws.cod_rechazo_fa            = "0"
    LET gr_salida_ws.des_rechazo_fa            = ""
    LET gr_salida_ws.saldo_pesos_fa            = "0"
    LET gr_salida_ws.tanto_adicional_fa        = "0"
    LET gr_salida_ws.estado_solicitud_ley73    = "0"
    LET gr_salida_ws.cod_rechazo_ley73         = "0"
    LET gr_salida_ws.des_rechazo_ley73         = ""
    LET gr_salida_ws.saldo_aivs_viv92          = "0"
    LET gr_salida_ws.saldo_pesos_viv92         = "0"
    LET gr_salida_ws.saldo_aivs_viv97          = "0"
    LET gr_salida_ws.saldo_pesos_viv97         = "0"
    LET gr_salida_ws.saldo_pesos_total         = "0"
    LET gr_salida_ws.fecha_valuacion           = TODAY USING "YYYYMMDD"
    LET gr_salida_ws.sello                     = NULL
    LET gr_salida_ws.pdf                       = ""
    
    CASE ps_modalidad 
         WHEN 2
              LET gr_salida_ws.estado_solicitud_fa       = "100"
              LET gr_salida_ws.cod_rechazo_fa            = ps_codigo_error
              LET gr_salida_ws.des_rechazo_fa            = ps_mensaje
         WHEN 3
              LET gr_salida_ws.estado_solicitud_ley73    = "100"
              LET gr_salida_ws.cod_rechazo_ley73         = ps_codigo_error
              LET gr_salida_ws.des_rechazo_ley73         = ps_mensaje
         OTHERWISE
              LET gr_salida_ws.estado_solicitud_fa       = "100"
              LET gr_salida_ws.cod_rechazo_fa            = ps_codigo_error
              LET gr_salida_ws.des_rechazo_fa            = ps_mensaje
              LET gr_salida_ws.estado_solicitud_ley73    = "100"
              LET gr_salida_ws.cod_rechazo_ley73         = ps_codigo_error
              LET gr_salida_ws.des_rechazo_ley73         = ps_mensaje
    END CASE

    DISPLAY sfmt("Error %1 - %2",ps_codigo_error, ps_mensaje)
   
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

    DISPLAY "EP,EM,DR: ", p_estatus_peticion, " - ", p_estatus_marca, " - ", p_desc_rechazo
    
    -- si hubo error
    IF ( p_estatus_peticion <> 0 ) THEN
      
        LET gr_salida_ws.nss                    = gr_entrada_ws.nss
        LET gr_salida_ws.cod_rechazo_fa         = p_estatus_peticion
        LET gr_salida_ws.cod_rechazo_ley73      = p_estatus_peticion
        LET gr_salida_ws.estado_solicitud_fa    = 100
        LET gr_salida_ws.estado_solicitud_ley73 = 100

        IF ( NOT p_desc_rechazo.equals("") ) THEN
            LET gr_salida_ws.des_rechazo_fa    = p_desc_rechazo
            LET gr_salida_ws.des_rechazo_ley73 = p_desc_rechazo
        ELSE
            LET gr_salida_ws.des_rechazo_fa    = fn_desc_error(p_estatus_peticion)
            LET gr_salida_ws.des_rechazo_ley73 = fn_desc_error(p_estatus_peticion)
        END IF
        

        LET gr_salida_ws.saldo_pesos_fa        = 0
        LET gr_salida_ws.tanto_adicional_fa    = 0
        LET gr_salida_ws.saldo_aivs_viv92      = 0
        LET gr_salida_ws.saldo_pesos_viv92     = 0
        LET gr_salida_ws.saldo_aivs_viv97      = 0
        LET gr_salida_ws.saldo_pesos_viv97     = 0
        LET gr_salida_ws.saldo_pesos_total     = 0
    ELSE
        -- Retorna los datos registrados
        LET gr_salida_ws.nss                    = gr_entrada_ws.nss
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
    IF ( gr_salida_ws.des_rechazo_fa IS NOT NULL AND gr_salida_ws.des_rechazo_fa <> "" ) THEN
       DISPLAY "vpd aqui"
       RETURN gr_salida_ws.des_rechazo_fa
    END IF
    
    CASE p_codigo_error
       WHEN 0
            LET v_desc_error = "Correcto"

       WHEN GI_COD_RECHAZO_DATOS_INCOMPLETOS
          --LET v_desc_error = "El NSS, el caso CRM y el medio de entrega son requeridos"
            LET v_desc_error = "El NSS, Caso CRM, medio de entrega, Tp Beneficiaio, CLABE y entidad federativa son requeridos"

       WHEN GI_COD_RECHAZO_CAUSAL_INVALIDA
            LET v_desc_error = "Causal de retiro inválida"

       WHEN GI_COD_RECHAZO_ERROR_CREAR_SOLICITUD
            LET v_desc_error = "Error interno al intentar crear solicitud"

       WHEN GI_COD_RECHAZO_ERROR_GENERICO
            LET v_desc_error = gs_desc_error_auxiliar
       
       OTHERWISE
           LET v_desc_error = "Error no identificado"
    END CASE

    RETURN v_desc_error

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_despliega_debug(ps_mensaje)
   
    DEFINE ps_mensaje      STRING
    
    IF FGL_GETENV("DEBUGEFP") THEN 
       DISPLAY ps_mensaje
    END IF
    
END FUNCTION 
