--==============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 09-jul-20
--==============================================================================
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       =>                                                          #
--------------------------------------------------------------------------------
#MODULO            => RET                                                      #
#PROGRAMA          => RETWS35 (Base RETWS20)                                   #
#OBJETIVO          => WS CONSULTA DE SALDOS DISPONIBLES PARA RETIRO DE SOLO    #
#                     INFONAVIT                                                #
#FECHA INICIO      => 24-JUL-2020                                              #
################################################################################

IMPORT FGL WSHelper
IMPORT com
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"

GLOBALS

    -- registro de entrada
    DEFINE ws_ret_cons_saldos_disponibles_in RECORD
           nss                STRING, -- nss del trabajador
           causal_retiro      STRING, -- causal del retiro
           nrp                STRING, -- 
           medio_entrega      STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
           END RECORD
           
    -- registro de respuesta
    DEFINE ws_ret_cons_saldos_disponibles_out  RECORD
           nss                STRING, -- Número de seguridad social del trabajador
           estado_solicitud   STRING, -- estado de la solicitud
           cod_rechazo        STRING, -- codigo de rechazo
           des_rechazo        STRING, -- descripcion del rechazo 
           fecha_valuacion    STRING, -- fecha valor
           saldo_aivs         STRING, --
           monto_pesos        STRING  -- saldo en pesos equivalente a AIVs por valor accion
           END RECORD
         
    -- =======================================================
    -- constantes para la evaluacion del resultado de la ejecucion del webservice
    CONSTANT  g_res_procesada                    SMALLINT = 0  ,
              g_res_sin_solicitud                SMALLINT = -1 ,
              g_res_desconectado_del_servidor    SMALLINT = -2 ,
              g_res_conexion_con_cliente_perdida SMALLINT = -3 ,
              g_res_servidor_interrumpido_ctrl_c SMALLINT = -4 ,
              g_res_error_interno                SMALLINT = -10,
              g_msg_procesada                    STRING   = "Solicitud procesada"                  ,
              g_msg_sin_solicitud                STRING   = "Sin solicitud"                        ,
              g_msg_desconectado_del_servidor    STRING   = "Desconectado del servidor"            ,
              g_msg_conexion_con_cliente_perdida STRING   = "Se perdió la conexión con el cliente" ,
              g_msg_servidor_interrumpido_ctrl_c STRING   = "Se interrumpió el servidor con CTRL-C",
              g_msg_error_interno                STRING   = "Ocurrió un error interno"
    CONSTANT  g_marca_cod                        SMALLINT = 801
         
    DEFINE serverURL STRING -- URL del servidor
    DEFINE v_pantalla    SMALLINT

    
END GLOBALS

#
# MAIN
#
#==============================================================================#
#                                                                              #
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS35."
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

    IF FGL_GETENV("DEBUGDUMMY") = "1" THEN 
       DISPLAY "EJECUCION DE SERVICIO DUMMY"         
       CALL STARTLOG("C:\\TMP\\RETWS37.log")         
       CALL ERRORLOG("EJECUCION DE SERVICIO DUMMY")  
    ELSE
        CALL STARTLOG(v_ruta_log)
    END IF

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_solo_infonavit(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro 72-92 service") --, STYLE="naked")
        ELSE
            IF num_args() <> 0 THEN
                CALL exitHelp()
                EXIT PROGRAM
            END IF
        END IF
    END IF
  
    
    CALL ERRORLOG("invoca creacion de servicio Retiro")
    DISPLAY "INVOCA CREACION DE SERVICIO RETIRO DISPONIBILIDAD SOLO INFONAVIT v1.22.12.55"  --debug
    
    -- se crea el servicio
    CALL fn_crea_servicio_solo_infonavit(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Disponibilidad Solo Infonavit ...")

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

#==============================================================================#
#Nombre: fn_crea_servicio_solo_infonavit                                       #
#Fecha creacion: Julio 2020                                                    #
#Autor: Isai Jimenez Rojas                                                     #
#Objetivo: Genera el servicio web de retiro del Solo Infonavit que consulta    #
#          los saldos disponibles                                              #
#Modificacion:                                                                 #
#                                                                              #
#==============================================================================#
FUNCTION fn_crea_servicio_solo_infonavit(p_generar_WSDL)
    
    DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
    DEFINE v_webservice         com.WebService       # WebService
    DEFINE op                   com.WebOperation     # Operation of a WebService
    DEFINE v_service_NameSpace  STRING -- namespace del servicio
    DEFINE v_resultado          INTEGER
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroSaldosDisponiblesSI", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_saldo_disponible_SI","fn_saldo_disponible_SI",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_saldo_disponible_SI")
        CALL v_webservice.publishOperation(op, "fn_saldo_disponible_SI")

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
            CALL ERRORLOG("Se registro el servicio consulta de saldos disponibles para retiro")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de saldos disponibles para retiro': " || STATUS)
            EXIT PROGRAM
    END TRY

END FUNCTION


{
================================================================================
Nombre  : fn_saldo_disponible_SI    (Funcion principal del servicio)
Creacion: julio 2020
Autor   : Isai Jimenez Rojas, Omnisys
Objetivo: Funcion principal del WS que consulta los saldos disponibles para 
          retiros Solo Infonavit
Modificaciones:
================================================================================
}
FUNCTION fn_saldo_disponible_SI()

    DEFINE v_nss                     CHAR(11),
           v_saldo_aivs              DECIMAL(26,6), -- saldo del derechohabiente
           v_saldo_pesos             DECIMAL(22,2), -- saldo del derechohabiente
           v_conteo                  SMALLINT
    DEFINE v_f_valuacion             DATE 

    --recupera parametros de entrada (in)
    LET v_nss = ws_ret_cons_saldos_disponibles_in.nss
    
    -- se responde el servicio para pruebas
    LET ws_ret_cons_saldos_disponibles_out.nss = v_nss

    IF FGL_GETENV("DEBUGDUMMY") = "1" AND v_nss = "00000000000" THEN
        DISPLAY "SE EJECUTA FUNCION DUMMY"
        CALL ERRORLOG("SE EJECUTA FUNCION DUMMY...")
        CALL fn_saldo_dummy()
        RETURN
    END IF
    
    DISPLAY "Validando Solo Infonavit"
    DISPLAY "Parametros de entrada " 
    DISPLAY "NSS                     : ", v_nss

    --VALIDA CAUSAL DE RETIRO
    IF ws_ret_cons_saldos_disponibles_in.causal_retiro != "1" AND
       ws_ret_cons_saldos_disponibles_in.causal_retiro != "2" AND
       ws_ret_cons_saldos_disponibles_in.causal_retiro != "3" THEN
       DISPLAY "CAUSAL DE RETIRO INVALIDO..."
       
       --SE RECHAZA POR CAUSAL INVALIDO
       CALL fn_rechazo_solo_infonavit(v_nss,
                                      gi_solicitud_rechazada,
                                      gi_causal_retiro_invalido,
                                      "CAUSAL DE RETIRO INVALIDA")
    ELSE
       IF ws_ret_cons_saldos_disponibles_in.medio_entrega = 6 THEN
          DISPLAY "Debe existir la premarca para poder devolver el saldo"
       
          -- busca al derechohabiente en la tabla ret_fondo_ahorro con estatus en tramite
          SELECT COUNT(*)
          INTO   v_conteo
          FROM   ret_solicitud_generico
          WHERE  nss              = v_nss
          AND    modalidad_retiro = 1 --2
          AND    estado_solicitud = 8
          
          IF ( v_conteo = 1 ) THEN
              --recupera saldo
             CALL fn_recupera_saldo_solo_infonavit(v_nss) RETURNING v_saldo_aivs, v_saldo_pesos, v_f_valuacion
             
             LET ws_ret_cons_saldos_disponibles_out.nss              = v_nss
             LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = 10 --- Aceptada
             LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = 0  --- Sin código de rechazo
             LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = "" --- Sin descripción de rechazo
             LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = v_saldo_aivs
             LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = v_saldo_pesos
          ELSE 
             CALL ERRORLOG("Se rechaza porque debe existe otra solicitud en estado 8 para devolver el saldo")
             DISPLAY "YA EXISTE UNA SOLICITUD EN ESTADO 8" --debug
             
             -- se responde al WS que se tiene una solicitud en tramite
             CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0)
          END IF         
       ELSE 
          --- Valida el nss
          IF v_nss IS NULL THEN
             DISPLAY "NSS INVALIDO..."
       
             --SE RECHAZA POR CAUSAL INVALIDO
             CALL fn_rechazo_solo_infonavit(v_nss,
                                            gi_solicitud_rechazada,
                                            gi_nss_es_necesario,
                                            "NSS REQUERIDO ")
          ELSE 
             --rutina principal de disponibilidad
             CALL fn_ret_disponibilidad_solo_infonavit(v_nss,
                                                       ws_ret_cons_saldos_disponibles_in.causal_retiro,
                                                       ws_ret_cons_saldos_disponibles_in.medio_entrega,
                                                       TRUE)
          END IF 
       END IF 
    END IF 
       
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_rechazo_solo_infonavit(p_nss, p_estado_solicitud, p_cod_rechazo, p_des_rechazo)

    DEFINE p_nss              STRING -- NSS
    DEFINE p_estado_solicitud string -- tado de la solicitud
    DEFINE p_cod_rechazo      STRING -- codigo de rechazo
    DEFINE p_des_rechazo      STRING -- descripcion del rechazo

    LET ws_ret_cons_saldos_disponibles_out.nss              = p_nss
    LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = gi_solicitud_rechazada
    LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = p_des_rechazo
    LET ws_ret_cons_saldos_disponibles_out.fecha_valuacion  = NULL
    LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = 0
    LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = 0

END FUNCTION


#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_recupera_saldo_solo_infonavit(p_nss)

    DEFINE p_nss                LIKE afi_derechohabiente.nss,
           v_subcuenta          SMALLINT,
           v_f_valuacion        DATE, -- fecha de valuacion
           v_saldo_aivs         DECIMAL(24,6),
           v_saldo_pesos        DECIMAL(22,2), 
           v_resultado_consulta SMALLINT,
           v_sql                STRING

    -- se ejecuta el SP de consulta de saldo
    LET v_sql         = "EXECUTE FUNCTION fn_saldo_dia(?,NULL,?,?)"
    LET v_subcuenta   = 44
    LET v_f_valuacion = TODAY
   
    -- se ejecuta la consulta de saldo
    PREPARE sid_saldo FROM v_sql
    EXECUTE sid_saldo USING p_nss, v_subcuenta, v_f_valuacion
                     INTO v_resultado_consulta, v_saldo_aivs, v_saldo_pesos

   -- se devuelve el resultado de la consulta
   RETURN v_saldo_aivs, v_saldo_pesos, v_f_valuacion

END FUNCTION

{
================================================================================
Nombre  : fn_ret_disponibilidad_solo_infonavit
Creacion: Julio 2020
Autor   : Isai Jimenez Rojas, Omnisys
Objetivo: Verifica si un derechohabiente tiene disponibilidad de retiro de 
          su cuenta de Solo Infonavit
Modificaciones:
================================================================================
}
FUNCTION fn_ret_disponibilidad_solo_infonavit(p_nss, 
                                              p_causal_retiro,
                                              p_medio_entrega,
                                              p_es_consulta)

    DEFINE p_nss                  CHAR(11),    -- NSS
           p_causal_retiro        SMALLINT,    -- Causal de retiro 
           p_medio_entrega        SMALLINT,    -- Medio por el cual se hizo el llamado  
           p_es_consulta          SMALLINT     -- booleana que indica si es una consulta o inicio de tramite
    
    DEFINE v_conteo_nss           SMALLINT   ,
           v_ruta_log             STRING,
           v_saldo_aivs           DECIMAL(24,6),-- saldo del derechohabiente
           v_monto_pesos          DECIMAL(22,2),-- saldo del derechohabiente
           v_tipo_credito         SMALLINT,
           v_tipo_originacion     SMALLINT

    DEFINE v_f_inicio_pension     DATE      -- fecha de inicio de pension en el SPESS
    DEFINE v_f_resolucion         DATE
    DEFINE v_tpo_pension          CHAR(2)   -- Tipo de pension de la resolucion
    DEFINE v_tpo_prestacion       CHAR(2)   -- Tipo de prestacion de la resolucion
    DEFINE v_tpo_seguro           CHAR(2)   -- Tipo de seguro de la resolucion
    DEFINE v_tiene_spess          SMALLINT
    DEFINE v_regimen              SMALLINT
    DEFINE v_f_valuacion          DATE
    DEFINE v_porcentaje_valuacion SMALLINT

    DEFINE v_id_datamart          LIKE ret_datamart.id_datamart,
           v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
           v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
           v_pesos_viv92          DECIMAL(22,2), -- saldo pesos de viv92
           v_pesos_viv97          DECIMAL(22,2), -- saldo pesos de viv97
           v_resultado            SMALLINT,      -- resultado de la consulta
           v_cant_matriz_derechos SMALLINT,      -- Contador para las ocurrencias de registros en la matriz de derechos
           v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
           v_n_referencia         LIKE sfr_marca_activa.n_referencia, -- para validar marca
           v_sdo_tot_aivs         DECIMAL(24,6),  -- saldo total para respuesta aivs (viv92 + viv97)
           v_sdo_tot_pesos        DECIMAL(22,2)   -- saldo total para respuesta pesos (viv92 + viv97)

    ----------------------------------------------------------------------------

    CALL ERRORLOG("Ingresa a funcion para obtener disponibilidad")

    ---------------------------------------------------------------
    -- para validar el NSS se verifica que exista al menos una vez
    ---------------------------------------------------------------
    SELECT COUNT(*)
    INTO   v_conteo_nss 
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa
    
    IF v_conteo_nss = 0 THEN
       -- No existe el nss
       CALL ERRORLOG("No existe el NSS")  
       DISPLAY "NO EXISTE NSS"
       CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
       RETURN
    ELSE
       -- si se encuentra mas de uno, no procede su solicitud
       IF ( v_conteo_nss > 1 ) THEN
          CALL ERRORLOG("El NSS devuelve más de un registro")
          CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_mas_de_un_registro, 0)
          RETURN
       END IF
    END IF 

  
    DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
    DISPLAY "NSS evaluado: ", p_nss

    LET v_conteo_nss = 0

    CALL ERRORLOG("Validando solicitud para NSS: " || p_nss)

    -- se valida si existe otra solicitud en tramite
    IF ( fn_rechazo_por_tramite_solo_infonavit(p_nss) ) THEN
       CALL ERRORLOG("Se rechaza porque existe otra solicitud en tramite para el mismo NSS")
       DISPLAY "SE RECHAZA PORQUE EXISTE OTRA SOLICITUD EN TRAMITE PARA EL MISMO NSS"
       -- se responde al WS que se tiene una solicitud en tramite
       CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0)
       RETURN 
    END IF
   
    -- se verifica si el derechohabiente tiene un credito vigente
    IF ( fn_trabajador_credito_vigente(p_nss) ) THEN
       DISPLAY "TRABAJADOR CON CREDITO VIGENTE"
       -- se responde negativo por tener un credito
       CALL fn_trabajador_tipo_credito(p_nss) RETURNING v_tipo_credito, v_tipo_originacion

       IF v_tipo_credito = 1 AND v_tipo_originacion = 1 THEN -- 1-Credito Tradicional, 1-Transferencia de Acreditados
          CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_tiene_credito_vigente, 0)
          RETURN
       END IF

    END IF
    
    ---------------------------------
    -- OBTIENE SALDO SOLO INFONAVIT
    ---------------------------------
    
    DISPLAY "RECUPERA SALDO SOLO INFONAVIT..."
        
    CALL fn_recupera_saldo_solo_infonavit(p_nss) RETURNING v_saldo_aivs, v_monto_pesos, v_f_valuacion
    
    IF ( v_saldo_aivs <= 0 ) THEN
       -- solicitud rechazada por no contar con saldo suficiente
       CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_sin_saldo, 0)
       RETURN
    END IF
    
    -----------------
    --VALINDO MARCA
    -----------------
    CALL ERRORLOG("Verificando marca")
    DISPLAY "VERIFICANDO MARCA"

    -- se verifica la marca
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente 
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa

    CALL ERRORLOG("ID_DERECHOHABIENTE encontrado: " || v_id_derechohabiente)
    
    SELECT n_referencia
    INTO   v_n_referencia
    FROM   sfr_marca_activa
    WHERE  id_derechohabiente = v_id_derechohabiente
    AND    marca              = g_marca_cod   --801kjkj
    
    -- si aparece, se rechaza
    IF ( v_n_referencia IS NOT NULL ) THEN
       CALL ERRORLOG("Marca con referencia: " || v_n_referencia)
       -- se rechaza por estar en tramite
       CALL fn_respuesta_ws_solo_infonavit(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0)
       RETURN
    END IF
    
    ----------------------
    --VALIDA RESOLUCION 
    ----------------------
    
    IF p_causal_retiro = 2 THEN 
       LET v_tiene_spess = TRUE
       
       -- se verifica si el NSS tiene resolucion valida en el SPESS
       CALL fn_trabajador_resolucion_spess(p_nss,p_causal_retiro ) RETURNING v_tiene_spess, v_id_datamart
       
       
       -- si no tiene resolucion valida en el spess
       IF ( NOT v_tiene_spess ) THEN
           -- se rechaza en viv92 y viv97
           CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 44, 0, TODAY)
       ELSE
           -- se obtiene la fecha de resolucion de pension
           SELECT f_inicio_pension, f_resolucion, regimen, tpo_prestacion, tpo_seguro, tpo_pension, porcentaje_valuacion
           INTO   v_f_inicio_pension, v_f_resolucion, v_regimen, v_tpo_prestacion, v_tpo_seguro, v_tpo_pension, v_porcentaje_valuacion
           FROM   ret_datamart
           WHERE  id_datamart = v_id_datamart
           
           IF v_regimen = 97 THEN
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 44, 0, TODAY)
           ELSE 
               IF v_tpo_prestacion = "03" THEN 
                   CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_resolucion_neg_pension, 44, 0, TODAY)
               ELSE
                   IF v_tpo_seguro = "RT" AND v_tpo_pension = "IP" AND v_tpo_prestacion = "00" AND  v_porcentaje_valuacion < 50 THEN 
                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_porcentaje_menor_50, 44, 0, TODAY)
                   ELSE 
                       -- Busca la convinación en la matriz de derechos, si no la encuentra regresa codigo 91
                       SELECT COUNT(*)
                         INTO v_cant_matriz_derechos
                         FROM ret_matriz_derecho
                        WHERE tpo_prestacion = v_tpo_prestacion
                          AND tpo_seguro = v_tpo_seguro
                          AND tpo_pension = v_tpo_pension
                          AND regimen = v_regimen
                          AND tpo_retiro = 'E'
                          
                       IF v_cant_matriz_derechos = 0 THEN 
                           CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 44, 0, TODAY)
                       ELSE 
                           -- se calcula el saldo
                           CALL fn_calcula_saldo_ley73(p_nss, 44, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
                       
                           LET v_sdo_tot_aivs  = 0
                           LET v_sdo_tot_pesos = 0
                           
                           --- Se valuan los pesos al primer día del mes de la consulta
                           CALL fn_pesos_al_primero_mes(v_aivs_viv92) RETURNING v_pesos_viv92
                           CALL fn_pesos_al_primero_mes(v_aivs_viv97) RETURNING v_pesos_viv97
       
                           LET v_sdo_tot_aivs  = v_aivs_viv92 + v_aivs_viv97
                           LET v_sdo_tot_pesos = v_pesos_viv92 + v_pesos_viv92
       
                           -- Se implementa la busqueda de casos en CRM
                           --CALL fn_busca_caso(p_nss, p_medio_entrega) RETURNING v_resultado
                           --
                           --DISPLAY "El valor regresado por la búsqueda del caso :", v_resultado
                           ----LET v_resultado = 0 --- mientras se implementa lo de CRM
                       END IF
                   END IF
               END IF
           END IF 
       END IF
    END IF

    ----------------------------------------
    --PREPARA LOS DATOS FINALES A RETORNAR
    ----------------------------------------
    LET ws_ret_cons_saldos_disponibles_out.nss              = p_nss
    LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = gi_solicitud_aceptada
    LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = 0
    LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = " "
    LET ws_ret_cons_saldos_disponibles_out.fecha_valuacion  = v_f_valuacion
    LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = v_saldo_aivs
    LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = v_monto_pesos
    
END FUNCTION



{
================================================================================
Nombre  : fn_respuesta_ws_solo_infonavit
Creacion: Julio 2020
Autor   : Isai Jimenez Rojas, OMNISYS
Objetivo: Construye la respuesta de la validacion de disponibilidad del retiro 
          de Solo Infonavit

Modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION fn_respuesta_ws_solo_infonavit(p_estado_solicitud, p_cod_rechazo, p_importe)

    DEFINE p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
           p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
           p_importe          DECIMAL(12,2), -- Importe de vivienda
           v_devolver_saldo   SMALLINT, -- booleana que indica si el saldo se debe devolver
           v_desc_rechazo     CHAR(100)
         
    -- se verifica si se debe devolver el saldo de esta subcuenta (modalidad 1, subcuenta 44)
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(1, 44)

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
       LET p_estado_solicitud = gi_solicitud_rechazada
       LET p_cod_rechazo      = gi_no_disponible_para_retiro
    END IF

    LET v_desc_rechazo = ""
   
    IF p_cod_rechazo <> 0 THEN
       -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_cons_saldos_disponibles_out.des_rechazo = ""
       
       SELECT des_larga
       INTO   v_desc_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo
       
       IF v_desc_rechazo IS NULL THEN
          LET v_desc_rechazo = " "
       END IF
    END IF

    -- se construye la respuesta del ws
    LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = p_estado_solicitud
    LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = v_desc_rechazo
    LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = p_importe
    LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = p_importe

END FUNCTION


{ evalua si se ubica en libreria RETX30_1
================================================================================
Nombre  : fn_rechazo_por_tramite_solo_infonavit
Creacion: Julio 2020
Autor   : Isai Jimenez Rojas, Omnisys
Objetivo: valida que no exista otra solicitd en proceso de tramite pendiente de 
          algun proceso con el mismo nss

Modificaciones:
Autor         Fecha        Descrip. cambio

================================================================================
}
FUNCTION fn_rechazo_por_tramite_solo_infonavit(p_nss)

    DEFINE p_nss                LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
           v_id_derechohabiente LIKE ret_solicitud_generico.id_derechohabiente,
           v_conteo             SMALLINT,
           v_en_tramite         SMALLINT, -- booleana que indica si se tiene una solicitud en tramite
           v_cadena             STRING

    -- se asume que no hay solicitud en tramite 
    LET v_en_tramite = FALSE
    LET v_conteo     = 0

    -- se busca id derechohabiente de acuerdo al NSS
    SELECT MAX(id_derechohabiente)
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = p_nss
   
    -- busca al derechohabiente en la tabla ret_fondo_ahorro con estatus en tramite
    SELECT COUNT(*)
    INTO   v_conteo
    FROM   ret_solicitud_generico
    WHERE  id_derechohabiente = v_id_derechohabiente
    AND    modalidad_retiro   = 1   ---PENDIENTE CONFIRMAR
    AND    estado_solicitud   IN (8, 10, 15, 50, 60, 70, 700, 71,
                                  90, 209, 210, 211, 212)
    -- si lo encuentra
    IF ( v_conteo > 0 ) THEN
       LET v_en_tramite = TRUE
    END IF

    -- se devuelve el resultado de la consulta
    RETURN v_en_tramite
   
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
#                                                                              #
#==============================================================================#
FUNCTION fn_saldo_dummy()
 
    DISPLAY "ENTRA A FUNCION DUMMY...."

    LET ws_ret_cons_saldos_disponibles_out.nss              = ws_ret_cons_saldos_disponibles_in.nss
    LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = 0   -- Aceptada
    LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = 0   -- Sin código de rechazo
    LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = ""  -- Sin descripción de rechazo
    LET ws_ret_cons_saldos_disponibles_out.fecha_valuacion  = MDY(MONTH(TODAY),1,YEAR(TODAY))
    LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = 0.0            --aivs 
    LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = 0.0   --pesos
          
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
FUNCTION fn_respuesta_ws_ley73(p_estado_solicitud, p_cod_rechazo, p_subcuenta, p_importe_aivs, p_fecha_valuacion)

    DEFINE p_estado_solicitud   SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
           p_cod_rechazo        SMALLINT, -- Codigo de rechazo 
           p_subcuenta          SMALLINT, -- subcuenta de inversion
           p_importe_aivs       DECIMAL(24,6), -- monto en AIVS
           p_fecha_valuacion    DATE, -- fecha de valuacion
           v_valor_fondo        LIKE glo_valor_fondo.precio_fondo,
           v_devolver_saldo     SMALLINT -- booleana que indica si se debe devolver el saldo
    DEFINE v_modalidad_retiro   SMALLINT  --Modalidad de retiro 1=Solo infonavit
    DEFINE v_des_rechazo        CHAR(100)
    
    LET v_modalidad_retiro = 1 -- Solo infonavit
   
    -- se verifica si se debe devolver el saldo de esta subcuenta
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(v_modalidad_retiro, p_subcuenta)

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
        LET p_estado_solicitud = gi_solicitud_rechazada
        LET p_cod_rechazo      = gi_no_disponible_para_retiro
    END IF

    LET ws_ret_cons_saldos_disponibles_out.des_rechazo = " "  -- Sin descripción de rechazo

    IF p_cod_rechazo <> 0 THEN
        -- Busca la descripcion del error para regresarla en la consulta
        SELECT des_larga
        INTO   v_des_rechazo
        FROM   ret_rechazo_generico
        WHERE  cod_rechazo = p_cod_rechazo

        IF ws_ret_cons_saldos_disponibles_out.des_rechazo IS NULL THEN
           LET ws_ret_cons_saldos_disponibles_out.des_rechazo = " "
        ELSE
           LET ws_ret_cons_saldos_disponibles_out.des_rechazo = v_des_rechazo
        END IF

    END IF

    --- Se debe valuar al primer dia del mes de consulta    
    SELECT NVL(precio_fondo,0)
    INTO   v_valor_fondo
    FROM   glo_valor_fondo
    WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                          FROM   (SELECT LIMIT 1 1 
                                  FROM   systables))
    AND    fondo = 11

    -- si no se encuentra el precio del fondo, no se puede valuar
    IF ( v_valor_fondo IS NULL ) THEN
        LET ws_ret_cons_saldos_disponibles_out.nss              = ws_ret_cons_saldos_disponibles_in.nss
        LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = gi_solicitud_rechazada
        LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      =  gi_no_hay_precio_fondo -- no existe precio de fondo para valuar
        LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = "No existe precio de fondo"  -- Sin descripción de rechazo
        LET ws_ret_cons_saldos_disponibles_out.fecha_valuacion  = p_fecha_valuacion USING "YYYYMMDD"
        LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = 0
        LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = 0

    ELSE
        LET ws_ret_cons_saldos_disponibles_out.nss              = ws_ret_cons_saldos_disponibles_in.nss
        LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = p_estado_solicitud   -- Aceptada
        LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = p_cod_rechazo   -- Sin código de rechazo
        LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = ""  -- Sin descripción de rechazo
        LET ws_ret_cons_saldos_disponibles_out.fecha_valuacion  = p_fecha_valuacion USING "YYYYMMDD"
        LET ws_ret_cons_saldos_disponibles_out.saldo_aivs       = p_importe_aivs
        LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = ((p_importe_aivs) * v_valor_fondo)

    END IF

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_pesos_al_primero_mes(p_aivs)

   DEFINE p_aivs   DECIMAL(15,2)
   DEFINE v_pesos  DECIMAL(15,2)
   DEFINE v_mes    SMALLINT
   DEFINE v_anio   SMALLINT
   DEFINE v_dia    SMALLINT
   DEFINE v_fecha  DATE 
   DEFINE v_precio DECIMAL(19,14)

   LET v_pesos  = 0
   LET v_mes    = 0
   LET v_anio   = 0
   LET v_dia    = 1
   LET v_fecha  = NULL
   LET v_precio = 0.0

   SELECT MONTH(TODAY), YEAR(TODAY)
   INTO   v_mes, v_anio
   FROM   systables
   WHERE  tabid = 1 

   LET v_fecha = MDY(v_mes,v_dia,v_anio)

   SELECT precio_fondo
   INTO   v_precio
   FROM   glo_valor_fondo
   WHERE  f_valuacion = v_fecha
   AND    fondo = 11

   IF v_precio IS NOT NULL THEN 
      LET v_pesos = p_aivs * v_precio
   END IF 

   RETURN v_pesos

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_busca_caso(p_nss, p_medio_entrega)
    DEFINE p_nss           CHAR(11)
    DEFINE p_medio_entrega SMALLINT
    DEFINE v_regreso       SMALLINT 

    DEFINE arr_casos_crm DYNAMIC ARRAY OF RECORD
          casos           STRING, 
          fecha_creacion  CHAR(10),
          status          CHAR(5),
          fecha_modifica  CHAR(10),
          clase_operacion STRING,
          tipificacion    CHAR(4),
          texto_status    STRING,
          permite_adoc    CHAR(05),
          marca_origen    STRING
          END RECORD 

    DEFINE v_indice       INTEGER 
    DEFINE v_con_caso     INTEGER 


    LET v_regreso = 0
    LET v_indice             = 0
    LET v_con_caso           = 0

    IF p_medio_entrega = 1 THEN ---- solo se busca caso cuando el trámite es por tableta
       -- Llamado a la funcion de casos abiertos 
       CALL arr_casos_crm.clear()
--IJR       CALL fn_busca_caso_crm(p_nss) RETURNING v_regreso, arr_casos_crm 
       
       IF v_regreso = 0 THEN
         
          FOR v_indice = 1 TO arr_casos_crm.getLength()
             IF arr_casos_crm[v_indice].texto_status <> "Abierto" AND 
                arr_casos_crm[v_indice].texto_status <> "Abierto con Cita" AND 
                arr_casos_crm[v_indice].texto_status <> "Cerrado" THEN 
                LET v_con_caso = v_con_caso + 1
             END IF 
          END FOR
          
          IF v_con_caso > 0 THEN
             LET v_regreso = 1
          END IF
       ELSE 
          LET v_regreso = -1
       END IF 
    END IF 

    RETURN v_regreso

END FUNCTION
