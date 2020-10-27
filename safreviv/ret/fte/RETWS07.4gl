--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS07                                                 #
#OBJETIVO          => WS CONSULTA DE SALDOS DISPONIBLES PARA RETIRO DE        #
#                     LEY 73                                                  #
#FECHA INICIO      => 29-NOV-2017                                             #
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
DEFINE ws_ret_cons_saldos_disponibles_in RECORD
         nss              CHAR(11), -- nss del trabajador
         grupo_ley73      SMALLINT, -- num. de grupo al que perteneces segun retiro Ley 73
         medio_entrega    SMALLINT  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
       END RECORD,
       -- registro de respuesta 
       ws_ret_cons_saldos_disponibles_out  RECORD
         nss                 CHAR(11), --- Número de seguridad social del trabajador
         saldo_x_retiro      DYNAMIC ARRAY OF RECORD
           subcuenta          SMALLINT, -- subcuenta de inversion
           estado_solicitud   SMALLINT, -- estado de la solicitud
           cod_rechazo        SMALLINT, -- codigo de rechazo
           des_rechazo        CHAR(100),    -----  *****************************************
           f_valuacion        CHAR(8), -- fecha de valuacion de AIVs en formato AAAAMMDD
           monto_avis         DECIMAL(22,6), -- saldo en AIVs
           monto_pesos        DECIMAL(22,2) -- saldo en pesos equivalente a AIVs por valor accion
         END RECORD
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
-- ========================================================
-- constantes que identifican el medio de entrega
CONSTANT G_ME_TABLETA     SMALLINT = 1,
         G_ME_DEV_AUTO    SMALLINT = 2,
         G_ME_CRM         SMALLINT = 3,
         G_ME_AFORE       SMALLINT = 5,
         G_ME_EXCEPCIONES SMALLINT = 6,
         G_ME_MASIVO      SMALLINT = 7
         
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS07."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"

    -- se inicia el log del programa
    IF FGL_GETENV("RETWS07LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS07LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS07LOG")
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
        CALL fn_crea_servicio_retiro_disponibilidad_ley73(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Disponibilidad Ley 73 service") --, STYLE="naked")
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
    CALL fn_crea_servicio_retiro_disponibilidad_ley73(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Disponibilidad Ley 73 1.0 ...")

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
Nombre: fn_crea_servicio_retiro_disponibilidad_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_retiro_disponibilidad_ley73(p_generar_WSDL)
DEFINE v_webservice         com.WebService       # WebService
DEFINE op                   com.WebOperation     # Operation of a WebService
DEFINE v_service_NameSpace  STRING -- namespace del servicio
DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
DEFINE v_resultado          INTEGER
DEFINE v_urn                STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroSaldosDisponiblesLey73", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_saldos_disponibles_ley73","fn_ret_saldos_disponibles_ley73",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        CALL v_webservice.publishOperation(op, "fn_ret_saldos_disponibles_ley73")

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
            CALL ERRORLOG("Se registro el servicio consulta de saldos disponibles para retiro Ley 73")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de saldos disponibles para retiro Ley 73': " || STATUS)
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
Nombre: fn_ret_saldos_disponibles_ley73
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_saldos_disponibles_ley73()
DEFINE v_indice_retiro   SMALLINT,
       v_nss             LIKE afi_fondo72.nss,
       v_grupo           SMALLINT,
       v_medio_entrega   SMALLINT,
	   v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	   v_ruta_log        STRING,
	   v_cadena          STRING

   -- se responde el servicio para pruebas
   LET ws_ret_cons_saldos_disponibles_out.nss = ws_ret_cons_saldos_disponibles_in.nss

   LET v_nss           = ws_ret_cons_saldos_disponibles_in.nss
   LET v_grupo         = ws_ret_cons_saldos_disponibles_in.grupo_ley73
   LET v_medio_entrega = ws_ret_cons_saldos_disponibles_in.medio_entrega
   
   DISPLAY "Parametros recibidos:"
   DISPLAY "NSS          : ", v_nss
   DISPLAY "GRUPO        : ", v_grupo
   DISPLAY "MEDIO ENTREGA: ", v_medio_entrega

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
   
   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS07."
   LET v_cadena   = v_nss
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

    -- se inicia el log del programa
    IF FGL_GETENV("RETWS07LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS07LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS07LOG")
    ELSE
       DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
       CALL STARTLOG(v_ruta_log)
    END IF 
   
   -- se inicia el indice del retiro que se va a consultar
   LET g_indice_retiro = 1

   -- se verifica si se validara retiro Ley 73
   IF ( (v_grupo IS NOT NULL) AND (v_grupo <> 0) ) THEN
      DISPLAY "Validando Ley 73"
      IF v_grupo <> 1 AND v_medio_entrega = 1 THEN
         DISPLAY "El retiro para el grupo ", v_grupo, ", no se puede tramitar por tableta"
         RETURN 
      ELSE 
         IF ( v_medio_entrega = 0 OR 
              v_medio_entrega IS NULL OR 
              v_grupo         = 0 OR 
              v_grupo         IS NULL ) THEN 
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_datos_incompletos, 8, 0, TODAY,0)
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_datos_incompletos, 4, 0, TODAY,0)
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_datos_incompletos, 12,0, TODAY,0)
         ELSE      
            CALL fn_ret_disponibilidad_ley73(v_nss, v_grupo, v_medio_entrega, TRUE)
         END IF 
      END IF 
   END IF
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_ley73
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de vivienda segun ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Octubre 22, 2020        - PLAG138 se incluye el saldo de TESOFE para grupo 1 con medio de entrega CRM y portal
                                        en caso de existir
======================================================================
}
FUNCTION fn_ret_disponibilidad_ley73(p_nss, p_grupo, p_medio_entrega, p_es_consulta)
DEFINE p_nss                  CHAR(11), -- NSS
       p_grupo                SMALLINT, -- grupo de retiro segun Ley73
       p_medio_entrega        SMALLINT, -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
       p_es_consulta          SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess          SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart          LIKE ret_datamart.id_datamart,
       v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
       v_aivs_vol             DECIMAL(24,6), -- saldo AIVs de Aportaciones voluntarias
       v_aivs_tesofe          DECIMAL(24,6), -- saldo AIVs de TESOFE
       v_pesos_viv92          DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97          DECIMAL(22,2), -- saldo pesos de viv97
       v_pesos_vol            DECIMAL(22,2), -- saldo pesos de Aportaciones voluntarias
       v_pesos_tesofe         DECIMAL(22,2), -- saldo pesos de TESOFE
       v_monto_tesofe         DECIMAL(22,2), -- Saldo Tesofe
       v_resultado            SMALLINT, -- resultado de la consulta
       v_f_inicio_pension     DATE, -- fecha de inicio de pension en el SPESS
       v_f_resolucion         DATE, -- fecha de resolucion para validacion en grupo 1
       v_tpo_pension          CHAR(2), -- Tipo de pension de la resolucion
       v_tpo_prestacion       CHAR(2), -- Tipo de prestacion de la resolucion
       v_tpo_seguro           CHAR(2), -- Tipo de seguro de la resolucion
       v_regimen              SMALLINT,  -- Regimen de la Resolucion (97,73)
       v_porcentaje_valuacion SMALLINT,  -- Porcentaje de Valuación de la Resolución
       v_cant_matriz_derechos SMALLINT,  -- Contador para las ocurrencias de registros en la matriz de derechos
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       v_tiene_cta_clabe      SMALLINT,  -- Indica si el NSS no tiene Cuenta CLABE y por eso se debe regresar el codigo de error especifico 
       v_nss                  CHAR(11),  -- NSS a ser buscado para la cuenta CLABE en caso de que la solicitud este en proceso
       v_n_referencia         LIKE sfr_marca_activa.n_referencia, -- para validar marca
       v_saldo_total          DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_sdo_tot_aivs         DECIMAL(24,6),  -- saldo total para respuesta aivs (viv92 + viv97)
       v_sdo_tot_pesos        DECIMAL(22,2),  -- saldo total para respuesta pesos (viv92 + viv97)
       v_diagnostico          SMALLINT,       --diagnostico de la consulta del saldo en la afore
       v_estatus              SMALLINT,        -- estatus de la cuenta individual segun la consulta del saldo en la Afore
       v_id_cliente           SMALLINT,        -- Indica que como se le habla al servicio de consulta de saldo de la Afore 30 - Consulta, 44 - Consulta y marca y 60 - consulta y desmarca
       v_tiene_rch_siaff      SMALLINT        -- Nos indica si tiene rechazo por siaff para las solicitudes de ley73 grupo 4

   
    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0
    
    LET v_aivs_viv92    = 0
    LET v_aivs_viv97    = 0
    LET v_aivs_vol      = 0
    LET v_aivs_tesofe   = 0
    LET v_pesos_viv92   = 0
    LET v_pesos_viv97   = 0
    LET v_pesos_vol     = 0
    LET v_pesos_tesofe  = 0
    LET v_id_cliente    = 30

    -- si no se encontro
    IF ( v_id_derechohabiente IS NULL ) THEN
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 8, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 4, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 12,0, TODAY,0)

    ELSE
        -- se verifica si el NSS esta marcado de retiro ley73
        SELECT n_referencia
        INTO   v_n_referencia
        FROM   sfr_marca_activa
        WHERE  id_derechohabiente = v_id_derechohabiente
        AND    marca IN (803)

        -- si se encontro
        IF ( v_n_referencia IS NOT NULL ) THEN
            CALL ERRORLOG("ID marcado con 803 referencia: " || v_n_referencia)
            --*********   Se busca para grupo 4 si tiene una solicitud en tramite que esté pendiente por envia SIAFF por no tener cuenta CLABE o 
            --*********   por tener un rechazo por el banco
            --*********   ret_cuenta_clabe se busca por nss
            --*********
            IF ( p_grupo = 4 ) THEN 
                -- Primero valida si no tiene un rechazo por Banco
                LET v_tiene_rch_siaff = 0;
                
                SELECT COUNT(*)
                INTO v_tiene_rch_siaff
                FROM   ret_solicitud_generico rg,
                       ret_ley73_generico     rl
                WHERE  rg.id_solicitud     = rl.id_solicitud
                AND    rg.nss              = p_nss
                AND    rg.estado_solicitud in (90, 209, 210, 211, 213, 214)
                AND    rg.cod_rechazo      = 66
                AND    rg.modalidad_retiro = 3
                AND    rl.gpo_ley73        = 4;
                
                -- si tiene SIAF
                IF ( v_tiene_rch_siaff > 0 ) THEN 
                    -- se reporta sin disponibilidad
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 12, 0, TODAY,0)
                ELSE
                    -- verifica si se tiene cuenta CLABE
                    SELECT COUNT(*)
                    INTO   v_tiene_cta_clabe
                    FROM   ret_solicitud_generico rg,
                           ret_ley73_generico     rlg
                    WHERE  rg.id_solicitud       = v_n_referencia
                    AND    rg.estado_solicitud   IN (60)
                    AND    rg.id_solicitud       = rlg.id_solicitud
                    AND    rg.modalidad_retiro   = 3
                    AND    rlg.gpo_ley73         = 4
                    
                    -- si se encontro cuenta CLABE
                    IF ( v_tiene_cta_clabe > 0 ) THEN
                        LET v_tiene_cta_clabe = 0
                        
                        SELECT COUNT(*) -- si devuelve 0 es que no se ha recibido archivo con la cuenta clabe para el nss, si devuelve algo mayor a cero si se cuenta con clabe y esta próximo a enviarse
                        INTO v_tiene_cta_clabe
                        FROM   ret_solicitud_generico rg,
                               ret_ley73_generico     rlg,
                               ret_cuenta_clabe       rc
                        WHERE  rg.id_solicitud       = v_n_referencia
                        AND    rg.estado_solicitud   IN (60)
                        AND    rg.id_solicitud       = rlg.id_solicitud
                        AND    rg.modalidad_retiro   = 3
                        AND    rlg.gpo_ley73         = 4
                        AND    rc.nss                = rg.nss
                        
                        -- si no se encontro cuenta CLABE en retiro generico y solicitud generico 
                        IF ( v_tiene_cta_clabe = 0 ) THEN
                            -- se rechaza la solicitud porque no tiene cuenta CLABE
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 12,0, TODAY,0)
                        ELSE
                            -- se indica que no hay disponibilidad porque se tiene una solicitud en tramite
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 12,0, TODAY,0)
                        END IF 
                    ELSE 
                        -- se encuentra marcado ya y no esta disponible el fondo
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 12,0, TODAY,0)
                    END IF 
                END IF 
            ELSE 
                -- se encuentra marcado ya y no esta disponible el fondo
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 8, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 4, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 12,0, TODAY,0)
            END IF 
        ELSE      
            -- se verifica si el NSS tiene resolucion valida en el SPESS
            CALL fn_trabajador_resolucion_spess(p_nss, 5) RETURNING v_tiene_spess, v_id_datamart
            --LET v_tiene_spess = TRUE
            
            DISPLAY "v_tiene_spess, v_id_datamart",v_tiene_spess, v_id_datamart
            
            -- si no tiene resolucion valida en el spess
            IF ( NOT v_tiene_spess ) THEN
                -- se rechaza en viv92 y viv97
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 8, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 4, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 12,0, TODAY,0)
            ELSE
                -- se obtiene la fecha de resolucion de pension
                SELECT f_inicio_pension, f_resolucion, regimen, tpo_prestacion, tpo_seguro, tpo_pension, porcentaje_valuacion
                INTO   v_f_inicio_pension, v_f_resolucion, v_regimen, v_tpo_prestacion, v_tpo_seguro, v_tpo_pension, v_porcentaje_valuacion
                FROM   ret_datamart
                WHERE  id_datamart = v_id_datamart
                
                -- si el regimen en el datamar es 97
                IF ( v_regimen = 97 ) THEN
                    -- no hay dispobilidad por corresponder con otro regimen distinto a 73
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 12,0, TODAY,0)
                ELSE 
                    -- si el tipo de prestacion es 03, corresponde con una negativa de pension
                    IF v_tpo_prestacion = "03" THEN 
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_resolucion_neg_pension, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_resolucion_neg_pension, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_resolucion_neg_pension, 12,0, TODAY,0)
                    ELSE
                        IF v_tpo_seguro = "RT" AND v_tpo_pension = "IP" AND v_tpo_prestacion = "00" AND  v_porcentaje_valuacion < 50 THEN 
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_porcentaje_menor_50, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_porcentaje_menor_50, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_porcentaje_menor_50, 12,0, TODAY,0)
                        ELSE 
                            -- Busca la combinacion en la matriz de derechos, si no la encuentra regresa codigo 91
                            SELECT COUNT(*)
                              INTO v_cant_matriz_derechos
                              FROM ret_matriz_derecho
                             WHERE tpo_prestacion = v_tpo_prestacion
                               AND tpo_seguro = v_tpo_seguro
                               AND tpo_pension = v_tpo_pension
                               AND regimen = v_regimen
                               AND tpo_retiro = 'E'
                            
                            -- no se encontro la combinacion con tipo de retiro E
                            IF v_cant_matriz_derechos = 0 THEN 
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 12,0, TODAY,0)
                            ELSE 
                                -- se obtiene el saldo de vivienda 92 y vivienda 97
                                -- se obtiene el saldo de viv92
                                -- 11mar2014. Grupos 2 y 3 no pagan vivienda 92
                                IF ( p_grupo = 2 OR p_grupo = 3 ) THEN
                                    LET v_resultado   = 0
                                    LET v_aivs_viv92  = 0
                                    LET v_pesos_viv92 = 0
                                ELSE
                                    -- se calcula el saldo
                                    CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
                                END IF
                            
                                -- se obtiene el saldo de viv97
                                CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
                                
                                -- se integra el saldo de la subcuenta de aportaciones voluntarias.
                                CALL fn_calcula_saldo_ley73(p_nss, 55, TODAY) RETURNING v_resultado, v_aivs_vol, v_pesos_vol
                                
                                -- se calcula el saldo de TESOFE
                                CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_aivs_tesofe, v_pesos_tesofe

                                LET v_sdo_tot_aivs  = 0
                                LET v_sdo_tot_pesos = 0
                                
                                --- Se valuan los pesos al primer día del mes de la consulta
                                CALL fn_pesos_al_primero_mes(v_aivs_viv92) RETURNING v_pesos_viv92
                                CALL fn_pesos_al_primero_mes(v_aivs_viv97) RETURNING v_pesos_viv97
                                CALL fn_pesos_al_primero_mes(v_aivs_vol) RETURNING v_pesos_vol

                                LET v_sdo_tot_aivs  = v_aivs_viv92 + v_aivs_viv97 + v_aivs_vol
                                LET v_sdo_tot_pesos = v_pesos_viv92 + v_pesos_viv92 + v_pesos_vol

                                -- Se implementa la busqueda de casos en CRM
                                CALL fn_busca_caso(p_nss, p_medio_entrega) RETURNING v_resultado
                                
                                DISPLAY "El valor regresado por la búsqueda del caso :", v_resultado
                                LET v_resultado = 0 --- mientras se implementa lo de CRM
                                
                                IF v_resultado = 0 THEN 
                                   -- se verifica que grupo de retiro llego
                                   CASE p_grupo
                                       -- GRUPO 1
                                       WHEN 1
                                          -- si la fecha de inicio de pension es igual o posterior al 13 de enero de 2012
                                          IF ( v_f_resolucion >= "01/13/2012" ) THEN
                                             CALL fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_aivs_tesofe, v_f_resolucion, p_es_consulta)
                                          ELSE
                                               -- la fecha es invalida para grupo 1
                                               -- Se cambia al medio de entrega a 3 (Asesores telefónicos) en lugar de 4 CRM SACI2019-52
                                               -- Portal anónimo enviará medio de entrega 3 para validarle los otros grupos cuando no sea grupo 1
                                               -- CRM enviará medio de enterga 4 para el Asesor telefónico.
                                               IF p_medio_entrega = 3 THEN ---- Aplica validaciones para otros grupos  
                                                  -- Llamado a la funcion de validación para los otros grupos
                                                  CALL fn_retl73_valida_grupo5(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_f_inicio_pension, p_es_consulta) RETURNING  v_resultado, v_monto_tesofe
                                                  
                                                  IF v_resultado = 0 OR v_resultado =  gi_no_disponible_para_retiro THEN 
                                                     IF v_resultado =  gi_no_disponible_para_retiro  THEN 
                                                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro , 8, 0, TODAY,0)
                                                     ELSE 
                                                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, gi_no_es_grupo_1, 8, v_aivs_viv92, TODAY,0)
                                                     END IF 
                                                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, gi_no_es_grupo_1, 4, v_aivs_viv97 + v_aivs_vol+v_monto_tesofe, TODAY,v_monto_tesofe)
                                                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, gi_no_es_grupo_1, 12,v_aivs_viv92 + v_aivs_viv97 + v_aivs_vol+v_monto_tesofe, TODAY,v_monto_tesofe)
                                                  ELSE 
                                                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_resultado, 8, 0, TODAY,0)
                                                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_resultado, 4, 0, TODAY,0)
                                                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_resultado, 12,0, TODAY,0)
                                                  END IF 
                                               ELSE 
                                                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_corresponde_a_nuevo_pensionado, 8, 0, TODAY,0)
                                                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_corresponde_a_nuevo_pensionado, 4, 0, TODAY,0)
                                                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_corresponde_a_nuevo_pensionado, 12,0, TODAY,0)
                                               END IF 
                                           END IF

                                       -- GRUPO 2
                                       WHEN 2
                                           -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
                                           IF ( v_f_inicio_pension < "01/13/2012" ) THEN
                                               -- se valida la solicitud para un grupo 2
                                               CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_f_inicio_pension, p_es_consulta)
                                           ELSE
                                               -- la fecha es invalida para grupo 2
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 8, 0, TODAY,0)
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 4, 0, TODAY,0)            
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 12,0, TODAY,0)
                                           END IF

                                       -- GRUPO 3
                                       WHEN 3
                                           -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
                                           IF ( v_f_inicio_pension < "01/13/2012" ) THEN
                                               -- se valida la solicitud para un grupo 3
                                               CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_f_inicio_pension, p_es_consulta)
                                           ELSE
                                               -- la fecha es invalida para grupo 3
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 8, 0, TODAY,0)
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 4, 0, TODAY,0)
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 12,0, TODAY,0)            
                                           END IF

                                       -- GRUPO 4
                                       WHEN 4
                                           -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
                                           IF ( v_f_inicio_pension < "01/13/2012" ) THEN
                                               -- se valida la solicitud para un grupo 4
                                               CALL fn_retl73_valida_grupo4(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_f_inicio_pension, p_es_consulta)
                                           ELSE
                                               -- la fecha es invalida para grupo 4
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 8, 0, TODAY,0)
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 4, 0, TODAY,0)
                                               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 12,0, TODAY,0)
                                           END IF
                            
                                       -- otro grupo es invalido
                                       OTHERWISE
                                           -- se rechaza viv92 y viv97
                                           CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 8, 0, TODAY,0)
                                           CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 4, 0, TODAY,0)
                                           CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 12,0, TODAY,0) 
                                   END CASE
                                 ELSE 
                                    IF v_resultado = 1 THEN 
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 8, 0, TODAY,0)
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 4, 0, TODAY,0)
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_solicitud_en_tramite, 12,0, TODAY,0)
                                    ELSE 
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_ws_busca_caso_crm_no_disponible, 8, 0, TODAY,0)
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_ws_busca_caso_crm_no_disponible, 4, 0, TODAY,0)
                                       CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_ws_busca_caso_crm_no_disponible, 12,0, TODAY,0)
                                    END IF 
                                 END IF 
                            END IF 
                        END IF 
                    END IF
                END IF 
            END IF
        END IF
    END IF

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
Ivan Vega    Octubre 06, 2020      Si el nss en turno tiene un credito y este es tipo 43bis,
                                   si permitira continuar con el proceso pero solamente con
                                   el monto correspondiente a viv92
Ivan Vega    Octubre 22, 2020      Se obtiene el saldo de TESOFE de existir y se suma al saldo de
                                   viv97 para grupo 1 cuando es por CRM y Portal (medio de entrega 2 y 3)
                                   Se parametriza la llamada a los servicios de PROCESAR mediante variable de entorno
======================================================================
}
FUNCTION fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97, p_aivs_tesofe, v_fecha_resolucion, p_es_consulta)
DEFINE p_nss              CHAR(11), -- NSS
       p_grupo_ley73      SMALLINT, -- grupo de retiro segun Ley73
       p_es_consulta      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess      SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart      LIKE ret_datamart.id_datamart,
       v_aivs_viv92       DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97       DECIMAL(24,6), -- saldo AIVs de viv97
       p_aivs_tesofe      DECIMAL(24,6), -- saldo AIVs de TESOFE
       v_aivs_viv92_tmp   DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97_tmp   DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92      DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97      DECIMAL(22,2), -- saldo pesos de viv97
       v_pesos_tesofe     DECIMAL(22,2), -- saldo pesos TESOFE
       v_resultado        SMALLINT, -- resultado de la consulta
       v_tiene_credito    SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tipo_credito     SMALLINT, -- clave del tipo de credito
       v_fecha_resolucion DATE, -- fecha de resolucion en el SPESS
       v_saldo_total      DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_consulta         STRING,
       v_rch_cod          SMALLINT,
       v_existe_43_bis    SMALLINT,
       v_rch_desc         CHAR(40),
       v_causal_paso      SMALLINT,
       v_cod_rechazo      SMALLINT,  
       v_diagnostico      SMALLINT,       --diagnostico de la consulta del saldo en la afore
       v_estatus          SMALLINT,        -- estatus de la cuenta individual segun la consulta del saldo en la Afore
       v_cve_afore        CHAR(3)


   -- se calcula saldo total
   LET v_consulta = ""
   LET v_rch_cod  = 0
   LET v_rch_desc = ""
   LET v_resultado = 0


   -- se verifica si tuvo/tiene un retiro de devolucion
   IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
      -- se rechaza por insuficiencia de saldo
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)
   ELSE
      -- se verifica si el derechohabiente tiene un credito vigente
      --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
      DISPLAY "Se consulta la convivencia de marcas " 
      LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,803)";

      DISPLAY "La sentencia SQL ,", v_consulta

      PREPARE s_con_convive FROM v_consulta
      EXECUTE s_con_convive INTO v_rch_cod, v_rch_desc
      DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod
      IF v_rch_cod = 202 OR v_rch_cod = 212 OR v_rch_cod = 232 OR v_rch_cod = 218 OR  
         v_rch_cod = 227 OR v_rch_cod = 226 OR v_rch_cod = 222 OR v_rch_cod = 220 OR 
         v_rch_cod = 223 OR v_rch_cod = 233 THEN 
         DISPLAY"ENTRA"
         LET v_rch_cod = 0
      END IF 
      IF ( v_rch_cod <> 0 ) THEN
      DISPLAY "LLEGA"
         LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                           "FROM   sfr_convivencia AS c,         \n",
                           "       sfr_marca_activa AS a,        \n",
                           "       cat_rch_marca AS d,           \n",
                           "       afi_derechohabiente AS b      \n",
                           "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                           "AND    b.nss                = '",p_nss,"'\n",
                           "AND    a.marca              = c.marca_activa \n",
                           "AND    c.marca_entra        = 803    \n",
                           "AND    c.rch_cod            > 0      \n",
                           "AND    c.rch_cod            = d.rch_cod \n",
                           "AND    a.marca in (592,593,594,595,596,597,814)"
         PREPARE s_marca_cred FROM v_consulta
         EXECUTE s_marca_cred INTO v_rch_cod, v_rch_desc
         DISPLAY "resultado: ",v_rch_cod, v_rch_desc
         IF v_rch_cod IS NULL THEN 
            LET v_rch_cod = 0
         END IF 
         IF ( v_rch_cod = 592 OR v_rch_cod = 593 OR v_rch_cod = 594 OR 
              v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 597 OR 
              v_rch_cod = 814 ) THEN
            IF v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 593 OR v_rch_cod = 594 THEN 
               IF v_rch_cod = 595 THEN 
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 8, 0, TODAY,0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 4, 0, TODAY,0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 12, 0, TODAY,0)
               ELSE 
                  IF v_rch_cod = 596 THEN 
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 8, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 4, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 12, 0, TODAY,0)
                  ELSE
                     IF v_rch_cod = 593 THEN 
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 12, 0, TODAY,0)
                     ELSE 
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 12, 0, TODAY,0)
                     END IF 
                  END IF 
               END IF 
            ELSE 
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 8, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 4, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 12, 0, TODAY,0)
            END IF 
         ELSE 
            LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                              "FROM   sfr_convivencia AS c,         \n",
                              "       sfr_marca_activa AS a,        \n",
                              "       cat_rch_marca AS d,           \n",
                              "       afi_derechohabiente AS b      \n",
                              "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                              "AND    b.nss                = '",p_nss,"'\n",
                              "AND    a.marca              = c.marca_activa \n",
                              "AND    c.marca_entra        = 803    \n",
                              "AND    c.rch_cod            > 0      \n",
                              "AND    c.rch_cod            = d.rch_cod \n",
                              "AND    a.marca in (150,151,280,401,501,502,503,504,701,702,551)"
            PREPARE s_marca_unifica FROM v_consulta
            EXECUTE s_marca_unifica INTO v_rch_cod, v_rch_desc
            IF v_rch_cod IS NULL THEN 
               LET v_rch_cod = 0
            END IF 

            IF ( v_rch_cod = 150 OR v_rch_cod = 151 OR v_rch_cod = 280 OR 
               v_rch_cod = 401 OR v_rch_cod = 501 OR v_rch_cod = 502 OR 
               v_rch_cod = 503 OR v_rch_cod = 504 OR v_rch_cod = 701 OR 
               v_rch_cod = 702 OR v_rch_cod = 551 ) THEN
               LET v_causal_paso = 0;
               CASE v_rch_cod
                  WHEN 150 -- inhabilitacion por unificación imss
                     LET v_causal_paso = gi_inhabil_por_unificacion_imss

                  WHEN 151 -- inhabilitacion por unificación solo infonavit
                     LET v_causal_paso = gi_inhabil_por_unif_infonavit

                  WHEN 501 -- Unificación IMSS unificador 
                     LET v_causal_paso = gi_unif_imss_unificador

                  WHEN 502 -- Unificación IMSS unificado 
                     LET v_causal_paso = gi_unif_imss_unificado

                  WHEN 280 -- Separación de Cuentas
                     LET v_causal_paso = gi_sep_ctas

                  WHEN 551 -- Conciliación OP 28
                     LET v_causal_paso = gi_sep_ctas

                  WHEN 701 -- Separación de Cuentas Invadido
                     LET v_causal_paso = gi_sep_ctas_invadido

                  WHEN 702 -- Separación de Cuentas Asociado
                     LET v_causal_paso = gi_sep_ctas_asociado
                      
                  OTHERWISE -- En otro proceso administrativo
                     LET v_causal_paso = gi_cuenta_con_marca_admiva

               END CASE       
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
               LET v_causal_paso = 0;
            ELSE
               LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                 "FROM   sfr_convivencia AS c,         \n",
                                 "       sfr_marca_activa AS a,        \n",
                                 "       cat_rch_marca AS d,           \n",
                                 "       afi_derechohabiente AS b      \n",
                                 "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                 "AND    b.nss                = '",p_nss,"'\n",
                                 "AND    a.marca              = c.marca_activa \n",
                                 "AND    c.marca_entra        = 803    \n",
                                 "AND    c.rch_cod            > 0      \n",
                                 "AND    c.rch_cod            = d.rch_cod \n",
                                 "AND    a.marca in (803,805,806,808,815)"
               PREPARE s_marca_retiro FROM v_consulta
               EXECUTE s_marca_retiro INTO v_rch_cod, v_rch_desc
               IF v_rch_cod IS NULL THEN 
                  LET v_rch_cod = 0
               END IF 
               IF ( v_rch_cod = 803 OR v_rch_cod = 805 OR v_rch_cod = 806 OR 
                  v_rch_cod = 808 OR v_rch_cod = 815 ) THEN
                  LET v_causal_paso = 0;
                  CASE v_rch_cod
                     WHEN 803 -- Retiro Ley 73
                        LET v_causal_paso = gi_retiro_ley73

                     WHEN 805 -- Retiro por Disposición de Recursos
                        LET v_causal_paso = gi_retiro_disposicion

                     WHEN 806 -- Retiro por Transferencia de Recursos
                        LET v_causal_paso = gi_retiro_transferencia

                     WHEN 808 -- Retiro por Disposición de Recursos PMG
                        LET v_causal_paso = gi_retiro_pmg
                          
                     WHEN 815 -- Retiro Ley 73 Ventanilla Afore
                        LET v_causal_paso = gi_retiro_ley73_va

                     OTHERWISE -- En otro proceso de Retiro
                        LET v_causal_paso = gi_cuenta_en_proceso_de_retiro

                  END CASE       
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                  LET v_causal_paso = 0;
               ELSE
                  LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                    "FROM   sfr_convivencia AS c,         \n",
                                    "       sfr_marca_activa AS a,        \n",
                                    "       cat_rch_marca AS d,           \n",
                                    "       afi_derechohabiente AS b      \n",
                                    "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                    "AND    b.nss                = '",p_nss,"'\n",
                                    "AND    a.marca              = c.marca_activa \n",
                                    "AND    c.marca_entra        = 803    \n",
                                    "AND    c.rch_cod            > 0      \n",
                                    "AND    c.rch_cod            = d.rch_cod \n",
                                    "AND    a.marca in (201,203,204,205,206,210,211,213,214,215,216,217,221,225,231,234,235)"
                  PREPARE s_marca_otras FROM v_consulta
                  EXECUTE s_marca_otras INTO v_rch_cod, v_rch_desc
                  IF v_rch_cod IS NULL THEN 
                     LET v_rch_cod = 0
                  END IF 
                  IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                     v_rch_cod = 205 OR v_rch_cod = 206 OR v_rch_cod = 210 OR 
                     v_rch_cod = 211 OR v_rch_cod = 213 OR v_rch_cod = 214 OR 
                     v_rch_cod = 215 OR v_rch_cod = 216 OR v_rch_cod = 217 OR 
                     v_rch_cod = 221 OR v_rch_cod = 225 OR v_rch_cod = 231 OR 
                     v_rch_cod = 234 OR v_rch_cod = 235 ) THEN
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
                  ELSE
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 8, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 4, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 12, 0, TODAY,0)
                  END IF  
               END IF 
            END IF 
         END IF 
      ELSE 
         --- En este punto se debe llamar al servicio de consulta en adai para saber si existe una solicitud en trámite
         --- Aún no está definida
         LET v_aivs_viv92_tmp = v_aivs_viv92
         LET v_aivs_viv97_tmp = v_aivs_viv97
         -- se consulta el saldo en la afore via WS
         DISPLAY "Envia Solicitud de Saldo a la Afore ", CURRENT YEAR TO SECOND
         DISPLAY "RETWS07_INVOCAR_PROCESAR: ", FGL_GETENV("RETWS07_INVOCAR_PROCESAR")
         IF ( UPSHIFT(FGL_GETENV("RETWS07_INVOCAR_PROCESAR")) = "TRUE" ) THEN
            CALL fn_consulta_saldo_vivienda_afore_completa(p_nss, 30) 
                                                 RETURNING v_diagnostico, 
                                                           v_estatus, 
                                                           v_aivs_viv92, 
                                                           v_pesos_viv92, 
                                                           v_aivs_viv97, 
                                                           v_pesos_viv97,
                                                           v_cod_rechazo,
                                                           v_cve_afore
            
         ELSE
            DISPLAY "No se invocan los servicios de AFORE-PROCESAR para fines de prueba"
            LET v_diagnostico = 101
            LET v_estatus     = 101
         END IF
         
         LET v_aivs_viv92 = v_aivs_viv92_tmp
         LET v_aivs_viv97 = v_aivs_viv97_tmp                           
         DISPLAY "Guarda consulta de Saldo de la Afore ", CURRENT YEAR TO SECOND
         
         IF v_cve_afore IS NOT NULL THEN 
            CALL f_guarda_cve_afore(p_nss,v_cve_afore);
         END IF 
         
         CALL fn_guarda_consulta_ws_vent_afore(p_nss, 3, 3, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', '', '', 1)
                                               
         --CALL fn_busca_nss_pruebas (p_nss) RETURNING v_diagnostico, v_estatus, v_cod_rechazo
         DISPLAY "Diagnostico devuelto >", v_diagnostico, "<"
         DISPLAY "Estatus devuelto     >", v_estatus, "<"
         
         IF (v_diagnostico = 101 AND (v_estatus = 101 OR v_estatus = 201 OR v_estatus = 442)) THEN  
            ---OR (v_diagnostico = 127) THEN -- Solo se deben tramitar los que se puedan consultar en Procesar
            --- Se trabaja con los saldos del Infonavit

            -- se calculan los saldos al primer dia del mes
            CALL fn_pesos_al_primero_mes(v_aivs_viv92) RETURNING v_pesos_viv92
            CALL fn_pesos_al_primero_mes(v_aivs_viv97) RETURNING v_pesos_viv97
            
            LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
            -- si el saldo es mayor a cero
            IF ( v_saldo_total > 0 ) THEN
               LET v_existe_43_bis = 0
               
               SELECT COUNT(*)
               INTO   v_existe_43_bis
               FROM   sfr_marca_activa a, afi_derechohabiente b
               WHERE  a.id_derechohabiente = b.id_derechohabiente
               AND    b.nss                = p_nss
               AND    a.marca IN (202,212,218,220,222,223,224,226,227,232,233)
               
               IF v_existe_43_bis > 0 THEN
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                  
                  IF v_aivs_viv92 > 0 THEN 
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv92, TODAY,0)
                  ELSE 
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)
                  END IF 
               ELSE
                  -- el saldo es retirable
                  -- vivienda 92                  
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                  
                  -- vivienda 97
                  -- PLAG138 Si hay saldo en tesofe, grupo 1 para PORTAL y CRM, se agrega TESOFE al saldo de viv97
                  IF ( ws_ret_cons_saldos_disponibles_in.medio_entrega = G_ME_CRM OR ws_ret_cons_saldos_disponibles_in.medio_entrega = G_ME_DEV_AUTO ) THEN
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97 + p_aivs_tesofe, TODAY, p_aivs_tesofe)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv92 + v_aivs_viv97 + p_aivs_tesofe, TODAY, p_aivs_tesofe)                      
                  ELSE 
                     -- no se incluye TESOFE
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv92 + v_aivs_viv97, TODAY, 0)
                  END IF
               END IF 

            ELSE 
               -- PLAG138 Si hay saldo en tesofe, grupo 1 para PORTAL y CRM, se agrega TESOFE al saldo de viv97
               IF ( ws_ret_cons_saldos_disponibles_in.medio_entrega = G_ME_CRM OR ws_ret_cons_saldos_disponibles_in.medio_entrega = G_ME_DEV_AUTO 
                    AND p_aivs_tesofe > 0 ) THEN
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97 + p_aivs_tesofe, TODAY, p_aivs_tesofe)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv92 + v_aivs_viv97 + p_aivs_tesofe, TODAY, p_aivs_tesofe)                      
               ELSE 
                  -- no se incluye TESOFE
                  -- se rechaza por insuficiencia de saldo
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)   
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)   
               END IF
            END IF 
         ELSE 
            IF v_diagnostico <> 101 THEN 
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 8, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 4, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 12, 0, TODAY,0)
            ELSE 
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_cod_rechazo, 8, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_cod_rechazo, 4, 0, TODAY,0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_cod_rechazo, 12, 0, TODAY,0)
            END IF 
         END IF 
      END IF 
   END IF 

         
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo_2_y_3
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las valiaciones para un retiro de ley 73 por grupo 2 o 3

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     11mar2014               - Grupo 2 y 3 de Ley 73 no pagan vivienda 92
======================================================================
}
FUNCTION fn_retl73_valida_grupo_2_y_3(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_grupo_ley73        SMALLINT, -- grupo de retiro segun Ley73
       p_es_consulta        SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess        SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart        LIKE ret_datamart.id_datamart,
       v_aivs_viv92         DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97         DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92        DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97        DECIMAL(22,2), -- saldo pesos de viv97
       v_resultado          SMALLINT, -- resultado de la consulta
       v_tiene_credito      SMALLINT, -- booleana que indica si existe un credito vigente
       v_tipo_credito       SMALLINT, -- clave del tipo de credito vigente
       v_f_inicio_pension   DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tuvo_transferencia SMALLINT, -- booleana que indica si tuvo una transferencia tipo B
       v_monto_transferido  DECIMAL(22,2), -- monto transferido
       v_causal_paso        SMALLINT, 
       v_consulta           STRING,
       v_cant_marcas_43_bis SMALLINT, 
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40)

    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa

    -- se verifica si tuvo un retiro o devolucion
    IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
        -- se rechaza
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
    ELSE
        -- se verifica si tiene un credito vigente
        --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
        DISPLAY "Se consulta la convivencia de marcas para el grupo 2 y 3" 
        LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,803)";

        DISPLAY "La sentencia SQL ,", v_consulta

        PREPARE s_con_convive_g_2_y_3 FROM v_consulta
        EXECUTE s_con_convive_g_2_y_3 INTO v_rch_cod, v_rch_desc
        DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod
        IF v_rch_cod = 202 OR v_rch_cod = 212 OR v_rch_cod = 232 OR v_rch_cod = 218 OR  
           v_rch_cod = 227 OR v_rch_cod = 226 OR v_rch_cod = 222 OR v_rch_cod = 220 OR 
           v_rch_cod = 223 OR v_rch_cod = 233 THEN 
           LET v_rch_cod = 0
        END IF 
        IF ( v_rch_cod <> 0 ) THEN
        ---- no se paga ninguna subcuenta si tien credito
            -- se verifica si el credito es de tipo 43BIS
            --IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
            LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                              "FROM   sfr_convivencia AS c,         \n",
                              "       sfr_marca_activa AS a,        \n",
                              "       cat_rch_marca AS d,           \n",
                              "       afi_derechohabiente AS b      \n",
                              "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                              "AND    b.nss                = '",p_nss,"'\n",
                              "AND    a.marca              = c.marca_activa \n",
                              "AND    c.marca_entra        = 803    \n",
                              "AND    c.rch_cod            > 0      \n",
                              "AND    c.rch_cod            = d.rch_cod \n",
                              "AND    a.marca in (592,593,594,595,596,597,814)"
            PREPARE s_marca_cred_g_2_3 FROM v_consulta
            EXECUTE s_marca_cred_g_2_3 INTO v_rch_cod, v_rch_desc
            IF v_rch_cod IS NULL THEN 
               LET v_rch_cod = 0
            END IF 
            IF ( v_rch_cod = 592 OR v_rch_cod = 593 OR v_rch_cod = 594 OR 
                 v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 597 OR 
                 v_rch_cod = 814 ) THEN
                IF v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 593 OR v_rch_cod = 594 THEN 
                    IF v_rch_cod = 595 THEN 
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 12, 0, TODAY,0)
                    ELSE 
                        IF v_rch_cod = 596 THEN 
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 12, 0, TODAY,0)
                        ELSE
                            IF v_rch_cod = 593 THEN 
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 12, 0, TODAY,0)
                            ELSE 
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 12, 0, TODAY,0)
                            END IF 
                        END IF 
                    END IF 
                ELSE 
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 12, 0, TODAY,0)
                END IF 
            ELSE 
                LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                  "FROM   sfr_convivencia AS c,         \n",
                                  "       sfr_marca_activa AS a,        \n",
                                  "       cat_rch_marca AS d,           \n",
                                  "       afi_derechohabiente AS b      \n",
                                  "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                  "AND    b.nss                = '",p_nss,"'\n",
                                  "AND    a.marca              = c.marca_activa \n",
                                  "AND    c.marca_entra        = 803    \n",
                                  "AND    c.rch_cod            > 0      \n",
                                  "AND    c.rch_cod            = d.rch_cod \n",
                                  "AND    a.marca in (150,151,280,401,501,502,503,504,701,702,551)"
                PREPARE s_marca_unifica_g_2_3 FROM v_consulta
                EXECUTE s_marca_unifica_g_2_3 INTO v_rch_cod, v_rch_desc
                IF v_rch_cod IS NULL THEN 
                   LET v_rch_cod = 0
                END IF 
                IF ( v_rch_cod = 150 OR v_rch_cod = 151 OR v_rch_cod = 280 OR 
                     v_rch_cod = 401 OR v_rch_cod = 501 OR v_rch_cod = 502 OR 
                     v_rch_cod = 503 OR v_rch_cod = 504 OR v_rch_cod = 701 OR 
                     v_rch_cod = 702 OR v_rch_cod = 551 ) THEN
                    LET v_causal_paso = 0;
                    CASE v_rch_cod
                        WHEN 150 -- inhabilitacion por unificación imss
                            LET v_causal_paso = gi_inhabil_por_unificacion_imss

                        WHEN 151 -- inhabilitacion por unificación solo infonavit
                            LET v_causal_paso = gi_inhabil_por_unif_infonavit

                        WHEN 501 -- Unificación IMSS unificador 
                            LET v_causal_paso = gi_unif_imss_unificador

                        WHEN 502 -- Unificación IMSS unificado 
                            LET v_causal_paso = gi_unif_imss_unificado

                        WHEN 280 -- Separación de Cuentas
                            LET v_causal_paso = gi_sep_ctas

                        WHEN 551 -- Conciliación OP 28
                            LET v_causal_paso = gi_sep_ctas

                        WHEN 701 -- Separación de Cuentas Invadido
                            LET v_causal_paso = gi_sep_ctas_invadido

                        WHEN 702 -- Separación de Cuentas Asociado
                            LET v_causal_paso = gi_sep_ctas_asociado

                        OTHERWISE -- En otro proceso administrativo
                            LET v_causal_paso = gi_cuenta_con_marca_admiva

                    END CASE       
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                    LET v_causal_paso = 0;
                ELSE
                    LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                      "FROM   sfr_convivencia AS c,         \n",
                                      "       sfr_marca_activa AS a,        \n",
                                      "       cat_rch_marca AS d,           \n",
                                      "       afi_derechohabiente AS b      \n",
                                      "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                      "AND    b.nss                = '",p_nss,"'\n",
                                      "AND    a.marca              = c.marca_activa \n",
                                      "AND    c.marca_entra        = 803    \n",
                                      "AND    c.rch_cod            > 0      \n",
                                      "AND    c.rch_cod            = d.rch_cod \n",
                                      "AND    a.marca in (803,805,806,808,815)"
                    PREPARE s_marca_retiro_g_2_3 FROM v_consulta
                    EXECUTE s_marca_retiro_g_2_3 INTO v_rch_cod, v_rch_desc
                    IF v_rch_cod IS NULL THEN 
                       LET v_rch_cod = 0
                    END IF 
                    IF ( v_rch_cod = 803 OR v_rch_cod = 805 OR v_rch_cod = 806 OR 
                         v_rch_cod = 808 OR v_rch_cod = 815 ) THEN
                        LET v_causal_paso = 0;
                        CASE v_rch_cod
                            WHEN 803 -- Retiro Ley 73
                                LET v_causal_paso = gi_retiro_ley73

                            WHEN 805 -- Retiro por Disposición de Recursos
                                LET v_causal_paso = gi_retiro_disposicion

                            WHEN 806 -- Retiro por Transferencia de Recursos
                                LET v_causal_paso = gi_retiro_transferencia

                            WHEN 808 -- Retiro por Disposición de Recursos PMG
                                LET v_causal_paso = gi_retiro_pmg
                                
                            WHEN 815 -- Retiro Ley 73 Ventanilla Afore
                                LET v_causal_paso = gi_retiro_ley73_va

                            OTHERWISE -- En otro proceso de Retiro
                                LET v_causal_paso = gi_cuenta_en_proceso_de_retiro

                        END CASE       
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                        LET v_causal_paso = 0;
                    ELSE
                        LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                          "FROM   sfr_convivencia AS c,         \n",
                                          "       sfr_marca_activa AS a,        \n",
                                          "       cat_rch_marca AS d,           \n",
                                          "       afi_derechohabiente AS b      \n",
                                          "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                          "AND    b.nss                = '",p_nss,"'\n",
                                          "AND    a.marca              = c.marca_activa \n",
                                          "AND    c.marca_entra        = 803    \n",
                                          "AND    c.rch_cod            > 0      \n",
                                          "AND    c.rch_cod            = d.rch_cod \n",
                                          "AND    a.marca in (201,203,204,205,206,210,211,213,214,215,216,217,221,225,231,234,235)"
                        PREPARE s_marca_otras_g_2_3 FROM v_consulta
                        EXECUTE s_marca_otras_g_2_3 INTO v_rch_cod, v_rch_desc
                        IF v_rch_cod IS NULL THEN 
                           LET v_rch_cod = 0
                        END IF 
                        IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                           v_rch_cod = 205 OR v_rch_cod = 206 OR v_rch_cod = 210 OR 
                           v_rch_cod = 211 OR v_rch_cod = 213 OR v_rch_cod = 214 OR 
                           v_rch_cod = 215 OR v_rch_cod = 216 OR v_rch_cod = 217 OR 
                           v_rch_cod = 221 OR v_rch_cod = 225 OR 
                           v_rch_cod = 231 OR v_rch_cod = 234 OR  
                           v_rch_cod = 235 ) THEN
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
                        ELSE
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 12, 0, TODAY,0)
                        END IF  
                    END IF 
                END IF 
            END IF 
            
        ELSE
            LET v_cant_marcas_43_bis = 0
            SELECT COUNT(*) 
            INTO   v_cant_marcas_43_bis
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    marca IN (202,212,218,220,222,223,224,226,227,232,233)
            IF v_cant_marcas_43_bis = 0 THEN             -- se verifica si tuvo transferencia tipo B
                CALL fn_verifica_transferencia_tipo_b(v_id_derechohabiente)
                     RETURNING v_tuvo_transferencia, v_monto_transferido

                -- se verifica si tuvo transferencia tipo B
                IF ( v_tuvo_transferencia ) THEN

                    -- se obtiene el saldo de viv97 del anexo
                    LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido

                    -- el saldo es retirable. Vivienda 92 no se paga
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, v_aivs_viv92, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY,v_monto_transferido)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv97, TODAY,v_monto_transferido)
                ELSE
                    -- el saldo retirable es lo existente en viv92 y viv97
                    -- VIVIENDA 92 no se paga
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, v_aivs_viv92, TODAY,0)
                    
                    -- VIVIENDA 97
                    IF ( v_aivs_viv97 > 0 ) THEN
                        -- saldo retirable
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv97, TODAY,0)
                    ELSE
                        -- sin saldo
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)                             
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)                             
                    END IF
                END IF
            ELSE 
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
            END IF 
        END IF
    END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo4
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las valiaciones para un retiro de ley 73 por grupo 4

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    Octubre 21, 2020     - Se habilita el saldo de viv92 para retiro grupo 4, y se suma el monto disponible (de haber)
                                    en la cuenta 12 (suma de viv92 y viv97)
======================================================================
}
FUNCTION fn_retl73_valida_grupo4(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss              CHAR(11), -- NSS
       p_grupo_ley73      SMALLINT, -- grupo de retiro segun Ley73
       p_es_consulta      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess      SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart      LIKE ret_datamart.id_datamart,
       v_aivs_viv92       DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97       DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92      DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97      DECIMAL(22,2), -- saldo pesos de viv97
       v_aivs_subcta12    DECIMAL(24,6), -- saldo apra la cuenta 12 (suma de viv92 y viv97)
       v_resultado        SMALLINT, -- resultado de la consulta
       v_tiene_credito    SMALLINT, -- booleana que indica si el derechohabiente tiene un credito vigente
       v_tipo_credito     SMALLINT, -- clave del tipo de credito
       v_f_inicio_pension DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tuvo_transferencia SMALLINT, -- booleana que indica si tuvo una transferencia tipo B
       v_monto_transferido  DECIMAL(22,2), -- monto transferido
       v_consulta           STRING,
       v_causal_paso        SMALLINT, 
       v_rch_cod            SMALLINT,
       v_tiene_rch_siaff    SMALLINT,
       v_cant_marcas_43_bis SMALLINT, 
       v_rch_desc           CHAR(40)

    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa

    -- se verifica si tuvo un retiro o devolucion
    IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
        -- se rechaza
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
    ELSE
        -- se verifica si el derechohabiente tiene un credito vigente
        --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) 
        --     RETURNING v_tiene_credito, v_tipo_credito
        DISPLAY "Se consulta la convivencia de marcas grupo 4" 
        LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,803)";

        DISPLAY "La sentencia SQL ,", v_consulta

        PREPARE s_con_convive_g_4 FROM v_consulta
        EXECUTE s_con_convive_g_4 INTO v_rch_cod, v_rch_desc
        DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod
        IF v_rch_cod = 202 OR v_rch_cod = 212 OR v_rch_cod = 232 OR v_rch_cod = 218 OR  
           v_rch_cod = 227 OR v_rch_cod = 226 OR v_rch_cod = 222 OR v_rch_cod = 220 OR
           v_rch_cod = 223 OR v_rch_cod = 233 THEN 
           LET v_rch_cod = 0
        END IF 
        IF ( v_rch_cod <> 0 ) THEN
            -- se verifica si el credito es de tipo 43BIS
            LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                              "FROM   sfr_convivencia AS c,         \n",
                              "       sfr_marca_activa AS a,        \n",
                              "       cat_rch_marca AS d,           \n",
                              "       afi_derechohabiente AS b      \n",
                              "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                              "AND    b.nss                = '",p_nss,"'\n",
                              "AND    a.marca              = c.marca_activa \n",
                              "AND    c.marca_entra        = 803    \n",
                              "AND    c.rch_cod            > 0      \n",
                              "AND    c.rch_cod            = d.rch_cod \n",
                              "AND    a.marca in (592,593,594,595,596,597,814)"
            PREPARE s_marca_cred_g_4 FROM v_consulta
            EXECUTE s_marca_cred_g_4 INTO v_rch_cod, v_rch_desc
            IF v_rch_cod IS NULL THEN 
               LET v_rch_cod = 0
            END IF 
            IF ( v_rch_cod = 592 OR v_rch_cod = 593 OR v_rch_cod = 594 OR 
                 v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 597 OR 
                 v_rch_cod = 814 ) THEN
                IF v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 593 OR v_rch_cod = 594 THEN 
                    IF v_rch_cod = 595 THEN 
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_laudo_tramitado, 12, 0, TODAY,0)
                    ELSE 
                        IF v_rch_cod = 596 THEN 
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_amparo_tramitado, 12, 0, TODAY,0)
                        ELSE
                            IF v_rch_cod = 593 THEN 
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cargo_juridico, 12, 0, TODAY,0)
                            ELSE 
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pagado_por_cargo_juridico, 12, 0, TODAY,0)
                            END IF 
                        END IF 
                    END IF 
                ELSE 
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_cuenta_marcada_laudo, 12, 0, TODAY,0)
                END IF 
            ELSE
               LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
               	                 "FROM   sfr_convivencia AS c,         \n",
               	                 "       sfr_marca_activa AS a,        \n",
               	                 "       cat_rch_marca AS d,           \n",
               	                 "       afi_derechohabiente AS b      \n",
               	                 "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
               	                 "AND    b.nss                = '",p_nss,"'\n",
               	                 "AND    a.marca              = c.marca_activa \n",
               	                 "AND    c.marca_entra        = 803    \n",
               	                 "AND    c.rch_cod            > 0      \n",
               	                 "AND    c.rch_cod            = d.rch_cod \n",
               	                 "AND    a.marca in (150,151,280,401,501,502,503,504,701,702,551)"
                PREPARE s_marca_unifica_g_4 FROM v_consulta
                EXECUTE s_marca_unifica_g_4 INTO v_rch_cod, v_rch_desc
                IF v_rch_cod IS NULL THEN 
                   LET v_rch_cod = 0
                END IF 
                IF ( v_rch_cod = 150 OR v_rch_cod = 151 OR v_rch_cod = 280 OR 
                     v_rch_cod = 401 OR v_rch_cod = 501 OR v_rch_cod = 502 OR 
                     v_rch_cod = 503 OR v_rch_cod = 504 OR v_rch_cod = 701 OR 
                     v_rch_cod = 702 OR v_rch_cod = 551) THEN
                    LET v_causal_paso = 0;
                    CASE v_rch_cod
                        WHEN 150 -- inhabilitacion por unificación imss
                            LET v_causal_paso = gi_inhabil_por_unificacion_imss

                        WHEN 151 -- inhabilitacion por unificación solo infonavit
                            LET v_causal_paso = gi_inhabil_por_unif_infonavit

                        WHEN 501 -- Unificación IMSS unificador 
                            LET v_causal_paso = gi_unif_imss_unificador

                        WHEN 502 -- Unificación IMSS unificado 
                            LET v_causal_paso = gi_unif_imss_unificado

                        WHEN 280 -- Separación de Cuentas
                            LET v_causal_paso = gi_sep_ctas

                        WHEN 701 -- Separación de Cuentas Invadido
                            LET v_causal_paso = gi_sep_ctas_invadido

                        WHEN 702 -- Separación de Cuentas Asociado
                            LET v_causal_paso = gi_sep_ctas_asociado

                        OTHERWISE -- En otro proceso administrativo
                            LET v_causal_paso = gi_cuenta_con_marca_admiva

                    END CASE       
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                    LET v_causal_paso = 0;
                ELSE
                    LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                      "FROM   sfr_convivencia AS c,         \n",
                                      "       sfr_marca_activa AS a,        \n",
                                      "       cat_rch_marca AS d,           \n",
                                      "       afi_derechohabiente AS b      \n",
                                      "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                      "AND    b.nss                = '",p_nss,"'\n",
                                      "AND    a.marca              = c.marca_activa \n",
                                      "AND    c.marca_entra        = 803    \n",
                                      "AND    c.rch_cod            > 0      \n",
                                      "AND    c.rch_cod            = d.rch_cod \n",
                                      "AND    a.marca in (803,805,806,808,815)"
                    PREPARE s_marca_retiro_g_4 FROM v_consulta
                    EXECUTE s_marca_retiro_g_4 INTO v_rch_cod, v_rch_desc
                    IF v_rch_cod IS NULL THEN 
                       LET v_rch_cod = 0
                    END IF 
                    IF ( v_rch_cod = 803 OR v_rch_cod = 805 OR v_rch_cod = 806 OR 
                         v_rch_cod = 808 OR v_rch_cod = 815 ) THEN
                        LET v_causal_paso = 0;
                        CASE v_rch_cod
                            WHEN 803 -- Retiro Ley 73
                                LET v_causal_paso = gi_retiro_ley73

                            WHEN 805 -- Retiro por Disposición de Recursos
                                LET v_causal_paso = gi_retiro_disposicion

                            WHEN 806 -- Retiro por Transferencia de Recursos
                                LET v_causal_paso = gi_retiro_transferencia

                            WHEN 808 -- Retiro por Disposición de Recursos PMG
                                LET v_causal_paso = gi_retiro_pmg
                                
                            WHEN 815 -- Retiro Ley 73 Ventanilla Afore
                                LET v_causal_paso = gi_retiro_ley73_va

                            OTHERWISE -- En otro proceso de Retiro
                                LET v_causal_paso = gi_cuenta_en_proceso_de_retiro

                        END CASE       
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                        LET v_causal_paso = 0;
                    ELSE
                       LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                         "FROM   sfr_convivencia AS c,         \n",
                                         "       sfr_marca_activa AS a,        \n",
                                         "       cat_rch_marca AS d,           \n",
                                         "       afi_derechohabiente AS b      \n",
                                         "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                         "AND    b.nss                = '",p_nss,"'\n",
                                         "AND    a.marca              = c.marca_activa \n",
                                         "AND    c.marca_entra        = 803    \n",
                                         "AND    c.rch_cod            > 0      \n",
                                         "AND    c.rch_cod            = d.rch_cod \n",
                                         "AND    a.marca in (201,203,204,205,206,210,211,213,214,215,216,217,221,225,231,234,235)"
                       PREPARE s_marca_otras_g_4 FROM v_consulta
                       EXECUTE s_marca_otras_g_4 INTO v_rch_cod, v_rch_desc
                       IF v_rch_cod IS NULL THEN 
                          LET v_rch_cod = 0
                       END IF 
                       IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                           v_rch_cod = 205 OR v_rch_cod = 206 OR v_rch_cod = 210 OR 
                           v_rch_cod = 211 OR v_rch_cod = 213 OR v_rch_cod = 214 OR 
                           v_rch_cod = 215 OR v_rch_cod = 216 OR v_rch_cod = 217 OR 
                           v_rch_cod = 221 OR v_rch_cod = 225 OR 
                           v_rch_cod = 231 OR v_rch_cod = 234 OR  
                           v_rch_cod = 235 ) THEN
                          CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
                          CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                          CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
                        ELSE
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_error_marca_no_convive, 12, 0, TODAY,0)
                        END IF  
                    END IF 
                END IF 
            END IF 

        ELSE

            -- Valida si tiene Credito 43 BIS
            LET v_cant_marcas_43_bis = 0
            SELECT COUNT(*) 
            INTO   v_cant_marcas_43_bis
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    marca IN (202,212,218,220,222,223,224,226,227,232,233)
            
            IF v_cant_marcas_43_bis = 0 THEN 
                -- Valida si tiene una solicitud previa con rechazo por SIAFF
                LET v_tiene_rch_siaff = 0
                
                SELECT COUNT(*)
                INTO v_tiene_rch_siaff
                FROM   ret_solicitud_generico rg,
                       ret_ley73_generico     rl
                WHERE  rg.id_solicitud     = rl.id_solicitud
                AND    rg.nss              = p_nss
                AND    rg.estado_solicitud in (90, 209, 210, 211, 213, 214)
                AND    rg.cod_rechazo      = 66
                AND    rg.modalidad_retiro = 3
                AND    rl.gpo_ley73        = 4;
                
                IF v_tiene_rch_siaff > 0 THEN 
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 12, 0, TODAY,0)
                ELSE 
                    -- se verifica si tuvo transferencia tipo B
                    CALL fn_verifica_transferencia_tipo_b(v_id_derechohabiente)
                        RETURNING v_tuvo_transferencia, v_monto_transferido

                    -- se verifica si tuvo transferencia tipo B
                    IF ( v_tuvo_transferencia ) THEN

                        -- se obtiene el saldo de viv97 del anexo
                        LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido
                        LET v_aivs_subcta12 = v_aivs_viv97
                        -- el saldo es retirable
                        -- 20201021 si hay saldo en viv92 se muestra disponible para grupo 4 PLAG138
                        IF ( v_aivs_viv92 > 0 ) THEN
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                            LET v_aivs_subcta12 = v_aivs_viv97 + v_aivs_viv92
                        ELSE
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
                        END IF
                        
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, v_monto_transferido)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv97, TODAY, v_monto_transferido)
                    ELSE
                        -- se asume que no hay saldo
                        LET v_aivs_subcta12 = 0
                        -- el saldo retirable es lo existente en viv92 y viv97
                        -- VIVIENDA 92
                        IF ( v_aivs_viv92 > 0 ) THEN
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                            LET v_aivs_subcta12 = v_aivs_subcta12 + v_aivs_viv92
                        ELSE
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
                        END IF

                        -- VIVIENDA 97
                        IF ( v_aivs_viv97 > 0 ) THEN
                            -- saldo retirable
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
                            LET v_aivs_subcta12 = v_aivs_subcta12 + v_aivs_viv97
                        ELSE
                            -- sin saldo
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)                             
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)                             
                        END IF
                        
                        -- subcuenta12 viv92 + viv97
                        -- 20201021 si hay saldo de viv92 se agrega a la respuesta en la cuenta 12
                        IF ( v_aivs_subcta12 > 0 ) THEN
                            -- saldo retirable
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_subcta12, TODAY, 0)
                        ELSE
                            -- sin saldo                            
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)                             
                        END IF
                    END IF 
                END IF 
            ELSE 
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 12, 0, TODAY,0)
            END IF 
        END IF
    END IF

END FUNCTION
FUNCTION fn_retl73_valida_grupo5(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
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
       v_tiene_credito    SMALLINT, -- booleana que indica si el derechohabiente tiene un credito vigente
       v_tipo_credito     SMALLINT, -- clave del tipo de credito
       v_f_inicio_pension DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tuvo_transferencia SMALLINT, -- booleana que indica si tuvo una transferencia tipo B
       v_monto_transferido  DECIMAL(22,2), -- monto transferido
       v_consulta           STRING,
       v_causal_paso        SMALLINT, 
       v_rch_cod            SMALLINT,
       v_tiene_rch_siaff    SMALLINT,
       v_cant_marcas_43_bis SMALLINT, 
       v_rch_desc           CHAR(40)

    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss               = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa
    LET v_monto_transferido = 0

    -- se verifica si tuvo un retiro o devolucion
    IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
        -- se rechaza
        RETURN gi_tiene_credito_vigente
    ELSE
        -- se verifica si el derechohabiente tiene un credito vigente
        --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) 
        --     RETURNING v_tiene_credito, v_tipo_credito
        DISPLAY "Se consulta la convivencia de marcas medio entrega 4 " 
        LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,803)";

        DISPLAY "La sentencia SQL ,", v_consulta

        PREPARE s_con_convive_g_5 FROM v_consulta
        EXECUTE s_con_convive_g_5 INTO v_rch_cod, v_rch_desc
        DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod
        IF v_rch_cod = 202 OR v_rch_cod = 212 OR v_rch_cod = 232 OR v_rch_cod = 218 OR  
           v_rch_cod = 227 OR v_rch_cod = 226 OR v_rch_cod = 222 OR v_rch_cod = 220 OR 
           v_rch_cod = 223 OR v_rch_cod = 233 THEN 
           LET v_rch_cod = 0
        END IF 
        IF ( v_rch_cod <> 0 ) THEN
            LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                              "FROM   sfr_convivencia AS c,         \n",
                              "       sfr_marca_activa AS a,        \n",
                              "       cat_rch_marca AS d,           \n",
                              "       afi_derechohabiente AS b      \n",
                              "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                              "AND    b.nss                = '",p_nss,"'\n",
                              "AND    a.marca              = c.marca_activa \n",
                              "AND    c.marca_entra        = 803    \n",
                              "AND    c.rch_cod            > 0      \n",
                              "AND    c.rch_cod            = d.rch_cod \n",
                              "AND    a.marca in (592,593,594,595,596,597,814)"
            PREPARE s_marca_cred_g_5 FROM v_consulta
            EXECUTE s_marca_cred_g_5 INTO v_rch_cod, v_rch_desc
            IF v_rch_cod IS NULL THEN 
               LET v_rch_cod = 0
            END IF 
            -- se verifica si el credito es de tipo 43BIS
            IF ( v_rch_cod = 592 OR v_rch_cod = 593 OR v_rch_cod = 594 OR 
                 v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 597 OR 
                 v_rch_cod = 814 ) THEN
                IF v_rch_cod = 595 OR v_rch_cod = 596 OR v_rch_cod = 593 OR v_rch_cod = 594 THEN 
                    IF v_rch_cod = 595 THEN 
                        RETURN gi_pagado_laudo_tramitado, v_monto_transferido
                    ELSE 
                        IF v_rch_cod = 596 THEN 
                            RETURN gi_pagado_amparo_tramitado, v_monto_transferido
                        ELSE
                            IF v_rch_cod = 593 THEN
                                RETURN  gi_cargo_juridico, v_monto_transferido
                            ELSE 
                                RETURN gi_pagado_por_cargo_juridico, v_monto_transferido
                            END IF 
                        END IF 
                    END IF 
                ELSE 
                  RETURN gi_cuenta_marcada_laudo, v_monto_transferido
                END IF 
            ELSE 
                LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                  "FROM   sfr_convivencia AS c,         \n",
                                  "       sfr_marca_activa AS a,        \n",
                                  "       cat_rch_marca AS d,           \n",
                                  "       afi_derechohabiente AS b      \n",
                                  "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                  "AND    b.nss                = '",p_nss,"'\n",
                                  "AND    a.marca              = c.marca_activa \n",
                                  "AND    c.marca_entra        = 803    \n",
                                  "AND    c.rch_cod            > 0      \n",
                                  "AND    c.rch_cod            = d.rch_cod \n",
                                  "AND    a.marca in (150,151,280,401,501,502,503,504,701,702,551)"
                PREPARE s_marca_unifica_g_5 FROM v_consulta
                EXECUTE s_marca_unifica_g_5 INTO v_rch_cod, v_rch_desc
                IF v_rch_cod IS NULL THEN 
                   LET v_rch_cod = 0
                END IF 
                IF ( v_rch_cod = 150 OR v_rch_cod = 151 OR v_rch_cod = 280 OR 
                     v_rch_cod = 401 OR v_rch_cod = 501 OR v_rch_cod = 502 OR 
                     v_rch_cod = 503 OR v_rch_cod = 504 OR v_rch_cod = 701 OR 
                     v_rch_cod = 702 ) THEN
                    LET v_causal_paso = 0;
                    CASE v_rch_cod
                        WHEN 150 -- inhabilitacion por unificación imss
                            LET v_causal_paso = gi_inhabil_por_unificacion_imss

                        WHEN 151 -- inhabilitacion por unificación solo infonavit
                            LET v_causal_paso = gi_inhabil_por_unif_infonavit

                        WHEN 501 -- Unificación IMSS unificador 
                            LET v_causal_paso = gi_unif_imss_unificador

                        WHEN 502 -- Unificación IMSS unificado 
                            LET v_causal_paso = gi_unif_imss_unificado

                        WHEN 280 -- Separación de Cuentas
                            LET v_causal_paso = gi_sep_ctas

                        WHEN 701 -- Separación de Cuentas Invadido
                            LET v_causal_paso = gi_sep_ctas_invadido

                        WHEN 702 -- Separación de Cuentas Asociado
                            LET v_causal_paso = gi_sep_ctas_asociado

                        OTHERWISE -- En otro proceso administrativo
                            LET v_causal_paso = gi_cuenta_con_marca_admiva

                    END CASE       
                    RETURN v_causal_paso, v_monto_transferido
                ELSE
                    LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                      "FROM   sfr_convivencia AS c,         \n",
                                      "       sfr_marca_activa AS a,        \n",
                                      "       cat_rch_marca AS d,           \n",
                                      "       afi_derechohabiente AS b      \n",
                                      "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                      "AND    b.nss                = '",p_nss,"'\n",
                                      "AND    a.marca              = c.marca_activa \n",
                                      "AND    c.marca_entra        = 803    \n",
                                      "AND    c.rch_cod            > 0      \n",
                                      "AND    c.rch_cod            = d.rch_cod \n",
                                      "AND    a.marca in (803,805,806,808,815)"
                    PREPARE s_marca_retiro_g_5 FROM v_consulta
                    EXECUTE s_marca_retiro_g_5 INTO v_rch_cod, v_rch_desc
                    IF v_rch_cod IS NULL THEN 
                       LET v_rch_cod = 0
                    END IF 
                    IF ( v_rch_cod = 803 OR v_rch_cod = 805 OR v_rch_cod = 806 OR 
                         v_rch_cod = 808 OR v_rch_cod = 815 ) THEN
                        LET v_causal_paso = 0;
                        CASE v_rch_cod
                            WHEN 803 -- Retiro Ley 73
                                LET v_causal_paso = gi_retiro_ley73

                            WHEN 805 -- Retiro por Disposición de Recursos
                                LET v_causal_paso = gi_retiro_disposicion

                            WHEN 806 -- Retiro por Transferencia de Recursos
                                LET v_causal_paso = gi_retiro_transferencia

                            WHEN 808 -- Retiro por Disposición de Recursos PMG
                                LET v_causal_paso = gi_retiro_pmg
                                
                            WHEN 815 -- Retiro Ley 73 Ventanilla Afore
                                LET v_causal_paso = gi_retiro_ley73_va

                            OTHERWISE -- En otro proceso de Retiro
                                LET v_causal_paso = gi_cuenta_en_proceso_de_retiro

                        END CASE       
                        RETURN v_causal_paso, v_monto_transferido
                    ELSE
                        LET v_consulta = "SELECT FIRST 1 c.rch_cod, d.rch_desc \n",
                                          "FROM   sfr_convivencia AS c,         \n",
                                          "       sfr_marca_activa AS a,        \n",
                                          "       cat_rch_marca AS d,           \n",
                                          "       afi_derechohabiente AS b      \n",
                                          "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                                          "AND    b.nss                = '",p_nss,"'\n",
                                          "AND    a.marca              = c.marca_activa \n",
                                          "AND    c.marca_entra        = 803    \n",
                                          "AND    c.rch_cod            > 0      \n",
                                          "AND    c.rch_cod            = d.rch_cod \n",
                                          "AND    a.marca in (201,203,204,205,206,210,211,213,214,215,216,217,221,225,231,234,235)"
                        PREPARE s_marca_otras_g_5 FROM v_consulta
                        EXECUTE s_marca_otras_g_5 INTO v_rch_cod, v_rch_desc
                        IF v_rch_cod IS NULL THEN 
                           LET v_rch_cod = 0
                        END IF 
                        IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                           v_rch_cod = 205 OR v_rch_cod = 206 OR v_rch_cod = 210 OR 
                           v_rch_cod = 211 OR v_rch_cod = 213 OR v_rch_cod = 214 OR 
                           v_rch_cod = 215 OR v_rch_cod = 216 OR v_rch_cod = 217 OR 
                           v_rch_cod = 221 OR v_rch_cod = 225 OR 
                           v_rch_cod = 231 OR v_rch_cod = 234 OR  
                           v_rch_cod = 235 ) THEN
                            RETURN gi_tiene_credito_vigente, v_monto_transferido
                        ELSE
                            RETURN gi_error_marca_no_convive, v_monto_transferido
                        END IF  
                    END IF 
                END IF 
            END IF 

        ELSE

            -- Valida si tiene Credito 43 BIS
            LET v_cant_marcas_43_bis = 0
            SELECT COUNT(*) 
            INTO   v_cant_marcas_43_bis
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    marca IN (202,212,218,220,222,223,224,226,227,232,233)
            IF v_cant_marcas_43_bis = 0 THEN 
                -- Valida si tiene una solicitud previa con rechazo por SIAFF
                LET v_tiene_rch_siaff = 0
                SELECT COUNT(*)
                INTO v_tiene_rch_siaff
                FROM   ret_solicitud_generico rg,
                       ret_ley73_generico     rl
                WHERE  rg.id_solicitud     = rl.id_solicitud
                AND    rg.nss              = p_nss
                AND    rg.estado_solicitud in (90, 209, 210, 211, 213, 214)
                AND    rg.cod_rechazo      = 66
                AND    rg.modalidad_retiro = 3
                AND    rl.gpo_ley73        = 4;
                IF v_tiene_rch_siaff > 0 THEN 
                    RETURN gi_rechazo_banco_siaff, v_monto_transferido
                ELSE 
                    -- se verifica si tuvo transferencia tipo B
                    CALL fn_verifica_transferencia_tipo_b(v_id_derechohabiente)
                        RETURNING v_tuvo_transferencia, v_monto_transferido

                    -- se verifica si tuvo transferencia tipo B
                    IF ( v_tuvo_transferencia ) THEN

                        -- se obtiene el saldo de viv97 del anexo
                        LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido
                        
                        -- el saldo es retirable
                        RETURN gi_no_disponible_para_retiro, v_monto_transferido
                    ELSE
                        -- el saldo retirable es lo existente en viv92 y viv97
                        -- VIVIENDA 92
                        --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY,0)

                        -- VIVIENDA 97
                        IF ( v_aivs_viv97 > 0 ) THEN
                            RETURN gi_no_disponible_para_retiro, v_monto_transferido
                            -- saldo retirable
                        ELSE
                            RETURN gi_sin_saldo, v_monto_transferido
                            -- sin saldo
                        END IF
                    END IF 
                END IF 
            ELSE 
                RETURN gi_tiene_credito_vigente, v_monto_transferido
            END IF 
        END IF
    END IF

    RETURN 0, v_monto_transferido
   
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
FUNCTION fn_respuesta_ws_ley73(p_estado_solicitud, p_cod_rechazo, p_subcuenta, p_importe_aivs, p_fecha_valuacion,p_importe_tesofe)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_subcuenta        SMALLINT, -- subcuenta de inversion
         p_importe_aivs     DECIMAL(24,6), -- monto en AIVS
         p_fecha_valuacion  DATE, -- fecha de valuacion
         p_importe_tesofe   DECIMAL(24,2), -- monto en AIVs
         v_valor_fondo      LIKE glo_valor_fondo.precio_fondo,
         v_devolver_saldo   SMALLINT -- booleana que indica si se debe devolver el saldo
         
    -- se obtiene el valor de la accion
--    SELECT precio_fondo
--    INTO   v_valor_fondo
--    FROM   glo_valor_fondo
--    WHERE  f_valuacion = p_fecha_valuacion
--    AND    fondo = 11
    --- Se debe valuar al primer dia del mes de consulta    
      SELECT NVL(precio_fondo,0)
      INTO   v_valor_fondo
      FROM   glo_valor_fondo
      WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                          FROM   (SELECT LIMIT 1 1 
                                  FROM   systables))
      AND    fondo = 11
    LET v_devolver_saldo = TRUE 
    -- se verifica si se debe devolver el saldo de esta subcuenta
    IF p_subcuenta = 12 THEN 
        LET v_devolver_saldo = TRUE
    ELSE 
        LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(3, p_subcuenta)
    END IF 

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
        LET p_estado_solicitud = gi_solicitud_rechazada
        LET p_cod_rechazo      = gi_no_disponible_para_retiro
    END IF
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
    IF p_cod_rechazo <> 0 THEN
        -- Busca la descripcion del error para regresarla en la consulta
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = "";
        SELECT des_larga
        INTO   ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo
        FROM   ret_rechazo_generico
        WHERE  cod_rechazo = p_cod_rechazo;
        IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
        END IF
    END IF 
    -- si no se encuentra el precio del fondo, no se puede valuar
    IF ( v_valor_fondo IS NULL ) THEN
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].subcuenta        = p_subcuenta
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].estado_solicitud = gi_solicitud_rechazada
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].cod_rechazo      = gi_no_hay_precio_fondo -- no existe precio de fondo para valuar
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].f_valuacion      = p_fecha_valuacion USING "YYYYMMDD"
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_avis       = 0
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_pesos      = 0

        -- se incrementa el indice del retiro consultado
        LET g_indice_retiro = g_indice_retiro + 1

    ELSE
   
        -- se verifica si el importe es 0 se rechaza sin saldo
        --IF ( p_importe_aivs = 0 ) THEN
        --  LET p_estado_solicitud = gi_solicitud_rechazada
        --  LET p_cod_rechazo      = gi_sin_saldo
        --END IF

        -- MARZ0 2015 ICHP

        IF p_estado_solicitud <> gi_solicitud_rechazada THEN 

            IF ( p_importe_aivs = 0 AND p_importe_tesofe = 0) THEN

                LET p_estado_solicitud = gi_solicitud_rechazada
                LET p_cod_rechazo      = gi_sin_saldo
                LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
                IF p_cod_rechazo <> 0 THEN
                    -- Busca la descripcion del error para regresarla en la consulta
                    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = "";
                    SELECT des_larga
                    INTO   ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo
                    FROM   ret_rechazo_generico
                    WHERE  cod_rechazo = p_cod_rechazo;
                    IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
                        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
                    END IF
                END IF 
            END IF 

        END IF 
      
        -- MARZ0 2015 ICHP

        -- se genera el registro de disponibilidad      
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].subcuenta        = p_subcuenta
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].f_valuacion      = p_fecha_valuacion USING "YYYYMMDD"
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_avis       = p_importe_aivs
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_pesos      = ((p_importe_aivs - p_importe_tesofe) * v_valor_fondo) + p_importe_tesofe
        -- se incrementa el indice del retiro consultado
        LET g_indice_retiro = g_indice_retiro + 1
    END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws(p_nss, p_estado_solicitud, p_cod_rechazo, p_importe_viv7292, p_num_referencia)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv7292  DECIMAL(12,2), -- Importe de vivienda 72-92
         p_num_referencia   CHAR(18) -- referencia bancaria
{         
    -- se construye la respuesta del ws
    LET ret_respuesta.nss         = p_nss
    LET ret_respuesta.res_op      = p_estado_solicitud
    LET ret_respuesta.cod_rechazo = p_cod_rechazo
    LET ret_respuesta.imp_viv7292 = p_importe_viv7292
    LET ret_respuesta.num_ref     = p_num_referencia
}
END FUNCTION




{
======================================================================
Clave: 
Nombre: fn_busca_nss_pruebas
Fecha creacion: Febrero 21, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Se validan algunos NSS para forzar el resultado de la consulta con Procesar

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_nss_pruebas(p_nss)
DEFINE   p_nss           LIKE afi_derechohabiente.nss
DEFINE   v_res_diag      SMALLINT,       --diagnostico de la consulta del saldo en la afore
         v_res_estatus   SMALLINT,
         v_cod_rechazo   SMALLINT 

   LET v_res_diag = 101
   LET v_res_estatus = 101
   LET v_cod_rechazo = 0 
   
   IF (p_nss = '32775507521') THEN -- 999
      LET v_res_estatus = 102
      LET v_cod_rechazo = gi_101_102
   END IF 
   IF (p_nss = '32775707832') THEN --  22
      LET v_res_estatus = 201
      LET v_cod_rechazo = gi_101_201
   END IF 
   IF (p_nss = '32785717474') THEN -- 820  
      LET v_res_estatus = 202
      LET v_cod_rechazo = gi_101_202
   END IF 
   IF (p_nss = '32786016785') THEN -- 597
      LET v_res_estatus = 203
      LET v_cod_rechazo = gi_101_203
   END IF 
   IF (p_nss = '32795517583') THEN -- 821
      LET v_res_estatus = 204
      LET v_cod_rechazo = gi_101_204
   END IF 
   IF (p_nss = '32795705089') THEN -- 201
      LET v_res_estatus = 205
      LET v_cod_rechazo = gi_101_205
   END IF 
   IF (p_nss = '32795712358') THEN -- 822
      LET v_res_estatus = 207
      LET v_cod_rechazo = gi_101_207
   END IF 
   IF (p_nss = '32805611988') THEN -- 823
      LET v_res_estatus = 208
      LET v_cod_rechazo = gi_101_208
   END IF 
   IF (p_nss = '32805706010') THEN -- 824
      LET v_res_estatus = 211
      LET v_cod_rechazo = gi_101_211
   END IF 
   IF (p_nss = '32806363100') THEN -- 825
      LET v_res_estatus = 212
      LET v_cod_rechazo = gi_101_212
   END IF 
   IF (p_nss = '32815612406') THEN -- 826
      LET v_res_estatus = 213
      LET v_cod_rechazo = gi_101_213
   END IF 
   IF (p_nss = '32815712685') THEN -- 827
      LET v_res_estatus = 216
      LET v_cod_rechazo = gi_101_216
   END IF 
   IF (p_nss = '32816212677') THEN -- 828
      LET v_res_estatus = 217
      LET v_cod_rechazo = gi_101_217
   END IF 
   IF (p_nss = '32826222377') THEN -- 829
      LET v_res_estatus = 219
      LET v_cod_rechazo = gi_101_219
   END IF 
   IF (p_nss = '32835707590') THEN -- 836
      LET v_res_estatus = 220
      LET v_cod_rechazo = gi_101_220
   END IF 
   IF (p_nss = '32845701138') THEN -- 837
      LET v_res_estatus = 221
      LET v_cod_rechazo = gi_101_221
   END IF 
   IF (p_nss = '84745604185') THEN -- 830
      LET v_res_estatus = 223
      LET v_cod_rechazo = gi_101_223
   END IF 
   IF (p_nss = '84765703362') THEN -- 127
      LET v_res_estatus = 127
      LET v_cod_rechazo = gi_fallo_consulta_saldo_afore
   END IF 
         
   RETURN v_res_diag, v_res_estatus, v_cod_rechazo

END FUNCTION

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
      CALL fn_busca_caso_crm(p_nss) RETURNING v_regreso, arr_casos_crm 
      DISPLAY "datos retorno: >>", v_regreso, arr_casos_crm[1].*
      
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
DISPLAY "final: ",v_regreso
RETURN v_regreso

END FUNCTION
 
FUNCTION f_guarda_cve_afore(p_nss, p_cve_afore)
DEFINE p_nss       CHAR(11)
DEFINE p_cve_afore CHAR(3)
DEFINE v_cuantos   SMALLINT

    SELECT COUNT(*)
    INTO   v_cuantos
    FROM   ret_disp_afore_nss
    WHERE  nss = p_nss

    IF v_cuantos > 0 THEN 
        DELETE FROM ret_disp_afore_nss WHERE nss = p_nss;
        INSERT INTO ret_disp_afore_nss VALUES (p_nss, p_cve_afore);
    ELSE 
        INSERT INTO ret_disp_afore_nss VALUES (p_nss, p_cve_afore);
    END IF 

    
END FUNCTION 