--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW34                                                  #
#OBJETIVO          => WS CONSULTA DE SALDOS DISPONIBLES PARA RETIRO DE        #
#                     AMORTIZACIONES EXCEDENTES                               #
#FECHA INICIO      => 25-OCT-2016                                             #
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
         rfc              CHAR(13), -- rfc del trabajador
         causal_ret_fa    SMALLINT, -- causal del retiro de fondo de ahorro
         nrp              CHAR(11), -- NRP, usado para plan privado de pension en ret FA
         f_inicio_pension CHAR(8) , -- fecha de inicio de pension en formato AAAAMMDD
         grupo_ley73      SMALLINT, -- num. de grupo al que perteneces segun retiro Ley 73
         num_credito      CHAR(10) -- Numero de credito necesario para retiro de amortizaciones excedentes
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_saldos_disponibles_out  RECORD
         nss                 CHAR(11), --- Número de seguridad social del trabajador
         rfc                 CHAR(13), -- rfc del trabajador
         saldo_x_retiro      DYNAMIC ARRAY OF RECORD
           modalidad_retiro   SMALLINT, -- modalidad del retiro segun catalogo
           subcuenta          SMALLINT, -- subcuenta de inversion
           estado_solicitud   SMALLINT, -- estado de la solicitud
           cod_rechazo        SMALLINT, -- codigo de rechazo
           des_rechazo        CHAR(100),    -----  *****************************************
           f_valuacion        CHAR(8), -- fecha de valuacion de AIVs en formato AAAAMMDD
           monto_avis         DECIMAL(22,6), -- saldo en AIVs
           monto_pesos        DECIMAL(22,2), -- saldo en pesos equivalente a AIVs por valor accion
           pago_dap           DECIMAL(1,0)   -- se agrega este dato para indicar so debe o no pagar por DAP 1-Candidato a pago via DAP, 2-No es candidato a pago via DAP
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW44."
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
    CALL ERRORLOG("Iniciando servidor de Disponibilidad 2.0 ...")

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
DEFINE v_webservice         com.WebService       # WebService
DEFINE op                   com.WebOperation     # Operation of a WebService
DEFINE v_service_NameSpace  STRING -- namespace del servicio
DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
DEFINE v_resultado          INTEGER
DEFINE v_urn                STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroSaldosDisponiblesAmortExced", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_saldos_disponibles_amort_exced","fn_ret_saldos_disponibles_amort_exced",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_ret_saldos_disponibles_amort_exced")

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
            CALL ERRORLOG("Se registro el servicio consulta de saldos disponibles para retiro Amortizaciones Excedentes")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de saldos disponibles para retiro Amortizaciones Excedentes': " || STATUS)
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
Nombre: fn_ret_saldos_disponibles_amort_exced
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros de Amortizaciones Excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_saldos_disponibles_amort_exced()
DEFINE v_indice_retiro SMALLINT,
       v_nss            LIKE afi_fondo72.nss,
       v_rfc            LIKE afi_fondo72.rfc,
	   v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	   v_ruta_log        STRING,
	   v_cadena          STRING

    -- se responde el servicio para pruebas
    LET ws_ret_cons_saldos_disponibles_out.nss = ws_ret_cons_saldos_disponibles_in.nss
    LET ws_ret_cons_saldos_disponibles_out.rfc = ws_ret_cons_saldos_disponibles_in.rfc


    LET v_nss = ws_ret_cons_saldos_disponibles_in.nss
    LET v_rfc = ws_ret_cons_saldos_disponibles_in.rfc

    DISPLAY "Validando saldos para:"
    DISPLAY "NSS: ", v_nss
    DISPLAY "RFC: ", v_rfc

    -- se obtiene la ruta ejecutable
    SELECT ruta_bin
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW44."
    LET v_cadena   = v_nss
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"

    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    -- se inicia el indice del retiro que se va a consultar
    LET g_indice_retiro = 1

    -- se verifica si se recibieron datos para consultar fondo de ahorro
    -- se quita la validacion del RFC segun requerimiento PRODINF-876
    --IF ( v_rfc IS NOT NULL ) THEN


    IF ((ws_ret_cons_saldos_disponibles_in.causal_ret_fa IS NOT NULL) AND (ws_ret_cons_saldos_disponibles_in.causal_ret_fa <> 0) ) THEN
        DISPLAY "Validando Fondo de ahorro"
        DISPLAY "Parametros de entrada " 
        DISPLAY "NSS                     : ", v_nss
        DISPLAY "RFC                     : ", v_rfc
        DISPLAY "Causal                  : ", ws_ret_cons_saldos_disponibles_in.causal_ret_fa
        DISPLAY "NRP                     : ", ws_ret_cons_saldos_disponibles_in.nrp
        DISPLAY "Fecha inicio de Pensión : ", ws_ret_cons_saldos_disponibles_in.f_inicio_pension
        
        CALL fn_ret_disponibilidad_fondo_ahorro(v_nss, v_rfc, 
                                              ws_ret_cons_saldos_disponibles_in.causal_ret_fa,
                                              ws_ret_cons_saldos_disponibles_in.nrp,
                                              ws_ret_cons_saldos_disponibles_in.f_inicio_pension,
                                              TRUE)
    END IF 
    --ELSE 
    --    IF ((ws_ret_cons_saldos_disponibles_in.causal_ret_fa IS NOT NULL) AND (ws_ret_cons_saldos_disponibles_in.causal_ret_fa <> 0) ) THEN 
    --        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
    --    END IF 
    --END IF
   
    -- se verifica si se validara retiro Ley 73
    IF ( (ws_ret_cons_saldos_disponibles_in.grupo_ley73 IS NOT NULL) AND (ws_ret_cons_saldos_disponibles_in.grupo_ley73 <> 0) ) THEN
        DISPLAY "Validando Ley 73"
        CALL fn_ret_disponibilidad_ley73(v_nss, ws_ret_cons_saldos_disponibles_in.grupo_ley73, TRUE)
    END IF

    -- se verifica si hay retiro por amortizaciones excedentes
    DISPLAY "Validando Amortizaciones excedentes"
    CALL fn_ret_disponibilidad_amort_excedentes(v_nss, v_rfc, ws_ret_cons_saldos_disponibles_in.num_credito, TRUE)

    -- se verifica si hay retiro por aportaciones voluntarias
    DISPLAY "Validando Aportaciones voluntarias"
    CALL fn_ret_disponibilidad_aport_voluntarias(v_nss, TRUE)
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de vivienda segun ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     19 Dic 2012             - Se verifica que no este marcado de retiro
                                        Ley73 antes de proceder con el resto de validaciones
Ivan Vega     11mar2014               - Ley 73, grupos 2 y 3, no pagan vivienda 92
======================================================================
}
FUNCTION fn_ret_disponibilidad_ley73(p_nss, p_grupo_ley73, p_es_consulta)
DEFINE p_nss                  CHAR(11), -- NSS
       p_grupo_ley73          SMALLINT, -- grupo de retiro segun Ley73
       p_es_consulta          SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess          SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart          LIKE ret_datamart.id_datamart,
       v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
       v_aivs_vol             DECIMAL(24,6), -- saldo AIVs de Aportaciones voluntarias
       v_pesos_viv92          DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97          DECIMAL(22,2), -- saldo pesos de viv97
       v_pesos_vol            DECIMAL(22,2), -- saldo pesos de Aportaciones voluntarias
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
    
    LET v_aivs_viv92         = 0
    LET v_aivs_viv97         = 0
    LET v_aivs_vol           = 0
    LET v_pesos_viv92        = 0
    LET v_pesos_viv97        = 0
    LET v_pesos_vol          = 0
    LET v_id_cliente         = 30

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
            IF p_grupo_ley73 = 4 THEN 
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
                IF v_tiene_rch_siaff > 0 THEN 
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_rechazo_banco_siaff, 12, 0, TODAY,0)
                ELSE 
                    SELECT COUNT(*)
                    INTO   v_tiene_cta_clabe
                    FROM   ret_solicitud_generico rg,
                           ret_ley73_generico     rlg
                    WHERE  rg.id_solicitud       = v_n_referencia
                    AND    rg.estado_solicitud   IN (60)
                    AND    rg.id_solicitud       = rlg.id_solicitud
                    AND    rg.modalidad_retiro   = 3
                    AND    rlg.gpo_ley73         = 4
                    IF v_tiene_cta_clabe > 0 THEN 
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
                        IF v_tiene_cta_clabe = 0 THEN 
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 8, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 4, 0, TODAY,0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_pendiente_envio_clabe, 12,0, TODAY,0)
                        ELSE 
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
                IF v_regimen = 97 THEN
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_regimen_diferente_73, 12,0, TODAY,0)
                ELSE 
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
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 8, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 4, 0, TODAY,0)
                                CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_pension_vigente, 12,0, TODAY,0)
                            ELSE 
                                -- se obtiene el saldo de vivienda 92 y vivienda 97
                                -- se obtiene el saldo de viv92
                                -- 11mar2014. Grupos 2 y 3 no pagan vivienda 92
                                IF ( p_grupo_ley73 = 2 OR p_grupo_ley73 = 3 ) THEN
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

                                LET v_sdo_tot_aivs  = 0
                                LET v_sdo_tot_pesos = 0
                                LET v_sdo_tot_aivs  = v_aivs_viv92 + v_aivs_viv97 + v_aivs_vol
                                LET v_sdo_tot_pesos = v_pesos_viv92 + v_pesos_viv92 + v_pesos_vol
                            
                                -- se verifica que grupo de retiro llego
                                CASE p_grupo_ley73
                                    -- GRUPO 1
                                    WHEN 1
                                        -- si la fecha de inicio de pension es igual o posterior al 13 de enero de 2012
                                        IF ( v_f_resolucion >= "01/13/2012" ) THEN
                                            -- se consulta el saldo en la afore via WS
                                            --CALL fn_consulta_saldo_vivienda_afore(p_nss, v_id_cliente)
                                            --RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv92
                                            --IF v_diagnostico = 101 AND v_estatus = 101 THEN 
                                                -- se valida la solicitud para un grupo 1
                                                CALL fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97 + v_aivs_vol, v_f_resolucion, p_es_consulta)
                                            --ELSE 
                                                -- la fecha es invalida para grupo 1
                                            --    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 8, 0, TODAY,0)
                                            --    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 4, 0, TODAY,0)
                                            --    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fallo_consulta_saldo_afore, 12,0, TODAY,0)
                                            --END IF 
                                        ELSE
                                            -- la fecha es invalida para grupo 1
                                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 8, 0, TODAY,0)
                                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 4, 0, TODAY,0)
                                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 12,0, TODAY,0)
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

======================================================================
}
FUNCTION fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97, v_fecha_resolucion, p_es_consulta)
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
       v_fecha_resolucion DATE, -- fecha de resolucion en el SPESS
       v_saldo_total      DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_consulta         STRING,
       v_rch_cod          SMALLINT,
       v_existe_43_bis    SMALLINT,
       v_rch_desc         CHAR(40),
       v_causal_paso      SMALLINT,
      v_cod_rechazo       SMALLINT,  
       v_diagnostico      SMALLINT,       --diagnostico de la consulta del saldo en la afore
       v_estatus          SMALLINT        -- estatus de la cuenta individual segun la consulta del saldo en la Afore


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
        IF ( v_rch_cod <> 0 ) THEN
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
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                    LET v_causal_paso = 0;
                ELSE
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
                        IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                             v_rch_cod = 205 OR v_rch_cod = 210 OR v_rch_cod = 211 OR 
                             v_rch_cod = 214 OR v_rch_cod = 215 OR v_rch_cod = 216 OR 
                             v_rch_cod = 217 OR v_rch_cod = 221 OR v_rch_cod = 231 ) THEN
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
            -- se consulta el saldo en la afore via WS
            CALL fn_consulta_saldo_vivienda_afore(p_nss, 30)
            RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,v_cod_rechazo
            CALL fn_guarda_consulta_ws_vent_afore(p_nss, 3, 3, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                  v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', '', '', 1)
            DISPLAY "Diagnóstico devuelto >", v_diagnostico, "<"
            DISPLAY "Estatus devuelto     >", v_estatus, "<"
            IF v_diagnostico = 101 AND v_estatus = 101 THEN 
                LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
                -- si el saldo es mayor a cero
                IF ( v_saldo_total > 0 ) THEN
                    LET v_existe_43_bis = 0
                    SELECT COUNT(*)
                    INTO   v_existe_43_bis
                    FROM   sfr_marca_activa a, afi_derechohabiente b
                    WHERE  a.id_derechohabiente = b.id_derechohabiente
                    AND    b.nss                = p_nss
                    AND    a.marca IN (202,212,218,232)
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
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv92 + v_aivs_viv97, TODAY, 0)
                    END IF 

                ELSE 
                    -- se rechaza por insuficiencia de saldo
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)   
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 12, 0, TODAY,0)   
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
        IF ( v_rch_cod <> 0 ) THEN
        ---- no se paga ninguna subcuenta si tien credito
            -- se verifica si el credito es de tipo 43BIS
            --IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
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
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                    LET v_causal_paso = 0;
                ELSE
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
                        IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                             v_rch_cod = 205 OR v_rch_cod = 210 OR v_rch_cod = 211 OR 
                             v_rch_cod = 214 OR v_rch_cod = 215 OR v_rch_cod = 216 OR 
                             v_rch_cod = 217 OR v_rch_cod = 221 OR v_rch_cod = 231 ) THEN
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
            AND    marca IN (202, 212, 218, 223, 232, 233)
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
        DISPLAY "Se consulta la convivencia de marcas " 
        LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,803)";

        DISPLAY "La sentencia SQL ,", v_consulta

        PREPARE s_con_convive_g_4 FROM v_consulta
        EXECUTE s_con_convive_g_4 INTO v_rch_cod, v_rch_desc
        DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod
        IF ( v_rch_cod <> 0 ) THEN
            -- se verifica si el credito es de tipo 43BIS
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
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 8, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 4, 0, TODAY,0)
                    CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, v_causal_paso, 12, 0, TODAY,0)
                    LET v_causal_paso = 0;
                ELSE
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
                        IF ( v_rch_cod = 201 OR v_rch_cod = 203 OR v_rch_cod = 204 OR 
                             v_rch_cod = 205 OR v_rch_cod = 210 OR v_rch_cod = 211 OR 
                             v_rch_cod = 214 OR v_rch_cod = 215 OR v_rch_cod = 216 OR 
                             v_rch_cod = 217 OR v_rch_cod = 221 OR v_rch_cod = 231 ) THEN
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
            AND    marca IN (202, 212, 218, 223, 232, 233)
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
                        
                        -- el saldo es retirable
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY,0)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, v_monto_transferido)
                        CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv97, TODAY, v_monto_transferido)
                    ELSE
                        -- el saldo retirable es lo existente en viv92 y viv97
                        -- VIVIENDA 92
                        CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY,0)

                        -- VIVIENDA 97
                        IF ( v_aivs_viv97 > 0 ) THEN
                            -- saldo retirable
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
                            CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 12, v_aivs_viv97, TODAY, 0)
                        ELSE
                            -- sin saldo
                            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY,0)                             
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

{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_amort_excedentes
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de un credito por amortaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_disponibilidad_amort_excedentes(p_nss, p_rfc, p_num_credito, p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                LIKE afi_derechohabiente.rfc,
       p_num_credito        CHAR(10), -- numero de credito
       p_es_consulta        SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tabla_saldo        VARCHAR(40),
       v_saldo_aivs         DECIMAL(22,6),
       v_saldo_pesos        DECIMAL(22,2),
       v_fecha_saldo        DATE,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_n_referencia       DECIMAL(9,0), -- num de solicitud en marca activa
       v_sql                STRING,
       v_consulta           STRING,
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40)


    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa 
    
    -- Si no se encontro el nss se busca con la marca de unificado 
    IF v_id_derechohabiente IS NULL THEN 
        SELECT af.id_derechohabiente
        INTO   v_id_derechohabiente 
        FROM   sfr_marca_activa sm,
               afi_derechohabiente af
        WHERE  sm.id_derechohabiente = af.id_derechohabiente
        AND    sm.marca = 151
        AND    af.nss = p_nss  -- Se implemento esta busqueda por los que estan inhabilitados por unificacion  requerimiento PRODINF-935
    END IF 

    -- si se encontro el NSS
    IF ( v_id_derechohabiente IS NOT NULL ) THEN
   
        -- se verifica si el NSS tiene saldo de amortizaciones excedentes
        LET v_sql = "\nSELECT tabla_saldo",
        "\nFROM   safre_sdo@vivws_tcp:glo_saldo",
        "\nWHERE  ind_saldo = 1" -- la tabla activa

        PREPARE sid_tabla FROM v_sql
        EXECUTE sid_tabla INTO v_tabla_saldo

        -- se obtiene el saldo de su cuenta de amortizaciones excedentes     
        LET v_sql = "\n SELECT monto_acciones,",
        "\n        monto_pesos   ,",
        "\n        f_saldo        ",
        "\n FROM   safre_sdo@vivws_tcp:", v_tabla_saldo,
        "\n WHERE  id_derechohabiente = ?",
        "\n AND    subcuenta = 46",
        "\n AND    fondo_inversion <> 0 "

        PREPARE sid_amortexced FROM v_sql
        EXECUTE sid_amortexced USING v_id_derechohabiente
        INTO v_saldo_aivs, v_saldo_pesos, v_fecha_saldo

        -- se verifica si no esta marcado
        SELECT n_referencia
        INTO   v_n_referencia
        FROM   sfr_marca_activa
        WHERE  id_derechohabiente = v_id_derechohabiente
        AND    marca = 810 -- amort excedentes

        LET v_consulta = "EXECUTE FUNCTION fn_consulta_convivencia('",p_nss,"',0,810)";

        DISPLAY "La sentencia SQL ,", v_consulta

        PREPARE s_con_convive_amort FROM v_consulta
        EXECUTE s_con_convive_amort INTO v_rch_cod, v_rch_desc
        DISPLAY "El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod

        -- si la referencia existe, entonces el derechohabiente esta marcado y no procede su retiro
        IF ( v_rch_cod = 810 ) THEN
            -- el NSS ya esta marcado
            CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_solicitud_en_tramite, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
        ELSE        
            IF v_rch_cod <> 0 THEN 
                CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_error_marca_no_convive, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
            ELSE 
                -- si el NSS tiene saldo
                IF ( v_saldo_aivs IS NOT NULL AND v_saldo_aivs > 0 ) THEN

                    -- se escribe la respuesta
                    CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_aceptada, 0, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)

                ELSE
                    -- no tiene saldo
                    CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_sin_saldo, TODAY, 0, 0)
                END IF
            END IF  
        END IF
    ELSE
        -- el nss no existe
        CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, TODAY, 0, 0)      
    END IF

    -- se incrementa el indice
    --   LET g_indice_retiro = g_indice_retiro + 1

END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_aport_voluntarias
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
por concepto de aportaciones voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_disponibilidad_aport_voluntarias(p_nss, p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                LIKE afi_derechohabiente.rfc,
       p_num_credito        CHAR(10), -- numero de credito
       p_es_consulta        SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tabla_saldo        VARCHAR(40),
       v_saldo_aivs         DECIMAL(22,6),
       v_saldo_pesos        DECIMAL(22,2),
       v_fecha_saldo        DATE,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_n_referencia       DECIMAL(9,0), -- num de solicitud en marca activa
       v_sql                STRING

    -- se obtiene el id_derechohabiente
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa

    
    -- si se encontro el NSS
    IF ( v_id_derechohabiente IS NOT NULL ) THEN

        -- se verifica si el NSS tiene saldo de amortizaciones excedentes
        LET v_sql = "\nSELECT tabla_saldo",
                  "\nFROM   safre_sdo@vivws_tcp:glo_saldo",
                  "\nWHERE  ind_saldo = 1" -- la tabla activa

        PREPARE sid_tablaav FROM v_sql
        EXECUTE sid_tablaav INTO v_tabla_saldo

        -- se obtiene el saldo de su cuenta de aportaciones voluntarias
        LET v_sql = "\n SELECT monto_acciones,",
                  "\n        monto_pesos   ,",
                  "\n        f_saldo        ",
                  "\n FROM   safre_sdo@vivws_tcp:", v_tabla_saldo,
                  "\n WHERE  id_derechohabiente = ?",
                  "\n AND    subcuenta = 45 ", -- aportaciones voluntarias
                  "\n AND    fondo_inversion <> 0 "
                  
        PREPARE sid_avol FROM v_sql
        EXECUTE sid_avol USING v_id_derechohabiente
                       INTO v_saldo_aivs, v_saldo_pesos, v_fecha_saldo


        -- se verifica si no esta marcado
        SELECT n_referencia
        INTO   v_n_referencia
        FROM   sfr_marca_activa
        WHERE  id_derechohabiente = v_id_derechohabiente
        AND    marca = 809 -- aport voluntarias

        -- si la referencia existe, entonces el derechohabiente esta marcado y no procede su retiro
        IF ( v_n_referencia IS NOT NULL ) THEN
            -- el NSS ya esta marcado
            CALL fn_respuesta_ws_aport_voluntarias(p_nss, gi_solicitud_rechazada, gi_solicitud_en_tramite, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
        ELSE        
            -- si el NSS tiene saldo
            IF ( v_saldo_aivs IS NOT NULL AND v_saldo_aivs > 0 ) THEN
             
                -- se escribe la respuesta
                CALL fn_respuesta_ws_aport_voluntarias(p_nss, gi_solicitud_aceptada, 0, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)

            ELSE
                -- no tiene saldo
                CALL fn_respuesta_ws_aport_voluntarias(p_nss, gi_solicitud_rechazada, gi_sin_saldo, TODAY, 0, 0)
            END IF
        END IF
    ELSE
        -- el nss no existe
        CALL fn_respuesta_ws_aport_voluntarias(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, TODAY, 0, 0)      
    END IF

    -- se incrementa el indice
    LET g_indice_retiro = g_indice_retiro + 1
END FUNCTION

{
======================================================================
Nombre: fn_ret_disponibilidad_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene disponibilidad de retiro de su cuenta
de fondo anterior

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     19 Dic 2013            - Se verifica si el NSS ya esta marcado,
                                       y de ser asi, se rechaza la disponibilidad
======================================================================
}
FUNCTION fn_ret_disponibilidad_fondo_ahorro(p_nss, p_rfc, p_causal, p_nrp, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                CHAR(13), -- RFC
       p_causal             SMALLINT, -- causal de retiro
       p_nrp                CHAR(11), -- NRP
       v_f_inicio_pension   CHAR(8), -- fecha de inicio de pension en formato AAAAMMDD
       p_es_consulta        SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_f_cadena           VARCHAR(10), -- fecha formateada para transformar a DATE
       v_f_pension          DATE, -- fecha de inicio de pension en formato DATE
       v_count_bnd          SMALLINT   ,
       v_cod_rechazo        SMALLINT   ,
       v_id_solicitud       SMALLINT   ,
       v_conteo_nss         SMALLINT   ,
       v_ruta_ejecutable    VARCHAR(40),
       v_ruta_log           STRING,
       v_cadena             STRING,
       v_id_afi_fondo72     DECIMAL(9,0), -- id derechohabiente en fondo72
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_n_referencia       LIKE sfr_marca_Activa.n_referencia, -- para buscar nss marcado
       v_saldo              DECIMAL(22,2),-- saldo del derechohabiente
       v_tipo_credito       SMALLINT,
       v_tipo_originacion   SMALLINT 

    -- solo se buscara al derechohabiente por el NSS, se quita la busqueda por RFC   
    -- se verifica si se tiene NSS y RFC
    --IF ( p_nss IS NOT NULL AND p_rfc IS NOT NULL ) THEN
--
        -- para validar el NSS se verifica que exista al menos una vez
        --SELECT COUNT(*)
        --INTO   v_conteo_nss 
        --FROM   afi_fondo72
        --WHERE  nss = p_nss
        --AND    rfc = p_rfc
--
        --IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
            -- si no se encontraron coincidencias
            --CALL ERRORLOG("No existe el nss-rfc")
            --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
            --RETURN    
        --ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            --IF ( v_conteo_nss > 1 ) THEN
                --CALL ERRORLOG("El RFC/NSS devuelve más de un registro")
                --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0)
                --RETURN
            --END IF
        --END IF
    --ELSE
        -- se verifica si se recibio NSS
        IF ( p_nss IS NOT NULL ) THEN

            -- para validar el NSS se verifica que exista al menos una vez
            SELECT COUNT(*)
            INTO   v_conteo_nss 
            FROM   afi_fondo72
            WHERE  nss = p_nss
            AND    ind_estado_cuenta = 0  -- cuenta Activa

            IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
                -- No existe el nss
                CALL ERRORLOG("No existe el NSS")
                CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
                RETURN
            ELSE
                -- si se encuentra mas de uno, no procede su solicitud
                IF ( v_conteo_nss > 1 ) THEN
                    CALL ERRORLOG("El NSS devuelve más de un registro")
                    CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0)
                    RETURN
                END IF
            END IF 
        ELSE
            CALL ERRORLOG("No existe el NSS")
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
            RETURN
            -- se busca usando el RFC
            --SELECT COUNT(*)
            --INTO   v_conteo_nss 
            --FROM   afi_fondo72
            --WHERE  rfc = p_rfc
--
            -- si no se encontraron coincidencias
            --IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
                -- No existe el nss
                --CALL ERRORLOG("No existe el rfc")
                --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0)
                --RETURN
            --ELSE
                -- si se encuentra mas de uno, no procede su solicitud
                --IF ( v_conteo_nss > 1 ) THEN
                    --CALL ERRORLOG("El RFC devuelve más de un registro")
                    --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0)
                    --RETURN
                --END IF         
            --END IF

        END IF
    --END IF
  
    DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
    DISPLAY "NSS evaluado: ", p_nss


    LET v_conteo_nss          = 0

    IF ( p_nss IS NOT NULL ) THEN
        CALL ERRORLOG("Validando solicitud para NSS: " || p_nss)
    END IF

--    IF ( p_rfc IS NOT NULL ) THEN
--        CALL ERRORLOG("Validando solicitud para RFC: " || p_rfc)
--    END IF
       
    -- se valida si existe otra solicitud en tramite
    IF ( fn_rechazo_por_tramite_fondo_ahorro(p_nss, p_rfc) ) THEN
        CALL ERRORLOG("Se rechaza porque existe otra solicitud en tramite para el mismo NSS")

        -- se responde al WS que se tiene una solicitud en tramite
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0)
        RETURN 
    END IF
   
    -- si no se rechazo por tramite    
    -- valida el causal de retiro seleccionado 
    SELECT COUNT(*) 
    INTO   v_count_bnd
    FROM   ret_causal_retiro
    WHERE  causal_retiro = p_causal;   

    DISPLAY "Verifica causal retiro :", v_count_bnd

    IF ( v_count_bnd < 1 OR v_count_bnd IS NULL ) THEN
        -- se le regresa el cliente rechazo ya qe no coincide con la tabla de causal retiro
        -- debe ser entre 01-04
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0)     
        CALL ERRORLOG("El causal de retiro es invalido")
        RETURN 
    END IF      

    -- se verifica si el derechohabiente tiene un credito vigente
    IF ( fn_trabajador_credito_vigente(p_nss) ) THEN
        -- se responde negavito por tener un credito
        CALL fn_trabajador_tipo_credito(p_nss) RETURNING v_tipo_credito, v_tipo_originacion
        IF v_tipo_credito = 1 AND v_tipo_originacion = 1 THEN -- 1-Credito Tradicional, 1-Transferencia de Acreditados
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_tiene_credito_vigente, 0)
            RETURN
        END IF 
    END IF
   
    -- se verifica si tiene saldo
    CALL fn_recupera_saldo_fa(p_nss, p_rfc) RETURNING v_saldo

    IF ( v_saldo <= 0 ) THEN
        -- solicitud rechazada por no contar con saldo suficiente
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_saldo, 0)
        RETURN
    END IF

    -- se transforma la fecha AAAAMMDD a formato fecha
    LET v_f_cadena = v_f_inicio_pension[5,6], "/", v_f_inicio_pension[7,8], "/", v_f_inicio_pension[1,4]
    LET v_f_pension = DATE(v_f_cadena)


    CALL ERRORLOG("Verificando marca FA")
    -- se verifica la marca
    SELECT id_derechohabiente
    INTO   v_id_derechohabiente 
    FROM   afi_fondo72
    WHERE  nss = p_nss
    AND    ind_estado_cuenta = 0  -- cuenta Activa
--    AND    rfc = p_rfc
   
    -- si no se encontro
    IF ( v_id_derechohabiente IS NULL ) THEN        
        -- se verifica si esta en la tabla de id_derechohabientes nuevos para fondo72
        SELECT id_derechohabiente
        INTO   v_id_derechohabiente
        FROM   afi_fondo72_d
        WHERE  nss = p_nss
--        AND    rfc = p_rfc
    END IF

    -- si se encontro el id_derechohabiente, se verifica si tiene marca
    IF ( v_id_derechohabiente IS NOT NULL ) THEN

        CALL ERRORLOG("id_der encontrado: " || v_id_derechohabiente)

        SELECT n_referencia
        INTO   v_n_referencia
        FROM   sfr_marca_activa
        WHERE  id_derechohabiente = v_id_derechohabiente
        AND    marca = 802

        -- si aparece, se rechaza
        IF ( v_n_referencia IS NOT NULL ) THEN
            CALL ERRORLOG("Marca con referencia: " || v_n_referencia)
            -- se rechaza por estar en tramite
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0)
            RETURN
        END IF
    END  IF
   
    -- SI PASO LAS VALIDACIONES ANTERIORES
    -- se realiza la verificacion de la solicitud de acuerdo al tipo de causal de retiro recibida
    CASE p_causal
        WHEN 1 -- termino de relacion laboral
            DISPLAY "Termino relacion laboral"
            CALL fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, v_saldo, p_es_consulta)

        WHEN 2 -- pension IMSS
            DISPLAY "IMSS"
            CALL fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, v_saldo, p_es_consulta)

        WHEN 3 -- plan privado de pension
            DISPLAY "Plan privado pension"
            CALL fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, v_saldo, v_f_pension, p_es_consulta)

        WHEN 4 -- plan privado
            DISPLAY "defuncion"
            CALL fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_f_pension, v_saldo, p_es_consulta)

        OTHERWISE
            DISPLAY "otra causal de FA"
            -- causal de retiro invalido
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0)

    END CASE      

END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_termino_relacion_laboral
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal termino de relacion laboral
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    30 Abril 2013         - Ahora se recibe NSS y/o RFC.
                                      Si se recibe NSS se valida una pension vigente en el SPESS
                                      Si no se recibe NSS, con el RFC se busca 
Ivan Vega    19 Feb 2014           - La validacion de prescripcion ya no aplica segun reunion en
                                     INFONAVIT 18 feb 2014
======================================================================
}
FUNCTION fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, p_saldo, p_es_consulta)
DEFINE p_nss                           CHAR(11), -- NSS
       p_rfc                           CHAR(13), -- RFC
       p_causal                        SMALLINT, -- causal de retiro
       p_id_beneficiario               SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                        CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno                    CHAR(18), -- Apellido paterno 
       p_ap_materno                    CHAR(18), -- Apellido materno
       p_entidad                       SMALLINT, -- Entidad federativa 
       p_causal_adai                   SMALLINT, -- Causal de adai
       p_saldo                         DECIMAL(22,2),
       p_es_consulta                   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                          SMALLINT      ,
       v_ejecucion_ws                  SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional               DECIMAL(12,2),
       v_referencia_banc               CHAR(12),
       v_fecha_ultima_relacion_laboral DATE, -- ultima fecha de relacion laboral
       v_fecha_nacimiento              DATE, -- fecha de nacimiento del derechohabiente
       v_tiene_credito_vigente         SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess                   SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart                   LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_con_sin_rel_lab               SMALLINT   -- indica si tiene o no relacion laboral
   
    -- se calcula la edad del derechohabiente
--    IF ( p_nss IS NOT NULL AND p_nss <> "00000000000" ) THEN
        -- se calcula usando su nss
        CALL fn_edad_derechohabiente(p_nss) RETURNING r_edad
        CALL ERRORLOG("Edad calculada por NSS: ")
        CALL ERRORLOG(r_edad)
--    ELSE
        -- se calcula usando su rfc
--        CALL fn_edad_derechohabiente_rfc(p_rfc) RETURNING r_edad
--        CALL ERRORLOG("Edad calculada por RFC: ")
--        CALL ERRORLOG(r_edad)
--    END IF

    -- la edad debe ser mayor o igual a 50 anos
    IF ( r_edad >= 50 ) THEN
        CALL ERRORLOG("Edad >= 50, se revisa SPESS")
        -- se verifica que el trabajador tenga una resolucion en el spess
        CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

        -- si no tiene resolucion valida en el spess
        IF ( NOT v_tiene_spess ) THEN
            CALL ERRORLOG("No tiene spess, se revisa ultima relacion laboral")

            -- para verificar la relacion labora, se invoca el web service de consulta
            CALL fn_fecha_ultima_relacion_laboral(p_nss)
            RETURNING v_ejecucion_ws, v_fecha_ultima_relacion_laboral, v_con_sin_rel_lab

            CALL ERRORLOG("Ejecucion WS: ")
            CALL ERRORLOG(v_ejecucion_ws)		   
            CALL ERRORLOG("Fec. de ultima relacion laboral:")
            CALL ERRORLOG(v_fecha_ultima_relacion_laboral)

            -- si se ejecuto correctamente el WS
            IF ( v_ejecucion_ws = 0 ) THEN
                -- se verifica si tiene un ano sin relacion laboral
                IF ( NOT fn_verifica_ano_sin_relacion_laboral(v_fecha_ultima_relacion_laboral, TODAY) ) THEN
                -- derechohabiente sin resolucion de spess ni ano sin relacion laboral o no existe relacion laboral
                    IF v_fecha_ultima_relacion_laboral IS NULL THEN 
                        IF v_con_sin_rel_lab = 1 THEN 
                            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo)
                        ELSE 
                            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo)
                        END IF 
                    ELSE 
                        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_un_ano_relacion_laboral, p_saldo)
                    END IF 
                ELSE
                    -- 19feb2014. Se elimino la verificacion de la prescripcion
                    -- el saldo es retirable
                    IF v_con_sin_rel_lab = 1 THEN 
                        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo)
                    ELSE 
                        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo)
                    END IF             
                END IF
            ELSE
                -- se rechaza porque se detecto un error al consultar el WS de ultima relacion laboral
                -- por lo tanto no es posible validar la solicitud
                CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_ws_rel_laboral_no_disponible, p_saldo)
            END IF
        ELSE
            -- 19feb2014. Se elimino la verificacion de la prescripcion
            -- el monto retirable es dos veces el saldo y es retirable
            CALL ERRORLOG("Term Rel Laboral, edad > 50 y con SPESS, se paga el doble")
            LET p_saldo = p_saldo * 2                  
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo)
        END IF
    ELSE 
        -- solicitud rechazada por que el trabajador no tiene 50 anos cumplidos o mas
        CALL ERRORLOG("Solicitud rechazada porque el derechohabiente no tiene 50 anos o mas")

        -- edad inferior a 50 anos
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_edad_inferior_50_anos, p_saldo)
    END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: 
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones de una solicitud de retiro de Fondo de Ahorro
por resolucion de pension del IMSS
fn_resolucion_pension_imss
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      19Feb2014              - Se elimina la verificacion de la prescripcion de acuerdo
                                        con definicion de INFONAVIT en junta del 18 feb 2014
======================================================================
}
FUNCTION fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, p_saldo, p_es_consulta)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       p_saldo           DECIMAL(22,2),
       p_es_consulta     SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad            SMALLINT      ,
       v_ejecucion_ws    SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional DECIMAL(12,2),
       v_referencia_banc CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess     SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart     LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_f_resolucion    LIKE ret_datamart.f_resolucion, -- fecha de resolucion en el SPESS
       v_anos_prescripcion SMALLINT,
       v_error_det         SMALLINT     --contiene en error por la cual no se encontro resolucion en el spess

    -- se asume que no hay referencia bancaria
    LET v_referencia_banc = "0"
    DISPLAY "Llega causal a validacion de pension IMSS: ", p_causal

    -- se revisa que venga nss, ya que sin este es necesario para todo el proceso
    IF ( p_nss IS NULL OR p_nss = "00000000000" ) THEN
        -- caso invalido para pension del IMSS, se necesita un NSS valido
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_es_necesario, 0)

        -- no puede seguir el proceso
        RETURN
    END IF

    -- se verifica que el trabajador tenga una resolucion de pension valida
    CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

    IF ( v_tiene_spess ) THEN

        -- 19feb2014. Se elimina la verificacin de la prescripcion
        -- el saldo es retirable
        LET p_saldo = p_saldo * 2
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo)
    ELSE
        -- no tiene resolucion en el SPESS
        CALL fn_detalle_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart, v_error_det
        IF (v_error_det <> gi_resolucion_neg_pension AND v_error_det <> gi_porcentaje_menor_50) THEN 
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_resolucion_spess, p_saldo)
        ELSE 
            IF v_error_det = gi_resolucion_neg_pension THEN 
                CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_resolucion_neg_pension, p_saldo)
            END IF 
            IF v_error_det = gi_porcentaje_menor_50 THEN 
                CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_porcentaje_menor_50, p_saldo)
            END IF 
        END IF 
    END IF

    DISPLAY "Finalizado validacion pension imss"

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_plan_privado_pension
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal plan privado de pension 
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      19 Feb 2014            - Se elimina la validacion de prescripcion conforme definicion de
                                        INFONAVIT en junta del 18 feb 2014
======================================================================
}
FUNCTION fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, p_saldo, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_nrp                   LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       p_f_inicio_pension_caracter CHAR(8),
       v_fecha_inicio_pension  DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_es_consulta           SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                  SMALLINT      ,
       r_b_paso                SMALLINT      ,
       v_tanto_adicional       DECIMAL(12,2),
       v_referencia_banc       CHAR(12),
       v_cadena                STRING,
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_diferencia_anos       SMALLINT, -- diferencia en anos
       v_f_inicio_pension      LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_id_datamart           LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess

    -- se verifica si el NRP existe en catalogo
    IF ( fn_nrp_existe_en_catalogo(p_nrp) ) THEN

        -- se obtiene la fecha de inicio de pension
        { la fecha viene como parametro
        CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

        SELECT f_inicio_pension
        INTO   v_f_inicio_pension
        FROM   ret_datamart
        WHERE  id_datamart = v_id_datamart
        }

        -- 19feb2014. Se elimina la validacion de prescripcion
        -- el saldo es retirable
        LET p_saldo = p_saldo * 2
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo)
    ELSE
        -- se rechaza por inexistencia de NRP
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nrp_no_encontrado, p_saldo)
    END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retiro_fa_defuncion
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal defuncion
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      19 feb 2014            - Se elimina la validacion de prescripcion conforme definicion de 
                                        INFONAVIT en junta de 18 feb 2014
======================================================================
}
FUNCTION fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_fecha_inicio_pension, p_saldo, p_es_consulta)
DEFINE p_nss                           CHAR(11), -- NSS
       p_rfc                           CHAR(13), -- RFC
       p_causal                        SMALLINT, -- causal de retiro
       p_nrp                           LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_fecha_inicio_pension          DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario               SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                        CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno                    CHAR(18), -- Apellido paterno 
       p_ap_materno                    CHAR(18), -- Apellido materno
       p_entidad                       SMALLINT, -- Entidad federativa 
       p_causal_adai                   SMALLINT, -- Causal de adai
       p_saldo                         DECIMAL(22,2),
       p_es_consulta                   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                          SMALLINT      ,
       v_ejecucion_ws                  SMALLINT      ,
       v_tanto_adicional               DECIMAL(12,2),
       v_referencia_banc               CHAR(12),
       v_tiene_credito_vigente         SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess                   SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_f_inicio_pension              LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_diferencia_anos               SMALLINT, -- diferencia en anos de dos fechas
       v_id_datamart                   LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
	   v_fecha_ultima_relacion_laboral DATE, -- ultima fecha de relacion laboral
       v_con_sin_rel_lab               SMALLINT  -- Indica si tiene o no relacion laboral
       

    -- se verifica que el trabajador tenga una resolucion de pension valida
--    CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

--    IF ( v_tiene_spess ) THEN
--       SELECT f_inicio_pension
--        INTO   v_f_inicio_pension
--        FROM   ret_datamart
--        WHERE  id_datamart = v_id_datamart

        -- 19feb2014. Se elimina la validacion de prescripcion
        -- el saldo es retirable
        LET p_saldo = p_saldo * 2
        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo)
--    ELSE
--        CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_resolucion_spess, p_saldo)
        -- para verificar la relacion labora, se invoca el web service de consulta
        --CALL fn_fecha_ultima_relacion_laboral(p_nss)
        --RETURNING v_ejecucion_ws, v_fecha_ultima_relacion_laboral, v_con_sin_rel_lab
--
        --CALL ERRORLOG("Ejecucion WS: ")
        --CALL ERRORLOG(v_ejecucion_ws)		   
        --CALL ERRORLOG("Fec. de ultima relacion laboral:")
        --CALL ERRORLOG(v_fecha_ultima_relacion_laboral)
--
        -- si se ejecuto correctamente el WS
        --IF ( v_ejecucion_ws = 0 ) THEN
            -- si la fecha de defuncion es mayor o igual a la de la relacion laboral
            --IF ( v_fecha_ultima_relacion_laboral IS NOT NULL  AND v_fecha_inicio_pension >= v_fecha_ultima_relacion_laboral ) THEN
                -- el saldo es retirable
                --LET p_saldo = p_saldo * 2
                --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo)
            --ELSE
                --IF v_con_sin_rel_lab = 0 THEN 
                    -- sin resolucion en spess, se rechaza
                    --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_f_pension_mayor_f_ult_rel_laboral, p_saldo)
                --ELSE 
                    --IF v_con_sin_rel_lab = 1 THEN -- con relacion laboral actualmente
                        --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo)
                    --ELSE 
                        --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_rel_laboral, p_saldo)
                    --END IF 
                --END IF 
            --END IF
        --ELSE
            -- se rechaza porque se detecto un error al consultar el WS de ultima relacion laboral
            -- por lo tanto no es posible validar la solicitud
            --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_ws_rel_laboral_no_disponible, p_saldo)
        --END IF
--    END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la validacion de disponibilidad del retiro 
de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_fondo_ahorro(p_estado_solicitud, p_cod_rechazo, p_importe_viv72)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv72    DECIMAL(12,2), -- Importe de vivienda 72
         v_importe_dap      DECIMAL(12,2), -- importe para validacion de pago por dap o clabe 1000 o 2000
         v_devolver_saldo   SMALLINT -- booleana que indica si el saldo se debe devolver
         
    -- se verifica si se debe devolver el saldo de esta subcuenta
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(2, 40)
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].pago_dap         = 2
    LET v_importe_dap = 2000

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
        FROM   ret_rechazo
        WHERE  cod_rechazo = p_cod_rechazo;
        IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
        END IF
    END IF 

    -- se construye la respuesta del ws
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].modalidad_retiro = 2 -- fondo de ahorro
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].subcuenta        = 40 -- subcuenta de fondo de ahorr
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].f_valuacion      = TODAY USING "yyyymmdd"
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_avis       = 0
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_pesos      = p_importe_viv72

    IF (ws_ret_cons_saldos_disponibles_in.causal_ret_fa = 1) THEN -- Si tanto adicional
        LET v_importe_dap = 1000  -- el monto debe ser menor o igual a 1000 ya que no incluye el tanto adicional
    END IF 
    IF p_importe_viv72 <= v_importe_dap  AND p_importe_viv72 > 0 THEN 
        IF ws_ret_cons_saldos_disponibles_in.grupo_ley73 IS NULL OR ws_ret_cons_saldos_disponibles_in.grupo_ley73 = 0 THEN 
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].pago_dap     = 1
        END IF 
    END IF 


    -- se incrementa el indice del retiro consultado
    LET g_indice_retiro = g_indice_retiro + 1

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
    SELECT precio_fondo
    INTO   v_valor_fondo
    FROM   glo_valor_fondo
    WHERE  f_valuacion = p_fecha_valuacion
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
        FROM   ret_rechazo
        WHERE  cod_rechazo = p_cod_rechazo;
        IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
        END IF
    END IF 

    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].pago_dap      = 2
         
    -- si no se encuentra el precio del fondo, no se puede valuar
    IF ( v_valor_fondo IS NULL ) THEN
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].modalidad_retiro = 3 -- ley 73
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

            IF ( p_importe_aivs = 0 ) THEN

                LET p_estado_solicitud = gi_solicitud_rechazada
                LET p_cod_rechazo      = gi_sin_saldo
                LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
                IF p_cod_rechazo <> 0 THEN
                    -- Busca la descripcion del error para regresarla en la consulta
                    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = "";
                    SELECT des_larga
                    INTO   ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo
                    FROM   ret_rechazo
                    WHERE  cod_rechazo = p_cod_rechazo;
                    IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
                        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
                    END IF
                END IF 
            END IF 

        END IF 
      
        -- MARZ0 2015 ICHP

        -- se genera el registro de disponibilidad      
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].modalidad_retiro = 3 -- ley 73
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
Nombre: fn_respuesta_ws_amort_excedente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado de un retiro de amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_amort_excedente(p_nss, p_estado_solicitud, p_cod_rechazo, p_fecha_valuacion, p_aivs, p_pesos)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_fecha_valuacion  DATE, -- fecha de valuacion
         p_aivs             DECIMAL(24,6),
         p_pesos            DECIMAL(22,2),
         v_devolver_saldo   SMALLINT -- booleana que indica si se deve devolver el saldo

    -- se verifica si se debe devolver el saldo de esta subcuenta
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(9, 46)
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].pago_dap      = 2

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
        LET p_estado_solicitud = gi_solicitud_rechazada
        LET p_cod_rechazo      = gi_no_disponible_para_retiro -- nod
    END IF
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
    IF p_cod_rechazo <> 0 THEN
        -- Busca la descripcion del error para regresarla en la consulta
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = "";
        SELECT des_larga
        INTO   ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo
        FROM   ret_rechazo
        WHERE  cod_rechazo = p_cod_rechazo;
        IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
        END IF
    END IF 
    -- se construye la respuesta del servicio
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].modalidad_retiro = 9 -- amort excedentes
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].subcuenta        = 46
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].f_valuacion      = p_fecha_valuacion USING "YYYYMMDD"
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_avis       = p_aivs
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_pesos      = p_pesos

    -- se incrementa el indice del retiro consultado
    LET g_indice_retiro = g_indice_retiro + 1

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_aport_voluntarias
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado de un retiro de aportaciones voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_aport_voluntarias(p_nss, p_estado_solicitud, p_cod_rechazo, p_fecha_valuacion, p_aivs, p_pesos)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_fecha_valuacion  DATE, -- fecha de valuacion
         p_aivs             DECIMAL(24,6),
         p_pesos            DECIMAL(22,2),
         v_devolver_saldo   SMALLINT -- booleana que indica si se deve devolver el saldo

    -- se verifica si se debe devolver el saldo de esta subcuenta
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(10, 45)
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].pago_dap      = 2

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
        LET p_estado_solicitud = gi_solicitud_rechazada
        LET p_cod_rechazo      = gi_no_disponible_para_retiro -- nod
    END IF
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
    IF p_cod_rechazo <> 0 THEN
        -- Busca la descripcion del error para regresarla en la consulta
        LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = "";
        SELECT des_larga
        INTO   ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo
        FROM   ret_rechazo
        WHERE  cod_rechazo = p_cod_rechazo;
        IF ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].des_rechazo = " "; 
        END IF
    END IF 


    -- se construye la respuesta del servicio
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].modalidad_retiro = 10 -- aportaciones voluntarias
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].subcuenta        = 45 -- subcuenta de aport. voluntarias
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].f_valuacion      = p_fecha_valuacion USING "YYYYMMDD"
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_avis       = p_aivs
    LET ws_ret_cons_saldos_disponibles_out.saldo_x_retiro[g_indice_retiro].monto_pesos      = p_pesos

    -- se incrementa el indice del retiro consultado
    LET g_indice_retiro = g_indice_retiro + 1

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
