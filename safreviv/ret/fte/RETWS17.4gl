--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS17                                                 #
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
         nss              STRING, -- nss del trabajador
         medioEntrega     STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 2 - Devolución Automática
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_saldos_disponibles_out  RECORD
         nss               STRING, --- Número de seguridad social del trabajador
         estadoSolicitud   STRING, -- estado de la solicitud
         codRechazo        STRING, -- codigo de rechazo
         desRechazo        STRING,    -----  *****************************************
         fechaValuacion    STRING, -- fecha de valuacion de AIVs en formato AAAAMMDD
         saldoAvis         STRING, -- saldo en AIVs
         saldoPesos        STRING
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
    SELECT ruta_listados
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = "/safreviv_log/ret/RETWS17."
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
        LET v_webservice = com.WebService.CreateWebService("retiroDisponibilidadAmortExced", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_disponibilidad_amort_exced","fn_disponibilidad_amort_exced",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_disponibilidad_amort_exced")

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
Nombre: fn_disponibilidad_amort_exced
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros de Amortizaciones Excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_disponibilidad_amort_exced()
DEFINE v_indice_retiro   SMALLINT,
       v_nss             LIKE ret_solicitud_generico.nss,
	    v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	    v_ruta_log        STRING,
	    v_cadena          STRING

   -- se responde el servicio para pruebas
   LET ws_ret_cons_saldos_disponibles_out.nss = ws_ret_cons_saldos_disponibles_in.nss
   LET v_nss = ws_ret_cons_saldos_disponibles_in.nss

   DISPLAY "Validando saldos para:"
   DISPLAY "NSS: ", v_nss

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = "/safreviv_log/ret/RETWS17."
   LET v_cadena   = v_nss
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)

   -- se inicia el indice del retiro que se va a consultar
   LET g_indice_retiro = 1

   IF ws_ret_cons_saldos_disponibles_in.medioEntrega <> "0" AND 
      ws_ret_cons_saldos_disponibles_in.medioEntrega IS NOT NULL THEN  
      -- se verifica si hay retiro por amortizaciones excedentes
      DISPLAY "Validando Amortizaciones excedentes"
      DISPLAY CURRENT YEAR TO SECOND, " NSS :", v_nss, " Medio Entrega :",ws_ret_cons_saldos_disponibles_in.medioEntrega 
      CALL fn_ret_disponibilidad_amort_excedentes(v_nss, TRUE)
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
FUNCTION fn_ret_disponibilidad_amort_excedentes(p_nss, p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
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
    DISPLAY CURRENT YEAR TO SECOND, " Id derechohabiente :", v_id_derechohabiente
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
        DISPLAY CURRENT YEAR TO SECOND, " El valor regresado por la funcion de consulta de la convivencia marcas ,", v_rch_cod

        -- si la referencia existe, entonces el derechohabiente esta marcado y no procede su retiro
        IF ( v_rch_cod = 810 ) THEN
            -- el NSS ya esta marcado
            CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_solicitud_en_tramite, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
            DISPLAY CURRENT YEAR TO SECOND, " Solicitud en Trámite :", v_rch_cod
        ELSE        
            IF v_rch_cod <> 0 THEN 
                CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_error_marca_no_convive, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
                DISPLAY CURRENT YEAR TO SECOND, " NSS en otro proceso :", v_rch_cod
            ELSE 
                -- si el NSS tiene saldo
                IF ( v_saldo_aivs IS NOT NULL AND v_saldo_aivs > 0 ) THEN

                    -- se escribe la respuesta
                    CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_aceptada, 0, v_fecha_saldo, v_saldo_aivs, v_saldo_pesos)
                    DISPLAY CURRENT YEAR TO SECOND, " NSS con saldo para devolver, aivs :", v_saldo_aivs, " pesos :", v_saldo_pesos

                ELSE
                    -- no tiene saldo
                    CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_sin_saldo, TODAY, 0, 0)
                    DISPLAY CURRENT YEAR TO SECOND, " NSS sin saldo que devolver :", gi_sin_saldo
                END IF
            END IF  
        END IF
    ELSE
        -- el nss no existe
        CALL fn_respuesta_ws_amort_excedente(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, TODAY, 0, 0)      
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
         v_devolver_saldo   SMALLINT,
         v_desc_rechazo     CHAR(100) -- booleana que indica si se deve devolver el saldo

    -- se verifica si se debe devolver el saldo de esta subcuenta
    LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(9, 46)

    -- si no se debe devolver, entonces se cambia el resultado por no disponible
    IF ( NOT v_devolver_saldo ) THEN
        LET p_estado_solicitud = gi_solicitud_rechazada
        LET p_cod_rechazo      = gi_no_disponible_para_retiro -- nod
    END IF
    LET ws_ret_cons_saldos_disponibles_out.desRechazo = " "; 
    IF p_cod_rechazo <> 0 THEN
        -- Busca la descripcion del error para regresarla en la consulta
        LET ws_ret_cons_saldos_disponibles_out.desRechazo = "";
        SELECT des_larga
        INTO   v_desc_rechazo
        FROM   ret_rechazo
        WHERE  cod_rechazo = p_cod_rechazo;
        IF v_desc_rechazo IS NULL THEN
            LET ws_ret_cons_saldos_disponibles_out.desRechazo = " ";
        ELSE
           LET ws_ret_cons_saldos_disponibles_out.desRechazo = v_desc_rechazo 
        END IF
    END IF 
    -- se construye la respuesta del servicio
    LET ws_ret_cons_saldos_disponibles_out.estadoSolicitud = p_estado_solicitud
    LET ws_ret_cons_saldos_disponibles_out.codRechazo      = p_cod_rechazo
    LET ws_ret_cons_saldos_disponibles_out.fechaValuacion  = p_fecha_valuacion USING "YYYYMMDD"
    LET ws_ret_cons_saldos_disponibles_out.saldoAvis       = p_aivs
    LET ws_ret_cons_saldos_disponibles_out.saldoPesos      = p_pesos

    -- se incrementa el indice del retiro consultado
    LET g_indice_retiro = g_indice_retiro + 1

END FUNCTION

