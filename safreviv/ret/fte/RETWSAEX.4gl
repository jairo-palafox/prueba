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
PRIVATE DEFINE v_ruta_pdf    STRING
PRIVATE DEFINE v_archivo_pdf STRING 
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
-- =======================================================
-- Record de recepción de valores en las variables
DEFINE ret_marcaje RECORD 
         nss            STRING,
         casoCRM        STRING, -- caso CRM
         cuentaCLABE    STRING,   -------     *************************
         indicadorMarca STRING, -- indicador de marca/desmarca/aprobacion/rechazo
         codigoRechazo  STRING, -- codigo de rechazo en caso de desmarca y rechazo
         medioEntrega   STRING
       END RECORD
-- =======================================================
-- Record de envío de variables de respuesta
DEFINE ret_respuesta RECORD
         nss            STRING,
         casoCRM        STRING, -- caso CRM
         conRetiro      STRING,
         estatusMarca   STRING,
         codRechazo     STRING,
         desRechazo     STRING ------ *********************************
       END RECORD

-- registro de entrada para la consulta
DEFINE ws_ret_generico_solicitud_in RECORD
         nss              STRING, -- nss del trabajador
         casoCRM          STRING, -- numero de caso ADAI
         medioEntrega     STRING, -- Sello generado por la consulta biometrica (aplica para medio_entrega = 2  Devolucion automatica)
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              tipoBeneficiario   STRING,
              clabeBancaria      STRING,
              rfc                STRING,
              email              STRING,
              telefono           STRING,
              telMovil           STRING,
              nombre             STRING,
              apPaterno          STRING,
              apMaterno          STRING,
              entidadFederativa  STRING
         END RECORD
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
         nss                 STRING, --- Número de seguridad social del trabajador
         casoCRM             STRING,
         sello               STRING,
         archivoPdf          BYTE, 
         arr_modalidad_retiro DYNAMIC ARRAY OF RECORD
           estadoSolicitud     STRING, -- estado de la solicitud
           codRechazo          STRING, -- codigo de rechazo
           desRechazo          STRING,   ---- *************************************++
           saldoAvis           STRING, -- saldo en AIVs
           saldoPesos          STRING
         END RECORD
       END RECORD
         
DEFINE g_indice_retiro  SMALLINT -- indice del tipo de retiro consultado
DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws
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

    DISPLAY "Log del servicio: " || FGL_GETENV("RETWS17LOG")
    
    -- se inicia el log del programa
    IF FGL_GETENV("RETWS17LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS17LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS17LOG")
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
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroAmortExced", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_disponibilidad_amort_exced","fn_disponibilidad_amort_exced",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        CALL v_webservice.publishOperation(op, "fn_disponibilidad_amort_exced")

        LET op = com.WebOperation.CreateDOCStyle("fn_marcaje_ae","fn_marcaje_ae",ret_marcaje,ret_respuesta)
        CALL v_webservice.publishOperation(op, "fn_marcaje_ae")

        LET op = com.WebOperation.CreateDOCStyle("fn_ret_sol_dev_automatica_ae","fn_ret_sol_dev_automatica_ae",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
        CALL v_webservice.publishOperation(op, "fn_ret_sol_dev_automatica_ae")

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

    -- se inicia el log del programa
    IF FGL_GETENV("RETWS17LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS17LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS17LOG")
    ELSE
       DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
       CALL STARTLOG(v_ruta_log)
    END IF 

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
Ivan Vega     Noviembre 9, 2020     - La consulta de montos disponibles se realiza directamente
                                      en la tabla cta_movimiento en lugar de hacerlo en la
                                      instancia de saldos
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

-- ========================== PRUEBAS LOCALES
        LET v_sql = "\n SELECT sum(monto_acciones),",
        "\n        sum(monto_pesos)",
        "\n FROM   cta_movimiento",
        "\n WHERE  id_derechohabiente = ?",
        "\n AND    subcuenta = 46"
        

        PREPARE sid_amortexced FROM v_sql
        EXECUTE sid_amortexced USING v_id_derechohabiente
        INTO v_saldo_aivs, v_saldo_pesos

        LET v_fecha_saldo = TODAY
        
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

{
======================================================================
Clave: 
Nombre: fn_marcaje_ae
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Solicita que se marque la cuenta por tipo de retiro


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_marcaje_ae()
DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, --Identificador del derechohabiente
       v_longitud           SMALLINT, --Longitud del arreglo de llegada
       v_indice             SMALLINT, --Indice del arreglo
       v_error_en_datos     SMALLINT -- booleana que indica que hubo error en datos

   -- se verifica si se esta solicitando un eco
   IF ( UPSHIFT(ret_marcaje.nss CLIPPED) = "ECO" ) THEN
      LET ret_respuesta.nss = "ECO"
      -- se devuelve ECO indicando que el servicio esta respondiendo correctamente
      CALL fn_respuesta_wsmarcaje(1, 1, 0, 0, "ECO", 0, 0, 0)
   ELSE
      -- se procesa la solicitud
      -- Asgina valores de entrada
      LET ret_respuesta.nss = ret_marcaje.nss

      -- se crea el registro de la peticion de solicitud de ws de marca
      CALL fn_registra_peticion_marca(0, ret_marcaje.nss, ret_marcaje.casoCRM, ret_marcaje.cuentaCLABE, 0, 0)
           RETURNING g_id_peticion
      
      -- se verifica que se hayan recibido los datos
      IF ( ret_marcaje.nss IS NULL AND
           ret_marcaje.cuentaCLABE IS NULL AND 
           (ret_marcaje.medioEntrega IS NULL OR ret_marcaje.medioEntrega = "0")) THEN
         -- es un error
         CALL fn_respuesta_wsmarcaje(1, 2, 0, gi_datos_incompletos, ret_marcaje.casoCRM, 0, 0, 0)
         RETURN
      ELSE
         -- se verifica que el arreglo para marcar haya llegado correcto
         LET v_error_en_datos = FALSE
      
         CALL fn_registra_detalle_peticion_marca(g_id_peticion,
                                                 ret_marcaje.indicadorMarca,
                                                 ret_marcaje.codigoRechazo,
                                                 0, 0, 0, 0)
      
         -- Valida modalidad de retiro
         IF ( ret_marcaje.indicadorMarca IS NULL ) THEN 
            -- se marca que hubo error en la validacion de datos
            LET v_error_en_datos = TRUE
         END IF
         IF (ret_marcaje.medioEntrega = "1" OR
             ret_marcaje.medioEntrega = "2") AND 
            ret_marcaje.indicadorMarca = "1" THEN  
            IF ( NOT fn_verifica_estructura_clabe(ret_marcaje.cuentaCLABE) )  THEN
               LET v_error_en_datos = TRUE
               CALL fn_respuesta_wsmarcaje(1, 2, 0, gi_esctructura_cta_clabe_incorrecta, ret_marcaje.casoCRM, 0, 0, 0)
               RETURN
            END IF
         END IF 
         IF ret_marcaje.medioEntrega = "0" OR
            ret_marcaje.medioEntrega IS NULL THEN 
            LET v_error_en_datos = TRUE
         END IF 
         -- si hubo error en datos, se finaliza la peticion
         IF ( v_error_en_datos ) THEN
            CALL fn_respuesta_wsmarcaje(1, 2, 0, gi_datos_incompletos, ret_marcaje.casoCRM, 0, 0, 0)
            RETURN
         END IF
      END IF
      
      -- Obtiene el identificador del derechohabiente
      LET v_id_derechohabiente = fn_obtiene_id_derechohabiente(ret_marcaje.nss)
      
      -- Obtiene la longitud del arreglo que llega
      
      CALL fn_aplica_marca_modalidad(v_indice,
                                     ret_marcaje.nss,                                
                                     ret_marcaje.casoCRM,
                                     v_id_derechohabiente, 
                                     ret_marcaje.indicadorMarca,
                                     ret_marcaje.medioEntrega)      
                                            
   END IF   
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_aplica_marca_modalidad
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Aplica el marcaje para las demás modalidades


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     25 oct 2013             - La senal de desmarca tambien debera desmarcar
                                        y cancelar una solicitud
======================================================================
}
FUNCTION fn_aplica_marca_modalidad(p_indice, p_nss, p_caso_adai, p_id_derechohabiente,p_marca, p_medio_entrega)
DEFINE p_indice                 SMALLINT,
       p_nss                    CHAR(11),
       p_caso_adai              CHAR(10),
       p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente,
       p_marca                  SMALLINT,
       p_grupo                  SMALLINT, 
       p_medio_entrega          SMALLINT,
       v_sql                    STRING, --Cadena de ejecución de los querys
       v_marca_entra            SMALLINT,
       v_n_referencia           INTEGER ,
       v_folio                  DECIMAL(9,0),
       v_estado_marca           SMALLINT,
       v_codigo_rechazo         SMALLINT,--Variable de rechazo del store
       v_marca_causa            SMALLINT,
       v_fecha_causa            DATE,
       v_usuario                CHAR(20),
       v_proceso_cod            SMALLINT,
       v_estatus_marca          SMALLINT,
       v_respuesta_marcaje      SMALLINT,
       v_id_solicitud           DECIMAL(9,0),
       v_res_actualizacion      SMALLINT ,
       v_existe_solicitud       SMALLINT,
       v_cod_inconsistencia     SMALLINT, --Variable para indicar la causa de la inconsistencia
       v_cod_rechazo            SMALLINT, -- Codigo de rechazo para la maraca en procesar
       v_diagnostico            SMALLINT,       --diagnostico de la consulta del saldo en la afore
       v_estatus                SMALLINT,        -- estatus de la cuenta individual segun la consulta del saldo en la Afore
       v_aivs_viv92             DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97             DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92            DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97            DECIMAL(22,2), -- saldo pesos de viv97
       v_saldo_total            DECIMAL(24,6), -- saldo total trabajo
       v_saldo_aivs_total       DECIMAL(24,6), -- Saldo total en aivs
       v_saldo_tesofe           DECIMAL(22,2), -- Saldo en pesos de la Subcueta de la Tesofe
       v_saldo_paso             DECIMAL(22,2), -- Variable de paso para trabajar los saldos
       v_precio_primero_mes     DECIMAL(10,6), -- Precio del primer día natural del mes
       v_resultado              SMALLINT,      -- resultado del llamado a la funcion
       v_marca_credito          SMALLINT,
       v_val_clabe              SMALLINT

DEFINE v_caso_crm               CHAR(10)

   -- si el id_derechohabiente viene nulo, no se encontro
   IF ( p_id_derechohabiente IS NULL ) THEN
      -- No se encontro el NSS en la base de datos
      LET v_estatus_marca = gi_estatus_marca_no_exitoso
      
      -- no existe en nss
      LET v_cod_inconsistencia = gi_nss_rfc_no_existe
      CALL fn_respuesta_wsmarcaje(p_indice,v_estatus_marca,NULL,v_cod_inconsistencia, p_caso_adai, 0, 0, 0)
      RETURN
   END IF

    LET v_aivs_viv92       = 0
    LET v_aivs_viv97       = 0
    LET v_pesos_viv92      = 0
    LET v_pesos_viv97      = 0
    LET v_saldo_tesofe     = 0
    LET v_saldo_paso       = 0
    LET v_saldo_total      = 0
    LET v_saldo_aivs_total = 0
    LET v_caso_crm         = ""


   
   --Retiro ley 73
   LET v_marca_entra = 810
   LET v_proceso_cod = g_proceso_cod_ret_amort_excedentes
        	
   --Asigna variables para la ejecución del store
   LET v_folio          = 0
   LET v_estado_marca   = "0"
   LET v_codigo_rechazo = 0
   LET v_marca_causa    = "0"
   LET v_fecha_causa    = NULL
   LET v_usuario        = "safreviv"
  
  
   --Valida la acción de marcaje que se deberá hacer sobre el registro
   CASE p_marca
      WHEN 1
         --Solicitud de marcaje
         --Consulta si existe  otra solicitud para el derechohabiente 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente,9,1)
               RETURNING v_existe_solicitud, v_n_referencia

         -- Valida que no exista otra solicitud 
         IF ( NOT v_existe_solicitud ) THEN
            -- Crea Caso en CRM 
            CALL fn_crea_caso(p_nss, p_medio_entrega) RETURNING v_resultado, v_caso_crm
            IF v_resultado <> 0 THEN --- No se pudo crear el caso
                                 -- se indica que no se pudo marcar
--               IF v_resultado = 1 THEN 
--                  LET v_estatus_marca = gi_estatus_marca_no_exitoso
--                  LET v_cod_inconsistencia = gi_solicitud_en_tramite
--               ELSE 
--                  LET v_estatus_marca = gi_estatus_marca_no_exitoso
--                  LET v_cod_inconsistencia = gi_ws_busca_caso_crm_no_disponible
--               END IF 
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
               LET v_cod_inconsistencia = v_resultado
            ELSE 
               --Asigna estatus de la marca
               LET v_estatus_marca = 8

               --Se obtiene el número de solicitud
               SELECT seq_ret_solicitud.NEXTVAL
               INTO   v_n_referencia
               FROM   systables 
               WHERE  tabid = 1

               IF (ret_marcaje.medioEntrega = "1" OR
                   ret_marcaje.medioEntrega = "2") THEN 
                  LET ret_marcaje.casoCRM = v_caso_crm
                  LET p_caso_adai = v_caso_crm
               END IF 
                                       
               -- Actualiza tabla de retiro genérico   
               CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,9,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,1)
               RETURNING v_res_actualizacion

               -- Verifica que no haya existido un error en la actualización de la tabla
               IF ( v_res_actualizacion ) THEN
                  --- Deja huella del medio de entrega
                  CALL fn_medio_entrega(v_n_referencia, 0, p_medio_entrega);
                  ---Se ejecuta la función de marcaje para la nueva solicitud
                  CALL fn_ret_generico_marca_cuenta(p_id_derechohabiente,
                                                    v_marca_entra       ,
                                                    v_n_referencia      ,
                                                    v_folio             ,
                                                    v_estado_marca      ,
                                                    v_codigo_rechazo    ,
                                                    v_marca_causa       ,
                                                    v_fecha_causa       ,
                                                    v_usuario           ,
                                                    v_proceso_cod )
                        RETURNING v_respuesta_marcaje
                                          
                  -- Valida que la respuesta sea correcta
                  IF ( v_respuesta_marcaje = 0 ) THEN
      	             -- Se ejecutó correctamente el store, se indica que se marcó la cuenta
      	             LET v_estatus_marca = gi_estatus_marca_existoso
      	             
      	             --Se indica código de rechazo 0, para mostrar que la ejecución fué correcta
      	             LET v_cod_inconsistencia = 0
                     --Se indica código de rechazo 0, para mostrar que la ejecución fué correcta
                  ELSE
                     -- Sucedió un error al ejecutar la función de marcado
                     -- Se elimina el registro creado
                     CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, 9, v_n_referencia,
                                                  v_folio, v_estatus_marca, v_codigo_rechazo, 3)
                        RETURNING v_res_actualizacion

                     -- se indica que no se pudo marcar
                     LET v_estatus_marca = gi_estatus_marca_no_exitoso

                     -- se verifica si es por inconvivencia
                     IF ( v_respuesta_marcaje > 0 ) THEN
                        LET v_cod_inconsistencia = gi_error_marca_no_convive
                     ELSE
                        -- Error al marcar/desmarcar la cuenta
                        LET v_cod_inconsistencia = gi_error_interno_marca
                     END IF

                  END IF   	             
               ELSE
                  -- Existió un error en la integración de retiro genérico
                  LET v_estatus_marca = gi_estatus_marca_no_exitoso

                  -- error al generar la solicitud en ret_solicitud_generico
                  LET v_cod_inconsistencia = gi_error_marca_generar_solicitud

               END IF
   	      END IF         	            
         ELSE
            --Se solicitó marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
            LET v_estatus_marca = 0

            -- Se envía código de rechazo indicando que existe otra solicitud en marcha
            LET v_cod_inconsistencia = gi_solicitud_en_tramite

            -- se obtiene el caso adai original
            SELECT caso_adai
            INTO   p_caso_adai
            FROM   ret_solicitud_generico
            WHERE  id_solicitud = v_n_referencia
         
         END IF      
      
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          DESMARCAR CUENTA
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 2
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente,9,2)
              RETURNING v_existe_solicitud, v_n_referencia
         --	Valida que no se haya solicitado una marcación antes de una desmarcación
         IF ( v_existe_solicitud ) THEN
            -- Asigna estatus de la marca
            LET v_estatus_marca = 100
            -- Actualiza tabla de retiro genérico   
            CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,9,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,2)
                 RETURNING v_res_actualizacion
                
            IF ( v_res_actualizacion ) THEN 
               --Si la ejecución fué correcta se envía solicitud de desmarcado
               --Se ejecutó correctamente el store, se indica que se desmarco la cuenta
               LET v_estatus_marca = gi_estatus_marca_existoso
               --Actualiza codigo de rechazo
               LET v_cod_inconsistencia = 0     		                            
            ELSE
               --Existió un error en la integración de retiro genérico
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
               --Se indica código de rechazo para mostrar que ocurrió un error al integrar información en 
               --ret_solicitud_generico
               LET v_cod_inconsistencia = gi_error_interno_marca
            END IF
         ELSE
            -- se indica que no se pudo desmarcar
            LET v_estatus_marca = gi_estatus_marca_no_exitoso
            --Se envía código de rechazo indicando que no existe solicitud de marcado previa para poder desmarcar
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF  
                   
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          APROBAR SOLICITUD
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 3
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente, 9, 3)
              RETURNING v_existe_solicitud, v_n_referencia
         
         --	Valida que no se haya solicitado una marcación antes de una desmarcación
        IF ( v_existe_solicitud ) THEN
            --- Valida la CLABE Bancaria contra los beneficiarios
            --CALL fn_valida_clabe_bancaria(p_id_derechohabiente, v_n_referencia, 9) RETURNING v_val_clabe
            LET v_val_clabe = 0    ---- Se solicito quitar la validación via correo para la integración con CRM
            IF v_val_clabe = 0 THEN 
                -- aprobacion de solicitud
                LET v_estatus_marca = 15
                -- Recupera la referencia DAP si existe
                  -- Actualiza tabla de retiro genérico   
                CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, 9, v_n_referencia, 
                                                  v_folio, v_estatus_marca, 0, 2)
                     RETURNING v_res_actualizacion
                     
                  -- se indica que la aprobacion fue exitosa
                LET v_estatus_marca = gi_estatus_marca_existoso
             
                -- sin error
                LET v_cod_inconsistencia = 0

            ELSE 
                LET v_estatus_marca = gi_estatus_marca_no_exitoso
             
                -- Problemas con la CLABE BANCARIA
                LET v_cod_inconsistencia = gi_esctructura_cta_clabe_incorrecta
            END IF
         ELSE
         	  -- no se pudo aprobar
            LET v_estatus_marca = gi_estatus_marca_no_exitoso
         
            -- no existe una solicitud para aprobar
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF

      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          RECHAZAR SOLICITUD
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 4
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente, 9, 4)
              RETURNING v_existe_solicitud, v_n_referencia
         
         --	Valida que no se haya solicitado una marcación antes de una desmarcación
         IF ( v_existe_solicitud ) THEN
            -- rechazo de solicitud
            LET v_estatus_marca = 100
         
         	  -- Actualiza tabla de retiro genérico   
            CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, 9, v_n_referencia, 
                                              v_folio, v_estatus_marca,
                                              ret_marcaje.codigoRechazo, 2)
                 RETURNING v_res_actualizacion
                 
         	  -- se rechazo correctamente la solicitud
            LET v_estatus_marca = gi_estatus_marca_existoso
         
            -- sin error
            LET v_cod_inconsistencia = 0
         ELSE
         	  -- no se pudo aprobar la solicitud
            LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
         
            -- no existe una solicitud que rechazar
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF
      OTHERWISE
         --Se recibió un indicador de marcaje desconocido
         CALL ERRORLOG("Se recibió un indicador de marcado desconocido")
         -- marcaje no exitoso
         LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
         
         -- indicador de marca no reconocido
         LET v_cod_inconsistencia = gi_indicador_marca_invalido
   END CASE
   
   --Invoca función de respuesta del WS
   CALL fn_respuesta_wsmarcaje(p_indice,v_estatus_marca,v_n_referencia,v_cod_inconsistencia, p_caso_adai, v_saldo_aivs_total, v_saldo_total, 0)
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_obtiene_id_derechohabiente
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Obtiene el id_derechohabiente de la tabla de afi_derechohabiente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtiene_id_derechohabiente(p_nss)
DEFINE p_nss                   CHAR(11),
       v_sql                   STRING,
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente
       
    --Se arma el query de la consulta
    LET v_sql = "\n SELECT id_derechohabiente",
              "\n FROM afi_derechohabiente ",
              "\n WHERE nss               = '", p_nss, "'",
              "\n AND   ind_estado_cuenta = 0 "   -- cuenta Activa

            
    PREPARE stm_derechohabiente FROM v_sql
    EXECUTE stm_derechohabiente INTO v_id_derechohabiente       

  --Envía el id_derechohabiente consultado
  RETURN v_id_derechohabiente 

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_actualiza_retiro_generico
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico


Registro de modificaciones:
Autor         Fecha       Descrip. cambio
Ivan Vega     26 Nov 2013  - La solicitud se crea con grupo de ventanilla
                             infonavit
Eneas Armas   20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
              20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
======================================================================
}
FUNCTION fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_modalidad_retiro, p_id_solicitud,
                                      p_folio, p_estado_solicitud, p_cod_rechazo, p_tipo_actualizacion)
DEFINE p_id_derechohabiente       DECIMAL(9,0),
       p_nss                      CHAR(11),
       p_rfc                      CHAR(13),
       p_modalidad_retiro         SMALLINT,
       p_id_solicitud             DECIMAL(9,0),
       p_folio                    DECIMAL(9,0),
       p_estado_solicitud         SMALLINT,
       p_cod_rechazo              SMALLINT,
       p_tipo_actualizacion       SMALLINT, --Variable que recibe el tipo de actualización(INSERT,UPDATE,DELETE)
       v_sql                      STRING,
       v_respuesta                SMALLINT,
       v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud
       v_cuenta_clabe             CHAR(18)
   
   -- Valida el tipo de actualización 1-Inserción, 2-Actualización
   CASE p_tipo_actualizacion
   	
      WHEN 1
   		   -- Arma el query INSERCIÓN
         LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
         LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
         LET v_r_ret_solicitud_generico.nss                    = p_nss
         LET v_r_ret_solicitud_generico.rfc                    = p_rfc
         LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
         LET v_r_ret_solicitud_generico.folio                  = p_folio
         LET v_r_ret_solicitud_generico.caso_adai              = ret_marcaje.casoCRM
         LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
         LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
         LET v_r_ret_solicitud_generico.folio_restitucion      = 0
         LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
         LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
--         LET v_r_ret_solicitud_generico.folio_afore            = NULL -- se usa en ventanilla afore
--         LET v_r_ret_solicitud_generico.grupo_ventanilla       = gi_ventanilla_infonavit -- solicitud iniciada en infonavit
         LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
         LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
         LET v_r_ret_solicitud_generico.estado_solicitud       = p_estado_solicitud
         LET v_r_ret_solicitud_generico.cod_rechazo            = p_cod_rechazo
   		   
         -- se inserta el registro
         INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )
                                            
         -- Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta = FALSE
            CALL ERRORLOG("Se produjo un error al insertar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha insertado con éxito el registro en retiro genérico")
            IF (ret_marcaje.medioEntrega = "1" OR ret_marcaje.medioEntrega = "2") THEN
               LET v_cuenta_clabe =  ret_marcaje.cuentaCLABE
               --- Se inserta en ret_pago_spei por cambio en la tabla ret_ws_peticion_marca
               INSERT INTO ret_pago_spei VALUES (p_id_solicitud,1,1,0,0,v_cuenta_clabe);
            END IF 
         END IF
                                            
                
      WHEN 2
      
         -- si no llego el codigo de rechazo, se define el generico
         IF ( p_cod_rechazo IS NULL ) THEN
            LET p_cod_rechazo = 54 -- rechazo generico. CAMBIAR POR CONSTANTE GLOBAL
         END IF
      
         -- Arma el query ACTUALIZACION
         LET v_sql = "\nUPDATE ret_solicitud_generico",
                     "\nSET    estado_solicitud   =  ", p_estado_solicitud, ",",
                     "\n       cod_rechazo        =  ", p_cod_rechazo,
                     "\nWHERE  id_derechohabiente =  ", p_id_derechohabiente,
                     "\nAND    modalidad_retiro   =  ", p_modalidad_retiro,
                     "\nAND    id_solicitud       =  ", p_id_solicitud
                      
         PREPARE stm_update_retiro FROM v_sql
         EXECUTE stm_update_retiro 
                                           
         -- Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
         	  -- Asigna respuesta negativa
         	  LET v_respuesta = FALSE
         	  CALL ERRORLOG("Se produjo un error al actualizar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha actualizado con éxito el registro en retiro genérico")
            -- se actualiza la tabla de historicos con el estado de solicitud y cogido de rechazo segun la modalidad
            UPDATE ret_amort_excedente
            SET    estado_solicitud = p_estado_solicitud,
                   cod_rechazo      = p_cod_rechazo
            WHERE  id_solicitud     = p_id_solicitud
            
            -- si se trata de un rechazo, se desmarca
            IF ( p_estado_solicitud = 100 ) THEN
               CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 810, 
                                                    p_id_solicitud, 810,
                                                    "safreviv", g_proceso_cod_ret_amort_excedentes)
            END IF
               
         END IF
      WHEN 3
      
      	 -- Arma el query ELIMINACIÓN
         LET v_sql = "\n DELETE FROM ret_solicitud_generico",
                     "\n WHERE  id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND    modalidad_retiro   = ", p_modalidad_retiro,
                     "\n AND    id_solicitud       = ", p_id_solicitud
                      
         PREPARE stm_delete_retiro FROM v_sql
         EXECUTE stm_delete_retiro 
               
         --Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta=FALSE
            CALL ERRORLOG("Se ha producido un error al eliminar el registro en retiro genérico")
         ELSE
         	
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha eliminado con éxito el registro en retiro genérico")
         END IF              
   END CASE
   
   -- Regresa respuesta
   RETURN v_respuesta
END FUNCTION                  
{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_generico
Fecha creacion: Septiembre 27, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Verifica que no exista una solicitud en la tabla de retiro genérico

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     26 Nov 2013             - La discriminacion del registro se hace
                                        por grupo de ventanilla infonavit
======================================================================
}
FUNCTION fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad_retiro,p_accion)
DEFINE v_sql STRING,
       p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_modalidad_retiro   SMALLINT,
       p_accion             SMALLINT,
       v_ban_existe         SMALLINT,
       v_num_solicitud      DECIMAL(9,0)
       
       
   -- Inicializa valores
   LET v_ban_existe= FALSE    
 
 
   -- Valida el tipo de acción
   CASE p_accion
   	
      WHEN 1 
   	 	   -- Marcar 		  
         LET v_sql = "\n SELECT id_solicitud         ",
                     "\n FROM ret_solicitud_generico ",
                     "\n WHERE id_derechohabiente =  ", p_id_derechohabiente,
                     "\n AND modalidad_retiro     =  ", p_modalidad_retiro,
--                     "\n AND grupo_ventanilla     =  ", gi_ventanilla_infonavit,
                     "\n AND estado_solicitud IN (   ",
                     "\n 8, 10, 15, 50, 60, 70, 71,  ", -- precaptura, captura, aprobacion, preliq., liquid., enviada fico, conf. pago
                     "\n 90, 91, 209, 210, 211, 212, ", -- rch fico, rechazo banco, cancelacion CxP, 
                     "\n 213 )" -- restitucion
   	
      WHEN 2
         -- Desmarcar
         LET v_sql = "\n SELECT id_solicitud ",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ", p_modalidad_retiro,
--                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit,
                     "\n AND estado_solicitud     IN (8,10) "
       
      WHEN 3
         -- Aprobar solicitud
         LET v_sql = "\n SELECT id_solicitud ",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro,
--                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit,
                     "\n AND estado_solicitud     = 10"
                     
      WHEN 4 
         -- Rechazar la solicitud
         LET v_sql = "\n SELECT id_solicitud",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro
--                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit
         IF ret_marcaje.medioEntrega = "1" THEN 
            LET v_sql = v_sql CLIPPED,  "\n AND estado_solicitud     IN (8, 15) "
         ELSE 
            LET v_sql = v_sql CLIPPED,  "\n AND estado_solicitud     = 10"
         END IF 
   
   
   END CASE
   
   DISPLAY v_sql
   PREPARE stm_existe_solicitud FROM v_sql
   DECLARE cur_existe_solicitud CURSOR FOR stm_existe_solicitud
   	
   -- Itera resultados
   FOREACH cur_existe_solicitud INTO v_num_solicitud
      --Asigna valor
      IF ( v_num_solicitud IS NOT NULL ) THEN
         LET v_ban_existe = TRUE 
      END IF
   END FOREACH
  
  DISPLAY "Solicitud encontrada: "
  DISPLAY "Bandera: ", v_ban_existe
  DISPLAY "Solicitud: ", v_num_solicitud
  
   -- se devuelve el resultado de la creacion
   RETURN v_ban_existe, v_num_solicitud
END FUNCTION                 
{
======================================================================
Clave: 
Nombre: fn_respuesta_wsmarcaje
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico
Asigna los valores de retorno del Web Service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_wsmarcaje(p_indice, p_estatus_marca, p_id_solicitud, p_cod_rechazo, p_caso_adai, p_saldo_aivs_total, p_saldo_total, p_ref_dap)
DEFINE p_indice           SMALLINT,
       p_estatus_marca    SMALLINT,
       p_id_solicitud     DECIMAL(9,0),
       p_cod_rechazo      SMALLINT,
       p_caso_adai        LIKE ret_solicitud_generico.caso_adai, -- caso adai
       p_saldo_aivs_total DECIMAL(24,6),
       p_saldo_total      DECIMAL(22,6),
       p_ref_dap          DECIMAL(9,0),
       v_desc_rechazo     CHAR(100)
       
   --Asigna valores al arreglo
   --LET ret_respuesta.caso_adai = p_caso_adai
   LET ret_respuesta.estatusMarca   = p_estatus_marca
   IF ret_marcaje.medioEntrega = "1" THEN 
      LET ret_respuesta.conRetiro  = p_caso_adai
   ELSE 
      LET ret_respuesta.conRetiro  = p_id_solicitud
   END IF 
   LET ret_respuesta.codRechazo = p_cod_rechazo
   LET ret_respuesta.desRechazo = " ";

   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ret_respuesta.desRechazo = "";
       SELECT des_larga
       INTO   v_desc_rechazo 
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF v_desc_rechazo IS NULL THEN
           LET ret_respuesta.desRechazo = " ";
       ELSE 
         LET ret_respuesta.desRechazo = v_desc_rechazo
       END IF
   END IF    
   
   -- se actualiza el historico de la peticion
   CALL fn_registra_peticion_marca(g_id_peticion, ret_marcaje.nss, ret_marcaje.casoCRM, ret_marcaje.cuentaCLABE, p_cod_rechazo, 1)
   RETURNING g_id_peticion
   
   -- se actualiza el detalle de la peticion
   CALL fn_registra_detalle_peticion_marca(g_id_peticion, 
                                           ret_marcaje.indicadorMarca,
                                           ret_marcaje.codigoRechazo,
                                           p_estatus_marca, p_id_solicitud, p_cod_rechazo, 1)
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_marca
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS de marca 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_marca(p_id_peticion, p_nss, p_caso_adai, p_cuenta_clabe, p_res_ejecucion, p_accion)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_caso_adai               LIKE ret_solicitud_generico.caso_adai,
       p_cuenta_clabe            CHAR(18),
       p_res_ejecucion           SMALLINT, -- resultado de la ejecucion
       p_accion                  SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_peticion_marca RECORD LIKE ret_ws_peticion_marca.* -- registro de peticion al ws
		
   -- si se trata de nuevo registro
   IF ( p_accion = 0 ) THEN
      -- se obtiene el id de peticion nuevo
      SELECT seq_ret_ws_generico.nextVal
      INTO   p_id_peticion
      FROM   systables
      WHERE  tabid = 1
      
      -- se asignan los datos
      LET v_r_ret_ws_peticion_marca.id_peticion   = p_id_peticion
      LET v_r_ret_ws_peticion_marca.f_peticion    = TODAY
      LET v_r_ret_ws_peticion_marca.h_peticion    = CURRENT HOUR TO SECOND
      LET v_r_ret_ws_peticion_marca.nss           = p_nss
      LET v_r_ret_ws_peticion_marca.caso_adai     = p_caso_adai
      LET v_r_ret_ws_peticion_marca.res_ejecucion = p_res_ejecucion
--      LET v_r_ret_ws_peticion_marca.cuenta_clabe  = p_cuenta_clabe
      LET v_r_ret_ws_peticion_marca.rfc = ''

      
      -- se inserta el registro de peticion
      INSERT INTO ret_ws_peticion_marca VALUES ( v_r_ret_ws_peticion_marca.* )
   ELSE
      -- se actualiza el registro de la peticion
      UPDATE ret_ws_peticion_marca
      SET    res_ejecucion = p_res_ejecucion
      WHERE  id_peticion   = p_id_peticion
   END IF

   -- se devuelve el id de la peticion
   RETURN p_id_peticion
END FUNCTION


{
======================================================================
Nombre: fn_registra_detalle_peticion_marca
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos que se recibieron para generar el detalle de marca
de una solicitud de marca por ws

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_detalle_peticion_marca(p_id_peticion, p_ind_marca, p_cod_rechazo,
                                            p_resp_estado_marca, p_resp_id_solicitud, p_resp_cod_rechazp, p_accion)
DEFINE p_id_peticion                 DECIMAL(9,0),
       p_modalidad_retiro            LIKE ret_modalidad_retiro.modalidad_retiro,
       p_ind_marca                   SMALLINT,
       p_cod_rechazo                 LIKE ret_rechazo.cod_rechazo,
       p_resp_estado_marca           SMALLINT, -- resultado de la marca
       p_resp_id_solicitud           LIKE ret_solicitud_generico.id_solicitud, 
       p_resp_cod_rechazp            LIKE ret_rechazo.cod_rechazo,
       p_accion                      SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_det_peticion_marca RECORD LIKE ret_ws_det_peticion_marca.* -- registro de detalle de peticion WS de marca
		
   -- si se trata de nuevo registro
   IF ( p_accion = 0 ) THEN
      -- se asignan los datos
      LET v_r_ret_ws_det_peticion_marca.id_peticion       = p_id_peticion
      LET v_r_ret_ws_det_peticion_marca.modalidad_retiro  = 9    --- Amortizaciones Excedentes
      LET v_r_ret_ws_det_peticion_marca.ind_marca         = p_ind_marca
      LET v_r_ret_ws_det_peticion_marca.cod_rechazo       = p_cod_rechazo
      LET v_r_ret_ws_det_peticion_marca.resp_estado_marca = p_resp_estado_marca
      LET v_r_ret_ws_det_peticion_marca.resp_con_retiro   = p_resp_id_solicitud
      LET v_r_ret_ws_det_peticion_marca.resp_cod_rechazo  = p_resp_cod_rechazp
      
      -- se inserta el registro de peticion
      INSERT INTO ret_ws_det_peticion_marca VALUES ( v_r_ret_ws_det_peticion_marca.* )
   ELSE
      -- se actualiza el registro de la peticion
      UPDATE ret_ws_det_peticion_marca
      SET    resp_estado_marca = p_resp_estado_marca,
             resp_con_retiro   = p_resp_id_solicitud,
             resp_cod_rechazo  = p_resp_cod_rechazp
      WHERE  id_peticion       = p_id_peticion
      AND    modalidad_retiro  = 9 --- Amortizaciones Excedentes
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_ley73_doble_marca
Fecha creacion: Junio 11, 2014
Autor: Ricardo Perez Ramirez, EFP
Narrativa del proceso que realiza:
Verifica que exista una solicitud en la tabla de retiro genérico para 
permitir la caputra del tipo de retiro ley 73 ya que este intenta marcar doble
una marca para cada subcuenta, si es asi no debe rechazar la segunda marca

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_solicitud_ley73_doble_marca(p_id_derechohabiente,p_modalidad_retiro)
DEFINE p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_modalidad_retiro   SMALLINT,
       v_ban_existe         SMALLINT,
       v_cant_sol           SMALLINT
       
   -- Inicializa valores
   LET v_ban_existe= FALSE    
 
   IF p_modalidad_retiro = 3 THEN
       SELECT COUNT(*) 
         INTO v_cant_sol
         FROM ret_solicitud_generico    		  
        WHERE id_derechohabiente = p_id_derechohabiente
          AND modalidad_retiro = 3
          AND estado_solicitud = 8;

       IF v_cant_sol <> 1 THEN
           LET v_ban_existe = TRUE;
       ELSE 
          DISPLAY "Solicitud encontrada para retiro ley 73 se permite continuar: "
          DISPLAY "Bandera: ", v_ban_existe
       END IF
   ELSE
       LET v_ban_existe = TRUE;
   END IF;
  
   -- se devuelve el resultado de la creacion
   RETURN v_ban_existe
END FUNCTION                 
{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_ley73_doble_marca
Fecha creacion: Junio 11, 2014
Autor: Ricardo Perez Ramirez, EFP
Narrativa del proceso que realiza:
Verifica que exista una solicitud en la tabla de retiro genérico para 
permitir la caputra del tipo de retiro ley 73 ya que este intenta marcar doble
una marca para cada subcuenta, si es asi no debe rechazar la segunda marca

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_clabe_bancaria(p_id_derechohabiente,p_id_solicitud, p_v_modalidad)
DEFINE p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_id_solicitud       LIKE  ret_solicitud_generico.id_solicitud,
       p_v_modalidad        SMALLINT,
       v_res_valida         SMALLINT,
       v_cant_sol           SMALLINT,
       v_cuenta_clabe       CHAR(18)
       
    -- Inicializa valores
    LET v_res_valida = 0    
    IF ret_marcaje.cuentaCLABE IS NULL AND p_v_modalidad <> 2 THEN 
        LET v_res_valida = 1
    ELSE

        SELECT b.c_siaf + a.c_spei 
        INTO   v_cant_sol
        FROM   (SELECT COUNT(*) AS c_spei
                FROM   (SELECT DISTINCT cuenta_clabe 
                        FROM   ret_pago_spei
                        WHERE  id_solicitud IN (SELECT id_solicitud 
                                                FROM   ret_solicitud_generico
                                                WHERE  id_solicitud       = p_id_solicitud
                                                AND    id_derechohabiente = p_id_derechohabiente))) a,
               (SELECT COUNT(*) AS c_siaf
                FROM   (SELECT DISTINCT cuenta_clabe
                        FROM   ret_pago_siaf
                        WHERE  id_solicitud in (SELECT id_solicitud 
                                                FROM   ret_solicitud_generico
                                                WHERE  id_solicitud       = p_id_solicitud
                                                AND    id_derechohabiente = p_id_derechohabiente))) b
        IF v_cant_sol > 1 THEN 
            LET v_res_valida = 1
        ELSE 
            IF v_cant_sol = 1 THEN 
                SELECT DISTINCT cuenta_clabe
                INTO   v_cuenta_clabe
                FROM   ret_pago_spei
                WHERE  id_solicitud IN (
                       SELECT id_solicitud 
                       FROM   ret_solicitud_generico
                       WHERE  id_solicitud = p_id_solicitud
                       AND    id_derechohabiente = p_id_derechohabiente)
                IF v_cuenta_clabe IS NULL THEN 
                    SELECT DISTINCT cuenta_clabe
                    INTO   v_cuenta_clabe
                    FROM   ret_pago_siaf
                    WHERE  id_solicitud IN (
                           SELECT id_solicitud 
                           FROM   ret_solicitud_generico
                           WHERE  id_solicitud = p_id_solicitud
                           AND    id_derechohabiente = p_id_derechohabiente)
                END IF 
                
                IF v_cuenta_clabe <> ret_marcaje.cuentaCLABE THEN 
                    LET v_res_valida = 1
                ELSE 
                    LET v_res_valida = 0
                END IF 
            ELSE
                IF p_v_modalidad = 2 THEN 
                    LET v_res_valida = 0
                ELSE 
                    LET v_res_valida = 1
                END IF 
            END IF 
        END IF 
    END IF 
  
   -- se devuelve el resultado de la creacion
   RETURN v_res_valida
END FUNCTION                  

FUNCTION fn_medio_entrega(pe_id_solicitud, pe_grupo, pe_medio_entrega)
DEFINE pe_id_solicitud     DECIMAL(10,0)
DEFINE pe_grupo            SMALLINT
DEFINE pe_medio_entrega    SMALLINT 

   IF (pe_id_solicitud  IS NOT NULL AND 
       pe_grupo         IS NOT NULL AND
       pe_medio_entrega IS NOT NULL ) THEN 
       INSERT INTO ret_sol_medio_entrega (id_solicitud, grupo, medio_entrega,f_registro)
            VALUES (pe_id_solicitud, pe_grupo, pe_medio_entrega, CURRENT YEAR TO MINUTE);
   END IF 

END FUNCTION 

FUNCTION fn_crea_caso(p_nss, p_medio_entrega)
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

   DEFINE v_indice        INTEGER 
   DEFINE v_con_caso      INTEGER 
   DEFINE v_caso_crm      CHAR(10)
   DEFINE v_caso_a_cerrar DECIMAL(10,0)
   DEFINE v_ecod          INTEGER 
   DEFINE v_nombre        LIKE afi_derechohabiente.nombre_af
   DEFINE v_paterno       LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_materno       LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_rfc           LIKE afi_derechohabiente.rfc
   DEFINE v_curp          LIKE afi_derechohabiente.curp


   --- Verifica si hay caso abierto, si lo hay lo cierra y crea uno nuevo,
   --- si no hay caso abierto, crea uno nuevo
   --- si hay un caso abierto y no se puede cerrar, se rechaza la solicitud
   --- si no puede consumir el servicio de Busqueda o Cancelacion se rechaza la solicitud
   --- si no puede crear un nuevo caso rechaza la solicitud
   
   LET v_regreso       = 0
   LET v_caso_crm      = ''
   LET v_caso_a_cerrar = 0
   LET v_indice        = 0
   LET v_con_caso      = 0
   LET v_ecod          = 0
   LET v_nombre        = ''
   LET v_paterno       = ''
   LET v_materno       = ''
   LET v_rfc           = ''
   LET v_curp          = ''
   --- Busca el nombre del Derechohabiente
   SELECT nombre_af, ap_paterno_af, ap_materno_af, rfc, curp
   INTO   v_nombre, v_paterno, v_materno, v_rfc, v_curp
   FROM   afi_derechohabiente
   WHERE  nss = p_nss;
   -- Se comenta esta parte para tableta, los llamados a CRM los ará la tableta
--   IF p_medio_entrega = 1 THEN ---- Validac si hay un caso abierto y se pueda cerrar en su caso
--      -- Llamado a la funcion de casos abiertos 
--      CALL arr_casos_crm.clear()
--      CALL fn_busca_caso_crm(p_nss) RETURNING v_regreso, arr_casos_crm 
--      DISPLAY "El resultado en el llamado a Busca Caso :", v_regreso
--      IF v_regreso = 0 THEN --- El servicio de Búsqueda contestó correctamente 
--         FOR v_indice = 1 TO arr_casos_crm.getLength()   --- Barre el arreglo para saber si hay casos abiertos
--            IF arr_casos_crm[v_indice].texto_status <> "Abierto" AND 
--               arr_casos_crm[v_indice].texto_status <> "Abierto con Cita" AND 
--               arr_casos_crm[v_indice].texto_status <> "Cerrado" THEN 
--               LET v_con_caso = v_con_caso + 1
--            ELSE 
--               IF arr_casos_crm[v_indice].texto_status = "Abierto" OR 
--                  arr_casos_crm[v_indice].texto_status = "Abierto con cita" THEN
--                  LET v_caso_a_cerrar = arr_casos_crm[v_indice].casos
--               END IF 
--            END IF 
--         END FOR
--         IF v_con_caso > 0 THEN   --- Valida si hay casos abiertso que no se puedan cerrar
--            LET v_regreso = gi_solicitud_en_tramite   -- Rechaza solicitud ya que hay casos abiertos que no se pueden cerrar
--         ELSE 
--            IF v_caso_a_cerrar IS NOT NULL AND v_caso_a_cerrar <> 0 THEN  -- existe caso que cancelar  
--               CALL fn_cancela_caso_crm(v_caso_a_cerrar) RETURNING v_regreso, v_ecod  -- cancela caso
--               DISPLAY "El resultado en el llamado a Cancela Caso :", v_regreso
--            END IF 
--            IF v_regreso = 0 AND v_ecod = 0 THEN  -- Cancelo exitosamente o no hubo nada que cancelar
--
--               --- Crea nuevo caso
--               CALL fn_crea_caso_crm(p_nss, v_nombre, v_paterno, v_materno, v_rfc, v_curp, "ZG1T","A1ZN000127ZN01")  RETURNING v_regreso, v_caso_crm
--               DISPLAY "El regreso en el llamado a Crea Caso :", v_regreso
--               IF v_regreso <> 0 THEN
--                  LET v_regreso = gi_ws_crea_caso_crm_no_disponible
--               END IF 
--            ELSE      --- Hubo un error en el llamado a la cancelación o no pudo cancelar
--               IF v_ecod <> 0 THEN 
--                  LET v_regreso = gi_solicitud_en_tramite --- No se pudo cancelar el caso, se devuelve solicitud en trámite
--               ELSE 
--                  LET v_regreso = gi_ws_cancela_caso_crm_no_disponible  --- Problemas en el llamado al servicio de cancelación
--               END IF 
--            END IF 
--         END IF
--      ELSE 
--         LET v_regreso = gi_ws_busca_caso_crm_no_disponible
--      END IF
--   ELSE
      IF p_medio_entrega = 2 THEN   --- Crea caso para la devolución automática
         --- Crea nuevo caso
         CALL fn_crea_caso_crm(p_nss, v_nombre, v_paterno, v_materno,v_rfc, v_curp, "ZPEF","A1ZN000139ZN01")  RETURNING v_regreso, v_caso_crm
         DISPLAY "El regreso en el llamado a Crea Caso E-Firma :", v_regreso
         IF v_regreso <> 0 THEN 
            LET v_regreso = gi_ws_crea_caso_crm_no_disponible
         END IF 
      ELSE 
         LET v_regreso  = 0
         LET v_caso_crm = ''
      END IF 
--   END IF 

RETURN v_regreso, v_caso_crm

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ret_sol_dev_automatica_ae
Fecha creacion: Noviembre 30, 2017
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_ret_sol_dev_automatica_ae()
DEFINE v_indice_retiro       SMALLINT,
       v_nss                 LIKE afi_fondo72.nss,
       v_rfc                 LIKE afi_fondo72.rfc,
       v_indice_modalidad    SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
	    v_requiere_DAP        SMALLINT, -- booleana que indica si se necesita DAP
       v_modalidad_procesada SMALLINT, -- Indica si ya se proceso una solicitud de ley 73
       v_id_derechohabiente  DECIMAL(10,0) -- Identificador único del trabajador

DEFINE v_arr_beneficiario  DYNAMIC ARRAY OF RECORD
              tipoBeneficiario   SMALLINT,
              clabeBancaria      CHAR(18),
              rfc                CHAR(13),
              email              CHAR(50),
              telefono           CHAR(10),
              telMovil           CHAR(10),
              nombre             CHAR(40),
              apPaterno          CHAR(40),
              apMaterno          CHAR(40),
              entidadFederativa  CHAR(2)
         END RECORD
       
   -- se verifica si se esta solicitando eco
   IF ( UPSHIFT(ws_ret_generico_solicitud_in.nss) = "ECO" ) THEN
      -- se devuelve ECO
      LET ws_ret_generico_solicitud_out.nss = "ECO"
      
      -- se indica que hay un registro
      LET g_indice_retiro = 1
      CALL fn_respuesta_ws("ECO", "ECO", 0, 0, 0, 0, 0, NULL)
   ELSE
      -- se asignan los valores de respuesta
      LET ws_ret_generico_solicitud_out.nss = ws_ret_generico_solicitud_in.nss
      
      LET v_nss = ws_ret_generico_solicitud_in.nss
      
      -- se inicia el indice del retiro que se va a consultar
      LET g_indice_retiro = 1
      
      -- se crea el registro de la peticion de solicitud de ws registro de solicitud
      CALL fn_registra_peticion_registro_solicitud(ws_ret_generico_solicitud_in.nss,  
                                                   ws_ret_generico_solicitud_in.casoCRM)
           RETURNING g_id_peticion
      
      -- se asume que todos traen cuenta CLABE
      LET v_cta_clabe_correcta = TRUE
	  
	  -- si se tiene mas de una modalidad de retiro, todas deben tener cuenta CLABE
      IF ws_ret_generico_solicitud_in.medioEntrega <> "2" THEN 
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength() 
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria IS NULL ) THEN
               -- se rechaza porque todos deben traer CLABE, incluyendo Fondo de Ahorro
               LET v_cta_clabe_correcta = FALSE
               EXIT FOR
            END IF
         END FOR
      END IF 
			
		 
      -- si no fue correcto, se rechaza la solicitud para todas las modalidades
      IF ( NOT v_cta_clabe_correcta ) THEN
         CALL fn_respuesta_ws(v_nss, NULL, 9,
                              gi_solicitud_rechazada, gi_modalidad_multiple_sin_CLABE, 0, 0, NULL)
      END IF
   END IF
	  
   -- si paso la validacion de existencia de cuenta CLABE en modalidad multiple
   IF ( v_cta_clabe_correcta = TRUE ) THEN
      -- se verifica que modalidades se solicitaron
      IF ((v_modalidad_procesada = 1)) THEN 
         -- registro procesado con otra subcuenta
         LET v_modalidad_procesada = 1
      ELSE   
         -- se registran los detalles de modalidad de retiro
         CALL fn_registra_det_peticion_registro_solicitud(g_id_peticion,
                                                           9,
                                                           NULL,
                                                           NULL             ,
                                                           NULL,
                                                           NULL )
         -- se asume que el registro de beneficiario es correcto
         LET v_existe_beneficiario = TRUE

         -- se asume que las cuentas clabe son correctas
         LET v_cta_clabe_correcta = TRUE
         
         IF ws_ret_generico_solicitud_in.medioEntrega = "2" THEN
            --- Obtenemos los datos del beneficiario para grupo 1 y medio entrega 1 (tableta)
            LET v_indice_beneficiario = 1 
            LET v_id_derechohabiente  = 0
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario = "1" --- Titular
            
            SELECT DISTINCT c.id_derechohabiente, a.cuenta_clabe,
                   NVL(d.rfc,"SIN RFC"), NVL(d.nombre_af,"SIN NOMBRE"),
                   NVL(d.ap_paterno_af,"SIN PATERNO"), NVL(d.ap_materno_af,"SIN MATERNO")
            INTO   v_id_derechohabiente,
                   v_arr_beneficiario[v_indice_beneficiario].clabeBancaria,
                   v_arr_beneficiario[v_indice_beneficiario].rfc,
                   v_arr_beneficiario[v_indice_beneficiario].nombre,
                   v_arr_beneficiario[v_indice_beneficiario].apPaterno,
                   v_arr_beneficiario[v_indice_beneficiario].apMaterno
            FROM   ret_pago_spei             a,
                   ret_solicitud_generico    c,
                   afi_derechohabiente       d
            WHERE  c.nss                 = v_nss
            AND    d.id_derechohabiente  = c.id_derechohabiente
            AND    c.modalidad_retiro    = 9
            AND    c.estado_solicitud    = 8
            AND    a.id_solicitud        = c.id_solicitud
            AND    a.consec_beneficiario = 1

            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria = v_arr_beneficiario[v_indice_beneficiario].clabeBancaria
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc           = v_arr_beneficiario[v_indice_beneficiario].rfc
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre        = v_arr_beneficiario[v_indice_beneficiario].nombre
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno     = v_arr_beneficiario[v_indice_beneficiario].apPaterno
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apMaterno     = v_arr_beneficiario[v_indice_beneficiario].apMaterno


            SELECT TRIM(NVL(telefono,"0000000000"))
            INTO   v_arr_beneficiario[v_indice_beneficiario].telefono
            FROM   afi_telefono
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    id_telefono        = 1
            
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = v_arr_beneficiario[v_indice_beneficiario].telefono

            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = "0000000000"
            END IF 

            SELECT NVL(valor,"SIN CORREO")
            INTO   v_arr_beneficiario[v_indice_beneficiario].email 
            FROM   afi_contacto_electronico
            WHERE  id_derechohabiente      = v_id_derechohabiente
            AND    id_contacto_electronico = 1

            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = v_arr_beneficiario[v_indice_beneficiario].email

            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = "SIN CORREO"
            END IF 

            SELECT NVL(b.entidad_federativa,"9")
            INTO   v_arr_beneficiario[v_indice_beneficiario].entidadFederativa
            FROM   afi_domicilio a,
                   cat_cp        b
            WHERE  a.id_derechohabiente = v_id_derechohabiente
            AND    a.id_domicilio       = 1
            AND    a.cp                 = b.cp

            LET  ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa = v_arr_beneficiario[v_indice_beneficiario].entidadFederativa
            
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa = "9"
            END IF 
 
         END IF
         
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
            -- se registra en la bitacora los datos de beneficiarios recibidos
            CALL fn_registra_peticion_registro_solicitud_benef(g_id_peticion                                                                                                                   ,
                                                                9                                          ,
                                                                v_indice_beneficiario                                                                                                           ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria    ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc               ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email             ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono          ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telMovil         ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre            ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno        ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apMaterno        ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa)
             
            -- debe traer todos los datos
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario  IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc                 IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email               IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono            IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre              IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno          IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa  IS NULL ) THEN

               -- los datos de un beneficiario estan mal, no procede la solicitud
               LET v_existe_beneficiario = FALSE
               EXIT FOR
            ELSE
               -- si le falta la CLABE
               IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria IS NULL ) THEN
                  -- se rechaza
                  LET v_existe_beneficiario = FALSE
                  EXIT FOR
               END IF
               -- la modalidad es diferente de fondo de ahorro. Se valida la CLABE
               -- si la cuenta CLABE no es correcta
               --IF ( NOT fn_verifica_estructura_clabe(ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria) ) THEN
               --   LET v_cta_clabe_correcta = FALSE
               --   EXIT FOR
               --END IF
            END IF   
            IF ws_ret_generico_solicitud_in.medioEntrega = "2" AND v_indice_beneficiario = 1 THEN 
               EXIT FOR
            END IF 
         END FOR
         
         -- se verifica que la modalidad traiga beneficiarios
         IF ( NOT v_existe_beneficiario OR NOT v_cta_clabe_correcta ) THEN
            -- se verifica si es por falta de datos de un beneficiario
            IF ( NOT v_existe_beneficiario ) THEN
               CALL fn_respuesta_ws(v_nss, v_rfc, 9,
                                  gi_solicitud_rechazada, gi_solicitud_sin_beneficiarios, 0, 0, NULL)
            ELSE
               -- se rechaza por la estructura de la cuenta clabe
               CALL fn_respuesta_ws(v_nss, v_rfc, 9,
                                  gi_solicitud_rechazada, gi_esctructura_cta_clabe_incorrecta, 0, 0, NULL)               
            END IF
         ELSE            
            -- se verifica la modalidad
            CALL fn_ret_disponibilidad_amort_excedentes(v_nss, v_rfc, v_indice_modalidad)
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
Ivan Vega     18 Oct 2013             - La solicitud de amort. excedentes tambien tendra que pasar
                                        a ser autorizada por el CESI, por lo que ya no se crean
                                        preautorizadas
======================================================================
}
FUNCTION fn_ret_disponibilidad_amort_excedentes(p_nss, p_rfc, p_indice_modalidad)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                LIKE afi_derechohabiente.rfc,
       p_num_credito        CHAR(10), -- numero de credito
       p_indice_modalidad   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tabla_saldo        VARCHAR(40),
       v_saldo_aivs         DECIMAL(22,6),
       v_saldo_pesos        DECIMAL(22,2),
       v_fecha_saldo        DATE,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_sql                STRING,
       v_tabla_consulta     VARCHAR(40),
       v_valor_fondo        LIKE glo_valor_fondo.precio_fondo,
       v_estado_solicitud   LIKE ret_solicitud_generico.estado_solicitud

DEFINE v_caso_crm          CHAR(10)       
   -- se obtiene el id de solicitud creada en el marcado buscando estado en 8 precapturada
   -- CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 9, 8) RETURNING v_id_solicitud
   -- Se elimina este llamado para hacer la busqueda sin el RFC   RPR 140616 *****
   
   SELECT id_solicitud
   INTO   v_id_solicitud
   FROM   ret_solicitud_generico
   WHERE  nss              = p_nss
   AND    modalidad_retiro = 9
   AND    estado_solicitud = 8   

   -- si la cuenta aparece marcada y lista para ser generada una solicitud
   IF ( v_id_solicitud IS NOT NULL ) THEN

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss               = p_nss
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
         TRY -- se intenta obtener el saldo
            LET v_sql = "\n SELECT SUM(monto_acciones)  ,",
                        "\n        SUM(monto_pesos   )   ",
                        "\n FROM   cta_movimiento        ",
                        "\n WHERE  id_derechohabiente = ?",
                        "\n AND    subcuenta = 46 ",
                        "\n AND    fondo_inversion <> 0 "
            
                        
            PREPARE sid_amortexced FROM v_sql
            EXECUTE sid_amortexced USING v_id_derechohabiente
                                   INTO v_saldo_aivs, v_saldo_pesos
            
            -- se obtiene el valor del fondo de inversion
            CALL fn_obtener_precio_fondo(TODAY, 11) RETURNING v_valor_fondo
            
            -- si el NSS tiene saldo
            IF ( v_saldo_aivs IS NOT NULL AND v_saldo_aivs > 0 ) THEN
            
               -- se calculan los pesos segun las AIVs y el valor del fondo
               LET v_saldo_pesos = v_saldo_aivs * v_valor_fondo
               
               -- la solicitud es aceptada
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_aceptada, 0, v_saldo_aivs, v_saldo_pesos)
                        
               -- se crea la solicitud en la tabla de historicos
               CALL fn_genera_solicitud_ret_amort_excedentes(p_nss, v_id_derechohabiente, gi_solicitud_aceptada, 0, v_saldo_aivs, v_saldo_pesos, v_id_solicitud, p_indice_modalidad)
            ELSE
               -- no tiene saldo
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_sin_saldo, 0, 0)
               
               CALL fn_genera_solicitud_ret_amort_excedentes(p_nss, v_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo, v_saldo_aivs, v_saldo_pesos, v_id_solicitud, p_indice_modalidad)
            END IF
         CATCH
            -- no hay conexion a la instancia           
            DISPLAY "No se puede conectar a la instancia de saldos"
            CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, -999, 0, 0)
            
         END TRY
      ELSE
         -- el nss no existe
         CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, 0)
      END IF
   ELSE
      -- se verifica si existe la solicitud en un estado en el que indique que ya esta en proceso
      -- CALL fn_obtener_id_solicitud_generico_con_estado(p_nss, p_rfc, 9,
      --                                                  ws_ret_generico_solicitud_in.caso_adai)
      --     RETURNING v_id_solicitud, v_estado_solicitud
      -- Se cambia este llamado para no enviar el rfc en la busqueda del id solicitud RPR  140616 *****

      LET v_caso_crm = ws_ret_generico_solicitud_in.casoCRM
       SELECT id_solicitud    ,
                estado_solicitud
         INTO   v_id_solicitud,
                v_estado_solicitud
         FROM   ret_solicitud_generico
         WHERE  nss              = p_nss
         AND    modalidad_retiro = 9
         AND    caso_adai        = v_caso_crm
         
      -- si no se encuentra, entonces no existe la solicitud
      IF ( v_id_solicitud IS NULL ) THEN
         CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_no_existe_solicitud, 0, 0)
      ELSE
         -- si el estado esta entre 10 y 209, desde solicitado hasta en proceso de restitucion
         -- la solicitud se encuentra en tramite
         IF ( v_estado_solicitud >= 10 AND v_estado_solicitud <= 209 AND v_estado_solicitud <> 100 ) THEN
            CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_tramite, 0, 0)
         ELSE
            -- si es 100, la solicitud fue rechazada
            IF ( v_estado_solicitud = 100 ) THEN
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_edo_rechazo, 0, 0)
            ELSE
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_edo_no_reconocido, 0, 0)
            END IF
         END IF
      END IF
      
   END IF

   -- se incrementa el indice
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
Ivan Vega      25Feb2014              - Se agrega referencia DAP a la respuesta del WS
======================================================================
}
FUNCTION fn_respuesta_ws(p_nss, p_rfc, p_modalidad, p_estado_solicitud, p_cod_rechazo, p_aivs, p_pesos, p_referencia_dap)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         p_modalidad        SMALLINT, -- modalidad de retiro
         p_estado_solicitud SMALLINT, -- estado de la solicitud
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_aivs             DECIMAL(24,6), -- importe en aivs
         p_pesos            DECIMAL(22,2), -- importe en pesos
		 p_referencia_dap   CHAR(12) -- referencia DAP

DEFINE v_desc_rechazo     CHAR(100)
       
   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estadoSolicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].codRechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoAvis       = p_aivs
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoPesos      = p_pesos
   

   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = "";
       SELECT des_larga
       INTO   v_desc_rechazo 
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF v_desc_rechazo IS NULL IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " ";
       ELSE 
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo  = v_desc_rechazo
       END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, p_modalidad, 0,
                                                         p_estado_solicitud, p_cod_rechazo, p_aivs,
                                                         p_pesos, NULL)

END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para registro de solicitud de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud(p_nss, p_caso_adai)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_caso_adai               LIKE ret_solicitud_generico.caso_adai,
       p_res_ejecucion           SMALLINT, -- resultado de la ejecucion
       p_accion                  SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_peticion_crea_solicitud RECORD LIKE ret_ws_peticion_crea_solicitud.* -- registro de peticion al ws
		
   -- se obtiene el id de peticion nuevo
   SELECT seq_ret_ws_generico.nextVal
   INTO   p_id_peticion
   FROM   systables
   WHERE  tabid = 1
   
   -- se asignan los datos
   LET v_r_ret_ws_peticion_crea_solicitud.id_peticion   = p_id_peticion
   LET v_r_ret_ws_peticion_crea_solicitud.f_peticion    = TODAY
   LET v_r_ret_ws_peticion_crea_solicitud.h_peticion    = CURRENT HOUR TO SECOND
   LET v_r_ret_ws_peticion_crea_solicitud.nss           = p_nss
   LET v_r_ret_ws_peticion_crea_solicitud.rfc           = NULL
   LET v_r_ret_ws_peticion_crea_solicitud.caso_adai     = p_caso_adai
   
   -- se inserta el registro de peticion
   INSERT INTO ret_ws_peticion_crea_solicitud VALUES ( v_r_ret_ws_peticion_crea_solicitud.* )

   -- se devuelve el id de la peticion
   RETURN p_id_peticion
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
                                                     p_nrp, p_f_inicio_pension, p_num_credito)
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
   LET v_r_ret_ws_det_peticion_crea_solicitud.grupo_ley73      = 0
   LET v_r_ret_ws_det_peticion_crea_solicitud.num_credito      = 0
         
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
Ivan Vega      25feb2014              - Se agrega la referencia DAP a la respuesta del servicio
======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud_resp(p_id_peticion, p_modalidad_retiro, p_subcuenta,
                                                          p_estado_solicitud, p_cod_rechazo, p_monto_aivs,
                                                          p_monto_pesos, p_referencia_dap)
DEFINE p_id_peticion       DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro  SMALLINT, -- modalidad de retiro
       p_subcuenta         SMALLINT, -- subcuenta de inversion
       p_estado_solicitud  SMALLINT, -- estado de la solicitud
       p_cod_rechazo       SMALLINT, -- codigo de rechazo
       p_monto_aivs        DECIMAL(22,6), -- saldo en AIVs
       p_monto_pesos       DECIMAL(22,2), -- saldo en pesos equivalente a AIVs por valor accion
	   p_referencia_dap    CHAR(12), -- referencia DAP
       v_r_ret_ws_det_peticion_crea_solicitud_resp RECORD LIKE ret_ws_det_peticion_crea_solicitud_resp.* -- registro de respuesta detalle de peticion al ws

   -- se asignan los datos
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.id_peticion            = p_id_peticion
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.modalidad_retiro       = p_modalidad_retiro
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_subcuenta         = p_subcuenta
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_estado_solicitud  = p_estado_solicitud
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_cod_rechazo       = p_cod_rechazo
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_avis        = p_monto_aivs
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_pesos       = p_monto_pesos
   --LET v_r_ret_ws_det_peticion_crea_solicitud_resp.referencia_dap         = p_referencia_dap
         
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

{
======================================================================
Clave: 
Nombre: fn_hash
Fecha creacion: Marzo 03, 2016
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
Genera un código HASH (pasado por parámetro) de una cadena de texto STRING
(pasada por parámetro),

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_hash_local(toDigest, algo)

   DEFINE toDigest, algo, result STRING
   DEFINE dgst security.Digest

--   IF algo IS NULL OR algo = "" THEN
--      LET algo = "SHA1" --(Default)
--   END IF
--   LET algo = "SHA256"

   TRY
      LET dgst = security.Digest.CreateDigest("SHA256")
      CALL dgst.AddStringData(toDigest)
      --LET result = dgst.DoBase64Digest()
      LET result = dgst.DoHexBinaryDigest()
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   RETURN result
   
END FUNCTION

PUBLIC FUNCTION fn_load_pdf(v_ruta_reporte, v_archivo_reporte, p_caso)
   DEFINE archivo           BYTE
   DEFINE v_archivo         STRING 
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_archivo_reporte STRING 
   DEFINE v_comando         STRING 
   DEFINE p_caso            CHAR(10)             
   DEFINE v_resultado       SMALLINT 

   LOCATE archivo IN MEMORY
   #DISPLAY v_ruta_reporte
   
   CALL archivo.readFile(v_ruta_reporte)
   
   LET ws_ret_generico_solicitud_out.archivoPdf = archivo
   
   CALL security.Base64.LoadBinary(v_ruta_reporte) RETURNING v_archivo
   
   DISPLAY "Parámetros enviados a la Rutina de Adjunta Documentos"
   
   LET v_archivo_reporte = 'Acuse'
   
   DISPLAY "v_archivo_reporte: ", v_archivo_reporte
   DISPLAY "v_ruta_reporte: ",v_ruta_reporte
   DISPLAY "El archivo en base 64", v_archivo
   -- Se quita el envío a CRM, lo hará el portal CCARSAFRE19-2
   --CALL fn_adjunta_documento(v_archivo_reporte, v_archivo, p_caso) RETURNING v_resultado
   LET v_comando="rm "||v_ruta_reporte
--   RUN v_comando 
   
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                    p_id_solicitud,p_pesos,p_sello,
                                    p_caso,p_clabe)

    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92--
    DEFINE p_sello           STRING -- Acuse Generado
    DEFINE p_caso            CHAR(10) -- Caso CRM 
    DEFINE p_clabe           CHAR(18) -- Clabe interbancaria  
    DEFINE v_tramite         CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo           CHAR(55)
    DEFINE medioSolicitud    CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE pesosTotal        CHAR(18) -- Suma en pesos total devuelto
    DEFINE archivo           BYTE
    DEFINE estadoConsulta    SMALLINT     -- Resultado de la Consulta
    DEFINE codRechazo        SMALLINT      -- Código de rechazo
    DEFINE reporte           om.SaxDocumentHandler
    DEFINE i                 SMALLINT
    DEFINE v_reporte         STRING
    DEFINE v_archivo         STRING 
    DEFINE v_ruta_listados   CHAR(40)
    DEFINE v_ruta_reporte    STRING
    DEFINE f_inicio          DATE
    DEFINE f_fin             DATE
    DEFINE v_query           STRING 
    DEFINE v_nombre          CHAR(120)
    DEFINE v_rfc             CHAR(13)
    DEFINE v_curp            CHAR(18)
    DEFINE v_nombre_stg      STRING 
    DEFINE v_fecha_paso      CHAR(16)
    DEFINE v_fecha           CHAR(16)
    DEFINE v_aviso           CHAR(255)
    DEFINE v_sello_funcionario STRING
    DEFINE v_sello           STRING
    DEFINE v_error           STRING
    DEFINE v_result          SMALLINT
 
   LET v_reporte= "RETWS19.4rp"
   LET v_aviso = NULL
   LET v_archivo = NULL

    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED
    LET v_archivo =  p_nss CLIPPED,"_", 
                     p_rfc CLIPPED,"_",
                     p_id_solicitud USING "&&&&&&&&&&","_"
                     ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                     ||DAY(TODAY) CLIPPED,".pdf"
                     
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" , v_archivo CLIPPED 
    
    LET v_ruta_pdf    = v_ruta_reporte
    LET v_archivo_pdf = v_archivo
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   
   -- Busca si hay aviso que publicar en el acuse
   SELECT aviso
   INTO   v_aviso
   FROM   ret_aviso_pdf_ssv
   WHERE  f_vig_inicio <= TODAY 
   AND    f_vig_final  >= TODAY
   
   IF v_aviso IS NOT NULL THEN  --- Relaciona el mensaje con la solicitud
      INSERT INTO ret_sol_aviso_pdf VALUES (p_id_solicitud, v_aviso);
   END IF 

   -- Se obtiene el certificado del funcionario

   SELECT a.rfc
   INTO   v_rfc
   FROM   ret_rfc_firma_pdf a
   WHERE  a.id_firma = 2;   --- Funcionario de Cartera
   
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      CALL fn_obtiene_certificado(p_sello CLIPPED, v_rfc CLIPPED )  RETURNING v_result, v_error, v_sello_funcionario 
      DISPLAY "NSS reporte ", p_nss
      
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                       p_id_solicitud,p_pesos,v_sello_funcionario, v_aviso,
                                       p_caso,p_clabe)
         FINISH REPORT pdf_acuse
      END IF
   END IF
  
END FUNCTION 

REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                 p_id_solicitud,p_pesos,p_sello, p_aviso, p_caso,p_clabe) 
    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92
    DEFINE p_sello           STRING -- Acuse Generado
    DEFINE p_aviso           CHAR(255)  --- Mensaje eventual 
    DEFINE p_caso            CHAR(10)  -- Caso CRM
    DEFINE p_clabe           CHAR(18)  -- Clabe interbancaria

    DEFINE v_tramite            CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo              CHAR(55)
    DEFINE v_medioSolicitud     CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE v_pesos              CHAR(18) -- Vivienda 92
    DEFINE v_pesosViv97         CHAR(18) -- Vivienda 97
    DEFINE v_pesosTotal         CHAR(18) -- Suma en pesos total devuelto

    DEFINE v_nombre               CHAR(60)

   FORMAT

   FIRST PAGE HEADER

      PRINTX p_nss
      PRINTX p_rfc
      LET v_nombre = p_nombre CLIPPED, " ", p_paterno CLIPPED, " ", p_materno CLIPPED
      PRINTX v_nombre
      PRINTX p_clabe
      PRINTX p_fecha_hora
      PRINTX p_id_solicitud
      PRINTX p_caso
      LET v_medioSolicitud = "En línea"
      PRINTX v_medioSolicitud
      LET v_pesos = p_pesos USING "$$$,$$$,$$&.&&"
      PRINTX v_pesos
      PRINTX p_sello
      PRINTX p_aviso


END REPORT

FUNCTION fn_adjunta_documento(p_nombre_archivo, p_archivo, p_caso)
DEFINE p_nombre_archivo STRING 
DEFINE p_archivo        STRING 
DEFINE p_caso           CHAR(10)
DEFINE v_regreso        SMALLINT 
DEFINE v_codigo         INTEGER 

   DEFINE arr_documentos RECORD
         nombre_documento STRING, 
         documento        STRING 
   END RECORD 

  DISPLAY "Parametros recibidos para el consumo de la funcion de documentos"
  DISPLAY "p_archivo: ",p_archivo
  DISPLAY "p_nombre_archivo: ", p_nombre_archivo

   LET v_regreso = 0
   LET arr_documentos.nombre_documento = p_nombre_archivo
   LET arr_documentos.documento        = p_archivo
   
   CALL fn_adjunta_docto_crm(p_caso, arr_documentos.*) RETURNING v_regreso, v_codigo
   IF v_regreso = 0 THEN 
--      IF v_codigo = 0 THEN 
         DISPLAY "Documento enviado a CRM exitosamente :", p_caso
--      ELSE 
--         DISPLAY "No se pudo integrar el documento, respuesta del Servicio Adjunta Documento :", v_codigo
--      END IF
   ELSE 
      DISPLAY "Problemas al invocar el servicio de Adjunta Dcoumentos :", v_regreso
   END IF 

RETURN v_regreso

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_amort_excedente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado de un retiro por amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014              - Se agrega referencia dap a la respuesta. Siempre en blanco
                                        para amort excedente
======================================================================
}
FUNCTION fn_respuesta_ws_amort_excedente(p_nss, p_rfc, p_modalidad, p_estado_solicitud, p_cod_rechazo, p_aivs, p_pesos)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         p_modalidad        SMALLINT, -- modalidad de retiro
         p_estado_solicitud SMALLINT, -- estado de la solicitud
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_aivs             DECIMAL(24,6), -- importe en aivs
         p_pesos            DECIMAL(22,2) -- importe en pesos

DEFINE  v_desc_rechazo      CHAR(100)
         
   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estadoSolicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].codRechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoAvis       = p_aivs
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoPesos      = p_pesos
   
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = "";
       SELECT des_larga
       INTO   v_desc_rechazo 
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF v_desc_rechazo  IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " ";
       ELSE 
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = v_desc_rechazo
       END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, p_modalidad, 46,
                                                         p_estado_solicitud, p_cod_rechazo, p_aivs,
                                                         p_pesos, NULL)

END FUNCTION

{
======================================================================
Nombre: fn_genera_solicitud_ret_amort_excedentes
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro de amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_solicitud_ret_amort_excedentes(p_nss, p_id_derechohabiente, p_estado_solicitud, p_rechazo, p_aivs, p_pesos, p_id_solicitud, p_indice_modalidad)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
       p_nss                 LIKE afi_derechohabiente.nss, 
       p_rfc                 LIKE afi_derechohabiente.rfc,
       p_estado_solicitud    SMALLINT      , -- estatus de la solicitud
       p_rechazo             SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_aivs                DECIMAL(24,6),
       p_pesos               DECIMAL(22,2),
       p_referencia_bancaria CHAR(12), -- clave de referencia bancaria
       p_id_solicitud        LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       p_indice_modalidad    SMALLINT, -- indice de la modalidad del retiro
       v_estatus             SMALLINT      ,
       v_resultado           SMALLINT      , -- resultado de la ejecucion
       v_subcuenta           SMALLINT      , -- subcuenta de retiro
       v_id_solicitud        LIKE ret_fondo_ahorro.id_solicitud,
       v_marca_amort_exced   LIKE sfr_marca.marca, -- marca de amortizaciones excedentes
       v_saldo_poseido       LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       v_r_ret_amort_excedente RECORD LIKE ret_amort_excedente.*, -- registro de retiro de amortizaciones excedentes
       v_conteo                SMALLINT,
       v_tipo_pago             SMALLINT,
       v_sql                   STRING, -- cadena con enunciado SQL
       v_cadena               STRING,  -- Cadena de caracteres para generar el SHA
       v_algoritmo            CHAR(6),
       v_curp                 CHAR(18),
       v_rfc                  CHAR(13),
       v_nombre               CHAR(40),
       v_ape_paterno          CHAR(40),
       v_ape_materno          CHAR(40),
       v_sha                  STRING,
       v_c_sha                CHAR(64),
       v_fecha_paso           CHAR(16),
       v_fecha_hora           CHAR(16),
       v_caso_crm             CHAR(10)
       
   -- se asigna la marca
   LET v_marca_amort_exced = 810
   LET v_cadena = ""
   LET v_algoritmo = ""
   LET v_sha = ""
   LET v_rfc = ""
   LET v_fecha_paso = CURRENT YEAR TO MINUTE 
   LET v_caso_crm = ws_ret_generico_solicitud_in.casoCRM

   -- si la solicitud fue aceptada
   IF ( p_estado_solicitud = gi_solicitud_aceptada ) THEN

--- ************************************************************************************************

      IF ws_ret_generico_solicitud_in.medioEntrega = 1 OR 
         ws_ret_generico_solicitud_in.medioEntrega = 2 THEN 
         -- Se debe eliminar el registro de la tabla ret_pago_spei ya que la rutina de beneficiarios inserta los datos
         DELETE 
         FROM   ret_pago_spei 
         WHERE  id_solicitud = p_id_solicitud
         
          --- se obtiene el sello para guardarlo en tablas
         SELECT curp, nombre_af, ap_paterno_af, ap_materno_af, rfc
         INTO   v_curp, v_nombre, v_ape_paterno, v_ape_materno, v_rfc 
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = p_id_derechohabiente

         IF v_curp IS NULL THEN 
            LET v_curp = ""
         END IF 
         IF v_nombre IS NULL THEN 
            LET v_nombre = ""
         END IF 
         IF v_ape_paterno IS NULL THEN 
            LET v_ape_paterno = ""
         END IF
         IF v_ape_materno IS NULL THEN 
            LET v_ape_materno = ""
         END IF 
         LET v_fecha_hora = v_fecha_paso[9,10],"/",v_fecha_paso[6,7],"/",v_fecha_paso[1,4]," ",v_fecha_paso[12,16]
         LET v_cadena = p_id_solicitud USING "##########", ws_ret_generico_solicitud_in.nss, 
                        v_rfc, v_fecha_hora, v_ape_paterno CLIPPED, v_ape_materno CLIPPED, 
                        v_nombre CLIPPED, p_pesos USING "<,<<<,<<&.&&"
                        
         LET v_algoritmo = "SHA256"
         
         CALL ERRORLOG("cadena>"||v_cadena||"<") 
         CALL ERRORLOG("algoritmo>"||v_algoritmo||"<")
         
         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
         CALL ERRORLOG("sha>"||v_sha||"<")

         LET v_c_sha = v_sha

         CALL ERRORLOG("c_sha>"||v_c_sha||"<")

         UPDATE ret_sol_medio_entrega
         SET    sello = v_c_sha,
                f_registro = CURRENT YEAR TO MINUTE  
         WHERE  id_solicitud = p_id_solicitud;
         
         -- obtenemos el caso_crm de la tabla
         SELECT caso_adai
         INTO   v_caso_crm
         FROM   ret_solicitud_generico
         WHERE  id_solicitud = p_id_solicitud;
         
         LET ws_ret_generico_solicitud_in.casoCRM = v_caso_crm
         LET p_estado_solicitud = 10

         IF ws_ret_generico_solicitud_in.medioEntrega = 2 THEN 
            CALL fn_genera_reporte(ws_ret_generico_solicitud_in.nss,v_rfc,
                                   v_ape_paterno, v_ape_materno, v_nombre, 
                                   v_fecha_hora,p_id_solicitud,p_pesos,v_cadena, 
                                   ws_ret_generico_solicitud_in.casoCRM,
                                   ws_ret_generico_solicitud_in.arr_beneficiario[1].clabeBancaria)
            CALL fn_load_pdf(v_ruta_pdf, v_archivo_pdf, ws_ret_generico_solicitud_in.casoCRM)--Se crea, se envia y se borra el reporte.pdf
         END IF 
         LET ws_ret_generico_solicitud_out.sello = v_c_sha

         ------    Esto es de prueba solamente
--         LET v_cadena = "0|39957660671|FIAA760920HDFGGN09|07/03/2018|FIGUEROA|AGUILAR|ANGEL|$2,416.06"
--         LET v_algoritmo = "SHA256"
--         CALL ERRORLOG("cadena prueba   >"||v_cadena||"<") 
--         CALL ERRORLOG("algoritmo prueba>"||v_algoritmo||"<")
         
--         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
--         CALL ERRORLOG("sha prueba  >"||v_sha||"<")
--         LET v_c_sha = v_sha
--         CALL ERRORLOG("c_sha prueba>"||v_c_sha||"<")
         -------  Hasta aqui terminan los datos de prueba
      END IF 



--- ***************************************************************************************************
   
      -- se asignan los datos a la solicitud de retiro de amortizaciones excedentes
      LET v_r_ret_amort_excedente.id_solicitud       = p_id_solicitud
      LET v_r_ret_amort_excedente.folio              = 0
      LET v_r_ret_amort_excedente.id_derechohabiente = p_id_derechohabiente
      LET v_r_ret_amort_excedente.nss                = p_nss
      LET v_r_ret_amort_excedente.f_solicitud        = TODAY
      LET v_r_ret_amort_excedente.tpo_retiro         = 1 -- cual
      LET v_r_ret_amort_excedente.total_aivs         = p_aivs
      LET v_r_ret_amort_excedente.total_importe      = p_pesos
      LET v_r_ret_amort_excedente.cod_actividad      = 1
      LET v_r_ret_amort_excedente.cod_rechazo        = p_rechazo
      LET v_r_ret_amort_excedente.estado_solicitud   = p_estado_solicitud
          
      -- se inserta el registro   
      INSERT INTO ret_amort_excedente VALUES ( v_r_ret_amort_excedente.* )

      -- se actualiza el estado de la tabla de control
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud
      
      -- se crean los registros para los beneficiarios de esta solicitud
      FOR v_conteo = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
         -- si se tiene cuenta clabe, es pago por SPEI
         IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabeBancaria IS NOT NULL ) THEN
            -- SPEI
            LET v_tipo_pago = 1
         ELSE
            -- pago por DAP
            LET v_tipo_pago = 2
         END IF
      
         CALL fn_registra_beneficiario_retiro_generico(p_id_solicitud,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].tipoBeneficiario,
                                                       v_tipo_pago, -- FALTA TIPO DE PAGO
                                                       1, -- FALTA PARENTESCO
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].apPaterno,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].apMaterno,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].nombre,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].telefono,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].email,
                                                       100,
                                                       p_aivs,
                                                       p_pesos,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabeBancaria,
                                                       p_referencia_bancaria,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].entidadFederativa)
         
         -- se verifica si el beneficiario
      END FOR
      
   ELSE
      -- se rechaza la solicitud
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             cod_rechazo      = p_rechazo,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud

      -- se desmarca la cuenta
      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, v_marca_amort_exced, p_id_solicitud,
                                           v_marca_amort_exced, "safreviv", g_proceso_cod_ret_amort_excedentes) 

   END IF

END FUNCTION 
 
