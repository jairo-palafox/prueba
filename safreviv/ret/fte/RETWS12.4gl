--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS12                                                 #
#OBJETIVO          => WS CONSULTA DE ACUSES DE SOLICITUDES TRAMITADAS POR     #
#                     MI CUENTA INFONAVIT, DEVOLUCION AUTOMATICA DEL SSV      # 
#FECHA INICIO      => Marzo 4, 2018                                           #
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
DEFINE ws_ret_cons_acuses_in RECORD
         nss              CHAR(11),     -- nss del trabajador
         curp             CHAR(18),     -- CURP del trabajador
         conRetiro        CHAR(10),     -- Id Solicitud campo llave de la solicitud de retiro
         fTramite         CHAR(8),      -- Fecha de solicitud 
         grupo            CHAR(8)       -- Grupo Tramitado, debe ser 1
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_acuses_out  RECORD
         nss                 CHAR(11),     -- nss del trabajador
         curp                CHAR(18),     -- CURP del trabajador
         tramite             CHAR(50),     -- Descripción del Trámite
         grupo               CHAR(55),     -- Grupo Tramitado, debe ser 1
         apePaterno          CHAR(40), -- Apellido paterno
         apeMaterno          CHAR(40), -- Apellido materno
         nombre              CHAR(40), -- Nombre
         medioSolicitud      CHAR(10), -- Medio por el cual se hizo la solicitud 
         estadoConsulta      SMALLINT,  -- Resultado de la Consulta
         codRechazo          SMALLINT, 
         res_consulta      DYNAMIC ARRAY OF RECORD
           conRetiro          CHAR(10), -- Id Solicitud
           pesosViv92         CHAR(10), -- Monto de la Subcuenta de Vivienda 92
           pesosViv97         CHAR(10), -- Monto de la Subcuenta de Vivienda 97
           pesosTotal         CHAR(10), -- Suma de Vivienda 92 y Vivienda 97 devuelto
           fTramite           CHAR(16), -- Fecha y hora de la Solicitud
           sello              CHAR(64)  -- Acuse generado para el trámite
         END RECORD
       END RECORD
         
DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado
DEFINE g_identificador_servicio   SMALLINT
DEFINE g_eventoID                 CHAR(100)
DEFINE g_sesionID                 CHAR(100)
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS12."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  

   LET g_identificador_servicio   = 1
   LET g_eventoID                 = 'ConsultaAcuses'
   LET g_sesionID                 = v_ruta_log
    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_consulta_acuses_ley73(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Consulta Acuses Ley 73 service") --, STYLE="naked")
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
    CALL fn_crea_servicio_consulta_acuses_ley73(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta de Acuses Ley 73 1.0 ...")

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
Nombre: fn_crea_servicio_consulta_acuses_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de consulta de acuses de solicitudes tramitadas 
por Mi cuenta Infonavit, Devolución Automática del SSV a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_acuses_ley73(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroConsultaAcusesLey73", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_consulta_acuses_ley73","fn_ret_consulta_acuses_ley73",ws_ret_cons_acuses_in,ws_ret_cons_acuses_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_ret_consulta_acuses_ley73")

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
            --display_status("Retiro Disponibilidad Ley 73 Service registrado")
            CALL ERRORLOG("Se registro el servicio consulta de acuses para retiro Ley 73")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de acuses para retiro Ley 73': " || STATUS)
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
Nombre: fn_ret_consulta_acuses_ley73
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los acuses de solicitudes tramitadas por 
Mi cuenta Infonavit Devolución automática del SSV a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_consulta_acuses_ley73()
DEFINE v_indice_retiro SMALLINT,
       v_nss                LIKE afi_derechohabiente.nss,
       v_curp               LIKE afi_derechohabiente.curp,
       v_conRetiro          LIKE ret_solicitud_generico.id_solicitud,
       v_f_solicitud        LIKE ret_solicitud_generico.f_solicitud,
       v_f_sol_paso         CHAR(16),
	    v_grupo              LIKE ret_ley73_generico.gpo_ley73,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
	    v_ruta_log           STRING,
	    v_cadena             STRING,
       v_consulta           STRING,
       v_indice             INTEGER 

DEFINE arr_detalle RECORD
           conRetiro          CHAR(10), -- Id Solicitud
           pesosViv92         CHAR(10), -- Vivienda 92
           pesosViv97         CHAR(10), -- Vivienda 97
           pesosTotal         CHAR(10), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           id_derechohabiente DECIMAL(10,0) -- id_derechohabiente
END RECORD 
 
 
   -- se responde el servicio para pruebas
   LET v_nss                    = NULL
   LET v_conRetiro              = NULL
   LET v_f_solicitud            = NULL
   LET v_id_derechohabiente     = NULL
   LET v_indice                 = 1
   
   LET ws_ret_cons_acuses_out.nss            = ws_ret_cons_acuses_in.nss
   LET ws_ret_cons_acuses_out.curp           = ws_ret_cons_acuses_in.curp
   LET ws_ret_cons_acuses_out.tramite        = 'Devolución del saldo de la subcuenta de vivienda'
   LET ws_ret_cons_acuses_out.grupo          = 'Uno (pensionados posteriores al 13 de enero de 2012)'
   LET ws_ret_cons_acuses_out.medioSolicitud = 'Internet'
   LET ws_ret_cons_acuses_out.estadoConsulta = gi_solicitud_aceptada
   LET ws_ret_cons_acuses_out.codRechazo = 0
   

   LET v_nss           = ws_ret_cons_acuses_in.nss
   LET v_curp          = ws_ret_cons_acuses_in.curp
   LET v_conRetiro     = ws_ret_cons_acuses_in.conRetiro
   LET v_grupo         = ws_ret_cons_acuses_in.grupo
   
   IF ws_ret_cons_acuses_in.fTramite IS NOT NULL THEN 
      LET v_f_solicitud = MDY(ws_ret_cons_acuses_in.fTramite[5,6],ws_ret_cons_acuses_in.fTramite[7,8],ws_ret_cons_acuses_in.fTramite[1,4])
   END IF       
   
   DISPLAY "Parámetros recibidos:"
   DISPLAY "NSS             : ", v_nss
   DISPLAY "CURP            : ", v_curp
   DISPLAY "Consecutivo Ret : ", v_conRetiro
   DISPLAY "Fecha de Trámite: ", v_f_solicitud
   DISPLAY "Grupo           : ", v_grupo

   -- se registra bitácora
   -- se guarda la consulta de datos en la bitacora
   CALL fn_registra_bitacora()
   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS12."
   LET v_cadena   = "acuses"
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)


   --- Se validan los parámetros de entrada
   IF v_nss IS NULL AND v_conRetiro IS NULL AND v_f_solicitud IS NULL AND v_grupo IS NULL AND v_curp IS NULL THEN 
      DISPLAY "Debe al menos enviar un parámetro para realizar la consulta"
      LET ws_ret_cons_acuses_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_ret_cons_acuses_out.codRechazo = gi_datos_incompletos
   ELSE 
      -- se obtienen el nombre del trabajador
      SELECT nombre_af, 
             ap_paterno_af, 
             ap_materno_af,
             curp
      INTO   ws_ret_cons_acuses_out.nombre,
             ws_ret_cons_acuses_out.apePaterno,
             ws_ret_cons_acuses_out.apeMaterno,
             ws_ret_cons_acuses_out.curp
      FROM   afi_derechohabiente
      WHERE  nss = v_nss
      -- Se arma la consulta
      LET v_consulta = "\n SELECT a.id_solicitud, 0, 0, 0, b.f_registro, ",
                       "\n        b.sello, a.id_derechohabiente          ",
                       "\n FROM   ret_solicitud_generico a,              ",
                       "\n        ret_sol_medio_entrega b                ",
                       "\n WHERE  a.id_solicitud  = b.id_solicitud       ",
                       "\n AND    a.nss           = '", v_nss,           "'",
                       "\n AND    b.grupo         = 1                    ",
                       "\n AND    b.medio_entrega = 2                    ",
                       "\n AND    b.sello         IS NOT NULL            ",
                       "\n AND    b.f_registro    IS NOT NULL            "

      IF v_conRetiro IS NOT NULL THEN 
         LET v_consulta = v_consulta CLIPPED , "\n  AND    a.id_solicitud = ", v_conRetiro
      END IF 
      IF v_f_solicitud IS NOT NULL THEN 
         LET v_consulta = v_consulta CLIPPED, "\n  AND    a.f_solicitud BETWEEN '",v_f_solicitud, "' AND '", v_f_solicitud, "'"
      END if
      DISPLAY "La consulta es:", v_consulta
      PREPARE exe_consulta_detalle FROM v_consulta
      DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

      FOREACH cur_consulta_detalle INTO arr_detalle.*       
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].conRetiro       = arr_detalle.conRetiro
         LET v_f_sol_paso                                                  = arr_detalle.fTramite
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].fTramite        = v_f_sol_paso[9,10],"/",v_f_sol_paso[6,7],"/",v_f_sol_paso[1,4], " ", v_f_sol_paso[12,16] 
         DISPLAY "Fecha de Trámite :", arr_detalle.fTramite
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].sello           = arr_detalle.sello
         CALL fn_busca_movtos(arr_detalle.id_derechohabiente, arr_detalle.conRetiro, 8) RETURNING arr_detalle.pesosViv92 
         CALL fn_busca_movtos(arr_detalle.id_derechohabiente, arr_detalle.conRetiro, 4) RETURNING arr_detalle.pesosViv97
         LET arr_detalle.pesosTotal = arr_detalle.pesosViv92  + arr_detalle.pesosViv97 
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].pesosViv92      = arr_detalle.pesosViv92
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].pesosViv97      = arr_detalle.pesosViv97
         LET ws_ret_cons_acuses_out.res_consulta[v_indice].pesosTotal      = arr_detalle.pesosTotal
         LET v_indice = v_indice + 1
        
      END FOREACH 

   END IF 

   IF v_indice = 1 THEN 
      LET ws_ret_cons_acuses_out.apeMaterno     = ""
      LET ws_ret_cons_acuses_out.apePaterno     = ""
      LET ws_ret_cons_acuses_out.curp           = ""
      LET ws_ret_cons_acuses_out.grupo          = ""
      LET ws_ret_cons_acuses_out.medioSolicitud = ""
      LET ws_ret_cons_acuses_out.nombre         = ""
      LET ws_ret_cons_acuses_out.tramite        = ""
      LET ws_ret_cons_acuses_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_ret_cons_acuses_out.codRechazo     = gi_no_existe_solicitud
   END IF 

END FUNCTION

----- 

FUNCTION fn_busca_movtos(p_id_derechohabiente, p_conRetiro, p_subcuenta)
   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE p_conRetiro          DECIMAL(10,0)
   DEFINE p_subcuenta          SMALLINT 
   DEFINE v_monto              DECIMAL(12,2)

   LET v_monto = 0

   SELECT SUM(monto_pesos)
   INTO   v_monto
   FROM   ret_preliquida
   WHERE  id_derechohabiente = p_id_derechohabiente
   AND    id_referencia      = p_conRetiro
   AND    subcuenta          = p_subcuenta
   AND    monto_pesos        < 0

   IF v_monto IS NULL THEN 
      LET v_monto = 0
   ELSE 
      LET v_monto = v_monto * (-1)
   END IF 
   
RETURN v_monto
END FUNCTION 
################################################################################
#- funcion para registrar los eventos en la bitacora                           #
# Autor - Jairo Giovanny Palafox Sanchez                                       #
# Empresa -Omnisys                                                             #
# Fecha Creacion : 26-10-2020                                                  #
################################################################################
FUNCTION fn_registra_bitacora()
 DEFINE v_resultado           SMALLINT
 DEFINE v_identificadorID     CHAR(51)

  -- se asigna identificador 
  LET v_identificadorID  = ws_ret_cons_acuses_in.nss       CLIPPED ,
                           ws_ret_cons_acuses_in.curp      CLIPPED ,
                           ws_ret_cons_acuses_in.conRetiro CLIPPED , 
                           ws_ret_cons_acuses_in.fTramite  CLIPPED , 
                           ws_ret_cons_acuses_in.grupo     CLIPPED 

  -- se ejecuta funcion global para el registro de la bitacora por evento
  CALL fn_registra_bitacora_ws(g_identificador_servicio,g_sesionID,v_identificadorID) RETURNING v_resultado
  DISPLAY "Resultado registro Bitacora: >>", v_resultado  
END FUNCTION