--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS10                                                 #
#OBJETIVO          => WS CONSULTA DE TRAMITES 
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
DEFINE ws_ret_cons_tramites_in RECORD
         nss              CHAR(11),     -- nss del trabajador
         conRetiro        CHAR(10),     -- Id Solicitud campo llave de la solicitud de retiro
         fSolIni          CHAR(8),      -- Fecha inicial de solicitud para la consulta
         fSolFin          CHAR(8)       -- Fecha final de solicitud para la consulta
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_tramites_out  RECORD
         nss                 CHAR(11),  -- Número de seguridad social del trabajador
         conRetiro           CHAR(10),  -- Id Solicitud campo llave de la solicitud de retiro
         fSolIni             CHAR(8),   -- Fecha inicial de solicitud para la consulta
         fSolFin             CHAR(8),   -- Fecha final de solicitud para la consulta
         estadoConsulta      SMALLINT,  -- Resultado de la Consulta
         codRechazo          SMALLINT, 
         res_consulta      DYNAMIC ARRAY OF RECORD
           conRetiro          CHAR(10), -- Id Solicitud
           nss                CHAR(11), -- Nss consultado
           apePaterno         CHAR(40), -- Apellido paterno
           apeMaterno         CHAR(40), -- Apellido materno
           nombre             CHAR(40), -- Nombre
           montoPesos         CHAR(10), -- Monto del trámite
           fTramite           CHAR(8),  -- Fecha de la Solicitud
           estadoSolicitud    CHAR(40), -- Estado de la solicitud
           codRechazo         CHAR(100),-- Código de Rechazo
           fPago              CHAR(8),  -- Fecha en la que realizó el depósito a la cuenta AAAAMMDD
           usuario            CHAR(20)  -- Usuario que realizó el trámite
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS10."
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
        CALL fn_crea_servicio_consulta_tramites_ley73(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Consulta Trámites Ley 73 service") --, STYLE="naked")
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
    CALL fn_crea_servicio_consulta_tramites_ley73(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta de Trámites Ley 73 1.0 ...")

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
Nombre: fn_crea_servicio_consulta_tramites_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_tramites_ley73(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroConsultaTramitesLey73", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_consulta_tramites_ley73","fn_ret_consulta_tramites_ley73",ws_ret_cons_tramites_in,ws_ret_cons_tramites_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_ret_consulta_tramites_ley73")

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
            CALL ERRORLOG("Se registro el servicio consulta de trámites para retiro Ley 73")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta de trámites para retiro Ley 73': " || STATUS)
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
Nombre: fn_ret_consulta_tramites_ley73
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los trámites realizados por la tableta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_consulta_tramites_ley73()
DEFINE v_indice_retiro SMALLINT,
       v_nss             LIKE afi_fondo72.nss,
       v_conRetiro       LIKE ret_solicitud_generico.id_solicitud,
       v_f_solicitud_ini LIKE ret_solicitud_generico.f_solicitud,
       v_f_solicitud_fin LIKE ret_solicitud_generico.f_solicitud,
	    v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	    v_ruta_log        STRING,
	    v_cadena          STRING,
       v_consulta        STRING,
       v_indice          INTEGER 

DEFINE arr_detalle RECORD
           conRetiro          CHAR(10), -- Id Solicitud
           nss                CHAR(11), -- Nss consultado
           apePaterno         CHAR(40), -- Apellido paterno
           apeMaterno         CHAR(40), -- Apellido materno
           nombre             CHAR(40), -- Nombre
           montoPesos         CHAR(10), -- Monto del trámite
           fTramite           DATE,     -- Fecha de la Solicitud
           estadoSolicitud    CHAR(40), -- Estado de la solicitud
           codRechazo         CHAR(100),-- Código de Rechazo
           fPago              CHAR(8),     -- Fecha en la que realizó el depósito a la cuenta AAAAMMDD
           usuario            CHAR(20)  -- Usuario que realizó el trámi       
END RECORD 

   -- se responde el servicio para pruebas
   LET v_nss             = NULL
   LET v_conRetiro       = NULL
   LET v_f_solicitud_ini = NULL
   LET v_f_solicitud_fin = NULL
   LET v_indice          = 1
   
   LET ws_ret_cons_tramites_out.nss = ws_ret_cons_tramites_in.nss
   LET ws_ret_cons_tramites_out.conRetiro = ws_ret_cons_tramites_out.conRetiro
   LET ws_ret_cons_tramites_out.fSolIni = ws_ret_cons_tramites_in.fSolIni
   LET ws_ret_cons_tramites_out.fSolFin = ws_ret_cons_tramites_in.fSolFin
   LET ws_ret_cons_tramites_out.estadoConsulta = gi_solicitud_aceptada
   LET ws_ret_cons_tramites_out.codRechazo = 0
   

   LET v_nss           = ws_ret_cons_tramites_in.nss
   LET v_conRetiro     = ws_ret_cons_tramites_in.conRetiro
   IF ws_ret_cons_tramites_in.fSolIni IS NOT NULL THEN 
      LET v_f_solicitud_ini = MDY(ws_ret_cons_tramites_in.fSolIni[5,6],ws_ret_cons_tramites_in.fSolIni[7,8],ws_ret_cons_tramites_in.fSolIni[1,4])
   END IF       
   IF ws_ret_cons_tramites_in.fSolFin IS NOT NULL THEN 
      LET v_f_solicitud_fin = MDY(ws_ret_cons_tramites_in.fSolFin[5,6],ws_ret_cons_tramites_in.fSolFin[7,8],ws_ret_cons_tramites_in.fSolFin[1,4])
   END IF       
   
   
   DISPLAY "Parámetros recibidos:"
   DISPLAY "NSS             : ", v_nss
   DISPLAY "Consecutivo Ret : ", v_conRetiro
   DISPLAY "Fecha Ini Sol   : ", v_f_solicitud_ini
   DISPLAY "Fecha Fin Sol   : ", v_f_solicitud_fin

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS10."
   LET v_cadena   = "detalle"
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)


   --- Se validan los parámetros de entrada
   IF v_nss IS NULL AND v_conRetiro IS NULL AND v_f_solicitud_ini IS NULL AND v_f_solicitud_fin IS NULL THEN 
      DISPLAY "Debe al menos enviar un parámetro para realizar la consulta"
      LET ws_ret_cons_tramites_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_ret_cons_tramites_out.codRechazo = gi_datos_incompletos
   ELSE 
      -- Se arma la consulta
      LET v_consulta = "\n SELECT a.id_solicitud,  a.nss, b.ap_paterno_af, b.ap_materno_af,  ",
                       "\n         b.nombre_af, c.importe_viv92 + c.importe_viv97,           ",
                       "\n         c.f_solicitud, d.des_corta, e.des_corta, f.rsp_f_pago,    ",
                       "\n         c.usuario                                                 ",
                       "\n  FROM   ret_solicitud_generico a                                  ",
                       "\n         LEFT OUTER JOIN ret_ws_consulta_pago_fico f               ",
                       "\n                      ON a.id_solicitud = f.id_solicitud           ",
                       "\n                     AND f.rsp_estatus = 2,                        ",
                       "\n         afi_derechohabiente    b,                                 ",
                       "\n         ret_ley73_generico     c,                                 ",
                       "\n         ret_estado_solicitud   d,                                 ",
                       "\n         ret_rechazo_generico   e,                                 ",
                       "\n         ret_sol_medio_entrega  g                                  ",
                       "\n  WHERE  a.id_derechohabiente = b.id_derechohabiente               ",
                       "\n  AND    a.id_solicitud       = c.id_solicitud                     ",
                       "\n  AND    a.id_solicitud       = g.id_solicitud                     ",
                       "\n  AND    g.grupo              = 1                                  ",
                       "\n  AND    g.medio_entrega      = 1                                  ",
                       "\n  AND    a.estado_solicitud   = d.estado_solicitud                 ",
                       "\n  AND    a.cod_rechazo        = e.cod_rechazo                      " 
      IF v_nss IS NOT NULL THEN 
         LET v_consulta = v_consulta CLIPPED , "\n  AND    a.nss = '", v_nss, "'"
      END IF
      IF v_conRetiro IS NOT NULL THEN 
         LET v_consulta = v_consulta CLIPPED , "\n  AND    a.id_solicitud = ", v_conRetiro
      END IF 
      IF v_f_solicitud_ini IS NOT NULL AND v_f_solicitud_fin IS NOT NULL THEN 
         LET v_consulta = v_consulta CLIPPED, "\n  AND    a.f_solicitud BETWEEN '",v_f_solicitud_ini, "' AND '", v_f_solicitud_fin, "'"
      END if
   END IF 
   DISPLAY "La consulta es:", v_consulta
   PREPARE exe_consulta_detalle FROM v_consulta
   DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

   FOREACH cur_consulta_detalle INTO arr_detalle.*       
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].conRetiro       = arr_detalle.conRetiro
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].nss             = arr_detalle.nss
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].apePaterno      = arr_detalle.apePaterno
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].apeMaterno      = arr_detalle.apeMaterno
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].nombre          = arr_detalle.nombre
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].montoPesos      = arr_detalle.montoPesos
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].fTramite        = arr_detalle.fTramite USING "YYYYMMDD"
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].estadoSolicitud = arr_detalle.estadoSolicitud
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].codRechazo      = arr_detalle.codRechazo
      IF arr_detalle.fPago IS NOT NULL THEN 
         LET ws_ret_cons_tramites_out.res_consulta[v_indice].fPago           = arr_detalle.fPago --USING "YYYYMMDD"
      ELSE 
         LET ws_ret_cons_tramites_out.res_consulta[v_indice].fPago           = ""
      END IF 
      LET ws_ret_cons_tramites_out.res_consulta[v_indice].usuario         = arr_detalle.usuario

      LET v_indice = v_indice + 1

   END FOREACH 

   IF v_indice = 1 THEN 
      LET ws_ret_cons_tramites_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_ret_cons_tramites_out.codRechazo     = gi_no_existe_solicitud
   END IF 

END FUNCTION


