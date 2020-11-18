--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 27/10/2020
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS13                                                 #
#OBJETIVO          => WS CONSULTA DE PDF DE ACUSES DE SOLICITUDES TRAMITADAS  #
#                     POR MI CUENTA INFONAVIT, DEVOLUCION AUTOMATICA DEL SSV  # 
#FECHA INICIO      => Marzo 4, 2018                                           #
-------------------------------------------------------------------------------
#MODIFICACION 27/10/2020                                                      #
#OBJETIVO          => SE AGREGA EL LLAMADO A LA FUNCIÓN QUE REGISTRA LOS      #
#                     DATOS DE CADA INVOACIÓN A LOS WS QUE SEA REALIZADA      #
#POR               => EMMANUEL REYES, OMNISYS                                 #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_pdf_acuse_in RECORD
         nss              CHAR(11),     -- nss del trabajador
         conRetiro        CHAR(10)      -- Id Solicitud campo llave de la solicitud de retiro
       END RECORD,
       -- registro de respuesta
       ws_pdf_acuse_out  RECORD
         nss                 CHAR(11),     -- nss del trabajador
         curp                CHAR(18),     -- CURP del trabajador
         tramite             CHAR(50),     -- Descripción del Trámite
         grupo               CHAR(55),     -- Grupo Tramitado, debe ser 1
         apePaterno          CHAR(40),     -- Apellido paterno
         apeMaterno          CHAR(40),     -- Apellido materno
         nombre              CHAR(40),     -- Nombre
         medioSolicitud      CHAR(10),     -- Medio por el cual se hizo la solicitud 
         conRetiro           CHAR(10),     -- Id Solicitud
         pesosViv92          CHAR(18),     -- Monto de la Subcuenta de Vivienda 92
         pesosViv97          CHAR(18),     -- Monto de la Subcuenta de Vivienda 97
         pesosTotal          CHAR(18),     -- Suma de Vivienda 92 y Vivienda 97 devuelto
         fTramite            CHAR(16),     -- Fecha y hora de la Solicitud
         sello               CHAR(64),     -- Acuse generado para el trámite
         archivo             BYTE,         -- Archivo PDF
         estadoConsulta      SMALLINT,     -- Resultado de la Consulta
         codRechazo          SMALLINT      -- Código de rechazo
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
DEFINE g_sesion_id   CHAR(100)

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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS13."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    LET g_sesion_id = v_ruta_log.trim()

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_consulta_pdf_acuse_ley73(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Consulta PDF Acuse Ley 73 service") --, STYLE="naked")
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
    CALL fn_crea_servicio_consulta_pdf_acuse_ley73(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta del PDF Acuse Ley 73 1.0 ...")

    -- se inicia el motor de WS
    TRY
    CALL com.WebServiceEngine.Start()
    CATCH -- en caso de error
            DISPLAY("No se pudo iniciar el motor: " || STATUS)
            EXIT PROGRAM
    END TRY
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
Nombre: fn_crea_servicio_consulta_pdf_acuse_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de consulta de acuses de solicitudes tramitadas 
por Mi cuenta Infonavit, Devolución Automática del SSV a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_pdf_acuse_ley73(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroConsultaPDFAcuseLey73", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_consulta_pdf_acuse_ley73","fn_ret_consulta_pdf_acuse_ley73",ws_pdf_acuse_in,ws_pdf_acuse_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        DISPLAY "GENERA LA OPERACION"
        CALL v_webservice.publishOperation(op, "fn_ret_consulta_pdf_acuse_ley73")
        DISPLAY "OPERACION GENERADA"

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
            DISPLAY ("RegisterService OK "||STATUS)
            --display_status("Retiro Disponibilidad Ley 73 Service registrado")
            CALL ERRORLOG("Se registro el servicio consulta del PDF acuse para retiro Ley 73")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta del PDF acuse para retiro Ley 73': " || STATUS)
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
Nombre: fn_ret_consulta_pdf_acuse_ley73
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta el PDF de acuse de una solicitude tramitada por 
Mi cuenta Infonavit Devolución automática del SSV a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_consulta_pdf_acuse_ley73()
DEFINE v_indice_retiro SMALLINT,
       v_f_sol_paso         CHAR(16),
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
	    v_ruta_log           STRING,
	    v_cadena             STRING,
       v_consulta           STRING,
       v_indice             INTEGER,
       v_monto_paso         DECIMAL(14,2),
       v_monto_tot_paso     DECIMAL(14,2)
       
DEFINE arr_detalle RECORD
           nss                LIKE afi_derechohabiente.nss,
           curp               LIKE afi_derechohabiente.curp,
           tramite            CHAR(50),     -- Descripción del Trámite
	        grupo              CHAR(55),
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           conRetiro          LIKE ret_solicitud_generico.id_solicitud,
           pesosViv92         CHAR(18), -- Vivienda 92
           pesosViv97         CHAR(18), -- Vivienda 97
           pesosTotal         CHAR(18), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- Código de rechazo
           caso_crm           CHAR(10)       -- Caso CRM
END RECORD 
   -- se responde el servicio para pruebas
   INITIALIZE arr_detalle TO NULL  
   LET v_indice             = 1
   LET v_monto_paso         = 0
   LET v_monto_tot_paso     = 0
   
   LET arr_detalle.nss            = ws_pdf_acuse_in.nss
   LET arr_detalle.conRetiro      = ws_pdf_acuse_in.conRetiro
   LET arr_detalle.tramite        = 'Devolución del saldo de la subcuenta de vivienda'
   LET arr_detalle.grupo          = 'Uno (pensionados posteriores al 13 de enero de 2012)'
   LET arr_detalle.medioSolicitud = 'Internet'
   LET arr_detalle.estadoConsulta = gi_solicitud_aceptada
   LET arr_detalle.codRechazo     = 0

   DISPLAY "Parametros recibidos:"
   DISPLAY "NSS             : ", arr_detalle.nss
   DISPLAY "Consecutivo Ret : ", arr_detalle.conRetiro

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS13."
   LET v_cadena   = "pdf_acuses"
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)

   --- Se validan los parámetros de entrada
   IF arr_detalle.nss IS NULL AND arr_detalle.conRetiro IS NULL THEN 
      DISPLAY "Debe al menos enviar un parámetro para realizar la consulta"
      LET ws_pdf_acuse_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_pdf_acuse_out.codRechazo = gi_datos_incompletos
   ELSE 
      -- se obtienen el nombre del trabajador
      SELECT nombre_af, 
             ap_paterno_af, 
             ap_materno_af,
             curp
      INTO   arr_detalle.nombre,
             arr_detalle.apePaterno,
             arr_detalle.apeMaterno,
             arr_detalle.curp
      FROM   afi_derechohabiente
      WHERE  nss = arr_detalle.nss
      -- Se arma la consulta
      LET v_consulta = "\n SELECT a.id_solicitud, b.f_registro,          ",
                       "\n        b.sello, a.id_derechohabiente,         ",
                       "\n        a.caso_adai                            ",
                       "\n FROM   ret_solicitud_generico a,              ",
                       "\n        ret_sol_medio_entrega b                ",
                       "\n WHERE  a.id_solicitud  = b.id_solicitud       ",
                       "\n AND    a.nss           = '", arr_detalle.nss, "'",
                       "\n AND    b.grupo         = 1                    ",
                       "\n AND    b.medio_entrega = 2                    ",
                       "\n AND    b.sello         IS NOT NULL            ",
                       "\n AND    b.f_registro    IS NOT NULL            ",
                       "\n AND    a.id_solicitud  = ", arr_detalle.conRetiro


      DISPLAY "La consulta es:", v_consulta
      PREPARE exe_consulta_detalle FROM v_consulta
      DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

      FOREACH cur_consulta_detalle INTO arr_detalle.conRetiro, 
                                        arr_detalle.fTramite,
                                        arr_detalle.sello,
                                        v_id_derechohabiente,
                                        arr_detalle.caso_crm
         CALL fn_busca_movtos(v_id_derechohabiente, arr_detalle.conRetiro, 8) RETURNING v_monto_paso
         LET  v_monto_tot_paso = v_monto_paso
         DISPLAY "Viv 92 numerico :", v_monto_paso
         LET arr_detalle.pesosViv92 = v_monto_paso USING "$$,$$$,$$&.&&"
         DISPLAY "VIV 92 formateado :",arr_detalle.pesosViv92 
         CALL fn_busca_movtos(v_id_derechohabiente, arr_detalle.conRetiro, 4) RETURNING v_monto_paso
         LET v_monto_tot_paso = v_monto_tot_paso + v_monto_paso
         DISPLAY "Viv 97 numerico :", v_monto_paso
         LET arr_detalle.pesosViv97 = v_monto_paso USING "$$,$$$,$$&.&&"
         DISPLAY "VIV 97 formateado :",arr_detalle.pesosViv97
         LET arr_detalle.pesosTotal = v_monto_tot_paso  USING "$$,$$$,$$&.&&"
         LET v_indice = v_indice + 1
      END FOREACH 
      IF v_indice = 1 THEN 
         LET ws_pdf_acuse_out.apeMaterno     = ""
         LET ws_pdf_acuse_out.apePaterno     = ""
         LET ws_pdf_acuse_out.curp           = ""
         LET ws_pdf_acuse_out.grupo          = ""
         LET ws_pdf_acuse_out.medioSolicitud = ""
         LET ws_pdf_acuse_out.nombre         = ""
         LET ws_pdf_acuse_out.tramite        = ""
         LET ws_pdf_acuse_out.estadoConsulta = gi_solicitud_rechazada
         LET ws_pdf_acuse_out.codRechazo     = gi_no_existe_solicitud
      ELSE 
         CALL fn_genera_reporte(arr_detalle.*)
         CALL fn_load_pdf(v_ruta_pdf)--Se crea, se envia y se borra el reporte.pdf
         LET ws_pdf_acuse_out.apeMaterno     = arr_detalle.apeMaterno
         LET ws_pdf_acuse_out.apePaterno     = arr_detalle.apePaterno
         LET ws_pdf_acuse_out.codRechazo     = arr_detalle.codRechazo
         LET ws_pdf_acuse_out.conRetiro      = arr_detalle.conRetiro
         LET ws_pdf_acuse_out.curp           = arr_detalle.curp
         LET ws_pdf_acuse_out.estadoConsulta = arr_detalle.estadoConsulta
         LET v_f_sol_paso                    = arr_detalle.fTramite
         LET ws_pdf_acuse_out.fTramite       = v_f_sol_paso[9,10],"/",v_f_sol_paso[6,7],"/",v_f_sol_paso[1,4], " ", v_f_sol_paso[12,16] 
         LET ws_pdf_acuse_out.grupo          = arr_detalle.grupo
         LET ws_pdf_acuse_out.medioSolicitud = arr_detalle.medioSolicitud
         LET ws_pdf_acuse_out.nombre         = arr_detalle.nombre
         LET ws_pdf_acuse_out.pesosTotal     = arr_detalle.pesosTotal
         LET ws_pdf_acuse_out.pesosViv92     = arr_detalle.pesosViv92
         LET ws_pdf_acuse_out.pesosViv97     = arr_detalle.pesosViv97
         LET ws_pdf_acuse_out.sello          = arr_detalle.sello
         LET ws_pdf_acuse_out.tramite        = arr_detalle.tramite
      END IF 
   END IF

   DISPLAY "Invoca a la bitacora"
   CALL fn_invoca_registra_bitacora_ws()

END FUNCTION

----- 

FUNCTION fn_busca_movtos(p_id_derechohabiente, p_conRetiro, p_subcuenta)
   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE p_conRetiro          DECIMAL(10,0)
   DEFINE p_subcuenta          SMALLINT 
   DEFINE v_monto              DECIMAL(12,2)

   LET v_monto = 0

   IF p_subcuenta = 8 THEN 
      SELECT importe_viv92
      INTO   v_monto
      FROM   ret_ley73_generico
      WHERE  id_solicitud = p_conRetiro
   END IF 
   IF p_subcuenta = 4 THEN 
      SELECT importe_viv97
      INTO   v_monto
      FROM   ret_ley73_generico
      WHERE  id_solicitud = p_conRetiro
   END IF 
   
--   SELECT SUM(monto_pesos)
--   INTO   v_monto
--   FROM   ret_preliquida
--   WHERE  id_derechohabiente = p_id_derechohabiente
--   AND    id_referencia      = p_conRetiro
--   AND    subcuenta          = p_subcuenta
--   AND    monto_pesos        < 0

   IF v_monto IS NULL THEN 
      LET v_monto = 0
--   ELSE 
--      LET v_monto = v_monto * (-1)
   END IF 
   
RETURN v_monto
END FUNCTION 

PUBLIC FUNCTION fn_load_pdf(v_ruta_reporte)
   DEFINE archivo           BYTE
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_comando         STRING 

   LOCATE archivo IN MEMORY
   #DISPLAY v_ruta_reporte
   CALL archivo.readFile(v_ruta_reporte)
   LET ws_pdf_acuse_out.archivo = archivo
   LET v_comando="rm "||v_ruta_reporte
   RUN v_comando 
   
END FUNCTION

PRIVATE  FUNCTION fn_genera_reporte(p_arr_detalle)
DEFINE p_arr_detalle RECORD
           nss                LIKE afi_derechohabiente.nss,
           curp               LIKE afi_derechohabiente.curp,
           tramite            CHAR(50),     -- Descripción del Trámite
	        grupo              CHAR(55),
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           conRetiro          LIKE ret_solicitud_generico.id_solicitud,
           pesosViv92         CHAR(18), -- Vivienda 92
           pesosViv97         CHAR(18), -- Vivienda 97
           pesosTotal         CHAR(18), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- Código de rechazo
           caso_crm           CHAR(10)   -- Caso CRM
END RECORD 
    DEFINE reporte          om.SaxDocumentHandler
    DEFINE i                SMALLINT
    DEFINE v_reporte        STRING
    DEFINE v_ruta_listados  CHAR(40)
    DEFINE v_ruta_reporte   STRING
    DEFINE f_inicio         DATE
    DEFINE f_fin            DATE
    DEFINE v_query          STRING 
    DEFINE v_nombre         CHAR(120)
    DEFINE v_rfc            CHAR(13)
    DEFINE v_curp           CHAR(18)
    DEFINE v_nombre_stg     STRING 
    DEFINE v_fecha_paso     CHAR(16)
    DEFINE v_fecha          CHAR(16)
    DEFINE v_aviso          CHAR(255)
    
   LET v_reporte= "RETWS13.4rp"

    SELECT ruta_listados
    INTO v_ruta_listados
    FrOM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_fecha_paso = p_arr_detalle.fTramite
    LET v_fecha      = v_fecha_paso[9,10],"/",v_fecha_paso[6,7],"/",v_fecha_paso[1,4], " ", v_fecha_paso[12,16] 

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED 
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                       p_arr_detalle.nss CLIPPED,"_", 
                       p_arr_detalle.curp CLIPPED,"_",
                       p_arr_detalle.conRetiro USING "&&&&&&&&&&","_"
                       ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                       ||DAY(TODAY) CLIPPED,".pdf" 

    LET v_ruta_pdf = v_ruta_reporte
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   --- Busca si tiene un aviso que se haya incluido en el acuse original
   SELECT aviso
   INTO   v_aviso
   FROM   ret_sol_aviso_pdf 
   WHERE  id_solicitud = p_arr_detalle.conRetiro
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY "NSS reporte ", p_arr_detalle.nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_arr_detalle.*, v_fecha, v_aviso)
         FINISH REPORT pdf_acuse
      END IF
   END IF
  
END FUNCTION 

################################################################################
# fn_invoca_registra_bitacora_ws Se encarga de invocar a la función que recopila
#                                datos e invoca a la bitácora de WS 
# Requerimiento : Folio008-2020
# Fecha creación: 27/10/2020
# Autor         : Emmanuel Reyes, Omnisys
# Modificación  : Se cambia el 3er parámetro, ya no se envía un array ahora se
#               : manda una cadena de 51 posiciones, cambio hecho en el proceso
#               : de pruebas
# Fecha Modifica: 06/11/2020
# Autor         : Emmanuel Reyes, Omnisys
################################################################################
FUNCTION fn_invoca_registra_bitacora_ws()

   DEFINE v_resultado SMALLINT

   DEFINE v_fecha     DATE

   DEFINE v_id_ws_ctr_maestra SMALLINT --CHAR(50)

   DEFINE v_identificador_id  CHAR(50)
   
   LET v_fecha = TODAY

   --OBTIENE DATOS DEL CATALOGO
   SELECT id_ws_ctr_maestra
     INTO v_id_ws_ctr_maestra
     FROM ws_ctr_maestra
    WHERE id_ws_ctr_maestra = 2 -- EL ASIGNADO PARA EL WS ACTUAL 

    LET v_identificador_id = "50"{ws_pdf_acuse_in.nss       CLIPPED ,
                             ws_pdf_acuse_in.conRetiro CLIPPED}
    {DISPLAY "Parametros:"
    DISPLAY "v_id_ws_ctr_maestra: ",v_id_ws_ctr_maestra
    DISPLAY "g_sesion_id: ",g_sesion_id
    DISPLAY "v_identificador_id ", v_identificador_id CLIPPED}

    CALL fn_registra_bitacora_ws(v_id_ws_ctr_maestra CLIPPED,
                                 g_sesion_id         CLIPPED,
                                 v_identificador_id  CLIPPED)RETURNING v_resultado

    DISPLAY "Acaba invocacion a la bitacora"

END FUNCTION 

REPORT pdf_acuse(pm_arr_detalle, p_fecha, p_aviso) 
DEFINE pm_arr_detalle RECORD
           nss                LIKE afi_derechohabiente.nss,
           curp               LIKE afi_derechohabiente.curp,
           tramite            CHAR(50),     -- Descripción del Trámite
	        grupo              CHAR(55),
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           conRetiro          LIKE ret_solicitud_generico.id_solicitud,
           pesosViv92         CHAR(18), -- Vivienda 92
           pesosViv97         CHAR(18), -- Vivienda 97
           pesosTotal         CHAR(18), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- Código de rechazo
           caso_crm           CHAR(10)     -- Caso CRM
END RECORD 
DEFINE p_fecha                CHAR(16)
DEFINE v_nombre               CHAR(60)
DEFINE p_aviso                CHAR (255)

   FORMAT

   FIRST PAGE HEADER

      PRINTX pm_arr_detalle.nss
      PRINTX pm_arr_detalle.curp
      LET v_nombre = pm_arr_detalle.apePaterno CLIPPED, " ", pm_arr_detalle.apeMaterno CLIPPED, " ", pm_arr_detalle.nombre CLIPPED
      PRINTX v_nombre
      PRINTX pm_arr_detalle.tramite      
      PRINTX pm_arr_detalle.grupo
      PRINTX p_fecha
      PRINTX pm_arr_detalle.conRetiro
      PRINTX pm_arr_detalle.medioSolicitud
      PRINTX pm_arr_detalle.pesosViv92
      PRINTX pm_arr_detalle.pesosViv97
      PRINTX pm_arr_detalle.pesosTotal
      PRINTX pm_arr_detalle.sello
      PRINTX p_aviso
      PRINTX pm_arr_detalle.caso_crm
      


END REPORT