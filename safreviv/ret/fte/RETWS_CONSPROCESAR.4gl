--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS14                                                 #
#OBJETIVO          => WS CONSULTA - MARCA Y DESMARCA PROCESAR                 #
#                     LEY 73                                                  #
#FECHA INICIO      => Febrero 9, 2018                                         #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "ret_saldo_preliminar.inc" -- saldo prelimminar PROCESAR
GLOBALS "RETG01.4gl"               -- Constantes para los codigos de error

GLOBALS
-- registro de entrada para la consulta
DEFINE ws_in RECORD
         idSistema        SMALLINT,     --- (7)
         idEbusiness      SMALLINT,     --- (7)
         idPortafolio     SMALLINT,     --- (7)
         idServicio       SMALLINT,     --- (49)
         idCliente        SMALLINT,     --- (30 - Consulta, 44 - marca y consulta, 60 - desmarca) 
         idCanal          SMALLINT,     --- (7)
         codoperCliente   CHAR(50),     --- (INFONAVIT)
         fecha            DATETIME YEAR TO FRACTION(5),  --- (Fecha, horas, minutos, segundos y milesimas de segundo
         curp             CHAR(18),     --- (CURP A CONSULTAR)
         nss              CHAR(11),     --- (Nss del trabajador)
         canal            CHAR(10)      --- (Quien hace el llamado al servicio TRM, SACI, OTROS)
       END RECORD,
       -- registro de respuesta
       ws_out  RECORD
         codoper                         STRING, 
         codRespuesta                    STRING, 
         codRespuestaOpr                 STRING, 
         descRespuesta                   STRING, 
         motivos             DYNAMIC ARRAY OF RECORD  
            base                         STRING,
            idMotivo                     STRING,
            descripcion                  STRING 
         END RECORD, 
         codoperCliente                  STRING, 
         tiempoRespuesta                 STRING, 
         fecha                           DATETIME YEAR TO FRACTION(5), 
         nss                             CHAR(11),
         curp                            CHAR(18),
         canal                           CHAR(10),
         cveInstitutoOrigen              CHAR(10),
         cveAfore                        CHAR(03),
         nombre                          CHAR(40),
         apellidoPaterno                 CHAR(40),
         apellidoMaterno                 CHAR(40),
         rfc                             CHAR(13),
         curpRegistrada                  CHAR(18),
         saldoSar92                      CHAR(14),
         saldoRetiro97                   CHAR(14),
         saldoCuotaSocial                CHAR(14),
         saldoCesantiaVejez              CHAR(14),
         saldoVivienda97                 CHAR(14),
         saldoVivienda97AIVS             CHAR(14),
         saldoVivienda92                 CHAR(14),
         saldoVivienda92AIVS             CHAR(14),
         saldoAhorroRetiroIB             CHAR(14),
         saldoAportacionesVoluntarias    CHAR(14),
         saldoRetiro92I                  CHAR(14),
         saldoAportaCompRetiro           CHAR(14),
         saldoViviendaFI92               CHAR(14),
         saldoViviendaFI92AIVS           CHAR(14),
         saldoAportaLargoPlazo           CHAR(14),
         saldoFI08                       CHAR(14),
         saldoFI08AIVS                   CHAR(14),
         saldoRetiroI08                  CHAR(14),
         saldoCVI                        CHAR(14),
         saldoAhorroSolidario            CHAR(14),
         saldoCuotaSocialI               CHAR(14),
         saldoBonoMontoUDIS              CHAR(14),
         anhoVencimiento                 CHAR(14),
         estatusCuentaIndividual         CHAR(14),
         estatusViviendaI                CHAR(14),
         estatusViviendaF                CHAR(14),
         diagnostico                     CHAR(50),
         descripcionDiagnostico          CHAR(100),
         folioSolicitud                  CHAR(15),
         resultado                       CHAR(10),
         descripcion                     CHAR(100)
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS14."
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
        CALL fn_servicio_procesar(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Consulta y Marca Procesar service") --, STYLE="naked")
            --display_status("Retiro Service Startup")
        ELSE
            IF num_args() <> 0 THEN
                CALL exitHelp()
                EXIT PROGRAM
            END IF
        END IF
    END IF
  
    -- se crea el servicio
    CALL ERRORLOG("invoca creacion de servicio")
    CALL fn_servicio_procesar(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta y Marca Procesar 1.0 ...")

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
Nombre: fn_servicio_procesar
Fecha creacion: Febrero 12, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Servicio para llamado a la Consulta y Marca de ProceSAR

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_servicio_procesar(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroConsultaMarcaProcesar", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_Consulta_Marca_procesar","fn_Consulta_Marca_procesar",ws_in,ws_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_Consulta_Marca_procesar")

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
            CALL ERRORLOG("Se registro el servicio consulta y marca Procesar")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta y Marca ProceSAR': " || STATUS)
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
Nombre: fn_Consulta_Marca_procesar
Fecha creacion: Febrero 12, 2018
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta y marca en ProceSAR

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_Consulta_Marca_procesar()
DEFINE v_indice_retiro SMALLINT,
       v_nss             LIKE afi_fondo72.nss,
       v_grupo           SMALLINT,
       v_medio_entrega   SMALLINT,
	    v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	    v_ruta_log        STRING,
       v_resultado       STRING,
	    v_cadena          STRING,
       v_error           SMALLINT,
       v_peticion        DECIMAL(9,0)
       
       
   --CALL fn_registra_peticion() RETURNING v_peticion
   -- se responde el servicio para pruebas
   LET ws_out.nss   = ws_in.nss
   LET ws_out.curp  = ws_in.curp
   LET ws_out.canal = ws_in.canal
   
   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS14."
   LET v_cadena   = ws_in.nss
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   
   -- Aqui se guardan los valores recibidos 

   -- Se hace el llamado al servicio de ProceSAR
   -- Si no se recibieron los parámetros del encabezado se asignan por default
   IF ws_in.idSistema IS NULL OR ws_in.idSistema = 0 THEN 
      LET ws_in.idSistema = 7
   END IF 
   IF ws_in.idEbusiness IS NULL OR ws_in.idEbusiness = 0 THEN 
      LET ws_in.idEbusiness = 7
   END IF 
   IF ws_in.idPortafolio IS NULL OR ws_in.idPortafolio = 0 THEN 
      LET ws_in.idPortafolio = 7
   END IF 
   IF ws_in.idServicio IS NULL OR ws_in.idServicio = 0 THEN 
      LET ws_in.idServicio = 49
   END IF 
   IF ws_in.idCanal IS NULL OR ws_in.idCanal = 0 THEN 
      LET ws_in.idCanal = 7
   END IF 
   IF ws_in.codoperCliente IS NULL OR ws_in.codoperCliente <> "INFONAVIT" THEN 
      LET ws_in.codoperCliente = "INFONAVIT"
   END IF 
   
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = ws_in.idSistema 
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = ws_in.idEbusiness
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = ws_in.idPortafolio
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = ws_in.idServicio
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = ws_in.idCliente   -- 30 - Consulta, 44 - marca y consulta, 60 - desmarca 
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = ws_in.idCanal
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = ws_in.codoperCliente  
   LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
   DISPLAY "Parametros  de envío" 

   DISPLAY "idSistema     :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema
   DISPLAY "idEbusiness   :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness
   DISPLAY "idPortafolio  :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio
   DISPLAY "idServicio    :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio
   DISPLAY "idCliente     :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente
   DISPLAY "idCanal       :", ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal
   DISPLAY "codoperCliente:", ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente
   DISPLAY "fecha         :", ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha USING "YYYY-MM-DD HH:MIN:SS"
   
   -- se inicializan las variables del WS con los valores a consultar   
   LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = ws_in.curp
   LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = ws_in.nss
  
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado       

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET ws_out.codoper                      = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.codoper
      LET ws_out.codRespuesta                 = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.codRespuesta
      LET ws_out.codRespuestaOpr              = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.codRespuestaOpr
      LET ws_out.descRespuesta                = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.descRespuesta
      LET ws_out.motivos[1].base              = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.motivos.motivo[1].base
      LET ws_out.motivos[1].descripcion       = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.motivos.motivo[1].descripcion
      LET ws_out.motivos[1].idMotivo          = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.motivos.motivo[1].idMotivo
      LET ws_out.codoperCliente               = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.codoperCliente
      LET ws_out.tiempoRespuesta              = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.tiempoRespuesta
      IF ns1parametersConsultarSaldoPreliminarResponse.ssnrop.fecha IS NULL THEN 
         LET ws_out.fecha = CURRENT YEAR TO FRACTION 
      ELSE 
         LET ws_out.fecha                        = ns1parametersConsultarSaldoPreliminarResponse.ssnrop.fecha
      END IF 
      LET ws_out.anhoVencimiento              = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.anhoVencimiento
      LET ws_out.apellidoMaterno              = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.apellidoMaterno
      LET ws_out.apellidoPaterno              = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.apellidoPaterno
      LET ws_out.curpRegistrada               = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.curpRegistrada
      LET ws_out.cveAfore                     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.cveAfore
      LET ws_out.cveInstitutoOrigen           = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.cveInstitutoOrigen
      LET ws_out.descripcion                  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.descripcion
      LET ws_out.descripcionDiagnostico       = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.descripcionDiagnostico
      LET ws_out.diagnostico                  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
      LET ws_out.estatusCuentaIndividual      = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual
      LET ws_out.estatusViviendaF             = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaF
      LET ws_out.estatusViviendaI             = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaI
      LET ws_out.folioSolicitud               = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.folioSolicitud
      LET ws_out.nombre                       = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.nombre
      LET ws_out.resultado                    = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.resultado
      LET ws_out.rfc                          = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.rfc
      LET ws_out.saldoAhorroRetiroIB          = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoAhorroRetiroIB
      LET ws_out.saldoAhorroSolidario         = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoAhorroSolidario
      LET ws_out.saldoAportaCompRetiro        = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoAportaCompRetiro
      LET ws_out.saldoAportaLargoPlazo        = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoAportaLargoPlazo
      LET ws_out.saldoAportacionesVoluntarias = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoAportacionesVoluntarias
      LET ws_out.saldoBonoMontoUDIS           = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoBonoMontoUDIS
      LET ws_out.saldoCVI                     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoCVI
      LET ws_out.saldoCesantiaVejez           = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoCesantiaVejez
      LET ws_out.saldoCuotaSocial             = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoCuotaSocial
      LET ws_out.saldoCuotaSocialI            = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoCuotaSocialI
      LET ws_out.saldoFI08                    = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoFI08
      LET ws_out.saldoFI08AIVS                = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoFI08AIVS
      LET ws_out.saldoRetiro92I               = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoRetiro92I
      LET ws_out.saldoRetiro97                = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoRetiro97
      LET ws_out.saldoRetiroI08               = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoRetiroI08
      LET ws_out.saldoSar92                   = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoSar92
      LET ws_out.saldoVivienda92              = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
      LET ws_out.saldoVivienda92AIVS          = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
      LET ws_out.saldoVivienda97              = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97
      LET ws_out.saldoVivienda97AIVS          = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
      LET ws_out.saldoViviendaFI92            = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoViviendaFI92
      LET ws_out.saldoViviendaFI92AIVS        = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoViviendaFI92AIVS
      --CALL fn_guarda_respuesta(v_peticion) 
    ELSE 
        DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
        LET ws_out.diagnostico = 127
        -- se devuelve el codigo de error del WS y fecha nula
        --RETURN wsError.code, NULL
    END IF

   
END FUNCTION
{
======================================================================
Nombre: fn_guarda_respuesta
Fecha creacion: Febrero 12, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Registra los datos de respuesta que se enviaron de
una peticion de WS de Consulta - Marca ProceSAR

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_guarda_respuesta(p_id_peticion)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_rfc                     LIKE afi_derechohabiente.rfc,
       p_caso_adai               LIKE ret_solicitud_generico.caso_adai,
       p_cuenta_clabe            LIKE ret_ws_peticion_marca.cuenta_clabe,
       p_res_ejecucion           SMALLINT, -- resultado de la ejecucion
       p_accion                  SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_peticion_marca RECORD LIKE ret_ws_peticion_marca.* -- registro de peticion al ws
		

   UPDATE ret_ws_cons_marca
   SET    cveInstitutoOrigen           = ws_out.cveInstitutoOrigen,
          cveAfore                     = ws_out.cveAfore,
          nombre                       = ws_out.nombre,
          apellidoPaterno              = ws_out.apellidoPaterno,
          apellidoMaterno              = ws_out.apellidoMaterno,
          rfc                          = ws_out.rfc,
          curpRegistrada               = ws_out.curpRegistrada,
          saldoSar92                   = ws_out.saldoSar92,
          saldoRetiro97                = ws_out.saldoRetiro97,
          saldoCuotaSocial             = ws_out.saldoCuotaSocial,
          saldoCesantiaVejez           = ws_out.saldoCesantiaVejez,
          saldoVivienda97              = ws_out.saldoVivienda97,
          saldoVivienda97AIVS          = ws_out.saldoVivienda97AIVS,
          saldoVivienda92              = ws_out.saldoVivienda92,
          saldoVivienda92AIVS          = ws_out.saldoVivienda92AIVS,
          saldoAhorroRetiroIB          = ws_out.saldoAhorroRetiroIB,
          saldoAportacionesVoluntarias = ws_out.saldoAportacionesVoluntarias,
          saldoRetiro92I               = ws_out.saldoRetiro92I,
          saldoAportaCompRetiro        = ws_out.saldoAportaCompRetiro,
          saldoViviendaFI92            = ws_out.saldoViviendaFI92,
          saldoViviendaFI92AIVS        = ws_out.saldoViviendaFI92AIVS,
          saldoAportaLargoPlazo        = ws_out.saldoAportaLargoPlazo,
          saldoFI08                    = ws_out.saldoFI08,
          saldoFI08AIVS                = ws_out.saldoFI08AIVS,
          saldoRetiroI08               = ws_out.saldoRetiroI08,
          saldoCVI                     = ws_out.saldoCVI,
          saldoAhorroSolidario         = ws_out.saldoAhorroSolidario,
          saldoCuotaSocialI            = ws_out.saldoCuotaSocialI,
          saldoBonoMontoUDIS           = ws_out.saldoBonoMontoUDIS,
          anhoVencimiento              = ws_out.anhoVencimiento,
          estatusCuentaIndividual      = ws_out.estatusCuentaIndividual,
          estatusViviendaI             = ws_out.estatusViviendaI,
          estatusViviendaF             = ws_out.estatusViviendaF,
          diagnostico                  = ws_out.diagnostico,
          descripcionDiagnostico       = ws_out.descripcionDiagnostico,
          folioSolicitud               = ws_out.folioSolicitud,
          resultado                    = ws_out.resultado,
          descripcion                  = ws_out.descripcion 
   WHERE  id_peticion = p_id_peticion;
END FUNCTION
{
======================================================================
Nombre: fn_registra_peticion
Fecha creacion: Febrero 12, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada que se recibieron de
una peticion de WS 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion()
DEFINE p_id_peticion             DECIMAL(9,0)
		
   -- si se trata de nuevo registro
   -- se obtiene el id de peticion nuevo
   SELECT seq_ret_ws_cons_marca.nextVal
   INTO   p_id_peticion
   FROM   systables
   WHERE  tabid = 1

   INSERT INTO ret_ws_cons_marca (id_peticion,
                                  idsistema,
                                  idebusiness,
                                  idportafolio,
                                  idservicio,
                                  idcliente,
                                  idcanal,
                                  codopercliente,
                                  fecha,
                                  curp,
                                  nss,
                                  canal)  
        VALUES (p_id_peticion,
                ws_in.idSistema,
                ws_in.idEbusiness,
                ws_in.idPortafolio,
                ws_in.idServicio,
                ws_in.idCliente,   -- 30 - Consulta, 44 - marca y consulta, 60 - desmarca 
                ws_in.idCanal,
                ws_in.codoperCliente,
                ws_in.fecha,
                ws_in.curp,
                ws_in.nss,
                ws_in.canal);
      
   -- se devuelve el id de la peticion
   RETURN p_id_peticion
END FUNCTION
