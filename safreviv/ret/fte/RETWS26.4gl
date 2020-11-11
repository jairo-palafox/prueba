--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS26                                                 #
#OBJETIVO          => WS CONSULTA DE PDF DE ACUSES DE SOLICITUDES TRAMITADAS  #
#                     POR MI CUENTA INFONAVIT, DEVOLUCION AUTOMATICA DE       #
#                     AMORTIZACIONES EXCEDENTES                               #  
#FECHA INICIO      => Enero 21, 2020                                          #
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
DEFINE ws_pdf_acuse_in RECORD
         nss              CHAR(11),     -- nss del trabajador
         casoCRM          CHAR(10)      -- Caso asignado por CRM
       END RECORD,
       -- registro de respuesta
       ws_pdf_acuse_out  RECORD
         nss                 CHAR(11),     -- nss del trabajador
         rfc                 CHAR(13),     -- RFC del trabajador
--         tramite             CHAR(50),     -- Descripción del Trámite
--         apePaterno          CHAR(40),     -- Apellido paterno
--         apeMaterno          CHAR(40),     -- Apellido materno
--         nombre              CHAR(40),     -- Nombre
--         medioSolicitud      CHAR(10),     -- Medio por el cual se hizo la solicitud 
--         cuentaCLABE         CHAR(18),     -- Cuenta CLABE
--         pesos               CHAR(18),     -- Monto a devolver
--         fTramite            CHAR(16),     -- Fecha y hora de la Solicitud
--         sello               CHAR(64),     -- Acuse generado para el trámite
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
DEFINE g_identificador_servicio              SMALLINT
DEFINE g_eventoId                            CHAR(100)  
DEFINE g_sesionId                            CHAR(100)

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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS26."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET g_sesionId = v_ruta_log || v_cadena
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"

    LET g_identificador_servicio = 4
    LET g_eventoId = 'retiroConsultaPDFAcuseAE'
  
  
    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    LET v_pantalla = FALSE
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_consulta_pdf_acuse_ae(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Consulta PDF Acuse AE service") --, STYLE="naked")
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
    CALL fn_crea_servicio_consulta_pdf_acuse_ae(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta del PDF Acuse AE 1.0 ...")

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
Nombre: fn_crea_servicio_consulta_pdf_acuse_ae
Fecha creacion: Enero 21, 2020
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Genera el servicio web de consulta de acuses de solicitudes tramitadas 
por Mi cuenta Infonavit, Devolución Automática de Amortizaciones Excedentes a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_pdf_acuse_ae(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroConsultaPDFAcuseAE", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_ret_consulta_pdf_acuse_ae","fn_ret_consulta_pdf_acuse_ae",ws_pdf_acuse_in,ws_pdf_acuse_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_ret_consulta_pdf_acuse_ae")

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
            CALL ERRORLOG("Se registro el servicio consulta del PDF acuse para retiro AE")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta del PDF acuse para retiro AE': " || STATUS)
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
Nombre: fn_ret_consulta_pdf_acuse_ae
Fecha creacion: Enero 21, 2020
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta el PDF de acuse de una solicitude tramitada por 
Mi cuenta Infonavit Devolución automática de Amortizaciones Excedentes a través de la FIEL

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_consulta_pdf_acuse_ae()
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
           rfc                LIKE afi_derechohabiente.rfc,
           tramite            CHAR(50),     -- Descripción del Trámite
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           cuentaCLABE        LIKE ret_pago_spei.cuenta_clabe,
           pesos              CHAR(18), -- total a pagar
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- Código de rechazo
           caso_crm           CHAR(10),       -- Caso CRM
           id_solicitud       LIKE ret_solicitud_generico.id_solicitud
END RECORD 
   -- se responde el servicio para pruebas
   INITIALIZE arr_detalle TO NULL  
   LET v_indice             = 1
   LET v_monto_paso         = 0
   LET v_monto_tot_paso     = 0
   
   LET arr_detalle.nss            = ws_pdf_acuse_in.nss
   LET arr_detalle.caso_crm       = ws_pdf_acuse_in.casoCRM
   LET arr_detalle.tramite        = 'Devolución del saldo de la subcuenta de vivienda'
   LET arr_detalle.medioSolicitud = 'Internet'
   LET arr_detalle.estadoConsulta = gi_solicitud_aceptada
   LET arr_detalle.codRechazo     = 0

   DISPLAY "Parámetros recibidos:"
   DISPLAY "NSS             : ", arr_detalle.nss
   DISPLAY "Caso CRM        : ", arr_detalle.caso_crm 
   LET ws_pdf_acuse_out.nss = arr_detalle.nss

   -- se obtiene la ruta ejecutable
   SELECT ruta_listados
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS26."
   LET v_cadena   = "pdf_acuses"
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)


   --- Se validan los parámetros de entrada
   IF arr_detalle.nss IS NULL OR arr_detalle.caso_crm IS NULL THEN 
      DISPLAY "Debe enviar ambos parámetros para realizar la consulta"
      LET ws_pdf_acuse_out.estadoConsulta = gi_solicitud_rechazada
      LET ws_pdf_acuse_out.codRechazo = gi_datos_incompletos
   ELSE 
      -- se obtienen el nombre del trabajador
      SELECT nombre_af, 
             ap_paterno_af, 
             ap_materno_af,
             rfc
      INTO   arr_detalle.nombre,
             arr_detalle.apePaterno,
             arr_detalle.apeMaterno,
             arr_detalle.rfc
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
                       "\n AND    b.medio_entrega = 2                    ",
                       "\n AND    b.sello         IS NOT NULL            ",
                       "\n AND    b.f_registro    IS NOT NULL            ",
                       "\n AND    a.caso_adai  = ", arr_detalle.caso_crm

      DISPLAY "La consulta es:", v_consulta
      PREPARE exe_consulta_detalle FROM v_consulta
      DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

      FOREACH cur_consulta_detalle INTO arr_detalle.id_solicitud,
                                        arr_detalle.fTramite,
                                        arr_detalle.sello,
                                        v_id_derechohabiente,
                                        arr_detalle.caso_crm
         CALL fn_busca_movtos(v_id_derechohabiente, arr_detalle.id_solicitud, 46) RETURNING v_monto_paso
         LET  v_monto_tot_paso = v_monto_paso
         DISPLAY "Monto Amortizaciones :", v_monto_paso
         LET arr_detalle.pesos = v_monto_paso USING "$$,$$$,$$&.&&"
         DISPLAY "Formateado :",arr_detalle.pesos
         LET v_indice = v_indice + 1
      END FOREACH 
      IF v_indice = 1 THEN 
--         LET ws_pdf_acuse_out.apeMaterno     = ""
--         LET ws_pdf_acuse_out.apePaterno     = ""
         LET ws_pdf_acuse_out.rfc            = ""
--         LET ws_pdf_acuse_out.cuentaCLABE    = ""
--         LET ws_pdf_acuse_out.medioSolicitud = ""
--         LET ws_pdf_acuse_out.nombre         = ""
--         LET ws_pdf_acuse_out.tramite        = ""
         LET ws_pdf_acuse_out.estadoConsulta = gi_solicitud_rechazada
         LET ws_pdf_acuse_out.codRechazo     = gi_no_existe_solicitud
      ELSE 
         CALL fn_obtiene_cuenta_clabe(arr_detalle.id_solicitud) RETURNING arr_detalle.cuentaCLABE
--         IF arr_detalle.cuentaCLABE IS NOT NULL THEN
--            LET ws_pdf_acuse_out.cuentaCLABE = arr_detalle.cuentaCLABE
--         ELSE 
--            LET ws_pdf_acuse_out.cuentaCLABE = ''
--         END IF 
         CALL fn_genera_reporte(arr_detalle.*)
         CALL fn_load_pdf(v_ruta_pdf)--Se crea, se envia y se borra el reporte.pdf
--         LET ws_pdf_acuse_out.apeMaterno     = arr_detalle.apeMaterno CLIPPED 
--         LET ws_pdf_acuse_out.apePaterno     = arr_detalle.apePaterno CLIPPED 
         LET ws_pdf_acuse_out.codRechazo     = arr_detalle.codRechazo
         LET ws_pdf_acuse_out.rfc            = arr_detalle.rfc
         LET ws_pdf_acuse_out.estadoConsulta = arr_detalle.estadoConsulta
         LET v_f_sol_paso                    = arr_detalle.fTramite
--         LET ws_pdf_acuse_out.fTramite       = v_f_sol_paso[9,10],"/",v_f_sol_paso[6,7],"/",v_f_sol_paso[1,4], " ", v_f_sol_paso[12,16] 
--         LET ws_pdf_acuse_out.medioSolicitud = arr_detalle.medioSolicitud
--         LET ws_pdf_acuse_out.nombre         = arr_detalle.nombre CLIPPED 
--         LET ws_pdf_acuse_out.pesos          = arr_detalle.pesos CLIPPED 
--         LET ws_pdf_acuse_out.sello          = arr_detalle.sello CLIPPED 
--         LET ws_pdf_acuse_out.tramite        = ''
      END IF 
   END IF 
   DISPLAY "GENERA BITACORA"
   CALL fn_registro_bitacora()

END FUNCTION

----- 

FUNCTION fn_busca_movtos(p_id_derechohabiente, p_conRetiro, p_subcuenta)
   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE p_conRetiro          DECIMAL(10,0)
   DEFINE p_subcuenta          SMALLINT 
   DEFINE v_monto              DECIMAL(12,2)

   LET v_monto = 0

   IF p_subcuenta = 46 THEN 
      SELECT ABS(total_importe)
      INTO   v_monto
      FROM   ret_amort_excedente
      WHERE  id_solicitud = p_conRetiro
   END IF 
   
   IF v_monto IS NULL THEN 
      LET v_monto = 0
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
           rfc                LIKE afi_derechohabiente.rfc,
           tramite            CHAR(50),     -- Descripción del Trámite
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           cuentaCLABE        LIKE ret_pago_spei.cuenta_clabe,
           pesos              CHAR(18), -- Monto amortizaciones excedentes
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- Código de rechazo
           caso_crm           CHAR(10),   -- Caso CRM
           id_solicitud       LIKE ret_solicitud_generico.id_solicitud
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
    DEFINE v_archivo        STRING
    DEFINE v_cadena         STRING
    DEFINE v_sello_funcionario STRING
    DEFINE v_error           STRING
    DEFINE v_result          SMALLINT
    DEFINE v_existe_aviso    SMALLINT
    DEFINE v_sql             STRING


   LET v_reporte= "RETWS26.4rp"
   LET v_aviso = NULL
   LET v_archivo = NULL

    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED
    LET v_archivo =  p_arr_detalle.nss CLIPPED,"_", 
                     p_arr_detalle.rfc CLIPPED,"_",
                     p_arr_detalle.id_solicitud USING "&&&&&&&&&&","_"
                     ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                     ||DAY(TODAY) CLIPPED,".pdf" 
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" , v_archivo CLIPPED 
    
    LET v_ruta_pdf    = v_ruta_reporte
    LET v_archivo_pdf = v_archivo
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   -- Busca si hay aviso que publicar en el acuse
   LET v_existe_aviso = 0
   SELECT COUNT(*)
   INTO   v_existe_aviso
   FROM   ret_sol_aviso_pdf
   WHERE  id_solicitud = p_arr_detalle.id_solicitud
   IF v_existe_aviso = 0 THEN 
      SELECT aviso
      INTO   v_aviso
      FROM   ret_aviso_pdf_ssv
      WHERE  f_vig_inicio <= TODAY 
      AND    f_vig_final  >= TODAY
   
      IF v_aviso IS NOT NULL THEN  --- Relaciona el mensaje con la solicitud
         LET v_sql = "INSERT INTO ret_sol_aviso_pdf VALUES (", p_arr_detalle.id_solicitud, ",'", v_aviso CLIPPED, "')"
         PREPARE stm_inserta_aviso FROM  v_sql
         EXECUTE stm_inserta_aviso 
      END IF
   ELSE 
      SELECT aviso
      INTO   v_aviso
      FROM   ret_sol_aviso_pdf
      WHERE  id_solicitud = p_arr_detalle.id_solicitud
   END IF 

   -- Se obtiene el certificado del funcionario

   SELECT a.rfc
   INTO   v_rfc
   FROM   ret_rfc_firma_pdf a
   WHERE  a.id_firma = 2;   --- Funcionario de Cartera

   LET v_cadena = p_arr_detalle.id_solicitud USING "##########", p_arr_detalle.nss, 
                        p_arr_detalle.rfc, p_arr_detalle.fTramite, p_arr_detalle.apePaterno CLIPPED, p_arr_detalle.apeMaterno CLIPPED, 
                        p_arr_detalle.nombre CLIPPED, p_arr_detalle.pesos USING "<,<<<,<<&.&&"
   
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      CALL fn_obtiene_certificado(v_cadena CLIPPED, v_rfc CLIPPED )  RETURNING v_result, v_error, v_sello_funcionario 
      DISPLAY "NSS reporte ", p_arr_detalle.nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_arr_detalle.nss, p_arr_detalle.rfc, p_arr_detalle.apePaterno, p_arr_detalle.apeMaterno, 
                                       p_arr_detalle.nombre,p_arr_detalle.fTramite,
                                       p_arr_detalle.id_solicitud,p_arr_detalle.pesos,v_sello_funcionario, v_aviso,
                                       p_arr_detalle.caso_crm,p_arr_detalle.cuentaCLABE)
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
    DEFINE p_pesos           CHAR(18) -- Vivienda 92
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
      LET v_pesos = p_pesos --USING "$$$,$$$,$$&.&&"
      PRINTX v_pesos
      PRINTX p_sello
      PRINTX p_aviso

END REPORT

FUNCTION fn_obtiene_cuenta_clabe(p_id_solicitud)
DEFINE p_id_solicitud  LIKE ret_solicitud_generico.id_solicitud
DEFINE v_cuenta_clabe char(18)

    SELECT cuenta_clabe
    INTO   v_cuenta_clabe
    FROM   ret_pago_spei
    WHERE  id_solicitud = p_id_solicitud
    AND    consec_beneficiario = 1


    RETURN v_cuenta_clabe
    
END FUNCTION

FUNCTION fn_registro_bitacora()
DEFINE v_resultado         SMALLINT
{DEFINE v_array_eventos     DYNAMIC ARRAY OF RECORD
        eventoId           CHAR(100),
        timestamp          CHAR(50)
        END RECORD}
DEFINE IdentificadorId            CHAR(50)


  -- Se registra notificacion
  -- TITULO
  {LET v_array_eventos[1].eventoId =  g_eventoId
  LET v_array_eventos[1].timestamp = fn_obt_formato_timestamp()}
  -- DETALLE
  LET IdentificadorId =  ws_pdf_acuse_in.nss CLIPPED, 
                         ws_pdf_acuse_in.casoCRM CLIPPED
  --LET v_array_eventos[2].timestamp = fn_obt_formato_timestamp()

  CALL fn_registra_bitacora_ws(g_identificador_servicio,g_sesionId,IdentificadorId) RETURNING v_resultado

END FUNCTION
