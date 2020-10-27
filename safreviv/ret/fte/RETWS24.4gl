--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS23                                                 #
#OBJETIVO          => WS PARA REGISTRO DE PETICIONES DE GENERACIÓN DE CARTA   #
#                     DE NEGATIVA DEL FONDO DE AHORRO                         #
#FECHA INICIO      => 12-MARZO-2019                                           #
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
DEFINE ws_ret_consulta_in RECORD
         nss              STRING, -- nss del trabajador
         caso_crm         STRING, -- caso generado por CRM
         tipo_carta       STRING, -- tipo de carta a generar Saldo cero por Traspaso o Saldo cero por Devolución
         medio_entrega    STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
      END RECORD,
       -- registro de respuesta
      ws_ret_consulta_out  RECORD
         nss                STRING, --- Número de seguridad social del trabajador
         caso_crm           STRING, -- caso generado por CRM
         id_peticion        STRING, -- Número generado para la solicitud de generación de carta
         estado_solicitud   STRING, -- estado de la solicitud
         cod_rechazo        STRING, -- codigo de rechazo
         des_rechazo        STRING    -----  *****************************************
      END RECORD
         
DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado
CONSTANT gi_sin_saldo_con_credito SMALLINT = 1 
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS24."
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
        CALL fn_crea_servicio_solicitudes_negativa_fondo_ahorro(TRUE)
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
    CALL fn_crea_servicio_solicitudes_negativa_fondo_ahorro(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Solicitudes de Negativa del Fondo de Ahorro ...")

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
Nombre: fn_crea_servicio_solicitudes_negativa_fondo_ahorro
Fecha creacion: Marzo 12, 2019
Autor: Ricardo Pérez
Narrativa del proceso que realiza:
Servicio para registrar las peticiones de generación de carta de negativa del Fondo de Ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_solicitudes_negativa_fondo_ahorro(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroPeticionCartaNegativaFA", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_peticion_carta_negativa_fa","fn_peticion_carta_negativa_fa",ws_ret_consulta_in,ws_ret_consulta_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_peticion_carta_negativa_fa")
        CALL v_webservice.publishOperation(op, "fn_peticion_carta_negativa_fa")

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
            CALL ERRORLOG("Se registro el servicio de peticion de generación de carta de negativa del Fondo de Ahorro")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'peticion de generación de carta de negativa del Fondo de Ahorro': " || STATUS)
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
Nombre: fn_peticion_carta_negativa_fa
Fecha creacion: Marzo 12, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que Atiende las peticiones de generación de las cartas de negativa del Fondo de Ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_peticion_carta_negativa_fa()
DEFINE v_indice_retiro  SMALLINT,
       v_nss            CHAR(11),
       v_caso_crm       CHAR(10),
	   v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	   v_ruta_log        STRING,
	   v_cadena          STRING

    -- se responde el servicio para pruebas
    LET ws_ret_consulta_out.nss = ws_ret_consulta_in.nss
    LET ws_ret_consulta_out.caso_crm = ws_ret_consulta_in.caso_crm

    LET v_nss      = ws_ret_consulta_in.nss
    LET v_caso_crm = ws_ret_consulta_in.caso_crm

    DISPLAY CURRENT YEAR TO SECOND 
    DISPLAY "Procesando para:"
    DISPLAY "NSS     : ", v_nss
    DISPLAY "Caso CRM: ", v_caso_crm

    -- se obtiene la ruta ejecutable
    SELECT ruta_listados
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS24."
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

   IF ws_ret_consulta_in.nss IS NULL AND
      ws_ret_consulta_in.caso_crm IS NULL AND 
      ws_ret_consulta_in.medio_entrega IS NULL THEN 
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_datos_incompletos, 0)
      DISPLAY CURRENT YEAR TO SECOND 
   ELSE 
      DISPLAY "Registrando Petición de Generación de Carta de Negativa del Fondo de ahorro"
      DISPLAY "Parametros de entrada " 
      DISPLAY "NSS                     : ", v_nss
      DISPLAY "CASO CRM                : ", v_caso_crm
      DISPLAY "Tipo Carta              : ", ws_ret_consulta_in.tipo_carta
      DISPLAY "Medio entrega           : ", ws_ret_consulta_in.medio_entrega
        
      CALL fn_registra_petcion_fondo_ahorro(v_nss, v_caso_crm, ws_ret_consulta_in.tipo_carta, ws_ret_consulta_in.medio_entrega)
   END IF
   
END FUNCTION


{
======================================================================
Nombre: fn_registra_petcion_fondo_ahorro
Fecha creacion: Marzo 12, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Registra la solicitud de generación de carta de negativa del fondo de ahorro 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_registra_petcion_fondo_ahorro(p_nss, p_caso_crm, p_tipo_carta, p_medio_entrega)
DEFINE p_nss                CHAR(11),      -- NSS
       p_caso_crm           CHAR(10),      -- Caso generado por CRM
       p_tipo_carta         CHAR(1),       -- Tipo de carta a generar
       p_medio_entrega      SMALLINT,      -- Medio por el cual se hizo el llamado  
       v_id_peticion        DECIMAL(9,0),  -- Consecutivo generado por solicitud
       v_conteo_nss         SMALLINT,      -- Contador para saber si un nss está más de una vez en el catálogo
       v_id_afi_fondo72     DECIMAL(9,0)   -- id derechohabiente en fondo72

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

   END IF
  
   LET v_conteo_nss          = 0
   -- Inserta en la tabla de peticiones
   -- Obtiene el id_afi_fondo72
   SELECT id_afi_fondo72
   INTO   v_id_afi_fondo72
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    ind_estado_cuenta = 0  -- cuenta Activa

   -- Genera el id peticion
   SELECT seq_ret_sol_negativas_fondo_ahorro.nextval
   INTO   v_id_peticion
   FROM   systables
   WHERE  tabid = 1;
   
   INSERT INTO ret_sol_negativas_fondo_ahorro 
        VALUES (v_id_peticion, v_id_afi_fondo72, 10, TODAY, NULL, NULL, p_nss, p_caso_crm, p_tipo_carta, p_medio_entrega);

   CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, v_id_peticion)  -- Se devuelve con saldo     

END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_fondo_ahorro
Fecha creacion: Marzo 12, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la Petición de generación de la carta de negativa del fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_fondo_ahorro(p_estado_solicitud, p_cod_rechazo, p_id_peticion)
DEFINE   p_estado_solicitud SMALLINT,     -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT,     -- Codigo de rechazo 
         p_id_peticion      DECIMAL(9,0), -- Consecutivo generado para la solicitud
         v_desc_rechazo     CHAR(100)
         
   LET ws_ret_consulta_out.des_rechazo = " ";
   LET v_desc_rechazo = ""
   IF p_cod_rechazo <> 0 AND p_estado_solicitud = gi_solicitud_rechazada THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET ws_ret_consulta_out.des_rechazo = "";
      SELECT des_larga
      INTO   v_desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      IF v_desc_rechazo IS NULL THEN
         LET ws_ret_consulta_out.des_rechazo = " ";
      ELSE  
         LET ws_ret_consulta_out.des_rechazo = v_desc_rechazo
      END IF
   END IF 
   -- se construye la respuesta del ws
   LET ws_ret_consulta_out.estado_solicitud = p_estado_solicitud
   LET ws_ret_consulta_out.cod_rechazo      = p_cod_rechazo
   LET ws_ret_consulta_out.id_peticion      = p_id_peticion

END FUNCTION
