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
#OBJETIVO          => WS CONSULTA DE SALDOS Y CREDITO PARA RETIRO DEL         #
#                     FONDO DE AHORRO                                         #
#FECHA INICIO      => 07-ENERO-2019                                           #
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
         rfc              STRING, -- rfc del trabajador
         medio_entrega    STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
      END RECORD,
       -- registro de respuesta
      ws_ret_consulta_out  RECORD
         nss                STRING, --- Número de seguridad social del trabajador
         rfc                STRING, -- rfc del trabajador
         estado_solicitud   STRING, -- estado de la solicitud
         cod_rechazo        STRING, -- codigo de rechazo
         des_rechazo        STRING    -----  *****************************************
      END RECORD
         
DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado
CONSTANT gi_sin_saldo_con_traspaso   SMALLINT = 1 
CONSTANT gi_sin_saldo_con_devolucion SMALLINT = 2 
CONSTANT gi_sin_saldo_sin_traspaso_sin_devolucion  SMALLINT = 3

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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS23."
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
        CALL fn_crea_servicio_consulta_fondo_ahorro(TRUE)
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
    CALL fn_crea_servicio_consulta_fondo_ahorro(FALSE)

    -- se inicia el servidor
    CALL ERRORLOG("Iniciando servidor de Consulta Fondo de Ahorro ...")

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
Nombre: fn_crea_servicio_consulta_fondo_ahorro
Fecha creacion: Octubre 24, 2018
Autor: Ricardo Pérez
Narrativa del proceso que realiza:
Genera el servicio web de retiro del fondo de ahorro que consulta los saldos disponibles

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_fondo_ahorro(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroSaldoCreditofa", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_saldo_credito_fa","fn_saldo_credito_fa",ws_ret_consulta_in,ws_ret_consulta_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_saldo_credito_fa")
        CALL v_webservice.publishOperation(op, "fn_saldo_credito_fa")

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
            CALL ERRORLOG("Se registro el servicio consulta del Fondo de Ahorro")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Consulta del Fondo de Ahorro': " || STATUS)
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
Nombre: fn_saldo_credito_fa
Fecha creacion: Enero 07, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta el saldo y los créditos para el Fondo de Ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_saldo_credito_fa()
DEFINE v_indice_retiro SMALLINT,
       v_nss           LIKE afi_fondo72.nss,
       v_rfc           LIKE afi_fondo72.rfc,
	   v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
	   v_ruta_log        STRING,
	   v_cadena          STRING

    -- se responde el servicio para pruebas
    LET ws_ret_consulta_out.nss = ws_ret_consulta_in.nss
    LET ws_ret_consulta_out.rfc = ws_ret_consulta_in.rfc


    LET v_nss = ws_ret_consulta_in.nss
    LET v_rfc = ws_ret_consulta_in.rfc

    DISPLAY "Consultando para:"
    DISPLAY "NSS: ", v_nss
    DISPLAY "RFC: ", v_rfc

    -- se obtiene la ruta ejecutable
    SELECT ruta_listados
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS23."
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


   IF ((ws_ret_consulta_in.nss IS NOT NULL) AND (ws_ret_consulta_in.medio_entrega <> 0) ) THEN
      DISPLAY "Validando Fondo de ahorro"
      DISPLAY "Parametros de entrada " 
      DISPLAY "NSS                     : ", v_nss
      DISPLAY "RFC                     : ", v_rfc
      DISPLAY "Medio entrega           : ", ws_ret_consulta_in.medio_entrega
        
      CALL fn_ret_consulta_fondo_ahorro(v_nss, v_rfc,ws_ret_consulta_in.medio_entrega,TRUE)
    
   ELSE 
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_datos_incompletos, 0)
   END IF
   
END FUNCTION


{
======================================================================
Nombre: fn_ret_consulta_fondo_ahorro
Fecha creacion: Enero 07, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene saldo cero en el fondo de ahorro y crédito 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_ret_consulta_fondo_ahorro(p_nss, p_rfc, p_medio_entrega,p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                CHAR(13), -- RFC
       p_causal             SMALLINT, -- causal de retiro
       p_nrp                CHAR(11), -- NRP
       v_f_inicio_pension   CHAR(8), -- fecha de inicio de pension en formato AAAAMMDD
       p_medio_entrega      SMALLINT, -- Medio por el cual se hizo el llamado  
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
       v_tipo_originacion   SMALLINT, 
       v_tipo_cargo         SMALLINT,
       v_fecha              DATE,
       v_movimiento         STRING,
       v_importe            DECIMAL(12,2)

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
  
   DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
   DISPLAY "NSS evaluado: ", p_nss

   LET v_conteo_nss          = 0

   IF ( p_nss IS NOT NULL ) THEN
      CALL ERRORLOG("Validando solicitud para NSS: " || p_nss)
   END IF

   -- se verifica si tiene saldo
   CALL fn_recupera_saldo_fa(p_nss, p_rfc) RETURNING v_saldo

   IF ( v_saldo <= 0 ) THEN

      -- Busca los movimientos en la base histórica para determinar cual fue el movimiento que lo dejo en cero
      CALL fn_busca_mov_cargo_fa(p_nss) RETURNING v_tipo_cargo, v_fecha, v_movimiento, v_importe     -- Si devuelve "D" es una Devlución, si devuelve "T" es traspaso 
      -- se verifica si el derechohabiente tiene un credito vigente
      IF v_tipo_cargo = "T" OR v_tipo_cargo = "D" THEN 
         IF v_tipo_cargo = "T" THEN 
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, gi_sin_saldo_con_traspaso, 0)   --- Se devuelve sin saldo y con credito 1
         END IF 
         IF v_tipo_cargo = "D" THEN 
               CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, gi_sin_saldo_con_devolucion, 0)  -- Se devuelve sin saldo y sin credito 0
         END IF
      ELSE 
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, gi_sin_saldo_sin_traspaso_sin_devolucion, 0)  -- Se devuelve sin saldo y sin credito 0
      END IF
   ELSE 
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, 0)  -- Se devuelve con saldo
   END IF

END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_fondo_ahorro
Fecha creacion: Enero 07, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la consulta para el fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_fondo_ahorro(p_estado_solicitud, p_cod_rechazo, p_importe_viv72)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv72    DECIMAL(12,2), -- Importe de vivienda 72
         v_importe_dap      DECIMAL(12,2), -- importe para validacion de pago por dap o clabe 1000 o 2000
         v_devolver_saldo   SMALLINT, -- booleana que indica si el saldo se debe devolver
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
   IF p_cod_rechazo = 1 AND p_estado_solicitud = gi_solicitud_aceptada THEN
      LET ws_ret_consulta_out.des_rechazo = "Sin saldo y con crédito";
   END IF 
   IF p_cod_rechazo = 2 AND p_estado_solicitud = gi_solicitud_aceptada THEN
      LET ws_ret_consulta_out.des_rechazo = "Sin saldo con devolución";
   END IF 
   IF p_cod_rechazo = 3 AND p_estado_solicitud = gi_solicitud_aceptada THEN
      LET ws_ret_consulta_out.des_rechazo = "Sin saldo, sin crédito y sin devolución ";
      LET p_cod_rechazo = 0  -- Se devuelve cero para no afectar la lógica del Portal y CRM, solo se devuelve la descripción 
   END IF 
   -- se construye la respuesta del ws
   LET ws_ret_consulta_out.estado_solicitud = p_estado_solicitud
   LET ws_ret_consulta_out.cod_rechazo      = p_cod_rechazo
   

END FUNCTION
