--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW33                                                  #
#OBJETIVO          => WS CONSULTA DE SALDOS POR SUBCUENTA PARA RETIRO GENERICO#
#FECHA INICIO      => 01-oct-2013                                             #
###############################################################################

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv   
# 
# USER GLOBALS VARIABLES
#
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_ret_cons_saldos_subcuenta_in RECORD
         nss              CHAR(11), -- nss del trabajador
         rfc              CHAR(13)  -- rfc del trabajador
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_saldos_subcuenta_out  RECORD
         nss                 CHAR(11), --- Número de seguridad social del trabajador
         rfc                 CHAR(13), -- rfc del trabajador
         saldo_x_subcuenta   DYNAMIC ARRAY OF RECORD
           subcuenta          SMALLINT, -- subcuenta de inversion
           f_valuacion        CHAR(8), -- fecha de valuacion de AIVs en formato AAAAMMDD
           monto_aivs         DECIMAL(22,6), -- saldo en AIVs
           monto_pesos        DECIMAL(22,2) -- saldo en pesos equivalente a AIVs por valor accion
         END RECORD
       END RECORD
         
DEFINE g_indice_retiro      SMALLINT, -- indice del tipo de retiro consultado
       g_id_derechohabiente DECIMAL(9,0) ,
       g_id_fondo72         DECIMAL(9,0) ,
       g_causal_ref         SMALLINT     ,
       g_nss                CHAR(11)     ,
       g_rfc                CHAR(13)     , -- rfc del trabajador
       g_acc_acciones       DECIMAL(14,6),
       g_acc_pesos          DECIMAL(14,6),
       g_tanto_adicional    DECIMAL(14,6),
       g_id_solicitud       DECIMAL(9,0) ,
       g_refer              CHAR(18)     ,
       g_id_beneficiario              SMALLINT     , -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18)     , -- Nombre del beneficiario 
       g_ape_pat            CHAR(18)     , -- Apellido paterno 
       g_ape_mat            CHAR(18)     , -- Apellido materno           
       g_causal_adai        SMALLINT     , -- Clave de Adai 
       g_entidad            SMALLINT     , -- Entidad federativa
       g_id_datamart        DECIMAL(9,0) , -- Identificador datamart
       g_causal_retiro      SMALLINT     ,
       g_bnd_uso_seq        SMALLINT     ,
       g_sq_ret_solicitud   DECIMAL(9,0) -- id de solicitud nueva 

DEFINE g_r_tmp_id_fondo72   RECORD
        nss                  CHAR(11)     ,
        id_derechohabiente   DECIMAL(9,0) ,
        id_afi_fondo72       DECIMAL(9,0) ,
        importe              DECIMAL(12,2),
        rfc                  CHAR(13)     ,
        estatus              SMALLINT     ,
        rechazo_cod          SMALLINT
       END RECORD

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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW33."
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
      CALL fn_crea_servicio_consulta_saldo_retiro_generico(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET v_pantalla = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE330" ATTRIBUTES(TEXT = "Consulta de saldos por subcuenta")
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
  CALL fn_crea_servicio_consulta_saldo_retiro_generico(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor...")

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
Nombre: fn_crea_servicio_consulta_saldo_retiro_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_saldo_retiro_generico(p_generar_WSDL)
  DEFINE v_webservice         com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace = "http://localhost/"
  LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

  TRY
    -- =============================
    -- se crea el servicio
    LET v_webservice = com.WebService.CreateWebService("consultaSaldoRetiroGenerico", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    --LET op = com.WebOperation.CreateRPCStyle("fn_ret_saldos_subcuenta","fn_ret_saldos_subcuenta",ws_ret_cons_saldos_subcuenta_in,ws_ret_cons_saldos_subcuenta_out)
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_saldos_subcuenta","fn_ret_saldos_subcuenta",ws_ret_cons_saldos_subcuenta_in,ws_ret_cons_saldos_subcuenta_out)
    --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7780/consultaSaldoRetiroGenerico/fn_ret_saldos_subcuenta") -- se crea con el nombre de operacion fn_ret_saldos_subcuenta
    CALL v_webservice.publishOperation(op, "fn_ret_saldos_subcuenta") -- se crea con el nombre de operacion fn_ret_saldos_subcuenta

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
       CALL ERRORLOG("Se registro el servicio consulta de saldos disponibles para retiro")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Consulta de saldos disponibles para retiro': " || STATUS)
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
Nombre: fn_ret_saldos_subcuenta
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion del WS que obtiene los saldos por subcuenta de la dupla NSS-RFC
recibida

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_saldos_subcuenta()
DEFINE v_indice     SMALLINT, -- indice de la subcuenta
       v_nss        LIKE afi_fondo72.nss,
       v_rfc        LIKE afi_fondo72.rfc,
       v_subcuenta  LIKE cat_subcuenta.subcuenta,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
       v_aivs               DECIMAL(24,6), -- AIVs
       v_pesos              DECIMAL(22,2), -- pesos
       v_tabla_consulta     VARCHAR(40), -- tabla de donde se consultara el saldo
       v_f_saldo            DATE, -- fecha de calculo del saldo
       v_sql                STRING, -- cadena con una instruccion SQL
       v_id_afi_fondo72     LIKE afi_fondo72.id_afi_fondo72 -- id del derechohabiente en fondo de ahorro

   -- se crea la respuesta del servicio
   LET ws_ret_cons_saldos_subcuenta_out.nss = ws_ret_cons_saldos_subcuenta_in.nss
   LET ws_ret_cons_saldos_subcuenta_out.rfc = ws_ret_cons_saldos_subcuenta_in.rfc

   -- se obtiene NSS y RFC para las consultas
   LET v_nss = ws_ret_cons_saldos_subcuenta_in.nss
   LET v_rfc = ws_ret_cons_saldos_subcuenta_in.rfc
   
   -- se obtiene el id_derechohabiente
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss               = ws_ret_cons_saldos_subcuenta_in.nss
   AND    ind_estado_cuenta = 0   -- Cuenta Activa

   SELECT MAX(id_afi_fondo72)
   INTO   v_id_afi_fondo72
   FROM   afi_fondo72
   WHERE  nss               = ws_ret_cons_saldos_subcuenta_in.nss
   AND    ind_estado_cuenta = 0   --   Cuenta Activa
--   AND    rfc = ws_ret_cons_saldos_subcuenta_in.rfc
   
   -- si no se encuentra se devuelve un cero
   IF ( v_id_derechohabiente IS NULL AND v_id_afi_fondo72 IS NULL ) THEN
      LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].subcuenta   = 0
      LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].f_valuacion = TODAY USING "yyyymmdd"
      LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].monto_aivs  = 0
      LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].monto_pesos = 0
   ELSE
      -- se verifica de que tabla se debe obtener el saldo
      LET v_sql = "\nSELECT tabla_saldo",
                  "\nFROM   safre_sdo@vivws_tcp:glo_saldo",
                  "\nWHERE  ind_saldo = 1" -- la tabla activa
      
      PREPARE sid_tabla FROM v_sql
      EXECUTE sid_tabla INTO v_tabla_consulta
      
      -- si no se encontro la tabla, se devuelve cero
      IF ( v_tabla_consulta IS NULL ) THEN
         LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].subcuenta   = 0
         LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].f_valuacion = TODAY USING "yyyymmdd"
         LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].monto_aivs  = 0
         LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[1].monto_pesos = 0
      ELSE
         -- se responde el saldo por cada subcuenta
         LET v_indice = 1

         -- FONDO DE AHORRO. Se verifica si se envio RFC
--         IF ( v_rfc IS NOT NULL ) THEN
            SELECT SUM(a.importe)
            INTO   v_pesos
            FROM   cta_fondo72 a,
                   afi_fondo72 b
            WHERE  b.nss               = v_nss
            AND    b.ind_estado_cuenta = 0  -- Cuenta Activa
--            AND    b.rfc = v_rfc
            AND    b.id_afi_fondo72 = a.id_afi_fondo72

            -- si se encontro
            IF ( v_pesos IS NOT NULL ) THEN
               -- si la cuenta esta sobregirada, el saldo se deja en cero
               IF ( v_pesos < 0 ) THEN
                  LET v_pesos = 0
               END IF

               -- se escribe el registro al arreglo de salida
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].subcuenta   = 40 -- fondo de ahorro
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].f_valuacion = TODAY USING "yyyymmdd"
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].monto_aivs  = 0 -- fondo de ahorro no tiene AIVs
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].monto_pesos = v_pesos
               
               LET v_indice = v_indice + 1

            END IF
--         END IF -- RFC no nulo para buscar fondo de ahorro
         
         -- del listado de subcuentas se busca el saldo. Fondo de Ahorro y Decreto se buscan por separado
         DECLARE cur_subcuenta CURSOR FOR
         SELECT  subcuenta
         FROM    cat_subcuenta
         WHERE   subcuenta NOT IN (40, 48) -- FONDO DE AHORRO Y DECRETO
         ORDER BY subcuenta

         -- se obtiene el saldo de la subcuenta para el id_derechohabiente encontrado
         LET v_sql = "\n SELECT monto_acciones,",
                     "\n        monto_pesos   ,",
                     "\n        f_saldo        ",
                     "\n FROM   safre_sdo@vivws_tcp:", v_tabla_consulta,
                     "\n WHERE  id_derechohabiente = ?",
                     "\n AND    subcuenta = ? ",
                     "\n AND    fondo_inversion <> 0 "
                     
         DISPLAY v_sql
                     
         -- se prepara y ejecuta la consulta
         PREPARE sid_consultasaldo FROM v_sql
         
         -- se obtiene el saldo de cada subcuenta del NSS/RFC dados
         FOREACH cur_subcuenta INTO v_subcuenta
         
            LET v_aivs = NULL
            LET v_pesos = NULL
            LET v_f_saldo = NULL

            EXECUTE sid_consultasaldo USING v_id_derechohabiente, v_subcuenta
                                      INTO v_aivs, v_pesos, v_f_saldo
            
            DISPLAY "Subcuenta: ", v_subcuenta
            DISPLAY "AIVS: ", v_aivs
            DISPLAY "PESOS: ", v_pesos
            
            -- si se encontraron datos 
            IF ( SQLCA.SQLCODE = 0 ) THEN
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].subcuenta   = v_subcuenta
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].f_valuacion = v_f_saldo USING "yyyymmdd"
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].monto_aivs  = v_aivs
               LET ws_ret_cons_saldos_subcuenta_out.saldo_x_subcuenta[v_indice].monto_pesos = v_pesos
               
               LET v_indice = v_indice + 1
            END IF
         END FOREACH
      END IF        
   END IF
   
END FUNCTION
