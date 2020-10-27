--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW36                                                  #
#OBJETIVO          => WS CONSULTA DE RFC                                      #
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
DEFINE ws_ret_cons_rfc_nombre_in RECORD
         nss              CHAR(11) -- nss del trabajador
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_rfc_nombre_out  RECORD
         nss                  CHAR(11),    -- Número de seguridad social del trabajador
         codigo_retorno       SMALLINT,    -- codigo resultado de la busqueda 0-Busqueda exitosa, 1-Busqueda multiple exitosa, 2-Busqueda no exitosa
         ocurrencias_rfc   DYNAMIC ARRAY OF RECORD
           rfc                CHAR(13),    -- Registro Federal de Contribuyentes
           nombre             CHAR(60),    -- Nombre del derechohabiente
           id_afi_fondo72     DECIMAL(9,0) -- Iidentificador unico 
         END RECORD
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW36."
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
      CALL fn_crea_servicio_consulta_rfc(TRUE)
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
  CALL ERRORLOG("invoca creacion de servicio Consulta")
  CALL fn_crea_servicio_consulta_rfc(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor de Consulta de RFC 1.0 ...")

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
Nombre: fn_crea_servicio_consulta_rfc
Fecha creacion: Julio 23, 2014
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Genera el servicio web de consulta de RFC

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_rfc(p_generar_WSDL)
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
    LET v_webservice = com.WebService.CreateWebService("consultaRFC", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    --LET op = com.WebOperation.CreateRPCStyle("fn_ret_rfc_nombre","fn_ret_rfc_nombre",ws_ret_cons_rfc_nombre_in,ws_ret_cons_rfc_nombre_out)
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_rfc_nombre","fn_ret_rfc_nombre",ws_ret_cons_rfc_nombre_in,ws_ret_cons_rfc_nombre_out)
    --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7783/consultaSaldoRetiroGenericoSubcta12/fn_ret_rfc_nombre") -- se crea con el nombre de operacion fn_ret_rfc_nombre
    CALL v_webservice.publishOperation(op, "fn_ret_rfc_nombre") -- se crea con el nombre de operacion fn_ret_rfc_nombre

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
       --display_status("Consulta de RFC registrado")
       CALL ERRORLOG("Se registro el servicio consulta de RFC")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Consulta de RFC': " || STATUS)
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
Nombre: fn_ret_rfc_nombre
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion del WS que obtiene los saldos por subcuenta de la dupla NSS-RFC
recibida

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_rfc_nombre()
DEFINE v_indice     SMALLINT, -- indice de la subcuenta
       v_nss                LIKE afi_fondo72.nss,
       v_rfc                LIKE afi_fondo72.rfc,
       v_nombre             LIKE afi_fondo72.nombre,
       v_id_afi_fondo72     LIKE afi_fondo72.id_afi_fondo72, -- id del derechohabiente en fondo de ahorro
       v_ocurrencias        DECIMAL (5,0),      -- contine la cantidad de ocurrencias encontradas para el nss dado
       v_sql                STRING -- cadena con una instruccion SQL

   -- se crea la respuesta del servicio
   LET ws_ret_cons_rfc_nombre_out.nss            = ws_ret_cons_rfc_nombre_in.nss
   LET ws_ret_cons_rfc_nombre_out.codigo_retorno = 2 -- Se inicializa como no exitosa

   -- se obtiene NSS para las consultas
   LET v_nss = ws_ret_cons_rfc_nombre_in.nss
   LET v_indice = 1
   -- se obtiene el id_derechohabiente
   SELECT count(*)
   INTO   v_ocurrencias
   FROM   afi_fondo72
   WHERE  nss = ws_ret_cons_rfc_nombre_in.nss

   -- si no se encuentra se devuelve un cero
   IF ( v_ocurrencias IS NULL OR v_ocurrencias = 0 ) THEN
      LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].rfc   = ""
      LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].nombre = ""
      LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].id_afi_fondo72 = 0
   ELSE
      IF v_ocurrencias = 1 THEN 
          LET ws_ret_cons_rfc_nombre_out.codigo_retorno = 0
      ELSE 
          LET ws_ret_cons_rfc_nombre_out.codigo_retorno = 1
      END IF 
         
      -- Se buscan las coincidencias para el nss proporcionado
      DECLARE cur_nss CURSOR FOR
      SELECT id_afi_fondo72, rfc, nombre
      FROM   afi_fondo72
      WHERE  nss = ws_ret_cons_rfc_nombre_in.nss
        
      -- se obtiene el nombre y rfc del NSS dado
      FOREACH cur_nss INTO v_id_afi_fondo72, v_rfc, v_nombre
          LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].rfc            = v_rfc
          LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].nombre         = v_nombre
          LET ws_ret_cons_rfc_nombre_out.ocurrencias_rfc[v_indice].id_afi_fondo72 = v_id_afi_fondo72
          LET v_indice = v_indice + 1
      END FOREACH
   END IF
   
END FUNCTION
