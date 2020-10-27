--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWSCLABE                                              #
#OBJETIVO          => VALIDA SI UNA CUENTA CLABE ES CORRECTA                  #
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
         clabe            CHAR(18)      -- Cuenta CLABE a validar
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_tramites_out  RECORD
         resultado           CHAR(11)
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWSCLABE."
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
        CALL fn_crea_servicio_valida_cta_clabe(TRUE)
        EXIT PROGRAM
    ELSE 
        IF num_args() = 2 AND arg_val(1) = "-S" THEN
            LET v_pantalla = TRUE
            CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
            CLOSE WINDOW SCREEN

            -- se abre la ventana monitor del servidor (en consola)
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro Valida Cuenta Clabe service") --, STYLE="naked")
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
    CALL fn_crea_servicio_valida_cta_clabe(FALSE)

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
Nombre: fn_crea_servicio_valida_cta_clabe
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_valida_cta_clabe(p_generar_WSDL)
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
        LET v_webservice = com.WebService.CreateWebService("retiroValidaCuentaClabe", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_valida_cuenta_clabe","fn_valida_cuenta_clabe",ws_ret_cons_tramites_in,ws_ret_cons_tramites_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_ret_saldos_disponibles")
        CALL v_webservice.publishOperation(op, "fn_valida_cuenta_clabe")

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
            CALL ERRORLOG("Se registro el servicio valida cuenta clabe")
        END IF
    
        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Valida Cuenta CLABE': " || STATUS)
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
Nombre: fn_valida_cuenta_clabe
Fecha creacion: Noviembre 29, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los trámites realizados por la tableta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_cuenta_clabe()
DEFINE v_indice_retiro SMALLINT,
      v_clabe           CHAR(18),
      v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
      v_ruta_log        STRING,
      v_cadena          STRING,
      v_consulta        STRING,
      v_indice          INTEGER,
      v_clabe_correcta  SMALLINT 


   -- se responde el servicio para pruebas
   LET v_clabe   = ws_ret_cons_tramites_in.clabe
   
   DISPLAY "Parámetros recibidos:"
   DISPLAY "Cuenta CLABE    : ", v_clabe

   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWSCLABE."
   LET v_cadena   = "detalle"
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)


   --- Se validan los parámetros de entrada
   IF v_clabe IS NULL THEN 
      DISPLAY "Debe enviar la cuenta CLABE a validar"
      LET ws_ret_cons_tramites_out.resultado = "INVALIDA"
   ELSE 
   DISPLAY "Manda llamar la funcion de validacion con la CLABE :",v_clabe  
      CALL fn_verifica_clabe_algoritmo(v_clabe) RETURNING v_clabe_correcta
      IF v_clabe_correcta = TRUE THEN 
         LET ws_ret_cons_tramites_out.resultado = "CORRECTA"
      ELSE 
         LET ws_ret_cons_tramites_out.resultado = "INCORRECTA"
      END IF 
   END IF 

END FUNCTION

{
======================================================================
Nombre: fn_verifica_clabe_algoritmo
Fecha creacion: Abril 7, 2018
Autor: Ricardo Pérez
Narrativa del proceso que realiza:
Valida si una cuenta clabe cumple con el algoritmo del calculo del digito verificador

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_clabe_algoritmo(p_clabe)
DEFINE p_clabe          CHAR(18),
       v_clabe_digitos  CHAR(18),
       v_indice         SMALLINT,
       i                SMALLINT, 
       v_clabe_correcta SMALLINT, -- booleana que indica si la cuenta clabe es correcta
       v_caracter       CHAR(1),
       v_suma           SMALLINT,
       v_banco          SMALLINT,
       v_ocurre         SMALLINT,
       v_suma_c         CHAR(2),
       v_multiplo       SMALLINT,
       v_digito         SMALLINT 
       
   -- se asume que es correcta
   LET v_clabe_correcta = TRUE
   LET v_ocurre = 0
   LET v_digito = 0
   LET i = 0
   LET v_suma = 0
   LET v_suma_c = "00"

   DISPLAY "En la función de validacion CLABE :", p_clabe
   --- Valida el Banco
   LET v_banco = p_clabe[1,3]
   SELECT COUNT(*)
   INTO   v_ocurre
   FROM   cat_entidad_financiera
   WHERE  cve_ent_financiera = v_banco

   DISPLAY "Entro el banco ", v_banco, " - ", v_ocurre
   IF v_ocurre > 0 THEN 
      LET v_suma = 0
      FOR i = 1 TO 17
         LET v_multiplo = 1
         IF i = 1 OR i = 4 OR i = 7 OR i = 10 OR i = 13 OR i = 16 THEN
            LET v_multiplo = 3
         END IF
         IF i = 2 OR i = 5 OR i = 8 OR i = 11 OR i = 14 OR i = 17 THEN
            LET v_multiplo = 7
         END IF
         LET v_suma_c = (p_clabe[i,i] * v_multiplo) USING "&&"
         DISPLAY "v_suma_c :", v_suma_c, " con el indice :", i
         LET v_clabe_digitos[i,i] = v_suma_c[2,2]
         LET v_suma = v_suma + v_clabe_digitos[i,i]
      END FOR 
      DISPLAY "los multiplos >", v_clabe_digitos, "<"
      DISPLAY "La suma :", v_suma
      LET v_suma_c = v_suma USING "&&"
      DISPLAY "v_suma_c para el digito: ", v_suma_c[2,2]
      LET v_digito = 10 - v_suma_c[2,2] 
      DISPLAY "El digito :", v_digito
      DISPLAY "EL digito CLABE :", p_clabe[18,18]
      IF v_digito <> p_clabe[18,18] THEN 
         LET v_clabe_correcta = FALSE 
      END IF 
   ELSE 
      LET v_clabe_correcta = FALSE 
   END IF 
   -- se devuelve el resultado de la consulta
   RETURN v_clabe_correcta
END FUNCTION

