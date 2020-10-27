--===============================================================
{
VERSION: 1.0.0
FECHA ULTIMA MODIFICACION:
  15mayo2013. Se agrega a parametros de entrada:
              - ESTATUS_SSV. Estatus del proceso (solicitud del portal)
              - ID_PRO     . Identificador del proceso (marca)
              
              Se agrega a parametros de salida:
              - GRUPO       . Grupo de retiro
              - SALDO_ADIC  . Saldo adicional a integrar en futuros casos 
              - SALDO_NO_TES. Saldo no en BT
              - FECHA_TC    . Fecha del tipo de cambio usada
  27junio2013. Se redefinen los campos de entrada y salida del webservice

Entrada NSS           Número de Seguridad Social         CHAR 11 0
Entrada GRUPO         Número de Grupo a Consultar        CHAR  4 0
Entrada FECHA_VAL     Fecha de valuación de saldo        CHAR  8 0
Entrada ESTATUS_SSV   Estatus de proceso                 CHAR  4 0
Entrada ID_PRO        Identificador de Proceso           CHAR  2 0
Entrada IMP_PAGO_FICO Importe confirmado pagado por FICO CHAR 10 2
Entrada AIVS 92       Número de AIVS de SSV 92           CHAR 10 2
Entrada AIVS 97       Número de AIVS de SSV 97           CHAR 10 2

Salida  NSS            Número de Seguridad Social                    CHAR 11 0
Salida  GRUPO          Número de Grupo de retorno                    CHAR  4 0
Salida  SALDO_ADIC     Saldo adicional a integrar en futuros casos   CHAR 10 2
Salida  SALDO_NO_TES   Saldo no en BT * revisar la conta             CHAR 10 2
Salida  SSV97_TRANS    Saldo Transferido al Gobierno Federal         CHAR 10 2
Salida  SSV97_NO_TRANS Saldo no Transferido al Gobierno Federal      CHAR 10 2
Salida  AIV_92         Importe en AIVS 92                            CHAR 10 2
Salida  AIV_97         Importe en AIVS 97                            CHAR 10 2
Salida  PESOS_92       Importe en Pesos 92                           CHAR 10 2
Salida  PESOS_97       Importe en Pesos 97                           CHAR 10 2
Salida  TC             Tipo de cambio usuado para convertir a pesos  CHAR  5 5
Salida  FECHA_TC       Fecha de tipo de cambio                       CHAR  8 0
Salida  CODRET         Codigo de Retorno                             CHAR  2 
Salida  MENSAJE        Mensaje explicativo del CODRET                CHAR 50 

}
--===============================================================

###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW20                                                  #
#OBJETIVO          => WS Consulta de saltos para Retiros en RTM               #
#FECHA INICIO      => 14 mayo 2013                                            #
###############################################################################

--disparador de ws solicitud de retiro viv7292fondo
--/opt/fourjs/2.32/gas/. ./envas
--httpdispatch -f as_safreviv_ws1.xcf
--port 9186

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
GLOBALS

-- registro con datos de entrada
DEFINE gr_ws_consulta_retiro_in RECORD
          nss           CHAR(11),
          grupo         CHAR( 4),
          fecha_val     CHAR( 8),
          estatus_ssv   CHAR( 4),
          id_pro        CHAR( 2),
          imp_pago_fico CHAR(12),
          aivs_92       CHAR(12),
          aivs_97       CHAR(12)
       END RECORD,
       -- registro de respuesta
       gr_ws_consulta_retiro_out RECORD       
          nss            CHAR(11),
          grupo          CHAR( 4),
          saldo_adic     CHAR(12),
          saldo_no_tes   CHAR(12),
          ssv97_trans    CHAR(12),
          ssv97_no_trans CHAR(12),
          aiv_92         CHAR(12),
          aiv_97         CHAR(12),
          pesos_92       CHAR(12),
          pesos_97       CHAR(12),
          tc             CHAR(10),
          fecha_tc       CHAR( 8),
          codret         CHAR( 2),
          mensaje        CHAR(50)
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
          g_msg_error_interno                STRING = "Ocurrió un error interno",
          -- marca del retiro
          g_marca_retiro                     SMALLINT = 803 -- retiro ley 73

-- =======================================================
-- constantes para los estados de la solicitud y para los codigos de rechazo
CONSTANT  gi_consulta_correcta             SMALLINT = 0  ,
          gi_consulta_erronea              SMALLINT = 99 

CONSTANT  gi_datos_incompletos             SMALLINT = 99,
          gi_nss_rfc_no_existe             SMALLINT = 98,
          gi_causal_retiro_invalido        SMALLINT = 77,
          gi_solicitud_en_tramite          SMALLINT = 99,
          gi_tiene_credito_vigente         SMALLINT = 20,
          gi_sin_saldo                     SMALLINT = 10,
          gi_sin_un_ano_relacion_laboral   SMALLINT = 50,
          gi_edad_inferior_50_anos         SMALLINT = 40,
          gi_sin_pension_vigente           SMALLINT = 90,
          gi_mas_de_un_registro            SMALLINT = 100
         
DEFINE serverURL STRING -- URL del servidor
DEFINE v_pantalla    SMALLINT

END GLOBALS

{
======================================================================
Nombre: MAIN
Fecha creacion: Mayo 14, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra y realiza la atencion de solicitudes del webservice de consulta
de saldo para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW20."
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

  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_retiro(TRUE)
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
      IF ( num_args() <> 0 ) THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  -- se crea el servicio
  CALL ERRORLOG("invoca creacion de consulta de saldo para Retiros")
  CALL fn_crea_servicio_retiro(FALSE)

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
                DISPLAY "Se recibió otro codigo de retorno" TO msg
           END CASE
        
        -- si se elige cerrar
        ON ACTION CLOSE
           EXIT PROGRAM
     END MENU
    
  ELSE  -- no se tiene pantalla
    WHILE TRUE
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
Nombre: fn_crea_servicio_retiro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Crea el servicio web

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_retiro(generateWSDL)
  DEFINE serv                 com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE generateWSDL         SMALLINT
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace = "http://localhost/"
  LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

  TRY
    -- =============================
    -- se crea el servicio
    LET serv = com.WebService.CreateWebService("consultaSaldoRetiro", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateRPCStyle("fn_saldo_retiro","fn_saldo_retiro",gr_ws_consulta_retiro_in,gr_ws_consulta_retiro_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_saldo_retiro","fn_saldo_retiro",gr_ws_consulta_retiro_in,gr_ws_consulta_retiro_out)
    CALL serv.publishOperation(op, NULL)

    -- si se hace generacion del WSDL
    IF ( generateWSDL ) THEN
       -- Generar el WSDL
       LET v_resultado = serv.saveWSDL(serverURL)
       
       -- si se genero el WSDL sin errores
       IF ( v_resultado = 0 ) THEN
          DISPLAY "WSDL creado exitosamente"
       ELSE
          DISPLAY "ERROR: No se pudo crear el WSDL"
       END IF
    ELSE
       -- =========================
       -- REgistro del servicio
       CALL com.WebServiceEngine.RegisterService(serv)  
       CALL ERRORLOG("Se registro el servicio consulta de saldo para Retiro")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Consulta saldo para Retiro':" || STATUS)
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
Nombre: fn_saldo_retiro
Fecha creacion: Mayo 14, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion que consulta el saldo de un trabajador de acuerdo a la fecha de
consulta y con respecto al valor de las AIVs de la fecha de tipo de
cambio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_saldo_retiro()
DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- para validar existencia del NSS
       v_ruta_ejecutable    VARCHAR(40),
       v_cadena             STRING,
       v_ruta_log           STRING
      
   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
          
   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RW20_"
   LET v_cadena   = TODAY USING "yyyymmdd"
   LET v_ruta_log = v_ruta_log || v_cadena
   LET v_cadena   = "_" || gr_ws_consulta_retiro_in.nss
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   --LET v_ruta_log = trim(g_nss) || ".log" 
   
   DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
   DISPLAY "NSS evaluado: ", gr_ws_consulta_retiro_in.nss
   CALL STARTLOG(v_ruta_log) 
   
   IF ( gr_ws_consulta_retiro_in.nss IS NOT NULL ) THEN
      CALL ERRORLOG("Validando solicitud para NSS: " || gr_ws_consulta_retiro_in.nss)
   END IF

   -- se verifica si se recibieron los datos necesarios
   IF ( gr_ws_consulta_retiro_in.nss IS NULL ) THEN
      -- se necesita el NSS para poder consultar el saldo
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se recibio NSS")
      RETURN
   END IF
   
   -- se verifica si se recibio el grupo
   IF ( gr_ws_consulta_retiro_in.grupo IS NULL ) THEN
      -- se necesita el grupo para poder consultar el saldo
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se recibio Grupo")
      RETURN   
   END IF
   
   -- se obtiene el id_derechohabiente del NSS
   CALL fn_obtener_id_derechohabiente(gr_ws_consulta_retiro_in.nss) RETURNING v_id_derechohabiente
      
   IF ( v_id_derechohabiente IS NULL ) THEN
      -- si no se encontraron coincidencias
      CALL ERRORLOG("No existe el nss")
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_nss_rfc_no_existe, "NSS no existe en base de datos de Vivienda")
      RETURN    
   END IF
   
   -- se verifica el tipo de operacion que se esta solicitando
   CASE gr_ws_consulta_retiro_in.estatus_ssv
         
      -- consulta de saldo
      WHEN "0010"
         -- consulta el saldo marcando
         CALL fn_consulta_saldo_nss_grupo(v_id_derechohabiente, TRUE)
   
      -- ======================================================================
      -- generar el caso y resguardar monto en historico
      WHEN "0011"
         CALL fn_resguarda_valores(v_id_derechohabiente)
         
         
      -- ======================================================================
      -- se marca la cuenta
      WHEN "0012"
         CALL fn_marca_cuenta(v_id_derechohabiente)

      WHEN "0013"
         CALL fn_marca_cuenta(v_id_derechohabiente)
         
      -- ======================================================================
      -- confirmado por banco
      WHEN "0014"
         CALL fn_confirma_banco(v_id_derechohabiente)
   
      WHEN "0017"
         CALL fn_confirma_banco(v_id_derechohabiente)

      WHEN "0019"
         CALL fn_confirma_banco(v_id_derechohabiente)

      WHEN "0021"
         CALL fn_confirma_banco(v_id_derechohabiente)
      
      -- ======================================================================
      -- rechazo por banco
      WHEN "0015"
         CALL fn_rechazo_banco(v_id_derechohabiente)
         
      WHEN "0018"
         CALL fn_rechazo_banco(v_id_derechohabiente)
         
      WHEN "0020"
         CALL fn_rechazo_banco(v_id_derechohabiente)
         
      WHEN "0022"
         CALL fn_rechazo_banco(v_id_derechohabiente)
   
      -- ======================================================================      
      -- caso invalido
      OTHERWISE
         -- consulta el saldo sin marcar
         CALL fn_consulta_saldo_nss_grupo(v_id_derechohabiente, FALSE)
   END CASE

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_consulta_saldo_nss_grupo
Fecha creacion: Mayo 14, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza la consulta de saldo de un NSS y grupo dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_saldo_nss_grupo(v_id_derechohabiente, p_marcar_cuenta)
DEFINE p_marcar_cuenta      SMALLINT, -- boolena que indica si se marca la cuenta
       p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud       DECIMAL(9,0),
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_grupo_salida       CHAR(4),
       v_grupo_subcuenta    SMALLINT, -- grupo de subcuentas que se pagaran
       v_subcuenta          SMALLINT, -- subcuenta buscada
       v_cadena             STRING,
       v_estatus_marca      SMALLINT


   -- se obtiene el grupo de salida de tesofe y el grupo de subcuenta
   CALL fn_consulta_grupo_tesofe_salida_grupo_subcuenta(gr_ws_consulta_retiro_in.grupo)
        RETURNING v_grupo_salida, v_grupo_subcuenta
   
   -- se verifica si se debe consultar viv92
   SELECT subcuenta
   INTO   v_subcuenta
   FROM   ret_grupo_subcta
   WHERE  grupo = v_grupo_subcuenta
   AND    subcuenta = 8 --vid92
   
   -- si se debe pagar
   IF ( v_subcuenta = 8 ) THEN
      DISPLAY "Subcuenta buscada viv92: ", v_subcuenta
      -- vivienda 92 AIVs
      CALL fn_consulta_saldo_subcuenta_aivs(v_id_derechohabiente, 8, 11, TODAY)
           RETURNING p_aiv_92
   ELSE
      -- no se paga, se devuelve cero
      LET p_aiv_92 = 0
   END IF
   
   -- se verifica si se debe consultar viv92
   SELECT subcuenta
   INTO   v_subcuenta
   FROM   ret_grupo_subcta
   WHERE  grupo = v_grupo_subcuenta
   AND    subcuenta = 4 --viv97

   -- si se debe pagar
   IF ( v_subcuenta = 4 ) THEN
      DISPLAY "Subcuenta buscada viv97: ", v_subcuenta
      -- vivienda 97 AIVs
      CALL fn_consulta_saldo_subcuenta_aivs(v_id_derechohabiente, 4, 11, TODAY)
           RETURNING p_aiv_97
   ELSE
      -- no se paga, se devuelve cero
      LET p_aiv_97 = 0
   END IF
  
   -- se asume que la fecha del tipo de cambio sera la misma que la de consulta
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo
   
   -- si no se encontro valor de fondo es un error
   IF ( v_valor_fondo IS NULL ) THEN
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se encontró tipo de cambio para la fecha de valuación")
      RETURN
   END IF
   
   -- se asignan los valores para conformar la respuesta
   LET p_saldo_adic     = fn_consulta_saldo_adicional()
   LET p_saldo_no_tes   = fn_consulta_saldo_no_tes()
   LET p_ssv97_trans    = fn_consulta_saldo_ssv97_trans()
   LET p_ssv97_no_trans = fn_consulta_saldo_ssv97_no_trans()
   -- se valuan las AIVs de acuerdo al tipo de cambio
   LET p_pesos_92       = p_aiv_92 * v_valor_fondo
   LET p_pesos_97       = p_aiv_97 * v_valor_fondo
   LET p_tc             = v_valor_fondo
   LET p_codret         = gi_consulta_correcta
   LET p_mensaje        = "Consulta exitosa"

   -- la fecha del tipo de cambio es la que se recibe
   LET p_fecha_tc = v_fecha_tipo_cambio USING "yyyymmdd"
     
   -- si se solicito que se marcara la cuenta
   IF ( p_marcar_cuenta ) THEN
     
      -- se verifica si el derechohabiente ya esta en la tabla de solicitudes con un estatus no confirmado
      SELECT MAX(id_solicitud)
      INTO   v_id_solicitud
      FROM   ret_his_saldo_sV97
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    estatus_ssv IN ( "0010", "0011", "0012", "0013")
      
      -- si se encontro la solicitud, entonces ya no es necesario marcar
      IF ( v_id_solicitud IS NULL ) THEN
         -- se inserta el registro     
         CALL fn_inserta_ret_his_saldo_sv97(v_id_derechohabiente,
                                            gr_ws_consulta_retiro_in.grupo,
                                            gr_ws_consulta_retiro_in.estatus_ssv,
                                            10, -- estatus de la solicitud
                                            0, 0, 0, 0,
                                            p_aiv_92, p_aiv_97, p_fecha_tc, gi_consulta_correcta)
              RETURNING v_id_solicitud
        
         -- se intenta marcar la cuenta
         CALL fn_ejecuta_marca_cuenta(v_id_derechohabiente, v_id_solicitud) RETURNING v_estatus_marca
      END IF
   END IF

   -- se construye la respuesta de la consulta correcta
   CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss  ,
                        gr_ws_consulta_retiro_in.grupo,
                        p_saldo_adic                  ,
                        p_saldo_no_tes                ,
                        p_ssv97_trans                 ,
                        p_ssv97_no_trans              ,
                        p_aiv_92                      ,
                        p_aiv_97                      ,
                        p_pesos_92                    ,
                        p_pesos_97                    ,
                        p_tc                          ,
                        p_fecha_tc                    ,
                        p_codret                      ,
                        p_mensaje)

       
END FUNCTION 

{
======================================================================
Nombre: fn_consulta_saldo_subcuenta_aivs
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el saldo de una subcuenta en AIVs para un NSS y una fecha
determinada

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_saldo_subcuenta_aivs(p_id_derechohabiente, p_subcuenta, p_fondo_inversion, p_f_consulta)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_subcuenta           SMALLINT, -- subcuenta
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_sum_aivs            DECIMAL(20,6)
       
   DISPLAY "Fecha de consulta para subcuenta ", p_subcuenta, ": ", p_f_consulta
       
   -- se obtiene la suma de la subcuenta y fondo de inversion
   SELECT SUM(monto_acciones)
   INTO   v_sum_aivs
   FROM   cta_movimiento
   WHERE  id_derechohabiente = p_id_derechohabiente
   AND    subcuenta          = p_subcuenta
   AND    fondo_inversion    = p_fondo_inversion
   AND    f_liquida         <= p_f_consulta
   
   -- si el resultado de la consulta es nulo
   IF ( v_sum_aivs IS NULL OR v_sum_aivs < 0 ) THEN
      -- se devuelve cero
      LET v_sum_aivs = 0
   END IF
   
   -- se devuelve el resultado de la consulta
   RETURN v_sum_aivs
END FUNCTION

{
======================================================================
Nombre: fn_consulta_saldo_adicional
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el saldo adicional de una subcuenta para un NSS determinado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     01 julio 2013           - De momento no se calculara este saldo, por
                                        lo que se devolvera cero
======================================================================
}
FUNCTION fn_consulta_saldo_adicional()
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_subcuenta           SMALLINT, -- subcuenta
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_sum_aivs            DECIMAL(20,6)
       
   -- no se consultara este saldo en este momento
   LET v_sum_aivs = 0
   
   -- se devuelve el resultado de la consulta
   RETURN v_sum_aivs
END FUNCTION

{
======================================================================
Nombre: fn_consulta_saldo_no_tes
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el saldo no Tes de una subcuenta para un NSS determinado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     01 julio 2013           - De momento no se calculara este saldo, por
                                        lo que se devolvera cero
======================================================================
}
FUNCTION fn_consulta_saldo_no_tes()
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_subcuenta           SMALLINT, -- subcuenta
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_sum_aivs            DECIMAL(20,6)
       
   -- no se consultara este saldo en este momento
   LET v_sum_aivs = 0
   
   -- se devuelve el resultado de la consulta
   RETURN v_sum_aivs
END FUNCTION

{
======================================================================
Nombre: fn_consulta_saldo_ssv97_trans
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el saldo transferido al gobierno federal para un NSS determinado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     01 julio 2013           - De momento no se calculara este saldo, por
                                        lo que se devolvera cero
======================================================================
}
FUNCTION fn_consulta_saldo_ssv97_trans()
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_subcuenta           SMALLINT, -- subcuenta
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_sum_aivs            DECIMAL(20,6)
       
   -- de momento no se consultara este saldo
   LET v_sum_aivs = 0

   -- se devuelve el resultado de la consulta
   RETURN v_sum_aivs
END FUNCTION

{
======================================================================
Nombre: fn_consulta_saldo_ssv97_no_trans
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el saldo NO transferido al gobierno federal para un NSS determinado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     01 julio 2013           - De momento no se calculara este saldo, por
                                        lo que se devolvera cero
======================================================================
}
FUNCTION fn_consulta_saldo_ssv97_no_trans()
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_subcuenta           SMALLINT, -- subcuenta
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_sum_aivs            DECIMAL(20,6)
       
   
   -- no se consulta este saldo en este momento
   LET v_sum_aivs = 0
   
   -- se devuelve el resultado de la consulta
   RETURN v_sum_aivs
END FUNCTION

{
======================================================================
Nombre: fn_consulta_valor_fondo
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el valor de un fondo de inversion segun una fecha determinada

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_valor_fondo(p_fondo_inversion, p_f_consulta)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- nss del trabajador
       p_fondo_inversion     SMALLINT, -- fondo de inversion
       p_f_consulta          DATE, -- fecha de corte para la consulta
       v_valor_fondo         DECIMAL(19,14) -- valor del fondo
       
   -- se obtiene la suma de la subcuenta y fondo de inversion
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  fondo            = p_fondo_inversion
   AND    f_valuacion      = p_f_consulta
   
   -- se devuelve el resultado de la consulta
   RETURN v_valor_fondo
END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_rechazo_banco
Fecha creacion: Julio 02, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Rechaza una solicitud de retiro por causa de un rechazo bancario y desmarca
la cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_rechazo_banco(v_id_derechohabiente)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud       DECIMAL(9,0), -- solicitud de retiro
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_estatus_ssv        CHAR(4),
       v_i_estado_marca     SMALLINT

   -- se obtiene el valor del fondo
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo

   -- si no se encuentra valor del fondo
   IF ( v_valor_fondo IS NULL ) THEN
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se tiene valor de fondo")
   END IF
   
   -- se asignan los valores para conformar la respuesta
   LET p_saldo_adic     = 0
   LET p_saldo_no_tes   = 0
   LET p_ssv97_trans    = 0
   LET p_ssv97_no_trans = 0
   -- se valuan las AIVs de acuerdo al tipo de cambio
   LET p_aiv_92         = 0
   LET p_aiv_97         = 0
   LET p_pesos_92       = 0
   LET p_pesos_97       = 0
   LET p_tc             = 0
   LET p_codret         = gi_consulta_correcta
   LET p_mensaje        = "La solicitud ha sido rechazada por Banco"

   -- la fecha del tipo de cambio es la que se recibe
   LET p_fecha_tc = v_fecha_tipo_cambio USING "yyyymmdd"
     
   -- se verifica si el derechohabiente ya esta en la tabla de solicitudes con un estatus no confirmado
   SELECT MAX(id_solicitud)
   INTO   v_id_solicitud
   FROM   ret_his_saldo_sV97
   WHERE  id_derechohabiente = v_id_derechohabiente
   AND    estatus_ssv IN ( "0011", "0012", "0013")
   
   -- si no esta registrado aun
   IF ( v_id_solicitud IS NULL ) THEN
      -- no se pudo marcar la cuenta, es un error
      LET p_codret  = gi_consulta_erronea
      LET p_mensaje = "El NSS no tiene una solicitud de retiro"
   ELSE
      -- la solicitud ya existe, se actualiza el estatus a rechazado
      UPDATE ret_his_saldo_sv97
      SET    estatus_ssv          = gr_ws_consulta_retiro_in.estatus_ssv,
             estado_solicitud     = 100
      WHERE  id_solicitud         = v_id_solicitud
      
      
      -- se desmarca la cuenta
      CALL fn_ejecuta_desmarca_cuenta(v_id_derechohabiente, v_id_solicitud) RETURNING v_i_estado_marca
      
      -- si no se pudo desmarcar
      IF ( v_i_estado_marca <> 0 ) THEN
         LET p_codret  = gi_consulta_erronea
         LET p_mensaje = "La solicitud ha sido rechazada pero no se pudo desmarcar"
      END IF
   END IF

   -- se construye la respuesta de la consulta
   CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss  ,
                        gr_ws_consulta_retiro_in.grupo,
                        p_saldo_adic                  ,
                        p_saldo_no_tes                ,
                        p_ssv97_trans                 ,
                        p_ssv97_no_trans              ,
                        p_aiv_92                      ,
                        p_aiv_97                      ,
                        p_pesos_92                    ,
                        p_pesos_97                    ,
                        p_tc                          ,
                        p_fecha_tc                    ,
                        p_codret                      ,
                        p_mensaje)
END FUNCTION


{======================================================================
Clave: 
Nombre: fn_confirma_banco
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27junio2013           - Cambio el numero de campos de retorno. Se ajusta la signatura y
                                      valores de retorno de la funcion
======================================================================
}
FUNCTION fn_confirma_banco(v_id_derechohabiente)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       v_aiv_97_db          LIKE ret_preliquida.monto_acciones, -- AIVs viv97
       v_aiv_92_db          LIKE ret_preliquida.monto_acciones, -- AIVs viv92
       v_aiv_97_ws          LIKE ret_preliquida.monto_acciones, -- AIVs viv97
       v_aiv_92_ws          LIKE ret_preliquida.monto_acciones, -- AIVs viv92
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud       DECIMAL(9,0), -- solicitud de retiro
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_estatus_ssv        CHAR(4)

   -- se obtiene el valor del fondo
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo

   -- si no se encuentra valor del fondo
   IF ( v_valor_fondo IS NULL ) THEN
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se tiene valor de fondo")
   END IF
   
   -- se asignan los valores para conformar la respuesta
   LET p_saldo_adic     = 0
   LET p_saldo_no_tes   = 0
   LET p_ssv97_trans    = 0
   LET p_ssv97_no_trans = 0
   -- se valuan las AIVs de acuerdo al tipo de cambio
   LET p_aiv_92         = 0
   LET p_aiv_97         = 0
   LET p_pesos_92       = 0
   LET p_pesos_97       = 0
   LET p_tc             = 0
   LET p_codret         = gi_consulta_correcta
   LET p_mensaje        = "La cuenta se ha confirmado"

   -- la fecha del tipo de cambio es la que se recibe
   LET p_fecha_tc = v_fecha_tipo_cambio USING "yyyymmdd"
     
   -- se verifica si el derechohabiente ya esta en la tabla de solicitudes con un estatus no confirmado
   SELECT MAX(id_solicitud), aiv_92, aiv_97
   INTO   v_id_solicitud, v_aiv_92_db, v_aiv_97_db
   FROM   ret_his_saldo_sV97
   WHERE  id_derechohabiente = v_id_derechohabiente
   AND    estatus_ssv IN ( "0011", "0012", "0013")
   GROUP BY 2,3
     
   -- si no esta registrado aun
   IF ( v_id_solicitud IS NULL ) THEN
      -- no se pudo marcar la cuenta, es un error
      LET p_codret  = gi_consulta_erronea
      LET p_mensaje = "El NSS no tiene una solicitud de retiro con datos resguardados"
   ELSE
      LET v_aiv_92_ws = gr_ws_consulta_retiro_in.aivs_92
      LET v_aiv_97_ws = gr_ws_consulta_retiro_in.aivs_97
      -- la solicitud ya existe, se verifica si los montos enviados son iguales a los resguardados
      IF ( v_aiv_92_ws <> v_aiv_92_db OR v_aiv_97_ws <> v_aiv_97_db ) THEN
         -- los datos enviados no concuerdan con los resguardados
         DISPLAY "v_aiv_92_ws: ", v_aiv_92_ws
         DISPLAY "v_aiv_97_ws: ", v_aiv_97_ws
         DISPLAY "v_aiv_97_db: ", v_aiv_92_db
         DISPLAY "v_aiv_97_db: ", v_aiv_97_db

         LET p_codret  = gi_consulta_erronea
         LET p_mensaje = "Montos enviados diferentes de montos resguardados"
      ELSE
         -- los datos concuerdan, se confirma la solicitud
         UPDATE ret_his_saldo_sv97
         SET    estatus_ssv          = gr_ws_consulta_retiro_in.estatus_ssv
         WHERE  id_solicitud         = v_id_solicitud
      END IF
   END IF

   -- se construye la respuesta de la consulta
   CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss  ,
                        gr_ws_consulta_retiro_in.grupo,
                        p_saldo_adic                  ,
                        p_saldo_no_tes                ,
                        p_ssv97_trans                 ,
                        p_ssv97_no_trans              ,
                        p_aiv_92                      ,
                        p_aiv_97                      ,
                        p_pesos_92                    ,
                        p_pesos_97                    ,
                        p_tc                          ,
                        p_fecha_tc                    ,
                        p_codret                      ,
                        p_mensaje)

END FUNCTION

{======================================================================
Clave: 
Nombre: fn_marca_cuenta
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27junio2013           - Cambio el numero de campos de retorno. Se ajusta la signatura y
                                      valores de retorno de la funcion
======================================================================
}
FUNCTION fn_marca_cuenta(v_id_derechohabiente)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_estatus_ssv        CHAR(4),
       v_i_estado_marca     SMALLINT, -- resultado de marcar la cuenta
       v_id_solicitud       DECIMAL(9,0),
       v_tpo_originacion    SMALLINT,
       v_marca_infonavit    SMALLINT,
       v_marca_ws           SMALLINT


   -- se obtiene el valor del fondo
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo

   -- si no se encuentra valor del fondo
   IF ( v_valor_fondo IS NULL ) THEN
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se tiene valor de fondo")
   END IF
     
   -- se asignan los valores para conformar la respuesta
   LET p_saldo_adic     = 0
   LET p_saldo_no_tes   = 0
   LET p_ssv97_trans    = 0
   LET p_ssv97_no_trans = 0
   -- se valuan las AIVs de acuerdo al tipo de cambio
   LET p_aiv_92         = 0
   LET p_aiv_97         = 0
   LET p_pesos_92       = 0
   LET p_pesos_97       = 0
   LET p_tc             = 0
   LET p_codret         = gi_consulta_correcta
   LET p_mensaje        = "La cuenta se ha marcado"

   -- la fecha del tipo de cambio es la que se recibe
   LET p_fecha_tc = v_fecha_tipo_cambio USING "yyyymmdd"
     
   -- se verifica si el derechohabiente ya esta en la tabla de solicitudes con un estatus no confirmado
   SELECT MAX(id_solicitud)
   INTO   v_id_solicitud
   FROM   ret_his_saldo_sV97
   WHERE  id_derechohabiente = v_id_derechohabiente
   AND    estatus_ssv IN ( "0010", "0011", "0012", "0013")
   
   -- si no esta registrado aun
   IF ( v_id_solicitud IS NULL ) THEN
      -- se inserta el registro     
      CALL fn_inserta_ret_his_saldo_sv97(v_id_derechohabiente,
                                         gr_ws_consulta_retiro_in.grupo,
                                         gr_ws_consulta_retiro_in.estatus_ssv,
                                         10, -- estatus de la solicitud
                                         0, 0, 0, 0,
                                         p_aiv_92, p_aiv_97, p_fecha_tc, gi_consulta_correcta)
           RETURNING v_id_solicitud

      -- se intenta marcar la cuenta
      CALL fn_ejecuta_marca_cuenta(v_id_derechohabiente, v_id_solicitud) RETURNING v_i_estado_marca
      
      -- si no se pudo marcar
      IF ( v_i_estado_marca <> 0 ) THEN
         -- se obtiene el mensaje de error
         CALL fn_mensaje_error_marca_cuenta(v_i_estado_marca) RETURNING v_cadena

         -- no se pudo marcar la cuenta, es un error
         LET p_codret  = gi_consulta_erronea
         LET p_mensaje = v_cadena
      END IF
   ELSE
      -- la solicitud ya existe, se actualizan el estatus a marcado
      UPDATE ret_his_saldo_sv97
      SET    estatus_ssv          = gr_ws_consulta_retiro_in.estatus_ssv
      WHERE  id_solicitud         = v_id_solicitud
   END IF

   -- se construye la respuesta de la consulta
   CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss  ,
                        gr_ws_consulta_retiro_in.grupo,
                        p_saldo_adic                  ,
                        p_saldo_no_tes                ,
                        p_ssv97_trans                 ,
                        p_ssv97_no_trans              ,
                        p_aiv_92                      ,
                        p_aiv_97                      ,
                        p_pesos_92                    ,
                        p_pesos_97                    ,
                        p_tc                          ,
                        p_fecha_tc                    ,
                        p_codret                      ,
                        p_mensaje)

END FUNCTION

{======================================================================
Clave: 
Nombre: fn_resguarda_valores
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27junio2013           - Cambio el numero de campos de retorno. Se ajusta la signatura y
                                      valores de retorno de la funcion
======================================================================
}
FUNCTION fn_resguarda_valores(v_id_derechohabiente)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud       DECIMAL(9,0),
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_i_estado_marca     SMALLINT,
       v_grupo_salida       CHAR(4),
       v_grupo_subcuenta    SMALLINT, -- grupo de subcuentas que se pagaran
       v_subcuenta          SMALLINT -- subcuenta buscada

   -- se obtiene el grupo de salida de tesofe y el grupo de subcuenta
   CALL fn_consulta_grupo_tesofe_salida_grupo_subcuenta(gr_ws_consulta_retiro_in.grupo)
        RETURNING v_grupo_salida, v_grupo_subcuenta
   
   -- se verifica si se debe consultar viv92
   SELECT subcuenta
   INTO   v_subcuenta
   FROM   ret_grupo_subcta
   WHERE  grupo = v_grupo_subcuenta
   AND    subcuenta = 8 --vid92
   
   -- si se debe pagar
   IF ( v_subcuenta IS NOT NULL ) THEN
      -- vivienda 92 AIVs
      CALL fn_consulta_saldo_subcuenta_aivs(v_id_derechohabiente, 8, 11, TODAY)
           RETURNING p_aiv_92
   ELSE
      -- no se paga, se devuelve cero
      LET p_aiv_92 = 0
   END IF
   
   -- se verifica si se debe consultar viv92
   SELECT subcuenta
   INTO   v_subcuenta
   FROM   ret_grupo_subcta
   WHERE  grupo = v_grupo_subcuenta
   AND    subcuenta = 4 --viv97

   -- si se debe pagar
   IF ( v_subcuenta IS NOT NULL ) THEN
      -- vivienda 97 AIVs
      CALL fn_consulta_saldo_subcuenta_aivs(v_id_derechohabiente, 4, 11, TODAY)
           RETURNING p_aiv_97
   ELSE
      -- no se paga, se devuelve cero
      LET p_aiv_97 = 0
   END IF

   -- se obtiene el valor del fondo
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo

   -- si no se encuentra valor del fondo
   IF ( v_valor_fondo IS NULL ) THEN
      CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss, gr_ws_consulta_retiro_in.grupo, 
                           0,0,0,0,0,0,0,0,0,NULL, gi_consulta_erronea, "No se tiene valor de fondo")
   END IF
  
   -- se asignan los valores para conformar la respuesta
   LET p_saldo_adic     = fn_consulta_saldo_adicional()
   LET p_saldo_no_tes   = fn_consulta_saldo_no_tes()
   LET p_ssv97_trans    = fn_consulta_saldo_ssv97_trans()
   LET p_ssv97_no_trans = fn_consulta_saldo_ssv97_no_trans()
   -- se valuan las AIVs de acuerdo al tipo de cambio
   LET p_pesos_92       = p_aiv_92 * v_valor_fondo
   LET p_pesos_97       = p_aiv_97 * v_valor_fondo
   LET p_tc             = v_valor_fondo
   LET p_codret         = gi_consulta_correcta
   LET p_mensaje        = "Los valores han sido resguardados"

   -- la fecha del tipo de cambio es la que se recibe
   LET p_fecha_tc = v_fecha_tipo_cambio USING "yyyymmdd"
     
   -- se verifica si el derechohabiente ya esta en la tabla de solicitudes con un estatus no confirmado
   SELECT MAX(id_solicitud)
   INTO   v_id_solicitud
   FROM   ret_his_saldo_sV97
   WHERE  id_derechohabiente = v_id_derechohabiente
   AND    estatus_ssv IN ( "0010", "0011", "0012", "0013")
   
   -- si no esta registrado aun
   IF ( v_id_solicitud IS NULL ) THEN
      -- se inserta el registro     
      CALL fn_inserta_ret_his_saldo_sv97(v_id_derechohabiente,
                                         gr_ws_consulta_retiro_in.grupo,
                                         gr_ws_consulta_retiro_in.estatus_ssv,
                                         10, -- estatus de la solicitud
                                         0, 0, 0, 0,
                                         p_aiv_92, p_aiv_97, p_fecha_tc, gi_consulta_correcta)
           RETURNING v_id_solicitud

      -- se intenta marcar la cuenta
      CALL fn_ejecuta_marca_cuenta(v_id_derechohabiente, v_id_solicitud) RETURNING v_i_estado_marca
      
      -- si no se pudo marcar
      IF ( v_i_estado_marca <> 0 ) THEN
         -- se obtiene el mensaje de error
         CALL fn_mensaje_error_marca_cuenta(v_i_estado_marca) RETURNING v_cadena

         -- no se pudo marcar la cuenta, es un error
         LET p_codret  = gi_consulta_erronea
         LET p_mensaje = v_cadena
      END IF
   ELSE
      -- la solicitud ya existe, se actualizan los montos y no es necesario marcar
      UPDATE ret_his_saldo_sv97
      SET    saldo_adicional      = p_saldo_adic,
             saldo_no_tesofe      = p_saldo_no_tes,
             ssv97_transferido    = p_ssv97_trans,
             ssv97_no_transferido = p_ssv97_no_trans,
             aiv_92               = p_aiv_92,
             aiv_97               = p_aiv_97,
             pesos_92             = p_pesos_92,
             pesos_97             = p_pesos_97,
             estatus_ssv          = gr_ws_consulta_retiro_in.estatus_ssv
      WHERE  id_solicitud         = v_id_solicitud
   END IF

   -- se construye la respuesta de la consulta
   CALL fn_respuesta_ws(gr_ws_consulta_retiro_in.nss  ,
                        gr_ws_consulta_retiro_in.grupo,
                        p_saldo_adic                  ,
                        p_saldo_no_tes                ,
                        p_ssv97_trans                 ,
                        p_ssv97_no_trans              ,
                        p_aiv_92                      ,
                        p_aiv_97                      ,
                        p_pesos_92                    ,
                        p_pesos_97                    ,
                        p_tc                          ,
                        p_fecha_tc                    ,
                        p_codret                      ,
                        p_mensaje)
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_inserta_ret_his_saldo_sv97
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Inserta un registro en la tabla de solicitudes de retiro por web service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_inserta_ret_his_saldo_sv97(p_id_derechohabiente, p_grupo, p_estatus_ssv, p_estado_solicitud, 
                                       p_saldo_adic, p_saldo_no_tes, p_ssv97_trans, p_ssv97_no_trans,
                                       p_aiv_92, p_aiv_97, p_fecha_tc, p_codret)
DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       p_cod_origen           SMALLINT, -- QUE VA
       p_grupo                CHAR(4) , -- grupo del retiro
       p_estatus_ssv          CHAR(4) , -- estatus de la ssv
       p_estado_solicitud     SMALLINT, -- QUE VA
       p_saldo_adic           CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes         CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans          CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans       CHAR(12), -- saldo viv97 no transferido
       p_aiv_92               CHAR(12), -- AIVs viv92
       p_aiv_97               CHAR(12), -- AIVs viv97
       p_pesos_92             CHAR(12), -- Pesos Viv92
       p_pesos_97             CHAR(12), -- Pesos viv97
       p_fecha_tc             CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret               CHAR(2) , -- codigo retorno
       v_id_solicitud         DECIMAL(9,0),
       v_valor_fondo          DECIMAL(19,14),
       v_fecha_consulta       DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio    DATE,  -- fecha del tipo de cambio
       v_r_ret_his_saldo_sv97 RECORD LIKE ret_his_saldo_sv97.*, -- registro de saldo
       v_cadena               STRING,
       v_i_estado_marca       SMALLINT

   -- se obtiene la solicitud
   SELECT seq_ret_solicitud.NEXTVAL
   INTO   v_id_solicitud
   FROM   systables
   WHERE  tabid = 1

   -- se obtiene el valor del fondo
   LET v_cadena = gr_ws_consulta_retiro_in.fecha_val[5,6], "/", gr_ws_consulta_retiro_in.fecha_val[7,8], "/", gr_ws_consulta_retiro_in.fecha_val[1,4]
   LET v_fecha_tipo_cambio = DATE(v_cadena)
   CALL fn_consulta_valor_fondo(11, v_fecha_tipo_cambio) RETURNING v_valor_fondo

   -- se valuan las aivs
   LET p_pesos_92 = gr_ws_consulta_retiro_in.aivs_92 * v_valor_fondo
   LET p_pesos_97 = gr_ws_consulta_retiro_in.aivs_97 * v_valor_fondo

   -- se transfieren los datos al registro
   LET v_r_ret_his_saldo_sv97.id_solicitud         = v_id_solicitud
   LET v_r_ret_his_saldo_sv97.id_derechohabiente   = p_id_derechohabiente
   LET v_r_ret_his_saldo_sv97.cod_origen           = 1 -- es fijo en este momento
   LET v_r_ret_his_saldo_sv97.gpo_tesofe           = "0010" -- char(4) not null ,
   LET v_r_ret_his_saldo_sv97.gpo_tesofe_anterior  = p_grupo -- char(4) not null ,
   LET v_r_ret_his_saldo_sv97.grupo                = 4 -- smallint not null ,
   LET v_r_ret_his_saldo_sv97.estatus_ssv          = p_estatus_ssv
   LET v_r_ret_his_saldo_sv97.estado_solicitud     = p_estado_solicitud -- smallint not null ,
   LET v_r_ret_his_saldo_sv97.f_solicitud          = TODAY
   LET v_r_ret_his_saldo_sv97.num_operacion        = v_id_solicitud -- es un consecutivo segun
   LET v_r_ret_his_saldo_sv97.id_proceso           = "01" -- char(2),
   LET v_r_ret_his_saldo_sv97.saldo_adicional      = p_saldo_adic
   LET v_r_ret_his_saldo_sv97.saldo_no_tesofe      = p_saldo_no_tes
   LET v_r_ret_his_saldo_sv97.ssv97_transferido    = p_ssv97_trans
   LET v_r_ret_his_saldo_sv97.ssv97_no_transferido = p_ssv97_no_trans
   LET v_r_ret_his_saldo_sv97.aiv_92               = p_aiv_92
   LET v_r_ret_his_saldo_sv97.aiv_97               = p_aiv_97
   LET v_r_ret_his_saldo_sv97.pesos_92             = p_pesos_92
   LET v_r_ret_his_saldo_sv97.pesos_97             = p_pesos_97 
   LET v_r_ret_his_saldo_sv97.f_valor              = v_fecha_tipo_cambio -- date,
   LET v_r_ret_his_saldo_sv97.cod_retorno          = "01" -- char(2),

   -- se guardan los datos en la base de datos
   INSERT INTO ret_his_saldo_sv97 VALUES ( v_r_ret_his_saldo_sv97.* )
   
   -- se devuelve el num de solicitud dado
   RETURN v_id_solicitud
END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_respuesta_ws
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27junio2013           - Cambio el numero de campos de retorno. Se ajusta la signatura y
                                      valores de retorno de la funcion
======================================================================
}
FUNCTION fn_respuesta_ws(p_nss, p_grupo, p_saldo_adic, p_saldo_no_tes, p_ssv97_trans, p_ssv97_no_trans,
                         p_aiv_92, p_aiv_97, p_pesos_92, p_pesos_97, p_tc, p_fecha_tc, p_codret, p_mensaje)
DEFINE p_nss             CHAR(11), -- nss del trabajador
       p_grupo           CHAR(4) , -- grupo del retiro
       p_saldo_adic      CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes    CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans     CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans  CHAR(12), -- saldo viv97 no transferido
       p_aiv_92          CHAR(12), -- AIVs viv92
       p_aiv_97          CHAR(12), -- AIVs viv97
       p_pesos_92        CHAR(12), -- Pesos Viv92
       p_pesos_97        CHAR(12), -- Pesos viv97
       p_tc              CHAR(10), -- tipo de cambio
       p_fecha_tc        CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret          CHAR(2) , -- codigo retorno
       p_mensaje         CHAR(50) -- descripcion del codigo de retorno
       
   -- se construye la respuesta del webservice
   LET gr_ws_consulta_retiro_out.nss            = p_nss
   LET gr_ws_consulta_retiro_out.grupo          = p_grupo
   LET gr_ws_consulta_retiro_out.saldo_adic     = p_saldo_adic
   LET gr_ws_consulta_retiro_out.saldo_no_tes   = p_saldo_no_tes
   LET gr_ws_consulta_retiro_out.ssv97_trans    = p_ssv97_trans
   LET gr_ws_consulta_retiro_out.ssv97_no_trans = p_ssv97_no_trans
   LET gr_ws_consulta_retiro_out.aiv_92         = p_aiv_92
   LET gr_ws_consulta_retiro_out.aiv_97         = p_aiv_97
   LET gr_ws_consulta_retiro_out.pesos_92       = p_pesos_92
   LET gr_ws_consulta_retiro_out.pesos_97       = p_pesos_97
   LET gr_ws_consulta_retiro_out.tc             = p_tc
   LET gr_ws_consulta_retiro_out.fecha_tc       = p_fecha_tc
   LET gr_ws_consulta_retiro_out.codret         = p_codret
   LET gr_ws_consulta_retiro_out.mensaje        = p_mensaje

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_obtener_id_derechohabiente
Fecha creacion: 01 julio 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el id derechohabiente de un NSS dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtener_id_derechohabiente(v_nss)
DEFINE v_nss                CHAR(11),
       v_id_derechohabiente DECIMAL(9,0)
       
   -- para validar el NSS se verifica que exista al menos una vez
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = v_nss
   
   -- se devuelve el id_derechohabiente encontrado, NULL en caso contrario
   RETURN v_id_derechohabiente
END FUNCTION

{======================================================================
Clave: 
Nombre: fn_ejecuta_marca_cuenta
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la funcion de base de datos que realiza la marca de la cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ejecuta_marca_cuenta(v_id_derechohabiente, v_id_solicitud)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_estatus_ssv        CHAR(4),
       v_i_estado_marca     SMALLINT, -- resultado de marcar la cuenta
       v_id_solicitud       DECIMAL(9,0),
       v_tpo_originacion    SMALLINT,
       v_marca_infonavit    SMALLINT,
       v_marca_ws           SMALLINT
       
   -- se intenta marcar la cuenta
   LET v_cadena = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
   
   PREPARE sid_marcacuenta FROM v_cadena
   
   EXECUTE sid_marcacuenta  
   USING v_id_derechohabiente,
         g_marca_retiro      ,
         v_id_solicitud      ,
         "0"                 , -- folio
         "0"                 ,
         "0"                 ,
         "0"                 ,
         ""                  ,
         "safreviv"          ,
         "1521"
   INTO v_i_estado_marca

   DISPLAY "Marca: ", v_i_estado_marca

   RETURN v_i_estado_marca
END FUNCTION

{======================================================================
Clave: 
Nombre: fn_ejecuta_desmarca_cuenta
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la funcion de base de datos que realiza la desmarca de la cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ejecuta_desmarca_cuenta(v_id_derechohabiente, v_id_solicitud)
DEFINE p_nss                CHAR(11), -- nss del trabajador
       p_grupo              CHAR(4) , -- grupo del retiro
       p_saldo_adic         CHAR(12), -- saldo adicional a integrar en futuros casos
       p_saldo_no_tes       CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       p_ssv97_trans        CHAR(12), -- saldo viv97 transferido al gobierno federal
       p_ssv97_no_trans     CHAR(12), -- saldo viv97 no transferido
       p_aiv_92             CHAR(12), -- AIVs viv92
       p_aiv_97             CHAR(12), -- AIVs viv97
       p_pesos_92           CHAR(12), -- Pesos Viv92
       p_pesos_97           CHAR(12), -- Pesos viv97
       p_tc                 CHAR(10), -- tipo de cambio
       p_fecha_tc           CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       p_codret             CHAR(2) , -- codigo retorno
       p_mensaje            CHAR(50), -- descripcion del codigo de retorno
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_valor_fondo        DECIMAL(19,14),
       v_fecha_consulta     DATE, -- fecha de corte para la consulta
       v_fecha_tipo_cambio  DATE,  -- fecha del tipo de cambio
       v_cadena             STRING,
       v_estatus_ssv        CHAR(4),
       v_i_estado_marca     SMALLINT, -- resultado de marcar la cuenta
       v_id_solicitud       DECIMAL(9,0),
       v_tpo_originacion    SMALLINT,
       v_marca_infonavit    SMALLINT,
       v_marca_ws           SMALLINT
       
   -- se intenta marcar la cuenta
   LET v_cadena = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   
   PREPARE sid_desmarcacuenta FROM v_cadena
   
   EXECUTE sid_desmarcacuenta  
   USING v_id_derechohabiente,
         g_marca_retiro      ,
         v_id_solicitud      ,
         "0"                 , -- folio
         ""                 ,
         "safreviv"          ,
         "1521"
   INTO v_i_estado_marca

   DISPLAY "Marca: ", v_i_estado_marca

   RETURN v_i_estado_marca
END FUNCTION

{======================================================================
Clave: 
Nombre: fn_mensaje_error_marca_cuenta
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Interpreta el error de un intento de marcado y devuelve la cadena asociada

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_mensaje_error_marca_cuenta(v_i_estado_marca)
DEFINE v_i_estado_marca     SMALLINT, -- resultado de marcar la cuenta
       v_id_solicitud       DECIMAL(9,0),
       v_tpo_originacion    SMALLINT,
       v_marca_infonavit    SMALLINT,
       v_marca_ws           SMALLINT,
       v_mensaje            STRING

   -- si el error es por convencia de marca, se verifica si fue por credito, unificacion o separacion
   IF ( v_i_estado_marca = 501 OR v_i_estado_marca = 502 ) THEN
      -- marca de unificacion
      LET v_mensaje = "NSS en afiliacion"
   ELSE
      -- se revisa si la marca es por un credito de tipo garantia (anualidad o en garantia)
      SELECT tpo_originacion, marca_inf
      INTO   v_tpo_originacion, v_marca_infonavit
      FROM   cat_tipo_credito
      WHERE  tpo_originacion IN (2,4)
      AND    marca_inf = v_i_estado_marca;
        
      -- si se encontro, es un credito con alguna garantia
      IF ( v_marca_infonavit IS NOT NULL ) THEN
         LET v_mensaje = "NSS con saldo en garantía"
      ELSE
         -- se revisa si es un credito tradicional
         SELECT tpo_originacion, marca_inf
         INTO   v_tpo_originacion, v_marca_infonavit
         FROM   cat_tipo_credito
         WHERE  tpo_originacion = 1
         AND    marca_inf = v_i_estado_marca;
      
         -- si se encontro
         IF ( v_marca_infonavit IS NOT NULL ) THEN
            -- el NSS esta en proceso de credito
            LET v_mensaje = "NSS con crédito vigente"
         ELSE
            IF ( v_i_estado_marca = 280 ) THEN
               -- marca de separacion de cuentas
               LET v_mensaje = "NSS en separación de cuentas"
            ELSE
               -- si se trata del mismo tipo de retiro, es decir, disposicion
               IF ( v_i_estado_marca = 805 ) THEN
                  -- se marca como registro duplicado
                  LET v_mensaje = "NSS en retiro"
               ELSE
                  -- se trata de otra marca
                  LET v_mensaje = "NSS en otro proceso operativo"
               END IF -- marca de retiro
            END IF -- separacion de cuentas
         END IF -- marca credito tradicional
      END IF  -- marca de credito con garantia
   END IF -- marca de unificacion

   RETURN v_mensaje
END FUNCTION


{======================================================================
Clave: 
Nombre: fn_consulta_grupo_tesofe_salida_grupo_subcuenta
Fecha creacion: Mayo 14, 2013 
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el grupo de salida de tesofe y el grupo de subcuenta de pago
para un grupo de tesofe de entrada

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_grupo_tesofe_salida_grupo_subcuenta(v_grupo_entrada)
DEFINE v_grupo_entrada   CHAR(4),
       v_grupo_salida    CHAR(4),
       v_grupo_subcuenta SMALLINT

   -- se verifica que cuentas se deben consultar de acuerdo al grupo que llego como parametro
   -- ret_grupo_tesofe
   SELECT grupo_salida, grupo
   INTO   v_grupo_salida, v_grupo_subcuenta
   FROM   ret_grupo_tesofe
   WHERE  grupo_entrada = v_grupo_entrada
   
   -- se devuelve el resultado de la consulta
   RETURN v_grupo_salida, v_grupo_subcuenta
END FUNCTION