--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW32                                                  #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO GENERICO                                         #
#FECHA INICIO      => 18-SEP-2013                                             #
# Autor           Fecha      Modificación                                                    #
# Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
#                 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
# Ricardo Perez   20140616   Se modifican las consultad para obtener el id_solicitud
# *****                      se le quita la condicion del RFC, se deja solo para
#                            el Fondo de Ahorro  
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
DEFINE ws_ret_generico_solicitud_in RECORD
         nss              STRING, -- nss del trabajador
         rfc              STRING, -- rfc del trabajador
         caso_crm         STRING, -- numero de caso CRM
         causal_retiro    STRING,
         nrp              STRING,
         f_inicio_pension STRING, -- fecha de inicio de pension
         medio_entrega    STRING,
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              tipo_beneficiario  STRING,
              clabe_bancaria     STRING,
              rfc                STRING,
              email              STRING,
              telefono           STRING,
              tel_movil          STRING,
              nombre             STRING,
              ap_paterno         STRING,
              ap_materno         STRING,
              entidad_federativa STRING
         END RECORD
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
            nss                  STRING, --- Número de seguridad social del trabajador
            rfc                  STRING, -- rfc del trabajador
            caso_crm             STRING,
            estado_solicitud     STRING, -- estado de la solicitud
            cod_rechazo          STRING, -- codigo de rechazo
            des_rechazo          STRING,   ---- *************************************++
            monto_pesos          STRING, -- saldo en pesos 
            monto_adicional      STRING, -- monto adicional a devolver en pesos 
            monto_total          STRING, -- monto total a devolver en pesos
            referencia_dap       STRING -- referencia cuando es pago por DAP
       END RECORD
         
DEFINE g_indice_retiro  SMALLINT -- indice del tipo de retiro consultado
DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws

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
         
-- CONSTANTES PARA CAUSALES DE RETIRO DE FONDO DE AHORRO
CONSTANT g_cons_fa_termino_relacion_laboral SMALLINT = 1,
         g_cons_fa_pension_imss             SMALLINT = 2,
         g_cons_fa_plan_privado_pension     SMALLINT = 3,
         g_cons_fa_defuncion                SMALLINT = 4
		 
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS22."
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
      CALL fn_crea_servicio_fondo_ahorro(TRUE)
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
  CALL fn_crea_servicio_fondo_ahorro(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor... 20140616")

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
            CALL ERRORLOG(g_msg_procesada)
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_res_sin_solicitud
            CALL ERRORLOG(g_msg_sin_solicitud)
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_res_desconectado_del_servidor
            CALL ERRORLOG(g_msg_desconectado_del_servidor)
            CALL ERRORLOG(g_msg_procesada)
            EXIT PROGRAM   # The Application server has closed the connection
           
         WHEN g_res_conexion_con_cliente_perdida
            CALL ERRORLOG(g_msg_conexion_con_cliente_perdida)
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_res_servidor_interrumpido_ctrl_c
            CALL ERRORLOG(g_msg_servidor_interrumpido_ctrl_c)
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_msg_error_interno
            CALL ERRORLOG(g_msg_error_interno)
            CALL ERRORLOG(g_msg_procesada)
           
         OTHERWISE 
            -- se recibio algun otro codigo de retorno
            CALL ERRORLOG("Se recibio otro codigo de retorno")
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
Nombre: fn_crea_servicio_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_fondo_ahorro(p_generar_WSDL)
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
    LET v_webservice = com.WebService.CreateWebService("retiroCreaSolicitudFondoAhorro", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_solicitud_fondo_ahorro","fn_solicitud_fondo_ahorro",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL v_webservice.publishOperation(op, "fn_solicitud_fondo_ahorro")

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
       -- Registro del servicio
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
Nombre: fn_solicitud_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014.             - se agrega referencia DAP a la respuesta del servicio
Ivan Vega      05mar2014              - Si se tiene mas de una modalidad de retiro en la solicitud,
                                        todos los registros deben tener cuenta CLABE incliuyendo
										Fondo de Ahorro
======================================================================
}
FUNCTION fn_solicitud_fondo_ahorro()
DEFINE v_indice_retiro       SMALLINT,
       v_nss                 LIKE afi_fondo72.nss,
       v_rfc                 LIKE afi_fondo72.rfc,
       v_indice_modalidad    SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
	   v_requiere_DAP        SMALLINT, -- booleana que indica si se necesita DAP
       v_modalidad_procesada SMALLINT,  -- Indica si ya se proceso una solicitud de ley 73
       v_id_derechohabiente  DECIMAL(9,0)
   
   -- se verifica si se esta solicitando eco
   IF ( UPSHIFT(ws_ret_generico_solicitud_in.nss) = "ECO" ) THEN
      -- se devuelve ECO
      LET ws_ret_generico_solicitud_out.nss = "ECO"
      LET ws_ret_generico_solicitud_out.rfc = "ECO"
      
      -- se indica que hay un registro
      LET g_indice_retiro = 1
      CALL fn_respuesta_ws("ECO", "ECO", 0, 0, 0, 0, 0, NULL)
   ELSE
      -- se asignan los valores de respuesta
      LET ws_ret_generico_solicitud_out.nss = ws_ret_generico_solicitud_in.nss
      LET ws_ret_generico_solicitud_out.rfc = ws_ret_generico_solicitud_in.rfc
      
      LET v_nss = ws_ret_generico_solicitud_in.nss
      LET v_rfc = ws_ret_generico_solicitud_in.rfc
      
      -- se inicia el indice del retiro que se va a consultar
      LET g_indice_retiro = 1
      
      -- se crea el registro de la peticion de solicitud de ws registro de solicitud
      CALL fn_registra_peticion_registro_solicitud(ws_ret_generico_solicitud_in.nss, ws_ret_generico_solicitud_in.rfc, 
                                                   ws_ret_generico_solicitud_in.caso_crm)
           RETURNING g_id_peticion
      
      -- se asume que todos traen cuenta CLABE
      LET v_cta_clabe_correcta = TRUE
	  
	  -- si se tiene mas de una modalidad de retiro, todas deben tener cuenta CLABE
     -- Se deja de validar la cuenta CLABE ya que se puede pagar por DAP
--      IF ws_ret_generico_solicitud_in.medio_entrega <> "2" THEN 
--         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength() 
--            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
--               -- se rechaza porque todos deben traer CLABE, incluyendo Fondo de Ahorro
--               LET v_cta_clabe_correcta = FALSE
--               EXIT FOR
--            END IF
--         END FOR
--      END IF 			

      
		 -- si no fue correcto, se rechaza la solicitud para todas las modalidades
		 IF ( NOT v_cta_clabe_correcta ) THEN
          CALL fn_respuesta_ws(v_nss, v_rfc, 2,
                               gi_solicitud_rechazada, gi_modalidad_multiple_sin_CLABE, 0, 0, NULL)
      END IF
	END IF
	  
   -- si paso la validacion de existencia de cuenta CLABE en modalidad multiple
   IF ( v_cta_clabe_correcta = TRUE ) THEN
      -- se verifica que modalidades se solicitaron
      IF ((v_modalidad_procesada = 1)) THEN 
         -- registro procesado con otra subcuenta
         LET v_modalidad_procesada = 1
      ELSE   
         -- se registran los detalles de modalidad de retiro
         CALL fn_registra_det_peticion_registro_solicitud(g_id_peticion,
                                                        2,
                                                        ws_ret_generico_solicitud_in.causal_retiro   ,
                                                        ws_ret_generico_solicitud_in.nrp             ,
                                                        ws_ret_generico_solicitud_in.f_inicio_pension)
             
         -- se asume que el registro de beneficiario es correcto
         LET v_existe_beneficiario = TRUE

         -- se asume que las cuentas clabe son correctas
         LET v_cta_clabe_correcta = TRUE
             
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
            -- se registra en la bitacora los datos de beneficiarios recibidos
            CALL fn_registra_peticion_registro_solicitud_benef(g_id_peticion                                                                                                                   ,
                                                             2                                          ,
                                                             v_indice_beneficiario                                                                                                           ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria    ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc               ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email             ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono          ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tel_movil         ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre            ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno        ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_materno        ,
                                                             ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa)

            -- debe traer todos los datos
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario   IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc                 IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email               IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono            IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre              IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno          IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa  IS NULL ) THEN

               -- los datos de un beneficiario estan mal, no procede la solicitud
               LET v_existe_beneficiario = FALSE
               EXIT FOR
            END IF     
         END FOR
         -- se verifica que la modalidad traiga beneficiarios
         IF ( NOT v_existe_beneficiario OR NOT v_cta_clabe_correcta ) THEN
            -- se verifica si es por falta de datos de un beneficiario
            IF ( NOT v_existe_beneficiario ) THEN
               CALL fn_respuesta_ws(v_nss, v_rfc, 2,
                                  gi_solicitud_rechazada, gi_solicitud_sin_beneficiarios, 0, 0, NULL)
            ELSE
               -- se rechaza por la estructura de la cuenta clabe
               CALL fn_respuesta_ws(v_nss, v_rfc, 2,
                                  gi_solicitud_rechazada, gi_esctructura_cta_clabe_incorrecta, 0, 0, NULL)               
            END IF
         ELSE
            LET v_requiere_DAP = FALSE

            -- se busca si todos los beneficiarios tienen cuenta CLABE
            FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength() 
               IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
                  -- se requiere DAP en al menos uno
                  LET v_requiere_DAP = TRUE
                  EXIT FOR
               END IF
            END FOR

            -- se valida Fondo de Ahorro
            CALL fn_ret_disponibilidad_fondo_ahorro(v_nss, v_rfc, 
                                                  ws_ret_generico_solicitud_in.causal_retiro,
                                                  ws_ret_generico_solicitud_in.nrp,
                                                  ws_ret_generico_solicitud_in.f_inicio_pension,
                                                  v_indice_modalidad, v_requiere_DAP)
                                                                    
             
         END IF
      END IF
   END IF
END FUNCTION

{
======================================================================
Nombre: fn_ret_disponibilidad_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene disponibilidad de retiro de su cuenta
de fondo anterior

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014              - se agrega la referencia DAP a la salida del servicio
======================================================================
}
FUNCTION fn_ret_disponibilidad_fondo_ahorro(p_nss, p_rfc, p_causal, p_nrp, v_f_inicio_pension, p_indice_modalidad, p_requiere_DAP)
DEFINE p_nss              CHAR(11), -- NSS
       p_rfc              CHAR(13), -- RFC
       p_causal           SMALLINT, -- causal de retiro
       p_nrp              CHAR(11), -- NRP
       v_f_inicio_pension CHAR(8), -- fecha de inicio de pension en formato AAAAMMDD
       p_indice_modalidad SMALLINT, -- indice de modalidad de retiro en la solicitud
	   p_requiere_DAP     SMALLINT, -- booleana que indica si se necesita DAP
       v_f_cadena         VARCHAR(10), -- fecha formateada para transformar a DATE
       v_f_pension        DATE, -- fecha de inicio de pension en formato DATE
       v_count_bnd        SMALLINT   ,
       v_cod_rechazo      SMALLINT   ,
       v_conteo_nss       SMALLINT   ,
       v_ruta_ejecutable  VARCHAR(40),
       v_ruta_log         STRING,
       v_cadena           STRING,
       v_id_afi_fondo72   DECIMAL(9,0), -- id derechohabiente
       v_saldo            DECIMAL(22,2), -- saldo del derechohabiente
       v_id_solicitud     LIKE ret_solicitud_generico.id_solicitud,  -- id de la solicitud
       v_tipo_credito     SMALLINT,
       v_tipo_originacion SMALLINT 
      
   -- saldos segun retiro (ret_modalidad_retiro)
   -- FONDO DE AHORRO    
       
   -- se verifica si se tiene NSS y RFC
   IF ( p_nss IS NOT NULL AND p_rfc IS NOT NULL ) THEN

      -- para validar el NSS se verifica que exista al menos una vez
      SELECT COUNT(*)
      INTO   v_conteo_nss 
      FROM   afi_fondo72
      WHERE  nss               = p_nss
      AND    ind_estado_cuenta = 0    -- Cuenta Activa
--      AND    rfc               = p_rfc
      
      IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
         -- si no se encontraron coincidencias
         CALL ERRORLOG("No existe el nss-rfc")
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0,0, NULL)
         RETURN    
      ELSE
         -- si se encuentra mas de uno, no procede su solicitud
         IF ( v_conteo_nss > 1 ) THEN
            CALL ERRORLOG("El RFC/NSS devuelve más de un registro")
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0,0, NULL)
            RETURN
         END IF
      END IF
   ELSE
      -- se verifica si se recibio NSS
      IF ( p_nss IS NOT NULL ) THEN
      
         -- para validar el NSS se verifica que exista al menos una vez
         SELECT COUNT(*)
         INTO   v_conteo_nss 
         FROM   afi_fondo72
         WHERE  nss               = p_nss
         AND    ind_estado_cuenta = 0    -- Cuenta Activa
         
         IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
            -- No existe el nss
            CALL ERRORLOG("No existe el NSS")
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0,0, NULL)
            RETURN
         ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            IF ( v_conteo_nss > 1 ) THEN
               CALL ERRORLOG("El NSS devuelve más de un registro")
               CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0,0, NULL)
               RETURN
            END IF
         END IF 
      ELSE
            CALL ERRORLOG("No existe el NSS")
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0,0, NULL)
      END IF
   END IF
  
   DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
   DISPLAY "NSS evaluado: ", p_nss

   LET v_conteo_nss          = 0
   
   IF ( p_nss IS NOT NULL ) THEN
      CALL ERRORLOG("Validando solicitud para NSS: " || p_nss)
   END IF
   
   -- se valida si existe otra solicitud en tramite
   IF ( fn_rechazo_por_tramite_fondo_ahorro(p_nss, p_rfc) ) THEN
      CALL ERRORLOG("Se rechaza porque existe otra solicitud en tramite para el mismo NSS")
      -- se responde al WS que se tiene una solicitud en tramite
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0,0, NULL)
      RETURN 
   END IF
   
   -- si no se rechazo por tramite    
   -- valida el causal de retiro seleccionado 
   SELECT COUNT(*) 
   INTO   v_count_bnd
   FROM   ret_causal_retiro
   WHERE  causal_retiro = p_causal;   

   DISPLAY "Verifica causal retiro :", v_count_bnd

   IF ( v_count_bnd < 1 OR v_count_bnd IS NULL ) THEN
      -- se le regresa el cliente rechazo ya qe no coincide con la tabla de causal retiro
      -- debe ser entre 01-04
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0,0, NULL)     
      CALL ERRORLOG("El causal de retiro es invalido")
      RETURN 
   END IF      

   -- se obtiene el id de solicitud creada en el marcado
   CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud
   DISPLAY "Solicitud encontrada en inicio de verificacion de retFA: ", v_id_solicitud

  
   -- se verifica si el derechohabiente tiene un credito vigente
   IF ( fn_trabajador_credito_vigente(p_nss) ) THEN
      -- se responde negavito por tener un credito
      CALL fn_trabajador_tipo_credito(p_nss) RETURNING v_tipo_credito, v_tipo_originacion
      IF v_tipo_credito = 1 AND v_tipo_originacion = 1 THEN -- 1-Credito Tradicional, 1-Transferencia de Acreditados

          CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_tiene_credito_vigente, 0,0, NULL)

          CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_tiene_credito_vigente, 0, 0, "NA", v_id_solicitud, p_indice_modalidad,0)
          DISPLAY "Rechazada por credito vigente"
          RETURN
      END IF 
   END IF
   
   -- se verifica si tiene saldo
   CALL fn_recupera_saldo_fa(p_nss, p_rfc) RETURNING v_saldo
   
   IF ( v_saldo <= 0 ) THEN
      -- solicitud rechazada por no contar con saldo suficiente
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_saldo, 0,0, NULL)
      CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_sin_saldo, 0, 0, "NA", v_id_solicitud, p_indice_modalidad, 0)
      DISPLAY "Rechazada sin saldo"
      RETURN
   END IF
   
   -- se transforma la fecha AAAAMMDD a formato fecha
   LET v_f_cadena = v_f_inicio_pension[5,6], "/", v_f_inicio_pension[7,8], "/", v_f_inicio_pension[1,4]
   LET v_f_pension = DATE(v_f_cadena)
     
   -- se realiza la verificacion de la solicitud de acuerdo al tipo de causal de retiro recibida
   CASE p_causal
      WHEN 1 -- termino de relacion laboral
         CALL fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, v_saldo, p_indice_modalidad, p_requiere_DAP)
      
      WHEN 2 -- pension IMSS
         DISPLAY "IMSS"
         CALL fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, v_saldo, p_indice_modalidad, p_requiere_DAP)
      
      WHEN 3 -- plan privado de pension
         DISPLAY "Plan privado pension"
         CALL fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, v_saldo, v_f_pension, p_indice_modalidad, p_requiere_DAP)
      
      WHEN 4 -- plan privado
         DISPLAY "defuncion"
         CALL fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_f_pension, v_saldo, p_indice_modalidad, p_requiere_DAP)
      
      OTHERWISE 
         -- causal de retiro invalido
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0,0, NULL)
         
   END CASE

END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_termino_relacion_laboral
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal termino de relacion laboral
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    30 Abril 2013         - Ahora se recibe NSS y/o RFC.
                                     . Si se recibe NSS se valida una pension vigente en el SPESS
                                     . Si no se recibe NSS, con el RFC se busca 
Ivan Vega    25Feb2014             - Se elimmina la validacion de la prescripcion
======================================================================
}
FUNCTION fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, p_saldo, p_indice_modalidad, p_requiere_DAP)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_indice_modalidad      SMALLINT, -- indice de la modalidad de retiro en la solicitud
	    p_requiere_DAP          SMALLINT,
       r_edad                  SMALLINT      ,
       v_ejecucion_ws          SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional       DECIMAL(12,2),
       v_referencia_banc       CHAR(12),
       v_solicitud_previa      SMALLINT, -- indica si existe una solicitud previa
       v_fecha_ultima_relacion_laboral DATE, -- ultima fecha de relacion laboral
       v_fecha_nacimiento              DATE, -- fecha de nacimiento del derechohabiente
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_solicitud          LIKE ret_fondo_ahorro.id_solicitud,
       v_id_datamart           LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_con_sin_rel_lab       SMALLINT -- indica si tiene o no relacion laboral actualmente
   
   -- se asume que no hay referencia DAP
   LET v_referencia_banc = NULL
   
   -- se calcula la edad del derechohabiente
   IF ( p_nss IS NOT NULL AND p_nss <> "00000000000" ) THEN
      -- se calcula usando su nss
      CALL fn_edad_derechohabiente(p_nss) RETURNING r_edad
      DISPLAY "Edad calculada por NSS: ", r_edad
   ELSE
      -- se calcula usando su rfc
      CALL fn_edad_derechohabiente_rfc(p_rfc) RETURNING r_edad
      DISPLAY "Edad calculada por RFC: ", r_edad
   END IF
  
   -- se obtiene el id de solicitud creada en el marcado
   CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud
   DISPLAY "Solicitud encontrada RelLab: ", v_id_solicitud

   -- si la solicitud existe
   IF ( v_id_solicitud IS NOT NULL ) THEN
  
      -- se obtiene la fecha de nacimiento
      LET v_fecha_nacimiento = fn_fecha_nacimiento_rfc(p_rfc)
      
      -- la edad debe ser mayor o igual a 50 anos
      IF ( r_edad >= 50 ) THEN
      
         -- se verifica que el trabajador tenga una resolucion en el spess
         CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
         
         -- si no tiene resolucion valida en el spess
         IF ( NOT v_tiene_spess ) THEN
               

            -- para verificar la relacion labora, se invoca el web service de consulta
            CALL fn_fecha_ultima_relacion_laboral(p_nss)
            RETURNING v_ejecucion_ws, v_fecha_ultima_relacion_laboral, v_con_sin_rel_lab
            
            CALL ERRORLOG("Ejecucion WS: ")
            CALL ERRORLOG(v_ejecucion_ws)		   
            CALL ERRORLOG("Fec. de ultima relacion laboral:")
            CALL ERRORLOG(v_fecha_ultima_relacion_laboral)
		    
            -- si se ejecuto correctamente el WS
            IF ( v_ejecucion_ws = 0 ) THEN

               -- se verifica si tiene un ano sin relacion laboral
               IF ( NOT fn_verifica_ano_sin_relacion_laboral(v_fecha_ultima_relacion_laboral, TODAY) ) THEN
                  -- derechohabiente sin resolucion de spess ni ano sin relacion laboral
                 IF v_fecha_ultima_relacion_laboral IS NULL THEN 
                     IF v_con_sin_rel_lab = 1 THEN 
                         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, 0,0, v_referencia_banc)
                         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
                     ELSE 
                          IF ( p_requiere_DAP ) THEN
                             LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
                          ELSE
                             LET v_referencia_banc = NULL
                          END IF

                          CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo, 0, v_referencia_banc)
                          -- se genera la solicitud
                          CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, 10, 0, p_saldo, 0, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
                     END IF 
                 ELSE 
                     CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_un_ano_relacion_laboral, 0,0, v_referencia_banc)
                      
                     CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_sin_un_ano_relacion_laboral, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
                 END IF 
                  
               ELSE
                  -- 25feb2014. Se elimina la validacion de la prescripcion
                  -- el saldo es retirable
                  -- se genera la referencia bancaria
                  IF ( p_requiere_DAP ) THEN
                     LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
                  ELSE
                     LET v_referencia_banc = NULL
                  END IF
			      
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo, 0, v_referencia_banc)
                  
                  -- se genera la solicitud
                  CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, 10, 0, p_saldo, 0, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
                  
               END IF
			   ELSE
               -- se rechaza porque se detecto un error al consultar el WS de ultima relacion laboral
               -- por lo tanto no es posible validar la solicitud
               CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_ws_rel_laboral_no_disponible, 0,0, v_referencia_banc)
			   END IF
            
         ELSE
            -- 25Feb2014. Se elimina la validacion de la prescripcion               
            -- se genera la referencia bancaria
            IF ( p_requiere_DAP ) THEN
               LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
            ELSE
               LET v_referencia_banc = NULL
            END IF
            
            -- el monto retirable es dos veces el saldo y es retirable
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,p_saldo, v_referencia_banc)
                           
            -- se genera la solicitud
            CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, 10, 0, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
         END IF
      ELSE 
         -- solicitud rechazada por que el trabajador no tiene 50 anos cumplidos o mas
         DISPLAY "Solicitud rechazada porque el derechohabiente no tiene 50 anos o mas"
         -- se responde la consulta
         -- edad inferior a 50 anos
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_edad_inferior_50_anos, 0,0, v_referencia_banc)
         
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_edad_inferior_50_anos, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_termino_relacion_laboral)
      END IF
   ELSE
      -- no se encontro una solicitud de fondo de ahorro
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_solicitud, 0,0, v_referencia_banc)
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_resolucion_pension_imss
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones de una solicitud de retiro de Fondo de Ahorro
por resolucion de pension del IMSS

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     25Feb2014               - Se elimina la validacion de la prescripcion
                                      - Se agrega la referencia DAP a la salida del servicio
======================================================================
}
FUNCTION fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, p_saldo, p_indice_modalidad, p_requiere_DAP)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_indice_modalidad      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
	    p_requiere_DAP          SMALLINT,
       r_edad                  SMALLINT      ,
       r_b_paso                SMALLINT      ,
       v_tanto_adicional       DECIMAL(12,2),
       v_id_solicitud          LIKE ret_solicitud_generico.id_solicitud, -- id de la solicitud del retiro
       v_referencia_banc       CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart           LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_tpo_pension           LIKE ret_datamart.tpo_pension, -- tipo de pension
       v_f_resolucion          LIKE ret_datamart.f_resolucion, -- fecha de resolucion en el SPESS
       v_anos_prescripcion     SMALLINT,
       v_error_det             SMALLINT 

   -- se asume que no hay referencia bancaria
   LET v_referencia_banc = NULL

   -- se revisa que venga nss, ya que sin este es necesario para todo el proceso
   IF ( p_nss IS NULL OR p_nss = "00000000000" ) THEN
      -- caso invalido para pension del IMSS, se necesita un NSS valido
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_es_necesario, 0,0, v_referencia_banc)
      
      -- no puede seguir el proceso
      RETURN
   END IF

   -- se obtiene el id de solicitud creada en el marcado
   CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud
   DISPLAY "Solicitud encontrada PIMSS: ", v_id_solicitud

   -- si la solicitud existe
   IF ( v_id_solicitud IS NOT NULL ) THEN
   
      -- se verifica que el trabajador tenga una resolucion de pension valida
      CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
      
      IF ( v_tiene_spess ) THEN
         -- se obtiene la fecha de resolucion
         SELECT f_inicio_pension, tpo_pension
         INTO   v_f_resolucion, v_tpo_pension
         FROM   ret_datamart
         WHERE  id_datamart = v_id_datamart
      
	     -- se genera la referencia bancaria
		 IF ( p_requiere_DAP ) THEN
            LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, v_tpo_pension)
	     ELSE
		    LET v_referencia_banc = NULL
		 END IF
	  
	     -- 25feb2014. Se elmina la validacion de la prescripcion
         -- el saldo es retirable (EL DOBLE)
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,p_saldo, v_referencia_banc)
                  
         -- se genera la solicitud
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, 10, 0, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_pension_imss)
      
      ELSE
         -- no tiene resolucion en el SPESS
          CALL fn_detalle_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart, v_error_det
          IF (v_error_det <> gi_resolucion_neg_pension AND v_error_det <> gi_porcentaje_menor_50) THEN 
              CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_resolucion_spess, 0,0, v_referencia_banc)
              CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_sin_resolucion_spess, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_pension_imss)
          ELSE 
              IF v_error_det = gi_resolucion_neg_pension THEN 
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_resolucion_neg_pension, 0,0, v_referencia_banc)
                  CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_resolucion_neg_pension, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_pension_imss)
              END IF 
              IF v_error_det = gi_porcentaje_menor_50 THEN 
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_porcentaje_menor_50, 0,0, v_referencia_banc)
                  CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_porcentaje_menor_50, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_pension_imss)
              END IF 
          END IF 
      END IF
   ELSE
      -- no se encontro una solicitud de fondo de ahorro
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_solicitud, 0,0, v_referencia_banc)
   END IF

END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_plan_privado_pension
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal plan privado de pension 
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     25feb2014               - Se elimina la validacion de la prescripcion
                                      - se agrega la referencia bancaria a la respuesta del servicio
======================================================================
}
FUNCTION fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, p_saldo, v_fecha_inicio_pension, p_indice_modalidad, p_requiere_DAP)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_nrp                   LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_fecha_inicio_pension  DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_indice_modalidad      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
	   p_requiere_DAP          SMALLINT,
       r_edad                  SMALLINT      ,
       r_b_paso                SMALLINT      ,
       v_tanto_adicional       DECIMAL(12,2),
       v_referencia_banc       CHAR(12),
       v_id_solicitud          LIKE ret_solicitud_generico.id_solicitud,
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_diferencia_anos       SMALLINT, -- diferencia en anos
       v_f_inicio_pension      LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_id_datamart           LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess

   -- se obtiene el id de solicitud creada en el marcado
   CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud
   DISPLAY "Solicitud encontrada PPP: ", v_id_solicitud

   -- se asume que no hay referencia DAP
   LET v_referencia_banc = NULL
   
   -- si la solicitud existe
   IF ( v_id_solicitud IS NOT NULL ) THEN

      -- se verifica si el NRP existe en catalogo
      IF ( fn_nrp_existe_en_catalogo(p_nrp) ) THEN
         -- 25feb2014. Se elimina la validacion de la prescripcion
		 IF ( p_requiere_DAP ) THEN
            LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
		 ELSE
		    LET v_referencia_banc = NULL
		 END IF
		 
         -- el saldo es retirable (EL DOBLE)
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,p_saldo, v_referencia_banc)
               
         -- se genera la solicitud
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, 10, 0, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_plan_privado_pension)
      ELSE
         -- se rechaza por inexistencia de NRP
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nrp_no_encontrado, 0,0, v_referencia_banc)
         
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_nrp_no_encontrado, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_plan_privado_pension)
      END IF
   ELSE
      -- no se encontro una solicitud de fondo de ahorro
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_solicitud, 0,0, v_referencia_banc)
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retiro_fa_defuncion
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal defuncion
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014              - Se elimina la validacion de la prescripcion
                                      - se agrega la referencia DAP a la respuesta del servicio
======================================================================
}
FUNCTION fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_fecha_inicio_pension, p_saldo, p_indice_modalidad, p_requiere_DAP)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_nrp                   LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_fecha_inicio_pension  DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_indice_modalidad      SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
	   p_requiere_DAP          SMALLINT, 
       r_edad                  SMALLINT      ,
       v_ejecucion_ws          SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional       DECIMAL(12,2),
       v_referencia_banc       CHAR(12),
       v_id_solicitud          LIKE ret_solicitud_generico.id_solicitud,
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_f_inicio_pension      LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_f_ultima_rel_laboral  DATE, -- fecha de ultima relacion laboral
       v_diferencia_anos       SMALLINT, -- diferencia en anos de dos fechas
       v_id_datamart           LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_con_sin_rel_lab       SMALLINT -- indica si tiene o no relacion laboral

   -- se obtiene el id de solicitud creada en el marcado
   CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 2, 8) RETURNING v_id_solicitud
   DISPLAY "Solicitud encontrada defuncion: ", v_id_solicitud

   -- se asume que no hay referencia
   LET v_referencia_banc = NULL
   
   -- si la solicitud existe
   IF ( v_id_solicitud IS NOT NULL ) THEN

      -- se verifica que el trabajador tenga una resolucion de pension valida
--      CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
      
--      IF ( v_tiene_spess ) THEN
         -- 25feb2014. Se elimina la validacion de la prescripcion
         IF ( p_requiere_DAP ) THEN
            LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
         ELSE
            LET v_referencia_banc = NULL
         END IF
		  	
         -- el saldo es retirable (EL DOBLE)
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,p_saldo, v_referencia_banc)
         
         -- se genera la solicitud
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_aceptada, 0, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
--      ELSE
--	     CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_sin_resolucion_spess, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
         -- para verificar la relacion labora, se invoca el web service de consulta
         --CALL fn_fecha_ultima_relacion_laboral(p_nss)
         --RETURNING v_ejecucion_ws, v_f_ultima_rel_laboral, v_con_sin_rel_lab
--
         --CALL ERRORLOG("Ejecucion WS: ")
         --CALL ERRORLOG(v_ejecucion_ws)		   
         --CALL ERRORLOG("Fec. de ultima relacion laboral:")
         --CALL ERRORLOG(v_f_ultima_rel_laboral)
		 --
         -- si se ejecuto correctamente el WS
         --IF ( v_ejecucion_ws = 0 ) THEN
--
            -- se verifica si la fecha de defuncion debe ser mayor o igual a la fecha de la ultima relacion laboral
            --IF ( v_f_inicio_pension >= v_f_ultima_rel_laboral ) THEN
               -- 25feb2014. Se elimina la validacion de la prescripcion
               --IF ( p_requiere_DAP ) THEN
                  --LET v_referencia_banc = fn_genera_referencia_bancaria(p_causal, null)
               --ELSE
                  --LET v_referencia_banc = NULL
               --END IF
		  	 --
               -- el saldo es retirable (EL DOBLE)
               --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,p_saldo, v_referencia_banc)
               --
               -- se genera la solicitud
               --CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_aceptada, 0, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
		      --ELSE
               -- se rechaza
                  --IF v_con_sin_rel_lab = 0 THEN 
                       --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_f_pension_mayor_f_ult_rel_laboral, 0,0, v_referencia_banc)
                       --
                       --CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_f_pension_mayor_f_ult_rel_laboral, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
                  --ELSE 
                     --IF v_con_sin_rel_lab = 1 THEN -- con relacion laboral actual
                         --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, 0,0, v_referencia_banc)
                         --CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
                     --ELSE 
                         --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_rel_laboral, 0,0, v_referencia_banc)
                         --CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada, gi_no_existe_rel_laboral, p_saldo, p_saldo, v_referencia_banc, v_id_solicitud, p_indice_modalidad, g_cons_fa_defuncion)
                     --END IF 
--
--
                  --END IF 
            --END IF
		   --ELSE
            -- se rechaza porque se detecto un error al consultar el WS de ultima relacion laboral
            -- por lo tanto no es posible validar la solicitud
            --CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_ws_rel_laboral_no_disponible, 0,0, v_referencia_banc)
         --END IF	  
--      END IF
   ELSE
      -- no se encontro una solicitud de fondo de ahorro
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_no_existe_solicitud, 0,0, v_referencia_banc)
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la validacion de disponibilidad del retiro 
de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     25feb2014               - Se agrega la referencia DAP al registro de salida del servicio
Ivan Vega     05mar2014               - Si el monto excede 1000 (mil) pesos y viene por DAP, entonces
                                        se rechaza
======================================================================
}
FUNCTION fn_respuesta_ws_fondo_ahorro(p_estado_solicitud, p_cod_rechazo, p_importe_viv72, p_tanto_adicional, p_referencia_DAP)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv72    DECIMAL(12,2), -- Importe de vivienda 72
		 p_tanto_adicional  DECIMAL(12,2), -- Importe de vivienda 72
		 v_importe_total    DECIMAL(12,2), -- importe total: viv72 + tanto adicional
		 p_referencia_DAP   CHAR(12),
       v_desc_rechazo     CHAR(100)

   -- se suman los importes
   LET v_importe_total = p_importe_viv72 + p_tanto_adicional
		 
   -- se responde la generacion de solicitud de retiro de fondo de ahorro
   LET ws_ret_generico_solicitud_out.estado_solicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.cod_rechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.monto_pesos      = p_importe_viv72
   LET ws_ret_generico_solicitud_out.monto_adicional  = p_tanto_adicional
   LET ws_ret_generico_solicitud_out.monto_total      = v_importe_total

   -- 25feb2014. referencia DAP
   LET ws_ret_generico_solicitud_out.referencia_dap = p_referencia_DAP
   
   -- 05mar2014. Si el monto excede mil pesos y se envio por DAP, se rechaza la solicitud
   -- Se quita la validacaión por petición del usuario, se debe pagar por cualquier medio
--   IF ( p_importe_viv72 > 1000 AND p_referencia_DAP IS NOT NULL ) THEN
--      -- se rechaza por monto superior en pago DAP
--      LET p_estado_solicitud = gi_solicitud_rechazada
--      LET p_cod_rechazo      = gi_monto_excedido_pago_dap

--      LET ws_ret_generico_solicitud_out.estado_solicitud = p_estado_solicitud
--      LET ws_ret_generico_solicitud_out.cod_rechazo      = p_cod_rechazo
--   END IF

   LET ws_ret_generico_solicitud_out.des_rechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET ws_ret_generico_solicitud_out.des_rechazo = "";
      LET v_desc_rechazo = ""
      SELECT des_larga
      INTO   v_desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      IF v_desc_rechazo IS NULL THEN
         LET ws_ret_generico_solicitud_out.des_rechazo = " ";
      ELSE 
         LET ws_ret_generico_solicitud_out.des_rechazo = v_desc_rechazo
      END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, 2, 40,
                                                         p_estado_solicitud, p_cod_rechazo, 0,
                                                         v_importe_total, p_referencia_DAP)

END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_respuesta_ws
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25Feb2014              - Se agrega referencia DAP a la respuesta del WS
======================================================================
}
FUNCTION fn_respuesta_ws(p_nss, p_rfc, p_modalidad, p_estado_solicitud, p_cod_rechazo, p_aivs, p_pesos, p_referencia_dap)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         p_modalidad        SMALLINT, -- modalidad de retiro
         p_estado_solicitud SMALLINT, -- estado de la solicitud
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_aivs             DECIMAL(24,6), -- importe en aivs
         p_pesos            DECIMAL(22,2), -- importe en pesos
		   p_referencia_dap   CHAR(12), -- referencia DAP
         v_desc_rechazo     CHAR(100)
       

   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.estado_solicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.cod_rechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.monto_pesos      = p_pesos
   
   -- 25feb2014. Referencia DAP
   LET ws_ret_generico_solicitud_out.referencia_dap = p_referencia_dap

   LET ws_ret_generico_solicitud_out.des_rechazo = " ";
   LET v_desc_rechazo = ""
   IF p_cod_rechazo <> 0 THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET ws_ret_generico_solicitud_out.des_rechazo = "";
      SELECT des_larga
      INTO   v_desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      IF v_desc_rechazo IS NULL THEN
         LET ws_ret_generico_solicitud_out.des_rechazo = " ";
      ELSE 
         LET ws_ret_generico_solicitud_out.des_rechazo = v_desc_rechazo
      END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, p_modalidad, 0,
                                                         p_estado_solicitud, p_cod_rechazo, p_aivs,
                                                         p_pesos, NULL)
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_genera_solicitud_retiro_fa
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     05mar2014               - Si el monto de viv72 es superior al maximo permitido en DAP,
                                        y el pago es por este medio (1000 mil pesos), la solicitud se rechaza
======================================================================
}
FUNCTION fn_genera_solicitud_retiro_fa(p_nss, p_rfc, p_estatus, p_rechazo, p_saldo, p_tanto_adicional,
                                       p_referencia_bancaria, p_id_solicitud, p_indice_modalidad, p_causal_retiro)								   
DEFINE p_nss                         LIKE afi_derechohabiente.nss, 
       p_rfc                         LIKE afi_derechohabiente.rfc,
       p_estatus                     SMALLINT      , -- estatus de la solicitud
       p_rechazo                     SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_saldo                       DECIMAL(12,2), -- saldo calculado
       p_tanto_adicional             DECIMAL(12,2), -- tanto adicional
       p_referencia_bancaria         CHAR(12), -- clave de referencia bancaria
       p_id_solicitud                LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       p_indice_modalidad            SMALLINT, -- indice de la modalidad de retiro en la solicitud
	   p_causal_retiro               SMALLINT, -- causal del retiro de fondo de ahorro
       v_id_afi_fondo72              LIKE afi_fondo72.id_afi_fondo72, -- id del trabajador
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente, -- id del trabajador en AFI_DER
       v_estatus                     SMALLINT      ,
       v_resultado                   SMALLINT      , -- resultado de la ejecucion
       v_conteo                      SMALLINT      , -- contador de beneficiarios
       f_saldo                       DATE          ,
       h_saldo                       VARCHAR(20)   ,
       v_subcuenta                   SMALLINT      , -- subcuenta de retiro
       v_marca_fondo_ahorro          LIKE sfr_marca.marca, -- marca de fondo de ahorro
       v_saldo_poseido               LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       r_ret_fondo_ahorro_generico   RECORD LIKE ret_fondo_ahorro_generico.*, -- registro de fondo de ahorro generico
       v_r_ret_beneficiario_generico RECORD LIKE ret_beneficiario_generico.*, -- registro de beneficiario del retiro
       v_r_ret_pago_spei             RECORD LIKE ret_pago_spei.*, -- registro para pago por SPEI
       v_r_ret_pago_dap              RECORD LIKE ret_pago_dap.*, -- registro para pago por DAP
       v_sql                         STRING, -- cadena con enunciado SQL
       v_tipo_pago                   SMALLINT, -- tipo de pago
       v_caso_crm                    CHAR(10)
       
   
   -- marca de fondo de ahorro
   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_subcuenta          = 40
   LET v_caso_crm           = ws_ret_generico_solicitud_in.caso_crm
    
   -- se obtiene el id_derechohabiente 
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   ret_solicitud_generico
   WHERE  id_solicitud = p_id_solicitud
   
   -- se obtiene el id_afi_fondo72
   SELECT MAX(id_afi_fondo72)
   INTO   v_id_afi_fondo72
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    ind_estado_cuenta = 0   -- Cuenta Activa
--   AND    rfc = p_rfc
	  
   -- si la solicitud fue aceptada
   IF ( p_estatus = gi_solicitud_aceptada ) THEN
    
      -- se actualiza la tabla de control de la solicitud a estado 10 (capturada)
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 10,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud
      
	  -- se asignan los datos al registro de fondo de ahorro generico
      LET r_ret_fondo_ahorro_generico.id_solicitud       = p_id_solicitud
      LET r_ret_fondo_ahorro_generico.id_derechohabiente = v_id_derechohabiente
      LET r_ret_fondo_ahorro_generico.id_afi_fondo72     = v_id_afi_fondo72
      LET r_ret_fondo_ahorro_generico.causal_retiro      = p_causal_retiro
      LET r_ret_fondo_ahorro_generico.id_datamart        = NULL
      LET r_ret_fondo_ahorro_generico.f_ult_rel_laboral  = NULL
      LET r_ret_fondo_ahorro_generico.f_inicio_pension   = NULL --ws_ret_generico_solicitud_in.arr_modalidad_retiro[p_indice_modalidad].f_incio_pension
      LET r_ret_fondo_ahorro_generico.nrp                = ws_ret_generico_solicitud_in.nrp
      LET r_ret_fondo_ahorro_generico.f_defuncion        = NULL
      LET r_ret_fondo_ahorro_generico.folio              = 0
      LET r_ret_fondo_ahorro_generico.saldo_viv72        = p_saldo
      LET r_ret_fondo_ahorro_generico.tanto_adicional    = p_tanto_adicional
      LET r_ret_fondo_ahorro_generico.f_solicitud        = TODAY
      LET r_ret_fondo_ahorro_generico.f_captura          = TODAY
      LET r_ret_fondo_ahorro_generico.h_captura          = CURRENT HOUR TO MINUTE
      LET r_ret_fondo_ahorro_generico.estado_solicitud   = p_estatus
      LET r_ret_fondo_ahorro_generico.cod_rechazo        = p_rechazo
	  
      -- se inserta el registro de fondo de ahorro generico en su tabla
      INSERT INTO ret_fondo_ahorro_generico VALUES ( r_ret_fondo_ahorro_generico.* )
      
      -- se crean los registros para los beneficiarios de esta solicitud
      FOR v_conteo = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
         -- si se tiene cuenta clabe, es pago por SPEI
         IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabe_bancaria IS NOT NULL ) THEN
            -- SPEI
            LET v_tipo_pago = 1
         ELSE
            -- pago por DAP
            LET v_tipo_pago = 2
         END IF
		 
         -- 05mar2014. Si el importe de vivienda excede 1000 (mil) pesos y es pago por DAP,
         -- se rechaza por exceder monto maximo de pago por DAP
         -- Se deja de validar el monto para pago via DAP
--         IF ( v_tipo_pago = 2 AND p_saldo > 1000 ) THEN
--            -- la solicitud se rechaza
--            UPDATE ret_solicitud_generico
--            SET    estado_solicitud = gi_solicitud_rechazada    ,
--                  cod_rechazo      = gi_monto_excedido_pago_dap,
--                  caso_adai        = v_caso_crm
--            WHERE  id_solicitud     = p_id_solicitud
           
--            -- se actualiza la tabla de fondo de ahorro generico a rechazo
--            UPDATE ret_fondo_ahorro_generico
--            SET    estado_solicitud = gi_solicitud_rechazada    ,
--                   cod_rechazo      = gi_monto_excedido_pago_dap
--            WHERE  id_solicitud     = p_id_solicitud

--            -- se desmarca la cuenta
--            CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, v_marca_fondo_ahorro, p_id_solicitud,
--                                                         v_marca_fondo_ahorro, "safreviv", g_proceso_cod_ret_fondo_ahorro) -- fondo de ahorro
--         ELSE
            CALL fn_registra_beneficiario_retiro_generico(p_id_solicitud,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].tipo_beneficiario,
                                                          v_tipo_pago, -- FALTA TIPO DE PAGO
                                                          1, -- FALTA PARENTESCO
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].ap_paterno,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].ap_materno,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].nombre,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].telefono,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].email,
                                                          100,
                                                          0,
                                                          p_saldo + p_tanto_adicional,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabe_bancaria,
                                                          p_referencia_bancaria,
                                                          ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].entidad_federativa)
            
--         END IF
      END FOR
   ELSE
      -- la solicitud se rechaza
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estatus,
             cod_rechazo      = p_rechazo,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud
      
      -- se desmarca la cuenta
      CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, v_marca_fondo_ahorro, p_id_solicitud,
                                           v_marca_fondo_ahorro, "safreviv", g_proceso_cod_ret_fondo_ahorro) -- fondo de ahorro
   END IF

END FUNCTION 

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para registro de solicitud de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud(p_nss, p_rfc, p_caso_adai)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_rfc                     LIKE afi_derechohabiente.rfc,
       p_caso_adai               LIKE ret_solicitud_generico.caso_adai,
       p_res_ejecucion           SMALLINT, -- resultado de la ejecucion
       p_accion                  SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_peticion_crea_solicitud RECORD LIKE ret_ws_peticion_crea_solicitud.* -- registro de peticion al ws
		
   -- se obtiene el id de peticion nuevo
   SELECT seq_ret_ws_generico.nextVal
   INTO   p_id_peticion
   FROM   systables
   WHERE  tabid = 1
   
   -- se asignan los datos
   LET v_r_ret_ws_peticion_crea_solicitud.id_peticion   = p_id_peticion
   LET v_r_ret_ws_peticion_crea_solicitud.f_peticion    = TODAY
   LET v_r_ret_ws_peticion_crea_solicitud.h_peticion    = CURRENT HOUR TO SECOND
   LET v_r_ret_ws_peticion_crea_solicitud.nss           = p_nss
   LET v_r_ret_ws_peticion_crea_solicitud.rfc           = p_rfc
   LET v_r_ret_ws_peticion_crea_solicitud.caso_adai     = p_caso_adai
   
   -- se inserta el registro de peticion
   INSERT INTO ret_ws_peticion_crea_solicitud VALUES ( v_r_ret_ws_peticion_crea_solicitud.* )

   -- se devuelve el id de la peticion
   RETURN p_id_peticion
END FUNCTION

{
======================================================================
Nombre: fn_registra_det_peticion_registro_solicitud
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra el detalle de los datos enviados como encabezado de modalidad de retiro
en el servicio de registro de solicitud de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud(p_id_peticion, p_modalidad_retiro, p_causal_retiro,
                                                     p_nrp, p_f_inicio_pension)
DEFINE p_id_peticion                          DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro                     SMALLINT, -- modalidad de retiro
       p_causal_retiro                        SMALLINT, -- causal de retiro en fondo72
       p_nrp                                  CHAR(18), -- NRP del trabajador
       p_f_inicio_pension                     CHAR(8), -- fecha de inicio de pension
       p_grupo_ley73                          SMALLINT, -- grupo de ley73
       p_num_credito                          CHAR(20), -- numero de credito para Amort Excedentes
       v_r_ret_ws_det_peticion_crea_solicitud RECORD LIKE ret_ws_det_peticion_crea_solicitud.* -- registro de detalle de peticion al ws

   -- se asignan los datos
   LET v_r_ret_ws_det_peticion_crea_solicitud.id_peticion      = p_id_peticion
   LET v_r_ret_ws_det_peticion_crea_solicitud.modalidad_retiro = p_modalidad_retiro
   LET v_r_ret_ws_det_peticion_crea_solicitud.causal_retiro    = p_causal_retiro
   LET v_r_ret_ws_det_peticion_crea_solicitud.nrp              = p_nrp
   LET v_r_ret_ws_det_peticion_crea_solicitud.f_inicio_pension = p_f_inicio_pension
   LET v_r_ret_ws_det_peticion_crea_solicitud.grupo_ley73      = 0
   LET v_r_ret_ws_det_peticion_crea_solicitud.num_credito      = 0
         
   -- se inserta el registro de detalle de peticion
   INSERT INTO ret_ws_det_peticion_crea_solicitud VALUES ( v_r_ret_ws_det_peticion_crea_solicitud.* )
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_det_peticion_registro_solicitud_resp
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra la respuesta enviada al cliente de una modalidad de retiro
recibida como solicitud de registro de solicitud de retiro generico

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014              - Se agrega la referencia DAP a la respuesta del servicio
======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud_resp(p_id_peticion, p_modalidad_retiro, p_subcuenta,
                                                          p_estado_solicitud, p_cod_rechazo, p_monto_aivs,
                                                          p_monto_pesos, p_referencia_dap)
DEFINE p_id_peticion       DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro  SMALLINT, -- modalidad de retiro
       p_subcuenta         SMALLINT, -- subcuenta de inversion
       p_estado_solicitud  SMALLINT, -- estado de la solicitud
       p_cod_rechazo       SMALLINT, -- codigo de rechazo
       p_monto_aivs        DECIMAL(22,6), -- saldo en AIVs
       p_monto_pesos       DECIMAL(22,2), -- saldo en pesos equivalente a AIVs por valor accion
	   p_referencia_dap    CHAR(12), -- referencia DAP
       v_r_ret_ws_det_peticion_crea_solicitud_resp RECORD LIKE ret_ws_det_peticion_crea_solicitud_resp.* -- registro de respuesta detalle de peticion al ws

   -- se asignan los datos
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.id_peticion            = p_id_peticion
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.modalidad_retiro       = p_modalidad_retiro
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_subcuenta         = p_subcuenta
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_estado_solicitud  = p_estado_solicitud
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_cod_rechazo       = p_cod_rechazo
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_avis        = p_monto_aivs
   LET v_r_ret_ws_det_peticion_crea_solicitud_resp.resp_monto_pesos       = p_monto_pesos
         
   -- se inserta el registro de detalle de peticion
   INSERT INTO ret_ws_det_peticion_crea_solicitud_resp VALUES ( v_r_ret_ws_det_peticion_crea_solicitud_resp.* )
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud_benef
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de un beneficiario dado de alta en una solicitud de retiro generico
por modalidad de retiro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud_benef(p_id_peticion, p_modalidad_retiro, p_consec_benef, p_tipo_beneficiario,
                                                       p_clabe_bancaria, p_rfc, p_email, p_telefono, p_tel_movil,
                                                       p_nombre, p_ap_paterno, p_ap_materno, p_entidad_federativa)
DEFINE p_id_peticion        DECIMAL(9,0), -- id de la peticion
       p_modalidad_retiro   SMALLINT, -- modalidad de retiro
       p_consec_benef       SMALLINT, -- consecutivo de beneficiario
       p_tipo_beneficiario  SMALLINT, -- tipo de beneficiario
       p_clabe_bancaria     CHAR(18), -- CLABE de transferencia interbancaria
       p_rfc                char(13), -- RFC del beneficiario
       p_email              CHAR(50), -- email
       p_telefono           CHAR(10), -- telefono fijo
       p_tel_movil          CHAR(10), -- telefono movil
       p_nombre             char(40), -- nombre de pila
       p_ap_paterno         char(40), -- apellido paterno
       p_ap_materno         char(40), -- apellido materno
       p_entidad_federativa CHAR(2), -- entidad federativa
       v_r_ret_ws_peticion_crea_sol_benef RECORD LIKE ret_ws_peticion_crea_sol_benef.* -- registro de respuesta detalle de peticion al ws

   -- se asignan los datos
   LET v_r_ret_ws_peticion_crea_sol_benef.id_peticion        = p_id_peticion
   LET v_r_ret_ws_peticion_crea_sol_benef.modalidad_retiro   = p_modalidad_retiro  
   LET v_r_ret_ws_peticion_crea_sol_benef.consec_benef       = p_consec_benef      
   LET v_r_ret_ws_peticion_crea_sol_benef.tipo_beneficiario  = p_tipo_beneficiario 
   LET v_r_ret_ws_peticion_crea_sol_benef.clabe_bancaria     = p_clabe_bancaria    
   LET v_r_ret_ws_peticion_crea_sol_benef.rfc                = p_rfc               
   LET v_r_ret_ws_peticion_crea_sol_benef.email              = p_email             
   LET v_r_ret_ws_peticion_crea_sol_benef.telefono           = p_telefono          
   LET v_r_ret_ws_peticion_crea_sol_benef.tel_movil          = p_tel_movil         
   LET v_r_ret_ws_peticion_crea_sol_benef.nombre             = p_nombre            
   LET v_r_ret_ws_peticion_crea_sol_benef.ap_paterno         = p_ap_paterno        
   LET v_r_ret_ws_peticion_crea_sol_benef.ap_materno         = p_ap_materno        
   LET v_r_ret_ws_peticion_crea_sol_benef.entidad_federativa = p_entidad_federativa
         
   -- se inserta el registro de detalle de beneficiarios
   INSERT INTO ret_ws_peticion_crea_sol_benef VALUES ( v_r_ret_ws_peticion_crea_sol_benef.* )
   
END FUNCTION
