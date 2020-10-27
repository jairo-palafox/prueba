--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS09                                                 #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO GENERICO                                         #
#FECHA INICIO      => 30-NOV-2017                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_ret_generico_solicitud_in RECORD
         nss              CHAR(11), -- nss del trabajador
         caso_adai        CHAR(10), -- numero de caso ADAI
         grupo            SMALLINT, -- grupo para el retiro de Ley 73
         medio_entrega    SMALLINT, -- medio por el cual se hace la transaccion del retiro
         sello            CHAR(14), -- Sello generado por la consulta biometrica (aplica para medio_entrega = 1 tableta)
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              tipo_beneficiario  SMALLINT,
              clabe_bancaria     CHAR(18),
              rfc                LIKE afi_derechohabiente.rfc,
              email              VARCHAR(50),
              telefono           VARCHAR(10),
              tel_movil          VARCHAR(10),
              nombre             LIKE afi_derechohabiente.nombre_af,
              ap_paterno         LIKE afi_derechohabiente.ap_paterno_af,
              ap_materno         LIKE afi_derechohabiente.ap_materno_af,
              entidad_federativa CHAR(2)
         END RECORD
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
         nss                 CHAR(11), --- Número de seguridad social del trabajador
         caso_adai           CHAR(18),
         arr_modalidad_retiro DYNAMIC ARRAY OF RECORD
           subcuenta            SMALLINT, -- subcuenta de inversion
           estado_solicitud     SMALLINT, -- estado de la solicitud
           cod_rechazo          SMALLINT, -- codigo de rechazo
           des_rechazo          CHAR(100),   ---- *************************************++
           monto_avis           DECIMAL(22,6), -- saldo en AIVs
           monto_pesos          DECIMAL(22,2), -- saldo en pesos equivalente a AIVs por valor accion
		     referencia_dap       CHAR(12) -- referencia cuando es pago por DAP
         END RECORD
       END RECORD
-- ========================================================
-- constantes que identifican el medio de entrega
CONSTANT G_ME_TABLETA     SMALLINT = 1,
         G_ME_DEV_AUTO    SMALLINT = 2,
         G_ME_CRM         SMALLINT = 3,
         G_ME_AFORE       SMALLINT = 5,
         G_ME_EXCEPCIONES SMALLINT = 6,
         G_ME_MASIVO      SMALLINT = 7
         
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
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
  
  -- se define la ruta del log
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS09."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
    DISPLAY "Log del servicio: " || FGL_GETENV("RETWS09LOG")
    
    -- se inicia el log del programa
    IF FGL_GETENV("RETWS09LOG") THEN
       CALL STARTLOG(FGL_GETENV("RETWS09LOG"))
       DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS09LOG")
    ELSE
       DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
       CALL STARTLOG(v_ruta_log)
    END IF 

  LET v_pantalla = FALSE
  #
  # Check arguments
  #
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_retiro_ley73(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET v_pantalla = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro service") --, STYLE="naked")
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
  CALL fn_crea_servicio_retiro_ley73(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor... 1.1")

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
Nombre: fn_crea_servicio_retiro_ley73
Fecha creacion: Noviembre 30, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que complementa la solicitud del retiro de ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_retiro_ley73(p_generar_WSDL)
  DEFINE v_webservice         com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

  TRY
    -- =============================
    -- se crea el servicio
    LET v_webservice = com.WebService.CreateWebService("retiroCreaSolicitudLey73", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_generico_solicitud_ley73","fn_ret_generico_solicitud_ley73",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL v_webservice.publishOperation(op, "fn_ret_generico_solicitud_ley73")

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
Nombre: fn_ret_generico_solicitud_ley73
Fecha creacion: Noviembre 30, 2017
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_ret_generico_solicitud_ley73()
DEFINE v_indice_retiro       SMALLINT,
       v_nss                 LIKE afi_fondo72.nss,
       v_rfc                 LIKE afi_fondo72.rfc,
       v_indice_modalidad    SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
	    v_requiere_DAP        SMALLINT, -- booleana que indica si se necesita DAP
       v_modalidad_procesada SMALLINT, -- Indica si ya se proceso una solicitud de ley 73
       v_id_derechohabiente  DECIMAL(10,0) -- Identificador único del trabajador
   
   -- se verifica si se esta solicitando eco
   IF ( UPSHIFT(ws_ret_generico_solicitud_in.nss) = "ECO" ) THEN
      -- se devuelve ECO
      LET ws_ret_generico_solicitud_out.nss = "ECO"
      
      -- se indica que hay un registro
      LET g_indice_retiro = 1
      CALL fn_respuesta_ws("ECO", "ECO", 0, 0, 0, 0, 0, NULL)
   ELSE
      -- se asignan los valores de respuesta
      LET ws_ret_generico_solicitud_out.nss = ws_ret_generico_solicitud_in.nss
      
      LET v_nss = ws_ret_generico_solicitud_in.nss
      
      -- se inicia el indice del retiro que se va a consultar
      LET g_indice_retiro = 1
      
      -- se crea el registro de la peticion de solicitud de ws registro de solicitud
      CALL fn_registra_peticion_registro_solicitud(ws_ret_generico_solicitud_in.nss,  
                                                   ws_ret_generico_solicitud_in.caso_adai)
           RETURNING g_id_peticion
      
      -- se asume que todos traen cuenta CLABE
      LET v_cta_clabe_correcta = TRUE
	  
	  -- si se tiene mas de una modalidad de retiro, todas deben tener cuenta CLABE
      IF ws_ret_generico_solicitud_in.grupo <> 1 AND ws_ret_generico_solicitud_in.medio_entrega <> 1 THEN 
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength() 
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
               -- se rechaza porque todos deben traer CLABE, incluyendo Fondo de Ahorro
               LET v_cta_clabe_correcta = FALSE
               EXIT FOR
            END IF
         END FOR
      END IF 
			
		 
      -- si no fue correcto, se rechaza la solicitud para todas las modalidades
      IF ( NOT v_cta_clabe_correcta ) THEN
         CALL fn_respuesta_ws(v_nss, NULL, 3,
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
                                                           3,
                                                           NULL,
                                                           NULL             ,
                                                           NULL,
                                                           ws_ret_generico_solicitud_in.grupo     ,
                                                           NULL )
         -- se asume que el registro de beneficiario es correcto
         LET v_existe_beneficiario = TRUE

         -- se asume que las cuentas clabe son correctas
         LET v_cta_clabe_correcta = TRUE
         
         IF ws_ret_generico_solicitud_in.grupo = 1 AND 
            (ws_ret_generico_solicitud_in.medio_entrega = 1 OR 
             ws_ret_generico_solicitud_in.medio_entrega = 2) THEN
            --- Obtenemos los datos del beneficiario para grupo 1 y medio entrega 1 (tableta)
            LET v_indice_beneficiario = 1 
            LET v_id_derechohabiente  = 0
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario = 1 --- Titular
            {
            SELECT DISTINCT a.cuenta_clabe,NVL(d.rfc,"SIN RFC"),NVL(d.nombre_af,"SIN NOMBRE"),
                   NVL(d.ap_paterno_af,"SIN PATERNO"),NVL(d.ap_materno_af,"SIN MATERNO"),
                   TRIM(NVL(e.cve_lada,"00")) || TRIM(NVL(e.telefono,"0000000000")), NVL(f.valor,"SIN CORREO"),
                   NVL(h.entidad_federativa,"9")
            INTO   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc           ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre        ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno    ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_materno    ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono      ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email         ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa
            FROM   ret_ws_peticion_marca a,
                   ret_ws_det_peticion_marca b,
                   ret_solicitud_generico c,
                   afi_derechohabiente d
                   LEFT OUTER JOIN afi_telefono e
                                ON d.id_derechohabiente = e.id_derechohabiente
                               AND e.id_telefono = 1
                   LEFT OUTER JOIN afi_contacto_electronico f
                                ON d.id_derechohabiente = f.id_derechohabiente
                               AND f.id_contacto_electronico = 1
                   LEFT OUTER JOIN afi_domicilio g
                                ON d.id_derechohabiente = g.id_derechohabiente
                               AND g.id_domicilio = 1
                   LEFT OUTER JOIN cat_cp h
                                ON g.cp = h.cp
            WHERE  c.nss = v_nss
            AND    d.id_derechohabiente = c.id_derechohabiente
            AND    c.modalidad_retiro = 3
            AND    c.estado_solicitud = 8
            AND    b.resp_con_retiro  = c.id_solicitud
            AND    a.id_peticion      = b.id_peticion }
            SELECT DISTINCT c.id_derechohabiente, a.cuenta_clabe,
                   NVL(d.rfc,"SIN RFC"), NVL(d.nombre_af,"SIN NOMBRE"),
                   NVL(d.ap_paterno_af,"SIN PATERNO"), NVL(d.ap_materno_af,"SIN MATERNO")
            INTO   v_id_derechohabiente,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc           ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre        ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno    ,
                   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_materno    
            FROM   ret_pago_spei             a,
                   ret_solicitud_generico    c,
                   afi_derechohabiente       d
            WHERE  c.nss                 = v_nss
            AND    d.id_derechohabiente  = c.id_derechohabiente
            AND    c.modalidad_retiro    = 3
            AND    c.estado_solicitud    = 8
            AND    a.id_solicitud        = c.id_solicitud
            AND    a.consec_beneficiario = 1


            SELECT TRIM(NVL(telefono,"0000000000"))
            INTO   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono
            FROM   afi_telefono
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    id_telefono        = 1
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = "0000000000"
            END IF 

            SELECT NVL(valor,"SIN CORREO")
            INTO   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email
            FROM   afi_contacto_electronico
            WHERE  id_derechohabiente      = v_id_derechohabiente
            AND    id_contacto_electronico = 1
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = "SIN CORREO"
            END IF 

            SELECT NVL(b.entidad_federativa,"9")
            INTO   ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa
            FROM   afi_domicilio a,
                   cat_cp        b
            WHERE  a.id_derechohabiente = v_id_derechohabiente
            AND    a.id_domicilio       = 1
            AND    a.cp                 = b.cp
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa = "9"
            END IF 
 
         END IF  
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
            -- se registra en la bitacora los datos de beneficiarios recibidos
            CALL fn_registra_peticion_registro_solicitud_benef(g_id_peticion                                                                                                                   ,
                                                                3                                          ,
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
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email               IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono            IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre              IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno          IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa  IS NULL ) THEN

               -- los datos de un beneficiario estan mal, no procede la solicitud
               LET v_existe_beneficiario = FALSE
               EXIT FOR
            ELSE
               -- si le falta la CLABE
               IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
                  -- se rechaza
                  LET v_existe_beneficiario = FALSE
                  EXIT FOR
               END IF
               -- la modalidad es diferente de fondo de ahorro. Se valida la CLABE
               -- si la cuenta CLABE no es correcta
               --IF ( NOT fn_verifica_estructura_clabe(ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria) ) THEN
               --   LET v_cta_clabe_correcta = FALSE
               --   EXIT FOR
               --END IF
            END IF   
            IF ws_ret_generico_solicitud_in.grupo = 1 AND 
               (ws_ret_generico_solicitud_in.medio_entrega = 1 OR 
                ws_ret_generico_solicitud_in.medio_entrega = 2) AND 
                v_indice_beneficiario = 1 THEN 
               EXIT FOR
            END IF 
         END FOR
         -- se verifica que la modalidad traiga beneficiarios
         IF ( NOT v_existe_beneficiario OR NOT v_cta_clabe_correcta ) THEN
            -- se verifica si es por falta de datos de un beneficiario
            IF ( NOT v_existe_beneficiario ) THEN
               CALL fn_respuesta_ws(v_nss, v_rfc, 3,
                                  gi_solicitud_rechazada, gi_solicitud_sin_beneficiarios, 0, 0, NULL)
            ELSE
               -- se rechaza por la estructura de la cuenta clabe
               CALL fn_respuesta_ws(v_nss, v_rfc, 3,
                                  gi_solicitud_rechazada, gi_esctructura_cta_clabe_incorrecta, 0, 0, NULL)               
            END IF
         ELSE            
            -- se verifica la modalidad
            CALL fn_ret_disponibilidad_ley73(v_nss, v_rfc, ws_ret_generico_solicitud_in.grupo, v_indice_modalidad)
            LET v_modalidad_procesada = 1
         END IF
      END IF 
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de vivienda segun ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27 Nov 2013             - Ley73 no rechaza solicitudes cuando el saldo de Infonavit
                                        sea menor al de la AFORE. Se registrara en la tabla
                                        ret_his_saldo para poder identificar las que van a sobregirar
Ivan Vega     Octubre 22, 2020        - PLAG138 para grupo 1, entrada por CRM, PORTAL, si se tiene saldo en TESOFE, se agrega como
                                        parte del monto de vivienda 97
======================================================================
}
FUNCTION fn_ret_disponibilidad_ley73(p_nss, p_rfc, p_grupo_ley73, p_indice_modalidad)
DEFINE p_nss                  LIKE afi_derechohabiente.nss, -- NSS
       p_rfc                  LIKE afi_derechohabiente.rfc, -- RFC del trabajador
       p_grupo_ley73          SMALLINT, -- grupo de retiro segun Ley73
       p_indice_modalidad     SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_cuenta_clabe         CHAR(18), -- cuenta CLABE para transferencia bancaria
       v_tiene_spess          SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart          LIKE ret_datamart.id_datamart,
       v_aivs_viv92           DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97           DECIMAL(24,6), -- saldo AIVs de viv97
       v_aivs_vol             DECIMAL(24,6), -- saldo AIVs de aportaciones voluntarias
       v_aivs_tesofe          DECIMAL(24,6), -- saldo AIVs de TESOFE
       v_pesos_viv92          DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97          DECIMAL(22,2), -- saldo pesos de viv97
       v_pesos_vol            DECIMAL(22,2), -- saldo pesos de aportaciones voluntarias
       v_pesos_tesofe         DECIMAL(22,2), -- saldo pesos de TESOFE
       v_aivs_viv92_afore     DECIMAL(24,6), -- saldo AIVs de viv92 en AFORE
       v_aivs_viv97_afore     DECIMAL(24,6), -- saldo AIVs de viv97 en AFORE
       v_pesos_viv92_afore    DECIMAL(22,2), -- saldo pesos de viv92 en AFORE
       v_pesos_viv97_afore    DECIMAL(22,2), -- saldo pesos de viv97 en AFORE
       v_resultado            SMALLINT, -- resultado de la consulta
       v_f_inicio_pension     DATE, -- fecha de inicio de pension en el SPESS
       v_fecha_resolucion     DATE, --fehca de resolucion de pension en el SPESS
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud         LIKE ret_solicitud_generico.id_solicitud, -- id de la solicitud
       v_precio_fondo         LIKE glo_valor_fondo.precio_fondo, -- precio de la accion
       v_saldo_total          DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_diagnostico          SMALLINT,
       v_estatus              SMALLINT,
       v_id_cliente           SMALLINT,    -- indica el tipo de consulta 30 - Consulta, 44 - Consulta y marca y 60 - Consulta y desmarca
       v_curp_sello           CHAR(18)
       

   -- se obtiene el id de solicitud creada en el marcado buscando estado en 8 precapturada
   -- CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 3, 8) RETURNING v_id_solicitud
   -- Se elimina este llamado para hacer la busqueda sin el RFC   RPR 140616 *****
   
   SELECT id_solicitud
   INTO   v_id_solicitud
   FROM   ret_solicitud_generico
   WHERE  nss              = p_nss
   AND    modalidad_retiro = 3
   AND    estado_solicitud = 8
   
   -- si la cuenta aparece marcada y lista para ser generada una solicitud
   IF ( v_id_solicitud IS NOT NULL ) THEN


   
      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente, curp
      INTO   v_id_derechohabiente, v_curp_sello
      FROM   afi_derechohabiente
      WHERE  nss               = p_nss   
      AND    ind_estado_cuenta = 0   -- cuenta Activa

      -- si se encontro el NSS
      IF ( v_id_derechohabiente IS NOT NULL ) THEN
   
         -- se verifica si el NSS tiene resolucion valida en el SPESS
         CALL fn_trabajador_resolucion_spess(p_nss, 5) RETURNING v_tiene_spess, v_id_datamart
            
         -- si no tiene resolucion valida en el spess
         IF ( NOT v_tiene_spess ) THEN
            -- se rechaza en viv92 y viv97
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 8, 0, TODAY, 0)
            CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_resolucion_spess, 4, 0, TODAY, 0)
            
            -- rechazo por no tener resolucion en el spess            
            CALL fn_genera_solicitud_ret_ley73(p_nss, v_id_derechohabiente, gi_solicitud_rechazada, gi_sin_resolucion_spess,
                                               0, 0, 0, 0,
                                               v_id_solicitud, p_indice_modalidad
                                               ,p_grupo_ley73,0)
         ELSE
            -- se obtiene la fecha de resolucion de pension
            SELECT f_resolucion, f_inicio_pension
            INTO   v_fecha_resolucion, v_f_inicio_pension
            FROM   ret_datamart
            WHERE  id_datamart = v_id_datamart

            -- se obtiene el saldo de vivienda 92 y vivienda 97 (INFONAVIT)
            -- se obtiene el saldo de viv92
            CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
            
            -- se obtiene el saldo de viv97
            CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
            
            -- se obtiene el saldo de aportaciones voluntarias
            CALL fn_calcula_saldo_ley73(p_nss, 55, TODAY) RETURNING v_resultado, v_aivs_vol, v_pesos_vol

            -- PLAG128 se obtiene el saldo de tesofe
            CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_aivs_tesofe, v_pesos_tesofe
           
            -- se valuan las AIVs al valor de la accion del dia de consulta
            SELECT precio_fondo
            INTO   v_precio_fondo
            FROM   glo_valor_fondo
            WHERE  fondo = 11
            AND    f_valuacion = TODAY

            SELECT precio_fondo
            INTO   v_precio_fondo
            FROM   glo_valor_fondo
            WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                FROM   (SELECT LIMIT 1 1 
                                        FROM   systables))
            AND    fondo = 11            
            
            LET v_pesos_viv92 = v_aivs_viv92 * v_precio_fondo
            LET v_pesos_viv97 = v_aivs_viv97 * v_precio_fondo
            LET v_pesos_vol   = v_aivs_vol   * v_precio_fondo
            -- PLAG138 se agrega monto de TESOFE
            LET v_pesos_tesofe = v_aivs_tesofe * 1 -- tesofe esta invertido en pesos
           
            -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- se verifica que grupo de retiro que se recibio en la solicitud
            CASE p_grupo_ley73
               -- GRUPO 1. Este grupo paga el monto que define la AFORE. Regla del 26 de nov 2013
               WHEN 1               
                  -- si la fecha de resolucion es igual o posterior al 13 de enero de 2012
                  IF ( v_fecha_resolucion >= "01/13/2012" ) THEN
                     -- Se recupera el saldo que se guardo en la consulta de la marca
                     CALL fn_recupera_saldo_consulta(p_nss) 
                          RETURNING v_aivs_viv92_afore, v_pesos_viv92_afore, v_aivs_viv97_afore, v_pesos_viv97_afore
                        -- si el saldo de afore es mayor que el de infonavit, se registra que la solicitud tendra un sobregiro
                     IF ( (v_aivs_viv92_afore > v_aivs_viv92) OR 
                        (v_aivs_viv97_afore > (v_aivs_viv97+ v_aivs_vol)) ) THEN
                           
                        -- vivienda92
                        INSERT INTO ret_his_saldo (id_solicitud, subcuenta,fondo_inversion,
                                                   saldo_acciones,saldo_pesos,folio,
                                                   f_registro,h_registro     
                                                  )
                                    VALUES (v_id_solicitud,8,11,
                                            v_aivs_viv92,v_pesos_viv92,0,
                                            TODAY,CURRENT HOUR TO SECOND
                                           )
                        
                        -- vivienda97
                        INSERT INTO ret_his_saldo (id_solicitud, subcuenta,fondo_inversion,
                                                   saldo_acciones,saldo_pesos,folio,
                                                   f_registro,h_registro     
                                                  )
                                    VALUES (v_id_solicitud,4,11,
                                            v_aivs_viv97,v_pesos_viv97,0,
                                            TODAY,CURRENT HOUR TO SECOND
                                           )
                           -- Aportaciones Voluntarias 
                        INSERT INTO ret_his_saldo (id_solicitud, subcuenta,fondo_inversion,
                                                   saldo_acciones,saldo_pesos,folio,
                                                   f_registro,h_registro     
                                                  )
                                    VALUES (v_id_solicitud,55,11,
                                            v_aivs_vol,v_pesos_vol,0,
                                            TODAY,CURRENT HOUR TO SECOND
                                           )
                     END IF
                     -- se valida la solicitud para un grupo 1
                     CALL fn_retl73_valida_grupo1(p_nss, v_id_derechohabiente, v_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97+v_aivs_vol, v_pesos_viv97+v_pesos_vol, v_fecha_resolucion, p_indice_modalidad,p_grupo_ley73, v_pesos_tesofe)
                  ELSE
                     -- la fecha es invalida para grupo 1
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_corresponde_a_nuevo_pensionado, 8, 0, TODAY, 0)
                     CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_corresponde_a_nuevo_pensionado, 4, 0, TODAY, 0)
                  END IF
               -- GRUPO 2. SE PAGA SALDO INFONAVIT
               WHEN 2
                  -- se valida la solicitud para un grupo 2
                  CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_id_derechohabiente, v_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97 + v_aivs_vol, v_pesos_viv97 + v_pesos_vol, v_f_inicio_pension, p_indice_modalidad,p_grupo_ley73)
                  
               -- GRUPO 3. SE PAGA SALDO INFONAVIT
               WHEN 3
                  -- se valida la solicitud para un grupo 3
                  CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_id_derechohabiente, v_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97 + v_aivs_vol, v_pesos_viv97 + v_pesos_vol, v_f_inicio_pension, p_indice_modalidad,p_grupo_ley73)
            
               -- GRUPO 4. SE PAGA SALDO INFONAVIT
               WHEN 4
                  -- se valida la solicitud para un grupo 4
                  CALL fn_retl73_valida_grupo4(p_nss, v_id_derechohabiente, v_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97 + v_aivs_vol, v_pesos_viv97 + v_pesos_vol, v_f_inicio_pension, p_indice_modalidad,p_grupo_ley73)
            
               -- otro grupo es invalido
               OTHERWISE
                  -- se rechaza viv92 y viv97
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 8, 0, TODAY, 0)
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 4, 0, TODAY, 0)
            END CASE
         END IF
      ELSE
         -- no se encuentra el NSS en la base de datos de vivienda
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 8, 0, TODAY, 0)
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 4, 0, TODAY, 0)         
      END IF
   ELSE
      -- no se tiene una solicitud de retiro ley 73 precapturada con el nss dado
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_existe_solicitud, 8, 0, TODAY, 0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_existe_solicitud, 4, 0, TODAY, 0)
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo1
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de un credito por amortaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    Octubre 22, 2020        - PLAG138. Para grupo 1, entrada por CRM o Portal, si se tiene saldo de TESOFE
                                       se agrega al saldo de vivienda 97
======================================================================
}
FUNCTION fn_retl73_valida_grupo1(p_nss, p_id_derechohabiente, p_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_fecha_resolucion, p_indice_modalidad,p_grupo_ley73, p_pesos_tesofe)
DEFINE p_nss                CHAR(11), -- NSS
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_solicitud       LIKE ret_solicitud_generico.id_solicitud, -- solicitud de retiro
       p_grupo_ley73        SMALLINT, -- grupo de retiro segun Ley73
       p_indice_modalidad   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess        SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart        LIKE ret_datamart.id_datamart,
       v_aivs_viv92         DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97         DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92        DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97        DECIMAL(22,2), -- saldo pesos de viv97
       p_pesos_tesofe       DECIMAL(22,2), -- saldo pesos de TESOFE
       v_resultado          SMALLINT, -- resultado de la consulta
       v_tiene_credito      SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tipo_credito       SMALLINT, -- clave del tipo de credito
       v_fecha_resolucion   DATE, -- fecha de resolucion en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_consulta           STRING,
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40) -- saldo total (viv92 + viv97)

   -- se calcula saldo total
   LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
   
   -- si el saldo es mayor a cero
   {
   IF ( v_saldo_total > 0 ) THEN
   }
   
      -- se verifica si tuvo un retiro de devolucion
      IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
         -- se rechaza por insuficiencia de saldo
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)
         
         -- se rechaza la solicitud precapturada         
         CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo,
                                            0, 0, 0, 0,
                                            p_id_solicitud, p_indice_modalidad
                                            ,p_grupo_ley73,0)
         
      ELSE
         -- se verifica si el derechohabiente tiene un credito vigente
         --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
         --
         --IF ( v_tiene_credito ) THEN
            -- se verifica si el credito es de tipo 43BIS
            --IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
               -- el saldo de vivienda92 es mayor a cero
               --IF ( v_aivs_viv92 > 0 ) THEN              
                  -- el saldo es retirable
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY, 0)
                  --
                  --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                     --v_aivs_viv92, v_pesos_viv92, 0, 0,
                                                     --p_id_solicitud, p_indice_modalidad
                                                     --,p_grupo_ley73,0)
--
               --ELSE
                  -- se rechaza por insuficiencia
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)
                  --
                  --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo,
                                                     --0, 0, 0, 0,
                                                     --p_id_solicitud, p_indice_modalidad
                                                     --,p_grupo_ley73,0)
--
               --END IF
            --ELSE
               -- se rechaza por tener credigo vigente
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY, 0)
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY, 0)
               --
               --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                                  --0, 0, 0, 0,
                                                  --p_id_solicitud, p_indice_modalidad
                                                  --,p_grupo_ley73,0)
--
            --END IF
         --ELSE
            -- el saldo es retirable
            LET v_tiene_credito = 0
            
            SELECT COUNT(*)
            INTO   v_tiene_credito
            FROM   sfr_marca_activa a, afi_derechohabiente b
            WHERE  a.id_derechohabiente = b.id_derechohabiente
            AND    b.nss                = p_nss
            AND    a.marca IN (202,212,218,220,222,223,224,226,227,232,233)
            
            IF v_tiene_credito > 0 THEN
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY,0)
               
               LET v_aivs_viv97 = 0
               LET v_pesos_viv97 = 0
               
               -- si el derechohabiente tiene un credito43 BIS, el monto de vivienda92 es retirable
               IF v_aivs_viv92 > 0 AND p_grupo_ley73 = 1 THEN 
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)

                  CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                     v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,
                                                     p_id_solicitud, p_indice_modalidad
                                                     ,p_grupo_ley73,0)
               ELSE
                  -- se rechaza el saldo
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY,0)
               
                  LET v_aivs_viv92 = 0
                  LET v_pesos_viv92 = 0

                  CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                                     0, 0, 0, 0,
                                                     p_id_solicitud, p_indice_modalidad
                                                     ,p_grupo_ley73,0)
               END IF 
            ELSE
               -- el saldo es retirable
               CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
               
               -- PLAG138 Si hay saldo en tesofe, grupo 1 para PORTAL y CRM, se agrega TESOFE al saldo de viv97
               IF ( ws_ret_generico_solicitud_in.medio_entrega = G_ME_CRM OR ws_ret_generico_solicitud_in.medio_entrega = G_ME_DEV_AUTO 
                    AND p_pesos_tesofe > 0 ) THEN
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, p_pesos_tesofe)                      
               ELSE
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
               END IF
               

               CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                  v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,
                                                  p_id_solicitud, p_indice_modalidad
                                                  ,p_grupo_ley73, p_pesos_tesofe)
            END IF 

         --END IF
      END IF

   {
   ELSE
      -- se rechaza por insuficiencia de saldo
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)
      
      CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo,
                                         0, 0, 0, 0,
                                         p_id_solicitud, p_indice_modalidad
                                         ,p_grupo_ley73,0)

   END IF
   }
         
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo_2_y_3
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las valiaciones para un retiro de ley 73 por grupo 2 o 3

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     11mar2014               - Grupo 2 y 3 de Ley73 no pagan vivienda 92
======================================================================
}
FUNCTION fn_retl73_valida_grupo_2_y_3(p_nss, p_id_derechohabiente, p_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_f_inicio_pension, p_indice_modalidad, p_grupo_ley73)
DEFINE p_nss                CHAR(11), -- NSS
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_solicitud       LIKE ret_solicitud_generico.id_solicitud, -- solicitud de retiro
       p_grupo_ley73        SMALLINT, -- grupo de retiro segun Ley73
       p_indice_modalidad   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess        SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart        LIKE ret_datamart.id_datamart,
       v_aivs_viv92         DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97         DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92        DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97        DECIMAL(22,2), -- saldo pesos de viv97
       v_resultado          SMALLINT, -- resultado de la consulta
       v_tiene_credito      SMALLINT, -- booleana que indica si existe un credito vigente
       v_tipo_credito       SMALLINT, -- clave del tipo de credito vigente
       v_f_inicio_pension   DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_tuvo_transferencia SMALLINT, -- booleana que indica si tuvo una transferencia tipo B
       v_monto_transferido  DECIMAL(22,2), -- monto transferido
       v_consulta           STRING,
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40)

   -- la fecha de inicio de pension es anterior a 13/enero/2012
   IF ( v_f_inicio_pension < "01/13/2012" ) THEN
      -- se verifica si tuvo un retiro o devolucion
      IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
         -- se rechaza
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY, 0)
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY, 0)
         
         CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                            0, 0, 0, 0,
                                            p_id_solicitud, p_indice_modalidad
                                            ,p_grupo_ley73,0)
   
      ELSE
         -- se verifica si tiene un credito vigente
         --CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
         --
         --IF ( v_tiene_credito ) THEN
            -- se verifica si el credito es de tipo 43BIS
            --IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
               -- se verifica si tuvo transferencia tipo B
               --CALL fn_verifica_transferencia_tipo_b(p_id_derechohabiente)
                    --RETURNING v_tuvo_transferencia, v_monto_transferido
                    --
               --IF ( v_tuvo_transferencia ) THEN
                  -- se obtiene el saldo de viv97 del anexo
                  --LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido
                  --
                  -- el saldo es retirable
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY, 0)
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
                  --
                  --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                     --0, 0, v_aivs_viv97, v_pesos_viv97,
                                                     --p_id_solicitud, p_indice_modalidad
                                                     --,p_grupo_ley73,0)
   --
               --ELSE
                  -- el saldo a devolver es el de viv97 mas viv92
                  -- VIVIENDA 92 no se paga
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY, 0)
                  --
                  -- VIVIENDA 97
                  --IF ( v_aivs_viv97 > 0 ) THEN
                     -- saldo retirable
                     --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)                        
                  --ELSE
                     -- sin saldo
                     --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)
                     --
                  --END IF
                  --
                  -- si se tiene saldo en algina de las cuentas
                  --IF ( v_aivs_viv97 > 0 ) THEN
                     -- se genera la solicitud
                     --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                        --0, 0, v_aivs_viv97, v_pesos_viv97,
                                                        --p_id_solicitud, p_indice_modalidad
                                                        --,p_grupo_ley73,0)
                  --END IF
               --END IF
                  --
            --ELSE
               -- se rechaza
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY, 0)
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY, 0)
   --
               --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                                  --0, 0, 0, 0,
                                                  --p_id_solicitud, p_indice_modalidad
                                                  --,p_grupo_ley73,0)
            --END IF
         --ELSE
            -- se verifica si tuvo transferencia tipo B
            CALL fn_verifica_transferencia_tipo_b(p_id_derechohabiente)
                 RETURNING v_tuvo_transferencia, v_monto_transferido
            IF ( v_tuvo_transferencia ) THEN
               {-- se obtiene el saldo de viv97 del anexo
               LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido
               
               -- el saldo es retirable
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY, 0)
               CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
               
               CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                  0, 0, v_aivs_viv97, v_pesos_viv97,
                                                  p_id_solicitud, p_indice_modalidad
                                                  ,p_grupo_ley73,0)}

               -- Se cambio para poder liquidar en diferente subcuenta TESOFE
               -- el saldo es retirable
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY, 0)
               -- Se notifican juntas
               CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, (v_aivs_viv97 + v_monto_transferido), TODAY, 0)
               
               CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                  0, 0, v_aivs_viv97, v_pesos_viv97,
                                                  p_id_solicitud, p_indice_modalidad
                                                  ,p_grupo_ley73,v_monto_transferido)
   
            ELSE
               -- el saldo retirable es lo existente en viv92 y viv97
               -- VIVIENDA 92 no se paga
               CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_no_disponible_para_retiro, 8, 0, TODAY, 0)
               
               -- VIVIENDA 97
               IF ( v_aivs_viv97 > 0 ) THEN
                  -- saldo retirable
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)
               ELSE
                  -- sin saldo
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)
               END IF
               
               -- si se tiene saldo en algina de las cuentas
               IF ( v_aivs_viv97 > 0 ) THEN
                  -- se genera la solicitud
                  CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                     0, 0, v_aivs_viv97, v_pesos_viv97,
                                                     p_id_solicitud, p_indice_modalidad
                                                     ,p_grupo_ley73,0)
               END IF
            END IF            
         --END IF
      END IF
   ELSE
      -- se rechaza por incompatibilidad de fecha de inicio de pension
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 8, 0, TODAY, 0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 4, 0, TODAY, 0)
      
      CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73,
                                         0, 0, 0, 0,
                                         p_id_solicitud, p_indice_modalidad
                                         ,p_grupo_ley73,0)
   
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_retl73_valida_grupo4
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las valiaciones para un retiro de ley 73 por grupo 4

Registro de modificaciones:
Autor           Fecha               Descrip. cambio
Eneas Armas     20140122          Se agrega ,p_grupo_ley73,v_monto_transferido del anexo 1
Ivan Vega       Octubre 21, 2020  - se habilita saldo de vivienda 92 cuando se trata del grupo 4
======================================================================
}
FUNCTION fn_retl73_valida_grupo4(p_nss, p_id_derechohabiente, p_id_solicitud, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_f_inicio_pension, p_indice_modalidad,p_grupo_ley73)
DEFINE p_nss                CHAR(11), -- NSS
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_solicitud       LIKE ret_solicitud_generico.id_solicitud, -- solicitud de retiro
       p_grupo_ley73        SMALLINT, -- grupo de retiro segun Ley73
       p_indice_modalidad   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_tiene_spess        SMALLINT, -- booleana que indica si tiene una resolucion en SPESS
       v_id_datamart        LIKE ret_datamart.id_datamart,
       v_aivs_viv92         DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97         DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92        DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97        DECIMAL(22,2), -- saldo pesos de viv97
       v_resultado          SMALLINT, -- resultado de la consulta
       v_tiene_credito      SMALLINT, -- booleana que indica si el derechohabiente tiene un credito vigente
       v_tipo_credito       SMALLINT, -- clave del tipo de credito
       v_f_inicio_pension   DATE, -- fecha de inicio de pension en el SPESS
       v_saldo_total        DECIMAL(24,6), -- saldo total (viv92 + viv97)
       v_tuvo_transferencia SMALLINT, -- booleana que indica si tuvo una transferencia tipo B
       v_monto_transferido  DECIMAL(22,2), -- monto transferido
       v_consulta           STRING,
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40)
       
   -- la fecha de inicio de pension es anterior a 13/enero/2012
   IF ( v_f_inicio_pension < "01/13/2012" ) THEN
      -- se verifica si tuvo un retiro o devolucion
      IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
         -- se rechaza
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY, 0)
         CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY, 0)
         
         CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                            0, 0, 0, 0,
                                            p_id_solicitud, p_indice_modalidad
                                            ,p_grupo_ley73,0)
      ELSE
         -- se verifica si el derechohabiente tiene un credito vigente
--         CALL fn_ret_ley73_credito_vigente(p_nss, p_grupo_ley73) RETURNING v_tiene_credito, v_tipo_credito
         
         --IF ( v_tiene_credito ) THEN
--CALL ERRORLOG("n-ax v_tiene_credito then")
            -- se verifica si el credito es de tipo 43BIS
            --IF ( fn_verifica_tipo_credito_43bis(v_tipo_credito) ) THEN
               -- se verifica si tiene saldo en viv92
               --IF ( v_aivs_viv92 > 0 ) THEN
                  -- se verifica si tuvo transferencia tipo B
                  --CALL fn_verifica_transferencia_tipo_b(p_id_derechohabiente)
                       --RETURNING v_tuvo_transferencia, v_monto_transferido
                  --IF ( v_tuvo_transferencia ) THEN
--CALL ERRORLOG("n-ax v_tuvo_transferencia then")
                     -- se obtiene el saldo de viv97 del anexo
                     --LET v_aivs_viv97 = v_aivs_viv97 + v_monto_transferido
                     --
                     -- el saldo es retirable
                     --20140122 se agrega ,p_grupo_ley73,v_monto_transferido del anexo 1
                     --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY,0)
                     --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY,v_monto_transferido)
--
                     --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                        --v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,
                                                        --p_id_solicitud, p_indice_modalidad
                                                        --,p_grupo_ley73,v_monto_transferido)
   --
                  --ELSE
--CALL ERRORLOG("n-ax v_tuvo_transferencia else")
                     -- el saldo a devolver es el de viv97 mas viv92
                     -- VIVIENDA 92
                     --IF ( v_aivs_viv92 > 0 ) THEN
                        -- saldo retirable
                        --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY, 0)
                     --ELSE
                        -- sin saldo
                        --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)                        
                     --END IF
                     --
                     -- VIVIENDA 97
                     --IF ( v_aivs_viv97 > 0 ) THEN
                        -- saldo retirable
                        --CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, 0)                        
                     --ELSE
                        -- sin saldo
                        --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)                        
                     --END IF
                     --
                     -- si se tiene saldo en alguna de las cuentas
                     --IF ( v_aivs_viv92 > 0 OR v_aivs_viv97 > 0 ) THEN
                        --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                           --v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,
                                                           --p_id_solicitud, p_indice_modalidad
                                                           --,p_grupo_ley73,0)
                     --END IF
                  --END IF
                  --
               --ELSE
                  -- se rechaza por insuficiencia de saldo
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)
                  --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, 0)
                  --
                  --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo,
                                                     --0, 0, 0, 0,
                                                     --p_id_solicitud, p_indice_modalidad
                                                     --,p_grupo_ley73,0)
               --END IF
            --ELSE
               -- se rechaza
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 8, 0, TODAY, 0)
               --CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_tiene_credito_vigente, 4, 0, TODAY, 0)
   --
               --CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_tiene_credito_vigente,
                                                  --0, 0, 0, 0,
                                                  --p_id_solicitud, p_indice_modalidad
                                                  --,p_grupo_ley73,0)   
            --END IF
         --ELSE
--CALL ERRORLOG("n-ax v_tiene_credito else")

            -- se verifica si tuvo transferencia tipo B
            CALL fn_verifica_transferencia_tipo_b(p_id_derechohabiente)
                 RETURNING v_tuvo_transferencia, v_monto_transferido

            -- si tuvo transferencia
            IF ( NOT v_tuvo_transferencia ) THEN
--CALL ERRORLOG("n-ax v_tuvo_transferencia then")
               -- el saldo es retirable
               --20140122 se agrega ,p_grupo_ley73,v_monto_transferido del anexo 1
               LET v_monto_transferido = 0
            END IF
            
--CALL ERRORLOG("n-ax v_tuvo_transferencia else")
               -- el saldo retirable es lo existente en viv92 y viv97
               -- VIVIENDA 92
               -- 20201021 PLAG138 si se tiene salgo en viv92, se habilita para grupo 4
               IF ( v_aivs_viv92 > 0 ) THEN
                  -- saldo retirable
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 8, v_aivs_viv92, TODAY, 0)                  
               ELSE
                  -- sin saldo
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 8, 0, TODAY, 0)
                  LET v_aivs_viv92 = 0
               END IF
               
               -- VIVIENDA 97
               IF ( v_aivs_viv97 > 0 ) THEN
                  -- saldo retirable
                  CALL fn_respuesta_ws_ley73(gi_solicitud_aceptada, 0, 4, v_aivs_viv97, TODAY, v_monto_transferido)                  
               ELSE
                  -- sin saldo
                  CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_sin_saldo, 4, 0, TODAY, v_monto_transferido)
                  LET v_aivs_viv97 = 0
               END IF
               
               -- si se tiene saldo en alguna de las cuentas
               -- 20201021 si se tiene saldo en viv92, tambien se registra
               IF ( v_aivs_viv97 > 0 OR v_aivs_viv92 > 0 ) THEN
                  CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_aceptada, 0,
                                                     v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,
                                                     p_id_solicitud, p_indice_modalidad
                                                     ,p_grupo_ley73,v_monto_transferido)
                                                     
               END IF
            
         --END IF
      END IF
   ELSE
      -- se rechaza por incompatibilidad de fecha de inicio de pension
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 8, 0, TODAY, 0)
      CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73, 4, 0, TODAY, 0)
      
      CALL fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, gi_solicitud_rechazada, gi_fecha_inicio_pension_invalida_l73,
                                         0, 0, 0, 0,
                                         p_id_solicitud, p_indice_modalidad
                                         ,p_grupo_ley73,0)
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_ley73
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la validacion de disponibilidad del retiro 
de ley 73

Registro de modificaciones:
Autor           Fecha      Descrip. cambio
Eneas Armas     20140122   Se agrega p_monto_transferido a la suma ya
                           existente es del anexo1 (solo se suma a los pesos)
Ivan Vega      25Feb2014   Se agrega referencia dap en la respuesta. Siempre es NULL en ley73
======================================================================
}
FUNCTION fn_respuesta_ws_ley73(p_estado_solicitud, p_cod_rechazo, p_subcuenta, p_importe_aivs, p_fecha_valuacion,p_monto_transferido)
DEFINE   p_estado_solicitud  SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo       SMALLINT, -- Codigo de rechazo 
         p_subcuenta         SMALLINT, -- subcuenta de inversion
         p_importe_aivs      DECIMAL(24,6), -- monto en AIVs
         p_fecha_valuacion   DATE, -- fecha de valuacion
         v_valor_fondo       LIKE glo_valor_fondo.precio_fondo,
         p_monto_transferido DECIMAL(22,2) -- monto transferido
         
   -- se obtiene el valor del fondo de inversion
   CALL fn_obtener_precio_fondo(p_fecha_valuacion, 11) RETURNING v_valor_fondo
         
   -- si no se encuentra el precio del fondo, no se puede valuar
   IF ( v_valor_fondo IS NULL ) THEN
      LET p_estado_solicitud = gi_solicitud_rechazada
      LET p_cod_rechazo      = gi_no_hay_precio_fondo
      LET p_importe_aivs     = 0
      LET v_valor_fondo      = 0
   END IF
   
   -- se genera el registro de disponibilidad      
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].subcuenta        = p_subcuenta
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].monto_avis       = p_importe_aivs
   
   --20140122 Se agrega p_monto_transferido a la suma ya existente (solo se suma a los pesos)
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].monto_pesos      = p_importe_aivs * v_valor_fondo + p_monto_transferido
   
   -- 25feb2014 Se agrega referencia DAP a la respuesta. siempre en blanco para ley 73
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].referencia_dap   = NULL

   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = "";
       SELECT des_larga
       INTO   ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = " "; 
       END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, 3, p_subcuenta,
                                                         p_estado_solicitud, p_cod_rechazo, p_importe_aivs,
                                                         p_importe_aivs * v_valor_fondo, NULL)


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
		 p_referencia_dap   CHAR(12) -- referencia DAP

   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].subcuenta        = 0
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estado_solicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].cod_rechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].monto_avis       = p_aivs
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].monto_pesos      = p_pesos
   
   -- 25feb2014. Referencia DAP
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].referencia_dap = p_referencia_dap

   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = "";
       SELECT des_larga
       INTO   ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].des_rechazo = " "; 
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
Nombre: fn_genera_solicitud_ret_ley73
Fecha creacion: Noviembre 11, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro Ley 73 de vivienda 92/97 para un
NSS dado

Registro de modificaciones:
Autor           Fecha      Descrip. cambio
Eneas Armas     20140122   Se se agrega condicion para soportar importe del anexo1
                20140122   Se cambia tabla ret_ley73 a ret_ley73_generico tiene diferentes campos
                20140122   Se cambia tabla ret_ley73 a ret_ley73_generico tiene diferentes campos
======================================================================
}
FUNCTION fn_genera_solicitud_ret_ley73(p_nss, p_id_derechohabiente, p_estado_solicitud, p_rechazo,
                                       p_aivs_viv92, p_pesos_viv92, p_aivs_viv97, p_pesos_viv97,
                                       p_id_solicitud, p_indice_modalidad
                                       ,p_gpo_ley73,p_importe_viv97_anexo1
                                       )
DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       p_nss                  LIKE afi_derechohabiente.nss, 
       p_rfc                  LIKE afi_derechohabiente.rfc,
       p_estado_solicitud     SMALLINT      , -- estatus de la solicitud
       p_rechazo              SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_aivs_viv92           DECIMAL(24,6),
       p_pesos_viv92          DECIMAL(22,2),
       p_aivs_viv97           DECIMAL(24,6),
       p_pesos_viv97          DECIMAL(22,2),
       p_id_solicitud         LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       p_indice_modalidad     SMALLINT, -- indice de la modalidad del retiro
       v_id_solicitud         LIKE ret_fondo_ahorro.id_solicitud,
       v_marca_ley73          LIKE sfr_marca.marca, -- marca de amortizaciones excedentes
       v_saldo_poseido        LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       v_r_ret_ley73_generico RECORD LIKE ret_ley73_generico.*, -- registro de retiro de ley73
       v_conteo               SMALLINT,
       v_tipo_pago            SMALLINT,
       v_total_aivs           DECIMAL(24,6), -- total de AIVs
       v_total_pesos          DECIMAL(22,2), -- total de pesos
       v_sql                  STRING, -- cadena con enunciado SQL
--       p_folio                decimal(9,0),
       p_gpo_ley73            smallint,
       v_subgrupo             smallint,
       p_importe_viv97_anexo1   decimal(12,2),
       v_cadena               STRING,  -- Cadena de caracteres para generar el SHA
       v_algoritmo            CHAR(6),
       v_curp                 CHAR(18),
       v_rfc                  CHAR(13),
       v_nombre               CHAR(40),
       v_ape_paterno          CHAR(40),
       v_ape_materno          CHAR(40),
       v_sha                  STRING,
       v_c_sha                CHAR(64) 
       
   -- se asigna la marca
   LET v_marca_ley73 = 803 -- ley 73
   LET v_cadena = ""
   LET v_algoritmo = ""
   LET v_sha = ""
   LET v_rfc = ""
       
   -- si la solicitud fue aceptada
   IF ( p_estado_solicitud = gi_solicitud_aceptada ) THEN      
      --selección de tipo TESOFE
      --20140122 Se se agrega condicion para soportar importe del anexo1
      CASE
         WHEN p_pesos_viv97 > 0 AND p_importe_viv97_anexo1 <= 0
            LET v_subgrupo = 114
         WHEN p_pesos_viv97 > 0 AND p_importe_viv97_anexo1 > 0
            LET v_subgrupo = 124
         WHEN p_pesos_viv97 = 0 AND p_importe_viv97_anexo1 > 0
            LET v_subgrupo = 104
         OTHERWISE 
            LET v_subgrupo = 0
      END CASE

      IF (ws_ret_generico_solicitud_in.medio_entrega = 1 OR
          ws_ret_generico_solicitud_in.medio_entrega = 2) AND 
         ws_ret_generico_solicitud_in.grupo = 1 THEN 
         LET p_estado_solicitud = 15 --- Estas solicitudes se dan de alta con estado "Autorizada"
         -- Las solicitudes de tableta se dejan con estado solicitud = 10 ya que se espera la autorización para confirmar la conclusión del trámite
         IF ws_ret_generico_solicitud_in.medio_entrega = 1 AND 
            ws_ret_generico_solicitud_in.grupo = 1 THEN 
            LET p_estado_solicitud = 10
         END IF 
         -- Se debe eliminar el registro de la tabla ret_pago_spei ya que la rutina de beneficiarios inserta los datos
         DELETE 
         FROM   ret_pago_spei 
         WHERE  id_solicitud = p_id_solicitud
         
          --- se obtiene el sello para guardarlo en tablas
         SELECT curp, nombre_af, ap_paterno_af, ap_materno_af, rfc
         INTO   v_curp, v_nombre, v_ape_paterno, v_ape_materno, v_rfc 
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = p_id_derechohabiente

         IF v_curp IS NULL THEN 
            LET v_curp = ""
         END IF 
         IF v_nombre IS NULL THEN 
            LET v_nombre = ""
         END IF 
         IF v_ape_paterno IS NULL THEN 
            LET v_ape_paterno = ""
         END IF
         IF v_ape_materno IS NULL THEN 
            LET v_ape_materno = ""
         END IF 
         LET v_cadena = p_id_solicitud USING "<<<<<<<<<<", "|", ws_ret_generico_solicitud_in.nss, "|",
                        v_curp, "|", TODAY USING "dd/mm/yyyy", "|", v_ape_paterno CLIPPED, "|",
                        v_ape_materno CLIPPED, "|", v_nombre CLIPPED, "|", p_pesos_viv92 + p_pesos_viv97 USING "<,<<<,<<$.&&"
         LET v_algoritmo = "SHA256"
         CALL ERRORLOG("cadena>"||v_cadena||"<") 
         CALL ERRORLOG("algoritmo>"||v_algoritmo||"<")
         
         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
         CALL ERRORLOG("sha>"||v_sha||"<")
         LET v_c_sha = v_sha
         CALL ERRORLOG("c_sha>"||v_c_sha||"<")
         UPDATE ret_sol_medio_entrega
         SET    sello = v_c_sha,
                f_registro = NULL
         WHERE  id_solicitud = p_id_solicitud;
         ------    Esto es de prueba solamente
--         LET v_cadena = "0|39957660671|FIAA760920HDFGGN09|07/03/2018|FIGUEROA|AGUILAR|ANGEL|$2,416.06"
--         LET v_algoritmo = "SHA256"
--         CALL ERRORLOG("cadena prueba   >"||v_cadena||"<") 
--         CALL ERRORLOG("algoritmo prueba>"||v_algoritmo||"<")
         
--         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
--         CALL ERRORLOG("sha prueba  >"||v_sha||"<")
--         LET v_c_sha = v_sha
--         CALL ERRORLOG("c_sha prueba>"||v_c_sha||"<")
         -------  Hasta aqui terminan los datos de prueba
      END IF 
      
      
      -- se asginan los datos al retistro de solicitud
      --20140122 Se cambia tabla ret_ley73 a ret_ley73_generico tiene diferentes campos
      LET v_r_ret_ley73_generico.id_solicitud         = p_id_solicitud
      LET v_r_ret_ley73_generico.id_derechohabiente   = p_id_derechohabiente
      LET v_r_ret_ley73_generico.f_solicitud          = TODAY
      LET v_r_ret_ley73_generico.f_valuacion          = TODAY
      LET v_r_ret_ley73_generico.aivs_viv92           = p_aivs_viv92
      LET v_r_ret_ley73_generico.aivs_viv97           = p_aivs_viv97
      LET v_r_ret_ley73_generico.importe_viv92        = p_pesos_viv92
      LET v_r_ret_ley73_generico.importe_viv97        = p_pesos_viv97
      LET v_r_ret_ley73_generico.f_captura            = TODAY
      LET v_r_ret_ley73_generico.h_captura            = CURRENT HOUR TO SECOND
      
      SELECT usuario_marca
      INTO   v_r_ret_ley73_generico.usuario
      FROM   sfr_marca_activa
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    marca = 803
      AND    n_referencia = p_id_solicitud
      
      --LET v_r_ret_ley73_generico.usuario              = "safreviv"
      LET v_r_ret_ley73_generico.estado_solicitud     = p_estado_solicitud
      LET v_r_ret_ley73_generico.cod_rechazo          = p_rechazo
      LET v_r_ret_ley73_generico.gpo_ley73            = p_gpo_ley73
      LET v_r_ret_ley73_generico.subgrupo             = v_subgrupo
      LET v_r_ret_ley73_generico.importe_viv97_anexo1 = p_importe_viv97_anexo1

      -- se inserta el registro de solicitud en la tabla historica
      INSERT INTO ret_ley73_generico VALUES ( v_r_ret_ley73_generico.* )

      -- se actualiza el estado de la tabla de control
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             --caso_adai        = ws_ret_generico_solicitud_in.caso_adai,
             rfc              = v_rfc
      WHERE  id_solicitud     = p_id_solicitud
      
      -- se calculan los montos totales
      LET v_total_aivs  = p_aivs_viv92 + p_aivs_viv97
      LET v_total_pesos = p_pesos_viv92 + p_pesos_viv97
      
      -- se crean los registros para los beneficiarios de esta solicitud
      FOR v_conteo = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
         --esta vlidación toma solo en cuenta 92 y 97 si tiene pago se le agrega lo del anexo
         --20140122  Se cambia condición para soportar pago SIAF
         CASE v_subgrupo
            WHEN 104 -- todo en TESOFE, se paga por SIAF
               LET v_tipo_pago = 3 --SIAF

            OTHERWISE -- todos los demas con SPEI
               LET v_tipo_pago = 1 --SPEI
         END CASE
		 
         LET v_total_pesos = v_total_pesos + p_importe_viv97_anexo1
         
         CALL fn_registra_beneficiario_retiro_generico(
            p_id_solicitud,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].tipo_beneficiario,
            v_tipo_pago, -- FALTA TIPO DE PAGO
            1, -- FALTA PARENTESCO
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].ap_paterno,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].ap_materno,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].nombre,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].telefono,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].email,
            100,
            v_total_aivs,
            v_total_pesos,
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabe_bancaria,
            "",
            ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].entidad_federativa)
      END FOR      
   ELSE
      -- se rechaza la solicitud
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             cod_rechazo      = p_rechazo --,
             --caso_adai        = ws_ret_generico_solicitud_in.caso_adai
      WHERE  id_solicitud     = p_id_solicitud

      -- se desmarca la cuenta
      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, v_marca_ley73, p_id_solicitud,
                                           v_marca_ley73, "safreviv", g_proceso_cod_ret_ley73_ws) -- ley 73

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
FUNCTION fn_registra_peticion_registro_solicitud(p_nss, p_caso_adai)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
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
   LET v_r_ret_ws_peticion_crea_solicitud.rfc           = NULL
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
                                                     p_nrp, p_f_inicio_pension, p_grupo_ley73, p_num_credito)
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
   LET v_r_ret_ws_det_peticion_crea_solicitud.grupo_ley73      = p_grupo_ley73
   LET v_r_ret_ws_det_peticion_crea_solicitud.num_credito      = p_num_credito
         
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
   --LET v_r_ret_ws_det_peticion_crea_solicitud_resp.referencia_dap         = p_referencia_dap
         
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

{
======================================================================
Clave: 
Nombre: fn_hash
Fecha creacion: Marzo 03, 2016
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
Genera un código HASH (pasado por parámetro) de una cadena de texto STRING
(pasada por parámetro),

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_hash_local(toDigest, algo)

   DEFINE toDigest, algo, result STRING
   DEFINE dgst security.Digest

--   IF algo IS NULL OR algo = "" THEN
--      LET algo = "SHA1" --(Default)
--   END IF
--   LET algo = "SHA256"

   TRY
      LET dgst = security.Digest.CreateDigest("SHA256")
      CALL dgst.AddStringData(toDigest)
      --LET result = dgst.DoBase64Digest()
      LET result = dgst.DoHexBinaryDigest()
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   RETURN result
   
END FUNCTION
