--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW05                                                  #
#OBJETIVO          => WS VALIDACION DE CUENTA PARA RETIRO FONDO DE AHORRO     #
#FECHA INICIO      => 08 JUNIO, 2013                                          #
###############################################################################
IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS
    --servicio de consulta al ws
    --############################################
DEFINE ret_fa_valida_solicitud_in RECORD
         nss              CHAR(11) , -- nss del trabajador
         rfc              CHAR(13)   -- rfc del trabajador
       END RECORD,
       -- registro de respuesta
       ret_fa_valida_solicitud_out  RECORD
         nss           CHAR(11)      , --- Número de seguridad social del trabajador
         rfc           CHAR(13)      ,
         res_op        SMALLINT      , -- Respuesta de operación.  Aceptado / Rechazado
         cod_rechazo   SMALLINT      , -- Código de rechazo 
         imp_viv7292   DECIMAL(12,2)   -- Importe de vivienda 72-92
       END RECORD
{
DEFINE ret_fa_valida_solicitud_in RECORD
         nss              CHAR(11) , -- nss del trabajador
         rfc              CHAR(13) , -- rfc del trabajador
         causal_ref       SMALLINT , -- causal del retiro
         id_beneficiario  SMALLINT , -- Identificador de beneficiario (si aplica)
         nombre           CHAR(18) , -- Nombre del beneficiario 
         ap_paterno       CHAR(18) , -- Apellido paterno 
         ap_materno       CHAR(18) , -- Apellido materno
         entidad          SMALLINT , -- Entidad federativa 
         causal_adai      SMALLINT   -- Causal de adai  
       END RECORD,
       -- registro de respuesta
       ret_fa_valida_solicitud_out  RECORD
         nss           CHAR(11)      , --- Número de seguridad social del trabajador
         res_op        SMALLINT      , -- Respuesta de operación.  Aceptado / Rechazado
         cod_rechazo   SMALLINT      , -- Código de rechazo 
         imp_viv7292   DECIMAL(12,2) , -- Importe de vivienda 72-92
         num_ref       CHAR(18)        -- Referencia del banco (cuenta donde se realizara el deposito)
       END RECORD
}
         
DEFINE g_id_derechohabiente DECIMAL(9,0) ,
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

-- =======================================================
-- constantes para los estados de la solicitud y para los codigos de rechazo
CONSTANT  gi_solicitud_aceptada              SMALLINT = 10  ,
          gi_solicitud_rechazada             SMALLINT = 100 

CONSTANT  gi_datos_incompletos             SMALLINT = 99,
          gi_nss_rfc_no_existe             SMALLINT = 999,
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW05."
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
  --fglrun RETWE03 -S 
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
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  -- se crea el servicio
  CALL ERRORLOG("invoca creacion de servicio Retiro")
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
    LET serv = com.WebService.CreateWebService("validaSolicitudRetFondoAhorro", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateRPCStyle("fn_ret_fa_valida_solicitud","fn_ret_fa_valida_solicitud",ret_fa_valida_solicitud_in,ret_fa_valida_solicitud_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
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
       --display_status("Retiro 72-92 Service registrado")
       CALL ERRORLOG("Se registro el servicio retiro7292")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Valida Solicitud Retiro Fondo Ahorro':" || STATUS)
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
Nombre: fn_ret_fa_valida_solicitud
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion que valida las solicitudes de retiro de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_fa_valida_solicitud()
   LET g_nss              = ret_fa_valida_solicitud_in.nss
   LET g_rfc              = ret_fa_valida_solicitud_in.rfc
{
   LET g_nombre           = ret_fa_valida_solicitud_in.nombre
   LET g_ape_mat          = ret_fa_valida_solicitud_in.ap_materno
   LET g_ape_pat          = ret_fa_valida_solicitud_in.ap_paterno
   LET g_id_beneficiario  = ret_fa_valida_solicitud_in.id_beneficiario
   LET g_causal_adai      = ret_fa_valida_solicitud_in.causal_adai
   LET g_entidad          = ret_fa_valida_solicitud_in.entidad
 }  
   --CALL ERRORLOG("Se reciben los datos")
   --CALL ERRORLOG("g_causal_ref  :" || g_causal_ref )
   --CALL ERRORLOG("g_nss         :" || g_nss        )
   --CALL ERRORLOG("g_nombre      :" || g_nombre     )
   --CALL ERRORLOG("g_ape_mat     :" || g_ape_mat    )
   --CALL ERRORLOG("g_ape_pat     :" || g_ape_pat    )
   --CALL ERRORLOG("g_id_beneficiario       :" || g_id_beneficiario      )
   --CALL ERRORLOG("g_causal_adai :" || g_causal_adai)
   --CALL ERRORLOG("g_entidad     :" || g_entidad    )
   DISPLAY "invocando validacion"
   -- se invoca la validacion de la solicitud de retiro de fondo de ahorro
   --CALL fn_valida_solicitud_fa(g_nss, g_rfc, g_causal_ref, g_id_beneficiario, g_nombre, g_ape_pat, g_ape_mat, g_entidad, g_causal_adai)
   CALL fn_valida_solicitud_fa(g_nss, g_rfc)
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_valida_solicitud_fa
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Procesa una solitud de retiro de Fondo de Ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
--FUNCTION fn_valida_solicitud_fa(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
FUNCTION fn_valida_solicitud_fa(p_nss, p_rfc)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       v_count_bnd       SMALLINT   ,
       v_cod_rechazo     SMALLINT   ,
       v_id_solicitud    SMALLINT   ,
       v_conteo_nss      SMALLINT   ,
       v_ruta_ejecutable VARCHAR(40),
       v_ruta_log        STRING,
       v_cadena          VARCHAR(40),
       v_saldo           DECIMAL(12,2), -- saldo del derechohabiente
       v_id_afi_fondo72  DECIMAL(9,0) -- id derechohabiente
      
   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
     
   -- validacion de que todos los campos hayan sido capturados
   IF ( p_nss IS NULL AND p_rfc IS NULL ) THEN
{        (p_causal          IS NULL) OR
        (p_id_beneficiario IS NULL) OR
        (p_nombre          IS NULL) OR
        (p_ap_paterno      IS NULL) OR
        (p_ap_materno      IS NULL) OR
        (p_entidad         IS NULL) OR
        (p_causal_adai     IS NULL) )  THEN
}
   
       -- error, se deben enviar todos los datos
       -- NSS,edo solicitud, cod_rechazo_importeviv,referencia
       CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_datos_incompletos, 0, "0")
      
       CALL ERRORLOG("Se deben capturar todos los datos")
       RETURN             
   END IF
   
   -- se verifica si se tiene NSS y RFC
   IF ( p_nss IS NOT NULL AND p_rfc IS NOT NULL ) THEN

      -- para validar el NSS se verifica que exista al menos una vez
      SELECT COUNT(*)
      INTO   v_conteo_nss 
      FROM   afi_fondo72
      WHERE  nss = p_nss
      AND    rfc = p_rfc
         
      IF ( v_conteo_nss IS NULL ) THEN
         -- si no se encontraron coincidencias
         CALL ERRORLOG("No existe el nss-rfc")
         CALL ERRORLOG("No existe el nss-rfc")
         CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
         RETURN    
      ELSE
         -- si se encuentra mas de uno, no procede su solicitud
         IF ( v_conteo_nss > 1 ) THEN
            CALL ERRORLOG("El RFC/NSS devuelve más de un registro")
            CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
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
         WHERE  nss = p_nss
         
         IF ( v_conteo_nss IS NULL ) THEN
            -- No existe el nss
            CALL ERRORLOG("No existe el NSS")
            CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
            RETURN
         ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            IF ( v_conteo_nss > 1 ) THEN
               CALL ERRORLOG("El NSS devuelve más de un registro")
               CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
               RETURN
            END IF
         END IF 
      ELSE
         -- se busca usando el RFC
         SELECT COUNT(*)
         INTO   v_conteo_nss 
         FROM   afi_fondo72
         WHERE  rfc = p_rfc
         
         -- si no se encontraron coincidencias
         IF ( v_conteo_nss IS NULL ) THEN
            -- No existe el nss
            CALL ERRORLOG("No existe el RFC")
            CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
            RETURN
         ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            IF ( v_conteo_nss > 1 ) THEN
               CALL ERRORLOG("El RFC devuelve más de un registro")
               CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
               RETURN
            END IF         
         END IF
        
      END IF
   END IF

   DISPLAY "Validando la solicitud"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RW05_"
   LET v_cadena   = TODAY USING "yyyymmdd"
   LET v_ruta_log = v_ruta_log || v_cadena
   IF ( p_nss IS NOT NULL ) THEN
      LET v_cadena   = "_" || p_nss
   ELSE
      LET v_cadena   = "_" || p_rfc
   END IF
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"

   --LET v_ruta_log = trim(g_nss) || ".log" 
   
   DISPLAY "Ruta del log del NSS evaluado: ", v_ruta_log
   DISPLAY "NSS evaluado: ", p_nss
   DISPLAY "RFC evaluado: ", p_rfc
   CALL STARTLOG(v_ruta_log) 
   LET g_id_derechohabiente  = 0 
   LET g_id_fondo72          = 0
   LET g_tanto_adicional     = 0
   LET g_bnd_uso_seq         = 1
   LET g_sq_ret_solicitud    = 0
   LET v_conteo_nss          = 0
   
   IF ( p_nss IS NOT NULL ) THEN
      CALL ERRORLOG("Validando solicitud para NSS: " || p_nss)
   END IF
   
   IF ( p_rfc IS NOT NULL ) THEN
      CALL ERRORLOG("Validando solicitud para RFC: " || p_rfc)
   END IF

   DISPLAY "Buscando saldo del trabajador"
   
   -- se verifica si tiene saldo
   CALL fn_recupera_saldo(p_nss, p_rfc) RETURNING v_saldo
   
   DISPLAY "Saldo: ", v_saldo
   
   -- si tiene saldo, se verifica que no este marcado
   IF ( v_saldo > 0 ) THEN
      -- se verifica que no esté marcado
      IF ( 1 = 2 ) THEN
         -- el derechohabiente esta marcado, no procede la solicitud
      ELSE
         -- la solicitud es valida, se responde
         CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_aceptada, 10, v_saldo, "0")
      END IF
   ELSE
      -- el derechohabiente no tiene saldo, por lo que no pocede su solicitud
      CALL fn_respuesta_ws(p_nss, p_rfc, gi_solicitud_rechazada, gi_sin_saldo, 0, "0")
   END IF

       
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_recupera_saldo
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el saldo de fondo de ahorro de un trabajador identificado
por su NSS para los n-afi_fondo72 que obtenga

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recupera_saldo(p_nss, p_rfc)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- nss del trabajador
       p_rfc            LIKE afi_derechohabiente.rfc, -- RFC del trabajador
       v_saldo_pesos    DECIMAL(22,2) -- suma del saldo del trabajador

   -- se inicia el acumulador
   LET v_saldo_pesos  = 0 

   SELECT SUM(a.importe)
   INTO   v_saldo_pesos
   FROM   cta_fondo72 a,
          afi_fondo72 b
   WHERE  b.nss = p_nss -- el trabajador
   AND    b.rfc = p_rfc
   AND    a.id_afi_fondo72 = b.id_afi_fondo72 -- coinddencias en la tabla de movimientos

   IF ( v_saldo_pesos <= 0 OR v_saldo_pesos IS NULL ) THEN
      -- el saldo se considera cero
      LET v_saldo_pesos = 0
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_saldo_pesos
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

======================================================================
}
FUNCTION fn_respuesta_ws(p_nss, p_rfc, p_estado_solicitud, p_cod_rechazo, p_importe_viv7292, p_num_referencia)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv7292  DECIMAL(12,2), -- Importe de vivienda 72-92
         p_num_referencia   CHAR(18) -- referencia bancaria
         
   -- se construye la respuesta del ws
   LET ret_fa_valida_solicitud_out.nss         = p_nss
   LET ret_fa_valida_solicitud_out.rfc         = p_rfc
   LET ret_fa_valida_solicitud_out.res_op      = p_estado_solicitud
   LET ret_fa_valida_solicitud_out.cod_rechazo = p_cod_rechazo
   LET ret_fa_valida_solicitud_out.imp_viv7292 = p_importe_viv7292

END FUNCTION
