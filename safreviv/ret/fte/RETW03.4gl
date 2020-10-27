--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW03                                                  #
#OBJETIVO          => WS SOLICITUD DE RETIRO 72-92 FONDO AHORRO               #
#FECHA INICIO      => 22-FEB-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro viv7292fondo
--/opt/fourjs/2.32/gas/. ./envas
--httpdispatch -f as_safreviv_ws1.xcf
--port 9186

IMPORT FGL WSHelper
IMPORT com

--DATABASE safre_tmp   
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS
    --servicio de consulta al ws
    --############################################
DEFINE ret_retiro_fondo RECORD
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
       ret_respuesta  RECORD
         nss           CHAR(11)      , --- Número de seguridad social del trabajador
         res_op        SMALLINT      , -- Respuesta de operación.  Aceptado / Rechazado
         cod_rechazo   SMALLINT      , -- Código de rechazo 
         imp_viv7292   DECIMAL(12,2) , -- Importe de vivienda 72-92
         num_ref       CHAR(18)        -- Referencia del banco (cuenta donde se realizara el deposito)
       END RECORD
         
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
          gi_nss_es_necesario              SMALLINT = 98, -- el nss es necesario para el proceso en turno
          gi_nss_rfc_no_existe             SMALLINT = 999,
          gi_causal_retiro_invalido        SMALLINT = 77,
          gi_solicitud_en_tramite          SMALLINT = 99,
          gi_tiene_credito_vigente         SMALLINT = 20,
          gi_sin_saldo                     SMALLINT = 10,
          gi_sin_un_ano_relacion_laboral   SMALLINT = 50,
          gi_edad_inferior_50_anos         SMALLINT = 40,
          gi_sin_pension_vigente           SMALLINT = 90,
          gi_sin_resolucion_spess          SMALLINT = 91,
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW03."
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
    LET serv = com.WebService.CreateWebService("retiro7292", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateRPCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
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
    DISPLAY("No se pudo crear el servicio 'Retiro 72-92':" || STATUS)
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
Nombre: fn_retiro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion que valida las solicitudes de retiro de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_retiro()
   LET g_causal_ref       = ret_retiro_fondo.causal_ref
   LET g_nss              = ret_retiro_fondo.nss
   LET g_rfc              = ret_retiro_fondo.rfc
   LET g_nombre           = ret_retiro_fondo.nombre
   LET g_ape_mat          = ret_retiro_fondo.ap_materno
   LET g_ape_pat          = ret_retiro_fondo.ap_paterno
   LET g_id_beneficiario  = ret_retiro_fondo.id_beneficiario
   LET g_causal_adai      = ret_retiro_fondo.causal_adai
   LET g_entidad          = ret_retiro_fondo.entidad
   
   --CALL ERRORLOG("Se reciben los datos")
   --CALL ERRORLOG("g_causal_ref  :" || g_causal_ref )
   --CALL ERRORLOG("g_nss         :" || g_nss        )
   --CALL ERRORLOG("g_nombre      :" || g_nombre     )
   --CALL ERRORLOG("g_ape_mat     :" || g_ape_mat    )
   --CALL ERRORLOG("g_ape_pat     :" || g_ape_pat    )
   --CALL ERRORLOG("g_id_beneficiario       :" || g_id_beneficiario      )
   --CALL ERRORLOG("g_causal_adai :" || g_causal_adai)
   --CALL ERRORLOG("g_entidad     :" || g_entidad    )
   
   -- se invoca la validacion de la solicitud de retiro de fondo de ahorro
   CALL fn_valida_solicitud_fa(g_nss, g_rfc, g_causal_ref, g_id_beneficiario, g_nombre, g_ape_pat, g_ape_mat, g_entidad, g_causal_adai)
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
FUNCTION fn_valida_solicitud_fa(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
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
       v_id_afi_fondo72  DECIMAL(9,0) -- id derechohabiente
      
   -- se obtiene la ruta ejecutable
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
     
   -- validacion de que todos los campos hayan sido capturados
   IF ( (p_nss IS NULL AND p_rfc IS NULL ) OR
        (p_causal          IS NULL) OR
        (p_id_beneficiario IS NULL) OR
        (p_nombre          IS NULL) OR
        (p_ap_paterno      IS NULL) OR
        (p_ap_materno      IS NULL) OR
        (p_entidad         IS NULL) OR
        (p_causal_adai     IS NULL) )  THEN
   
       -- error, se deben enviar todos los datos
       -- NSS,edo solicitud, cod_rechazo_importeviv,referencia
       CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_datos_incompletos, 0, "0")
      
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
      
      IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
         -- si no se encontraron coincidencias
         CALL ERRORLOG("No existe el nss-rfc")
         CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
         RETURN    
      ELSE
         -- si se encuentra mas de uno, no procede su solicitud
         IF ( v_conteo_nss > 1 ) THEN
            CALL ERRORLOG("El RFC/NSS devuelve más de un registro")
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
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
         
         IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
            -- No existe el nss
            CALL ERRORLOG("No existe el NSS")
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
            RETURN
         ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            IF ( v_conteo_nss > 1 ) THEN
               CALL ERRORLOG("El NSS devuelve más de un registro")
               CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
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
         IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
            -- No existe el nss
            CALL ERRORLOG("No existe el rfc")
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, "0")
            RETURN
         ELSE
            -- si se encuentra mas de uno, no procede su solicitud
            IF ( v_conteo_nss > 1 ) THEN
               CALL ERRORLOG("El RFC devuelve más de un registro")
               CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_mas_de_un_registro, 0, "0")
               RETURN
            END IF         
         END IF
        
      END IF
   END IF

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RW03_"
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
       
   -- se valida si existe otra solicitud en tramite
   IF ( fn_rechazo_por_tramite(p_nss) ) THEN
      CALL ERRORLOG("Se rechaza porque existe otra solicitud en tramite para el mismo NSS")
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
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_causal_retiro_invalido, 0, "0")
      
      CALL ERRORLOG("El causal de retiro es invalido")
      RETURN 
   END IF      
     
   -- se realiza la verificacion de la solicitud de acuerdo al tipo de causal de retiro recibida
   CASE p_causal
      WHEN 1 -- termino de relacion laboral
         CALL fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
      
      WHEN 2 -- pension IMSS
         DISPLAY "IMSS"
         CALL fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
      
      WHEN 3 -- plan privado de pension
         DISPLAY "Plan privado pension"
         CALL fn_plan_privado_pension_defuncion(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
      
      WHEN 4 -- plan privado
         DISPLAY "plan privado defuncion"
         CALL fn_plan_privado_pension_defuncion(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
      
      OTHERWISE 
         -- causal de retiro invalido
         CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_causal_retiro_invalido, 0, "0")
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
                                     
======================================================================
}
FUNCTION fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       v_saldo           DECIMAL(19,14),
       r_edad            SMALLINT      ,
       r_b_paso          SMALLINT      ,
       v_tanto_adicional DECIMAL(12,2),
       v_referencia_banc CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess     SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart     LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess
   
   LET g_tanto_adicional = 0
   LET v_saldo           = 0
   LET v_referencia_banc = "0"
   
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
  
   -- la edad debe ser mayor o igual a 50 anos
   IF ( r_edad >= 50 ) THEN
   
      -- se verifica si se recibio NSS
      IF ( p_nss IS NOT NULL AND p_nss <> "00000000000" ) THEN
   
         -- se verifica que el trabajador tenga una resolucion en el spess
         CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
      
         -- si no tiene resolucion valida en el spess
         IF ( NOT v_tiene_spess ) THEN
            -- se verifica si tiene un ano sin relacion laboral
            IF ( NOT fn_verifica_ano_sin_relacion_laboral(p_nss) ) THEN
               -- derechohabiente sin resolucion de spess ni ano sin relacion laboral
               CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                                  gi_sin_un_ano_relacion_laboral, 
                                                  v_saldo, v_tanto_adicional, v_referencia_banc)
               
               -- sin un ano de relacion laboral
               CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_un_ano_relacion_laboral, 0, "0")
               RETURN
            END IF
         END IF
      END IF
      
      -- se obtiene el saldo del trabajador
      CALL fn_recupera_saldo(p_nss, p_rfc) RETURNING v_saldo
      
      -- si se obtuvo saldo
      IF ( v_saldo > 0 ) THEN
         DISPLAY "Saldo fondo ahorro: ",  v_saldo  
         
         -- se verifica si el trabajador tiene un credito vigente SOLO SI TIENE NSS, SI NO, SE ASUME SIN CREDITO
         IF ( p_nss IS NOT NULL AND p_nss <> "00000000000" ) THEN
            CALL fn_trabajador_credito_vigente(p_nss) RETURNING v_tiene_credito_vigente
         ELSE
            -- se trata de un registro con NSS = 00000000000 y RFC valido, se asumen sin credito vigente
            LET v_tiene_credito_vigente = FALSE
            DISPLAY "DH sin NSS y RFC valido. Se asume sin credito vigente por regla de negocio"
         END IF

         IF ( v_tiene_credito_vigente ) THEN
            DISPLAY "Derechohabiente con credito vigente"
            DISPLAY "se genera rechazo por credito"
            -- derechohabiente con credito vigente
            CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                               gi_tiene_credito_vigente, 
                                               v_saldo, v_tanto_adicional, v_referencia_banc)

            -- Derechohabiente con credito vigente
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_tiene_credito_vigente, 0, "0")
         ELSE 
            DISPLAY "no tiene credito vigente"
            
            -- se crea la referencia bancaria
            CALL fn_genera_referencia_bancaria()
                 RETURNING v_referencia_banc
            
            IF ( fn_trabajador_pensionado(p_nss) ) THEN
               DISPLAY "trabajador cuenta con pension"
               DISPLAY "se entrega dos veces el saldo"             
               -- si esta pensionado se le paga 2 veces el saldo. El extra va en el tanto adicional                 
               LET v_tanto_adicional = v_saldo
            ELSE 
               DISPLAY "trabajador no es pensionado"
               DISPLAY "se genera preautorizacion"
               DISPLAY v_saldo 
               LET v_tanto_adicional = 0
            END IF
            
            -- se genera la solicitud para el trabajador
            CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_aceptada,0, v_saldo, v_tanto_adicional, v_referencia_banc)

            -- solicitud preautorizada
            CALL fn_respuesta_ws(p_nss, gi_solicitud_aceptada, 0, v_saldo, v_referencia_banc)
         END IF 
      ELSE 
         DISPLAY "rechaza solicitud por insuficiencia de saldo"
         DISPLAY "se notifica al canal"
         -- solicitud rechazada por insuficiencia de saldo
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                            gi_sin_saldo, 
                                            v_saldo, v_tanto_adicional, v_referencia_banc)
         
         -- solicitud rechazada por no contar con saldo suficiente
         CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_saldo, 0, "0")
      END IF
   ELSE 
      -- solicitud rechazada por que el trabajador no tiene 50 anos cumplidos o mas
      DISPLAY "Solicitud rechazada porque el derechohabiente no tiene 50 anos o mas"
      CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                         gi_edad_inferior_50_anos, 
                                         v_saldo, v_tanto_adicional, v_referencia_banc)
      
      -- se responde la consulta
      -- edad inferior a 50 anos
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_edad_inferior_50_anos, 0, "0")
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

======================================================================
}
FUNCTION fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       v_saldo           DECIMAL(12,2),
       r_edad            SMALLINT      ,
       r_b_paso          SMALLINT      ,
       v_tanto_adicional DECIMAL(12,2),
       v_referencia_banc CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess     SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart     LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess

   -- se asume que no hay referencia bancaria
   LET v_referencia_banc = "0"

   -- se revisa que venga nss, ya que sin este es necesario para todo el proceso
   IF ( p_nss IS NULL OR p_nss = "00000000000" ) THEN
      -- caso invalido para pension del IMSS, se necesita un NSS valido
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_nss_es_necesario, 0, "0")
      
      -- no puede seguir el proceso
      RETURN
   END IF
       
   -- se verifica que el trabajador tenga una resolucion de pension valida
   CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
   
   IF ( v_tiene_spess ) THEN
      -- se verifica que la resolucion este vigente
      IF ( TRUE ) THEN
         -- se calcula el saldo del derechohabiente
         CALL fn_recupera_saldo(p_nss, p_rfc) RETURNING v_saldo
         
         -- se verifica si el saldo es mayor a cero
         IF ( v_saldo > 0 ) THEN
            
            -- se verifica si el trabajador tiene un credito vigente
            CALL fn_trabajador_credito_vigente(p_nss) RETURNING v_tiene_credito_vigente
            
            IF ( NOT v_tiene_credito_vigente ) THEN
            
               -- se entrega el doble del saldo del trabajador en forma de tanto adicional
               LET v_tanto_adicional = v_saldo
               
               -- se crea la referencia bancaria
               CALL fn_genera_referencia_bancaria()
                    RETURNING v_referencia_banc          
               
               -- se guarda la solicitud en estado capturada y en espera de aprobacion
               CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_aceptada,0,v_saldo, v_tanto_adicional, v_referencia_banc)

               -- solicitud preautorizada
               CALL fn_respuesta_ws(p_nss, gi_solicitud_aceptada, 0, v_saldo, v_referencia_banc)

            ELSE
               -- solicitud rechazada por tener un credito vigente
               CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                                  gi_tiene_credito_vigente, 
                                                  v_saldo, v_tanto_adicional, v_referencia_banc)
         
               -- el derechohabiente tiene un credito vigente
               CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_tiene_credito_vigente, 0, "0")
            END IF
               
         ELSE
            -- se rechaza la solicitud por no tener saldo
            CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                               gi_sin_saldo, 
                                               v_saldo, v_tanto_adicional, v_referencia_banc)
         
            -- no tiene saldo
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_saldo, 0, "0")
         END IF

      ELSE
         -- se rechaza la solicitud, no tiene pension vigente
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                            gi_sin_pension_vigente, 
                                            v_saldo, v_tanto_adicional, v_referencia_banc)

         
         -- La resolucion del SPESS expiro
         CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_pension_vigente, 0, "0")
      END IF
         
   ELSE     
      -- se rechaza la solicitud (NSS, edo solicitud, cod_rechazo, saldo, tanto adic)
      CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                         gi_sin_resolucion_spess, 
                                         v_saldo, v_tanto_adicional, v_referencia_banc)
      
      -- no tiene resolucion en el SPESS
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_resolucion_spess, 0, "0")
   END IF

END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_plan_privado_pension_defuncion
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones correspondientes a un solicitud de retiro
fondo de ahorro generada por causal plan privado de pension o defuncion
para un id_afi_fondo72 dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_plan_privado_pension_defuncion(p_nss, p_rfc, p_causal, p_id_beneficiario, p_nombre, p_ap_paterno, p_ap_materno, p_entidad, p_causal_adai)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       v_saldo           DECIMAL(19,14),
       r_edad            SMALLINT      ,
       r_b_paso          SMALLINT      ,
       v_tanto_adicional DECIMAL(12,2),
       v_referencia_banc CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess     SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart     LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess

   
   LET v_tanto_adicional = 0
   LET v_saldo           = 0
   LET v_referencia_banc = "0"


   -- Se verifica que tenga una resolucion de pension valida en el spess
   CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
   
   IF ( v_tiene_spess ) THEN
   
    
   
      -- se verfica que la resolucion sea vigente
      IF ( true ) THEN
   
         
         -- se calcula el saldo del derechohabiente
         CALL fn_recupera_saldo(p_nss, p_rfc) RETURNING v_saldo
         
         IF ( v_saldo > 0 ) THEN
            -- se verifica si tiene un credito vigente
            CALL fn_trabajador_credito_vigente(p_nss) RETURNING v_tiene_credito_vigente
            
            IF ( NOT v_tiene_credito_vigente ) THEN
               -- se paga el doble del saldo a traves del tanto adicional
               LET v_tanto_adicional = v_saldo
                
               -- se crea la referencia bancaria
               CALL fn_genera_referencia_bancaria()
                    RETURNING v_referencia_banc          
                              
               -- se genera la solicitud para el trabajador con el tanto adicional
               CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_aceptada,0,v_saldo, v_tanto_adicional, v_referencia_banc)

               -- solicitud preautorizada
               CALL fn_respuesta_ws(p_nss, gi_solicitud_aceptada, 0, v_saldo, v_referencia_banc)

            ELSE
               -- se rechaza por tener un credito vigente
               CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                                  gi_tiene_credito_vigente, 
                                                  v_saldo, v_tanto_adicional,v_referencia_banc)

               
               -- Derechohabiente con credito vigente
               CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_tiene_credito_vigente, 0, "0")

            END IF
         ELSE
            -- se rechaza por no contar con saldo para el retiro
            CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                               gi_sin_saldo, 
                                               v_saldo, v_tanto_adicional, v_referencia_banc)

            
            -- solicitud rechazada por no contar con saldo suficiente
            CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_saldo, 0, "0")
         END IF
      
      ELSE
         -- se rechaza la solicitud, no tiene pension vigente
         CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                            gi_sin_pension_vigente, 
                                            v_saldo, v_tanto_adicional, v_referencia_banc)

         
         -- La resolucion del SPESS expiro
         CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, gi_sin_pension_vigente, 0, "0")
      END IF
   
   ELSE
      -- solicitud rechazada por no tener resolucion en el spess
      CALL fn_genera_solicitud_retiro_fa(p_nss, p_rfc, gi_solicitud_rechazada,
                                         gi_sin_resolucion_spess, 
                                         v_saldo, v_tanto_adicional, v_referencia_banc)

      
      -- no tiene resolucion en el SPESS
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, 90, 0, "0")   
   END IF


END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_trabajador_pensionado
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un trabajador ya esta pensionado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_pensionado(p_nss) 
DEFINE p_nss LIKE afi_derechohabiente.nss,
       v_id_datamart DECIMAL(9,0),
       v_pension     SMALLINT  

   SELECT COUNT(a.nss)
   INTO   v_id_datamart
   FROM   ret_datamart a
         ,ret_matriz_derecho b
   WHERE a.nss            = p_nss
   AND diag_registro    = 101
   AND b.tpo_retiro     = 'E'
   AND (a.tpo_pension   <> 'IP'
   OR  (a.tpo_pension   = 'IP' AND a.porcentaje_valuacion >= 50))
   AND a.regimen        = b.regimen
   AND a.tpo_seguro     = b.tpo_seguro
   AND a.tpo_pension    = b.tpo_pension
   AND a.tpo_prestacion = b.tpo_prestacion  

   -- si se encontro, esta pensionado
   IF ( v_id_datamart > 0 ) THEN
      LET  v_pension = TRUE
   ELSE 
      LET  v_pension = FALSE
   END IF   
    
   -- se devuelve el resultado de la consulta
   RETURN v_pension
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_trabajador_resolucion_spess
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si el trabajador tiene una resolucion en el SPESS

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_resolucion_spess(p_nss, p_causal)
DEFINE  v_tiene_resolucion SMALLINT, -- booleana que indica si el trabajador tiene resolucion en spess
        p_nss              LIKE afi_derechohabiente.nss, -- nss del trabajador
        p_causal           SMALLINT, -- causal de retiro
        v_sec_pension      LIKE ret_datamart.sec_pension, -- max secuencia de pension
        v_diag_registro    LIKE ret_datamart.diag_registro, -- diagnostico del registro
        v_id_datamart      LIKE ret_datamart.id_datamart -- clave de la tabla del datamart
   
   -- se asume que el trabajador no tiene resolucion
   LET v_tiene_resolucion = FALSE
   
   CASE p_causal

      -- termino de relacion laboral
      WHEN 1
         -- Se obtiene la maxima sec de pension de la resolucion del 
         -- spess/datamart que tenga tipo de retiro E
         -- y cuya combinacion de caracteristicas del retiro
         -- exista en la matriz de derechos
         SELECT MAX(a.sec_pension), a.diag_registro, a.id_datamart
         INTO  v_sec_pension, v_diag_registro, v_id_datamart
         FROM  ret_datamart a
               ,ret_matriz_derecho b
         WHERE a.nss            = p_nss -- el trabajador
         AND   b.tpo_retiro     = 'E' -- retiro avalado por el imss
         AND   a.regimen        = b.regimen -- condiciones del retiro validas
         AND   a.tpo_seguro     = b.tpo_seguro
         AND   a.tpo_pension    = b.tpo_pension
         AND   a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.diag_registro, a.id_datamart
         
         -- si el diagnostico es 101, tiene resolucion valida
         IF ( v_diag_registro = "101" ) THEN
            LET v_tiene_resolucion = TRUE
         END IF
     
      -- Resolucion de pension otorgada por el IMSS
      WHEN 2
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO  v_sec_pension, v_id_datamart
         FROM  ret_datamart a
               ,ret_matriz_derecho b
         WHERE a.nss            = p_nss -- el trabajador
         AND   b.tpo_retiro     = 'E' -- retiro avalado por el imss
         AND   a.regimen        = b.regimen -- condiciones del retiro validas
         AND   a.tpo_seguro     = b.tpo_seguro
         AND   a.tpo_pension    = b.tpo_pension
         AND   a.tpo_prestacion = b.tpo_prestacion
         AND ( -- se revisa que sea por incapacidad, invalidez, cesantia, vejez
               b.tpo_pension IN ("CE", -- cesantia
                                 "IN", -- invalidez
                                 "IP", -- incapacidad
                                 "VE")  -- vejez
             )
         GROUP BY a.id_datamart
         
         DISPLAY "Resolucion de pension IMSS con NSS: ", p_nss
         DISPLAY "Encontrada: ", v_sec_pension
         DISPLAY "id_datamart: ", v_id_datamart
         
         -- si tiene una secuencia de pension, tiene resolucion de pension del imss
         IF ( v_id_datamart IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
         ELSE
            LET v_tiene_resolucion = FALSE
         END IF
              
      -- plan privado de pension
      WHEN 3
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO   v_sec_pension, v_id_datamart
         FROM   ret_datamart a
               ,ret_matriz_derecho b
         WHERE a.nss = g_nss
         AND   diag_registro    = 101
         AND   b.tpo_retiro     = 'F'
         AND   TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR  
         AND   a.regimen        = b.regimen
         AND   a.tpo_seguro     = 'PP'
         AND   a.tpo_pension    = b.tpo_pension
         AND   a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.id_datamart
     
         -- si tiene una secuencia de pension, tiene resolucion de pension del imss
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
         END IF
     
     
      WHEN 4
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO    v_sec_pension, v_id_datamart
         FROM    ret_datamart a
                ,ret_matriz_derecho b
         WHERE  a.nss = g_nss
         AND    diag_registro    = 101
         AND    b.tpo_retiro     = 'E'
         AND    TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR  
         AND    a.regimen        = b.regimen
         AND    a.tpo_seguro     = b.tpo_seguro
         AND    a.tpo_pension    IN ('OR','VI','VO','AS')
         AND    a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.id_datamart
     
         -- si tiene una secuencia de pension, tiene resolucion de pension del imss
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
         END IF
     
      
   END CASE
   
   -- se devuelve el resultado de la consulta
   RETURN v_tiene_resolucion, v_id_datamart
END FUNCTION 


{
======================================================================
Clave: 
Nombre: fn_verifica_ano_sin_relacion_laboral
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un trabajador tiene un ano sin relacion laboral

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_verifica_ano_sin_relacion_laboral(p_nss)
DEFINE p_nss              CHAR(11),
       v_ano_sin_relacion SMALLINT -- TRUE lo tiene, FALSE no lo tiene

   DISPLAY "valida el tiempo sin actividd laboral del derechohabiente"

   -- se asume que tiene un ano sin relacion laboral
   LET v_ano_sin_relacion = TRUE

   -- NSS sin relacion laboral
   IF ( p_nss = "01531906822" ) THEN
      LET v_ano_sin_relacion = TRUE
   ELSE
      LET v_ano_sin_relacion = FALSE
   END IF
  
   -- se devuelve el resultado de la consulta
   RETURN v_ano_sin_relacion
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_trabajador_credito_vigente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene un credito vigente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_credito_vigente(p_nss)
DEFINE p_nss                LIKE afi_derechohabiente.nss,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_credito         LIKE afi_derechohabiente.id_credito, -- indicador de credito vigente. 1 con credito. 0 sin credito
       v_conteo             SMALLINT, -- contador
       v_tiene_credito      SMALLINT -- booleana que indica si existe credito

   -- se asume que no tiene credito
   LET v_tiene_credito = FALSE
  
   -- se obtiene el id_credito del derechohabiente
   SELECT id_credito
   INTO   v_id_credito
   FROM   afi_derechohabiente
   WHERE  nss = p_nss
   
   DISPLAY "NSS buscado en credito: ", p_nss
   DISPLAY "ID_credito: ", v_id_credito
   
   -- si el trabajador no existe, no se encontrara el id_credito
   IF ( v_id_credito IS NULL ) THEN
      -- no puede tener credito
      LET v_tiene_credito = FALSE
   ELSE
      -- credito vigente
      IF ( v_id_credito = 1 ) THEN
         -- tiene credito vigente
         LET v_tiene_credito = TRUE
      ELSE
         -- no tiene credito vigente
         LET v_tiene_credito = FALSE
      END IF
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_tiene_credito
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
       p_rfc            LIKE afi_derechohabiente.rfc, -- nss del trabajador
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
Nombre: fn_edad_derechohabiente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene la edad cumplida de un derechohabiente registrado en afi_fondo72
identificado por su id_afi_fondo72

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_edad_derechohabiente(p_nss)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- nss del trabajador
       p_id_afi_fondo72 DECIMAL(9,0), -- id afi del trabajador
       v_f_nac_rfc      CHAR(13),
       v_f_year         CHAR(2),
       v_f_month        CHAR(2),
       v_f_day          CHAR(2),
       v_f_year_today   CHAR(4),
       v_f_nac_bueno    DECIMAL(4,0),
       v_f_nac          DATE,
       v_edad           SMALLINT,
       v_rfc            LIKE afi_fondo72.rfc, -- rfc del trabajador
       v_sql            STRING -- cadena con consulta sql

   -- se obtiene el id_afi_fondo72 del trabajador
   LET v_sql = "\nSELECT FIRST 1 id_afi_fondo72",
               "\nFROM   afi_fondo72",
               "\nWHERE  nss = ? "
               
   -- se ejecuta la consulta
   PREPARE sid_obtener_id_afi FROM v_sql
   EXECUTE sid_obtener_id_afi INTO p_id_afi_fondo72 USING p_nss

   -- se recupera el rfc del derechohabiente
   SELECT rfc
   INTO   v_rfc
   FROM   afi_fondo72
   WHERE  id_afi_fondo72 = p_id_afi_fondo72

   -- ====================================================
   -- se calcula la edad a partir del RFC
   
   -- si no se encontro el rfc, no se puede calcular la edad
   IF ( v_rfc = 0 OR v_rfc IS NULL ) THEN
      LET v_f_nac  = -1
   ELSE
      -- se obtienen las particulas de la fecha de nacimiento
      LET v_f_year  = v_rfc [5,6]
      LET v_f_month = v_rfc [7,8]
      LET v_f_day   = v_rfc [9,10]
 
      -- se obtiene el ano de la fecha del dia
      LET v_f_year_today = YEAR(TODAY)
     
      -- se valida si la fecha es de los anos 1900
      IF ( v_f_year_today[3,4] < v_f_year ) THEN
         LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
         --DISPLAY v_f_nac_bueno
      ELSE
         -- es de los anos 2000 en adelante
         LET v_f_nac_bueno =  v_f_year_today[1,2] || v_f_year
         --DISPLAY v_f_nac_bueno
      END IF 
    
      -- la fecha de nacimiento se crea a partir del ano calculado
      LET v_f_nac = MDY(v_f_month,v_f_day,v_f_nac_bueno)    

      -- se valida qe la fecha sea correcta si no lo es se rechaza solicitud por edad
      IF ( v_f_nac = 0 ) THEN
        LET v_f_nac = -1
      ELSE
        -- se calcula la edad del trabajador  
        -- si coincide el dia y el mes de nacimiento con el dia y mes de la fecha del dia
        IF ( MONTH(TODAY) >= MONTH(v_f_nac) AND DAY(TODAY) >= DAY(v_f_nac) ) THEN 
           LET v_edad = YEAR(TODAY) - YEAR(v_f_nac)
        ELSE
           -- como es una fecha anterior al cumpleanos, no se cuenta el ano corriente por no ser ano cumplido
           LET v_edad = (YEAR(TODAY) - YEAR(v_f_nac)) - 1
        END IF 
      END IF 
   END IF 
   
   -- se devuelve la edad calculada
   RETURN v_edad
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_edad_derechohabiente_rfc
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene la edad cumplida de un derechohabiente mediante su RFC

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_edad_derechohabiente_rfc(p_rfc)
DEFINE p_rfc            LIKE afi_derechohabiente.rfc, -- rfc del trabajador
       p_id_afi_fondo72 DECIMAL(9,0), -- id afi del trabajador
       v_f_nac_rfc      CHAR(13),
       v_f_year         CHAR(2),
       v_f_month        CHAR(2),
       v_f_day          CHAR(2),
       v_f_year_today   CHAR(4),
       v_f_nac_bueno    DECIMAL(4,0),
       v_f_nac          DATE,
       v_edad           SMALLINT,
       v_rfc            LIKE afi_fondo72.rfc, -- rfc del trabajador
       v_sql            STRING -- cadena con consulta sql

   -- ====================================================
   -- se calcula la edad a partir del RFC
   
   -- si no se encontro el rfc, no se puede calcular la edad
   IF ( p_rfc = 0 OR p_rfc IS NULL ) THEN
      LET v_f_nac  = -1
   ELSE
      -- se obtienen las particulas de la fecha de nacimiento
      LET v_f_year  = p_rfc [5,6]
      LET v_f_month = p_rfc [7,8]
      LET v_f_day   = p_rfc [9,10]
 
      -- se obtiene el ano de la fecha del dia
      LET v_f_year_today = YEAR(TODAY)

      { -- ISVH 19 jul 2013
      -- se valida si la fecha es de los anos 1900
      IF ( v_f_year_today[3,4] < v_f_year ) THEN
         LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
         --DISPLAY v_f_nac_bueno
      ELSE
         -- es de los anos 2000 en adelante
         LET v_f_nac_bueno =  v_f_year_today[1,2] || v_f_year
         --DISPLAY v_f_nac_bueno
      END IF 
      }

      -- ISVH 19 Julio 2013
      -- como es fondo de ahorro, ningun trabajador nacido en la decada de los 2000 puede estar registrado
      -- por lo tanto, todas las fechas se asumen como de los anos 1900
      LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
    
      -- la fecha de nacimiento se crea a partir del ano calculado
      LET v_f_nac = MDY(v_f_month,v_f_day,v_f_nac_bueno)    

      -- se valida qe la fecha sea correcta si no lo es se rechaza solicitud por edad
      IF ( v_f_nac = 0 ) THEN
        LET v_f_nac = -1
      ELSE
        -- se calcula la edad del trabajador  
        -- si coincide el dia y el mes de nacimiento con el dia y mes de la fecha del dia
        IF ( MONTH(TODAY) >= MONTH(v_f_nac) AND DAY(TODAY) >= DAY(v_f_nac) ) THEN 
           LET v_edad = YEAR(TODAY) - YEAR(v_f_nac)
        ELSE
           -- como es una fecha anterior al cumpleanos, no se cuenta el ano corriente por no ser ano cumplido
           LET v_edad = (YEAR(TODAY) - YEAR(v_f_nac)) - 1
        END IF 
      END IF 
   END IF 
   
   -- se devuelve la edad calculada
   RETURN v_edad
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

======================================================================
}
FUNCTION fn_genera_solicitud_retiro_fa(p_nss, p_rfc, p_estatus, p_rechazo, p_saldo, p_tanto_adicional, p_referencia_bancaria)
DEFINE p_nss                 LIKE afi_derechohabiente.nss, 
       p_rfc                 LIKE afi_derechohabiente.rfc,
       p_estatus             SMALLINT      , -- estatus de la solicitud
       p_rechazo             SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_saldo               DECIMAL(12,2), -- saldo calculado
       p_tanto_adicional     DECIMAL(12,2), -- tanto adicional
       p_referencia_bancaria CHAR(12), -- clave de referencia bancaria
       v_id_afi_fondo72      LIKE afi_fondo72.id_afi_fondo72, -- id del trabajador
       v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- id del trabajador en AFI_DER
       v_estatus             SMALLINT      ,
       v_resultado           SMALLINT      , -- resultado de la ejecucion
       v_count_reg           SMALLINT      ,
       f_saldo               DATE          ,
       h_saldo               VARCHAR(20)   ,
       v_subcuenta           SMALLINT      , -- subcuenta de retiro
       v_id_solicitud        LIKE ret_fondo_ahorro.id_solicitud,
       v_marca_fondo_ahorro  LIKE sfr_marca.marca, -- marca de fondo de ahorro
       v_saldo_poseido       LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       --r_ret_det_fondo72   RECORD LIKE ret_det_fondo72.*,
       r_ret_fondo_ahorro    RECORD LIKE ret_fondo_ahorro.*, -- registro de fondo de ahorro
       v_sql                 STRING -- cadena con enunciado SQL
   
   -- marca de fondo de ahorro
   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_subcuenta          = 40
    
   -- se obtiene el id_derechohabiente 
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    rfc = p_rfc
   AND    id_derechohabiente IS NOT NULL
   
   -- si no se encuentra el NSS en vivienda
   IF ( v_id_derechohabiente IS NULL ) THEN
      
      -- se verifica si esta en la tabla de id_derechohabientes nuevos
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_fondo72_d
      WHERE  nss = p_nss
      AND    rfc = p_rfc
   
      -- si no se encontro
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- 11jul2013. Se crea el id_derechohabiente usando la secuencia de afi_derechohabiente
         -- en una tabla donde se relacione el nss, rfc, e id_derechohabiente creado
         SELECT seq_derechohabiente.nextVal
         INTO   v_id_derechohabiente
         FROM   systables
         WHERE  tabid = 1;
         
         -- se asigna el id_derechohabiente a la tabla de relacion
         INSERT INTO afi_fondo72_d (nss, rfc, id_derechohabiente)
         VALUES (p_nss,p_rfc,v_id_derechohabiente);
      END IF

   END IF
      
   -- se obtiene el id de solicitud nuevo
   SELECT seq_ret_solicitud.NEXTVAL
   INTO   v_id_solicitud
   FROM   systables 
   WHERE  tabid = 1
   
   -- se asignan los datos al registro de fondo de ahorro
   LET r_ret_fondo_ahorro.id_solicitud       = v_id_solicitud
   LET r_ret_fondo_ahorro.id_derechohabiente = v_id_derechohabiente
   LET r_ret_fondo_ahorro.f_solicitud        = TODAY
   LET r_ret_fondo_ahorro.estado_solicitud   = p_estatus
   LET r_ret_fondo_ahorro.causal_retiro      = 1
   LET r_ret_fondo_ahorro.id_datamart        = 1
   LET r_ret_fondo_ahorro.folio              = 0
   LET r_ret_fondo_ahorro.cve_referencia     = p_referencia_bancaria
   LET r_ret_fondo_ahorro.saldo_viv72        = p_saldo
   LET r_ret_fondo_ahorro.tanto_adicional    = p_tanto_adicional
   LET r_ret_fondo_ahorro.caso_adai          = 1
   LET r_ret_fondo_ahorro.entidad_federativa = 1
   LET r_ret_fondo_ahorro.f_captura          = TODAY
   LET r_ret_fondo_ahorro.h_captura          = CURRENT HOUR TO SECOND
   LET r_ret_fondo_ahorro.usuario            = "safreviv"
   LET r_ret_fondo_ahorro.cod_rechazo        = p_rechazo
   
   -- se intenta marcar si no esta rechazada
   IF ( p_estatus = gi_solicitud_aceptada ) THEN
   
      -- se marca la cuenta
      LET v_sql = "\nEXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
      PREPARE sid_marcacuenta FROM v_sql
      
      -- se ejecuta el enunciado de marcaje
      EXECUTE sid_marcacuenta USING v_id_derechohabiente
                                   ,v_marca_fondo_ahorro -- fondo de ahorro
                                   ,v_id_solicitud  
                                   ,"0" -- folio
                                   ,"0" -- estado marca
                                   ,"0" -- codigo de rechazo
                                   ,"0" -- marca de la causa
                                   ,"" -- fecha de la causa
                                   ,"safreviv"
                                   ,"1503" -- retiro Fondo Ahorro WS
             INTO v_resultado 
         
      -- si no se pudo marcar, se rechaza el registro
      IF ( v_resultado > 0 ) THEN
         DISPLAY "No se pudo marcar"
         -- se recchaza la solicitud
         
         LET r_ret_fondo_ahorro.estado_solicitud = 100; -- rechazada
         LET r_ret_fondo_ahorro.cod_rechazo      = 130 -- marca no convive
      END IF
   END IF

   -- se inserta el registro   
   INSERT INTO ret_fondo_ahorro VALUES ( r_ret_fondo_ahorro.* )
   
   -- si se acepto la solicitud se buscan sus montos
   IF ( p_estatus = gi_solicitud_aceptada ) THEN
      -- se obtiene el id_afi_fondo72
      SELECT MAX(id_afi_fondo72)
      INTO   v_id_afi_fondo72
      FROM   afi_fondo72
      WHERE  nss = p_nss
      AND    rfc = p_rfc
      
      -- se buscan los id_afi_fondo72 que corresponden con la dupla NSS/RFC
      DECLARE cur_afifondo72 CURSOR FOR
      SELECT afi.id_afi_fondo72,
             SUM(importe)
      FROM   afi_fondo72 afi,
             cta_fondo72 cta
      WHERE  nss                = p_nss
      AND    rfc                = p_rfc
      AND    afi.id_afi_fondo72 = cta.id_afi_fondo72
      AND    afi.id_afi_fondo72 IS NOT NULL
      AND    cta.subcuenta      = v_subcuenta 
      GROUP BY afi.id_afi_fondo72
            
      FOREACH cur_afifondo72 INTO v_id_afi_fondo72, v_saldo_poseido
            
         IF ( v_id_afi_fondo72 IS NOT NULL ) THEN
            -- 24Ene2012. Si la cuenta esta sobregirada se vuelve a sobregirar
            IF ( v_saldo_poseido < 0 ) THEN
               LET v_saldo_poseido = 0;
            END IF
          
            INSERT INTO ret_det_fondo72 ( 
                id_afi_fondo72
               ,id_solicitud
               ,saldo_viv72
               ,tanto_adicional
               ,id_datamart
               ,f_saldo
               ,h_saldo
               ,estado_detalle
               ,cod_rechazo)
            VALUES ( 
                v_id_afi_fondo72
               ,v_id_solicitud
               ,v_saldo_poseido
               ,p_tanto_adicional
               ,r_ret_fondo_ahorro.id_datamart
               ,TODAY
               ,CURRENT HOUR TO SECOND
               ,"1"
               ,p_rechazo
            );
         END IF
      END FOREACH
   END IF -- montos

END FUNCTION 


{
======================================================================
Clave: 
Nombre: fn_genera_referencia_bancaria
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Crea la referencia bancaria para un NSS-RFC y una causal de retiro
dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_referencia_bancaria()
DEFINE v_referencia       CHAR(12)    ,
       v_concepto_pago    CHAR(2), -- 1-2	Siempre 50	Concepto de pago para FICO (Tesorería)
       v_ano_solicitud    CHAR(2), -- 3-4	Año 	Año en que se solicita la Devolución de Fondo de Ahorro 
       v_ref_fondo_ahorro CHAR(2), -- 5-6	Siempre 72	Referencia a que se paga Fondo de Ahorro 72-92 
       v_causal_pago      CHAR(1), -- 7	Causal del pago (va del 1 al 9)
       v_num_consecutivo  CHAR(5), -- 8-12	Numero consecutivo	Se toma de una tabla
       v_consecutivo      DECIMAL(5,0), -- numero consecutivo 
       v_ano_fecha_dia    CHAR(4)  -- para extrar las unidades y decenas del ano

   -- se asingnan los datos de la referencia
   LET v_concepto_pago    = "50"
   LET v_ano_fecha_dia    = YEAR(TODAY)
   LET v_ano_solicitud    = v_ano_fecha_dia[3,4]
   LET v_ref_fondo_ahorro = "72"
   
   -- causal de pago
--   CASE p_causal

      -- 1 CESANTIA 
      -- 2 VEJEZ
      -- 3 PLAN PRIVADO DE PENSION
      -- 4 INCAPACIDAD TOTAL
      -- 5 INCPACIDAD PARCIAL < AL 50 %
      -- 6 INVALIDEZ 
      -- 7 DEFUNCION
      -- 8 TERMINACIÓN DE TERMINACIÖN LABORAL o 50 años cumplidos + 1 año sin RL.   
--      WHEN 1
--         LET v_causal_pago = "1"
--      OTHERWISE
         LET v_causal_pago = "1"
--   END CASE
   
   -- se obtiene el numero consecutivo
   SELECT seq_ret_ref_bancaria72.nextVal
   INTO   v_consecutivo
   FROM   systables
   WHERE  tabid = 1

   -- se formatea el numero a 5 cifras
   LET v_num_consecutivo = v_consecutivo USING "&&&&&"

   -- se concatenan los componentes de la referencia
   LET v_referencia = v_concepto_pago, v_ano_solicitud, v_ref_fondo_ahorro,
                      v_causal_pago, v_num_consecutivo
   
   -- se devuelve la referencia
   RETURN v_referencia
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_rechazo_por_tramite
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
valida que no exista otra solicitd en proceso de tramite 
pendiente de algun proceso con el mismo nss

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_rechazo_por_tramite(p_nss)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
       v_id_afi_fondo72 LIKE afi_fondo72.id_afi_fondo72,
       v_conteo         SMALLINT,
       v_en_tramite     SMALLINT, -- booleana que indica si se tiene una solicitud en tramite
       v_cadena         STRING

   -- se asume que no hay solicitud en tramite 
   LET v_en_tramite = FALSE
   LET v_conteo = 0

   CALL ERRORLOG("Validando si existe el id_derechohabiente en ret_fondo_ahorro")
   LET v_cadena = "ID_DER: ", g_id_derechohabiente
   CALL ERRORLOG(v_cadena)

   -- se busca el id_afi_fondo72 del nss
   SELECT MAX(id_afi_fondo72)
   INTO   v_id_afi_fondo72
   FROM   afi_fondo72
   WHERE  nss = p_nss

   -- busca al derechohabiente en la tabla ret_fondo_ahorro con estatus en tramite
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_fondo_ahorro
   WHERE  id_derechohabiente = v_id_afi_fondo72
   AND    estado_solicitud IN (10,12,14,50)

   -- si lo encuentra
   IF ( v_conteo > 0 ) THEN
      LET v_en_tramite = TRUE

      -- se responde al WS que se tiene una solicitud en tramite
      CALL fn_respuesta_ws(p_nss, gi_solicitud_rechazada, 30, 0, "0")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_en_tramite
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
FUNCTION fn_respuesta_ws(p_nss, p_estado_solicitud, p_cod_rechazo, p_importe_viv7292, p_num_referencia)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv7292  DECIMAL(12,2), -- Importe de vivienda 72-92
         p_num_referencia   CHAR(18) -- referencia bancaria
         
   -- se construye la respuesta del ws
   LET ret_respuesta.nss         = p_nss
   LET ret_respuesta.res_op      = p_estado_solicitud
   LET ret_respuesta.cod_rechazo = p_cod_rechazo
   LET ret_respuesta.imp_viv7292 = p_importe_viv7292
   LET ret_respuesta.num_ref     = p_num_referencia

END FUNCTION
