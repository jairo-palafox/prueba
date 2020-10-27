--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWE03                                                  #
#OBJETIVO          => WS SOLICITUD DE RETIRO 72-92  FONDO AHORRO              #
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
         g_nss            CHAR(11),
         g_causal_ref     SMALLINT ,
         g_ident          SMALLINT,       -- Identificador de beneficiario (si aplica)
         g_nombre         CHAR(18),       -- Nombre del beneficiario 
         g_ape_pat        CHAR(18),       -- Apellido paterno 
         g_ape_mat        CHAR(18),       -- Apellido materno
         g_entidad        SMALLINT,       -- Entidad federativa 
         g_causal_adai    SMALLINT        -- Causal de adai  
       END RECORD,

       ret_respuesta    RECORD
         --r_status          INTEGER,
         --r_rechazo         INTEGER,       
         r_g_nss          CHAR(11),       -- Número de seguridad social del trabajador
         r_g_res_op       SMALLINT,       -- Respuesta de operación.  Aceptado / Rechazado
         r_g_cod_rechazo  SMALLINT ,      -- Código de rechazo 
         r_g_imp_viv7292  DECIMAL(19,14), -- Importe de vivienda 72-92
         r_g_num_ret      CHAR(18)        -- Referencia del banco (cuenta donde se realizara el deposito)
       END RECORD



    --############################################
    --consulta de la descripcion del rechazo
    --############################################
   { ,ret_estatus RECORD
      g_estado    SMALLINT ,
      g_rechazo   SMALLINT 
    END RECORD
    
    ,ret_desc_estatus RECORD 
    g_desc_estatus varchar(50)
    END RECORD}
    --############################################
    
DEFINE g_id_derechohabiente DECIMAL(9,0)
DEFINE g_id_fondo72         DECIMAL(9,0)
DEFINE g_causal_ref         SMALLINT
DEFINE g_nss                CHAR(11)
DEFINE g_acc_acciones       DECIMAL(14,6)
DEFINE g_acc_pesos          DECIMAL(14,6)
DEFINE g_tanto_adicional    DECIMAL(14,6)
DEFINE g_id_solicitud       DECIMAL(9,0)
DEFINE g_refer              CHAR(18),
       g_ident              SMALLINT,     -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18),     -- Nombre del beneficiario 
       g_ape_pat            CHAR(18),     -- Apellido paterno 
       g_ape_mat            CHAR(18),     -- Apellido materno           
       g_causal_adai        SMALLINT,     -- Clave de Adai 
       g_entidad            SMALLINT,     -- Entidad federativa
       g_id_datamart        DECIMAL(9,0), -- Identificador datamart
       g_causal_retiro      SMALLINT
DEFINE g_bnd_uso_seq        SMALLINT 
      ,g_sq_ret_solicitud   SMALLINT 

DEFINE g_r_tmp_id_fondo72     RECORD
         nss                 CHAR(11)
        ,id_derechohabiente  DECIMAL(9,0)
        ,id_afi_fondo72      DECIMAL(9,0)
        ,importe             DECIMAL(12,2)
        ,rfc                 CHAR(13)
        ,estatus             SMALLINT 
        ,rechazo_cod         SMALLINT 
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

END GLOBALS

DEFINE serverURL STRING -- URL del servidor
DEFINE SCREEN    BOOLEAN

&define display_status(status) \
  IF NOT screen THEN \
    DISPLAY status \
  ELSE \
    DISPLAY status TO MSG \
    MENU ON idle 1 EXIT MENU ON ACTION close EXIT PROGRAM END MENU \
  END IF

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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWE03."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
  DISPLAY "Ruta del log creada: ", v_ruta_log
  
  -- se inicia el log del programa
  --CALL STARTLOG("/safreviv/ret/bin/LOGWSRET7292.log")
  CALL STARTLOG(v_ruta_log)

  LET SCREEN = FALSE
  #
  # Check arguments
  #
  --fglrun RETWE03 -S 
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRetiroService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Retiro 72-92 service") --, STYLE="naked")
      display_status("Retiro Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create Retiro service
  #
  CALL ERRORLOG("invoca creacion de servicio Retiro")
  CALL CreateRetiroService(FALSE)
  #
  # Start the server
  #
  display_status("Inicianso servidor...")
  CALL ERRORLOG("Iniciando servidor...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=8090)
  # 
  CALL com.WebServiceEngine.Start()
  display_status("El servidor esta en escucha")
  CALL ERRORLOG("Servidor en escucha")

  -- si se tiene pantalla
  IF ( SCREEN ) THEN
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

--
-- Generacion del servicio
--
FUNCTION CreateRetiroService(generateWSDL)
  DEFINE serv                 com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE generateWSDL         SMALLINT
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace       = "http://localhost/"
  --LET v_service_NameSpace       = "http://http://10.90.8.132/"
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
       display_status("Retiro 72-92 Service registrado")
       CALL ERRORLOG("Se registro el servicio retiro7292")
    END IF
    
  CATCH -- en caso de error
    display_status("No se pudo crear el servicio 'Retiro 72-92':" || STATUS)
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

-- ====================================================================
-- Funciones publicas
-- ====================================================================
FUNCTION fn_retiro()
   LET g_causal_ref   = ret_retiro_fondo.g_causal_ref
   LET g_nss          = ret_retiro_fondo.g_nss
   LET g_nombre       = ret_retiro_fondo.g_nombre
   LET g_ape_mat      = ret_retiro_fondo.g_ape_mat
   LET g_ape_pat      = ret_retiro_fondo.g_ape_pat
   LET g_ident        = ret_retiro_fondo.g_ident
   LET g_causal_adai  = ret_retiro_fondo.g_causal_adai
   LET g_entidad      = ret_retiro_fondo.g_entidad
   
   --CALL ERRORLOG("Se reciben los datos")
   --CALL ERRORLOG("g_causal_ref  :" || g_causal_ref )
   --CALL ERRORLOG("g_nss         :" || g_nss        )
   --CALL ERRORLOG("g_nombre      :" || g_nombre     )
   --CALL ERRORLOG("g_ape_mat     :" || g_ape_mat    )
   --CALL ERRORLOG("g_ape_pat     :" || g_ape_pat    )
   --CALL ERRORLOG("g_ident       :" || g_ident      )
   --CALL ERRORLOG("g_causal_adai :" || g_causal_adai)
   --CALL ERRORLOG("g_entidad     :" || g_entidad    )
   
   CALL valida_solicitud()
END FUNCTION

FUNCTION valida_solicitud()
  
   DEFINE v_count_bnd           SMALLINT         
         ,v_cod_rechazo         SMALLINT
         ,v_id_solicitud        SMALLINT  
         ,v_count_nss           SMALLINT
         ,v_ruta_ejecutable     VARCHAR(40)
         ,v_ruta_log            VARCHAR(40)
         ,v_cadena              VARCHAR(40)
      
    -- se obtiene la ruta ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
  
  -- se define la ruta del log
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWE03_Validacion."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
  DISPLAY "Ruta del log creada: ", v_ruta_log    
  CALL STARTLOG(v_ruta_log) 
    LET g_id_derechohabiente  = 0 
    LET g_id_fondo72          = 0
    LET g_tanto_adicional     = 0
    LET g_bnd_uso_seq         = 1
    LET g_sq_ret_solicitud    = 0
    LET v_count_nss           = 0

    CALL ERRORLOG("Validando solicitud")

    -- validacion de que todos los campos hayan sido capturados
    IF ( g_causal_ref   IS NULL OR g_nss     IS NULL
       OR g_nombre      IS NULL OR g_ape_mat IS NULL
       OR g_ape_pat     IS NULL OR g_ident   IS NULL
       OR g_causal_adai IS NULL OR g_entidad IS NULL )  THEN

        LET ret_respuesta.r_g_res_op       = 100
        LET ret_respuesta.r_g_cod_rechazo  = 99 --ERV
        LET ret_respuesta.r_g_imp_viv7292  = 0 
        
        CALL ERRORLOG("Se deben capturar todos los datos")
        RETURN             
    END IF           
   
    -- sep 19 2012
    -- ISVH se busca el id_derechohabiente del nss que hace la solicitud
    SELECT id_derechohabiente
    INTO   g_id_derechohabiente
    FROM   afi_fondo72
    WHERE  nss = g_nss

    SELECT COUNT(*)
      INTO v_count_nss 
      FROM  afi_fondo72
     WHERE  nss = g_nss

    IF v_count_nss  <= 0 THEN
       --No existe el nss
       CALL ERRORLOG("No existe el nss")
       LET ret_respuesta.r_g_res_op       = 100
       LET ret_respuesta.r_g_cod_rechazo  = 999 --ERV
       LET ret_respuesta.r_g_imp_viv7292  = 0 
      RETURN    
    END IF 
    
       
    -- se valida si existe otra solicitud en tramite
    IF ( fn_rechazo_por_tramite() ) THEN
       CALL ERRORLOG("Se rechaza por tramite")
       RETURN 
    END IF
    
    -- si no se rechazo por tramite    
    -- valida el causal de retiro seleccionado 
    SELECT COUNT(*) 
      INTO v_count_bnd
      FROM ret_causal_retiro
     WHERE causal_retiro = g_causal_ref;   

    IF ( v_count_bnd <= 0 ) THEN
       -- se le regresa el cliente rechazo ya qe no coincide con la tabla de causal retiro
       -- debe ser entre 01-04
       LET ret_respuesta.r_g_res_op       = 100
       LET ret_respuesta.r_g_cod_rechazo  = 77 --ERV
       LET ret_respuesta.r_g_imp_viv7292  = 0 
       
       CALL ERRORLOG("No coincide con la tabla de causal de retiro")
       RETURN 
    END IF      

    
    DECLARE cr_fondo72 CURSOR FOR
    SELECT nss
           ,id_derechohabiente
           ,cta.id_afi_fondo72
           ,importe
           ,rfc
           ,0,0 --estatus ,cod rechazo
    FROM   afi_fondo72 afi,
           cta_fondo72 cta
    WHERE  afi.id_afi_fondo72 = cta.id_afi_fondo72
    AND    nss                = g_nss
    AND    subcuenta          = 40 

    -- para cada registro encontrado
    FOREACH cr_fondo72 INTO g_r_tmp_id_fondo72.*                        
       LET  g_id_derechohabiente = g_r_tmp_id_fondo72.id_derechohabiente            
       
       -- se verifica el tipo de causal     
       CASE g_causal_ref
          WHEN 1 -- termino de relacion laboral
             CALL fn_term_rel_lab()
          
          WHEN 2 -- pension IMSS
             CALL fn_res_pens_imss()
          
          WHEN 3 -- plan privado de pension
             CALL fn_plan_privad_def()
          
          WHEN 4 -- plan privado
             CALL fn_plan_privad_def()
          
          OTHERWISE 
             --DISPLAY "No existe opcion"
             EXIT FOREACH 
       END CASE
    END FOREACH 

    CALL ERRORLOG("se obtiene saldo de viv72, tanto adicional de la solicitud")
    SELECT SUM(saldo_viv72) , SUM(tanto_adicional)
      INTO g_acc_pesos, g_tanto_adicional
      FROM ret_det_fondo72
      WHERE estado_detalle = 1
        AND id_solicitud = g_sq_ret_solicitud   
        
    IF ( g_acc_pesos > 0 ) THEN
       CALL ERRORLOG("Genera solicitud (10,0)")
       CALL fn_genera_status(10,0) RETURNING v_id_solicitud
       CALL ERRORLOG("ID_SOL")
       CALL ERRORLOG(v_id_solicitud)
    ELSE
       LET v_cod_rechazo     = 66  --no se encontro registros en ret_fondo72--ERV        
       
       SELECT MIN(cod_rechazo)
         INTO v_cod_rechazo
         FROM ret_det_fondo72
        WHERE estado_detalle = 0
          AND id_solicitud = g_sq_ret_solicitud 
        
       IF ( v_cod_rechazo <> 66 AND v_cod_rechazo IS NOT NULL ) THEN
          --DISPLAY v_cod_rechazo
          CALL fn_genera_status(100,v_cod_rechazo) RETURNING v_id_solicitud
       ELSE
          LET v_cod_rechazo      = 66 -- ERV no se encontro registros en ret_fondo72
          
          -- no se encontro el nss solicitado 
          LET ret_respuesta.r_g_res_op       = 100
          LET ret_respuesta.r_g_cod_rechazo  = v_cod_rechazo
          LET ret_respuesta.r_g_imp_viv7292  = 0 
          
          -- codigo de error cuando no se genere la solicitd de retiro 
          -- en ret_fondo72             
          CALL fn_genera_status(100,v_cod_rechazo) RETURNING v_id_solicitud 
          RETURN  
       END IF    
    END IF 
       
       
END FUNCTION 

-- ===============================================================
-- Objetivo validaciones para el tipo de retiro relacion laboral
FUNCTION fn_term_rel_lab()
   DEFINE r_saldo              DECIMAL(19,14)
   DEFINE r_edad               SMALLINT
   DEFINE r_b_paso             SMALLINT 
   
   LET g_tanto_adicional = 0
   LET r_saldo           = 0
  
   CALL fn_edad_derechohab() RETURNING r_edad
  
   IF ( r_edad >= 50 ) THEN
      -- valida vigencia de resolucion y recupera id_datamart 
      CALL fn_valida_resolucion() RETURNING r_b_paso
      
      IF ( r_b_paso ) THEN
         CALL fn_recupera_saldo() RETURNING r_saldo
         
         IF ( r_saldo > 0 ) THEN
            DISPLAY "Saldo en cuenta de vivienda :",  r_saldo  
            
            #Obtener monto del movimiento del derechohabiente
            #funcion de calculo de saldo de retiro72-92
            IF ( fn_rec_credito() > 0 ) THEN
               DISPLAY "tiene credito vigente"
               DISPLAY "se genera rechazo por credito"
               CALL fn_rechazo_credito(r_saldo) --se genero rechazo por credito
            ELSE 
               DISPLAY "no tiene credito vigente"
               IF ( fn_trab_pensionado() ) THEN
                  DISPLAY "trabajador cuenta con pension"
                  DISPLAY "se entrega dos veces el saldo"
                  
                  #se suprmio esta parte por campo nuevo
                  --LET r_saldo           = r_saldo * 2
                  LET g_tanto_adicional = r_saldo
                  #DISPLAY "r_saldo = r_saldo * 2", r_saldo
                  CALL fn_genera_preautorizacion(r_saldo,0)
               ELSE 
                  DISPLAY "trabajador no es pensionado"
                  DISPLAY "se genera preautorizacion"
                  DISPLAY r_saldo 
                  CALL fn_genera_preautorizacion(r_saldo,0)
               END IF
            END IF 
         ELSE 
            DISPLAY "rechaza solicitud por insuficiencia de saldo"
            DISPLAY "se notifica al canal"
            CALL fn_rechazo_sin_saldo(r_saldo)  --se genero rechazo por insuficiencia de saldo
         END IF
      ELSE
         IF ( fn_rec_fin_lab() = 1 ) THEN

            CALL fn_recupera_saldo() RETURNING r_saldo
            IF ( r_saldo > 0 ) THEN
               --DISPLAY "saldo en cuenta de vivienda :",  r_saldo  
               
               #Obtener monto del movimiento del derechohabiente  
               #funcion de calculo de saldo de retiro72-92
               IF ( fn_rec_credito() > 0 ) THEN
                  DISPLAY "tiene credito vigente"
                  DISPLAY "se genera rechazo por credito"
                  CALL fn_rechazo_credito(r_saldo) --se genero rechazo por credito
               ELSE 
                  DISPLAY "trabajador es pensionado"
                  DISPLAY "se genera preautorizacion"                      
                  DISPLAY "no tiene credito vigente"
                  IF ( fn_trab_pensionado() ) THEN
                      DISPLAY "trabajador cuenta con pension"
                      DISPLAY "se entrega dos veces el saldo"
                     
                      #se suprmio esta parte por campo nuevo
                      --LET r_saldo           = r_saldo * 2
                      LET g_tanto_adicional = r_saldo
                      #DISPLAY "r_saldo = r_saldo * 2", r_saldo
                    
                      DISPLAY "se genera preautorizacion"
                      CALL fn_genera_preautorizacion(r_saldo,0)
                  
                  ELSE 
                     DISPLAY "trabajador no es pensionado"
                     DISPLAY "se genera preautorizacion"
                     DISPLAY r_saldo 
                     CALL fn_genera_preautorizacion(r_saldo,0)
                  END IF
               END IF 
            ELSE 
               DISPLAY "rechaza solicitud por insuficiencia de saldo"
               DISPLAY "se notifica al canal"
               CALL fn_rechazo_sin_saldo(r_saldo)  --se genero rechazo por insuficiencia de saldo
            END IF            
         ELSE 
           CALL fn_rechazo_ult_rel_lab(r_saldo)  --ultima relacion laboral > de 1 año
         END IF
      END IF 
   ELSE 
     DISPLAY "no es mayor de 50 años"
     CALL fn_rechazo_menor_50(r_saldo)  --se genero rechazo por edad < 50
   END IF
END FUNCTION

-- =============================================================
-- Objetivo validaciones para el tipo de retiro pension imss
FUNCTION fn_res_pens_imss() 
   DEFINE r_saldo         DECIMAL(19,14)
   
   LET r_saldo           = 0
   LET g_tanto_adicional = 0

   IF fn_rechazo_por_tramite() THEN
     RETURN 
   END IF 
   
   --valida que exista resolucion en datamart
   IF ( fn_resolucion_spes() ) THEN
      --valida vigencia de resolucion y recupera id_datamart 
      IF fn_valida_resolucion() THEN
         --valida que si la resolucion es IP sea mayor al 50%
         IF fn_incapacidad_parcial() = 1 THEN  
            --DISPLAY "existe resolucion vigente"
            CALL fn_recupera_saldo() RETURNING r_saldo
            IF r_saldo > 0 THEN
               --DISPLAY "Cuenta con saldo en su cuenta de vivienda"
               IF fn_rec_credito() THEN
                  --DISPLAY "tiene credito vegente"
                  CALL fn_rechazo_credito(r_saldo)
               ELSE
                  --DISPLAY "no tiene credito vegente"
                  --LET r_saldo           = r_saldo * 2 
                  LET g_tanto_adicional = r_saldo
                  CALL fn_genera_preautorizacion(r_saldo,0)
               END IF 
            ELSE
               DISPLAY "Se genera rechazo por falta de saldo en subcuenta e vivienda"
               DISPLAY "Se notifica al canal"
               CALL fn_rechazo_sin_saldo(r_saldo)
            END IF 
         ELSE 
            --DISPLAY "la incapacidad no es mayor a 50%"
            CALL fn_rechazo_incapacidad(r_saldo)
         END IF        
      ELSE
         DISPLAY "no existe resolucion vigente"
         CALL fn_rechazo_sin_resolucion(r_saldo)
      END IF       
   ELSE 
      --DISPLAY "no tiene resolucion en el spes"
      CALL fn_rechazo_sin_resolucion(r_saldo)
   END IF
END FUNCTION 

-- ===================================================================
-- Objetivo validaciones para el tipo de retiro plan privado y defuncion 
FUNCTION fn_plan_privad_def()
DEFINE r_saldo    DECIMAL(19,14)

   LET g_tanto_adicional = 0
   LET r_saldo           = 0
   
   --valida vigencia de resolucion y recupera id_datamart  
   IF fn_valida_resolucion() THEN 
      DISPLAY "tiene resolucion vigente"
      CALL fn_recupera_saldo() RETURNING r_saldo
      IF r_saldo > 0 THEN
         --DISPLAY "tiene saldo"
         IF fn_rec_credito() THEN
            DISPLAY "tiene saldo vigente"
            DISPLAY "se genera rechazo por credito"
            DISPLAY "se notifica al canal"
            CALL fn_rechazo_credito(r_saldo)
         ELSE
            DISPLAY "no tiene credito vigente"
            --LET r_saldo           = r_saldo * 2 
            LET g_tanto_adicional = r_saldo 
            CALL fn_genera_preautorizacion(r_saldo,1)
         END IF 
      ELSE 
         DISPLAY "no tiene saldo"
         DISPLAY "se genera rechazo por insuficiencia de saldo"
         DISPLAY "se notifica al canal"
         CALL fn_rechazo_sin_saldo(r_saldo) --se genera recahzo por insuficiencia de saldo
      END IF
   ELSE 
      DISPLAY "no tiene resolucion vigente"
      CALL fn_rechazo_sin_resolucion(r_saldo)  --se genera solicitud por resolucion vigente
   END IF 
END FUNCTION 

-- ======================================================
-- Objetivo genera solicitud de rechazo por sin resolucion 
FUNCTION fn_rechazo_sin_resolucion(p_saldo)
DEFINE p_saldo DECIMAL(19,14)
  
   --CALL fn_genera_solicitud(100,60,p_saldo) --se genero rechazo por sin resolucion
   CALL fn_genera_solicitud(100,90,p_saldo) --se genero rechazo por sin resolucion  
END FUNCTION  

-- =========================================================
-- Objetivo genera solicitud de rechazo por incapacidad menor a 50%
FUNCTION fn_rechazo_incapacidad(p_saldo)
DEFINE p_saldo DECIMAL(19,14)  

   CALL fn_genera_solicitud(100,140,p_saldo) --se genero rechazo por incapacidad menor a 50%
END FUNCTION  

-- ==========================================================
-- Objetivo genera solicitud de rechazo por sin saldo
FUNCTION fn_rechazo_sin_saldo(p_saldo)
DEFINE p_saldo DECIMAL(19,14)

   CALL fn_genera_solicitud(100,10,p_saldo) --se genero rechazo por insuficiencia de saldo
END FUNCTION  

-- =========================================================
-- Objetivo genera solicitud de rechazo por credito
FUNCTION fn_rechazo_credito(p_saldo)
DEFINE p_saldo DECIMAL(19,14)

   CALL fn_genera_solicitud(100,20,p_saldo) --se genero rechazo por credito
END FUNCTION

-- ==========================================================
-- Objetivo genera solicitud de rechazo por edad menor a 50
FUNCTION fn_rechazo_menor_50(p_saldo)
DEFINE p_saldo DECIMAL(19,14)

   CALL fn_genera_solicitud(100,40,p_saldo) --se genero rechazo edad < 50 
END FUNCTION

-- ===========================================================
-- Objetivo genera solicitud de rechazo por relacion laboral
FUNCTION fn_rechazo_ult_rel_lab(p_saldo)
DEFINE p_saldo DECIMAL(19,14)

   CALL fn_genera_solicitud(100,50,p_saldo) --se genero rechazo ultima relacion laboral > de 1 año
END FUNCTION

-- ===========================================================
-- Objetivo valida si cuenta con resolucion vigente 
FUNCTION fn_resolucion_spes()
DEFINE v_bnd_paso    SMALLINT
DEFINE v_id_datamart DECIMAL(9,0)

   LET v_id_datamart = 0
   LET v_bnd_paso    = 0 

   SELECT COUNT(a.id_derechohabiente)
     INTO v_id_datamart
     FROM ret_datamart a
          ,ret_matriz_derecho b
   WHERE  a.nss = g_nss
     AND  diag_registro    = 101
     AND  b.tpo_retiro     = 'F'
     AND  a.regimen        = b.regimen
     AND  a.tpo_seguro     = b.tpo_seguro
     AND  a.tpo_pension    = b.tpo_pension
     AND  a.tpo_prestacion = b.tpo_prestacion   

   IF ( v_id_datamart = 0 ) THEN
      LET v_bnd_paso    = 0
      LET v_id_datamart = 0
   ELSE
      LET v_bnd_paso = 1
      LET v_id_datamart = g_id_datamart
   END IF 
   
   RETURN v_bnd_paso
END FUNCTION 

-- =============================================================
-- Objetivo recupera si el derechohabiente cuenta con pension  
FUNCTION fn_trab_pensionado() 
DEFINE v_id_datamart DECIMAL(9,0)
DEFINE v_pension     SMALLINT  

   SELECT COUNT(a.nss)
     INTO v_id_datamart
     FROM ret_datamart a
         ,ret_matriz_derecho b
    WHERE a.nss            = g_nss
      AND diag_registro    = 101
      AND b.tpo_retiro     = 'E'
      AND (a.tpo_pension   <> 'IP'
       OR (a.tpo_pension   = 'IP' AND a.porcentaje_valuacion >= 50))
      AND a.regimen        = b.regimen
      AND a.tpo_seguro     = b.tpo_seguro
      AND a.tpo_pension    = b.tpo_pension
      AND a.tpo_prestacion = b.tpo_prestacion  

   IF ( v_id_datamart > 0 ) THEN
      LET  v_pension = 1
   ELSE 
      LET  v_pension = 0
   END IF   
    
   --DISPLAY "valida si derechohabiente tiene pension"
   --1 = Se encontro pension otorgada
   --0 = No se encontro pension otorgada 
   RETURN v_pension
END FUNCTION

-- ===================================================================
-- Objetivo valida si el derechohabiente tiene alguna resolucion vigente
FUNCTION fn_valida_resolucion()
DEFINE v_bnd_paso SMALLINT
--DISPLAY "valida si el derechohabiente tiene alguna resolucion vigente si paso"
   
   LET g_id_datamart = 0
   LET v_bnd_paso    = 0 
   
   CASE g_causal_ref
     WHEN 3
      SELECT UNIQUE NVL(id_datamart,0) 
        INTO g_id_datamart
        FROM ret_datamart a
            ,ret_matriz_derecho b
       WHERE a.nss = g_nss
         AND diag_registro    = 101
         AND b.tpo_retiro     = 'F'
         AND TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR  
         AND a.regimen        = b.regimen
         AND a.tpo_seguro     = 'PP'
         AND a.tpo_pension    = b.tpo_pension
         AND a.tpo_prestacion = b.tpo_prestacion
   
     WHEN 4
      SELECT UNIQUE NVL(id_datamart,0) 
        INTO g_id_datamart
        FROM ret_datamart a
            ,ret_matriz_derecho b
       WHERE a.nss = g_nss
         AND diag_registro    = 101
         AND b.tpo_retiro     = 'F'
         AND TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR  
         AND a.regimen        = b.regimen
         AND a.tpo_seguro     = b.tpo_seguro
         AND a.tpo_pension    IN ('OR','VI','VO','AS')
         AND a.tpo_prestacion = b.tpo_prestacion
   
     OTHERWISE  
      SELECT UNIQUE NVL(id_datamart,0) 
        INTO g_id_datamart
        FROM ret_datamart a
            ,ret_matriz_derecho b
       WHERE nss = g_nss
         AND diag_registro = 101
         AND b.tpo_retiro     = 'E'
         AND TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR
       AND a.regimen          = b.regimen
         AND a.tpo_seguro     = b.tpo_seguro
         AND a.tpo_pension    = b.tpo_pension
         AND a.tpo_prestacion = b.tpo_prestacion  
   END CASE 
   
   IF ( g_id_datamart = 0 ) THEN
      LET v_bnd_paso = 0
   ELSE
      LET v_bnd_paso = 1
   END IF   

   RETURN v_bnd_paso
END FUNCTION 

-- ====================================================================
-- Objetivo recupera solictud a ws de infonavit para recuperar relacion 
-- laboral del derchohabiente
FUNCTION fn_rec_fin_lab() --ERV
--DISPLAY "valida el tiempo sin actividd laboral del derechohabiente"
--retorna la cantidad de dias de salida

   RETURN 1    -- tiene mas de un año, 2 no tiene mas de un año
END FUNCTION 


#Objetivo valdaciones de control de autorizacdion ependiendo del monto tope
FUNCTION fn_genera_preautorizacion(r_saldo, v_b_def)
DEFINE r_saldo      DECIMAL(19,14),
       --r_monto_tope DECIMAL(19,14)#se suprimio este codigo ya que las validaciones se realizaran del lado del cliente
       v_b_def      SMALLINT      -- 0 = relacion laboral/pension imss 
                                  -- 1 = plan privado/ defuncion
   
   IF ( fn_rechazo_por_tramite() ) THEN
      RETURN 
   END IF 
      
   CALL fn_genera_solicitud(10,0,r_saldo) --se genero solicitud estatus capturado

  #se suprimio este codigo ya que las validaciones se realizaran del lado del cliente 
   --falta saber de donde se tomara el monto tope
  --CALL fn_rec_valor_tope(r_saldo) RETURNING r_monto_tope
   --LET r_monto_tope = 2541.0
   --
   --IF r_saldo >  r_monto_tope THEN
      --DISPLAY "se solicita a operaciones autorizacion de pago" 
   --ELSE
      --IF v_b_def THEN 
         --IF fn_solicitud_cesi() THEN 
            --DISPLAY "se solicita a supervisor autorizacion para pago"
         --ELSE 
            --DISPLAY "se solicita a operaciones autorizacion para pago"
         --END IF
      --ELSE 
         --DISPLAY "se solicita a supervisor autorizacion para pago"
      --END IF 
   --END IF 
   
END FUNCTION

-- ====================================================================
-- Objetivo valida si existe credito para el derechohabiente
FUNCTION fn_rec_credito()
DEFINE v_credito  SMALLINT 

   -- DISPLAY "busco credito default manda cero por falta de tabla"
   LET v_credito = 0
  
   SELECT COUNT(*)
     INTO v_credito
     FROM cta_credito
    WHERE id_derechohabiente  = g_id_derechohabiente
    --AND TODAY BETWEEN f_credito AND  f_credito UNITS YEAR  --ERV
    
   RETURN v_credito
END FUNCTION

-- =====================================================================
-- Objetivo calcula el saldo en la subcuenta de vivienda 
FUNCTION fn_recupera_saldo()
DEFINE r_saldo_acciones    DECIMAL(22,2),
       r_saldo_pesos       DECIMAL(22,2)

   LET r_saldo_pesos  = 0 

   -- el importe se esta calculando al momentode recuperar los registros
   LET r_saldo_pesos = g_r_tmp_id_fondo72.importe

    --SELECT NVL(SUM(importe),0)
      --INTO r_saldo_pesos
      --FROM cta_fondo72
     --WHERE id_afi_fondo72 IN (
              --SELECT id_afi_fondo72
                --FROM afi_fondo72
               --WHERE id_derechohabiente = g_id_derechohabiente)
     --AND importe   > 0
     --AND subcuenta = 40

    IF ( r_saldo_pesos <= 0 ) THEN
       LET r_saldo_acciones = 0
       LET r_saldo_pesos    = 0
    END IF

    --DISPLAY "r_saldo_pesos", r_saldo_pesos ," g_nss: ", g_nss , g_r_tmp_id_fondo72.importe

   LET g_acc_acciones = r_saldo_acciones
   LET g_acc_pesos    = r_saldo_pesos
   
   -- se devuelve el resultado de la funcion
   RETURN r_saldo_pesos
END FUNCTION  

-- =================================================================
-- Objetivo calcula la edad del derechohabiente apartir de afi_fondo72
FUNCTION fn_edad_derechohab()
--Se valida la edad del derechohabiente --ERV 
-- se modifico afi_fondo72 por afi_fondo72
DEFINE v_f_nac_rfc   CHAR(13),
       v_f_year       CHAR(2),
       v_f_month      CHAR(2),
       v_f_day        CHAR(2),
       v_f_year_today CHAR(4),
       v_f_nac_bueno  DECIMAL(4,0),
       v_f_nac        DATE,
       v_edad         SMALLINT      

   -- se recupera el rfc del derechohabiente
   LET v_f_nac_rfc = g_r_tmp_id_fondo72.rfc

   IF ( v_f_nac_rfc = 0 OR v_f_nac_rfc IS NULL ) THEN
      LET v_f_nac  = -1
   ELSE
      LET v_f_year  = v_f_nac_rfc [5,6]
      LET v_f_month = v_f_nac_rfc [7,8]
      LET v_f_day   = v_f_nac_rfc [9,10]
 
      LET v_f_year_today = YEAR(TODAY)
     
      -- se valida el siglo a la fecha del rfc recuperada
      IF ( v_f_year_today[3,4] < v_f_year ) THEN 
         LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
         --DISPLAY v_f_nac_bueno
      ELSE 
         LET v_f_nac_bueno =  v_f_year_today[1,2] || v_f_year
         --DISPLAY v_f_nac_bueno
      END IF 
    
      LET v_f_nac  = MDY(v_f_month,v_f_day,v_f_nac_bueno)    

      #se valida qe la fecha sea correcta si no lo es se rechaza solicitud por edad
      IF ( v_f_nac = 0 ) THEN
        LET v_f_nac  = -1
      ELSE
        #se calcula la edad del trabajador  
        IF ( MONTH(TODAY) >= MONTH(v_f_nac) AND DAY(TODAY) >= DAY(v_f_nac) ) THEN 
           LET v_edad = YEAR(TODAY) - YEAR(v_f_nac)
        ELSE
           LET v_edad = (YEAR(TODAY) - YEAR(v_f_nac)) - 1
        END IF 
      END IF 
   END IF 
   
   -- se devuelve la edad calculada
   RETURN v_edad
END FUNCTION 

#se suprio parte ya que esta validacion estara controlada por el cliente
--FUNCTION fn_solicitud_cesi()
   --DISPLAY "solicitud cesi"
  --RETURN 1 
--END FUNCTION 

--FUNCTION  fn_rec_valor_tope(p_saldo)
--DEFINE p_saldo decimal(19,14)
--DEFINE v_monto_tope decimal(19,14)
--
   --LET v_monto_tope = 2541.0
   --RETURN  v_monto_tope
--END FUNCTION 

#Objetvo genera el registro de solicitud
FUNCTION fn_genera_solicitud(p_estatus,p_rechazo,p_saldo)
DEFINE p_estatus      SMALLINT 
      ,p_rechazo      SMALLINT
      ,p_saldo        DECIMAL(19,14)
      ,v_estatus      SMALLINT 
      ,v_count_reg    SMALLINT
      ,f_saldo        DATE
      ,h_saldo        VARCHAR(20) 
      

   DISPLAY "Generando la solicitud"

   LET v_count_reg = 0
   
   -- si existe rechazo por tramite
   IF ( fn_rechazo_por_tramite() ) THEN
      RETURN 
   END IF 
   
   --,r_id_solicitud SMALLINT 
   IF ( p_estatus = 100 ) THEN
      LET p_saldo   = 0
      LET v_estatus = 0   --agregada ala solicitud
   ELSE
      LET v_estatus = 1   --no se agrego ala solicitud 
   END IF  

   IF ( g_bnd_uso_seq = 1 ) THEN 
      SELECT seq_ret_solicitud.NEXTVAL
        INTO g_sq_ret_solicitud
        FROM systables 
       WHERE tabname = "seq_ret_solicitud"
       
      LET g_bnd_uso_seq = 0
      DISPLAY g_sq_ret_solicitud
   END IF         

   SELECT COUNT(*)
     INTO v_count_reg 
     FROM ret_det_fondo72
    WHERE id_afi_fondo72 = g_r_tmp_id_fondo72.id_afi_fondo72
      AND id_solicitud   = g_sq_ret_solicitud
   
   IF ( v_count_reg <= 0 ) THEN 
      LET f_saldo = TODAY
      LET h_saldo = CURRENT HOUR TO SECOND
   {
      PREPARE sp_solicitud FROM "EXECUTE PROCEDURE sp_insert_solicitud_ret_det_fondo72(?,?,?,?,?,?,?,?,?)"
      EXECUTE sp_solicitud USING g_r_tmp_id_fondo72.id_afi_fondo72 --id_afi_fondo72 
                                         ,g_sq_ret_solicitud                --id_solicitud
                                         ,p_saldo                           --saldo_viv72
                                         ,g_tanto_adicional                 --tanto_adicional
                                         ,"0"                                 --id_datamart
                                         ,f_saldo                             --f_saldo
                                         ,h_saldo                             --h_saldo
                                         ,v_estatus                         --estado_detalle
                                         ,p_rechazo  
   }                             
      INSERT INTO ret_det_fondo72 (
         id_afi_fondo72  ,
         id_solicitud    ,
         saldo_viv72     ,
         tanto_adicional , 
         id_datamart     ,
         f_saldo         ,
         h_saldo         ,
         estado_detalle  ,
         cod_rechazo 
         )
      VALUES ( 
         g_r_tmp_id_fondo72.id_afi_fondo72 --id_afi_fondo72 
         ,g_sq_ret_solicitud                --id_solicitud
         ,p_saldo                           --saldo_viv72
         ,g_tanto_adicional                 --tanto_adicional
         ,0                                 --id_datamart
         ,TODAY                             --f_saldo
         ,CURRENT HOUR TO SECOND            --h_saldo
         ,v_estatus                         --estado_detalle
         ,p_rechazo                         --cod_rechazo
         )
      DISPLAY "Resultado insercion: ", SQLCA.SQLCODE
   END IF 

END FUNCTION 

-- ==============================================================================
-- Objetivo genera solicitudes y regresa el estatus y cod_rechazo segn corresponda
FUNCTION fn_genera_status(p_estatus,p_causa_rec)
DEFINE v_id_solicitud       SMALLINT,
       --v_tipo_r             SMALLINT,
       p_estatus            SMALLINT,
       p_causa_rec          SMALLINT,
       v_sql_error          INTEGER, -- error en SQL
       v_isam_error         INTEGER, -- error ISAM
       v_mensaje            VARCHAR(250) -- mensaje devuelto por el SP
       
   
   LET g_causal_retiro = g_causal_ref
   LET v_id_solicitud  = 0 
       
   IF ( fn_rechazo_por_tramite() ) THEN
      RETURN 1
   END IF     

   IF ( p_estatus = 10 AND p_causa_rec = 0 ) THEN
      CALL fn_generacion_referencias() RETURNING  g_refer         
   ELSE 
      LET g_refer = "NA"
   END IF

   DISPLAY "g_sq_ret_solicitud  " ," ",g_sq_ret_solicitud
            ,"\n","g_id_derechohabiente"," ",g_id_derechohabiente
            ,"\n","p_causa_rec         "," ",p_causa_rec
            ,"\n","p_estatus           "," ",p_estatus
            ,"\n","g_refer             "," ",g_refer
            ,"\n","g_acc_pesos         "," ",g_acc_pesos
            ,"\n","g_tanto_adicional   "," ",g_tanto_adicional
            ,"\n","g_causal_retiro     "," ",g_causal_retiro
            ,"\n","g_id_datamart       "," ",g_id_datamart
            ,"\n","g_ident             "," ",g_ident
            ,"\n","g_nombre            "," ",g_nombre
            ,"\n","g_ape_pat           "," ",g_ape_pat
            ,"\n","g_ape_mat           "," ",g_ape_mat      
            ,"\n","g_entidad           "," ",g_entidad 
            ,"\n","g_causal_adai       "," ",g_causal_adai      
            ,"\n","'A'                 "," ",'A'
                              
   IF ( g_id_derechohabiente IS NULL ) THEN
      LET g_id_derechohabiente = 0 
   END IF 
                                
   PREPARE sp_det_solicitud FROM "EXECUTE PROCEDURE sp_insert_solicitud_retiro_fondo_ahorro(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   EXECUTE sp_det_solicitud USING g_sq_ret_solicitud
                              ,g_id_derechohabiente
                              ,p_causa_rec
                              ,p_estatus
                              ,g_refer
                              ,g_acc_pesos
                              ,g_tanto_adicional
                              ,g_causal_retiro
                              ,g_id_datamart
                              ,g_ident
                              ,g_nombre
                              ,g_ape_pat
                              ,g_ape_mat      
                              ,g_entidad 
                              ,g_causal_adai      
                              ,'A'
   INTO v_sql_error, v_isam_error, v_mensaje
   
   -- si se inserto la solicitud correctamente
   IF ( v_sql_error = 0 ) THEN
      -- se asigna la rspuesta para el cliente en el caso existoso
      LET ret_respuesta.r_g_res_op       = p_estatus
      LET ret_respuesta.r_g_cod_rechazo  = p_causa_rec
      LET ret_respuesta.r_g_imp_viv7292  = g_acc_pesos 
      LET ret_respuesta.r_g_num_ret      = g_refer
   ELSE
      -- se envia respuesta con error
      LET ret_respuesta.r_g_res_op       = 100 -- rechazo
      LET ret_respuesta.r_g_cod_rechazo  = v_sql_error
      LET ret_respuesta.r_g_imp_viv7292  = 0
      LET ret_respuesta.r_g_num_ret      = 0
   END IF
      
   -- se devuelve el ID_SOLICITUD
   RETURN v_id_solicitud
END FUNCTION

-- ==============================================================================
-- Objetivo genera referencia bancaria 
FUNCTION fn_generacion_referencias()
DEFINE v_refer     CHAR(12)    ,
       v_pago_fico DECIMAL(2,0),  --Concepto de pago para FICO (Tesorería)
       v_fec_dev   DECIMAL(2,0),  --Año en que se solicita la Devolución de Fondo de Ahorro 
       v_ref_pag   DECIMAL(2,0),  --Referencia a que se paga Fondo de Ahorro 72-92 
       v_causal    DECIMAL(1,0)  --1 CESANTIA 
                                 --2 VEJEZ
                                 --3 PLAN PRIVADO DE PENSION
                                 --4 INCAPACIDAD TOTAL
                                 --5 INCPACIDAD PARCIAL < AL 50 %
                                 --6 INVALIDEZ 
                                 --7 DEFUNCION
                                 --8 TERMINACIÓN LABORAL o 50 años cumplidos + 1 año sin RL. 
                                 --9 DESCONOCIDO
--Se toma de una tabla. 
--Cuando se vaya a generar la primera referencia en SAFRE, se deberá inicializar 
--la tabla correspondiente con el número que se tenga en Legacy o ADS-CONSIST 
--para continuar con la generación subsecuente de referencias 
DEFINE v_folio_cons DECIMAL(5,0),
       v_tpo_pension CHAR(2)

   LET v_pago_fico = 50
   LET v_fec_dev   = TODAY USING "yy"
   LET v_ref_pag   = 72
   
   CASE  g_causal_ref 
      WHEN 1 
         LET v_causal = 8      --Se le coloca 8 --TERMINACIÓN LABORAL o 50 años cumplidos + 1 año sin RL. 
      
      WHEN 2                  --Se le coloca 1,2,4,6 segun sea el caso
                              --CESANTIA ,VEJEZ,INCAPACIDAD TOTAL,INVALIDEZ
         LET v_tpo_pension = NULL
           
         SELECT tpo_pension
           INTO v_tpo_pension
           FROM ret_matriz_derecho
          WHERE id_ret_matriz_derecho = g_id_datamart
          
         CASE v_tpo_pension
            WHEN 'CE' 
               LET v_causal = 1
            WHEN 'VE' 
               LET v_causal = 2
            WHEN 'IP' 
               LET v_causal = 4
            WHEN 'IN' 
               LET v_causal = 6
            OTHERWISE
               --display "que hace si no coincide con ningncodigo anterior, v_tpo_pension"
               LET v_causal = 1  --ERV 
         END CASE
      
      WHEN 3
         LET v_causal = 3      --Se le coloca 3 --PLAN PRIVADO DE PENSION
        
      WHEN 4
         LET v_causal = 7      --Se le coloca 7 --DEFUNCION
        
   END CASE 
   
   SELECT seq_ret_ref_bancaria.NEXTVAL
     INTO v_folio_cons
     FROM systables
    WHERE tabname = 'seq_ret_ref_bancaria' 
   
   LET v_refer = v_pago_fico USING "&&", v_fec_dev USING "&&", v_ref_pag USING "&&"
                ,v_causal USING "&", v_folio_cons USING "&&&&&"
   
   RETURN v_refer
END FUNCTION

-- ==============================================================================
-- Objetivo valida que no exista otra solicitd en proceso de tramite (pendiente de algun proceso con el mismo nss) 
FUNCTION fn_rechazo_por_tramite()
DEFINE v_count_bnd SMALLINT,
       v_cadena    STRING

   LET v_count_bnd = 0

   CALL ERRORLOG("Validando si existe el id_derechohabiente en ret_fondo_ahorro")
   LET v_cadena = "ID_DER: ", g_id_derechohabiente
   CALL ERRORLOG(v_cadena)

   -- busca al derechohabiente en la tabla ret_fondo_ahorro
   SELECT COUNT(*)
     INTO v_count_bnd
     FROM ret_fondo_ahorro
    WHERE id_derechohabiente = g_id_derechohabiente
      AND id_derechohabiente <> 0
      AND  estado_solicitud IN (10,12,14,50)


   -- si lo encuentra
   IF ( v_count_bnd > 0 ) THEN
      LET ret_respuesta.r_g_res_op      = 100
      LET ret_respuesta.r_g_cod_rechazo = 30
      LET ret_respuesta.r_g_imp_viv7292 = 0 
      LET ret_respuesta.r_g_num_ret     = "NA"
   ELSE

      -- si no lo encontro, lo busca en afi_fondo72
      SELECT COUNT(*)
        INTO v_count_bnd
        FROM ret_det_fondo72  det,
             ret_fondo_ahorro ret,
             afi_fondo72    afi
       WHERE ret.id_solicitud = det.id_solicitud
         AND det.estado_detalle = 1
         AND afi.id_afi_fondo72 = det.id_afi_fondo72
         AND afi.nss = g_nss
         AND ret.estado_solicitud IN (10,12,14,50)
      
      IF ( v_count_bnd > 0 ) THEN
         LET ret_respuesta.r_g_res_op      = 100
         LET ret_respuesta.r_g_cod_rechazo = 30
         LET ret_respuesta.r_g_imp_viv7292 = 0 
         LET ret_respuesta.r_g_num_ret     = "NA"
      END IF
   END IF


{-- codigo original
   -- si lo encuentra
   IF ( v_count_bnd > 0 ) THEN
      LET ret_respuesta.r_g_res_op      = 100
      LET ret_respuesta.r_g_cod_rechazo = 30
      LET ret_respuesta.r_g_imp_viv7292 = 0 
      LET ret_respuesta.r_g_num_ret     = "NA"
      
   END IF

   -- si no lo encontro, lo busca en afi_fondo72
   SELECT COUNT(*)
     INTO v_count_bnd
     FROM ret_det_fondo72  det,
          ret_fondo_ahorro ret,
          afi_fondo72    afi
    WHERE ret.id_solicitud = det.id_solicitud
      AND det.estado_detalle = 1
      AND afi.id_afi_fondo72 = det.id_afi_fondo72
      AND afi.nss = g_nss
      AND ret.estado_solicitud IN (10,12,14,50)

   IF ( v_count_bnd > 0 ) THEN
      LET ret_respuesta.r_g_res_op      = 100
      LET ret_respuesta.r_g_cod_rechazo = 30
      LET ret_respuesta.r_g_imp_viv7292 = 0 
      LET ret_respuesta.r_g_num_ret     = "NA"
   END IF
}
   RETURN v_count_bnd
END FUNCTION  

-- ==============================================================================
-- Objetivo recupera si el nss cuenta con pension por incapacidad mayor al 50%  
FUNCTION fn_incapacidad_parcial() 
DEFINE v_id_datamart DECIMAL(9,0),
       v_pension     SMALLINT  

   SELECT COUNT(a.nss)
     INTO v_id_datamart
     FROM ret_datamart a
         ,ret_matriz_derecho b
    WHERE a.nss            = g_nss
      AND diag_registro    = 101
      AND a.regimen        = b.regimen
      AND a.tpo_seguro     = b.tpo_seguro
      AND a.tpo_pension    = b.tpo_pension
      AND a.tpo_prestacion = b.tpo_prestacion  
   
   IF ( v_id_datamart > 0 ) THEN
      LET  v_pension = 1
   ELSE 
      LET  v_pension = 0
   END IF   
   
   -- se devuelve la pension
   RETURN v_pension
END FUNCTION
