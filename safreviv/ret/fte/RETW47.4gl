--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW47                                                  #
#OBJETIVO          => WS PARA IDENTIFICAR EL PERFIL DEL PENSIONADO EN         #
#                     MI CUENTA INFONAVIT                                     #
#                     RETIRO                                                  #
#FECHA INICIO      => 29-DIC-2016                                             #
# Autor           Fecha      Modificación                                     #
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
DEFINE ws_solicitud_in RECORD
         nss              CHAR(11) -- nss del trabajador
       END RECORD,
       -- registro de respuesta
       ws_solicitud_out  RECORD
         nss                 CHAR(11), --- Número de seguridad social del trabajador
         cod_rechazo         SMALLINT, -- Codigo de rechazo de la consulta
         sin_tramite         SMALLINT, -- Pensionado sin trámite de crédito
         credito_vigente     SMALLINT, -- Pensionado con crédito vigente
         credito_liquidado   SMALLINT, -- Pensionado con crédito liquidado
         credito_cancelado   SMALLINT  -- Pensionado con crédito cancelado
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
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
  
  -- se define la ruta del log
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW47."
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
      CALL fn_crea_servicio_retiro_generico(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET v_pantalla = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Perfil de Pensionado") --, STYLE="naked")
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
  CALL fn_crea_servicio_retiro_generico(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor... 20161229")

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
Nombre: fn_crea_servicio_retiro_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta los saldos disponibles
para retiro por tipo de cuenta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_retiro_generico(p_generar_WSDL)
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
    LET v_webservice = com.WebService.CreateWebService("retiroPerfilPensionado", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_perfil_pensionado","fn_ret_perfil_pensionado",ws_solicitud_in,ws_solicitud_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL v_webservice.publishOperation(op, "fn_ret_perfil_pensionado")

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
       CALL ERRORLOG("Se registro el servicio consulta del perfil del Pensionado")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Consulta del perfil del Pensionado': " || STATUS)
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
Nombre: fn_ret_perfil_pensionado
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta el perfil del Pensionado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_ret_perfil_pensionado()
DEFINE v_indice_retiro       SMALLINT,
       v_nss                 LIKE afi_fondo72.nss,
       v_rfc                 LIKE afi_fondo72.rfc,
       v_indice_modalidad    SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
	    v_requiere_DAP        SMALLINT, -- booleana que indica si se necesita DAP
       v_modalidad_procesada SMALLINT, -- Indica si ya se proceso una solicitud de ley 73
       v_resultado           SMALLINT,  -- Indica si fue o no exitoso el llamado a la función
       v_sin_tramite         SMALLINT,
       v_credito_vigente     SMALLINT,
       v_credito_liquidado   SMALLINT,
       v_credito_cancelado   SMALLINT

   -- se verifica si se esta solicitando eco
   IF ( UPSHIFT(ws_solicitud_in.nss) = "ECO" ) THEN
      -- se devuelve ECO
      LET ws_solicitud_out.nss = "ECO"
      
   ELSE
      -- se asignan los valores de respuesta
      LET ws_solicitud_out.nss = ws_solicitud_in.nss
      
      LET v_nss = ws_solicitud_in.nss
      LET ws_solicitud_out.credito_vigente   = 0
      LET ws_solicitud_out.credito_cancelado = 0
      LET ws_solicitud_out.credito_liquidado = 0
      LET ws_solicitud_out.sin_tramite       = 0
      LET ws_solicitud_out.cod_rechazo       = 0
      LET v_resultado                        = 0
      LET v_credito_vigente                  = 0
      LET v_credito_cancelado                = 0
      LET v_credito_liquidado                = 0
      LET v_sin_tramite                      = 0
      DISPLAY " *****************   ------------   *******************"
      DISPLAY " Inicio ... ", CURRENT 
      DISPLAY "Nss Recibido ", v_nss
      CALL fn_valida_nss(v_nss) RETURNING v_resultado

      IF v_resultado = 0 THEN 
         CALL fn_busca_resolucion(v_nss) RETURNING v_resultado
         IF v_resultado = 0 THEN 
            CALL fn_busca_info_credito(v_nss) 
                 RETURNING v_resultado, v_credito_vigente, v_credito_cancelado, v_credito_liquidado, v_sin_tramite
            DISPLAY "los valores regresados por la funcion fn_busca_info_credito son:"
            DISPLAY "v_resultado         >", v_resultado, "<"
            DISPLAY "v_credito_vigente   >", v_credito_vigente, "<"
            DISPLAY "v_credito_cancelado >", v_credito_cancelado, "<"
            DISPLAY "v_credito_liquidado >", v_credito_liquidado, "<"
            DISPLAY "v_sin_tramite       >", v_sin_tramite, "<"
            IF v_resultado = 0 THEN 
               LET ws_solicitud_out.credito_vigente = v_credito_vigente
               LET ws_solicitud_out.credito_cancelado = v_credito_cancelado
               LET ws_solicitud_out.credito_liquidado = v_credito_liquidado
               LET ws_solicitud_out.sin_tramite = v_sin_tramite
            ELSE 
               LET ws_solicitud_out.cod_rechazo = v_resultado
            END IF 
         ELSE 
            LET ws_solicitud_out.cod_rechazo = v_resultado
         END IF 
      ELSE 
         LET ws_solicitud_out.cod_rechazo = v_resultado
      END IF 
      DISPLAY "Los valores regresados por el WS "
      DISPLAY "NSS               >", ws_solicitud_out.nss, "<"      
      DISPLAY "Código Rechazo    >", ws_solicitud_out.cod_rechazo, "<"
      DISPLAY "Sin Trámite       >", ws_solicitud_out.sin_tramite, "<"
      DISPLAY "Crédito Vigente   >", ws_solicitud_out.credito_vigente, "<"
      DISPLAY "Crédito Liquidado >", ws_solicitud_out.credito_liquidado, "<"
      DISPLAY "Crédito Cancelado >", ws_solicitud_out.credito_cancelado, "<"
      DISPLAY "Fin ", CURRENT 
      DISPLAY " *****************   ------------   *******************"
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_valida_nss
Fecha creacion: Diciembre 29, 2016
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Verifica si el NSS es válido, asi como si existe en la base de derechohabientes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_valida_nss(p_nss)
DEFINE p_nss                  LIKE afi_derechohabiente.nss,
       v_diagnostico          SMALLINT,
       v_estatus              SMALLINT,
       v_id_derechohabiente   DECIMAL(9,0)   -- indica el tipo de consulta 30 - Consulta, 44 - Consulta y marca y 60 - Consulta y desmarca
       
          
   -- se obtiene el id_derechohabiente
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss               = p_nss   
   AND    ind_estado_cuenta = 0   -- cuenta Activa

   LET v_diagnostico = gi_nss_rfc_no_existe

   -- si se encontro el NSS
   IF ( v_id_derechohabiente IS NOT NULL ) THEN
      LET v_diagnostico = 0
   END IF 
   DISPLAY "Regresa de validar NSS >", v_diagnostico, "<"
   RETURN v_diagnostico
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_busca_resolucion
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de un credito por amortaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_resolucion(p_nss)
DEFINE p_nss                   CHAR(11), -- NSS
       v_sec_pension           SMALLINT, 
       v_regimen               SMALLINT, 
       v_tpo_pension           CHAR(2),  
       v_tpo_prestacion        CHAR(2), 
       v_porcentaje            DECIMAL(5,2),
       v_resolucion_encontrada SMALLINT,
       v_diagnostico           SMALLINT  

   LET v_diagnostico = gi_sin_resolucion_spess
   
   DECLARE cur_datamart CURSOR 
       FOR SELECT sec_pension, regimen, tpo_pension, tpo_prestacion, porcentaje_valuacion
           FROM  ret_datamart
           WHERE nss = p_nss
           ORDER BY sec_pension DESC
           
   FOREACH cur_datamart INTO v_sec_pension, v_regimen, v_tpo_pension, v_tpo_prestacion, v_porcentaje

      IF v_sec_pension IS NOT NULL THEN 
         IF v_regimen = 73 THEN 
            IF v_tpo_prestacion = "00" THEN 
               IF v_tpo_pension = 'IP' THEN 
                  IF v_porcentaje >= 50 THEN 
                     LET v_diagnostico = 0
                     EXIT FOREACH 
                  ELSE
                     LET v_diagnostico = gi_porcentaje_menor_50
                  END IF
               ELSE
                  IF v_tpo_pension = 'VI' OR
                     v_tpo_pension = 'OR' OR
                     v_tpo_pension = 'AS' OR 
                     v_tpo_pension = 'VO' THEN
                     LET v_diagnostico = gi_sin_resolucion_spess
                  ELSE 
                     LET v_diagnostico = 0
                     EXIT FOREACH
                  END IF 
               END IF 
            ELSE
               LET v_diagnostico = gi_sin_resolucion_spess
            END IF 
         ELSE
            LET v_diagnostico = gi_regimen_diferente_73
         END IF 
      ELSE
         LET v_diagnostico = gi_sin_resolucion_spess
      END IF 
   END FOREACH 

   DISPLAY "Regresa de validar la Resolucion >", v_diagnostico, "<"
   RETURN v_diagnostico 
         
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_busca_info_credito
Fecha creacion: Diciembre 29, 2016
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Busca la información de los créditos para un nss
 - Sin Trámite
 - Con Crédito Vigente
 - Con Crédito Liquidado
 - Con Crédito Cancelado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_info_credito(p_nss)
DEFINE p_nss                CHAR(11), -- NSS
       v_consulta           STRING,
       v_rch_cod            SMALLINT,
       v_rch_desc           CHAR(40),
       v_credito_vigente    SMALLINT,
       v_credito_cancelado  SMALLINT,
       v_credito_liquidado  SMALLINT, 
       v_sin_tramite        SMALLINT,
       v_id_derechohabiente DECIMAL(9,0),
       v_entidad            SMALLINT, 
       v_entidad_desc       CHAR(30)

       LET v_credito_vigente   = 0
       LET v_credito_cancelado = 0
       LET v_credito_liquidado = 0
       LET v_sin_tramite       = 0
       LET v_rch_cod           = 0

   DECLARE cur_credito CURSOR 
       FOR SELECT DISTINCT c.id_derechohabiente, m.entidad, e.entidad_desc
             FROM cre_acreditado c,
                  cat_tipo_credito d,
                  cat_maq_credito m,
                  cat_cre_entidad e,
                  afi_derechohabiente a
            WHERE c.id_derechohabiente = a.id_derechohabiente
              AND a.nss = p_nss
              AND c.tpo_originacion = d.tpo_originacion
              AND c.tpo_credito = d.tpo_credito
              AND c.estado = m.estado
              AND m.entidad = e.entidad
           
   FOREACH cur_credito INTO v_id_derechohabiente, v_entidad, v_entidad_desc
      DISPLAY " los datos regresados por el query dentro del fetch:"
      DISPLAY "v_id_derechohabiente >", v_id_derechohabiente, "<"
      DISPLAY "v_entidad            >", v_entidad, "<"
      DISPLAY "v_entidad_desc       >", v_entidad_desc, "<"
      IF v_id_derechohabiente IS NOT NULL THEN 
         CASE v_entidad
            WHEN 1 
               LET v_credito_vigente = 1
            WHEN 2
               LET v_credito_liquidado = 1
            WHEN 3
               LET v_sin_tramite = 1
            WHEN 5 
               LET v_credito_cancelado = 1
         END CASE
      ELSE 
         LET v_rch_cod = 1      
      END IF 
   END FOREACH 


   DISPLAY "Regresa de validar las banderas del credito >", v_rch_cod, "<"
   RETURN v_rch_cod,        
          v_credito_vigente,
          v_credito_cancelado,
          v_credito_liquidado, 
          v_sin_tramite

END FUNCTION

