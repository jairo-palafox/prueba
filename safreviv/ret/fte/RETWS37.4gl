--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
################################################################################
#PROYECTO         => SAFRE VIVIENDA                                            #
#PROPIETARIO      => E.F.P.                                                    #
#------------------------------------------------------------------------------#
#MODULO           => RET                                                       #
#PROGRAMA         => RETWS09                                                   #
#OBJETIVO         => WS GENERACION DE SOLICITUD DE RETIRO SOLO INFONAVIT DESDE #
#                 => VENTANILLA UNICA                                          #
#FECHA CREACION   => NOVIEMBRE-2020                                            #
################################################################################

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
DEFINE gr_crea_solicitud_in      RECORD
       nss                       CHAR(11), -- nss del trabajador
       caso_crm                  CHAR(10), -- numero de caso ADAI
       causal_retiro             SMALLINT, -- causal de retiro
       nrp                       CHAR(18), -- numero de registro patronal
       medio_entrega             SMALLINT, -- medio por el cual se hace la transaccion del retiro
     --sello                     CHAR(14), -- Sello generado por la consulta biometrica (aplica para medio_entrega = 1 tableta)
       arr_beneficiario          DYNAMIC ARRAY OF RECORD
            tipo_beneficiario    SMALLINT,
            clabe_bancaria       CHAR(18),
            rfc                  LIKE afi_derechohabiente.rfc,
            email                VARCHAR(50),
            telefono             VARCHAR(10),
            tel_movil            VARCHAR(10),
            nombre               LIKE afi_derechohabiente.nombre_af,
            ap_paterno           LIKE afi_derechohabiente.ap_paterno_af,
            ap_materno           LIKE afi_derechohabiente.ap_materno_af,
            entidad_federativa   CHAR(2)
            END RECORD
       END RECORD

-- registro de respuesta
DEFINE gr_crea_solicitud_out     RECORD
       nss                       CHAR(11),      -- Número de seguridad social del trabajador
       caso_crm                  CHAR(18),      -- Caso crm
       estado_solicitud          SMALLINT,      -- estado de la solicitud
       cod_rechazo               SMALLINT,      -- codigo de rechazo
       des_rechazo               CHAR(100),     -- descripcion del rechazo
       saldo_aivs                DECIMAL(22,6), -- saldo en AIVs de la subcuenta
       saldo_pesos               DECIMAL(22,2)  -- saldo en pesos de la subcuenta
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


-- CONSTANTES PROPIAS PARA EL PROCESO
CONSTANT gs_modalidad_ret SMALLINT = 1     --1=SOLO INFONAVIT
CONSTANT gs_marca_ret     SMALLINT = 801   --801=SOLO INFONAVIT
CONSTANT gs_subcuenta_si  SMALLINT = 44     --44=solo infonavit

END GLOBALS

#==============================================================================#
# MAIN                                                                         #
#==============================================================================#
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
  
    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
  
    -- se inicia el log del programa
    IF FGL_GETENV("DEBUGDUMMY") = "1" THEN
       DISPLAY "Ruta del log creada de forma local en C:/TMP/REWS37.log"
       CALL STARTLOG("C:\\TMP\\RETWS37.log")
    ELSE
       CALL STARTLOG(v_ruta_log)
    END IF

    LET v_pantalla = FALSE

    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
        LET serverURL = arg_val(2)
        CALL fn_crea_servicio_retiro_SI(TRUE)
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

    ------------------------------------------------
    --se ejecuta funcion para generacion de servicio
    ------------------------------------------------
    CALL fn_crea_servicio_retiro_SI(FALSE)

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
================================================================================
Nombre  : fn_crea_servicio_retiro_SI
Creacion: Noviembre 30, 2017
Modifico: Isai JImenez Rojas, Omnisys
Objetivo: Genera el servicio web de retiro generico solo infonavit
================================================================================
}
FUNCTION fn_crea_servicio_retiro_SI(p_generar_WSDL)

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
      LET v_webservice = com.WebService.CreateWebService("retiroCreaSolicitudSI", v_service_NameSpace)
      CALL v_webservice.setFeature("Soap1.1",TRUE)
    
      -- =============================
      -- Publicacion de las funciones
      
      -- fn_retiro 
      LET op = com.WebOperation.CreateDOCStyle("fn_ret_solicitud_SI","fn_ret_solicitud_SI",gr_crea_solicitud_in,gr_crea_solicitud_out)

      CALL v_webservice.publishOperation(op, "fn_ret_solicitud_SI")
    
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

#==============================================================================#
#                                                                              #
#==============================================================================#
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
================================================================================
Nombre  : fn_ret_solicitud_SI
Creacion: Noviembre 2020
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Funcion principal del WS que guarda solicitud solo infonavit
================================================================================
}
FUNCTION fn_ret_solicitud_SI()

    DEFINE 
           v_nss                 CHAR(11),
           v_rfc                 CHAR(13),
           v_indice_beneficiario SMALLINT, -- contador de beneficiarios
           v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
           v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
           v_id_derechohabiente  DECIMAL(10,0) -- Identificador único del trabajador
   
    -- se verifica si se esta solicitando eco
    IF ( UPSHIFT(gr_crea_solicitud_in.nss) = "ECO" ) THEN
       -- se devuelve ECO
       LET gr_crea_solicitud_out.nss = "ECO"
       
       -- se indica que hay un registro
       LET g_indice_retiro = 1
       CALL fn_respuesta_ws("ECO", "ECO", 0, 0, 0, 0, 0)
    ELSE
       -- se asignan los valores de respuesta
       LET gr_crea_solicitud_out.nss = gr_crea_solicitud_in.nss
       
       LET v_nss = gr_crea_solicitud_in.nss
       
       -- se inicia el indice del retiro que se va a consultar
       LET g_indice_retiro = 1
       
       -- se crea el registro de la peticion de solicitud de ws registro de solicitud
       CALL fn_registra_peticion_crea_solicitud(gr_crea_solicitud_in.nss,
                                                gr_crea_solicitud_in.caso_crm)
            RETURNING g_id_peticion
       
       -- se asume que todos traen cuenta CLABE
       LET v_cta_clabe_correcta = TRUE
    
       --IJR --si se tiene mas de una modalidad de retiro, todas deben tener cuenta CLABE
       --IJR IF gr_crea_solicitud_in.grupo <> 1 AND gr_crea_solicitud_in.medio_entrega <> 1 THEN
       --IJR    FOR v_indice_beneficiario = 1 TO gr_crea_solicitud_in.arr_beneficiario.getLength()
       --IJR       IF ( gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
       --IJR          -- se rechaza porque todos deben traer CLABE, incluyendo Fondo de Ahorro
       --IJR          LET v_cta_clabe_correcta = FALSE
       --IJR          EXIT FOR
       --IJR       END IF
       --IJR    END FOR
       --IJR END IF 
       --IJR
       --IJR -- si no fue correcto, se rechaza la solicitud para todas las modalidades
       --IJR IF ( NOT v_cta_clabe_correcta ) THEN
       --IJR    CALL fn_respuesta_ws(v_nss, NULL, gs_modalidad_ret,gi_solicitud_rechazada,
       --IJR                         gi_modalidad_multiple_sin_CLABE, 0, 0)
       --IJR END IF
    END IF
    
    -- si paso la validacion de existencia de cuenta CLABE en modalidad multiple
    IF ( v_cta_clabe_correcta = TRUE ) THEN
      
          -- se registran los detalles de modalidad de retiro
       CALL fn_registra_det_peticion_crea_solicitud(g_id_peticion,
                                                    gs_modalidad_ret,
                                                    NULL            ,
                                                    NULL            ,
                                                    NULL            ,
                                                    NULL )

       -- se asume que el registro de beneficiario es correcto
       LET v_existe_beneficiario = TRUE
    
       IF --gr_crea_solicitud_in.grupo = 1 AND
          (gr_crea_solicitud_in.medio_entrega = 1 OR 
           gr_crea_solicitud_in.medio_entrega = 2) THEN

          --- Obtenemos los datos del beneficiario para grupo 1 y medio entrega 1 (tableta)
          LET v_indice_beneficiario = 1 
          LET v_id_derechohabiente  = 0

          LET gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario = 1 --- Titular

          ----------------------------------------------------------------------
          SELECT DISTINCT c.id_derechohabiente, a.cuenta_clabe,
                 NVL(d.rfc,"SIN RFC"), NVL(d.nombre_af,"SIN NOMBRE"),
                 NVL(d.ap_paterno_af,"SIN PATERNO"), NVL(d.ap_materno_af,"SIN MATERNO")
          INTO   v_id_derechohabiente,
                 gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria,
                 gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc           ,
                 gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre        ,
                 gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno    ,
                 gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_materno    
          FROM   ret_pago_spei             a,
                 ret_solicitud_generico    c,
                 afi_derechohabiente       d
          WHERE  c.nss                 = v_nss
          AND    d.id_derechohabiente  = c.id_derechohabiente
          AND    c.modalidad_retiro    = gs_modalidad_ret    --1=solo infonavit, 3=ley73
          AND    c.estado_solicitud    = 8
          AND    a.id_solicitud        = c.id_solicitud
          AND    a.consec_beneficiario = 1
    
          ----------------------------------------------------------------------
          SELECT TRIM(NVL(telefono,"0000000000"))
          INTO   gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono
          FROM   afi_telefono
          WHERE  id_derechohabiente = v_id_derechohabiente
          AND    id_telefono        = 1
          
          IF gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono IS NULL THEN 
             LET gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = "0000000000"
          END IF 
    
          ----------------------------------------------------------------------
          SELECT NVL(valor,"SIN CORREO")
          INTO   gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].email
          FROM   afi_contacto_electronico
          WHERE  id_derechohabiente      = v_id_derechohabiente
          AND    id_contacto_electronico = 1

          IF gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].email IS NULL THEN 
             LET gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = "SIN CORREO"
          END IF 
    
          ----------------------------------------------------------------------
          SELECT NVL(b.entidad_federativa,"9")
          INTO   gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa
          FROM   afi_domicilio a,
                 cat_cp        b
          WHERE  a.id_derechohabiente = v_id_derechohabiente
          AND    a.id_domicilio       = 1
          AND    a.cp                 = b.cp

          IF gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa IS NULL THEN 
             LET gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa = "9"
          END IF 
    
       END IF
       
       FOR v_indice_beneficiario = 1 TO gr_crea_solicitud_in.arr_beneficiario.getLength()
           -- se registra en la bitacora los datos de beneficiarios recibidos
           CALL fn_registra_peticion_crea_solicitud_benef(g_id_peticion                                                                   ,
                                                          gs_modalidad_ret                                                               ,
                                                          v_indice_beneficiario                                                          ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria    ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc               ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].email             ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono          ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].tel_movil         ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre            ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno        ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_materno        ,
                                                          gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa)
           
           -- debe traer todos los datos
           IF (gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipo_beneficiario   IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc                 IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].email               IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono            IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre              IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].ap_paterno          IS NULL OR
               gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidad_federativa  IS NULL ) THEN
           
              -- los datos de un beneficiario estan mal, no procede la solicitud
              LET v_existe_beneficiario = FALSE
              EXIT FOR
           ELSE
              -- si le falta la CLABE
              --IJR IF ( gr_crea_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabe_bancaria IS NULL ) THEN
              --IJR    -- se rechaza
              --IJR    LET v_existe_beneficiario = FALSE
              --IJR    EXIT FOR
              --IJR END IF
           END IF
           
           IF --gr_crea_solicitud_in.grupo         = 1 AND 
              (gr_crea_solicitud_in.medio_entrega = 1 OR 
               gr_crea_solicitud_in.medio_entrega = 2) AND 
               v_indice_beneficiario = 1 THEN
              EXIT FOR
           END IF

       END FOR

       -- se verifica que la modalidad traiga beneficiarios
       IF ( NOT v_existe_beneficiario OR NOT v_cta_clabe_correcta ) THEN
         
          -- se verifica si es por falta de datos de un beneficiario
          IF ( NOT v_existe_beneficiario ) THEN
             
             --registra datos de retorno por falta de beneficiarios
             CALL fn_respuesta_ws(v_nss, 
                                  v_rfc, 
                                  gs_modalidad_ret,
                                  gi_solicitud_rechazada, 
                                  gi_solicitud_sin_beneficiarios, 
                                  0, 
                                  0)
          ELSE
             -- se rechaza por la estructura de la cuenta clabe
             CALL fn_respuesta_ws(v_nss, 
                                  v_rfc, 
                                  gs_modalidad_ret,
                                  gi_solicitud_rechazada, 
                                  gi_esctructura_cta_clabe_incorrecta, 
                                  0, 
                                  0)
          END IF
       ELSE
          -- se verifica la modalidad
          CALL fn_ret_disponibilidad_SI(v_nss, v_rfc)

       END IF
    END IF

END FUNCTION

{
================================================================================
Nombre  : fn_ret_disponibilidad_SI
Creacion: Noviembre 2020
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Verifica si un derechohabiente puede realizar el retiro de Solo infonavit
================================================================================
}
FUNCTION fn_ret_disponibilidad_SI(p_nss, p_rfc)

    DEFINE p_nss                  LIKE afi_derechohabiente.nss, -- NSS
           p_rfc                  LIKE afi_derechohabiente.rfc  -- RFC del trabajador

    DEFINE 
           v_aivs_viv97           DECIMAL(24,6),                -- saldo AIVs de viv97
           v_pesos_viv97          DECIMAL(22,2),                -- saldo pesos de viv97
         --v_aivs_viv92_afore     DECIMAL(24,6),                -- saldo AIVs de viv92 en AFORE
         --v_aivs_viv97_afore     DECIMAL(24,6),                -- saldo AIVs de viv97 en AFORE
         --v_pesos_viv92_afore    DECIMAL(22,2),                -- saldo pesos de viv92 en AFORE
         --v_pesos_viv97_afore    DECIMAL(22,2),                -- saldo pesos de viv97 en AFORE
           v_fecha_resolucion     DATE,                         --fehca de resolucion de pension en el SPESS
           v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
           v_id_solicitud         LIKE ret_solicitud_generico.id_solicitud,     -- id de la solicitud
           v_saldo_aivs           DECIMAL(24,6),
           v_monto_pesos          DECIMAL(22,2),
           v_f_valuacion          DATE

    ----------------------------------------------------------------------------

    -- se obtiene el id de solicitud creada en el marcado buscando estado en 8 precapturada
    -- CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 3, 8) RETURNING v_id_solicitud
    -- Se elimina este llamado para hacer la busqueda sin el RFC   RPR 140616 *****
   
    SELECT id_solicitud
    INTO   v_id_solicitud 
    FROM   ret_solicitud_generico
    WHERE  nss              = p_nss
    AND    modalidad_retiro = gs_modalidad_ret
    AND    estado_solicitud = 8
   
    -- si la cuenta aparece marcada y lista para ser generada una solicitud
    IF ( v_id_solicitud IS NOT NULL ) THEN
      
       -- se obtiene el id_derechohabiente
       SELECT id_derechohabiente
       INTO   v_id_derechohabiente
       FROM   afi_derechohabiente
       WHERE  nss               = p_nss   
       AND    ind_estado_cuenta = 0   -- cuenta Activa
       
       -- si se encontro el NSS
       IF ( v_id_derechohabiente IS NOT NULL ) THEN

          -- se obtiene el saldo de subuenta asociada con solo infonavit
          CALL fn_recupera_saldo_solo_infonavit(p_nss) RETURNING v_saldo_aivs, v_monto_pesos, v_f_valuacion

          -------------------------------------------------------------------
          -- Se recupera el saldo que se guardo en la consulta de la marca
          --IJR CALL fn_recupera_saldo_consulta(p_nss) 
          --IJR      RETURNING v_aivs_viv92_afore, v_pesos_viv92_afore, v_aivs_viv97_afore, v_pesos_viv97_afore

          -- si el saldo de afore es mayor que el de infonavit, se registra que la solicitud tendra un sobregiro
          --IJR IF ( (v_aivs_viv92_afore > v_aivs_viv92) OR 
          --IJR    (v_aivs_viv97_afore > (v_aivs_viv97+ v_aivs_vol)) ) THEN
                
             -- Registra el saldo de vivienda97 Solo infonavit
             INSERT INTO ret_his_saldo (id_solicitud, subcuenta,fondo_inversion,
                                        saldo_acciones,saldo_pesos,folio,
                                        f_registro,h_registro     
                                       )
                         VALUES (v_id_solicitud        ,   -- id_solicitud   
                                 gs_subcuenta_si       ,   -- subcuenta      
                                 11                    ,   -- fondo_inversion
                                 v_saldo_aivs          ,   -- saldo_acciones 
                                 v_monto_pesos         ,   -- saldo_pesos    
                                 0                     ,   -- folio          
                                 TODAY                 ,   -- f_registro     
                                 CURRENT HOUR TO SECOND    -- h_registro     
                                )
          --IJR END IF
          CALL fn_ret_valida(p_nss, v_id_derechohabiente, v_id_solicitud, v_aivs_viv97, v_pesos_viv97, v_fecha_resolucion)
       ELSE
          -- no se encuentra el NSS en la base de datos de vivienda
          CALL fn_respuesta_ws_SI(gi_solicitud_rechazada, gi_nss_rfc_no_existe, gs_subcuenta_si, 0, TODAY, 0)
       END IF
   ELSE
      -- no se tiene una solicitud de retiro ley 73 precapturada con el nss dado
      CALL fn_respuesta_ws_SI(gi_solicitud_rechazada, gi_no_existe_solicitud, gs_subcuenta_si, 0, TODAY, 0)
   END IF

END FUNCTION

{
================================================================================
Nombre  : fn_ret_valida
Creacion: Marzo 22, 2013
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Verifica si un derechohabiente puede realizar el retiro de su saldo 
          de cuenta de un credito por amortaciones excedentes
================================================================================
}
FUNCTION fn_ret_valida(p_nss               ,
                       p_id_derechohabiente,
                       p_id_solicitud      ,
                       v_aivs_viv97        ,
                       v_pesos_viv97       ,
                       v_fecha_resolucion
                       )
    
    DEFINE p_nss                CHAR(11),      -- NSS
           p_id_derechohabiente DECIMAL(9),    --
           p_id_solicitud       DECIMAL(9),    -- solicitud de retiro
           v_aivs_viv97         DECIMAL(24,6), -- saldo AIVs de viv97
           v_pesos_viv97        DECIMAL(22,2), -- saldo pesos de viv97
           v_tiene_credito      SMALLINT,      -- booleana que indica si se tiene un credito vigente
           v_fecha_resolucion   DATE,          -- fecha de resolucion en el SPESS
           v_saldo_total        DECIMAL(24,6)  -- saldo total (viv92 + viv97)

    -- se calcula saldo total
    LET v_saldo_total = v_aivs_viv97

    -- se verifica si tuvo un retiro de devolucion
    IF ( fn_nss_tuvo_retiro(p_nss) ) THEN
       -- se rechaza por insuficiencia de saldo
       CALL fn_respuesta_ws_SI(gi_solicitud_rechazada, 
                               gi_sin_saldo, 
                               gs_subcuenta_si, 
                               0, 
                               TODAY,
                               0)

       -- se rechaza la solicitud precapturada
       CALL fn_genera_solicitud_ret_SI(p_nss,                       --01
                                       p_id_derechohabiente,        --02
                                       gi_solicitud_rechazada,      --03
                                       gi_sin_saldo,                --04
                                       0,                           --05
                                       0,                           --06
                                       p_id_solicitud)              --07
    ELSE
       LET v_tiene_credito = 0
       
       SELECT COUNT(*)
       INTO   v_tiene_credito
       FROM   sfr_marca_activa a, afi_derechohabiente b
       WHERE  a.id_derechohabiente = b.id_derechohabiente
       AND    b.nss                = p_nss
       AND    a.marca IN (202,212,218,220,222,223,224,226,227,232,233)
       
       IF v_tiene_credito > 0 THEN
         
          CALL fn_respuesta_ws_SI(gi_solicitud_rechazada, 
                                  gi_tiene_credito_vigente, 
                                  gs_subcuenta_si, 
                                  0, 
                                  TODAY,
                                  0)
          
          LET v_aivs_viv97  = 0
          LET v_pesos_viv97 = 0
          
          --
          CALL fn_respuesta_ws_SI(gi_solicitud_aceptada, 0, gs_subcuenta_si, v_aivs_viv97, TODAY,0)
          ---------------------
          -- GENERA SOLICITUD
          ---------------------
          CALL fn_genera_solicitud_ret_SI(p_nss,                   --01
                                          p_id_derechohabiente,    --02
                                          gi_solicitud_aceptada,   --03
                                          0,                       --04
                                          v_aivs_viv97,            --05
                                          v_pesos_viv97,           --06
                                          p_id_solicitud)          --07

       ELSE
          -- el saldo es retirable
          CALL fn_respuesta_ws_SI(gi_solicitud_aceptada, 
                                  0              ,
                                  gs_subcuenta_si,
                                  v_aivs_viv97   ,
                                  TODAY          ,
                                  0)

          -- 
          CALL fn_genera_solicitud_ret_SI(p_nss,                   --01
                                          p_id_derechohabiente,    --02
                                          gi_solicitud_aceptada,   --03
                                          0,                       --04
                                          v_aivs_viv97,            --05
                                          v_pesos_viv97,           --06
                                          p_id_solicitud)          --07

       END IF

    END IF

END FUNCTION

{
================================================================================
Nombre  : fn_respuesta_ws_SI
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Construye la respuesta de la validacion de disponibilidad del retiro 
          solo infonavit
================================================================================
}
FUNCTION fn_respuesta_ws_SI(p_estado_solicitud,
                            p_cod_rechazo     ,
                            p_subcuenta       ,
                            p_importe_aivs    ,
                            p_fecha_valuacion ,
                            p_monto_transferido)

    DEFINE   p_estado_solicitud  SMALLINT,      -- Resp. de la solicidut, aceptada-rechazada
             p_cod_rechazo       SMALLINT,      -- Codigo de rechazo 
             p_subcuenta         SMALLINT,      -- subcuenta de inversion
             p_importe_aivs      DECIMAL(24,6), -- monto en AIVs
             p_fecha_valuacion   DATE,          -- fecha de valuacion
             v_valor_fondo       LIKE glo_valor_fondo.precio_fondo,
             p_monto_transferido DECIMAL(22,2)  -- monto transferido
         
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
    LET gr_crea_solicitud_out.estado_solicitud = p_estado_solicitud
    LET gr_crea_solicitud_out.cod_rechazo      = p_cod_rechazo
    LET gr_crea_solicitud_out.saldo_aivs       = p_importe_aivs
    

    LET gr_crea_solicitud_out.saldo_pesos      = p_importe_aivs * v_valor_fondo + p_monto_transferido
    
    LET gr_crea_solicitud_out.des_rechazo = " "
    
    IF p_cod_rechazo <> 0 THEN
       -- Busca la descripcion del error para regresarla en la consulta
       LET gr_crea_solicitud_out.des_rechazo = " "

       SELECT des_larga
       INTO   gr_crea_solicitud_out.des_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       
       IF gr_crea_solicitud_out.des_rechazo IS NULL THEN
           LET gr_crea_solicitud_out.des_rechazo = " "
       END IF

    END IF
    
    -- se registra la respuesta de la peticion
    CALL fn_registra_peticion_crea_solicitud_resp(g_id_peticion     , 
                                                          gs_modalidad_ret  ,
                                                          p_subcuenta       ,
                                                          p_estado_solicitud,
                                                          p_cod_rechazo     ,
                                                          p_importe_aivs    ,
                                                          p_importe_aivs * v_valor_fondo)
END FUNCTION

{
================================================================================
Nombre  : fn_respuesta_ws
Creacion: Noviembre 2020
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Construye la respuesta para contestar la peticion del webservice
================================================================================
}
FUNCTION fn_respuesta_ws(p_nss             ,   --1
                         p_rfc             ,   --2
                         p_modalidad       ,   --3
                         p_estado_solicitud,   --4
                         p_cod_rechazo     ,   --5
                         p_aivs            ,   --6
                         p_pesos           )   --7
    
    DEFINE p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
           p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
           p_modalidad        SMALLINT,                     -- modalidad de retiro
           p_estado_solicitud SMALLINT,                     -- estado de la solicitud
           p_cod_rechazo      SMALLINT,                     -- Codigo de rechazo 
           p_aivs             DECIMAL(24,6),                -- importe en aivs
           p_pesos            DECIMAL(22,2)                 -- importe en pesos

   -- se escribe la respuesta de la solicitud generica
   LET gr_crea_solicitud_out.estado_solicitud = p_estado_solicitud
   LET gr_crea_solicitud_out.cod_rechazo      = p_cod_rechazo
   LET gr_crea_solicitud_out.saldo_aivs       = p_aivs
   LET gr_crea_solicitud_out.saldo_pesos      = p_pesos
   LET gr_crea_solicitud_out.des_rechazo      = " "


   IF p_cod_rechazo <> 0 THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET gr_crea_solicitud_out.des_rechazo = "";
      
      SELECT des_larga
      INTO   gr_crea_solicitud_out.des_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      
      IF gr_crea_solicitud_out.des_rechazo IS NULL THEN
         LET gr_crea_solicitud_out.des_rechazo = " "; 
      END IF
   END IF

   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_peticion_crea_solicitud_resp(g_id_peticion     ,
                                                         p_modalidad       ,
                                                         0                 ,
                                                         p_estado_solicitud,
                                                         p_cod_rechazo     ,
                                                         p_aivs            ,
                                                         p_pesos           )

END FUNCTION

{
================================================================================
Nombre  : fn_genera_solicitud_ret_SI
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Genera una solicitud de retiro Solo Infonacit para un NSS proporcionado
================================================================================
}
FUNCTION fn_genera_solicitud_ret_SI(p_nss               ,           --01
                                    p_id_derechohabiente,           --02
                                    p_estado_solicitud  ,           --03
                                    p_rechazo           ,           --04
                                    p_aivs_viv97        ,           --05
                                    p_pesos_viv97       ,           --06
                                    p_id_solicitud                  --07
                                    )

    DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
           p_nss                  LIKE afi_derechohabiente.nss, 
           p_rfc                  LIKE afi_derechohabiente.rfc,
           p_estado_solicitud     SMALLINT      , -- estatus de la solicitud
           p_rechazo              SMALLINT      , -- 
           p_aivs_viv92           DECIMAL(24,6),
           p_pesos_viv92          DECIMAL(22,2),
           p_aivs_viv97           DECIMAL(24,6),
           p_pesos_viv97          DECIMAL(22,2),
           p_id_solicitud         LIKE ret_fondo_ahorro.id_solicitud  -- num de solicitud
    
    DEFINE lr_ret_solo_infonavit  RECORD LIKE ret_solo_infonavit.*    -- Solo Infonavit

    DEFINE v_conteo               SMALLINT,
           v_tipo_pago            SMALLINT,
           v_total_aivs           DECIMAL(24,6), -- total de AIVs
           v_total_pesos          DECIMAL(22,2), -- total de pesos
           v_subgrupo             smallint,
           p_importe_viv97_anexo1 DECIMAL(12,2),
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
    LET v_cadena    = ""
    LET v_algoritmo = ""
    LET v_sha       = ""
    LET v_rfc       = ""

    -- si la solicitud fue aceptada
    IF ( p_estado_solicitud = gi_solicitud_aceptada ) THEN

       IF (gr_crea_solicitud_in.medio_entrega = 1  OR
           gr_crea_solicitud_in.medio_entrega = 2) THEN --AND
         --gr_crea_solicitud_in.grupo = 1 THEN

          LET p_estado_solicitud = 15 --- Estas solicitudes se dan de alta con estado "Autorizada"
          
          -- Las solicitudes de tableta se dejan con estado solicitud = 10 ya que se espera la autorización para confirmar la conclusión del trámite
          IF gr_crea_solicitud_in.medio_entrega = 1 THEN --AND gr_crea_solicitud_in.grupo = 1 THEN 
             LET p_estado_solicitud = 10
          END IF

          -- Se debe eliminar el registro de la tabla ret_pago_spei ya que la rutina de beneficiarios inserta los datos
          DELETE 
          FROM   ret_pago_spei 
          WHERE  id_solicitud = p_id_solicitud
          
           --- se obtiene el sello para guardarlo en tablas
          SELECT curp, nombre_af, ap_paterno_af, ap_materno_af, rfc
          INTO   v_curp       ,
                 v_nombre     ,
                 v_ape_paterno,
                 v_ape_materno,
                 v_rfc 
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
          
          LET v_cadena = p_id_solicitud USING "<<<<<<<<<<", "|", gr_crea_solicitud_in.nss, "|",
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
    
       END IF
       
       -- Se assignan los datos a ser almacenados en la solicitud
       LET lr_ret_solo_infonavit.id_solicitud         = p_id_solicitud
       LET lr_ret_solo_infonavit.id_derechohabiente   = p_id_derechohabiente
       LET lr_ret_solo_infonavit.nss                  = p_nss
       LET lr_ret_solo_infonavit.f_solicitud          = TODAY
       LET lr_ret_solo_infonavit.estado_solicitud     = p_estado_solicitud
       LET lr_ret_solo_infonavit.folio                = ""
       LET lr_ret_solo_infonavit.aivs_viv97           = p_aivs_viv97
       LET lr_ret_solo_infonavit.importe_viv97        = p_pesos_viv97
       LET lr_ret_solo_infonavit.clabe                = ""
       LET lr_ret_solo_infonavit.banco                = ""
       LET lr_ret_solo_infonavit.entidad_federativa   = ""
       LET lr_ret_solo_infonavit.causal_retiro        = gr_crea_solicitud_in.causal_retiro
       LET lr_ret_solo_infonavit.f_valuacion          = TODAY
       LET lr_ret_solo_infonavit.f_captura            = TODAY
       LET lr_ret_solo_infonavit.h_captura            = CURRENT HOUR TO SECOND
       LET lr_ret_solo_infonavit.usuario              = CURRENT HOUR TO SECOND

       SELECT usuario_marca
       INTO   lr_ret_solo_infonavit.usuario
       FROM   sfr_marca_activa
       WHERE  id_derechohabiente = p_id_derechohabiente
       AND    marca        = gs_marca_ret
       AND    n_referencia = p_id_solicitud

       LET lr_ret_solo_infonavit.cod_rechazo          = p_rechazo
    
       -- se inserta el registro de solicitud en la tabla historica
       INSERT INTO ret_solo_infonavit VALUES ( lr_ret_solo_infonavit.* )
    
       -- se actualiza el estado de la tabla de control
       UPDATE ret_solicitud_generico
       SET    estado_solicitud = p_estado_solicitud,
              rfc              = v_rfc
       WHERE  id_solicitud     = p_id_solicitud
       
       -- se calculan los montos totales
       LET v_total_aivs  = p_aivs_viv97
       LET v_total_pesos = p_pesos_viv97
       
       -- se crean los registros para los beneficiarios de esta solicitud
       FOR v_conteo = 1 TO gr_crea_solicitud_in.arr_beneficiario.getLength()
    
          LET v_tipo_pago = 1 --SPEI

          LET v_total_pesos = v_total_pesos
          
          CALL fn_registra_beneficiario_retiro_generico(p_id_solicitud,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].tipo_beneficiario,
                                                        v_tipo_pago, -- FALTA TIPO DE PAGO
                                                        1,           -- FALTA PARENTESCO
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].ap_paterno,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].ap_materno,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].nombre,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].telefono,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].email,
                                                        100,
                                                        v_total_aivs,
                                                        v_total_pesos,
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].clabe_bancaria,
                                                        "",
                                                        gr_crea_solicitud_in.arr_beneficiario[v_conteo].entidad_federativa)
       END FOR
    ELSE
       -- se rechaza la solicitud
       UPDATE ret_solicitud_generico
       SET    estado_solicitud = p_estado_solicitud,
              cod_rechazo      = p_rechazo --,
       WHERE  id_solicitud     = p_id_solicitud
    
       -- se desmarca la cuenta
       CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, gs_marca_ret, p_id_solicitud,
                                            gs_marca_ret, "safreviv", g_proceso_cod_ret_ley73_ws) -- ley 73
    
    END IF

END FUNCTION 

{
================================================================================
Nombre  : fn_registra_peticion_crea_solicitud
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Registra los datos de entrada que se recibieron de una peticion de WS
          para registro de solicitud de retiro
================================================================================
}
FUNCTION fn_registra_peticion_crea_solicitud(p_nss, p_caso_crm)

    DEFINE p_nss                   LIKE afi_derechohabiente.nss,
           p_caso_crm              LIKE ret_solicitud_generico.caso_adai

    DEFINE v_id_peticion           DECIMAL(9,0)
    DEFINE lr_peticion             RECORD LIKE ret_ws_peticion_crea_solicitud.*
  
    -- se obtiene el id de peticion nuevo
    SELECT seq_ret_ws_generico.nextVal
    INTO   v_id_peticion
    FROM   systables
    WHERE  tabid = 1
   
    -- se asignan los datos
    LET lr_peticion.id_peticion   = v_id_peticion
    LET lr_peticion.f_peticion    = TODAY
    LET lr_peticion.h_peticion    = CURRENT HOUR TO SECOND
    LET lr_peticion.nss           = p_nss
    LET lr_peticion.rfc           = NULL
    LET lr_peticion.caso_adai     = p_caso_crm
   
    -- se inserta el registro de peticion
    INSERT INTO ret_ws_peticion_crea_solicitud VALUES ( lr_peticion.* )

    RETURN v_id_peticion

END FUNCTION

{
================================================================================
Nombre  : fn_registra_det_peticion_crea_solicitud
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Registra el detalle de los datos enviados como encabezado de modalidad de retiro
          en el servicio de registro de solicitud de retiro
================================================================================
}
FUNCTION fn_registra_det_peticion_crea_solicitud(p_id_peticion     ,
                                                 p_modalidad_retiro,
                                                 p_causal_retiro   ,
                                                 p_nrp             ,
                                                 p_f_inicio_pension,
                                                 p_num_credito)

    DEFINE p_id_peticion                          DECIMAL(9,0), -- id de la peticion
           p_modalidad_retiro                     SMALLINT,     -- modalidad de retiro
           p_causal_retiro                        SMALLINT,     -- causal de retiro en fondo72
           p_nrp                                  CHAR(18),     -- NRP del trabajador
           p_f_inicio_pension                     CHAR(8),      -- fecha de inicio de pension
           p_num_credito                          CHAR(20)      -- numero de credito para Amort Excedentes
    
    -- registro de detalle de peticion al ws
    DEFINE lr_peticion RECORD LIKE ret_ws_det_peticion_crea_solicitud.*

    -- se asignan los datos
    LET lr_peticion.id_peticion      = p_id_peticion
    LET lr_peticion.modalidad_retiro = p_modalidad_retiro
    LET lr_peticion.causal_retiro    = p_causal_retiro
    LET lr_peticion.nrp              = p_nrp
    LET lr_peticion.f_inicio_pension = p_f_inicio_pension
    LET lr_peticion.num_credito      = p_num_credito

    -- se inserta el registro de detalle de peticion
    INSERT INTO ret_ws_det_peticion_crea_solicitud VALUES ( lr_peticion.* )

END FUNCTION

{
================================================================================
Nombre  : fn_registra_peticion_crea_solicitud_resp
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Registra la respuesta enviada al cliente de una modalidad de retiro
          recibida como solicitud de registro de solicitud de retiro generico
================================================================================
}
FUNCTION fn_registra_peticion_crea_solicitud_resp(p_id_peticion, 
                                                  p_modalidad_retiro, 
                                                  p_subcuenta,
                                                  p_estado_solicitud, 
                                                  p_cod_rechazo, 
                                                  p_monto_aivs,
                                                  p_monto_pesos)

    DEFINE p_id_peticion       DECIMAL(9,0),  -- id de la peticion
           p_modalidad_retiro  SMALLINT,      -- modalidad de retiro
           p_subcuenta         SMALLINT,      -- subcuenta de inversion
           p_estado_solicitud  SMALLINT,      -- estado de la solicitud
           p_cod_rechazo       SMALLINT,      -- codigo de rechazo
           p_monto_aivs        DECIMAL(22,6), -- saldo en AIVs
           p_monto_pesos       DECIMAL(22,2)  -- saldo en pesos
           
    -- registro de respuesta detalle de peticion al ws
    DEFINE lr_respuesta        RECORD LIKE ret_ws_det_peticion_crea_solicitud_resp.* 


    LET lr_respuesta.id_peticion            = p_id_peticion
    LET lr_respuesta.modalidad_retiro       = p_modalidad_retiro
    LET lr_respuesta.resp_subcuenta         = p_subcuenta
    LET lr_respuesta.resp_estado_solicitud  = p_estado_solicitud
    LET lr_respuesta.resp_cod_rechazo       = p_cod_rechazo
    LET lr_respuesta.resp_monto_avis        = p_monto_aivs
    LET lr_respuesta.resp_monto_pesos       = p_monto_pesos

    -- se inserta el registro de detalle de peticion
    INSERT INTO ret_ws_det_peticion_crea_solicitud_resp VALUES ( lr_respuesta.* )

END FUNCTION



{
================================================================================
Nombre  : fn_registra_peticion_crea_solicitud_benef
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Registra los datos de un beneficiario dado de alta en una solicitud de
          retiro generico por modalidad de retiro
================================================================================
}
FUNCTION fn_registra_peticion_crea_solicitud_benef(p_id_peticion      ,
                                                   p_modalidad_retiro ,
                                                   p_consec_benef     ,
                                                   p_tipo_beneficiario,
                                                   p_clabe_bancaria   ,
                                                   p_rfc              ,
                                                   p_email            ,
                                                   p_telefono         ,
                                                   p_tel_movil        ,
                                                   p_nombre           ,
                                                   p_ap_paterno       ,
                                                   p_ap_materno       ,
                                                   p_entidad_federativa)

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
           p_entidad_federativa CHAR(2)   -- entidad federativa
    
    -- registro de respuesta detalle de peticion al ws
    DEFINE lr_peticion RECORD LIKE ret_ws_peticion_crea_sol_benef.* 

    -- se asignan los datos
    LET lr_peticion.id_peticion        = p_id_peticion
    LET lr_peticion.modalidad_retiro   = p_modalidad_retiro  
    LET lr_peticion.consec_benef       = p_consec_benef      
    LET lr_peticion.tipo_beneficiario  = p_tipo_beneficiario 
    LET lr_peticion.clabe_bancaria     = p_clabe_bancaria    
    LET lr_peticion.rfc                = p_rfc               
    LET lr_peticion.email              = p_email             
    LET lr_peticion.telefono           = p_telefono          
    LET lr_peticion.tel_movil          = p_tel_movil         
    LET lr_peticion.nombre             = p_nombre            
    LET lr_peticion.ap_paterno         = p_ap_paterno        
    LET lr_peticion.ap_materno         = p_ap_materno        
    LET lr_peticion.entidad_federativa = p_entidad_federativa

    -- se inserta el registro de detalle de beneficiarios
    INSERT INTO ret_ws_peticion_crea_sol_benef VALUES ( lr_peticion.* )
   
END FUNCTION

{
================================================================================
Nombre  : fn_hash
Creacion: Noviembre 2020             
Modifico: Isai Jimenez Rojas, Omnisys
Objetivo: Genera un código HASH (pasado por parámetro) de una cadena de texto 
          STRING (pasada por parámetro)

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5
================================================================================
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

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_recupera_saldo_solo_infonavit(p_nss)

    DEFINE p_nss                LIKE afi_derechohabiente.nss,
           v_subcuenta          SMALLINT,
           v_f_valuacion        DATE, -- fecha de valuacion
           v_saldo_aivs         DECIMAL(24,6),
           v_saldo_pesos        DECIMAL(22,2), 
           v_resultado_consulta SMALLINT,
           v_sql                STRING

    -- se ejecuta el SP de consulta de saldo
    LET v_sql         = "EXECUTE FUNCTION fn_saldo_dia(?,NULL,?,?)"
    LET v_subcuenta   = gs_subcuenta_si
    LET v_f_valuacion = TODAY
   
    -- se ejecuta la consulta de saldo
    PREPARE sid_saldo FROM v_sql
    EXECUTE sid_saldo USING p_nss, v_subcuenta, v_f_valuacion
                     INTO v_resultado_consulta, v_saldo_aivs, v_saldo_pesos

   -- se devuelve el resultado de la consulta
   RETURN v_saldo_aivs, v_saldo_pesos, v_f_valuacion

END FUNCTION