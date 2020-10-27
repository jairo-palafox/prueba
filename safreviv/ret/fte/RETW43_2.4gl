--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW43                                                  #
#OBJETIVO          => WS ACTUALIZACIÓN CUENTA CLABE                           #
#                     VENTANILLA AFORE                                        #
#FECHA INICIO      => 24-SEP-2015                                             #
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
         aiv_92            DECIMAL(13,2),
         aiv_97            DECIMAL(13,2),
         beneficiario      SMALLINT     ,
         clabe             CHAR (18)    ,
         clave1            CHAR (18)    ,
         clave2            CHAR (18)    ,
         comentarios       CHAR (127)   ,
         contabilizado     CHAR (10)    , 
         curpB             CHAR (18)    ,
         curpT             CHAR (18)    ,
         cve_afore         SMALLINT     ,
         entidad           SMALLINT     ,
         fechaPago         CHAR(8)      ,
         fecha_viv         CHAR(8)      ,
         folioNoti         CHAR(29)     ,
         folioOper         CHAR(10)     ,
         grupo             CHAR(4)      ,
         maternoB          CHAR(40)     ,
         maternoT          CHAR(40)     ,
         neto1             DECIMAL(13,0),
         neto2             DECIMAL(13,0),
         neto_viv_mxn      DECIMAL(13,0),
         nombreB           CHAR(40)     ,
         nombreT           CHAR(40)     ,
         nss               CHAR(11)     ,
         observaciones     CHAR(127)    ,
         otros1            DECIMAL(13,2),
         otros2            DECIMAL(13,2),
         otros_viv         DECIMAL(13,0),
         paternoB          CHAR(40)     ,
         paternoT          CHAR(40)     ,
         pension           CHAR(2)      ,
         prestacion        CHAR(2)      ,
         referencia        CHAR(20)     ,
         regimen           CHAR(2)      ,
         retiro            CHAR(1)      ,
         retiro92_1        DECIMAL(13,2),
         retiro92_2        DECIMAL(13,2),
         retiro97_1        DECIMAL(13,2),
         retiro97_2        DECIMAL(13,2),
         rfcB              CHAR(13)     ,
         rfcT              CHAR(13)     ,
--         curp              CHAR(18)     ,
         secuencia         CHAR(5)      ,
         seguro            CHAR(2)      ,
         semanas           SMALLINT     ,
         total_            DECIMAL(13,2),     
         viv_92_mxn        DECIMAL(13,2),
         viv_97_mxn        DECIMAL(13,2)
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
         codigoRetorno  SMALLINT, -- codigo de retorno
         mensaje        STRING    -- descripcion del codigo de retorno
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
         
-- constantes para codigos de retorno
CONSTANT gi_solicitud_aceptada_afore            SMALLINT = 15 -- En ventanilla AFORE las solicitudes se crean autorizadas
CONSTANT gi_error_id_beneficiario_vacio                 SMALLINT = 201,
         gi_error_nss_vacio                             SMALLINT = 202,
         gi_error_nss_no_encontrado                     SMALLINT = 261,
         gi_error_rfc_vacio                             SMALLINT = 206,
         gi_error_rfc_invalido                          SMALLINT = 233,
         gi_error_curp_vacio                            SMALLINT = 207,
         gi_error_curp_invalido                         SMALLINT = 234,
         gi_error_clabe_vacio                           SMALLINT = 209,
         gi_error_clabe_estructura_invalida             SMALLINT = 237, -- longitud invalida
         gi_error_clabe_invalido                        SMALLINT = 258, -- algo esta mal en la clabe
         gi_error_gpo_trabajador_vacio                  SMALLINT = 210,
         gi_error_sec_pension_vacio                     SMALLINT = 211,
         gi_error_regimen_vacio                         SMALLINT = 212,
         gi_error_regimen_invalido                      SMALLINT = 239, -- cuando es diferente a 73
         gi_error_tpo_retiro_vacio                      SMALLINT = 213,
         gi_error_tpo_seguro_vacio                      SMALLINT = 214,
         gi_error_tpo_pension_vacio                     SMALLINT = 215,
         gi_error_tpo_prestacion_vacio                  SMALLINT = 216,
         gi_error_sem_cotizadas_vacio                   SMALLINT = 217,
         gi_error_nombre_pensionado_vacio               SMALLINT = 218,
         gi_error_ap_paterno_pensionado_vacio           SMALLINT = 204,
         gi_error_ap_pat_beneficiario_vacio             SMALLINT = 407,
         gi_error_ap_paterno_pensionado_invalido        SMALLINT = 470,
         gi_error_rfc_beneficiario_vacio                SMALLINT = 221,
         gi_error_rfc_beneficiario_invalido             SMALLINT = 240,
         gi_error_curp_beneficiario_vacio               SMALLINT = 241,
         gi_error_aivs_viv92_viv97_vacio                SMALLINT = 242,
         gi_error_aivs_viv97_vacio                      SMALLINT = 242,
         gi_error_fec_valor_vivienda_vacio              SMALLINT = 244,
         gi_error_fec_valor_vivienda_invalido           SMALLINT = 256,
         gi_error_pesos_viv92_vacio                     SMALLINT = 246,
         gi_error_pesos_viv92_viv97_vacio               SMALLINT = 246,
         gi_error_pesos_viv92_negativo                  SMALLINT = 260,
         gi_error_pesos_viv92_viv97_negativo            SMALLINT = 260,
         gi_error_pesos_viv97_vacio                     SMALLINT = 246,
         gi_error_pesos_viv97_negativo                  SMALLINT = 260,
         gi_error_imp_neto_vivienda_vacio               SMALLINT = -999 -- no viene codigo pero si que es obligatorio
{
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201,
         gi_error_beneficiario_vacio                    SMALLINT = 201
}         
         
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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW43."
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
            OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Actualización CLABE Ventanilla Afore Service") --, STYLE="naked")
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
    DISPLAY "Iniciando servidor de WS de Actualización de Cuenta CLABE Ventanilla Afore..."

    -- se inicia el motor de WS
    CALL com.WebServiceEngine.Start()
    DISPLAY "Servidor en escucha..."

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
                    DISPLAY "Se recibio otro codigo de retorno: ", v_resultado
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
    LET v_service_NameSpace = "http://ws.actualizaClabe.org.infonavit.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("WSActualizaClabePort", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("actualizaClabe","actualizaClabe",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        CALL v_webservice.publishOperation(op, "actualizaClabe")

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
            CALL ERRORLOG("Se registro el servicio Actualizacion de Cuenta CLABE Ventanilla Afore")
        END IF

        CATCH -- en caso de error
            DISPLAY("No se pudo crear el servicio 'Actualizacion de Cuenta CLABE Ventanilla Afore': " || STATUS)
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
Nombre: actualizaClabe
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que Actualiza la Cuenta CLABE de proceso Ventanilla Afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION actualizaClabe()
DEFINE v_indice_retiro         SMALLINT,
       v_nss                   LIKE ret_solicitud_generico.nss,
       v_datos_validos         SMALLINT, -- booleana que indica si los datos de entrada son correctos
       v_disponibilidad_valida SMALLINT, -- Bolleana que indica el resultado de la consulta de disponibilidad
       v_cod_retorno           SMALLINT, -- codigo de retorno
       v_desc_retorno          STRING,   -- descripcion del codigo de retorno
       v_cta_clabe_correcta    SMALLINT,  -- booleana que indica si la cuenta clabe tiene estructura correcta
       v_id_derechohabiente    DECIMAL(9,0),
       v_respuesta_marca       SMALLINT,
       v_id_solicitud          DECIMAL(9,0),
       v_resultado             SMALLINT 
       
    -- se verifica si se esta solicitando eco
    IF ( UPSHIFT(ws_ret_generico_solicitud_in.nss) = "ECO" ) THEN
        -- se devuelve ECO
        LET ws_ret_generico_solicitud_out.codigoretorno  = 0
        LET ws_ret_generico_solicitud_out.mensaje = "ECO"
        DISPLAY "Se recibio ECO"
    ELSE
        -- se crea el registro de la bitacora de la peticion ws registro de solicitud
        CALL fn_registra_peticion_registro_solicitud_va(1, NULL)
        RETURNING g_id_peticion

        -- se valida que los datos se hayan recibido
        CALL fn_valida_datos_entrada() RETURNING v_datos_validos, v_cod_retorno, v_desc_retorno

        -- si los datos son incorrectos
        IF ( NOT v_datos_validos ) THEN
            -- se rechaza la solicitud
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        -- se obtiene el NSS
        LET v_nss = ws_ret_generico_solicitud_in.nss

        -- se valida que los datos se hayan recibido
        CALL fn_valida_disponibilidad(v_nss) RETURNING v_disponibilidad_valida, v_cod_retorno, v_desc_retorno
        IF v_cod_retorno <> 99 THEN 
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        CALL fn_busca_solicitud(v_nss) RETURNING v_id_solicitud
        IF v_id_solicitud = 0 THEN 
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, gi_error_nss_no_encontrado)
            RETURN
        END IF
        -- Guarda la Solicitud
        CALL fn_actaliza_cuenta_clabe(v_nss, v_id_solicitud, gi_solicitud_aceptada_afore, 
                                     ws_ret_generico_solicitud_in.clabe) RETURNING v_resultado
        IF v_resultado <> 0 THEN 
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, -999)
            RETURN
        END IF 

        CALL fn_respuesta_ws_ActClabe(gi_solicitud_aceptada_afore, v_cod_retorno)

    END IF 
   
END FUNCTION

{
======================================================================
Nombre: fn_valida_datos_entrada
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Valida que se hayan recibido el NSS y la Cuenta CLABE

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_datos_entrada()
DEFINE v_datos_correctos SMALLINT,
       v_cod_retorno     SMALLINT,
       v_desc_retorno    STRING
       
    -- se asume que los datos son correctos
    LET v_datos_correctos = TRUE
    LET v_cod_retorno     = 99 -- solicitud correcta
    LET v_desc_retorno    = "SOLICITUD ACEPTADA"


    -- se valida que vengan el NSS y la cuenta CLABE

    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.nss) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_nss_vacio              -- 202
        LET v_desc_retorno    = "NSS obligatorio"
    END IF

    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.clabe) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_clabe_vacio                       -- 209
        LET v_desc_retorno    = "CLABE obligatorio"
    ELSE
      -- debe tener 18 caracteres
      -- Validacion ID 6 CLABE Punto 1.60 G. O.
      IF ( NOT fn_verifica_estructura_clabe(ws_ret_generico_solicitud_in.clabe) ) THEN
         LET v_datos_correctos = FALSE
         LET v_cod_retorno     = gi_error_clabe_estructura_invalida      -- 237
         LET v_desc_retorno    = "CLABE menor a 18 caracteres"
--      ELSE
--         IF ( NOT fn_verifica_clabe_algoritmo(ws_ret_generico_solicitud_in.clabe) ) THEN
--            LET v_datos_correctos = FALSE
--            LET v_cod_retorno     = gi_error_clabe_invalido      -- 258
--            LET v_desc_retorno    = "CLABE inválida"
--         END IF
      END IF
    END IF

    -- se devuelve el resultado de la validacion
    RETURN v_datos_correctos, v_cod_retorno, v_desc_retorno
END FUNCTION
{
======================================================================
Nombre: fn_valida_disponibilidad
Fecha creacion: Diciembre 04, 2013
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Vaida si previamente existe un registro con resultado exitoso para 
continuar con la captura de la solicitud
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_disponibilidad(p_nss)
DEFINE p_nss                   CHAR(11),
       v_disponibilidad_valida SMALLINT,
       v_cod_retorno           SMALLINT,
       v_desc_retorno          STRING,
       v_id_consulta           DECIMAL(9,0),
       v_mensaje               CHAR(100),
       ls_cadena               STRING -- cadena auxiliar para 

    -- se asume que los datos son correctos
    LET v_disponibilidad_valida = TRUE
    LET v_id_consulta           = 0
    LET v_cod_retorno           = 99 -- solicitud correcta
    LET v_desc_retorno          = "SOLICITUD ACEPTADA"


    SELECT COUNT(*) 
    INTO   v_id_consulta
    FROM   ret_solicitud_generico rg,
           ret_ley73_generico rl,
           ret_sol_medio_entrega rsme
    WHERE  rl.id_solicitud     = rg.id_solicitud
    AND    rg.id_solicitud     = rsme.id_solicitud
    AND    rg.nss              = p_nss
    AND    rg.modalidad_retiro = 3
    AND    rl.gpo_ley73        = 1
    AND    rsme.medio_entrega  = 5
    AND    rsme.grupo          = 1
    AND    rg.estado_solicitud = 10

    IF v_id_consulta = 0 OR v_id_consulta IS NULL THEN 
        LET v_cod_retorno  = 102
        LET v_desc_retorno = "NSS no se encuentra registrado en la base de datos del INFONAVIT"
        LET v_disponibilidad_valida = FALSE 
    END IF 
    --RETURN TRUE,0,"CORRECTO"

    -- se devuelve el resultado de la validacion
    RETURN v_disponibilidad_valida, v_cod_retorno, v_desc_retorno
END FUNCTION

{
======================================================================
Nombre: fn_dato_vacio
Fecha creacion: Diciembre 04, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un dato esta vacio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_dato_vacio(p_dato)
DEFINE p_dato STRING

    -- se verifica si el dato esta vacio
    IF ( p_dato.trim() IS NULL ) THEN
        RETURN TRUE
    ELSE
        RETURN FALSE
    END IF
END FUNCTION
       

       

{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_ActClabe
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Construye la respuesta de la Actualización de la Cuenta CLABE

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_ws_ActClabe(p_estado_solicitud, p_cod_rechazo)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         v_des_larga        LIKE ret_rechazo.des_larga, -- descripcion del codigo de rechazo
         p_subcuenta        SMALLINT, -- subcuenta de inversion
         p_importe_aivs     DECIMAL(24,6), -- monto en AIVs
         p_fecha_valuacion  DATE, -- fecha de valuacion
         v_valor_fondo      LIKE glo_valor_fondo.precio_fondo
            
    -- se genera el registro de disponibilidad
    IF ( p_estado_solicitud = gi_solicitud_aceptada_afore ) THEN
        -- solicitud aceptada
        LET ws_ret_generico_solicitud_out.codigoRetorno  = 0
        LET ws_ret_generico_solicitud_out.mensaje = "Correcto"
    ELSE
        -- peticion rechazada
        -- se obtiene la descripcion del error
        SELECT des_larga
        INTO   v_des_larga
        FROM   ret_rechazo
        WHERE  cod_rechazo = (p_cod_rechazo + 1000)
      
        -- si no se encuentra se verifica si es alguno de los que se tienen en las validaciones
        IF ( v_des_larga IS NULL ) THEN
            CASE p_cod_rechazo
                WHEN gi_error_nss_vacio                      
                    LET v_des_larga = "NSS obligatorio"
                WHEN gi_error_nss_no_encontrado              
                    LET v_des_larga = "Solicitud de devolución no existe"
                WHEN gi_error_clabe_vacio                    
                    LET v_des_larga = "CLABE obligatorio"
                WHEN gi_error_clabe_estructura_invalida      
                    LET v_des_larga = "CLABE estructura invalida"
                WHEN gi_error_clabe_invalido                 
                    LET v_des_larga = "CLABE invalida"
                OTHERWISE
                    LET v_des_larga = "Codigo no definido."
            END CASE
        END IF
      
        LET ws_ret_generico_solicitud_out.codigoRetorno  = p_cod_rechazo
        LET ws_ret_generico_solicitud_out.mensaje = v_des_larga
    END IF

END FUNCTION




{
======================================================================
Nombre: fn_actaliza_cuenta_clabe
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Actualiza la solicitud de retiro a autorizada y actualiza la cuenta clabe

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_actaliza_cuenta_clabe(p_nss, p_id_solicitud, p_estado_solicitud, p_clabe)
DEFINE p_id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
       p_nss                 LIKE ret_solicitud_generico.nss, 
       p_clabe               LIKE ret_pago_spei.cuenta_clabe,
       p_estado_solicitud    SMALLINT,  -- estatus de la solicitud
       v_resultado           SMALLINT 
             

    -- si la solicitud fue aceptada
    IF ( p_estado_solicitud = gi_solicitud_aceptada_afore ) THEN
        DISPLAY "Actualiza la solicitud y la cuenta clabe ..."      
        LET v_resultado = 0
        
        -- se asginan los datos al retistro de solicitud
        UPDATE ret_solicitud_generico
        SET    estado_solicitud = p_estado_solicitud
        WHERE  id_solicitud = p_id_solicitud

        UPDATE ret_ley73_generico
        SET    estado_solicitud = p_estado_solicitud
        WHERE  id_solicitud = p_id_solicitud

        UPDATE ret_pago_spei
        SET    cuenta_clabe = p_clabe
        WHERE  id_solicitud = p_id_solicitud
    ELSE 
        LET v_resultado = 1
    END IF 
    
    RETURN v_resultado
END FUNCTION 

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud_va
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para la Actualizaciónn de la Cuenta CLABE 
de Ventanilla Afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud_va(p_ind_operacion, p_id_solicitud)
DEFINE v_id_peticion                    DECIMAL(9,0), -- id de la peticion
       p_ind_operacion                  SMALLINT, -- 1 inserta, 2 actualiza
       p_id_solicitud                   DECIMAL(9,0), -- id de la solicitud generada
       v_r_ret_ws_sol_retiro_vent_afore RECORD LIKE ret_ws_sol_retiro_vent_afore.* -- registro de peticion al ws
		
    -- si se solicita crear la bitacora de la peticion
    IF ( p_ind_operacion = 1 ) THEN
        -- se obtiene el id de peticion nuevo
        SELECT seq_ret_ws_generico.nextVal
        INTO   v_id_peticion
        FROM   systables
        WHERE  tabid = 1
      
        -- se asignan los datos
        LET v_r_ret_ws_sol_retiro_vent_afore.id_peticion             = v_id_peticion
        LET v_r_ret_ws_sol_retiro_vent_afore.id_solicitud            = NULL
        LET v_r_ret_ws_sol_retiro_vent_afore.ind_beneficiario            = ws_ret_generico_solicitud_in.beneficiario       
        LET v_r_ret_ws_sol_retiro_vent_afore.entidad_federativa      = ws_ret_generico_solicitud_in.entidad     
        LET v_r_ret_ws_sol_retiro_vent_afore.nss                     = ws_ret_generico_solicitud_in.nss                    
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc                     = ws_ret_generico_solicitud_in.rfcT                    
--        LET v_r_ret_ws_sol_retiro_vent_afore.curp                    = ws_ret_generico_solicitud_in.curp                   
        LET v_r_ret_ws_sol_retiro_vent_afore.clabe                   = ws_ret_generico_solicitud_in.clabe                  
        LET v_r_ret_ws_sol_retiro_vent_afore.gpo_trabajador          = ws_ret_generico_solicitud_in.grupo
        LET v_r_ret_ws_sol_retiro_vent_afore.sec_pension             = ws_ret_generico_solicitud_in.secuencia            
        LET v_r_ret_ws_sol_retiro_vent_afore.regimen                 = ws_ret_generico_solicitud_in.regimen                
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_retiro              = ws_ret_generico_solicitud_in.retiro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_seguro              = ws_ret_generico_solicitud_in.seguro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_pension             = ws_ret_generico_solicitud_in.pension            
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_prestacion          = ws_ret_generico_solicitud_in.prestacion         
        LET v_r_ret_ws_sol_retiro_vent_afore.sem_cotizadas           = ws_ret_generico_solicitud_in.semanas
        LET v_r_ret_ws_sol_retiro_vent_afore.nombre_pensionado       = ws_ret_generico_solicitud_in.nombreT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_pensionado   = ws_ret_generico_solicitud_in.paternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_pensionado   = ws_ret_generico_solicitud_in.maternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc_beneficiario        = ws_ret_generico_solicitud_in.rfcB
        LET v_r_ret_ws_sol_retiro_vent_afore.curp_beneficiario       = ws_ret_generico_solicitud_in.curpB
--        LET v_r_ret_ws_sol_retiro_vent_afore.cve1_siefore            = ws_ret_generico_solicitud_in.cve1_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve1_siefore      = ws_ret_generico_solicitud_in.retiro92_1
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve1_siefore      = ws_ret_generico_solicitud_in.retiro97_1
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve1_siefore      = ws_ret_generico_solicitud_in.otros1
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve1_siefore   = ws_ret_generico_solicitud_in.neto1
--        LET v_r_ret_ws_sol_retiro_vent_afore.cve2_siefore            = ws_ret_generico_solicitud_in.cve2_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve2_siefore      = ws_ret_generico_solicitud_in.retiro92_2
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve2_siefore      = ws_ret_generico_solicitud_in.retiro97_2
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve2_siefore      = ws_ret_generico_solicitud_in.otros2
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve2_siefore   = ws_ret_generico_solicitud_in.neto2
--        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_total_siefores = ws_ret_generico_solicitud_in.neto_total_siefores
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97              = ws_ret_generico_solicitud_in.aiv_97             
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92              = ws_ret_generico_solicitud_in.aiv_92             
        LET v_r_ret_ws_sol_retiro_vent_afore.f_valor                 = ws_ret_generico_solicitud_in.fecha_viv                
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv92             = ws_ret_generico_solicitud_in.viv_92_mxn            
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv97             = ws_ret_generico_solicitud_in.viv_97_mxn            
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_vivienda          = ws_ret_generico_solicitud_in.otros_viv         
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_dep_vivienda   = ws_ret_generico_solicitud_in.neto_viv_mxn  
--        LET v_r_ret_ws_sol_retiro_vent_afore.imp_aplicados_afore     = ws_ret_generico_solicitud_in.imp_aplicados_afore    
        LET v_r_ret_ws_sol_retiro_vent_afore.f_pago                  = ws_ret_generico_solicitud_in.fechaPago                 
        LET v_r_ret_ws_sol_retiro_vent_afore.referencia_pago         = ws_ret_generico_solicitud_in.referencia
        LET v_r_ret_ws_sol_retiro_vent_afore.observaciones           = ws_ret_generico_solicitud_in.observaciones          
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_notificacion      = ws_ret_generico_solicitud_in.folioNoti
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_operacion         = ws_ret_generico_solicitud_in.folioOper
--        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92_aplicados    = ws_ret_generico_solicitud_in.aivs_viv92_aplicados   
--        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97_aplicados    = ws_ret_generico_solicitud_in.aivs_viv97_aplicados   
--        LET v_r_ret_ws_sol_retiro_vent_afore.viv92_aplicados         = ws_ret_generico_solicitud_in.viv92_aplicados        
--        LET v_r_ret_ws_sol_retiro_vent_afore.viv97_aplicados         = ws_ret_generico_solicitud_in.viv97_aplicados        
--        LET v_r_ret_ws_sol_retiro_vent_afore.estatus_vivienda        = ws_ret_generico_solicitud_in.estatus_vivienda       
--        LET v_r_ret_ws_sol_retiro_vent_afore.diag_recepcion          = ws_ret_generico_solicitud_in.diag_recepcion         
--        LET v_r_ret_ws_sol_retiro_vent_afore.desc_diagnostico        = ws_ret_generico_solicitud_in.desc_diagnostico       
--        LET v_r_ret_ws_sol_retiro_vent_afore.result_operacion        = ws_ret_generico_solicitud_in.result_operacion       
--        LET v_r_ret_ws_sol_retiro_vent_afore.det_resultado           = ws_ret_generico_solicitud_in.det_resultado          
        LET v_r_ret_ws_sol_retiro_vent_afore.cve_afore               = ws_ret_generico_solicitud_in.cve_afore              
        LET v_r_ret_ws_sol_retiro_vent_afore.nom_trabajador          = ws_ret_generico_solicitud_in.nombreT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_trabajador   = ws_ret_generico_solicitud_in.paternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_trabajador   = ws_ret_generico_solicitud_in.maternoT
      
        -- se inserta el registro de peticion
        INSERT INTO ret_ws_sol_retiro_vent_afore VALUES ( v_r_ret_ws_sol_retiro_vent_afore.* )
    END IF

    -- se devuelve el id de la peticion
    RETURN v_id_peticion
END FUNCTION


{
======================================================================
Nombre: fn_busca_solicitud
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Busca el Id_solicitud para la actualización de la cuenta CLABE
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_solicitud(p_nss)
DEFINE p_nss                   CHAR(11),
       v_id_solicitud          DECIMAL(9,0),
       v_cod_retorno           SMALLINT,
       v_desc_retorno          STRING,
       v_id_consulta           DECIMAL(9,0),
       v_mensaje               CHAR(100),
       ls_cadena               STRING -- cadena auxiliar para 

    -- se asume que los datos son correctos
    LET v_id_solicitud     = 0


    SELECT rg.id_solicitud
    INTO   v_id_solicitud
    FROM   ret_solicitud_generico rg,
           ret_ley73_generico rl,
           ret_sol_medio_entrega rsme
    WHERE  rl.id_solicitud     = rg.id_solicitud
    AND    rg.id_solicitud     = rsme.id_solicitud
    AND    rg.nss              = p_nss
    AND    rg.modalidad_retiro = 3
    AND    rl.gpo_ley73        = 1
    AND    rsme.medio_entrega  = 5
    AND    rsme.grupo          = 1
--    AND    rg.grupo_ventanilla = 101
    AND    rg.estado_solicitud = 10

    IF v_id_solicitud IS NULL OR v_id_solicitud = 0 THEN 
        LET v_id_solicitud = 0
    END IF 

    -- se devuelve el id_derechohabiente encontrado
    RETURN v_id_solicitud
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

--   DISPLAY "En la función de validacion CLABE :", p_clabe
   --- Valida el Banco
   LET v_banco = p_clabe[1,3]
   SELECT COUNT(*)
   INTO   v_ocurre
   FROM   cat_entidad_financiera
   WHERE  cve_ent_financiera = v_banco

--   DISPLAY "Entro el banco ", v_banco, " - ", v_ocurre
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
--         DISPLAY "v_suma_c :", v_suma_c, " con el indice :", i
         LET v_clabe_digitos[i,i] = v_suma_c[2,2]
         LET v_suma = v_suma + v_clabe_digitos[i,i]
      END FOR 
--      DISPLAY "los multiplos >", v_clabe_digitos, "<"
--      DISPLAY "La suma :", v_suma
      LET v_suma_c = v_suma USING "&&"
--      DISPLAY "v_suma_c para el digito: ", v_suma_c[2,2]
      LET v_digito = 10 - v_suma_c[2,2] 
--      DISPLAY "El digito :", v_digito
--      DISPLAY "EL digito CLABE :", p_clabe[18,18]
      IF v_digito <> p_clabe[18,18] THEN 
         LET v_clabe_correcta = FALSE 
      END IF 
   ELSE 
      LET v_clabe_correcta = FALSE 
   END IF 
   -- se devuelve el resultado de la consulta
   RETURN v_clabe_correcta
END FUNCTION
