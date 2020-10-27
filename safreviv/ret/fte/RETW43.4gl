--===============================================================
-- VERSION: 2.0.0
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
#FECHA INICIO      => 04-SEP-2018                                             #
###############################################################################
IMPORT FGL WSHelper

IMPORT com
IMPORT XML
DATABASE safre_viv


GLOBALS "RETW43.inc"
GLOBALS "RETG01.4gl"

GLOBALS 
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

   DEFINE serverURL STRING -- URL del servidor
   DEFINE v_pantalla    SMALLINT

   END GLOBALS 
         
MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 
   
   CALL CreateWSActualizaClabeServiceService() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      #
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      #
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM   # The Application server has closed the connection
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF     
   END WHILE
END MAIN

#-------------------------------------------------------------------------------
# Service: WSActualizaClabeService
# Port:    WSActualizaClabePort
#-------------------------------------------------------------------------------
#
# FUNCTION CreateWSActualizaClabeServiceService
#   RETURNING soapstatus
#
FUNCTION CreateWSActualizaClabeServiceService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("WSActualizaClabeService","http://ws.actualizaClabe.org.infonavit.mx/")
    CALL service.setFeature("Soap1.1",TRUE)

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(WSActualizaClabeServiceHttpIn)
    CALL service.registerOutputHttpVariable(WSActualizaClabeServiceHttpOut)


    #
    # Create Faults
    #
    CALL service.createFault(ProcesarExcepcion,false)

    #
    # Operation: actualizaClabe
    #

    # Publish Operation : actualizaClabe
    LET operation = com.WebOperation.CreateDOCStyle("actualizaClabe","actualizaClabe",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
    CALL operation.addFault(ProcesarExcepcion,NULL)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

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
    IF ( UPSHIFT(ws_ret_generico_solicitud_in.arg0.nss = "ECO" )) THEN
        -- se devuelve ECO
        LET ws_ret_generico_solicitud_out.return.codigoRetorno  = 0
        LET ws_ret_generico_solicitud_out.return.mensaje = "ECO"
        DISPLAY "Se recibio ECO"
    ELSE
        -- se crea el registro de la bitacora de la peticion ws registro de solicitud
        CALL fn_registra_peticion_registro_solicitud_va(1, NULL)
        RETURNING g_id_peticion
        DISPLAY "<", CURRENT YEAR TO FRACTION, "> NSS RECIBIDO:",ws_ret_generico_solicitud_in.arg0.nss

        -- se valida que los datos se hayan recibido
        CALL fn_valida_datos_entrada() RETURNING v_datos_validos, v_cod_retorno, v_desc_retorno

        -- si los datos son incorrectos
        IF ( NOT v_datos_validos ) THEN
            -- se rechaza la solicitud
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        -- se valida que los datos se hayan recibido
        CALL fn_valida_datos_entrada_complemento() RETURNING v_datos_validos, v_cod_retorno, v_desc_retorno

        -- si los datos son incorrectos
        IF ( NOT v_datos_validos ) THEN
            -- se rechaza la solicitud
            CALL fn_respuesta_ws_ActClabe(gi_solicitud_rechazada, v_cod_retorno)
            RETURN
        END IF

        
        -- se obtiene el NSS
        LET v_nss = ws_ret_generico_solicitud_in.arg0.nss

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
                                     ws_ret_generico_solicitud_in.arg0.clabe) RETURNING v_resultado
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

    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.nss) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_nss_vacio              -- 202
        LET v_desc_retorno    = "NSS obligatorio"
    END IF

    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.clabe) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_clabe_vacio                       -- 209
        LET v_desc_retorno    = "CLABE obligatorio"
    ELSE
      -- debe tener 18 caracteres
      -- Validacion ID 6 CLABE Punto 1.60 G. O.
      IF ( NOT fn_verifica_estructura_clabe(ws_ret_generico_solicitud_in.arg0.clabe) ) THEN
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
Nombre: fn_valida_datos_entrada_complemento
Fecha creacion: Septiembre 24, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Valida que se hayan recibido el NSS y la Cuenta CLABE

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_datos_entrada_complemento()
DEFINE v_datos_correctos SMALLINT,
       v_cod_retorno     SMALLINT,
       v_desc_retorno    STRING
       
    -- se asume que los datos son correctos
    LET v_datos_correctos = TRUE
    LET v_cod_retorno     = 99 -- solicitud correcta
    LET v_desc_retorno    = "SOLICITUD ACEPTADA"

    -- Valida Indicador Beneficiario
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.beneficiario) OR 
         (ws_ret_generico_solicitud_in.arg0.beneficiario <> 1 AND 
          ws_ret_generico_solicitud_in.arg0.beneficiario <> 2)) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_id_beneficiario_vacio              -- 202
        LET v_desc_retorno    = "Beneficiario obligatorio y válido"
    END IF
    -- Valida RFC del Trabajador
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.rfcT) ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_rfc_vacio              -- 206
        LET v_desc_retorno    = "RFC del trabajador obligatorio"
    ELSE 
      IF LENGTH(ws_ret_generico_solicitud_in.arg0.rfcT) < 13 THEN 
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_rfc_invalido              -- 233
        LET v_desc_retorno    = "RFC del trabajador < a 13 posiciones"
      END IF 
    END IF

    -- Valida el Apellido paterno
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.paternoT)) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_ap_paterno_pensionado_vacio              -- 204
        LET v_desc_retorno    = "Apellido paterno del pensionado obligatorio"
    END IF

    -- Valida la CURP del trabajador
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.curpT)) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_curp_vacio              -- 207
        LET v_desc_retorno    = "CURP del trabajador Obligatorio"
    ELSE 
      IF LENGTH(ws_ret_generico_solicitud_in.arg0.curpT) < 18 THEN 
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_curp_invalido              -- 234
        LET v_desc_retorno    = "CURP del trabajador < a 18 posiciones"
      END IF 
    END IF

    -- Valida el Grupo de Trabajador
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.grupo)) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_gpo_trabajador_vacio              -- 210
        LET v_desc_retorno    = "Grupo de trabajador obligatorio"
    END IF

    -- Valida la secuencia de pensión
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.secuencia )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_sec_pension_vacio             -- 211
        LET v_desc_retorno    = "Secuencia de pensión obligatorio"
    END IF

    -- Valida el régimen
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.regimen )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_regimen_vacio             -- 212
        LET v_desc_retorno    = "Régimen obligatorio"
    ELSE 
        IF ws_ret_generico_solicitud_in.arg0.regimen <> 73 THEN 
           LET v_datos_correctos = FALSE
           LET v_cod_retorno     = gi_error_regimen_vacio             -- 239
           LET v_desc_retorno    = "Régimen diferente a 73"
        END IF 
    END IF

    -- Valida el tipo de retiro
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.retiro )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_retiro_vacio             -- 213
        LET v_desc_retorno    = "Tipo de retiro obligatorio"
    END IF

    -- Valida el tipo de seguro
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.seguro )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_seguro_vacio             -- 214
        LET v_desc_retorno    = "Tipo de seguro obligatorio"
    END IF
    
    -- Valida el tipo de pensión
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.pension )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_pension_vacio             -- 215
        LET v_desc_retorno    = "Tipo de pensión obligatorio"
    END IF

    -- Valida el tipo de prestación
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.prestacion )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_tpo_prestacion_vacio             -- 216
        LET v_desc_retorno    = "Tipo de prestación obligatorio"
    END IF

    -- Valida las semanas cotizadas
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.semanas )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_sem_cotizadas_vacio             -- 217
        LET v_desc_retorno    = "Semanas cotizadas obligatorias"
    END IF

    -- Valida el nombre del beneficiario
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.nombreB ) AND 
         ws_ret_generico_solicitud_in.arg0.beneficiario = 1 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_nombre_pensionado_vacio             -- 218
        LET v_desc_retorno    = "Nombre de beneficiario obligatorio"
    END IF

    -- Valida el apellido paterno beneficiario
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.paternoB ) AND 
         ws_ret_generico_solicitud_in.arg0.beneficiario = 1 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_ap_paterno_pensionado_invalido             -- 470
        LET v_desc_retorno    = "Apellido paterno inválido"
    END IF

    -- Valida el RFC del beneficiario
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.rfcB ) AND 
         ws_ret_generico_solicitud_in.arg0.beneficiario = 1 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_rfc_beneficiario_vacio             -- 221
        LET v_desc_retorno    = "RFC del beneficiario obligatorio"
    ELSE 
      IF LENGTH(ws_ret_generico_solicitud_in.arg0.rfcB) < 13 THEN 
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_rfc_beneficiario_invalido              -- 240
        LET v_desc_retorno    = "RFC del beneficiario < a 13 posiciones"
      END IF 
    END IF

    -- Valida el CURP del beneficiario
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.curpB ) AND 
         ws_ret_generico_solicitud_in.arg0.beneficiario = 1 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_curp_beneficiario_vacio             -- 241
        LET v_desc_retorno    = "CURP del beneficiario < a 18 posiciones"
    END IF

    -- Valida los montos de las AIVS
    IF ( ws_ret_generico_solicitud_in.arg0.aiv_92 <= 0  AND 
         ws_ret_generico_solicitud_in.arg0.aiv_97 <= 0 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_aivs_viv92_viv97_vacio             -- 242
        LET v_desc_retorno    = "Vivienda 92 AIVS y vivienda 97 AIVS están con importe en cero"
    END IF

    -- Valida la fecha valor de vivienda
    IF ( fn_dato_vacio(ws_ret_generico_solicitud_in.arg0.fecha_viv )) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_fec_valor_vivienda_vacio             -- 244
        LET v_desc_retorno    = "Falta fecha valor vivienda (movimiento contable)"
    END IF

    -- Valida los montos de las viviendas en pesos
    IF ( ws_ret_generico_solicitud_in.arg0.viv_92_mxn <= 0  AND 
         ws_ret_generico_solicitud_in.arg0.viv_97_mxn <= 0 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_pesos_viv92_viv97_vacio             -- 242
        LET v_desc_retorno    = "Vivienda 92 mxn y vivienda 97 mxn están con importe en cero"
    END IF
    
    -- Valida los montos de las viviendas en pesos no sean negativos
    IF ws_ret_generico_solicitud_in.arg0.viv_92_mxn IS NULL OR 
       ws_ret_generico_solicitud_in.arg0.viv_92_mxn = ' ' THEN 
      LET ws_ret_generico_solicitud_in.arg0.viv_92_mxn = 0 
    END IF
    IF ws_ret_generico_solicitud_in.arg0.viv_97_mxn IS NULL OR 
       ws_ret_generico_solicitud_in.arg0.viv_97_mxn = ' ' THEN 
       LET ws_ret_generico_solicitud_in.arg0.viv_97_mxn = 0 
    END IF 
    IF ( ws_ret_generico_solicitud_in.arg0.viv_92_mxn < 0  OR  
         ws_ret_generico_solicitud_in.arg0.viv_97_mxn < 0 ) THEN
        LET v_datos_correctos = FALSE
        LET v_cod_retorno     = gi_error_pesos_viv92_viv97_negativo             -- 260
        LET v_desc_retorno    = "Vivienda 92 mxn y vivienda 97 mxn están con importe negativos"
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


--    SELECT COUNT(*) 
--    INTO   v_id_consulta
--    FROM   ret_solicitud_generico rg,
--           ret_ley73_generico rl,
--           ret_sol_medio_entrega rsme
--    WHERE  rl.id_solicitud     = rg.id_solicitud
--    AND    rg.id_solicitud     = rsme.id_solicitud
--    AND    rg.nss              = p_nss
--    AND    rg.modalidad_retiro = 3
--    AND    rl.gpo_ley73        = 1
--    AND    rsme.medio_entrega  = 5
---   AND    rsme.grupo          = 1
--    AND    rg.estado_solicitud = 10

    SELECT COUNT(*)
    INTO   v_id_consulta
    FROM   afi_derechohabiente
    WHERE  nss = p_nss

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
         v_resultado        INTEGER
         
    -- se genera el registro de disponibilidad
    IF ( p_estado_solicitud = gi_solicitud_aceptada_afore ) THEN
        -- solicitud aceptada
        LET ws_ret_generico_solicitud_out.return.codigoRetorno  = 101
        LET ws_ret_generico_solicitud_out.return.mensaje = "Correcto"
    ELSE
        -- peticion rechazada
        -- se obtiene la descripcion del error
        SELECT des_larga
        INTO   v_des_larga
        FROM   ret_rechazo_generico
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
      
        LET ws_ret_generico_solicitud_out.return.codigoRetorno  = p_cod_rechazo
        LET ws_ret_generico_solicitud_out.return.mensaje = v_des_larga
    END IF
    DISPLAY "<", CURRENT YEAR TO FRACTION, "> Código : ", ws_ret_generico_solicitud_out.return.codigoRetorno 
    DISPLAY "<", CURRENT YEAR TO FRACTION, "> Mensaje: ", ws_ret_generico_solicitud_out.return.mensaje
    DISPLAY "<", CURRENT YEAR TO FRACTION, "> NSS    : ", ws_ret_generico_solicitud_in.arg0.nss
    CALL fn_registra_respuesta(ws_ret_generico_solicitud_out.return.codigoRetorno,ws_ret_generico_solicitud_out.return.mensaje) RETURNING v_resultado
    
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

        UPDATE ret_ws_sol_retiro_vent_afore
        SET    id_solicitud = p_id_solicitud
        WHERE id_peticion = g_id_peticion
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
        LET v_r_ret_ws_sol_retiro_vent_afore.f_confirma              = TODAY 
        LET v_r_ret_ws_sol_retiro_vent_afore.id_solicitud            = NULL
        LET v_r_ret_ws_sol_retiro_vent_afore.ind_beneficiario            = ws_ret_generico_solicitud_in.arg0.beneficiario       
        LET v_r_ret_ws_sol_retiro_vent_afore.entidad_federativa      = ws_ret_generico_solicitud_in.arg0.entidad     
        LET v_r_ret_ws_sol_retiro_vent_afore.nss                     = ws_ret_generico_solicitud_in.arg0.nss                    
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc                     = ws_ret_generico_solicitud_in.arg0.rfcT                    
--        LET v_r_ret_ws_sol_retiro_vent_afore.curp                    = ws_ret_generico_solicitud_in.curp                   
        LET v_r_ret_ws_sol_retiro_vent_afore.clabe                   = ws_ret_generico_solicitud_in.arg0.clabe                  
        LET v_r_ret_ws_sol_retiro_vent_afore.gpo_trabajador          = ws_ret_generico_solicitud_in.arg0.grupo
        LET v_r_ret_ws_sol_retiro_vent_afore.sec_pension             = ws_ret_generico_solicitud_in.arg0.secuencia            
        LET v_r_ret_ws_sol_retiro_vent_afore.regimen                 = ws_ret_generico_solicitud_in.arg0.regimen                
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_retiro              = ws_ret_generico_solicitud_in.arg0.retiro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_seguro              = ws_ret_generico_solicitud_in.arg0.seguro             
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_pension             = ws_ret_generico_solicitud_in.arg0.pension            
        LET v_r_ret_ws_sol_retiro_vent_afore.tpo_prestacion          = ws_ret_generico_solicitud_in.arg0.prestacion         
        LET v_r_ret_ws_sol_retiro_vent_afore.sem_cotizadas           = ws_ret_generico_solicitud_in.arg0.semanas
        LET v_r_ret_ws_sol_retiro_vent_afore.nombre_pensionado       = ws_ret_generico_solicitud_in.arg0.nombreT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_pensionado   = ws_ret_generico_solicitud_in.arg0.paternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_pensionado   = ws_ret_generico_solicitud_in.arg0.maternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.rfc_beneficiario        = ws_ret_generico_solicitud_in.arg0.rfcB
        LET v_r_ret_ws_sol_retiro_vent_afore.curp_beneficiario       = ws_ret_generico_solicitud_in.arg0.curpB
--        LET v_r_ret_ws_sol_retiro_vent_afore.cve1_siefore            = ws_ret_generico_solicitud_in.cve1_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve1_siefore      = ws_ret_generico_solicitud_in.arg0.retiro92_1
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve1_siefore      = ws_ret_generico_solicitud_in.arg0.retiro97_1
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve1_siefore      = ws_ret_generico_solicitud_in.arg0.otros1
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve1_siefore   = ws_ret_generico_solicitud_in.arg0.neto1
--        LET v_r_ret_ws_sol_retiro_vent_afore.cve2_siefore            = ws_ret_generico_solicitud_in.cve2_siefore           
        LET v_r_ret_ws_sol_retiro_vent_afore.ret92_cve2_siefore      = ws_ret_generico_solicitud_in.arg0.retiro92_2
        LET v_r_ret_ws_sol_retiro_vent_afore.ret97_cve2_siefore      = ws_ret_generico_solicitud_in.arg0.retiro97_2
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_cve2_siefore      = ws_ret_generico_solicitud_in.arg0.otros2
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_cve2_siefore   = ws_ret_generico_solicitud_in.arg0.neto2
--        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_total_siefores = ws_ret_generico_solicitud_in.neto_total_siefores
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97              = ws_ret_generico_solicitud_in.arg0.aiv_97             
        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92              = ws_ret_generico_solicitud_in.arg0.aiv_92             
        LET v_r_ret_ws_sol_retiro_vent_afore.f_valor                 = ws_ret_generico_solicitud_in.arg0.fecha_viv                
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv92             = ws_ret_generico_solicitud_in.arg0.viv_92_mxn            
        LET v_r_ret_ws_sol_retiro_vent_afore.pesos_viv97             = ws_ret_generico_solicitud_in.arg0.viv_97_mxn            
        LET v_r_ret_ws_sol_retiro_vent_afore.otros_vivienda          = ws_ret_generico_solicitud_in.arg0.otros_viv         
        LET v_r_ret_ws_sol_retiro_vent_afore.imp_neto_dep_vivienda   = ws_ret_generico_solicitud_in.arg0.neto_viv_mxn  
--        LET v_r_ret_ws_sol_retiro_vent_afore.imp_aplicados_afore     = ws_ret_generico_solicitud_in.imp_aplicados_afore    
        LET v_r_ret_ws_sol_retiro_vent_afore.f_pago                  = ws_ret_generico_solicitud_in.arg0.fechaPago                 
        LET v_r_ret_ws_sol_retiro_vent_afore.referencia_pago         = ws_ret_generico_solicitud_in.arg0.referencia
        LET v_r_ret_ws_sol_retiro_vent_afore.observaciones           = ws_ret_generico_solicitud_in.arg0.observaciones          
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_notificacion      = ws_ret_generico_solicitud_in.arg0.folioNoti
        LET v_r_ret_ws_sol_retiro_vent_afore.folio_operacion         = ws_ret_generico_solicitud_in.arg0.folioOper
--        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv92_aplicados    = ws_ret_generico_solicitud_in.aivs_viv92_aplicados   
--        LET v_r_ret_ws_sol_retiro_vent_afore.aivs_viv97_aplicados    = ws_ret_generico_solicitud_in.aivs_viv97_aplicados   
--        LET v_r_ret_ws_sol_retiro_vent_afore.viv92_aplicados         = ws_ret_generico_solicitud_in.viv92_aplicados        
--        LET v_r_ret_ws_sol_retiro_vent_afore.viv97_aplicados         = ws_ret_generico_solicitud_in.viv97_aplicados        
--        LET v_r_ret_ws_sol_retiro_vent_afore.estatus_vivienda        = ws_ret_generico_solicitud_in.estatus_vivienda       
--        LET v_r_ret_ws_sol_retiro_vent_afore.diag_recepcion          = ws_ret_generico_solicitud_in.diag_recepcion         
--        LET v_r_ret_ws_sol_retiro_vent_afore.desc_diagnostico        = ws_ret_generico_solicitud_in.desc_diagnostico       
--        LET v_r_ret_ws_sol_retiro_vent_afore.result_operacion        = ws_ret_generico_solicitud_in.result_operacion       
--        LET v_r_ret_ws_sol_retiro_vent_afore.det_resultado           = ws_ret_generico_solicitud_in.det_resultado          
        LET v_r_ret_ws_sol_retiro_vent_afore.cve_afore               = ws_ret_generico_solicitud_in.arg0.cve_afore              
        LET v_r_ret_ws_sol_retiro_vent_afore.nom_trabajador          = ws_ret_generico_solicitud_in.arg0.nombreT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_paterno_trabajador   = ws_ret_generico_solicitud_in.arg0.paternoT
        LET v_r_ret_ws_sol_retiro_vent_afore.ap_materno_trabajador   = ws_ret_generico_solicitud_in.arg0.maternoT
      
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

{
======================================================================
Nombre: fn_registra_respuesta
Fecha creacion: Febrero 15, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Deja evidencia de la respuesta de la notificación
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_registra_respuesta(p_codigo, p_mensaje)
DEFINE p_codigo        SMALLINT,
       p_mensaje       CHAR(100),
       v_resultado     SMALLINT 
             

    IF (p_codigo IS NOT NULL AND p_codigo <> 0) AND  
       (p_mensaje IS NOT NULL ) THEN  

       -- si la solicitud fue aceptada
        DISPLAY "Se deja evidencia de la respuesta ..."      
        LET v_resultado = 0

        INSERT INTO ret_ws_notifica_cta_clabe_resp (id_peticion, f_notifica, codigo, mensaje)
             VALUES (g_id_peticion, CURRENT YEAR TO SECOND, p_codigo, p_mensaje);
    ELSE 
        LET v_resultado = 1
    END IF 
    
    RETURN v_resultado
END FUNCTION 