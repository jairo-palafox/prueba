--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
################################################################################
#PROYECTO        => SAFRE VIVIENDA                                             #
#PROPIETARIO     => INFONAVIT                                                  #
-------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETWS36                                                    #
#OBJETIVO        => WS SOLICITUD DE MARCAJE DE LA CUENTA DE SOLO INFONAVIT     #
#FECHA CREACION  => SEPTIEMBRE 2020                                            #
################################################################################


IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 

# 
# USER GLOBALS VARIABLES
#

GLOBALS "RETG01.4gl"
GLOBALS
DEFINE serverURL     STRING -- URL del servidor
DEFINE v_pantalla    SMALLINT


-- =======================================================
-- Record de recepción de valores en las variables
DEFINE gr_entrada    RECORD 
       nss           CHAR(11),
       rfc           STRING  ,
       caso_crm      CHAR(10), -- caso adai
       cuenta_clabe  CHAR(18),   -------     *************************
       ind_marca     SMALLINT, -- indicador de marca/desmarca/aprobacion/rechazo
       cod_rechazo   SMALLINT, -- codigo de rechazo en caso de desmarca y rechazo
       grupo         SMALLINT, -- Indica el grupo que se marcará
       medio_entrega SMALLINT, -- Medio por el cual se solicita la marca
       usuario       CHAR(20) -- Usuario que dío de alta el trámite
       END RECORD

-- =======================================================
-- Record de envío de variables de respuesta

DEFINE gr_salida      RECORD
       nss            STRING,
       est_marca      STRING,
       con_retiro     STRING,
       caso_crm       STRING,
       cod_rechazo    STRING,
       des_rechazo    CHAR(100),
       saldo_aivs     STRING,
       monto_pesos    STRING
       END RECORD
       
       

DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws

-- =======================================================
-- constantes para las respuesta y los mensajes que se escribirán en el log
CONSTANT g_res_procesada                    SMALLINT = 0  ,
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

CONSTANT C_MODALIDAD_RETIRO SMALLINT = 1
CONSTANT C_MARCA_SI         SMALLINT = 801

CONSTANT C_INSERTAR         SMALLINT = 1
CONSTANT C_DESMARCAR        SMALLINT = 2
CONSTANT C_ELIMINAR         SMALLINT = 3

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN

    DEFINE v_resultado        INTEGER, -- recibe el resultado de la ejecucion del servicio 
           v_ruta_log         STRING,
           v_cadena           STRING,
           v_ruta_ejecutable  VARCHAR(40)
      
    DEFER INTERRUPT

    -- se obtiene la ruta ejecutable
    SELECT ruta_bin
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
    
    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS36."
    LET v_cadena   = TODAY USING "yyyymmdd"
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT HOUR TO HOUR
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT MINUTE TO MINUTE
    LET v_ruta_log = v_ruta_log || v_cadena
    LET v_cadena   = CURRENT SECOND TO SECOND
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
    -- se inicia el log del programa
    IF fgl_getenv("FGLSQlDEBUG") THEN
        DISPLAY "Inicia log: C:\\tmp\\RETWS35.log"
        CALL STARTLOG("C:\\tmp\\RETWS36.log")
    ELSE
        DISPLAY "Inicia log: ",v_ruta_log
        CALL STARTLOG(v_ruta_log)
    END IF 
DISPLAY "Mensaje de prueba"
    CALL errorlog("mensaje de prueba")
  
    #
    # Check arguments
    #
    IF num_args() = 2 AND arg_val(1) = "-W" THEN
       LET serverURL = arg_val(2)
       CALL fn_crea_servicio_marcaje_SI(TRUE)
       EXIT PROGRAM
    ELSE 
       IF num_args() = 2 AND arg_val(1) = "-S" THEN
          LET v_pantalla = TRUE
          CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
          CLOSE WINDOW SCREEN
          
          -- se abre la ventana monitor del servidor (en consola)
       -- OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Solcitud de marcaje Ley 73") --, STYLE="naked")
          OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Solcitud Solo Infonavit")
          --display_status("Retiro Service Startup")
       ELSE
          IF num_args() <> 0 THEN
            CALL exitHelp()
            EXIT PROGRAM
          END IF
       END IF
    END IF

    CALL ERRORLOG("Invoca creacion de servicio de solicitud de marcaje Solo Infonavit")

    -- se crea el servicio    
    CALL fn_crea_servicio_marcaje_SI(FALSE)

    -- se inicia el servidor
    DISPLAY "Iniciando servidor de Marcaje para retiro Solo Infonavit..."

    -- se inicia el motor de WS
    CALL com.WebServiceEngine.Start()

    DISPLAY "Servidor en escucha"

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

#==============================================================================#
# Nombre  : fn_crea_servicio_marcaje_SI                                        #
# Creacion: Septiembre 2020                                                    #
# Autor   : Isai Jimenez Rojas, Omnisys                                        #
# Objetivo: Crea el servicio web para solicitud de marcaje solo infonavit      #
#==============================================================================#
FUNCTION fn_crea_servicio_marcaje_SI(generaWSDL)

    DEFINE generaWSDL           SMALLINT,
           serv                 com.WebService,       # WebService
           op                   com.WebOperation,     # Operation of a WebService
           v_service_NameSpace  STRING, -- namespace del servicio       
           v_respuesta          INTEGER
       
    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://www.infonavit.org.mx/"

    TRY
       -- =============================
       -- se crea el servicio
       LET serv = com.WebService.CreateWebService("retiroMarcaSI", v_service_NameSpace)
       
       -- =============================
       -- Publicacion de las funciones
       -- =============================

       LET op = com.WebOperation.CreateDOCStyle("fn_marcaje_SI","fn_marcaje_SI",gr_entrada,gr_salida)

       CALL serv.publishOperation(op, "fn_marcaje_SI")
       
       -- si se hace generacion del WSDL
       IF ( generaWSDL ) THEN
          -- Generar el WSDL
          LET v_respuesta = serv.saveWSDL(serverURL)
          
          -- si se genero el WSDL sin errores
          IF ( v_respuesta = 0 ) THEN
             DISPLAY "WSDL creado exitosamente"
          ELSE
             DISPLAY "ERROR: No se pudo crear el WSDL"
          END IF
       ELSE
          -- =========================
          -- REgistro del servicio
          CALL com.WebServiceEngine.RegisterService(serv)  
          --display_status("Retiro 72-92 Service registrado")
          CALL ERRORLOG("Se registro el servicio retiroMarcaSI")
       END IF
      
    CATCH -- en caso de error
       DISPLAY("No se pudo crear el servicio 'Solicitud de marcaje Ley 73':" || STATUS)
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

#==============================================================================#
# Nombre  : fn_marcaje_SI                                                      #
# Creacion: Septiembre 2020                                                    #
# Autor   : Isai Jimenez Rojas, Omnisys                                        #
# Objetivo: Solicita que se marque la cuenta por tipo de retiro                #
#==============================================================================#
FUNCTION fn_marcaje_SI()

    DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, --Identificador del derechohabiente
           v_error_en_datos     SMALLINT -- booleana que indica que hubo error en datos

    -- se verifica si se esta solicitando un eco
    IF ( UPSHIFT(gr_entrada.nss CLIPPED) = "ECO" ) THEN
       LET gr_salida.nss = "ECO"
       -- se devuelve ECO indicando que el servicio esta respondiendo correctamente
       CALL fn_respuesta_wsmarcaje(1, 1, 0, "ECO", 0, 0, 0)
    ELSE
       -- se procesa la solicitud
       -- Asgina valores de entrada
       LET gr_salida.nss = gr_entrada.nss
    
       -- se crea el registro de la peticion de solicitud de ws de marca
       CALL fn_registra_peticion_marca(0, gr_entrada.nss, gr_entrada.caso_crm, gr_entrada.cuenta_clabe, 0, 0)
            RETURNING g_id_peticion
       
       -- se verifica que se hayan recibido los datos
       IF ( gr_entrada.nss           IS NULL AND
          --gr_entrada.cuenta_clabe  IS NULL AND   Comentado en conferencia
           (gr_entrada.medio_entrega IS NULL OR gr_entrada.medio_entrega = 0)) THEN
          -- es un error
          CALL fn_respuesta_wsmarcaje(1,0,gi_datos_incompletos,gr_entrada.caso_crm,0,0,0)

          RETURN
       ELSE
          -- se verifica que el arreglo para marcar haya llegado correcto
          LET v_error_en_datos = FALSE
       
          CALL fn_registra_detalle_peticion_marca(g_id_peticion,
                                                  gr_entrada.ind_marca,
                                                  gr_entrada.cod_rechazo,
                                                  0, 0, 0, 0)
       
          -- Valida modalidad de retiro
          IF ( gr_entrada.ind_marca IS NULL ) THEN 
             -- se marca que hubo error en la validacion de datos
             LET v_error_en_datos = TRUE
          END IF

          IF( gr_entrada.grupo         = 1 AND
             (gr_entrada.medio_entrega = 1 OR gr_entrada.medio_entrega = 2) AND 
              gr_entrada.ind_marca = 1)    OR
             (gr_entrada.medio_entrega = 3 AND gr_entrada.ind_marca = 3) THEN
               
             IF ( NOT fn_verifica_estructura_clabe(gr_entrada.cuenta_clabe) )  THEN
                LET v_error_en_datos = TRUE
                CALL fn_respuesta_wsmarcaje(1, 0, gi_esctructura_cta_clabe_incorrecta, gr_entrada.caso_crm, 0, 0, 0)
                RETURN
             END IF
          END IF

       END IF
       
       -- Obtiene el identificador del derechohabiente
       LET v_id_derechohabiente = fn_obtiene_id_derechohabiente(gr_entrada.nss)
       
       -- Obtiene la longitud del arreglo que llega
       CALL fn_aplica_marca_modalidad(gr_entrada.nss,
                                      gr_entrada.caso_crm,
                                      v_id_derechohabiente, 
                                      gr_entrada.ind_marca,
                                      gr_entrada.grupo,
                                      gr_entrada.medio_entrega)
    END IF

END FUNCTION

#==============================================================================#
# Nombre  : fn_aplica_marca_modalidad                                          #
# Creacion: Septiembre 2020                                                    #
# Modificó: Isai Jimenez Rojas, Omnisys                                        #
# Objetivo: Aplica el marcaje para las demás modalidades                       #
#==============================================================================#
FUNCTION fn_aplica_marca_modalidad(p_nss               ,
                                   p_caso_crm          ,
                                   p_id_derechohabiente,
                                   p_ind_marca         ,
                                   p_grupo             ,
                                   p_medio_entrega)

    DEFINE p_nss                    CHAR(11),
           p_caso_crm               CHAR(10),
           p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente,
           p_ind_marca              SMALLINT,
           p_grupo                  SMALLINT,
           p_medio_entrega          SMALLINT
           
    DEFINE v_marca_entra            SMALLINT,
           v_n_referencia           INTEGER ,
           v_folio                  DECIMAL(9,0),
           v_estado_marca           SMALLINT,
           v_codigo_rechazo         SMALLINT,--Variable de rechazo del store
           v_marca_causa            SMALLINT,
           v_fecha_causa            DATE,
           v_usuario                CHAR(20),
           v_proceso_cod            SMALLINT,
           v_estatus_marca          SMALLINT,
           v_respuesta_marcaje      SMALLINT,
           v_res_actualizacion      SMALLINT ,
           v_existe_solicitud       SMALLINT,
           v_cod_inconsistencia     SMALLINT,       -- Variable para indicar la causa de la inconsistencia
           v_cod_rechazo            SMALLINT,       -- Codigo de rechazo para la maraca en procesar
           v_diagnostico            SMALLINT,       -- diagnostico de la consulta del saldo en la afore
           v_estatus                SMALLINT,       -- estatus de la cuenta individual segun la consulta del saldo en la Afore
           v_aivs_viv92             DECIMAL(24,6),  -- saldo AIVs de viv92
           v_aivs_viv97             DECIMAL(24,6),  -- saldo AIVs de viv97
           v_pesos_viv92            DECIMAL(22,2),  -- saldo pesos de viv92
           v_pesos_viv97            DECIMAL(22,2),  -- saldo pesos de viv97
           v_saldo_total            DECIMAL(24,6),  -- saldo total trabajo
           v_saldo_aivs_total       DECIMAL(24,6),  -- Saldo total en aivs
           v_saldo_tesofe           DECIMAL(22,2),  -- Saldo en pesos de la Subcueta de la Tesofe
           v_saldo_paso             DECIMAL(22,2),  -- Variable de paso para trabajar los saldos
           v_precio_primero_mes     DECIMAL(10,6),  -- Precio del primer día natural del mes
           v_resultado              SMALLINT,       -- resultado del llamado a la funcion
           v_marca_credito          SMALLINT,
           v_val_clabe              SMALLINT
    DEFINE v_caso_crm               CHAR(10)

    DEFINE ls_modalidad             SMALLINT
    DEFINE ls_inserta               SMALLINT
    DEFINE ls_actualiza             SMALLINT
    DEFINE ls_elimina               SMALLINT

    ----------------------------------------------------------------------------

    -- si el id_derechohabiente viene nulo, no se encontro
    IF ( p_id_derechohabiente IS NULL ) THEN
       -- No se encontro el NSS en la base de datos
       LET v_estatus_marca = gi_estatus_marca_no_exitoso
       
       -- no existe en nss
       LET v_cod_inconsistencia = gi_nss_rfc_no_existe
       
       CALL fn_respuesta_wsmarcaje(v_estatus_marca,NULL,v_cod_inconsistencia, p_caso_crm, 0, 0, 0)

       RETURN
    END IF

    LET v_aivs_viv92       = 0
    LET v_aivs_viv97       = 0
    LET v_pesos_viv92      = 0
    LET v_pesos_viv97      = 0
    LET v_saldo_tesofe     = 0
    LET v_saldo_paso       = 0
    LET v_saldo_total      = 0
    LET v_saldo_aivs_total = 0
    LET v_caso_crm         = ""
   
    --Retiro Solo Infonavit
    LET v_marca_entra      = C_MARCA_SI
    LET v_proceso_cod      = g_proceso_cod_con_ret_solo_infonavit --1508
    
    --VVariables para controlar comportamiento
    LET ls_modalidad       = 1   --Solo infonavit
    LET ls_inserta         = 1   --codigo de insersion
    LET ls_actualiza       = 2   --codigo de eliminacion
    LET ls_elimina         = 3   --codigo de eliminacion
    
    --Asigna variables para la ejecución del store
    LET v_folio            = 0
    LET v_estado_marca     = "0"
    LET v_codigo_rechazo   = 0
    LET v_marca_causa      = "0"
    LET v_fecha_causa      = NULL


    IF  gr_entrada.usuario IS NOT NULL THEN
       LET v_usuario        = gr_entrada.usuario
    ELSE 
       LET v_usuario        = "safreviv"
    END IF 

  
    --Valida la acción de marcaje que se deberá hacer sobre el registro
    CASE p_ind_marca

       -------------------------------------------------------------------------
       -- SOLICIYUD DE MARCAJE
       -------------------------------------------------------------------------
       WHEN 1
       
          --Consulta si existe  otra solicitud para el derechohabiente 
          CALL fn_verifica_solicitud_generico(p_id_derechohabiente,ls_modalidad,1)
                RETURNING v_existe_solicitud, v_n_referencia
    
          -- Valida que no exista otra solicitud 
          IF ( NOT v_existe_solicitud ) THEN
             -- Crea Caso en CRM 
             CALL fn_crea_caso(p_nss, p_medio_entrega) RETURNING v_resultado, v_caso_crm
             
             IF v_resultado <> 0 THEN --- No se pudo crear el caso
                -- se indica que no se pudo marcar
                LET v_estatus_marca      = gi_estatus_marca_no_exitoso
                LET v_cod_inconsistencia = v_resultado
             ELSE 
                --Asigna estatus de la marca
                LET v_estatus_marca = 8
    
                --Se obtiene el número de solicitud
                SELECT seq_ret_solicitud.NEXTVAL
                INTO   v_n_referencia
                FROM   systables 
                WHERE  tabid = 1
    
                IF gr_entrada.grupo = 1 AND gr_entrada.medio_entrega = 2 THEN 
                   -- (gr_entrada.medio_entrega = 1 OR  --- Se quita el medio de entrega 1 (Tableta) la tableta genera solicita el número de caso
                   LET gr_entrada.caso_crm = v_caso_crm
                   LET p_caso_crm          = v_caso_crm
                END IF 
                                        
                -- Actualiza tabla de retiro genérico   
                CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,C_MODALIDAD_RETIRO,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,ls_inserta)
                     RETURNING v_res_actualizacion
    
                -- Verifica que no haya existido un error en la actualización de la tabla
                IF ( v_res_actualizacion ) THEN
                   --- Deja huella del medio de entrega
                   CALL fn_registra_medio_entrega(v_n_referencia, p_grupo, p_medio_entrega);
                   ---Se ejecuta la función de marcaje para la nueva solicitud
                   CALL fn_ret_generico_marca_cuenta(p_id_derechohabiente,
                                                     v_marca_entra       ,
                                                     v_n_referencia      ,
                                                     v_folio             ,
                                                     v_estado_marca      ,
                                                     v_codigo_rechazo    ,
                                                     v_marca_causa       ,
                                                     v_fecha_causa       ,
                                                     v_usuario           ,
                                                     v_proceso_cod )
                         RETURNING v_respuesta_marcaje
                                           
                   -- Valida que la respuesta sea correcta
                   IF ( v_respuesta_marcaje = 0 ) THEN
                      LET v_cod_inconsistencia = 0
                      
                      SELECT NVL(precio_fondo,0)
                      INTO   v_precio_primero_mes
                      FROM   glo_valor_fondo
                      WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1
                                            FROM   (SELECT LIMIT 1 1 
                                                    FROM   systables))
                      AND    fondo = 11

                      -- Se ejecutó correctamente el store, se indica que se marcó la cuenta
                      LET v_estatus_marca = gi_estatus_marca_existoso

                      --CALL fn_consulta_saldo_vivienda_afore(p_nss, 44)   --44=consuta y marca
                      --    RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
                      --CALL fn_calcula_saldo_ley73(p_nss, 4 , TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
                      --CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY)  RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
                      --CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_saldo_paso, v_saldo_tesofe
                      
                      --RECUPERA SALDOS
                      CALL fn_calcula_saldo_SI(p_nss, 44, TODAY)  RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
                      
                      LET v_saldo_total = (v_aivs_viv97 * v_precio_primero_mes)

                      DISPLAY " Saldo Total: ", v_saldo_total

                      --CHECAR IF (v_diagnostico = 127) OR (v_diagnostico = 101 AND (v_estatus = 101 OR v_estatus = 201 OR v_estatus = 442) AND v_saldo_total > 0) THEN
                      --CHECAR    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 1, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                      --CHECAR                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- Se deberá reenviar la solicitud de marca 
                      --CHECAR    
                      --CHECAR    --consulta el ultimo saldo devuelto por procesar
                      --CHECAR    CALL fn_recupera_saldo_consulta(p_nss)
                      --CHECAR         RETURNING v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
                      --CHECAR END IF
                      --CHECAR 
                      --CHECAR IF (v_diagnostico = 101 AND (v_estatus <> 101 AND v_estatus <> 201 AND v_estatus <> 442))  THEN 
                      --CHECAR    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 2, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                      --CHECAR                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- La solicitud no se pudo marcar porque esta marcada en otro proceso
                      --CHECAR    LET v_estatus_marca = gi_estatus_marca_no_exitoso
                      --CHECAR    LET v_cod_inconsistencia = v_cod_rechazo
                      --CHECAR 
                      --CHECAR    CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, C_MODALIDAD_RETIRO, v_n_referencia,
                      --CHECAR                                      v_folio, v_estatus_marca, v_cod_rechazo, ls_elimina)
                      --CHECAR         RETURNING v_res_actualizacion
                      --CHECAR    CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, C_MARCA_SI,
                      --CHECAR                  v_n_referencia, C_MARCA_SI,
                      --CHECAR                  "safreviv", g_proceso_cod_con_ret_solo_infonavit)
                      --CHECAR END IF
                      --CHECAR 
                      --CHECAR IF (v_diagnostico = 101 AND (v_estatus = 101 OR v_estatus = 201 OR v_estatus = 442) AND v_saldo_total <= 0) THEN 
                      --CHECAR    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 2, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                      --CHECAR                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- La solicitud se marca, pero tiene saldo cero
                      --CHECAR    LET v_estatus_marca = gi_estatus_marca_no_exitoso
                      --CHECAR    LET v_cod_inconsistencia = gi_sin_saldo
                      --CHECAR END IF
                      
                      LET v_saldo_total         = v_aivs_viv97 * v_precio_primero_mes
                      LET v_saldo_aivs_total    = v_aivs_viv97
                      LET gr_salida.saldo_aivs  = v_aivs_viv97
                      LET gr_salida.monto_pesos = v_aivs_viv97 * v_precio_primero_mes

                   ELSE
                      -- Sucedió un error al ejecutar la función de marcado
                      -- Se elimina el registro creado
                      CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, C_MODALIDAD_RETIRO, v_n_referencia,
                                                   v_folio, v_estatus_marca, v_codigo_rechazo, ls_elimina)
                         RETURNING v_res_actualizacion
    
                      -- se indica que no se pudo marcar
                      LET v_estatus_marca = gi_estatus_marca_no_exitoso
    
                      -- se verifica si es por inconvivencia
                      IF ( v_respuesta_marcaje > 0 ) THEN
                         LET v_cod_inconsistencia = gi_error_marca_no_convive
                      ELSE
                         -- Error al marcar/desmarcar la cuenta
                         LET v_cod_inconsistencia = gi_error_interno_marca
                      END IF
    
                   END IF
                ELSE
                   -- Existió un error en la integración de retiro genérico
                   LET v_estatus_marca = gi_estatus_marca_no_exitoso
    
                   -- error al generar la solicitud en ret_solicitud_generico
                   LET v_cod_inconsistencia = gi_error_marca_generar_solicitud
    
                END IF
             END IF
          ELSE
             LET v_estatus_marca = gi_estatus_marca_existoso
    
             --Se indica código de rechazo 0, para mostrar que la ejecución fué correcta
             LET v_cod_inconsistencia = 0

          END IF
       
       -------------------------------------------------------------------------
       --          DESMARCAR CUENTA
       -------------------------------------------------------------------------
       WHEN 2
          -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
          CALL fn_verifica_solicitud_generico(p_id_derechohabiente,C_MODALIDAD_RETIRO,C_DESMARCAR)  --desmarcar=2
               RETURNING v_existe_solicitud, v_n_referencia

          --Valida que no se haya solicitado una marcación antes de una desmarcación
          IF ( v_existe_solicitud ) THEN
             -- Asigna estatus de la marca
             LET v_estatus_marca = 100
             -- Actualiza tabla de retiro genérico   
             CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,C_MODALIDAD_RETIRO,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,ls_actualiza)
                  RETURNING v_res_actualizacion
                 
             IF ( v_res_actualizacion ) THEN 
                --Si la ejecución fué correcta se envía solicitud de desmarcado
                --Se ejecutó correctamente el store, se indica que se desmarco la cuenta
                LET v_estatus_marca = gi_estatus_marca_existoso
                --Actualiza codigo de rechazo
                LET v_cod_inconsistencia = 0
    
                IF p_grupo = 1 THEN
                   CALL fn_consulta_saldo_vivienda_afore(p_nss, 60)  -- Desmarca en Procesar   60=desmarca
                        RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
                   IF v_diagnostico = 127 THEN 
                       CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- Se deberá reenviar la solicitud de marca 
                       LET v_cod_inconsistencia = v_cod_rechazo
                   ELSE 
                       CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- Se deberá reenviar la solicitud de marca 
                   END IF
                END IF                
             ELSE
                --Existió un error en la integración de retiro genérico
                LET v_estatus_marca = gi_estatus_marca_no_exitoso
                --Se indica código de rechazo para mostrar que ocurrió un error al integrar información en 
                --ret_solicitud_generico
                LET v_cod_inconsistencia = gi_error_interno_marca
             END IF
          ELSE
             -- se verifica si hay una solicitud ya generada
             CALL fn_verifica_solicitud_generico(p_id_derechohabiente, C_MODALIDAD_RETIRO, 4)   --modalidad 1
                  RETURNING v_existe_solicitud, v_n_referencia
             -- Valida que no se haya solicitado una marcación antes de una desmarcación
             IF ( v_existe_solicitud ) THEN
                -- rechazo de solicitud
                LET v_estatus_marca = 100
                -- Actualiza tabla de retiro genérico   
                CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, C_MODALIDAD_RETIRO, v_n_referencia, 
                                                  v_folio, v_estatus_marca,
                                                  gr_entrada.cod_rechazo, ls_actualiza)
                     RETURNING v_res_actualizacion
                -- se rechazo correctamente la solicitud
                LET v_estatus_marca = gi_estatus_marca_existoso
                -- sin error
                LET v_cod_inconsistencia = 0
             ELSE
                -- se indica que no se pudo desmarcar
                LET v_estatus_marca = gi_estatus_marca_no_exitoso
                --Se envía código de rechazo indicando que no existe solicitud de marcado previa para poder desmarcar
                LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
             END IF
          END IF  

          LET gr_salida.saldo_aivs  = 0
          LET gr_salida.monto_pesos = 0
                    
       -------------------------------------------------------------------------
       --          APROBAR SOLICITUD
       -------------------------------------------------------------------------
       WHEN 3
          -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
          CALL fn_verifica_solicitud_generico(p_id_derechohabiente, 3, 3)
               RETURNING v_existe_solicitud, v_n_referencia
          
          -- Valida que no se haya solicitado una marcación antes de una desmarcación
          IF ( v_existe_solicitud ) THEN

             LET v_val_clabe = 0         -- Se solicito quitar la validación via correo para la integración con CRM
             LET v_estatus_marca = 15    -- Se dejará fluir la solicitud

             -- Actualiza tabla de retiro genérico   
             CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, C_MODALIDAD_RETIRO, v_n_referencia, 
                                             v_folio, v_estatus_marca, 0, ls_actualiza)
                  RETURNING v_res_actualizacion
    
             -- se aprobo correctamente la solicitud
             LET v_estatus_marca = gi_estatus_marca_existoso -- rechazo de solicitud
    
             -- sin error
             LET v_cod_inconsistencia = 0
          ELSE
             -- no se pudo aprobar la solicitud
             LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
    
             -- no existe una solicitud que aprobar
             LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
          END IF

          LET gr_salida.saldo_aivs  = 0
          LET gr_salida.monto_pesos = 0

       -------------------------------------------------------------------------
       --          RECHAZAR SOLICITUD
       -------------------------------------------------------------------------
       WHEN 4
          -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
          CALL fn_verifica_solicitud_generico(p_id_derechohabiente, C_MODALIDAD_RETIRO, 4)
               RETURNING v_existe_solicitud, v_n_referencia
          
          --Valida que no se haya solicitado una marcación antes de una desmarcación
          IF ( v_existe_solicitud ) THEN
             -- rechazo de solicitud
             LET v_estatus_marca = 100
          
             -- Actualiza tabla de retiro genérico   
             CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, C_MODALIDAD_RETIRO, v_n_referencia, 
                                               v_folio, v_estatus_marca,
                                               gr_entrada.cod_rechazo, ls_actualiza)
                  RETURNING v_res_actualizacion

             IF ( v_res_actualizacion ) THEN 
                --Si la ejecución fué correcta se envía solicitud de desmarcado
                LET v_estatus_marca = gi_estatus_marca_existoso
                --Actualiza codigo de rechazo
                LET v_cod_inconsistencia = 0
    
                IF p_grupo = 1 THEN
                   --desmarca y consulta
                   CALL fn_consulta_saldo_vivienda_afore(p_nss, 60)  -- Desmarca en Procesar
                        RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo

                   IF v_diagnostico = 127 THEN 
                       CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- Se deberá reenviar la solicitud de marca 
                       LET v_cod_inconsistencia = v_cod_rechazo
                   ELSE 
                       CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_crm, 1) -- Se deberá reenviar la solicitud de marca 
                   END IF
                END IF   
                -- se rechazo correctamente la solicitud
                LET v_estatus_marca = gi_estatus_marca_existoso
             
                -- sin error
                LET v_cod_inconsistencia = 0             
             ELSE
                --Existió un error en la integración de retiro genérico
                LET v_estatus_marca = gi_estatus_marca_no_exitoso
                --Se indica código de rechazo para mostrar que ocurrió un error al integrar información en 
                --ret_solicitud_generico
                LET v_cod_inconsistencia = gi_error_interno_marca
             END IF
          ELSE
             -- no se pudo aprobar la solicitud
             LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
          
             -- no existe una solicitud que rechazar
             LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
          END IF

          LET gr_salida.saldo_aivs  = 0
          LET gr_salida.monto_pesos = 0

       OTHERWISE
          --Se recibió un indicador de marcaje desconocido
          CALL ERRORLOG("Se recibió un indicador de marcado desconocido")
          -- marcaje no exitoso
          LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
          
          -- indicador de marca no reconocido
          LET v_cod_inconsistencia = gi_indicador_marca_invalido
          LET gr_salida.saldo_aivs  = 0
          LET gr_salida.monto_pesos = 0
    END CASE
    
    --Invoca función de respuesta del WS
    CALL fn_respuesta_wsmarcaje(v_estatus_marca,v_n_referencia,v_cod_inconsistencia, p_caso_crm, v_saldo_aivs_total, v_saldo_total, 0)

END FUNCTION

#==============================================================================#
#Nombre  : fn_obtiene_id_derechohabiente                                       #
#Creacion: Septiembre 25, 2013                                                 #
#Autor   : Isai Jimenez Rojas, Omnisys                                         #
#Objetivo: Obtiene el id_derechohabiente de la tabla de afi_derechohabiente    #
#==============================================================================#

FUNCTION fn_obtiene_id_derechohabiente(p_nss)

    DEFINE p_nss                   CHAR(11),
           v_sql                   STRING,
           v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente
       
    --Se arma el query de la consulta
    LET v_sql = "\n SELECT id_derechohabiente  ",
                "\n FROM   afi_derechohabiente ",
                "\n WHERE  nss               = '", p_nss, "'",
                "\n AND    ind_estado_cuenta = 0 "   -- cuenta Activa

    PREPARE stm_derechohabiente FROM v_sql
    EXECUTE stm_derechohabiente INTO v_id_derechohabiente       

    --Envía el id_derechohabiente consultado
    RETURN v_id_derechohabiente 

END FUNCTION

#==============================================================================#
#Nombre  : fn_actualiza_retiro_generico                                        #
#Creacion: Septiembre 2020                                                     #
#Modifico: Isai Jimenez Rojas, Omnisys                                         #
#Objetivo: Ejecuta la actualización de la respuesta del marcaje en la tabla    #
#          de retiro genérico                                                  #
#==============================================================================#
FUNCTION fn_actualiza_retiro_generico(p_id_derechohabiente,
                                      p_nss               ,
                                      p_modalidad_retiro  ,
                                      p_id_solicitud      ,
                                      p_folio             ,
                                      p_estado_solicitud  ,
                                      p_cod_rechazo       ,
                                      p_tipo_actualizacion)
                                      
    DEFINE p_id_derechohabiente       DECIMAL(9,0),
           p_nss                      CHAR(11),
           p_rfc                      CHAR(13),
           p_modalidad_retiro         SMALLINT,
           p_id_solicitud             DECIMAL(9,0),
           p_folio                    DECIMAL(9,0),
           p_estado_solicitud         SMALLINT,
           p_cod_rechazo              SMALLINT,
           p_tipo_actualizacion       SMALLINT --Variable que recibe el tipo de actualización(INSERT,UPDATE,DELETE)
    
    DEFINE v_sql                      STRING,
           v_respuesta                SMALLINT,
           v_es_spei                  SMALLINT,
           v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud
   
    -- Valida el tipo de actualización 1-Inserción, 2-Actualización

    CASE p_tipo_actualizacion

       -------------------------------------------------------------------------
       -- INSERTA
       -------------------------------------------------------------------------
       WHEN 1
       
          -- Arma el query INSERCIÓN
          LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
          LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
          LET v_r_ret_solicitud_generico.nss                    = p_nss
          LET v_r_ret_solicitud_generico.rfc                    = p_rfc
          LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
          LET v_r_ret_solicitud_generico.folio                  = p_folio
          LET v_r_ret_solicitud_generico.caso_adai              = gr_entrada.caso_crm
          LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
          LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
          LET v_r_ret_solicitud_generico.folio_restitucion      = 0
          LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
          LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
          LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
          LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
          LET v_r_ret_solicitud_generico.estado_solicitud       = p_estado_solicitud
          LET v_r_ret_solicitud_generico.cod_rechazo            = p_cod_rechazo

          -- se inserta el registro
          INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )
                                             
          -- Valida que la ejecución del insert haya sido correcta
          IF ( SQLCA.SQLCODE < 0 ) THEN
             -- Asigna respuesta negativa
             LET v_respuesta = FALSE
             CALL ERRORLOG("Se produjo un error al insertar el registro en retiro genérico")
          ELSE
             -- Asigna respuesta afirmativa
             LET v_respuesta = TRUE
             CALL ERRORLOG("Se ha insertado con éxito el registro en retiro genérico")
             
             IF gr_entrada.grupo = 1 AND (gr_entrada.medio_entrega = 1 OR gr_entrada.medio_entrega = 2) THEN 
                --- Se inserta en ret_pago_spei por cambio en la tabla ret_ws_peticion_marca
                INSERT INTO ret_pago_spei VALUES (p_id_solicitud,1,1,0,0,gr_entrada.cuenta_clabe);
             END IF 
          END IF

       -------------------------------------------------------------------------
       -- ACTUALIZA
       -------------------------------------------------------------------------
       WHEN 2 
       
          -- si no llego el codigo de rechazo, se define el generico
          IF ( p_cod_rechazo IS NULL ) THEN
             LET p_cod_rechazo = 54 -- rechazo generico. CAMBIAR POR CONSTANTE GLOBAL
          END IF

          -- Arma el query ACTUALIZACION
          LET v_sql = "\nUPDATE ret_solicitud_generico",
                      "\nSET    estado_solicitud   =  ", p_estado_solicitud, ",",
                      "\n       cod_rechazo        =  ", p_cod_rechazo,
                      "\nWHERE  id_derechohabiente =  ", p_id_derechohabiente,
                      "\nAND    modalidad_retiro   =  ", p_modalidad_retiro,
                      "\nAND    id_solicitud       =  ", p_id_solicitud
                       
          PREPARE stm_update_retiro FROM v_sql
          EXECUTE stm_update_retiro 
                                            
          -- Valida que la ejecución del insert haya sido correcta
          IF ( SQLCA.SQLCODE < 0 ) THEN
              -- Asigna respuesta negativa
              LET v_respuesta = FALSE
              CALL ERRORLOG("Se produjo un error al actualizar el registro en retiro genérico")
          ELSE
             -- Asigna respuesta afirmativa
             LET v_respuesta = TRUE
             CALL ERRORLOG("Se ha actualizado con éxito el registro en retiro genérico")
             
             -- se actualiza la tabla de historicos con el estado de solicitud y cogido de rechazo segun la modalidad
             
             --Solo llegará modalida 1 (se deja para posible apertura en la funcionalidad FUV)
             CASE p_modalidad_retiro
                 WHEN 1 --Solo Infonavit
                    --UPDATE ret_ley73_generico     --No se localizó un equivalente para solo infonavit
                    --SET    estado_solicitud = p_estado_solicitud,
                    --       cod_rechazo      = p_cod_rechazo
                    --WHERE  id_solicitud     = p_id_solicitud
                    
                    -- si se trata de un rechazo, se desmarca
                    IF ( p_estado_solicitud = 100 ) THEN
                       CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 
                                                            C_MARCA_SI,
                                                            p_id_solicitud, 
                                                            C_MARCA_SI,
                                                            "safreviv", 
                                                            g_proceso_cod_con_ret_solo_infonavit)
                    END IF
                 
                    IF (p_estado_solicitud = 15 AND gr_entrada.medio_entrega = 3) THEN -- se actualiza la cuenta clabe
                       LET v_es_spei = 0
                 
                       SELECT COUNT(*) 
                       INTO   v_es_spei 
                       FROM   ret_pago_spei 
                       WHERE  id_solicitud = p_id_solicitud
                       AND    consec_beneficiario = 1
                 
                       IF v_es_spei = 1 THEN 
                          UPDATE ret_pago_spei
                          SET    cuenta_clabe = gr_entrada.cuenta_clabe
                          WHERE  id_solicitud = p_id_solicitud
                          AND    consec_beneficiario = 1
                       ELSE 
                          SELECT COUNT(*) 
                          INTO   v_es_spei 
                          FROM   ret_pago_siaf 
                          WHERE  id_solicitud = p_id_solicitud 
                          AND    consec_beneficiario = 1
                 
                          IF v_es_spei = 1 THEN 
                             UPDATE ret_pago_siaf
                             SET    cuenta_clabe = gr_entrada.cuenta_clabe
                             WHERE  id_solicitud = p_id_solicitud
                             AND    consec_beneficiario = 1
                          END IF 
                       END IF 
                    END IF

               --WHEN 2 -- fondo de ahorro                                                               --
               --   UPDATE ret_fondo_ahorro_generico                                                     --
               --   SET    estado_solicitud = p_estado_solicitud,                                        --
               --          cod_rechazo      = p_cod_rechazo                                              --
               --   WHERE  id_solicitud     = p_id_solicitud                                             --
               --                                                                                        --
               --   -- si se trata de un rechazo, se desmarca                                            --
               --   IF ( p_estado_solicitud = 100 ) THEN                                                 --
               --      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 802,                   --
               --                                           p_id_solicitud, 802,                         --
               --                                           "safreviv", g_proceso_cod_ret_fondo_ahorro)  --
               --   END IF                                                                               --
               --
               --WHEN 3 -- ley 73
               --   UPDATE ret_ley73_generico
               --   SET    estado_solicitud = p_estado_solicitud,
               --          cod_rechazo      = p_cod_rechazo
               --   WHERE  id_solicitud     = p_id_solicitud
               --   
               --   -- si se trata de un rechazo, se desmarca
               --   IF ( p_estado_solicitud = 100 ) THEN
               --      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 803,
               --                                           p_id_solicitud, 803,
               --                                           "safreviv", g_proceso_cod_con_ret_solo_infonavit)
               --   END IF
               --
               --   IF (p_estado_solicitud = 15 AND gr_entrada.medio_entrega = 3) THEN -- se actualiza la cuenta clabe
               --      LET v_es_spei = 0
               --
               --      SELECT COUNT(*) 
               --      INTO   v_es_spei 
               --      FROM   ret_pago_spei 
               --      WHERE  id_solicitud = p_id_solicitud
               --      AND    consec_beneficiario = 1
               --
               --      IF v_es_spei = 1 THEN 
               --         UPDATE ret_pago_spei
               --         SET    cuenta_clabe = gr_entrada.cuenta_clabe
               --         WHERE  id_solicitud = p_id_solicitud
               --         AND    consec_beneficiario = 1
               --      ELSE 
               --         SELECT COUNT(*) 
               --         INTO   v_es_spei 
               --         FROM   ret_pago_siaf 
               --         WHERE  id_solicitud = p_id_solicitud 
               --         AND    consec_beneficiario = 1
               --
               --         IF v_es_spei = 1 THEN 
               --            UPDATE ret_pago_siaf
               --            SET    cuenta_clabe = gr_entrada.cuenta_clabe
               --            WHERE  id_solicitud = p_id_solicitud
               --            AND    consec_beneficiario = 1
               --         END IF 
               --      END IF 
               --   END IF
               --
               --WHEN 9 -- amortizaciones excedentes
               --   UPDATE ret_amort_excedente
               --   SET    estado_solicitud = p_estado_solicitud,
               --          cod_rechazo      = p_cod_rechazo
               --   WHERE  id_solicitud     = p_id_solicitud
               --   
               --   -- si se trata de un rechazo, se desmarca
               --   IF ( p_estado_solicitud = 100 ) THEN
               --      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 810, 
               --                                           p_id_solicitud, 810,
               --                                           "safreviv", g_proceso_cod_ret_amort_excedentes)
               --   END IF
               --
               --WHEN 10 -- aportaciones voluntarias
               --   UPDATE ret_voluntaria
               --   SET    estado_solicitud = p_estado_solicitud,
               --          cod_rechazo      = p_cod_rechazo
               --   WHERE  id_solicitud     = p_id_solicitud
               --   
               --   -- si se trata de un rechazo, se desmarca
               --   IF ( p_estado_solicitud = 100 ) THEN
               --      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 809, 
               --                                           p_id_solicitud, 809,
               --                                           "safreviv", g_proceso_cod_ret_aport_voluntarias)
               --   END IF
             END CASE
          END IF

       -------------------------------------------------------------------------
       -- ELIMINA
       -------------------------------------------------------------------------
       WHEN 3
       
          -- Arma el query ELIMINACIÓN
          LET v_sql = "\n DELETE FROM ret_solicitud_generico",
                      "\n WHERE  id_derechohabiente = ", p_id_derechohabiente,
                      "\n AND    modalidad_retiro   = ", p_modalidad_retiro,
                      "\n AND    id_solicitud       = ", p_id_solicitud
                       
          PREPARE stm_delete_retiro FROM v_sql
          EXECUTE stm_delete_retiro 
                
          --Valida que la ejecución del insert haya sido correcta
          IF ( SQLCA.SQLCODE < 0 ) THEN
             -- Asigna respuesta negativa
             LET v_respuesta=FALSE
             CALL ERRORLOG("Se ha producido un error al eliminar el registro en retiro genérico")
          ELSE
             -- Asigna respuesta afirmativa
             LET v_respuesta = TRUE
             CALL ERRORLOG("Se ha eliminado con éxito el registro en retiro genérico")
          END IF

    END CASE
    
    -- Regresa respuesta
    RETURN v_respuesta

END FUNCTION

#==============================================================================#
# Nombre  : fn_verifica_solicitud_generico                                     #
# Creacion: Septiembre 27, 2013                                                #
# Autor   : Isai Jimenez Rojas, Omnisys                                        #
# Objetivo: Verifica que no exista una solicitud en la tabla de retiro genérico#
#==============================================================================#
FUNCTION fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad_retiro,p_accion)

    DEFINE v_sql                STRING,
           p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
           p_modalidad_retiro   SMALLINT,
           p_accion             SMALLINT
    
    DEFINE v_ban_existe         SMALLINT,
           v_num_solicitud      DECIMAL(9,0)
       
    --Inicializa valores
    LET v_ban_existe= FALSE
 
    -- Valida el tipo de acción
    CASE p_accion

       WHEN 1 
          -- MARCAR
          LET v_sql = "\n SELECT id_solicitud         ",
                      "\n FROM ret_solicitud_generico ",
                      "\n WHERE id_derechohabiente =  ", p_id_derechohabiente,
                      "\n AND modalidad_retiro     =  ", p_modalidad_retiro,
                      "\n AND estado_solicitud IN (   ",
                      "\n 8, 10, 15, 50, 60, 70, 71,  ", -- precaptura, captura, aprobacion, preliq., liquid., enviada fico, conf. pago
                      "\n 90, 91, 209, 210, 211, 212, ", -- rch fico, rechazo banco, cancelacion CxP, 
                      "\n 213 )"                         -- restitucion

       WHEN 2
          -- DESMARCAR
          LET v_sql = "\n SELECT id_solicitud ",
                      "\n FROM ret_solicitud_generico",
                      "\n WHERE id_derechohabiente = ", p_id_derechohabiente,
                      "\n AND modalidad_retiro     = ", p_modalidad_retiro,
                      "\n AND estado_solicitud     IN (8,10) "
        
       WHEN 3
          -- APROBAR SOLICITUD
          LET v_sql = "\n SELECT id_solicitud ",
                      "\n FROM ret_solicitud_generico",
                      "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                      "\n AND modalidad_retiro     = ",p_modalidad_retiro,
                      "\n AND estado_solicitud     = 10"
                      
       WHEN 4 
          -- RECHAZAR LA SOLICITUD
          LET v_sql = "\n SELECT id_solicitud",
                      "\n FROM ret_solicitud_generico",
                      "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                      "\n AND modalidad_retiro     = ",p_modalidad_retiro

          IF gr_entrada.grupo = 1 AND gr_entrada.medio_entrega = 1 THEN 
             LET v_sql = v_sql CLIPPED,  "\n AND estado_solicitud     IN (8, 10) "  -- Se modifica por el 10 por los cambios que se hicieron en la tableta
          ELSE 
             LET v_sql = v_sql CLIPPED,  "\n AND estado_solicitud     = 10 "
          END IF 

    END CASE
    
    DISPLAY v_sql   --debug
    
    PREPARE stm_existe_solicitud FROM v_sql
    DECLARE cur_existe_solicitud CURSOR FOR stm_existe_solicitud

    -- Itera resultados
    FOREACH cur_existe_solicitud INTO v_num_solicitud
       --Asigna valor
       IF ( v_num_solicitud IS NOT NULL ) THEN
          LET v_ban_existe = TRUE 
       END IF
    END FOREACH
  
    DISPLAY "Solicitud encontrada: "
    DISPLAY "Bandera: ", v_ban_existe
    DISPLAY "Solicitud: ", v_num_solicitud

    -- se devuelve el resultado de la creacion
    RETURN v_ban_existe, v_num_solicitud

END FUNCTION

#==============================================================================#
#Nombre  : fn_respuesta_wsmarcaje                                              #
#Creacion: Septiembre 2020                                                     #
#Modifico: Isai Jimenez Rojas, Omnisys                                         #
#Objetivo: Ejecuta la actualización de la respuesta del marcaje en la tabla de #
#          retiro genérico. Asigna los valores de retorno del Web Service      #
#==============================================================================#
FUNCTION fn_respuesta_wsmarcaje(p_estatus_marca   ,
                                p_id_solicitud    ,
                                p_cod_rechazo     ,
                                p_caso_crm        ,
                                p_saldo_aivs_total,
                                p_saldo_total     ,
                                p_ref_dap)

    DEFINE p_estatus_marca    SMALLINT,
           p_id_solicitud     DECIMAL(9,0),
           p_cod_rechazo      SMALLINT,
           p_caso_crm         LIKE ret_solicitud_generico.caso_adai,  --caso_crm
           p_saldo_aivs_total DECIMAL(24,6),
           p_saldo_total      DECIMAL(22,6),
           p_ref_dap          DECIMAL(9,0)
       
    --Asigna valores al arreglo
    --LET gr_salida.caso_crm = p_caso_crm
    LET gr_salida.est_marca   = p_estatus_marca

    LET gr_salida.con_retiro  = p_id_solicitud


    LET gr_salida.cod_rechazo       = p_cod_rechazo
    LET gr_salida.des_rechazo       = " "
    LET gr_salida.saldo_aivs  = 0
    LET gr_salida.monto_pesos = 0

    IF p_cod_rechazo <> 0 THEN
       --Busca la descripcion del error para regresarla en la consulta
       LET gr_salida.des_rechazo = ""
        
       SELECT des_larga
       INTO   gr_salida.des_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo

       IF gr_salida.des_rechazo IS NULL THEN
           LET gr_salida.des_rechazo = " "
       END IF
    END IF
   
    -- se actualiza el historico de la peticion
    CALL fn_registra_peticion_marca(g_id_peticion, gr_entrada.nss, gr_entrada.cuenta_clabe, gr_entrada.caso_crm, p_cod_rechazo, 1)
         RETURNING g_id_peticion
   
    -- se actualiza el detalle de la peticion
    CALL fn_registra_detalle_peticion_marca(g_id_peticion, 
                                            gr_entrada.ind_marca,
                                            gr_entrada.cod_rechazo,
                                            p_estatus_marca, p_id_solicitud, p_cod_rechazo, 1)
   
END FUNCTION

#==============================================================================#
#Nombre  : fn_registra_peticion_marca                                          #
#Creacion: Septiembre 20, 2013                                                 #
#Modifico: Isai Jimenez Rojas, Omnisys                                         #
#Objetivo: Registra los datos de entrada y respuesta que se recibieron/enviaron#
#          de una peticion de WS de marca                                      #
#==============================================================================#
FUNCTION fn_registra_peticion_marca(p_id_peticion, p_nss, p_caso_crm, p_cuenta_clabe, p_res_ejecucion, p_accion)

    DEFINE p_id_peticion             DECIMAL(9,0),
           p_nss                     LIKE afi_derechohabiente.nss,
           p_caso_crm                LIKE ret_solicitud_generico.caso_adai,
           p_cuenta_clabe            CHAR(18),
           p_res_ejecucion           SMALLINT, -- resultado de la ejecucion
           p_accion                  SMALLINT, -- 0: nuevo registro, 1: actualiza
           v_r_ret_ws_peticion_marca RECORD LIKE ret_ws_peticion_marca.* -- registro de peticion al ws

    -- si se trata de nuevo registro
    IF ( p_accion = 0 ) THEN
      
       -- se obtiene el id de peticion nuevo
       SELECT seq_ret_ws_generico.nextVal
       INTO   p_id_peticion
       FROM   systables
       WHERE  tabid = 1
       
       -- se asignan los datos
       LET v_r_ret_ws_peticion_marca.id_peticion   = p_id_peticion
       LET v_r_ret_ws_peticion_marca.f_peticion    = TODAY
       LET v_r_ret_ws_peticion_marca.h_peticion    = CURRENT HOUR TO SECOND
       LET v_r_ret_ws_peticion_marca.nss           = p_nss
       LET v_r_ret_ws_peticion_marca.rfc           = ''
       LET v_r_ret_ws_peticion_marca.caso_adai     = p_caso_crm
       LET v_r_ret_ws_peticion_marca.res_ejecucion = p_res_ejecucion
       
    
       -- se inserta el registro de peticion
       INSERT INTO ret_ws_peticion_marca VALUES ( v_r_ret_ws_peticion_marca.* )
    ELSE
       -- se actualiza el registro de la peticion
       UPDATE ret_ws_peticion_marca
       SET    res_ejecucion = p_res_ejecucion
       WHERE  id_peticion   = p_id_peticion
    END IF

    -- se devuelve el id de la peticion
    RETURN p_id_peticion

END FUNCTION


#==============================================================================#
#Nombre  : fn_registra_detalle_peticion_marca                                  #
#Creacion: Septiembre 20, 2013                                                 #
#Modifico: Isai Jimenez Rojas, Omnisys                                         #
#Objetivo: Registra los datos que se recibieron para generar el detalle de     #
#          marca de una solicitud de marca por ws                              #
#==============================================================================#
FUNCTION fn_registra_detalle_peticion_marca(p_id_peticion      ,
                                            p_ind_marca        ,
                                            p_cod_rechazo      ,
                                            p_resp_estado_marca,
                                            p_resp_id_solicitud,
                                            p_resp_cod_rechazp ,
                                            p_accion)

    DEFINE p_id_peticion                 DECIMAL(9,0),
           p_modalidad_retiro            LIKE ret_modalidad_retiro.modalidad_retiro,
           p_ind_marca                   SMALLINT,
           p_cod_rechazo                 LIKE ret_rechazo.cod_rechazo,
           p_resp_estado_marca           SMALLINT, -- resultado de la marca
           p_resp_id_solicitud           LIKE ret_solicitud_generico.id_solicitud, 
           p_resp_cod_rechazp            LIKE ret_rechazo.cod_rechazo,
           p_accion                      SMALLINT -- 0: nuevo registro, 1: actualiza
           
    DEFINE v_r_ret_ws_det_peticion_marca RECORD LIKE ret_ws_det_peticion_marca.* -- registro de detalle de peticion WS de marca

    -- si se trata de nuevo registro
    IF ( p_accion = 0 ) THEN
       -- se asignan los datos
       LET v_r_ret_ws_det_peticion_marca.id_peticion       = p_id_peticion
       LET v_r_ret_ws_det_peticion_marca.modalidad_retiro  = C_MODALIDAD_RETIRO   --1
       LET v_r_ret_ws_det_peticion_marca.ind_marca         = p_ind_marca
       LET v_r_ret_ws_det_peticion_marca.cod_rechazo       = p_cod_rechazo
       LET v_r_ret_ws_det_peticion_marca.resp_estado_marca = p_resp_estado_marca
       LET v_r_ret_ws_det_peticion_marca.resp_con_retiro   = p_resp_id_solicitud
       LET v_r_ret_ws_det_peticion_marca.resp_cod_rechazo  = p_resp_cod_rechazp
       
       -- se inserta el registro de peticion
       INSERT INTO ret_ws_det_peticion_marca VALUES ( v_r_ret_ws_det_peticion_marca.* )
    ELSE
       -- se actualiza el registro de la peticion
       UPDATE ret_ws_det_peticion_marca
       SET    resp_estado_marca = p_resp_estado_marca,
              resp_con_retiro   = p_resp_id_solicitud,
              resp_cod_rechazo  = p_resp_cod_rechazp
       WHERE  id_peticion       = p_id_peticion
       AND    modalidad_retiro  = C_MODALIDAD_RETIRO   --1=Solo infonavit
    END IF

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_registra_medio_entrega(pe_id_solicitud, pe_grupo, pe_medio_entrega)

    DEFINE pe_id_solicitud     DECIMAL(10,0)
    DEFINE pe_grupo            SMALLINT
    DEFINE pe_medio_entrega    SMALLINT 

    IF (pe_id_solicitud  IS NOT NULL AND 
        pe_grupo         IS NOT NULL AND
        pe_medio_entrega IS NOT NULL ) THEN
         
        --Inserta solicitud de medio de entrega
        INSERT INTO ret_sol_medio_entrega (id_solicitud, grupo, medio_entrega,f_registro)
             VALUES (pe_id_solicitud, pe_grupo, pe_medio_entrega, CURRENT YEAR TO MINUTE)
    END IF 

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_crea_caso(p_nss, p_medio_entrega)

   DEFINE p_nss           CHAR(11)
   DEFINE p_medio_entrega SMALLINT

   DEFINE v_regreso       SMALLINT
   DEFINE v_caso_crm      CHAR(10)
   DEFINE v_nombre        LIKE afi_derechohabiente.nombre_af
   DEFINE v_paterno       LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_materno       LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_rfc           LIKE afi_derechohabiente.rfc
   DEFINE v_curp          LIKE afi_derechohabiente.curp


   --- Verifica si hay caso abierto, si lo hay lo cierra y crea uno nuevo,
   --- si no hay caso abierto, crea uno nuevo
   --- si hay un caso abierto y no se puede cerrar, se rechaza la solicitud
   --- si no puede consumir el servicio de Busqueda o Cancelacion se rechaza la solicitud
   --- si no puede crear un nuevo caso rechaza la solicitud
   
   LET v_regreso       = 0
   LET v_caso_crm      = ''
   LET v_nombre        = ''
   LET v_paterno       = ''
   LET v_materno       = ''
   LET v_rfc           = ''
   LET v_curp          = ''
   
   --- Busca el nombre del Derechohabiente
   SELECT nombre_af, ap_paterno_af, ap_materno_af, rfc, curp
   INTO   v_nombre, v_paterno, v_materno, v_rfc, v_curp
   FROM   afi_derechohabiente
   WHERE  nss = p_nss

   IF p_medio_entrega = 2 THEN   --- Crea caso para la devolución automática
      
      --- Crea nuevo caso
      CALL fn_crea_caso_crm(p_nss, v_nombre, v_paterno, v_materno,v_rfc, v_curp, "ZG1D","A1ZN000127ZN02")  ----REVISAR PENDIENTE
           RETURNING v_regreso, v_caso_crm

      DISPLAY "El regreso en el llamado a Crea Caso Portal :", v_regreso

      IF v_regreso <> 0 THEN 
         LET v_regreso = gi_ws_crea_caso_crm_no_disponible
      END IF
   ELSE 
      LET v_regreso  = 0
      LET v_caso_crm = ''
   END IF 

   RETURN v_regreso, v_caso_crm

END FUNCTION
 
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fn_calcula_saldo_SI(p_nss, p_subcuenta, p_f_valuacion)

    DEFINE p_nss                LIKE afi_derechohabiente.nss,
           p_subcuenta          SMALLINT,
           p_f_valuacion        DATE -- fecha de valuacion
           
    DEFINE v_saldo_aivs         DECIMAL(24,6),
           v_saldo_pesos        DECIMAL(22,2), 
           v_resultado_consulta SMALLINT,
           v_sql                STRING

   -- se ejecuta el SP de consulta de saldo
   LET v_sql = "EXECUTE FUNCTION fn_saldo_dia(?,NULL,?,?)"
   
   -- se ejecuta la consulta de saldo
   PREPARE sid_saldo FROM v_sql
   EXECUTE sid_saldo USING p_nss, p_subcuenta, p_f_valuacion
                     INTO v_resultado_consulta, v_saldo_aivs, v_saldo_pesos

   -- se devuelve el resultado de la consulta
   RETURN v_resultado_consulta, v_saldo_aivs, v_saldo_pesos

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
