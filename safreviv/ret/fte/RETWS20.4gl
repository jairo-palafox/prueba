--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS20                                                 #
#OBJETIVO          => WS CONSULTA DE SALDOS DISPONIBLES PARA RETIRO DEL       #
#                     FONDO DE AHORRO                                         #
#FECHA INICIO      => 24-OCT-2018                                             #
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
DEFINE ws_ret_cons_saldos_disponibles_in RECORD
         nss              STRING, -- nss del trabajador
         rfc              STRING, -- rfc del trabajador
         causal_ret_fa    STRING, -- causal del retiro de fondo de ahorro
         nrp              STRING, -- NRP, usado para plan privado de pension en ret FA
         f_inicio_pension STRING, -- fecha de inicio de pension en formato AAAAMMDD
         medio_entrega    STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
      END RECORD,
       -- registro de respuesta
      ws_ret_cons_saldos_disponibles_out  RECORD
         nss                STRING, --- Número de seguridad social del trabajador
         rfc                STRING, -- rfc del trabajador
         estado_solicitud   STRING, -- estado de la solicitud
         cod_rechazo        STRING, -- codigo de rechazo
         des_rechazo        STRING,    -----  *****************************************
         monto_pesos        STRING, -- saldo en pesos equivalente a AIVs por valor accion
         monto_adicional    STRING, -- monto del tanto adicional
         monto_total        STRING, -- monto total a devolver
         pago_dap           STRING   -- se agrega este dato para indicar so debe o no pagar por DAP 1-Candidato a pago via DAP, 2-No es candidato a pago via DAP
      END RECORD
         
DEFINE g_indice_retiro      SMALLINT -- indice del tipo de retiro consultado

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
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS20."
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
    CALL ERRORLOG("Iniciando servidor de Disponibilidad Fondo de Ahorro ...")

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
Nombre: fn_crea_servicio_fondo_ahorro
Fecha creacion: Octubre 24, 2018
Autor: Ricardo Pérez
Narrativa del proceso que realiza:
Genera el servicio web de retiro del fondo de ahorro que consulta los saldos disponibles

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
DEFINE v_urn                STRING -- URN
  

    -- se declara el namespace del servicio
    LET v_service_NameSpace = "http://localhost/"
    LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

    TRY
        -- =============================
        -- se crea el servicio
        LET v_webservice = com.WebService.CreateWebService("retiroSaldosDisponiblesfa", v_service_NameSpace)

        -- =============================
        -- Publicacion de las funciones

        -- fn_retiro 
        LET op = com.WebOperation.CreateDOCStyle("fn_saldo_disponible_fa","fn_saldo_disponible_fa",ws_ret_cons_saldos_disponibles_in,ws_ret_cons_saldos_disponibles_out)
        --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
        --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7777/retiroSaldosDisponibles/fn_saldo_disponible_fa")
        CALL v_webservice.publishOperation(op, "fn_saldo_disponible_fa")

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
            -- REgistro del servicio
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
Nombre: fn_saldo_disponible_fa
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_saldo_disponible_fa()
DEFINE v_indice_retiro          SMALLINT,
       v_nss                    LIKE afi_fondo72.nss,
       v_rfc                    LIKE afi_fondo72.rfc,
	   v_ruta_ejecutable         LIKE seg_modulo.ruta_bin,
	   v_ruta_log                STRING,
	   v_cadena                  STRING,
      v_saldo                   DECIMAL(22,2), -- saldo del derechohabiente
      v_conteo                  SMALLINT,
      v_bandera_tanto_adicional SMALLINT 

    -- se responde el servicio para pruebas
    LET ws_ret_cons_saldos_disponibles_out.nss = ws_ret_cons_saldos_disponibles_in.nss
    LET ws_ret_cons_saldos_disponibles_out.rfc = ws_ret_cons_saldos_disponibles_in.rfc


    LET v_nss = ws_ret_cons_saldos_disponibles_in.nss
    LET v_rfc = ws_ret_cons_saldos_disponibles_in.rfc

    DISPLAY "Validando saldos para:"
    DISPLAY "NSS: ", v_nss
    DISPLAY "RFC: ", v_rfc

    -- se obtiene la ruta ejecutable
    SELECT ruta_listados
    INTO   v_ruta_ejecutable
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- se define la ruta del log
    LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS20."
    LET v_cadena   = v_nss
    LET v_ruta_log = v_ruta_log || v_cadena || ".log"

    DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

    -- se inicia el log del programa
    CALL STARTLOG(v_ruta_log)

    -- se inicia el indice del retiro que se va a consultar
    LET g_indice_retiro = 1

    -- se verifica si se recibieron datos para consultar fondo de ahorro
    -- se quita la validacion del RFC segun requerimiento PRODINF-876
    --IF ( v_rfc IS NOT NULL ) THEN


   IF ((ws_ret_cons_saldos_disponibles_in.causal_ret_fa IS NOT NULL) AND (ws_ret_cons_saldos_disponibles_in.causal_ret_fa <> 0) ) THEN
      DISPLAY "Validando Fondo de ahorro"
      DISPLAY "Parametros de entrada " 
      DISPLAY "NSS                     : ", v_nss
      DISPLAY "RFC                     : ", v_rfc
      DISPLAY "Causal                  : ", ws_ret_cons_saldos_disponibles_in.causal_ret_fa
      DISPLAY "NRP                     : ", ws_ret_cons_saldos_disponibles_in.nrp
      DISPLAY "Fecha inicio de Pensión : ", ws_ret_cons_saldos_disponibles_in.f_inicio_pension
      IF ws_ret_cons_saldos_disponibles_in.medio_entrega = 6 THEN
         DISPLAY "Debe existir la premarca para poder devolver el saldo"
         -- busca al derechohabiente en la tabla ret_fondo_ahorro con estatus en tramite
         SELECT COUNT(*)
         INTO   v_conteo
         FROM   ret_solicitud_generico
         WHERE  nss = v_nss
         AND    modalidad_retiro = 2
         AND    estado_solicitud =  8;
         IF ( v_conteo = 1 ) THEN
            CALL fn_recupera_saldo_fa(v_nss, v_rfc) RETURNING v_saldo
            LET ws_ret_cons_saldos_disponibles_out.nss              = v_nss
            LET ws_ret_cons_saldos_disponibles_out.rfc              = v_rfc
            LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = 10 --- Aceptada
            LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = 0  --- Sin código de rechazo
            LET ws_ret_cons_saldos_disponibles_out.des_rechazo      = "" --- Sin descripción de rechazo
            LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = v_saldo
            LET ws_ret_cons_saldos_disponibles_out.monto_adicional  = 0
            LET ws_ret_cons_saldos_disponibles_out.monto_total      = v_saldo
            CALL fn_valida_tanto_adicional(v_nss,v_rfc,ws_ret_cons_saldos_disponibles_in.causal_ret_fa, ws_ret_cons_saldos_disponibles_in.nrp) RETURNING v_bandera_tanto_adicional
            IF v_bandera_tanto_adicional = 1 THEN
               LET ws_ret_cons_saldos_disponibles_out.monto_adicional  = v_saldo
               LET ws_ret_cons_saldos_disponibles_out.monto_total  = v_saldo * 2
            END IF
         ELSE 
            CALL ERRORLOG("Se rechaza porque debe existe otra solicitud en estado 8 para devolver el saldo")
            -- se responde al WS que se tiene una solicitud en tramite
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0,0)
         END IF         
      ELSE 
         --- Valida el RFC
         IF v_rfc IS NULL  OR v_nss IS NULL THEN
            IF v_nss IS NULL THEN 
               CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_es_necesario, 0,0)
            ELSE 
               CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_rfc_obligatorio, 0,0)
            END IF 
         ELSE 
            CALL fn_ret_disponibilidad_fondo_ahorro(v_nss, v_rfc, 
                                                 ws_ret_cons_saldos_disponibles_in.causal_ret_fa,
                                                 ws_ret_cons_saldos_disponibles_in.nrp,
                                                 ws_ret_cons_saldos_disponibles_in.f_inicio_pension,
                                                 ws_ret_cons_saldos_disponibles_in.medio_entrega,
                                                 TRUE)
         END IF 
      END IF 
   ELSE 
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_datos_incompletos, 0,0)
   END IF
   
END FUNCTION
{
Función para validar si se regresa el tanto adicional o no, solo para el medio de entrega 6
}
FUNCTION fn_valida_tanto_adicional(p_nss, p_rfc, p_causal, p_nrp)
DEFINE p_nss                           CHAR(11), -- NSS
       p_rfc                           CHAR(13), -- RFC
       p_causal                        SMALLINT, -- causal de retiro
       p_nrp                           LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_tanto_adicional               SMALLINT,   -- indica si se paga o no tanto adicional
       v_tiene_spess                   SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart                   LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess
       
   
   LET v_tanto_adicional = 0
   IF p_causal = 1 OR p_causal = 2 THEN
      CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart
      -- si tiene resolucion paga tanto adicional
      IF ( v_tiene_spess ) THEN
         LET v_tanto_adicional = 1
      END IF 
   END IF 
   IF p_causal = 3 THEN
      -- se verifica si el NRP existe en catalogo
      IF p_nrp IS NOT NULL THEN 
         IF ( fn_nrp_existe_en_catalogo(p_nrp) ) THEN
            LET v_tanto_adicional = 1
         END IF
      END IF 
   END IF 
   IF p_causal = 4 THEN
      LET v_tanto_adicional = 1
   END IF 

   RETURN v_tanto_adicional
   
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
Ivan Vega     19 Dic 2013            - Se verifica si el NSS ya esta marcado,
                                       y de ser asi, se rechaza la disponibilidad
======================================================================
}
FUNCTION fn_ret_disponibilidad_fondo_ahorro(p_nss, p_rfc, p_causal, p_nrp, v_f_inicio_pension, p_medio_entrega,p_es_consulta)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                CHAR(13), -- RFC
       p_causal             SMALLINT, -- causal de retiro
       p_nrp                CHAR(11), -- NRP
       v_f_inicio_pension   CHAR(8), -- fecha de inicio de pension en formato AAAAMMDD
       p_medio_entrega      SMALLINT, -- Medio por el cual se hizo el llamado  
       p_es_consulta        SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_f_cadena           VARCHAR(10), -- fecha formateada para transformar a DATE
       v_f_pension          DATE, -- fecha de inicio de pension en formato DATE
       v_count_bnd          SMALLINT   ,
       v_cod_rechazo        SMALLINT   ,
       v_id_solicitud       SMALLINT   ,
       v_conteo_nss         SMALLINT   ,
       v_ruta_ejecutable    VARCHAR(40),
       v_ruta_log           STRING,
       v_cadena             STRING,
       v_id_afi_fondo72     DECIMAL(9,0), -- id derechohabiente en fondo72
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_n_referencia       LIKE sfr_marca_Activa.n_referencia, -- para buscar nss marcado
       v_saldo              DECIMAL(22,2),-- saldo del derechohabiente
       v_tipo_credito       SMALLINT,
       v_tipo_originacion   SMALLINT 

   -- se verifica si se recibio NSS
   IF ( p_nss IS NOT NULL ) THEN

      -- para validar el NSS se verifica que exista al menos una vez
      SELECT COUNT(*)
      INTO   v_conteo_nss 
      FROM   afi_fondo72
      WHERE  nss = p_nss
      AND    ind_estado_cuenta = 0  -- cuenta Activa

      IF ( v_conteo_nss IS NULL OR v_conteo_nss < 1 ) THEN
         -- No existe el nss
         CALL ERRORLOG("No existe el NSS")
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0,0)
         RETURN
      ELSE
         -- si se encuentra mas de uno, no procede su solicitud
         IF ( v_conteo_nss > 1 ) THEN
            CALL ERRORLOG("El NSS devuelve más de un registro")
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_mas_de_un_registro, 0,0)
            RETURN
         END IF
      END IF 
   ELSE
      CALL ERRORLOG("No existe el NSS")
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0,0)
      RETURN

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
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0,0)
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
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0,0)     
      CALL ERRORLOG("El causal de retiro es invalido")
      RETURN 
   END IF      

   -- se verifica si el derechohabiente tiene un credito vigente
   IF ( fn_trabajador_credito_vigente(p_nss) ) THEN
      -- se responde negativo por tener un credito
      CALL fn_trabajador_tipo_credito(p_nss) RETURNING v_tipo_credito, v_tipo_originacion
      IF v_tipo_credito = 1 AND v_tipo_originacion = 1 THEN -- 1-Credito Tradicional, 1-Transferencia de Acreditados
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_tiene_credito_vigente, 0,0)
         RETURN
      END IF 
   END IF
   
   -- se verifica si tiene saldo
   CALL fn_recupera_saldo_fa(p_nss, p_rfc) RETURNING v_saldo

   IF ( v_saldo <= 0 ) THEN
      -- solicitud rechazada por no contar con saldo suficiente
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_saldo, 0,0)
      RETURN
   END IF

   -- se transforma la fecha AAAAMMDD a formato fecha
   LET v_f_cadena = v_f_inicio_pension[5,6], "/", v_f_inicio_pension[7,8], "/", v_f_inicio_pension[1,4]
   LET v_f_pension = DATE(v_f_cadena)


   CALL ERRORLOG("Verificando marca FA")
   -- se verifica la marca
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente 
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    ind_estado_cuenta = 0  -- cuenta Activa
   
   -- si no se encontro
   IF ( v_id_derechohabiente IS NULL ) THEN        
      -- se verifica si esta en la tabla de id_derechohabientes nuevos para fondo72
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_fondo72_d
      WHERE  nss = p_nss
   END IF

   -- si se encontro el id_derechohabiente, se verifica si tiene marca
   IF ( v_id_derechohabiente IS NOT NULL ) THEN

      CALL ERRORLOG("id_der encontrado: " || v_id_derechohabiente)

      SELECT n_referencia
      INTO   v_n_referencia
      FROM   sfr_marca_activa
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    marca = 802

      -- si aparece, se rechaza
      IF ( v_n_referencia IS NOT NULL ) THEN
         CALL ERRORLOG("Marca con referencia: " || v_n_referencia)
         -- se rechaza por estar en tramite
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_solicitud_en_tramite, 0,0)
         RETURN
      END IF
   END  IF
   
   -- SI PASO LAS VALIDACIONES ANTERIORES
   -- se realiza la verificacion de la solicitud de acuerdo al tipo de causal de retiro recibida
   CASE p_causal
      WHEN 1 -- termino de relacion laboral
         DISPLAY "Termino relacion laboral"
         CALL fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, v_saldo, p_es_consulta)
      WHEN 2 -- pension IMSS
         DISPLAY "IMSS"
         CALL fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, v_saldo, p_es_consulta)
      WHEN 3 -- plan privado de pension
         DISPLAY "Plan privado pension"
         CALL fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, v_saldo, v_f_pension, p_es_consulta)
      WHEN 4 -- plan privado
         DISPLAY "defuncion"
         CALL fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_f_pension, v_saldo, p_es_consulta)
      OTHERWISE
         DISPLAY "otra causal de FA"
         -- causal de retiro invalido
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_causal_retiro_invalido, 0,0)
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
                                      Si se recibe NSS se valida una pension vigente en el SPESS
                                      Si no se recibe NSS, con el RFC se busca 
Ivan Vega    19 Feb 2014           - La validacion de prescripcion ya no aplica segun reunion en
                                     INFONAVIT 18 feb 2014
======================================================================
}
FUNCTION fn_termino_relacion_laboral(p_nss, p_rfc, p_causal, p_saldo, p_es_consulta)
DEFINE p_nss                           CHAR(11), -- NSS
       p_rfc                           CHAR(13), -- RFC
       p_causal                        SMALLINT, -- causal de retiro
       p_id_beneficiario               SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                        CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno                    CHAR(18), -- Apellido paterno 
       p_ap_materno                    CHAR(18), -- Apellido materno
       p_entidad                       SMALLINT, -- Entidad federativa 
       p_causal_adai                   SMALLINT, -- Causal de adai
       p_saldo                         DECIMAL(22,2),
       p_es_consulta                   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                          SMALLINT      ,
       v_ejecucion_ws                  SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional               DECIMAL(12,2),
       v_referencia_banc               CHAR(12),
       v_fecha_ultima_relacion_laboral DATE, -- ultima fecha de relacion laboral
       v_fecha_nacimiento              DATE, -- fecha de nacimiento del derechohabiente
       v_tiene_credito_vigente         SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess                   SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart                   LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_con_sin_rel_lab               SMALLINT   -- indica si tiene o no relacion laboral
   
   -- se calcula la edad del derechohabiente
   CALL fn_edad_derechohabiente(p_nss) RETURNING r_edad
   CALL ERRORLOG("Edad calculada por NSS: ")
   CALL ERRORLOG(r_edad)

   -- la edad debe ser mayor o igual a 50 anos
   IF ( r_edad >= 50 ) THEN
      CALL ERRORLOG("Edad >= 50, se revisa SPESS")
      -- se verifica que el trabajador tenga una resolucion en el spess
      CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

      -- si no tiene resolucion valida en el spess
      IF ( NOT v_tiene_spess ) THEN
         CALL ERRORLOG("No tiene spess, se revisa ultima relacion laboral")

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
               -- derechohabiente sin resolucion de spess ni ano sin relacion laboral o no existe relacion laboral
               IF v_fecha_ultima_relacion_laboral IS NULL THEN 
                  IF v_con_sin_rel_lab = 1 THEN 
                     CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo,0)
                  ELSE 
                     CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo,0)
                  END IF 
               ELSE 
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_un_ano_relacion_laboral, p_saldo,0)
               END IF 
            ELSE
               -- 19feb2014. Se elimino la verificacion de la prescripcion
               -- el saldo es retirable
               IF v_con_sin_rel_lab = 1 THEN 
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_con_rel_laboral_actual, p_saldo,0)
               ELSE 
                  CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, g_res_procesada, p_saldo,0)
               END IF             
            END IF
         ELSE
            -- se rechaza porque se detecto un error al consultar el WS de ultima relacion laboral
            -- por lo tanto no es posible validar la solicitud
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_ws_rel_laboral_no_disponible, p_saldo,0)
         END IF
      ELSE
         -- 19feb2014. Se elimino la verificacion de la prescripcion
         -- el monto retirable es dos veces el saldo y es retirable
         CALL ERRORLOG("Term Rel Laboral, edad > 50 y con SPESS, se paga el doble")
         LET p_saldo = p_saldo * 2                  
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,1) ----**********
      END IF
   ELSE 
      -- solicitud rechazada por que el trabajador no tiene 50 anos cumplidos o mas
      CALL ERRORLOG("Solicitud rechazada porque el derechohabiente no tiene 50 anos o mas")

      -- edad inferior a 50 anos
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_edad_inferior_50_anos, p_saldo,0)
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: 
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza las validaciones de una solicitud de retiro de Fondo de Ahorro
por resolucion de pension del IMSS
fn_resolucion_pension_imss
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      19Feb2014              - Se elimina la verificacion de la prescripcion de acuerdo
                                        con definicion de INFONAVIT en junta del 18 feb 2014
======================================================================
}
FUNCTION fn_resolucion_pension_imss(p_nss, p_rfc, p_causal, p_saldo, p_es_consulta)
DEFINE p_nss             CHAR(11), -- NSS
       p_rfc             CHAR(13), -- RFC
       p_causal          SMALLINT, -- causal de retiro
       p_id_beneficiario SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre          CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno      CHAR(18), -- Apellido paterno 
       p_ap_materno      CHAR(18), -- Apellido materno
       p_entidad         SMALLINT, -- Entidad federativa 
       p_causal_adai     SMALLINT, -- Causal de adai
       p_saldo           DECIMAL(22,2),
       p_es_consulta     SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad            SMALLINT      ,
       v_ejecucion_ws    SMALLINT      , -- bandera que indica si se ejecuto correctamente el webservice
       v_tanto_adicional DECIMAL(12,2),
       v_referencia_banc CHAR(12),
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess     SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_id_datamart     LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
       v_f_resolucion    LIKE ret_datamart.f_resolucion, -- fecha de resolucion en el SPESS
       v_anos_prescripcion SMALLINT,
       v_error_det         SMALLINT     --contiene en error por la cual no se encontro resolucion en el spess

   -- se asume que no hay referencia bancaria
   LET v_referencia_banc = "0"
   DISPLAY "Llega causal a validacion de pension IMSS: ", p_causal

   -- se revisa que venga nss, ya que sin este es necesario para todo el proceso
   IF ( p_nss IS NULL OR p_nss = "00000000000" ) THEN
      -- caso invalido para pension del IMSS, se necesita un NSS valido
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nss_es_necesario, 0,0)

      -- no puede seguir el proceso
      RETURN
   END IF

   -- se verifica que el trabajador tenga una resolucion de pension valida
   CALL fn_trabajador_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart

   IF ( v_tiene_spess ) THEN

      -- 19feb2014. Se elimina la verificacin de la prescripcion
      -- el saldo es retirable
      LET p_saldo = p_saldo * 2
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,1) ----**********
   ELSE
      -- no tiene resolucion en el SPESS
      CALL fn_detalle_resolucion_spess(p_nss, p_causal) RETURNING v_tiene_spess, v_id_datamart, v_error_det
      IF (v_error_det <> gi_resolucion_neg_pension AND v_error_det <> gi_porcentaje_menor_50) THEN 
         CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_sin_resolucion_spess, p_saldo,0)
      ELSE 
         IF v_error_det = gi_resolucion_neg_pension THEN 
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_resolucion_neg_pension, p_saldo,0)
         END IF 
         IF v_error_det = gi_porcentaje_menor_50 THEN 
            CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_porcentaje_menor_50, p_saldo,0)
         END IF 
      END IF 
   END IF

   DISPLAY "Finalizado validacion pension imss"

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
Ivan Vega      19 Feb 2014            - Se elimina la validacion de prescripcion conforme definicion de
                                        INFONAVIT en junta del 18 feb 2014
======================================================================
}
FUNCTION fn_plan_privado_pension(p_nss, p_rfc, p_causal, p_nrp, p_saldo, v_f_inicio_pension, p_es_consulta)
DEFINE p_nss                   CHAR(11), -- NSS
       p_rfc                   CHAR(13), -- RFC
       p_causal                SMALLINT, -- causal de retiro
       p_nrp                   LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       p_f_inicio_pension_caracter CHAR(8),
       v_fecha_inicio_pension  DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario       SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno            CHAR(18), -- Apellido paterno 
       p_ap_materno            CHAR(18), -- Apellido materno
       p_entidad               SMALLINT, -- Entidad federativa 
       p_causal_adai           SMALLINT, -- Causal de adai
       p_saldo                 DECIMAL(22,2),
       p_es_consulta           SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                  SMALLINT      ,
       r_b_paso                SMALLINT      ,
       v_tanto_adicional       DECIMAL(12,2),
       v_referencia_banc       CHAR(12),
       v_cadena                STRING,
       v_tiene_credito_vigente SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess           SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_diferencia_anos       SMALLINT, -- diferencia en anos
       v_f_inicio_pension      LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_id_datamart           LIKE ret_datamart.id_datamart -- clave de la resolucion en el spess

   -- se verifica si el NRP existe en catalogo
   IF ( fn_nrp_existe_en_catalogo(p_nrp) ) THEN
      LET p_saldo = p_saldo * 2
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,1) ----**********
   ELSE
      -- se rechaza por inexistencia de NRP
      CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_rechazada, gi_nrp_no_encontrado, p_saldo,0)
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
Ivan Vega      19 feb 2014            - Se elimina la validacion de prescripcion conforme definicion de 
                                        INFONAVIT en junta de 18 feb 2014
======================================================================
}
FUNCTION fn_retiro_fa_defuncion(p_nss, p_rfc, p_causal, v_fecha_inicio_pension, p_saldo, p_es_consulta)
DEFINE p_nss                           CHAR(11), -- NSS
       p_rfc                           CHAR(13), -- RFC
       p_causal                        SMALLINT, -- causal de retiro
       p_nrp                           LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_fecha_inicio_pension          DATE, -- fecha de inicio de pension en formado DATE
       p_id_beneficiario               SMALLINT, -- Identificador de beneficiario (si aplica)
       p_nombre                        CHAR(18), -- Nombre del beneficiario 
       p_ap_paterno                    CHAR(18), -- Apellido paterno 
       p_ap_materno                    CHAR(18), -- Apellido materno
       p_entidad                       SMALLINT, -- Entidad federativa 
       p_causal_adai                   SMALLINT, -- Causal de adai
       p_saldo                         DECIMAL(22,2),
       p_es_consulta                   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       r_edad                          SMALLINT      ,
       v_ejecucion_ws                  SMALLINT      ,
       v_tanto_adicional               DECIMAL(12,2),
       v_referencia_banc               CHAR(12),
       v_tiene_credito_vigente         SMALLINT, -- booleana que indica si se tiene un credito vigente
       v_tiene_spess                   SMALLINT, -- booleana para verificar si se tiene resolucion valida de spess
       v_f_inicio_pension              LIKE ret_datamart.f_inicio_pension, -- fecha de inicio de pension
       v_diferencia_anos               SMALLINT, -- diferencia en anos de dos fechas
       v_id_datamart                   LIKE ret_datamart.id_datamart, -- clave de la resolucion en el spess
	   v_fecha_ultima_relacion_laboral DATE, -- ultima fecha de relacion laboral
       v_con_sin_rel_lab               SMALLINT  -- Indica si tiene o no relacion laboral
       

   -- 19feb2014. Se elimina la validacion de prescripcion
   -- el saldo es retirable
   LET p_saldo = p_saldo * 2
   CALL fn_respuesta_ws_fondo_ahorro(gi_solicitud_aceptada, 0, p_saldo,1) ----**********

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

======================================================================
}
FUNCTION fn_respuesta_ws_fondo_ahorro(p_estado_solicitud, p_cod_rechazo, p_importe_viv72,p_tanto_adicional)
DEFINE   p_estado_solicitud SMALLINT, -- Resp. de la solicidut, aceptada-rechazada
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_importe_viv72    DECIMAL(12,2), -- Importe de vivienda 72
         p_tanto_adicional  SMALLINT,
         v_importe_dap      DECIMAL(12,2), -- importe para validacion de pago por dap o clabe 1000 o 2000
         v_devolver_saldo   SMALLINT, -- booleana que indica si el saldo se debe devolver
         v_desc_rechazo     CHAR(100)
         
   -- se verifica si se debe devolver el saldo de esta subcuenta
   LET v_devolver_saldo = fn_buscar_disponibilidad_retiro(2, 40)
   LET ws_ret_cons_saldos_disponibles_out.pago_dap         = 2
   LET v_importe_dap = 2000

   -- si no se debe devolver, entonces se cambia el resultado por no disponible
   IF ( NOT v_devolver_saldo ) THEN
      LET p_estado_solicitud = gi_solicitud_rechazada
      LET p_cod_rechazo      = gi_no_disponible_para_retiro
   END IF
   LET ws_ret_cons_saldos_disponibles_out.des_rechazo = " ";
   LET v_desc_rechazo = ""
   IF p_cod_rechazo <> 0 THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET ws_ret_cons_saldos_disponibles_out.des_rechazo = "";
      SELECT des_larga
      INTO   v_desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      IF v_desc_rechazo IS NULL THEN
         LET ws_ret_cons_saldos_disponibles_out.des_rechazo = " ";
      ELSE  
         LET ws_ret_cons_saldos_disponibles_out.des_rechazo = v_desc_rechazo
      END IF
   END IF 

   -- se construye la respuesta del ws
   LET ws_ret_cons_saldos_disponibles_out.estado_solicitud = p_estado_solicitud
   LET ws_ret_cons_saldos_disponibles_out.cod_rechazo      = p_cod_rechazo
   LET ws_ret_cons_saldos_disponibles_out.monto_total      = p_importe_viv72
   IF p_tanto_adicional = 1 THEN 
      LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = p_importe_viv72 / 2
      LET ws_ret_cons_saldos_disponibles_out.monto_adicional  = p_importe_viv72 / 2
   ELSE
      LET ws_ret_cons_saldos_disponibles_out.monto_adicional  = 0
      LET ws_ret_cons_saldos_disponibles_out.monto_pesos      = p_importe_viv72
   END IF 

   IF (ws_ret_cons_saldos_disponibles_in.causal_ret_fa = 1) THEN -- Si tanto adicional
      LET v_importe_dap = 1000  -- el monto debe ser menor o igual a 1000 ya que no incluye el tanto adicional
   END IF 
   IF p_importe_viv72 <= v_importe_dap  AND p_importe_viv72 > 0 THEN 
      LET ws_ret_cons_saldos_disponibles_out.pago_dap = 1
   END IF 

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
{         
    -- se construye la respuesta del ws
    LET ret_respuesta.nss         = p_nss
    LET ret_respuesta.res_op      = p_estado_solicitud
    LET ret_respuesta.cod_rechazo = p_cod_rechazo
    LET ret_respuesta.imp_viv7292 = p_importe_viv7292
    LET ret_respuesta.num_ref     = p_num_referencia
}
END FUNCTION
