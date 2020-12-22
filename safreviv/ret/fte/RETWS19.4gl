--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:27/10/2020
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS19                                                 #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO DE LA DEVOLUCIÓN AUTOMÁTICA DE AMORTIZACIONES    #
#                     EXCEDENTES A TRAVÉS DE LA FIEL                          #
#FECHA INICIO      => 30-NOV-2017                                             #
# Autor           Fecha      Modificación                                     #
-------------------------------------------------------------------------------
#MODIFICACION 27/10/2020                                                      #
#OBJETIVO          => SE AGREGA EL LLAMADO A LA FUNCIÓN QUE REGISTRA LOS      #
#                     DATOS DE CADA INVOACIÓN A LOS WS QUE SEA REALIZADA      #
#POR               => EMMANUEL REYES, OMNISYS                                 #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
PRIVATE DEFINE v_archivo_pdf STRING 
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_ret_generico_solicitud_in RECORD
         nss              STRING, -- nss del trabajador
         casoCRM          STRING, -- numero de caso ADAI
         medioEntrega     STRING, -- Sello generado por la consulta biometrica (aplica para medio_entrega = 2  Devolucion automatica)
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              tipoBeneficiario   STRING,
              clabeBancaria      STRING,
              rfc                STRING,
              email              STRING,
              telefono           STRING,
              telMovil           STRING,
              nombre             STRING,
              apPaterno          STRING,
              apMaterno          STRING,
              entidadFederativa  STRING
         END RECORD
       END RECORD,
       -- registro de respuesta
       ws_ret_generico_solicitud_out  RECORD
         nss                 STRING, --- Número de seguridad social del trabajador
         casoCRM             STRING,
         sello               STRING,
         archivoPdf          BYTE, 
         arr_modalidad_retiro DYNAMIC ARRAY OF RECORD
           estadoSolicitud     STRING, -- estado de la solicitud
           codRechazo          STRING, -- codigo de rechazo
           desRechazo          STRING,   ---- *************************************++
           saldoAvis           STRING, -- saldo en AIVs
           saldoPesos          STRING
         END RECORD
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
DEFINE g_sesion_id   CHAR(100)

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
  LET v_ruta_log = "/safreviv_log/ret/RETWS19."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
  DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

  LET g_sesion_id = v_ruta_log.trim()
  
  -- se inicia el log del programa
  CALL STARTLOG(v_ruta_log)

  LET v_pantalla = FALSE
  #
  # Check arguments
  #
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_dev_automatica_ae(TRUE)
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
  CALL fn_crea_servicio_dev_automatica_ae(FALSE)

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
Nombre: fn_crea_servicio_dev_automatica_ae
Fecha creacion: Noviembre 30, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro de amortizaciones excedentes de devolución automática

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_dev_automatica_ae(p_generar_WSDL)
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
    LET v_webservice = com.WebService.CreateWebService("retiroCreaSolicitudDevAutomaticaAE", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_sol_dev_automatica_ae","fn_ret_sol_dev_automatica_ae",ws_ret_generico_solicitud_in,ws_ret_generico_solicitud_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL v_webservice.publishOperation(op, "fn_ret_sol_dev_automatica_ae")

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
Nombre: fn_ret_sol_dev_automatica_ae
Fecha creacion: Noviembre 30, 2017
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion principal del WS que consulta los saldos disponibles para retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_ret_sol_dev_automatica_ae()
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

DEFINE v_arr_beneficiario  DYNAMIC ARRAY OF RECORD
              tipoBeneficiario   SMALLINT,
              clabeBancaria      CHAR(18),
              rfc                CHAR(13),
              email              CHAR(50),
              telefono           CHAR(10),
              telMovil           CHAR(10),
              nombre             CHAR(40),
              apPaterno          CHAR(40),
              apMaterno          CHAR(40),
              entidadFederativa  CHAR(2)
         END RECORD
       
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
                                                   ws_ret_generico_solicitud_in.casoCRM)
           RETURNING g_id_peticion
      
      -- se asume que todos traen cuenta CLABE
      LET v_cta_clabe_correcta = TRUE
	  
	  -- si se tiene mas de una modalidad de retiro, todas deben tener cuenta CLABE
      IF ws_ret_generico_solicitud_in.medioEntrega <> "2" THEN 
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength() 
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria IS NULL ) THEN
               -- se rechaza porque todos deben traer CLABE, incluyendo Fondo de Ahorro
               LET v_cta_clabe_correcta = FALSE
               EXIT FOR
            END IF
         END FOR
      END IF 
			
		 
      -- si no fue correcto, se rechaza la solicitud para todas las modalidades
      IF ( NOT v_cta_clabe_correcta ) THEN
         CALL fn_respuesta_ws(v_nss, NULL, 9,
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
                                                           9,
                                                           NULL,
                                                           NULL             ,
                                                           NULL,
                                                           NULL )
         -- se asume que el registro de beneficiario es correcto
         LET v_existe_beneficiario = TRUE

         -- se asume que las cuentas clabe son correctas
         LET v_cta_clabe_correcta = TRUE
         
         IF ws_ret_generico_solicitud_in.medioEntrega = "2" THEN
            --- Obtenemos los datos del beneficiario para grupo 1 y medio entrega 1 (tableta)
            LET v_indice_beneficiario = 1 
            LET v_id_derechohabiente  = 0
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario = "1" --- Titular
            SELECT DISTINCT c.id_derechohabiente, a.cuenta_clabe,
                   NVL(d.rfc,"SIN RFC"), NVL(d.nombre_af,"SIN NOMBRE"),
                   NVL(d.ap_paterno_af,"SIN PATERNO"), NVL(d.ap_materno_af,"SIN MATERNO")
            INTO   v_id_derechohabiente,
                   v_arr_beneficiario[v_indice_beneficiario].clabeBancaria,
                   v_arr_beneficiario[v_indice_beneficiario].rfc,
                   v_arr_beneficiario[v_indice_beneficiario].nombre,
                   v_arr_beneficiario[v_indice_beneficiario].apPaterno,
                   v_arr_beneficiario[v_indice_beneficiario].apMaterno
            FROM   ret_pago_spei             a,
                   ret_solicitud_generico    c,
                   afi_derechohabiente       d
            WHERE  c.nss                 = v_nss
            AND    d.id_derechohabiente  = c.id_derechohabiente
            AND    c.modalidad_retiro    = 9
            AND    c.estado_solicitud    = 8
            AND    a.id_solicitud        = c.id_solicitud
            AND    a.consec_beneficiario = 1

            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria = v_arr_beneficiario[v_indice_beneficiario].clabeBancaria
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc           = v_arr_beneficiario[v_indice_beneficiario].rfc
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre        = v_arr_beneficiario[v_indice_beneficiario].nombre
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno     = v_arr_beneficiario[v_indice_beneficiario].apPaterno
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apMaterno     = v_arr_beneficiario[v_indice_beneficiario].apMaterno


            SELECT TRIM(NVL(telefono,"0000000000"))
            INTO   v_arr_beneficiario[v_indice_beneficiario].telefono
            FROM   afi_telefono
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    id_telefono        = 1
            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = v_arr_beneficiario[v_indice_beneficiario].telefono
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono = "0000000000"
            END IF 

            SELECT NVL(valor,"SIN CORREO")
            INTO   v_arr_beneficiario[v_indice_beneficiario].email 
            FROM   afi_contacto_electronico
            WHERE  id_derechohabiente      = v_id_derechohabiente
            AND    id_contacto_electronico = 1

            LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = v_arr_beneficiario[v_indice_beneficiario].email

            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email = "SIN CORREO"
            END IF 

            SELECT NVL(b.entidad_federativa,"9")
            INTO   v_arr_beneficiario[v_indice_beneficiario].entidadFederativa
            FROM   afi_domicilio a,
                   cat_cp        b
            WHERE  a.id_derechohabiente = v_id_derechohabiente
            AND    a.id_domicilio       = 1
            AND    a.cp                 = b.cp

            LET  ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa = v_arr_beneficiario[v_indice_beneficiario].entidadFederativa
            IF ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa IS NULL THEN 
               LET ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa = "9"
            END IF 
 
         END IF  
         FOR v_indice_beneficiario = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
            -- se registra en la bitacora los datos de beneficiarios recibidos
            CALL fn_registra_peticion_registro_solicitud_benef(g_id_peticion                                                                                                                   ,
                                                                9                                          ,
                                                                v_indice_beneficiario                                                                                                           ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria    ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc               ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email             ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono          ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telMovil         ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre            ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno        ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apMaterno        ,
                                                                ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa)
             
            -- debe traer todos los datos
            IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].tipoBeneficiario  IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].rfc                 IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].email               IS NULL OR
               --ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].telefono            IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].nombre              IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].apPaterno          IS NULL OR
               ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].entidadFederativa  IS NULL ) THEN

               -- los datos de un beneficiario estan mal, no procede la solicitud
               LET v_existe_beneficiario = FALSE
               EXIT FOR
            ELSE
               -- si le falta la CLABE
               IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_indice_beneficiario].clabeBancaria IS NULL ) THEN
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
            IF ws_ret_generico_solicitud_in.medioEntrega = "2" AND v_indice_beneficiario = 1 THEN 
               EXIT FOR
            END IF 
         END FOR
         -- se verifica que la modalidad traiga beneficiarios
         IF ( NOT v_existe_beneficiario OR NOT v_cta_clabe_correcta ) THEN
            -- se verifica si es por falta de datos de un beneficiario
            IF ( NOT v_existe_beneficiario ) THEN
               CALL fn_respuesta_ws(v_nss, v_rfc, 9,
                                  gi_solicitud_rechazada, gi_solicitud_sin_beneficiarios, 0, 0, NULL)
            ELSE
               -- se rechaza por la estructura de la cuenta clabe
               CALL fn_respuesta_ws(v_nss, v_rfc, 9,
                                  gi_solicitud_rechazada, gi_esctructura_cta_clabe_incorrecta, 0, 0, NULL)               
            END IF
         ELSE            
            -- se verifica la modalidad
            CALL fn_ret_disponibilidad_amort_excedentes(v_nss, v_rfc, v_indice_modalidad)
         END IF
      END IF 
   END IF

   DISPLAY "Invoca a la bitacora"
   CALL fn_invoca_registra_bitacora_ws()

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_ret_disponibilidad_amort_excedentes
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente puede realizar el retiro de su saldo de cuenta
de un credito por amortaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     18 Oct 2013             - La solicitud de amort. excedentes tambien tendra que pasar
                                        a ser autorizada por el CESI, por lo que ya no se crean
                                        preautorizadas
======================================================================
}
FUNCTION fn_ret_disponibilidad_amort_excedentes(p_nss, p_rfc, p_indice_modalidad)
DEFINE p_nss                CHAR(11), -- NSS
       p_rfc                LIKE afi_derechohabiente.rfc,
       p_num_credito        CHAR(10), -- numero de credito
       p_indice_modalidad   SMALLINT, -- booleana que indica si es una consulta o inicio de tramite
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_tabla_saldo        VARCHAR(40),
       v_saldo_aivs         DECIMAL(22,6),
       v_saldo_pesos        DECIMAL(22,2),
       v_fecha_saldo        DATE,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_sql                STRING,
       v_tabla_consulta     VARCHAR(40),
       v_valor_fondo        LIKE glo_valor_fondo.precio_fondo,
       v_estado_solicitud   LIKE ret_solicitud_generico.estado_solicitud

DEFINE v_caso_crm          CHAR(10)       
   -- se obtiene el id de solicitud creada en el marcado buscando estado en 8 precapturada
   -- CALL fn_obtener_id_solicitud_generico(p_nss, p_rfc, 9, 8) RETURNING v_id_solicitud
   -- Se elimina este llamado para hacer la busqueda sin el RFC   RPR 140616 *****
   
   SELECT id_solicitud
   INTO   v_id_solicitud
   FROM   ret_solicitud_generico
   WHERE  nss              = p_nss
   AND    modalidad_retiro = 9
   AND    estado_solicitud = 8   

   -- si la cuenta aparece marcada y lista para ser generada una solicitud
   IF ( v_id_solicitud IS NOT NULL ) THEN

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss               = p_nss
      AND    ind_estado_cuenta = 0  -- cuenta Activa 
    
      -- Si no se encontro el nss se busca con la marca de unificado 
      IF v_id_derechohabiente IS NULL THEN 
          SELECT af.id_derechohabiente
          INTO   v_id_derechohabiente 
          FROM   sfr_marca_activa sm,
                 afi_derechohabiente af
          WHERE  sm.id_derechohabiente = af.id_derechohabiente
          AND    sm.marca = 151
          AND    af.nss = p_nss  -- Se implemento esta busqueda por los que estan inhabilitados por unificacion  requerimiento PRODINF-935
      END IF       
      -- si se encontro el NSS
      IF ( v_id_derechohabiente IS NOT NULL ) THEN
         TRY -- se intenta obtener el saldo
            LET v_sql = "\n SELECT SUM(monto_acciones)  ,",
                        "\n        SUM(monto_pesos   )   ",
                        "\n FROM   cta_movimiento        ",
                        "\n WHERE  id_derechohabiente = ?",
                        "\n AND    subcuenta = 46 ",
                        "\n AND    fondo_inversion <> 0 "
            
                        
            PREPARE sid_amortexced FROM v_sql
            EXECUTE sid_amortexced USING v_id_derechohabiente
                                   INTO v_saldo_aivs, v_saldo_pesos
            
            -- se obtiene el valor del fondo de inversion
            CALL fn_obtener_precio_fondo(TODAY, 11) RETURNING v_valor_fondo
            
            -- si el NSS tiene saldo
            IF ( v_saldo_aivs IS NOT NULL AND v_saldo_aivs > 0 ) THEN
            
               -- se calculan los pesos segun las AIVs y el valor del fondo
               LET v_saldo_pesos = v_saldo_aivs * v_valor_fondo
               
               -- la solicitud es aceptada
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_aceptada, 0, v_saldo_aivs, v_saldo_pesos)
                        
               -- se crea la solicitud en la tabla de historicos
               CALL fn_genera_solicitud_ret_amort_excedentes(p_nss, v_id_derechohabiente, gi_solicitud_aceptada, 0, v_saldo_aivs, v_saldo_pesos, v_id_solicitud, p_indice_modalidad)
            ELSE
               -- no tiene saldo
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_sin_saldo, 0, 0)
               
               CALL fn_genera_solicitud_ret_amort_excedentes(p_nss, v_id_derechohabiente, gi_solicitud_rechazada, gi_sin_saldo, v_saldo_aivs, v_saldo_pesos, v_id_solicitud, p_indice_modalidad)
            END IF
         CATCH
            -- no hay conexion a la instancia           
            DISPLAY "No se puede conectar a la instancia de saldos"
            CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, -999, 0, 0)
            
         END TRY
      ELSE
         -- el nss no existe
         CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_nss_rfc_no_existe, 0, 0)
      END IF
   ELSE
      -- se verifica si existe la solicitud en un estado en el que indique que ya esta en proceso
      -- CALL fn_obtener_id_solicitud_generico_con_estado(p_nss, p_rfc, 9,
      --                                                  ws_ret_generico_solicitud_in.caso_adai)
      --     RETURNING v_id_solicitud, v_estado_solicitud
      -- Se cambia este llamado para no enviar el rfc en la busqueda del id solicitud RPR  140616 *****

      LET v_caso_crm = ws_ret_generico_solicitud_in.casoCRM
       SELECT id_solicitud    ,
                estado_solicitud
         INTO   v_id_solicitud,
                v_estado_solicitud
         FROM   ret_solicitud_generico
         WHERE  nss              = p_nss
         AND    modalidad_retiro = 9
         AND    caso_adai        = v_caso_crm
         
      -- si no se encuentra, entonces no existe la solicitud
      IF ( v_id_solicitud IS NULL ) THEN
         CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_no_existe_solicitud, 0, 0)
      ELSE
         -- si el estado esta entre 10 y 209, desde solicitado hasta en proceso de restitucion
         -- la solicitud se encuentra en tramite
         IF ( v_estado_solicitud >= 10 AND v_estado_solicitud <= 209 AND v_estado_solicitud <> 100 ) THEN
            CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_tramite, 0, 0)
         ELSE
            -- si es 100, la solicitud fue rechazada
            IF ( v_estado_solicitud = 100 ) THEN
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_edo_rechazo, 0, 0)
            ELSE
               CALL fn_respuesta_ws_amort_excedente(p_nss, p_rfc, 9, gi_solicitud_rechazada, gi_solicitud_en_edo_no_reconocido, 0, 0)
            END IF
         END IF
      END IF
      
   END IF

   -- se incrementa el indice
   LET g_indice_retiro = g_indice_retiro + 1

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

DEFINE v_desc_rechazo     CHAR(100)
       
   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estadoSolicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].codRechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoAvis       = p_aivs
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoPesos      = p_pesos
   

   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = "";
       SELECT des_larga
       INTO   v_desc_rechazo 
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF v_desc_rechazo IS NULL IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " ";
       ELSE 
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo  = v_desc_rechazo
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
                                                     p_nrp, p_f_inicio_pension, p_num_credito)
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
   LET v_r_ret_ws_det_peticion_crea_solicitud.grupo_ley73      = 0
   LET v_r_ret_ws_det_peticion_crea_solicitud.num_credito      = 0
         
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
PUBLIC FUNCTION fn_load_pdf(v_ruta_reporte, v_archivo_reporte, p_caso)
   DEFINE archivo           BYTE
   DEFINE v_archivo         STRING 
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_archivo_reporte STRING 
   DEFINE v_comando         STRING 
   DEFINE p_caso            CHAR(10)             
   DEFINE v_resultado       SMALLINT 

   LOCATE archivo IN MEMORY
   #DISPLAY v_ruta_reporte
   CALL archivo.readFile(v_ruta_reporte)
   LET ws_ret_generico_solicitud_out.archivoPdf = archivo
   CALL security.Base64.LoadBinary(v_ruta_reporte) RETURNING v_archivo
   DISPLAY "Parámetros enviados a la Rutina de Adjunta Documentos"
   LET v_archivo_reporte = 'Acuse'
   DISPLAY "v_archivo_reporte: ", v_archivo_reporte
   DISPLAY "v_ruta_reporte: ",v_ruta_reporte
   DISPLAY "El archivo en base 64", v_archivo
   -- Se quita el envío a CRM, lo hará el portal CCARSAFRE19-2
   --CALL fn_adjunta_documento(v_archivo_reporte, v_archivo, p_caso) RETURNING v_resultado
   LET v_comando="rm "||v_ruta_reporte
--   RUN v_comando 
   
END FUNCTION

PRIVATE  FUNCTION fn_genera_reporte(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                    p_id_solicitud,p_pesos,p_sello,
                                    p_caso,p_clabe)

    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92--
    DEFINE p_sello           STRING -- Acuse Generado
    DEFINE p_caso            CHAR(10) -- Caso CRM 
    DEFINE p_clabe           CHAR(18) -- Clabe interbancaria  
    DEFINE v_tramite         CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo           CHAR(55)
    DEFINE medioSolicitud    CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE pesosTotal        CHAR(18) -- Suma en pesos total devuelto
    DEFINE archivo           BYTE
    DEFINE estadoConsulta    SMALLINT     -- Resultado de la Consulta
    DEFINE codRechazo        SMALLINT      -- Código de rechazo
    DEFINE reporte           om.SaxDocumentHandler
    DEFINE i                 SMALLINT
    DEFINE v_reporte         STRING
    DEFINE v_archivo         STRING 
    DEFINE v_ruta_listados   CHAR(40)
    DEFINE v_ruta_reporte    STRING
    DEFINE f_inicio          DATE
    DEFINE f_fin             DATE
    DEFINE v_query           STRING 
    DEFINE v_nombre          CHAR(120)
    DEFINE v_rfc             CHAR(13)
    DEFINE v_curp            CHAR(18)
    DEFINE v_nombre_stg      STRING 
    DEFINE v_fecha_paso      CHAR(16)
    DEFINE v_fecha           CHAR(16)
    DEFINE v_aviso           CHAR(255)
    DEFINE v_sello_funcionario STRING
    DEFINE v_sello           STRING
    DEFINE v_error           STRING
    DEFINE v_result          SMALLINT
 
    
    
   LET v_reporte= "RETWS19.4rp"
   LET v_aviso = NULL
   LET v_archivo = NULL

    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED
    LET v_archivo =  p_nss CLIPPED,"_", 
                     p_rfc CLIPPED,"_",
                     p_id_solicitud USING "&&&&&&&&&&","_"
                     ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                     ||DAY(TODAY) CLIPPED,".pdf" 
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" , v_archivo CLIPPED 
    
    LET v_ruta_pdf    = v_ruta_reporte
    LET v_archivo_pdf = v_archivo
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   -- Busca si hay aviso que publicar en el acuse
   SELECT aviso
   INTO   v_aviso
   FROM   ret_aviso_pdf_ssv
   WHERE  f_vig_inicio <= TODAY 
   AND    f_vig_final  >= TODAY 
   IF v_aviso IS NOT NULL THEN  --- Relaciona el mensaje con la solicitud
      INSERT INTO ret_sol_aviso_pdf VALUES (p_id_solicitud, v_aviso);
   END IF 

   -- Se obtiene el certificado del funcionario

   SELECT a.rfc
   INTO   v_rfc
   FROM   ret_rfc_firma_pdf a
   WHERE  a.id_firma = 2;   --- Funcionario de Cartera
   
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      CALL fn_obtiene_certificado(p_sello CLIPPED, v_rfc CLIPPED )  RETURNING v_result, v_error, v_sello_funcionario 
      DISPLAY "NSS reporte ", p_nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                       p_id_solicitud,p_pesos,v_sello_funcionario, v_aviso,
                                       p_caso,p_clabe)
         FINISH REPORT pdf_acuse
      END IF
   END IF
  
END FUNCTION 

REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                 p_id_solicitud,p_pesos,p_sello, p_aviso, p_caso,p_clabe) 
    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92
    DEFINE p_sello           STRING -- Acuse Generado
    DEFINE p_aviso           CHAR(255)  --- Mensaje eventual 
    DEFINE p_caso            CHAR(10)  -- Caso CRM
    DEFINE p_clabe           CHAR(18)  -- Clabe interbancaria

    DEFINE v_tramite            CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo              CHAR(55)
    DEFINE v_medioSolicitud     CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE v_pesos              CHAR(18) -- Vivienda 92
    DEFINE v_pesosViv97         CHAR(18) -- Vivienda 97
    DEFINE v_pesosTotal         CHAR(18) -- Suma en pesos total devuelto

    DEFINE v_nombre               CHAR(60)

   FORMAT

   FIRST PAGE HEADER

      PRINTX p_nss
      PRINTX p_rfc
      LET v_nombre = p_nombre CLIPPED, " ", p_paterno CLIPPED, " ", p_materno CLIPPED
      PRINTX v_nombre
      PRINTX p_clabe
      PRINTX p_fecha_hora
      PRINTX p_id_solicitud
      PRINTX p_caso
      LET v_medioSolicitud = "En línea"
      PRINTX v_medioSolicitud
      LET v_pesos = p_pesos USING "$$$,$$$,$$&.&&"
      PRINTX v_pesos
      PRINTX p_sello
      PRINTX p_aviso


END REPORT

FUNCTION fn_adjunta_documento(p_nombre_archivo, p_archivo, p_caso)
DEFINE p_nombre_archivo STRING 
DEFINE p_archivo        STRING 
DEFINE p_caso           CHAR(10)
DEFINE v_regreso        SMALLINT 
DEFINE v_codigo         INTEGER 

   DEFINE arr_documentos RECORD
         nombre_documento STRING, 
         documento        STRING 
   END RECORD 

  DISPLAY "Parametros recibidos para el consumo de la funcion de documentos"
  DISPLAY "p_archivo: ",p_archivo
  DISPLAY "p_nombre_archivo: ", p_nombre_archivo
   LET v_regreso = 0
   LET arr_documentos.nombre_documento = p_nombre_archivo
   LET arr_documentos.documento        = p_archivo
   CALL fn_adjunta_docto_crm(p_caso, arr_documentos.*) RETURNING v_regreso, v_codigo
   IF v_regreso = 0 THEN 
--      IF v_codigo = 0 THEN 
         DISPLAY "Documento enviado a CRM exitosamente :", p_caso
--      ELSE 
--         DISPLAY "No se pudo integrar el documento, respuesta del Servicio Adjunta Documento :", v_codigo
--      END IF
   ELSE 
      DISPLAY "Problemas al invocar el servicio de Adjunta Dcoumentos :", v_regreso
   END IF 

RETURN v_regreso

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_respuesta_ws_amort_excedente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Construye la respuesta para contestar la peticion del webservice
para el nss dado de un retiro por amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      25feb2014              - Se agrega referencia dap a la respuesta. Siempre en blanco
                                        para amort excedente
======================================================================
}
FUNCTION fn_respuesta_ws_amort_excedente(p_nss, p_rfc, p_modalidad, p_estado_solicitud, p_cod_rechazo, p_aivs, p_pesos)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         p_modalidad        SMALLINT, -- modalidad de retiro
         p_estado_solicitud SMALLINT, -- estado de la solicitud
         p_cod_rechazo      SMALLINT, -- Codigo de rechazo 
         p_aivs             DECIMAL(24,6), -- importe en aivs
         p_pesos            DECIMAL(22,2) -- importe en pesos

DEFINE  v_desc_rechazo      CHAR(100)
         
   -- se escribe la respuesta de la solicitud generica
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].estadoSolicitud = p_estado_solicitud
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].codRechazo      = p_cod_rechazo
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoAvis       = p_aivs
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].saldoPesos      = p_pesos
   
   LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " "; 
   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = "";
       SELECT des_larga
       INTO   v_desc_rechazo 
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF v_desc_rechazo  IS NULL THEN
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = " ";
       ELSE 
           LET ws_ret_generico_solicitud_out.arr_modalidad_retiro[g_indice_retiro].desRechazo = v_desc_rechazo
       END IF
   END IF 
   
   -- se incrementa el indice del retiro consultado
   LET g_indice_retiro = g_indice_retiro + 1

   -- se registra la respuesta de la peticion
   CALL fn_registra_det_peticion_registro_solicitud_resp(g_id_peticion, p_modalidad, 46,
                                                         p_estado_solicitud, p_cod_rechazo, p_aivs,
                                                         p_pesos, NULL)

END FUNCTION

{
======================================================================
Nombre: fn_genera_solicitud_ret_amort_excedentes
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro de amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_solicitud_ret_amort_excedentes(p_nss, p_id_derechohabiente, p_estado_solicitud, p_rechazo, p_aivs, p_pesos, p_id_solicitud, p_indice_modalidad)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
       p_nss                 LIKE afi_derechohabiente.nss, 
       p_rfc                 LIKE afi_derechohabiente.rfc,
       p_estado_solicitud    SMALLINT      , -- estatus de la solicitud
       p_rechazo             SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_aivs                DECIMAL(24,6),
       p_pesos               DECIMAL(22,2),
       p_referencia_bancaria CHAR(12), -- clave de referencia bancaria
       p_id_solicitud        LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       p_indice_modalidad    SMALLINT, -- indice de la modalidad del retiro
       v_estatus             SMALLINT      ,
       v_resultado           SMALLINT      , -- resultado de la ejecucion
       v_subcuenta           SMALLINT      , -- subcuenta de retiro
       v_id_solicitud        LIKE ret_fondo_ahorro.id_solicitud,
       v_marca_amort_exced   LIKE sfr_marca.marca, -- marca de amortizaciones excedentes
       v_saldo_poseido       LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       v_r_ret_amort_excedente RECORD LIKE ret_amort_excedente.*, -- registro de retiro de amortizaciones excedentes
       v_conteo                SMALLINT,
       v_tipo_pago             SMALLINT,
       v_sql                   STRING, -- cadena con enunciado SQL
       v_cadena               STRING,  -- Cadena de caracteres para generar el SHA
       v_algoritmo            CHAR(6),
       v_curp                 CHAR(18),
       v_rfc                  CHAR(13),
       v_nombre               CHAR(40),
       v_ape_paterno          CHAR(40),
       v_ape_materno          CHAR(40),
       v_sha                  STRING,
       v_c_sha                CHAR(64),
       v_fecha_paso           CHAR(16),
       v_fecha_hora           CHAR(16),
       v_caso_crm             CHAR(10)
       
   -- se asigna la marca
   LET v_marca_amort_exced = 810
   LET v_cadena = ""
   LET v_algoritmo = ""
   LET v_sha = ""
   LET v_rfc = ""
   LET v_fecha_paso = CURRENT YEAR TO MINUTE 
   LET v_caso_crm = ws_ret_generico_solicitud_in.casoCRM

   -- si la solicitud fue aceptada
   IF ( p_estado_solicitud = gi_solicitud_aceptada ) THEN

--- ************************************************************************************************

      IF ws_ret_generico_solicitud_in.medioEntrega = 1 OR 
         ws_ret_generico_solicitud_in.medioEntrega = 2 THEN 
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
         LET v_fecha_hora = v_fecha_paso[9,10],"/",v_fecha_paso[6,7],"/",v_fecha_paso[1,4]," ",v_fecha_paso[12,16]
         LET v_cadena = p_id_solicitud USING "##########", ws_ret_generico_solicitud_in.nss, 
                        v_rfc, v_fecha_hora, v_ape_paterno CLIPPED, v_ape_materno CLIPPED, 
                        v_nombre CLIPPED, p_pesos USING "<,<<<,<<&.&&"
         LET v_algoritmo = "SHA256"
         CALL ERRORLOG("cadena>"||v_cadena||"<") 
         CALL ERRORLOG("algoritmo>"||v_algoritmo||"<")
         
         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
         CALL ERRORLOG("sha>"||v_sha||"<")
         LET v_c_sha = v_sha
         CALL ERRORLOG("c_sha>"||v_c_sha||"<")
         UPDATE ret_sol_medio_entrega
         SET    sello = v_c_sha,
                f_registro = CURRENT YEAR TO MINUTE  
         WHERE  id_solicitud = p_id_solicitud;
         -- obtenemos el caso_crm de la tabla
         SELECT caso_adai
         INTO   v_caso_crm
         FROM   ret_solicitud_generico
         WHERE  id_solicitud = p_id_solicitud;
         LET ws_ret_generico_solicitud_in.casoCRM = v_caso_crm
         
         LET p_estado_solicitud = 10
         
         IF ws_ret_generico_solicitud_in.medioEntrega = 2 THEN 
            CALL fn_genera_reporte(ws_ret_generico_solicitud_in.nss,v_rfc,
                                   v_ape_paterno, v_ape_materno, v_nombre, 
                                   v_fecha_hora,p_id_solicitud,p_pesos,v_cadena, 
                                   ws_ret_generico_solicitud_in.casoCRM,
                                   ws_ret_generico_solicitud_in.arr_beneficiario[1].clabeBancaria)
            CALL fn_load_pdf(v_ruta_pdf, v_archivo_pdf, ws_ret_generico_solicitud_in.casoCRM)--Se crea, se envia y se borra el reporte.pdf
         END IF 
         LET ws_ret_generico_solicitud_out.sello = v_c_sha

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



--- ***************************************************************************************************
   
      -- se asignan los datos a la solicitud de retiro de amortizaciones excedentes
      LET v_r_ret_amort_excedente.id_solicitud       = p_id_solicitud
      LET v_r_ret_amort_excedente.folio              = 0
      LET v_r_ret_amort_excedente.id_derechohabiente = p_id_derechohabiente
      LET v_r_ret_amort_excedente.nss                = p_nss
      LET v_r_ret_amort_excedente.f_solicitud        = TODAY
      LET v_r_ret_amort_excedente.tpo_retiro         = 1 -- cual
      LET v_r_ret_amort_excedente.total_aivs         = p_aivs
      LET v_r_ret_amort_excedente.total_importe      = p_pesos
      LET v_r_ret_amort_excedente.cod_actividad      = 1
      LET v_r_ret_amort_excedente.cod_rechazo        = p_rechazo
      LET v_r_ret_amort_excedente.estado_solicitud   = p_estado_solicitud
          
      -- se inserta el registro   
      INSERT INTO ret_amort_excedente VALUES ( v_r_ret_amort_excedente.* )

      -- se actualiza el estado de la tabla de control
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud
      
      -- se crean los registros para los beneficiarios de esta solicitud
      FOR v_conteo = 1 TO ws_ret_generico_solicitud_in.arr_beneficiario.getLength()
         -- si se tiene cuenta clabe, es pago por SPEI
         IF ( ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabeBancaria IS NOT NULL ) THEN
            -- SPEI
            LET v_tipo_pago = 1
         ELSE
            -- pago por DAP
            LET v_tipo_pago = 2
         END IF
      
         CALL fn_registra_beneficiario_retiro_generico(p_id_solicitud,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].tipoBeneficiario,
                                                       v_tipo_pago, -- FALTA TIPO DE PAGO
                                                       1, -- FALTA PARENTESCO
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].apPaterno,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].apMaterno,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].nombre,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].telefono,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].email,
                                                       100,
                                                       p_aivs,
                                                       p_pesos,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].clabeBancaria,
                                                       p_referencia_bancaria,
                                                       ws_ret_generico_solicitud_in.arr_beneficiario[v_conteo].entidadFederativa)
         
         -- se verifica si el beneficiario
      END FOR
      
   ELSE
      -- se rechaza la solicitud
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = p_estado_solicitud,
             cod_rechazo      = p_rechazo,
             caso_adai        = v_caso_crm
      WHERE  id_solicitud     = p_id_solicitud

      -- se desmarca la cuenta
      CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, v_marca_amort_exced, p_id_solicitud,
                                           v_marca_amort_exced, "safreviv", g_proceso_cod_ret_amort_excedentes) 

   END IF

END FUNCTION 

################################################################################
# fn_invoca_registra_bitacora_ws Se encarga de invocar a la función que recopila
#                                datos e invoca a la bitácora de WS 
# Requerimiento : Folio008-2020
# Fecha creación: 27/10/2020
# Autor         : Emmanuel Reyes, Omnisys
# Modificación  : Se cambia el 3er parámetro, ya no se envía un array ahora se
#               : manda una cadena de 51 posiciones, cambio hecho en el proceso
#               : de pruebas
# Fecha Modifica: 06/11/2020
# Autor         : Emmanuel Reyes, Omnisys
################################################################################
FUNCTION fn_invoca_registra_bitacora_ws()

   DEFINE v_resultado SMALLINT

   DEFINE v_fecha     DATE

   DEFINE v_id_ws_ctr_maestra SMALLINT --CHAR(50)

   DEFINE v_identificador_id  CHAR(50)
   
   LET v_fecha = TODAY

   --OBTIENE DATOS DEL CATALOGO
   SELECT id_ws_ctr_maestra
     INTO v_id_ws_ctr_maestra
     FROM ws_ctr_maestra
    WHERE id_ws_ctr_maestra = 7 -- EL ASIGNADO PARA EL WS ACTUAL 

    LET v_identificador_id = ws_ret_generico_solicitud_in.nss CLIPPED,
                             ws_ret_generico_solicitud_in.casoCRM CLIPPED,
                             ws_ret_generico_solicitud_in.medioEntrega CLIPPED

    {DISPLAY "Parametros:"
    DISPLAY "v_id_ws_ctr_maestra: ",v_id_ws_ctr_maestra
    DISPLAY "g_sesion_id: ",g_sesion_id
    DISPLAY "v_identificador_id ", v_identificador_id CLIPPED}

    CALL fn_registra_bitacora_ws(v_id_ws_ctr_maestra CLIPPED,
                                 g_sesion_id         CLIPPED,
                                 v_identificador_id  CLIPPED)RETURNING v_resultado

    DISPLAY "Acaba invocacion a la bitacora"

END FUNCTION 