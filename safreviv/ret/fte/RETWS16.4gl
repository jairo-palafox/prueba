--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS16                                                 #
#OBJETIVO          => WS ACTUALIZACION DE LOS PORCENTAJES DE BENEFICIARIOS    #
#                     Y AUTORIZACION DE LA SOLICITUD                          #
#FECHA INICIO      => 31-JUL-2018                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

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
DEFINE ws_in RECORD
         nss              CHAR(11), -- nss del trabajador
         casoCrm          CHAR(10), -- numero de caso CRM
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              idBeneficiario    SMALLINT,
              nombre            LIKE afi_derechohabiente.nombre_af,
              apPaterno         LIKE afi_derechohabiente.ap_paterno_af,
              apMaterno         LIKE afi_derechohabiente.ap_materno_af,
              clabeBancaria     CHAR(18),
              porcentaje        SMALLINT
         END RECORD
      END RECORD,
       -- registro de respuesta
      ws_out  RECORD
         nss                 CHAR(11),
         casoCrm             CHAR(10),
         estMarca            SMALLINT,
         codRechazo          SMALLINT,
         desRechazo          CHAR(100), ------ *********************************         END RECORD
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              idBeneficiario    SMALLINT,
              nombre            LIKE afi_derechohabiente.nombre_af,
              apPaterno         LIKE afi_derechohabiente.ap_paterno_af,
              apMaterno         LIKE afi_derechohabiente.ap_materno_af,
              clabeBancaria     CHAR(18),
              porcentaje        SMALLINT,
              estMarca            SMALLINT,
              codRechazo          SMALLINT,
              desRechazo          CHAR(100)
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
CONSTANT GI_RECHAZO_JURIDICO                 SMALLINT = 61
         
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS16."
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
  IF FGL_GETENV("RETWS16LOG") THEN
     CALL STARTLOG(FGL_GETENV("RETWS16LOG"))
     DISPLAY "Ruta del log creada del servidor: " || FGL_GETENV("RETWS16LOG")
  ELSE
     DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
     CALL STARTLOG(v_ruta_log)
  END IF 

  LET v_pantalla = FALSE
  #
  # Check arguments
  #
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_porcentaje_benef(TRUE)
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
  CALL fn_crea_servicio_porcentaje_benef(FALSE)

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
Nombre: fn_crea_servicio_porcentaje_benef
Fecha creacion: Noviembre 30, 2017
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que complementa la solicitud del retiro de ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_porcentaje_benef(p_generar_WSDL)
  DEFINE v_webservice         com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

  TRY
    -- =============================
    -- se crea el servicio
    LET v_webservice = com.WebService.CreateWebService("retiroAutorizaPorcentajeBenef", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_auto_porcentaje_benef","fn_auto_porcentaje_benef",ws_in,ws_out)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL v_webservice.publishOperation(op, "fn_auto_porcentaje_benef")

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
    DISPLAY("No se pudo crear el servicio 'Crea Solicitud 4 Grupos Ley 73': " || STATUS)
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
Nombre: fn_auto_porcentaje_benef
Fecha creacion: Julio 31, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Funcion para actualizar los porcentajes de los beneficiarios y el estado de la solicitud

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
======================================================================
}
FUNCTION fn_auto_porcentaje_benef()
DEFINE v_indice_retiro       SMALLINT
DEFINE v_nss                 LIKE afi_fondo72.nss
DEFINE v_rfc                 LIKE afi_fondo72.rfc
DEFINE v_indice_modalidad    SMALLINT -- indice de modalidad de retiro
DEFINE v_indice_beneficiario SMALLINT -- contador de beneficiarios
DEFINE v_existe_beneficiario SMALLINT -- booleana que indica si esta bien el registro de beneficiario
DEFINE v_cta_clabe_correcta  SMALLINT -- booleana que indica si la cuenta clabe tiene estructura correcta
DEFINE v_requiere_DAP        SMALLINT -- booleana que indica si se necesita DAP
DEFINE v_modalidad_procesada SMALLINT -- Indica si ya se proceso una solicitud de ley 73
DEFINE v_id_derechohabiente  DECIMAL(10,0) -- Identificador único del trabajador
DEFINE v_error_beneficiarios SMALLINT
DEFINE v_suma_porcentajes    SMALLINT 
DEFINE v_indice              SMALLINT
DEFINE v_id_solicitud        DECIMAL(10,0)
DEFINE v_importe_total       DECIMAL(14,2)
DEFINE v_aivs_total          DECIMAL(14,2)
DEFINE v_arr_beneficiario    RECORD 
             idBeneficiario SMALLINT,
             nombre         CHAR(40),
             ap_paterno     CHAR(40),
             ap_materno     CHAR(40),
             clabe_bancaria CHAR(18),
             porcentaje     SMALLINT
       END RECORD 
DEFINE v_consecutivo        SMALLINT 
DEFINE v_resultado          SMALLINT
DEFINE v_errores            SMALLINT
DEFINE v_des_rechazo        LIKE ret_rechazo_generico.des_larga
   
   -- se verifica si se esta solicitando eco
   IF ( UPSHIFT(ws_in.nss) = "ECO" ) THEN
      -- se devuelve ECO
      LET ws_out.nss = "ECO"
      
      -- se indica que hay un registro
      LET g_indice_retiro = 1
     --CALL fn_respuesta_ws("ECO", "ECO", 0, 0, 0, 0, 0, NULL)
   ELSE
      -- se crea el registro de la peticion de solicitud de ws registro de solicitud
      CALL fn_registra_peticion_registro_solicitud(ws_in.nss, ws_in.casoCrm) RETURNING g_id_peticion
      -- se asignan los valores de respuesta
      LET ws_out.nss = ws_in.nss
      LET ws_out.casoCrm = ws_in.casoCrm
      LET ws_out.estMarca = gi_solicitud_aceptada
      LET ws_out.codRechazo = 0
      LET ws_out.desRechazo = ''
      LET v_resultado = 0
      LET v_errores = 0
      LET v_suma_porcentajes = 0

      FOR v_indice = 1 TO ws_in.arr_beneficiario.getLength()
         CALL fn_registra_det_peticion_registro_solicitud(g_id_peticion,ws_in.arr_beneficiario[v_indice].*)
         LET ws_out.arr_beneficiario[v_indice].idBeneficiario = ws_in.arr_beneficiario[v_indice].idBeneficiario
         LET ws_out.arr_beneficiario[v_indice].nombre = ws_in.arr_beneficiario[v_indice].nombre
         LET ws_out.arr_beneficiario[v_indice].apPaterno = ws_in.arr_beneficiario[v_indice].apPaterno
         LET ws_out.arr_beneficiario[v_indice].apMaterno = ws_in.arr_beneficiario[v_indice].apMaterno
         LET ws_out.arr_beneficiario[v_indice].clabeBancaria = ws_in.arr_beneficiario[v_indice].clabeBancaria
         LET ws_out.arr_beneficiario[v_indice].porcentaje = ws_in.arr_beneficiario[v_indice].porcentaje
         LET ws_out.arr_beneficiario[v_indice].estMarca = 0
         LET ws_out.arr_beneficiario[v_indice].codRechazo = 0
         LET ws_out.arr_beneficiario[v_indice].desRechazo = ''
         LET v_suma_porcentajes = v_suma_porcentajes + ws_in.arr_beneficiario[v_indice].porcentaje
      END FOR 
      IF v_suma_porcentajes <> 100 THEN 
         LET ws_out.estMarca = gi_solicitud_rechazada
         LET ws_out.codRechazo = gi_benef_porcentaje_no_suma_cien
         LET ws_out.desRechazo = 'La suma de porcentajes no es el 100 %'
      ELSE 
         CALL fn_busca_solicitud(ws_in.nss, ws_in.casoCrm) RETURNING v_id_solicitud, v_importe_total, v_aivs_total
         IF v_id_solicitud = 0 THEN 
            LET ws_out.estMarca = gi_solicitud_rechazada
            LET ws_out.codRechazo = gi_no_existe_solicitud
            LET ws_out.desRechazo = 'No existe solicitud'
         ELSE 
            LET v_indice = 0
            FOR v_indice = 1 TO ws_in.arr_beneficiario.getLength()
               LET v_arr_beneficiario.* = ws_in.arr_beneficiario[v_indice].*
               IF v_arr_beneficiario.idBeneficiario IS NOT NULL THEN 
                  CALL fn_busca_beneficiario(v_id_solicitud, v_arr_beneficiario.*) RETURNING v_consecutivo
                  IF v_consecutivo <> 0 THEN 
                     CALL fn_actualiza_porcentaje(v_id_solicitud, v_consecutivo, v_arr_beneficiario.porcentaje, v_importe_total,v_aivs_total) RETURNING v_resultado
                     
                     --20201020 si el porcentaje es cero se rechaza la solicitud del beneficiario
                     IF ( v_arr_beneficiario.porcentaje <= 0 ) THEN
                        LET ws_out.arr_beneficiario[v_indice].estMarca = gi_solicitud_rechazada
                        LET ws_out.arr_beneficiario[v_indice].codRechazo = GI_RECHAZO_JURIDICO
                        
                        -- descripcion del rechazo
                        SELECT concat(trim(des_larga), ": porcentaje cero")
                        INTO v_des_rechazo
                        FROM ret_rechazo_generico
                        WHERE cod_rechazo = GI_RECHAZO_JURIDICO
                        
                        LET ws_out.arr_beneficiario[v_indice].desRechazo = v_des_rechazo
                     END IF
                  ELSE 
                     LET ws_out.arr_beneficiario[v_indice].estMarca = gi_solicitud_rechazada
                     LET ws_out.arr_beneficiario[v_indice].codRechazo = gi_no_existe_solicitud
                     LET ws_out.arr_beneficiario[v_indice].desRechazo = 'No existe beneficiario'
                     LET ws_out.estMarca = gi_solicitud_rechazada
                     LET ws_out.codRechazo = gi_no_existe_solicitud
                     LET ws_out.desRechazo = 'Uno de los beneficiarios no existe'
                     LET v_errores = v_errores + 1
                  END IF 
               END IF 
            END FOR 
            IF v_errores = 0 THEN
               CALL fn_autoriza_solicitud(v_id_solicitud) RETURNING v_resultado
            END IF 
         END IF 
      END IF 
   END IF
END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_registro_solicitud
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS para la actualización de los porcentajes de los beneficiarios

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_registro_solicitud(p_nss, p_caso_crm)
DEFINE p_id_peticion                 DECIMAL(9,0),
       p_nss                         LIKE afi_derechohabiente.nss,
       p_caso_crm                    LIKE ret_solicitud_generico.caso_adai,
       v_r_ret_ws_peticion_act_benef RECORD LIKE ret_ws_peticion_act_benef.* -- registro de peticion al ws
		
   -- se obtiene el id de peticion nuevo
   SELECT seq_ret_ws_generico.nextVal
   INTO   p_id_peticion
   FROM   systables
   WHERE  tabid = 1
   
   -- se asignan los datos
   LET v_r_ret_ws_peticion_act_benef.id_peticion   = p_id_peticion
   LET v_r_ret_ws_peticion_act_benef.f_peticion    = TODAY
   LET v_r_ret_ws_peticion_act_benef.h_peticion    = CURRENT HOUR TO SECOND
   LET v_r_ret_ws_peticion_act_benef.nss           = p_nss
   LET v_r_ret_ws_peticion_act_benef.caso_crm      = p_caso_crm
   
   -- se inserta el registro de peticion
   INSERT INTO ret_ws_peticion_act_benef VALUES ( v_r_ret_ws_peticion_act_benef.* )

   -- se devuelve el id de la peticion
   RETURN p_id_peticion
END FUNCTION

{
======================================================================
Nombre: fn_registra_det_peticion_registro_solicitud
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Registra el detalle de los beneficiarios enviados para actualiozación del porcentaje

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_det_peticion_registro_solicitud(p_id_peticion, p_arr_det_benef)
DEFINE p_id_peticion                          DECIMAL(9,0), -- id de la peticion
       p_arr_det_benef  RECORD
           idBeneficiario SMALLINT,
           nombre         CHAR(40),
           ap_paterno     CHAR(40),
           ap_materno     CHAR(40),
           clabe_bancaria CHAR(18),
           porcentaje     SMALLINT 
       END RECORD, 
       v_consecutivo      SMALLINT,
       v_r_ret_ws_det_peticion_act_benef RECORD LIKE ret_ws_det_peticion_act_benef.* -- registro de detalle de peticion al ws

     -- Ya no se busca el consecutivo porque este se recibe como parámetro  
--   SELECT NVL(MAX(consecutivo),0) + 1
--   INTO   v_consecutivo
--   FROM   ret_ws_det_peticion_act_benef
--   WHERE  id_peticion = p_id_peticion;
   
   -- se asignan los datos
   LET v_r_ret_ws_det_peticion_act_benef.id_peticion      = p_id_peticion
   LET v_r_ret_ws_det_peticion_act_benef.consecutivo      = p_arr_det_benef.idBeneficiario
   LET v_r_ret_ws_det_peticion_act_benef.nombre           = p_arr_det_benef.nombre
   LET v_r_ret_ws_det_peticion_act_benef.ap_paterno       = p_arr_det_benef.ap_paterno
   LET v_r_ret_ws_det_peticion_act_benef.ap_materno       = p_arr_det_benef.ap_materno
   LET v_r_ret_ws_det_peticion_act_benef.clabe_bancaria   = p_arr_det_benef.clabe_bancaria
   LET v_r_ret_ws_det_peticion_act_benef.porcentaje       = p_arr_det_benef.porcentaje
         
   -- se inserta el registro de detalle de peticion
   INSERT INTO ret_ws_det_peticion_act_benef VALUES ( v_r_ret_ws_det_peticion_act_benef.* )
   
END FUNCTION

{
======================================================================
Nombre: fn_busca_solicitud
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Busca la solicitud de retiro a traves del NSS y el caso CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_solicitud(p_nss, p_caso_crm)
DEFINE p_nss                         LIKE afi_derechohabiente.nss,
       p_caso_crm                    LIKE ret_solicitud_generico.caso_adai,
       v_id_solicitud                DECIMAL(10,0),
       v_importe                     DECIMAL(14,2),
       v_aivs                        DECIMAL(14,2)
		
   LET v_id_solicitud = 0
      -- se obtiene el id de peticion nuevo
   SELECT NVL(a.id_solicitud,0), NVL(b.importe_viv92+importe_viv97+importe_viv97_anexo1,0),
          NVL(b.aivs_viv92+b.aivs_viv97,0)
   INTO   v_id_solicitud, v_importe, v_aivs
   FROM   ret_solicitud_generico a,
          ret_ley73_generico b
   WHERE  a.id_solicitud = b.id_solicitud
   AND    a.nss = p_nss
   AND    a.caso_adai = p_caso_crm
   AND    a.estado_solicitud = 10;

   RETURN v_id_solicitud, v_importe, v_aivs
END FUNCTION

{
======================================================================
Nombre: fn_busca_beneficiario
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Busca el beneficiario asociado a la solicitud utilizando el nombre y la cuenta clabe

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_beneficiario(p_id_solicitud, p_arr_beneficiario)
DEFINE p_id_solicitud      DECIMAL(10)
DEFINE p_arr_beneficiario    RECORD 
             idBeneficiario SMALLINT,
             nombre         CHAR(40),
             ap_paterno     CHAR(40),
             ap_materno     CHAR(40),
             clabe_bancaria CHAR(18),
             porcentaje     SMALLINT
       END RECORD 
DEFINE v_consecutivo        SMALLINT 

   LET v_consecutivo = 0
      -- se obtiene el id de peticion nuevo
   SELECT NVL(consec_beneficiario,0)
   INTO   v_consecutivo
   FROM   ret_beneficiario_generico
   WHERE  id_solicitud = p_id_solicitud
   AND    consec_beneficiario = p_arr_beneficiario.idBeneficiario;
--   AND    nombre       = p_arr_beneficiario.nombre
--   AND    ap_paterno   = p_arr_beneficiario.ap_paterno
--   AND    ap_materno   = p_arr_beneficiario.ap_materno;
   

   RETURN v_consecutivo
END FUNCTION

{
======================================================================
Nombre: fn_actualiza_porcentaje
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Actualiza el porcentaje de un beneficiario en particular

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega    Octubre 19, 2020    - PLAG135. Cuando el porcentaje del beneficiaro es cero, el estatus del beneficiario sera 100/rechazado
======================================================================
}
FUNCTION fn_actualiza_porcentaje(p_id_solicitud, p_consecutivo, p_porcentaje,p_importe_total,p_aivs_total)
DEFINE p_id_solicitud      DECIMAL(10)
DEFINE p_consecutivo       SMALLINT
DEFINE p_porcentaje        SMALLINT  
DEFINE p_importe_total     DECIMAL(14,2) 
DEFINE p_aivs_total        DECIMAL(14,2)
DEFINE v_resultado         SMALLINT
DEFINE v_es_spei           SMALLINT 


   LET v_resultado     = 0
      -- se obtiene el id de peticion nuevo
   UPDATE ret_beneficiario_generico
   SET    porcentaje       = p_porcentaje,
          importe          = p_importe_total * (p_porcentaje / 100),
          aivs             = p_aivs_total * (p_porcentaje / 100)
   WHERE  id_solicitud = p_id_solicitud
   AND    consec_beneficiario = p_consecutivo;

   IF p_porcentaje > 0 THEN 
      UPDATE ret_beneficiario_juridico
      SET    estado_solicitud = 15
      WHERE  id_solicitud = p_id_solicitud
      AND    consec_beneficiario = p_consecutivo;
   ELSE
      -- 20201019 PLAG135
      -- rechazo de derechohabiente por porcentaje cero
      UPDATE ret_beneficiario_juridico
      SET    estado_solicitud = 100
      WHERE  id_solicitud = p_id_solicitud
      AND    consec_beneficiario = p_consecutivo;      
   END IF 
   
   -- se debe actualizar la cuenta clabe para cada uno de los beneficiarios
   SELECT COUNT(*)
   INTO   v_es_spei
   FROM   ret_pago_spei
   WHERE  id_solicitud = p_id_solicitud
   AND    consec_beneficiario = p_consecutivo
   
   IF v_es_spei = 1 THEN 
      UPDATE ret_pago_spei
      SET    cuenta_clabe = ws_in.arr_beneficiario[p_consecutivo].clabeBancaria
      WHERE  id_solicitud = p_id_solicitud
      AND    consec_beneficiario = p_consecutivo
   ELSE 
      SELECT COUNT(*) 
      INTO   v_es_spei 
      FROM   ret_pago_siaf 
      WHERE  id_solicitud = p_id_solicitud 
      AND    consec_beneficiario = p_consecutivo
      IF v_es_spei = 1 THEN 
         UPDATE ret_pago_siaf
         SET    cuenta_clabe = ws_in.arr_beneficiario[p_consecutivo].clabeBancaria
         WHERE  id_solicitud = p_id_solicitud
         AND    consec_beneficiario = p_consecutivo
      END IF 
   END IF 

   RETURN v_resultado
END FUNCTION

{
======================================================================
Nombre: fn_autoriza_solicitud
Fecha creacion: Agosto 1, 2018
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Actualiza el estado de la solicitud a 15 - Autorizado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_autoriza_solicitud(p_id_solicitud)
DEFINE p_id_solicitud      DECIMAL(10)
DEFINE v_resultado         SMALLINT 

   LET v_resultado = 0
      -- se actualiza ret_solicitud_generico
   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 15    --- Autorizada
   WHERE  id_solicitud = p_id_solicitud;
   
      -- se actualiza ret_ley73_generico
   UPDATE ret_ley73_generico
   SET    estado_solicitud = 15    --- Autorizada
   WHERE  id_solicitud = p_id_solicitud;

   RETURN v_resultado
END FUNCTION
