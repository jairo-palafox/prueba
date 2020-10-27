--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS21                                                 #
#OBJETIVO          => WS SOLICITUD DE MARCAJE DE LA CUENTA POR TIPO RETIRO    #
#FECHA INICIO      => 24-SEP-2013                                             #
# Autor           Fecha      Modificación                                     #
# Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
#                 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
# Ricardo Perez   20140610   Solo se exige el RFC para el retiro de Fondo de Ahorro
#                            las modificaciones estan identificadas con ***** 
#                 20140611   Se valida la doble marca para los casos de Retiro ley 73
#                            verificando que en el caso de que exista una marca previa
#                            con estado solicitud = 8 permitira seguir con el proceso
###############################################################################


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
DEFINE ret_marcaje RECORD 
         nss            STRING,
         rfc            STRING,
         caso_crm       STRING, -- caso crm
         cuenta_clabe   STRING,   -------     *************************
         ind_marca      STRING, -- indicador de marca/desmarca/aprobacion/rechazo
         cod_rechazo    STRING, -- codigo de rechazo en caso de desmarca y rechazo
         medio_entrega  STRING  -- Indica el grupo que se marcará
       END RECORD
-- =======================================================
-- Record de envío de variables de respuesta
DEFINE ret_respuesta RECORD
         nss          STRING,
         rfc          STRING,
         est_marca    STRING,
         con_retiro   STRING,
         caso_crm     STRING,
         cod_rechazo  STRING,
         des_rechazo  STRING, ------ *********************************
         saldo_pesos  STRING,  
         cve_dap      STRING    -- este campo solo tendra valor cuando sea fondo de ahorro y el monto de retiro sea menor a 1000 pesos
       END RECORD

DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws

-- =======================================================
-- constantes para las respuesta y los mensajes que se escribirán en el log
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
MAIN

DEFINE v_resultado        INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log         STRING,
       v_cadena           STRING,
       v_ruta_ejecutable  VARCHAR(40)
      
  DEFER INTERRUPT

  -- se obtiene la ruta ejecutable
  SELECT ruta_listados
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
    
  -- se define la ruta del log
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETWS21."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
    -- se inicia el log del programa
  CALL STARTLOG(v_ruta_log)
  
  
   #
  # Check arguments
  #
  --fglrun RETW31 -S 
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_marcaje_FA(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET v_pantalla = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE030" ATTRIBUTES(TEXT = "Solcitud de marcaje ") --, STYLE="naked")
      --display_status("Retiro Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF


  -- se crea el servicio
  CALL ERRORLOG("Invoca creacion de servicio de solicitud de marcaje")
  CALL fn_crea_servicio_marcaje_FA(FALSE)

  -- se inicia el servidor
  DISPLAY "Iniciando servidor de Marcaje para retiro generico Version 140611..."

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
{
======================================================================
Clave: 
Nombre: fn_crea_servicio_marcaje
Fecha creacion: Septiembre 24, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Crea el servicio web para solicitud de marcaje por tipo de retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_crea_servicio_marcaje_FA(generaWSDL)

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
    LET serv = com.WebService.CreateWebService("retiroMarcaFA", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_marcaje_fa","fn_marcaje_fa",ret_marcaje,ret_respuesta)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL serv.publishOperation(op, "fn_marcaje_fa")

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
       CALL ERRORLOG("Se registro el servicio sol_marca")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Solicitudes de marcaje':" || STATUS)
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
Nombre: fn_marcaje_fa
Fecha creacion: Septiembre 24, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Solicita que se marque la cuenta por tipo de retiro


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     11 Oct 2013             - El servicio recibe dos valores mas para la marca,
                                        donde uno aprueba la solicitud y el otro la rechaza
                                        ademas de la marca y desmarca
                                      - Tambien recibira el caso ADAI y generara la solicitud
                                        desde un inicio con el caso
======================================================================
}
FUNCTION fn_marcaje_fa()
DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, --Identificador del derechohabiente
       v_longitud           SMALLINT, --Longitud del arreglo de llegada
       v_indice             SMALLINT, --Indice del arreglo
       v_error_en_datos     SMALLINT -- booleana que indica que hubo error en datos

   -- se verifica si se esta solicitando un eco
   IF ( UPSHIFT(ret_marcaje.nss CLIPPED) = "ECO" ) THEN
      LET ret_respuesta.nss = "ECO"
      LET ret_respuesta.rfc = "ECO"

      -- se devuelve ECO indicando que el servicio esta respondiendo correctamente
      CALL fn_respuesta_wsmarcaje(1, 0, 1, 0, 0, "ECO", 0, 0, 0)
   ELSE
      -- se procesa la solicitud
      -- Asgina valores de entrada
      LET ret_respuesta.nss = ret_marcaje.nss
      LET ret_respuesta.rfc = ret_marcaje.rfc
      
      -- se crea el registro de la peticion de solicitud de ws de marca
      CALL fn_registra_peticion_marca(0, ret_marcaje.nss, ret_marcaje.rfc, ret_marcaje.caso_crm, ret_marcaje.cuenta_clabe, 0, 0)
           RETURNING g_id_peticion
      
      -- se verifica que se hayan recibido los datos
      -- ***** se modifica la validacion, se omite el rfc y solo se validara para los retiros de Fondo de Ahorro
      -- ***** RPR 140610
      IF ( ret_marcaje.nss IS NULL OR
           ret_marcaje.caso_crm IS NULL OR 
           ret_marcaje.medio_entrega IS NULL OR
           ret_marcaje.ind_marca IS NULL ) THEN
         -- es un error
         CALL fn_respuesta_wsmarcaje(1, 0, 2, 0, gi_datos_incompletos, ret_marcaje.caso_crm, 0, 0, 0)
         RETURN
      ELSE
         LET v_error_en_datos = FALSE
         CALL fn_registra_detalle_peticion_marca(g_id_peticion,
                                                 2,
                                                 ret_marcaje.ind_marca,
                                                 ret_marcaje.cod_rechazo,
                                                 0, 0, 0, 0)
         -- Valida modalidad de retiro
         IF ( ret_marcaje.ind_marca IS NULL ) THEN 
            -- se marca que hubo error en la validacion de datos
            LET v_error_en_datos = TRUE
         END IF
         IF (ret_marcaje.medio_entrega = "1" OR
             ret_marcaje.medio_entrega = "2") AND 
            ret_marcaje.ind_marca = "1" THEN  
            IF ( NOT fn_verifica_estructura_clabe(ret_marcaje.cuenta_clabe) )  THEN
               LET v_error_en_datos = TRUE
               CALL fn_respuesta_wsmarcaje(1, 2, 2, 0, gi_esctructura_cta_clabe_incorrecta, ret_marcaje.caso_crm, 0, 0, 0)
               RETURN
            END IF
         END IF 
         IF ret_marcaje.medio_entrega = "0" OR
            ret_marcaje.medio_entrega IS NULL THEN 
            LET v_error_en_datos = TRUE
         END IF 
         -- si hubo error en datos, se finaliza la peticion
         IF ( v_error_en_datos ) THEN
            CALL fn_respuesta_wsmarcaje(1, 2, 2, 0, gi_datos_incompletos, ret_marcaje.caso_crm, 0, 0, 0)
            RETURN
         END IF
     
      END IF
      
      -- Obtiene el identificador del derechohabiente
      LET v_id_derechohabiente = fn_obtiene_id_derechohabiente(ret_marcaje.nss, ret_marcaje.rfc, 0)
      
         	
      DISPLAY "Procesando solicitud de marcado de fondo de ahorro"
      
       -- Modalidad fondo de ahorro
      LET v_indice = 1
      CALL fn_aplica_marca_fondo_ahorro(v_indice,
                                        ret_marcaje.nss,
                                        ret_marcaje.rfc,
                                        ret_marcaje.caso_crm,
                                        ret_marcaje.ind_marca,
                                        ret_marcaje.medio_entrega)
      
   END IF   
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_aplica_marca_fondo_ahorro
Fecha creacion: Septiembre 24, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Aplica el marcaje para la modalidad de fondo de ahorro


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_aplica_marca_fondo_ahorro(p_indice, p_nss, p_rfc, p_caso_crm, p_marca,p_medio_entrega)
DEFINE p_indice                SMALLINT,--Indice que contendrá el registro de respuesta
       p_nss                   CHAR(11), --NSS del registro
       p_rfc                   CHAR(13), --Clave de RFC del registro
       p_caso_crm             CHAR(10), -- caso de ADAI
       p_marca                 SMALLINT, --Indicador de marca
       p_medio_entrega         SMALLINT,
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente, --Identificador del derechohabiente
       v_id_afi_fondo72        LIKE afi_fondo72.id_afi_fondo72,    --Identificador del fondo 72
       v_sql                   STRING, --Cadena de ejecución de los querys
       v_marca_entra           SMALLINT,
       v_n_referencia          INTEGER ,
       v_folio                 DECIMAL(9,0),
       v_estado_marca          SMALLINT,
       v_codigo_rechazo        SMALLINT,
       v_cod_inconsistencia    SMALLINT,
       v_marca_causa           SMALLINT,
       v_fecha_causa           DATE,
       v_usuario               CHAR(20),
       v_proceso_cod           SMALLINT,
       v_respuesta_marcaje     SMALLINT,
       v_id_solicitud          DECIMAL(9,0),
       v_estatus_marca         SMALLINT,
       v_modalidad             SMALLINT,
       v_res_actualizacion     SMALLINT,
       v_existe_solicitud      SMALLINT,
       v_ocurrencias           INTEGER,
       v_val_clabe             SMALLINT,
       v_existe_dap            SMALLINT,
       v_ref_dap               CHAR(12)
       
   -- Asigna valor de la modalidad
   LET v_modalidad    = 2
   LET v_n_referencia = NULL
   LET v_ref_dap      = ""
   -- se obtiene el id_derechohabiente 
   SELECT COUNT(*) 
   INTO   v_ocurrencias
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    ind_estado_cuenta = 0  -- cuenta Activa
   
   IF v_ocurrencias <> 1 THEN 
      -- Problemas en la marca, el nss se encuentra mas de una vez o no existe.
      LET v_id_derechohabiente = NULL
      LET v_id_afi_fondo72     = NULL 
   ELSE 
      SELECT id_derechohabiente,
             id_afi_fondo72
      INTO   v_id_derechohabiente,
             v_id_afi_fondo72
      FROM   afi_fondo72
      WHERE  nss = p_nss
      AND    ind_estado_cuenta = 0  -- cuenta Activa
      --   AND    rfc = p_rfc
   END IF  
   
   --Valida que exista el nss y rfc
   IF ( v_id_afi_fondo72 IS NOT NULL ) THEN

      -- Si no se encuentra el NSS en vivienda
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
            UPDATE afi_fondo72
            SET    id_derechohabiente = v_id_derechohabiente
            WHERE  id_afi_fondo72     = v_id_afi_fondo72;
         END IF
      
      END IF

      -- Asigna variables para la ejecución del store
      LET v_marca_entra    = 802
      LET v_folio          = "0"
      LET v_estado_marca   = "0"
      LET v_codigo_rechazo = "0"
      LET v_marca_causa    = "0"
      LET v_fecha_causa    = NULL
      LET v_usuario        = "safreviv"
      LET v_proceso_cod    = g_proceso_cod_ret_fondo_ahorro
         
      -- se verifica si se desea marcar/desmarcar o aprobar/rechazar
      CASE p_marca
         WHEN 1 -- marcar cuenta
         
      	    -- Consulta si existe  otra solicitud para el derechohabiente 
            CALL fn_verifica_solicitud_generico(v_id_derechohabiente,v_modalidad,1)
                 RETURNING v_existe_solicitud, v_n_referencia
      		  
            -- Valida que no exista otra solicitud 
            IF ( NOT v_existe_solicitud ) THEN
               -- Asigna estatus de la marca
               LET v_estatus_marca = 8

               -- Se obtiene el número de solicitud
               SELECT seq_ret_solicitud.NEXTVAL
               INTO   v_n_referencia
               FROM   systables 
               WHERE  tabid = 1
      		  	
      		  	 --Actualiza tabla de retiro genérico   
               CALL fn_actualiza_retiro_generico(v_id_derechohabiente, p_nss, p_rfc, v_modalidad, v_n_referencia,
                                                 v_folio, v_estatus_marca, v_codigo_rechazo, 1)
                    RETURNING v_res_actualizacion
                           
               -- Verifica que no haya existido un error en la actualización de la tabla
               IF ( v_res_actualizacion ) THEN
                  --- Deja huella del medio de entrega
                  CALL fn_medio_entrega(v_n_referencia, 0, p_medio_entrega);                      	
                  -- se marca la cuenta
                  CALL fn_ret_generico_marca_cuenta(v_id_derechohabiente,
                                                    v_marca_entra       ,
                                                    v_n_referencia      ,
                                                    v_folio             , 
                                                    v_estado_marca      ,
                                                    v_codigo_rechazo    ,
                                                    v_marca_causa       ,
                                                    v_fecha_causa       ,
                                                    v_usuario           ,
                                                    v_proceso_cod       )
      		           RETURNING v_respuesta_marcaje
      	                    
      	         -- Valida que la respuesta sea correcta
      	         IF ( v_respuesta_marcaje = 0 ) THEN
      	          	
      	            -- Se ejecutó correctamente el store, se indica que se marcó la cuenta
      	            LET v_estatus_marca = gi_estatus_marca_existoso
      	             
      	            --Se indica código de rechazo 0, para mostrar que la ejecución fué correcta
      	            LET v_cod_inconsistencia = 0
      	         ELSE
                     --Sucedió un error al ejecutar la función de marcado
                     --Se elimina el registro creado
                     CALL ERRORLOG("El error regresado en la marca es >" || v_respuesta_marcaje || "<")
                     CALL fn_actualiza_retiro_generico(v_id_derechohabiente,p_nss,p_rfc,v_modalidad,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,3)
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
              	
              	   -- no se pudo marcar
                  LET v_estatus_marca = gi_estatus_marca_no_exitoso
      	          
      	         -- error al generar la solicitud en ret_solicitud_generico en WS de marca
      	         LET v_cod_inconsistencia = gi_error_marca_generar_solicitud
               END IF
      	             	            
      	   ELSE
               -- Se solicitó marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
               LET v_estatus_marca = 0
               
               -- ya existe una solicitud en tramite
               LET v_cod_inconsistencia = gi_solicitud_en_tramite
               
               -- se obtiene el caso adai original
               SELECT caso_adai
               INTO   p_caso_crm
               FROM   ret_solicitud_generico
               WHERE  id_solicitud = v_n_referencia
      	   END IF      
         
         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         --          DESMARCAR CUENTA
         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         WHEN 2
            -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
            CALL fn_verifica_solicitud_generico(v_id_derechohabiente, v_modalidad,2)
                 RETURNING v_existe_solicitud, v_n_referencia
         
            --	Valida que no se haya solicitado una marcación antes de una desmarcación
            IF ( v_existe_solicitud ) THEN

               -- Asigna estatus de la marca
               LET v_estatus_marca = 100
                           
                 -- Actualiza tabla de retiro genérico   
               CALL fn_actualiza_retiro_generico(v_id_derechohabiente,p_nss,p_rfc,v_modalidad,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,2)
                    RETURNING v_res_actualizacion
                
               IF ( v_res_actualizacion ) THEN 
                  --Si la ejecución fué correcta se envía solicitud de desmarcado
                        
                  --Se ejecutó correctamente el store, se indica que se desmarco la cuenta
                  LET v_estatus_marca = gi_estatus_marca_existoso
                     
                  --Actualiza codigo de rechazo
                  LET v_cod_inconsistencia = 0  		                            
               ELSE
                  -- Existió un error en la integración de retiro genérico
                  LET v_estatus_marca = gi_estatus_marca_no_exitoso                                     
                      
                  -- hubo un error al ejecutar la operacion de desmarca
                  LET v_cod_inconsistencia = gi_error_interno_marca            	
               END IF
            ELSE
               --Se solicitó marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
            
               -- no existe una solicitud que desmarcar
               LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         	
            END IF                                    
                   
         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         --          APROBAR SOLICITUD
         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         WHEN 3
            -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
            CALL fn_verifica_solicitud_generico(v_id_derechohabiente, v_modalidad, 3)
                 RETURNING v_existe_solicitud, v_n_referencia
            
            --	Valida que no se haya solicitado una marcación antes de una desmarcación
            IF ( v_existe_solicitud ) THEN
               --- Valida la CLABE Bancaria contra los beneficiarios
               CALL fn_valida_clabe_bancaria(v_id_derechohabiente, v_n_referencia, v_modalidad) RETURNING v_val_clabe
               IF v_val_clabe = 0 THEN 
                  -- aprobacion de solicitud
                  LET v_estatus_marca = 15
                  -- Recupera la referencia DAP si existe
                  SELECT COUNT(*)
                  INTO   v_existe_dap
                  FROM   ret_pago_dap
                  WHERE  id_solicitud = v_n_referencia
                  IF v_existe_dap <> 0 THEN 
                     SELECT cve_referencia 
                     INTO   v_ref_dap
                     FROM   ret_pago_dap
                     WHERE  id_solicitud = v_n_referencia
                  END IF 
                  -- Actualiza tabla de retiro genérico   
                  CALL fn_actualiza_retiro_generico(v_id_derechohabiente, p_nss, p_rfc, v_modalidad, v_n_referencia, 
                                                  v_folio, v_estatus_marca, 0, 2)
                       RETURNING v_res_actualizacion
                     
                  -- se indica que la aprobacion fue exitosa
                  LET v_estatus_marca = gi_estatus_marca_existoso

                  -- sin error
                  LET v_cod_inconsistencia = 0
               ELSE 
                  LET v_estatus_marca = gi_estatus_marca_no_exitoso
             
                  -- Problemas con la CLABE BANCARIA
                  LET v_cod_inconsistencia = gi_esctructura_cta_clabe_incorrecta
               END IF
            ELSE
               -- no se pudo aprobar
               LET v_estatus_marca = gi_estatus_marca_no_exitoso

               -- no existe una solicitud para aprobar
               LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
            END IF


         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         --          RECHAZAR SOLICITUD
         -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         WHEN 4
            -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
            CALL fn_verifica_solicitud_generico(v_id_derechohabiente, v_modalidad, 4)
                 RETURNING v_existe_solicitud, v_n_referencia
            
            --	Valida que no se haya solicitado una marcación antes de una desmarcación
            IF ( v_existe_solicitud ) THEN
               -- rechazo de solicitud
               LET v_estatus_marca = 100
            
                 -- Actualiza tabla de retiro genérico   
               CALL fn_actualiza_retiro_generico(v_id_derechohabiente, p_nss, p_rfc, v_modalidad, v_n_referencia, 
                                                 v_folio, v_estatus_marca,
                                                 ret_marcaje.cod_rechazo, 2)
                    RETURNING v_res_actualizacion
                    
                 -- se indica que el rechazo de solicitud fue exitoso
               LET v_estatus_marca = gi_estatus_marca_existoso
            
               -- sin error
               LET v_cod_inconsistencia = 0
            ELSE
                 -- Se solicitó marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
            
               -- no existe una solicitud para rechazar con esos parametros
               LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
            END IF

                   
         OTHERWISE
            --Se recibió un indicador de marcaje desconocido
            CALL ERRORLOG("Se recibió un indicador de marcado desconocido")
            --Se envía mensaje de error para indicar que no se pudo marcar
            LET v_estatus_marca = gi_estatus_marca_no_exitoso
            
            -- el indicador de marca no es valido
            LET v_cod_inconsistencia = gi_indicador_marca_invalido

      END CASE

   ELSE
      --No existe el derechohabiente con NSS y RFC	
      --Se envía mensaje de error para indicar que no se pudo marcar
      LET v_estatus_marca = gi_estatus_marca_no_exitoso
      IF v_ocurrencias > 1 THEN 
         LET v_cod_inconsistencia = gi_mas_de_un_registro
      ELSE 
         --Se envía código de error que indica que el derehohabiente no existe
         LET v_cod_inconsistencia = gi_nss_rfc_no_existe
      END IF 
   END IF
   
   -- Invoca función de respuesta del WS
   CALL fn_respuesta_wsmarcaje(p_indice,v_modalidad,v_estatus_marca,v_n_referencia,v_cod_inconsistencia, p_caso_crm, 0, 0, v_ref_dap)

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_obtiene_id_derechohabiente
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Obtiene el id_derechohabiente de la tabla de afi_derechohabiente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtiene_id_derechohabiente(p_nss, p_rfc, p_modalidad)
DEFINE p_nss                   CHAR(11),
       p_rfc                   CHAR(13),
       p_modalidad             SMALLINT,
       v_sql                   STRING,
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente
       
    --Se arma el query de la consulta
    LET v_sql = "\n SELECT id_derechohabiente",
              "\n FROM afi_derechohabiente ",
              "\n WHERE nss               = '", p_nss, "'",
              "\n AND   ind_estado_cuenta = 0 "   -- cuenta Activa

            
    PREPARE stm_derechohabiente FROM v_sql
    EXECUTE stm_derechohabiente INTO v_id_derechohabiente       

    IF p_modalidad = 9 AND v_id_derechohabiente IS NULL THEN 
        LET v_sql = "\n SELECT af.id_derechohabiente                         ",
                    "\n FROM   sfr_marca_activa sm,                          ",
                    "\n afi_derechohabiente af                               ",
                    "\n WHERE  sm.id_derechohabiente = af.id_derechohabiente ",
                    "\n AND    sm.marca = 151                                ", 
                    "\n AND    af.nss = '", p_nss, "'"
        PREPARE stm_derechohabiente_unif FROM v_sql
        EXECUTE stm_derechohabiente_unif INTO v_id_derechohabiente       
    END IF 
  
  --Envía el id_derechohabiente consultado
  RETURN v_id_derechohabiente 

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_actualiza_retiro_generico
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico


Registro de modificaciones:
Autor         Fecha       Descrip. cambio
Ivan Vega     26 Nov 2013  - La solicitud se crea con grupo de ventanilla
                             infonavit
Eneas Armas   20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
              20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
======================================================================
}
FUNCTION fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad_retiro, p_id_solicitud,
                                      p_folio, p_estado_solicitud, p_cod_rechazo, p_tipo_actualizacion)
DEFINE p_id_derechohabiente       DECIMAL(9,0),
       p_nss                      CHAR(11),
       p_rfc                      CHAR(13),
       p_modalidad_retiro         SMALLINT,
       p_id_solicitud             DECIMAL(9,0),
       p_folio                    DECIMAL(9,0),
       p_estado_solicitud         SMALLINT,
       p_cod_rechazo              SMALLINT,
       p_tipo_actualizacion       SMALLINT, --Variable que recibe el tipo de actualización(INSERT,UPDATE,DELETE)
       v_sql                      STRING,
       v_respuesta                SMALLINT,
       v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud
   
   -- Valida el tipo de actualización 1-Inserción, 2-Actualización
   CASE p_tipo_actualizacion
   	
      WHEN 1
   		   -- Arma el query INSERCIÓN
         LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
         LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
         LET v_r_ret_solicitud_generico.nss                    = p_nss
         LET v_r_ret_solicitud_generico.rfc                    = p_rfc
         LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
         LET v_r_ret_solicitud_generico.folio                  = p_folio
         LET v_r_ret_solicitud_generico.caso_adai              = ret_marcaje.caso_crm
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
            CALL ERRORLOG("Se ha producido un error al insertar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha insertado con éxito el registro en retiro genérico")
         END IF
                                            
                
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
         	  CALL ERRORLOG("Se ha producido un error al actualizar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha actualizado con éxito el registro en retiro genérico")
            
            -- se actualiza la tabla de historicos con el estado de solicitud y cogido de rechazo segun la modalidad
            -- 20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
            -- 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
            CASE p_modalidad_retiro
               WHEN 2 -- fondo de ahorro               
                  UPDATE ret_fondo_ahorro_generico
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 802, 
                                                          p_id_solicitud, 802,
                                                          "safreviv", g_proceso_cod_ret_fondo_ahorro)
                  END IF
               
               WHEN 3 -- ley 73
                  UPDATE ret_ley73_generico
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 803,
                                                          p_id_solicitud, 803,
                                                          "safreviv", g_proceso_cod_ret_ley73_ws)
                  END IF
               
               WHEN 9 -- amortizaciones excedentes
                  UPDATE ret_amort_excedente
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 810, 
                                                          p_id_solicitud, 810,
                                                          "safreviv", g_proceso_cod_ret_amort_excedentes)
                  END IF

               
               WHEN 10 -- aportaciones voluntarias
                  UPDATE ret_voluntaria
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 809, 
                                                          p_id_solicitud, 809,
                                                          "safreviv", g_proceso_cod_ret_aport_voluntarias)
                  END IF
            END CASE
         END IF
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
{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_generico
Fecha creacion: Septiembre 27, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Verifica que no exista una solicitud en la tabla de retiro genérico

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     26 Nov 2013             - La discriminacion del registro se hace
                                        por grupo de ventanilla infonavit
======================================================================
}
FUNCTION fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad_retiro,p_accion)
DEFINE v_sql STRING,
       p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_modalidad_retiro   SMALLINT,
       p_accion             SMALLINT,
       v_ban_existe         SMALLINT,
       v_num_solicitud      DECIMAL(9,0)
       
       
   -- Inicializa valores
   LET v_ban_existe= FALSE    
 
 
   -- Valida el tipo de acción
   CASE p_accion
   	
      WHEN 1 
   	 	   -- Marcar 		  
         LET v_sql = "\n SELECT id_solicitud         ",
                     "\n FROM ret_solicitud_generico ",
                     "\n WHERE id_derechohabiente =  ", p_id_derechohabiente,
                     "\n AND modalidad_retiro     =  ", p_modalidad_retiro,
                     "\n AND estado_solicitud IN (   ",
                     "\n 8, 10, 15, 50, 60, 70, 71,  ", -- precaptura, captura, aprobacion, preliq., liquid., enviada fico, conf. pago
                     "\n 90, 91, 209, 210, 211, 212, ", -- rch fico, rechazo banco, cancelacion CxP, 
                     "\n 213 )" -- restitucion
   	
      WHEN 2
         -- Desmarcar
         LET v_sql = "\n SELECT id_solicitud ",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ", p_modalidad_retiro,
                     "\n AND estado_solicitud     IN (8,10) "
       
      WHEN 3
         -- Aprobar solicitud
         LET v_sql = "\n SELECT id_solicitud ",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro,
                     "\n AND estado_solicitud     = 10"
                     
      WHEN 4 
         -- Rechazar la solicitud
         LET v_sql = "\n SELECT id_solicitud",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro,
                     "\n AND estado_solicitud     = 10"
   
   
   END CASE
   
   DISPLAY v_sql
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
{
======================================================================
Clave: 
Nombre: fn_respuesta_wsmarcaje
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico
Asigna los valores de retorno del Web Service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_wsmarcaje(p_indice, p_modalidad, p_estatus_marca, p_id_solicitud, p_cod_rechazo, p_caso_crm, p_saldo_aivs_total, p_saldo_total, p_ref_dap)
DEFINE p_indice           SMALLINT,
       p_modalidad        SMALLINT,
       p_estatus_marca    SMALLINT,
       p_id_solicitud     DECIMAL(9,0),
       p_cod_rechazo      SMALLINT,
       p_caso_crm        LIKE ret_solicitud_generico.caso_adai, -- caso adai
       p_saldo_aivs_total DECIMAL(24,6),
       p_saldo_total      DECIMAL(22,6),
       p_ref_dap          DECIMAL(9,0),
       v_desc_rechazo     CHAR(100)
       
   --Asigna valores al arreglo
   --LET ret_respuesta.caso_crm = p_caso_crm
   LET ret_respuesta.est_marca   = p_estatus_marca
   LET ret_respuesta.con_retiro  = p_id_solicitud
   LET ret_respuesta.cod_rechazo = p_cod_rechazo
   LET ret_respuesta.caso_crm    = p_caso_crm
   LET ret_respuesta.des_rechazo = " ";
   LET ret_respuesta.saldo_pesos = p_saldo_total
   LET ret_respuesta.cve_dap     = p_ref_dap

   IF p_cod_rechazo <> 0 THEN
      -- Busca la descripcion del error para regresarla en la consulta
      LET ret_respuesta.des_rechazo = "";
      LET v_desc_rechazo = ""
      SELECT des_larga
      INTO   v_desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = p_cod_rechazo;
      IF v_desc_rechazo IS NULL THEN
         LET ret_respuesta.des_rechazo = " ";
      ELSE
         LET ret_respuesta.des_rechazo = v_desc_rechazo
      END IF
   END IF    
   
   -- se actualiza el historico de la peticion
   CALL fn_registra_peticion_marca(g_id_peticion, ret_marcaje.nss, ret_marcaje.rfc, ret_marcaje.cuenta_clabe, ret_marcaje.caso_crm, p_cod_rechazo, 1)
        RETURNING g_id_peticion
   
   -- se actualiza el detalle de la peticion
   CALL fn_registra_detalle_peticion_marca(g_id_peticion, p_modalidad,
                                           ret_marcaje.ind_marca,
                                           ret_marcaje.cod_rechazo,
                                           p_estatus_marca, p_id_solicitud, p_cod_rechazo, 1)
   
END FUNCTION

{
======================================================================
Nombre: fn_registra_peticion_marca
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos de entrada y respuesta que se recibieron/enviaron de
una peticion de WS de marca 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_peticion_marca(p_id_peticion, p_nss, p_rfc, p_caso_crm, p_cuenta_clabe, p_res_ejecucion, p_accion)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_rfc                     LIKE afi_derechohabiente.rfc,
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
      LET v_r_ret_ws_peticion_marca.rfc           = p_rfc
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


{
======================================================================
Nombre: fn_registra_detalle_peticion_marca
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Registra los datos que se recibieron para generar el detalle de marca
de una solicitud de marca por ws

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_registra_detalle_peticion_marca(p_id_peticion, p_modalidad_retiro, p_ind_marca, p_cod_rechazo,
                                            p_resp_estado_marca, p_resp_id_solicitud, p_resp_cod_rechazp, p_accion)
DEFINE p_id_peticion                 DECIMAL(9,0),
       p_modalidad_retiro            LIKE ret_modalidad_retiro.modalidad_retiro,
       p_ind_marca                   SMALLINT,
       p_cod_rechazo                 LIKE ret_rechazo.cod_rechazo,
       p_resp_estado_marca           SMALLINT, -- resultado de la marca
       p_resp_id_solicitud           LIKE ret_solicitud_generico.id_solicitud, 
       p_resp_cod_rechazp            LIKE ret_rechazo.cod_rechazo,
       p_accion                      SMALLINT, -- 0: nuevo registro, 1: actualiza
       v_r_ret_ws_det_peticion_marca RECORD LIKE ret_ws_det_peticion_marca.* -- registro de detalle de peticion WS de marca
		
   -- si se trata de nuevo registro
   IF ( p_accion = 0 ) THEN
      -- se asignan los datos
      LET v_r_ret_ws_det_peticion_marca.id_peticion       = p_id_peticion
      LET v_r_ret_ws_det_peticion_marca.modalidad_retiro  = p_modalidad_retiro
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
      AND    modalidad_retiro  = p_modalidad_retiro
   END IF
END FUNCTION


FUNCTION fn_valida_clabe_bancaria(p_id_derechohabiente,p_id_solicitud, p_v_modalidad)
DEFINE p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_id_solicitud       LIKE  ret_solicitud_generico.id_solicitud,
       p_v_modalidad        SMALLINT,
       v_res_valida         SMALLINT,
       v_cant_sol           SMALLINT,
       v_cuenta_clabe       CHAR(18)
       
    -- Inicializa valores
    LET v_res_valida = 0    
    IF ret_marcaje.cuenta_clabe IS NULL AND p_v_modalidad <> 2 THEN 
        LET v_res_valida = 1
    ELSE

        SELECT b.c_siaf + a.c_spei 
        INTO   v_cant_sol
        FROM   (SELECT COUNT(*) AS c_spei
                FROM   (SELECT DISTINCT cuenta_clabe 
                        FROM   ret_pago_spei
                        WHERE  id_solicitud IN (SELECT id_solicitud 
                                                FROM   ret_solicitud_generico
                                                WHERE  id_solicitud       = p_id_solicitud
                                                AND    id_derechohabiente = p_id_derechohabiente))) a,
               (SELECT COUNT(*) AS c_siaf
                FROM   (SELECT DISTINCT cuenta_clabe
                        FROM   ret_pago_siaf
                        WHERE  id_solicitud in (SELECT id_solicitud 
                                                FROM   ret_solicitud_generico
                                                WHERE  id_solicitud       = p_id_solicitud
                                                AND    id_derechohabiente = p_id_derechohabiente))) b
        IF v_cant_sol > 1 THEN 
            LET v_res_valida = 1
        ELSE 
            IF v_cant_sol = 1 THEN 
                SELECT DISTINCT cuenta_clabe
                INTO   v_cuenta_clabe
                FROM   ret_pago_spei
                WHERE  id_solicitud IN (
                       SELECT id_solicitud 
                       FROM   ret_solicitud_generico
                       WHERE  id_solicitud = p_id_solicitud
                       AND    id_derechohabiente = p_id_derechohabiente)
                IF v_cuenta_clabe IS NULL THEN 
                    SELECT DISTINCT cuenta_clabe
                    INTO   v_cuenta_clabe
                    FROM   ret_pago_siaf
                    WHERE  id_solicitud IN (
                           SELECT id_solicitud 
                           FROM   ret_solicitud_generico
                           WHERE  id_solicitud = p_id_solicitud
                           AND    id_derechohabiente = p_id_derechohabiente)
                END IF 
                
                IF v_cuenta_clabe <> ret_marcaje.cuenta_clabe THEN 
                    LET v_res_valida = 1
                ELSE 
                    LET v_res_valida = 0
                END IF 
            ELSE
                IF p_v_modalidad = 2 THEN 
                    LET v_res_valida = 0
                ELSE 
                    LET v_res_valida = 1
                END IF 
            END IF 
        END IF 
    END IF 
  
   -- se devuelve el resultado de la creacion
   RETURN v_res_valida
END FUNCTION   

FUNCTION fn_medio_entrega(pe_id_solicitud, pe_grupo, pe_medio_entrega)
DEFINE pe_id_solicitud     DECIMAL(10,0)
DEFINE pe_grupo            SMALLINT
DEFINE pe_medio_entrega    SMALLINT 

   IF (pe_id_solicitud  IS NOT NULL AND 
       pe_grupo         IS NOT NULL AND
       pe_medio_entrega IS NOT NULL ) THEN 
       INSERT INTO ret_sol_medio_entrega (id_solicitud, grupo, medio_entrega,f_registro)
            VALUES (pe_id_solicitud, pe_grupo, pe_medio_entrega, CURRENT YEAR TO MINUTE);
   END IF 

END FUNCTION                