--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW31                                                  #
#OBJETIVO          => WS SOLICITUD DE MARCAJE DE LA CUENTA POR TIPO RETIRO    #
#FECHA INICIO      => 24-SEP-2013                                             #
# Autor           Fecha      Modificaci�n                                                    #
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
-- Record de recepci�n de valores en las variables
DEFINE ret_marcaje RECORD 
         nss            CHAR(11),
         rfc            CHAR(13),
         caso_adai      CHAR(10), -- caso adai
         cuenta_clabe   CHAR(18),   -------     *************************
         arr_mod_retiro DYNAMIC ARRAY OF RECORD    
           mod_retiro    SMALLINT, -- modalidad del retiro
           ind_marca     SMALLINT, -- indicador de marca/desmarca/aprobacion/rechazo
           cod_rechazo   SMALLINT, -- codigo de rechazo en caso de desmarca y rechazo
           grupo_ley73   SMALLINT  -- Indica el grupo que se marcar�
         END RECORD
       END RECORD
-- =======================================================
-- Record de env�o de variables de respuesta
DEFINE ret_respuesta RECORD
         nss    CHAR(11),
         rfc    CHAR(13),
         arr_mod_retiro_resp DYNAMIC ARRAY OF RECORD
           mod_retiro   SMALLINT,
           est_marca    SMALLINT,
           con_retiro   INTEGER ,
           cod_rechazo  SMALLINT,
           des_rechazo  CHAR(100), ------ *********************************
           saldo_aivs   DECIMAL(14,2),  -- este campo solo se devueve cuando es ley 73 y grupo 1,2,3 o 4
           saldo_pesos  DECIMAL(14,2),  -- este campo solo se devueve cuando es ley 73 y grupo 1,2,3 o 4
           cve_dap      DECIMAL(9,0)    -- este campo solo tendra valor cuando sea fondo de ahorro y el monto de retiro sea menor a 1000 pesos
         END RECORD
       END RECORD

DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws

-- =======================================================
-- constantes para las respuesta y los mensajes que se escribir�n en el log
CONSTANT  g_res_procesada                    SMALLINT = 0  ,
          g_res_sin_solicitud                SMALLINT = -1 ,
          g_res_desconectado_del_servidor    SMALLINT = -2 ,
          g_res_conexion_con_cliente_perdida SMALLINT = -3 ,
          g_res_servidor_interrumpido_ctrl_c SMALLINT = -4 ,
          g_res_error_interno                SMALLINT = -10,
          g_msg_procesada                    STRING = "Solicitud procesada"                  ,
          g_msg_sin_solicitud                STRING = "Sin solicitud"                        ,
          g_msg_desconectado_del_servidor    STRING = "Desconectado del servidor"            ,
          g_msg_conexion_con_cliente_perdida STRING = "Se perdi� la conexi�n con el cliente" ,
          g_msg_servidor_interrumpido_ctrl_c STRING = "Se interrumpi� el servidor con CTRL-C",
          g_msg_error_interno                STRING = "Ocurri� un error interno"
          
END GLOBALS
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
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW31."
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
      CALL fn_crea_servicio_marcaje(TRUE)
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
  CALL fn_crea_servicio_marcaje(FALSE)

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
                DISPLAY "Se recibi� otro codigo de retorno" TO msg
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
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Crea el servicio web para solicitud de marcaje por tipo de retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_crea_servicio_marcaje(generaWSDL)

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
    LET serv = com.WebService.CreateWebService("sol_marca", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    -- fn_retiro 
    LET op = com.WebOperation.CreateDOCStyle("fn_marcaje","fn_marcaje",ret_marcaje,ret_respuesta)
    --LET op = com.WebOperation.CreateDOCStyle("fn_retiro","fn_retiro",ret_retiro_fondo,ret_respuesta)
    CALL serv.publishOperation(op, "fn_marcaje")

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
Nombre: fn_marcaje
Fecha creacion: Septiembre 24, 2013
Autor: Esteban S�nchez Zepeda, EFP
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
FUNCTION fn_marcaje()
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
      CALL fn_registra_peticion_marca(0, ret_marcaje.nss, ret_marcaje.rfc, ret_marcaje.caso_adai, ret_marcaje.cuenta_clabe, 0, 0)
           RETURNING g_id_peticion
      
      -- se verifica que se hayan recibido los datos
      -- ***** se modifica la validacion, se omite el rfc y solo se validara para los retiros de Fondo de Ahorro
      -- ***** RPR 140610
      IF ( ret_marcaje.nss IS NULL OR
           ret_marcaje.caso_adai IS NULL ) THEN
         -- es un error
         CALL fn_respuesta_wsmarcaje(1, 0, 2, 0, gi_datos_incompletos, ret_marcaje.caso_adai, 0, 0, 0)
         RETURN
      ELSE
         -- se verifica que el arreglo para marcar haya llegado correcto
         LET v_longitud = ret_marcaje.arr_mod_retiro.getLength()
         LET v_error_en_datos = FALSE
      
         FOR v_indice = 1 TO v_longitud
            -- se registran los datos de detalle recibidos en la peticion de ws
            CALL fn_registra_detalle_peticion_marca(g_id_peticion,
                                                    ret_marcaje.arr_mod_retiro[v_indice].mod_retiro,
                                                    ret_marcaje.arr_mod_retiro[v_indice].ind_marca,
                                                    ret_marcaje.arr_mod_retiro[v_indice].cod_rechazo,
                                                    0, 0, 0, 0)
      
         
            -- Valida modalidad de retiro
            IF ( (ret_marcaje.arr_mod_retiro[v_indice].mod_retiro IS NULL OR
                 ret_marcaje.arr_mod_retiro[v_indice].ind_marca IS NULL )) --OR
                --(ret_marcaje.arr_mod_retiro[v_indice].mod_retiro = 2 AND  -- ***** se adiciono esta validacion para los casos 
                -- ret_marcaje.rfc IS NULL) ) 
                THEN                      -- ***** de retiro de fondo de ahorro RPR 140610
      
               -- se marca que hubo error en la validacion de datos
               LET v_error_en_datos = TRUE
            END IF
         END FOR
      
         -- si hubo error en datos, se finaliza la peticion
         IF ( v_error_en_datos ) THEN
            CALL fn_respuesta_wsmarcaje(1, 0, 2, 0, gi_datos_incompletos, ret_marcaje.caso_adai, 0, 0, 0)
            RETURN
         END IF
      END IF
      
      
      -- Obtiene el identificador del derechohabiente
      LET v_id_derechohabiente = fn_obtiene_id_derechohabiente(ret_marcaje.nss, ret_marcaje.rfc, 0)
      
      -- Obtiene la longitud del arreglo que llega
      LET v_longitud = ret_marcaje.arr_mod_retiro.getLength()
      
      -- Recorre arreglo de modalidades de retiro para marca
      FOR v_indice = 1 TO v_longitud
         -- Valida modalidad de retiro
         IF ( ret_marcaje.arr_mod_retiro[v_indice].mod_retiro = 2 ) THEN 
         	
         	DISPLAY "Procesando solicitud de marcado de fondo de ahorro"
         	
         	 -- Modalidad fondo de ahorro
            CALL fn_aplica_marca_fondo_ahorro(v_indice,
                                              ret_marcaje.nss,
                                              ret_marcaje.rfc,
                                              ret_marcaje.caso_adai,
                                              ret_marcaje.arr_mod_retiro[v_indice].ind_marca)
      
         ELSE
         
         	 -- Marcado para los dem�s tipos de modalidad
            CALL fn_aplica_marca_modalidad(v_indice,
                                           ret_marcaje.nss,                                
                                           ret_marcaje.rfc,
                                           ret_marcaje.caso_adai,
                                           v_id_derechohabiente, 
                                           ret_marcaje.arr_mod_retiro[v_indice].mod_retiro,                               
                                           ret_marcaje.arr_mod_retiro[v_indice].ind_marca,
                                           ret_marcaje.arr_mod_retiro[v_indice].grupo_ley73)      
                                            
         END IF 	
      END FOR
   END IF   
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_aplica_marca_fondo_ahorro
Fecha creacion: Septiembre 24, 2013
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Aplica el marcaje para la modalidad de fondo de ahorro


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_aplica_marca_fondo_ahorro(p_indice, p_nss, p_rfc, p_caso_adai, p_marca)
DEFINE p_indice                SMALLINT,--Indice que contendr� el registro de respuesta
       p_nss                   CHAR(11), --NSS del registro
       p_rfc                   CHAR(13), --Clave de RFC del registro
       p_caso_adai             CHAR(10), -- caso de ADAI
       p_marca                 SMALLINT, --Indicador de marca
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente, --Identificador del derechohabiente
       v_id_afi_fondo72        LIKE afi_fondo72.id_afi_fondo72,    --Identificador del fondo 72
       v_sql                   STRING, --Cadena de ejecuci�n de los querys
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

      -- Asigna variables para la ejecuci�n del store
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
               
               -- Se obtiene el n�mero de solicitud
               SELECT seq_ret_solicitud.NEXTVAL
               INTO   v_n_referencia
               FROM   systables 
               WHERE  tabid = 1
      		  	
      		  	 --Actualiza tabla de retiro gen�rico   
               CALL fn_actualiza_retiro_generico(v_id_derechohabiente, p_nss, p_rfc, v_modalidad, v_n_referencia,
                                                 v_folio, v_estatus_marca, v_codigo_rechazo, 1)
               RETURNING v_res_actualizacion
                           
               -- Verifica que no haya existido un error en la actualizaci�n de la tabla
               IF ( v_res_actualizacion ) THEN
                      	
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
      	          	
      	             -- Se ejecut� correctamente el store, se indica que se marc� la cuenta
      	             LET v_estatus_marca = gi_estatus_marca_existoso
      	             
      	             --Se indica c�digo de rechazo 0, para mostrar que la ejecuci�n fu� correcta
      	             LET v_cod_inconsistencia = 0
      	          ELSE
                     --Sucedi� un error al ejecutar la funci�n de marcado
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
                -- Se solicit� marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
                LET v_estatus_marca = 0
               
                -- ya existe una solicitud en tramite
                LET v_cod_inconsistencia = gi_solicitud_en_tramite
               
                -- se obtiene el caso adai original
                SELECT caso_adai
                INTO   p_caso_adai
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
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
         IF ( v_existe_solicitud ) THEN

            -- Asigna estatus de la marca
            LET v_estatus_marca = 100
         	     	    	
         	  -- Actualiza tabla de retiro gen�rico   
            CALL fn_actualiza_retiro_generico(v_id_derechohabiente,p_nss,p_rfc,v_modalidad,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,2)
                 RETURNING v_res_actualizacion
                
            IF ( v_res_actualizacion ) THEN 
               --Si la ejecuci�n fu� correcta se env�a solicitud de desmarcado
               	   
               --Se ejecut� correctamente el store, se indica que se desmarco la cuenta
               LET v_estatus_marca = gi_estatus_marca_existoso
                  
               --Actualiza codigo de rechazo
               LET v_cod_inconsistencia = 0  		                            
            ELSE
               -- Existi� un error en la integraci�n de retiro gen�rico
               LET v_estatus_marca = gi_estatus_marca_no_exitoso                                     
                   
               -- hubo un error al ejecutar la operacion de desmarca
               LET v_cod_inconsistencia = gi_error_interno_marca            	
            END IF
         ELSE
         	  --Se solicit� marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
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
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
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
                  -- Actualiza tabla de retiro gen�rico   
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
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
         IF ( v_existe_solicitud ) THEN
            -- rechazo de solicitud
            LET v_estatus_marca = 100
         
         	  -- Actualiza tabla de retiro gen�rico   
            CALL fn_actualiza_retiro_generico(v_id_derechohabiente, p_nss, p_rfc, v_modalidad, v_n_referencia, 
                                              v_folio, v_estatus_marca,
                                              ret_marcaje.arr_mod_retiro[p_indice].cod_rechazo, 2)
                 RETURNING v_res_actualizacion
                 
         	  -- se indica que el rechazo de solicitud fue exitoso
            LET v_estatus_marca = gi_estatus_marca_existoso
         
            -- sin error
            LET v_cod_inconsistencia = 0
         ELSE
         	  -- Se solicit� marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
            LET v_estatus_marca = gi_estatus_marca_no_exitoso
         
            -- no existe una solicitud para rechazar con esos parametros
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF

                   
      OTHERWISE
         --Se recibi� un indicador de marcaje desconocido
         CALL ERRORLOG("Se recibi� un indicador de marcado desconocido")
         --Se env�a mensaje de error para indicar que no se pudo marcar
         LET v_estatus_marca = gi_estatus_marca_no_exitoso
         
         -- el indicador de marca no es valido
         LET v_cod_inconsistencia = gi_indicador_marca_invalido

      END CASE

   ELSE
      --No existe el derechohabiente con NSS y RFC	
      --Se env�a mensaje de error para indicar que no se pudo marcar
      LET v_estatus_marca = gi_estatus_marca_no_exitoso
      IF v_ocurrencias > 1 THEN 
          LET v_cod_inconsistencia = gi_mas_de_un_registro
      ELSE 
          --Se env�a c�digo de error que indica que el derehohabiente no existe
          LET v_cod_inconsistencia = gi_nss_rfc_no_existe
      END IF 
   END IF
   
   -- Invoca funci�n de respuesta del WS
   CALL fn_respuesta_wsmarcaje(p_indice,v_modalidad,v_estatus_marca,v_n_referencia,v_cod_inconsistencia, p_caso_adai, 0, 0, v_ref_dap)

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_aplica_marca_modalidad
Fecha creacion: Septiembre 25, 2013
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Aplica el marcaje para las dem�s modalidades


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     25 oct 2013             - La senal de desmarca tambien debera desmarcar
                                        y cancelar una solicitud
======================================================================
}
FUNCTION fn_aplica_marca_modalidad(p_indice, p_nss, p_rfc, p_caso_adai, p_id_derechohabiente,p_modalidad, p_marca, p_grupo)
DEFINE p_indice                 SMALLINT,
       p_nss                    CHAR(11),
       p_rfc                    CHAR(13),
       p_caso_adai              CHAR(10),
       p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente,
       p_modalidad              SMALLINT,
       p_marca                  SMALLINT,
       p_grupo                  SMALLINT, 
       v_sql                    STRING, --Cadena de ejecuci�n de los querys
       v_marca_entra            SMALLINT,
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
       v_id_solicitud           DECIMAL(9,0),
       v_res_actualizacion      SMALLINT ,
       v_existe_solicitud       SMALLINT,
       v_cod_inconsistencia     SMALLINT, --Variable para indicar la causa de la inconsistencia
       v_cod_rechazo            SMALLINT, -- Codigo de rechazo para la maraca en procesar
       v_diagnostico            SMALLINT,       --diagnostico de la consulta del saldo en la afore
       v_estatus                SMALLINT,        -- estatus de la cuenta individual segun la consulta del saldo en la Afore
       v_aivs_viv92             DECIMAL(24,6), -- saldo AIVs de viv92
       v_aivs_viv97             DECIMAL(24,6), -- saldo AIVs de viv97
       v_pesos_viv92            DECIMAL(22,2), -- saldo pesos de viv92
       v_pesos_viv97            DECIMAL(22,2), -- saldo pesos de viv97
       v_saldo_total            DECIMAL(24,6), -- saldo total trabajo
       v_saldo_aivs_total       DECIMAL(24,6), -- Saldo total en aivs
       v_saldo_tesofe           DECIMAL(22,2), -- Saldo en pesos de la Subcueta de la Tesofe
       v_saldo_paso             DECIMAL(22,2), -- Variable de paso para trabajar los saldos
       v_precio_primero_mes     DECIMAL(10,6), -- Precio del primer d�a natural del mes
       v_resultado              SMALLINT,      -- resultado del llamado a la funcion
       v_marca_credito          SMALLINT,
       v_val_clabe              SMALLINT

   IF p_modalidad = 9 THEN 
       LET p_id_derechohabiente = fn_obtiene_id_derechohabiente(ret_marcaje.nss, ret_marcaje.rfc, p_modalidad)
   END IF 
   -- si el id_derechohabiente viene nulo, no se encontro
   IF ( p_id_derechohabiente IS NULL ) THEN
      -- No se encontro el NSS en la base de datos
      LET v_estatus_marca = gi_estatus_marca_no_exitoso
      
      -- no existe en nss
      LET v_cod_inconsistencia = gi_nss_rfc_no_existe
      CALL fn_respuesta_wsmarcaje(p_indice,p_modalidad,v_estatus_marca,NULL,v_cod_inconsistencia, p_caso_adai, 0, 0, 0)
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


   
   --Asigna la marca correcta y el proceso 
   CASE p_modalidad
   	
      WHEN 1 -- Solo Infonavit
         LET v_marca_entra = 801
         LET v_proceso_cod = g_proceso_cod_ret_solo_infonavit
      
      WHEN 3 --Retiro ley 73
         LET v_marca_entra = 803
         LET v_proceso_cod = g_proceso_cod_ret_ley73_ws
           
      WHEN 7 --Retiro por fortalecimiento al cr�dito
         LET v_marca_entra = 807
         LET v_proceso_cod = g_proceso_cod_ret_fortal_credito
           
      WHEN 9 --Retiro por amortizaciones excedentes
         LET v_marca_entra = 810
         LET v_proceso_cod = g_proceso_cod_ret_amort_excedentes

      WHEN 10 --Retiro aportaciones voluntarias
         LET v_marca_entra = 809
         LET v_proceso_cod = g_proceso_cod_ret_aport_voluntarias
        	
      OTHERWISE
         CALL ERRORLOG("Modalidad no reconocida para ejecutar marca")
         -- la modalidad es invalida
         LET v_estatus_marca = gi_estatus_marca_no_exitoso
      
         -- modalidad invalida
         LET v_cod_inconsistencia = gi_modalidad_invalida
         
         --Invoca funci�n de respuesta del WS
         CALL fn_respuesta_wsmarcaje(p_indice,p_modalidad,v_estatus_marca,NULL,v_cod_inconsistencia, p_caso_adai, 0, 0, 0)
         RETURN

   END CASE
   
  
   --Asigna variables para la ejecuci�n del store
   LET v_folio          = 0
   LET v_estado_marca   = "0"
   LET v_codigo_rechazo = 0
   LET v_marca_causa    = "0"
   LET v_fecha_causa    = NULL
   LET v_usuario        = "safreviv"
  
  
   --Valida la acci�n de marcaje que se deber� hacer sobre el registro
    CASE p_marca
        WHEN 1
            --Solicitud de marcaje
   	        --Consulta si existe  otra solicitud para el derechohabiente 
            CALL fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad,1)
                 RETURNING v_existe_solicitud, v_n_referencia
   		  
   		    -- Valida que no exista otra solicitud 
   		    IF ( NOT v_existe_solicitud ) THEN
   		        --Asigna estatus de la marca
                LET v_estatus_marca = 8
                     
                --Se obtiene el n�mero de solicitud
                SELECT seq_ret_solicitud.NEXTVAL
                INTO   v_n_referencia
                FROM   systables 
                WHERE  tabid = 1
   		   	
                -- Actualiza tabla de retiro gen�rico   
                CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,p_rfc,p_modalidad,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,1)
                RETURNING v_res_actualizacion
                 
                -- Verifica que no haya existido un error en la actualizaci�n de la tabla
                IF ( v_res_actualizacion ) THEN
            	
            	    ---Se ejecuta la funci�n de marcaje para la nueva solicitud
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
                        IF p_modalidad = 3 THEN 
                            SELECT NVL(precio_fondo,0)
                            INTO   v_precio_primero_mes
                            FROM   glo_valor_fondo
                            WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                                  FROM   (SELECT LIMIT 1 1 
                                                          FROM   systables))
                            AND    fondo = 11
                            -- Se ejecut� correctamente el store, se indica que se marc� la cuenta
                            LET v_estatus_marca = gi_estatus_marca_existoso
                            IF p_modalidad = 3 AND p_grupo <> 1 THEN  -- En estos casos no se paga Vivienda 92 solo se obtendra el saldo del viv 97 y tesofe 

                                
                                CALL fn_calcula_saldo_ley73(p_nss, 4 , TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
                                CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_saldo_paso, v_saldo_tesofe
                                LET v_saldo_total = v_aivs_viv97 * v_precio_primero_mes + v_saldo_tesofe
                                LET v_saldo_aivs_total = v_aivs_viv97 + v_saldo_tesofe
                                
                            END IF 
                            IF p_modalidad = 3 AND p_grupo = 1 THEN 
                                CALL fn_consulta_saldo_vivienda_afore(p_nss, 44)
                                     RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo

                                LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
                                IF (v_diagnostico = 127) THEN 
                                    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 1, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- Se deber� reenviar la solicitud de marca 
                                    CALL fn_recupera_saldo_consulta(p_nss)
                                         RETURNING v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
                                END IF 
                                IF (v_diagnostico = 101 AND v_estatus <> 101) THEN 
                                    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 2, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- La solicitud no se pudo marcar porque esta marcada en otro proceso
                                    LET v_estatus_marca = gi_estatus_marca_no_exitoso
                                    LET v_cod_inconsistencia = v_cod_rechazo
                                    CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia,
                                                                      v_folio, v_estatus_marca, v_cod_rechazo, 3)
                                         RETURNING v_res_actualizacion
                                    CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 803,
                                                  v_n_referencia, 803,
                                                  "safreviv", g_proceso_cod_ret_ley73_ws)
                                END IF 
                                IF (v_diagnostico = 101 AND v_estatus = 101 AND v_saldo_total <= 0) THEN 
                                    CALL fn_guarda_consulta_ws_vent_afore(p_nss, 1, 2, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                          v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- La solicitud se marca, pero tiene saldo cero
                                    LET v_estatus_marca = gi_estatus_marca_no_exitoso
                                    LET v_cod_inconsistencia = gi_sin_saldo
                                END IF 
                                LET v_saldo_total = (v_aivs_viv92 + v_aivs_viv97) * v_precio_primero_mes
                                LET v_saldo_aivs_total = v_aivs_viv92 + v_aivs_viv97
                            END IF 
                        END IF 
   	                    --Se indica c�digo de rechazo 0, para mostrar que la ejecuci�n fu� correcta
   	                    
   	                ELSE
   	           	        -- Sucedi� un error al ejecutar la funci�n de marcado
   	           	        -- Se elimina el registro creado
   	           	 	    CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia,
   	           	 	                                      v_folio, v_estatus_marca, v_codigo_rechazo, 3)
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
                    -- Existi� un error en la integraci�n de retiro gen�rico
                    LET v_estatus_marca = gi_estatus_marca_no_exitoso
               
                    -- error al generar la solicitud en ret_solicitud_generico
                    LET v_cod_inconsistencia = gi_error_marca_generar_solicitud
            	
                END IF
   	              	            
   	        ELSE
                --------- Validar si el tipo de retiro es ley 73 ya que este tipo de retiro intenta marca doblemente
                --------- una por vivienda 92 y otra por vivienda 97, en el segundo intento no debe regresar error
                --------- siempre y cuando se cumplan las condiciones sig:
                --------- El intento de marca se debe hacer el mismo dia, para el mismo id_solicitud,
                --------- la misma modalidad y la solicitud debe estar en estado 8
                IF p_modalidad = 3 THEN  --- Se verifica nuevamente para los casos de retiro 73 por la doble marca RPR 140611
                    CALL fn_verifica_solicitud_ley73_doble_marca(p_id_derechohabiente, p_modalidad)
                        RETURNING v_existe_solicitud
                END IF
                IF (NOT v_existe_solicitud) THEN
                    LET v_estatus_marca = gi_estatus_marca_existoso
                 
                    --Se indica c�digo de rechazo 0, para mostrar que la ejecuci�n fu� correcta
                    LET v_cod_inconsistencia = 0
                ELSE
                    --Se solicit� marcar cuando existe otra solicitud en marcha y se indica que no se pudo marcar
                    LET v_estatus_marca = gi_estatus_marca_no_exitoso
                    
                    -- Se env�a c�digo de rechazo indicando que existe otra solicitud en marcha
                    LET v_cod_inconsistencia = gi_solicitud_en_tramite
                    
                    -- se obtiene el caso adai original
                    SELECT caso_adai
                    INTO   p_caso_adai
                    FROM   ret_solicitud_generico
                    WHERE  id_solicitud = v_n_referencia
                END IF            
         
   	        END IF      
      
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          DESMARCAR CUENTA
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 2
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente,p_modalidad,2)
              RETURNING v_existe_solicitud, v_n_referencia
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
         IF ( v_existe_solicitud ) THEN

            -- Asigna estatus de la marca
            LET v_estatus_marca = 100
         	     	    	
         	  -- Actualiza tabla de retiro gen�rico   
            CALL fn_actualiza_retiro_generico(p_id_derechohabiente,p_nss,p_rfc,p_modalidad,v_n_referencia,v_folio,v_estatus_marca,v_codigo_rechazo,2)
                 RETURNING v_res_actualizacion
                
            IF ( v_res_actualizacion ) THEN 
               --Si la ejecuci�n fu� correcta se env�a solicitud de desmarcado
               	   
               --Se ejecut� correctamente el store, se indica que se desmarco la cuenta
               LET v_estatus_marca = gi_estatus_marca_existoso
                  
               --Actualiza codigo de rechazo
               LET v_cod_inconsistencia = 0     		                            
            ELSE
               --Existi� un error en la integraci�n de retiro gen�rico
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
                   
               --Se indica c�digo de rechazo para mostrar que ocurri� un error al integrar informaci�n en 
               --ret_solicitud_generico
               LET v_cod_inconsistencia = gi_error_interno_marca
            END IF
         ELSE
            -- se verifica si hay una solicitud ya generada
            CALL fn_verifica_solicitud_generico(p_id_derechohabiente, p_modalidad, 4)
                 RETURNING v_existe_solicitud, v_n_referencia
            
            --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
            IF ( v_existe_solicitud ) THEN
               -- rechazo de solicitud
               LET v_estatus_marca = 100
            
            	  -- Actualiza tabla de retiro gen�rico   
               CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia, 
                                                 v_folio, v_estatus_marca,
                                                 ret_marcaje.arr_mod_retiro[p_indice].cod_rechazo, 2)
                    RETURNING v_res_actualizacion
                    
            	  -- se rechazo correctamente la solicitud
               LET v_estatus_marca = gi_estatus_marca_existoso
            
               -- sin error
               LET v_cod_inconsistencia = 0
            ELSE
           
         	     -- se indica que no se pudo desmarcar
               LET v_estatus_marca = gi_estatus_marca_no_exitoso
               
               --Se env�a c�digo de rechazo indicando que no existe solicitud de marcado previa para poder desmarcar
               LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
            END IF
         	
         END IF                                    
                   
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          APROBAR SOLICITUD
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 3
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente, p_modalidad, 3)
              RETURNING v_existe_solicitud, v_n_referencia
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
         IF ( v_existe_solicitud ) THEN
            CALL fn_valida_clabe_bancaria(p_id_derechohabiente, v_n_referencia, p_modalidad) RETURNING v_val_clabe
            IF v_val_clabe = 0 THEN 
         
                IF p_modalidad = 3 THEN 
                   SELECT NVL(precio_fondo,0)
                    INTO   v_precio_primero_mes
                    FROM   glo_valor_fondo
                    WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                          FROM   (SELECT LIMIT 1 1 
                                                  FROM   systables))
                    AND    fondo = 11
                    IF p_grupo <> 1 THEN
                        CALL fn_calcula_saldo_ley73(p_nss, 4 , TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
                        CALL fn_calcula_saldo_ley73(p_nss, 47, TODAY) RETURNING v_resultado, v_saldo_paso, v_saldo_tesofe
                        LET v_saldo_total = v_aivs_viv97 * v_precio_primero_mes + v_saldo_tesofe
                        LET v_saldo_aivs_total = v_aivs_viv97 + v_saldo_tesofe
                        LET v_estatus_marca = 15    -- Se dejar� fluir la solicitud
                    ELSE 

                        CALL fn_consulta_saldo_vivienda_afore(p_nss, 30)
                             RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
                        IF (v_diagnostico = 127) THEN
                           CALL fn_guarda_consulta_ws_vent_afore(p_nss, 3, 7, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                 v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- Se deber� reenviar la solicitud de consulta
                        ELSE
                           CALL fn_guarda_consulta_ws_vent_afore(p_nss, 3, 3, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                 v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1)
                        END IF
                        LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
                        IF v_diagnostico = 127 THEN 
                            LET v_estatus_marca = 12    -- Se reenviar� la solicitud de saldo
                        ELSE 
                            IF v_diagnostico = 101 AND v_estatus = 101 THEN 
                                IF v_saldo_total = 0 THEN  --- Se debe desmarcar en SACI y en Procesar
                                    -- rechazo de solicitud
                                    LET v_estatus_marca = 100
                                 
                                      -- Actualiza tabla de retiro gen�rico   
                                    CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia, 
                                                                      v_folio, v_estatus_marca,
                                                                      v_cod_rechazo, 2)
                                         RETURNING v_res_actualizacion
                                    -- Reviso que no exista una solicitud de marca pendiente
                                    SELECT *
                                    FROM ret_ctr_marca_procesar_maestra
                                    WHERE nss = p_nss
                                    AND id_solicitud = v_n_referencia
                                    AND estado_indicador = 1         --No marcado
                                    --
                                    IF SQLCA.sqlcode = NOTFOUND THEN
                                       CALL fn_consulta_saldo_vivienda_afore(p_nss, 60)  -- Desmarca en Procesar
                                            RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
                                       IF v_diagnostico = 127 THEN 
                                           CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                                 v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- Se deber� reenviar la solicitud de marca 
                                           LET v_cod_inconsistencia = v_cod_rechazo
                                       ELSE 
                                           CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                                 v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- Se deber� reenviar la solicitud de marca 
                                       END IF
                                    ELSE
                                       --Ya no env�o Desmarca a Procesar
                                       CALL fn_guarda_consulta_ws_vent_afore(p_nss, 2, 6, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_n_referencia, p_caso_adai, 1) -- Se deber� reenviar la solicitud de marca 
                                    END IF
                                ELSE 
                                    -- Valida si solo se debe pagar vivienda 92
                                    LET v_marca_credito = 0
                                    SELECT COUNT(*) 
                                    INTO   v_marca_credito
                                    FROM   sfr_marca_activa
                                    WHERE  marca IN (202, 212, 218, 223, 232, 233)
                                    AND    id_derechohabiente = p_id_derechohabiente;
                                    IF v_marca_credito <> 0 THEN 
                                        LET v_aivs_viv97 = 0
                                    END IF
                                    
                                    -- Actualiza los saldos con los devueltos por el WS de Consulta de Procesar
                                    UPDATE ret_ley73_generico
                                    SET    aivs_viv92 = v_aivs_viv92,
                                           aivs_viv97 = v_aivs_viv97
                                    WHERE  id_solicitud = v_n_referencia
                                    LET v_estatus_marca = 15
                                END IF 
                            ELSE 
                            ---  Aun esta pendiente la definicion, ya que el WS devolver� saldo cuando ya se encuentre marcada la cuenta
                                LET v_estatus_marca = 15    -- Se dejar� fluir la solicitud
                            END IF 
                        END IF 
                    END IF 
                ELSE 
                    LET v_estatus_marca = 15
                END IF 
             
                  -- Actualiza tabla de retiro gen�rico   
                CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia, 
                                                  v_folio, v_estatus_marca, 0, 2)
                     RETURNING v_res_actualizacion

                  -- se aprobo correctamente la solicitud
                LET v_estatus_marca = gi_estatus_marca_existoso -- rechazo de solicitud
             
                -- sin error
                LET v_cod_inconsistencia = 0
            ELSE 
                LET v_estatus_marca = gi_estatus_marca_no_exitoso
             
                -- Problemas con la CLABE BANCARIA
                LET v_cod_inconsistencia = gi_esctructura_cta_clabe_incorrecta
            END IF

         ELSE
         	  -- no se pudo aprobar la solicitud
            LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
         
            -- no existe una solicitud que aprobar
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF


      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      --          RECHAZAR SOLICITUD
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WHEN 4
         -- Consulta si existe otra solicitud para el derechohabiente, que se pueda desmarcar 
         CALL fn_verifica_solicitud_generico(p_id_derechohabiente, p_modalidad, 4)
              RETURNING v_existe_solicitud, v_n_referencia
         
         --	Valida que no se haya solicitado una marcaci�n antes de una desmarcaci�n
         IF ( v_existe_solicitud ) THEN
            -- rechazo de solicitud
            LET v_estatus_marca = 100
         
         	  -- Actualiza tabla de retiro gen�rico   
            CALL fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad, v_n_referencia, 
                                              v_folio, v_estatus_marca,
                                              ret_marcaje.arr_mod_retiro[p_indice].cod_rechazo, 2)
                 RETURNING v_res_actualizacion
                 
         	  -- se rechazo correctamente la solicitud
            LET v_estatus_marca = gi_estatus_marca_existoso
         
            -- sin error
            LET v_cod_inconsistencia = 0
         ELSE
         	  -- no se pudo aprobar la solicitud
            LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
         
            -- no existe una solicitud que rechazar
            LET v_cod_inconsistencia = gi_error_marca_no_existe_solicitud
         END IF

                   
      OTHERWISE
         --Se recibi� un indicador de marcaje desconocido
         CALL ERRORLOG("Se recibi� un indicador de marcado desconocido")
         -- marcaje no exitoso
         LET v_estatus_marca = gi_estatus_marca_no_exitoso -- rechazo de solicitud
         
         -- indicador de marca no reconocido
         LET v_cod_inconsistencia = gi_indicador_marca_invalido

      
   END CASE
   
   --Invoca funci�n de respuesta del WS
   CALL fn_respuesta_wsmarcaje(p_indice,p_modalidad,v_estatus_marca,v_n_referencia,v_cod_inconsistencia, p_caso_adai, v_saldo_aivs_total, v_saldo_total, 0)

      
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_obtiene_id_derechohabiente
Fecha creacion: Septiembre 25, 2013
Autor: Esteban S�nchez Zepeda, EFP
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
  
  --Env�a el id_derechohabiente consultado
  RETURN v_id_derechohabiente 

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_actualiza_retiro_generico
Fecha creacion: Septiembre 25, 2013
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualizaci�n de la respuesta del marcaje en la tabla de retiro gen�rico


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
       p_tipo_actualizacion       SMALLINT, --Variable que recibe el tipo de actualizaci�n(INSERT,UPDATE,DELETE)
       v_sql                      STRING,
       v_respuesta                SMALLINT,
       v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud
   
   -- Valida el tipo de actualizaci�n 1-Inserci�n, 2-Actualizaci�n
   CASE p_tipo_actualizacion
   	
      WHEN 1
   		   -- Arma el query INSERCI�N
         LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
         LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
         LET v_r_ret_solicitud_generico.nss                    = p_nss
         LET v_r_ret_solicitud_generico.rfc                    = p_rfc
         LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
         LET v_r_ret_solicitud_generico.folio                  = p_folio
         LET v_r_ret_solicitud_generico.caso_adai              = ret_marcaje.caso_adai
         LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
         LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
         LET v_r_ret_solicitud_generico.folio_restitucion      = 0
         LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
         LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
         LET v_r_ret_solicitud_generico.folio_afore            = NULL -- se usa en ventanilla afore
         LET v_r_ret_solicitud_generico.grupo_ventanilla       = gi_ventanilla_infonavit -- solicitud iniciada en infonavit
         LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
         LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
         LET v_r_ret_solicitud_generico.estado_solicitud       = p_estado_solicitud
         LET v_r_ret_solicitud_generico.cod_rechazo            = p_cod_rechazo
   		   
         -- se inserta el registro
         INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )
                                            
         -- Valida que la ejecuci�n del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta = FALSE
            CALL ERRORLOG("Se ha producido un error al insertar el registro en retiro gen�rico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha insertado con �xito el registro en retiro gen�rico")
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
                                           
         -- Valida que la ejecuci�n del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
         	  -- Asigna respuesta negativa
         	  LET v_respuesta = FALSE
         	  CALL ERRORLOG("Se ha producido un error al actualizar el registro en retiro gen�rico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha actualizado con �xito el registro en retiro gen�rico")
            
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
      
      	 -- Arma el query ELIMINACI�N
         LET v_sql = "\n DELETE FROM ret_solicitud_generico",
                     "\n WHERE  id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND    modalidad_retiro   = ", p_modalidad_retiro,
                     "\n AND    id_solicitud       = ", p_id_solicitud
                      
         PREPARE stm_delete_retiro FROM v_sql
         EXECUTE stm_delete_retiro 
               
         --Valida que la ejecuci�n del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta=FALSE
            CALL ERRORLOG("Se ha producido un error al eliminar el registro en retiro gen�rico")
         ELSE
         	
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha eliminado con �xito el registro en retiro gen�rico")
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
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Verifica que no exista una solicitud en la tabla de retiro gen�rico

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
 
 
   -- Valida el tipo de acci�n
   CASE p_accion
   	
      WHEN 1 
   	 	   -- Marcar 		  
         LET v_sql = "\n SELECT id_solicitud         ",
                     "\n FROM ret_solicitud_generico ",
                     "\n WHERE id_derechohabiente =  ", p_id_derechohabiente,
                     "\n AND modalidad_retiro     =  ", p_modalidad_retiro,
                     "\n AND grupo_ventanilla     =  ", gi_ventanilla_infonavit,
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
                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit,
                     "\n AND estado_solicitud     IN (8,10) "
       
      WHEN 3
         -- Aprobar solicitud
         LET v_sql = "\n SELECT id_solicitud ",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro,
                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit,
                     "\n AND estado_solicitud     = 10"
                     
      WHEN 4 
         -- Rechazar la solicitud
         LET v_sql = "\n SELECT id_solicitud",
                     "\n FROM ret_solicitud_generico",
                     "\n WHERE id_derechohabiente = ",p_id_derechohabiente,
                     "\n AND modalidad_retiro     = ",p_modalidad_retiro,
                     "\n AND grupo_ventanilla     = ", gi_ventanilla_infonavit,
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
Autor: Esteban S�nchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualizaci�n de la respuesta del marcaje en la tabla de retiro gen�rico
Asigna los valores de retorno del Web Service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_respuesta_wsmarcaje(p_indice, p_modalidad, p_estatus_marca, p_id_solicitud, p_cod_rechazo, p_caso_adai, p_saldo_aivs_total, p_saldo_total, p_ref_dap)
DEFINE p_indice           SMALLINT,
       p_modalidad        SMALLINT,
       p_estatus_marca    SMALLINT,
       p_id_solicitud     DECIMAL(9,0),
       p_cod_rechazo      SMALLINT,
       p_caso_adai        LIKE ret_solicitud_generico.caso_adai, -- caso adai
       p_saldo_aivs_total DECIMAL(24,6),
       p_saldo_total      DECIMAL(22,6),
       p_ref_dap          DECIMAL(9,0)
       
   --Asigna valores al arreglo
   --LET ret_respuesta.caso_adai = p_caso_adai
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].mod_retiro  = p_modalidad
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].est_marca   = p_estatus_marca
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].con_retiro  = p_id_solicitud
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].cod_rechazo = p_cod_rechazo
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].des_rechazo = " ";
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].saldo_aivs  = p_saldo_aivs_total
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].saldo_pesos = p_saldo_total
   LET ret_respuesta.arr_mod_retiro_resp[p_indice].cve_dap     = p_ref_dap

   IF p_cod_rechazo <> 0 THEN
   -- Busca la descripcion del error para regresarla en la consulta
       LET ret_respuesta.arr_mod_retiro_resp[p_indice].des_rechazo = "";
       SELECT des_larga
       INTO   ret_respuesta.arr_mod_retiro_resp[p_indice].des_rechazo
       FROM   ret_rechazo_generico
       WHERE  cod_rechazo = p_cod_rechazo;
       IF ret_respuesta.arr_mod_retiro_resp[p_indice].des_rechazo IS NULL THEN
           LET ret_respuesta.arr_mod_retiro_resp[p_indice].des_rechazo = " "; 
       END IF
   END IF    
   
   -- se actualiza el historico de la peticion
   CALL fn_registra_peticion_marca(g_id_peticion, ret_marcaje.nss, ret_marcaje.rfc, ret_marcaje.cuenta_clabe, ret_marcaje.caso_adai, p_cod_rechazo, 1)
   RETURNING g_id_peticion
   
   -- se actualiza el detalle de la peticion
   CALL fn_registra_detalle_peticion_marca(g_id_peticion, p_modalidad,
                                           ret_marcaje.arr_mod_retiro[p_indice].ind_marca,
                                           ret_marcaje.arr_mod_retiro[p_indice].cod_rechazo,
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
FUNCTION fn_registra_peticion_marca(p_id_peticion, p_nss, p_rfc, p_caso_adai, p_cuenta_clabe, p_res_ejecucion, p_accion)
DEFINE p_id_peticion             DECIMAL(9,0),
       p_nss                     LIKE afi_derechohabiente.nss,
       p_rfc                     LIKE afi_derechohabiente.rfc,
       p_caso_adai               LIKE ret_solicitud_generico.caso_adai,
       p_cuenta_clabe            LIKE ret_ws_peticion_marca.cuenta_clabe,
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
      LET v_r_ret_ws_peticion_marca.caso_adai     = p_caso_adai
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

{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_ley73_doble_marca
Fecha creacion: Junio 11, 2014
Autor: Ricardo Perez Ramirez, EFP
Narrativa del proceso que realiza:
Verifica que exista una solicitud en la tabla de retiro gen�rico para 
permitir la caputra del tipo de retiro ley 73 ya que este intenta marcar doble
una marca para cada subcuenta, si es asi no debe rechazar la segunda marca

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_solicitud_ley73_doble_marca(p_id_derechohabiente,p_modalidad_retiro)
DEFINE p_id_derechohabiente LIKE  afi_derechohabiente.id_derechohabiente,
       p_modalidad_retiro   SMALLINT,
       v_ban_existe         SMALLINT,
       v_cant_sol           SMALLINT
       
   -- Inicializa valores
   LET v_ban_existe= FALSE    
 
   IF p_modalidad_retiro = 3 THEN
       SELECT COUNT(*) 
         INTO v_cant_sol
         FROM ret_solicitud_generico    		  
        WHERE id_derechohabiente = p_id_derechohabiente
          AND modalidad_retiro = 3
          AND estado_solicitud = 8;

       IF v_cant_sol <> 1 THEN
           LET v_ban_existe = TRUE;
       ELSE 
          DISPLAY "Solicitud encontrada para retiro ley 73 se permite continuar: "
          DISPLAY "Bandera: ", v_ban_existe
       END IF
   ELSE
       LET v_ban_existe = TRUE;
   END IF;
  
   -- se devuelve el resultado de la creacion
   RETURN v_ban_existe
END FUNCTION                 
{
======================================================================
Clave: 
Nombre: fn_verifica_solicitud_ley73_doble_marca
Fecha creacion: Junio 11, 2014
Autor: Ricardo Perez Ramirez, EFP
Narrativa del proceso que realiza:
Verifica que exista una solicitud en la tabla de retiro gen�rico para 
permitir la caputra del tipo de retiro ley 73 ya que este intenta marcar doble
una marca para cada subcuenta, si es asi no debe rechazar la segunda marca

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
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