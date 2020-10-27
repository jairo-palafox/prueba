####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW13                                        #
#Objetivo          =>Programa obtiene la lista de cuentas para     #
#                    solicitud de desmarca en procesar             #
#Fecha inicio      =>30 MARZO 2012                                 #
####################################################################

DATABASE safre_viv

GLOBALS "CTAW12.inc"    #Archivo de variables globales del WS de marcas
GLOBALS "CTAW13.inc"    #Archivo de variables globales del lanzador

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

#Variables para el registro del historico de acreditados
PRIVATE DEFINE v_edo_procesar      LIKE cre_his_acreditado.edo_procesar
PRIVATE DEFINE v_diagnostico_hist  LIKE cre_his_acreditado.diagnostico
PRIVATE DEFINE v_estado_hist       LIKE cre_his_acreditado.estado
PRIVATE DEFINE v_nom_imss          LIKE cre_his_acreditado.nom_imss

#Objetivo: Funcion que inicia el envio de solicitudes de marca a procesar
MAIN
	DEFINE r_resultado_opera               INTEGER
	-- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)


   WHENEVER ERROR CONTINUE

	CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso

	DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

	-- se solicita el numero de folio asociado a la operacion
	CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
	RETURNING v_folio

	#Se actualiza el folio del proceso               
	UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

	UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   #Se ejecuta la funcion que establece las instrucciones SQL que se ejecutaran
   CALL fn_inicializacion()
   
   #Se ejecuta la funcion que obtiene los parametros de conexion del cliente
   CALL fn_configura_ws()

	DISPLAY ""
	DISPLAY "Iniciando en envio de solicitudes de marca con procesar..."
	DISPLAY ""

   #Se ejecuta la funcion principal que solicitara las marcas a las solicitudes de transferencia
   CALL fn_ejecuta_solicitud_desmarca()

	CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
	RETURNING r_resultado_opera
	IF(r_resultado_opera <> 0)THEN         
		# Actualiza a estado erróneo
		DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
		CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
		RETURNING r_resultado_opera
	END IF

	DISPLAY "*******************************************************************"
	DISPLAY ""
	DISPLAY "Termino el envio se solicitudes de desmarca en procesar: "
	DISPLAY ""
	DISPLAY " PROCESO            : ",v_proceso_desc
	DISPLAY " OPERACIÓN          : ",v_opera_desc
	DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
	DISPLAY " HORA               : ",TIME(CURRENT)
	DISPLAY ""
	DISPLAY "*******************************************************************"

	WHENEVER ERROR STOP
   
END MAIN

PRIVATE FUNCTION fn_inicializacion()
   #Variable para la instruccion de consulta del orden de marca/desmarca
   DEFINE v_consulta_orden                   STRING
   #Variable para almacenar la instruccion de consulta de solicitudes de marca
   DEFINE v_consulta_solicitud               STRING
   #Variavle para almacenar la instruccion de consulta de rechazos
   DEFINE v_consulta_rechazo                 STRING
   #Variable para almacenar la instruccion que inserta el historico
   DEFINE v_inserta_historico                STRING
   #Variable para almacenar la instruccion que elimina las solicitudes de marcas
   DEFINE v_elimina_solicitud                STRING
   #Variable para almacenar la instruccion que inserta una solicitud de marca por rechazo
   DEFINE v_inserta_rechazo                  STRING
   #Variable para almacenar la instruccion que inserta en el historico de acreditados
   DEFINE v_historico_acre                   STRING
   #Variable para almacenar la instruccion que ejecuta la funcion que cambia el estado a la tabla de transferencia 
   #DEFINE v_sp_actualiza_edo_procesar        STRING
   DEFINE v_actualiza_edo_procesar           STRING
   #Variable para almacenar la instruccion que ejecuta la funcion marcas
   DEFINE v_fn_desmarca_cuenta                  STRING

   #Fecha del dia
   LET g_f_dia = TODAY

   LET v_consulta_orden =  "SELECT tpo_credito, ",
                           "marca_prc ",
                           "FROM cat_tipo_credito ",
                           "ORDER BY prioridad_marca, tpo_credito "
   PREPARE exe_consulta_orden FROM  v_consulta_orden

   #instruccion de consulta de solicitudes de marca
   LET v_consulta_solicitud = "  SELECT ",
                                 "acreditado.id_cre_ctr_archivo, ",
                                 "solicitud.id_derechohabiente, ",
                                 "solicitud.id_origen, ",      
                                 "solicitud.modulo_cod, ",     
                                 "solicitud.tpo_credito, ",    
                                 "solicitud.marca, ",
                                 "solicitud.f_solicita, ",
                                 "solicitud.intento, ",
                                 "solicitud.cod_result_op, ",  
                                 "solicitud.diagnostico, ",    
                                 "solicitud.situacion, ",      
                                 "solicitud.num_credito, ",    
                                 "solicitud.f_infonavit, ",    
                                 "solicitud.marca_procesar, ",
                                 "solicitud.folio_archivo, ",
                                 "solicitud.usuario, ",
                                 "derechohabiente.nss, ",      
                                 "derechohabiente.ap_paterno_af, ",
                                 "derechohabiente.ap_materno_af, ",
                                 "derechohabiente.nombre_af, ",
                                 "derechohabiente.rfc ",        
                              "FROM cta_marca_ws solicitud ",
                              "INNER JOIN afi_derechohabiente derechohabiente ", 
                                 "ON solicitud.id_derechohabiente = derechohabiente.id_derechohabiente ",
                              "INNER JOIN cre_acreditado acreditado ", 
                                 "ON (solicitud.id_origen = acreditado.id_cre_acreditado AND solicitud.id_derechohabiente = acreditado.id_derechohabiente) ",
                              "WHERE solicitud.f_solicita <= ? ",
                              "AND solicitud.tpo_credito = ? ",
                              "AND solicitud.marca = ? ",
                              "AND solicitud.situacion = ? "
   PREPARE exe_consulta_solicitud FROM v_consulta_solicitud

   #instruccion de consulta de dias para reintento por tipo de rechazo
   LET v_consulta_rechazo = " SELECT dias_reintento  ",
                              "FROM cat_rechazo  ",
                              "WHERE tpo_rechazo = 'RCH' ",
                              "AND cod_rechazo = ? "
   PREPARE exe_consulta_rechazo FROM v_consulta_rechazo

   #instruccion que inserta el historico de marcas
   LET v_inserta_historico = "INSERT INTO cta_his_marca_ws (id_derechohabiente,
                                                            id_origen,
                                                            modulo_cod,
                                                            tpo_credito,
                                                            marca,
                                                            f_solicita,
                                                            intento,
                                                            cod_result_op,
                                                            diagnostico,
                                                            situacion,
                                                            num_credito,
                                                            f_infonavit,
                                                            marca_procesar, 
                                                            f_actualiza, 
                                                            usuario)
                              VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_inserta_historico FROM v_inserta_historico

   #instruccion que elimina las solicitudes de marcas
   LET v_elimina_solicitud = "DELETE FROM cta_marca_ws WHERE  id_derechohabiente = ? 
                                                               AND id_origen = ?"
   PREPARE exe_elimina_solicitud FROM v_elimina_solicitud

   #instruccion que inserta una solicitud de marca por rechazo
   LET v_inserta_rechazo = "INSERT INTO cta_marca_ws(id_derechohabiente,
                                                      id_origen,
                                                      modulo_cod,
                                                      tpo_credito,
                                                      marca,
                                                      f_solicita,
                                                      intento,
                                                      cod_result_op,
                                                      diagnostico,
                                                      situacion,
                                                      num_credito,
                                                      f_infonavit,
                                                      marca_procesar,
                                                      folio_archivo,
                                                      usuario) 
                              VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_inserta_rechazo FROM v_inserta_rechazo

   #instruccion que inserta el historico de transferencia
   LET v_historico_acre = "INSERT INTO cre_his_acreditado(id_cre_acreditado,
                                                            id_cre_ctr_archivo,
                                                            tpo_transferencia,
                                                            edo_procesar,
                                                            diagnostico,
                                                            estado,
                                                            nss_afore,
                                                            rfc_afore,
                                                            paterno_afore,
                                                            materno_afore,
                                                            nombre_afore,
                                                            nom_imss,
                                                            f_proceso) 
                              VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_inserta_historico_acre FROM v_historico_acre

   #Instruccion para ejecutar la funcion que cambia el estado a la tabla de transferencia
   #LET v_sp_actualiza_edo_procesar = "EXECUTE PROCEDURE sp_act_edo_procesar(?,?)"
   #PREPARE exe_sp_actualiza_edo_procesar FROM v_sp_actualiza_edo_procesar
   LET v_actualiza_edo_procesar =   "UPDATE cre_acreditado ",
                                    "SET edo_procesar =  ? ",
                                    "WHERE id_cre_acreditado = ?;"
   PREPARE exe_actualiza_edo_procesar FROM v_actualiza_edo_procesar

   #Instruccion para ejecutar la funcion de marcas
   LET v_fn_desmarca_cuenta = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE exe_fn_desmarca_cuenta FROM v_fn_desmarca_cuenta
END FUNCTION

PRIVATE FUNCTION fn_configura_ws()
   DEFINE v_consulta    STRING

   #La clave 'cre_2' del catalogo de clientes de webServices corresponde a la solicitud de desmarca
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING WS_DESMARCA INTO  v_url_servidor,
                                                v_usuario,
                                                v_password,
                                                v_intentos
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_solicitud_desmarca()
   DEFINE v_solicitud solicitaDesmarca
   DEFINE v_tipo_credito   LIKE cat_tipo_credito.tpo_credito
   DEFINE v_marca_procesar LIKE cat_tipo_credito.marca_prc
   DEFINE v_tipo_solicitud LIKE cta_marca_ws.situacion

   LET v_tipo_solicitud = 0   #cta_marca_ws.situacion = 0 significa solicitud de desmarca

   #Se obtiene el orden en que se ejecutaran las solicitudes
   DECLARE orden CURSOR FOR exe_consulta_orden
   FOREACH orden INTO v_tipo_credito, v_marca_procesar
      #Se buscan las cuentas pendientes de marca con procesar
      DECLARE consulta CURSOR FOR exe_consulta_solicitud

      #Se manda la solicitud de marca por cada registro
      FOREACH consulta USING g_f_dia, v_tipo_credito, v_marca_procesar, v_tipo_solicitud INTO v_solicitud.*
         CALL fn_envia_desmarca(v_solicitud.*)
      END FOREACH
   END FOREACH

   UPDATE STATISTICS FOR TABLE cta_his_marca_ws
END FUNCTION

PRIVATE FUNCTION fn_envia_desmarca(p_solicitud)
   DEFINE p_solicitud   solicitaDesmarca
   DEFINE v_solicita    tSolicDesmarcaVO
   DEFINE v_respuesta   tSolicDesmarcaRespVO

   #se llenan las variables para el historico de transferencia
   LET v_nom_imss = NULL
   LET v_estado_hist = 170
   
   LET v_edo_procesar = 180
   LET v_diagnostico_hist = NULL

   #Se inserta en el historico de acreditados un registro de solicitud de desmarca procesar
   EXECUTE exe_inserta_historico_acre     USING p_solicitud.id_origen,              --id_cre_acreditado
                                                p_solicitud.id_cre_ctr_archivo,     --id_cre_ctr_archivo
                                                p_solicitud.modulo_cod,             --tpo_transferencia             = 22
                                                v_edo_procesar,                     --edo_procesar          = 180
                                                v_diagnostico_hist,                 --diagnostico           = null
                                                v_estado_hist,                      --estado                = 170
                                                p_solicitud.nss,                    --nss_afore
                                                p_solicitud.rfc,                    --rfc_afore
                                                p_solicitud.ap_paterno,             --paterno_afore
                                                p_solicitud.ap_materno,             --materno_afore
                                                p_solicitud.nombre,                 --nombre_afore
                                                v_nom_imss,                         --nom_imss              = null
                                                g_f_dia                             --f_proceso             = TODAY

   #se actualiza el estado de la tabla de transferencia a 180 - Solicitud desmarca Procesar
   #EXECUTE exe_sp_actualiza_edo_procesar USING p_solicitud.id_origen, v_edo_procesar
   EXECUTE exe_actualiza_edo_procesar USING v_edo_procesar, p_solicitud.id_origen

   #se llena la variable para solicitud de desmarca
   LET v_solicita.nss =                   p_solicitud.nss CLIPPED
   LET v_solicita.fechaPresentacion =     p_solicitud.f_infonavit USING 'yyyymmdd'
   LET v_solicita.apePaterno =            p_solicitud.ap_paterno CLIPPED
   LET v_solicita.apeMaterno =            p_solicitud.ap_materno CLIPPED
   LET v_solicita.nombres =               p_solicitud.nombre CLIPPED
   LET v_solicita.rfc =                   p_solicitud.rfc CLIPPED
   LET v_solicita.numCreditoInfonavit =   p_solicitud.num_credito
   
   #Se manda llamar la funcion que invoca el cliente del WS
   #DISPLAY  "Ejecutando..."
   #CALL fn_desmarca_procesar( v_url_servidor CLIPPED,
   CALL fn_solicita_desmarca( v_url_servidor CLIPPED, 
                              v_usuario      CLIPPED, 
                              v_password     CLIPPED, 
                              v_intentos, 
                              v_solicita.*) 
   RETURNING v_respuesta.*

   #Se ejecuta la funcion que procesa la respuesta de procesar
   CALL fn_respuesta_desmarca(p_solicitud.*, v_respuesta.*)
END FUNCTION

PRIVATE FUNCTION fn_respuesta_desmarca(p_solicitud, p_respuesta)
   DEFINE p_solicitud      solicitaDesmarca
   DEFINE p_respuesta      tSolicDesmarcaRespVO
   
   DEFINE v_diagnostico       LIKE cta_his_marca_ws.diagnostico
   DEFINE v_cod_result_op     LIKE cta_his_marca_ws.cod_result_op
   DEFINE v_num_rechazo       INTEGER
   DEFINE v_f_reintento       DATE

   #Variables para enviar los parametros a la funcion de marcas
   DEFINE v_estado_marca      SMALLINT
   DEFINE v_marca_causa    SMALLINT
   DEFINE v_fecha_causa       DATE
   DEFINE v_result_marca      SMALLINT

   #Variable para el manejo del codigo de rechazo que envia el WS de marcas
   DEFINE v_cod_rechazo       LIKE cat_rechazo.cod_rechazo
   DEFINE v_dias_reintento    LIKE cat_rechazo.dias_reintento

   LET v_diagnostico = p_respuesta.diagProceso
   LET v_cod_result_op = p_respuesta.resultOperacion

      
   #Se inserta en el historico de marcas
	EXECUTE exe_inserta_historico USING    p_solicitud.id_derechohabiente,
                                          p_solicitud.id_origen,
                                          p_solicitud.modulo_cod,
                                          p_solicitud.tpo_credito,
                                          p_solicitud.marca,
                                          p_solicitud.f_solicita,
                                          p_solicitud.intento,
                                          v_cod_result_op,
                                          v_diagnostico,
                                          p_solicitud.situacion,
                                          p_solicitud.num_credito,
                                          p_solicitud.f_infonavit,
                                          p_solicitud.marca_procesar, 
                                          g_f_dia, 
                                          p_solicitud.usuario

   #Se elimina la solicitud de desmarca ejecutada
   EXECUTE exe_elimina_solicitud USING    p_solicitud.id_derechohabiente,
                                          p_solicitud.id_origen
   
   IF p_respuesta.diagProceso = '01' THEN
      #ACEPTADO
      #se llenan las variables para el historico de transferencia
      LET v_edo_procesar = 210
      LET v_diagnostico_hist = v_diagnostico

      #se desmarca la cuenta con la marca del instituto indicando que procesar ya desmarco al trabajador
      LET v_estado_marca = 0
      LET v_marca_causa = 0
      LET v_fecha_causa = NULL
      
      EXECUTE exe_fn_desmarca_cuenta USING   p_solicitud.id_derechohabiente,       --p_id_derechohabiente
                                             p_solicitud.marca,                    --p_marca_entra       
                                             p_solicitud.id_origen,                --p_n_referencia
                                             v_estado_marca,                       --p_estado_marca      
                                             v_marca_causa,                        --p_marca_causa       
                                             p_solicitud.usuario,                  --p_usuario
                                             p_proceso_cod                         --p_proceso_cod
                                    INTO v_result_marca

      IF v_result_marca <> 0 THEN
         DISPLAY "La marca entrante (", p_solicitud.marca, ") fue rechazada por el codigo ", v_result_marca
      END IF
   ELSE
      #RECHAZADO
      #se llenan las variables para el historico de acreditados
      LET v_edo_procesar = 190        --Respuesta de desmarca rechazada
      LET v_diagnostico_hist = p_respuesta.diagProceso

      #Se inserta en el historico de transferancia un registro de solicitud de desmarca procesar
      EXECUTE exe_inserta_historico_acre USING  p_solicitud.id_origen,              --id_cre_acreditado
                                                p_solicitud.id_cre_ctr_archivo,     --id_cre_ctr_archivo
                                                p_solicitud.modulo_cod,             --tpo_transferencia     = 22
                                                v_edo_procesar,                     --edo_procesar          = 190
                                                v_diagnostico_hist,                 --diagnostico           = p_respuesta.diagProceso
                                                v_estado_hist,                      --estado                = 170
                                                p_solicitud.nss,                    --nss_afore
                                                p_solicitud.rfc,                    --rfc_afore
                                                p_solicitud.ap_paterno,             --paterno_afore
                                                p_solicitud.ap_materno,             --materno_afore
                                                p_solicitud.nombre,                 --nombre_afore
                                                v_nom_imss,                         --nom_imss              = null
                                                g_f_dia                             --f_proceso             = TODAY

      #se actualiza el estado de la tabla de transferencia a 190 - Proceso Respuesta desmarca rechazada
      #EXECUTE exe_sp_actualiza_edo_procesar USING p_solicitud.id_origen, v_edo_procesar
      EXECUTE exe_actualiza_edo_procesar USING v_edo_procesar, p_solicitud.id_origen

      LET v_cod_rechazo = p_respuesta.diagProceso

      #Se asignan los valores para solicitar un nuevo intento de desmarca
      LET v_num_rechazo = p_solicitud.intento + 1

     IF v_cod_rechazo = "-1" THEN     #Codigo de error en la comunicacion con el servidor
         LET v_dias_reintento = 0
      ELSE
         EXECUTE exe_consulta_rechazo USING v_cod_rechazo INTO v_dias_reintento
      END IF

      #Se programa el siguiente reintento para que se ejecute en la proxima ejecucion del proceso
      LET v_f_reintento = g_f_dia

      #Solo se programaran reintentos para los rechazos que configuren sus dias distintos a -1
      IF v_dias_reintento <> -1 THEN
         #Se inserta una nueva solicitud de desmarca por el rechazo de la actual
         EXECUTE exe_inserta_rechazo USING   p_solicitud.id_derechohabiente,
                                             p_solicitud.id_origen,
                                             p_solicitud.modulo_cod,
                                             p_solicitud.tpo_credito,
                                             p_solicitud.marca,
                                             v_f_reintento,                      --TODAY
                                             v_num_rechazo,
                                             v_cod_result_op,                    --p_solicitud.cod_result_op,
                                             v_diagnostico,                      --p_solicitud.diagnostico,
                                             p_solicitud.situacion,
                                             p_solicitud.num_credito,
                                             p_solicitud.f_infonavit,
                                             p_solicitud.marca_procesar,
                                             p_solicitud.folio_archivo,
                                             p_solicitud.usuario
      END IF
                                          
      #Estado que se asignara a la tabla de transferencia
      LET v_edo_procesar = 200
   END IF

   #Se inserta en el historico de transferancia un registro de solicitud de marca procesar
   EXECUTE exe_inserta_historico_acre USING     p_solicitud.id_origen,              --id_cre_acreditado
                                                p_solicitud.id_cre_ctr_archivo,     --id_cre_ctr_archivo
                                                p_solicitud.modulo_cod,             --tpo_transferencia     = 22
                                                v_edo_procesar,                     --edo_procesar          = 210 para aceptado o 200 para rechazado
                                                v_diagnostico_hist,                 --diagnostico           = p_respuesta.diagProceso
                                                v_estado_hist,                      --estado                = 170
                                                p_solicitud.nss,                    --nss_afore
                                                p_solicitud.rfc,                    --rfc_afore
                                                p_solicitud.ap_paterno,             --paterno_afore
                                                p_solicitud.ap_materno,             --materno_afore
                                                p_solicitud.nombre,                 --nombre_afore
                                                v_nom_imss,                         --nom_imss              = null
                                                g_f_dia                             --f_proceso             = TODAY

   #se actualiza el estado de la tabla de transferencia a 200 - Marca rechazada o 210 - Marca aceptada
   #EXECUTE exe_sp_actualiza_edo_procesar USING p_solicitud.id_origen, v_edo_procesar
   EXECUTE exe_actualiza_edo_procesar USING v_edo_procesar, p_solicitud.id_origen
END FUNCTION