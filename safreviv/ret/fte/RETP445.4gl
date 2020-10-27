--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP445                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de la carga del archivo del Excepciones de la Devolución del SSV       #
#Fecha inicio => AGOSTO 8, 2017                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS "ret_consPagoFicoPrevio.inc" -- consulta de pago a FICO
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_error_isam          INTEGER
       ,v_s_comando           STRING
       ,v_mensaje             VARCHAR(250)
       ,v_nss_error           LIKE afi_derechohabiente.nss

DEFINE v_r_excep_devol_ssv   RECORD 
       id_solicitud          DECIMAL(9,0),
       nss                   CHAR(11)
   END RECORD 

DEFINE v_contador            INTEGER 
DEFINE v_cadena              STRING 
DEFINE v_estatus_fico        SMALLINT
DEFINE v_r_ret_ws_consulta_pago_fico_previo RECORD LIKE ret_excep_pagos_previos.*
DEFINE v_f_paso              CHAR(10)

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- Excepciones de la Devolución del SSV
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
   
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0
   LET v_contador            = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_integra_excep_devol_ssv(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_nss_error
   
   -- Cierra la operación
   DISPLAY "La integración se terminó completamente."
   DISPLAY "Estatus de integración:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : EXCEPCIONES DE LA DEVOLUCION DEL SSV\n",
                   "Operación    : INTEGRACIÓN\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n",
                   "El folio asociado a su operación es: ", p_folio, "\n\n"

   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
------- *******************************************************************************
-------  Verifica si tiene pagos previos
      DECLARE cur_conspagoprevio CURSOR FOR
      SELECT 
             id_solicitud,
             nss 
      FROM   ret_excep_devol_ssv
      WHERE  estado_solicitud = 10 -- recibidas
      AND    folio            = p_folio

      
      -- para cada solicitud encontrada
      FOREACH cur_conspagoprevio INTO v_r_excep_devol_ssv.*
     
         -- si no se encontro su respuesta, se salta el registro
         IF ( v_r_excep_devol_ssv.id_solicitud IS NULL OR 
              v_r_excep_devol_ssv.nss IS NULL ) THEN     
            CONTINUE FOREACH
         END IF
      
         -- se consulta si ya se efectuo el pago o se rechazo
         LET MT_ConsultaHistPagoSSV_req.EJERCICIO = '0000' -- YEAR(TODAY)
--         LET MT_ConsultaHistPagoSSV_req.SOCIEDAD  = ' '
--         LET MT_ConsultaHistPagoSSV_req.EJERCICIO = 2001
         LET MT_ConsultaHistPagoSSV_req.SOCIEDAD  = 'INFO'
         LET MT_ConsultaHistPagoSSV_req.I_NSS     = v_r_excep_devol_ssv.nss

         -- valores enviados a consulta de pago
         DISPLAY "Valores enviados a consulta de pagos previos:"
         DISPLAY "ejercicio: ", MT_ConsultaHistPagoSSV_req.EJERCICIO
         DISPLAY "sociedad : ", MT_ConsultaHistPagoSSV_req.SOCIEDAD
         DISPLAY "NSS      : ", MT_ConsultaHistPagoSSV_req.I_NSS

         -- se invoca la consulta de pagos previos a FICO 
         CALL SI_ConsultaHistPagoSSV_SO_g() RETURNING v_i_resultado
         
         -- si el webservice se ejecuto correctamente
         IF ( v_i_resultado = 0 ) THEN      
            DISPLAY "CONSULTA DE PAGOS PREVIOS WS ejecutada correctamente"
         
            FOR v_contador = 1 TO MT_ConsultaHistPagoSSV_res.MessPago.getLength()
               DISPLAY "documento         : ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].DOCU
               DISPLAY "ejercicio         : ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].EJER
               DISPLAY "estatus           : '", MT_ConsultaHistPagoSSV_res.MessPago[v_contador].ESTATUS, "'"
               DISPLAY "importe           : ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].MONTO
               DISPLAY "indicadorRetencion: ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].WT_WITHCD
               DISPLAY "referencia        : ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].REFPAGO
               DISPLAY "fechaPago         : ",  MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO
            
               -- si ya se pago, se actualiza la solicitud
               LET v_cadena = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].ESTATUS
               -- se transforma el estatus a numerico
               LET v_estatus_fico = v_cadena.trim()
               
               DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico

               -- se guarda la respuesta obtenida
               LET v_r_ret_ws_consulta_pago_fico_previo.id_solicitud       = v_r_excep_devol_ssv.id_solicitud
               LET v_r_ret_ws_consulta_pago_fico_previo.nss                = v_r_excep_devol_ssv.nss
               LET v_r_ret_ws_consulta_pago_fico_previo.documento          = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].DOCU
               LET v_r_ret_ws_consulta_pago_fico_previo.ejercicio          = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].EJER
               LET v_r_ret_ws_consulta_pago_fico_previo.estatus            = v_estatus_fico
               LET v_r_ret_ws_consulta_pago_fico_previo.f_pago             = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO
               LET v_r_ret_ws_consulta_pago_fico_previo.monto              = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].MONTO
               LET v_r_ret_ws_consulta_pago_fico_previo.referencia         = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].REFPAGO
               LET v_r_ret_ws_consulta_pago_fico_previo.wt_withcd          = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].WT_WITHCD

               IF ( MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO IS NULL OR 
                    MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO = "0000-00-00") THEN
                  LET MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO = TODAY USING "yyyymmdd"
               ELSE
                  LET v_f_paso = MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO
                  LET MT_ConsultaHistPagoSSV_res.MessPago[v_contador].FECHPAGO = v_f_paso[1,4]||v_f_paso[6,7]||v_f_paso[9,10]
               END IF

               -- se inserta la respuesta
               INSERT INTO ret_excep_pagos_previos VALUES ( v_r_ret_ws_consulta_pago_fico_previo.* )
               
            END FOR
         ELSE
            DISPLAY "ERROR al invocar webservice de consulta de pagos previos "
            DISPLAY "CODE       : ", wsError.code
            DISPLAY "CODENS     : ", wsError.codeNS
            DISPLAY "DESRIPTION : ", wsError.description
            DISPLAY "ACTION     : ", wsError.action
         END IF

      END FOREACH

------- *******************************************************************************
      DISPLAY v_mensaje

      DISPLAY "___________________________________________________________________________________"
      DISPLAY "Generando archivos de aceptados y rechazados: "
      -- parametros folio, usuario, incluir_rechazos, es_previo
      LET v_s_comando = "fglrun RETS445.42r ",p_folio ," ", p_usuario_cod, " 1 1"
      RUN v_s_comando
      
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración realizada con éxito.\n"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación.\n"
      DISPLAY "El proceso de Integración ha finalizado pero con errores de validación.\n"
      DISPLAY "Error (SQL)         : ", v_i_resultado
      DISPLAY "Error (ISAM)        : ", v_error_isam
      DISPLAY "Error (Mensaje)     : ", v_mensaje
      DISPLAY "Ultimo NSS procesado: ", v_nss_error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado

   END IF

   LET p_titulo = "Finalización de operación - EXCEPCION DE LA DEVOLUCIÓN DEL SSV - INTEGRACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION")


END MAIN