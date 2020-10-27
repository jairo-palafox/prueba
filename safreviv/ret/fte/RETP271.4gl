--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP271                                                                 #
#OBJETIVO          =>Programa consulta las solicitudes de retiro generico enviadas a FICO    #
#                    para verificar si fueron pagadas o rechazadas por el banco              #
#                                                                                            #
##############################################################################################
{
Registro de modificaciones:
Autor           Fecha      Descrip. cambio
Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
                20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico

}
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS "ret_consPagoFico-SSL.inc" -- consulta de pago a FICO
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- nombre del usuario
       p_folio                       LIKE glo_folio.folio, -- numero de folio
       v_bandera                     SMALLINT,
       v_conteo                      INTEGER, -- contador de registros
       p_titulo                      STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                     STRING, -- cuerpo del mensaje enviado       
       v_sql                         STRING, -- cadena con enunciado SQL
       v_contador                    SMALLINT, -- contador de registros
       v_ws_status                   SMALLINT, -- estatus de ejecucion de un webservice
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud


--       v_r_ret_solicitud_generico    RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud de retiro
-- se pone emergente para actualizacion en produccion
v_r_ret_solicitud_generico RECORD
    id_solicitud decimal(9,0),
    id_derechohabiente decimal(9,0),
    nss char(11),
    rfc char(13),
    modalidad_retiro smallint,
    folio decimal(9,0),
    caso_adai char(10),
    id_archivo_envio decimal(9,0),
    id_archivo_respuesta decimal(9,0),
    folio_restitucion decimal(9,0),
    id_archivo_cancela_cxp decimal(9,0),
    id_archivo_resp_cxp decimal(9,0),
    f_solicitud date,
    h_solicitud datetime hour to second,
    estado_solicitud smallint,
    cod_rechazo SMALLINT
END RECORD,



       v_f_paso                      CHAR(10),
       v_cadena                      STRING, -- cadena auxiliar
       v_estatus_fico                SMALLINT, -- estatus de pago en fico en formato numerico
       v_r_ret_respuesta_fico        RECORD LIKE ret_respuesta_fico.*,
       v_cambio_cuenta               SMALLINT, -- booleana para ver si hubo cambio dee stado de la cuenta
       v_proceso_cod                 LIKE cat_proceso.proceso_cod, -- codigo de proceso
       v_marca                       LIKE sfr_marca.marca, -- marca del proceso
       v_r_ret_ws_consulta_pago_fico RECORD 
         id_solicitud         DECIMAL(9,0),
         documento            CHAR(10),
         ejercicio            CHAR(4),
         sociedad             CHAR(4),
         f_consulta           DATE,
         h_consulta           DATETIME HOUR TO SECOND,
         consec_respuesta     SMALLINT,
         rsp_documento        CHAR(10),
         rsp_ejercicio        CHAR(4),
         rsp_estatus          SMALLINT,
         rsp_importe          DECIMAL(22,2),
         rsp_ind_retencion    CHAR(2),
         rsp_referencia       CHAR(16),
         rsp_f_pago           CHAR(8)
      END RECORD -- registro de la respuesta de FICO
          
   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP271.log")
          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "CONSULA PAGO SACI-FICO")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con SAP-FICO"

   -- 16Dic2013. Se verifica si hay datos para consulta de pago FICO
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico A INNER JOIN
          ret_respuesta_fico R ON R.referencia = A.id_solicitud
   WHERE  A.estado_solicitud = 700 -- listos para consulta de pago FICO
   
   -- si no hay registros para consulta de pago FICO
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Consulta de pago FICO"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO AMORTIZACIONES EXCEDENTES\n",
                      "Operación    : CONSULTA DE PAGO FICO\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para la consulta de pago FICO.\nNo es necesario ejecutar esta etapa.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)

                       
   ELSE -- se ejecuta la consulta de pago FICO
      
      -- se leen las solicitudes en estatus de enviadas a tesoreria (FICO)
      DECLARE cur_conspago CURSOR FOR
      SELECT 
    id_solicitud,
    id_derechohabiente,
    nss ,
    rfc ,
    modalidad_retiro ,
    folio ,
    caso_adai ,
    id_archivo_envio ,
    id_archivo_respuesta ,
    folio_restitucion ,
    id_archivo_cancela_cxp ,
    id_archivo_resp_cxp ,
    f_solicitud ,
    h_solicitud ,
    estado_solicitud,
    cod_rechazo
      FROM   ret_solicitud_generico
      WHERE  estado_solicitud = 700
      AND    modalidad_retiro = 9 -- enviadas a tesoreria, SOLO CONSIDERA LAS DE AMORTIZACIONES EXCEDENTES

   --and id_solicitud in (6235232, 6235230)



      
      -- para cada solicitud encontrada
      FOREACH cur_conspago INTO v_r_ret_solicitud_generico.*
         LET v_r_ret_respuesta_fico.referencia = null
         -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
         LET v_sql = "\nSELECT FIRST 1 *",
                     "\nFROM   ret_respuesta_fico",
                     "\nWHERE  referencia = ?",
                     "\nORDER  BY folio"

         PREPARE sid_respfico FROM v_sql
         EXECUTE sid_respfico USING v_r_ret_solicitud_generico.id_solicitud
         INTO    v_r_ret_respuesta_fico.*
      
         -- se asume que la cuenta no cambia
         LET v_cambio_cuenta = FALSE
     
         -- si no se encontro su respuesta, se salta el registro
         IF ( v_r_ret_respuesta_fico.referencia IS NULL ) THEN     
            CONTINUE FOREACH
         END IF
      
         -- se consulta si ya se efectuo el pago o se rechazo
         LET ZFICO_CONSULTAPAGOTABLET.DOCUMENTO = v_r_ret_respuesta_fico.cta_x_pagar
         LET ZFICO_CONSULTAPAGOTABLET.EJERCICIO = v_r_ret_respuesta_fico.anho
         LET ZFICO_CONSULTAPAGOTABLET.SOCIEDAD  = v_r_ret_respuesta_fico.sociedad
         
         -- valores enviados a consulta de pago
         DISPLAY "Valores enviados a consulta de pago:"
         DISPLAY "documento: ", ZFICO_CONSULTAPAGOTABLET.DOCUMENTO
         DISPLAY "ejercicio: ", ZFICO_CONSULTAPAGOTABLET.EJERCICIO
         DISPLAY "sociedad : ", ZFICO_CONSULTAPAGOTABLET.SOCIEDAD

         -- se invoca la consulta de pago a FICO
         CALL SI_ConsultarEstatusPago_SO_g() RETURNING v_ws_status
         
   -- se forza la respuesta  
   --LET v_ws_status = 0     
   --LET consultarResponse.salida[1].fechaPago = "20131023"
   --LET consultarResponse.salida[1].referencia = v_r_ret_solicitud_generico.id_solicitud USING "&&&&&&&&&"



         
         -- si el webservice se ejecuto correctamente
         IF ( v_ws_status = 0 ) THEN      
            DISPLAY "CONSULTA DE PAGO WS ejecutada correctamente"
         
            FOR v_contador = 1 TO ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item.getLength()
               DISPLAY "documento         : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].DOCU
               DISPLAY "ejercicio         : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].EJER
               DISPLAY "estatus           : '", ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].ESTATUS, "'"
               DISPLAY "importe           : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].MONTO
               DISPLAY "indicadorRetencion: ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].WT_WITHCD
               DISPLAY "referencia        : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].REFPAGO
               DISPLAY "fechaPago         : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO
            
               -- si ya se pago, se actualiza la solicitud
               LET v_cadena = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].ESTATUS
               -- se transforma el estatus a numerico
               LET v_estatus_fico = v_cadena.trim()
               
               DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico

               -- 26 oct 2013. FICO dice que en rechazos no viene la fecha, por tanto
               -- se usara la fecha del dia
               IF ( ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO IS NULL OR 
                    ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO = "0000-00-00") THEN
                  LET ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO = TODAY USING "yyyymmdd"
               ELSE
                  LET v_f_paso = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO
                  LET ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO = v_f_paso[1,4]||v_f_paso[6,7]||v_f_paso[9,10]
               END IF

               -- se guarda la respuesta obtenida
               LET v_r_ret_ws_consulta_pago_fico.id_solicitud       = v_r_ret_solicitud_generico.id_solicitud
               LET v_r_ret_ws_consulta_pago_fico.documento          = v_r_ret_respuesta_fico.cta_x_pagar
               LET v_r_ret_ws_consulta_pago_fico.ejercicio          = v_r_ret_respuesta_fico.anho
               LET v_r_ret_ws_consulta_pago_fico.sociedad           = v_r_ret_respuesta_fico.sociedad
               LET v_r_ret_ws_consulta_pago_fico.f_consulta         = TODAY
               LET v_r_ret_ws_consulta_pago_fico.h_consulta         = CURRENT HOUR TO SECOND
               LET v_r_ret_ws_consulta_pago_fico.consec_respuesta   = v_contador
               LET v_r_ret_ws_consulta_pago_fico.rsp_documento      = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].DOCU
               LET v_r_ret_ws_consulta_pago_fico.rsp_ejercicio      = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].EJER
               LET v_r_ret_ws_consulta_pago_fico.rsp_estatus        = v_estatus_fico
               LET v_r_ret_ws_consulta_pago_fico.rsp_importe        = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].MONTO
               LET v_r_ret_ws_consulta_pago_fico.rsp_ind_retencion  = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].WT_WITHCD
               LET v_r_ret_ws_consulta_pago_fico.rsp_referencia     = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].REFPAGO
               LET v_r_ret_ws_consulta_pago_fico.rsp_f_pago         = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].FECHPAGO

               -- se inserta la respuesta
              -- INSERT INTO ret_ws_consulta_pago_fico VALUES ( v_r_ret_ws_consulta_pago_fico.* )
              INSERT INTO ret_ws_consulta_pago_fico (id_solicitud, 
                                                     documento, 
                                                     ejercicio, 
                                                     sociedad, 
                                                     f_consulta,
                                                     h_consulta, 
                                                     consec_respuesta, 
                                                     rsp_documento, 
                                                     rsp_ejercicio,
                                                     rsp_estatus, 
                                                     rsp_importe, 
                                                     rsp_ind_retencion, 
                                                     rsp_referencia,
                                                     rsp_f_pago)
                                              VALUES (v_r_ret_ws_consulta_pago_fico.id_solicitud,
                                                      v_r_ret_ws_consulta_pago_fico.documento,
                                                      v_r_ret_ws_consulta_pago_fico.ejercicio,
                                                      v_r_ret_ws_consulta_pago_fico.sociedad,
                                                      v_r_ret_ws_consulta_pago_fico.f_consulta,
                                                      v_r_ret_ws_consulta_pago_fico.h_consulta,
                                                      v_r_ret_ws_consulta_pago_fico.consec_respuesta,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_documento,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_ejercicio,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_estatus,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_importe,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_ind_retencion,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_referencia,
                                                      v_r_ret_ws_consulta_pago_fico.rsp_f_pago);
               
               CASE v_estatus_fico
               
                  WHEN 2       
                     DISPLAY "Pagado"
                  WHEN 3       
                     DISPLAY "Vencido"
                  WHEN 4       
                     DISPLAY "Rechazado"
                  WHEN 20      
                     DISPLAY "Esta Autorizado"
                  WHEN 21      
                     DISPLAY "Operado"
                  WHEN 22      
                     DISPLAY "Protegido"
                  WHEN 23      
                     DISPLAY "Anulado"
                  WHEN 24      
                     DISPLAY "Cheque Cancelado"
               
               END CASE
               
               
   -- QUITAR, ESTO ES PARA PRUEBAS
   --            LET v_estatus_fico = 2
   -- se acepta ese
   {
   if ( v_r_ret_solicitud_generico.id_solicitud = 6235230 ) THEN
     LET v_estatus_fico = 2
   ELSE
     -- se rechaza este
     IF ( v_r_ret_solicitud_generico.id_solicitud = 6235232 ) then
        LET v_estatus_fico = 4
     else
        let v_estatus_fico = 0
     end if
   END IF
               }
               
               
               
               {
               Codigos de Retorno en Estatus	
               Codigo	Descripcion

               2       Pagado

               3       Vencido
               4       Rechazado
               20      Esta Autorizado
               21      Operado
               22      Protegido
               23      Anulado
               24      Cheque Cancelado
               
               }
               -- ESTATUS 2 es pagado
               IF ( v_estatus_fico = 2 ) THEN
                             

                  -- se comunica como aceptada/pagada
                  LET v_estado_solicitud = 71
                  LET v_cambio_cuenta = TRUE
                        
                  UPDATE ret_solicitud_generico
                  SET    estado_solicitud = v_estado_solicitud -- pagada
                  WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                  
                        LET v_sql = "\nUPDATE ret_amort_excedente"
                        LET v_proceso_cod = g_proceso_cod_ret_amort_excedentes
                        LET v_marca = 810
                        
                        -- se complementa la actualizacion
                  LET v_sql = v_sql , "\nSET    estado_solicitud = ?",
                                      "\nWHERE  id_solicitud     = ?"
                        
                        --DISPLAY v_sql
                        -- se ejecuta la actualizacion
                  PREPARE sid_actpago FROM v_sql
                  EXECUTE sid_actpago USING v_estado_solicitud, v_r_ret_solicitud_generico.id_solicitud

               ELSE
                  -- se verifica si fue rechazado           
                  IF ( ( v_estatus_fico = 3  ) OR -- VENCIDO
                       ( v_estatus_fico = 4  ) OR -- RECHAZADO
                       ( v_estatus_fico = 23 ) OR -- ANULADO
                       ( v_estatus_fico = 24 )    -- CHEQUE CANCELADO
                                                  ) THEN
                     {
                     Codigos de Retorno en Estatus	
                     Codigo	Descripcion
                     2       Pagado
                     3       Vencido
                     4       Rechazado
                     20      Esta Autorizado
                     21      Operado
                     22      Protegido
                     23      Anulado
                     24      Cheque Cancelado
                     }
                     
                     -- estado rechazo
                     LET v_estado_solicitud = 90
                     LET v_cambio_cuenta = TRUE
                           
                     -- si fue rechazada, se actualiza la solicitud
                     UPDATE ret_solicitud_generico
                     SET    estado_solicitud = v_estado_solicitud, -- rechazada por el banco
                            cod_rechazo      = 65 -- rechazada por el banco
                     WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud

                           LET v_sql = "\nUPDATE ret_amort_excedente"
                           LET v_proceso_cod = g_proceso_cod_ret_amort_excedentes
                           LET v_marca = 810
                           
                     -- se complementa la actualizacion
                     LET v_sql = v_sql , "\nSET    estado_solicitud = ?,",
                                         "\n       cod_rechazo      = 65",
                                         "\nWHERE  id_solicitud     = ? "
                           
                     -- se ejecuta la actualizacion
                     PREPARE sid_rechpago FROM v_sql
                     EXECUTE sid_rechpago USING v_estado_solicitud, v_r_ret_solicitud_generico.id_solicitud

                  END IF
               END IF
               
            END FOR
         ELSE
            DISPLAY "ERROR al invocar webservice de consulta de pago"
            DISPLAY "CODE       : ", wsError.code
            DISPLAY "CODENS     : ", wsError.codeNS
            DISPLAY "DESRIPTION : ", wsError.description
            DISPLAY "ACTION     : ", wsError.action
         END IF

      END FOREACH


      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "RETIRO AMORTIZACIONES EXCEDENTES - CONSULTA DE PAGO SACI-FICO."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - RETIRO AMORTIZACIONES EXCEDENTES - CONSULTA PAGO SACI-FICO"
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "CONSULTA PAGO SACI-FICO")
END MAIN