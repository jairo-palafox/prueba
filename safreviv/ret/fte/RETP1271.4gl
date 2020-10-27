--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP1271                                                                #
#OBJETIVO          =>Programa consulta las solicitudes de retiro generico enviadas a FICO    #
#                    para verificar si fueron pagadas o rechazadas por el banco              #
#                                                                                            #
##############################################################################################
{
Registro de modificaciones:
Autor           Fecha      Descrip. cambio

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
       v_conteo_benef                INTEGER, -- contador de registros beneficiarios       
       p_titulo                      STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                     STRING, -- cuerpo del mensaje enviado       
       v_sql                         STRING, -- cadena con enunciado SQL
       v_contador                    SMALLINT, -- contador de registros
       v_ws_status                   SMALLINT, -- estatus de ejecucion de un webservice
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud


--       v_r_ret_solicitud_generico    RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud de retiro
-- se pone emergente para actualizacion en produccion
v_r_ret_solicitud_generico RECORD
    id_solicitud               DECIMAL(9,0),
    id_derechohabiente         DECIMAL(9,0),
    nss                        CHAR(11),
    rfc                        CHAR(13),
    modalidad_retiro           SMALLINT,
    folio                      DECIMAL(9,0),
    caso_adai                  CHAR(10),
    id_archivo_envio           DECIMAL(9,0),
    id_archivo_respuesta       DECIMAL(9,0),
    folio_restitucion          DECIMAL(9,0),
    id_archivo_cancela_cxp     DECIMAL(9,0),
    id_archivo_resp_cxp        DECIMAL(9,0),
    f_solicitud                DATE,
    h_solicitud                DATETIME HOUR TO SECOND,
    estado_solicitud           SMALLINT,
    cod_rechazo                SMALLINT,
    tipo_solicitud             SMALLINT
END RECORD,



       v_f_paso                      CHAR(10),
       v_cadena                      STRING, -- cadena auxiliar
       v_estatus_fico                SMALLINT, -- estatus de pago en fico en formato numerico
       v_r_ret_respuesta_fico        RECORD LIKE ret_respuesta_fico.*,
       v_cambio_cuenta               SMALLINT, -- booleana para ver si hubo cambio dee stado de la cuenta
       v_proceso_cod                 LIKE cat_proceso.proceso_cod, -- codigo de proceso
       v_marca                       LIKE sfr_marca.marca, -- marca del proceso
       v_r_ret_ws_consulta_pago_fico RECORD LIKE ret_ws_consulta_pago_fico.* -- registro de la respuesta de FICO

DEFINE v_beneficiario_procesados     SMALLINT
DEFINE v_total_beneficiarios         SMALLINT
DEFINE v_beneficiarios_71            SMALLINT
DEFINE v_cod_rechazo                 SMALLINT
DEFINE v_id_solicitud_sin_consec     DECIMAL(9,0)
        
   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP1271.log")
          
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
   AND    A.modalidad_retiro = 3
   SELECT COUNT(*)
   INTO   v_conteo_benef
   FROM   ret_beneficiario_juridico a INNER JOIN
          ret_respuesta_fico r ON r.referencia = a.id_solicitud||a.consec_beneficiario
   WHERE  a.estado_solicitud = 700
   
   -- si no hay registros para consulta de pago FICO
   IF ( (v_conteo + v_conteo_benef) < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Consulta de pago FICO"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO Ley 73\n",
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
             a.id_solicitud,
             a.id_derechohabiente,
             a.nss ,
             a.rfc ,
             a.modalidad_retiro ,
             a.folio ,
             a.caso_adai ,
             a.id_archivo_envio ,
             a.id_archivo_respuesta ,
             a.folio_restitucion ,
             a.id_archivo_cancela_cxp ,
             a.id_archivo_resp_cxp ,
             a.f_solicitud ,
             a.h_solicitud ,
             a.estado_solicitud,
             a.cod_rechazo,
             1
      FROM   ret_solicitud_generico a,
             ret_beneficiario_generico b
      WHERE  a.id_solicitud = b.id_solicitud
      AND    b.tpo_beneficiario = 1
      AND    a.estado_solicitud = 700 -- enviadas a tesoreria
      AND    a.modalidad_retiro = 3
      UNION 
      SELECT 
             to_number(b.id_solicitud||b.consec_beneficiario),
             a.id_derechohabiente,
             a.nss ,
             a.rfc ,
             a.modalidad_retiro ,
             a.folio ,
             a.caso_adai ,
             a.id_archivo_envio ,
             a.id_archivo_respuesta ,
             a.folio_restitucion ,
             a.id_archivo_cancela_cxp ,
             a.id_archivo_resp_cxp ,
             a.f_solicitud ,
             a.h_solicitud ,
             b.estado_solicitud,
             b.cod_rechazo,
             2
      FROM   ret_solicitud_generico a,
             ret_beneficiario_juridico b,
             ret_beneficiario_generico c
      WHERE  a.id_solicitud = b.id_solicitud
      AND    a.id_solicitud = c.id_solicitud
      AND    b.consec_beneficiario = c.consec_beneficiario
      AND    c.tpo_beneficiario = 2
      AND    b.estado_solicitud = 700 -- enviadas a tesoreria
      AND    a.modalidad_retiro = 3
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
               DISPLAY "Desc estatus      : ",  ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].DESCSTAT
            
            
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
               LET v_r_ret_ws_consulta_pago_fico.rsp_desc_estatus   = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[v_contador].DESCSTAT

               -- se inserta la respuesta
               INSERT INTO ret_ws_consulta_pago_fico VALUES ( v_r_ret_ws_consulta_pago_fico.* )
               
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
               
               -- ESTATUS 2 es pagado
               IF ( v_estatus_fico = 2 ) THEN
                             

                  -- se comunica como aceptada/pagada
                  LET v_estado_solicitud = 71
                  LET v_cambio_cuenta = TRUE
                  IF v_r_ret_solicitud_generico.tipo_solicitud = 1 THEN       
                     UPDATE ret_solicitud_generico
                     SET    estado_solicitud = v_estado_solicitud -- pagada
                     WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud

                     UPDATE ret_ley73_generico
                     SET    estado_solicitud = v_estado_solicitud
                     WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                     
                     LET v_proceso_cod = g_proceso_cod_ret_Ley73_arch
                     LET v_marca = 803
                  ELSE
                     UPDATE ret_beneficiario_juridico
                     SET    estado_solicitud = v_estado_solicitud -- pagada
                     WHERE  id_solicitud||consec_beneficiario = v_r_ret_solicitud_generico.id_solicitud
                  END IF 

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
                     IF v_r_ret_solicitud_generico.tipo_solicitud = 1 THEN       
                        -- si fue rechazada, se actualiza la solicitud
                        UPDATE ret_solicitud_generico
                        SET    estado_solicitud = v_estado_solicitud, -- rechazada por el banco
                               cod_rechazo      = 65 -- rechazada por el banco
                        WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud

                        UPDATE ret_ley73_generico
                        SET    estado_solicitud = v_estado_solicitud,
                               cod_rechazo      = 65
                        WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                     ELSE
                        UPDATE ret_beneficiario_juridico
                        SET    estado_solicitud = v_estado_solicitud, -- rechazada por el banco
                               cod_rechazo      = 65 -- rechazada por el banco
                        WHERE  id_solicitud||consec_beneficiario = v_r_ret_solicitud_generico.id_solicitud
                     END IF 
                  END IF
               END IF
               
            END FOR
            IF v_r_ret_solicitud_generico.tipo_solicitud = 2 THEN  
               LET v_id_solicitud_sin_consec = 0
               LET v_beneficiario_procesados = 0
               LET v_total_beneficiarios     = 0
               LET v_beneficiarios_71        = 0
               LET v_estado_solicitud        = 0
               LET v_cod_rechazo             = 0
               --- Se valida el proceso de todos los beneficiarios para actualizar la solicitud
               SELECT COUNT(*)
               INTO   v_beneficiario_procesados
               FROM   ret_beneficiario_juridico a,
                      ret_beneficiario_generico b
               WHERE  a.id_solicitud        = b.id_solicitud
               AND    a.consec_beneficiario = b.consec_beneficiario
               AND    a.estado_solicitud    IN (71,90,209,210,214)
               AND    b.porcentaje          > 0
               AND    a.id_solicitud        IN (SELECT id_solicitud
                                                FROM   ret_beneficiario_juridico
                                                WHERE  id_solicitud||consec_beneficiario = v_r_ret_solicitud_generico.id_solicitud);                     
               SELECT COUNT(*), id_solicitud
               INTO   v_total_beneficiarios, v_id_solicitud_sin_consec
               FROM   ret_beneficiario_generico
               WHERE  id_solicitud IN (SELECT id_solicitud
                                       FROM   ret_beneficiario_juridico
                                       WHERE  id_solicitud||consec_beneficiario = v_r_ret_solicitud_generico.id_solicitud)
               AND    porcentaje   > 0
               GROUP  BY id_solicitud;

               IF v_beneficiario_procesados = v_total_beneficiarios THEN 
                  -- Busca el estado que se le pondrá a la solicitud
                  SELECT COUNT(*)
                  INTO   v_beneficiarios_71
                  FROM   ret_beneficiario_juridico a,
                         ret_beneficiario_generico b
                  WHERE  a.id_solicitud        = b.id_solicitud
                  AND    a.consec_beneficiario = b.consec_beneficiario
                  AND    a.estado_solicitud    IN (71)
                  AND    b.porcentaje          > 0
                  AND    a.id_solicitud        IN (SELECT id_solicitud
                                                   FROM   ret_beneficiario_juridico
                                                   WHERE  id_solicitud||consec_beneficiario = v_r_ret_solicitud_generico.id_solicitud);
                  IF v_beneficiarios_71 = v_total_beneficiarios THEN 
                     LET v_estado_solicitud = 71;
                     LET v_cod_rechazo      =   0;
                  ELSE 
                     IF v_beneficiarios_71 = 0 THEN 
                        LET v_estado_solicitud = 90;
                        LET v_cod_rechazo      = 65;
                     ELSE 
                        LET v_estado_solicitud = 710;   --- Pagada Parcialmente
                        LET v_cod_rechazo      =  65;
                     END IF 
                  END IF 
                  UPDATE ret_solicitud_generico 
                  SET    estado_solicitud     = v_estado_solicitud,
                         cod_rechazo          = v_cod_rechazo
                  WHERE  id_solicitud         = v_id_solicitud_sin_consec
                  AND    estado_solicitud     IN (700, 790);
                     
                  UPDATE ret_ley73_generico
                  SET    estado_solicitud = v_estado_solicitud,
                         cod_rechazo      = v_cod_rechazo
                  WHERE  id_solicitud     = v_id_solicitud_sin_consec
                  AND    estado_solicitud IN (700, 790);
               END IF 
            END IF 
         ELSE
            DISPLAY "ERROR al invocar webservice de consulta de pago"
            DISPLAY "CODE       : ", wsError.code
            DISPLAY "CODENS     : ", wsError.codeNS
            DISPLAY "DESRIPTION : ", wsError.description
            DISPLAY "ACTION     : ", wsError.action
         END IF

      END FOREACH
      -- Valida los beneficiario para actualizar la solicitud

      ----
      

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "RETIRO GENERICO - CONSULTA DE PAGO SACI-FICO."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - RETIRO GENERICO - CONSULTA PAGO SACI-FICO"
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "CONSULTA PAGO SACI-FICO")
END MAIN