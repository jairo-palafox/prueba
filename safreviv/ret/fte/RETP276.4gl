--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP276                                                                 #
#OBJETIVO          =>Programa que notifica las solicitudes de retiro pagadas y canceladas a  #
#                    PROCESAR de las solicitudes recibidas por ventanilla AFORE              #
# Autor           Fecha      Modificación                                                    #
# Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
#                 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
--GLOBALS "confirmaPagoAdai.dir/ret_adai_confirma_pago_adai.inc" -- notificacion a ADAI
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       arr_respuesta_fico DYNAMIC ARRAY OF RECORD 
         documento             CHAR(10),          
         ejercicio             CHAR(4) ,          
         estatus               SMALLINT,          
         importe               DECIMAL(22,2),      
         indicadorRetencion    CHAR(2),           
         referencia            CHAR(16),          
         fechaPago             CHAR(8)            
      END RECORD                                  
END GLOBALS                                       

MAIN
DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- nombre del usuario
       p_folio                       LIKE glo_folio.folio, -- numero de folio
       v_bandera                     SMALLINT,
       p_titulo                      STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                     STRING, -- cuerpo del mensaje enviado       
       v_sql                         STRING, -- cadena con enunciado SQL
       v_r_afi_derechohabiente       RECORD LIKE afi_derechohabiente.*, -- registro de derechohabiente
       v_contador                    SMALLINT, -- contador de registros
       v_total_aivs                  LIKE ret_voluntaria.total_aivs,
       v_id_solicitud                LIKE ret_voluntaria.id_solicitud,
       v_clave_acreedor              VARCHAR(10),
       v_ws_status                   SMALLINT, -- estatus de ejecucion de un webservice
       v_status                      SMALLINT, -- estatus consulta FICO
       v_folio_restitucion           LIKE glo_folio.folio,
       v_i_resultado                 INTEGER,                             -- resultado del proceso
       v_error_isam                  INTEGER,
       v_mensaje                     VARCHAR(255),
       v_solicitud_error             LIKE ret_fondo_ahorro_generico.id_solicitud,
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud
       v_titulo_notificacion         STRING, -- titulo de la notificacion al beneficiario
       v_mensaje_notificacion        STRING, -- mensaje de la notificacion al beneficiario
       v_r_ret_solicitud_generico    RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud de retiro
       v_r_ret_beneficiario_generico RECORD LIKE ret_beneficiario_generico.*, -- registro de beneficiario
       v_cadena                      STRING, -- cadena auxiliar
       v_estatus_fico                SMALLINT, -- estatus de pago en fico en formato numerico
       v_fecha_pago                  CHAR(8), -- fecha de pago
       v_r_ret_respuesta_fico        RECORD LIKE ret_respuesta_fico.*,
       v_cambio_cuenta               SMALLINT, -- booleana para ver si hubo cambio dee stado de la cuenta
       v_caso_adai_numerico          DECIMAL(10,0), -- caso adai en numero
       v_caso_adai_cadena            STRING, -- caso adai en cadena
       v_proceso_cod                 LIKE cat_proceso.proceso_cod, -- codigo de proceso
       v_marca                       LIKE sfr_marca.marca, -- marca del proceso
       v_caso_adai_buscado           LIKE ret_solicitud_generico.caso_adai,
       v_num_solicitudes_caso        SMALLINT, -- numero de solicitudes de un caso adai
       v_num_solicitudes_con_result  SMALLINT, -- numero de solicitudes que ya tienen resultado de pago/cancelacion
       v_referencia_bancaria         LIKE ret_pago_dap.cve_referencia -- referencia bancaria de pagos DAP


   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP276.log")
          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACION PROCESAR")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con PROCESAR para notificación de solicitudes aceptadas y rechazadas"

   
   -- se leen las solicitudes en estatus de enviadas a tesoreria (FICO) del caso adai
   DECLARE cur_conspago CURSOR FOR
   SELECT *
   FROM   ret_solicitud_generico
   WHERE  grupo_ventanilla = gi_ventanilla_afore -- ventanilla afore
   AND    estado_solicitud IN (71, 212) -- pagadas y con cuenta por pagar cancelada
   OR     ( estado_solicitud = 210 AND
            cod_rechazo = 64 )
   
   
   --and id_solicitud in (6235232, 6235230)
   
   -- para cada solicitud encontrada
   FOREACH cur_conspago INTO v_r_ret_solicitud_generico.*
   
      -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
      SELECT *
      INTO   v_r_ret_respuesta_fico.*
      FROM   ret_respuesta_fico
      WHERE  referencia = v_r_ret_solicitud_generico.id_solicitud
   
      -- se asume que la cuenta no cambia
      LET v_cambio_cuenta = FALSE      
   
      -- si no se encontro su respuesta, se salta el registro
      IF ( v_r_ret_respuesta_fico.referencia IS NULL ) THEN     
         CONTINUE FOREACH
      END IF
   
      -- se invoca la consulta de pago a FICO
      CALL fn_consulta_FICO(v_r_ret_respuesta_fico.cta_x_pagar,
                            v_r_ret_respuesta_fico.anho,
                            v_r_ret_respuesta_fico.sociedad) 
         RETURNING v_status

      --validación si no se tiene información el registro de solicitud
      IF (v_status = FALSE) THEN
         DISPLAY "NO EXISTE INFORMACIÓN AL CONSULTAR LA RESPUESTA DE FICO"
         CONTINUE FOREACH
      END IF

      -- si el webservice se ejecuto correctamente y tiene información
      DISPLAY "CONSULTA DE PAGO a FICO ejecutada correctamente y con datos"
      
      FOR v_contador = 1 TO arr_respuesta_fico.getLength()
         DISPLAY "documento         : ", arr_respuesta_fico[v_contador].documento
         DISPLAY "ejercicio         : ", arr_respuesta_fico[v_contador].ejercicio
         DISPLAY "estatus           : ", arr_respuesta_fico[v_contador].estatus
         DISPLAY "importe           : ", arr_respuesta_fico[v_contador].importe
         DISPLAY "indicadorRetencion: ", arr_respuesta_fico[v_contador].indicadorRetencion
         DISPLAY "referencia        : ", arr_respuesta_fico[v_contador].referencia
         DISPLAY "fechaPago         : ", arr_respuesta_fico[v_contador].fechaPago
      
         -- si ya se pago, se actualiza la solicitud
         LET v_cadena = arr_respuesta_fico[v_contador].estatus
         -- se transforma el estatus a numerico
         LET v_estatus_fico = v_cadena.trim()
         
         DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico
   
          -- 26 oct 2013. FICO dice que en rechazos no viene la fecha, por tanto
          -- se usara la fecha del dia
          IF ( arr_respuesta_fico[v_contador].fechaPago IS NULL ) THEN
             LET arr_respuesta_fico[v_contador].fechaPago = TODAY USING "yyyymmdd"
          END IF
         
         CASE v_estatus_fico
            WHEN  2 DISPLAY "Pagado"
            WHEN  3 DISPLAY "Vencido"
            WHEN  4 DISPLAY "Rechazado"
            WHEN 20 DISPLAY "Esta Autorizado"
            WHEN 21 DISPLAY "Operado"
            WHEN 22 DISPLAY "Protegido"
            WHEN 23 DISPLAY "Anulado"
            WHEN 24 DISPLAY "Cheque Cancelado"
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

         -- ESTATUS 2 es pagado
         IF ( v_estatus_fico = 2 ) THEN
            -- se informa a ADAI el cambio de estatus
            --LET v_caso_adai_numerico = v_r_ret_solicitud_generico.caso_adai
            --LET v_caso_adai_cadena   = v_caso_adai_numerico USING "&&&&&&&&&&"
   --
            --LET ns2ZcrmWsConfirmacionPago.ICaso = v_caso_adai_cadena
            --LET ns2ZcrmWsConfirmacionPago.INss = v_r_ret_solicitud_generico.nss
            --LET ns2ZcrmWsConfirmacionPago.IRfc = v_r_ret_solicitud_generico.rfc
            -- arreglo de retiros
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = "00"
            --
            -- si la modalidad de retiro es fondo de ahorro
            --IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
               -- se veriica si tiene DAP
               --SELECT cve_referencia
               --INTO   v_referencia_bancaria
               --FROM   ret_pago_dap
               --WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
               --
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
            --ELSE
               -- es otra modalidad de retiro y se paga por SPEI
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = ""
            --END IF
            --
            -- para formatear fecha
            --LET v_fecha_pago = arr_respuesta_fico[v_contador].fechaPago
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha      = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha      = arr_respuesta_fico[v_contador].fechaPago
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro  = v_r_ret_solicitud_generico.modalidad_retiro
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago    = arr_respuesta_fico[v_contador].referencia
            --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci = 80 -- pagado en ADAI
            --
            --
            --DISPLAY "\n\nDATOS ENVIADOS A ADAI..."
            --DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
            --DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
            --DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
            --DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
            --DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap       
            --DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha     
            --DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro 
            --DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago   
            --DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
   
   
            -- se invoca el WS de confirmacion para ADAI
            --CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
            
LET v_ws_status = 0 --quitar este es para prueba
            -- si no se ejecuto correctamente
            IF ( v_ws_status <> 0 ) THEN
               --DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
               --DISPLAY "CODE       : ", wsError.code
               --DISPLAY "CODENS     : ", wsError.codeNS
               --DISPLAY "DESRIPTION : ", wsError.description
               --DISPLAY "ACTION     : ", wsError.action
               CONTINUE FOR
            END IF
            DISPLAY "WS de confirmacion de pago PROCESA ejecutado correctamente"
{               DISPLAY "Caso ADAI   : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
               DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
               DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
               DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
               DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
}
            -- si se no se actualizo correctamente en ADAI
            IF ( FALSE ) THEN--cambiar el true por el correcto
            --ELSE
               --DISPLAY "No se pudo confirmar en ADAI..."
               --CASE ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                  --WHEN 1
                     --DISPLAY "01: Favor de llenar campos obligatorios"
   --
                  --WHEN 2
                     --DISPLAY "02: No existe caso en tabla de Devolución de pagos"
   --
                  --WHEN 3
                     --DISPLAY "03: La modalidad de Retiro no coincide con el Número de Caso"
   --
                  --WHEN 4
                     --DISPLAY "04: No se actualizó el estatus a comprobante de Pago"
   --
                  --OTHERWISE
                     --DISPLAY "Código no reconocido: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
               --END CASE                     
               CONTINUE FOR
            END IF
            DISPLAY "Estatus confirmado en PROCESAR..."
            
            -- se comunica como aceptada/pagada
            LET v_estado_solicitud = 72
            LET v_cambio_cuenta = TRUE
            
            -- se actualiza la solicitud en la tabla de retiro en turno
            CALL fn_cambia_estado_solicitud(v_estado_solicitud,v_r_ret_solicitud_generico.id_solicitud,v_r_ret_solicitud_generico.modalidad_retiro)
               RETURNING v_proceso_cod,v_marca
   
            -- se desmarca la cuenta
            CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                 v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                 "saferviv", v_proceso_cod)
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
               
               -- se informa a ADAI el RECHAZO DEL PAGO
               --LET v_caso_adai_numerico = v_r_ret_solicitud_generico.caso_adai
               --LET v_caso_adai_cadena   = v_caso_adai_numerico USING "&&&&&&&&&&"
               --
               --LET ns2ZcrmWsConfirmacionPago.ICaso = v_caso_adai_cadena
               --LET ns2ZcrmWsConfirmacionPago.INss = v_r_ret_solicitud_generico.nss
               --LET ns2ZcrmWsConfirmacionPago.IRfc = v_r_ret_solicitud_generico.rfc
               -- arreglo de retiros
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = v_estatus_fico USING "&&"

               -- si la modalidad de retiro es fondo de ahorro
               IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
                  -- se veriica si tiene DAP
                  SELECT cve_referencia
                  INTO   v_referencia_bancaria
                  FROM   ret_pago_dap
                  WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
                  
                  --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
               --ELSE
                  -- es otra modalidad de retiro y se paga por SPEI
                  --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = ""
               END IF   

               -- para formatear fecha
               --LET v_fecha_pago = arr_respuesta_fico[v_contador].fechaPago
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha      = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha      = arr_respuesta_fico[v_contador].fechaPago
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro  = v_r_ret_solicitud_generico.modalidad_retiro
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago    = arr_respuesta_fico[v_contador].referencia
               --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci = 90 -- NO pagado en ADAI
               --
               --
               --DISPLAY "\n\nDATOS ENVIADOS A ADAI..."
               --DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
               --DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
               --DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
               --DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
               --DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap       
               --DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha     
               --DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro 
               --DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago   
               --DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
               
               
               -- se invoca el WS de confirmacion para ADAI
               --CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
LET v_ws_status = 0 --quitar es para prueba
               -- si no se ejecuto correctamente
               IF ( v_ws_status <> 0 ) THEN
                  --DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
                  --DISPLAY "CODE       : ", wsError.code
                  --DISPLAY "CODENS     : ", wsError.codeNS
                  --DISPLAY "DESRIPTION : ", wsError.description
                  --DISPLAY "ACTION     : ", wsError.action
                  CONTINUE FOR
               END IF
               DISPLAY "WS de confirmacion de pago PROCESA ejecutado correctamente"
               --DISPLAY "Caso ADAI   : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
               --DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
               --DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
               --DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
               --DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
               
               -- si no se actualizo correctamente en ADAI
               IF ( FALSE ) THEN
                  --DISPLAY "No se pudo confirmar en ADAI..."
                  --CASE ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                     --WHEN 1
                        --DISPLAY "01: Favor de llenar campos obligatorios"
   --
                     --WHEN 2
                        --DISPLAY "02: No existe caso en tabla de Devolución de pagos"
   --
                     --WHEN 3
                        --DISPLAY "03: La modalidad de Retiro no coincide con el Número de Caso"
   --
                     --WHEN 4
                        --DISPLAY "04: No se actualizo el estatus a comprobante de Pago"
   --
                     --OTHERWISE
                        --DISPLAY "Código no reconocido: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                  --END CASE                     
                  CONTINUE FOR
               END IF
               DISPLAY "Estatus confirmado en PROCESAR..."
               
               -- estado rechazo
               LET v_estado_solicitud = 214
               LET v_cambio_cuenta = TRUE
               
               -- si fue rechazada, se actualiza la solicitud
               CALL fn_cambia_estado_solicitud(v_estado_solicitud,v_r_ret_solicitud_generico.id_solicitud,v_r_ret_solicitud_generico.modalidad_retiro)
                  RETURNING v_proceso_cod,v_marca
   
               -- se desmarca la cuenta
               CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                    v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                    "safreviv", v_proceso_cod)
            END IF
         END IF
{  
         -- si hubo aceptacion o rechazo
         IF ( v_cambio_cuenta ) THEN
            -- se notifica al derechohabiente el resultado de la consulta de pago
            DECLARE cur_benefpago CURSOR FOR
            SELECT *
            FROM   ret_beneficiario_generico
            WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
            
            FOREACH cur_benefpago INTO v_r_ret_beneficiario_generico.*
            
               -- se construye la notificacion           
               IF ( v_estado_solicitud = 72 ) THEN
                  -- titulo del mensaje
                  LET v_titulo_notificacion = "SOLICITUD DE RETIRO PAGADA"
                  
                  -- cuerpo del mensaje
                  LET v_mensaje_notificacion = "\nESTIMADO(A) ", v_r_ret_beneficiario_generico.nombre,
                                               "\nSU PAGO HA SIDO EFECTUADO"
               ELSE
                  -- titulo
                  LET v_titulo_notificacion = "SOLICITUD DE RETIRO RECHAZADA"
                  
                  -- cuerpo del mensaje
                  LET v_mensaje_notificacion = "\nESTIMADO(A) ", v_r_ret_beneficiario_generico.nombre,
                                               "\nLAMENTAMOS COMUNICARLE QUE SU SOLICITUD DE RETIRO HA SIDO RECHAZADA POR EL BANCO"
                  
               END IF
            
               -- se envia el mensaje
               DISPLAY "ENVIANDO CORREO ELECTRONICO A BENEFICIARIO: ",  v_r_ret_beneficiario_generico.correo
               DISPLAY "ASUNTO: ", v_titulo_notificacion
               DISPLAY "CUERPO: ", v_mensaje_notificacion
               
               -- instruccion de envio
               CALL fn_notificacion_correo_retiro(v_r_ret_beneficiario_generico.correo CLIPPED, NULL,
                                                  v_titulo_notificacion,v_mensaje_notificacion)
   
            END FOREACH
         END IF -- cambio en la cuenta aceptado/rechazado
      }-- se comenta el envio de correo         
      END FOR
   END FOREACH
   -------------------------------------------------------------------------------
   -------------------------------------------------------------------------------
   -- se leen las solicitudes que fueron rechazadas por FICO del caso adai
   DECLARE cur_conspago2 CURSOR FOR
   SELECT *
   FROM   ret_solicitud_generico
   WHERE  ( estado_solicitud = 210 AND cod_rechazo = 64 )-- solicitudes restituidas canceladas por fico
   AND    caso_adai = v_caso_adai_buscado
   
   -- para cada solicitud encontrada
   FOREACH cur_conspago2 INTO v_r_ret_solicitud_generico.*
   
      -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
      SELECT *
      INTO   v_r_ret_respuesta_fico.*
      FROM   ret_respuesta_fico
      WHERE  referencia = v_r_ret_solicitud_generico.id_solicitud
   
      -- si no se encontro su respuesta, se salta el registro
      IF ( v_r_ret_respuesta_fico.referencia IS NULL ) THEN     
         CONTINUE FOREACH
      END IF
   
      -- si ya se pago, se actualiza la solicitud
      -- se transforma el estatus a numerico
      LET v_estatus_fico = v_r_ret_respuesta_fico.bandera
            
      DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico
   
      -- 26 oct 2013. FICO dice que en rechazos no viene la fecha, por tanto
      -- se usara la fecha del dia
      LET v_fecha_pago = TODAY USING "yyyymmdd"
            
       -- se informa a ADAI el RECHAZO DEL PAGO
       --LET v_caso_adai_numerico = v_r_ret_solicitud_generico.caso_adai
       --LET v_caso_adai_cadena   = v_caso_adai_numerico USING "&&&&&&&&&&"
                  --
       --LET ns2ZcrmWsConfirmacionPago.ICaso = v_caso_adai_cadena
       --LET ns2ZcrmWsConfirmacionPago.INss = v_r_ret_solicitud_generico.nss
       --LET ns2ZcrmWsConfirmacionPago.IRfc = v_r_ret_solicitud_generico.rfc
       -- arreglo de retiros
       --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = v_estatus_fico USING "&&"

       -- si la modalidad de retiro es fondo de ahorro
       IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
          -- se veriica si tiene DAP
          SELECT cve_referencia
          INTO   v_referencia_bancaria
          FROM   ret_pago_dap
          WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
          
          --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
       --ELSE
          -- es otra modalidad de retiro y se paga por SPEI
          --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = ""
       END IF   

       -- para formatear fecha
       --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha      = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
       --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro  = v_r_ret_solicitud_generico.modalidad_retiro
       --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago    = v_r_ret_solicitud_generico.id_solicitud
       --LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci = 95 -- NO pagado por FICO en ADAI     
       --
       --DISPLAY "\n\nDATOS ENVIADOS A ADAI..."
       --DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
       --DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
       --DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
       --DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
       --DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap       
       --DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha     
       --DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro 
       --DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago   
       --DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
       
                  
       -- se invoca el WS de confirmacion para ADAI
       --CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
LET v_ws_status = 0 --quitar es para prueba      
       -- si NO se ejecuto correctamente
       IF ( v_ws_status <> 0 ) THEN
          --DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
          --DISPLAY "CODE       : ", wsError.code
          --DISPLAY "CODENS     : ", wsError.codeNS
          --DISPLAY "DESRIPTION : ", wsError.description
          --DISPLAY "ACTION     : ", wsError.action
          CONTINUE FOREACH
       END IF
       DISPLAY "WS de confirmacion de pago PROCESA ejecutado correctamente"
       --DISPLAY "Caso ADAI   : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
       --DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
       --DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
       --DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
       --DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
          
          -- si se actualizo correctamente en ADAI

       IF ( FALSE ) THEN--cambia es para prueba
          --DISPLAY "No se pudo confirmar en ADAI..."
          --CASE ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
             --WHEN 1
                --DISPLAY "01: Favor de llenar campos obligatorios"
   --
             --WHEN 2
                --DISPLAY "02: No existe caso en tabla de Devolución de pagos"
   --
             --WHEN 3
                --DISPLAY "03: La modalidad de Retiro no coincide con el Número de Caso"
   --
             --WHEN 4
                --DISPLAY "04: No se actualizo el estatus a comprobante de Pago"
   --
             --OTHERWISE
                --DISPLAY "Código no reconocido: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
          --END CASE                     
          CONTINUE FOREACH
       END IF
       DISPLAY "Estatus confirmado en PROCESA..."
       
       -- estado rechazo
       LET v_estado_solicitud = 214
       LET v_cambio_cuenta = TRUE

       -- si fue rechazada, se actualiza la solicitud
       CALL fn_cambia_estado_solicitud(v_estado_solicitud,v_r_ret_solicitud_generico.id_solicitud,v_r_ret_solicitud_generico.modalidad_retiro)
         RETURNING v_proceso_cod,v_marca
   
       -- se desmarca la cuenta
       CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                            v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                            "safreviv", v_proceso_cod)
   END FOREACH
   -------------------------------------------------------------------------------
   -------------------------------------------------------------------------------
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "NOTIFICACION PROCESAR")

   -- se finaliza la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING v_bandera

   -- se complementa el mensaje
   LET p_mensaje = "RETIRO GENERICO - NOTIFICACION PROCESAR."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIRO GENERICO - NOTIFICACION PROCESAR"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN

--consulta la tabla de respuesta de fico proveniente del WS
FUNCTION fn_consulta_FICO(p_documento,p_ejercicio,p_sociedad)
DEFINE p_documento    CHAR(10),
       p_ejercicio    CHAR(4),
       p_sociedad     CHAR(4),
       v_sql          STRING,
       v_i_indice     SMALLINT,
       v_i_ban_existe SMALLINT

   -- se inicializa variable 
   LET v_i_indice     = 1
   LET v_i_ban_existe = FALSE 

   -- arma la selección de la tabla de respuesta de fico
   LET v_sql = "\n SELECT documento        ,         ",
               "\n       ejercicio         ,         ",
               "\n       rsp_estatus       ,         ",
               "\n       rsp_importe       ,         ",
               "\n       rsp_ind_retencion ,         ",
               "\n       rsp_referencia    ,         ",
               "\n       rsp_f_pago                  ",
               "\n FROM  ret_ws_consulta_pago_fico    ",
               "\n WHERE documento = '",p_documento,"'",
               "\n AND   ejercicio = '",p_ejercicio,"'",
               "\n AND   sociedad  = '",p_sociedad, "'"

   PREPARE stm_resp_fico  FROM v_sql
   DECLARE cur_resp_fico CURSOR FOR stm_resp_fico

  -- itera el resultado
   FOREACH cur_resp_fico INTO arr_respuesta_fico[v_i_indice].*
      -- incrementa el indice
      LET v_i_indice     = v_i_indice + 1
      LET v_i_ban_existe = TRUE
   END FOREACH

   -- se borra el ultimo registro que se genera por el foreach
   CALL arr_respuesta_fico.deleteElement(arr_respuesta_fico.getLength())

   -- regresa si existió información
   RETURN v_i_ban_existe

END FUNCTION

FUNCTION fn_cambia_estado_solicitud(p_estado_solicitud,p_id_solicitud,p_modalidad_retiro)
DEFINE p_estado_solicitud LIKE ret_solicitud_generico.estado_solicitud -- estado de la solicitud
   ,p_id_solicitud     LIKE ret_voluntaria.id_solicitud
   ,p_modalidad_retiro LIKE ret_solicitud_generico.modalidad_retiro
   ,v_proceso_cod      LIKE cat_proceso.proceso_cod -- codigo de proceso
   ,v_marca            LIKE sfr_marca.marca -- marca del proceso
   ,v_sql              STRING
   -- se actualiza la solicitud
   UPDATE ret_solicitud_generico
   SET    estado_solicitud = p_estado_solicitud -- rechazada por el banco
   WHERE  id_solicitud     = p_id_solicitud

   -- 20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
   -- 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
   CASE p_modalidad_retiro
      WHEN 2 -- FONDO DE AHORRO
         LET v_sql = "\nUPDATE ret_fondo_ahorro_generico"
         LET v_proceso_cod = g_proceso_cod_ret_fondo_ahorro
         LET v_marca = 802
         
      WHEN 3 -- ley 73
         LET v_sql = "\nUPDATE ret_ley73_generico"
         LET v_proceso_cod = g_proceso_cod_ret_ley73_ws
         LET v_marca = 803
   
      WHEN 9 -- amortizaciones excedentes
         LET v_sql = "\nUPDATE ret_amort_excedente"
         LET v_proceso_cod = g_proceso_cod_ret_amort_excedentes
         LET v_marca = 810
   
      WHEN 10 -- aportaciones voluntarias
         LET v_sql = "\nUPDATE ret_voluntaria"
         LET v_proceso_cod = g_proceso_cod_ret_aport_voluntarias
         LET v_marca = 809
   END CASE
   
   -- se complementa la actualizacion
   LET v_sql = v_sql , "\nSET    estado_solicitud = ?",
                       "\nWHERE  id_solicitud     = ?"
   
   -- se ejecuta la actualizacion
   PREPARE sid_rechpagof FROM v_sql
   EXECUTE sid_rechpagof USING p_estado_solicitud, p_id_solicitud

   RETURN v_proceso_cod,v_marca
END FUNCTION