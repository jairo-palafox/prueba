--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP273                                                                 #
#OBJETIVO          =>Programa que notifica las solicitudes de retiro pagadas y canceladas a  #
#                    CRM para que haga el cierre de casos                                   #
##############################################################################################
{Autor               Fecha        Modificación                                              #
Esteban Sánchez Zepeda 30/10/2013   Se elimina la consulta a FICO                           #
                                    y se consulta la tabla donde se almacena                #
                                    la respuesta                                            #                                                                                            #
Ivan Vega              14/nov/2013  Los rechazados por FICO al generar                      #
                                    la notificacion a CRM se hara con                      #
                                    clave 95 para diferenciarlos de los                     #
                                    no pagados por banco con 90                             #
                                    Se notifican las solicitudes por caso                   #
                                    crm, es decir, hasta que todas las                     #
                                    solicitudes de un caso CRM tengan respuesta            #
                                    es cuando se notifican a CRM                           #
Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
                20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
                20150710   Se cambia el llamado al WS cambia de CRM a CRM
                
}
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS "ret_confirma_pago_ae_adai.inc" -- notificacion a CRM
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
       v_conteo                      INTEGER, -- contador de registros
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
       v_solicitud_error             LIKE ret_fondo_ahorro.id_solicitud,
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud
       v_titulo_notificacion         STRING, -- titulo de la notificacion al beneficiario
       v_mensaje_notificacion        STRING, -- mensaje de la notificacion al beneficiario

--       v_r_ret_solicitud_generico    RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud de retiro
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

   DEFINE v_count_medio_entrega      SMALLINT 
   DEFINE v_c_rechazo                CHAR(3)
   DEFINE v_des_rechazo              CHAR(30)
   DEFINE v_resultado                INTEGER
   DEFINE v_codigo                   CHAR(4)

           -- Consulta pago fico
    DEFINE v_h_consulta         LIKE ret_ws_consulta_pago_fico.h_consulta
    DEFINE v_rsp_referencia     LIKE ret_ws_consulta_pago_fico.rsp_referencia
    DEFINE v_rsp_f_pago         LIKE ret_ws_consulta_pago_fico.rsp_f_pago
    DEFINE v_rsp_estatus        LIKE ret_ws_consulta_pago_fico.rsp_estatus   

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP273.log")          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACION CRM")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con CRM para notificación de solicitudes aceptadas y rechazadas"

      -- 16Dic2013. Se verifica si hay datos para finalización de notificación
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico
   WHERE  estado_solicitud IN (71,212) -- pagadas y con cuenta por pagar cancelada
   AND    modalidad_retiro = 9  --  Solo Amortizaciones Excedentes
          OR (estado_solicitud = 210 AND cod_rechazo IN (64,65) AND modalidad_retiro = 9)

   -- si no hay registros para finalización de notificación
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de finalización de notificación"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO AMORTIZACIONES EXCEDENTES\n",
                      "Operación    : FINALIZACIÓN DE NOTIFICACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para la finalización de notificación.\nNo es necesario ejecutar esta etapa.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
                       
   ELSE -- se ejecuta la repuesta de finalización de notificación
       -- Datos de ret_ws_consulta_pago_fico
       LET v_sql = "SELECT FIRST 1 MAX(h_consulta),rsp_referencia,               "||
                   "       rsp_f_pago,rsp_estatus                                "||
                   "FROM   ret_ws_consulta_pago_fico rpf                         "||
                   "WHERE  f_consulta = (SELECT MAX(f_consulta)                  "||
                   "                     FROM   ret_ws_consulta_pago_fico        "||
                   "                     WHERE  id_solicitud = rpf.id_solicitud) "||
                   "  AND  id_solicitud = ?                                      "||
                   "GROUP  BY rsp_referencia,rsp_f_pago,rsp_estatus              "
       PREPARE prp_consulta_pago_fico FROM v_sql

      -- se obtienen los casos crm que ya tienen al menos una solicitud lista para confirmar
      DECLARE cur_casoadai CURSOR FOR
      SELECT DISTINCT caso_adai
      FROM   ret_solicitud_generico
      WHERE  estado_solicitud IN (71, 212)
      AND    modalidad_retiro = 9
      OR     ( estado_solicitud = 210 AND
               cod_rechazo IN (64,65) AND 
               modalidad_retiro = 9)

      -- se leen las solicitudes de estos casos
      FOREACH cur_casoadai INTO v_caso_adai_buscado
         -- se cuenta cuantas solicitudes tiene el caso
         SELECT COUNT(*)
         INTO   v_num_solicitudes_caso
         FROM   ret_solicitud_generico
         WHERE  caso_adai = v_caso_adai_buscado
         
         -- se cuenta cuantas solicitudes ya tienen resultado de pago de ese caso
         SELECT COUNT(*)
         INTO   v_num_solicitudes_con_result
         FROM   ret_solicitud_generico
         WHERE  caso_adai = v_caso_adai_buscado
         AND    ( estado_solicitud IN (71, 212)
                  OR ( estado_solicitud = 210 AND cod_rechazo IN (64,65) )
                )
                  
         -- si el conteo no es igual, entonces no se tiene resultado de todas las solicitudes del caso y por tanto no
         -- se debe notificar
         IF ( v_num_solicitudes_caso <> v_num_solicitudes_con_result ) THEN
            DISPLAY "\n----------------------------------------------------------------------"
            DISPLAY "Caso CRM: ", v_caso_adai_buscado
            DISPLAY "No se puede notificar aun pues todavía tiene solicitudes pendientes..."
            DISPLAY "Num. de solicitudes del caso: ", v_num_solicitudes_caso
            DISPLAY "Num. de solicitudes listas para ser notificadas: ", v_num_solicitudes_con_result
            -- se ignora la solicitud
            CONTINUE FOREACH
         END IF
      
         -- se leen las solicitudes en estatus de enviadas a tesoreria (FICO) del caso adai
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
         WHERE  estado_solicitud IN (71, 212) -- pagadas y con cuenta por pagar cancelada
         AND    caso_adai = v_caso_adai_buscado

         --and id_solicitud in (6235232, 6235230)
         
         -- para cada solicitud encontrada
         FOREACH cur_conspago INTO v_r_ret_solicitud_generico.*

            --- Se valida el medio por el que entró para saber por donde se notifica
            SELECT COUNT(*)
            INTO   v_count_medio_entrega
            FROM   ret_sol_medio_entrega
            WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
            IF v_count_medio_entrega < 1 THEN 
         
               -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
               LET v_sql = "\nSELECT FIRST 1 *",
                           "\nFROM   ret_respuesta_fico",
                           "\nWHERE  referencia = ?",
                           "\nORDER  BY folio"

               PREPARE sid_respfico FROM v_sql
               EXECUTE sid_respfico USING v_r_ret_solicitud_generico.id_solicitud
               INTO    v_r_ret_respuesta_fico.*
         {   
               -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
               SELECT *
               INTO   v_r_ret_respuesta_fico.*
               FROM   ret_respuesta_fico
               WHERE  referencia = v_r_ret_solicitud_generico.id_solicitud
            }
               -- se asume que la cuenta no cambia
               LET v_cambio_cuenta = FALSE      
            
               -- si no se encontro su respuesta, se salta el registro
               IF ( v_r_ret_respuesta_fico.referencia IS NULL ) THEN     
                  CONTINUE FOREACH
               END IF
            
               -- se invoca la consulta de pago a FICO
               CALL fn_consulta_FICO(v_r_ret_solicitud_generico.id_solicitud,
                                     v_r_ret_respuesta_fico.cta_x_pagar,
                                     v_r_ret_respuesta_fico.anho,
                                     v_r_ret_respuesta_fico.sociedad) 
                    RETURNING v_status
               
               -- si el webservice se ejecuto correctamente
               --IF ( v_ws_status = 0 ) THEN
               IF (v_status = TRUE) THEN
                  DISPLAY "\n-----------------------------------------------------------------"
                  DISPLAY "PROCESANDO SOLICITUD: ", v_r_ret_solicitud_generico.id_solicitud
                  DISPLAY "NSS      : ", v_r_ret_solicitud_generico.nss
                  DISPLAY "RFC      : ", v_r_ret_solicitud_generico.rfc
                  DISPLAY "CASO ADAI: ", v_r_ret_solicitud_generico.caso_adai
               
                  FOR v_contador = 1 TO arr_respuesta_fico.getLength()
                     DISPLAY "> Datos de programación de pago: <"
                     DISPLAY "Documento         : ", arr_respuesta_fico[v_contador].documento
                     DISPLAY "Ejercicio         : ", arr_respuesta_fico[v_contador].ejercicio
                     DISPLAY "Estatus           : ", arr_respuesta_fico[v_contador].estatus
                     DISPLAY "Importe           : ", arr_respuesta_fico[v_contador].importe
                     DISPLAY "IndicadorRetencion: ", arr_respuesta_fico[v_contador].indicadorRetencion
                     DISPLAY "Referencia        : ", arr_respuesta_fico[v_contador].referencia
                     DISPLAY "FechaPago         : ", arr_respuesta_fico[v_contador].fechaPago
                  
                  
                     -- si ya se pago, se actualiza la solicitud
                     LET v_cadena = arr_respuesta_fico[v_contador].estatus
                     -- se transforma el estatus a numerico
                     LET v_estatus_fico = v_cadena.trim()
                     
                     --DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico
            
                     -- 26 oct 2013. FICO dice que en rechazos no viene la fecha, por tanto
                     -- se usara la fecha del dia
                     IF ( arr_respuesta_fico[v_contador].fechaPago IS NULL ) THEN
                        LET arr_respuesta_fico[v_contador].fechaPago = TODAY USING "yyyymmdd"
                     END IF

                     -- se verifica el estado y se despliega la leyenda
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
                        
                        -- se informa a ADAI el cambio de estatus
                        LET v_caso_adai_numerico                                     = v_r_ret_solicitud_generico.caso_adai
                        LET v_caso_adai_cadena                                       = v_caso_adai_numerico USING "&&&&&&&&&&"
                        LET ns2ZcrmWsConfirmacionPago.ICaso = v_caso_adai_cadena
                        --LET confirmacionPago.noCaso                          = v_caso_adai_cadena
                        LET ns2ZcrmWsConfirmacionPago.INss    = v_r_ret_solicitud_generico.nss
                        --LET confirmacionPago.nss                           = v_r_ret_solicitud_generico.nss
                        LET ns2ZcrmWsConfirmacionPago.IRfc    = v_r_ret_solicitud_generico.rfc
                        --LET confirmacionPago.IRfc                           = v_r_ret_solicitud_generico.rfc
                        LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = "00"
                        --LET confirmacionPago.ItArrModRet.item[1].CodRechazo = "00" -- arreglo de retiros
                        
                        -- si la modalidad de retiro es fondo de ahorro
                        IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
                           -- se veriica si tiene DAP
                           LET v_referencia_bancaria = ""
                           SELECT cve_referencia
                           INTO   v_referencia_bancaria
                           FROM   ret_pago_dap
                           WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
                           IF v_referencia_bancaria  IS NOT NULL THEN 
                               --LET confirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
                               LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap = v_referencia_bancaria
                           ELSE 
                               LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap = ""
                           END IF
                        ELSE
                           -- es otra modalidad de retiro y se paga por SPEI
                           --LET confirmacionPago.ItArrModRet.item[1].Dap        = ""
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = ""
                        END IF
                        
                        -- para formatear fecha
                        LET v_fecha_pago = arr_respuesta_fico[v_contador].fechaPago
                        --LET confirmacionPago.ItArrModRet.item[1].Fecha      = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                        --LET confirmacionPago.request.confirmacionPago.FPago = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                        LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                        --LET confirmacionPago.ItArrModRet.item[1].Fecha      = arr_respuesta_fico[v_contador].fechaPago4
                        --LET confirmacionPago.ItArrModRet.item[1].ModRetiro  = v_r_ret_solicitud_generico.modalidad_retiro
                        LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro = v_r_ret_solicitud_generico.modalidad_retiro

                        --LET confirmacionPago.ItArrModRet.item[1].RefPago    = arr_respuesta_fico[v_contador].referencia
                        LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago = arr_respuesta_fico[v_contador].referencia
                        --LET confirmacionPago.ItArrModRet.item[1].StatusSaci = 80 -- pagado en ADAI
                        LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci = 80
                        
                        
                        DISPLAY "\n\nDATOS ENVIADOS A CRM..."
                        DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
                        DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
                        DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
                        DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
                        DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap
                        DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha
                        DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro
                        DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago
                        DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
            
                        -- se invoca el WS de confirmacion para CRM
                        CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
                        
                        -- si se ejecuto correctamente
                        IF ( v_ws_status = 0 ) THEN
                           DISPLAY "WS de confirmacion de pago CRM ejecutado correctamente"
                           --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                           --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                           --DISPLAY "Cod. Retorno: ", confirmacionPagoResponse.ECodigoret
                           --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                           --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc
                           
                           -- en el cambio a CRM se pierden campos que ADAI enviaba 
                           --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                           --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                           DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                           DISPLAY "Caso        : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
                           DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
                           DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
                           DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
                           --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                           --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc
            
                           -- si se actualizo correctamente en ADAI
                           --IF ( confirmacionPagoResponse.EIndicador = 1 ) THEN
                           --   DISPLAY "-------------- Estatus confirmado en ADAI --------------"
                           IF  ns2ZcrmWsConfirmacionPagoResponse.ECodigoret = 0 THEN
                              DISPLAY "-------------- Estatus confirmado en CRM --------------"
                              
                              -- se comunica como aceptada/pagada
                              LET v_estado_solicitud = 72
                              LET v_cambio_cuenta    = TRUE
                              
                              UPDATE ret_solicitud_generico
                              SET    estado_solicitud = v_estado_solicitud -- pagada
                              WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                              
                              -- se actualiza la solicitud en la tabla de retiro en turno
                              CASE v_r_ret_solicitud_generico.modalidad_retiro
                              
                                 WHEN 2 -- FONDO DE AHORRO
                                    --20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
                                    LET v_sql = "\nUPDATE ret_fondo_ahorro_generico"
                                    LET v_proceso_cod = g_proceso_cod_ret_fondo_ahorro
                                    LET v_marca = 802
                                    
                                 WHEN 3 -- ley 73
                                    --20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
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
                              
                              --DISPLAY v_sql
                              -- se ejecuta la actualizacion
                              PREPARE sid_actpago FROM v_sql
                              EXECUTE sid_actpago USING v_estado_solicitud, v_r_ret_solicitud_generico.id_solicitud
            
                              -- se desmarca la cuenta
                              CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                                   v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                                   p_usuario_cod, v_proceso_cod)
                           ELSE
                              IF ns2ZcrmWsConfirmacionPagoResponse.ECodigoret = 1 THEN 
                                  DISPLAY "No se pudo confirmar debido a una caida en netweaver"
                              ELSE 
                                  DISPLAY "Hubo un error en la confirmación debido a > ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret, " < "
                              END IF 
                           
                              -- no se pudo confirmar el caso en adai, se despliega el mensaje de error
                              --DISPLAY "No se pudo confirmar en ADAI..."
                              --CASE confirmacionPagoResponse.return.codigo
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
                                    --DISPLAY "Código no reconocido: ", confirmacionPagoResponse.return.codigo
                              --END CASE
                           END IF
                           
                        ELSE
                           DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
                           DISPLAY "CODE        : ", wsError.code
                           DISPLAY "CODENS      : ", wsError.codeNS
                           DISPLAY "DESCRIPTION : ", wsError.description
                           DISPLAY "ACTION      : ", wsError.action
                           DISPLAY "=========================================================\n"
                        END IF
                     ELSE
                        -- ========================================================================
                        -- NOTIFICACION DE PAGOS RECHAZADOS
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
                           LET v_caso_adai_numerico                                     = v_r_ret_solicitud_generico.caso_adai
                           LET v_caso_adai_cadena                                       = v_caso_adai_numerico USING "&&&&&&&&&&"
                           LET ns2ZcrmWsConfirmacionPago.ICaso                    = v_caso_adai_cadena
                           LET ns2ZcrmWsConfirmacionPago.INss                     = v_r_ret_solicitud_generico.nss
                           LET ns2ZcrmWsConfirmacionPago.IRfc                     = v_r_ret_solicitud_generico.rfc
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = v_estatus_fico USING "&&" -- arreglo de retiros

                           -- si la modalidad de retiro es fondo de ahorro
                           IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
                              -- se veriica si tiene DAP
                              LET v_referencia_bancaria = ""
                              SELECT cve_referencia
                              INTO   v_referencia_bancaria
                              FROM   ret_pago_dap
                              WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
                              IF v_referencia_bancaria IS NOT NULL THEN 
                                  --LET confirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
                                  LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = v_referencia_bancaria
                              ELSE
                                  LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = ""
                              END IF 
                           ELSE
                              -- es otra modalidad de retiro y se paga por SPEI
                              --LET confirmacionPago.ItArrModRet.item[1].Dap        = ""
                              LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = ""
                           END IF   

                           -- para formatear fecha
                           LET v_fecha_pago                                             = arr_respuesta_fico[v_contador].fechaPago
                           
                           --LET confirmacionPago.request.confirmacionPago.FPago       = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha       = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro   = v_r_ret_solicitud_generico.modalidad_retiro
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago     = arr_respuesta_fico[v_contador].referencia
                           LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci  = 90 -- NO pagado en ADAI

                           
                           DISPLAY "\n\nDATOS ENVIADOS A ADAI..."
                           DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
                           DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
                           DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
                           DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
                           DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap
                           DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha
                           DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro
                           DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago
                           DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
                           
                           
                           -- se invoca el WS de confirmacion para ADAI
                           CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
                           
                           -- si se ejecuto correctamente
                           IF ( v_ws_status = 0 ) THEN
                              DISPLAY "WS de confirmacion de pago ADAI ejecutado correctamente"
                              --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                              --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                              --DISPLAY "Cod. Retorno: ", confirmacionPagoResponse.ECodigoret
                              --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                              --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc
                              
                              --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                              --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                              DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                              DISPLAY "Caso        : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
                              DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
                              DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
                              DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
                              --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                              --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc

                              -- si se actualizo correctamente en ADAI
                              IF  ns2ZcrmWsConfirmacionPagoResponse.ECodigoret = 0  THEN
                                 DISPLAY "-------------- Estatus confirmado en CRM --------------"
                                 
                                 -- estado rechazo
                                 LET v_estado_solicitud = 214
                                 LET v_cambio_cuenta    = TRUE
                                 
                                 -- si fue rechazada, se actualiza la solicitud
                                 UPDATE ret_solicitud_generico
                                 SET    estado_solicitud = v_estado_solicitud -- rechazada por el banco
                                 WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                                 
                                 CASE v_r_ret_solicitud_generico.modalidad_retiro
                                 
                                    WHEN 2 -- FONDO DE AHORRO
                                       --20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
                                       LET v_sql = "\nUPDATE ret_fondo_ahorro_generico"
                                       LET v_proceso_cod = g_proceso_cod_ret_fondo_ahorro
                                       LET v_marca = 802
                                       
                                    WHEN 3 -- ley 73
                                       --20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
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
                                 PREPARE sid_rechpago FROM v_sql
                                 EXECUTE sid_rechpago USING v_estado_solicitud, v_r_ret_solicitud_generico.id_solicitud
            
                                 -- se desmarca la cuenta
                                 CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                                      v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                                      p_usuario_cod, v_proceso_cod)
            
            
                              ELSE
                                   IF ns2ZcrmWsConfirmacionPagoResponse.ECodigoret = 1 THEN 
                                       DISPLAY "No se pudo confirmar debido a una caida en netweaver"
                                   ELSE 
                                       DISPLAY "Hubo un error en la confirmación debido a > ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret, " < "
                                   END IF 
                                 --DISPLAY "No se pudo confirmar en ADAI..."
                                 --CASE confirmacionPagoResponse.return.codigo
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
                                       --DISPLAY "Código no reconocido: ", confirmacionPagoResponse.return.codigo
                                 --END CASE                     
            
                              END IF
                              
                           ELSE
                              DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
                              DISPLAY "CODE        : ", wsError.code
                              DISPLAY "CODENS      : ", wsError.codeNS
                              DISPLAY "DESCRIPTION : ", wsError.description
                              DISPLAY "ACTION      : ", wsError.action
                              DISPLAY "=========================================================\n"
                           END IF
            
                           
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
               ELSE
                   DISPLAY "NO EXISTE INFORMACIÓN AL CONSULTAR LA RESPUESTA DE FICO"        
               END IF
            ELSE -- Se tramito por devolución automática
               EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                  USING v_r_ret_solicitud_generico.id_solicitud
            
               -- se informa a CRM el cambio de estatus
               IF v_r_ret_solicitud_generico.estado_solicitud = 212 THEN 
                  IF v_r_ret_solicitud_generico.cod_rechazo = 64 THEN
                     LET v_c_rechazo = '030'
                     LET v_des_rechazo = ''
                  END IF
                  IF v_r_ret_solicitud_generico.cod_rechazo = 65 THEN
                     SELECT rsp_desc_estatus
                     INTO   v_des_rechazo
                     FROM   ret_ws_consulta_pago_fico
                     WHERE  id_solicitud = v_id_solicitud
                     AND    rsp_referencia = v_rsp_referencia
                     AND    rsp_f_pago = v_rsp_f_pago
                     AND    rsp_estatus = v_rsp_estatus
                     AND    h_consulta = v_h_consulta
                     IF v_des_rechazo IS NULL THEN 
                        LET v_c_rechazo = '040'
                        LET v_des_rechazo = ''
                     ELSE
                        LET v_c_rechazo = v_des_rechazo[1,3]
                     END IF 
                  END IF
               ELSE 
                  LET v_c_rechazo = 0
                  LET v_des_rechazo = ''
               END IF 
               DISPLAY "===================================================="
               DISPLAY "Se envia informacion para confirmación de pago a CRM, estado 71-212:"
               DISPLAY "NSS              :", v_r_ret_solicitud_generico.nss
               DISPLAY "Caso CRM         :", v_r_ret_solicitud_generico.caso_adai
               DISPLAY "Referencia       :", v_rsp_referencia
               DISPLAY "Estado Solicitud :", v_r_ret_solicitud_generico.estado_solicitud
               DISPLAY "Código de Rechazo:", v_c_rechazo
               CALL fn_confirma_pago_ae(v_r_ret_solicitud_generico.nss, v_r_ret_solicitud_generico.caso_adai, 1 , v_rsp_referencia, v_r_ret_solicitud_generico.estado_solicitud,v_c_rechazo, v_des_rechazo) RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios

               DISPLAY "Respuesta recibida :"
               DISPLAY "v_resultado        :", v_resultado
               DISPLAY "v_codigo           :", v_codigo
               DISPLAY "===================================================="

               
               IF v_resultado = 0 AND v_codigo = "0000" THEN 

                  IF v_r_ret_solicitud_generico.estado_solicitud  = 212 THEN 
                     -- se comunica como rechazada/no pagada
                     LET v_r_ret_solicitud_generico.estado_solicitud = 214
                  ELSE 
                     -- se comunica como aceptada/pagada
                     LET v_r_ret_solicitud_generico.estado_solicitud = 72
                  END IF 
                                
                  UPDATE ret_solicitud_generico
                  SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud -- pagada
                  WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud

                  UPDATE ret_amort_excedente
                  SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud
                  WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                  -- se desmarca la cuenta
                  LET v_marca = 810
                  CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                       v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                       p_usuario_cod, g_proceso_cod)
               ELSE 
                  DISPLAY "ERROR al invocar webservice de confirmacion de pago CRM"
                  DISPLAY "=========================================================\n"
               END IF

            END IF 
         
         END FOREACH
         -------------------------------------------------------------------------------
         -------------------------------------------------------------------------------
         -- se leen las solicitudes que fueron rechazadas por FICO del caso adai
         DECLARE cur_conspago2 CURSOR FOR
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
         WHERE  ( estado_solicitud = 210 AND cod_rechazo IN (64,65) )-- solicitudes restituidas canceladas por fico
         AND    caso_adai = v_caso_adai_buscado
         
         -- para cada solicitud encontrada
         FOREACH cur_conspago2 INTO v_r_ret_solicitud_generico.*
            -- Se verifica si entró por Devolución Automática
            SELECT COUNT(*)
            INTO   v_count_medio_entrega
            FROM   ret_sol_medio_entrega
            WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
            IF v_count_medio_entrega <= 0 THEN 
               LET v_sql = "\nSELECT FIRST 1 *",
                           "\nFROM   ret_respuesta_fico",
                           "\nWHERE  referencia = ?",
                           "\nORDER  BY folio"

               PREPARE sid_respfico2 FROM v_sql
               EXECUTE sid_respfico2 USING v_r_ret_solicitud_generico.id_solicitud
               INTO    v_r_ret_respuesta_fico.*
               -- se obtienen los datos de las solicitudes que sean encontradas en la respuesta de fico
   {
               SELECT *
               INTO   v_r_ret_respuesta_fico.*
               FROM   ret_respuesta_fico
               WHERE  referencia = v_r_ret_solicitud_generico.id_solicitud
    }        
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
                LET v_caso_adai_numerico = v_r_ret_solicitud_generico.caso_adai
                LET v_caso_adai_cadena   = v_caso_adai_numerico USING "&&&&&&&&&&"
                           

                LET ns2ZcrmWsConfirmacionPago.ICaso                    = v_caso_adai_cadena
                LET ns2ZcrmWsConfirmacionPago.INss                       = v_r_ret_solicitud_generico.nss
                LET ns2ZcrmWsConfirmacionPago.IRfc                     = v_r_ret_solicitud_generico.rfc
                -- arreglo de retiros
                LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo = v_estatus_fico USING "&&"

                
                -- si la modalidad de retiro es fondo de ahorro
                IF ( v_r_ret_solicitud_generico.modalidad_retiro = 2 ) THEN
                   -- se veriica si tiene DAP
                   LET v_referencia_bancaria = ""
                   SELECT cve_referencia
                   INTO   v_referencia_bancaria
                   FROM   ret_pago_dap
                   WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
                   IF v_referencia_bancaria IS NOT NULL THEN 
                       --LET confirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
                       LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap        = v_referencia_bancaria
                   ELSE
                       LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = ""
                   END IF 
                ELSE
                   -- es otra modalidad de retiro y se paga por SPEI
                   --LET confirmacionPago.ItArrModRet.item[1].Dap        = ""
                   LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap   = ""
                END IF   

                -- para formatear fecha

                --LET confirmacionPago.request.confirmacionPago.FPago       = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha       = v_fecha_pago[1,4], "-", v_fecha_pago[5,6], "-", v_fecha_pago[7,8]
                LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro     = v_r_ret_solicitud_generico.modalidad_retiro
                LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago       = v_r_ret_solicitud_generico.id_solicitud
                LET ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci = 95 -- NO pagado por FICO en ADAI     
                
                DISPLAY "\n\nDATOS ENVIADOS A ADAI..."
                
                DISPLAY "ICaso     : ", ns2ZcrmWsConfirmacionPago.ICaso
                DISPLAY "INss      : ", ns2ZcrmWsConfirmacionPago.INss
                DISPLAY "IRfc      : ", ns2ZcrmWsConfirmacionPago.IRfc
                DISPLAY "CodRechazo: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].CodRechazo
                DISPLAY "Dap       : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Dap     
                DISPLAY "Fecha     : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].Fecha     
                DISPLAY "ModRetiro : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].ModRetiro
                DISPLAY "RefPago   : ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].RefPago
                DISPLAY "StatusSaci: ", ns2ZcrmWsConfirmacionPago.ItArrModRet.item[1].StatusSaci
                           
                -- se invoca el WS de confirmacion para ADAI
                CALL ZcrmWsConfirmacionPago_g() RETURNING v_ws_status
            
                -- si se ejecuto correctamente
                IF ( v_ws_status = 0 ) THEN
                   DISPLAY "WS de confirmacion de pago ADAI ejecutado correctamente"
                   --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                   --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                   --DISPLAY "Cod. Retorno: ", confirmacionPagoResponse.ECodigoret
                   --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                   --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc
                   
                   --DISPLAY "Caso ADAI   : ", confirmacionPagoResponse.ECaso
                   --DISPLAY "Indicador   : ", confirmacionPagoResponse.EIndicador
                   DISPLAY "Cod. Retorno: ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret
                   DISPLAY "Caso        : ", ns2ZcrmWsConfirmacionPagoResponse.ECaso
                   DISPLAY "Indicador   : ", ns2ZcrmWsConfirmacionPagoResponse.EIndicador
                   DISPLAY "NSS         : ", ns2ZcrmWsConfirmacionPagoResponse.ENss
                   DISPLAY "RFC         : ", ns2ZcrmWsConfirmacionPagoResponse.ERfc
                   --DISPLAY "NSS         : ", confirmacionPagoResponse.ENss
                   --DISPLAY "RFC         : ", confirmacionPagoResponse.ERfc

                   -- si se actualizo correctamente en ADAI
      
                   IF ns2ZcrmWsConfirmacionPagoResponse.ECodigoret  = 0 THEN
                      DISPLAY "Estatus confirmado en ADAI..."
                      
                      -- estado rechazo
                      LET v_estado_solicitud = 214
                      LET v_cambio_cuenta = TRUE
                      
                      -- si fue rechazada, se actualiza la solicitud
                      UPDATE ret_solicitud_generico
                      SET    estado_solicitud = v_estado_solicitud -- rechazada por el banco
                      WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                      
                      CASE v_r_ret_solicitud_generico.modalidad_retiro
                      
                         WHEN 2 -- FONDO DE AHORRO
                            --20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
                            LET v_sql = "\nUPDATE ret_fondo_ahorro_generico"
                            LET v_proceso_cod = g_proceso_cod_ret_fondo_ahorro
                            LET v_marca = 802
                            
                         WHEN 3 -- ley 73
                            --20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
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
                      PREPARE sid_rechpago2 FROM v_sql
                      EXECUTE sid_rechpago2 USING v_estado_solicitud, v_r_ret_solicitud_generico.id_solicitud
            
                      -- se desmarca la cuenta
                      CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                           v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                           p_usuario_cod, v_proceso_cod)
            
                   ELSE
                       IF ns2ZcrmWsConfirmacionPagoResponse.ECodigoret = 1 THEN 
                           DISPLAY "No se pudo confirmar debido a una caida en netweaver"
                       ELSE 
                           DISPLAY "Hubo un error en la confirmación debido a > ", ns2ZcrmWsConfirmacionPagoResponse.ECodigoret, " < "
                       END IF 

                       --CASE confirmacionPagoResponse.return.codigo
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
                            --DISPLAY "Código no reconocido: ", confirmacionPagoResponse.return.codigo, "< ", confirmacionPagoResponse.return.descripcion 
                      --END CASE                     
            
                   END IF
                   
                ELSE
                   DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
                   DISPLAY "CODE        : ", wsError.code
                   DISPLAY "CODENS      : ", wsError.codeNS
                   DISPLAY "DESCRIPTION : ", wsError.description
                   DISPLAY "ACTION      : ", wsError.action
                   DISPLAY "=========================================================\n"
                END IF
            
                {       
                -- si hubo aceptacion o rechazo
                IF ( v_cambio_cuenta ) THEN
                   -- se notifica al derechohabiente el resultado de la consulta de pago
                   DECLARE cur_benefpago2 CURSOR FOR
                   SELECT *
                   FROM   ret_beneficiario_generico
                   WHERE  id_solicitud = v_r_ret_solicitud_generico.id_solicitud
                   
                   FOREACH cur_benefpago2 INTO v_r_ret_beneficiario_generico.*
                   
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
                   
                      -- se notifica a ADAI el resultado
                      DISPLAY "NOTIFICACION A ADAI AQUI"
                   
                   END FOREACH
                END IF -- cambio en la cuenta aceptado/rechazado
                }
            ELSE -- Se tramito por devolución automática
               EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                  USING v_r_ret_solicitud_generico.id_solicitud
            
               -- se informa a CRM el cambio de estatus
               IF v_r_ret_solicitud_generico.estado_solicitud = 210 THEN 
                  IF v_r_ret_solicitud_generico.cod_rechazo = 64 THEN
                     LET v_c_rechazo = '030'
                     LET v_des_rechazo = ''
                  END IF
                  IF v_r_ret_solicitud_generico.cod_rechazo = 65 THEN
                     SELECT rsp_desc_estatus
                     INTO   v_des_rechazo
                     FROM   ret_ws_consulta_pago_fico
                     WHERE  id_solicitud = v_id_solicitud
                     AND    rsp_referencia = v_rsp_referencia
                     AND    rsp_f_pago = v_rsp_f_pago
                     AND    rsp_estatus = v_rsp_estatus
                     AND    h_consulta = v_h_consulta
                     IF v_des_rechazo IS NULL THEN 
                        LET v_c_rechazo = '040'
                        LET v_des_rechazo = ''
                     ELSE
                        LET v_c_rechazo = v_des_rechazo[1,3]
                     END IF 
                  END IF
               ELSE 
                  LET v_c_rechazo = 0
                  LET v_des_rechazo = ''
               END IF 
               DISPLAY "===================================================="
               DISPLAY "Se envia informacion para confirmación de pago a CRM, estado 210:"
               DISPLAY "NSS              :", v_r_ret_solicitud_generico.nss
               DISPLAY "Caso CRM         :", v_r_ret_solicitud_generico.caso_adai
               DISPLAY "Referencia       :", v_rsp_referencia
               DISPLAY "Estado Solicitud :", v_r_ret_solicitud_generico.estado_solicitud
               DISPLAY "Código de Rechazo:", v_c_rechazo
               
               CALL fn_confirma_pago_ae(v_r_ret_solicitud_generico.nss, v_r_ret_solicitud_generico.caso_adai, 1 , v_rsp_referencia, v_r_ret_solicitud_generico.estado_solicitud,v_c_rechazo, v_des_rechazo) RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios

               DISPLAY "Respuesta recibida :"
               DISPLAY "v_resultado        :", v_resultado
               DISPLAY "v_codigo           :", v_codigo
               DISPLAY "===================================================="

               IF v_resultado = 0 AND v_codigo = "0000" THEN 

                  IF v_r_ret_solicitud_generico.estado_solicitud  = 210 THEN 
                     -- se comunica como rechazada/no pagada
                     LET v_r_ret_solicitud_generico.estado_solicitud = 214
                  ELSE 
                     -- se comunica como aceptada/pagada
                     LET v_r_ret_solicitud_generico.estado_solicitud = 72
                  END IF 
                                
                  UPDATE ret_solicitud_generico
                  SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud -- pagada
                  WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud

                  UPDATE ret_amort_excedente
                  SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud
                  WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
                  -- se desmarca la cuenta
                  LET v_marca = 810
                  CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                       v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                       p_usuario_cod, g_proceso_cod)
               ELSE 
                  DISPLAY "ERROR al invocar webservice de confirmacion de pago CRM"
                  DISPLAY "=========================================================\n"
               END IF

            END IF 
   
         END FOREACH
      END FOREACH -- CASOS ADAI
      -------------------------------------------------------------------------------
      -------------------------------------------------------------------------------

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "RETIRO GENERICO - NOTIFICACION ADAI."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - RETIRO GENERICO - NOTIFICACION ADAI"
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "NOTIFICACION ADAI")

END MAIN

--consulta la tabla de respuesta de fico proveniente del WS
FUNCTION fn_consulta_FICO(p_id_solicitud,p_documento,p_ejercicio,p_sociedad)
DEFINE p_id_solicitud LIKE ret_solicitud_generico.id_solicitud,
       p_documento    CHAR(10),
       p_ejercicio    CHAR(4),
       p_sociedad     CHAR(4),
       v_sql          STRING,
       v_i_indice     SMALLINT,
       v_i_ban_existe SMALLINT

   -- se inicializa variable 
   LET v_i_indice     = 1
   LET v_i_ban_existe = FALSE 
   CALL arr_respuesta_fico.clear()

   -- arma la selección de la tabla de respuesta de fico
   LET v_sql = "\n SELECT documento        ,           ",
               "\n       ejercicio         ,           ",
               "\n       rsp_estatus       ,           ",
               "\n       rsp_importe       ,           ",
               "\n       rsp_ind_retencion ,           ",
               "\n       rsp_referencia    ,           ",
               "\n       rsp_f_pago                    ",
               "\n FROM  ret_ws_consulta_pago_fico     ",
               "\n WHERE id_solicitud = ", p_id_solicitud,
               "\n AND   documento    = '",p_documento,"'",
               "\n AND   ejercicio    = '",p_ejercicio,"'",
               "\n AND   sociedad     = '",p_sociedad, "'"

               
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