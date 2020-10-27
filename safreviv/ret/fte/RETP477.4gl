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

   DEFINE v_id_solicitud_masivo       DECIMAL(9,0)
   DEFINE v_id_derechohabiente_masivo DECIMAL(9,0)
   DEFINE v_estado_solicitud_masivo   SMALLINT 

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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP477.log")          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACION CRM")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con CRM para notificación de solicitudes aceptadas y rechazadas"

      -- 16Dic2013. Se verifica si hay datos para finalización de notificación
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico
   WHERE  estado_solicitud IN (71,212) -- pagadas y con cuenta por pagar cancelada
   AND    modalidad_retiro = 2         --  Solo Fondo de Ahorro
          OR (estado_solicitud = 210 AND cod_rechazo = 64 AND modalidad_retiro = 2)

   -- si no hay registros para finalización de notificación
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de finalización de notificación"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO FONDO DE AHORRO\n",
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
      SELECT DISTINCT a.caso_adai
      FROM   ret_solicitud_generico a,
             ret_sol_medio_entrega b
      WHERE  a.id_solicitud = b.id_solicitud
      AND    b.medio_entrega <> 7
      AND    (a.estado_solicitud IN (71, 212)
      AND     a.modalidad_retiro = 2
      OR     (a.estado_solicitud = 210 AND
               a.cod_rechazo = 64 AND 
               a.modalidad_retiro = 2))

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
                  OR ( estado_solicitud = 210 AND cod_rechazo = 64 )
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
            --CALL fn_confirma_pago_crm(v_r_ret_solicitud_generico.nss, v_r_ret_solicitud_generico.caso_adai, 1 , v_rsp_referencia, v_r_ret_solicitud_generico.estado_solicitud,v_c_rechazo, v_des_rechazo) RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios

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

               UPDATE ret_fondo_ahorro_generico
               SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud
               WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
               -- se desmarca la cuenta
               LET v_marca = 802
               CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                    v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                    p_usuario_cod, g_proceso_cod)
            ELSE 
               DISPLAY "ERROR al invocar webservice de confirmacion de pago CRM"
               DISPLAY "=========================================================\n"
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
         WHERE  ( estado_solicitud = 210 AND cod_rechazo = 64 )-- solicitudes restituidas canceladas por fico
         AND    caso_adai = v_caso_adai_buscado
         
         -- para cada solicitud encontrada
         FOREACH cur_conspago2 INTO v_r_ret_solicitud_generico.*
            -- Se verifica si entró por Devolución Automática
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
                  AND    rsp_referecnia = v_rsp_referencia
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

               UPDATE ret_fondo_ahorro_generico
               SET    estado_solicitud = v_r_ret_solicitud_generico.estado_solicitud
               WHERE  id_solicitud     = v_r_ret_solicitud_generico.id_solicitud
               -- se desmarca la cuenta
               LET v_marca = 802
               CALL fn_ret_generico_desmarca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente, v_marca,
                                                    v_r_ret_solicitud_generico.id_solicitud, v_marca,
                                                    p_usuario_cod, g_proceso_cod)
            ELSE 
               DISPLAY "ERROR al invocar webservice de confirmacion de pago CRM"
               DISPLAY "=========================================================\n"
            END IF
   
         END FOREACH
      END FOREACH -- CASOS ADAI
      -------------------------------------------------------------------------------
      -------------------------------------------------------------------------------

      -- se obtienen los casos que se deben desmarcar del masivo
      -- se leen las solicitudes en estatus de enviadas a tesoreria (FICO) del caso adai
      DISPLAY "ACTUALIZANDO LAS SOLICITUDES DEL MASIVO "
      DECLARE cur_conspago_masivo CURSOR FOR
      SELECT 
          a.id_solicitud,
          a.id_derechohabiente,
          a.estado_solicitud
      FROM   ret_solicitud_generico a,
             ret_sol_medio_entrega b
      WHERE  a.id_solicitud = b.id_solicitud
      AND    b.medio_entrega = 7
      AND    a.modalidad_retiro = 2
      AND    (a.estado_solicitud IN (71, 212)
      OR     ( estado_solicitud = 210 AND cod_rechazo = 64 ))
       -- para cada solicitud encontrada
      FOREACH cur_conspago_masivo INTO v_id_solicitud_masivo, v_id_derechohabiente_masivo, v_estado_solicitud_masivo
         IF v_estado_solicitud_masivo = 212 OR 
            v_estado_solicitud_masivo = 210 THEN 
            LET v_estado_solicitud_masivo = 214
         ELSE 
            -- se comunica como aceptada/pagada
            LET v_estado_solicitud_masivo = 72
         END IF 
                             
         UPDATE ret_solicitud_generico
         SET    estado_solicitud = v_estado_solicitud_masivo -- pagada
         WHERE  id_solicitud     = v_id_solicitud_masivo

         UPDATE ret_fondo_ahorro_generico
         SET    estado_solicitud = v_estado_solicitud_masivo
         WHERE  id_solicitud     = v_id_solicitud_masivo
         -- se desmarca la cuenta
         LET v_marca = 802
         CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente_masivo, v_marca,
                                                    v_id_solicitud_masivo, v_marca,
                                                    p_usuario_cod, g_proceso_cod)

      END FOREACH

      DISPLAY " SE ACTUALIZARON SOLICITUDES DEL MASIVO "
      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "RETIRO GENERICO - NOTIFICACION CRM."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - RETIRO GENERICO - NOTIFICACION CRM"
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "NOTIFICACION CRM")

END MAIN
