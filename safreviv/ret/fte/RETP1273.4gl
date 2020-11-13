--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP1273                                                                #
#OBJETIVO          =>Programa que notifica las solicitudes de retiro pagadas y canceladas a  #
#                    CRM para que haga el cierre de casos                                    #
##############################################################################################
{Autor               Fecha        Modificación                                               #
#Fecha de modificacion =>   Modificación                                                     #
#                      =>   20-Oct-2020                                                      #
#                      =>   Jairo Giovanny Palafox                                           #
#                      =>  se adapta notificacion para agregar solicitudes                   #
#                          de CRM que tuvieron un rechazo bancario y                         #
#                          se deben desmarcaen procesar                                      #               
##############################################################################################
}
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
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
       v_id_solicitud                LIKE ret_solicitud_generico.id_solicitud,
       v_nss                         LIKE ret_solicitud_generico.nss,
       v_id_derechohabiente          LIKE ret_solicitud_generico.id_derechohabiente,
       v_clave_acreedor              VARCHAR(10),
       v_ws_status                   SMALLINT, -- estatus de ejecucion de un webservice
       v_status                      SMALLINT, -- estatus consulta FICO
       v_folio_restitucion           LIKE glo_folio.folio,
       v_i_resultado                 INTEGER,                             -- resultado del proceso
       v_resultado                   INTEGER ,
       v_codigo                      CHAR(4),
       v_error_isam                  INTEGER,
       v_mensaje                     VARCHAR(255),
       v_solicitud_error             LIKE ret_fondo_ahorro.id_solicitud,
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud
       v_titulo_notificacion         STRING, -- titulo de la notificacion al beneficiario
       v_mensaje_notificacion        STRING, -- mensaje de la notificacion al beneficiario
       v_estado                      SMALLINT, -- Control de solicitudes de PROCESAR
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
       v_caso_adai_numerico          DECIMAL(10,0), -- caso CRM en numero
       v_caso_adai_cadena            STRING, -- caso CRM en cadena
       v_proceso_cod                 LIKE cat_proceso.proceso_cod, -- codigo de proceso
       v_marca                       LIKE sfr_marca.marca, -- marca del proceso
       v_caso_adai_buscado           LIKE ret_solicitud_generico.caso_adai,
       v_num_solicitudes_caso        SMALLINT, -- numero de solicitudes de un caso CRM
       v_num_solicitudes_con_result  SMALLINT, -- numero de solicitudes que ya tienen resultado de pago/cancelacion
       v_referencia_bancaria         LIKE ret_pago_dap.cve_referencia, -- referencia bancaria de pagos DAP
       v_cod_rechazo                 SMALLINT,
       v_c_rechazo                   CHAR(3),
       v_des_rechazo                 CHAR(30)
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP1273.log")          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACION CRM")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con CRM para notificación de solicitudes aceptadas y rechazadas"

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

   
      -- 16Dic2013. Se verifica si hay datos para finalización de notificación
--   SELECT COUNT(*)
--   INTO   v_conteo
--   FROM   ret_solicitud_generico
--   WHERE  modalidad_retiro = 3
--   AND    (caso_adai IS NOT NULL AND caso_adai <> 0)
--   AND    ((estado_solicitud IN (71) -- pagadas 
--          OR (estado_solicitud = 210 AND cod_rechazo IN (64,65))))

   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico a, 
          ret_beneficiario_generico b
   WHERE  a.id_solicitud = b.id_solicitud 
   AND    a.modalidad_retiro = 3                                    
   -- AND    b.tpo_beneficiario = 1
   AND    b.tpo_beneficiario IN (1,2)                                --Mod JCA 22-07-2020
   AND    (a.caso_adai IS NOT NULL AND a.caso_adai <> 0)
   AND    ((a.estado_solicitud IN (71) -- pagadas 
     OR (a.estado_solicitud = 210 AND a.cod_rechazo IN (64,65,66)))) --Mod JCA 22-07-2020
          
   -- si no hay registros para finalización de notificación
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de finalización de notificación"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO LEY 73\n",
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
      LET v_marca = 803

      -- se obtienen los casos crm que ya tienen al menos una solicitud lista para confirmar
      DECLARE cur_casoadai CURSOR FOR
--      SELECT DISTINCT caso_adai, id_solicitud, nss, id_derechohabiente, estado_solicitud, cod_rechazo
--      FROM   ret_solicitud_generico 
--      WHERE  modalidad_retiro = 3
--      AND    (caso_adai IS NOT NULL AND caso_adai <> 0)
--      AND    (estado_solicitud IN (71)
--      OR     ( estado_solicitud = 210 AND
--               cod_rechazo IN (64,65) ))

      SELECT DISTINCT a.caso_adai, a.id_solicitud, a.nss, a.id_derechohabiente, a.estado_solicitud, a.cod_rechazo
      FROM   ret_solicitud_generico a,
             ret_beneficiario_generico b
      WHERE  a.id_solicitud = b.id_solicitud
      -- AND    b.tpo_beneficiario = 1
      AND    b.tpo_beneficiario IN (1,2)                                --Mod JCA 22-07-2020
      AND    a.modalidad_retiro = 3
      AND    (a.caso_adai IS NOT NULL AND a.caso_adai <> 0)
      AND    (a.estado_solicitud IN (71)
      OR     ( a.estado_solicitud = 210 AND
               a.cod_rechazo IN (64,65,66) ) )  --mod 22-7-2020 JCA
               
      -- se leen las solicitudes de estos casos
      FOREACH cur_casoadai INTO v_caso_adai_buscado, v_id_solicitud, v_nss, v_id_derechohabiente, v_estado_solicitud, v_cod_rechazo
         -- se busca la respuesta de 
         -- Consulta pago fico
         EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                        USING v_id_solicitud
            
         -- se informa a CRM el cambio de estatus
         IF v_estado_solicitud = 210 THEN 
            IF v_cod_rechazo = 64 THEN
               LET v_c_rechazo = '030'
               LET v_des_rechazo = ''
            END IF
            IF v_cod_rechazo = 65 THEN                     
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

            IF v_cod_rechazo = 66 THEN                     --22/07/2020 SE AGREGA 66
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
         DISPLAY "-------------------"
         DISPLAY "-------------------"
         DISPLAY   "v_nss",              v_nss                     
         DISPLAY   "v_caso_adai_buscado",v_caso_adai_buscado       
         DISPLAY   "v_id_solicitud",     v_id_solicitud            
         DISPLAY   "v_rsp_referencia",   v_rsp_referencia          
         DISPLAY   "v_estado_solicitud", v_estado_solicitud        
         DISPLAY   "v_c_rechazo",        v_c_rechazo               
         DISPLAY   "v_des_rechazo",      v_des_rechazo  
                 
         CALL fn_confirma_pago_crm(v_nss, v_caso_adai_buscado, v_id_solicitud , v_rsp_referencia, v_estado_solicitud,v_c_rechazo, v_des_rechazo) RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios
         --LET v_resultado = 0 
         --LET v_codigo    = '0000'
         IF v_resultado = 0 AND v_codigo = "0000" THEN 
            --DISPLAY "v_estado_solicitud",v_estado_solicitud
            IF v_estado_solicitud  = 210 THEN 
               -- se comunica como rechazada/no pagada
               LET v_estado_solicitud = 214
               -- se crean los datos para el envio de solicitudes a procesar
               CALL fn_crea_notificacion_procesar(v_id_solicitud,v_id_derechohabiente,v_nss,v_caso_adai_buscado) RETURNING v_estado

               IF v_estado = 0 THEN
                  DISPLAY "RESULTADO COMUNICACION PROCESAR SATISFACTORIO SOLICITUD: ",v_id_solicitud, "DH:",v_id_derechohabiente
               ELSE
                  DISPLAY "RESULTADO COMUNICACION PROCESAR NO REALIZADO SOLICITUD: ",v_id_solicitud, "DH:",v_id_derechohabiente
               END IF   
            ELSE 
               -- se comunica como aceptada/pagada
               LET v_estado_solicitud = 72
            END IF 
                          
            UPDATE ret_solicitud_generico
            SET    estado_solicitud = v_estado_solicitud -- pagada
            WHERE  id_solicitud     = v_id_solicitud

            UPDATE ret_ley73_generico
            SET    estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud     = v_id_solicitud
            -- se desmarca la cuenta
            CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, v_marca,
                                                 v_id_solicitud, v_marca,
                                                 p_usuario_cod, g_proceso_cod)
         ELSE 
            DISPLAY "ERROR al invocar webservice de confirmacion de pago CRM"
            DISPLAY "=========================================================\n"
         END IF
      END FOREACH
         -------------------------------------------------------------------------------
         -------------------------------------------------------------------------------
      -------------------------------------------------------------------------------
      -------------------------------------------------------------------------------

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

FUNCTION fn_crea_notificacion_procesar(v_id_solicitud,v_id_derechohabiente,v_nss,v_caso_adai_buscado)
  --variables de entrada
  DEFINE v_id_solicitud                LIKE ret_solicitud_generico.id_solicitud,
         v_nss                         LIKE ret_solicitud_generico.nss,
         v_id_derechohabiente          LIKE ret_solicitud_generico.id_derechohabiente,
         v_caso_adai_buscado           LIKE ret_solicitud_generico.caso_adai
  -- Variables de control
  DEFINE
         v_solicitudes_procesadas   INTEGER,
         v_estado                   SMALLINT,
         reg_marca_procesar_maestra RECORD
         id_solicitud            DECIMAL(9,0),
         nss                     CHAR(11),
         estado_indicador        SMALLINT,
         origen                  SMALLINT
      END RECORD,
      v_diagnostico              SMALLINT,
      v_estatus                  SMALLINT,
      v_aivs_viv92               DECIMAL(24,6),
      v_pesos_viv92              DECIMAL(22,2),
      v_aivs_viv97               DECIMAL(24,6),
      v_pesos_viv97              DECIMAL(22,2),
      v_cod_rechazo              SMALLINT

   -- Variables auxiliares
   DEFINE v_modalidad         SMALLINT
  
    -- para cada una de las solicitudes notificadas se manda desmarcar en procesar
    
    -- 60 Desmarca en procesar
    -- 40 marca en procesar
    LET v_modalidad = 60

    --Mando llamar funcionalidad de Desmarca a Procesar
    CALL fn_consulta_saldo_vivienda_afore(v_nss, v_modalidad)
         RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo

    DISPLAY "SALDO RECUPERADO >>: "
    DISPLAY "AIVS92: ", v_aivs_viv92
    DISPLAY "PESOS92: ", v_pesos_viv92
    DISPLAY "AIVS97: ", v_aivs_viv97
    DISPLAY "PESOS97: ", v_pesos_viv97
    DISPLAY "CÓDIGO DE RECHAZO: ", v_cod_rechazo
      
    IF (v_diagnostico = 127) THEN
       --Existio un error desde Procesar
       CALL fn_guarda_consulta_ws_vent_afore(v_nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_id_solicitud, v_caso_adai_buscado, 2) -- Se deberá reenviar la solicitud de marca 
    ELSE
       CALL fn_guarda_consulta_ws_vent_afore(v_nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                             v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', v_id_solicitud, v_caso_adai_buscado, 2) -- La solicitud no se pudo marcar porque esta marcada en otro proceso
    END IF

    LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1
    DISPLAY "SOLICITUDES NOTIFICADAS:"
    DISPLAY "ID DH: >> ",v_id_derechohabiente
    DISPLAY "ID Solicitud: >> ",v_id_solicitud
    DISPLAY "NSS: >> ",v_nss
    DISPLAY "Caso CRM: >> ",v_caso_adai_buscado

    DISPLAY "Se mandaron desmarcar a Procesar: ", v_solicitudes_procesadas, " solicitudes correctamente."

   RETURN v_estado

END FUNCTION

