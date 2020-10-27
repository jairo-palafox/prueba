--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP474                                                                 #
#OBJETIVO          =>Programa que notifica las solicitudes de retiro pagadas y canceladas a  #
#                    Juridico de los beneficiario para que haga el cierre de casos           #
##############################################################################################
{Autor               Fecha        Modificación                                              #

}
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS "ret_notifica_beneficiarios.inc"
GLOBALS "ret_confirma_pago_crm.inc"

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
       v_codigo                      CHAR(2),
       v_error_isam                  INTEGER,
       v_mensaje                     VARCHAR(255),
       v_solicitud_error             LIKE ret_fondo_ahorro.id_solicitud,
       v_estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud, -- estado de la solicitud
       v_titulo_notificacion         STRING, -- titulo de la notificacion al beneficiario
       v_mensaje_notificacion        STRING, -- mensaje de la notificacion al beneficiario
       v_importe_confirma            DECIMAL(14,2),

--       v_r_ret_solicitud_generico    RECORD LIKE ret_solicitud_generico.*, -- registro de solicitud de retiro
   v_r_ret_solicitud_generico RECORD
       id_solicitud           DECIMAL(9,0),
       id_derechohabiente     DECIMAL(9,0),
       nss                    CHAR(11),
       rfc                    CHAR(13),
       modalidad_retiro       SMALLINT,
       folio                  DECIMAL(9,0),
       caso_adai              CHAR(10),
       id_archivo_envio       DECIMAL(9,0),
       id_archivo_respuesta   DECIMAL(9,0),
       folio_restitucion      DECIMAL(9,0),
       id_archivo_cancela_cxp DECIMAL(9,0),
       id_archivo_resp_cxp    DECIMAL(9,0),
       f_solicitud            DATE,
       h_solicitud            DATETIME HOUR TO SECOND,
       estado_solicitud       SMALLINT,
       cod_rechazo            SMALLINT
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
    DEFINE v_h_consulta             LIKE ret_ws_consulta_pago_fico.h_consulta
    DEFINE v_rsp_referencia         LIKE ret_ws_consulta_pago_fico.rsp_referencia
    DEFINE v_rsp_f_pago             LIKE ret_ws_consulta_pago_fico.rsp_f_pago
    DEFINE v_rsp_estatus            LIKE ret_ws_consulta_pago_fico.rsp_estatus
    DEFINE v_indice                 INTEGER 
    DEFINE v_regs_por_caso          INTEGER
    DEFINE v_reg_por_caso_pagados   INTEGER 
    DEFINE v_reg_por_caso_rechazado INTEGER

DEFINE v_arr_beneficiario DYNAMIC ARRAY OF RECORD 
         idBeneficiario      CHAR(1),
         nombreBeneficiario  CHAR(40),
         paternoBeneficiario CHAR(40),
         maternoBeneficiario CHAR(40),
         porcentaje          SMALLINT,
         referenciaPago      CHAR(10),
         marcaPago           CHAR(10),
         nota                CHAR(50),
         sol_conesc          CHAR(12),
         estado_solicitud    SMALLINT,
         cod_rechazo         SMALLINT
      END RECORD                                  

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP474.log")          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACION JURIDICO")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con JURIDICO para notificación de solicitudes de beneficiario aceptadas y rechazadas"

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

   LET v_sql = " SELECT a.consec_beneficiario, b.nombre, b.ap_paterno,       ",
               "        b.ap_materno, b.porcentaje, ' ' as refpago,          ",
               "        ' ' as marcapago, ' ' as nota,                       ",
               "        a.id_solicitud||a.consec_beneficiario as sol_consec, ",
               "        a.estado_solicitud, a.cod_rechazo                    ",
               " FROM   ret_beneficiario_juridico a,                         ",
               "        ret_beneficiario_generico b,                         ",
               "        ret_solicitud_generico c                             ", 
               " WHERE  a.id_solicitud = b.id_solicitud                      ",
               " AND    a.consec_beneficiario = b.consec_beneficiario        ",
               " AND    a.id_solicitud = c.id_solicitud                      ",
               " AND    c.modalidad_retiro = 3                               ",
               " AND    b.tpo_beneficiario = 2                               ",
               " AND    (c.caso_adai IS NOT NULL AND c.caso_adai <> 0)       ",
               " AND    (a.estado_solicitud IN (71)  OR                      ",
               "        (a.estado_solicitud = 210 AND a.cod_rechazo IN (64,65) )) ",
               " AND    c.id_solicitud = ?                                   "   
               
   PREPARE prp_consulta_beneficiario FROM v_sql
   DECLARE cur_consulta_beneficiario CURSOR FOR prp_consulta_beneficiario 

   
   -- 16Dic2013. Se verifica si hay datos para finalización de notificación
--   SELECT COUNT(*)
--   INTO   v_conteo
--   FROM   ret_beneficiario_juridico
--   WHERE  ((estado_solicitud IN (71) -- pagadas 
--          OR (estado_solicitud = 210 AND cod_rechazo IN (64,65))))

   SELECT COUNT(DISTINCT c.id_solicitud)
   INTO   v_conteo
   FROM   ret_solicitud_generico a, 
          ret_beneficiario_generico b,
          ret_beneficiario_juridico c
   WHERE  a.id_solicitud = b.id_solicitud 
   AND    a.id_solicitud = c.id_solicitud
   AND    b.consec_beneficiario = c.consec_beneficiario
   AND    a.modalidad_retiro = 3
   AND    b.tpo_beneficiario = 2
   AND    (a.caso_adai IS NOT NULL AND a.caso_adai <> 0)
   AND    ((c.estado_solicitud IN (71) OR  -- pagadas 
           (c.estado_solicitud = 210 AND c.cod_rechazo IN (64,65))))          

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
      SELECT DISTINCT a.caso_adai, b.id_solicitud, a.nss, a.id_derechohabiente, 
             c.importe_viv92+c.importe_viv97+importe_viv97_anexo1
      FROM   ret_solicitud_generico a,
             ret_beneficiario_juridico b,
             ret_beneficiario_generico d,
             ret_ley73_generico c 
      WHERE  a.modalidad_retiro = 3
      AND    a.id_solicitud = c.id_solicitud
      AND    a.id_solicitud = b.id_solicitud
      AND    a.id_solicitud = d.id_solicitud
      AND    b.consec_beneficiario = d.consec_beneficiario
      AND    d.tpo_beneficiario = 2
      AND    (a.caso_adai IS NOT NULL AND a.caso_adai <> 0)
      AND    (b.estado_solicitud IN (71) OR 
             ( b.estado_solicitud = 210 AND b.cod_rechazo IN (64,65) ))

      -- se leen las solicitudes de estos casos
      FOREACH cur_casoadai INTO v_caso_adai_buscado, v_id_solicitud, v_nss, v_id_derechohabiente, v_importe_confirma 
         -- se busca la respuesta de 
         -- Consulta pago fico
         INITIALIZE confPagoJuridicoSSV        TO NULL
         INITIALIZE MT_confirmacionPagoSSV_req TO NULL 
         LET confPagoJuridicoSSV.requestConfirmacionPago.nss                     = v_nss
         LET confPagoJuridicoSSV.requestConfirmacionPago.noCaso                  = v_caso_adai_buscado
         LET confPagoJuridicoSSV.requestConfirmacionPago.referenciaPago          = v_rsp_referencia
         LET confPagoJuridicoSSV.requestConfirmacionPago.tipoBeneficiario        = 2
         LET confPagoJuridicoSSV.requestConfirmacionPago.montoFinal              = v_importe_confirma
         LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.nss              = v_nss
         LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.noCaso           = v_caso_adai_buscado
         LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.tipoBeneficiario = 2
         LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.referenciaPago   = v_rsp_referencia

         CALL v_arr_beneficiario.clear()
         SELECT COUNT(*) --- Cantidad beneficiarios por Caso CRM 
         INTO   v_regs_por_caso
         FROM   ret_beneficiario_juridico
         WHERE  id_solicitud = v_id_solicitud;

         SELECT COUNT(*) --- Cantidad beneficiarios pagados por Caso CRM
         INTO   v_reg_por_caso_pagados
         FROM   ret_beneficiario_juridico
         WHERE  id_solicitud = v_id_solicitud
         AND    estado_solicitud = 71;

         SELECT COUNT(*) --- Cantidad beneficiarios rechazados por Caso CRM
         INTO   v_reg_por_caso_rechazado
         FROM   ret_beneficiario_juridico
         WHERE  id_solicitud = v_id_solicitud
         AND    estado_solicitud = 210;

         IF v_regs_por_caso = (v_reg_por_caso_pagados + v_reg_por_caso_rechazado) THEN 
         
            IF (v_reg_por_caso_pagados > 0) AND (v_reg_por_caso_rechazado > 0) THEN 
               LET confPagoJuridicoSSV.confirmacionPago.estatusSACI = '03'
               LET confPagoJuridicoSSV.confirmacionPago.cRechazo = ''
               LET confPagoJuridicoSSV.confirmacionPago.dRechazo = ''
            ELSE 
               IF v_regs_por_caso = v_reg_por_caso_pagados THEN 
                  LET confPagoJuridicoSSV.confirmacionPago.estatusSACI = '01'
                  LET confPagoJuridicoSSV.confirmacionPago.cRechazo = ''
                  LET confPagoJuridicoSSV.confirmacionPago.dRechazo = ''
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.estatusSACI = '01'
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.cRechazo = ''
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.dRechazo = ''
               ELSE 
                  LET confPagoJuridicoSSV.confirmacionPago.estatusSACI = '02'
                  LET confPagoJuridicoSSV.confirmacionPago.cRechazo = '030'
                  LET confPagoJuridicoSSV.confirmacionPago.dRechazo = ''
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.estatusSACI = '95'
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.cRechazo = '040'
                  LET MT_confirmacionPagoSSV_req.confirmacionPago.dRechazo = ''
               END IF 
            END IF 
            
            LET v_indice = 1
            FOREACH cur_consulta_beneficiario USING v_id_solicitud INTO v_arr_beneficiario[v_indice].*
         
               EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                             USING v_arr_beneficiario[v_indice].sol_conesc
               -- Arma información para notificar a Juridico
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].idBeneficiario = v_arr_beneficiario[v_indice].idBeneficiario
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].nombreBeneficiario = v_arr_beneficiario[v_indice].nombreBeneficiario
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].paternoBeneficiario = v_arr_beneficiario[v_indice].paternoBeneficiario
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].maternoBeneficiario = v_arr_beneficiario[v_indice].maternoBeneficiario
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].porcentaje = v_arr_beneficiario[v_indice].porcentaje
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].referenciaPago = v_rsp_referencia
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].marcaPago = v_arr_beneficiario[v_indice].marcaPago
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].fechaPago = v_rsp_f_pago
               LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].montoPago = v_importe_confirma * (v_arr_beneficiario[v_indice].porcentaje/100) USING '&&&&&&&&&&.&&'
               IF v_arr_beneficiario[v_indice].estado_solicitud = 71 THEN 
                  LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].nota = "PAGADO"
               ELSE
                  LET confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].nota = "RECHAZADO"
               END IF 
               -- Arma información para notificar a CRM
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].idBeneficiario = v_arr_beneficiario[v_indice].idBeneficiario
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].nombreBeneficiario = v_arr_beneficiario[v_indice].nombreBeneficiario
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].paternoBeneficiario = v_arr_beneficiario[v_indice].paternoBeneficiario
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].maternoBeneficiario = v_arr_beneficiario[v_indice].maternoBeneficiario
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].porcentaje = v_arr_beneficiario[v_indice].porcentaje
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].referenciaPago = v_rsp_referencia
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].marcaPago = v_arr_beneficiario[v_indice].marcaPago
               LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[v_indice].nota = confPagoJuridicoSSV.confirmacionBeneficiarios[v_indice].nota 
               LET v_indice = v_indice + 1
            END FOREACH 
            -- se informa a CRM el cambio de estatus
            INITIALIZE confPagoJuridicoSSVResponse TO NULL
            CALL confPagoJuridicoSSV_g() RETURNING v_resultado -- Se envia 1 mientras se definen los beneficiarios


            DISPLAY "Codigo respuesta ", confPagoJuridicoSSVResponse.responseConfirmacionPago.codigo
            DISPLAY "Descripción      ", confPagoJuridicoSSVResponse.responseConfirmacionPago.descripcion
            DISPLAY "Mensaje          ", confPagoJuridicoSSVResponse.responseConfirmacionPago.mensaje
            
            IF v_resultado = 0 AND confPagoJuridicoSSVResponse.responseConfirmacionPago.codigo = "000" THEN
            
               DISPLAY "Notificado exitosamente"
               DISPLAY "           Caso CRM: ", v_caso_adai_buscado
               DISPLAY "                NSS: ", v_nss
               DISPLAY "        d Solicitud: ", v_id_solicitud 
               --- Se actualizan los registros segun su estado
               UPDATE ret_beneficiario_juridico
               SET    estado_solicitud = 72 -- pagada
               WHERE  id_solicitud     = v_id_solicitud
               AND    estado_solicitud = 71
               UPDATE ret_beneficiario_juridico
               SET    estado_solicitud = 214 -- pagada
               WHERE  id_solicitud     = v_id_solicitud
               AND    estado_solicitud = 210
               -- Se notifica a CRM
               INITIALIZE MT_confirmacionPagoSSV_res TO NULL 
               CALL SI_confirmacionPagoSSV_SO_g() RETURNING v_resultado
               IF ( v_resultado = 0 ) THEN      
                  LET v_codigo = MT_confirmacionPagoSSV_res.confirmacionPago[1].codigo
                  LET v_mensaje = CURRENT YEAR TO SECOND, " El código devuelto >", MT_confirmacionPagoSSV_res.confirmacionPago[1].codigo, "<"
                  DISPLAY v_mensaje
                  LET v_mensaje = CURRENT YEAR TO SECOND, " La descripcón devuelta >", MT_confirmacionPagoSSV_res.confirmacionPago[1].descripcion, "<"
                  DISPLAY v_mensaje
                  LET v_mensaje = CURRENT YEAR TO SECOND, " El mensaje devuelto >", MT_confirmacionPagoSSV_res.confirmacionPago[1].mensaje, "<"
                  DISPLAY v_mensaje
                  IF v_regs_por_caso = v_reg_por_caso_pagados THEN 
                     UPDATE ret_solicitud_generico
                     SET    estado_solicitud = 72 -- pagada
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 71
                     UPDATE ret_ley73_generico
                     SET    estado_solicitud = 72 -- pagada
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 71
                  END IF 
                  IF v_regs_por_caso = v_reg_por_caso_rechazado THEN 
                     UPDATE ret_solicitud_generico
                     SET    estado_solicitud = 214 -- recahzada
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 210
                     UPDATE ret_ley73_generico
                     SET    estado_solicitud = 214 -- rechazada
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 210
                  END IF 
                  IF v_reg_por_caso_rechazado > 0 AND v_reg_por_caso_pagados > 0 THEN
                     UPDATE ret_solicitud_generico
                     SET    estado_solicitud = 720 -- pagada parcialmente notidicada CRM
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 710
                     UPDATE ret_ley73_generico
                     SET    estado_solicitud = 720 -- pagada parcialmente notidicada CRM
                     WHERE  id_solicitud     = v_id_solicitud
                     AND    estado_solicitud = 710
                  END IF  
                  -- se desmarca la cuenta
                  CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, v_marca,
                                                       v_id_solicitud, v_marca,
                                                       p_usuario_cod, g_proceso_cod)

               ELSE 
                  LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de confirmación de pago a CRM"
                  DISPLAY v_mensaje
                  DISPLAY "NSS      = ", v_nss
                  DISPLAY "Caso CRM = ", v_caso_adai_buscado
                  DISPLAY "CODE       : ", wsError.code
                  DISPLAY "CODENS     : ", wsError.codeNS
                  DISPLAY "DESCRIPTION: ", wsError.description
                  DISPLAY "ACTION     : ", wsError.action	  
               END IF
            ELSE 
               DISPLAY "ERROR al invocar webservice de confirmacion de pago de beneficiarios a Jurídico"
               DISPLAY "NSS      = ", v_nss
               DISPLAY "Caso CRM = ", v_caso_adai_buscado
               DISPLAY "=========================================================\n"
            END IF
            
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