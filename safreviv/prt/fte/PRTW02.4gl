--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/03/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTW02                                                        #
#Objetivo     => Invoca Cliente (PRTW03) de notificación de marca a adai       #
#Fecha inicio => 09 Marzo 2015                                                 #
################################################################################
IMPORT com

DATABASE safre_viv

GLOBALS "PRTWS02.inc"
GLOBALS "PRTG01.4gl"

CONSTANT C_ORIGEN_SOLICITUD              CHAR(3)  = "002"
CONSTANT C_CERO                          SMALLINT = 0
CONSTANT C_RECHAZO_VALIDACION            SMALLINT = 30     
CONSTANT C_RECHAZO_PORTABILIDAD_PROCESAR SMALLINT = 999  

DEFINE g_today               DATE
DEFINE v_origen_solicitud    CHAR(003)

DEFINE g_usuario_cod         CHAR(20),
       g_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
       g_respuesta_adai      RECORD
          v_resultado_ws        INTEGER,
          v_resultado_operacion CHAR(5),
          v_mensaje             VARCHAR(100)
       END RECORD,
       g_nulo                CHAR(1),
       g_resultado_maq RECORD
          v_ind            SMALLINT,
          v_diag           CHAR(3),
          v_error_sql      INTEGER,
          v_error_isam     INTEGER,
          v_msg_sql        VARCHAR(254),
          v_estado_destino SMALLINT
       END RECORD,
       g_errores RECORD
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  CHAR(254),
          v_ind        CHAR(5),
          v_diag       CHAR(254)
       END RECORD,
       g_notificacion_marca RECORD
          v_bus_proceso      LIKE prt_bus_notifica_marca.bus_proceso_cod,
          v_bus_operacion    LIKE prt_bus_notifica_marca.bus_operacion_cod,
          v_nss              LIKE prt_bus_notifica_marca.nss,
          v_diag_procesar    LIKE prt_bus_notifica_marca.diagnostico_procesar
       END RECORD

MAIN
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_folio_procesar           LIKE prt_bus_notifica_marca.folio_procesar

   # Datos recibidos de procesar a travez del bus
   LET p_id_bus_solicitud_tramite = ARG_VAL(1)
   LET p_folio_procesar           = ARG_VAL(2)

   LET g_today                    = TODAY   
   
   # establece 5 segundos como tiempo maximo de espera en request y response
   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 5 )

   CALL fn_inicializa_consultas()
   CALL fn_determina_flujo(p_id_bus_solicitud_tramite,
                           p_folio_procesar)

END MAIN

# Descripción: inicializa consultas
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET g_usuario_cod = "SAFREVIV" # Por no intervenir un usuario especifico, se usa safreviv

   LET v_consulta = "INSERT INTO prt_his_solicitud_cedente(",
                    " id_his_solicitud_cedente,",
                    " id_prt_solicitud_cedente,",
                    " id_cat_dato_actualizado,",
                    " f_modificacion,",  
                    " valor_modificado,",
                    " valor_actual,",
                    " usuario) VALUES(seq_prt_his_solicitud_cedente.NEXTVAL,?,?,?,?,?,?)"
   PREPARE prp_ins_his_sol_cedente FROM v_consulta

   LET v_consulta = "INSERT INTO prt_reenvios_solicitud(",
                    " id_solicitud,",
                    " tipo_solicitud,",
                    " estado_envio,",
                    " diagnostico_sol)",
                    " VALUES(?,?,?,?)"
   PREPARE prp_ins_reg_reenvio FROM v_consulta
   
   LET v_consulta = " SELECT FIRST 1 id_derechohabiente",
                    "   FROM prt_bus_notifica_marca bus JOIN afi_derechohabiente afi",
                    "     ON afi.nss = bus.nss",
                    "  WHERE id_bus_solicitud_tramite = ?"
   PREPARE prp_rec_id_derech FROM v_consulta

   LET v_consulta = " SELECT n_referencia",
                    "   FROM sfr_marca_activa",
                    "  WHERE id_derechohabiente = ?",
                    "    AND marca = ?"
   PREPARE prp_verifica_marca FROM v_consulta
   
   LET v_consulta = " SELECT bus_proceso_cod,",
                    "        bus_operacion_cod,",
                    "        nss,",
                    "        diagnostico_procesar",
                    "   FROM prt_bus_notifica_marca",
                    "  WHERE id_bus_solicitud_tramite = ?"
   PREPARE prp_recupera_res_marca FROM v_consulta

   LET v_consulta = " SELECT ruta_bin,",
                    "        ruta_listados",
                    "   FROM seg_modulo",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_recupera_rutas FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_derechohabiente",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_id_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 estado",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_rec_estado_sol FROM v_consulta

   LET v_consulta = " SELECT id_prt_solicitud_cedente,",
                    "        n_caso,",
                    "        diagnostico_interno",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND estado = ?"
   PREPARE prp_recupera_solicitud_prt FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 diagnostico_interno",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_recupera_diag_solicitud FROM v_consulta

   LET v_consulta = " SELECT diagnostico_interno,",
                    "        descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_externo = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_interno FROM v_consulta

   LET v_consulta = " SELECT diagnostico_externo,",
                    "        descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_interno = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_externo FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 etq.id_entidad_etiqueta",
                    "   FROM glo_entidad_etiqueta etq JOIN glo_entidad_historico_consulta his",
                    "     ON etq.id_entidad_historico = his.id_entidad_historico",
                    "  WHERE his.entidad_cod = 'prt_solicitud_cedente'",
                    "    AND etq.cve_natural = 'diagnostico_interno'"
   PREPARE prp_rec_id_entidad_etiqueta FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET folio_procesar = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_folio_proc FROM v_consulta 

   LET v_consulta = " UPDATE prt_his_id_folio " ,
                    "    SET folio_procesar = ? ",
                    "  WHERE id_prt_origen = ? "
   PREPARE prp_actualiza_folio_proc_his FROM v_consulta 

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET diagnostico_interno = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " DELETE",
                    "   FROM prt_bus_notifica_marca",
                    "  WHERE id_bus_solicitud_tramite = ?"
   PREPARE prp_eli_tramite_tmp FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_glo_maq_individual(?,?,?,?)"
   PREPARE prp_avanza_maquinaria FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_marca_cuenta FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE prp_desmarca_cuenta FROM v_consulta
   
   LET v_consulta = "EXECUTE PROCEDURE sp_prt_solicita_traspaso_cedente(?,?,?,?)"
   PREPARE prp_solicita_traspaso FROM v_consulta

   LET v_consulta = "EXECUTE PROCEDURE sp_prt_error_bus(?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_error_bus FROM v_consulta

   LET v_consulta = "EXECUTE PROCEDURE sp_prt_recibe_notif_marca_receptora(?,?)"
   PREPARE prp_notifica_receptora FROM v_consulta   

END FUNCTION

# Descripción: determina si se marca o desmarca la cuenta
FUNCTION fn_determina_flujo(p_id_bus_solicitud_tramite,
                            p_folio_procesar)
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_folio_procesar           LIKE prt_bus_notifica_marca.folio_procesar,
       v_id_derechohabienre       LIKE prt_bus_notifica_marca.nss,
       v_id_prt_solicitud         LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_estado                   LIKE prt_solicitud_cedente.estado,
       v_solicitud_prt RECORD
          v_id_prt_solicitud     LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_num_caso             LIKE prt_solicitud_cedente.n_caso,
          v_diagnostico_int      LIKE prt_solicitud_cedente.diagnostico_interno
       END RECORD

   EXECUTE prp_rec_id_derech USING p_id_bus_solicitud_tramite
                              INTO v_id_derechohabienre
                              
   SELECT a.origen_solicitud
   INTO   v_origen_solicitud
   FROM   prt_bus_notifica_marca a
   WHERE  a.id_bus_solicitud_tramite = p_id_bus_solicitud_tramite

   # si el origen solicitud es 002 (fovissste) se ejecuta store de receptora
   IF v_origen_solicitud = "002" THEN
      EXECUTE prp_notifica_receptora USING p_id_bus_solicitud_tramite ,
                                           p_folio_procesar
      EXIT PROGRAM                                          
   END IF 

   # Recupera la notificación
   EXECUTE prp_recupera_res_marca USING p_id_bus_solicitud_tramite
                                   INTO g_notificacion_marca.*
                              
   INITIALIZE v_id_prt_solicitud TO NULL
   EXECUTE prp_verifica_marca USING v_id_derechohabienre,
                                    C_MARCA_PRT_CEDENTE
                               INTO v_id_prt_solicitud

   IF( v_id_prt_solicitud IS NULL )THEN
      # Verifica el diagnóstico = 0
      EXECUTE prp_recupera_solicitud_prt USING g_notificacion_marca.v_nss,
                                               C_ESTADO_MARCA_SOL_PRO
                                          INTO v_solicitud_prt.*
      IF( v_solicitud_prt.v_diagnostico_int = C_DIAGNOSTICO_INTERNO_0 )THEN
         CALL fn_notifica_marca(p_id_bus_solicitud_tramite,
                                p_folio_procesar,
                                v_solicitud_prt.*)
      ELSE
         LET g_errores.v_sql_error = 100
         LET g_errores.v_msg_error = "Notificación de marca no procede para nss "||g_notificacion_marca.v_nss CLIPPED||" y tramite "||p_id_bus_solicitud_tramite CLIPPED||" con diagnóstico "||g_notificacion_marca.v_diag_procesar
         LET g_errores.v_diag      = "Solicitud solicitud no encontrada"
            
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
      END IF
   ELSE
      # desmarca sólo si esta en estado 42 pendiente de notificar a CRM o 90 desmarca solicitada a procesar
      EXECUTE prp_rec_estado_sol USING v_id_prt_solicitud
                                  INTO v_estado
      CASE v_estado

         WHEN C_ESTADO_PENDIE_NOTIF_CRM
            CALL fn_desmarca_cuenta(p_id_bus_solicitud_tramite,
                                    p_folio_procesar,
                                    v_id_prt_solicitud)

         WHEN C_ESTADO_DESMARCA_SOL_PRO
            CALL fn_desmarca_cuenta(p_id_bus_solicitud_tramite,
                                    p_folio_procesar,
                                    v_id_prt_solicitud)
            IF( g_notificacion_marca.v_diag_procesar = C_DIAG_PROCESAR_MARCA_ACEPTADA )THEN 
               -- Avanza a desmarcada
               EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                                   v_id_prt_solicitud,
                                                   C_SENAL_DESMARCA_PRO_AC,
                                                   g_usuario_cod
                                              INTO g_resultado_maq.*
            ELSE
               EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                                   v_id_prt_solicitud,
                                                   C_SENAL_DESMARCA_PRO_RE,
                                                   g_usuario_cod
                                              INTO g_resultado_maq.*
            END IF

         OTHERWISE
            LET g_errores.v_sql_error = 100
            LET g_errores.v_msg_error = "Desmarca no procede para solicitud "||v_id_prt_solicitud CLIPPED
            --LET v_errores.v_ind       = 
            LET g_errores.v_diag      = "Cuenta no desmarcada para nss "||g_notificacion_marca.v_nss||" estado de solicitud "||v_estado

            CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                     g_notificacion_marca.v_bus_proceso,
                                     g_notificacion_marca.v_bus_operacion,
                                     g_errores.*)

      END CASE
      {IF( v_estado = C_ESTADO_PENDIE_NOTIF_CRM OR v_estado = C_ESTADO_DESMARCA_SOL_PRO )THEN
         CALL fn_desmarca_cuenta(p_id_bus_solicitud_tramite,
                                 p_folio_procesar,
                                 v_id_prt_solicitud)
      ELSE
         LET g_errores.v_sql_error = 100
         LET g_errores.v_msg_error = "Desmarca no procede para solicitud "||v_id_prt_solicitud CLIPPED
         --LET v_errores.v_ind       = 
         LET g_errores.v_diag      = "Cuenta no desmarcada para nss "||g_notificacion_marca.v_nss||" estado de solicitud "||v_estado
            
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
      END IF}
   END IF
END FUNCTION

# Descripción: envia notificación de marca a ADAI y solicita traspasos de saldos a procesar
FUNCTION fn_notifica_marca(p_id_bus_solicitud_tramite,
                           p_folio_procesar,
                           p_solicitud_prt)
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_folio_procesar           LIKE prt_bus_notifica_marca.folio_procesar,       
       p_solicitud_prt RECORD
          v_id_prt_solicitud     LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_num_caso             LIKE prt_solicitud_cedente.n_caso,
          v_diagnostico_int      LIKE prt_solicitud_cedente.diagnostico_interno
       END RECORD,
       v_diagnostico_interno RECORD
          v_diag_int LIKE prt_diagnostico.diagnostico_interno,
          v_desc_gen LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_diagnostico_externo RECORD
          v_diag_ext LIKE prt_diagnostico.diagnostico_externo,
          v_desc_gen LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_id_entidad_etiqueta DECIMAL(9,0),
       r_cod_rch             SMALLINT

   WHENEVER ERROR CONTINUE

   # Actualiza el folio procesar en la solicitud cedente
   EXECUTE prp_actualiza_folio_proc USING p_folio_procesar,
                                          p_solicitud_prt.v_id_prt_solicitud

   EXECUTE prp_actualiza_folio_proc_his USING p_folio_procesar,
                                              p_solicitud_prt.v_id_prt_solicitud

   EXECUTE prp_rec_id_derechohabiente USING g_notificacion_marca.v_nss
                                       INTO g_id_derechohabiente

   # Para histórico de la solicitud
   EXECUTE prp_rec_id_entidad_etiqueta INTO v_id_entidad_etiqueta

   # Recupera diagnóstico interno de procesar
   EXECUTE prp_consulta_diag_interno USING g_notificacion_marca.v_diag_procesar,
                                           C_DESTINO_DIAG_P_I
                                      INTO v_diagnostico_interno.*
   # Genera registro histórico
   EXECUTE prp_ins_his_sol_cedente USING p_solicitud_prt.v_id_prt_solicitud,
                                         v_id_entidad_etiqueta,
                                         g_today,
                                         p_solicitud_prt.v_diagnostico_int,
                                         v_diagnostico_interno.v_diag_int,
                                         g_usuario_cod

   IF( g_notificacion_marca.v_diag_procesar = C_DIAG_PROCESAR_MARCA_ACEPTADA )THEN # Marca aceptada por procesar
      LET g_nulo = NULL
      EXECUTE prp_marca_cuenta USING g_id_derechohabiente, # Derechohabiente
                                     C_MARCA_PRT_CEDENTE,  # Marca portabilidad
                                     p_solicitud_prt.v_id_prt_solicitud, # ID referencia
                                     p_id_bus_solicitud_tramite, # Folio, en lugar de folio se usa id tramite bus para referenciarlo
                                     C_CERO, # Estado marca
                                     C_CERO, # Código rechazo
                                     g_nulo, # Marca causa
                                     g_nulo, # Fecha causa
                                     g_usuario_cod,
                                     C_PROCESO_COD_MARCA
                                INTO r_cod_rch
      IF( r_cod_rch <> 0)THEN
         # Actualiza diagnóstico para indicar falla en ws
         EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_19,
                                               p_solicitud_prt.v_id_prt_solicitud
         LET g_errores.v_sql_error = 100
         LET g_errores.v_msg_error = "Cuenta no marcada para id derechohabiente "||g_id_derechohabiente CLIPPED||" y solicitud "||p_solicitud_prt.v_id_prt_solicitud
         LET g_errores.v_ind       = r_cod_rch
         LET g_errores.v_diag      = "Marca rechazada con código de rechazo "||r_cod_rch
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL
      --ELSE
      END IF
      # Cambia el estado indicando que se encuentra marcada en procesar
      EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                          p_solicitud_prt.v_id_prt_solicitud,
                                          C_SENAL_ACEPTADA_MARCA,                                                
                                          g_usuario_cod
                                     INTO g_resultado_maq.*
      IF NOT( g_resultado_maq.v_ind = 0 AND g_resultado_maq.v_error_sql = 0 )THEN
         LET g_errores.v_sql_error  = g_resultado_maq.v_error_sql
         LET g_errores.v_isam_error = g_resultado_maq.v_error_isam
         LET g_errores.v_msg_error  = g_resultado_maq.v_msg_sql
         LET g_errores.v_ind        = g_resultado_maq.v_ind
         LET g_errores.v_diag       = "Error al actualizar maquinaria notificación marca aceptada a adai, en solicitud "||p_solicitud_prt.v_id_prt_solicitud
            
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL
      END IF

      # Invoca cliente ws para enviar notificación del resultado de la marca a ADAI
      # C_DIAG_PROCESAR_MARCA_ACEPTADA 000 por ser aceptada
      CALL recibeMarcaCedente(p_solicitud_prt.v_num_caso,
                              C_ID_ESTATUS_NOTIFICA_MARCA,
                              C_DIAG_PROCESAR_MARCA_ACEPTADA) RETURNING g_respuesta_adai.v_resultado_ws,
                                                                        g_respuesta_adai.v_resultado_operacion,
                                                                        g_respuesta_adai.v_mensaje
      # Sí ADAI recibe correctamente la solicitud
      IF( g_respuesta_adai.v_resultado_ws = 0 AND 
          g_respuesta_adai.v_resultado_operacion = C_ID_RESPUESTA_ADAI )THEN
            
         # Elimina la notificación temporal hasta haber realizado todo correctamente
         EXECUTE prp_eli_tramite_tmp USING p_id_bus_solicitud_tramite
         
      ELSE
         EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_17,
                                               p_solicitud_prt.v_id_prt_solicitud

         # se avanza maquinaria para quedar en estado pendiente de notificar a CRM                                                  
         EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                             p_solicitud_prt.v_id_prt_solicitud,
                                             C_SENAL_NOTIFICAR_CRM,                                                
                                             g_usuario_cod
                                        INTO g_resultado_maq.*

         EXECUTE prp_ins_reg_reenvio USING p_solicitud_prt.v_id_prt_solicitud,
                                           C_FLUJO_CEDENTE, # tipo_solicitud
                                           C_REENVIO_NO_ENVIADO, # estado envío
                                           g_notificacion_marca.v_diag_procesar
                                                  
         LET g_errores.v_sql_error = g_respuesta_adai.v_resultado_ws
         LET g_errores.v_msg_error = "Error de comunicación con ADAI, solicitud ACEPTADA "||p_solicitud_prt.v_id_prt_solicitud
         LET g_errores.v_ind       = g_respuesta_adai.v_resultado_operacion
         LET g_errores.v_diag      = "Marca no informada a adai, mensaje recibido: "||g_respuesta_adai.v_mensaje
           
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL
      END IF
      
      # Solicita traspaso de saldos
      CALL fn_solicita_traspaso(p_id_bus_solicitud_tramite,
                                p_solicitud_prt.v_id_prt_solicitud)
                                
   ELSE
      # EL DIAGNOSTICO YA SE RECUPERA DESDE ANTES
      EXECUTE prp_consulta_diag_externo USING g_notificacion_marca.v_diag_procesar,
                                              C_DESTINO_DIAG_P_A
                                         INTO v_diagnostico_externo.*
      EXECUTE prp_actualiza_solicitud USING v_diagnostico_interno.v_diag_int,
                                            p_solicitud_prt.v_id_prt_solicitud
                                      
      # se registra intento histórico de marca como rechazo por validacion 
      LET g_nulo = NULL
      EXECUTE prp_marca_cuenta USING g_id_derechohabiente               , # Derechohabiente
                                     C_MARCA_PRT_CEDENTE                , # Marca portabilidad
                                     p_solicitud_prt.v_id_prt_solicitud , # ID referencia
                                     p_id_bus_solicitud_tramite         , # Folio, en lugar de folio se usa id tramite bus para referenciarlo
                                     C_RECHAZO_VALIDACION               , # Estado marca - rechazo por validacion
                                     C_RECHAZO_PORTABILIDAD_PROCESAR    , # Código rechazo rechazo portabilidad procesar
                                     C_MARCA_PRT_CEDENTE                , # Marca causa portabilidad cedente
                                     g_today                            , # Fecha causa
                                     g_usuario_cod,
                                     C_PROCESO_COD_MARCA
                                INTO r_cod_rch

      EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                          p_solicitud_prt.v_id_prt_solicitud,
                                          C_SENAL_RECHAZADA_MARCA,
                                          g_usuario_cod
                                     INTO g_resultado_maq.*

      IF NOT( g_resultado_maq.v_ind = 0 AND g_resultado_maq.v_error_sql = 0 )THEN
         LET g_errores.v_sql_error  = g_resultado_maq.v_error_sql
         LET g_errores.v_isam_error = g_resultado_maq.v_error_isam
         LET g_errores.v_msg_error  = g_resultado_maq.v_msg_sql
         LET g_errores.v_ind        = g_resultado_maq.v_ind
         LET g_errores.v_diag       = "Error al actualizar maquinaria notificación marca rechazada a adai, en solicitud "||p_solicitud_prt.v_id_prt_solicitud
            
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL
      END IF        

      # Invoca cliente ws para enviar notificación del resultado de la marca a ADAI
      CALL recibeMarcaCedente(p_solicitud_prt.v_num_caso,
                              C_ID_ESTATUS_NOTIFICA_MARCA,
                              --v_notificacion_marca.v_diag_procesar) # Envia el diagnóstico de procesar directamente a ADAI
                              v_diagnostico_externo.v_diag_ext CLIPPED) # Envia el diagnóstico de procesar directamente a ADAI
                                                                      RETURNING g_respuesta_adai.v_resultado_ws,
                                                                                g_respuesta_adai.v_resultado_operacion,
                                                                                g_respuesta_adai.v_mensaje
      IF( g_respuesta_adai.v_resultado_ws = 0 AND 
          g_respuesta_adai.v_resultado_operacion = C_ID_RESPUESTA_ADAI )THEN
         # Elimina la notificación temporal hasta haber realizado todo correctamente
         EXECUTE prp_eli_tramite_tmp USING p_id_bus_solicitud_tramite
         
      ELSE
         # Actualiza diagnóstico para indicar falla en ws
         EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_17,
                                               p_solicitud_prt.v_id_prt_solicitud

         EXECUTE prp_ins_reg_reenvio USING p_solicitud_prt.v_id_prt_solicitud,
                                           C_FLUJO_CEDENTE, # tipo_solicitud
                                           C_REENVIO_NO_ENVIADO, # estado envío
                                           g_notificacion_marca.v_diag_procesar
                                           
         LET g_errores.v_sql_error = g_respuesta_adai.v_resultado_ws
         LET g_errores.v_msg_error = "Error de comunicación con ADAI, solicitud RECHAZADA "||p_solicitud_prt.v_id_prt_solicitud
         LET g_errores.v_ind       = g_respuesta_adai.v_resultado_operacion
         LET g_errores.v_diag      = "Marca no informada a adai, mensaje recibido: "||g_respuesta_adai.v_mensaje
           
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL
      END IF
   END IF
END FUNCTION

# Descripción: registra solicitud e invoca WS bus para solicitar transferencia de saldos
FUNCTION fn_solicita_traspaso(p_id_bus_solicitud_tramite,
                              p_id_prt_solicitud_cedente)
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       r_ind        INTEGER,
       r_diag       CHAR(3),
       r_error_sql  INTEGER,
       r_error_isam INTEGER,
       r_msg_sql    VARCHAR(254),
       r_estado_destino SMALLINT

   WHENEVER ERROR CONTINUE # no debe detenerse la ejecución

   # SP avanza la maquinaria
   EXECUTE prp_solicita_traspaso USING p_id_prt_solicitud_cedente,
                                       C_BUS_PROCESO_COD_SOL_TRASP,
                                       C_BUS_OPERACION_COD_SOL_TRASP,
                                       g_usuario_cod
                                  INTO r_ind,
                                       r_diag,
                                       r_error_sql,
                                       r_error_isam,
                                       r_msg_sql,
                                       r_estado_destino
   
   IF( r_error_sql <> 0 )THEN
      DISPLAY "Error solicitar traspaso:"
      DISPLAY "ID prt solicitud: ",p_id_prt_solicitud_cedente
      DISPLAY "Código: ",r_error_sql
      DISPLAY "Mensaje: ",r_msg_sql

      EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_1,
                                            p_id_prt_solicitud_cedente
      LET g_errores.v_sql_error  = r_error_sql
      LET g_errores.v_isam_error = r_error_isam
      LET g_errores.v_msg_error  = r_msg_sql
      LET g_errores.v_ind        = r_ind
      LET g_errores.v_diag       = "Error al solicitar traspaso cedente "||p_id_prt_solicitud_cedente
            
      CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                               C_BUS_PROCESO_COD_SOL_TRASP,
                               C_BUS_OPERACION_COD_SOL_TRASP,
                               g_errores.*)
      INITIALIZE g_errores TO NULL
   
   END IF

END FUNCTION

# Descripción: genera registro de error bus
FUNCTION fn_genera_error_bus(p_id_bus_solicitud_tramite,
                             p_bus_proceso,
                             p_bus_operacion,
                             p_errores)
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_bus_proceso      LIKE prt_bus_notifica_marca.bus_proceso_cod,
       p_bus_operacion    LIKE prt_bus_notifica_marca.bus_operacion_cod,
       f_actual             DATETIME YEAR TO SECOND,
       p_errores RECORD
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  CHAR(254),
          v_ind        CHAR(5),
          v_diag       CHAR(254)
       END RECORD

   LET f_actual = CURRENT YEAR TO SECOND
   EXECUTE prp_error_bus USING p_id_bus_solicitud_tramite,
                               p_bus_proceso,
                               p_bus_operacion,
                               g_usuario_cod,
                               f_actual,
                               p_errores.v_sql_error,
                               p_errores.v_isam_error,
                               p_errores.v_msg_error,
                               "PRTW02",
                               p_errores.v_ind,
                               p_errores.v_diag
END FUNCTION

# Descripción: Desmarca cuenta para la solicitud
FUNCTION fn_desmarca_cuenta(p_id_bus_solicitud_tramite,
                            p_folio_procesar,
                            p_id_prt_solicitud)
DEFINE p_id_bus_solicitud_tramite LIKE prt_bus_notifica_marca.id_bus_solicitud_tramite,
       p_folio_procesar           LIKE prt_bus_notifica_marca.folio_procesar,
       p_id_prt_solicitud         LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_rechazo_cod SMALLINT,
       v_diag_int_anterior LIKE prt_diagnostico.diagnostico_interno,
       v_diagnostico_interno RECORD
          v_diag_int LIKE prt_diagnostico.diagnostico_interno,
          v_desc_gen LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_id_entidad_etiqueta DECIMAL(9,0)
       
   # Actualiza el folio procesar en la solicitud cedente
   EXECUTE prp_actualiza_folio_proc USING p_folio_procesar,
                                          p_id_prt_solicitud

   EXECUTE prp_actualiza_folio_proc_his USING p_folio_procesar,
                                              p_id_prt_solicitud

   EXECUTE prp_rec_id_derechohabiente USING g_notificacion_marca.v_nss
                                       INTO g_id_derechohabiente

   EXECUTE prp_recupera_diag_solicitud USING p_id_prt_solicitud
                                        INTO v_diag_int_anterior
   # Para histórico de la solicitud
   EXECUTE prp_rec_id_entidad_etiqueta INTO v_id_entidad_etiqueta
   
   # Actualiza diagnóstico de rechazo de desmarcamarca según el código devuelto por procesar
   EXECUTE prp_consulta_diag_interno USING g_notificacion_marca.v_diag_procesar,
                                           C_DESTINO_DIAG_P_I
                                      INTO v_diagnostico_interno.*
                                         
   # Genera registro histórico
   EXECUTE prp_ins_his_sol_cedente USING p_id_prt_solicitud,--v_solicitud_prt.v_id_prt_solicitud,
                                         v_id_entidad_etiqueta,
                                         g_today,
                                         v_diag_int_anterior,
                                         v_diagnostico_interno.v_diag_int,
                                         g_usuario_cod

   IF( g_notificacion_marca.v_diag_procesar = C_DIAG_PROCESAR_MARCA_ACEPTADA )THEN 
      EXECUTE prp_desmarca_cuenta USING g_id_derechohabiente,
                                        C_MARCA_PRT_CEDENTE, # Marca entra
                                        p_id_prt_solicitud, # Referencia
                                        C_CERO,             # Estado marca
                                        C_CERO,
                                        g_usuario_cod,
                                        C_PROCESO_COD_MARCA
                                   INTO v_rechazo_cod
      IF( v_rechazo_cod = 0 )THEN
         # Actualiza a cuenta desmarcada
         # Diagnóstico con  destino_diag=INF 
         EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_25,
                                               p_id_prt_solicitud
         EXECUTE prp_eli_tramite_tmp USING p_id_bus_solicitud_tramite      

      ELSE
         EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_23,
                                               p_id_prt_solicitud
            
         LET g_errores.v_sql_error = v_rechazo_cod
         LET g_errores.v_msg_error = "Error al desmarcar cuenta aceptada para solicitud "||p_id_prt_solicitud
         --LET v_errores.v_ind       = 
         LET g_errores.v_diag      = "Cuenta no desmarcada para nss "||g_notificacion_marca.v_nss||" y código de rechazo "||v_rechazo_cod
            
         CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                  g_notificacion_marca.v_bus_proceso,
                                  g_notificacion_marca.v_bus_operacion,
                                  g_errores.*)
         INITIALIZE g_errores TO NULL

      END IF
   ELSE
      # Actualiza diagnóstico de rechazo de desmarcamarca según el código devuelto por procesar
      {EXECUTE prp_consulta_diag_interno USING v_notificacion_marca.v_diag_procesar,
                                              C_DESTINO_DIAG_P_I
                                         INTO v_diagnostico_interno.*}
                                      
      EXECUTE prp_actualiza_solicitud USING v_diagnostico_interno.v_diag_int,
                                            p_id_prt_solicitud
                                            
      EXECUTE prp_eli_tramite_tmp USING p_id_bus_solicitud_tramite      
   END IF

END FUNCTION 
