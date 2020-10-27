--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS02                                                       #
#Objetivo     => Funciones de solicitud de portabilidad cedente                #
#                Constandes provenientes de PRTWS02.inc                        #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "PRTWS01.inc"
GLOBALS "PRTWS02.inc"
GLOBALS "PRTW01.inc"
GLOBALS "PRTG01.4gl"

PRIVATE
DEFINE v_f_actual  DATE,
       v_error     BOOLEAN,
       v_id_prt_solicitud     LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_id_prt_solicitud_tmp LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       v_valida_marca RECORD
          v_marca_error   SMALLINT,
          v_mensaje_marca VARCHAR(254)
       END RECORD,
       v_diagnostico_interno_marca LIKE prt_diagnostico.diagnostico_interno,
       r_resultado_invoca_ws  INTEGER

PRIVATE
DEFINE r_vigencia_credito RECORD
          v_resultado        SMALLINT,
          v_tipo_originacion SMALLINT,
          v_tpo_credito      SMALLINT,
          v_num_credito      DECIMAL(10,0),
          v_f_otorga         DATE,
          v_f_liquida        DATE
       END RECORD,
       v_saldo_derechohabiente RECORD
          v_acciones LIKE cta_movimiento.monto_acciones,
          v_pesos    LIKE cta_movimiento.monto_pesos
       END RECORD

PRIVATE
DEFINE v_diagostico_externo RECORD
          v_diag_ext LIKE prt_diagnostico.diagnostico_externo,
          v_desc_gen LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_diagnostico_interno RECORD
          v_diag_int LIKE prt_diagnostico.diagnostico_interno,
          v_desc_gen LIKE prt_diagnostico.descripcion_general
       END RECORD

PRIVATE
DEFINE v_resultado_maq RECORD
          v_ind            SMALLINT,
          v_diag           CHAR(3),
          v_sql_error      INTEGER,
          v_isam_error     INTEGER,
          v_msg_error      VARCHAR(100),
          v_estado_destino SMALLINT
       END RECORD

DEFINE r_error            BOOLEAN

# Descripción: Inicializa consultas SQL
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 seq_prt_solicitud_cedente.NEXTVAL",
                    "   FROM systables"
   PREPARE prp_rec_seq_sol FROM v_consulta           
   
   LET v_consulta = " INSERT INTO prt_solicitud_cedente",
                    " (id_prt_solicitud_cedente,",
                    "  nss,",
                    "  curp,",
                    "  id_credito_fovissste,",
                    "  f_ini_tramite,",
                    "  f_consulta_credito,",
                    "  tipo_portabilidad,",
                    "  n_caso,",
                    "  estado,",
                    "  diagnostico_interno)",
                    " VALUES(?,?,?,?,?,?,0,?,?,0)"
   PREPARE prp_genera_solicitud_cedente FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET id_credito_fovissste = ?,",
                    "        diagnostico_interno = ?,",
                    "        resultado_operacion = ?,",
                    "        saldo_insoluto_credito_fovissste = ?,",
                    "        nombre = ?,",
                    "        paterno = ?,",
                    "        materno = ?,",
                    "        n_caso = ?,",
                    "        tipo_portabilidad = ?,",
                    "        f_originacion_fovissste = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET diagnostico_interno = ?,",
                    "        resultado_operacion = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_solicitud_error FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET correo_e = ?,",
                    "        telefono = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_inf_contacto_sol FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET f_ini_tramite = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_f_tramite FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_prt_solicitud_cedente",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND estado <> 16 ",
                    "    AND (estado BETWEEN ? AND ? OR estado BETWEEN ? AND ? )" #Modificado a varios rangos de estados
   PREPARE prp_rec_existe_derechohabiente_prt FROM v_consulta

   LET v_consulta = " SELECT MAX(id_prt_solicitud_cedente)",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND ((estado = ? AND resultado_operacion = ?) OR estado BETWEEN ? AND ? OR estado BETWEEN ? AND ? )"
   PREPARE prp_rec_existe_derechohabiente_prt_portal FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_prt_solicitud_cedente,",
                    "        tipo_portabilidad",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND estado = ?",
                    "  ORDER BY id_prt_solicitud_cedente DESC"
   PREPARE prp_rec_busca_derechohabiente_prt FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_prt_solicitud_cedente,",
                    "        nombre,",
                    "        paterno,",
                    "        materno,",
                    "        tipo_portabilidad",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE n_caso = ?",
                    "    AND estado = ?"
   PREPARE prp_val_num_caso FROM v_consulta
   
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

   LET v_consulta = " SELECT FIRST 1 id_derechohabiente,",
                    "        nombre_af,",
                    "        ap_paterno_af,",
                    "        ap_materno_af",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_id_derechohabiente FROM v_consulta
   
   LET v_consulta = " SELECT SUM(monto_acciones),",
                    "        SUM(monto_pesos)",
                    "   FROM cta_movimiento",
                    "  WHERE id_derechohabiente = ?",
                    "    AND subcuenta = ?"
   PREPARE prp_rec_saldo_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT diagnostico_interno",
                    "   FROM prt_rch_marca_diagnostico",
                    "  WHERE marca_activa = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_rec_diag_marca FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_credito_vivienda(?,?)"
   PREPARE prp_valida_credito_vigente FROM v_consulta
   
   # parámetros:
   # 1 --> identificador de maquinaria
   # 2 --> identificador de registro de tabla de maquinaria
   # 3 --> señal a ejecutar
   # 4 --> usuario que ejecuta maquinaria 
   LET v_consulta = " EXECUTE FUNCTION fn_glo_maq_individual(?,?,?,?)"
   PREPARE prp_actualiza_maquinaria FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_prt_solicita_marca_cedente(?,?,?,?)"
   PREPARE prp_solicita_marca_cedente FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_glo_valida_marcaje(?,?)"
   PREPARE prp_valida_marca FROM v_consulta

   # Especifica 5 segundos como máximo para la espera de respuesta del servidor externo
   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 5 )

END FUNCTION

FUNCTION fn_determina_tipo_consulta()

   # Inicializa datos que se envian en respuestas para no conservar datos anteriores
   INITIALIZE r_respuesta_fovissste.* TO NULL
   INITIALIZE v_mensajeEntradaParaFovissste.* TO NULL
   INITIALIZE r_respuesta_cartera.* TO NULL
   
   CASE mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus

      WHEN C_ID_ESTATUS_1 # consulta del portal

         CALL fn_registra_solicitud_cedente() RETURNING r_error
                                               
         IF NOT(r_error)THEN
            # Valida datos en Infonavit(saci)
            # Parámetro 1 --> Valida solicitud del Portal
            CALL fn_valida_solicitud_portabilidad(1) RETURNING r_error
            IF NOT( r_error )THEN
               # Invocar servicio de fovissste
               CALL fn_consulta_credito_fovissste() RETURNING r_error
               IF NOT( r_error )THEN
                  # función para actulizar registros según información devuelta por fovissste
                  CALL fn_actualiza_solicitud(C_SENAL_ACEPTADA_VAL_PRESOL,
                                              C_SENAL_RECAZADA_VAL_PRESOL) # Sólo utiliza la señal de rechazo en dado caso
               END IF
            END IF
         END IF

      WHEN C_ID_ESTATUS_VALIDACION # Consulta de validación de portabilidad ADAI
         CALL fn_recupera_id_solicitud() RETURNING r_error
         
         IF NOT( r_error )THEN
            # Valida datos en Infonavit(saci)
            # Parámetro 2 --> valida solicitud de ADAI
            CALL fn_valida_solicitud_portabilidad(2) RETURNING r_error
            IF NOT( r_error )THEN
               
               # Invocar servicio de fovissste
               CALL fn_consulta_credito_fovissste() RETURNING r_error
               IF NOT( r_error )THEN           
                  
                  # función para actulizar registros según infromación devuelta por fovissste
                  CALL fn_actualiza_solicitud(C_SENAL_ACEPTADA_ING_SOL,
                                              C_SENAL_RECAZADA_VAL_PRESOL)
                  # Actualiza fecha de inicio de tramite
                  CALL fn_actualiza_f_inicio_tramite()
               END IF
            END IF
         END IF

      WHEN C_ID_ESTATUS_SOL_FIRMADA # Confirmación de ADAI para inicio de portabilidad
         CALL fn_valida_numero_caso() RETURNING r_error
         IF NOT ( r_error )THEN
            CALL fn_actualiza_sol_formalizada() RETURNING r_error
            IF NOT( r_error )THEN
               # Llamada a bus para enviar solicitud de portabilidad a procesar
               CALL fn_solicita_marca()
            END IF
         END IF

      OTHERWISE
         EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_7,
                                                 C_DESTINO_DIAG_ADA
                                            INTO v_diagostico_externo.*
         {CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_9,
                                           v_diagostico_externo.v_diag_ext,
                                           v_diagostico_externo.v_desc_gen)}
         CALL fn_asigna_msj_salida(--C_ID_ESTATUS_NO_PROCEDENTE, # resultado operación
                                   v_diagostico_externo.v_diag_ext)
         
   END CASE

END FUNCTION

#Objetivo: Registra la pre solicitud de portabilidad cedente
FUNCTION fn_registra_solicitud_cedente()

   LET v_f_actual = TODAY
   TRY 
      INITIALIZE v_id_prt_solicitud TO NULL
      # Recupera sequencia
      EXECUTE prp_rec_seq_sol INTO v_id_prt_solicitud
      EXECUTE prp_genera_solicitud_cedente USING v_id_prt_solicitud,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCredito,
                                                 v_f_actual, # f_ini_tramite
                                                 v_f_actual, # f_consulta_credito
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                                 C_ESTADO_INI_SOLICITUD_CEDENTE

   CATCH # Captura error sql
      DISPLAY "Error al insertar datos para:"
      DISPLAY "NSS: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
      DISPLAY "CURP: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
      DISPLAY "Crédito: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCredito
      DISPLAY "Código: ",SQLCA.SQLCODE
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)

      RETURN TRUE # Error en sql             
   END TRY
                                              
   RETURN FALSE # Ejecución realizada correctamente
END FUNCTION

#Objetivo: Funcion para validar que los datos clave sean correctos y estado de crédito
FUNCTION fn_valida_solicitud_portabilidad(p_verifica_existente)
DEFINE p_verifica_existente SMALLINT

   LET v_error = FALSE # sin error
   # VALIDA NSS
   IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss = ' ')THEN
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_2,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_2,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      LET v_error = TRUE
   ELSE
      # VALIDA CURP
      IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp = ' ')THEN
         EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_3,
                                                 C_DESTINO_DIAG_ADA
                                            INTO v_diagostico_externo.*

         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_3,
                                           v_diagostico_externo.v_diag_ext,
                                           v_diagostico_externo.v_desc_gen)
         LET v_error = TRUE 
      ELSE
         # VALIDA EXISTENCIA DERECHOHABIENTE
         TRY
            INITIALIZE v_id_derechohabiente TO NULL
            EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                                INTO v_id_derechohabiente,
                                                     v_mensajeEntradaParaFovissste.nombre,
                                                     v_mensajeEntradaParaFovissste.apPaterno,
                                                     v_mensajeEntradaParaFovissste.apMaterno
            IF(v_id_derechohabiente IS NOT NULL)THEN
               # Asigna datos para consultar en fovissste, que se confirmó que existe de manera local
               LET v_mensajeEntradaParaFovissste.nss       = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
               LET v_mensajeEntradaParaFovissste.curp      = mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
               LET v_mensajeEntradaParaFovissste.idCredito = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCredito

               IF(p_verifica_existente)THEN
                  CALL fn_valida_existencia_portabilidad(p_verifica_existente) RETURNING v_error
               ELSE
                  LET v_error = FALSE
               END IF
               
               
               IF NOT(v_error)THEN
                  #VALIDA CRÉDITO VIGENTE
                  #-->
                  EXECUTE prp_valida_credito_vigente USING v_id_derechohabiente,
                                                           C_CONSULTA_CREDITO_VIGENTE
                                                      INTO r_vigencia_credito.* # resultado --> 0 vigente, 1 liquidado o no vigente
                  IF( r_vigencia_credito.v_resultado <> 0 )THEN
                  #<--
                     # VALIDA MARCA DE PORTABILIDAD
                     INITIALIZE v_valida_marca.* TO NULL
                     EXECUTE prp_valida_marca USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                    C_MARCA_PRT_CEDENTE
                                               INTO v_error,
                                                    v_valida_marca.v_marca_error,
                                                    v_valida_marca.v_mensaje_marca
                     IF NOT( v_error )THEN
                     #NO SE INCLUYE VALIDACIÓN POR EL MOMENTO-->
                     # false credito no vigente, true crédito vigente o no se puede consultar
                     {CALL fn_consulta_credito_cartera() RETURNING v_error
                     IF NOT( v_error )THEN}
                     #NO SE INCLUYE VALIDACIÓN POR EL MOMENTO<--
                        # VALIDA SALDO EN CUENTA
                        #-->
                        INITIALIZE v_saldo_derechohabiente.* TO NULL
                        EXECUTE prp_rec_saldo_derechohabiente USING v_id_derechohabiente,
                                                                    C_SUBCUENTA_VIVIENDA_97
                                                               INTO v_saldo_derechohabiente.v_acciones,
                                                                    v_saldo_derechohabiente.v_pesos
                        # saldo debe ser mayor a cero para solicitar los datos a FOVISSSTE
                        IF(v_saldo_derechohabiente.v_acciones <= 0 OR v_saldo_derechohabiente.v_acciones IS NULL)THEN
                           EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_6,
                                                                   C_DESTINO_DIAG_ADA
                                                              INTO v_diagostico_externo.*

                           CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_6,
                                                             v_diagostico_externo.v_diag_ext,
                                                             v_diagostico_externo.v_desc_gen)
                           LET v_error = TRUE
                        END IF
                        #<--
                     #NO SE INCLUYE VALIDACIÓN POR EL MOMENTO-->
                     {ELSE
                        EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_24,
                                                                C_DESTINO_DIAG_C_A
                                                           INTO v_diagostico_externo.*
                        CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_24,
                                                          v_diagostico_externo.v_diag_ext,
                                                          v_diagostico_externo.v_desc_gen)
                     END IF}
                     #NO SE INCLUYE VALIDACIÓN POR EL MOMENTO<--
                     ELSE
                        INITIALIZE v_diagnostico_interno_marca TO NULL
                        # Según la marca unificación o portabilidad se genera el motivo de rechazo
                        EXECUTE prp_rec_diag_marca USING v_valida_marca.v_marca_error,
                                                         C_DESTINO_DIAG_ADA
                                                    INTO v_diagnostico_interno_marca
                        IF( v_diagnostico_interno_marca IS NOT NULL )THEN                           
                           EXECUTE prp_consulta_diag_externo USING v_diagnostico_interno_marca,
                                                                   C_DESTINO_DIAG_ADA
                                                              INTO v_diagostico_externo.*
                           CALL fn_asigna_msj_erroneo_salida(v_diagnostico_interno_marca,
                                                             v_diagostico_externo.v_diag_ext, 
                                                             v_diagostico_externo.v_desc_gen)
                        ELSE # cualquier marca no registrada, se considera diagnóstico para retiros
                           EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_20,
                                                                   C_DESTINO_DIAG_ADA
                                                              INTO v_diagostico_externo.*

                           CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_20,
                                                             v_diagostico_externo.v_diag_ext,
                                                             v_diagostico_externo.v_desc_gen)
                        END IF
                        LET v_error = TRUE
                     END IF
                  #-->
                  ELSE
                     EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_5,
                                                             C_DESTINO_DIAG_ADA
                                                        INTO v_diagostico_externo.*

                     CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_5,
                                                       v_diagostico_externo.v_diag_ext,
                                                       v_diagostico_externo.v_desc_gen)            
                     LET v_error = TRUE
                  END IF
                  #<--
               END IF
            ELSE
               EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_4,
                                                       C_DESTINO_DIAG_ADA
                                                  INTO v_diagostico_externo.*

               CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_4,
                                                 v_diagostico_externo.v_diag_ext,
                                                 v_diagostico_externo.v_desc_gen)            
               LET v_error = TRUE            
            END IF
         CATCH
            DISPLAY "Error en estructura"
            DISPLAY "NSS:",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
            DISPLAY "Código:",SQLCA.SQLCODE
            DISPLAY "Mensaje:",SQLCA.sqlerrm
            # Error de estructura de datos
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_7,
                                                    C_DESTINO_DIAG_ADA
                                               INTO v_diagostico_externo.*

            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_7,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)
            LET v_error = TRUE
         END TRY         
      END IF      
   END IF

   RETURN v_error
END FUNCTION

FUNCTION fn_asigna_error_marca()

END FUNCTION

# Descripción: Función para validar la existencia del nss en portabilidad cedente
FUNCTION fn_valida_existencia_portabilidad(p_entidad_consulta)
DEFINE p_entidad_consulta SMALLINT

   # VALIDA SI YA EXISTE EL NSS EN PORTABILIDAD
   INITIALIZE v_id_prt_solicitud_tmp TO NULL
   {CASE p_entidad_consulta
      WHEN 1 # Portal 
         EXECUTE prp_rec_existe_derechohabiente_prt_portal USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                                 C_ESTADO_ACEPTADA_FOVISSSTE_CEDENTE,
                                                                 C_RESULTADO_OP_ACEPTADA_FOV,
                                                                 C_ESTADO_ACEPTADA_FOVISSSTE_CEDENTE,
                                                                 C_ESTADO_MARCA_SOL_PRO,
                                                                 C_ESTADO_MARCADA_PRO,
                                                                 C_ESTADO_SDO_LIQUIDADO_CED
                                                            INTO v_id_prt_solicitud_tmp
      WHEN 2 # ADAI
         EXECUTE prp_rec_existe_derechohabiente_prt USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                          C_ESTADO_SOLICITADA_CESI_CEDENTE,
                                                          C_ESTADO_MARCA_SOL_PRO,
                                                          C_ESTADO_MARCADA_PRO,
                                                          C_ESTADO_SDO_LIQUIDADO_CED
                                                     INTO v_id_prt_solicitud_tmp

   END CASE}
   EXECUTE prp_rec_existe_derechohabiente_prt USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                    C_ESTADO_SOLICITADA_CESI_CEDENTE,
                                                    C_ESTADO_MARCA_SOL_PRO,
                                                    C_ESTADO_MARCADA_PRO,
                                                    C_ESTADO_SDO_LIQUIDADO_CED
                                               INTO v_id_prt_solicitud_tmp
   IF(v_id_prt_solicitud_tmp IS NULL)THEN
      RETURN FALSE
   ELSE
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_12,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_12,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      RETURN TRUE
   END IF

END FUNCTION

# Descripción: Recupera y valida existencia de solicitud previa
FUNCTION fn_recupera_id_solicitud()

   INITIALIZE v_id_prt_solicitud TO NULL
   
   EXECUTE prp_rec_busca_derechohabiente_prt USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                   C_ESTADO_ACEPTADA_FOVISSSTE_CEDENTE
                                              INTO v_id_prt_solicitud,
                                                   r_respuesta_fovissste.tpoCredito # se recupera sólo para devolver en mensaje de salida
   IF(v_id_prt_solicitud IS NULL)THEN
      # Al no encontrar la solicitud, regresa mensaje con diagnóstico no existe nss
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_4,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_salida(--C_ID_ESTATUS_NO_PROCEDENTE,
                                v_diagostico_externo.v_diag_ext)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

# Descripción: Función para validar que el número de caso exista en portabilidad cedente
FUNCTION fn_valida_numero_caso()

   INITIALIZE v_id_prt_solicitud TO NULL
   EXECUTE prp_val_num_caso USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                  C_ESTADO_SOLICITADA_CESI_CEDENTE
                             INTO v_id_prt_solicitud,
                                  v_mensajeEntradaParaFovissste.nombre,
                                  v_mensajeEntradaParaFovissste.apPaterno,
                                  v_mensajeEntradaParaFovissste.apMaterno,
                                  r_respuesta_fovissste.tpoCredito # sólo se recupera para enviarlo como mensaje de salida
                                  
   IF(v_id_prt_solicitud IS NULL)THEN # si el número de caso recibido no existe devuelve mensaje rechazado
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_15,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      # Asigna mensaje de respuesta a portal
      CALL fn_asigna_msj_salida(--C_ID_ESTATUS_NO_PROCEDENTE,
                                v_diagostico_externo.v_diag_ext)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
   

END FUNCTION

#Objetivo: Función para asignar los valores al mensaje de salida
FUNCTION fn_asigna_msj_salida(--p_resultado_operacion,
                              p_id_motivo)
DEFINE p_resultado_operacion LIKE prt_diagnostico.diagnostico_externo,
       p_id_motivo           LIKE prt_diagnostico.diagnostico_externo

   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.numeroCaso       = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.nss              = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.apPaterno        = v_mensajeEntradaParaFovissste.apPaterno
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.apMaterno        = v_mensajeEntradaParaFovissste.apMaterno
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.nombre           = v_mensajeEntradaParaFovissste.nombre
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.tipoPortabilidad = r_respuesta_fovissste.tpoCredito
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.idEstatus        = mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.diagnostico      = p_id_motivo

--   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.resultadoOperacion = p_resultado_operacion
--   LET mensajeSalidaSolicitud.solicitaCreditoFovisssteReturn.idMotivo           = p_id_motivo

END FUNCTION

#Objetivo: Función para asignar mensaje erroneo de salida
FUNCTION fn_asigna_msj_erroneo_salida(p_diag_interno,
                                      p_diag_externo,
                                      p_des_diagnostico)
DEFINE p_diag_interno    LIKE prt_diagnostico.diagnostico_interno,
       p_diag_externo    LIKE prt_diagnostico.diagnostico_externo,
       p_des_diagnostico LIKE prt_diagnostico.descripcion_general

   TRY
   
      # Actuzliza registros de solicitud rechazada
      EXECUTE prp_actualiza_solicitud_error USING p_diag_interno,
                                                  C_RESULTADO_OP_RECHAZADA_FOV,
                                                  v_id_prt_solicitud
      {EXECUTE prp_actualiza_solicitud USING r_respuesta_fovissste.idCredito,
                                            p_diag_interno,
                                            C_RESULTADO_OP_RECHAZADA_FOV,
                                            r_respuesta_fovissste.saldoInsolutoCredito,
                                            v_mensajeEntradaParaFovissste.nombre,
                                            v_mensajeEntradaParaFovissste.apPaterno,
                                            v_mensajeEntradaParaFovissste.apMaterno,
                                            mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                            r_respuesta_fovissste.tpoCredito,
                                            v_id_prt_solicitud}
   CATCH
      DISPLAY "Error al actualizar datos:"
      DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
      DISPLAY "Código: ",SQLCA.SQLCODE
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*
      LET p_diag_externo = v_diagostico_externo.v_diag_ext
   END TRY

   CALL fn_asigna_msj_salida(--C_ID_ESTATUS_NO_PROCEDENTE, # resultado operación
                             p_diag_externo)

END FUNCTION

#Objetivo: Invoca función de WS consulta de crédito FOVISSSTE (PRTW01)
FUNCTION fn_consulta_credito_fovissste()
   
   # Asigna secuencia como id del folio hacia fovissste
   LET v_mensajeEntradaParaFovissste.folioConsulta = v_id_prt_solicitud

   INITIALIZE r_respuesta_fovissste.* TO NULL
   # Invocar el servicio de fovissste
   CALL consultaCreditoFovissste(v_mensajeEntradaParaFovissste.nss,
                                 v_mensajeEntradaParaFovissste.curp,
                                 v_mensajeEntradaParaFovissste.idCredito,
                                 v_mensajeEntradaParaFovissste.folioConsulta                                 
                                 ) RETURNING r_resultado_invoca_ws,
                                             r_respuesta_fovissste.nss,
                                             r_respuesta_fovissste.curp,
                                             r_respuesta_fovissste.fechaOtorgamientoCad,
                                             r_respuesta_fovissste.idCredito,
                                             r_respuesta_fovissste.folioConsulta,
                                             r_respuesta_fovissste.folioRespuesta,
                                             r_respuesta_fovissste.saldoInsolutoCredito,
                                             r_respuesta_fovissste.tpoCredito,
                                             r_respuesta_fovissste.diagnostico,
                                             r_respuesta_fovissste.idMotivo
# Pruebas aceptadas
{LET r_resultado_invoca_ws = 0
LET r_respuesta_fovissste.nss = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
LET r_respuesta_fovissste.curp = mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
LET r_respuesta_fovissste.fechaOtorgamientoCad = "20010807"
LET r_respuesta_fovissste.idCredito = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCredito
LET r_respuesta_fovissste.folioConsulta = v_id_prt_solicitud
LET r_respuesta_fovissste.folioRespuesta = 1
LET r_respuesta_fovissste.saldoInsolutoCredito = "58320"
LET r_respuesta_fovissste.tpoCredito = "1"
LET r_respuesta_fovissste.diagnostico = 1
LET r_respuesta_fovissste.idMotivo = "1000"}

   IF(r_resultado_invoca_ws <> 0)THEN
      DISPLAY "Error de comunicación con WS FOVISSSTE:"
      DISPLAY "Estado:",r_resultado_invoca_ws
            
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_9,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_9,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      LET v_error = TRUE
   ELSE
      TRY
         LET r_respuesta_fovissste.fechaOtorgamiento = DATE(r_respuesta_fovissste.fechaOtorgamientoCad[5,6]||"/"||
                                                            r_respuesta_fovissste.fechaOtorgamientoCad[7,8]||"/"||
                                                            r_respuesta_fovissste.fechaOtorgamientoCad[1,4])
      CATCH
         DISPLAY "Error en fecha otorgamiento para solicitud: ",v_id_prt_solicitud
         DISPLAY "Fecha recuperada: ",r_respuesta_fovissste.fechaOtorgamientoCad
      END TRY
      LET v_error = FALSE
   END IF
   
   RETURN v_error
END FUNCTION

#Objetivo: Función para actualizar el estado de la solicitud cedente
FUNCTION fn_actualiza_solicitud(p_senal_aceptada,
                                p_senal_rechazo)
DEFINE p_avanza_maquinaria BOOLEAN,
       p_senal_aceptada    SMALLINT,
       p_senal_rechazo     SMALLINT

   # El registro de salida ya debe haber recuperado el reultado de la operación
   CASE r_respuesta_fovissste.diagnostico
      WHEN C_RESULTADO_OP_ACEPTADA_FOV
         TRY
            # Actualiza registros de solicitud aceptada
            EXECUTE prp_actualiza_solicitud USING r_respuesta_fovissste.idCredito,
                                                  C_MOTIVO_DIAG_PROCEDENTE,
                                                  C_RESULTADO_OP_ACEPTADA_FOV,
                                                  r_respuesta_fovissste.saldoInsolutoCredito,
                                                  v_mensajeEntradaParaFovissste.nombre,    # Consultados en safre en la validación del nss
                                                  v_mensajeEntradaParaFovissste.apPaterno, # Consultados en safre en la validación del nss
                                                  v_mensajeEntradaParaFovissste.apMaterno, # Consultados en safre en la validación del nss
                                                  mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                                  r_respuesta_fovissste.tpoCredito,
                                                  r_respuesta_fovissste.fechaOtorgamiento,
                                                  v_id_prt_solicitud

            # Avanza maquinaria según la señal
            EXECUTE prp_actualiza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                                   v_id_prt_solicitud,
                                                   p_senal_aceptada,
                                                   g_usuario_cod
                                              INTO v_resultado_maq.*

            IF(v_resultado_maq.v_sql_error = 0 AND v_resultado_maq.v_diag = 0)THEN
            
               EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_0,
                                                       C_DESTINO_DIAG_ADA
                                                  INTO v_diagostico_externo.*
                                               
               # Asigna mensaje de respuesta a portal
               CALL fn_asigna_msj_salida(--C_ID_ESTATUS_PROCEDENTE,
                                         v_diagostico_externo.v_diag_ext)
            ELSE
               DISPLAY "Error al actualizar maquinaria aceptada:"
               DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
               DISPLAY "Código:",v_resultado_maq.v_sql_error
               DISPLAY "Mensaje:",v_resultado_maq.v_msg_error
               DISPLAY "ind:",v_resultado_maq.v_ind
               EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                       C_DESTINO_DIAG_ADA
                                                  INTO v_diagostico_externo.*

               CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_8,
                                                 v_diagostico_externo.v_diag_ext,
                                                 v_diagostico_externo.v_desc_gen)               
            END IF

         CATCH
            DISPLAY "Error al actualizar datos aceptados:"
            DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                    C_DESTINO_DIAG_ADA
                                               INTO v_diagostico_externo.*

            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)

         END TRY

      WHEN C_RESULTADO_OP_RECHAZADA_FOV
         TRY
            # Actualiza estado a rechazado
            EXECUTE prp_actualiza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                                   v_id_prt_solicitud,
                                                   p_senal_rechazo,
                                                   g_usuario_cod
                                              INTO v_resultado_maq.*

            IF(v_resultado_maq.v_sql_error = 0 AND v_resultado_maq.v_diag = 0)THEN                                  
               # Consulta diagnostico interno para actualizar solicitud
               EXECUTE prp_consulta_diag_interno USING r_respuesta_fovissste.idMotivo, # Diagnóstico de fovissste
                                                       C_DESTINO_DIAG_F_I
                                                  INTO v_diagnostico_interno.*
               # Recupera diagnostico a envíar a adai
               EXECUTE prp_consulta_diag_externo USING r_respuesta_fovissste.idMotivo,
                                                       C_DESTINO_DIAG_F_A
                                                  INTO v_diagostico_externo.*
                                                              
               CALL fn_asigna_msj_erroneo_salida(v_diagnostico_interno.v_diag_int,
                                                 v_diagostico_externo.v_diag_ext,
                                                 v_diagostico_externo.v_desc_gen)
            ELSE
               DISPLAY "Error al actualizar maquinaria rechazada:"
               DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
               DISPLAY "Código:",v_resultado_maq.v_sql_error
               DISPLAY "Mensaje:",v_resultado_maq.v_msg_error
               DISPLAY "ind:",v_resultado_maq.v_ind
               EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                       C_DESTINO_DIAG_ADA
                                                  INTO v_diagostico_externo.*

               CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_8,
                                                 v_diagostico_externo.v_diag_ext,
                                                 v_diagostico_externo.v_desc_gen) 
            END IF
         CATCH
            DISPLAY "Error al actualizar datos rechazados:"
            DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                    C_DESTINO_DIAG_ADA
                                               INTO v_diagostico_externo.*

            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)
         END TRY
         
   END CASE

END FUNCTION

# Descripción: Actualiza la fecha de inicio de tramite portabilidad
FUNCTION fn_actualiza_f_inicio_tramite()
DEFINE v_fecha_actual DATE 

   LET v_fecha_actual = TODAY
   EXECUTE prp_actualiza_f_tramite USING v_fecha_actual,
                                         v_id_prt_solicitud

END FUNCTION

# Llamada al WS de cartera para verificar que el crédito NO esté vigente
# toma diag_cartera = "01" y "02" como valor para decir que el crédito no está vigente
FUNCTION fn_consulta_credito_cartera()

   LET v_error = TRUE
   INITIALIZE r_respuesta_cartera.* TO NULL
   
   # Invocar servicio de cartera
   CALL consultaCreditoCartera(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                               r_vigencia_credito.v_num_credito # número de crédito recuperado de la consulta de vigencia del crédito, y crédito infonavit para revisar
                               --mensajeEntradaSolicitud.idCredito                               
                               ) RETURNING r_resultado_invoca_ws,
                                           r_respuesta_cartera.v_nss,
                                           r_respuesta_cartera.v_num_credito,
                                           r_respuesta_cartera.v_diag_cartera,
                                           r_respuesta_cartera.v_tpo_credito,
                                           r_respuesta_cartera.v_saldo_credito,
                                           r_respuesta_cartera.v_monto_originacion,
                                           r_respuesta_cartera.v_f_originacion,
                                           r_respuesta_cartera.v_causa_diag

   IF(r_resultado_invoca_ws <> 0)THEN
      DISPLAY "Error de comunicación con WS CARTERA:"
      DISPLAY "Estado:",r_resultado_invoca_ws
      LET v_error = TRUE
   ELSE
      # 00 crédito vigente, 01 o 02 probable crédito no vigente, 05 no se considera sin crédito por no tener acceso a BD
      CASE r_respuesta_cartera.v_diag_cartera
         WHEN C_CART_CRED_VIGENTE # con crédito
            LET v_error = TRUE

         WHEN C_CART_CRED_INEXISTENTE # Crédito no existente
            LET v_error = FALSE
            
         WHEN C_CART_CRED_LIQUIDADO # Crédito liquidado
            LET v_error = FALSE
            
         WHEN C_CART_SIN_ACCESO_BD # sin acceso a BD
            LET v_error = TRUE
            
         OTHERWISE
            LET v_error = TRUE

      END CASE
   END IF
   
   RETURN v_error
END FUNCTION

# Descripción: Actualiza datos personales del nss
FUNCTION fn_actualiza_sol_formalizada()

   # Actualiza los datos que envia ADAI
   EXECUTE prp_actualiza_inf_contacto_sol USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.correoElectronico,
                                                mensajeEntradaSolicitud.mensajeEntradaSolicitud.telCelular,
                                                v_id_prt_solicitud

   # Avanza maquinaria según la señal
   EXECUTE prp_actualiza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                          v_id_prt_solicitud,
                                          C_SENAL_RECIBIR_CONFIRMACION,
                                          g_usuario_cod
                                     INTO v_resultado_maq.*

   IF(v_resultado_maq.v_sql_error = 0 AND v_resultado_maq.v_diag = 0)THEN
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_0,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*
                                               
      # Asigna mensaje de respuesta a portal
      CALL fn_asigna_msj_salida(--C_ID_ESTATUS_PROCEDENTE,
                                v_diagostico_externo.v_diag_ext)
      RETURN FALSE
   ELSE
      DISPLAY "Error al actualizar maquinaria aceptada:"
      DISPLAY "Id solicitud cedente:",v_id_prt_solicitud
      DISPLAY "Código:",v_resultado_maq.v_sql_error
      DISPLAY "Mensaje:",v_resultado_maq.v_msg_error
      DISPLAY "ind:",v_resultado_maq.v_ind
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_8,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_8,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      RETURN TRUE # ERROR               
   END IF
            
END FUNCTION

# Descripción: Invoca WS bus para solicitar marca a procesar
FUNCTION fn_solicita_marca()
DEFINE r_error_sql  INTEGER,
       r_error_isam INTEGER,
       r_msg_sql    CHAR(254)

   WHENEVER ERROR CONTINUE
   
   EXECUTE prp_solicita_marca_cedente USING v_id_prt_solicitud,
                                            C_BUS_PROCESO_COD_SOL_MARCA,
                                            C_BUS_OPERACION_COD_SOL_MARCA,
                                            v_id_prt_solicitud
                                       INTO r_error_sql,
                                            r_error_isam,
                                            r_msg_sql
   IF(r_error_sql <> 0)THEN
      DISPLAY "Error solicitar marca:"
      DISPLAY "ID prt solicitud: ",v_id_prt_solicitud
      DISPLAY "Código: ",r_error_sql
      DISPLAY "Mensaje: ",r_msg_sql
      
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
   ELSE

      # Si envió solicitud de marca correctamente avanza maquinaria de solicitud cedente
      EXECUTE prp_actualiza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                             v_id_prt_solicitud,
                                             C_SENAL_SOLICITA_MARCA,
                                             g_usuario_cod
                                        INTO v_resultado_maq.*
                                        
      IF( v_resultado_maq.v_sql_error <> 0 OR v_resultado_maq.v_ind <> 0 )THEN
         DISPLAY "Error maquinaria solicita marca:"
         DISPLAY "ID prt solicitud: ",v_id_prt_solicitud
         DISPLAY "Código: ",v_resultado_maq.v_sql_error
         DISPLAY "Mensaje: ",v_resultado_maq.v_msg_error
         DISPLAY "ind: ",v_resultado_maq.v_ind
         
         EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_8,
                                                 C_DESTINO_DIAG_ADA
                                            INTO v_diagostico_externo.*

         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_8,
                                           v_diagostico_externo.v_diag_ext,
                                           v_diagostico_externo.v_desc_gen)
      ELSE
         EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_18,
                                                 C_DESTINO_DIAG_ADA
                                            INTO v_diagostico_externo.*
                                               
         # Asigna mensaje de respuesta a portal
         CALL fn_asigna_msj_salida(--C_ID_ESTATUS_PROCEDENTE,
                                   v_diagostico_externo.v_diag_ext)
      END IF
   END IF
END FUNCTION