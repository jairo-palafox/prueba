--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS02                                                       #
#Objetivo     => Funciones de solicitud de portabilidad cedente                #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "PRTWS06.inc"
GLOBALS "PRTWS07.inc"
GLOBALS "PRTG01.4gl"

PRIVATE
DEFINE v_error BOOLEAN,
       v_id_prt_solicitud_receptora     DECIMAL(9,0),
       v_tmp_id_prt_solicitud_receptora DECIMAL(9,0),
       r_resultado_invoca_ws   INTEGER,
       g_fecha_tmp             DATE, # Fecha originación del crédito en formato mm/dd/aaaa
       g_tipo_portabilidad     SMALLINT,
       g_ind_elegibilidad_cred SMALLINT,
       v_valida_marca RECORD
          v_marca_error   SMALLINT,
          v_mensaje_marca VARCHAR(254)
       END RECORD,
       v_diagnostico_interno_marca LIKE prt_diagnostico.diagnostico_interno
       
              
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
DEFINE r_vigencia_credito RECORD
          v_resultado        SMALLINT,
          v_tipo_originacion SMALLINT,
          v_tpo_credito      SMALLINT,
          v_num_credito      DECIMAL(10,0),
          v_f_otorga         DATE,
          v_f_liquida        DATE
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

# Descripción: función para inicializar las consultas
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 seq_prt_solicitud_receptora.NEXTVAL",
                    "   FROM systables"
   PREPARE prp_rec_seq_sol FROM v_consulta
   
   LET v_consulta = " INSERT INTO prt_solicitud_receptora",
                    " (id_prt_solicitud_receptora,",
                    "  folio_fovissste,",
                    "  nss,",
                    "  curp,",
                    "  f_consulta_credito,",
                    "  id_credito_infonavit,",
                    "  estado,",
                    "  diagnostico_interno)",
                    " VALUES(?,?,?,?,?,?,?,0)"
   PREPARE prp_genera_solicitud_receptora FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_receptora",
                    "    SET id_credito_infonavit = ?,",
                    "        diagnostico_interno = ?,",
                    "        resultado_operacion = ?,",
                    "        saldo_insoluto_credito_infonavit = ?,",
                    "        mto_originacion_credito = ?,",
                    "        f_originacion_infonavit = ?,",
                    "        tipo_portabilidad = ?,",
                    "        nombre = ?,",
                    "        paterno = ?,",
                    "        materno = ?",
                    "  WHERE id_prt_solicitud_receptora = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_prt_solicitud_receptora",
                    "   FROM prt_solicitud_receptora",
                    "  WHERE nss = ?",
                    "    AND estado > ?"
   PREPARE prp_rec_busca_derechohabiente_prt FROM v_consulta

   LET v_consulta = " SELECT diagnostico_externo,",
                    "        descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_interno = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_externo FROM v_consulta
   
   LET v_consulta = " SELECT diagnostico_interno,",
                    "        descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_externo = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_interno FROM v_consulta

   LET v_consulta = " SELECT id_derechohabiente,",
                    "        nombre_af,",
                    "        ap_paterno_af,",
                    "        ap_materno_af",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_id_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 ind_elegibilidad",
                    "   FROM prt_valida_tipo_credito",
                    "  WHERE tpo_credito = ?",
                    "    AND tpo_originacion = ?"
   PREPARE prp_valida_credito FROM v_consulta

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
   PREPARE prp_avanza_maquinaria FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_glo_valida_marcaje(?,?)"
   PREPARE prp_valida_marca FROM v_consulta

   # Especifica 5 segundos como máximo para la espera de respuesta del servidor externo
   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 5 )

END FUNCTION

# Descripción: inicializa el envío de crédito a fovissste
FUNCTION fn_inicia_solicitud()

   CALL fn_registra_solicitud_cedente() RETURNING v_error
   IF(v_error = 0)THEN
      CALL fn_valida_solicitud_portabilidad() RETURNING v_error
      IF(v_error = 0)THEN 
         # Invocar sp para ejecutar safre bus y a su vez el servicio de fovissste
         CALL fn_consulta_credito_cartera() RETURNING v_error
         IF NOT( v_error )THEN                                                   
            # función para actulizar registros según infromación devuelta por fovissste
            CALL fn_actualiza_solicitud()
         END IF
      END IF
   END IF

END FUNCTION

#Objetivo: Registra la pre solicitud de portabilidad receptora
FUNCTION fn_registra_solicitud_cedente()
DEFINE v_fecha_actual DATE

   TRY
      INITIALIZE v_id_prt_solicitud_receptora,
                 g_tipo_portabilidad, 
                 g_fecha_tmp,
                 mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.* TO NULL
      LET v_fecha_actual = TODAY
      # Recupera sequencia
      EXECUTE prp_rec_seq_sol INTO v_id_prt_solicitud_receptora
      EXECUTE prp_genera_solicitud_receptora USING v_id_prt_solicitud_receptora,
                                                   mensajeEntradaSolicitud.mensajeEntradaSolicitud.folioConsulta,
                                                   mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                   mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp,
                                                   v_fecha_actual, # f_consulta_credito
                                                   mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroDeCredito,
                                                   C_ESTADO_INI_SOLICITUD_RECEPTORA
                                                   
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.folioRespuesta = v_id_prt_solicitud_receptora

      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.nss             = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.curp            = mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroDeCredito
   CATCH # Captura error sql
      DISPLAY "Error al insertar datos para:"
      DISPLAY "Folio transacción: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.folioConsulta
      DISPLAY "NSS: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
      DISPLAY "CURP: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
      DISPLAY "Código: ",SQLCA.SQLCODE
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                              C_DESTINO_DIAG_FOV
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)

      RETURN 1 # Error en sql
             
   END TRY
                                              
   RETURN 0 # Ejecución realizada correctamente
          
END FUNCTION

#Objetivo: Función para validar que los datos clave sean correctos y el estado de crédito
FUNCTION fn_valida_solicitud_portabilidad()

   LET v_error = FALSE # sin error
   # VALIDA NSS
   IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss = ' ')THEN
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_2,
                                              C_DESTINO_DIAG_FOV
                                         INTO v_diagostico_externo.*

      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_2,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      LET v_error = TRUE
   ELSE
      # VALIDA CURP
      IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp = ' ')THEN
         EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_3,
                                                 C_DESTINO_DIAG_FOV
                                            INTO v_diagostico_externo.*

         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_3,
                                           v_diagostico_externo.v_diag_ext,
                                           v_diagostico_externo.v_desc_gen)
         LET v_error = TRUE 
      ELSE
         # VALIDA EXISTENCIA DERECHOHABIENTE
         TRY
            INITIALIZE v_derechohabiente.* TO NULL
            EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                                INTO v_derechohabiente.v_id_derechohabiente,
                                                     v_derechohabiente.v_nombre,
                                                     v_derechohabiente.v_apPaterno,
                                                     v_derechohabiente.v_apMaterno
            IF(v_derechohabiente.v_id_derechohabiente IS NOT NULL)THEN
               # VALIDA EXISTENCIA DE NSS EN PORTABILIDAD
               {INITIALIZE v_tmp_id_prt_solicitud_receptora TO NULL
               EXECUTE prp_rec_busca_derechohabiente_prt USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                               C_ESTADO_CONSULTADA
                                                          INTO v_tmp_id_prt_solicitud_receptora
               IF(v_tmp_id_prt_solicitud_receptora IS NULL)THEN}               
                  #VALIDA CRÉDITO VIGENTE
                  INITIALIZE r_vigencia_credito.* TO NULL
                  EXECUTE prp_valida_credito_vigente USING v_derechohabiente.v_id_derechohabiente,
                                                           C_CONSULTA_CREDITO_VIGENTE # 0 vigencia crédito, 1 crédito liquidado
                                                      INTO r_vigencia_credito.* # resultado --> 0 vigente, 1 liquidado 
                  IF( r_vigencia_credito.v_resultado = 0)THEN
                     # VALIDA ELEGIBILIDAD DEL CRÉDITO
                     INITIALIZE g_ind_elegibilidad_cred TO NULL
                     EXECUTE prp_valida_credito USING r_vigencia_credito.v_tpo_credito,
                                                      r_vigencia_credito.v_tipo_originacion
                                                 INTO g_ind_elegibilidad_cred
                     IF( g_ind_elegibilidad_cred = C_IND_CREDITO_ELEGIBLE )THEN
                        # VALIDA MARCA DE PORTABILIDAD
                        INITIALIZE v_valida_marca.* TO NULL
                        EXECUTE prp_valida_marca USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                       C_MARCA_PRT_RECEPTORA
                                                  INTO v_error,
                                                       v_valida_marca.v_marca_error,
                                                       v_valida_marca.v_mensaje_marca

                        IF( v_error )THEN
                           INITIALIZE v_diagnostico_interno_marca TO NULL
                           # Según la marca unificación o portabilidad se genera el motivo de rechazo
                           EXECUTE prp_rec_diag_marca USING v_valida_marca.v_marca_error,
                                                            C_DESTINO_DIAG_FOV
                                                       INTO v_diagnostico_interno_marca
                           IF( v_diagnostico_interno_marca IS NOT NULL )THEN                           
                              EXECUTE prp_consulta_diag_externo USING v_diagnostico_interno_marca,
                                                                      C_DESTINO_DIAG_FOV
                                                                 INTO v_diagostico_externo.*
                              CALL fn_asigna_msj_erroneo_salida(v_diagnostico_interno_marca, 
                                                                v_diagostico_externo.v_diag_ext, 
                                                                v_diagostico_externo.v_desc_gen)
                           ELSE # cualquier marca no registrada, se considera diagnóstico para retiros
                              EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_20,
                                                                      C_DESTINO_DIAG_FOV
                                                                INTO v_diagostico_externo.*

                              CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_20,
                                                                v_diagostico_externo.v_diag_ext,
                                                                v_diagostico_externo.v_desc_gen)
                           END IF
                           LET v_error = TRUE
                        END IF
                     ELSE
                        EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_23,
                                                                C_DESTINO_DIAG_FOV
                                                           INTO v_diagostico_externo.*
                        CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_23, v_diagostico_externo.v_diag_ext, v_diagostico_externo.v_desc_gen)
                        LET v_error = TRUE
                     END IF
                  ELSE
                     # si el crédito no está vigente establece error por crédito no vigente                  
                     EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_10,
                                                             C_DESTINO_DIAG_FOV
                                                        INTO v_diagostico_externo.*

                     CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_10,
                                                       v_diagostico_externo.v_diag_ext,
                                                       v_diagostico_externo.v_desc_gen)            
                     LET v_error = TRUE
                  END IF
               {ELSE
                  EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_12,
                                                          C_DESTINO_DIAG_FOV
                                                     INTO v_diagostico_externo.*

                  CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_12,
                                                    v_diagostico_externo.v_diag_ext,
                                                    v_diagostico_externo.v_desc_gen)            
                  LET v_error = TRUE
               END IF}
            ELSE
               EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_4,
                                                       C_DESTINO_DIAG_FOV
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
                                                    C_DESTINO_DIAG_FOV
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

#Objetivo: Función para asignar los valores al mensaje de salida
FUNCTION fn_asigna_msj_salida(p_resultado_operacion,
                              p_id_motivo,
                              p_descripcion)
DEFINE p_resultado_operacion LIKE prt_solicitud_receptora.resultado_operacion,
       p_id_motivo           LIKE prt_diagnostico.diagnostico_externo,
       p_descripcion         LIKE prt_diagnostico.descripcion_general

   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.nss                   = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.curp                  = mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito       = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroDeCredito
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.folioConsulta         = mensajeEntradaSolicitud.mensajeEntradaSolicitud.folioConsulta
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.indicadorTipoCredito  = g_tipo_portabilidad
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.folioRespuesta        = v_id_prt_solicitud_receptora
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = p_resultado_operacion
   LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.motivoRechazo         = p_id_motivo

END FUNCTION

#Objetivo: Función para asignar mensaje erroneo de salida
FUNCTION fn_asigna_msj_erroneo_salida(p_diag_interno,
                                      p_diag_externo,
                                      p_des_diagnostico)
DEFINE p_diag_interno    LIKE prt_diagnostico.diagnostico_interno,
       p_diag_externo    LIKE prt_diagnostico.diagnostico_externo,
       p_des_diagnostico LIKE prt_diagnostico.descripcion_general

   TRY
      # Establece datos por defecto si los datos son nulos
      IF( mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.saldoInsoluto IS NULL )THEN
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.saldoInsoluto = 0.0
      END IF
      IF( g_tipo_portabilidad IS NULL )THEN 
         # POR PETICIÓN INFONAVIT TODOS SON NUEVO CRÉDITO
         LET g_tipo_portabilidad = C_CREDITO_NUEVO
         --LET g_tipo_portabilidad = C_SIN_TIPO_PRT
      END IF      
      IF( mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento IS NULL)THEN
         LET g_fecha_tmp = C_FECHA_DEFECTO_ORIGINACION_CRED
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento = C_CADENA_DEFECTO_ORIGINACION_CRED
      ELSE
         # Formato de fecha originacion es AAAAMMDD converción a MM/DD/AAAA
         CALL fn_convierte_fecha_originacion(mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento) RETURNING g_fecha_tmp         
      END IF      
      
      # Actualiza registros de solicitud rechazada
      EXECUTE prp_actualiza_solicitud USING mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito,
                                            p_diag_interno,
                                            C_RESULTADO_OP_RECHAZADA,
                                            r_respuesta_cartera.v_saldo_credito ,
                                            r_respuesta_cartera.v_monto_originacion,
                                            g_fecha_tmp,
                                            g_tipo_portabilidad,
                                            v_derechohabiente.v_nombre,
                                            v_derechohabiente.v_apPaterno,
                                            v_derechohabiente.v_apMaterno,
                                            v_id_prt_solicitud_receptora

   CATCH
      DISPLAY "Error al actualizar datos:"
      DISPLAY "Id solicitud receptora:",v_id_prt_solicitud_receptora
      DISPLAY "Código: ",SQLCA.SQLCODE
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                              C_DESTINO_DIAG_ADA
                                         INTO v_diagostico_externo.*
      LET p_diag_externo    = v_diagostico_externo.v_diag_ext
      LET p_des_diagnostico = v_diagostico_externo.v_desc_gen
   END TRY
         
   CALL fn_asigna_msj_salida(C_RESULTADO_OP_RECHAZADA, # resultado operación
                             p_diag_externo,
                             p_des_diagnostico)

END FUNCTION

#Objetivo: Invoca función de WS consulta de crédito CARTERA (PRTW06)
FUNCTION fn_consulta_credito_cartera()

   LET v_error = FALSE
   INITIALIZE r_respuesta_cartera.* TO NULL
   # Invocar servicio de cartera
   CALL consultaCreditoCartera(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                               mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroDeCredito
                               ) RETURNING r_resultado_invoca_ws,
                                           r_respuesta_cartera.v_nss,
                                           r_respuesta_cartera.v_num_credito,
                                           r_respuesta_cartera.v_diag_cartera,
                                           r_respuesta_cartera.v_tpo_credito,
                                           r_respuesta_cartera.v_saldo_credito,
                                           r_respuesta_cartera.v_monto_originacion,
                                           r_respuesta_cartera.v_f_originacion,
                                           r_respuesta_cartera.v_causa_diag

   # POR PETICIÓN INFONAVIT TODOS SON NUEVO CRÉDITO
   LET g_tipo_portabilidad = C_CREDITO_NUEVO
   # Establece fecha de originación
   IF( r_respuesta_cartera.v_f_originacion IS NULL OR
       r_respuesta_cartera.v_f_originacion = " " OR
       r_respuesta_cartera.v_f_originacion = "00000000")THEN
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento = C_CADENA_DEFECTO_ORIGINACION_CRED
      # POR PETICIÓN INFONAVIT TODOS SON NUEVO CRÉDITO
      --LET g_tipo_portabilidad = C_SIN_TIPO_PRT
   ELSE
      # Recupera fecha de originación en formato mm/dd/aaaa
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento = r_respuesta_cartera.v_f_originacion
      CALL fn_convierte_fecha_originacion(r_respuesta_cartera.v_f_originacion) RETURNING g_fecha_tmp
      # POR PETICIÓN INFONAVIT TODOS SON NUEVO CRÉDITO
      --CALL fn_determina_tipo_portabilidad(g_fecha_tmp) RETURNING g_tipo_portabilidad
   END IF
   # Valida crédito en caso de que sea nulo
   IF( r_respuesta_cartera.v_num_credito IS NULL)THEN
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito = 0
   ELSE
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito = r_respuesta_cartera.v_num_credito
   END IF
      
   # Valida saldo en caso de que sea nulo
   IF( r_respuesta_cartera.v_monto_originacion IS NULL )THEN
      LET r_respuesta_cartera.v_monto_originacion = 0
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.saldoInsoluto = 0
   ELSE
      LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.saldoInsoluto = r_respuesta_cartera.v_monto_originacion
   END IF

   
   
   # 00 procedente cualquier otro no es procesdente
   CASE r_respuesta_cartera.v_diag_cartera
      WHEN C_CART_CRED_VIGENTE # Crédito vigente  
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = C_RESULTADO_OP_ACEPTADA
         
      WHEN C_CART_CRED_INEXISTENTE # Crédito inexistente
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = C_RESULTADO_OP_RECHAZADA

      WHEN C_CART_CRED_LIQUIDADO # Crédito liquidado
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = C_RESULTADO_OP_RECHAZADA

      WHEN C_CART_SIN_ACCESO_BD # sin acceso a BD
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = C_RESULTADO_OP_RECHAZADA
         
      OTHERWISE
         LET mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos = C_RESULTADO_OP_RECHAZADA         

   END CASE
   
   IF(r_resultado_invoca_ws <> 0)THEN
      DISPLAY "Error de comunicación con WS CARTERA:"
      DISPLAY "Estado:",r_resultado_invoca_ws
            
      EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_11,
                                              C_DESTINO_DIAG_FOV
                                         INTO v_diagostico_externo.*
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_11,
                                        v_diagostico_externo.v_diag_ext,
                                        v_diagostico_externo.v_desc_gen)
      LET v_error = TRUE

   END IF
   
   RETURN v_error
END FUNCTION

#Objetivo: Convierte fecha en formato AAAAMMDD a MM/DD/AAAA para la fecha de originación
FUNCTION fn_convierte_fecha_originacion(p_fecha_originacion_txt)
DEFINE p_fecha_originacion_txt CHAR(8), # AAAAMMDD
       v_fecha_originacion     DATE

   TRY
      LET v_fecha_originacion = DATE(p_fecha_originacion_txt[5,6]||"/"||p_fecha_originacion_txt[7,8]||"/"||p_fecha_originacion_txt[1,4])
   CATCH
      DISPLAY "ERROR: Fecha de originación erronea para solicitud: ",v_id_prt_solicitud_receptora
      DISPLAY "Fecha recibida: ",p_fecha_originacion_txt
      LET v_fecha_originacion = C_FECHA_DEFECTO_ORIGINACION_CRED # Fecha por defecto
   END TRY
   
   RETURN v_fecha_originacion
END FUNCTION

#Objetivo: Determina tipo de portabilidad con la fecha de originación
# Regla:
{
Nuevo (traspaso de saldo y subsecuentes)
• Créditos originados entre 19 marzo 2014 y 25 de mayo 2015. 
• Créditos originados posterior al 25 de mayo de 2015 deberá considerar como nuevo aquellos créditos en los cuales la consulta no exceda de 90 días naturales posterior a la fecha de originación. 

Preexiste (sólo subsecuentes)
• Créditos originados antes del 19 marzo 2014. 
• Créditos originados posterior al 25 de mayo de 2015 deberá considerar como preexistente aquellos créditos en los cuales la consulta exceda de 90 días naturales posterior a la fecha de originación.
}
FUNCTION fn_determina_tipo_portabilidad(p_fecha_originacion)
DEFINE p_fecha_originacion DATE,
       v_fecha_actual      DATE,
       v_fecha_calculada   DATE,
       v_tipo_prt          SMALLINT

   LET v_fecha_actual = TODAY
   IF( v_fecha_actual >= C_FECHA_INI_TIPO_PRT )THEN
      IF( p_fecha_originacion < C_FECHA_INI_TIPO_PRT )THEN
         LET v_tipo_prt = C_CREDITO_EXISTENTE
      ELSE      
         IF( p_fecha_originacion <= C_FECHA_FIN_TIPO_PRT )THEN
            LET v_tipo_prt = C_CREDITO_NUEVO
         ELSE         
            LET v_fecha_calculada = p_fecha_originacion + C_PERIODO_ORINARIO_CREDITO         
            IF( v_fecha_actual <= v_fecha_calculada )THEN
               LET v_tipo_prt = C_CREDITO_NUEVO   
            ELSE
               LET v_tipo_prt = C_CREDITO_EXISTENTE      
            END IF            
         END IF
      END IF
   ELSE
      LET v_tipo_prt = C_CREDITO_EXISTENTE
   END IF
      
   RETURN v_tipo_prt
END FUNCTION

#Objetivo: Función para actualizar el estado de la solicitud receptora
FUNCTION fn_actualiza_solicitud()

   # El registro de salida ya debe haber recuperado el reultado de la operación en la consulta de crédito
   CASE mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.diagnosticoInstitutos
      WHEN C_RESULTADO_OP_ACEPTADA
         TRY
            # Formato de fecha originacion es AAAAMMDD
            # conversión de fecha a MM/DD/AAAA
            --CALL fn_convierte_fecha_originacion(mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.fechaOtorgamiento) RETURNING g_fecha_tmp 
            
            # Actuzliza registros de solicitud aceptada
            EXECUTE prp_actualiza_solicitud USING mensajeSalidaSolicitud.enviaCreditoInfonavitReturn.numeroDeCredito,
                                                  C_DIAGNOSTICO_INTERNO_0,  # diagnostico
                                                  C_RESULTADO_OP_ACEPTADA,# respuesta
                                                  r_respuesta_cartera.v_saldo_credito,
                                                  r_respuesta_cartera.v_monto_originacion,
                                                  g_fecha_tmp,
                                                  g_tipo_portabilidad,
                                                  v_derechohabiente.v_nombre,
                                                  v_derechohabiente.v_apPaterno,
                                                  v_derechohabiente.v_apMaterno,
                                                  v_id_prt_solicitud_receptora

            # Avanza maquinaria para indicar que fovissste ha consultado el crédito
            EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_RECP,
                                                v_id_prt_solicitud_receptora,
                                                C_ID_SENAL_CON_CRE_CART,
                                                g_usuario_cod
                                           INTO v_resultado_maq.*
            
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_0,
                                                    C_DESTINO_DIAG_FOV
                                               INTO v_diagostico_externo.*
            # Cuando es aceptada, fovissste sólo recibe el resultado aceptado, motivo y descripción no se especifican
            CALL fn_asigna_msj_salida(C_RESULTADO_OP_ACEPTADA, # Diagnóstico
                                      v_diagostico_externo.v_diag_ext,
                                      "")

         CATCH
            DISPLAY "Error al actualizar datos aceptados:"
            DISPLAY "Id solicitud receptora:",v_id_prt_solicitud_receptora
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                    C_DESTINO_DIAG_FOV
                                               INTO v_diagostico_externo.*

            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)

         END TRY

      WHEN C_RESULTADO_OP_RECHAZADA
         TRY
            # Avanza maquinaria para indicar que fovissste ha consultado el crédito
            EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_RECP,
                                                v_id_prt_solicitud_receptora,
                                                C_ID_SENAL_RECHAZAR,
                                                g_usuario_cod
                                           INTO v_resultado_maq.*

            # Consulta diagnostico interno para actualizar solicitud
            EXECUTE prp_consulta_diag_interno USING r_respuesta_cartera.v_diag_cartera, # Diagnóstico de cartera
                                                    C_DESTINO_DIAG_C_I
                                               INTO v_diagnostico_interno.*
            # Recupera diagnostico a envíar a fovissste
            EXECUTE prp_consulta_diag_externo USING r_respuesta_cartera.v_diag_cartera,
                                                    C_DESTINO_DIAG_C_F
                                               INTO v_diagostico_externo.*
                                               
            CALL fn_asigna_msj_erroneo_salida(v_diagnostico_interno.v_diag_int,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)
                                              
         CATCH
            DISPLAY "Error al actualizar datos rechazados:"
            DISPLAY "Id solicitud receptora:",v_id_prt_solicitud_receptora
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            EXECUTE prp_consulta_diag_externo USING C_DIAGNOSTICO_INTERNO_1,
                                                    C_DESTINO_DIAG_FOV
                                               INTO v_diagostico_externo.*

            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_INTERNO_1,
                                              v_diagostico_externo.v_diag_ext,
                                              v_diagostico_externo.v_desc_gen)
         END TRY
   END CASE

END FUNCTION