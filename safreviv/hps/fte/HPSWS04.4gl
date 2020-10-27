--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/08/2015
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSWS04                                                       #
#Objetivo     => Funciones de WS de hipoteca, solicitud de cancelación de pagos#
#                Constandes provenientes de HPSWS04.inc                        #
#Fecha inicio => 10 Agosto 2015                                                #
################################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "HPSWS04.inc"
GLOBALS "HPSWS03.inc"
--GLOBALS "PRTW01.inc"
--GLOBALS "PRTG01.4gl"

PRIVATE
DEFINE v_f_actual  DATE,
       v_error     BOOLEAN,
       v_id_hps_solicitud     LIKE hps_solicitud_cancelacion.id_hps_solicitud_cancelacion,
       v_id_hps_solicitud_tmp LIKE hps_solicitud_cancelacion.id_hps_solicitud_cancelacion,
       v_id_hps_solicitud_pago_servicio LIKE hps_solicitud_pago_servicio.id_solicitud_pago_servicio,
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente

PRIVATE
DEFINE v_saldo_derechohabiente RECORD
          v_acciones LIKE cta_movimiento.monto_acciones,
          v_pesos    LIKE cta_movimiento.monto_pesos
       END RECORD

PRIVATE
DEFINE v_derechohabiente RECORD
          apPaterno LIKE afi_derechohabiente.ap_paterno_af,
          apMaterno LIKE afi_derechohabiente.ap_materno_af,
          nombre    LIKE afi_derechohabiente.nombre_af
       END RECORD

DEFINE r_error            BOOLEAN

# Descripción: Inicializa consultas SQL
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 seq_hps_solicitud_cancelacion.NEXTVAL",
                    "   FROM systables"
   PREPARE prp_rec_seq_sol FROM v_consulta           
   
   LET v_consulta = " INSERT INTO hps_solicitud_cancelacion",
                    " (id_hps_solicitud_cancelacion,",
                    "  id_solicitud_pago_servicio,",
                    "  nss,",
                    "  n_caso,",
                    --"  resultado_operacion,",
                    --"  diagnostico_interno,",
                    "  estado,",
                    "  f_actualiza)",
                    " VALUES(?,0,?,?,?,?)"
   PREPARE prp_genera_solicitud_cedente FROM v_consulta

   LET v_consulta = "INSERT INTO hps_restitucion",
                    "(id_hps_restitucion,",
                    " id_solicitud_pago_servicio,",
                    " id_hps_solicitud_cancelacion,",
                    " id_derechohabiente,",
                    " nss,",
                    " subcuenta,",
                    " mto_acciones,",
                    " mto_pesos,",
                    " estado)",
                    "VALUES(seq_hps_restitucion.NEXTVAL,?,?,?,?,?,?,?,?)"
   PREPARE prp_genera_restitucion FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET diagnostico_interno = ?,",
                    "        resultado_operacion = ?,",
                    "        id_solicitud_pago_servicio = ?,",
                    "        nombre = ?,",
                    "        paterno = ?,",
                    "        materno = ?,",
                    "        n_caso = ?,",
                    "        estado = ?,",
                    "        f_actualiza = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET diagnostico_interno = ?,",
                    "        resultado_operacion = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_solicitud_error FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET estado = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_estado_solicitud_cancelacion FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_pago_servicio",
                    "    SET ind_actividad = ?",
                    "  WHERE id_solicitud_pago_servicio = ?"
   PREPARE prp_actualiza_estado_pago_srv FROM v_consulta

   LET v_consulta = " UPDATE hps_cat_pago_servicio",
                    "    SET estado = ?",
                    "  WHERE id_solicitud_pago_servicio = ?"
   PREPARE prp_actualiza_estado_cat_pago_srv FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_solicitud_pago_servicio",
                    "   FROM hps_solicitud_pago_servicio",
                    "  WHERE nss = ?", 
                    "    AND ind_actividad = ? "
   PREPARE prp_rec_existe_pago_nss FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE nss = ?",
                    "    AND estado = ?",
                    "  ORDER BY id_hps_solicitud_cancelacion DESC"
   PREPARE prp_busca_solicitud_cancelacion FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE nss = ?",
                    "    AND estado IN (?,?)",
                    "  ORDER BY id_hps_solicitud_cancelacion DESC"
   PREPARE prp_valida_existencia_solicitud FROM v_consulta
   
   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion,",
                    "        id_solicitud_pago_servicio",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE n_caso = ?",
                    "    AND estado = ?"
   PREPARE prp_val_num_caso FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 diagnostico_interno",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE id_hps_solicitud_cancelacion = ?",
                    "    AND estado = ?"
   PREPARE prp_consulta_diag_restitucion FROM v_consulta
   
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
                    "    AND subcuenta IN (?,?)"
   PREPARE prp_rec_saldo_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT diagnostico_interno",
                    "   FROM prt_rch_marca_diagnostico",
                    "  WHERE marca_activa = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_rec_diag_marca FROM v_consulta

   # Especifica 5 segundos como máximo para la espera de respuesta del servidor externo
   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 5 )

END FUNCTION

FUNCTION fn_determina_tipo_consulta()

   INITIALIZE v_derechohabiente.*,
              v_id_hps_solicitud,
              v_id_hps_solicitud_pago_servicio,
              v_id_derechohabiente,
              v_saldo_derechohabiente.* TO NULL
   
   CASE mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus

      WHEN C_ID_ESTATUS_VALIDACION_PORTAL # consulta del portal
         CALL fn_registra_solicitud_cancelacion() RETURNING r_error                                               
         IF NOT(r_error)THEN
            CALL fn_valida_solicitud_portabilidad(1) RETURNING r_error
         END IF

      WHEN C_ID_ESTATUS_VALIDACION_CRM # Consulta de validación de portabilidad ADAI
         CALL fn_recupera_id_solicitud() RETURNING r_error         
         IF NOT( r_error )THEN
            CALL fn_valida_solicitud_portabilidad(1) RETURNING r_error
         END IF

      WHEN C_ID_ESTATUS_CANCELA_HPS # Confirmación de ADAI para cancelaciónde pagos
         CALL fn_valida_numero_caso() RETURNING r_error
         IF NOT ( r_error )THEN
            CALL fn_actualiza_sol_formalizada() RETURNING r_error
            IF NOT( r_error )THEN
               # Llamada a bus para enviar solicitud de portabilidad a procesar
               CALL fn_genera_restitucion()
            END IF
         END IF

      OTHERWISE
         CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1001)
         
   END CASE

END FUNCTION

#Objetivo: Registra la solicitud de cancelación de pagos
FUNCTION fn_registra_solicitud_cancelacion()

   LET v_f_actual = TODAY
   TRY 
      INITIALIZE v_id_hps_solicitud TO NULL
      # Recupera sequencia
      EXECUTE prp_rec_seq_sol INTO v_id_hps_solicitud
      EXECUTE prp_genera_solicitud_cedente USING v_id_hps_solicitud,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,                                                 
                                                 C_ESTADO_INI_SOLICITUD_CANCELACION,
                                                 v_f_actual

   CATCH # Captura error sql
      DISPLAY "Error al insertar datos para:"
      DISPLAY "NSS:      ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
      DISPLAY "No. caso: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso
      DISPLAY "Flujo:    ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
      DISPLAY "Código:   ",SQLCA.SQLCODE
      DISPLAY "Mensaje:  ",SQLCA.sqlerrm
      
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)

      RETURN TRUE # Error en sql             
   END TRY
                                              
   RETURN FALSE # Ejecución realizada correctamente
END FUNCTION

#Objetivo: Funcion para validar que los datos de la solicitud de cancelación
FUNCTION fn_valida_solicitud_portabilidad(p_verifica_existente)
DEFINE v_estado             SMALLINT,
       p_verifica_existente SMALLINT

   INITIALIZE v_estado TO NULL
   LET v_error = FALSE # sin error
   # VALIDA NSS
   IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss = ' ')THEN
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
      LET v_error = TRUE
   ELSE
      TRY
         # BUSCA DERECHOHABIENTE EN afi_derechohabiente
         INITIALIZE v_id_derechohabiente,v_derechohabiente.* TO NULL
         EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                             INTO v_id_derechohabiente,
                                                  v_derechohabiente.nombre,
                                                  v_derechohabiente.apPaterno,
                                                  v_derechohabiente.apMaterno
         IF( v_id_derechohabiente IS NOT NULL )THEN
            IF( p_verifica_existente )THEN
               CALL fn_valida_existencia_portabilidad() RETURNING v_error
            ELSE
               LET v_error = FALSE
            END IF
            IF NOT( v_error )THEN   
               # VALIDA EXISTENCIA DE PAGO DE SERVICIOS
               INITIALIZE v_id_hps_solicitud_pago_servicio TO NULL
               EXECUTE prp_rec_existe_pago_nss USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                     C_ESTADO_PAGO_SERVICIO_ACTIVO
                                                INTO v_id_hps_solicitud_pago_servicio
               IF( v_id_hps_solicitud_pago_servicio IS NOT NULL)THEN
                  IF NOT( v_error )THEN
                     # VALIDA EXISTENCIA DE SALDOS DE PAGOS
                     INITIALIZE v_saldo_derechohabiente.* TO NULL
                     EXECUTE prp_rec_saldo_derechohabiente USING v_id_derechohabiente,
                                                                 C_SUBCUENTA_PREDIAL,
                                                                 C_SUBCUENTA_CONSERVACION
                                                            INTO v_saldo_derechohabiente.v_acciones,
                                                                 v_saldo_derechohabiente.v_pesos
                     # DETERMINA ESTADO DE LA SOLICITUD SEGÚN FLUJO
                     CASE mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
                        WHEN C_ID_ESTATUS_VALIDACION_PORTAL
                           LET v_estado = C_ESTADO_SOLICITUD_REGISTRADA

                        WHEN C_ID_ESTATUS_VALIDACION_CRM
                        LET v_estado = C_ESTADO_SOLICITUD_ACEPTADA

                     END CASE
                     # SALDO > 0 con restitución, SALDO <= 0 sin restitución
                     IF( v_saldo_derechohabiente.v_acciones > 0 )THEN
                        CALL fn_actualiza_solicitud(C_RESULTADO_OP_ACEPTADA_FOV,
                                                    C_DIAGNOSTICO_1000,
                                                    v_estado)
                     ELSE
                        CALL fn_actualiza_solicitud(C_RESULTADO_OP_ACEPTADA_FOV,
                                                    C_DIAGNOSTICO_1002,
                                                    v_estado)
                     END IF
                  END IF
               ELSE
                  CALL fn_actualiza_solicitud(C_RESULTADO_OP_RECHAZADA_FOV,
                                              C_DIAGNOSTICO_1001,
                                              C_ESTADO_SOLICITUD_RECHAZADA)
                           
                  LET v_error = TRUE            
               END IF
            END IF
         ELSE
            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
            LET v_error = TRUE
         END IF
      CATCH
         DISPLAY "Error en estructura"
         DISPLAY "NSS:    ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
         DISPLAY "Código: ",SQLCA.SQLCODE
         DISPLAY "Mensaje:",SQLCA.sqlerrm
      
         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
         LET v_error = TRUE
      END TRY      
   END IF

   RETURN v_error
END FUNCTION

# Descripción: Función para validar la existencia del nss solicitud de cancelación
FUNCTION fn_valida_existencia_portabilidad()

   # VALIDA SI YA EXISTE EL NSS EN SOLICITUD DE CANCELACIÓN
   INITIALIZE v_id_hps_solicitud_tmp TO NULL

   EXECUTE prp_valida_existencia_solicitud USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 C_ESTADO_SOLICITUD_ACEPTADA,
                                                 C_ESTADO_SOL_REST_SOLICITADA
                                            INTO v_id_hps_solicitud_tmp
   IF(v_id_hps_solicitud_tmp IS NULL)THEN
      RETURN FALSE
   ELSE
      IF( mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus = C_ID_ESTATUS_VALIDACION_CRM )THEN
         INITIALIZE v_derechohabiente.*,mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      END IF
      
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   END IF

END FUNCTION

# Descripción: Recupera y valida existencia de solicitud previa
FUNCTION fn_recupera_id_solicitud()

   INITIALIZE v_id_hps_solicitud TO NULL   
   EXECUTE prp_busca_solicitud_cancelacion USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 C_ESTADO_SOLICITUD_REGISTRADA
                                            INTO v_id_hps_solicitud
   IF(v_id_hps_solicitud IS NULL)THEN
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

# Descripción: Función para validar que el número de caso exista en las solicitudes de cancelación
FUNCTION fn_valida_numero_caso()

   INITIALIZE v_id_hps_solicitud TO NULL
   EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                       INTO v_id_derechohabiente,
                                            v_derechohabiente.nombre,
                                            v_derechohabiente.apPaterno,
                                            v_derechohabiente.apMaterno
   INITIALIZE v_derechohabiente.* TO NULL
   EXECUTE prp_val_num_caso USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                  C_ESTADO_SOLICITUD_ACEPTADA
                             INTO v_id_hps_solicitud,
                                  v_id_hps_solicitud_pago_servicio
                                  
   IF(v_id_hps_solicitud IS NULL)THEN # si el número de caso recibido no existe devuelve mensaje rechazado
      # Asigna mensaje de respuesta a portal
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

#Objetivo: Función para asignar los valores al mensaje de salida
FUNCTION fn_asigna_msj_salida(p_diagnostico)
DEFINE p_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.numeroCaso  = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.nss         = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.apPaterno   = v_derechohabiente.apPaterno
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.apMaterno   = v_derechohabiente.apMaterno
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.nombre      = v_derechohabiente.nombre   
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.idEstatus   = mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.diagnostico = p_diagnostico

END FUNCTION

#Objetivo: Función para asignar mensaje erroneo de salida
FUNCTION fn_asigna_msj_erroneo_salida(p_diagnostico)
DEFINE p_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   TRY   
      # Actuzliza registros de solicitud rechazada
      EXECUTE prp_actualiza_solicitud_error USING p_diagnostico,
                                                  C_RESULTADO_OP_RECHAZADA_FOV,
                                                  v_id_hps_solicitud
   CATCH
      DISPLAY "Error al actualizar datos:"
      DISPLAY "Id solicitud: ",v_id_hps_solicitud
      DISPLAY "Código:       ",SQLCA.SQLCODE
      DISPLAY "Mensaje:      ",SQLCA.sqlerrm
   END TRY

   CALL fn_asigna_msj_salida(p_diagnostico)

END FUNCTION

#Objetivo: Función para actualizar el estado de la solicitud cedente
FUNCTION fn_actualiza_solicitud(p_diagnostico_operacion,
                                p_diagnostico,
                                p_estado)
DEFINE p_diagnostico_operacion CHAR(2),
       p_diagnostico           SMALLINT,
       p_estado                SMALLINT,
       v_fecha_actual          DATE

   # El registro de salida ya debe haber recuperado el reultado de la operación
   CASE p_diagnostico_operacion
      WHEN C_RESULTADO_OP_ACEPTADA_FOV
         TRY
            LET v_fecha_actual = TODAY
            # Actualiza registros de solicitud aceptada
            EXECUTE prp_actualiza_solicitud USING p_diagnostico,
                                                  p_diagnostico_operacion,
                                                  v_id_hps_solicitud_pago_servicio,
                                                  v_derechohabiente.nombre,
                                                  v_derechohabiente.apPaterno,
                                                  v_derechohabiente.apMaterno,
                                                  mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                                  p_estado,
                                                  v_fecha_actual,
                                                  v_id_hps_solicitud

            IF( mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus = C_ID_ESTATUS_VALIDACION_CRM )THEN
               INITIALIZE v_derechohabiente.*,mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
            END IF
            # Asigna mensaje de respuesta a portal
            CALL fn_asigna_msj_salida(p_diagnostico)
            
         CATCH
            DISPLAY "Error al actualizar datos aceptados:"
            DISPLAY "Id solicitud cedente:",v_id_hps_solicitud
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)

         END TRY

      WHEN C_RESULTADO_OP_RECHAZADA_FOV
         TRY
            # Actualiza estado a rechazado
            EXECUTE prp_actualiza_estado_solicitud_cancelacion USING p_estado,
                                                                     v_id_hps_solicitud

            CALL fn_asigna_msj_erroneo_salida(p_diagnostico)
         CATCH
            DISPLAY "Error al actualizar datos rechazados:"
            DISPLAY "Id solicitud cedente:",v_id_hps_solicitud
            DISPLAY "Código: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm

            CALL fn_asigna_msj_erroneo_salida(p_diagnostico)
         END TRY
         
   END CASE

END FUNCTION

# Descripción: Actualiza datos personales del nss
FUNCTION fn_actualiza_sol_formalizada()
DEFINE v_error BOOLEAN

   LET v_error = FALSE
   TRY

      EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_PAGO_CANCELADA,
                                                               v_id_hps_solicitud

      EXECUTE prp_actualiza_estado_pago_srv USING C_ESTADO_PAGO_SERVICIO_CANCELADO,
                                                  v_id_hps_solicitud_pago_servicio

      EXECUTE prp_actualiza_estado_cat_pago_srv USING C_ESTADO_PAGO_SERVICIO_CANCELADO,
                                                      v_id_hps_solicitud_pago_servicio
      LET v_error = FALSE
   CATCH
      DISPLAY "Error al actualizar solicitud formalizada:"
      DISPLAY "Id solicitud cancelación:",v_id_hps_solicitud
      DISPLAY "Id pago de servicios:    ",v_id_hps_solicitud_pago_servicio
      DISPLAY "Código:                  ",SQLCA.SQLCODE
      DISPLAY "Mensaje:                 ",SQLCA.sqlerrm
      LET v_error = TRUE
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)      
   END TRY
   RETURN v_error
END FUNCTION

# Descripción: Genera restitución en caso de que el diagnóstico sea
FUNCTION fn_genera_restitucion()
DEFINE v_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   EXECUTE prp_consulta_diag_restitucion USING v_id_hps_solicitud,
                                               C_ESTADO_SOL_PAGO_CANCELADA
                                          INTO v_diagnostico

   IF( v_diagnostico = C_DIAGNOSTICO_1000 )THEN
      TRY
         INITIALIZE v_saldo_derechohabiente.* TO NULL
         EXECUTE prp_rec_saldo_derechohabiente USING v_id_derechohabiente,
                                                     C_SUBCUENTA_PREDIAL,
                                                     C_SUBCUENTA_CONSERVACION
                                                INTO v_saldo_derechohabiente.v_acciones,
                                                     v_saldo_derechohabiente.v_pesos
                                                     
         EXECUTE prp_genera_restitucion USING v_id_hps_solicitud_pago_servicio,
                                              v_id_hps_solicitud,                                              
                                              v_id_derechohabiente,
                                              mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                              C_SUBCUENTA_VIVIENDA,
                                              v_saldo_derechohabiente.v_acciones,
                                              v_saldo_derechohabiente.v_pesos,
                                              C_ESTADO_REST_REGISTRADA

         EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_REST_REGISTRADA,
                                                                  v_id_hps_solicitud
         INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
         CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1100)
      CATCH
         DISPLAY "Error al registrar restitución:"
         DISPLAY "Id solicitud cancelación:",v_id_hps_solicitud
         DISPLAY "Id pago de servicios:    ",v_id_hps_solicitud_pago_servicio
         DISPLAY "Código:                  ",SQLCA.SQLCODE
         DISPLAY "Mensaje:                 ",SQLCA.sqlerrm

         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)
      END TRY
   ELSE
      EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_REST_REGISTRADA,
                                                               v_id_hps_solicitud
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1100)
   END IF
END FUNCTION