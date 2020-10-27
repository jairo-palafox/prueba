--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/11/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS11                                                       #
#Objetivo     => Diagnostica aclaración de Portabilidad                        #
#Fecha inicio => 10 Noviembre 2015                                             #
################################################################################

DATABASE safre_viv

GLOBALS "PRTWS11.inc"
GLOBALS "PRTWS02.inc"
GLOBALS "PRTWS07.inc"
GLOBALS "PRTG01.4gl"

DEFINE v_diagnostico RECORD
          v_nss     LIKE afi_derechohabiente.nss,
          v_paterno LIKE afi_derechohabiente.ap_materno_af,
          v_materno LIKE afi_derechohabiente.ap_materno_af,
          v_nombre  LIKE afi_derechohabiente.nombre_af,
          v_edo_aclaracion  LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
          v_tipo_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
          v_diagnostico     LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion
       END RECORD,
       g_derechohabiente RECORD
          v_nss    CHAR(11)
       END RECORD 

# Descripción: Inicializa consultas SQL
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 n_referencia,",
                    "        marca,",
                    "        f_inicio",
                    "   FROM sfr_marca_activa",
                    "  WHERE id_derechohabiente = ?",
                    "    AND marca IN (?,?)"
   PREPARE prp_consulta_marca FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_derechohabiente",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_id_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 NVL(nombre_af,''),",
                    "        NVL(ap_paterno_af,''),",
                    "        NVL(ap_materno_af,'')",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT estado,",
                    "        f_ini_tramite",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_rec_estado_ced FROM v_consulta

   LET v_consulta = " SELECT estado",
                    "   FROM prt_solicitud_receptora",
                    "  WHERE id_prt_solicitud_receptora = ?"
   PREPARE prp_rec_estado_rec FROM v_consulta

   LET v_consulta = " SELECT MAX(id_prt_solicitud_cedente)",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND estado = (SELECT MAX(estado) FROM prt_solicitud_cedente WHERE nss = ?)"
   PREPARE prp_rec_id_ced FROM v_consulta

   LET v_consulta = " SELECT MAX(id_prt_solicitud_receptora)",
                    "   FROM prt_solicitud_receptora",
                    "  WHERE nss = ?",
                    "    AND estado = (SELECT MAX(estado) FROM prt_solicitud_receptora WHERE nss = ?)"
   PREPARE prp_rec_id_rec FROM v_consulta

   LET v_consulta = " SELECT NVL(SUM(monto_acciones),0)",
                    "   FROM TABLE(MULTISET(",
                    " SELECT monto_acciones",
                    "   FROM cta_movimiento ",
                    "  WHERE id_derechohabiente = ?",
                    "    AND subcuenta          = ?",
                    "    AND movimiento NOT BETWEEN 1600 AND 1699", # cualquier movimiento que no sea de portabilidad (sólo movimientos de registro de pagos)
                    " UNION",
                    " SELECT monto_acciones",
                    "   FROM cta_movimiento ",
                    "  WHERE id_derechohabiente = ?",
                    "    AND subcuenta          = ?",
                    "    AND movimiento = ? ))"
   PREPARE prp_val_sdo_ced FROM v_consulta

   LET v_consulta = " SELECT SUM(monto_acciones)",
                    "   FROM cta_movimiento",
                    "  WHERE id_derechohabiente = ?",
                    "    AND subcuenta = ?",
                    "    AND movimiento IN (?,?)"
   PREPARE prp_con_mov FROM v_consulta 

   LET v_consulta = " INSERT INTO prt_aclaracion",
                    " (id_prt_aclaracion,",
                    "  origen_aclaracion,",
                    "  id_solicitud_portabilidad,",
                    "  nss,",
                    "  numero_caso,",
                    "  f_aclaracion,",
                    "  estatus_aclaracion,",
                    "  tipo_aclaracion,",
                    "  diagnostico_aclaracion)",
                    " VALUES(seq_prt_aclaracion.NEXTVAL,?,?,?,?,?,?,?,?)"
   PREPARE pr_ins_reg_aclaracion FROM v_consulta

END FUNCTION

# Descripción: 
FUNCTION fn_determina_tipificacion(p_nss)
DEFINE p_nss    CHAR(11),
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_marca_prt          LIKE sfr_marca_activa.marca,
       v_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_f_marca            LIKE sfr_marca_activa.f_inicio,
       v_id_solicitud_prt   LIKE prt_solicitud_cedente.id_prt_solicitud_cedente

   INITIALIZE g_derechohabiente.* TO NULL
   LET g_derechohabiente.v_nss    = p_nss

   EXECUTE prp_rec_id_derechohabiente USING p_nss 
                                       INTO v_id_derechohabiente

   IF( v_id_derechohabiente IS NULL )THEN
      CALL fn_asigna_diagnostico(NULL,
                                 C_ORIGEN_ACL_CED,
                                 C_ESTADO_ACL_NO_PROCEDENTE,
                                 C_TIPO_ACL_FOV,
                                 C_DIAGNOSTICO_ACL_10)
      RETURN v_diagnostico.*
   END IF
   INITIALIZE v_marca_prt,
              v_id_solicitud_portabilidad TO NULL
   EXECUTE prp_consulta_marca USING v_id_derechohabiente,
                                    C_MARCA_PRT_CEDENTE,
                                    C_MARCA_PRT_RECEPTORA
                               INTO v_id_solicitud_portabilidad,
                                    v_marca_prt,
                                    v_f_marca

   CASE v_marca_prt

      WHEN C_MARCA_PRT_CEDENTE
         CALL fn_valida_matriz_marcada_ced(v_id_derechohabiente,
                                           v_id_solicitud_portabilidad,
                                           v_f_marca)

      WHEN C_MARCA_PRT_RECEPTORA
         CALL fn_valida_matriz_marcada_rec(v_id_solicitud_portabilidad,
                                           v_f_marca,
                                           v_id_derechohabiente)

      OTHERWISE
         # Consulta solicitud Cedente
         INITIALIZE v_id_solicitud_prt TO NULL
         EXECUTE prp_rec_id_ced USING p_nss,
                                      p_nss
                                 INTO v_id_solicitud_prt

         IF( v_id_solicitud_prt IS NOT NULL )THEN
            CALL fn_valida_matriz_sin_marcada_ced(v_id_solicitud_prt)
         ELSE
            # Consulta solicitud Receptora
            INITIALIZE v_id_solicitud_prt TO NULL
            EXECUTE prp_rec_id_rec USING p_nss,
                                         p_nss
                                    INTO v_id_solicitud_prt

            IF( v_id_solicitud_prt IS NOT NULL )THEN
               CALL fn_valida_matriz_sin_marcada_rec(v_id_solicitud_prt,
                                                     v_id_derechohabiente)
            ELSE
               CALL fn_asigna_diagnostico(NULL,
                                          C_ORIGEN_ACL_CED,
                                          C_ESTADO_ACL_NO_PROCEDENTE,
                                          C_TIPO_ACL_FOV,
                                          C_DIAGNOSTICO_ACL_10)

            END IF
         END IF

   END CASE
                                    
   RETURN v_diagnostico.*
END FUNCTION

# Descripción: 
FUNCTION fn_asigna_diagnostico(p_id_solicitud_portabilidad,
                               p_origen_aclaracion,
                               p_edo_aclaracion,
                               p_tipo_aclaracion,
                               p_diagnostico)
DEFINE p_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       p_origen_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       p_edo_aclaracion  LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
       p_tipo_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       p_diagnostico     LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion,
       v_derechohabiente RECORD
          v_nombre  LIKE afi_derechohabiente.nombre_af,
          v_paterno LIKE afi_derechohabiente.ap_paterno_af,
          v_materno LIKE afi_derechohabiente.ap_materno_af
       END RECORD,
       f_actual DATETIME YEAR TO SECOND,
       v_nulo   CHAR(1)

   TRY
      LET f_actual = CURRENT YEAR TO SECOND
      LET v_nulo = NULL
      
      EXECUTE prp_rec_derechohabiente USING g_derechohabiente.v_nss
                                       INTO v_derechohabiente.*
      {IF( SQLCA.sqlcode = 100 )THEN # no encontró registros
         LET v_derechohabiente.v_nombre  = " "
         LET v_derechohabiente.v_paterno = " "
         LET v_derechohabiente.v_materno = " "
      END IF}

      LET v_diagnostico.v_nss             = g_derechohabiente.v_nss
      LET v_diagnostico.v_paterno         = v_derechohabiente.v_paterno
      LET v_diagnostico.v_materno         = v_derechohabiente.v_materno
      LET v_diagnostico.v_nombre          = v_derechohabiente.v_nombre
      LET v_diagnostico.v_edo_aclaracion  = p_edo_aclaracion
      LET v_diagnostico.v_tipo_aclaracion = p_tipo_aclaracion
      LET v_diagnostico.v_diagnostico     = p_diagnostico

      EXECUTE pr_ins_reg_aclaracion USING p_origen_aclaracion,
                                          p_id_solicitud_portabilidad,
                                          g_derechohabiente.v_nss,
                                          v_nulo,
                                          f_actual,
                                          p_edo_aclaracion,
                                          p_tipo_aclaracion,
                                          p_diagnostico
   CATCH
      DISPLAY "ERROR AL REGISTRAR LOG ACLARACION:"
      DISPLAY "CÓDIGO:",SQLCA.sqlcode
      DISPLAY "MENSAJE:",SQLCA.sqlerrm
      LET v_diagnostico.v_nss             = g_derechohabiente.v_nss
      LET v_diagnostico.v_paterno         = v_derechohabiente.v_paterno
      LET v_diagnostico.v_materno         = v_derechohabiente.v_materno
      LET v_diagnostico.v_nombre          = v_derechohabiente.v_nombre
      LET v_diagnostico.v_edo_aclaracion  = p_edo_aclaracion
      LET v_diagnostico.v_tipo_aclaracion = p_tipo_aclaracion
      LET v_diagnostico.v_diagnostico     = p_diagnostico
   END TRY

END FUNCTION

# Descripción: 
FUNCTION fn_valida_matriz_marcada_ced(p_id_derechohabiente,
                                      p_id_solicitud_portabilidad, 
                                      p_f_marca)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       p_f_marca       LIKE sfr_marca_activa.f_inicio,
       v_estado_sol    LIKE prt_solicitud_cedente.estado,
       v_f_ini_tramite LIKE prt_solicitud_cedente.f_ini_tramite, # no se usa aqui
       v_f_actual      DATE,
       v_dias_dif      SMALLINT,
       v_origen_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_edo_aclaracion    LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
       v_tipo_aclaracion   LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_diagnostico       LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion,
       v_monto_aivs        LIKE cta_movimiento.monto_acciones

   EXECUTE prp_rec_estado_ced USING p_id_solicitud_portabilidad                                 
                               INTO v_estado_sol,
                                    v_f_ini_tramite

   LET v_f_actual = TODAY
   DISPLAY "p_f_marca:",p_f_marca
   DISPLAY "v_f_actual:",v_f_actual
   LET v_dias_dif = v_f_actual - p_f_marca

   LET v_origen_aclaracion = C_ORIGEN_ACL_CED
   LET v_tipo_aclaracion   = C_TIPO_ACL_INF
   
   CASE 

      WHEN v_estado_sol = C_ESTADO_MARCADA_PRO OR  #40,42,44,50,60,70
           v_estado_sol = C_ESTADO_PENDIE_NOTIF_CRM OR 
           v_estado_sol = C_ESTADO_SDO_SOLICITADO_PRO OR 
           v_estado_sol = C_ESTADO_SDO_NOTIFICADO_PRO OR
           v_estado_sol = C_ESTADO_SDO_PRELIQUIDADO_CED
         DISPLAY "v_dias_dif:",v_dias_dif 
         IF( v_dias_dif > C_DIAS_ACLARACION_MARCADO )THEN # dias dif > 60
            LET v_edo_aclaracion = C_ESTADO_ACL_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_5
         ELSE
            LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_6
         END IF

      WHEN v_estado_sol = C_ESTADO_SDO_RECHAZADO_PRO #55
         LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
         LET v_diagnostico    = C_DIAGNOSTICO_ACL_7

      WHEN v_estado_sol = C_ESTADO_SDO_LIQUIDADO_CED #80
         LET v_monto_aivs = 0
         EXECUTE prp_val_sdo_ced USING p_id_derechohabiente,
                                       C_SUBCUENTA_PRT,
                                       p_id_derechohabiente,
                                       C_SUBCUENTA_PRT,
                                       C_MOV_CED_CARGO_SUB
                                  INTO v_monto_aivs
         IF( v_monto_aivs > 0)THEN
            LET v_edo_aclaracion = C_ESTADO_ACL_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_9
         ELSE
            LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_8
         END IF

   END CASE

   CALL fn_asigna_diagnostico(p_id_solicitud_portabilidad,
                              v_origen_aclaracion,
                              v_edo_aclaracion,
                              v_tipo_aclaracion,
                              v_diagnostico)
   
END FUNCTION

# Descripción: 
FUNCTION fn_valida_matriz_marcada_rec(p_id_solicitud_portabilidad, p_f_marca,p_id_derechohabiente)
DEFINE p_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_f_marca    LIKE sfr_marca_activa.f_inicio,
       v_estado_sol LIKE prt_solicitud_cedente.estado,
       v_f_actual   DATE,
       v_dias_dif   SMALLINT,
       v_origen_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_edo_aclaracion  LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
       v_tipo_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_diagnostico     LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion

   EXECUTE prp_rec_estado_rec USING p_id_solicitud_portabilidad
                               INTO v_estado_sol

   LET v_f_actual = TODAY
   LET v_dias_dif = v_f_actual - p_f_marca

   LET v_origen_aclaracion = C_ORIGEN_ACL_REC
   LET v_tipo_aclaracion   = C_TIPO_ACL_FOV

   CASE 

      WHEN v_estado_sol = C_ESTADO_MARCA_PRROCESADA_RECEPTORA OR  #20,30
           v_estado_sol = C_ESTADO_MARCA_RECHAZADA_RECEPTORA
         
         IF( v_dias_dif > C_DIAS_ACLARACION_MARCADO )THEN
            LET v_edo_aclaracion = C_ESTADO_ACL_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_11
         ELSE
            LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_12
         END IF

      WHEN v_estado_sol = C_ESTADO_SALDO_LIQUIDADO_RECEPTORA #40
         CALL fn_valida_saldos_rec(p_id_derechohabiente) RETURNING v_edo_aclaracion,v_diagnostico

   END CASE

   CALL fn_asigna_diagnostico(p_id_solicitud_portabilidad,
                              v_origen_aclaracion,
                              v_edo_aclaracion,
                              v_tipo_aclaracion,
                              v_diagnostico)
                               
END FUNCTION

# Descripción: 
FUNCTION fn_valida_matriz_sin_marcada_ced(p_id_solicitud_portabilidad)
DEFINE p_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_estado_sol    LIKE prt_solicitud_cedente.estado,
       v_f_ini_tramite DATETIME YEAR TO SECOND,
       v_f_actual      DATETIME YEAR TO SECOND,
       v_horas_dif     INTERVAL DAY TO SECOND,
       v_origen_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_edo_aclaracion    LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
       v_tipo_aclaracion   LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_diagnostico       LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion

   EXECUTE prp_rec_estado_ced USING p_id_solicitud_portabilidad
                               INTO v_estado_sol,
                                    v_f_ini_tramite

   LET v_f_actual = CURRENT YEAR TO SECOND
   # si la diferencia es de más de un mes, la variable es nula
   LET v_horas_dif = v_f_actual - v_f_ini_tramite

   LET v_origen_aclaracion = C_ORIGEN_ACL_CED
   LET v_tipo_aclaracion   = C_TIPO_ACL_SIN
   
   CASE 

      WHEN v_estado_sol = C_ESTADO_INI_SOLICITUD_CEDENTE OR  #0,10,12,15
           v_estado_sol = C_ESTADO_ACEPTADA_FOVISSSTE_CEDENTE OR
           v_estado_sol = C_ESTADO_RECHAZADA_FOVISSSTE_CEDENTE OR
           v_estado_sol = C_ESTADO_SOLICITADA_CESI_CEDENTE

         LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
         LET v_diagnostico    = C_DIAGNOSTICO_ACL_2
         
      WHEN v_estado_sol = C_ESTADO_FORMALIZADA_CESI_CEDENTE OR #20,30
           v_estado_sol = C_ESTADO_SALDO_PRELIQUIDADO_RECEPTORA
         IF( v_horas_dif <= C_HORAS_ACLARACION_REC )THEN
            LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_3
         ELSE
            LET v_edo_aclaracion = C_ESTADO_ACL_PROCEDENTE
            LET v_diagnostico    = C_DIAGNOSTICO_ACL_1
         END IF

      WHEN v_estado_sol = C_ESTADO_MARCA_RCH_PRO # 35 
         LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
         LET v_diagnostico    = C_DIAGNOSTICO_ACL_4

   END CASE

   CALL fn_asigna_diagnostico(p_id_solicitud_portabilidad,
                              v_origen_aclaracion,
                              v_edo_aclaracion,
                              v_tipo_aclaracion,
                              v_diagnostico)
                              
END FUNCTION

# Descripción: 
FUNCTION fn_valida_matriz_sin_marcada_rec(p_id_solicitud_portabilidad,p_id_derechohabiente)
DEFINE p_id_solicitud_portabilidad LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_estado_sol LIKE prt_solicitud_cedente.estado,
       v_origen_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_edo_aclaracion  LIKE prt_cat_estatus_aclaracion.estatus_aclaracion,
       v_tipo_aclaracion LIKE prt_cat_origen_aclaracion.origen_aclaracion,
       v_diagnostico     LIKE prt_cat_diagnostico_aclaracion.diagnostico_aclaracion

   EXECUTE prp_rec_estado_rec USING p_id_solicitud_portabilidad
                               INTO v_estado_sol

   LET v_origen_aclaracion = C_ORIGEN_ACL_REC
   LET v_tipo_aclaracion   = C_TIPO_ACL_FOV

   CASE 

      WHEN v_estado_sol = C_ESTADO_SOL_REGISTRADA OR
           v_estado_sol = C_ESTADO_SOL_CONSULTADA_RECEPTORA OR
           v_estado_sol = C_ESTADO_SOL_RECHAZADA_RECEPTORA OR
           v_estado_sol = C_ESTADO_MARCA_RECHAZADA_RECEPTORA

         LET v_edo_aclaracion = C_ESTADO_ACL_NO_PROCEDENTE
         LET v_diagnostico    = C_DIAGNOSTICO_ACL_10

      WHEN v_estado_sol = C_ESTADO_SALDO_LIQUIDADO_RECEPTORA #40
         CALL fn_valida_saldos_rec(p_id_derechohabiente) RETURNING v_edo_aclaracion,v_diagnostico

   END CASE

   CALL fn_asigna_diagnostico(p_id_solicitud_portabilidad,
                              v_origen_aclaracion,
                              v_edo_aclaracion,
                              v_tipo_aclaracion,
                              v_diagnostico)
                              
END FUNCTION

# Descripción: 
FUNCTION fn_valida_saldos_rec(p_id_derechohabiente)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_monto_acciones LIKE cta_movimiento.monto_acciones,
       v_estado      SMALLINT,
       v_diagnostico SMALLINT

   LET v_monto_acciones = 0   
   EXECUTE prp_con_mov USING p_id_derechohabiente,
                             C_SUBCUENTA_PRT,
                             C_MOV_REC_ABONO_TRASPASO,
                             C_MOV_REC_CARGO_DISP
                        INTO v_monto_acciones

   IF( v_monto_acciones > 0 )THEN # Si no se ha dispersado el traspaso receptora
      LET v_estado      = C_ESTADO_ACL_PROCEDENTE
      LET v_diagnostico = C_DIAGNOSTICO_ACL_14
   ELSE
      LET v_monto_acciones = 0
      EXECUTE prp_con_mov USING p_id_derechohabiente,
                                C_SUBCUENTA_PRT,
                                C_MOV_REC_ABONO_SUBSECUENTE,
                                C_MOV_CED_CARGO_SUBSECUENTE
                           INTO v_monto_acciones

      IF( v_monto_acciones > 0 )THEN # Si no se ha dispersado subsecuente receptora
         LET v_estado      = C_ESTADO_ACL_PROCEDENTE
         LET v_diagnostico = C_DIAGNOSTICO_ACL_15
      ELSE
         # Si los saldos del traspaso y subsecuentes en la subcuenta de portabilidad están en cero,
         # la aclaración no procede
         LET v_estado      = C_ESTADO_ACL_NO_PROCEDENTE
         LET v_diagnostico = C_DIAGNOSTICO_ACL_13
      END IF
   END IF

   RETURN v_estado,
          v_diagnostico
END FUNCTION