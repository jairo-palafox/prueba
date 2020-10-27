






CREATE PROCEDURE "safreviv".sp_prt_valida_trasp_subsec_receptora()

DEFINE v_tipo_registro    CHAR(2);
DEFINE v_f_trans_lote     DATE;
DEFINE v_f_trans_lote_tmp CHAR(8);

DEFINE v_tpo_registro_det CHAR(2);
DEFINE v_instituto_origen CHAR(3);
DEFINE v_tpo_movimiento   CHAR(2);
DEFINE v_nss              CHAR(11);
DEFINE v_paterno          CHAR(40);
DEFINE v_materno          CHAR(40);
DEFINE v_nombre           CHAR(40);
DEFINE v_curp             CHAR(18);
DEFINE v_tpo_operacion    CHAR(2);
DEFINE v_id_credito       CHAR(10);
DEFINE v_sdo_insoluto                CHAR(12);
DEFINE v_f_originacion_infonavit     DATE;
DEFINE v_f_originacion_infonavit_tmp CHAR(8);
DEFINE v_monto_pesos_fovissste       CHAR(12);
DEFINE v_f_valor_transferencia       DATE;
DEFINE v_f_valor_transferencia_tmp   CHAR(8);

DEFINE v_tipo_registro_sum   CHAR(2);
DEFINE v_total_registros     CHAR(10);
DEFINE v_mto_pesos_fov2008   CHAR(12);

DEFINE v_registro VARCHAR(213);
DEFINE v_resultado_op CHAR(2);
DEFINE v_diagnostico  CHAR(3);

DEFINE v_registros_rch DECIMAL(9,0);
DEFINE v_registro_arch DECIMAL(9,0);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   --ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
   --   -- error de conversion de fecha
   --   IF(v_error_sql = -1204 OR
   --      v_error_sql = -1205 OR
   --      v_error_sql = -1206 OR
   --      v_error_sql = -1218)THEN      
   --   END IF   
   --END EXCEPTION;
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_valida_trasp_subsec_receptora.trace';
   --TRACE ON;
   
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   SELECT tipo_registro,
          f_trans_lote
     INTO v_tipo_registro,
          v_f_trans_lote
     FROM safre_tmp:tmp_prt_cza_trasp_subsec_recep;
     
   LET v_f_trans_lote_tmp = TO_CHAR(v_f_trans_lote,'%Y%m%d');
   LET v_registro = v_tipo_registro||v_f_trans_lote_tmp;
   INSERT INTO tmp_prt_rechazados(registro) VALUES(TRIM(v_registro));
   
   LET v_registro_arch = 1;
   
   FOREACH SELECT tipo_registro,
                  instituto_origen,
                  tpo_movimiento,
                  nss,
                  paterno,
                  materno,
                  nombre,
                  curp,
                  tpo_operacion,
                  id_credito,
                  sdo_insoluto,
                  f_originacion_infonavit,
                  monto_pesos_fovissste,
                  f_valor_transferencia
             INTO v_tpo_registro_det,
                  v_instituto_origen,
                  v_tpo_movimiento,
                  v_nss,
                  v_paterno,
                  v_materno,
                  v_nombre,
                  v_curp,
                  v_tpo_operacion,
                  v_id_credito,
                  v_sdo_insoluto,
                  v_f_originacion_infonavit,
                  v_monto_pesos_fovissste,
                  v_f_valor_transferencia
             FROM safre_tmp:tmp_prt_det_trasp_subsec_recep
      
      LET v_resultado_op = "01"; -- ACEPTADO
      LET v_diagnostico = NULL;
      
      IF( LENGTH(TRIM(v_instituto_origen)) = 0 )THEN
         INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"001");
         LET v_resultado_op = "02"; -- RECHAZADO
         LET v_diagnostico = "001";
      ELSE
         IF( LENGTH(TRIM(v_nss)) = 0 )THEN
            INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"002");
            LET v_resultado_op = "02"; -- RECHAZADO
            LET v_diagnostico = "002";
         ELSE
            IF( LENGTH(TRIM(v_curp)) = 0 )THEN
               INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"003");
               LET v_resultado_op = "02"; -- RECHAZADO
               LET v_diagnostico = "003";
            ELSE
               IF( LENGTH(TRIM(v_tpo_operacion)) = 0 )THEN
                  INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"004");
                  LET v_resultado_op = "02"; -- RECHAZADO
                  LET v_diagnostico = "004";
               ELSE
                  IF( LENGTH(TRIM(v_id_credito)) = 0 )THEN
                     INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"005");
                     LET v_resultado_op = "02"; -- RECHAZADO
                     LET v_diagnostico = "005";
                  ELSE
                     IF( v_f_originacion_infonavit IS NULL)THEN
                        INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"006");
                        LET v_resultado_op = "02"; -- RECHAZADO
                        LET v_diagnostico = "006";
                     ELSE
                        IF( LENGTH(TRIM(v_sdo_insoluto)) = 0 )THEN
                           INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"007");
                           LET v_resultado_op = "02"; -- RECHAZADO
                           LET v_diagnostico = "007";
                        ELSE
                           IF( LENGTH(TRIM(v_monto_pesos_fovissste)) = 0 )THEN
                              INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"008");
                              LET v_resultado_op = "02"; -- RECHAZADO
                              LET v_diagnostico = "008";
                           ELSE
                              IF( v_f_valor_transferencia IS NULL )THEN
                                 INSERT INTO tmp_prt_inconsistencias(registro,rechazo_cod) VALUES(v_registro_arch,"009");
                                 LET v_resultado_op = "02"; -- RECHAZADO
                                 LET v_diagnostico = "009";
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF   
      END IF

      LET v_f_originacion_infonavit_tmp = TO_CHAR(v_f_originacion_infonavit,'%Y%m%d');
      LET v_f_valor_transferencia_tmp = TO_CHAR(v_f_valor_transferencia,'%Y%m%d');
      
      LET v_registro = v_tpo_registro_det||
                       v_instituto_origen||
                       v_tpo_movimiento||
                       v_nss||
                       v_paterno||
                       v_materno||
                       v_nombre||
                       v_curp||
                       v_tpo_operacion||
                       v_id_credito||
                       v_sdo_insoluto||
                       v_f_originacion_infonavit_tmp||
                       v_monto_pesos_fovissste||
                       v_f_valor_transferencia_tmp||
                       v_resultado_op||
                       v_diagnostico;
      INSERT INTO tmp_prt_rechazados(registro) VALUES(v_registro);
      
      LET v_registro_arch = v_registro_arch + 1;
             
   END FOREACH
   
   SELECT tipo_registro,
          total_registros,
          mto_pesos_fov2008
     INTO v_tipo_registro_sum,
          v_total_registros,
          v_mto_pesos_fov2008
     FROM safre_tmp:tmp_prt_sum_trasp_subsec_recep;
     
   LET v_registro = v_tipo_registro_sum||v_total_registros||v_mto_pesos_fov2008;
   INSERT INTO tmp_prt_rechazados(registro) VALUES(TRIM(v_registro));
   
   LET v_registros_rch = NULL;
   SELECT COUNT(*)
     INTO v_registros_rch
     FROM tmp_prt_inconsistencias
    WHERE 1 = 1;
   IF( v_registros_rch > 0 )THEN
      SELECT registro
        INTO v_registro
        FROM tmp_prt_rechazados
       WHERE registro MATCHES '01*';
       
      LET v_registro = TRIM(v_registro) || '02';
      
      UPDATE tmp_prt_rechazados
         SET registro = v_registro
       WHERE registro MATCHES '01*';   
   END IF
   
END PROCEDURE;


