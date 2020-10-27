






CREATE FUNCTION "safreviv".fn_prt_integra_traspaso_sdo_receptora(p_usuario     CHAR(20),
                                                      p_pid         DECIMAL(9,0),
                                                      p_proceso_cod SMALLINT,
                                                      p_folio       DECIMAL(10,0), 
                                                      p_archivo     CHAR(40),
                                                      p_estado_solicitud_ini SMALLINT,
                                                      p_estado_solicitud_fin SMALLINT)
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          INTEGER,
          INTEGER;
          
DEFINE v_error_sql     INTEGER;
DEFINE v_error_isam    INTEGER;
DEFINE v_msg_sql       CHAR(254);

DEFINE v_tpo_registro_cza CHAR(2);
DEFINE v_tipo_traspaso    CHAR(2);
DEFINE v_f_trans_lote     DATE;
DEFINE v_f_trans_lote_tmp CHAR(8);

--DEFINE v_monto_aivs_tmp   DECIMAL(10,2);
DEFINE v_monto_pesos_tmp  DECIMAL(10,2);

DEFINE v_tpo_registro_det CHAR(2);
DEFINE v_folio_procesar   CHAR(50);
DEFINE v_instituto_origen CHAR(3);
DEFINE v_tpo_movimiento   CHAR(2);
DEFINE v_nss              CHAR(11);
DEFINE v_paterno          CHAR(40);
DEFINE v_materno          CHAR(40);
DEFINE v_nombre           CHAR(40);
DEFINE v_curp             CHAR(18);
DEFINE v_tpo_operacion    CHAR(2);
DEFINE v_id_credito       CHAR(10);
DEFINE v_id_credito_tmp   DECIMAL(10,0);
DEFINE v_sdo_insoluto     CHAR(12);
DEFINE v_sdo_insoluto_tmp DECIMAL(10,2);
--DEFINE v_sdo_insoluto_his DECIMAL(10,2);
DEFINE v_f_originacion_infonavit DATE;
DEFINE v_f_originacion_infonavit_tmp CHAR(8);
--DEFINE v_precio_aiv_fov2008        CHAR(15);
--DEFINE v_precio_aiv_fov2008_tmp    DECIMAL(9,6);
--DEFINE v_f_precio_aiv_fov2008      DATE;
--DEFINE v_f_precio_aiv_fov2008_tmp  CHAR(8);
--DEFINE v_monto_aivs_fovissste      CHAR(12);
DEFINE v_monto_pesos_fovissste     CHAR(12);
DEFINE v_f_valor_transferencia     DATE;
DEFINE v_f_valor_transferencia_tmp CHAR(8);
DEFINE v_resultado_op              CHAR(2);

DEFINE v_tipo_registro_sum CHAR(2);
DEFINE v_total_registros   CHAR(10);
DEFINE v_mto_aivs_fov2008  CHAR(12);
DEFINE v_mto_pesos_fov2008 CHAR(12);

DEFINE v_id_prt_solicitud_receptora DECIMAL(9,0);
DEFINE v_id_derechoabiente          DECIMAL(9,0);
DEFINE v_existe     SMALLINT;
DEFINE v_estado     SMALLINT;
DEFINE v_estado_cza SMALLINT;
DEFINE v_registro   VARCHAR(213);

DEFINE v_total_procesados INTEGER;
DEFINE v_total_rechazados INTEGER;
DEFINE v_rechaza_registro SMALLINT;

DEFINE v_motivo_rechazo CHAR(3);
DEFINE v_cambio_pesos   INTEGER;

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql  
                    
      LET v_total_procesados = 0;
      LET v_total_rechazados = 0;

      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_total_procesados,
             v_total_rechazados;
             
   END EXCEPTION;
   
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_prt_integra_traspaso_sdo_receptora.trace";
   --TRACE ON;
      
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   LET v_total_procesados = 0;
   LET v_total_rechazados = 0;
   LET v_cambio_pesos = 100;
   
   LET v_estado_cza = 10; -- Lote integrado
   
   LET v_tipo_traspaso = '01'; -- TRANSFERENCIA DE SALDO PORTABILIDAD
   
   UPDATE glo_ctr_archivo
      SET estado = 2, -- Integrado
          folio = p_folio
    WHERE nombre_archivo = p_archivo
      AND estado = 1;
    
   --UPDATE glo_folio
   --   SET status = 1 -- Integrado
   -- WHERE folio = p_folio;

   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod; 
      
   UPDATE bat_ctr_proceso
      SET folio       = p_folio
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod;
      
   SELECT tipo_registro,
          f_trans_lote
     INTO v_tpo_registro_cza,
          v_f_trans_lote
     FROM safre_tmp:tmp_prt_cza_traspaso_receptora;
   
   INSERT INTO prt_cza_receptora
   (folio_liquida,
    tipo_traspaso,
    fecha_presentacion,
    estado)
   VALUES(
    p_folio,
    v_tipo_traspaso,
    v_f_trans_lote,
    v_estado_cza);
    
   LET v_f_trans_lote_tmp = TO_CHAR(v_f_trans_lote,'%Y%m%d');   
   LET v_registro = v_tpo_registro_cza||v_f_trans_lote_tmp;
   INSERT INTO tmp_prt_rechazados(registro) VALUES(TRIM(v_registro));
   
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
             FROM safre_tmp:tmp_prt_det_traspaso_receptora
             
      LET v_rechaza_registro = 0;
      LET v_motivo_rechazo = "";
      LET v_id_prt_solicitud_receptora = 0;      
      LET v_id_derechoabiente = NULL;
      
      -- Recupera id_derechohabiente
      SELECT FIRST 1 id_derechohabiente
        INTO v_id_derechoabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;
      IF( v_id_derechoabiente IS NULL)THEN
         LET v_rechaza_registro = 1;
         LET v_motivo_rechazo = "014";
      ELSE
         -- Verifica si existe la solicitud para el nss recibido
         SELECT MAX( NVL(id_prt_solicitud_receptora,0))
           INTO v_id_prt_solicitud_receptora
           FROM prt_solicitud_receptora
          WHERE nss = v_nss
            AND estado BETWEEN p_estado_solicitud_ini AND p_estado_solicitud_fin; -- Solicitud marcada en fovissste o estado posterior
            
         IF( v_id_prt_solicitud_receptora IS NULL OR v_id_prt_solicitud_receptora = 0 )THEN
            LET v_id_prt_solicitud_receptora = 0;
            LET v_rechaza_registro = 1;
            LET v_motivo_rechazo = "011";
         ELSE
            LET v_existe = NULL;
            SELECT 1
              INTO v_existe
              FROM prt_cat_rch_traspasos
             WHERE rechazo_cod = v_tpo_operacion;
            
            IF NOT( v_existe )THEN
               LET v_rechaza_registro = 1;
               LET v_motivo_rechazo = "012";
            ELSE
               -- Valida saldo insoluto
               --SELECT NVL(saldo_insoluto_credito_infonavit,0)
               --  INTO v_sdo_insoluto_his
               --  FROM prt_solicitud_receptora
               -- WHERE id_prt_solicitud_receptora = v_id_prt_solicitud_receptora;
               
               --LET v_sdo_insoluto_tmp = v_sdo_insoluto[1,10]||'.'||v_sdo_insoluto[11,12];
               
               --IF( v_sdo_insoluto_his <> v_sdo_insoluto_tmp )THEN
               --   LET v_rechaza_registro = 1;
               --   LET v_motivo_rechazo = "SALDO INSOLUTO NO COINCIDE CON EL SALDO REGISTRADO";
               --END IF
               
               -- Valida monto traspaso mayor a cero
               IF NOT( v_monto_pesos_fovissste > 0 )THEN
                  LET v_rechaza_registro = 1;
                  LET v_motivo_rechazo = "013";
               END IF
            END IF
         END IF
      END IF
      
      LET v_monto_pesos_tmp             = v_monto_pesos_fovissste/v_cambio_pesos;
      LET v_sdo_insoluto_tmp            = v_sdo_insoluto/v_cambio_pesos;
      LET v_f_originacion_infonavit_tmp = TO_CHAR(v_f_originacion_infonavit,'%Y%m%d');
      LET v_f_valor_transferencia_tmp   = TO_CHAR(v_f_valor_transferencia,'%Y%m%d');

      IF( v_rechaza_registro = 1)THEN
         LET v_total_rechazados = v_total_rechazados + 1;
         LET v_resultado_op = "02";
         LET v_estado = 5; -- Rechazado
         INSERT INTO safre_tmp:tmp_prt_integrados VALUES(v_tpo_operacion,v_nss,1,v_monto_pesos_tmp,v_motivo_rechazo);
      ELSE
         LET v_estado = 0; -- Inicial o registrado
         LET v_resultado_op = "01";
         INSERT INTO safre_tmp:tmp_prt_integrados VALUES(v_tpo_operacion,v_nss,0,v_monto_pesos_tmp,v_motivo_rechazo);
      END IF
      
      -- En caso de rechazar lote los registros se van almzacenano para generar archivo de rechazos
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
                       v_motivo_rechazo;
      INSERT INTO tmp_prt_rechazados(registro) VALUES(v_registro);
      
      LET v_total_procesados = v_total_procesados + 1;
      
      INSERT INTO prt_traspaso_receptora
      (id_prt_traspaso_receptora,
       id_prt_solicitud_receptora,
       folio_liquida,
       instituto_origen,
       tpo_movimiento,
       id_derechohabiente,
       nss,
       ap_paterno,
       ap_materno,
       nombre,
       curp,
       tpo_operacion,
       id_credito_infonavit,
       sdo_insoluto_infonavit,
       f_originacion_infonavit,
       mto_pesos_fov2008,
       f_valor_transferencia,
       estado,
       usuario)
      VALUES(
       seq_prt_traspaso_receptora.NEXTVAL,
       v_id_prt_solicitud_receptora,
       p_folio,
       v_instituto_origen,
       v_tpo_movimiento,
       v_id_derechoabiente,
       v_nss,
       v_paterno,
       v_materno,
       v_nombre,
       v_curp,
       v_tpo_operacion,
       v_id_credito,
       v_sdo_insoluto_tmp,
       v_f_originacion_infonavit,
       v_monto_pesos_tmp,
       v_f_valor_transferencia,
       v_estado,
       p_usuario);
   
   END FOREACH
   
   SELECT tipo_registro,
          total_registros,
          mto_pesos_fov2008
     INTO v_tipo_registro_sum,
          v_total_registros,
          v_mto_pesos_fov2008
     FROM safre_tmp:tmp_prt_sum_traspaso_receptora;
   
   LET v_monto_pesos_tmp = v_mto_pesos_fov2008/v_cambio_pesos;
     
   INSERT INTO prt_sum_receptora
   (folio_liquida,
    total_registros,
    mto_pesos_fov2008)
   VALUES(
    p_folio,
    v_total_registros,
    v_monto_pesos_tmp);   
   
   LET v_registro = v_tipo_registro_sum||v_total_registros||v_mto_pesos_fov2008;
   INSERT INTO tmp_prt_rechazados(registro) VALUES(v_registro);
   
   IF( v_total_rechazados > 0 )THEN
      -- Si existe un registro rechazado, se rechaza todo el lote
      UPDATE prt_cza_receptora
         SET estado = 5
       WHERE folio_liquida = p_folio;
       
      UPDATE prt_traspaso_receptora
         SET estado = 5  -- Rechazada
       WHERE folio_liquida = p_folio; -- Ingresada
       
     SELECT registro
       INTO v_registro
       FROM tmp_prt_rechazados
      WHERE registro MATCHES '01*'; 
     
     LET v_registro = TRIM(v_registro) || '02';
     
      UPDATE tmp_prt_rechazados
         SET registro = v_registro
       WHERE registro MATCHES '01*'; 
      
   ELSE
      UPDATE prt_traspaso_receptora
         SET estado = 10   -- Ingresada
       WHERE folio_liquida = p_folio; -- Integrada
   END IF
   
   RETURN v_error_sql,
          v_error_isam,
          v_msg_sql,
          v_total_procesados,
          v_total_rechazados;

END FUNCTION;


