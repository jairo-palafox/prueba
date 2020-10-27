






CREATE FUNCTION "safreviv".fn_prt_preliq_trasp_subsec_receptora(p_folio         DECIMAL(9,0),
                                                     p_usuario_cod   CHAR(20))
RETURNING INTEGER, 
          CHAR(254), 
          INTEGER,
          DECIMAL(22,2),
          DECIMAL(22,2);

DEFINE v_id_prt_traspaso_receptora  DECIMAL(9,0);
DEFINE v_id_prt_solicitud_receptora DECIMAL(9,0);
DEFINE v_folio_procesar             CHAR(50);
DEFINE v_instituto_origen           CHAR(3);
DEFINE v_tpo_movimiento             CHAR(2);
DEFINE v_id_derechohabiente         DECIMAL(9,0);
DEFINE v_nss                        CHAR(11);
DEFINE v_ap_paterno                 CHAR(8);
DEFINE v_ap_materno                 CHAR(8);
DEFINE v_nombre                     CHAR(8);
DEFINE v_curp                       CHAR(18);
DEFINE v_tpo_operacion              CHAR(2);
DEFINE v_id_credito_infonavit       DECIMAL(10,0);
DEFINE v_sdo_insoluto_infonavit     DECIMAL(22,2);
DEFINE v_f_originacion_infonavit    DATE;
DEFINE v_precio_aiv_fov2008         DECIMAL(15,6);
DEFINE v_f_precio_aiv_fov2008       DATE;
DEFINE v_mto_aivs_fov2008           DECIMAL(22,2);
DEFINE v_mto_pesos_fov2008          DECIMAL(22,2);
DEFINE v_f_valor_transferencia      DATE;
DEFINE v_diag_procesar              CHAR(3);
DEFINE v_estado                     SMALLINT;
DEFINE v_usuario                    CHAR(20);

DEFINE v_registros_preliquidados INTEGER;
DEFINE v_total_abonos            DECIMAL(22,2);
DEFINE v_total_saldo_insoluto    DECIMAL(22,2);

-- Variables para movimientos de saldos
DEFINE v_subcuenta_prt   SMALLINT;
DEFINE v_abono_traspaso  SMALLINT;
DEFINE v_cargo_traspaso  SMALLINT;
DEFINE v_fondo_inversion SMALLINT;

DEFINE v_hora_registro   DATETIME HOUR TO SECOND;
DEFINE r_aivs_preliquida DECIMAL(18,2);

DEFINE v_error_sql      INTEGER ;
DEFINE v_error_isam     INTEGER ;
DEFINE v_msg_error      CHAR(254);

DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);
DEFINE v_estado_destino SMALLINT;
DEFINE v_fecha_1er_natural DATE;
DEFINE v_fecha_actual      DATE;

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_error
                    
      LET v_registros_preliquidados = 0;
      LET v_total_abonos         = 0.0;
      LET v_total_saldo_insoluto = 0.0;
   
      RETURN v_error_sql,
             v_msg_error,
             v_registros_preliquidados,
             v_total_abonos,
             v_total_saldo_insoluto;
   END EXCEPTION ;
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_prt_preliq_trasp_subsec_receptora.trace';
   --TRACE ON;

   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_error  = "";
   
   LET v_registros_preliquidados = 0;
   LET v_total_abonos         = 0.0;
   LET v_total_saldo_insoluto = 0.0;
   
   LET v_subcuenta_prt  = 60; -- Subcuenta de portabilidad
   LET v_abono_traspaso = 1603; -- Movimiento de Abono por traspaso de saldos subsecuentes receptora
 
   LET v_fondo_inversion = 10;

   -- Recupera primer día natural del siguiente mes    
   LET v_fecha_1er_natural = ADD_MONTHS(MDY(MONTH(TODAY),'01',YEAR(TODAY)),1);
   LET v_fecha_actual = TODAY;

   UPDATE glo_folio 
      SET status = 1 -- Preliquidado
    WHERE folio = p_folio;
      
   UPDATE prt_cza_receptora
      SET estado = 20 -- Folio preliquidado
    WHERE folio_liquida = p_folio;
   
   FOREACH SELECT id_prt_traspaso_receptora,
                  id_prt_solicitud_receptora,
                  id_derechohabiente,
                  sdo_insoluto_infonavit,
                  mto_pesos_fov2008
             INTO v_id_prt_traspaso_receptora,
                  v_id_prt_solicitud_receptora,
                  v_id_derechohabiente,
                  v_sdo_insoluto_infonavit,
                  v_mto_pesos_fov2008
             FROM prt_traspaso_receptora
            WHERE folio_liquida = p_folio

      EXECUTE FUNCTION fn_consulta_precio_fondo(v_mto_pesos_fov2008,
                                                v_fecha_1er_natural,
                                                v_fondo_inversion) INTO r_aivs_preliquida;
      
      LET v_hora_registro = CURRENT HOUR TO SECOND;
       
      INSERT INTO prt_preliquida(f_liquida,
                                 id_derechohabiente,
                                 subcuenta,
                                 fondo_inversion,
                                 movimiento,
                                 folio_liquida,
                                 id_referencia,
                                 monto_acciones,
                                 monto_pesos,
                                 f_valor,
                                 f_registro,
                                 h_registro,
                                 origen)
                          VALUES(v_fecha_actual,
                                 v_id_derechohabiente,
                                 v_subcuenta_prt,
                                 v_fondo_inversion,
                                 v_abono_traspaso,
                                 p_folio,
                                 v_id_prt_traspaso_receptora,
                                 r_aivs_preliquida,
                                 v_mto_pesos_fov2008,
                                 v_fecha_1er_natural,
                                 v_fecha_actual,
                                 v_hora_registro,
                                 v_id_prt_solicitud_receptora);
                                          
      LET v_registros_preliquidados = v_registros_preliquidados + 1;
      
      UPDATE prt_traspaso_receptora
         SET estado = 20 -- Preliquidado
       WHERE id_prt_traspaso_receptora = v_id_prt_traspaso_receptora;   
       
      LET v_total_abonos         = v_total_abonos + v_mto_pesos_fov2008;
      LET v_total_saldo_insoluto = v_total_saldo_insoluto + v_sdo_insoluto_infonavit;
      
   END FOREACH
   
   RETURN v_error_sql,
          v_msg_error,
          v_registros_preliquidados,
          v_total_abonos,
          v_total_saldo_insoluto;

END FUNCTION;


