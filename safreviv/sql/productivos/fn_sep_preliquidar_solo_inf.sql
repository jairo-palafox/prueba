






CREATE FUNCTION "safreviv".fn_sep_preliquidar_solo_inf(p_folio       DECIMAL(9,0), 
                                            p_pid         DECIMAL(9,0),
                                            p_proceso_cod SMALLINT,
                                            p_opera_cod   SMALLINT,
                                            p_usuario     CHAR(20),
                                            p_senial_expediente SMALLINT)
                                            
RETURNING INTEGER, 
          DECIMAL(16,6), 
          DECIMAL(16,6),
          INTEGER,
          CHAR(200);
          
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(200);

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);

DEFINE v_total_expedientes INTEGER;
DEFINE v_total_cargo       DECIMAL(16,6);
DEFINE v_total_abono       DECIMAL(16,6);


-- registros de saldos de cargo y abono por expediente
DEFINE v_id_expediente     DECIMAL(9,0);
DEFINE v_invadido          CHAR(11);
DEFINE v_asociado          CHAR(11);
DEFINE v_subcuenta         SMALLINT;
DEFINE v_aivs              DECIMAL(16,6);
DEFINE v_pesos             DECIMAL(12,2);
-- datos para preliquidación
DEFINE v_fecha_actual           DATE;
DEFINE v_id_derechohabiente_inv DECIMAL(9,0);
DEFINE v_id_derechohabiente_asc DECIMAL(9,0);
--DEFINE v_subcuenta_preliq       SMALLINT;
DEFINE v_fondo_inversion        SMALLINT;
DEFINE v_movimiento_abono       SMALLINT;
DEFINE v_movimiento_cargo       SMALLINT;
--DEFINE v_id_sep_movimiento_invadido DECIMAL(9,0);
DEFINE v_h_registro             DATETIME HOUR TO SECOND;
DEFINE v_valor_accion           DECIMAL(28,6);

          
   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
      LET v_total_expedientes = 0;
      LET v_total_cargo = 0;
      LET v_total_abono = 0;
      RETURN v_total_expedientes,
             v_total_cargo,
             v_total_abono,
             v_sql_error,
             v_msg_error;
   END EXCEPTION ;
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_preliquidar_solo_inf.trace';
   
   LET v_total_expedientes = 0;
   LET v_total_cargo = 0;
   LET v_total_abono = 0;
   LET v_sql_error = 0;
   LET v_msg_error = NULL;
   
   --TRACE "1.- actualiza folio";
   -- Actualiza estado del folio a preliquidado
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio
      AND proceso_cod  = p_proceso_cod
      AND opera_cod = p_opera_cod;
   
   -- información de registros de preliquidación
   -- LET v_subcuenta_preliq = 44;--viv 97 sólo inf
   LET v_fondo_inversion  = 11;
   LET v_movimiento_abono = 381;   LET v_movimiento_cargo = 382;   --TRACE "2.- ciclo de expedientes";
   FOREACH SELECT id_expediente, 
                  invadido, 
                  asociado, 
                  subcuenta,
                  SUM(aivs),
                  SUM(pesos)
             INTO v_id_expediente,
                  v_invadido,
                  v_asociado,
                  v_subcuenta,-- 44  viv 97 sólo inf
                  v_aivs,
                  v_pesos
             FROM TABLE( MULTISET(
                             SELECT mov.id_expediente, 
                                    mov.invadido, 
                                    mov.asociado, 
                                    mov.subcuenta,
                                    SUM(mov.monto_acciones) aivs,
                                    SUM(mov.monto_pesos) pesos
                               FROM sep_expediente exp JOIN sep_movimiento_invadido mov
                                 ON mov.id_expediente = exp.id_expediente
                              WHERE exp.estado = 75 -- expediente previo confirmado
                                AND mov.ind_mov_asociado = 1 -- monto a separar
                                AND mov.movimiento <> 999 -- diferente de saldo inicial
                              GROUP BY mov.id_expediente,mov.invadido,mov.asociado,mov.subcuenta
                      UNION 
                             SELECT mov.id_expediente,
                                    mov.invadido,
                                    mov.asociado,
                                    mov.subcuenta,
                                    SUM(sdo.mto_aivs_separado) aivs,
                                    SUM(mov.monto_pesos) pesos
                               FROM sep_expediente exp JOIN sep_movimiento_invadido mov
                                 ON mov.id_expediente = exp.id_expediente
                                    JOIN sep_saldo_inicial sdo
                                 ON sdo.id_sep_movimiento_invadido = mov.id_sep_movimiento_invadido
                              WHERE exp.estado = 75 -- expediente previo confirmado
                                AND mov.ind_mov_asociado = 1 -- monto a separar
                                AND mov.movimiento = 999
                              GROUP BY mov.id_expediente,mov.invadido,mov.asociado,mov.subcuenta))
            WHERE 1 = 1
            GROUP BY id_expediente,invadido,asociado,subcuenta
            
      LET v_total_cargo = v_total_cargo + v_aivs;
      LET v_total_abono = v_total_abono + v_aivs;
      --TRACE "3.- recupera id_derechohabiente";
      SELECT id_derechohabiente
        INTO v_id_derechohabiente_inv
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 1; -- invadido
      
      SELECT id_derechohabiente
        INTO v_id_derechohabiente_asc
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 2; -- asociado
         
      -- SELECT FIRST 1 id_sep_movimiento_invadido
      --   INTO v_id_sep_movimiento_invadido
      --   FROM sep_movimiento_invadido
      --  WHERE id_expediente = v_id_expediente
      --    AND invadido = v_invadido;
      
      -- recupera el valor de las acciones a la fecha indicada
      SELECT precio_fondo
        INTO v_valor_accion
        FROM glo_valor_fondo
       WHERE fondo = 11
         AND f_valuacion = TODAY; -- fecha de preliquidación
          
      LET v_fecha_actual = TODAY;
      LET v_h_registro   = CURRENT HOUR TO SECOND;
      -- cálcula el valor en persos de las acciones para la fecha
      LET v_pesos = v_aivs * v_valor_accion; -- valor considerado a la fecha de preliquidación
      -------------------------------
      -- Movimientos para el invadido
      INSERT INTO safre_viv:sep_preliquida_solo_infonavit (f_liquida         ,
                                       id_derechohabiente,
                                       subcuenta         ,
                                       fondo_inversion   ,
                                       movimiento        ,
                                       folio_liquida     ,
                                       id_referencia     ,
                                       monto_acciones    ,
                                       monto_pesos       ,
                                       f_valor           ,
                                       f_registro        ,
                                       h_registro        ,
                                       origen            )
                               VALUES (v_fecha_actual         , --f_liquida
                                       v_id_derechohabiente_inv,
                                       v_subcuenta         , -- 44
                                       v_fondo_inversion   , -- 11
                                       v_movimiento_cargo  ,
                                       p_folio             , -- folio_liquida
                                       v_id_expediente     , -- id_referencia
                                       -v_aivs              , -- monto_acciones
                                       -v_pesos             , -- monto_pesos
                                       v_fecha_actual      , -- f_valor
                                       v_fecha_actual      , -- f_valor
                                       v_h_registro        , -- h_registro
                                       v_id_expediente  );
            
      -------------------------------
      -- Movimientos para el asociado
      INSERT INTO safre_viv:sep_preliquida_solo_infonavit (f_liquida         ,
                                       id_derechohabiente,
                                       subcuenta         ,
                                       fondo_inversion   ,
                                       movimiento        ,
                                       folio_liquida     ,
                                       id_referencia     ,
                                       monto_acciones    ,
                                       monto_pesos       ,
                                       f_valor           ,
                                       f_registro        ,
                                       h_registro        ,
                                       origen            )
                               VALUES (v_fecha_actual         , --f_liquida
                                       v_id_derechohabiente_asc,
                                       v_subcuenta         , -- 44
                                       v_fondo_inversion   , -- 11
                                       v_movimiento_abono  ,
                                       p_folio             , -- folio_liquida
                                       v_id_expediente     , -- id_referencia
                                       v_aivs              , -- monto_acciones
                                       v_pesos             , -- monto_pesos
                                       v_fecha_actual      , -- f_valor
                                       v_fecha_actual      , -- f_valor
                                       v_h_registro        , -- h_registro
                                       v_id_expediente  );
      --TRACE "4.- avanza maquinaria";
      EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_expediente",
                                                v_id_expediente,
                                                "id_expediente",
                                                p_senial_expediente,
                                                p_usuario) INTO v_ind, v_diag, v_estado_destino;
      --TRACE "ind "||v_ind;
      --TRACE "v_diag "||v_diag;
      --TRACE "v_estado_destino "||v_estado_destino;
      -- realiza el conte de expedientes procesados
      LET v_total_expedientes = v_total_expedientes + 1;
   END FOREACH
                                            
   RETURN v_total_expedientes,
          v_total_cargo,
          v_total_abono,
          v_sql_error,
          v_msg_error;
END FUNCTION;


