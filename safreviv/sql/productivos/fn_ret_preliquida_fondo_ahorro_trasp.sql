






CREATE FUNCTION "safreviv".fn_ret_preliquida_fondo_ahorro_trasp(v_folio_liquida    DECIMAL(10,0),
                                                             v_proceso_cod      SMALLINT,
                                                             v_opera_cod        SMALLINT,
                                                             v_usuario_cod      VARCHAR(20),
                                                             v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)


   DEFINE v_nss                        CHAR(11);
   DEFINE v_rfc                        CHAR(13);
   DEFINE v_id_solicitud               DECIMAL(9,0);
   DEFINE v_imp_solicitado             DECIMAL(14,2);
   DEFINE v_imp_sol_paso               DECIMAL(14,2);
   DEFINE v_saldo                      DECIMAL(14,2);
   DEFINE v_query                      CHAR(500);
   DEFINE v_query_where                CHAR(80);


   DEFINE  v_b_paso                      SMALLINT;
   DEFINE  v_id_afi_fondo72              DECIMAL(9,0);
   DEFINE  v_id_afi_fondo72_temp         DECIMAL(9,0);
   DEFINE  v_id_derechohabiente          DECIMAL(9,0);
   DEFINE  v_importe                     DECIMAL(14,2);   
   DEFINE  v_saldo_del_trabajador        DECIMAL(14,2); -- saldo del trabajador en cta_fondo72
   DEFINE  v_tanto_adicional_solicitud   DECIMAL(14,2); -- tanto adicional de la solicitud
   DEFINE  v_saldo_viv72_solicitado      DECIMAL(14,2);
   DEFINE  p_saldo_viv72_solicitado      DECIMAL(14,2);
   DEFINE  v_saldo_viv72_solicitado_orig DECIMAL(14,2);
   DEFINE  v_saldo_viv72                 DECIMAL(14,2);
   DEFINE  v_id_afi_saldo_viv72          DECIMAL(14,2);
   DEFINE  v_saldo_diferencia            DECIMAL(14,2);
   DEFINE  v_movimiento_fondo_ahorro     SMALLINT;
   DEFINE  v_movimiento_tanto_adicional  SMALLINT;
   DEFINE  v_movimiento_sobregiro        SMALLINT;
   DEFINE  v_valor_mov                   SMALLINT;
   DEFINE  v_origen                      CHAR(20);
   DEFINE  v_subcuenta                   SMALLINT;
   DEFINE  v_conteo                 SMALLINT; -- conteo de coincidencias de solicitante
                                    
   DEFINE  v_resultado_consulta     SMALLINT;
                                    
   DEFINE v_i_estado_marca          INTEGER;
   DEFINE v_marca_fondo_ahorro      INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_bnd_preli               SMALLINT;
   DEFINE r_tanto_adicional         DECIMAL(14,2);
   DEFINE v_estado_solicitud        INTEGER;  
   
   DEFINE v_bnd_saldo_igual         SMALLINT;
   DEFINE v_bnd_saldo_id_igual      SMALLINT;
   DEFINE v_bnd_saldo_id_diferencia SMALLINT;
   
   -- banderas para controlar de donde se obtendra el recurso economico
   -- para el retiro
   DEFINE v_el_saldo_es_igual      SMALLINT;
   DEFINE v_saldo_entre_varios_afi SMALLINT;
   DEFINE v_saldo_con_sobregiro    SMALLINT;

   DEFINE p_id_afi_fondo72_temp    DECIMAL(9,0);
   DEFINE p_subcuenta              SMALLINT;
   DEFINE p_movimiento             SMALLINT;
   DEFINE p_folio                  DECIMAL(9,0);
   DEFINE p_id_solicitud           DECIMAL(9,0);
   DEFINE p_pes_viv72              DECIMAL(14,2);
   DEFINE p_tanto_adicional        DECIMAL(14,2);
   DEFINE p_saldo_diferencia       DECIMAL(14,2);  
   DEFINE v_saldo_viv72_activo     DECIMAL(14,2);

   -- Control de Excepciones
   DEFINE v_si_resultado          SMALLINT;
   DEFINE sql_err                 INTEGER;
   DEFINE isam_err                INTEGER;
   DEFINE err_txt                 VARCHAR(250);
   DEFINE v_c_msj                 VARCHAR(250);

      -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, v_id_solicitud;
   END EXCEPTION

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro         = 802; -- marca para retiro de fondo ahorro
   LET v_movimiento_fondo_ahorro    = 182;
   LET v_movimiento_tanto_adicional = 422;
   LET v_movimiento_sobregiro       = 802;
   LET v_origen                     = "RETIRO W";
   LET v_subcuenta                  = 40;
   LET v_estado_solicitud           = 15; -- ACEPTADAS

   LET v_i_estado_marca          = 0;
   LET v_bnd_preli               = 1;
   LET v_b_paso                  = 0;
   LET v_bnd_saldo_igual         = 0;
   LET v_bnd_saldo_id_igual      = 0;
   LET v_bnd_saldo_id_diferencia = 0;    
   
   LET v_importe                 = 0;
   LET v_saldo_viv72_activo      = 0;
   LET v_id_afi_fondo72          = 0;
   LET v_id_afi_fondo72_temp     = 0;
   LET v_id_derechohabiente      = 0;
   LET v_id_solicitud            = 0;
   LET v_saldo_del_trabajador    = 0; -- saldo que el trabajador tiene
   LET v_resultado_consulta      = 0;
   LET v_saldo_viv72_solicitado  = 0;
   LET p_saldo_viv72_solicitado  = 0;
   LET v_saldo_viv72_solicitado_orig  = 0;
   LET v_saldo_viv72             = 0; 
   LET v_id_afi_saldo_viv72      = 0; 
   LET v_saldo_diferencia        = 0;   
   
   LET p_id_afi_fondo72_temp     = 0;
   LET p_subcuenta               = 0;
   LET p_movimiento              = 0;
   LET p_folio                   = 0;
   LET p_id_solicitud            = 0;
   LET p_pes_viv72               = 0;
   LET p_tanto_adicional         = 0;
   LET p_saldo_diferencia        = 0;

   -- se inician las banderas del recurso economico   
   LET v_el_saldo_es_igual      = 0;
   LET v_saldo_entre_varios_afi = 0;
   LET v_saldo_con_sobregiro    = 0;

   -- se asume que el proceso termina bien
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso finalizó exitosamente.';

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ret_preliquida_fondo_ahorro_trasp.log';

   -- actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status       = 1 -- preliquidado
   WHERE  folio        = v_folio_liquida
   AND    proceso_cod  = v_proceso_cod
   AND    status       = 0;

   --actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;
   
   -- se obtiene el signo del movimiento de retiro fondo de ahorro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_fondo_ahorro ;
   DROP TABLE IF EXISTS tmp_ret_fondo_ahorro_trasp;
   CREATE TABLE tmp_ret_fondo_ahorro_trasp (
           id_solicitud       DECIMAL(9,0),
           nss                CHAR(11),
           rfc                CHAR(13),
           imp_solicitado     DECIMAL(14,2),
           id_afi_fondo72     DECIMAL(9,0),
           id_derechohabiente DECIMAL(9,0));
          
   -- se leen las solicitudes aprobadas para su preliquidacion
   FOREACH
      SELECT nss, rfc, id_solicitud, saldo
      INTO   v_nss, v_rfc, v_id_solicitud, v_imp_solicitado
      FROM   ret_fondo_ahorro_trasp
      WHERE  folio = v_folio_liquida
      AND    estado_solicitud = v_estado_solicitud
      AND    saldo > 0 

      LET v_query = "INSERT INTO tmp_ret_fondo_ahorro_trasp "||
              " SELECT "|| v_id_solicitud|| ", '"|| v_nss|| 
              "', '"|| v_rfc|| "', "|| v_imp_solicitado|| 
              ", id_afi_fondo72, id_derechohabiente "||
              " FROM  afi_fondo72 ";
      -- se verifica cuantas veces aparece el solicitante en la tabla de coincidencias
      SELECT COUNT(*)
      INTO   v_conteo
      FROM   afi_fondo72
      WHERE  nss = v_nss
      AND    rfc = v_rfc;

      IF v_conteo > 0 THEN 
         LET v_query_where = " WHERE nss = '"|| v_nss|| "' "||
                             " AND   rfc = '"|| v_rfc|| "' ";
      ELSE 
         SELECT COUNT(*)
         INTO   v_conteo
         FROM   afi_fondo72
         WHERE  rfc = v_rfc;
         IF v_conteo > 0 THEN 
            LET v_query_where = " WHERE rfc = '"|| v_rfc|| "' ";
         ELSE 
            SELECT COUNT(*)
            INTO   v_conteo
            FROM   afi_fondo72
            WHERE  nss = v_nss;
            IF v_conteo > 0 THEN 
               LET v_query_where = " WHERE nss = '"|| v_nss|| "' ";
            END IF
         END IF
      END IF
      LET v_query = TRIM(v_query)|| " "|| TRIM(v_query_where);
      --PREPARE sid_inserta FROM v_query
      EXECUTE IMMEDIATE v_query;
      --FREE sid_inserta;
   END FOREACH;

   trace "Paso el primer ciclo";
   DROP TABLE IF EXISTS tmp_ret_fondo_ahorro_trasp_saldos;
   CREATE TABLE tmp_ret_fondo_ahorro_trasp_saldos (
           id_solicitud       DECIMAL(9,0),
           nss                CHAR(11),
           rfc                CHAR(13),
           imp_solicitado     DECIMAL(14,2),
           id_afi_fondo72     DECIMAL(9,0),
           id_derechohabiente DECIMAL(9,0),
           saldo              DECIMAL(14,2));
   trace "Creo la tabla temporal tmp_ret_fondo_ahorro_trasp_saldos";
   FOREACH
      SELECT id_solicitud, nss, rfc, imp_solicitado, id_afi_fondo72, id_derechohabiente
      INTO   v_id_solicitud, v_nss, v_rfc, v_imp_solicitado, v_id_afi_fondo72, v_id_derechohabiente
      FROM   tmp_ret_fondo_ahorro_trasp
      ORDER BY id_solicitud, id_afi_fondo72

      SELECT SUM(importe) 
      INTO   v_saldo
      FROM  cta_fondo72 
      WHERE id_afi_fondo72 = v_id_afi_fondo72
      AND   (movimiento <> 422 and movimiento <> 601);

      INSERT INTO tmp_ret_fondo_ahorro_trasp_saldos
             VALUES (v_id_solicitud, v_nss, v_rfc, 
                     v_imp_solicitado, v_id_afi_fondo72, 
                     v_id_derechohabiente, v_saldo);
   END FOREACH;

   trace "Se obtuvieron los saldos";
   
   FOREACH
      SELECT DISTINCT id_solicitud, nss, rfc, imp_solicitado
      INTO   v_id_solicitud, v_nss, v_rfc, v_imp_solicitado
      FROM   tmp_ret_fondo_ahorro_trasp_saldos
      ORDER BY id_solicitud

      trace "Insertando en ret_preliquida72";
      
      LET v_imp_sol_paso = v_imp_solicitado;
      LET v_bnd_preli = 1;

      FOREACH
         SELECT id_afi_fondo72, saldo
         INTO   v_id_afi_fondo72, v_saldo
         FROM   tmp_ret_fondo_ahorro_trasp_saldos
         WHERE  id_solicitud = v_id_solicitud
         AND    saldo > 0
         ORDER BY saldo

         IF v_imp_sol_paso >= v_saldo THEN 
            INSERT INTO ret_preliquida72 (id_afi_fondo72, f_liquida, subcuenta, movimiento, folio_liquida,
                                          id_referencia, importe, f_registro, h_registro, origen)
                        VALUES (v_id_afi_fondo72, today, 40, v_movimiento_fondo_ahorro, v_folio_liquida, 
                                v_id_solicitud,v_saldo * v_valor_mov, today, CURRENT, "Trasp FA");
            LET v_imp_sol_paso = v_imp_sol_paso - v_saldo;
            IF v_imp_sol_paso = 0 THEN 
               EXIT FOREACH;
            END IF
         ELSE 
            INSERT INTO ret_preliquida72 (id_afi_fondo72, f_liquida, subcuenta, movimiento, folio_liquida,
                                          id_referencia, importe, f_registro, h_registro, origen)
                        VALUES (v_id_afi_fondo72, today, 40, v_movimiento_fondo_ahorro, v_folio_liquida, 
                                v_id_solicitud,v_imp_sol_paso * v_valor_mov, today, CURRENT, "Trasp FA");
            LET v_imp_sol_paso = 0;
            EXIT FOREACH;
         END IF 
      END FOREACH;
      IF v_imp_sol_paso > 0 THEN 
         FOREACH
            SELECT id_afi_fondo72, saldo
            INTO   v_id_afi_fondo72, v_saldo
            FROM   tmp_ret_fondo_ahorro_trasp_saldos
            WHERE  id_solicitud = v_id_solicitud
            --AND    saldo > 0
            ORDER BY saldo DESC
            INSERT INTO ret_preliquida72 (id_afi_fondo72, f_liquida, subcuenta, movimiento, folio_liquida,
                                          id_referencia, importe, f_registro, h_registro, origen)
                        VALUES (v_id_afi_fondo72, today, 40, v_movimiento_sobregiro, v_folio_liquida, 
                                v_id_solicitud,v_imp_sol_paso * v_valor_mov, today, CURRENT, "Trasp FA");
            EXIT FOREACH;
         END FOREACH;
      END IF 
         
   END FOREACH;   

   UPDATE ret_fondo_ahorro_trasp
   SET    estado_solicitud = 50
   WHERE  folio = v_folio_liquida
   AND    estado_solicitud = v_estado_solicitud;
    
   IF ( v_bnd_preli = 0 ) THEN
      -- no se preliquidaron registros
      LET v_si_resultado = 1000;
      LET v_c_msj        = "No se preliquidaron registros. Favor de verificar";
   END IF

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida72;
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, v_id_solicitud;
END FUNCTION;


