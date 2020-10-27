






CREATE FUNCTION "safreviv".fn_mdt_preliquida_pago_instruccion(p_id_cat_mandato     SMALLINT,
                                                   p_estado             SMALLINT,
                                                   p_desc_mandato       VARCHAR(40),
                                                   p_entidad_federativa SMALLINT,
                                                   p_municipio          VARCHAR(120),
                                                   p_folio              DECIMAL(9,0),
                                                   p_id_ctr_aplica_pago_mandato DECIMAL(9,0),
                                                   p_usuario            CHAR(20))

RETURNING INTEGER,
          SMALLINT,
          CHAR(80),
          VARCHAR(40),
          DECIMAL(22,2);


DEFINE v_consulta   CHAR(1024);

DEFINE v_mdt_det_aplica_monto_id_derechohabiente DECIMAL(9,0);
DEFINE v_mdt_det_aplica_monto_sum_monto_pesos    DECIMAL(22,2);
DEFINE v_sum_monto_pesos                         DECIMAL(22,2);
DEFINE v_mdt_det_aplica_monto_sum_monto_acciones DECIMAL(22,2);
DEFINE v_mdt_ctr_mandato_id_credito              DECIMAL(10,0);
DEFINE v_mdt_det_ctr_mandato_cve_mandato         CHAR(18);
DEFINE v_mdt_det_ctr_mandato_valor_descuento_mandato DECIMAL(22,2);
DEFINE v_mdt_det_ctr_mandato_f_inicio_mandato    DATE;

DEFINE v_mdt_det_aplica_pago_mandato_caso_proceso SMALLINT;

DEFINE v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato DECIMAL(9,0);

DEFINE v_mdt_preliquida_subcuenta                 SMALLINT;
DEFINE v_mdt_preliquida_movimiento                SMALLINT;
DEFINE v_mdt_preliquida_fondo_inversion           SMALLINT;
DEFINE v_tpo_mandato                              CHAR(2);
DEFINE v_mdt_preliquida_h_registro                CHAR(8);

DEFINE v_nombre_derechohabiente  VARCHAR(50);

DEFINE v_estado_destino SMALLINT;   -- estado destino correspondiente a la señal y estado origen
DEFINE v_ind        SMALLINT;   -- idicador de error
DEFINE v_diag       CHAR(3);    -- diagnostico de error
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(80);




   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      --LET v_mdt_det_aplica_monto_sum_monto_pesos = 0;
      LET v_sum_monto_pesos = 0;
      RETURN v_sql_error, 
             v_isam_error, 
             v_msg_error,
             p_desc_mandato,
             v_sum_monto_pesos;
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_preliquida_pago_instruccion.trace';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = '';
   --LET v_mdt_det_aplica_monto_sum_monto_pesos = 0;
   LET v_sum_monto_pesos = 0;
   
   LET v_mdt_det_aplica_pago_mandato_caso_proceso = 5;
   
   LET v_mdt_preliquida_subcuenta                 = 51; -- Mandatos
   LET v_mdt_preliquida_fondo_inversion           = 10; -- Pesos
   
   -- Actualiza el folio a preliquidado
   UPDATE glo_folio 
      SET  status = 1
    WHERE  folio =  p_folio;
      

   --TRACE "0";
   FOREACH SELECT mto.id_derechohabiente,
                  ctr.id_credito,
                  det.cve_mandato,
                  det.valor_descuento_mandato,
                  det.f_inicio_mandato,
                  SUM(mto.monto_pesos),
                  SUM(mto.monto_acciones)
             INTO v_mdt_det_aplica_monto_id_derechohabiente,
                  v_mdt_ctr_mandato_id_credito,
                  v_mdt_det_ctr_mandato_cve_mandato,
                  v_mdt_det_ctr_mandato_valor_descuento_mandato,
                  v_mdt_det_ctr_mandato_f_inicio_mandato,
                  v_mdt_det_aplica_monto_sum_monto_pesos,
                  v_mdt_det_aplica_monto_sum_monto_acciones
             FROM mdt_det_aplica_monto mto JOIN mdt_det_ctr_mandato det 
               ON det.id_derechohabiente = mto.id_derechohabiente
              AND det.id_cat_mandato = mto.id_cat_mandato
                  JOIN mdt_ctr_mandato ctr
               ON ctr.id_ctr_mandato = det.id_ctr_mandato
            WHERE mto.id_cat_mandato = p_id_cat_mandato
              AND mto.estado = p_estado -- 100 abonado 
            GROUP BY 1,2,3,4,5
   
      --TRACE "1";
      LET v_sum_monto_pesos = v_sum_monto_pesos + v_mdt_det_aplica_monto_sum_monto_pesos;
      LET v_nombre_derechohabiente = "";
      FOREACH SELECT TRIM(nombre_af)||" "||
                     TRIM(ap_paterno_af)||" "||
                     TRIM(ap_materno_af)
                INTO v_nombre_derechohabiente
                FROM afi_derechohabiente afi
               WHERE id_derechohabiente = v_mdt_det_aplica_monto_id_derechohabiente
               
      END FOREACH
      --LET v_nombre_derechohabiente = "";
      
      --TRACE "2";
      LET v_consulta = "INSERT INTO mdt_det_aplica_pago_mandato"||
                       "(id_det_aplica_pago_mandato,"||
                       " id_ctr_aplica_pago_mandato,"||
                       " caso_proceso,"||
                       " id_credito,"||
                       " ent_federativa,"||
                       " clave_convenio,"||
                       " valor_descuento,"||
                       " f_inicio_mandato,"||
                       " nombre_trabajador,"||
                       " municipio,"||
                       " monto_pesos,"||
                       " f_liquida)"||
                       " VALUES"||
                       "(seq_mdt_det_aplica_pago_mandato.NEXTVAL,"||
                         p_id_ctr_aplica_pago_mandato||","||
                         v_mdt_det_aplica_pago_mandato_caso_proceso||","||
                         v_mdt_ctr_mandato_id_credito||","||
                         p_entidad_federativa||",'"||
                         v_mdt_det_ctr_mandato_cve_mandato||"',"||
                         v_mdt_det_ctr_mandato_valor_descuento_mandato||",'"||
                         v_mdt_det_ctr_mandato_f_inicio_mandato||"','"||
                         v_nombre_derechohabiente||"',"||
                         p_municipio ||","||
                         v_mdt_det_aplica_monto_sum_monto_pesos||","||
                         "TODAY)";
      EXECUTE IMMEDIATE v_consulta;
      --TRACE "3";
      SELECT FIRST 1 seq_mdt_det_aplica_pago_mandato.CURRVAL        
        INTO v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato
        FROM mdt_det_aplica_pago_mandato;
        
      LET v_tpo_mandato = v_mdt_det_ctr_mandato_cve_mandato[1,2];
      IF(v_tpo_mandato = "01")THEN -- Predial
         LET v_mdt_preliquida_movimiento = 314;
      ELSE
         IF(v_tpo_mandato = "02")THEN -- Mantenimiento
            LET v_mdt_preliquida_movimiento = 334;
         ELSE
            IF(v_tpo_mandato = "03")THEN -- Srevicios
               LET v_mdt_preliquida_movimiento = 324;
            END IF;
         END IF;      
      END IF;
      
      LET v_consulta = "INSERT INTO mdt_preliquida"||
                       "(f_liquida,"||
                       " id_derechohabiente,"||
                       " subcuenta,"||
                       " fondo_inversion,"||
                       " movimiento,"||
                       " folio_liquida,"||
                       " id_referencia,"||
                       " monto_acciones,"||
                       " monto_pesos,"||
                       " f_valor,"||
                       " f_registro,"||
                       " h_registro,"||
                       " origen)"||
                       "VALUES"||
                       "(TODAY,"||
                         v_mdt_det_aplica_monto_id_derechohabiente||","||
                         v_mdt_preliquida_subcuenta||","||
                         v_mdt_preliquida_fondo_inversion||","||
                         v_mdt_preliquida_movimiento||","||
                         p_folio||","||
                         v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato||","||
                         v_mdt_det_aplica_monto_sum_monto_acciones||","||
                         v_mdt_det_aplica_monto_sum_monto_pesos||","||
                       " TODAY,"||
                       " TODAY,"||
                       " EXTEND(CURRENT,HOUR TO SECOND),'"||
                       p_desc_mandato ||"')";
                       
     --TRACE "4";
     EXECUTE IMMEDIATE v_consulta;
     
     
     -- Actualiza mdt_det_aplica_monto para el monto preliquidado
     UPDATE mdt_det_aplica_monto
        SET id_det_aplica_pago_mandato = v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato
      WHERE id_derechohabiente = v_mdt_det_aplica_monto_id_derechohabiente
        AND id_cat_mandato = p_id_cat_mandato
        AND id_det_aplica_pago_mandato IS NULL;
        
     -- Avanza maquinaria
     EXECUTE FUNCTION fn_glo_maq_individual(1, -- Maquinaria maq_mdt_AplInstMdt
                                            v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                                            10, -- Preliquidar mandato
                                            p_usuario) 
        INTO v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   
     IF(v_ind <> 0 OR v_sql_error <> 0)THEN
        RETURN v_sql_error, 
               v_isam_error, 
               v_msg_error,
               p_desc_mandato,
               v_sum_monto_pesos;
     
     END IF;
   END FOREACH
      
   --TRACE "6";
   UPDATE STATISTICS FOR TABLE mdt_det_aplica_monto;
   UPDATE STATISTICS FOR TABLE mdt_preliquida;
   UPDATE STATISTICS FOR TABLE mdt_det_aplica_pago_mandato;
   
   LET p_desc_mandato = "";
   RETURN v_sql_error, 
          v_isam_error, 
          v_msg_error,
          p_desc_mandato,
          v_sum_monto_pesos;

END FUNCTION;


