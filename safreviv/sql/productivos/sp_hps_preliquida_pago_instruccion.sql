






CREATE FUNCTION "safreviv".sp_hps_preliquida_pago_instruccion(p_id_cat_mandato     SMALLINT,
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

--=================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-03-2015
-- MODULO : HPS
-- Sistema: SAFRE (SACI) INFONAVIT
-- Autor: Jesus David Yañez Moreno
-- Preliquida los pagos hacia las entidades receptoras de mandatos
-- de acuerdo a las instrucciones de pago registradas en el
-- catalgo de instrucciones de mandatos (trabajador)
--=================================================================

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

DEFINE v_mdt_preliquida_movimiento                SMALLINT;
DEFINE v_mdt_preliquida_fondo_inversion           SMALLINT;
DEFINE v_tpo_mandato                              CHAR(2);
DEFINE v_mdt_preliquida_h_registro                CHAR(8);
DEFINE v_hps_id_cat_pago_servicio                 DECIMAL(9,0);
DEFINE v_hps_id_cat_mandato                       DECIMAL(9,0);
DEFINE v_hps_subcuenta                            SMALLINT;
DEFINE v_hps_movimiento                           SMALLINT;

DEFINE v_hps_sol_pag_f_primer_pago_predial        DATE;
DEFINE v_hps_sol_pag_mto_primer_pago_predial      DECIMAL(22,2);
DEFINE v_hps_sol_pag_aivs_primer_pago_predial      DECIMAL(22,2);
DEFINE v_hps_sol_pag_f_primer_pago_conservacion   DATE;
DEFINE v_hps_sol_pag_mto_primer_pago_conservacion  DECIMAL(22,2);
DEFINE v_hps_sol_pag_aivs_primer_pago_conservacion DECIMAL(22,2);

DEFINE v_fecha_actual DATE;

DEFINE v_nombre_derechohabiente  VARCHAR(50);
DEFINE v_nss                     CHAR(11);
DEFINE v_movimiento_valido       SMALLINT;

DEFINE v_estado_destino SMALLINT;   -- estado destino correspondiente a la señal y estado origen
DEFINE v_ind        SMALLINT;   -- idicador de error
DEFINE v_diag       CHAR(3);    -- diagnostico de error
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  VARCHAR(200);
DEFINE v_existe_reg SMALLINT;


   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      --LET v_mdt_det_aplica_monto_sum_monto_pesos = 0;
      LET v_sum_monto_pesos = 0;
      RETURN v_sql_error,
             v_isam_error,
             v_msg_error,
             p_desc_mandato,
             v_sum_monto_pesos;
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_hps_preliquida_pago_instruccion.trace';
   SET DEBUG FILE TO '/safreviv_int/BD/sp_hps_preliquida_pago_instruccion.trace';
   TRACE ON;
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = '';
   --LET v_mdt_det_aplica_monto_sum_monto_pesos = 0;
   LET v_sum_monto_pesos = 0;

   LET v_mdt_det_aplica_pago_mandato_caso_proceso = 5;

   LET v_mdt_preliquida_fondo_inversion           = 11;

   -- Actualiza el folio a preliquidado
   UPDATE glo_folio
      SET  status = 1
    WHERE  folio =  p_folio;
 
  --TRACE "p_id_cat_mandato:"||p_id_cat_mandato;
 
  FOREACH SELECT  mto.id_cat_pago_servicio ,
                  mto.id_derechohabiente,
                  det.id_credito,
                  mto.cve_mandato,
                  mto.valor_descuento_servicio,
                  mto.f_inicio,
                  mto.valor_descuento_servicio, -- pesos
                  fn_consulta_precio_fondo(mto.valor_descuento_servicio,today,11), --acciones,
                  mto.id_cat_mandato,
                  mto.scta_descuento ,
                  mto.movimiento,
                  det.f_primer_pago_predial,
                  det.mto_primer_pago_predial,
                  fn_consulta_precio_fondo(det.mto_primer_pago_predial,today,11), --acciones,
                  det.f_primer_pago_conservacion,
                  det.mto_primer_pago_conservacion,
                  fn_consulta_precio_fondo(det.mto_primer_pago_conservacion,today,11) --acciones,
             INTO v_hps_id_cat_pago_servicio ,
                  v_mdt_det_aplica_monto_id_derechohabiente,
                  v_mdt_ctr_mandato_id_credito,
                  v_mdt_det_ctr_mandato_cve_mandato,
                  v_mdt_det_ctr_mandato_valor_descuento_mandato,
                  v_mdt_det_ctr_mandato_f_inicio_mandato,
                  v_mdt_det_aplica_monto_sum_monto_pesos,
                  v_mdt_det_aplica_monto_sum_monto_acciones,
                  v_hps_id_cat_mandato,
                  v_hps_subcuenta ,
                  v_hps_movimiento,
                  v_hps_sol_pag_f_primer_pago_predial,
                  v_hps_sol_pag_mto_primer_pago_predial,
                  v_hps_sol_pag_aivs_primer_pago_predial,
                  v_hps_sol_pag_f_primer_pago_conservacion,
                  v_hps_sol_pag_mto_primer_pago_conservacion,
                  v_hps_sol_pag_aivs_primer_pago_conservacion
             FROM hps_cat_pago_servicio mto JOIN hps_solicitud_pago_servicio det
               ON det.id_solicitud_pago_servicio = mto.id_solicitud_pago_servicio
              AND mto.id_cat_mandato = p_id_cat_mandato
              AND det.ind_actividad = 103

      LET v_nombre_derechohabiente = "";

      SELECT nss,TRIM(nombre_af)||" "||
             TRIM(ap_paterno_af)||" "||
             TRIM(ap_materno_af)
        INTO v_nss,v_nombre_derechohabiente
        FROM afi_derechohabiente afi
       WHERE id_derechohabiente = v_mdt_det_aplica_monto_id_derechohabiente;

      LET v_tpo_mandato = v_mdt_det_ctr_mandato_cve_mandato[1,2];
      IF(v_tpo_mandato = "01")THEN -- Predial
         LET v_mdt_preliquida_movimiento = 314;
         LET v_hps_subcuenta = 51;
      ELSE
         IF(v_tpo_mandato = "02")THEN -- Mantenimiento
            LET v_mdt_preliquida_movimiento = 334;
            LET v_hps_subcuenta = 53;
         ELSE
            IF(v_tpo_mandato = "03")THEN -- Srevicios
               LET v_mdt_preliquida_movimiento = 324;
               LET v_hps_subcuenta = 52;
            END IF;
         END IF;
      END IF;

      EXECUTE PROCEDURE sp_hps_valida_cuenta_pago_instruccion(v_nss,
                                                              v_mdt_det_aplica_monto_id_derechohabiente,
                                                              v_hps_subcuenta,
                                                              v_mdt_det_ctr_mandato_cve_mandato,
                                                              v_mdt_det_ctr_mandato_valor_descuento_mandato,
                                                              v_hps_sol_pag_f_primer_pago_predial,
                                                              v_hps_sol_pag_mto_primer_pago_predial,
                                                              v_hps_sol_pag_f_primer_pago_conservacion,
                                                              v_hps_sol_pag_mto_primer_pago_conservacion)
                                                         INTO v_sql_error,
                                                              v_isam_error,
                                                              v_msg_error,
                                                              v_movimiento_valido;
      -- Realiza los pagos si hay suficiencia de saldos en cada subcuenta
      IF( v_movimiento_valido = 1 )THEN
         LET v_sum_monto_pesos = v_sum_monto_pesos + v_mdt_det_aplica_monto_sum_monto_pesos;

         INSERT INTO hps_det_aplica_pago_servicio (id_det_aplica_pago_servicio,
                                                   id_ctr_aplica_pago_servicio,
                                                   caso_proceso,
                                                   id_credito,
                                                   ent_federativa,
                                                   clave_convenio,
                                                   valor_descuento,
                                                   f_inicio_mandato,
                                                   nombre_trabajador,
                                                   municipio,
                                                   monto_pesos,
                                                   f_liquida )
                                            VALUES
                                                  (seq_hps_det_aplica_pago_servicio.NEXTVAL,
                                                   p_id_ctr_aplica_pago_mandato,
                                                   v_mdt_det_aplica_pago_mandato_caso_proceso,
                                                   v_mdt_ctr_mandato_id_credito,
                                                   p_entidad_federativa,
                                                   v_mdt_det_ctr_mandato_cve_mandato,
                                                   v_mdt_det_ctr_mandato_valor_descuento_mandato ,
                                                   v_mdt_det_ctr_mandato_f_inicio_mandato,
                                                   v_nombre_derechohabiente,
                                                   p_municipio,
                                                   v_mdt_det_aplica_monto_sum_monto_pesos,
                                                   TODAY );

         SELECT FIRST 1 seq_hps_det_aplica_pago_servicio.CURRVAL
           INTO v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato
           FROM hps_det_aplica_pago_servicio;

         INSERT INTO hps_preliquida
                          (f_liquida,
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
                          VALUES
                          (TODAY,
                           v_mdt_det_aplica_monto_id_derechohabiente,
                           v_hps_subcuenta,
                           v_mdt_preliquida_fondo_inversion,
                           v_mdt_preliquida_movimiento,
                           p_folio,
                           v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                           - v_mdt_det_aplica_monto_sum_monto_acciones,
                           - v_mdt_det_aplica_monto_sum_monto_pesos,
                           TODAY,
                           TODAY,
                           EXTEND(CURRENT,HOUR TO SECOND),
                           p_desc_mandato);

         --TRACE "4";
         -- inserta historico de aplicacion de pago del fondo de servicios
         INSERT INTO hps_det_aplica_monto VALUES(seq_hps_det_aplica_monto.NEXTVAL,
                                                 v_hps_id_cat_pago_servicio      ,
                                                 v_mdt_det_aplica_monto_id_derechohabiente,
                                                 v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                                                 v_nss ,
                                                 v_hps_id_cat_mandato ,
                                                 v_mdt_det_ctr_mandato_cve_mandato,
                                                 v_hps_subcuenta ,
                                                 v_mdt_preliquida_movimiento,
                                                 - v_mdt_det_aplica_monto_sum_monto_acciones,
                                                 - v_mdt_det_aplica_monto_sum_monto_pesos,
                                                 102);  -- preliquidado

         --*******VALIDA SI ES NECEASRIO LIQUIDAR LOS PRIMEROS PAGOS*******--
         -- Verifica predial
         IF( v_hps_sol_pag_f_primer_pago_predial <= v_fecha_actual AND v_hps_sol_pag_mto_primer_pago_predial <> 0 )THEN
            LET v_existe_reg = 0;
            SELECT FIRST 1 NVL(1,0)
              INTO v_existe_reg
              FROM hps_det_aplica_pago_servicio
             WHERE clave_convenio = v_mdt_det_ctr_mandato_cve_mandato
               AND valor_descuento = v_hps_sol_pag_mto_primer_pago_predial;

            LET v_fecha_actual = TODAY;
            -- Si no existe el primer pago y la fecha se ha cumplido, se aplica
            IF( v_existe_reg = 0 OR v_existe_reg IS NULL )THEN

               LET v_hps_subcuenta = 51; -- Subcuenta de predial
               SELECT FIRST 1 seq_hps_det_aplica_pago_servicio.CURRVAL
                 INTO v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato
                 FROM hps_det_aplica_pago_servicio;

               LET v_sum_monto_pesos = v_sum_monto_pesos + v_hps_sol_pag_mto_primer_pago_predial;
 
               INSERT INTO hps_det_aplica_pago_servicio
                      (id_det_aplica_pago_servicio,
                       id_ctr_aplica_pago_servicio,
                       caso_proceso,
                       id_credito,
                       ent_federativa,
                       clave_convenio,
                       valor_descuento,
                       f_inicio_mandato,
                       nombre_trabajador,
                       municipio,
                       monto_pesos,
                       f_liquida )
                VALUES
                      (seq_hps_det_aplica_pago_servicio.NEXTVAL,
                       p_id_ctr_aplica_pago_mandato,
                       v_mdt_det_aplica_pago_mandato_caso_proceso,
                       v_mdt_ctr_mandato_id_credito,
                       p_entidad_federativa,
                       v_mdt_det_ctr_mandato_cve_mandato,
                       v_hps_sol_pag_mto_primer_pago_predial ,
                       v_mdt_det_ctr_mandato_f_inicio_mandato,
                       v_nombre_derechohabiente,
                       p_municipio,
                       v_hps_sol_pag_mto_primer_pago_predial,
                       TODAY );

               INSERT INTO hps_preliquida
                     (f_liquida,
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
               VALUES
                     (TODAY,
                      v_mdt_det_aplica_monto_id_derechohabiente,
                      v_hps_subcuenta,
                      v_mdt_preliquida_fondo_inversion,
                      v_mdt_preliquida_movimiento,
                      p_folio,
                      v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                      - v_hps_sol_pag_aivs_primer_pago_predial,
                      - v_hps_sol_pag_mto_primer_pago_predial,
                      TODAY,
                      TODAY,
                      EXTEND(CURRENT,HOUR TO SECOND),
                      p_desc_mandato);

               INSERT INTO hps_det_aplica_monto VALUES(seq_hps_det_aplica_monto.NEXTVAL,
                                                       v_hps_id_cat_pago_servicio      ,
                                                       v_mdt_det_aplica_monto_id_derechohabiente,
                                                       v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                                                       v_nss ,
                                                       v_hps_id_cat_mandato ,
                                                       v_mdt_det_ctr_mandato_cve_mandato,
                                                       v_hps_subcuenta ,
                                                       v_mdt_preliquida_movimiento,
                                                       - v_hps_sol_pag_aivs_primer_pago_predial,
                                                       - v_hps_sol_pag_mto_primer_pago_predial,
                                                       102);  -- preliquidado

            END IF
         END IF

         IF( v_hps_sol_pag_f_primer_pago_conservacion <= v_fecha_actual AND v_hps_sol_pag_mto_primer_pago_conservacion <> 0 )THEN
            --Verifica Cuota de conservación
            LET v_existe_reg = 0;
            SELECT FIRST 1 NVL(1,0)
              INTO v_existe_reg
              FROM hps_det_aplica_pago_servicio
             WHERE clave_convenio = v_mdt_det_ctr_mandato_cve_mandato
               AND valor_descuento = v_hps_sol_pag_mto_primer_pago_conservacion;

            -- Si no existe el primer pago y la fecha se ha cumplido, se aplica
            IF( v_existe_reg = 0 OR v_existe_reg IS NULL )THEN

               LET v_hps_subcuenta = 53; -- Subcuenta de conservación

               SELECT FIRST 1 seq_hps_det_aplica_pago_servicio.CURRVAL
                 INTO v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato
                 FROM hps_det_aplica_pago_servicio;

               LET v_sum_monto_pesos = v_sum_monto_pesos + v_hps_sol_pag_mto_primer_pago_conservacion;

               INSERT INTO hps_det_aplica_pago_servicio
                      (id_det_aplica_pago_servicio,
                       id_ctr_aplica_pago_servicio,
                       caso_proceso,
                       id_credito,
                       ent_federativa,
                       clave_convenio,
                       valor_descuento,
                       f_inicio_mandato,
                       nombre_trabajador,
                       municipio,
                       monto_pesos,
                       f_liquida )
                VALUES
                      (seq_hps_det_aplica_pago_servicio.NEXTVAL,
                       p_id_ctr_aplica_pago_mandato,
                       v_mdt_det_aplica_pago_mandato_caso_proceso,
                       v_mdt_ctr_mandato_id_credito,
                       p_entidad_federativa,
                       v_mdt_det_ctr_mandato_cve_mandato,
                       v_hps_sol_pag_mto_primer_pago_conservacion ,
                       v_mdt_det_ctr_mandato_f_inicio_mandato,
                       v_nombre_derechohabiente,
                       p_municipio,
                       v_hps_sol_pag_mto_primer_pago_conservacion,
                       TODAY );

               INSERT INTO hps_preliquida
                     (f_liquida,
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
               VALUES
                     (TODAY,
                      v_mdt_det_aplica_monto_id_derechohabiente,
                      v_hps_subcuenta,
                      v_mdt_preliquida_fondo_inversion,
                      v_mdt_preliquida_movimiento,
                      p_folio,
                      v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                      - v_hps_sol_pag_aivs_primer_pago_conservacion,
                      - v_hps_sol_pag_mto_primer_pago_conservacion,
                      TODAY,
                      TODAY,
                      EXTEND(CURRENT,HOUR TO SECOND),
                      p_desc_mandato);

               INSERT INTO hps_det_aplica_monto VALUES(seq_hps_det_aplica_monto.NEXTVAL,
                                                       v_hps_id_cat_pago_servicio      ,
                                                       v_mdt_det_aplica_monto_id_derechohabiente,
                                                       v_mdt_det_aplica_pago_mandato_id_det_aplica_pago_mandato,
                                                       v_nss ,
                                                       v_hps_id_cat_mandato ,
                                                       v_mdt_det_ctr_mandato_cve_mandato,
                                                       v_hps_subcuenta ,
                                                       v_mdt_preliquida_movimiento,
                                                       - v_hps_sol_pag_aivs_primer_pago_conservacion,
                                                       - v_hps_sol_pag_mto_primer_pago_conservacion,
                                                       102);  -- preliquidado
            END IF
         END IF
         ----------------------********************************-------------------------
      END IF
   END FOREACH

   --TRACE "6";

   UPDATE STATISTICS FOR TABLE hps_det_aplica_monto;
   UPDATE STATISTICS FOR TABLE hps_preliquida;
   UPDATE STATISTICS FOR TABLE hps_det_aplica_pago_servicio;

   LET p_desc_mandato = "";
   RETURN v_sql_error,
          v_isam_error,
          v_msg_error,
          p_desc_mandato,
          v_sum_monto_pesos;

END FUNCTION;


