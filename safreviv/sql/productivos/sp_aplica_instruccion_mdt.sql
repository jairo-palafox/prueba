






CREATE PROCEDURE "safreviv".sp_aplica_instruccion_mdt(p_folio   DECIMAL(9,0),
                                           p_usuario CHAR(20))
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

   DEFINE v_existe           INTEGER;
   DEFINE v_folio_referencia DECIMAL(9,0);
   DEFINE v_mov_abono        SMALLINT;
   DEFINE v_acumulado_amort  DECIMAL(12,2);
   -- Arreglo de mdt_ctr_aplica_mandato
   DEFINE v_m_id_ctr_aplica_mandato DECIMAL(9,0);
   DEFINE v_m_folio_dispersion      DECIMAL(9,0);
   DEFINE v_m_folio_pago            DECIMAL(9,0);
   DEFINE v_m_folio_aplica          DECIMAL(9,0);
   DEFINE v_m_f_liquida_pago        DATE;
   DEFINE v_m_f_aplica              DATE;
   DEFINE v_m_f_dispersion          DATE;
   DEFINE v_m_estado                SMALLINT;
   DEFINE v_m_h_dispersion          DATETIME HOUR TO MINUTE;
   DEFINE v_m_usuario               CHAR(20);

   -- Arreglo de mdt_det_aplica_mandato
   DEFINE v_d_id_det_aplica_mandato     DECIMAL(9,0);
   DEFINE v_d_folio_pago                DECIMAL(9,0);
   DEFINE v_d_id_referencia_pago        DECIMAL(9,0);
   DEFINE v_d_folio_dispersion          DECIMAL(9,0);
   DEFINE v_d_id_ctr_aplica_mandato     DECIMAL(9,0);
   DEFINE v_d_id_derechohabiente        DECIMAL(9,0);
   DEFINE v_d_periodo_pago              CHAR(8);
   DEFINE v_d_tot_acciones_amortizacion DECIMAL(16,6); -- cambio precision
   DEFINE v_d_tot_pesos_amortizacion    DECIMAL(12,2); -- cambio precision
   DEFINE v_d_subcuenta                 SMALLINT;
   DEFINE v_g_tot_acciones_amortizacion DECIMAL(18,6); -- cambio precision
   DEFINE v_g_tot_pesos_amortizacion    DECIMAL(22,2);
   DEFINE v_g_hay_saldo                 DECIMAL(22,2);

   -- Arreglo de mdt_det_aplica_monto
   DEFINE v_dm_id_det_aplica_monto    DECIMAL(9,0);
   DEFINE v_dm_id_det_aplica_mandato  DECIMAL(9,0);
   DEFINE v_dm_id_det_ctr_mandato     DECIMAL(9,0);
   DEFINE v_dm_id_derechohabiente     DECIMAL(9,0);
   DEFINE v_dm_subcuenta              SMALLINT;
   DEFINE v_dm_monto_pesos            DECIMAL(12,2); -- cambio precision
   DEFINE v_dm_monto_acciones         DECIMAL(16,6); -- cambio precision
   DEFINE v_dm_estado_aplica_monto    SMALLINT;

   -- Arreglo de dis_preliquida
   DEFINE v_p_f_liquida               DATE;
   DEFINE v_p_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_p_subcuenta               SMALLINT;
   DEFINE v_p_fondo_inversion         SMALLINT;
   DEFINE v_p_movimiento              SMALLINT;
   DEFINE v_p_folio_liquida           DECIMAL(9,0);
   DEFINE v_p_id_referencia           DECIMAL(9,0);
   DEFINE v_p_monto_acciones          DECIMAL(16,6); -- cambio precision
   DEFINE v_p_monto_pesos             DECIMAL(12,2); -- cambio precision
   DEFINE v_p_f_valor                 DATE;
   DEFINE v_p_f_registro              DATE;
   DEFINE v_p_h_registro              DATETIME HOUR TO SECOND;
   DEFINE v_p_desc_mandato            CHAR(040);
   DEFINE v_p_id_cat_mandato          CHAR(040);

   DEFINE v_valor_descuento_mandato   DECIMAL(12,2); -- cambio precision
   DEFINE v_precio_fondo              DECIMAL(19,14);
   DEFINE v_movto_clase               VARCHAR(20);
   DEFINE v_qrytxt                    CHAR(500);
   DEFINE v_cta_exis                  SMALLINT;
   DEFINE v_d_pdo_pago_val            CHAR(8);

   -- Control de Excepciones
   DEFINE sql_err         INTEGER;
   DEFINE isam_err        INTEGER;
   DEFINE err_txt         CHAR(200);
   DEFINE v_c_msj         VARCHAR(250);
   DEFINE v_si_resultado  SMALLINT;

   -- se asigna el codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_aplica_instruccion_mdt.trace';
   --TRACE 'Inicia el store procedure de preliquidacion de mandatos';

   --TRACE "Parte 1 - verifica folio en glo_folio ";
   -- se asume que no presenta error
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'La aplicacion de Mandatos finalizo correctamente';


   CREATE TEMP TABLE mdt_tmp_id_derechohabiente(id_derechohabiente decimal(9,0));

   SELECT NVL(folio_referencia,0), f_actualiza
     INTO v_m_folio_pago, v_m_f_liquida_pago
     FROM glo_folio
    WHERE folio       = p_folio
      AND status      = 0; -- se cambia de 1 a 0 por instrucciones de Fernando Herrera

   IF ( v_m_folio_pago = 0 OR v_m_folio_pago IS NULL ) THEN
      -- no existe folio de pago
      LET v_si_resultado = 1;
      LET isam_err       = 0;
      LET v_c_msj        = 'No se encontraron folios de pago';

      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF;

   SELECT NVL(max(id_ctr_aplica_mandato),0)+1 INTO v_m_id_ctr_aplica_mandato
     FROM mdt_ctr_aplica_mandato;

   LET v_m_folio_dispersion      = p_folio;
   LET v_m_f_dispersion          = TODAY;
   LET v_m_f_aplica              = TODAY;
   --LET v_m_estado                = 100;   -- Estado identificado
   LET v_m_estado                = 50;   -- Estado identificado
   LET v_dm_estado_aplica_monto   = 50;
   LET v_m_h_dispersion          = CURRENT HOUR TO MINUTE;
   LET v_m_usuario               = p_usuario;


   EXECUTE FUNCTION fn_genera_folio (1313,1,p_usuario)
   INTO v_m_folio_aplica;

   -- se actualiza status del folio a preliquidado
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio =  v_m_folio_aplica;

   INSERT INTO mdt_ctr_aplica_mandato
        VALUES (v_m_id_ctr_aplica_mandato,
                v_m_folio_dispersion     ,
                v_m_folio_pago           ,
                v_m_folio_aplica         ,
                v_m_f_liquida_pago       ,
                v_m_f_dispersion         ,
                v_m_f_aplica             ,
                v_m_estado               ,
                v_m_h_dispersion         ,
                v_m_usuario);


   -- Se llena tabla temporal con los id_derechohabientes

   INSERT INTO mdt_tmp_id_derechohabiente
   SELECT UNIQUE id_derechohabiente
     FROM mdt_det_ctr_mandato
    WHERE estado = 103;


   --TRACE "Parte 2 - Busqueda de mandatos activos por derechohabiente";
   --FOREACH SELECT UNIQUE id_derechohabiente
   --          INTO v_d_id_derechohabiente
   --          FROM mdt_det_ctr_mandato
   --         WHERE estado = 103

      LET v_cta_exis = 0;
      LET v_p_movimiento = 0;

      --TRACE "Parte 2.1 - Busqueda de cuentas para el derechohabiente:"||v_d_id_derechohabiente||" de mandato, y folio: "||v_m_folio_pago;
      LET v_g_tot_pesos_amortizacion = 0;
      FOREACH SELECT id.id_derechohabiente,
                     c.id_referencia,
                     c.monto_acciones,
                     c.monto_pesos,
                     m.movto_clase,
                     c.subcuenta
                INTO v_d_id_derechohabiente ,
                     v_d_id_referencia_pago,
                     v_d_tot_acciones_amortizacion,
                     v_d_tot_pesos_amortizacion,
                     v_movto_clase ,
                     v_d_subcuenta
                FROM cta_movimiento c,
                     mdt_cat_movto_pago m ,
                     mdt_tmp_id_derechohabiente id
               WHERE c.id_derechohabiente = id.id_derechohabiente
                 AND c.subcuenta          = 41
                 AND c.movimiento         = m.movto_pago
                 AND c.folio_liquida      = v_m_folio_pago

{
                 AND c.id_derechohabiente = v_d_id_derechohabiente
                 AND c.movimiento         = m.movto_pago
                 AND c.subcuenta          in (SELECT a.scta_origen_descuento
                                              FROM   mdt_det_ctr_mandato a
                                              WHERE  a.id_derechohabiente = v_d_id_derechohabiente
                                              AND    a.estado = 103)
}
         --TRACE "Parte 3 - Para cada tipo movimiento: ";
         LET v_g_tot_pesos_amortizacion = v_g_tot_pesos_amortizacion + v_d_tot_pesos_amortizacion;

         IF v_movto_clase IS NOT NULL THEN
            LET v_d_periodo_pago = "--------";
            LET v_qrytxt = " SELECT periodo_pago FROM "||v_movto_clase||
                           " WHERE folio = "||v_m_folio_pago||" AND id_referencia = " ||v_d_id_referencia_pago||
                           " AND id_derechohabiente = "||v_d_id_derechohabiente;

--                           " WHERE folio = "||v_m_folio_pago||" AND id_derechohabiente = " ||v_d_id_derechohabiente||
--                          " AND id_referencia = "||v_d_id_referencia_pago;

            --TRACE TRIM(v_qrytxt);
            PREPARE ptxt FROM v_qrytxt;
            DECLARE cptxt CURSOR FOR ptxt;
               OPEN cptxt;
              FETCH cptxt INTO v_d_periodo_pago;
              CLOSE cptxt;
               FREE cptxt;
               FREE ptxt;
         ELSE
            --TRACE "Parte 3.1 - Problemas para extraer periodo_pago de tabla: "||v_movto_clase;
         END IF;

         --TRACE "<"||v_d_periodo_pago||">";
         IF v_d_periodo_pago = '--------' THEN
            --TRACE "Parte 3.2 - Problemas para obtener periodo_pago. Se omite movimiento";
            CONTINUE FOREACH;
         END IF;

         --TRACE "Parte 4 - genera max de det_aplica_mandato";
         SELECT NVL(max(id_det_aplica_mandato),0)+1
           INTO v_d_id_det_aplica_mandato
           FROM mdt_det_aplica_mandato;

         LET v_d_folio_pago                = v_m_folio_pago;
         LET v_d_folio_dispersion          = p_folio;
         LET v_d_id_ctr_aplica_mandato     = v_m_id_ctr_aplica_mandato;

         --TRACE "Parte 4.1 - insert en mdtdet_aplica_mdt";
         INSERT INTO mdt_det_aplica_mandato
              VALUES (v_d_id_det_aplica_mandato    ,
                      v_d_folio_pago               ,
                      v_d_id_referencia_pago       ,
                      v_d_folio_dispersion         ,
                      v_d_id_ctr_aplica_mandato    ,
                      v_d_id_derechohabiente       ,
                      v_d_periodo_pago             ,
                      v_d_tot_acciones_amortizacion,
                      v_d_tot_pesos_amortizacion);

         --CONVERTIR v_d_tot_pesos_amortizacion a aivs

         LET v_precio_fondo = 1;

         --TRACE "Parte 4.2 - precio_fondo - "||v_precio_fondo;

         FOREACH SELECT d.id_det_ctr_mandato              ,
                        d.scta_origen_descuento           ,
                        d.valor_descuento_mandato*cm.tipo ,
                        tm.movto_tpo_mandato              ,
                        m.desc_mandato                    ,
                        m.id_cat_mandato
                   INTO v_dm_id_det_ctr_mandato           ,
                        v_dm_subcuenta                    ,
                        v_valor_descuento_mandato         ,
                        v_p_movimiento                    ,
                        v_p_desc_mandato                  ,
                        v_p_id_cat_mandato
                   FROM mdt_det_ctr_mandato d,
                        mdt_cat_mandato m,
                        mdt_tpo_mandato tm,
                        mdt_det_prelacion p,
                        cat_movimiento cm
                  WHERE d.id_derechohabiente = v_d_id_derechohabiente
                    AND d.estado             = 103
                    AND d.id_cat_mandato     = m.id_cat_mandato
                    AND tm.tpo_mandato       = m.tpo_mandato
                    AND tm.tpo_mandato       = p.tpo_mandato
                    AND tm.movto_tpo_mandato = cm.movimiento
                    AND d.scta_origen_descuento = v_d_subcuenta
                  ORDER BY p.prelacion

            --TRACE "Parte 5 - Descuentos por mandato";

            SELECT NVL(max(id_det_aplica_monto),0)+1 INTO v_dm_id_det_aplica_monto
              FROM mdt_det_aplica_monto;

            --TRACE "Amortizacion actual  : "||v_g_tot_pesos_amortizacion;

            -- si ya no queda amortizacion no se aplica el mandato
            IF v_g_tot_pesos_amortizacion =  0  THEN
               CONTINUE FOREACH;
            END IF

            LET v_g_hay_saldo = 0;
            LET v_g_hay_saldo = v_g_tot_pesos_amortizacion;

            LET v_g_tot_pesos_amortizacion    = v_g_tot_pesos_amortizacion + v_valor_descuento_mandato;


            --verifica si la amortizacion es suficiente
            --TRACE "Amortizacion aplicada: "||v_g_tot_pesos_amortizacion;

            -- si alcanza la amortizacion se hace la aplicacion del mandato
            IF v_g_tot_pesos_amortizacion >=  0  THEN
               LET v_dm_monto_pesos        =  v_valor_descuento_mandato;
            END IF

            -- si no alcanza la amortizacion se hace por lo que sobre
            IF v_g_tot_pesos_amortizacion <  0  THEN
               LET v_dm_monto_pesos        = - v_g_hay_saldo;
               LET v_g_tot_pesos_amortizacion = 0;
            END IF

            LET v_dm_id_det_aplica_mandato = v_d_id_det_aplica_mandato;
            LET v_dm_id_derechohabiente    = v_d_id_derechohabiente;
            LET v_dm_monto_acciones        = v_dm_monto_pesos;  -- por fondo 10 1=1

            --TRACE "Parte 6 - Insert en mdt_det_aplica_monto";
            INSERT INTO mdt_det_aplica_monto
                 VALUES (v_dm_id_det_aplica_monto   ,
                         v_dm_id_det_aplica_mandato ,
                         v_dm_id_det_ctr_mandato    ,
                         v_dm_id_derechohabiente    ,
                         v_p_id_cat_mandato         , --se agrega el id_cat_mandato para facilitar la liquidacion
                         ""                         , --se agrega el espacio para el id del pago
                         v_dm_subcuenta             ,
                         v_dm_monto_pesos           ,
                         v_dm_monto_acciones        ,
                         v_dm_estado_aplica_monto   );

            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_dm_id_derechohabiente;
            LET v_p_subcuenta          = v_dm_subcuenta;
            LET v_p_fondo_inversion    = 10;
            LET v_p_folio_liquida      = v_m_folio_dispersion;
            LET v_p_id_referencia      = v_dm_id_det_aplica_monto; -- v_d_id_referencia_pago;
            --LET v_p_monto_pesos        = v_valor_descuento_mandato;
            LET v_p_monto_pesos        = v_dm_monto_pesos;
            LET v_p_monto_acciones     = v_p_monto_pesos; -- por cambio a fondo 10 1=1
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;


            --TRACE "Parte 7.1 - Insert en dis_preliquida";
            INSERT INTO dis_preliquida
                        (f_liquida         ,
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
                 VALUES (v_p_f_liquida         ,
                         v_p_id_derechohabiente,
                         v_p_subcuenta         ,
                         v_p_fondo_inversion   ,
                         v_p_movimiento        ,
                         v_p_folio_liquida     ,
                         v_p_id_referencia     ,
                         v_p_monto_acciones    ,
                         v_p_monto_pesos       ,
                         v_p_f_valor           ,
                         v_p_f_registro        ,
                         v_p_h_registro        ,
                         v_p_desc_mandato    );


            IF v_p_movimiento = 312 THEN LET v_mov_abono = 313;  END IF
            IF v_p_movimiento = 322 THEN LET v_mov_abono = 323;  END IF
            IF v_p_movimiento = 332 THEN LET v_mov_abono = 333;  END IF

            --TRACE "Parte 7.3 - Insert en mdt_preliquida" abono al mandato;
            INSERT INTO mdt_preliquida
                        (f_liquida         ,
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
                 VALUES (v_p_f_liquida         ,
                         v_p_id_derechohabiente,
                         "51"                  ,--v_p_subcuenta, scta mandato
                         v_p_fondo_inversion   ,
                         v_mov_abono           ,
                         v_m_folio_aplica      ,
                         v_p_id_referencia     ,
                         - v_p_monto_acciones  ,
                         - v_p_monto_pesos     ,
                         v_p_f_valor           ,
                         v_p_f_registro        ,
                         v_p_h_registro        ,
                         v_p_desc_mandato    );
         END FOREACH;

         LET v_cta_exis = 1;

         LET v_p_f_liquida          = TODAY;
         LET v_p_id_derechohabiente = v_d_id_derechohabiente;
         LET v_p_subcuenta          = 41;
         LET v_p_fondo_inversion    = 10;
         LET v_p_movimiento         = 52; -- Amortización real
         LET v_p_folio_liquida      = v_m_folio_dispersion;
         LET v_p_id_referencia      = v_d_id_referencia_pago;
         LET v_p_monto_pesos        = v_g_tot_pesos_amortizacion * -1;
         LET v_p_monto_acciones     = v_p_monto_pesos; -- por cambio a fondo 10 1=1
         LET v_p_f_valor            = TODAY;
         LET v_p_f_registro         = TODAY;
         LET v_p_h_registro         = CURRENT HOUR TO SECOND;

         --TRACE "Parte 8 - Insert en dis_preliquida amortizacion real del derechoahabiente: "||v_d_id_derechohabiente||" "||v_p_id_referencia||" "||v_p_movimiento;

         INSERT INTO dis_preliquida
                     (f_liquida         ,
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
              VALUES (v_p_f_liquida         ,
                      v_p_id_derechohabiente,
                      v_p_subcuenta         ,
                      v_p_fondo_inversion   ,
                      v_p_movimiento        ,
                      v_p_folio_liquida     ,
                      v_p_id_referencia     ,
                      v_p_monto_acciones    ,
                      v_p_monto_pesos       ,
                      v_p_f_valor           ,
                      v_p_f_registro        ,
                      v_p_h_registro        ,
                      "AMORT REAL DESP MDT");

         LET v_p_monto_acciones            = 0;
         LET v_g_tot_acciones_amortizacion = 0;
         LET v_p_monto_pesos               = 0;
         LET v_g_tot_pesos_amortizacion    = 0;

         --TRACE "Parte 8.1 - fin Insert en dis_preliquida amortizacion real del derechoahabiente: "||v_d_id_derechohabiente||v_p_id_referencia||v_p_movimiento;
      END FOREACH;

  -- END FOREACH;

   -- Actualizacion de estadisticas al finalizar folio de pago
   UPDATE STATISTICS FOR TABLE dis_preliquida;
   UPDATE STATISTICS FOR TABLE mdt_det_aplica_monto;
   UPDATE STATISTICS FOR TABLE mdt_det_aplica_mandato;

   --TRACE "Fin del SP";
   RETURN v_si_resultado, isam_err, v_c_msj;
END PROCEDURE;


