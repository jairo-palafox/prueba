






CREATE PROCEDURE "safreviv".sp_hps_aplica_fondo_servicio(p_folio_originacion    DECIMAL(9,0),
                                              p_usuario_originacion  CHAR(20))
RETURNING SMALLINT, INTEGER, VARCHAR(250)
--===============================================================
-- Version: 1.0.0
--
-- Fecha ultima modificacion: 17 marzo 2015
-- Por : Jesus Yáñez Moreno
--
-- Funcion que aplica el fondeo de recursos a las subcuentas de
-- servicios provienientes del deudor de subcuenta viv 97
-- segun instruccion en el ws informa credito ejercido es decir
-- en la originación del créidto.
--===============================================================

   -- Arreglo de hps_ctr_aplica_servicio (m)

   DEFINE v_cas_id_ctr_aplica_servicio         DECIMAL(9,0);
   DEFINE v_cas_folio_deudor_originacion       DECIMAL(9,0);
   DEFINE v_cas_folio_aplica_servicio          DECIMAL(9,0);
   DEFINE v_cas_f_liquida_deudor_originacion   DATE;
   DEFINE v_cas_f_aplica_servicio              DATE;
   DEFINE v_cas_estado                         SMALLINT;
   DEFINE v_cas_usuario_originacion            CHAR(20);

   -- Arreglo de mdt_det_aplica_servicio (d)

   DEFINE v_das_id_det_aplica_servicio     DECIMAL(9,0);
   DEFINE v_das_id_ctr_aplica_servicio     DECIMAL(9,0);
   DEFINE v_das_id_solicitud_pago_servicio DECIMAL(9,0);
   DEFINE v_das_id_derechohabiente         DECIMAL(9,0);
   DEFINE v_das_nss                        CHAR(011);
   DEFINE v_das_cve_mandato_predial        CHAR(018);
   DEFINE v_das_cve_mandato_cc             CHAR(018);
   DEFINE v_das_id_cat_mandato_predial     DECIMAL(9,0);
   DEFINE v_das_id_cat_mandato_cc          DECIMAL(9,0);
   DEFINE v_das_subcuenta_predial          SMALLINT;
   DEFINE v_das_movimiento_predial         SMALLINT;
   DEFINE v_das_subcuenta_cc               SMALLINT;
   DEFINE v_das_movimiento_cc              SMALLINT;
   DEFINE v_das_mto_acciones_predial       DECIMAL(16,6);
   DEFINE v_das_mto_pesos_predial          DECIMAL(16,2); -- predial
   DEFINE v_das_mto_acciones_cc            DECIMAL(16,6);
   DEFINE v_das_mto_pesos_cc               DECIMAL(16,2); -- cuota conservacion
   DEFINE v_das_estado_aplica_servicio     SMALLINT;

DEFINE v_existe_fondo SMALLINT;

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
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_hps_aplica_fondo_servicio.trace';
   SET DEBUG FILE TO '/safreviv_int/BD/sp_hps_aplica_fondo_servicio.trace'; --QA
   TRACE ON;
   --TRACE 'Inicia el store procedure de preliquidacion de mandatos';


   --TRACE "Parte 1 - verifica folio en glo_folio ";
   -- se asume que no presenta error
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'La aplicacion de Fondos de Servicios finalizo correctamente';

   ----------------------------------------------------------
   -- HABILITAR HASTA CONTAR CON EL FOLIO DUMMY DE ORIGINACION
   -----------------------------------------------------------
   SELECT NVL(folio,0), f_actualiza
     INTO v_cas_folio_deudor_originacion, v_cas_f_liquida_deudor_originacion
     FROM glo_folio
    WHERE folio       = p_folio_originacion;
     -- AND status      = 0; -- verificar el status del folio de originacion

   IF ( v_cas_folio_deudor_originacion = 0 OR v_cas_folio_deudor_originacion IS NULL ) THEN
      -- no existe folio de originacion
      LET v_si_resultado = 1;
      LET isam_err       = 0;
      LET v_c_msj        = 'No se encontraron folios de pago';

      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF;

   --------------------------------------------------------------------
   LET v_cas_folio_deudor_originacion = p_folio_originacion ;
   LET v_cas_f_liquida_deudor_originacion = today;
   --------------------------------------------------------------------

   LET v_cas_f_aplica_servicio      = TODAY;
   LET v_cas_estado                 = 101;   -- Registrado
   LET v_das_estado_aplica_servicio = 101;

   LET v_cas_usuario_originacion    = p_usuario_originacion;

   -- se obtiene el folio para la liquidación del fondeo de las suctas servicios (predial y cc)

   EXECUTE FUNCTION fn_genera_folio (3101,1,p_usuario_originacion)
   INTO v_cas_folio_aplica_servicio;

   -- encabezado del lote a liquidar

   LET v_cas_id_ctr_aplica_servicio    = seq_hps_ctr_aplica_servicio.NEXTVAL;

   INSERT INTO hps_ctr_aplica_servicio
        VALUES (v_cas_id_ctr_aplica_servicio        ,
                v_cas_folio_deudor_originacion      ,
                v_cas_folio_aplica_servicio         ,
                v_cas_f_liquida_deudor_originacion  ,
                v_cas_f_aplica_servicio             ,
                v_cas_estado                        ,
                v_cas_usuario_originacion             ,
                ""                                  );
   LET v_das_id_ctr_aplica_servicio    = v_cas_id_ctr_aplica_servicio;
   LET v_das_estado_aplica_servicio    = 50;  -- registrado
   LET v_das_subcuenta_predial         = 51 ; -- predial
   LET v_das_movimiento_predial        = 313; -- abono predial
   LET v_das_subcuenta_cc              = 53 ; -- cc
   LET v_das_movimiento_cc             = 333; -- abono cc

   FOREACH cur_solicitud_pago_servicio FOR

           SELECT a.id_solicitud_pago_servicio                                ,
                  a.id_derechohabiente                                        ,
                  a.nss                                                       ,
                  a.cve_mandato_predial                                       ,
                  a.cve_mandato_conservacion                                  ,
                  fn_consulta_precio_fondo(a.mto_fondo_predial,today,11)      , --mto acciones
                  a.mto_fondo_predial                                         , -- pesos
                  fn_consulta_precio_fondo(a.mto_fondo_conservacion,today,11) , --mto acciones
                  a.mto_fondo_conservacion                                      -- pesos

           INTO   v_das_id_solicitud_pago_servicio  ,
                  v_das_id_derechohabiente         ,
                  v_das_nss                        ,
                  v_das_cve_mandato_predial        ,
                  v_das_cve_mandato_cc             ,
                  v_das_mto_acciones_predial       ,
                  v_das_mto_pesos_predial          ,
                  v_das_mto_acciones_cc            ,
                  v_das_mto_pesos_cc

           FROM   hps_solicitud_pago_servicio a
           WHERE  a.ind_actividad               = 101  -- registrado


          IF v_das_mto_pesos_predial > 0  THEN

             SELECT a.id_cat_mandato
             INTO   v_das_id_cat_mandato_predial
             FROM   mdt_cat_mandato_paquete a
             WHERE  a.cve_mandato = v_das_cve_mandato_predial;

             -- registro para predial


             INSERT INTO hps_det_aplica_servicio VALUES (seq_hps_det_aplica_servicio.NEXTVAL ,
                                                         v_das_id_ctr_aplica_servicio        ,
                                                         v_das_id_solicitud_pago_servicio    ,
                                                         v_das_id_derechohabiente            ,
                                                         v_das_id_cat_mandato_predial        ,
                                                         v_das_cve_mandato_predial           ,
                                                         v_das_nss                           ,
                                                         v_das_subcuenta_predial             ,
                                                         v_das_movimiento_predial            ,
                                                         v_das_mto_acciones_predial          ,
                                                         v_das_mto_pesos_predial             ,
                                                         v_das_estado_aplica_servicio        );

          END IF;


          IF v_das_mto_pesos_cc > 0  THEN

             SELECT a.id_cat_mandato
             INTO   v_das_id_cat_mandato_cc
             FROM   mdt_cat_mandato_paquete a
             WHERE  a.cve_mandato = v_das_cve_mandato_cc;
             -- registro para cuota conservacion
             INSERT INTO hps_det_aplica_servicio VALUES (seq_hps_det_aplica_servicio.NEXTVAL ,
                                                         v_das_id_ctr_aplica_servicio        ,
                                                         v_das_id_solicitud_pago_servicio    ,
                                                         v_das_id_derechohabiente            ,
                                                         v_das_id_cat_mandato_cc             ,
                                                         v_das_cve_mandato_cc                ,
                                                         v_das_nss                           ,
                                                         v_das_subcuenta_cc                  ,
                                                         v_das_movimiento_cc                 ,
                                                         v_das_mto_acciones_cc               ,
                                                         v_das_mto_pesos_cc                  ,
                                                         v_das_estado_aplica_servicio        );
         END IF;

         UPDATE hps_solicitud_pago_servicio
            SET ind_actividad = 102
          WHERE CURRENT OF cur_solicitud_pago_servicio;

      END FOREACH;


   -- Actualizacion de estadisticas al finalizar folio de pago

   UPDATE STATISTICS FOR TABLE hps_det_aplica_monto;
   UPDATE STATISTICS FOR TABLE hps_det_aplica_servicio;

   RETURN v_si_resultado, isam_err, v_c_msj;
END PROCEDURE;


