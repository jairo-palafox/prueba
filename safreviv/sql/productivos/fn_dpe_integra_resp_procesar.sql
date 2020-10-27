






CREATE FUNCTION "safreviv".fn_dpe_integra_resp_procesar(p_usuario_cod    CHAR(20),
                                             p_folio          DECIMAL(10),
                                             p_nombre_archivo CHAR(40),
                                             p_pid            DECIMAL(9,0),
                                             p_proceso_cod    SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), VARCHAR(11)

-- campos de la tabla origen
DEFINE v_reg_patronal          CHAR(11);
DEFINE v_nss                   CHAR(11);
DEFINE v_periodo_pago          CHAR(6);
DEFINE v_imp_viv_dev           DECIMAL(12,2);
DEFINE v_aivs_viv_dev          DECIMAL(16,6);
DEFINE v_precio_fondo          DECIMAL(19,14);

-- constantes
DEFINE v_diagnostico_respuesta SMALLINT;
DEFINE v_resultado             SMALLINT;

-- para control de excepciones
DEFINE v_error_code            INTEGER;
DEFINE v_error_isam            INTEGER;
DEFINE v_mensaje               VARCHAR(255);
DEFINE v_regs_aceptados        INTEGER;
DEFINE v_regs_rechazados       INTEGER;

DEFINE v_resultado_operacion   CHAR(2);
DEFINE v_diagnostico1          SMALLINT;
DEFINE v_id_dpe_referencia     DECIMAL(9,0);
DEFINE v_marca                 SMALLINT;
DEFINE v_id_dh_marca           DECIMAL(9,0);
DEFINE v_id_derechohabiente    DECIMAL(9,0);
define v_id_dpe_patron         DECIMAL(9,0);
DEFINE v_bnd_desmarca          SMALLINT;
DEFINE v_f_valor_viv           INTEGER;

   -- en caso de error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
      RETURN v_error_code, v_error_isam, v_mensaje, v_nss;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/dpe/envio/trace_resp_procesar.trace";
   --TRACE ON;

   -- se asume que no hay errores
   LET v_error_code            = 0;
   LET v_error_isam            = 0;
   LET v_mensaje               = "Finalizado correctamente.";
   LET v_nss                   = NULL;
   LET v_regs_aceptados        = 0;
   LET v_regs_rechazados       = 0;
   LET v_marca                 = 401;
   LET v_resultado             = 0;
   LET v_id_dh_marca           = 0;
   LET v_id_derechohabiente    = 0;
   LET v_precio_fondo          = 0;
   LET v_bnd_desmarca          = 0;
   LET v_diagnostico_respuesta = 2;


   -- se contruye el enuncionado SQL
   FOREACH
      SELECT num_reg_pat_imss,
             nss_aportacion,
             per_pago,
             imp_total_apo_patronal / 100 ,
             num_aplicaciones_inter / 1000000,
             resultado_operacion,
             diagnostico1
      INTO   v_reg_patronal,
             v_nss,
             v_periodo_pago,
             v_imp_viv_dev,
             v_aivs_viv_dev,
             v_resultado_operacion,
             v_diagnostico1
      FROM   safre_tmp:tmp_dpe_det_det_trabajador


      LET v_id_dpe_referencia = 0;

       FOREACH
         SELECT id_dpe_referencia,
                id_derechohabiente,
                id_dpe_patron
         INTO   v_id_dpe_referencia,
                v_id_derechohabiente,
                v_id_dpe_patron
         FROM   dpe_sol_trabajador
         WHERE  reg_patronal_imss = v_reg_patronal
         AND    nss               = v_nss
         AND    periodo_pago      = v_periodo_pago
         AND    folio_respuesta IS NULL

         --trace "REFERENCIA :"|| v_id_dpe_referencia;
         SELECT f_valor_viv
         INTO   v_f_valor_viv
         FROM   dpe_patron
         WHERE  id_dpe_referencia = v_id_dpe_patron;

         IF  (v_id_dpe_referencia > 0) OR  (v_id_dpe_referencia IS NOT NULL )THEN
            SELECT id_derechohabiente
            INTO   v_id_dh_marca
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    marca = v_marca
            AND    n_referencia = v_id_dpe_referencia;

            SELECT precio_fondo
            INTO   v_precio_fondo
            FROM   glo_valor_fondo
            WHERE  fondo       = 11
            AND    f_valuacion = v_f_valor_viv;

            IF v_precio_fondo IS NOT NULL THEN
               LET v_imp_viv_dev = v_aivs_viv_dev * v_precio_fondo;
            END IF

            IF v_id_dh_marca IS NOT NULL THEN
               INSERT INTO dpe_resp_procesar(id_dpe_referencia,
                                             folio,
                                             reg_patronal_imss,
                                             nss,
                                             periodo_pago,
                                             imp_viv_dev,
                                             aivs_viv_dev,
                                             resul_op,
                                             f_respuesta)
                                     VALUES (v_id_dpe_referencia,
                                             p_folio,
                                             v_reg_patronal,
                                             v_nss,
                                             v_periodo_pago,
                                             v_imp_viv_dev ,
                                             v_aivs_viv_dev,
                                             v_resultado_operacion,
                                             TODAY);

               UPDATE dpe_sol_trabajador
               SET    resul_op           = v_resultado_operacion,
                      diag_procesa       = v_diagnostico1,
                      folio_respuesta    = p_folio,
                      diagnostico        = v_diagnostico_respuesta
               WHERE  id_dpe_referencia  = v_id_dpe_referencia;
            ELSE
               LET v_resultado_operacion = 2 ;
               LET v_diagnostico_respuesta = 24;

               INSERT INTO dpe_resp_procesar(id_dpe_referencia,
                                             folio,
                                             reg_patronal_imss,
                                             nss,
                                             periodo_pago,
                                             imp_viv_dev,
                                             aivs_viv_dev,
                                             resul_op,
                                             f_respuesta)
                                     VALUES (v_id_dpe_referencia,
                                             p_folio,
                                             v_reg_patronal,
                                             v_nss,
                                             v_periodo_pago,
                                             v_imp_viv_dev ,
                                             v_aivs_viv_dev,
                                             v_resultado_operacion,
                                             TODAY);

               UPDATE dpe_sol_trabajador
               SET    resul_op           = v_resultado_operacion,
                      diag_procesa       = v_diagnostico1,
                      folio_respuesta    = p_folio,
                      diagnostico        = v_diagnostico_respuesta
               WHERE  id_dpe_referencia  = v_id_dpe_referencia;
            END IF

            IF v_resultado_operacion = "02" THEN
               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                   v_marca,
                                                   v_id_dpe_referencia,
                                                   0,
                                                   "",
                                                   p_usuario_cod,
                                                   p_proceso_cod)
               INTO v_bnd_desmarca;
            END IF
         END IF
END FOREACH      ;
      LET v_resultado = 1;
   END FOREACH;

   UPDATE STATISTICS FOR TABLE dpe_resp_procesar;
   UPDATE STATISTICS FOR TABLE dpe_sol_trabajador;

   -- si se integraron datos
   IF (v_resultado = 1) THEN
      --Se actualiza estado el control de archivo
      UPDATE glo_ctr_archivo
         SET folio = p_folio,
             estado = 2 -- integrado
       WHERE proceso_cod    = 1006
         AND opera_cod      = 1
         AND estado         = 1;

      -- Agregar folio a operacion de integracion
      UPDATE bat_ctr_operacion
         SET folio       = p_folio,
             nom_archivo = p_nombre_archivo
       WHERE pid         = p_pid;
   END IF

   -- se devuelve el resultado de la integracion
   RETURN v_error_code,
          v_error_isam,
          v_mensaje,
          v_nss;
END FUNCTION
;


