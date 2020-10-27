






CREATE FUNCTION "safreviv".fn_dpe_integra_acuse_procesar(p_usuario_cod    CHAR(20),
                                              p_folio          DECIMAL(10), 
                                              p_nombre_archivo CHAR(18),
                                              p_pid            DECIMAL(9,0),
                                              p_proceso_cod    SMALLINT)
   RETURNING INTEGER, 
             INTEGER, 
             VARCHAR(255), 
             VARCHAR(11)
 
-- campos de la tabla origen
DEFINE v_reg_patronal                    CHAR(11);
DEFINE v_nss                             CHAR(11);
DEFINE v_periodo_pago                    CHAR(6);
DEFINE v_imp_patronal_infonavit_devolver DECIMAL(9,0);
DEFINE v_apli_interes_vivienda           DECIMAL(15,0);

-- constantes
DEFINE v_pago_recibido_procesar_total    SMALLINT;
DEFINE v_pago_recibido_procesar_parcial  SMALLINT;
DEFINE v_resultado                       SMALLINT;

-- para control de excepciones  
DEFINE v_error_code                      INTEGER;
DEFINE v_error_isam                      INTEGER;
DEFINE v_mensaje                         VARCHAR(255);
DEFINE v_regs_aceptados                  INTEGER;
DEFINE v_regs_rechazados                 INTEGER;

DEFINE v_resultado_operacion             CHAR(2);
DEFINE v_id_dpe_referencia               DECIMAL(9,0);
DEFINE v_diagnostico1                    CHAR(3); --SMALLINT;
DEFINE v_diagnostico2                    CHAR(3); --SMALLINT;
DEFINE v_diagnostico3                    CHAR(3); --SMALLINT;
DEFINE v_marca                           SMALLINT;
DEFINE v_id_derechohabiente              DECIMAL(9,0);
DEFINE v_bnd_desmarca                    SMALLINT;
                                                     
   -- en caso de error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
   
      RETURN v_error_code, v_error_isam, v_mensaje, v_nss;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/dpe/envio/debug_fn_dpe_integra_resp_procesar.trace";
   --TRACE ON;
   
   -- se asume que no hay errores
   LET v_error_code      = 0;
   LET v_error_isam      = 0;
   LET v_mensaje         = "Finalizado correctamente.";
   LET v_nss             = NULL;
   LET v_regs_aceptados  = 0;
   LET v_regs_rechazados = 0;
   LET v_resultado_operacion = 0;
   LET v_diagnostico1    = 0;
   LET v_diagnostico2    = 0;
   LET v_diagnostico3    = 0;  
   LET v_marca           = 401;
   LET v_id_derechohabiente = NULL;
   LET v_bnd_desmarca = 0;


   LET v_resultado = 0; -- no se integraron datos

   LET v_pago_recibido_procesar_total   = 12;
   LET v_pago_recibido_procesar_parcial = 13;

   -- se contruye el enuncionado SQL
   FOREACH
      SELECT reg_patronal,
             nss,
             periodo_pago,
             imp_patronal_infonavit_devolver / 100 ,
             apli_interes_vivienda / 1000000,
             resul_opera,
             diagnostico1,
             diagnostico2,
             diagnostico3
      INTO   v_reg_patronal,
             v_nss,
             v_periodo_pago,
             v_imp_patronal_infonavit_devolver,
             v_apli_interes_vivienda,
             v_resultado_operacion,
             v_diagnostico1,
             v_diagnostico2,
             v_diagnostico3
      FROM   safre_tmp:tmp_det_rch_devolucion_dpe

      LET v_id_dpe_referencia = 0;
      LET v_id_derechohabiente = NULL;

      FOREACH         
         SELECT id_dpe_referencia, 
                id_derechohabiente
         INTO   v_id_dpe_referencia,
                v_id_derechohabiente
         FROM   dpe_sol_trabajador
         WHERE  reg_patronal_imss = v_reg_patronal
         AND    nss               = v_nss         
         AND    periodo_pago      = v_periodo_pago
         AND    folio_respuesta   IS NULL

         IF  v_id_dpe_referencia > 0 AND v_resultado_operacion = 2 THEN
            UPDATE dpe_sol_trabajador
            SET    resul_op           = v_resultado_operacion,
                   diag_procesa       = v_diagnostico1,
                   folio_respuesta    = p_folio
            WHERE  id_dpe_referencia  = v_id_dpe_referencia;


            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                v_marca,
                                                v_id_dpe_referencia,
                                                0,
                                                "",
                                                p_usuario_cod,
                                                p_proceso_cod)
            INTO v_bnd_desmarca;
         END IF
      END FOREACH;
      
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
                     v_imp_patronal_infonavit_devolver ,
                     v_apli_interes_vivienda,
                     v_resultado_operacion,
                     TODAY);
      
      -- se activa la bandera de integracion
      LET v_resultado = 1;
      
   END FOREACH;
   
   UPDATE STATISTICS FOR TABLE dpe_resp_procesar;

   -- si se integraron datos
   IF ( v_resultado = 1 ) THEN
      --Se actualiza estado el control de archivo
      UPDATE glo_ctr_archivo
         SET folio = p_folio, 
             estado = 2 -- integrado
       WHERE proceso_cod    = 1008
         AND opera_cod      = 2 -- archivo cargado
         AND estado         = 1; -- etapa de carga
         
      -- Agregar folio a operacion de integracion
      UPDATE bat_ctr_operacion 
         SET folio       = p_folio
       WHERE proceso_cod = 1008
         AND opera_cod   = 3
         AND pid         = p_pid;
   END IF      

   -- se devuelve el resultado de la integracion
   RETURN v_error_code,
          v_error_isam,
          v_mensaje,
          v_nss;

END FUNCTION
;


