






CREATE FUNCTION "safreviv".fn_dpe_integra_arh_historicos(p_usuario_cod    CHAR(20), 
                                              p_folio          DECIMAL(10),
                                              p_nombre_archivo CHAR(40),
                                              p_pid            DECIMAL(9,0), 
                                              p_proceso_cod    SMALLINT) 

   RETURNING INTEGER, INTEGER, VARCHAR(255), VARCHAR(11)
 
-- campos de la tabla origen
DEFINE v_reg_patronal          CHAR(11);
DEFINE v_nss                   CHAR(11);
DEFINE v_rfc_trabajador        CHAR(13);
DEFINE v_curp                  CHAR(18);
DEFINE v_periodo_pago          CHAR(6);
DEFINE v_imp_viv_dev           DECIMAL(9,0);
DEFINE v_aivs_viv_dev          DECIMAL(16,6);
DEFINE v_precio_fondo          DECIMAL(19,14);
DEFINE v_dh_marca              DECIMAL(9,0);

-- constantes
DEFINE v_diagnostico_respuesta SMALLINT;
DEFINE v_resultado             SMALLINT;
DEFINE v_i_estado_marca        SMALLINT;

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
DEFINE v_bnd_desmarca          SMALLINT;

DEFINE v_folio_sua              INTEGER;
DEFINE v_rfc_patron             CHAR(13);
DEFINE v_pat_num_reg_pat_imss   CHAR(11);
DEFINE v_pat_rfc_patron         CHAR(13);
DEFINE v_pat_per_pago           CHAR(6);
DEFINE v_pat_folio_sua          INTEGER;
DEFINE v_pat_nombre_razon_soc   CHAR(50);
DEFINE v_pat_num_solicitud      CHAR(13);
DEFINE v_pat_tipo_cotizacion    SMALLINT;
DEFINE v_pat_num_trab_sol       INTEGER;
DEFINE v_pat_fec_pago           DATE;
DEFINE v_pat_fec_valor_viv      DATE;
DEFINE v_pat_fec_valor_rcv      DATE;
DEFINE v_pat_tmp_fec_pago       CHAR(8);
DEFINE v_pat_tmp_fec_valor_viv  CHAR(8);
DEFINE v_pat_tmp_fec_valor_rcv  CHAR(8);
DEFINE v_pat_resultado_op       CHAR(2);
DEFINE v_pat_delegacion         CHAR(2);
DEFINE v_id_referencia_pat      DECIMAL(9,0);
DEFINE v_dpe_patron             DECIMAL(9,0);
DEFINE v_existe_reg             INTEGER;
DEFINE v_estado_solicitud       SMALLINT;
DEFINE v_f_valor_viv            DATE;

   -- en caso de error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
      RETURN v_error_code, v_error_isam, v_mensaje, v_nss;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/dpe/envio/integra_historicos.trace";
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
   LET v_diagnostico_respuesta = 110;
   LET v_existe_reg            = 0;
   LET v_estado_solicitud      = 1;

   FOREACH 
      SELECT num_reg_pat_imss,
             rfc_patron      ,
             per_pago        ,
             folio_sua       ,
             nombre_razon_soc,
             num_solicitud   ,
             tipo_cotizacion ,
             num_trab_sol    ,
             fec_pago        ,
             fec_valor_viv   ,
             fec_valor_rcv   ,
             resultado_op    ,
             delegacion
      INTO   v_pat_num_reg_pat_imss,
             v_pat_rfc_patron      ,
             v_pat_per_pago        ,
             v_pat_folio_sua       ,
             v_pat_nombre_razon_soc,
             v_pat_num_solicitud   ,
             v_pat_tipo_cotizacion ,
             v_pat_num_trab_sol    ,
             v_pat_tmp_fec_pago        ,
             v_pat_tmp_fec_valor_viv   ,
             v_pat_tmp_fec_valor_rcv   ,
             v_pat_resultado_op    ,
             v_pat_delegacion
      FROM   safre_tmp:tmp_dpe_cza_pago_patronal
      
      SELECT seq_dpe_patron.NEXTVAL
      INTO   v_id_referencia_pat
      FROM   SYSTABLES
      WHERE  tabname = "dpe_patron";

      LET v_pat_fec_pago      = v_pat_tmp_fec_pago[5,6]      ||"/"||v_pat_tmp_fec_pago[7,8]      ||"/"|| v_pat_tmp_fec_pago[1,4];
      LET v_pat_fec_valor_viv = v_pat_tmp_fec_valor_viv[5,6] ||"/"||v_pat_tmp_fec_valor_viv[7,8] ||"/"|| v_pat_tmp_fec_valor_viv[1,4];
      LET v_pat_fec_valor_rcv = v_pat_tmp_fec_valor_rcv[5,6] ||"/"||v_pat_tmp_fec_valor_rcv[7,8] ||"/"|| v_pat_tmp_fec_valor_rcv[1,4];

      IF (p_folio IS NOT NULL) AND (v_pat_num_reg_pat_imss IS NOT NULL) THEN 
         INSERT INTO dpe_patron (folio            ,
                                 id_dpe_referencia,
                                 reg_patronal_imss,
                                 rfc_patron       ,
                                 periodo_pago     ,
                                 folio_sua        ,
                                 razon_social     ,
                                 numero_solicitud ,
                                 tipo_cotizacion  ,
                                 tot_tra_solicitud,
                                 f_pago           ,
                                 f_valor_viv      ,
                                 f_valor_rcv      ,
                                 subdelegacion    ,
                                 result_op)
                         VALUES (p_folio          ,
                                 v_id_referencia_pat,
                                 v_pat_num_reg_pat_imss,  
                                 v_pat_rfc_patron      ,  
                                 v_pat_per_pago        ,  
                                 v_pat_folio_sua       ,  
                                 v_pat_nombre_razon_soc,  
                                 v_pat_num_solicitud   ,  
                                 v_pat_tipo_cotizacion ,  
                                 v_pat_num_trab_sol    ,  
                                 v_pat_fec_pago        ,  
                                 v_pat_fec_valor_viv   ,  
                                 v_pat_fec_valor_rcv   ,  
                                 v_pat_resultado_op    ,  
                                 v_pat_delegacion         
                                 );
      END IF
   END FOREACH

   -- se contruye el enuncionado SQL
   FOREACH
      SELECT num_reg_pat_imss,
             folio_sua,
             rfc_patron,
             nss_aportacion,
             per_pago,
             rfc_trabajador, 
             curp,
             imp_total_apo_patronal / 100 ,
             num_aplicaciones_inter / 1000000,
             resultado_operacion,
             diagnostico1
      INTO   v_reg_patronal,
             v_folio_sua,
             v_rfc_patron,
             v_nss,
             v_periodo_pago,
             v_rfc_trabajador,
             v_curp,
             v_imp_viv_dev,
             v_aivs_viv_dev,
             v_resultado_operacion,
             v_diagnostico1
      FROM   safre_tmp:tmp_dpe_det_det_trabajador

      LET v_id_dpe_referencia = 0;

      FOREACH
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente 
         WHERE  nss = v_nss
         
         IF v_id_derechohabiente IS NOT NULL THEN
            SELECT COUNT (id_dpe_referencia)
            INTO   v_existe_reg
            FROM   dpe_sol_trabajador
            WHERE  folio             = p_folio
            AND    reg_patronal_imss = v_reg_patronal
            AND    periodo_pago      = v_periodo_pago
            AND    nss               = v_nss;
            
            IF v_existe_reg = 0 OR v_existe_reg IS NULL THEN 
               SELECT seq_dpe_sol_trabajador.NEXTVAL
               INTO v_id_dpe_referencia
               FROM systables
               WHERE tabname = "dpe_sol_trabajador";
               
               SELECT id_dpe_referencia,
                      f_valor_viv
               INTO   v_dpe_patron,
                      v_f_valor_viv
               FROM   dpe_patron                               
               WHERE  folio = p_folio                          
               AND    reg_patronal_imss =  v_reg_patronal  
               AND    periodo_pago      =  v_periodo_pago  
               AND    folio_sua         =  v_folio_sua;

               INSERT INTO dpe_sol_trabajador (id_dpe_referencia,
                                               id_dpe_patron,
                                               folio,
                                               reg_patronal_imss,
                                               id_derechohabiente,
                                               nss,
                                               periodo_pago,
                                               rfc,
                                               curp,
                                               imp_viv_dev,
                                               avis_viv_dev,
                                               porcentaje_dev,
                                               estado_solicitud,
                                               diagnostico,
                                               resul_op,
                                               diag_procesa,
                                               folio_respuesta)
                                       VALUES (v_id_dpe_referencia,
                                               v_dpe_patron,
                                               p_folio,
                                               v_reg_patronal,
                                               v_id_derechohabiente,
                                               v_nss,
                                               v_periodo_pago,
                                               v_rfc_trabajador,
                                               v_curp,
                                               v_imp_viv_dev,
                                               v_aivs_viv_dev,
                                               "100",
                                               v_estado_solicitud,
                                               v_diagnostico_respuesta,
                                               v_resultado_operacion,
                                               v_diagnostico1,
                                               p_folio);

               LET v_dpe_patron = NULL;

               SELECT COUNT (id_derechohabiente)
               INTO   v_dh_marca
               FROM   sfr_marca_activa 
               WHERE  id_derechohabiente = v_id_derechohabiente
               AND    marca = 401;
               
               IF (v_dh_marca = 0) THEN 
                  EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                   401,                       -- marca de imms
                                                   v_id_dpe_referencia,  
                                                   p_folio,
                                                   0,                         -- estado marca
                                                   0,                         -- codigo de rechazo
                                                   0,                         -- marca de la causa
                                                   NULL,                      -- fecha de la causa
                                                   p_usuario_cod,
                                                   p_proceso_cod)
                  INTO v_i_estado_marca;
               END IF
               
               SELECT precio_fondo
               INTO   v_precio_fondo
               FROM   glo_valor_fondo
               WHERE  f_valuacion = v_f_valor_viv
               AND    fondo       = 11;
               
               IF v_precio_fondo IS NOT NULL THEN
                  LET v_imp_viv_dev = v_aivs_viv_dev * v_precio_fondo;
               END IF 
               
               IF v_id_dh_marca = 0 THEN
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
               
                  EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                      v_marca,
                                                      v_id_dpe_referencia,
                                                      0,
                                                      "",
                                                      p_usuario_cod,
                                                      p_proceso_cod)
                  INTO v_bnd_desmarca;
               
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
         END IF
      END FOREACH
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
       WHERE proceso_cod    = 1007
         AND nombre_archivo = p_nombre_archivo;
   END IF

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE pid         = p_pid
   AND proceso_cod = 1007
   AND opera_cod   = 2;

   -- se devuelve el resultado de la integracion
   RETURN v_error_code,
          v_error_isam,
          v_mensaje,
          v_nss;
END FUNCTION
;


