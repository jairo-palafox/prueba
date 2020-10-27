






CREATE FUNCTION "safreviv".fn_dpe_integra_det_complementario(p_usuario_cod    CHAR   (20), 
                                                  p_pid            DECIMAL(9,0), 
                                                  p_nombre_archivo CHAR   (18), 
                                                  p_folio          DECIMAL(10), 
                                                  p_proceso_cod    SMALLINT,    
                                                  p_opera_cod      SMALLINT)    

  RETURNING INTEGER,
            CHAR(200),
            SMALLINT,
            CHAR(11),
            INTEGER  

   -- Control de Excepciones
   DEFINE sql_err                      INTEGER;
   DEFINE isam_err                     INTEGER;
   DEFINE err_txt                      CHAR(200);
   DEFINE v_i_resultado                INTEGER;
   DEFINE v_si_correcto_integra        SMALLINT;   
   DEFINE v_tmp_num_reg_pat_imss       CHAR(11);
   DEFINE v_tmp_rfc_patron             CHAR(13);
   DEFINE v_tmp_per_pago               CHAR(6) ;
   DEFINE v_tmp_folio_sua              DECIMAL(6,0);
   DEFINE v_tmp_nss_aportacion         CHAR(11);
   DEFINE v_tmp_rfc_trabajador         CHAR(13);
   DEFINE v_tmp_curp                   CHAR(18);
   DEFINE v_tmp_resul_operacion        SMALLINT;
   DEFINE v_resul_operacion_compl      SMALLINT;
   DEFINE v_tmp_diagnostico            SMALLINT;
   DEFINE v_resul_op_compl             SMALLINT;   
   DEFINE v_id_derechohabiente         DECIMAL(9,0);
   DEFINE v_reg_patronal_imss          CHAR(11);
   DEFINE v_periodo_pago               CHAR(6);
   DEFINE v_nss                        CHAR(11);
   DEFINE v_folio                      DECIMAL(9,0);
   DEFINE v_rp_folio                   DECIMAL(9,0); 
   DEFINE v_rp_reg_patronal_imss       CHAR(11);
   DEFINE v_rp_nss                     CHAR(11);
   DEFINE v_rp_periodo_pago            CHAR(6);
   DEFINE v_rp_resul_op                SMALLINT;
   DEFINE v_subcuenta_tmp              SMALLINT;
   DEFINE v_sub_cuenta                 SMALLINT;     
   DEFINE v_fondo_inversion            SMALLINT;
   DEFINE v_si_estado_marca            SMALLINT;
   DEFINE v_folio_complementario       DECIMAL(9,0);
   DEFINE v_si_estado                  SMALLINT;
   DEFINE v_si_regs_encontrados        INTEGER;
   DEFINE v_c_cadena                   CHAR(250); 
   DEFINE v_diagnostico                SMALLINT;
   DEFINE v_resp_nss                   CHAR(11); 
   DEFINE v_dpe_referencia             DECIMAL(9,0);

   DEFINE v_tmp_clave_afore_trabajador DECIMAL(3,0);

   DEFINE v_tmp_imp_ret_devolver       DECIMAL(12,6);
   DEFINE v_tmp_imp_act_recargos       DECIMAL(12,6);
   DEFINE v_tmp_imp_ces_vejez          DECIMAL(12,6);
   DEFINE v_tmp_imp_act_ces_vejez      DECIMAL(12,6);
   DEFINE v_tmp_imp_plusv_retiro       DECIMAL(12,6);
   DEFINE v_tmp_imp_minus_retiro       DECIMAL(12,6);
   DEFINE v_tmp_imp_comisiones_afore   DECIMAL(12,6);
   DEFINE v_tmp_imp_total_apo_patronal DECIMAL(12,6);
   DEFINE v_tmp_imp_plusv_apo_patronal DECIMAL(12,6);
   DEFINE v_tmp_num_aplicaciones_inter DECIMAL(12,6);
   DEFINE v_rp_imp_viv_dev             DECIMAL(12,6);
   DEFINE v_rp_aivs_viv_dev            DECIMAL(12,6);
   DEFINE v_acciones                   DECIMAL(12,6); 
   DEFINE v_pesos                      DECIMAL(12,6); 

   DEFINE v_monto_aplicado             DECIMAL(12,6);
   DEFINE v_porcentaje_dev             DECIMAL(6,2);
   DEFINE v_folio_liquidacion          DECIMAL(9,0);
   DEFINE v_tiene_respuesta            INTEGER;
   DEFINE v_folio_integra              DECIMAL(9,0);
   DEFINE v_rp_total_respuestas        INTEGER;
   DEFINE v_tot_regsistros             INTEGER;

   ON EXCEPTION SET sql_err, isam_err, err_txt
      
      LET v_i_resultado = sql_err;
      LET v_si_correcto_integra = 1;
      LET v_resp_nss = v_rp_nss;
      LET v_tot_regsistros = 0;
      
      RETURN v_i_resultado,
             err_txt,
             v_si_correcto_integra,
             v_rp_nss,
             v_tot_regsistros;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_dpe_integra_complementario.trace";
   SET DEBUG FILE TO "/safreviv_int/dpe/envio/trace_dpe_integra_complementario.trace";
   TRACE ON;

   LET sql_err                      = 0  ;
   LET isam_err                     = 0  ;
   LET err_txt                      = "" ;
   LET v_i_resultado                = 0  ;
   LET v_si_correcto_integra        = 0  ;
   LET v_tmp_num_reg_pat_imss       = ""; 
   LET v_tmp_rfc_patron             = ""; 
   LET v_tmp_per_pago               = ""; 
   LET v_tmp_folio_sua              = 0;  
   LET v_tmp_nss_aportacion         = ""; 
   LET v_tmp_rfc_trabajador         = ""; 
   LET v_tmp_curp                   = ""; 
   LET v_tmp_imp_ret_devolver       = 0.0;
   LET v_tmp_imp_act_recargos       = 0.0;
   LET v_tmp_imp_ces_vejez          = 0.0;
   LET v_tmp_imp_act_ces_vejez      = 0.0;
   LET v_tmp_imp_plusv_retiro       = 0.0;
   LET v_tmp_imp_minus_retiro       = 0.0;
   LET v_tmp_imp_comisiones_afore   = 0.0;
   LET v_tmp_imp_total_apo_patronal = 0.0;
   LET v_tmp_imp_plusv_apo_patronal = 0.0;
   LET v_tmp_clave_afore_trabajador = 0;  
   LET v_tmp_num_aplicaciones_inter = 0.0;
   LET v_monto_aplicado             = 0.0; 
   LET v_porcentaje_dev             = 0.0; 
   LET v_tmp_resul_operacion        = 0.0; 
   LET v_resul_operacion_compl      = 0.0; 
   LET v_tmp_diagnostico            = 0.0; 
   LET v_id_derechohabiente         = 0.0;   
   LET v_reg_patronal_imss          = "" ;   
   LET v_periodo_pago               = "" ;   
   LET v_nss                        = "" ;   
   LET v_folio                      = 0.0;   
   LET v_rp_folio                   = 0.0;   
   LET v_rp_reg_patronal_imss       = "" ;
   LET v_rp_nss                     = "" ;
   LET v_rp_periodo_pago            = "" ;
   LET v_rp_imp_viv_dev             = 0.00;
   LET v_rp_aivs_viv_dev            = 0.00;
   LET v_rp_resul_op                = 0  ;
   LET v_subcuenta_tmp              = 0  ;
   LET v_sub_cuenta                 = 0  ;     
   LET v_fondo_inversion            = 0  ;
   LET v_acciones                   = 0.00; 
   LET v_pesos                      = 0.00;    
   LET v_si_estado_marca            = 0  ;       
   LET v_folio_complementario       = 0.00;
   LET v_si_estado                  = 0  ;
   LET v_si_regs_encontrados        = 0  ;   
   LET v_c_cadena                   = "" ;
   LET v_diagnostico                = "";          
   LET v_dpe_referencia             = 0;
   LET v_tiene_respuesta            = 0;
   LET v_rp_total_respuestas        = 0;
   LET v_tot_regsistros             = 0;
   

   FOREACH --Consulta datos de temporal de PROCESAR      
      SELECT num_reg_pat_imss,
             rfc_patron,
             per_pago,
             folio_sua,
             nss_aportacion,
             rfc_trabajador,
             curp,
             imp_ret_devolver/100,
             imp_act_recargos/100,
             imp_ces_vejez/100,
             imp_act_ces_vejez/100,
             imp_plusv_retiro/100,
             imp_minus_retiro/100,
             imp_comisiones_afore/100,
             imp_total_apo_patronal/100,
             imp_plusv_apo_patronal/100,
             clave_afore_trabajador,
             num_aplicaciones_inter/1000000,
             resultado_operacion,    --Respuesta de procesar
             diagnostico1            --Aceptado, parcial 
      INTO   v_tmp_num_reg_pat_imss       ,
             v_tmp_rfc_patron             ,
             v_tmp_per_pago               ,
             v_tmp_folio_sua              ,
             v_tmp_nss_aportacion         ,
             v_tmp_rfc_trabajador         ,
             v_tmp_curp                   ,
             v_tmp_imp_ret_devolver       ,
             v_tmp_imp_act_recargos       ,
             v_tmp_imp_ces_vejez          ,
             v_tmp_imp_act_ces_vejez      ,
             v_tmp_imp_plusv_retiro       ,
             v_tmp_imp_minus_retiro       ,
             v_tmp_imp_comisiones_afore   ,
             v_tmp_imp_total_apo_patronal ,
             v_tmp_imp_plusv_apo_patronal ,
             v_tmp_clave_afore_trabajador ,
             v_tmp_num_aplicaciones_inter ,
             v_tmp_resul_operacion        ,
             v_tmp_diagnostico
      FROM   safre_tmp:tmp_dpe_det_det_trabajador
      
      FOREACH
         --Busca si ya recibio alguna respuesta de PROCESAR
         SELECT folio,
                reg_patronal_imss,
                nss,
                periodo_pago,
                imp_viv_dev,
                aivs_viv_dev,
                resul_op,
                count(*)
         INTO   v_rp_folio,
                v_rp_reg_patronal_imss,
                v_rp_nss,
                v_rp_periodo_pago,
                v_rp_imp_viv_dev,
                v_rp_aivs_viv_dev,
                v_rp_resul_op,
                v_rp_total_respuestas
         FROM   dpe_resp_procesar
         WHERE  reg_patronal_imss = v_tmp_num_reg_pat_imss
         AND    periodo_pago      = v_tmp_per_pago
         AND    nss               = v_tmp_nss_aportacion
         GROUP BY 1,2,3,4,5,6,7
         
         --IF v_rp_nss IS NULL THEN
         IF v_rp_total_respuestas  = 0 THEN 
            --Se recupera la referencia 
            SELECT seq_dpe_sol_trab_complementario.NEXTVAL
            INTO   v_dpe_referencia
	          FROM   SYSTABLES
	          WHERE  tabname = "seq_dpe_sol_trab_complementario";      
	          
	          LET v_tmp_diagnostico = "";
	          LET v_folio_integra = p_folio;
            --Se insertan en tabla de complementarios los registros que no se tienen en el historico 
            INSERT INTO dpe_sol_trab_complementario(id_dpe_compl_referencia,
                                                    folio_integra,
                                                    num_reg_pat_imss,
                                                    rfc_patron,
                                                    per_pago,
                                                    folio_sua,
                                                    nss_aportacion,
                                                    rfc_trabajador,
                                                    curp,
                                                    imp_ret_devolver,
                                                    imp_act_recargos,
                                                    imp_ces_vejez,
                                                    imp_act_ces_vejez,
                                                    imp_plusv_retiro,
                                                    imp_minus_retiro,
                                                    imp_comisiones_afore,
                                                    imp_total_apo_patronal,
                                                    imp_plusv_apo_patronal,
                                                    clave_afore_trabajador,
                                                    num_aplicaciones_inter,
                                                    monto_aplicado,
                                                    porcentaje_dev,
                                                    resul_operacion,
                                                    resul_operacion_compl,
                                                    diagnostico)
                                             VALUES(v_dpe_referencia,
                                                    v_folio_integra,
                                                    v_tmp_num_reg_pat_imss,
                                                    v_tmp_rfc_patron,
                                                    v_tmp_per_pago,
                                                    v_tmp_folio_sua,
                                                    v_tmp_nss_aportacion,
                                                    v_tmp_rfc_trabajador,
                                                    v_tmp_curp,
                                                    v_tmp_imp_ret_devolver,
                                                    v_tmp_imp_act_recargos,
                                                    v_tmp_imp_ces_vejez,
                                                    v_tmp_imp_act_ces_vejez,
                                                    v_tmp_imp_plusv_retiro,
                                                    v_tmp_imp_minus_retiro,
                                                    v_tmp_imp_comisiones_afore,
                                                    v_tmp_imp_total_apo_patronal,
                                                    v_tmp_imp_plusv_apo_patronal,
                                                    v_tmp_clave_afore_trabajador,
                                                    v_tmp_num_aplicaciones_inter,
                                                    v_monto_aplicado,
                                                    v_porcentaje_dev,
                                                    v_tmp_resul_operacion,
                                                    v_resul_operacion_compl,
                                                    v_tmp_diagnostico);
            
            --Total de registros insertados                                         
            LET v_tot_regsistros = v_tot_regsistros + 1;
            
            --Se tiene que validar si el registro fue aceptado y tiene diagnóstico 01 
            IF v_tmp_resul_operacion = "01" OR v_tmp_resul_operacion = 1 THEN       
               --Valida que el NSS recibido exista en SAFRE
               SELECT id_derechohabiente 
               INTO   v_id_derechohabiente
               FROM   afi_derechohabiente
               WHERE  nss = v_tmp_nss_aportacion;
         
               --Se recupera la referencia 
               SELECT seq_dpe_sol_trab_complementario.CURRVAL
               INTO   v_dpe_referencia
	             FROM   SYSTABLES
	             WHERE  tabname = "seq_dpe_sol_trab_complementario";      
	          
               LET v_si_estado_marca = 0;
               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                                401,                    -- marca de imms
                                                v_dpe_referencia,
                                                p_folio,                                                     
                                                0,                      -- estado marca                         
                                                0,                      -- codigo de rechazo                    
                                                0,                      -- marca de la causa                    
                                                NULL,                   -- fecha de la causa                    
                                                p_usuario_cod,                                                     
                                                p_proceso_cod)                                                     
               INTO v_si_estado_marca;
         
               --Si la cuenta no tiene alguna marca
               IF v_si_estado_marca = 0 THEN                
                  --Si el derechohabiente existe
                  IF v_id_derechohabiente IS NOT NULL THEN             
                     LET v_subcuenta_tmp = 4;
                     FOREACH 
                        --Si el diagnóstico es aceptado se obtiene el saldo del día            
                        EXECUTE FUNCTION fn_saldo_actual(v_tmp_nss_aportacion,v_subcuenta_tmp,TODAY)
                           INTO v_sub_cuenta,             
                                v_fondo_inversion,        
                                v_acciones,               
                                v_pesos

                        --Si el saldo es mayor a cero ejecuta marca, preliquidación
                        IF v_acciones > 0 AND  v_pesos > 0 THEN
                           --Comparar saldo contra la solicitud // Determinar si es pago parcial o total 
                           IF(v_acciones >= v_tmp_num_aplicaciones_inter)THEN
                              -- Aplica pago de tipo total.
                              LET v_tmp_diagnostico  = 0; -- Pago por liquidar
                              LET v_porcentaje_dev   = 100;
                              LET v_monto_aplicado   = v_tmp_num_aplicaciones_inter;
         
                              UPDATE dpe_sol_trab_complementario
                              SET    porcentaje_dev        = v_porcentaje_dev,
                                     resul_operacion_compl = 1, --Aceptado (1) o rechazado (2)
                                     diagnostico           = v_diagnostico  --Total(0) o Parcial (1)
                              WHERE  num_reg_pat_imss      = v_tmp_num_reg_pat_imss
                                AND  per_pago              = v_tmp_per_pago
                                AND  nss_aportacion        = v_tmp_nss_aportacion;
                           ELSE
                              -- Indica que si hay importe pero no se cubre el total
                              --  solo una parcialidad.
                              LET v_diagnostico    = 1; -- pago parcial
                              LET v_monto_aplicado   = v_acciones;
                              LET v_porcentaje_dev   = ((v_acciones*100)/v_tmp_num_aplicaciones_inter);
         
                              UPDATE dpe_sol_trab_complementario
                              SET    porcentaje_dev        = v_porcentaje_dev,
                                     resul_operacion_compl = 1, --Aceptado (1) o rechazado (2)
                                     diagnostico           = v_diagnostico  --Total(0) o Parcial (1)
                              WHERE  num_reg_pat_imss      = v_tmp_num_reg_pat_imss
                                AND  per_pago              = v_tmp_per_pago
                                AND  nss_aportacion        = v_tmp_nss_aportacion;
                           END IF           
                        ELSE
                           LET v_rp_resul_op    = 2;                             
                           LET v_diagnostico    = 21; -- RECHAZO POR FALTA DE SALDO
                           LET v_porcentaje_dev = 0;
                        
                           UPDATE dpe_sol_trab_complementario                    
                           SET    resul_operacion_compl = v_rp_resul_op, --Aceptado (1) o rec
                                  diagnostico           = v_diagnostico
                           WHERE  num_reg_pat_imss      = v_tmp_num_reg_pat_imss      
                             AND  per_pago              = v_tmp_per_pago                      
                             AND  nss_aportacion        = v_tmp_nss_aportacion;
                        END IF --Si cuenta tiene saldo                
                     END FOREACH;
                  ELSE
                     --Si derechohabiente no existe   
                     LET v_rp_resul_op    = 2;
                     LET v_diagnostico    = 10; --NO SE ENCONTRO NSS EN BASE DE DATOS
                     LET v_porcentaje_dev = 0;
                  
                     UPDATE dpe_sol_trab_complementario
                     SET    resul_operacion_compl = v_rp_resul_op, --Aceptado (1) o rechazado (2)
                            diagnostico           = v_diagnostico
                     WHERE  num_reg_pat_imss      = v_tmp_num_reg_pat_imss
                       AND  per_pago              = v_tmp_per_pago
                       AND  nss_aportacion        = v_tmp_nss_aportacion;
                  END IF --Si derechohabiente existe
               --Si la cuenta tiene alguna marca
               ELSE
                  LET v_rp_resul_op    = 2;
                  LET v_diagnostico    = 24;  --Rechazo por marca activa o 
                  LET v_porcentaje_dev = 0;
         
                  UPDATE dpe_sol_trab_complementario
                  SET    resul_operacion_compl = v_rp_resul_op, --Aceptado (1) o rechazado (2)
                         diagnostico           = v_diagnostico
                  WHERE  num_reg_pat_imss      = v_tmp_num_reg_pat_imss
                    AND  per_pago              = v_tmp_per_pago
                    AND  nss_aportacion        = v_tmp_nss_aportacion;
               END IF
            END IF--Fin registro tiene diagnostico ACEPTADO 
         END IF --Fin si no hay respuestas anteriores
      END FOREACH;   
   END FOREACH; --Consulta datos de temporal                  

   -- se actualiza glo_folio
   UPDATE glo_folio
      SET status      = 2
    WHERE folio       = p_folio
      AND opera_cod   = p_opera_cod
      AND proceso_cod = p_proceso_cod;
   --TERMINA PRELIQUIDACION

   UPDATE STATISTICS FOR TABLE dpe_sol_trab_complementario;

 RETURN v_i_resultado,                             
        err_txt,                                   
        v_si_correcto_integra,
        v_tmp_nss_aportacion,
        v_tot_regsistros;
END FUNCTION;


