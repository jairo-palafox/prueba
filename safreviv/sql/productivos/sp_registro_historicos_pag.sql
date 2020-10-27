






CREATE PROCEDURE "safreviv".sp_registro_historicos_pag(ld_folio DECIMAL(9,0), 
                                            p_pid DECIMAL(9,0),
                                            p_proceso_cod SMALLINT
                                            )

RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR (11)

   DEFINE lr_tmp_cza_recauda_tpo_registro    CHAR(2);
   DEFINE lr_tmp_cza_recauda_id_servicio     CHAR(2);
   DEFINE lr_tmp_cza_recauda_id_operacion    CHAR(2);
   DEFINE lr_tmp_cza_recauda_tpo_ent_origen  CHAR(2);
   DEFINE lr_tmp_cza_recauda_cve_ent_origen  CHAR(3);
   DEFINE lr_tmp_cza_recauda_tpo_ent_destino CHAR(2);
   DEFINE lr_tmp_cza_recauda_cve_ent_destino CHAR(3);
   DEFINE lr_tmp_cza_recauda_f_transferencia DATE   ;
   DEFINE lr_tmp_cza_recauda_consecutivo_dia CHAR(3);
   DEFINE lr_tmp_cza_recauda_mod_recp_envio  CHAR(2);

   DEFINE vperiodo CHAR(06);
   DEFINE vmes     SMALLINT;
   DEFINE vano     SMALLINT;

   DEFINE lr_tmp_cza_ent_recauda_tpo_registro    CHAR(2);
   DEFINE lr_tmp_cza_ent_recauda_id_servicio     CHAR(2);
   DEFINE lr_tmp_cza_ent_recauda_tpo_ent_origen  CHAR(2);
   DEFINE lr_tmp_cza_ent_recauda_cve_ent_origen  CHAR(3);
   DEFINE lr_tmp_cza_ent_recauda_tpo_ent_destino CHAR(2);
   DEFINE lr_tmp_cza_ent_recauda_cve_ent_destino CHAR(3);
   DEFINE lr_tmp_cza_ent_recauda_cve_ent_recep   CHAR(3);
   DEFINE lr_tmp_cza_ent_recauda_periodo_pago    CHAR(6);
   DEFINE lr_tmp_cza_ent_recauda_f_transferencia DATE   ;

   DEFINE lr_tmp_cza_pag_patronal_tpo_registro       CHAR(2) ;
   DEFINE lr_tmp_cza_pag_patronal_id_servicio        CHAR(2) ;
   DEFINE lr_tmp_cza_pag_patronal_cve_ent_recep      CHAR(3);
   DEFINE lr_tmp_cza_pag_patronal_nrp                CHAR(11);
   DEFINE lr_tmp_cza_pag_patronal_rfc                CHAR(13);
   DEFINE lr_tmp_cza_pag_patronal_periodo_pago       CHAR(6);
   DEFINE lr_tmp_cza_pag_patronal_folio_sua          CHAR(6) ;
   DEFINE lr_tmp_cza_pag_patronal_tpo_inf_patron     CHAR(1) ;
   DEFINE lr_tmp_cza_pag_patronal_f_pago             DATE    ;
   DEFINE lr_tmp_cza_pag_patronal_f_valor_rcv        DATE    ;
   DEFINE lr_tmp_cza_pag_patronal_f_valor_4seguros   DATE    ;
   DEFINE lr_tmp_cza_pag_patronal_razon_social       CHAR(50);
   DEFINE lr_tmp_cza_pag_patronal_calle              CHAR(40);
   DEFINE lr_tmp_cza_pag_patronal_deleg              CHAR(40);
   DEFINE lr_tmp_cza_pag_patronal_ent_fed            CHAR(2) ;
   DEFINE lr_tmp_cza_pag_patronal_cp                 CHAR(5) ;
   DEFINE lr_tmp_cza_pag_patronal_telefono           CHAR(15);
   DEFINE lr_tmp_cza_pag_patronal_act_econ           CHAR(40);
   DEFINE lr_tmp_cza_pag_patronal_area_geo_sm        CHAR(1) ;
   DEFINE lr_tmp_cza_pag_patronal_porcent_ap         SMALLINT;
   DEFINE lr_tmp_cza_pag_patronal_conv_reem_subsidio CHAR(1) ;
   DEFINE lr_tmp_cza_pag_patronal_tot_dias_cot_sdesc INTEGER;
   --modifique este 
   DEFINE lr_tmp_cza_pag_patronal_num_trab_cot       DECIMAL(12,0);

   DEFINE lr_tmp_sum_pag_patronal_tpo_registro CHAR(2)    ;
   DEFINE lr_tmp_sum_pag_patronal_id_servicio  CHAR(2)    ;
   DEFINE lr_tmp_sum_pag_patronal_nrp          CHAR(11)   ;
   DEFINE lr_tmp_sum_pag_patronal_periodo_pago CHAR(6)    ;
   DEFINE lr_tmp_sum_pag_patronal_folio_sua    CHAR(6)    ;
   DEFINE lr_tmp_sum_pag_patronal_num_trab_crd INTEGER    ;
      
   DEFINE lr_tmp_sum_pag_patronal_diag_confronta CHAR(2)      ;
   DEFINE lr_tmp_sum_pag_patronal_folio_req      CHAR(6)      ;
   DEFINE lr_tmp_sum_pag_patronal_cve_ent_recep  CHAR(3);

   DEFINE lr_pag_sum_pag_patronal_tasa_actualiz         DECIMAL(12,6);
   DEFINE lr_pag_sum_pag_patronal_tasa_recargos         DECIMAL(12,6);
   DEFINE lr_pag_sum_pag_patronal_imp_tot_ap_pat        DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_ap_pat            DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_ap_pat_am_crd     DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_tot_am_crd        DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_am_crd            DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_act_ap_pat_am_crd DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_rec_ap_pat_am_crd DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_imp_ren_viv_pgo_ext   DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_aiv_ap_pat            DECIMAL(18,6);
   DEFINE lr_pag_sum_pag_patronal_valor_aiv             DECIMAL(18,6);
   DEFINE lr_pag_sum_pag_patronal_multas_ifv            DECIMAL(12,2);
   DEFINE lr_pag_sum_pag_patronal_donativo_fundemex     DECIMAL(12,2);
   DEFINE lr_pag_sum_pag_patronal_int_gen_pgo_ext       DECIMAL(15,2);
   DEFINE lr_pag_sum_pag_patronal_aiv_gen_pgo_ext       DECIMAL(18,6);

   DEFINE lr_tmp_sum_ent_recauda_tpo_registro         CHAR(2);
   DEFINE lr_tmp_sum_ent_recauda_id_servicio          CHAR(2);
   DEFINE lr_tmp_sum_ent_recauda_id_operacion         CHAR(2);
   DEFINE lr_tmp_sum_ent_recauda_tpo_ent_origen       CHAR(2);
   DEFINE lr_tmp_sum_ent_recauda_cve_ent_origen       CHAR(3);
   DEFINE lr_tmp_sum_ent_recauda_tpo_ent_destino      CHAR(2);
   DEFINE lr_tmp_sum_ent_recauda_cve_ent_destino      CHAR(3);
   DEFINE lr_tmp_sum_ent_recauda_cve_ent_receptora    CHAR(3);
   DEFINE lr_tmp_sum_ent_recauda_f_transferencia      DATE   ;
   DEFINE lr_tmp_sum_ent_recauda_tot_reg_patronales   INTEGER;
   DEFINE lr_tmp_sum_ent_recauda_tot_reg_trabajadores INTEGER;
   DEFINE lr_tmp_sum_ent_recauda_tot_reg_movimientos  INTEGER;

   DEFINE lr_tmp_sum_recauda_tpo_registro    CHAR(2)      ;
   DEFINE lr_tmp_sum_recauda_id_servicio     CHAR(2)      ;
   DEFINE lr_tmp_sum_recauda_id_operacion    CHAR(2)      ;
   DEFINE lr_tmp_sum_recauda_tpo_ent_origen  CHAR(2)      ;
   DEFINE lr_tmp_sum_recauda_cve_ent_origen  CHAR(3)      ;
   DEFINE lr_tmp_sum_recauda_tpo_ent_destino CHAR(2)      ;
   DEFINE lr_tmp_sum_recauda_cve_ent_destino CHAR(3)      ;
   DEFINE lr_tmp_sum_recauda_f_transferencia DATE         ;
   DEFINE lr_tmp_sum_recauda_consecutivo_dia CHAR(3)      ;
   DEFINE lr_tmp_sum_recauda_total_registros INTEGER      ;
   
   DEFINE lr_pag_sum_recauda_imp_tot_ap_pat        DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_ap_pat            DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_ap_pat_am_crd     DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_tot_am_crd        DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_am_crd            DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_act_ap_pat_am_crd DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_rec_ap_pat_am_crd DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_ren_viv_pgo_ext   DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_aiv_ap_pat            DECIMAL(18,6);
   DEFINE lr_pag_sum_recauda_imp_multas_ifv        DECIMAL(18,2);
   DEFINE lr_pag_sum_recauda_imp_donativo_fundemex DECIMAL(18,2);

   DEFINE lr_tmp_det_trabajador_nss                  CHAR(11)     ;

   DEFINE ld_ide_derechohabiente  DECIMAL(9);
   DEFINE lc_cve_ent_recep        CHAR(3);
   DEFINE lc_periodo_pago         CHAR(6);
   DEFINE li_contador             INTEGER;
   DEFINE v_error                 SMALLINT;
   
      --TABLA tmp_sum_recauda

   DEFINE tmp_sum_pag_imp_ap_pat               DECIMAL(18,2);
   DEFINE tmp_sum_pag_suma_imp_amd_crd         DECIMAL(18,2);
   DEFINE tmp_sum_pag_suma_imp_ren_viv_pgo_ext DECIMAL(18,2);
   DEFINE tmp_sum_pag_suma_aiv_ap_pat          DECIMAL(18,6);
   DEFINE tmp_sum_pag_imp_ap_pat_am_crd        DECIMAL(18,6);
   DEFINE tmp_sum_pag_imp_ap_pat_suma          DECIMAL(18,6);

   --TABLA tmp_det_trabajador
   DEFINE tmp_sum_pag_tot_imp_ap_pat          DECIMAL(18,2);
   DEFINE tmp_sum_pag_tot_imp_am_cre          DECIMAL(18,2);
   DEFINE tmp_sum_pag_tot_imp_ren_viv_pgo_ext DECIMAL(18,2);
   DEFINE tmp_sum_pag_tot_aiv_ap_pat          DECIMAL(18,6);
   
     -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle     INTEGER; -- registros detalle
   DEFINE v_num_registros_detalle_sum INTEGER; -- registros de detalle indicados en sumario

    -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide      SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide        SMALLINT;
   DEFINE v_err_suma_rendiemiento_no_coincide        SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                SMALLINT;
   DEFINE v_err_pago_duplicado                       SMALLINT;
   
   DEFINE v_seq_referencia                DECIMAL(9,0); ---Valor de secuencia de la tabla pag_det_trabajador
   DEFINE v_d_f_pago_tmp_cza_pag_patronal DATE        ; -- fecha de pago del encabezado
   DEFINE v_c_tpo_patron                  CHAR(2)     ;
   
     -- Control de Excepciones
   DEFINE isam_err       INTEGER       ;
   DEFINE err_txt        VARCHAR(255)  ;
   DEFINE v_c_msj        VARCHAR(255)  ;
   DEFINE v_si_resultado SMALLINT      ;
   DEFINE v_d_fecha_hoy  DATE          ;

   DEFINE v_i_cont_tmp_cza_ent_recauda  INTEGER ;
   DEFINE v_i_cont_tmp_cza_pag_patronal INTEGER ;
   DEFINE v_i_cont_tmp_det_trabajador   INTEGER ;
   DEFINE v_i_cont_tmp_mov_incidencias  INTEGER ;
   DEFINE v_i_cont_tmp_sum_pag_patronal INTEGER ;
   DEFINE v_i_cont_tmp_sum_ent_recauda  INTEGER ;
   
   DEFINE v_tpo_trabajador CHAR(01);
   
   DEFINE v_periodo_pago  CHAR(6);
   DEFINE v_nrp           CHAR(11);
   DEFINE v_folio_sua     CHAR(6);
   DEFINE v_cve_ent_recep CHAR(3);
   DEFINE v_f_pago        DATE;
                       
{  --sinclave                     
   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      RETURN v_error, isam_err, err_txt, lr_tmp_det_trabajador_nss ;
   END EXCEPTION 
   
   SET DEBUG FILE TO '/safreviv_int/BD/sp_registro_historicos_pag.trace';
}

   LET li_contador = 1;   
   LET v_d_fecha_hoy = TODAY ;
   LET lr_tmp_det_trabajador_nss = "SIN ASIGNAR";

    -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide      = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide = 2;
   LET v_err_suma_amortizacion_no_coincide        = 3;
   LET v_err_suma_rendiemiento_no_coincide        = 4;
   LET v_err_suma_AIVS_no_coincide                = 5;
   LET v_err_pago_duplicado                       = 6;

   LET tmp_sum_pag_imp_ap_pat_suma = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = ld_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = ld_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = ld_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

   SELECT  
      SUM(imp_ap_pat)/100          ,
      SUM(imp_ap_pat_am_crd)/100   ,      
      SUM(imp_am_crd)/100          ,
      SUM(imp_ren_viv_pgo_ext )/100,
      SUM(aiv_ap_pat)/1000000       
   INTO    
      tmp_sum_pag_imp_ap_pat              , 
      tmp_sum_pag_imp_ap_pat_am_crd       ,
      tmp_sum_pag_suma_imp_amd_crd        , 
      tmp_sum_pag_suma_imp_ren_viv_pgo_ext, 
      tmp_sum_pag_suma_aiv_ap_pat           
   FROM tmp_sum_recauda;

   -- se obtiene la suma de los monto de la tabla de detalle       
   SELECT 
      SUM(imp_ap_pat) / 100         ,
      SUM(imp_am_cre) / 100         ,
      SUM(imp_ren_viv_pgo_ext) / 100,
      SUM(aiv_ap_pat) / 1000000
   INTO 
      tmp_sum_pag_tot_imp_ap_pat           ,
      tmp_sum_pag_tot_imp_am_cre           ,
      tmp_sum_pag_tot_imp_ren_viv_pgo_ext  ,
      tmp_sum_pag_tot_aiv_ap_pat            
   FROM tmp_det_trabajador;       

   -- se verifica que el numero de registros en la tabla de detalle sea el mismo
   -- que el encontrado en el sumario
      
   SELECT COUNT(*)
   INTO   v_i_cont_tmp_cza_ent_recauda 
   FROM   tmp_cza_ent_recauda  ;
   
   SELECT COUNT(*)  
   INTO   v_i_cont_tmp_cza_pag_patronal 
   FROM   tmp_cza_pag_patronal  ;
   
   SELECT COUNT(*)  
   INTO   v_i_cont_tmp_det_trabajador 
   FROM   tmp_det_trabajador  ;
   
   SELECT COUNT(*) 
   INTO   v_i_cont_tmp_mov_incidencias 
   FROM   tmp_mov_incidencias  ;
   
   SELECT COUNT(*)  
   INTO   v_i_cont_tmp_sum_pag_patronal 
   FROM   tmp_sum_pag_patronal ;
   
   SELECT COUNT(*)  
   INTO   v_i_cont_tmp_sum_ent_recauda 
   FROM   tmp_sum_ent_recauda  ;  
      
    --se hace la suma de los registroS insertados en las tablas temporales   
   LET v_num_registros_detalle =  v_i_cont_tmp_cza_ent_recauda   + 
                                  v_i_cont_tmp_cza_pag_patronal  +
                                  v_i_cont_tmp_det_trabajador    +
                                  v_i_cont_tmp_mov_incidencias   +
                                  v_i_cont_tmp_sum_pag_patronal  +
                                  v_i_cont_tmp_sum_ent_recauda   ; 

   SELECT total_registros
   INTO   v_num_registros_detalle_sum
   FROM   tmp_sum_recauda;
   
   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_cza_ent_recauda < 1 OR v_i_cont_tmp_cza_ent_recauda IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de encabezado de entidad recaudadora.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_cza_pag_patronal < 1 OR v_i_cont_tmp_cza_pag_patronal IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de encabezado de pago patronal.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_det_trabajador < 1 OR v_i_cont_tmp_det_trabajador IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de detalle por trabajador.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_mov_incidencias < 1 OR v_i_cont_tmp_mov_incidencias IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de incidencias.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_sum_pag_patronal < 1 OR v_i_cont_tmp_sum_pag_patronal IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de sumario de pago patronal.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_i_cont_tmp_sum_ent_recauda < 1 OR v_i_cont_tmp_sum_ent_recauda IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de sumario de entidad recaudadora.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- se verifica que se haya recibido al menos un registro por cada tabla temporal
   IF ( v_num_registros_detalle_sum < 1 OR v_num_registros_detalle_sum IS NULL ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "No se encontro registro de sumario de recaudacion.";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF
   
   -- ========================================================================
   -- se verifican las cifras de montos totales recibidos
   -- ========================================================================
   -- aportaciones patronales
   -- se consideran la suma de las aportaciones patronales 
   
   LET tmp_sum_pag_imp_ap_pat_suma = tmp_sum_pag_imp_ap_pat + tmp_sum_pag_imp_ap_pat_am_crd ;
  
   IF ( tmp_sum_pag_imp_ap_pat_suma <> tmp_sum_pag_tot_imp_ap_pat ) THEN

      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "La suma de aportaciones patronales de detalle no corresponde con el reportado en el sumario. " ||
                            tmp_sum_pag_imp_ap_pat_suma || " - " || tmp_sum_pag_tot_imp_ap_pat ;   --sinclave
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF
   
   -- amortizacion del credito
   IF ( tmp_sum_pag_suma_imp_amd_crd <> tmp_sum_pag_tot_imp_am_cre ) THEN

      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "La suma de amortización del crédito de detalle rgado no corresponde con el reportado en el sumario: " || 
                            tmp_sum_pag_tot_imp_am_cre || " - " || tmp_sum_pag_suma_imp_amd_crd ;  --sinclave
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- aplicacion de rendimiento subcuenta de  pagos extemporaneos
   IF ( tmp_sum_pag_suma_imp_ren_viv_pgo_ext <> tmp_sum_pag_tot_imp_ren_viv_pgo_ext ) THEN

      LET v_si_resultado = v_err_suma_rendiemiento_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "La suma de rendimientos de pagos extemporaneos de detalle no corresponde con el reportado en el sumario" ||
                           tmp_sum_pag_suma_imp_ren_viv_pgo_ext || " - " ||  tmp_sum_pag_tot_imp_ren_viv_pgo_ext;   --sinclave

      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   -- aplicacion de rendimiento subcuenta de  pagos extemporaneos
   IF ( tmp_sum_pag_suma_aiv_ap_pat <> tmp_sum_pag_tot_aiv_ap_pat ) THEN

      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "La suma de aplicación de intereses de vivienda de detalle no corresponde con el reportado en el sumario" ||
                            tmp_sum_pag_suma_aiv_ap_pat || " - " || tmp_sum_pag_tot_aiv_ap_pat;      --sinclave

      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF

   CREATE INDEX tmp_cza_pag_pat_idx on tmp_cza_pag_patronal (periodo_pago,nrp,folio_sua,cve_ent_recep,f_pago);
   UPDATE STATISTICS FOR TABLE tmp_cza_pag_patronal;


----------- SE COMENTA ESTA FUNCIONALIDAD POR SOLICITUD DE JIRA-301 -----------
{
   -- validacion de pagos duplicados --
   IF EXISTS (
              SELECT his.folio
              FROM   pag_cza_pag_patronal his, tmp_cza_pag_patronal dup
              WHERE  his.periodo_pago   = dup.periodo_pago
              AND    his.nrp            = dup.nrp
              AND    his.folio_sua      = dup.folio_sua 
              AND    his.cve_ent_recep  = dup.cve_ent_recep
              AND    his.f_pago         = dup.f_pago) THEN

      FOREACH
         SELECT periodo_pago,
                nrp,
                folio_sua,
                cve_ent_recep,
                f_pago
         INTO   v_periodo_pago, 
                v_nrp,          
                v_folio_sua,    
                v_cve_ent_recep,
                v_f_pago       
         FROM   tmp_cza_pag_patronal
 
         IF EXISTS (SELECT "G"
                    FROM   pag_cza_pag_patronal
                    WHERE  periodo_pago  = v_periodo_pago 
                    AND    nrp           = v_nrp         
                    AND    folio_sua     = v_folio_sua   
                    AND    cve_ent_recep = v_cve_ent_recep
                    AND    f_pago        = v_f_pago) THEN
                    
            LET v_si_resultado = v_err_pago_duplicado;
            LET isam_err       = 0;
            LET err_txt        = "Ya existe un pago en el historico, periodo_pago: " ||v_periodo_pago || 
                                 " nrp: " ||v_nrp || "folio sua: " ||v_folio_sua || 
                                 " cve ent recep: " ||v_cve_ent_recep || " f_pago: "||v_f_pago;  
         END IF;
         
         EXIT FOREACH;
      END FOREACH
      
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;      
   
   END IF;
 
----------- SE COMENTA ESTA FUNCIONALIDAD POR SOLICITUD DE JIRA-301 -----------   
}      
      
   FOREACH SELECT 
   	       tpo_registro,
           id_servicio,
           id_operacion,
           tpo_ent_origen,
           cve_ent_origen,
           tpo_ent_destino,
           cve_ent_destino,
           f_transferencia,
           consecutivo_dia,
           mod_recp_envio          
      INTO lr_tmp_cza_recauda_tpo_registro   ,
           lr_tmp_cza_recauda_id_servicio    ,
           lr_tmp_cza_recauda_id_operacion   ,
           lr_tmp_cza_recauda_tpo_ent_origen ,
           lr_tmp_cza_recauda_cve_ent_origen ,
           lr_tmp_cza_recauda_tpo_ent_destino,
           lr_tmp_cza_recauda_cve_ent_destino,
           lr_tmp_cza_recauda_f_transferencia,
           lr_tmp_cza_recauda_consecutivo_dia,
           lr_tmp_cza_recauda_mod_recp_envio
      FROM tmp_cza_recauda

      LET li_contador = li_contador + 1;

      INSERT INTO pag_cza_recauda(
                                  folio                   ,
                                  tpo_registro            ,
                                  id_servicio             ,
                                  id_operacion            ,
                                  tpo_ent_origen          ,
                                  cve_ent_origen          ,
                                  tpo_ent_destino         ,
                                  cve_ent_destino         ,
                                  f_transferencia         ,
                                  consecutivo_dia         ,
                                  mod_recp_envio          
                                 )
                          VALUES(
                                  ld_folio, 
                                  lr_tmp_cza_recauda_tpo_registro,
                                  lr_tmp_cza_recauda_id_servicio, 
                                  lr_tmp_cza_recauda_id_operacion,
                                  lr_tmp_cza_recauda_tpo_ent_origen, 
                                  lr_tmp_cza_recauda_cve_ent_origen,
                                  lr_tmp_cza_recauda_tpo_ent_destino, 
                                  lr_tmp_cza_recauda_cve_ent_destino,
                                  lr_tmp_cza_recauda_f_transferencia, 
                                  lr_tmp_cza_recauda_consecutivo_dia,
                                  lr_tmp_cza_recauda_mod_recp_envio 
                                 );
   END FOREACH
   
   
   LET vmes = MONTH(lr_tmp_cza_recauda_f_transferencia);
   LET vano = YEAR(lr_tmp_cza_recauda_f_transferencia);
   
   IF vmes = 1 THEN
      LET vmes = 12;
      LET vano = vano -1;
   ELSE
      LET vmes = vmes -1;
   END IF;
   
   
   LET vperiodo = vano||LPAD(vmes,2,"0");
   
   LET li_contador = 1;
      
   FOREACH SELECT tpo_registro       ,
                  id_servicio        ,
                  tpo_ent_origen     ,
                  cve_ent_origen     ,
                  tpo_ent_destino    ,
                  cve_ent_destino    ,
                  cve_ent_recep      ,
                  periodo_pago       ,
                  f_transferencia    
            INTO lr_tmp_cza_ent_recauda_tpo_registro   ,
                 lr_tmp_cza_ent_recauda_id_servicio    ,
                 lr_tmp_cza_ent_recauda_tpo_ent_origen ,
                 lr_tmp_cza_ent_recauda_cve_ent_origen ,
                 lr_tmp_cza_ent_recauda_tpo_ent_destino,
                 lr_tmp_cza_ent_recauda_cve_ent_destino,
                 lr_tmp_cza_ent_recauda_cve_ent_recep  ,
                 lr_tmp_cza_ent_recauda_periodo_pago   ,
                 lr_tmp_cza_ent_recauda_f_transferencia
            FROM tmp_cza_ent_recauda

      LET li_contador = li_contador + 1;

      INSERT INTO pag_cza_ent_recauda(folio                ,
                                      periodo_pago         ,
                                      cve_ent_recep        ,
                                      tpo_registro         ,
                                      id_servicio          ,
                                      tpo_ent_origen       ,
                                      cve_ent_origen       ,
                                      tpo_ent_destino      ,
                                      cve_ent_destino      ,
                                      f_transferencia      
                                     )      
                               VALUES(ld_folio, 
                                      lr_tmp_cza_ent_recauda_periodo_pago,
                                      lr_tmp_cza_ent_recauda_cve_ent_recep,   
                                      lr_tmp_cza_ent_recauda_tpo_registro,
                                      lr_tmp_cza_ent_recauda_id_servicio,     
                                      lr_tmp_cza_ent_recauda_tpo_ent_origen,
                                      lr_tmp_cza_ent_recauda_cve_ent_origen,  
                                      lr_tmp_cza_ent_recauda_tpo_ent_destino,
                                      lr_tmp_cza_ent_recauda_cve_ent_destino, 
                                      lr_tmp_cza_ent_recauda_f_transferencia 
                                     );
   END FOREACH

   LET li_contador = 1;

   FOREACH SELECT tpo_registro    ,
            id_servicio           ,
            cve_ent_recep         ,
            nrp                   ,
            rfc                   ,
            periodo_pago          ,
            folio_sua             ,
            tpo_inf_patron        ,
            f_pago                ,
            f_valor_rcv           ,
            f_valor_4seguros      ,
            razon_social          ,
            calle                 ,
            deleg                 ,
            ent_fed               ,
            cp                    ,
            telefono              ,
            act_econ              ,
            area_geo_sm           ,
            porcent_ap            ,
            conv_reem_subsidio    ,
            tot_dias_cot_sdesc    ,
            num_trab_cot             

      INTO lr_tmp_cza_pag_patronal_tpo_registro      ,    --tpo_registro
           lr_tmp_cza_pag_patronal_id_servicio       ,    --id_servicio
           lr_tmp_cza_pag_patronal_cve_ent_recep     ,    --cve_ent_recep
           lr_tmp_cza_pag_patronal_nrp               ,    --nrp
           lr_tmp_cza_pag_patronal_rfc               ,    --rfc
           lr_tmp_cza_pag_patronal_periodo_pago      ,    --periodo_pago
           lr_tmp_cza_pag_patronal_folio_sua         ,    --folio_sua
           lr_tmp_cza_pag_patronal_tpo_inf_patron    ,    --tpo_inf_patron
           lr_tmp_cza_pag_patronal_f_pago            ,    --f_pago
           lr_tmp_cza_pag_patronal_f_valor_rcv       ,    --f_valor_rcv
           lr_tmp_cza_pag_patronal_f_valor_4seguros  ,    --f_valor_4seguros
           lr_tmp_cza_pag_patronal_razon_social      ,    --razon_social
           lr_tmp_cza_pag_patronal_calle             ,    --calle
           lr_tmp_cza_pag_patronal_deleg             ,    --deleg
           lr_tmp_cza_pag_patronal_ent_fed           ,    --ent_fed
           lr_tmp_cza_pag_patronal_cp                ,    --cp
           lr_tmp_cza_pag_patronal_telefono          ,    --telefono
           lr_tmp_cza_pag_patronal_act_econ          ,    --act_econ
           lr_tmp_cza_pag_patronal_area_geo_sm       ,    --area_geo_sm
           lr_tmp_cza_pag_patronal_porcent_ap        ,    --porcent_ap
           lr_tmp_cza_pag_patronal_conv_reem_subsidio,    --conv_reem_subsidio
           lr_tmp_cza_pag_patronal_tot_dias_cot_sdesc,    --tot_dias_cot_sdesc
           lr_tmp_cza_pag_patronal_num_trab_cot           --num_trab_cot
      FROM tmp_cza_pag_patronal

      LET li_contador = li_contador + 1;

      INSERT INTO pag_cza_pag_patronal(
                       folio               ,
                       periodo_pago        ,
                       nrp                 ,
                       folio_sua           ,
                       cve_ent_recep       ,
                       tpo_registro        ,
                       id_servicio         ,
                       rfc                 ,
                       tpo_inf_patron      ,
                       f_pago              ,
                       f_valor_rcv         ,
                       f_valor_4seguros    ,
                       razon_social        ,
                       calle               ,
                       deleg               ,
                       ent_fed             ,
                       cp                  ,
                       telefono            ,
                       act_econ            ,
                       area_geo_sm         ,
                       porcent_ap          ,
                       conv_reem_subsidio  ,
                       tot_dias_cot_sdesc  ,
                       num_trab_cot        
                      ) 
               VALUES( ld_folio                                    , --folio
                       lr_tmp_cza_pag_patronal_periodo_pago        , --periodo_pago
                       lr_tmp_cza_pag_patronal_nrp                 , --nrp
                       lr_tmp_cza_pag_patronal_folio_sua           , --folio_sua
                       lr_tmp_cza_pag_patronal_cve_ent_recep       , --cve_ent_recep
                       lr_tmp_cza_pag_patronal_tpo_registro        , --tpo_registro
                       lr_tmp_cza_pag_patronal_id_servicio         , --id_servicio
                       lr_tmp_cza_pag_patronal_rfc                 , --rfc
                       lr_tmp_cza_pag_patronal_tpo_inf_patron      , --tpo_inf_patron
                       lr_tmp_cza_pag_patronal_f_pago              , --f_pago
                       lr_tmp_cza_pag_patronal_f_valor_rcv         , --f_valor_rcv
                       lr_tmp_cza_pag_patronal_f_valor_4seguros    , --f_valor_4seguros
                       lr_tmp_cza_pag_patronal_razon_social        , --razon_social
                       lr_tmp_cza_pag_patronal_calle               , --calle
                       lr_tmp_cza_pag_patronal_deleg               , --deleg
                       lr_tmp_cza_pag_patronal_ent_fed             , --ent_fed
                       lr_tmp_cza_pag_patronal_cp                  , --cp
                       lr_tmp_cza_pag_patronal_telefono            , --telefono
                       lr_tmp_cza_pag_patronal_act_econ            , --act_econ
                       lr_tmp_cza_pag_patronal_area_geo_sm         , --area_geo_sm
                       lr_tmp_cza_pag_patronal_porcent_ap          , --porcent_ap
                       lr_tmp_cza_pag_patronal_conv_reem_subsidio  , --conv_reem_subsidio
                       lr_tmp_cza_pag_patronal_tot_dias_cot_sdesc  , --tot_dias_cot_sdesc
                       lr_tmp_cza_pag_patronal_num_trab_cot        --num_trab_cot
                      );
   END FOREACH

   LET li_contador = 1;

   FOREACH SELECT tpo_registro      ,
           id_servicio              ,
           nrp                      ,
           periodo_pago             ,
           folio_sua                ,
           num_trab_crd             ,
           tasa_actualiz/1000000    ,
           tasa_recargos/1000000    ,
           imp_tot_ap_pat/100       ,
           imp_ap_pat/100           ,
           imp_ap_pat_am_crd/100    ,
           imp_tot_am_crd/100       ,
           imp_am_crd/100           ,
           imp_act_ap_pat_am_crd/100,
           imp_rec_ap_pat_am_crd/100,
           imp_ren_viv_pgo_ext/100  ,
           diag_confronta           ,
           aiv_ap_pat/1000000       ,
           valor_aiv/1000000        ,
           multas_ifv/100           ,
           donativo_fundemex/100    ,
           folio_req                ,
           int_gen_pgo_ext/100      ,
           aiv_gen_pgo_ext/1000000           

      INTO lr_tmp_sum_pag_patronal_tpo_registro         ,    --tpo_registro     
           lr_tmp_sum_pag_patronal_id_servicio          ,    --id_servicio      
           lr_tmp_sum_pag_patronal_nrp                  ,    --nrp              
           lr_tmp_sum_pag_patronal_periodo_pago         ,    --periodo_pago     
           lr_tmp_sum_pag_patronal_folio_sua            ,    --folio_sua        
           lr_tmp_sum_pag_patronal_num_trab_crd         ,    --num_trab_crd     
           lr_pag_sum_pag_patronal_tasa_actualiz        ,    --tasa_actualiz    
           lr_pag_sum_pag_patronal_tasa_recargos        ,    --tasa_recargos    
           lr_pag_sum_pag_patronal_imp_tot_ap_pat       ,    --imp_tot_ap_pat   
           lr_pag_sum_pag_patronal_imp_ap_pat           ,    --imp_ap_pat       
           lr_pag_sum_pag_patronal_imp_ap_pat_am_crd    ,    --imp_ap_pat_am_crd
           lr_pag_sum_pag_patronal_imp_tot_am_crd       ,    --imp_tot_am_crd   
           lr_pag_sum_pag_patronal_imp_am_crd           ,    --imp_am_crd       
           lr_pag_sum_pag_patronal_imp_act_ap_pat_am_crd,    --imp_act_ap_pat_am+
           lr_pag_sum_pag_patronal_imp_rec_ap_pat_am_crd,    --imp_rec_ap_pat_am+
           lr_pag_sum_pag_patronal_imp_ren_viv_pgo_ext  ,    --imp_ren_viv_pgo_e+
           lr_tmp_sum_pag_patronal_diag_confronta       ,    --diag_confronta   
           lr_pag_sum_pag_patronal_aiv_ap_pat           ,    --aiv_ap_pat       
           lr_pag_sum_pag_patronal_valor_aiv            ,    --valor_aiv        
           lr_pag_sum_pag_patronal_multas_ifv           ,    --multas_ifv       
           lr_pag_sum_pag_patronal_donativo_fundemex    ,    --donativo_fundemex
           lr_tmp_sum_pag_patronal_folio_req            ,    --folio_req        
           lr_pag_sum_pag_patronal_int_gen_pgo_ext      ,    --int_gen_pgo_ext  
           lr_pag_sum_pag_patronal_aiv_gen_pgo_ext           --aiv_gen_pgo_ext  
      FROM tmp_sum_pag_patronal

      LET li_contador = li_contador + 1;

      INSERT INTO pag_sum_pag_patronal (
                 folio                  ,
                 periodo_pago           ,
                 nrp                    ,
                 folio_sua              ,
                 tpo_registro           ,
                 id_servicio            ,
                 num_trab_crd           ,
                 tasa_actualiz          ,
                 tasa_recargos          ,
                 imp_tot_ap_pat         ,
                 imp_ap_pat             ,
                 imp_ap_pat_am_crd      ,
                 imp_tot_am_crd         ,
                 imp_am_crd             ,
                 imp_act_ap_pat_am_crd  ,
                 imp_rec_ap_pat_am_crd  ,
                 imp_ren_viv_pgo_ext    ,
                 diag_confronta         ,
                 aiv_ap_pat             ,
                 valor_aiv              ,
                 multas_ifv             ,
                 donativo_fundemex      ,
                 folio_req              ,
                 int_gen_pgo_ext        ,
                 aiv_gen_pgo_ext        
               )

       VALUES(
               ld_folio                                        , --folio
               lr_tmp_sum_pag_patronal_periodo_pago            , --periodo_pago
               lr_tmp_sum_pag_patronal_nrp                     , --nrp
               lr_tmp_sum_pag_patronal_folio_sua               , --folio_sua
               lr_tmp_sum_pag_patronal_tpo_registro            , --tpo_registro
               lr_tmp_sum_pag_patronal_id_servicio             , --id_servicio
               lr_tmp_sum_pag_patronal_num_trab_crd            , --num_trab_crd
               lr_pag_sum_pag_patronal_tasa_actualiz           , --tasa_actualiz
               lr_pag_sum_pag_patronal_tasa_recargos           , --tasa_recargos
               lr_pag_sum_pag_patronal_imp_tot_ap_pat          , --imp_tot_ap_pat
               lr_pag_sum_pag_patronal_imp_ap_pat              , --imp_ap_pat
               lr_pag_sum_pag_patronal_imp_ap_pat_am_crd       , --imp_ap_pat_am_crd
               lr_pag_sum_pag_patronal_imp_tot_am_crd          , --imp_tot_am_crd
               lr_pag_sum_pag_patronal_imp_am_crd              , --imp_am_crd
               lr_pag_sum_pag_patronal_imp_act_ap_pat_am_crd   , --imp_act_ap_pat_am+
               lr_pag_sum_pag_patronal_imp_rec_ap_pat_am_crd   , --imp_rec_ap_pat_am+
               lr_pag_sum_pag_patronal_imp_ren_viv_pgo_ext     , --imp_ren_viv_pgo_e+
               lr_tmp_sum_pag_patronal_diag_confronta          , --diag_confronta
               lr_pag_sum_pag_patronal_aiv_ap_pat              , --aiv_ap_pat
               lr_pag_sum_pag_patronal_valor_aiv               , --valor_aiv
               lr_pag_sum_pag_patronal_multas_ifv              , --multas_ifv
               lr_pag_sum_pag_patronal_donativo_fundemex       , --donativo_fundemex
               lr_tmp_sum_pag_patronal_folio_req               , --folio_req
               lr_pag_sum_pag_patronal_int_gen_pgo_ext         , --int_gen_pgo_ext
               lr_pag_sum_pag_patronal_aiv_gen_pgo_ext      --aiv_gen_pgo_ext
              );  
   END FOREACH
   
   LET li_contador = 1;
   
   FOREACH SELECT tpo_registro    ,
           id_servicio            ,
           id_operacion           ,
           tpo_ent_origen         ,
           cve_ent_origen         ,
           tpo_ent_destino        ,
           cve_ent_destino        ,
           cve_ent_receptora      ,
           f_transferencia        ,
           tot_reg_patronales     ,
           tot_reg_trabajadores   ,
           tot_reg_movimientos  

      INTO lr_tmp_sum_ent_recauda_tpo_registro        ,  --tpo_registro
           lr_tmp_sum_ent_recauda_id_servicio         ,  --id_servicio
           lr_tmp_sum_ent_recauda_id_operacion        ,  --id_operacion
           lr_tmp_sum_ent_recauda_tpo_ent_origen      ,  --tpo_ent_origen
           lr_tmp_sum_ent_recauda_cve_ent_origen      ,  --cve_ent_origen
           lr_tmp_sum_ent_recauda_tpo_ent_destino     ,  --tpo_ent_destino
           lr_tmp_sum_ent_recauda_cve_ent_destino     ,  --cve_ent_destino
           lr_tmp_sum_ent_recauda_cve_ent_receptora   ,  --cve_ent_receptora
           lr_tmp_sum_ent_recauda_f_transferencia     ,  --f_transferencia
           lr_tmp_sum_ent_recauda_tot_reg_patronales  ,  --tot_reg_patronales
           lr_tmp_sum_ent_recauda_tot_reg_trabajadores,  --tot_reg_trabajado+
           lr_tmp_sum_ent_recauda_tot_reg_movimientos    --tot_reg_movimient+
      FROM tmp_sum_ent_recauda

      LET li_contador = li_contador + 1;
      LET lc_cve_ent_recep = NULL;
      LET lc_periodo_pago  = NULL;
      
      INSERT INTO pag_sum_ent_recauda (
              folio                ,
              cve_ent_receptora    ,
              tpo_registro         ,
              id_servicio          ,
              id_operacion         ,
              tpo_ent_origen       ,
              tpo_ent_destino      ,
              cve_ent_destino      ,
              f_transferencia      ,
              tot_reg_patronales   ,
              tot_reg_trabajadores ,
              tot_reg_movimientos   
             )
      
      VALUES(
             ld_folio                                     ,   --folio
             lr_tmp_sum_ent_recauda_cve_ent_receptora     ,   --cve_ent_receptora
             lr_tmp_sum_ent_recauda_tpo_registro          ,   --tpo_registro
             lr_tmp_sum_ent_recauda_id_servicio           ,   --id_servicio
             lr_tmp_sum_ent_recauda_id_operacion          ,   --id_operacion
             lr_tmp_sum_ent_recauda_tpo_ent_origen        ,   --tpo_ent_origen
             lr_tmp_sum_ent_recauda_tpo_ent_destino       ,   --tpo_ent_destino
             lr_tmp_sum_ent_recauda_cve_ent_destino       ,   --cve_ent_destino
             lr_tmp_sum_ent_recauda_f_transferencia       ,   --f_transferencia
             lr_tmp_sum_ent_recauda_tot_reg_patronales    ,   --tot_reg_patronales
             lr_tmp_sum_ent_recauda_tot_reg_trabajadores  ,   --tot_reg_trabajado+
             lr_tmp_sum_ent_recauda_tot_reg_movimientos     --tot_reg_movimient+
            );   
   END FOREACH

   LET li_contador = 1;

   FOREACH SELECT tpo_registro                 ,
                  id_servicio                  ,
                  id_operacion                 ,
                  tpo_ent_origen               ,
                  cve_ent_origen               ,
                  tpo_ent_destino              ,
                  cve_ent_destino              ,
                  f_transferencia              ,
                  consecutivo_dia              ,
                  total_registros              ,
                  imp_tot_ap_pat/100           ,
                  imp_ap_pat/100               ,
                  imp_ap_pat_am_crd/100        ,
                  imp_tot_am_crd/100           ,
                  imp_am_crd/100               ,
                  imp_act_ap_pat_am_crd/100    ,
                  imp_rec_ap_pat_am_crd/100    ,
                  imp_ren_viv_pgo_ext/100      ,
                  aiv_ap_pat/1000000           ,
                  imp_multas_ifv/100           ,
                  imp_donativo_fundemex/100        
      INTO lr_tmp_sum_recauda_tpo_registro         ,
           lr_tmp_sum_recauda_id_servicio          ,
           lr_tmp_sum_recauda_id_operacion         ,
           lr_tmp_sum_recauda_tpo_ent_origen       ,
           lr_tmp_sum_recauda_cve_ent_origen       ,
           lr_tmp_sum_recauda_tpo_ent_destino      ,
           lr_tmp_sum_recauda_cve_ent_destino      ,
           lr_tmp_sum_recauda_f_transferencia      ,
           lr_tmp_sum_recauda_consecutivo_dia      ,
           lr_tmp_sum_recauda_total_registros      ,
           lr_pag_sum_recauda_imp_tot_ap_pat       ,
           lr_pag_sum_recauda_imp_ap_pat           ,
           lr_pag_sum_recauda_imp_ap_pat_am_crd    ,
           lr_pag_sum_recauda_imp_tot_am_crd       ,
           lr_pag_sum_recauda_imp_am_crd           ,
           lr_pag_sum_recauda_imp_act_ap_pat_am_crd,
           lr_pag_sum_recauda_imp_rec_ap_pat_am_crd,
           lr_pag_sum_recauda_imp_ren_viv_pgo_ext  ,
           lr_pag_sum_recauda_aiv_ap_pat           ,
           lr_pag_sum_recauda_imp_multas_ifv       ,
           lr_pag_sum_recauda_imp_donativo_fundemex
      FROM tmp_sum_recauda

      LET li_contador = li_contador + 1;

      INSERT INTO pag_sum_recauda (
              folio                     ,
              tpo_registro              ,
              id_servicio               ,
              id_operacion              ,
              tpo_ent_origen            ,
              cve_ent_origen            ,
              tpo_ent_destino           ,
              cve_ent_destino           ,
              f_transferencia           ,
              consecutivo_dia           ,
              total_registros           ,
              imp_tot_ap_pat            ,
              imp_ap_pat                ,
              imp_ap_pat_am_crd         ,
              imp_tot_am_crd            ,
              imp_am_crd                ,
              imp_act_ap_pat_am_crd     ,
              imp_rec_ap_pat_am_crd     ,
              imp_ren_viv_pgo_ext       ,
              aiv_ap_pat                ,
              imp_multas_ifv            ,
              imp_donativo_fundemex   
             )
      VALUES(
              ld_folio,
              lr_tmp_sum_recauda_tpo_registro,
              lr_tmp_sum_recauda_id_servicio,
              lr_tmp_sum_recauda_id_operacion,
              lr_tmp_sum_recauda_tpo_ent_origen,
              lr_tmp_sum_recauda_cve_ent_origen,
              lr_tmp_sum_recauda_tpo_ent_destino,
              lr_tmp_sum_recauda_cve_ent_destino,
              lr_tmp_sum_recauda_f_transferencia,
              lr_tmp_sum_recauda_consecutivo_dia,
              lr_tmp_sum_recauda_total_registros,
              lr_pag_sum_recauda_imp_tot_ap_pat,
              lr_pag_sum_recauda_imp_ap_pat,
              lr_pag_sum_recauda_imp_ap_pat_am_crd,
              lr_pag_sum_recauda_imp_tot_am_crd,
              lr_pag_sum_recauda_imp_am_crd,
              lr_pag_sum_recauda_imp_act_ap_pat_am_crd,
              lr_pag_sum_recauda_imp_rec_ap_pat_am_crd,
              lr_pag_sum_recauda_imp_ren_viv_pgo_ext,
              lr_pag_sum_recauda_aiv_ap_pat,
              lr_pag_sum_recauda_imp_multas_ifv,
              lr_pag_sum_recauda_imp_donativo_fundemex
             );
   END FOREACH;
   UPDATE STATISTICS FOR TABLE pag_cza_ent_recauda;
   UPDATE STATISTICS FOR TABLE pag_cza_pag_patronal;
   UPDATE STATISTICS FOR TABLE pag_cza_recauda;
   UPDATE STATISTICS FOR TABLE pag_sum_ent_recauda;
   UPDATE STATISTICS FOR TABLE pag_sum_pag_patronal;
   UPDATE STATISTICS FOR TABLE pag_sum_recauda;

   -- EJECUTA DETALLE --
   
   EXECUTE PROCEDURE sp_registro_detalle_pag(ld_folio,vperiodo)
   INTO v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;

   IF v_si_resultado <> 0 THEN
      RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;
   END IF;
   
   UPDATE STATISTICS FOR TABLE cta_pag_complemento;
   UPDATE STATISTICS FOR TABLE cta_his_pagos;

   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;

END PROCEDURE;


