






CREATE PROCEDURE "safreviv".sp_carga_inicial_aclaraciones_debug(p_folio DECIMAL(9,0))
RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(11)
   --TABLA tmp_det_aclaracion
   DEFINE tmp_det_acl_tpo_registro         char(1);
   DEFINE tmp_det_acl_cve_ent_receptora    char(3);
   DEFINE tmp_det_acl_f_pago_patron        date;
   DEFINE tmp_det_acl_nrp                  char(11);
   DEFINE tmp_det_acl_periodo_pago         char(6);
   DEFINE tmp_det_acl_folio_sua            char(6);
   DEFINE tmp_det_acl_nss                  char(11);
   DEFINE tmp_det_acl_rfc                  char(13);
   DEFINE tmp_det_acl_curp                 char(18);
   DEFINE tmp_det_acl_num_crd_ifv          char(10);
   DEFINE tmp_det_acl_f_ini_desc_cre_ifv   date;
   DEFINE tmp_det_acl_nombre_trabajador    char(50);
   DEFINE tmp_det_acl_ult_sdi              decimal(15,2);
   DEFINE tmp_det_acl_dias_cot_bim         decimal(4,0);
   DEFINE tmp_det_acl_dias_incap_bim       decimal(4,0);
   DEFINE tmp_det_acl_dias_ausent_bim      decimal(4,0);
   DEFINE tmp_det_acl_imp_ap_pat           decimal(12,2);
   DEFINE tmp_det_acl_imp_am_cre           decimal(12,2);
   DEFINE tmp_det_acl_ind_crd_ifv          char(1);
   DEFINE tmp_det_acl_tpo_aclaracion       char(2);
   DEFINE tmp_det_acl_aivs                 decimal(18,6);
   DEFINE tmp_det_acl_valor_aiv            decimal(18,6);
   DEFINE tmp_det_acl_int_pag_extemp       DECIMAL(7,2);
   DEFINE tmp_det_acl_aivs_pag_extemp      DECIMAL(15,6);


   --TABLA tmp_sum_aclaracion
   DEFINE tmp_sum_acl_tpo_registro         CHAR(1);
   DEFINE tmp_sum_acl_tot_ap               DECIMAL(9,0);
   DEFINE tmp_sum_acl_suma_ap_pat          DECIMAL(20,0);
   DEFINE tmp_sum_acl_suma_am              DECIMAL(20,0);
   DEFINE tmp_sum_acl_suma_aivs            DECIMAL(20,0);
   DEFINE tmp_sum_acl_suma_int_pag_extemp  DECIMAL(13,2);

   --TABLA acl_sum_aclaracion
   DEFINE acl_sum_acl_folio                DECIMAL(9,0);
   DEFINE acl_sum_acl_tpo_registro         CHAR(2);
   DEFINE acl_sum_acl_tot_ap               INTEGER;
   DEFINE acl_sum_acl_suma_ap_pat          DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_am              DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_aivs            DECIMAL(18,6);
   DEFINE acl_sum_acl_int_pag_extemp       DECIMAL(18,2); -- suma de intereses de pago extemporaneo
   
   DEFINE ld_ide_derechohabiente           DECIMAL(9,0);
   DEFINE contador   DECIMAL(9,0);
   DEFINE v_tipo_trabajador   CHAR(1);

   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

   -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         DECIMAL(12,0); -- registros detalle
   DEFINE v_num_registros_detalle_sum     DECIMAL(12,0); -- registros de detalle indicados en sumario

   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;
   DEFINE v_err_suma_int_pag_extemp_no_coincide         SMALLINT; -- error por diferencia en intereses de pagos extemporaneos
   
   DEFINE tmp_det_acl_nrp_short            CHAR(2);

   DEFINE v_ind_liquidacion SMALLINT;
   DEFINE v_tpo_patron      CHAR(02);
   DEFINE v_result_operacion SMALLINT;
DEFINE v_nss CHAR(11);
   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss;
   END EXCEPTION

   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   LET v_err_suma_int_pag_extemp_no_coincide         = 5;
   
   LET tmp_det_acl_nss = "NSSINICIO";
   
   LET tmp_det_acl_nrp_short = "";

   LET v_tpo_patron = "";
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/historicoenaclara.trace';
   --TRACE 'Inicia el store procedure de registro historicos de EN ACLARACION';
   -- se inicia el contador
   LET contador = 1;

   --TRACE 'inicia el foreach de la tabla tmp_det_sc_nss';
   FOREACH SELECT  tpo_registro 
                  ,cve_ent_receptora 
                  ,f_pago_patron 
                  ,nrp 
                  ,periodo_pago 
                  ,folio_sua 
                  ,nss  
                  ,rfc  
                  ,curp 
                  ,num_crd_ifv  
                  ,f_ini_desc_cre_ifv
                  ,nom_trabajador 
                  ,ult_sdi /100
                  ,dias_cot_bim   
                  ,dias_incap_bim 
                  ,dias_ausent_bim
                  ,imp_ap_pat /100
                  ,imp_am_cre /100
                  ,ind_crd_ifv 
                  ,tpo_aclaracion 
                  ,aivs      /1000000
                  ,valor_aiv /1000000
                  ,int_pag_extemp  / 100    -- campos agregados 6 sep 2012
                  ,aivs_pag_extemp / 1000000
           INTO  tmp_det_acl_tpo_registro
                ,tmp_det_acl_cve_ent_receptora
                ,tmp_det_acl_f_pago_patron
                ,tmp_det_acl_nrp
                ,tmp_det_acl_periodo_pago
                ,tmp_det_acl_folio_sua
                ,tmp_det_acl_nss
                ,tmp_det_acl_rfc
                ,tmp_det_acl_curp
                ,tmp_det_acl_num_crd_ifv
                ,tmp_det_acl_f_ini_desc_cre_ifv
                ,tmp_det_acl_nombre_trabajador
                ,tmp_det_acl_ult_sdi
                ,tmp_det_acl_dias_cot_bim
                ,tmp_det_acl_dias_incap_bim
                ,tmp_det_acl_dias_ausent_bim
                ,tmp_det_acl_imp_ap_pat
                ,tmp_det_acl_imp_am_cre
                ,tmp_det_acl_ind_crd_ifv
                ,tmp_det_acl_tpo_aclaracion
                ,tmp_det_acl_aivs
                ,tmp_det_acl_valor_aiv
                ,tmp_det_acl_int_pag_extemp  -- campos agregados 6 sep 2012
                ,tmp_det_acl_aivs_pag_extemp
           FROM safre_mig:tmp_det_aclaracion
           
           
           

     --Inicializacion de variables
      LET ld_ide_derechohabiente = NULL;
      LET v_tpo_patron = tmp_det_acl_nrp;
      LET v_result_operacion = 1;
      
--======================================================================
      -- Regla Negocio de Adelantamientos--

      IF v_tpo_patron = "99" THEN

         LET v_tipo_trabajador = "S";
         LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI

      ELSE

         LET v_tipo_trabajador = "I";

         IF (tmp_det_acl_tpo_aclaracion = "13" OR
             tmp_det_acl_tpo_aclaracion = "17") THEN

            LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

         ELSE

            LET v_ind_liquidacion  = 1; --ACL normal SIN liquidar
            LET v_result_operacion = 2;
            
         END IF
      END IF

--======================================================================

      --Obtiene el identificador del derechohabiente
      SELECT id_derechohabiente
        INTO ld_ide_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_det_acl_nss;

      LET v_nss = tmp_det_acl_nss;

      --Si no existe informacion en la tabla "afi_derechohabiente",
      --entonces se inserta el registro en dicha tabla (aperturar una cuenta)
      IF (ld_ide_derechohabiente IS NULL) THEN
                                                                       
           --LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(tmp_det_acl_nss 
           --                                                 ,tmp_det_acl_curp
           --                                                 ,tmp_det_acl_rfc 
           --                                                 ,1
           --                                                 ,tmp_det_acl_nombre_trabajador
           --                                                 ,v_tipo_trabajador
           --                                                 ,0                   -- Credito. se da por omision
           --                                                 ,4                   --4=CARGA INICIAL ENACLARA,
           --                                                 ,p_folio             -- folio del lote
           --                                                 ,"R");               -- origen afiliacion
         --TRACE "despues de derechohab. null";
      END IF

      LET  tmp_det_acl_nrp_short =  tmp_det_acl_nrp;
      -- Inserta a tabla cta_his_pagos para control de historicos
                           
                     
                           
      LET contador = contador + 1;
           
           
           
   END FOREACH;
   
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss;
END PROCEDURE;


