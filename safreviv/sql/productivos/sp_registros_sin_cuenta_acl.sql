






CREATE PROCEDURE "safreviv".sp_registros_sin_cuenta_acl(ld_folio DECIMAL(9,0))

   RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR (11)

   DEFINE lr_tmp_det_trabajador_tpo_registro         CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_id_servicio          CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_nrp                  CHAR(11)     ;
   DEFINE lr_tmp_det_trabajador_rfc_patron           CHAR(13)     ;
   DEFINE lr_tmp_det_trabajador_periodo_pago         CHAR(6)      ;
   DEFINE lr_tmp_det_trabajador_folio_sua            DECIMAL(6)   ;
   DEFINE lr_tmp_det_trabajador_nss                  CHAR(11)     ;
   DEFINE lr_tmp_det_trabajador_rfc                  CHAR(13)     ;
   DEFINE lr_tmp_det_trabajador_curp                 CHAR(18)     ;
   DEFINE lr_tmp_det_trabajador_num_crd_ifv          CHAR(10)     ;
   DEFINE lr_tmp_det_trabajador_f_ini_desc_crd_ifv   DATE         ;
   DEFINE lr_tmp_det_trabajador_num_mov_periodo      CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_nom_trabajador       CHAR(50)     ;

   DEFINE lr_tmp_det_trabajador_tpo_trabajador       CHAR(1)      ;
   DEFINE lr_tmp_det_trabajador_jornada              CHAR(1)      ;
   DEFINE lr_tmp_det_trabajador_localiza_trabajador  CHAR(1)      ;
   DEFINE lr_tmp_det_trabajador_destino_ap_viv       CHAR(1)      ;
   DEFINE lr_tmp_det_trabajador_dias_cot_bim         SMALLINT     ;
   DEFINE lr_tmp_det_trabajador_dias_incap_bim       SMALLINT     ;
   DEFINE lr_tmp_det_trabajador_dias_ausent_bim      SMALLINT     ;

   DEFINE lr_tmp_det_trabajador_marca_sua            CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_marca_bdnsa          CHAR(1)      ;
   DEFINE lr_tmp_det_trabajador_diag_confronta       CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_tpo_aclaracion       CHAR(2)      ;

   DEFINE lr_tmp_det_trabajador_tpo_patron           CHAR(2)      ;
   DEFINE lr_tmp_det_trabajador_ind_liquidacion      SMALLINT     ;

   DEFINE lr_pag_det_trabajador_ult_sdi              DECIMAL(7,2) ;
   DEFINE lr_pag_det_trabajador_imp_tot_ap_pat       DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_imp_ap_pat           DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_imp_tot_am_cre       DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_imp_am_cre           DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_imp_ren_viv_pgo_ext  DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_aiv_ap_pat           DECIMAL(18,6);
   DEFINE lr_pag_det_trabajador_valor_aiv            DECIMAL(18,6);
   DEFINE lr_pag_det_trabajador_int_gen_pgo_ext      DECIMAL(12,2);
   DEFINE lr_pag_det_trabajador_aiv_gen_pgo_ext      DECIMAL(18,6);

   DEFINE lr_pag_det_trabajador_cve_ent_recep        CHAR(03);
   DEFINE lr_pag_det_trabajador_fecha_pago           DATE;
   DEFINE lr_pag_det_trabajador_fecha_valor          DATE;

   DEFINE ld_ide_derechohabiente  DECIMAL(9);
   DEFINE lc_cve_ent_recep        CHAR(3);
   DEFINE lc_periodo_pago         CHAR(6);
   DEFINE li_contador             INTEGER;
   DEFINE v_error                 SMALLINT;

   DEFINE v_seq_referencia                DECIMAL(9,0); ---Valor de secuencia de la tabla pag_det_trabajador
   DEFINE v_d_f_pago_tmp_cza_pag_patronal DATE        ; -- fecha de pago del encabezado
   DEFINE v_c_tpo_patron                  CHAR(2)     ;
   
     -- Control de Excepciones
   DEFINE isam_err                        INTEGER       ;
   DEFINE err_txt                         VARCHAR(255)  ;
   DEFINE v_c_msj                         VARCHAR(255)  ;
   DEFINE v_si_resultado                  SMALLINT      ;
   DEFINE v_d_fecha_hoy                   DATE          ;
   DEFINE v_tpo_trabajador CHAR(01);

   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      RETURN v_error, isam_err, err_txt, lr_tmp_det_trabajador_nss ;
   END EXCEPTION 

   LET li_contador = 1;   
   LET v_d_fecha_hoy = TODAY ;
   LET lr_tmp_det_trabajador_nss = NULL;
   
   SET PDQPRIORITY HIGH;

--==================================================================
--=====  FOREACH DE NSS NO ENCONTRADOS APERTURA DE CUENTA      =====

   --Inicializacion de variables     
   LET ld_ide_derechohabiente = NULL;
   LET v_c_tpo_patron = NULL; 
      
   FOREACH SELECT
           tmp.nss                 ,
           tmp.nrp                 ,
           tmp.rfc                 ,
           tmp.curp                ,
           tmp.nombre_trabajador   ,
           tmp.localiza_trabajador 
        INTO 
           lr_tmp_det_trabajador_nss                 ,   --nss
           lr_tmp_det_trabajador_nrp                 ,   --nrp
           lr_tmp_det_trabajador_rfc                 ,   --rfc
           lr_tmp_det_trabajador_curp                ,   --curp
           lr_tmp_det_trabajador_nom_trabajador      ,   --nom_trabajador
           lr_tmp_det_trabajador_localiza_trabajador     --localiza_trabajador
     FROM  safre_tmp:tmp_det_sc_nss tmp
     WHERE tmp.nss NOT IN (SELECT afi.nss FROM afi_derechohabiente afi)

      --se asigna el tipo de patron con las posiciones 1,2 del nrp 
      LET v_c_tpo_patron = lr_tmp_det_trabajador_nrp ;
            
      IF v_c_tpo_patron = "99" THEN
         LET v_tpo_trabajador = "S";
      ELSE
         LET v_tpo_trabajador = "I";
      END IF
      
      -- APERTURA DE CUENTA 
      SELECT a.id_derechohabiente
      INTO   ld_ide_derechohabiente
      FROM   afi_derechohabiente a
      WHERE  a.nss = lr_tmp_det_trabajador_nss;
      
      IF ld_ide_derechohabiente IS NULL THEN
         LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(lr_tmp_det_trabajador_nss ,
                                                             lr_tmp_det_trabajador_curp,
                                                             lr_tmp_det_trabajador_rfc ,
                                                             1,
                                                             lr_tmp_det_trabajador_nom_trabajador,
                                                             v_tpo_trabajador,
                                                             0, -- Credito. se da por omision
                                                             5,--5=ACLARATORIO SIN CAMBIO NSS
                                                             ld_folio, -- folio del lote
                                                             "R" -- origen afiliacion
                                                             ); 
      END IF

   END FOREACH ;
----===========================================================   
   

   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;

END PROCEDURE;


