






CREATE PROCEDURE "safreviv".sp_registro_detalle_pag(ld_folio DECIMAL(9,0),p_periodo CHAR(06))

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
   DEFINE v_indices_deshabilitados        SMALLINT      ; -- 0=No se deshabilitaron. 1=Se deshabilitaron
   DEFINE v_tpo_trabajador CHAR(01);
   DEFINE v_conteo                        DECIMAL(9,0);
   DEFINE v_bim_pago       CHAR(06);
   
   DEFINE v_id_referencia DECIMAL(9,0);


   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      RETURN v_error, isam_err, err_txt, lr_tmp_det_trabajador_nss ;
   END EXCEPTION 


   LET li_contador = 1;   
   LET v_d_fecha_hoy = TODAY ;
   LET lr_tmp_det_trabajador_nss = "SIN ASIGNAR";

--g-   ALTER SEQUENCE seq_cta_his_pagos RESTART 1;
   
   -- se asume que no se deshabilitaran los indices
   LET v_indices_deshabilitados = 0;
   LET v_conteo = 0;
   LET v_id_referencia = 0;


--=================================================

   CREATE TEMP TABLE tmp_nss_cuenta
   ( 
     nss CHAR(11)
   );
 
   SET PDQPRIORITY HIGH;

   -- se cuentan los registros
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   tmp_det_trabajador;


   -- si el conteo es igual o mayor a 1 millon de registros, se deshabilitan los indices
  IF ( v_conteo > 1999999 ) THEN 

      -- inserta en tabla de masivo para manejo de dispersión
      
      IF v_conteo > 2999999 THEN
       
         EXECUTE PROCEDURE fn_bimestre_pago(p_periodo) INTO v_bim_pago;
         INSERT INTO pag_masivo VALUES (ld_folio,p_periodo,v_bim_pago,1);
      
      END IF;
      
      -- se activa la bandera de indices deshabilitados
      LET v_indices_deshabilitados = 1;

      -- se deshabilitan los indices
      SET INDEXES xie2cta_his_pagos DISABLED;
      SET INDEXES xie4cta_his_pagos DISABLED;
   END IF

--=====================================================
--=====   PRIMER FOREACH DE NSS ENCONTRADOS       =====

   FOREACH 
      SELECT afi.id_derechohabiente, 
   	         tmp.tpo_registro             ,
             tmp.id_servicio              ,
             tmp.nrp                      ,
             tmp.rfc_patron               ,
             tmp.periodo_pago             ,
             tmp.folio_sua                ,
             tmp.nss                      ,
             tmp.rfc                      ,
             tmp.curp                     ,
             tmp.num_crd_ifv              ,
             tmp.f_ini_desc_crd_ifv       ,
             tmp.num_mov_periodo          ,
             tmp.nom_trabajador           ,
             tmp.ult_sdi/100              ,
             tmp.tpo_trabajador           ,
             tmp.jornada                  ,
             tmp.localiza_trabajajdor     ,
             tmp.destino_ap_viv           ,
             tmp.dias_cot_bim             ,
             tmp.dias_incap_bim           ,
             tmp.dias_ausent_bim          ,
             tmp.imp_tot_ap_pat/100       ,
             tmp.imp_ap_pat/100           ,
             tmp.imp_tot_am_cre/100       ,
             tmp.imp_am_cre/100           ,
             tmp.imp_ren_viv_pgo_ext/100  ,
             tmp.marca_sua                ,
             tmp.marca_bdnsa              ,
             tmp.diag_confronta           ,
             tmp.tpo_aclaracion           ,
             tmp.aiv_ap_pat/1000000       ,
             tmp.valor_aiv/1000000        ,
             tmp.int_gen_pgo_ext/100      ,
             tmp.aiv_gen_pgo_ext/1000000  ,
             tmp.cve_ent_recep,
             tmp.fecha_pago,
             tmp.fecha_valor
      INTO   ld_ide_derechohabiente,
             lr_tmp_det_trabajador_tpo_registro        ,   --tpo_registro         
             lr_tmp_det_trabajador_id_servicio         ,   --id_servicio          
             lr_tmp_det_trabajador_nrp                 ,   --nrp                  
             lr_tmp_det_trabajador_rfc_patron          ,   --rfc_patron           
             lr_tmp_det_trabajador_periodo_pago        ,   --periodo_pago         
             lr_tmp_det_trabajador_folio_sua           ,   --folio_sua            
             lr_tmp_det_trabajador_nss                 ,   --nss                  
             lr_tmp_det_trabajador_rfc                 ,   --rfc                  
             lr_tmp_det_trabajador_curp                ,   --curp                 
             lr_tmp_det_trabajador_num_crd_ifv         ,   --num_crd_ifv          
             lr_tmp_det_trabajador_f_ini_desc_crd_ifv  ,   --f_ini_desc_crd_ifv   
             lr_tmp_det_trabajador_num_mov_periodo     ,   --num_mov_periodo      
             lr_tmp_det_trabajador_nom_trabajador      ,   --nom_trabajador       
             lr_pag_det_trabajador_ult_sdi             ,   --ult_sdi              
             lr_tmp_det_trabajador_tpo_trabajador      ,   --tpo_trabajador       
             lr_tmp_det_trabajador_jornada             ,   --jornada              
             lr_tmp_det_trabajador_localiza_trabajador ,   --localiza_trabajaj+   
             lr_tmp_det_trabajador_destino_ap_viv      ,   --destino_ap_viv       
             lr_tmp_det_trabajador_dias_cot_bim        ,   --dias_cot_bim         
             lr_tmp_det_trabajador_dias_incap_bim      ,   --dias_incap_bim       
             lr_tmp_det_trabajador_dias_ausent_bim     ,   --dias_ausent_bim      
             lr_pag_det_trabajador_imp_tot_ap_pat      ,   --imp_tot_ap_pat       
             lr_pag_det_trabajador_imp_ap_pat          ,   --imp_ap_pat           
             lr_pag_det_trabajador_imp_tot_am_cre      ,   --imp_tot_am_cre       
             lr_pag_det_trabajador_imp_am_cre          ,   --imp_am_cre           
             lr_pag_det_trabajador_imp_ren_viv_pgo_ext ,   --imp_ren_viv_pgo_e+   
             lr_tmp_det_trabajador_marca_sua           ,   --marca_sua            
             lr_tmp_det_trabajador_marca_bdnsa         ,   --marca_bdnsa          
             lr_tmp_det_trabajador_diag_confronta      ,   --diag_confronta       
             lr_tmp_det_trabajador_tpo_aclaracion      ,   --tpo_aclaracion       
             lr_pag_det_trabajador_aiv_ap_pat          ,   --aiv_ap_pat           
             lr_pag_det_trabajador_valor_aiv           ,   --valor_aiv            
             lr_pag_det_trabajador_int_gen_pgo_ext     ,   --int_gen_pgo_ext      
             lr_pag_det_trabajador_aiv_gen_pgo_ext     ,  --aiv_gen_pgo_ext       
             lr_pag_det_trabajador_cve_ent_recep       , 
             lr_pag_det_trabajador_fecha_pago          , 
             lr_pag_det_trabajador_fecha_valor
      FROM   tmp_det_trabajador tmp,
             afi_derechohabiente afi 
      WHERE tmp.nss = afi.nss

      -- asignación para cuando trae valores en nulo 12-feb-2012
      IF lr_pag_det_trabajador_int_gen_pgo_ext IS NULL THEN
         LET lr_pag_det_trabajador_int_gen_pgo_ext = 0;
      END IF

      IF lr_pag_det_trabajador_aiv_gen_pgo_ext IS NULL THEN
         LET lr_pag_det_trabajador_aiv_gen_pgo_ext = 0;
      END IF

      INSERT INTO tmp_nss_cuenta VALUES ( lr_tmp_det_trabajador_nss );

      IF lr_tmp_det_trabajador_num_crd_ifv = "/" THEN
         LET lr_tmp_det_trabajador_num_crd_ifv = "0";
      END IF

      --se asigna el tipo de patron con las posiciones 1,2 del nrp 
      LET v_c_tpo_patron = lr_tmp_det_trabajador_nrp ;


      IF (lr_tmp_det_trabajador_tpo_aclaracion=" " OR
          lr_tmp_det_trabajador_tpo_aclaracion="  " OR
          lr_tmp_det_trabajador_tpo_aclaracion IS NULL) THEN
          LET lr_tmp_det_trabajador_tpo_aclaracion = "26";
      END IF

      -- Regla Negocio de Adelantamientos--
      IF lr_tmp_det_trabajador_localiza_trabajador = "3" THEN  -- Aclaración
         
         IF v_c_tpo_patron = "99" THEN
         
            LET lr_tmp_det_trabajador_ind_liquidacion = 2;  --ACL adelantada liquidada SI
            
         ELSE

            IF (lr_tmp_det_trabajador_tpo_aclaracion = "13" OR
               lr_tmp_det_trabajador_tpo_aclaracion = "17") THEN
               
               LET lr_tmp_det_trabajador_ind_liquidacion = 3; --ACL adelantada liquidada IMSS
               
            ELSE   
               
               LET lr_tmp_det_trabajador_ind_liquidacion = 1; --ACL normal SIN liquidar
                     
            END IF
         END IF
      ELSE

         LET lr_tmp_det_trabajador_ind_liquidacion = 0;     --Aporte Normla liquidado

      END IF

      -- cambio por j741 portabilidad  -- nvo port                                             ---g--  --xvi-58
      IF lr_tmp_det_trabajador_destino_ap_viv = "3" THEN     -- Portabilidad                   ---g--  --xvi-58
         IF lr_tmp_det_trabajador_ind_liquidacion	<> 1 THEN  -- ACL normal sin liquidar        ---g--  --xvi-58
            LET lr_pag_det_trabajador_aiv_ap_pat      = lr_pag_det_trabajador_imp_ap_pat;      ---g--  --xvi-58
            LET lr_pag_det_trabajador_aiv_gen_pgo_ext = lr_pag_det_trabajador_int_gen_pgo_ext; ---g--  --xvi-58
         END IF
      END IF
    
      ---g--
      -- Se comenta siguiente "IF" por correo enviado por Hamir 
      -- con fehca 18-feb-2016, hora: 10:24 y asunto: incidente 926244
      -- Se aplica regla en el "IF" de arriba de este comentario,
      -- donde se colocan pesos a las aivs cuando del detino = 3 y
      -- ademas el ind_liquidacion sea <> 1, o sea aplica para
      -- adelantos y pago normal. Referencia ---g--
      -- ---g- cambio por j741 portabilidad     -- nvo port
      -- ---g- IF lr_tmp_det_trabajador_destino_ap_viv = "3" THEN            --- Portabilidad    
      -- ---g-    IF lr_tmp_det_trabajador_localiza_trabajador	<> "3" THEN   --- Aclaracion
      -- ---g-       LET lr_pag_det_trabajador_aiv_ap_pat      = lr_pag_det_trabajador_imp_ap_pat;
      -- ---g-       LET lr_pag_det_trabajador_aiv_gen_pgo_ext = lr_pag_det_trabajador_int_gen_pgo_ext;
      -- ---g-    END IF
      -- ---g- END IF 
   
      --g-
      LET v_id_referencia = v_id_referencia + 1;
                   
      INSERT INTO cta_his_pagos 
         (
         folio     ,  
         origen_archivo             ,  
         id_referencia              ,  
         cve_ent_receptora          ,  
         nrp                        ,  
         periodo_pago               ,  
         folio_sua                  ,  
         f_pago                     ,  
         id_derechohabiente         ,  
         localiza_trabajador        ,  
         tpo_aclaracion             ,  
         imp_ap_pat                 ,  
         imp_am_cre                 ,  
         imp_ren_viv_pgo_ext        ,  
         aiv_ap_pat                 ,  
         valor_aiv                  ,  
         int_gen_pgo_ext            ,  
         aiv_gen_pgo_ext            ,  
         result_operacion           ,  
         ind_liquidacion            ,  
         num_crd_ifv                ,  
         f_proceso                  ,  
         tpo_patron                 ,     
         folio_referencia           ,  
         f_valor                    ,
         destino_ap_viv             
         )                
      VALUES     
         (
         ld_folio                                  ,                           
         1                                         ,  -- origen_archivo=1      
--g-         seq_cta_his_pagos.NEXTVAL                 ,                           
         v_id_referencia,
         lr_pag_det_trabajador_cve_ent_recep       ,
         lr_tmp_det_trabajador_nrp                 ,
         lr_tmp_det_trabajador_periodo_pago        ,
         lr_tmp_det_trabajador_folio_sua           ,
         lr_pag_det_trabajador_fecha_pago          ,
         ld_ide_derechohabiente                    ,
         lr_tmp_det_trabajador_localiza_trabajador ,
         lr_tmp_det_trabajador_tpo_aclaracion      ,
         lr_pag_det_trabajador_imp_ap_pat          ,
         lr_pag_det_trabajador_imp_am_cre          ,
         lr_pag_det_trabajador_imp_ren_viv_pgo_ext ,
         lr_pag_det_trabajador_aiv_ap_pat          ,
         lr_pag_det_trabajador_valor_aiv           ,
         lr_pag_det_trabajador_int_gen_pgo_ext     ,
         lr_pag_det_trabajador_aiv_gen_pgo_ext     ,
         0                                         ,
         lr_tmp_det_trabajador_ind_liquidacion     ,   --ind_liquidacion
         lr_tmp_det_trabajador_num_crd_ifv         ,
         v_d_fecha_hoy                             ,
         v_c_tpo_patron                            ,
         0                                         ,   --folio_referencia                          
         lr_pag_det_trabajador_fecha_valor         ,
         lr_tmp_det_trabajador_destino_ap_viv
         );
              
      INSERT INTO cta_pag_complemento
         (
         folio                ,
         origen_archivo       ,
         id_referencia        ,
         id_derechohabiente   ,
         rfc_patron           ,
         rfc                  ,
         curp                 ,
         num_mov_periodo      ,
         f_ini_desc_crd_ifv   ,
         ult_sdi              ,
         tpo_trabajador       ,
         jornada              ,
         destino_ap_viv       ,
         dias_cot_bim         ,
         dias_incap_bim       ,
         dias_ausent_bim      ,
         marca_sua            ,
         marca_bdnsar         
         )
      VALUES
         (
         ld_folio                                       ,
         1                                              ,
--g-        seq_cta_his_pagos.CURRVAL                      ,
         v_id_referencia,
         ld_ide_derechohabiente                         ,
         lr_tmp_det_trabajador_rfc_patron               ,
         lr_tmp_det_trabajador_rfc                      ,
         lr_tmp_det_trabajador_curp                     ,
         lr_tmp_det_trabajador_num_mov_periodo          ,
         lr_tmp_det_trabajador_f_ini_desc_crd_ifv       ,
         lr_pag_det_trabajador_ult_sdi                  ,
         lr_tmp_det_trabajador_tpo_trabajador           ,
         lr_tmp_det_trabajador_jornada                  ,
         lr_tmp_det_trabajador_destino_ap_viv           ,
         lr_tmp_det_trabajador_dias_cot_bim             ,
         lr_tmp_det_trabajador_dias_incap_bim           ,
         lr_tmp_det_trabajador_dias_ausent_bim          ,
         lr_tmp_det_trabajador_marca_sua                ,
         lr_tmp_det_trabajador_marca_bdnsa
         );

   END FOREACH ;
  
   CREATE INDEX nss_cuenta ON tmp_nss_cuenta(nss);

   UPDATE STATISTICS FOR TABLE tmp_nss_cuenta;


--===========================================================================
--=====   SEGUNDO FOREACH DE NSS NO ENCONTRADOS APERTURA DE CUENTA      =====
   FOREACH
      SELECT tmp.tpo_registro             ,
             tmp.id_servicio              ,
             tmp.nrp                      ,
             tmp.rfc_patron               ,
             tmp.periodo_pago             ,
             tmp.folio_sua                ,
             tmp.nss                      ,
             tmp.rfc                      ,
             tmp.curp                     ,
             tmp.num_crd_ifv              ,
             tmp.f_ini_desc_crd_ifv       ,
             tmp.num_mov_periodo          ,
             tmp.nom_trabajador           ,
             tmp.ult_sdi/100              ult_sdi,
             tmp.tpo_trabajador           ,
             tmp.jornada                  ,
             tmp.localiza_trabajajdor     ,
             tmp.destino_ap_viv           ,
             tmp.dias_cot_bim             ,
             tmp.dias_incap_bim           ,
             tmp.dias_ausent_bim          ,
             tmp.imp_tot_ap_pat/100     imp_tot_ap_pat  ,
             tmp.imp_ap_pat/100         imp_ap_pat  ,
             tmp.imp_tot_am_cre/100     imp_tot_am_cre  ,
             tmp.imp_am_cre/100         imp_am_cre  ,
             tmp.imp_ren_viv_pgo_ext/100  imp_ren_viv_pgo_ext,
             tmp.marca_sua                ,
             tmp.marca_bdnsa              ,
             tmp.diag_confronta           ,
             tmp.tpo_aclaracion           ,
             tmp.aiv_ap_pat/1000000       aiv_ap_pat,
             tmp.valor_aiv/1000000        valor_aiv,
             tmp.int_gen_pgo_ext/100      int_gen_pgo_ext,
             tmp.aiv_gen_pgo_ext/1000000  aiv_gen_pgo_ext,        
             tmp.cve_ent_recep,
             tmp.fecha_pago,
             tmp.fecha_valor
      INTO   lr_tmp_det_trabajador_tpo_registro        ,   --tpo_registro
             lr_tmp_det_trabajador_id_servicio         ,   --id_servicio
             lr_tmp_det_trabajador_nrp                 ,   --nrp
             lr_tmp_det_trabajador_rfc_patron          ,   --rfc_patron
             lr_tmp_det_trabajador_periodo_pago        ,   --periodo_pago
             lr_tmp_det_trabajador_folio_sua           ,   --folio_sua
             lr_tmp_det_trabajador_nss                 ,   --nss
             lr_tmp_det_trabajador_rfc                 ,   --rfc
             lr_tmp_det_trabajador_curp                ,   --curp
             lr_tmp_det_trabajador_num_crd_ifv         ,   --num_crd_ifv
             lr_tmp_det_trabajador_f_ini_desc_crd_ifv  ,   --f_ini_desc_crd_ifv
             lr_tmp_det_trabajador_num_mov_periodo     ,   --num_mov_periodo
             lr_tmp_det_trabajador_nom_trabajador      ,   --nom_trabajador
             lr_pag_det_trabajador_ult_sdi             ,   --ult_sdi
             lr_tmp_det_trabajador_tpo_trabajador      ,   --tpo_trabajador
             lr_tmp_det_trabajador_jornada             ,   --jornada
             lr_tmp_det_trabajador_localiza_trabajador ,   --localiza_trabajaj+
             lr_tmp_det_trabajador_destino_ap_viv      ,   --destino_ap_viv
             lr_tmp_det_trabajador_dias_cot_bim        ,   --dias_cot_bim
             lr_tmp_det_trabajador_dias_incap_bim      ,   --dias_incap_bim
             lr_tmp_det_trabajador_dias_ausent_bim     ,   --dias_ausent_bim
             lr_pag_det_trabajador_imp_tot_ap_pat      ,   --imp_tot_ap_pat
             lr_pag_det_trabajador_imp_ap_pat          ,   --imp_ap_pat
             lr_pag_det_trabajador_imp_tot_am_cre      ,   --imp_tot_am_cre
             lr_pag_det_trabajador_imp_am_cre          ,   --imp_am_cre
             lr_pag_det_trabajador_imp_ren_viv_pgo_ext ,   --imp_ren_viv_pgo_e+
             lr_tmp_det_trabajador_marca_sua           ,   --marca_sua
             lr_tmp_det_trabajador_marca_bdnsa         ,   --marca_bdnsa
             lr_tmp_det_trabajador_diag_confronta      ,   --diag_confronta
             lr_tmp_det_trabajador_tpo_aclaracion      ,   --tpo_aclaracion
             lr_pag_det_trabajador_aiv_ap_pat          ,   --aiv_ap_pat
             lr_pag_det_trabajador_valor_aiv           ,   --valor_aiv
             lr_pag_det_trabajador_int_gen_pgo_ext     ,   --int_gen_pgo_ext
             lr_pag_det_trabajador_aiv_gen_pgo_ext     ,  --aiv_gen_pgo_ext
             lr_pag_det_trabajador_cve_ent_recep       , 
             lr_pag_det_trabajador_fecha_pago          , 
             lr_pag_det_trabajador_fecha_valor
      FROM  tmp_det_trabajador tmp
      WHERE tmp.nss NOT IN (SELECT afi.nss FROM tmp_nss_cuenta afi)

      IF lr_tmp_det_trabajador_num_crd_ifv = "/" THEN
         LET lr_tmp_det_trabajador_num_crd_ifv = "0";
      END IF

      -- asignación para cuando trae valores en nulo 12-feb-2012
      IF lr_pag_det_trabajador_int_gen_pgo_ext IS NULL THEN
         LET lr_pag_det_trabajador_int_gen_pgo_ext = 0;
      END IF

      IF lr_pag_det_trabajador_aiv_gen_pgo_ext IS NULL THEN
         LET lr_pag_det_trabajador_aiv_gen_pgo_ext = 0;
      END IF

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
         LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(
                       lr_tmp_det_trabajador_nss
                      ,lr_tmp_det_trabajador_curp
                      ,lr_tmp_det_trabajador_rfc
                      ,1
                      ,lr_tmp_det_trabajador_nom_trabajador
                      ,v_tpo_trabajador     -- "I" IMSS / "S" Solo infonavit
                      ,0                    -- Credito. se da por omision
                      ,1                    --  LQINFO = pag_tpo_archivo
                      ,ld_folio             -- folio del lote
                      ,"R"                  -- origen afiliacion
                      );
      END IF

      -- Regla Negocio de Adelantamientos--
      IF lr_tmp_det_trabajador_localiza_trabajador = "3" THEN 
         
         IF v_c_tpo_patron = "99" THEN
         
            LET lr_tmp_det_trabajador_ind_liquidacion = 2;  --ACL adelantada liquidada SI
            
            IF (lr_tmp_det_trabajador_tpo_aclaracion=" " OR
                lr_tmp_det_trabajador_tpo_aclaracion IS NULL) THEN
                LET lr_tmp_det_trabajador_tpo_aclaracion = "26";
            END IF            
            
         ELSE
         
            IF (lr_tmp_det_trabajador_tpo_aclaracion = "13" OR
               lr_tmp_det_trabajador_tpo_aclaracion = "17") THEN
               
               LET lr_tmp_det_trabajador_ind_liquidacion = 3; --ACL adelantada liquidada IMSS
               
            ELSE   
               
               LET lr_tmp_det_trabajador_ind_liquidacion = 1; --ACL normal SIN liquidar
                     
            END IF
         END IF
      ELSE
         LET lr_tmp_det_trabajador_ind_liquidacion = 0;     --Aporte Normla liquidado      
         
         IF (lr_tmp_det_trabajador_tpo_aclaracion=" " OR
             lr_tmp_det_trabajador_tpo_aclaracion IS NULL) THEN
             LET lr_tmp_det_trabajador_tpo_aclaracion = "26";
         END IF
         
      END IF

--====================================================================================================      
      -- cambio por j741 portabilidad  -- nvo port                                             ---g--  --xvi-58
      IF lr_tmp_det_trabajador_destino_ap_viv = "3" THEN     -- Portabilidad                   ---g--  --xvi-58
         IF lr_tmp_det_trabajador_ind_liquidacion	<> 1 THEN  -- ACL normal sin liquidar        ---g--  --xvi-58
            LET lr_pag_det_trabajador_aiv_ap_pat      = lr_pag_det_trabajador_imp_ap_pat;      ---g--  --xvi-58 
            LET lr_pag_det_trabajador_aiv_gen_pgo_ext = lr_pag_det_trabajador_int_gen_pgo_ext; ---g--  --xvi-58
         END IF
      END IF
    
      ---g--
      -- Se comenta siguiente "IF" por correo enviado por Hamir 
      -- con fehca 18-feb-2016, hora: 10:24 y asunto: incidente 926244
      -- Se aplica regla en el "IF" de arriba de este comentario,
      -- donde se colocan pesos a las aivs cuando del detino = 3 y
      -- ademas el ind_liquidacion sea <> 1, o sea aplica para
      -- adelantos y pago normal. Referencia ---g--
      -- ---g- cambio por j741 portabilidad     -- nvo port
      -- ---g- IF lr_tmp_det_trabajador_destino_ap_viv = "3" THEN            --- Portabilidad    
      -- ---g-    IF lr_tmp_det_trabajador_localiza_trabajador	<> "3" THEN   --- Aclaracion
      -- ---g-       LET lr_pag_det_trabajador_aiv_ap_pat      = lr_pag_det_trabajador_imp_ap_pat;
      -- ---g-       LET lr_pag_det_trabajador_aiv_gen_pgo_ext = lr_pag_det_trabajador_int_gen_pgo_ext;
      -- ---g-    END IF
      -- ---g- END IF       

--====================================================================================================
      
      --g-
      LET v_id_referencia = v_id_referencia + 1;
                   
      INSERT INTO cta_his_pagos 
         (
         folio     ,  
         origen_archivo             ,  
         id_referencia              ,  
         cve_ent_receptora          ,  
         nrp                        ,  
         periodo_pago               ,  
         folio_sua                  ,  
         f_pago                     ,  
         id_derechohabiente         ,  
         localiza_trabajador        ,  
         tpo_aclaracion             ,  
         imp_ap_pat                 ,  
         imp_am_cre                 ,  
         imp_ren_viv_pgo_ext        ,  
         aiv_ap_pat                 ,  
         valor_aiv                  ,  
         int_gen_pgo_ext            ,  
         aiv_gen_pgo_ext            ,  
         result_operacion           ,  
         ind_liquidacion            ,  
         num_crd_ifv                ,  
         f_proceso                  ,  
         tpo_patron                 ,     
         folio_referencia           ,
         f_valor                    ,
         destino_ap_viv             
         )                
      VALUES 
         (
         ld_folio                                  ,                           
         1                                         ,  -- origen_archivo=1      
--g-         seq_cta_his_pagos.NEXTVAL                 ,                           
         v_id_referencia,
         lr_pag_det_trabajador_cve_ent_recep       , 
         lr_tmp_det_trabajador_nrp                 ,
         lr_tmp_det_trabajador_periodo_pago        ,
         lr_tmp_det_trabajador_folio_sua           ,
         lr_pag_det_trabajador_fecha_pago          , 
         ld_ide_derechohabiente                    ,
         lr_tmp_det_trabajador_localiza_trabajador ,
         lr_tmp_det_trabajador_tpo_aclaracion      ,
         lr_pag_det_trabajador_imp_ap_pat          ,
         lr_pag_det_trabajador_imp_am_cre          ,
         lr_pag_det_trabajador_imp_ren_viv_pgo_ext ,
         lr_pag_det_trabajador_aiv_ap_pat          ,
         lr_pag_det_trabajador_valor_aiv           ,
         lr_pag_det_trabajador_int_gen_pgo_ext     ,
         lr_pag_det_trabajador_aiv_gen_pgo_ext     ,
         0                                         ,
         lr_tmp_det_trabajador_ind_liquidacion     ,   --ind_liquidacion
         lr_tmp_det_trabajador_num_crd_ifv         ,
         v_d_fecha_hoy                             ,
         v_c_tpo_patron                            ,
         0                                         ,   --folio_referencia
         lr_pag_det_trabajador_fecha_valor         ,
         lr_tmp_det_trabajador_destino_ap_viv
         );

      INSERT INTO cta_pag_complemento
         (
         folio                ,
         origen_archivo       ,
         id_referencia        ,
         id_derechohabiente   ,
         rfc_patron           ,
         rfc                  ,
         curp                 ,
         num_mov_periodo      ,
         f_ini_desc_crd_ifv   ,
         ult_sdi              ,
         tpo_trabajador       ,
         jornada              ,
         destino_ap_viv       ,
         dias_cot_bim         ,
         dias_incap_bim       ,
         dias_ausent_bim      ,
         marca_sua            ,
         marca_bdnsar         
         )
      VALUES
         (
         ld_folio                                       ,
         1                                              ,
--g-         seq_cta_his_pagos.CURRVAL                      ,
         v_id_referencia,
         ld_ide_derechohabiente                         ,
         lr_tmp_det_trabajador_rfc_patron               ,
         lr_tmp_det_trabajador_rfc                      ,
         lr_tmp_det_trabajador_curp                     ,
         lr_tmp_det_trabajador_num_mov_periodo          ,
         lr_tmp_det_trabajador_f_ini_desc_crd_ifv       ,
         lr_pag_det_trabajador_ult_sdi                  ,
         lr_tmp_det_trabajador_tpo_trabajador           ,
         lr_tmp_det_trabajador_jornada                  ,
         lr_tmp_det_trabajador_destino_ap_viv           ,
         lr_tmp_det_trabajador_dias_cot_bim             ,
         lr_tmp_det_trabajador_dias_incap_bim           ,
         lr_tmp_det_trabajador_dias_ausent_bim          ,
         lr_tmp_det_trabajador_marca_sua                ,
         lr_tmp_det_trabajador_marca_bdnsa
         );

   END FOREACH ;
----===========================================================   
   
   -- se verifica si se deshabilitaon los indices para rehabilitarlos
   IF ( v_indices_deshabilitados = 1 ) THEN   
      -- se desactiva la bandera
      LET v_indices_deshabilitados = 0;
    
      -- se reactivan los indices
      SET INDEXES xie2cta_his_pagos ENABLED;
      SET INDEXES xie4cta_his_pagos ENABLED;
   END IF

   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;

END PROCEDURE;


