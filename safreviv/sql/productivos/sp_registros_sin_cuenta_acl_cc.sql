






CREATE PROCEDURE "safreviv".sp_registros_sin_cuenta_acl_cc(ld_folio          DECIMAL(9,0), 
                                             lp_origen_archivo SMALLINT      )

RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR (11)

   DEFINE ld_ide_derechohabiente                     DECIMAL(9)   ;
   DEFINE lr_tmp_det_trabajador_nrp                  CHAR(11)     ;
   DEFINE lr_tmp_det_trabajador_nss                  CHAR(11)     ;
   DEFINE lr_tmp_det_cc_nss_dispersion               CHAR(11)     ;
   DEFINE lr_tmp_det_trabajador_rfc                  CHAR(13)     ;
   DEFINE lr_tmp_det_trabajador_curp                 CHAR(18)     ;
   DEFINE lr_tmp_det_trabajador_nom_trabajador       CHAR(50)     ;

   DEFINE lr_tmp_det_trabajador_localiza_trabajador  CHAR(1)      ;

   DEFINE li_contador             INTEGER;
   DEFINE v_error                 SMALLINT;

   DEFINE v_c_tpo_patron                  CHAR(2)     ;
   
     -- Control de Excepciones
   DEFINE isam_err                        INTEGER       ;
   DEFINE err_txt                         VARCHAR(255)  ;
   DEFINE v_si_resultado                  SMALLINT      ;

   DEFINE v_tpo_trabajador CHAR(01);

   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      RETURN v_error, isam_err, err_txt, lr_tmp_det_trabajador_nss ;
   END EXCEPTION 

   LET li_contador = 1;   
   LET lr_tmp_det_trabajador_nss = NULL;
   
   SET PDQPRIORITY HIGH;

   --Inicializacion de variables     
   LET v_c_tpo_patron = NULL; 
   
--==================================================================
--=====  FOREACH DE NSS NO ENCONTRADOS APERTURA DE CUENTA      =====

  --Inicializacion de variables      
  LET ld_ide_derechohabiente = NULL;   
     
  IF lp_origen_archivo = 6 THEN  -- con cambio de NSS
  	
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
     FROM  safre_tmp:tmp_det_cc_nss tmp
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
                                                            lp_origen_archivo, -- 6 = ACLARATORIO CON CAMBIO NSS  -- 7 ACLARATORIO CON CAMBIO NOMBRE
                                                            ld_folio, -- folio del lote
                                                            "R" -- origen afiliacion
                                                            ); 
      END IF
     END FOREACH ;
    
     -- SOLO PARA CAMBIO DE NSS se valida que exista el nss_dispersion en afi_derechohabiente si no, se crea la cuenta
           
     FOREACH SELECT
           tmp.nss_dispersion      ,
           tmp.nrp                 ,
           tmp.rfc                 ,
           tmp.curp                ,
           tmp.nombre_trabajador   ,
           tmp.localiza_trabajador 
        INTO 
           lr_tmp_det_cc_nss_dispersion              ,   --nss
           lr_tmp_det_trabajador_nrp                 ,   --nrp
           lr_tmp_det_trabajador_rfc                 ,   --rfc
           lr_tmp_det_trabajador_curp                ,   --curp
           lr_tmp_det_trabajador_nom_trabajador      ,   --nom_trabajador
           lr_tmp_det_trabajador_localiza_trabajador     --localiza_trabajador
     FROM  safre_tmp:tmp_det_cc_nss tmp
     WHERE tmp.nss_dispersion NOT IN (SELECT afi.nss FROM afi_derechohabiente afi)

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
      WHERE  a.nss = lr_tmp_det_cc_nss_dispersion;
      
      IF ld_ide_derechohabiente IS NULL THEN
        LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(lr_tmp_det_cc_nss_dispersion ,
                                                            lr_tmp_det_trabajador_curp,
                                                            lr_tmp_det_trabajador_rfc ,
                                                            1,
                                                            lr_tmp_det_trabajador_nom_trabajador,
                                                            v_tpo_trabajador,
                                                            0, -- Credito. se da por omision
                                                            lp_origen_archivo, -- 6 = ACLARATORIO CON CAMBIO NSS  -- 7 ACLARATORIO CON CAMBIO NOMBRE
                                                            ld_folio, -- folio del lote
                                                            "R" -- origen afiliacion
                                                            ); 
      END IF
     END FOREACH ;     
  
  ELSE -- lp_origen_archivo = 7 -- con cambio de Nombre   
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
     FROM  safre_tmp:tmp_det_cc_nom tmp
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
                                                            lp_origen_archivo, -- 6 = ACLARATORIO CON CAMBIO NSS  -- 7 ACLARATORIO CON CAMBIO NOMBRE
                                                            ld_folio, -- folio del lote
                                                            "R" -- origen afiliacion
                                                            ); 
      END IF
     END FOREACH ;  	 
  END IF
----===========================================================   
   


   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, lr_tmp_det_trabajador_nss;

END PROCEDURE;


