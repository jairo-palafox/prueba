






CREATE PROCEDURE "safreviv".sp_registros_sin_cuenta_acl_enaclara(ld_folio DECIMAL(9,0))

RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR (11)

   DEFINE lr_tmp_det_trabajador_nss                  CHAR(11)     ;
   DEFINE lr_tmp_det_trabajador_rfc                  CHAR(13)     ;
   DEFINE lr_tmp_det_trabajador_curp                 CHAR(18)     ;
   DEFINE lr_tmp_det_trabajador_nom_trabajador       CHAR(50)     ;
   DEFINE lr_tmp_det_trabajador_localiza_trabajador  CHAR(1)      ;
   DEFINE ld_ide_derechohabiente                     DECIMAL(9)   ;
   DEFINE v_tipo_trabajador                          CHAR(1)      ;

     -- Control de Excepciones
   DEFINE v_error                                    SMALLINT     ;
   DEFINE isam_err                                   INTEGER      ;
   DEFINE err_txt                                    VARCHAR(255) ;
   DEFINE v_si_resultado                             SMALLINT     ;
                                                     
   DEFINE v_indices_deshabilitados                   SMALLINT     ; -- 0=No se deshabilitaron. 1=Se deshabilitaron
   DEFINE v_conteo                                   DECIMAL(9,0) ;

   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      RETURN v_error, isam_err, err_txt, lr_tmp_det_trabajador_nss ;
   END EXCEPTION 

   LET lr_tmp_det_trabajador_nss = NULL;
   
   -- se asume que no se deshabilitaran los indices
   LET v_indices_deshabilitados = 0;
   LET v_conteo = 0;

   SET PDQPRIORITY HIGH;

   -- se cuentan los registros
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   tmp_det_aclaracion;

   -- si el conteo es igual o mayor a 1 millon de registros, se deshabilitan los indices
   IF ( v_conteo > 999999 ) THEN 
      
      -- se activa la bandera de indices deshabilitados
      LET v_indices_deshabilitados = 1;

      -- se deshabilitan los indices
      SET INDEXES xie2cta_his_pagos DISABLED;
      SET INDEXES xie4cta_his_pagos DISABLED;
   END IF


--==================================================================
--=====  FOREACH DE NSS NO ENCONTRADOS APERTURA DE CUENTA      =====

      --Inicializacion de variables     
      LET ld_ide_derechohabiente = NULL;
      
   FOREACH SELECT
           tmp.nss                  ,
           tmp.rfc                  ,
           tmp.curp                 ,
           tmp.nom_trabajador       
        INTO 
           lr_tmp_det_trabajador_nss                 ,   --nss
           lr_tmp_det_trabajador_rfc                 ,   --rfc
           lr_tmp_det_trabajador_curp                ,   --curp
           lr_tmp_det_trabajador_nom_trabajador          --nom_trabajador
     FROM  tmp_det_aclaracion tmp
     WHERE tmp.nss NOT IN (SELECT afi.nss FROM afi_derechohabiente afi)

      LET v_tipo_trabajador = "I";
      
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
                                                            v_tipo_trabajador,
                                                            0, -- Credito. se da por omision
                                                            8,--8=ACLARATORIO ENACLARA, 
                                                            ld_folio, -- folio del lote
                                                            "R" -- origen afiliacion
                                                            );  
      END IF
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


