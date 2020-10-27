






CREATE FUNCTION "safreviv".fn_dis_valida_pago(p_nss           CHAR(11),
                                   p_folio_sua     DECIMAL(6,0),
                                   p_periodo_pago  CHAR(06),
                                   p_fecha_pago    DATE,
                                   p_nrp           CHAR(11))

RETURNING SMALLINT, SMALLINT, CHAR(80), 
          SMALLINT, SMALLINT, SMALLINT, SMALLINT, SMALLINT, SMALLINT, DECIMAL(9,0);

--Última modificación 11082015
--Declaración de variables
DEFINE v_id_derechohabiente   DECIMAL(9,0);

DEFINE v_bnd_proceso          SMALLINT;       --Estatus del proceso
DEFINE v_status               SMALLINT;
DEFINE sql_err                INTEGER ;
DEFINE isam_err               INTEGER ;
DEFINE error_info             CHAR(80);
DEFINE v_char                 CHAR(40);


DEFINE v_id_dh_aux            DECIMAL(9,0);
DEFINE v_periodo_pago         CHAR(6);
DEFINE v_f_pago            	  DATE;
DEFINE v_folio_sua	     	  DECIMAL(6,0);
DEFINE v_nrp	              CHAR(11);
DEFINE v_folio_cta_hp         DECIMAL(9,0); 
DEFINE v_id_referencia_cta_hp DECIMAL(9,0);
DEFINE v_ind_liquidacion      SMALLINT;

DEFINE v_num_pagos            SMALLINT;
DEFINE v_bnd_nss_ok           SMALLINT;
DEFINE v_bnd_folio_sua_ok     SMALLINT;
DEFINE v_bnd_periodo_pago_ok  SMALLINT;
DEFINE v_bnd_fecha_pago_ok    SMALLINT;
DEFINE v_bnd_nrp_ok           SMALLINT;
DEFINE v_bnd_pago_ok          SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info, 0, 0, 0, 0, 0, 0, 999999999;
END EXCEPTION

 --SET DEBUG FILE TO '/safreviv_int/dis/ERR_cred_cero.TRACE';  
 --TRACE ON;

	--#Inicialización de variables
   LET v_id_derechohabiente      = 0;

   LET v_bnd_proceso             = 0; --Estado correcto
   LET v_status                  = 0;
   
   LET sql_err                   = 0;
   LET isam_err                  = 0;
   LET error_info                = "";

   LET v_id_dh_aux               = 0;
   LET v_periodo_pago       	   = "";
   LET v_f_pago            	   = "";
   LET v_folio_sua	     		   = 0;
   LET v_nrp                     = "";
   LET v_ind_liquidacion         = 0;

   LET v_num_pagos               = 0;
   LET v_bnd_nss_ok              = 0;
   LET v_bnd_folio_sua_ok        = 0;
   LET v_bnd_periodo_pago_ok     = 0;
   LET v_bnd_fecha_pago_ok       = 0;
   LET v_bnd_nrp_ok              = 0;
   LET v_bnd_pago_ok             = 1;
   LET v_folio_cta_hp            = 0; 
   LET v_id_referencia_cta_hp    = 0;
   
   --#Obtenemos id_derechohabiente
   SELECT afi.id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente afi
   WHERE  afi.nss = p_nss;
   

   IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
      LET v_id_derechohabiente = "999999999";
   END IF 

   --------------------------------------------------------------------- NSS
   LET v_num_pagos   = 0;
   SELECT COUNT(*)
   INTO   v_num_pagos
   FROM   tmp_dis_pagos_0 pag
   WHERE  pag.folio_sua                      = p_folio_sua
   AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
   AND    pag.f_pago                         = p_fecha_pago
   AND    pag.nrp                            = p_nrp;
   --AND    pag.ind_liquidacion           NOT IN (1,6);

   IF v_num_pagos >= 1 THEN  --Existe al menos un pago

      FOREACH
         SELECT pag.id_derechohabiente, 
                pag.folio_sua,
                fn_bimestre_pago(pag.periodo_pago),
                pag.f_pago,
                pag.nrp 
           INTO v_id_dh_aux,  
                v_folio_sua,	         
                v_periodo_pago,       	   
                v_f_pago,            	  
                v_nrp
         FROM   tmp_dis_pagos_0 pag
         WHERE  pag.id_derechohabiente             = v_id_derechohabiente
         AND    pag.folio_sua                      = p_folio_sua
         AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
         AND    pag.f_pago                         = p_fecha_pago
         AND    pag.nrp                            = p_nrp
         AND    pag.ind_liquidacion           NOT IN (1,6)


         IF v_bnd_nss_ok = 0 THEN   -- Aun no se ha verificado si existe pago con NSS (id_drechohabiente)
            IF v_id_dh_aux = v_id_derechohabiente THEN
               LET v_bnd_nss_ok = 1;
            END IF
         END IF

      END FOREACH;
      
   ELSE --No hay pagos que verificar
      LET v_bnd_nss_ok  = 1;
      LET v_bnd_pago_ok = 0;
   END IF

   --------------------------------------------------------------------- folio_sua
   LET v_num_pagos   = 0;
   SELECT COUNT(*)
   INTO   v_num_pagos
   FROM   tmp_dis_pagos_0 pag
   WHERE  fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
   AND    pag.f_pago                         = p_fecha_pago
   AND    pag.nrp                            = p_nrp;
   --AND    pag.ind_liquidacion           NOT IN (1,6);

   IF v_num_pagos >= 1 THEN  --Existe al menos un pago

      FOREACH
         SELECT pag.id_derechohabiente, 
                pag.folio_sua,
                fn_bimestre_pago(pag.periodo_pago),
                pag.f_pago,
                pag.nrp 
           INTO v_id_dh_aux,  
                v_folio_sua,	         
                v_periodo_pago,       	   
                v_f_pago,            	  
                v_nrp
         FROM   tmp_dis_pagos_0 pag
         WHERE  fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
         AND    pag.f_pago                         = p_fecha_pago
         AND    pag.nrp                            = p_nrp
         --AND    pag.ind_liquidacion           NOT IN (1,6)


         IF v_bnd_folio_sua_ok = 0 THEN   -- Aun no se ha verificado si existe pago con folio_sua
            IF v_folio_sua = p_folio_sua THEN
               LET v_bnd_folio_sua_ok = 1;
            END IF
         END IF

      END FOREACH;
      
   ELSE --No hay pagos que verificar
      LET v_bnd_folio_sua_ok  = 1;
      LET v_bnd_pago_ok = 0;
   END IF


   --------------------------------------------------------------------- periodo_pago
   LET v_num_pagos   = 0;
   SELECT COUNT(*)
   INTO   v_num_pagos
   FROM   tmp_dis_pagos_0 pag
   WHERE  pag.folio_sua                      = p_folio_sua
   AND    pag.f_pago                         = p_fecha_pago
   AND    pag.nrp                            = p_nrp;
   --AND    pag.ind_liquidacion           NOT IN (1,6);

   IF v_num_pagos >= 1 THEN  --Existe al menos un pago

      FOREACH
         SELECT pag.id_derechohabiente, 
                pag.folio_sua,
                fn_bimestre_pago(pag.periodo_pago),
                pag.f_pago,
                pag.nrp 
           INTO v_id_dh_aux,  
                v_folio_sua,	         
                v_periodo_pago,       	   
                v_f_pago,            	  
                v_nrp
         FROM   tmp_dis_pagos_0 pag
         WHERE  pag.folio_sua                      = p_folio_sua
         AND    pag.f_pago                         = p_fecha_pago
         AND    pag.nrp                            = p_nrp
         --AND    pag.ind_liquidacion           NOT IN (1,6)


         IF v_bnd_periodo_pago_ok = 0 THEN   -- Aun no se ha verificado si existe pago con periodo_pago
            IF v_periodo_pago = p_periodo_pago THEN
               LET v_bnd_periodo_pago_ok = 1;
            END IF
         END IF

      END FOREACH;
      
   ELSE --No hay pagos que verificar
      LET v_bnd_periodo_pago_ok  = 1;
      LET v_bnd_pago_ok = 0;
   END IF

   --------------------------------------------------------------------- fecha_pago
   LET v_num_pagos   = 0;
   SELECT COUNT(*)
   INTO   v_num_pagos
   FROM   tmp_dis_pagos_0 pag
   WHERE  pag.folio_sua                      = p_folio_sua
   AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
   AND    pag.nrp                            = p_nrp;
   --AND    pag.ind_liquidacion           NOT IN (1,6);

   IF v_num_pagos >= 1 THEN  --Existe al menos un pago

      FOREACH
         SELECT pag.id_derechohabiente, 
                pag.folio_sua,
                fn_bimestre_pago(pag.periodo_pago),
                pag.f_pago,
                pag.nrp 
           INTO v_id_dh_aux,  
                v_folio_sua,	         
                v_periodo_pago,       	   
                v_f_pago,            	  
                v_nrp
         FROM   tmp_dis_pagos_0 pag
         WHERE  pag.folio_sua                      = p_folio_sua
         AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
         AND    pag.nrp                            = p_nrp
         --AND    pag.ind_liquidacion           NOT IN (1,6)


         IF v_bnd_fecha_pago_ok = 0 THEN   -- Aun no se ha verificado si existe pago con fecha_pago
            IF v_f_pago = p_fecha_pago THEN
               LET v_bnd_fecha_pago_ok = 1;
            END IF
         END IF

      END FOREACH;
      
   ELSE --No hay pagos que verificar
      LET v_bnd_fecha_pago_ok  = 1;
      LET v_bnd_pago_ok = 0;
   END IF

   --------------------------------------------------------------------- nrp
   LET v_num_pagos   = 0;
   SELECT COUNT(*)
   INTO   v_num_pagos
   FROM   tmp_dis_pagos_0 pag
   WHERE  pag.folio_sua                      = p_folio_sua
   AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
   AND    pag.f_pago                         = p_fecha_pago;
   --AND    pag.ind_liquidacion           NOT IN (1,6);

   IF v_num_pagos >= 1 THEN  --Existe al menos un pago

      FOREACH
         SELECT pag.id_derechohabiente, 
                pag.folio_sua,
                fn_bimestre_pago(pag.periodo_pago),
                pag.f_pago,
                pag.nrp 
           INTO v_id_dh_aux,  
                v_folio_sua,	         
                v_periodo_pago,       	   
                v_f_pago,            	  
                v_nrp
         FROM   tmp_dis_pagos_0 pag
         WHERE  pag.folio_sua                      = p_folio_sua
         AND    fn_bimestre_pago(pag.periodo_pago) = p_periodo_pago
         AND    pag.f_pago                         = p_fecha_pago
         --AND    pag.ind_liquidacion           NOT IN (1,6)


         IF v_bnd_nrp_ok = 0 THEN   -- Aun no se ha verificado si existe pago con nrp
            IF v_nrp = p_nrp THEN
               LET v_bnd_nrp_ok = 1;
            END IF
         END IF

      END FOREACH;
      
   ELSE --No hay pagos que verificar
      LET v_bnd_nrp_ok  = 1;
      LET v_bnd_pago_ok = 0;
   END IF

   IF (v_bnd_nss_ok = 1) AND
      (v_bnd_folio_sua_ok = 1) AND
      (v_bnd_periodo_pago_ok = 1) AND
      (v_bnd_fecha_pago_ok = 1) AND
      (v_bnd_fecha_pago_ok = 1) AND
      (v_bnd_pago_ok = 1)THEN
      
      LET v_bnd_pago_ok = 1;
   ELSE
      LET v_bnd_pago_ok = 0;
   END IF
      

   
 --TRACE 'Finaliza sp_dis_pagos_avances con valor '||v_bnd_proceso;
   LET v_char = "Terminado validacion pago";
   
   RETURN v_bnd_proceso , 0 , v_char, 
          v_bnd_nss_ok, 
          v_bnd_folio_sua_ok, 
          v_bnd_periodo_pago_ok, 
          v_bnd_fecha_pago_ok, 
          v_bnd_nrp_ok,
          v_bnd_pago_ok,
          v_id_derechohabiente ;

END FUNCTION;


