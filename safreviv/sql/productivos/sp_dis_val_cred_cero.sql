






CREATE PROCEDURE "safreviv".sp_dis_val_cred_cero(p_folio    DECIMAL(9,0),
                                      p_edo_rech SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 03032016
--Declaración de variables
DEFINE v_nss 		        CHAR(11);
DEFINE v_num_credito	    	DECIMAL(10,0);
DEFINE v_periodo_pago	   	CHAR(6);
DEFINE v_f_pago	         	DATE;
--DEFINE v_ent_recaudadora		CHAR(3);
DEFINE v_nrp	            	CHAR(11);
DEFINE v_aportacion	      	DECIMAL(12,2);
DEFINE v_amortizacion	   	DECIMAL(12,2);
DEFINE v_folio_sua	     	DECIMAL(6,0);

DEFINE v_id_derechohabiente     DECIMAL(9,0);

DEFINE v_estado 	        SMALLINT;
DEFINE v_inconsis               CHAR(30);
DEFINE v_desc_resultado         CHAR(40);

DEFINE v_folio_liquida 	        DECIMAL(9,0);

DEFINE v_bnd_transaccion        SMALLINT;
DEFINE v_bnd_proceso            SMALLINT;       --Estatus del proceso
DEFINE v_status                 SMALLINT;
DEFINE sql_err                  INTEGER ;
DEFINE isam_err                 INTEGER ;
DEFINE error_info               CHAR(70);
DEFINE v_char                   CHAR(20);
DEFINE v_cnt_disp               SMALLINT;
DEFINE v_cnt_no_disp            SMALLINT;

DEFINE v_id_dis_arh_num_cred    DECIMAL(9,0);

DEFINE v_folio_cta_hp           DECIMAL(9,0); 
DEFINE v_id_referencia_cta_hp   DECIMAL(9,0);
DEFINE v_folio_glo              DECIMAL(9,0);
DEFINE v_bnd_existe_inconsis    SMALLINT;
DEFINE v_bnd_existe_inconsis_0  SMALLINT;
DEFINE v_bnd_existe_inconsis_10 SMALLINT;

DEFINE v_bnd_existe             SMALLINT;
DEFINE v_num_pagos              SMALLINT;
DEFINE v_bnd_existe_folio_disp  SMALLINT;
DEFINE v_bnd_mov_div_disp       DECIMAL(6,0);
DEFINE v_bnd_reg_duplicado      INTEGER;
DEFINE v_bnd_existe_nrp         SMALLINT;
DEFINE v_bnd_existe_info_inc    SMALLINT;

DEFINE v_status_pag             SMALLINT;
DEFINE isam_err_pag             INTEGER ;
DEFINE error_info_pag           CHAR(70);
DEFINE v_bnd_nss_ok             SMALLINT;
DEFINE v_bnd_folio_sua_ok       SMALLINT;
DEFINE v_bnd_periodo_pago_ok    SMALLINT;
DEFINE v_bnd_f_pago_ok          SMALLINT;
DEFINE v_bnd_nrp_ok             SMALLINT;
DEFINE v_bnd_pago_ok            SMALLINT;

DEFINE v_tmp_id			DECIMAL(9,0); 
DEFINE v_tmp_f_liquida          DATE; 
DEFINE v_tmp_mov                SMALLINT;

DEFINE v_num_pagos_arch_1       SMALLINT;
DEFINE v_num_pagos_arch_5       SMALLINT;
DEFINE v_folio_pag_arch_5       DECIMAL(9,0);
DEFINE v_ins_info_liq_arh       SMALLINT;

DEFINE v_f_actualiza			DATE;


ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

 --SET DEBUG FILE TO '/safreviv_int/dis/ERR_cred_cero.TRACE';
  --TRACE ON;

--#Inicialización de variables
   LET v_nss 		         = "";
   LET v_num_credito             = "";
   LET v_periodo_pago            = "";
   LET v_f_pago                  = "";
   LET v_nrp                     = "";
   LET v_aportacion              = 0;
   LET v_amortizacion            = 0;
   LET v_folio_sua               = "";

   LET v_status_pag              = 0;
   LET isam_err_pag              = 0;
   LET error_info_pag            = "";
   LET v_bnd_nss_ok              = 0;
   LET v_bnd_folio_sua_ok        = 0;
   LET v_bnd_periodo_pago_ok     = 0;
   LET v_bnd_f_pago_ok           = 0;
   LET v_bnd_nrp_ok              = 0;
   LET v_bnd_pago_ok             = 0;

   LET v_id_derechohabiente      = 0;
   LET v_estado                  = 0;
   LET v_desc_resultado          = "";

   LET v_bnd_proceso             = 0; --Estado correcto
   LET v_folio_liquida           = 0;
   LET v_status                  = 0;
   
   LET sql_err                   = 0;
   LET isam_err                  = 0;
   LET error_info                = "";

   LET v_cnt_disp                = 0;
   LET v_cnt_no_disp             = 0;
   LET v_id_dis_arh_num_cred     = 0;

   LET v_folio_cta_hp            = "";
   LET v_id_referencia_cta_hp    = "";
   LET v_folio_glo               = ""; 
   LET v_bnd_existe_inconsis     = 0;
   LET v_bnd_existe_inconsis_0   = 0;
   LET v_bnd_existe_inconsis_10  = 0;
   LET v_bnd_existe              = 0;
   LET v_num_pagos               = 0;
   LET v_bnd_existe_folio_disp   = 0;
   LET v_bnd_mov_div_disp        = 0;
   LET v_bnd_reg_duplicado       = 0;
   LET v_bnd_existe_nrp          = 0;
   LET v_bnd_existe_info_inc     = 0;
   
   LET v_tmp_id                  = 0;
   LET v_tmp_f_liquida           = "";  
   LET v_tmp_mov                 = 0;

   LET v_num_pagos_arch_1        = 0;
   LET v_num_pagos_arch_5        = 0;
   LET v_folio_pag_arch_5        = 0;
   LET v_ins_info_liq_arh        = 0;
   
   LET v_f_actualiza			 = "";

   FOREACH
      SELECT TRIM(nss),
             TRIM(num_credito),
             TRIM(periodo_pago),
             f_pago,
             TRIM(nrp),
             (aportacion/100),
             (amortizacion/100),
             TRIM(folio_sua)
      INTO   v_nss,
             v_num_credito,
             v_periodo_pago,
             v_f_pago,
             v_nrp,
             v_aportacion,
             v_amortizacion,
             v_folio_sua
      FROM   safre_tmp:tmp_dis_cred_cero1 cred_cero

      LET v_estado = 0;
      LET v_id_derechohabiente = 0;
      LET v_tmp_id  = 0;

      LET v_inconsis = "";

      --Validacion de Pago
      EXECUTE FUNCTION fn_dis_valida_pago(v_nss, v_folio_sua, v_periodo_pago, v_f_pago, v_nrp) 
                  INTO v_status_pag,
                       isam_err_pag,
                       error_info_pag,
                       v_bnd_nss_ok,
                       v_bnd_folio_sua_ok,
                       v_bnd_periodo_pago_ok,
                       v_bnd_f_pago_ok,
                       v_bnd_nrp_ok,
                       v_bnd_pago_ok,
                       v_id_derechohabiente;       
                                              
      LET v_bnd_existe = 0;
      IF v_id_derechohabiente = "999999999" THEN
         LET v_bnd_existe = 0;
      ELSE
         LET v_bnd_existe = 1;
      END IF

      LET v_bnd_existe_info_inc     = 0;

      SELECT COUNT(*)
      INTO v_bnd_existe_info_inc
      FROM dis_info_inconsistente dii
      WHERE dii.id_derechohabiente = v_id_derechohabiente
      AND   dii.tpo_inconsistente  = 0;

      --Validación de Negocio NSS
      IF (LENGTH(v_nss) <> 11) OR (v_nss IS NULL)  OR (v_nss = "") THEN
         --LET v_inconsis         = 10; --nss nulo
         LET v_inconsis = "01";
         LET v_estado = 1;
         LET v_desc_resultado = 'NSS NULO';
      ELSE
         IF (v_bnd_nss_ok = 1) THEN
            LET v_inconsis = "00";
         ELSE
            LET v_inconsis = "01"; 
            LET v_estado = 1;  
         END IF
      END IF

      --Validación de Negocio NRP
      IF (v_nrp IS NULL)  OR (v_nrp = "") OR (LENGTH(v_nrp) <> 11) THEN   
         LET v_inconsis = TRIM(v_inconsis)||"02";
         LET v_estado = 1;
         LET v_desc_resultado = 'NRP NULO';
      ELSE
         IF (v_bnd_nrp_ok = 1) THEN
            LET v_inconsis = TRIM(v_inconsis)||"00";
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"02"; 
            LET v_estado = 1;  
         END IF
      END IF

      --Validación de Negocio Folio SUA
      IF (v_folio_sua IS NULL)  OR (v_folio_sua = "") THEN
         LET v_inconsis = TRIM(v_inconsis)||"03";
         LET v_estado = 1;
         LET v_desc_resultado = 'FOLIO SUA NULO';
      ELSE
         IF (v_bnd_folio_sua_ok = 1) THEN
            LET v_inconsis = TRIM(v_inconsis)||"00";
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"03";
            LET v_estado = 1;   
         END IF
      END IF


      --Validación de Negocio Periodo de Pago
      IF (v_periodo_pago IS NULL)  OR (v_periodo_pago = "") OR 
         (LENGTH(v_periodo_pago) <> 6) OR (v_periodo_pago = "000000") THEN
         
         LET v_inconsis = TRIM(v_inconsis)||"04";
         LET v_estado = 1;
         LET v_desc_resultado = 'PERIODO PAGO NULO';
      ELSE
         IF (v_bnd_periodo_pago_ok = 1) THEN
            LET v_inconsis = TRIM(v_inconsis)||"00";
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"04"; 
            LET v_estado = 1;
         END IF
      END IF

      --Validación de Negocio Fecha de Pago
      IF (v_f_pago IS NULL)  OR (v_f_pago = "") OR (v_f_pago = 0) THEN
         LET v_inconsis = TRIM(v_inconsis)||"05";
         LET v_estado = 1;
         LET v_desc_resultado = 'FECHA PAGO NULA';
      ELSE
         IF (v_bnd_f_pago_ok = 1) THEN
            LET v_inconsis = TRIM(v_inconsis)||"00";
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"05";  
            LET v_estado = 1;
         END IF
      END IF


      --#Asigna id_derechohabiente si no se encuentra en tabla
      IF v_id_derechohabiente IS NULL THEN
         LET v_id_derechohabiente = "999999999";
         --LET v_inconsis         = 30;
         LET v_inconsis = TRIM(v_inconsis)|| "06";
         LET v_estado = 1;
         LET v_desc_resultado = 'NSS NO EXISTE EN LA BD DE AFILIADOS';
      ELSE
         LET v_inconsis = TRIM(v_inconsis)||"00";
      END IF

      LET v_folio_cta_hp = "";
      LET v_id_referencia_cta_hp = "";

      IF v_bnd_existe >= 1 THEN
         LET v_num_pagos        = 0;
         LET v_ins_info_liq_arh = 0;

         SELECT COUNT(*)
         INTO   v_num_pagos
         FROM   tmp_dis_pagos_0 cta
         WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
         AND    cta.folio_sua                      = v_folio_sua
         AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
         AND    cta.f_pago                         = v_f_pago
         AND    cta.nrp                            = v_nrp
         AND    cta.ind_liquidacion           NOT IN (1,6);

         IF v_num_pagos >= 2 THEN  ---DOBLE PAGO
            LET v_inconsis = TRIM(v_inconsis)||"000000001100";
            LET v_ins_info_liq_arh = 1;
            LET v_estado           = 0;

            LET v_num_pagos_arch_1 = 0;
            LET v_num_pagos_arch_5 = 0;
            LET v_folio_pag_arch_5 = 0;

            SELECT COUNT(*)
            INTO   v_num_pagos_arch_1
            FROM   tmp_dis_pagos_0 cta
            WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
            AND    cta.folio_sua                      = v_folio_sua
            AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
            AND    cta.f_pago                         = v_f_pago
            AND    cta.nrp                            = v_nrp
            AND    cta.ind_liquidacion           NOT IN (1,6);
            
            IF v_num_pagos_arch_1 > 1 THEN
            
               SELECT MIN(cta.id_referencia)
               INTO   v_folio_pag_arch_5
               FROM   tmp_dis_pagos_0 cta
               WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
               AND    cta.folio_sua                      = v_folio_sua
               AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
               AND    cta.f_pago                         = v_f_pago
               AND    cta.nrp                            = v_nrp
               AND    cta.ind_liquidacion           NOT IN (1,6);

               DELETE 
               FROM   tmp_dis_pagos_0 cta
               WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
               AND    cta.folio_sua                      = v_folio_sua
               AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
               AND    cta.f_pago                         = v_f_pago
               AND    cta.nrp                            = v_nrp
               AND    cta.ind_liquidacion           NOT IN (1,6)
               AND    cta.id_referencia                 <> v_folio_pag_arch_5;
               
            END IF

                  {SELECT COUNT(*)
                  INTO   v_num_pagos_arch_1
                  FROM   tmp_dis_pagos_0 cta
                  WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                  AND    cta.folio_sua                      = v_folio_sua
                  AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                  AND    cta.f_pago                         = v_f_pago
                  AND    cta.nrp                            = v_nrp
                  AND    cta.ind_liquidacion           NOT IN (1,6)
                  AND    cta.origen_archivo                 = 1;
                  IF v_num_pagos_arch_1 >= 1 THEN ---Archivo LQ
                     SELECT COUNT(*)
                     INTO   v_num_pagos_arch_5
                     FROM   tmp_dis_pagos_0 cta
                     WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                     AND    cta.folio_sua                      = v_folio_sua
                     AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                     AND    cta.f_pago                         = v_f_pago
                     AND    cta.nrp                            = v_nrp
                     AND    cta.ind_liquidacion           NOT IN (1,6)
                     AND    cta.origen_archivo                 = 5;
                     IF v_num_pagos_arch_5 >= 1 THEN ---Archivo ACL
                        DELETE 
                        FROM   tmp_dis_pagos_0 cta
                        WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                        AND    cta.folio_sua                      = v_folio_sua
                        AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                        AND    cta.f_pago                         = v_f_pago
                        AND    cta.nrp                            = v_nrp
                        AND    cta.ind_liquidacion           NOT IN (1,6)
                        AND    cta.origen_archivo                 = 5;
                     END IF
                  ELSE
                     SELECT COUNT(*)
                     INTO   v_num_pagos_arch_5
                     FROM   tmp_dis_pagos_0 cta
                     WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                     AND    cta.folio_sua                      = v_folio_sua
                     AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                     AND    cta.f_pago                         = v_f_pago
                     AND    cta.nrp                            = v_nrp
                     AND    cta.ind_liquidacion           NOT IN (1,6)
                     AND    cta.origen_archivo                 = 5;
                     IF v_num_pagos_arch_5 >= 1 THEN ---Archivo ACL
                        SELECT MIN(cta.folio)
                        INTO   v_folio_pag_arch_5
                        FROM   tmp_dis_pagos_0 cta
                        WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                        AND    cta.folio_sua                      = v_folio_sua
                        AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                        AND    cta.f_pago                         = v_f_pago
                        AND    cta.nrp                            = v_nrp
                        AND    cta.ind_liquidacion           NOT IN (1,6)
                        AND    cta.origen_archivo                 = 5;

                        DELETE 
                        FROM   tmp_dis_pagos_0 cta
                        WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
                        AND    cta.folio_sua                      = v_folio_sua
                        AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
                        AND    cta.f_pago                         = v_f_pago
                        AND    cta.nrp                            = v_nrp
                        AND    cta.ind_liquidacion           NOT IN (1,6)
                        AND    cta.origen_archivo                 = 5
                        AND    cta.folio                         <> v_folio_pag_arch_5;
                     END IF
                  END IF}
               
            IF v_num_credito IS NULL OR  v_num_credito = 0 THEN
               LET v_inconsis = TRIM(v_inconsis)||"13";  --13
               LET v_estado = 1;
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";  --13
            END IF

            LET v_bnd_reg_duplicado = 0;
            
            SELECT COUNT(*)
            INTO v_bnd_reg_duplicado
            FROM dis_arh_num_cred_0
            WHERE folio                = p_folio
            AND   id_derechohabiente   = v_id_derechohabiente
            AND   periodo_pago         = v_periodo_pago
            AND   f_pago               = v_f_pago
            AND   nrp                  = v_nrp
            AND   folio_sua            = v_folio_sua;

            IF v_bnd_reg_duplicado >= 1 THEN
               LET v_inconsis = TRIM(v_inconsis)||"14";  --14  REGISTRO DUPLICADO
               LET v_estado = 1;
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";  --14
            END IF
                  
            LET v_inconsis = TRIM(v_inconsis)||"00";     --15

            LET v_id_dis_arh_num_cred = seq_dis_arh_num_cred_0.NEXTVAL;

            INSERT INTO dis_arh_num_cred_0 VALUES(v_id_dis_arh_num_cred, 
                                                  p_folio,
                                                  v_id_derechohabiente,
                                                  v_nss,
                                                  v_num_credito,
                                                  v_periodo_pago,
                                                  v_f_pago,
                                                  v_nrp,
                                                  v_aportacion,
                                                  v_amortizacion,
                                                  v_folio_sua,
                                                  v_inconsis,
                                                  v_estado);

            IF v_ins_info_liq_arh = 1 THEN
			   
               LET v_folio_cta_hp         = 0;
               LET v_id_referencia_cta_hp = 0;

               SELECT cta.folio, cta.id_referencia
               INTO   v_folio_cta_hp, v_id_referencia_cta_hp
               FROM   tmp_dis_pagos_0 cta
               WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
               AND    cta.folio_sua                      = v_folio_sua
               AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
               AND    cta.f_pago                         = v_f_pago
               AND    cta.nrp                            = v_nrp
               AND    cta.ind_liquidacion           NOT IN (1,6);
				  
               LET v_folio_glo             = "";
               LET v_bnd_existe_folio_disp = 0; --12
				  
               FOREACH
                  SELECT FIRST 1 glo.folio, f_actualiza
                  INTO   v_folio_glo, v_f_actualiza
                  FROM   glo_folio glo
                  WHERE  glo.folio_referencia =  v_folio_cta_hp
                  AND    glo.status           = 2
                  AND    glo.proceso_cod      = 901
                  --ORDER BY f_actualiza DESC
                  ORDER BY f_actualiza ASC
               END FOREACH;
			   
               INSERT INTO dis_liq_inconsistente VALUES(v_id_derechohabiente,
                                                        v_id_referencia_cta_hp,
                                                        0,   ---ACT liquidacion de la dispersion
                                                        v_folio_cta_hp,
                                                        v_folio_glo,
                                                        v_estado,
                                                        NULL, ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        v_id_dis_arh_num_cred,
                                                        p_folio);

            END IF

            LET v_estado           = 0;
            LET v_ins_info_liq_arh = 0;
            
         ELSE
         
            LET v_folio_cta_hp         = 0;
            LET v_id_referencia_cta_hp = 0;

            SELECT cta.folio, cta.id_referencia
            INTO   v_folio_cta_hp, v_id_referencia_cta_hp
            FROM   tmp_dis_pagos_0 cta
            WHERE  cta.id_derechohabiente             = v_id_derechohabiente 
            AND    cta.folio_sua                      = v_folio_sua
            AND    fn_bimestre_pago(cta.periodo_pago) = v_periodo_pago
            AND    cta.f_pago                         = v_f_pago
            AND    cta.nrp                            = v_nrp
            AND    cta.ind_liquidacion           NOT IN (1,6);

            IF (v_folio_cta_hp IS NULL OR v_folio_cta_hp = "") AND
               (v_id_referencia_cta_hp IS NULL OR v_id_referencia_cta_hp = "") THEN
               --NO existe Pago
               --
               LET v_inconsis = TRIM(v_inconsis)||"07";

               IF v_bnd_existe_info_inc >= 1 THEN
                  LET v_inconsis = TRIM(v_inconsis)||"00";  --08
               ELSE
                  LET v_inconsis = TRIM(v_inconsis)||"08";  --08
               END IF
               
               LET v_inconsis = TRIM(v_inconsis)||"00";  --09
               LET v_estado = 1;
           
               LET v_bnd_existe_folio_disp = 0;          --12
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";   --07

               LET v_folio_glo = "";
               LET v_bnd_existe_folio_disp = 0; --12

               FOREACH
                 SELECT FIRST 1 glo.folio, f_actualiza
                 INTO   v_folio_glo, v_f_actualiza
                 FROM   glo_folio glo
                 WHERE  glo.folio_referencia =  v_folio_cta_hp
                 AND    glo.status           = 2
                 AND    glo.proceso_cod      = 901
                 --ORDER BY f_actualiza DESC
                 ORDER BY f_actualiza ASC
               END FOREACH;

               IF v_folio_glo IS NULL OR v_folio_glo = "" THEN
                  --No existe Folio de la Dispersion

                  IF v_bnd_existe_info_inc >= 1 THEN
                     LET v_inconsis = TRIM(v_inconsis)||"00";  --08
                  ELSE
                     LET v_inconsis = TRIM(v_inconsis)||"08";  --08
                  END IF
                  
                  LET v_inconsis = TRIM(v_inconsis)||"00";   --09
                  
                  LET v_bnd_existe_folio_disp = 0; --12
               ELSE
                  LET v_bnd_existe_folio_disp = 1; --12
                  LET v_bnd_existe_inconsis = 0;
                  
                  SELECT COUNT(*)
                  INTO  v_bnd_existe_inconsis
                  FROM  dis_info_inconsistente dii
                  WHERE dii.folio_liquida = v_folio_glo
                  AND   dii.id_referencia = v_id_referencia_cta_hp;

                  IF v_bnd_existe_inconsis >= 1 THEN  
                     --Existe información en dis_info_inconsistente
                     LET v_bnd_existe_inconsis_0 = 0;

                     SELECT COUNT(*)
                     INTO  v_bnd_existe_inconsis_0
                     FROM  dis_info_inconsistente dii
                     WHERE dii.folio_liquida = v_folio_glo
                     AND   dii.id_referencia = v_id_referencia_cta_hp
                     AND   dii.tpo_inconsistente = 0;

                     IF v_bnd_existe_inconsis_0 >= 1 THEN
                        LET v_inconsis = TRIM(v_inconsis)||"00";
                     ELSE
                        LET v_estado = 1;
                        LET v_inconsis = TRIM(v_inconsis)||"08";
                     END IF

                     LET v_bnd_existe_inconsis_10 = 0;

                     SELECT COUNT(*)
                     INTO  v_bnd_existe_inconsis_10
                     FROM  dis_info_inconsistente dii
                     WHERE dii.folio_liquida = v_folio_glo
                     AND   dii.id_referencia = v_id_referencia_cta_hp
                     AND   dii.tpo_inconsistente = 10;

                     IF v_bnd_existe_inconsis_10 >= 1 THEN
                        LET v_estado = 1;
                        LET v_inconsis = TRIM(v_inconsis)||"09";
                     ELSE
                        LET v_inconsis = TRIM(v_inconsis)||"00";
                     END IF
                  ELSE
                     --No existe información en dis_info_inconsistente
                     LET v_inconsis = TRIM(v_inconsis)||"08";   --08
                     LET v_inconsis = TRIM(v_inconsis)||"00";   --09
                     LET v_estado = 1;
                  END IF
               END IF
            END IF

            -- El abono o pago indicado en el archivo insumo presenta cargo 
            -- por un movimiento diverso al de Dispersión.
            LET v_bnd_mov_div_disp = 0;


                  { SELECT COUNT(*)
                  INTO v_bnd_mov_div_disp
                  FROM  tmp_cta_movimiento a
                  WHERE a.id_derechohabiente = v_id_derechohabiente
                  AND   a.f_liquida          >= v_f_pago 
                  AND   a.movimiento         IN (SELECT b.movimiento
                                                  FROM   cat_movimiento b
                                                  WHERE  b.categoria = 2);} --Cargos
											   
            SELECT COUNT(*)
            INTO v_bnd_mov_div_disp
            FROM  tmp_cta_movimiento a
            WHERE a.id_derechohabiente = v_id_derechohabiente
            AND   a.f_liquida         >= v_f_pago; --Fecha Pago

            IF v_bnd_mov_div_disp >= 1 THEN
               LET v_inconsis = TRIM(v_inconsis)||"10";  --10
               --LET v_estado = 1;
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";  --10
            END IF
               
            LET v_inconsis = TRIM(v_inconsis)||"00";  --11

            IF v_bnd_existe_folio_disp = 1 THEN
               LET v_inconsis = TRIM(v_inconsis)||"00";  --12
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"12";  --12
               LET v_estado = 1;
            END IF
               
            IF v_num_credito IS NULL OR  v_num_credito = 0 THEN
               LET v_inconsis = TRIM(v_inconsis)||"13";  --13
               LET v_estado = 1;
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";  --13
            END IF

            LET v_bnd_reg_duplicado = 0;
               
            SELECT COUNT(*)
            INTO v_bnd_reg_duplicado
            FROM dis_arh_num_cred_0
            WHERE folio                = p_folio
            AND   id_derechohabiente   = v_id_derechohabiente
            AND   periodo_pago         = v_periodo_pago
            AND   f_pago               = v_f_pago
            AND   nrp                  = v_nrp
            AND   folio_sua            = v_folio_sua;
                     
            IF v_bnd_reg_duplicado >= 1 THEN
               LET v_inconsis = TRIM(v_inconsis)||"14";  --14  REGISTRO DUPLICADO
               LET v_estado = 1;
            ELSE
               LET v_inconsis = TRIM(v_inconsis)||"00";  --14
            END IF
               
            LET v_inconsis = TRIM(v_inconsis)||"00";  --15

            LET v_id_dis_arh_num_cred = seq_dis_arh_num_cred_0.NEXTVAL;
               
            INSERT INTO dis_arh_num_cred_0 VALUES(v_id_dis_arh_num_cred, 
                                                  p_folio,
                                                  v_id_derechohabiente,
                                                  v_nss,
                                                  v_num_credito,
                                                  v_periodo_pago,
                                                  v_f_pago,
                                                  v_nrp,
                                                  v_aportacion,
                                                  v_amortizacion,
                                                  v_folio_sua,
                                                  v_inconsis,
                                                  v_estado);


            IF v_estado = 0 THEN
               --insertar en dis_liq_inconsistente
               {id_derechohabiente --v_id_derechohabiente
               id_referencia       --v_id_referencia_cta_hp 
               folio_liquida       --   
               folio_pago          --v_folio_cta_hp
               folio_liquida_orig  --v_folio_glo
               edo_liquida         --v_estado
               num_credito         -- pendiente...
               aportacion          -- pendiente... 
               amortizacion        -- pendiente...
               aivs                -- pendiente ...
               id_dis_arh_num_cred -- v_id_dis_arh_num_cred
               folio_arh_num_cred} -- p_folio

               INSERT INTO dis_liq_inconsistente VALUES(v_id_derechohabiente,
                                                        v_id_referencia_cta_hp,
                                                        0,   ---ACT liquidacion de la dispersion
                                                        v_folio_cta_hp,
                                                        v_folio_glo,
                                                        v_estado,
                                                        NULL, ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        0,    ---ACT liquidacion de la dispersion
                                                        v_id_dis_arh_num_cred,
                                                        p_folio);
            END IF

            LET v_estado = 0;
            LET v_bnd_existe_folio_disp = 0;
         END IF
         
      ELSE
         IF v_num_credito IS NULL OR  v_num_credito = 0 THEN
            LET v_inconsis = TRIM(v_inconsis)||"00000000000013";
            LET v_estado = 1;
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"00000000000000";
         END IF

         LET v_bnd_reg_duplicado = 0;
               
         SELECT COUNT(*)
         INTO v_bnd_reg_duplicado
         FROM dis_arh_num_cred_0
         WHERE folio                = p_folio
         AND   id_derechohabiente   = v_id_derechohabiente
         AND   periodo_pago         = v_periodo_pago
         AND   f_pago               = v_f_pago
         AND   nrp                  = v_nrp
         AND   folio_sua            = v_folio_sua;
         IF v_bnd_reg_duplicado >= 1 THEN
            LET v_inconsis = TRIM(v_inconsis)||"14";  --14  REGISTRO DUPLICADO
         ELSE
            LET v_inconsis = TRIM(v_inconsis)||"00";  --14
         END IF
                  
         LET v_inconsis = TRIM(v_inconsis)||"00";  --15
               
         LET v_id_dis_arh_num_cred = seq_dis_arh_num_cred_0.NEXTVAL;

         INSERT INTO dis_arh_num_cred_0 VALUES(v_id_dis_arh_num_cred, 
                                               p_folio,
                                               v_id_derechohabiente,
                                               v_nss,
                                               v_num_credito,
                                               v_periodo_pago,
                                               v_f_pago,
                                               v_nrp,
                                               v_aportacion,
                                               v_amortizacion,
                                               v_folio_sua,
                                               v_inconsis,
                                               v_estado);

         LET v_estado = 0;
         LET v_bnd_existe_folio_disp = 0;

      END IF
   END FOREACH;

   SET INDEXES FOR dis_arh_num_cred_0 ENABLED;
   SET INDEXES FOR dis_liq_inconsistente ENABLED;

   UPDATE STATISTICS FOR TABLE dis_arh_num_cred_0;

   --TRACE 'Finaliza sp_dis_pagos_avances con valor '||v_bnd_proceso;
   LET v_char = "Terminado Validacion_Cred_Cero_SPL";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


