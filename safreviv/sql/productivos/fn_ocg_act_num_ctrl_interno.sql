






CREATE FUNCTION "safreviv".fn_ocg_act_num_ctrl_interno(p_usuario CHAR(20))

   RETURNING SMALLINT

   DEFINE v_nss                  CHAR(11);
   DEFINE v_cve_ef               CHAR(3);
   DEFINE v_nvo_num_ctr_int      CHAR(18);
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion DECIMAL(9,0);
   DEFINE v_cve_ent_financiera   SMALLINT;
   DEFINE v_num_ctr_int_ef       CHAR(18);
   DEFINE v_id_ocg_sol_ug	       DECIMAL(9,0);
   DEFINE v_id_ocg_trans         DECIMAL(9,0);
   DEFINE v_id_ocg_form_ctr      DECIMAL(9,0);
   DEFINE v_id_ref_cta           DECIMAL(9,0);
   DEFINE v_rch_desc             CHAR(100);
   DEFINE v_error                SMALLINT;
   --var's de conteo
   DEFINE v_exist_dh             INTEGER;
   DEFINE v_exist_43Bis          INTEGER;
   DEFINE v_exist_ef             INTEGER;
   DEFINE v_dif_ef               INTEGER;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_actualiza_num_ctr_int_ef.trace';
   --TRACE ON ;

  LET v_error       = 0;
  LET v_exist_dh    = 0;
  LET v_exist_43Bis = 0;
  LET v_exist_ef    = 0;
  LET v_dif_ef      = 0;
  LET v_rch_desc    = NULL;

   FOREACH

      --#Recupera registros de la tabla tmp tmp_num_ctr_int_ef
      SELECT *
        INTO v_nss,
             v_cve_ef,
             v_nvo_num_ctr_int
        FROM safre_tmp:tmp_num_ctr_int_ef

       --#Verifica que exista en afi_derechohabiente
      SELECT COUNT(*) 
         INTO v_exist_dh
         from afi_derechohabiente
        WHERE nss = v_nss;

      IF(v_exist_dh = 0) THEN
	  
         SELECT rechazo_desc
            INTO v_rch_desc
            FROM cat_ocg_rechazos
           WHERE id_cod_rechazo = 4; -- El trabajador no existe

          INSERT INTO safre_tmp:tmp_reg_rechazados           
            VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
      ELSE
         --#Recupera el id_derechohabiente
         SELECT id_derechohabiente 
	   INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         --#Verifica que tenga un crédito 43Bis vigente
         SELECT COUNT(*)
            INTO v_exist_43Bis 
            FROM ocg_formalizacion
           WHERE id_derechohabiente = v_id_derechohabiente 
	     AND diagnostico = 1  --valido 
           --AND estado      = 20 -- vigente
             AND situacion IN (55,60,70,80); --aceptados

         IF(v_exist_43Bis = 0) THEN
            
            SELECT rechazo_desc
              INTO v_rch_desc
              FROM cat_ocg_rechazos
             WHERE id_cod_rechazo = 5; -- El trabajador no cuenta con crédito 43BIS vigente

            INSERT INTO safre_tmp:tmp_reg_rechazados
               VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
         ELSE 
            -- Verifica que el crédito esté formalizado con la EF capturada
            SELECT COUNT(*)
               INTO v_dif_ef
               FROM ocg_formalizacion
              WHERE id_derechohabiente = v_id_derechohabiente
                AND cve_ent_financiera = v_cve_ef
                AND diagnostico = 1
              --AND estado      = 20
                AND situacion IN (55,60,70,80);
            
            IF(v_dif_ef = 0) THEN
               
               SELECT rechazo_desc
                  INTO v_rch_desc
                  FROM cat_ocg_rechazos
                 WHERE id_cod_rechazo = 2; --EF diferente a la formalizada

               INSERT INTO safre_tmp:tmp_reg_rechazados
                  VALUES (v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
            ELSE 
             
               --#Si cuenta con un crédito 43Bis vigente se recupera información de ocg_formalizacion
               SELECT id_ocg_formalizacion,
                      cve_ent_financiera,
                      num_ctr_int_ef
                 INTO v_id_ocg_formalizacion,
	              v_cve_ent_financiera,
                      v_num_ctr_int_ef
	         FROM ocg_formalizacion
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND cve_ent_financiera = v_cve_ef
                  AND diagnostico = 1  --valido
                --AND estado      = 20 --vigente
                  AND situacion IN (55,60,70,80); --aceptados

               -- Antes de iniciar la actualizacion se verifica que exista la EF en el catálogo
               SELECT COUNT(*)
                 INTO v_exist_ef
                 FROM cat_entidad_financiera
                WHERE cve_ent_financiera = v_cve_ef; 

              IF (v_exist_ef = 0) THEN
                 --Si no existe en el catalogo rechaza el registro
                 SELECT rechazo_desc
                   INTO v_rch_desc
                   FROM cat_ocg_rechazos
                  WHERE id_cod_rechazo = 7; --La ef no existe en el catálogo de EF

                 INSERT INTO safre_tmp:tmp_reg_rechazados
                    VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc); 
              ELSE
                 /**********Actualiza en ocg_formalizacion**********/
	         UPDATE ocg_formalizacion
	            SET num_ctr_int_ef     = v_nvo_num_ctr_int
	          WHERE id_derechohabiente = v_id_derechohabiente
	            AND cve_ent_financiera = v_cve_ent_financiera
	            AND num_ctr_int_ef     = v_num_ctr_int_ef
                    AND diagnostico        = 1
                  --AND estado             = 20
                    AND situacion IN (55,60,70,80);

                 /*****Guarda en historico ocg_his_formalizacion*****/
                 INSERT INTO ocg_his_formalizacion(
		             	 id_ocg_formalizacion,
				 id_derechohabiente,
	                  	 cve_ent_financiera,
			         num_ctr_int_ef,
			         f_proceso,
				 usuario)
	        	 VALUES( v_id_ocg_formalizacion,
				 v_id_derechohabiente,
                                 v_cve_ent_financiera,
			         v_num_ctr_int_ef, --Original
		                 TODAY,
			         p_usuario
			        );
			      
	         --Se verifica que exista en ocg_solicitud_uso_garantia
                 IF EXISTS ( SELECT id_derechohabiente 
                               FROM ocg_solicitud_uso_garantia
                              WHERE id_derechohabiente = v_id_derechohabiente
                                AND cve_ent_financiera = v_cve_ent_financiera
                                AND num_ctr_int_ef     = v_num_ctr_int_ef
                                AND diagnostico        = 1
                              --AND estado             = 20
                                AND situacion IN (50,90)) THEN 
     
	            /*****Actualiza en ocg_solicitud_uso_garantia*****/
	            UPDATE ocg_solicitud_uso_garantia
	               SET num_ctr_int_ef = v_nvo_num_ctr_int
	             WHERE id_derechohabiente = v_id_derechohabiente
	               AND cve_ent_financiera = v_cve_ent_financiera
                       AND num_ctr_int_ef     = v_num_ctr_int_ef
                       AND diagnostico        = 1
                     --AND estado             = 20
                       AND situacion IN (50,90);
              
                    --Recupera regitros actualizados en ocg_solicitud_uso_garantia
	            FOREACH
	               SELECT id_ocg_solicitud_ug
		         INTO v_id_ocg_sol_ug 
                         FROM ocg_solicitud_uso_garantia
                        WHERE id_derechohabiente = v_id_derechohabiente
                          AND cve_ent_financiera = v_cve_ent_financiera
                          AND diagnostico        = 1
                        --AND estado             = 20
                          AND situacion IN (50,90) 

	               /*****Guarda historico en ocg_his_solic_uso_grantia*****/
	               INSERT INTO ocg_his_solic_uso_garantia
                               ( id_ocg_solicitud_ug,
                                 id_derechohabiente,
                                 cve_ent_financiera,
                                 num_ctr_int_ef,
                                 f_proceso,
                                 usuario)
                        VALUES ( v_id_ocg_sol_ug,
                                 v_id_derechohabiente,
                                 v_cve_ent_financiera,
                                 v_num_ctr_int_ef, --original
                                 TODAY,
                                 p_usuario
                               );
	            END FOREACH

                 END IF     
              END IF
            END IF
         END IF 
     END IF 
     
   END FOREACH
 
   --Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_his_formalizacion;
   UPDATE STATISTICS FOR TABLE ocg_his_solic_uso_garantia;

   RETURN v_error;

END FUNCTION;


