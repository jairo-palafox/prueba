






CREATE FUNCTION "safreviv".fn_ocg_actualiza_ef(p_usuario CHAR(20))

   RETURNING SMALLINT

   DEFINE v_nss                  CHAR(11);
   DEFINE v_orig_ef              CHAR(3);
   DEFINE v_nva_ef		           CHAR(3);
   DEFINE v_nvo_ctrl_int         CHAR(18);
   DEFINE v_rch_desc             CHAR(100);
   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion DECIMAL(9,0);
   DEFINE v_cve_ent_financiera   SMALLINT;
   DEFINE v_num_ctr_int_ef       CHAR(18);
   DEFINE v_error                SMALLINT;
   DEFINE v_id_ocg_tramite       DECIMAL(9,0);
   DEFINE v_id_ocg_sol_ug        DECIMAL(9,0);
   DEFINE v_id_ocg_dev           DECIMAL(9,0);
   DEFINE v_id_ocg_det           DECIMAL(9,0);
   DEFINE v_id_ocg_form          DECIMAL(9,0);
   DEFINE v_id_ocg_tmt           DECIMAL(9,0);
   DEFINE v_num_ctr_int_dev      CHAR(18);
   DEFINE v_id_ocg_ctr_trans     DECIMAL(9,0);
   DEFINE v_id_ocg_ctr_form      DECIMAL(9,0);
   DEFINE v_id_ref_cta		       DECIMAL(9,0);
   DEFINE v_id_ocgf_det          DECIMAL(9,0);
   DEFINE v_id_ocgf_trmt         DECIMAL(9,0);
   DEFINE v_id_ocgt_detalle      DECIMAL(9,0);
   DEFINE v_id_ocgug_det         DECIMAL(9,0);

 --var's de conteo
   DEFINE v_exist_dh             INTEGER;
   DEFINE v_exist_43Bis          INTEGER;
   DEFINE v_ef_diferente         INTEGER;
   DEFINE v_exist_ef             INTEGER;
  
   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ocg_actualiza_num_ctr_int_ef.trace';
   --TRACE ON ;

  LET v_error        = 0;
  LET v_exist_dh     = 0;
  LET v_exist_43Bis  = 0;
  LET v_ef_diferente = 0;
  LET v_exist_ef     = 0;

  FOREACH
      --#Recupera registros de la tabla  tmp_act_ef
      SELECT *
        INTO v_nss,
             v_orig_ef,
             v_nva_ef,
             v_nvo_ctrl_int
        FROM safre_tmp:tmp_act_ef

       --#Verifica que exista en afi_derechohabiente
      SELECT COUNT(*) 
         INTO v_exist_dh
         from afi_derechohabiente
        WHERE nss = v_nss;

      IF(v_exist_dh = 0) THEN
         --#Si no existe el dh, inserta en la tmp de rechazos
	       SELECT rechazo_desc
            INTO v_rch_desc
            FROM cat_ocg_rechazos
           WHERE id_cod_rechazo = 4; --EL TRABAJADOR NO EXISTE

          INSERT INTO safre_tmp:tmp_reg_rch_ef           
            VALUES ( v_nss,
                     v_orig_ef,
                     v_nva_ef,
                     v_nvo_ctrl_int,
                     v_rch_desc);
      ELSE
         --#Recupera el id_derechohabiente
         SELECT id_derechohabiente 
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         --Verifica que no sea un crédito liquidado
         IF EXISTS ( SELECT id_derechohabiente
                        FROM ocg_formalizacion
                       WHERE id_derechohabiente  = v_id_derechohabiente
                         AND cve_ent_financiera = v_orig_ef
                         AND diagnostico        = 1
                         AND situacion IN (140,150,160)) THEN
 
             --Rechazo por crédito liquidado
             SELECT rechazo_desc
               INTO v_rch_desc
               FROM cat_ocg_rechazos
              WHERE id_cod_rechazo = 8; --Crédito Liquidado

             INSERT INTO safre_tmp:tmp_reg_rch_ef
                VALUES ( v_nss,
                         v_orig_ef,
                         v_nva_ef,
                         v_nvo_ctrl_int,
                         v_rch_desc);
         ELSE  

         --#Verifica que tenga un crédito 43Bis vigente
         SELECT COUNT(*)
            INTO v_exist_43Bis 
            FROM ocg_formalizacion
           WHERE id_derechohabiente = v_id_derechohabiente
	           AND diagnostico = 1  
             AND estado      = 20 
             AND situacion IN (55,60,70,80); 

         IF(v_exist_43Bis = 0) THEN
            --#En caso de no tener un crédito vigente inserta en la tmp de rechazos
            SELECT rechazo_desc
              INTO v_rch_desc
              FROM cat_ocg_rechazos
             WHERE id_cod_rechazo = 5; --El trabajador no cuenta con crédito 43Biss vigente.

            INSERT INTO safre_tmp:tmp_reg_rch_ef
               VALUES ( v_nss,
                        v_orig_ef,
                        v_nva_ef,
                        v_nvo_ctrl_int,
                        v_rch_desc
                      );
         ELSE 
            --Verifica que el crédito esté formalizada con la EF capturada
            SELECT COUNT(*)
               INTO v_ef_diferente
               FROM ocg_formalizacion
              WHERE id_derechohabiente = v_id_derechohabiente
                AND cve_ent_financiera = v_orig_ef
                AND diagnostico = 1
                AND estado      = 20
                AND situacion IN (55,60,70,80);

            IF(v_ef_diferente = 0) THEN 

               SELECT rechazo_desc
                 INTO v_rch_desc
                 FROM cat_ocg_rechazos
                WHERE id_cod_rechazo = 2; -- EF diferente a la formalizada

               INSERT INTO safre_tmp:tmp_reg_rch_ef
                  VALUES ( v_nss,
                           v_orig_ef,
                           v_nva_ef,
                           v_nvo_ctrl_int,
                           v_rch_desc
                         );
            ELSE

               --#Si cuenta con un crédito 43Bis vigente se recupera información de ocg_formalizacion
               SELECT id_ocg_formalizacion,
                      id_ocg_detalle,
                      id_ocg_tramite,
                      cve_ent_financiera,
                      num_ctr_int_ef
                 INTO v_id_ocg_formalizacion,
                      v_id_ocgf_det,
                      v_id_ocgf_trmt,
	                    v_cve_ent_financiera,
                      v_num_ctr_int_ef
	               FROM ocg_formalizacion
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND cve_ent_financiera = v_orig_ef
                  AND diagnostico = 1  
                  AND estado      = 20 
               AND situacion IN (55,60,70,80);

               --#Antes de inciciar la actualización, se verifica que la EF exista en el catalogo de EF
	             SELECT COUNT(*)
                  INTO v_exist_ef
                  FROM cat_entidad_financiera
                 WHERE cve_ent_financiera = v_nva_ef;

               IF(v_exist_ef = 0) THEN

                  --Si no existe en el catalogo rechaza el registro
                  SELECT rechazo_desc
                     INTO v_rch_desc
                     FROM cat_ocg_rechazos
                    WHERE id_cod_rechazo = 7; --La EF no existe en el catálogo de EF

                  INSERT INTO safre_tmp:tmp_reg_rch_ef
                     VALUES(v_nss,
                            v_orig_ef,
                            v_nva_ef,
                            v_nvo_ctrl_int,
                            v_rch_desc);

               ELSE
                  IF( v_nvo_ctrl_int = "                  ") OR 
                  (v_nvo_ctrl_int IS NULL) THEN 

                     --Si es NULO solo actualiza la EF
                     UPDATE ocg_formalizacion
                        SET cve_ent_financiera   = v_nva_ef
                      WHERE id_derechohabiente   = v_id_derechohabiente
                       AND  id_ocg_formalizacion = v_id_ocg_formalizacion
                       AND  cve_ent_financiera   = v_orig_ef
                       AND  num_ctr_int_ef = v_num_ctr_int_ef
                       AND  diagnostico    = 1   
                       AND  estado         = 20  
                       AND  situacion IN (55,60,70,80); 
                  ELSE

                     --Si no es NULO actualiza la EF y el número de control interno
                     UPDATE ocg_formalizacion
                        SET cve_ent_financiera   = v_nva_ef,
                            num_ctr_int_ef       = v_nvo_ctrl_int
                      WHERE id_derechohabiente   = v_id_derechohabiente
                        AND id_ocg_formalizacion = v_id_ocg_formalizacion
                        AND cve_ent_financiera = v_orig_ef
                        AND num_ctr_int_ef     = v_num_ctr_int_ef
                        AND diagnostico        = 1   
                        AND estado             = 20  
                        AND situacion IN (55,60,70,80);

                  END IF

                  --Actualiza en ocg_detalle para el proceso = 2
                  UPDATE ocg_detalle
                     SET cve_ent_financiera = v_nva_ef
                   WHERE id_ocg_detalle     = v_id_ocgf_det
                     AND subproceso         = 2;  

                  --Guada historico en ocg_his_formalizacion
                  INSERT INTO ocg_his_formalizacion
                           	      ( id_ocg_formalizacion,
                                    id_derechohabiente,
                                    cve_ent_financiera,
                                    num_ctr_int_ef,
                                    f_proceso,
                                    usuario
                                  )
                           VALUES ( v_id_ocg_formalizacion,
                                    v_id_derechohabiente,
                                    v_orig_ef,         --original
                                    v_num_ctr_int_ef,  --original
                                    TODAY,
                                    p_usuario
                                  );

                  --Se comprueba que exista en ocg_tramite
                  IF EXISTS (SELECT id_derechohabiente
                               FROM ocg_tramite 
                              WHERE id_derechohabiente = v_id_derechohabiente
                                AND id_ocg_tramite     = v_id_ocgf_trmt
                                AND cve_ent_financiera = v_orig_ef
                                AND id_ocg_tramite     = v_id_ocgf_trmt
                                AND diagnostico        = 1
                                AND estado             = 20
                                AND situacion IN (55,60,70,80)) THEN 

                     /***** Actualiza en ocg_tramite *****/
                     UPDATE ocg_tramite
                        SET cve_ent_financiera = v_nva_ef
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND id_ocg_tramite     = v_id_ocgf_trmt
                        AND cve_ent_financiera = v_orig_ef
                        AND diagnostico        = 1  
                        AND estado             = 20  
                        AND situacion IN (55,60,70,80);

	                   --Recupera id_ocg_tramite y id_ocg_detalle
                     SELECT id_ocg_tramite,
                            id_ocg_detalle
	                     INTO v_id_ocg_tramite,
                            v_id_ocgt_detalle
                       FROM ocg_tramite 
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND id_ocg_tramite     = v_id_ocgf_trmt
                        AND diagnostico = 1  
                        AND estado      = 20 
                        AND situacion IN (55,60,70,80);

                     --Actualiza ocg_detalle para el proceso 1
                     UPDATE ocg_detalle
                        SET cve_ent_financiera = v_nva_ef
                      WHERE id_ocg_detalle     = v_id_ocgt_detalle
                        AND subproceso = 1;

                     --Guarda historico en ocg_his_tramite
                     INSERT INTO ocg_his_tramite
                                  ( id_ocg_tramite,
                                    cve_ent_financiera,
                                    id_derechohabiente,
                                    f_proceso,
                                    usuario
                                  )
                           VALUES ( v_id_ocg_tramite,
                                    v_orig_ef, --original
                                    v_id_derechohabiente,
                                    TODAY,
                                    p_usuario
                                  );
 
                  END IF
 
                 --Se comprueba que exista en ocg_solicitud_uso_garantia
                 IF EXISTS ( SELECT id_derechohabiente
                               FROM ocg_solicitud_uso_garantia
                              WHERE id_derechohabiente   = v_id_derechohabiente
                                AND id_ocg_formalizacion = v_id_ocg_formalizacion
                                AND cve_ent_financiera = v_orig_ef
                                AND num_ctr_int_ef     = v_num_ctr_int_ef
                                AND diagnostico        = 1
                                AND estado             = 20
                                AND situacion IN (50,90)) THEN 

                    IF (v_nvo_ctrl_int = "                  ") OR 
                       (v_nvo_ctrl_int IS NULL) THEN

                        --solo actualiza la EF
                        UPDATE ocg_solicitud_uso_garantia
                           SET cve_ent_financiera = v_nva_ef
                         WHERE id_derechohabiente = v_id_derechohabiente
                          AND  id_ocg_formalizacion = v_id_ocg_formalizacion
                          AND  cve_ent_financiera = v_orig_ef
                          AND  num_ctr_int_ef     = v_num_ctr_int_ef
                          AND  diagnostico        = 1
                          AND  estado             = 20
                          AND  situacion IN (50,90); 
                    ELSE
                       --En caso que no sea NULO actualiza EF y número de control interno
                        UPDATE ocg_solicitud_uso_garantia
                           SET cve_ent_financiera = v_nva_ef,
                               num_ctr_int_ef     = v_nvo_ctrl_int
                         WHERE id_derechohabiente = v_id_derechohabiente
                          AND  id_ocg_formalizacon = v_id_ocg_formalizacion
                          AND  cve_ent_financiera = v_orig_ef
                          AND  num_ctr_int_ef     = v_num_ctr_int_ef
                          AND  diagnostico        = 1
                          AND  estado             = 20
                          AND  situacion IN (50,90);

                    END IF 

	                  --Recupera los registros que se actualizaron para guardarlos en historico
	                 FOREACH

                       SELECT id_ocg_solicitud_ug,
                              id_ocg_detalle
		                     INTO v_id_ocg_sol_ug,
                              v_id_ocgug_det
                         FROM ocg_solicitud_uso_garantia
                        WHERE id_derechohabiente   = v_id_derechohabiente
                         AND  id_ocg_formalizacion = v_id_ocg_formalizacion
                         AND  diagnostico  = 1   
                         AND  estado       = 20  
                         AND  situacion IN (50,90)

                       IF(v_id_ocgug_det IS NOT NULL) THEN 
                          --Actualiza ocg_detalle para el subproceso 3
                          UPDATE ocg_detalle
                             SET cve_ent_financiera = v_nva_ef
                           WHERE id_ocg_detalle     = v_id_ocgug_det
                             AND subproceso = 3;

                       END IF

		                  --Guarda historico en ocg_his_solic_uso_garantia
		                  INSERT INTO ocg_his_solic_uso_garantia
                                 	      ( id_ocg_solicitud_ug,
                                          id_derechohabiente,
                                          cve_ent_financiera,
                                          num_ctr_int_ef,
                                          f_proceso,
                                          usuario)
                                 VALUES ( v_id_ocg_sol_ug,
                                          v_id_derechohabiente,
                                          v_orig_ef,         --original
                                          v_num_ctr_int_ef,  --original
                                          TODAY,
                                          p_usuario
                                         );
                   END FOREACH
                 END IF
                       -- Guarda en ocg_portabilidad el indicador de cómo se actualizó
                       INSERT INTO ocg_portabilidad
                                 ( id_derechohabiente,
                                   id_ocg_tramite,
                                   id_ocg_formalizacion,
                                   nss,
                                   cve_ent_fin_anterior,
                                   cve_ent_fin_actual,
                                   ind_actualiza,
                                   f_actualiza,
                                   usuario
                                 )
                          VALUES ( v_id_derechohabiente,
                                   v_id_ocgf_trmt,
                                   v_id_ocg_formalizacion,
                                   v_nss,
                                   v_orig_ef,
                                   v_nva_ef,
                                   3,          --Actualización por archivo
                                   TODAY,
                                   p_usuario);  
               END IF
            END IF  
	        END IF
        END IF  
      END IF 
   END FOREACH
 
   --Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_his_formalizacion;
   UPDATE STATISTICS FOR TABLE ocg_his_tramite;
   UPDATE STATISTICS FOR TABLE ocg_his_solic_uso_garantia; 

   RETURN v_error;
END FUNCTION;


