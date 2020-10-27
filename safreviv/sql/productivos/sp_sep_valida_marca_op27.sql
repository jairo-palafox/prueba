






CREATE PROCEDURE "safreviv".sp_sep_valida_marca_op27()
DEFINE v_val_nss           DECIMAL(18);

-- Detalle 02
DEFINE v_invadido_inv                 CHAR(11);
DEFINE v_tipo_entidad_admon_inv       CHAR(2);
DEFINE v_cve_entidad_admon_inv        CHAR(3);
DEFINE v_entidad_nacimiento_inv       CHAR(2);
DEFINE v_sexo_inv                     CHAR(1);
DEFINE v_rfc_inv                      CHAR(13);
DEFINE v_f_registro_inv               CHAR(8);
DEFINE v_f_marca_infosar_inv          CHAR(8);
DEFINE v_credito_infonavit_inv        CHAR(1);
DEFINE v_traspaso_previo_inv          CHAR(2);
DEFINE v_ind_cambio_clasificacion_inv CHAR(1);
DEFINE v_f_notificacion				  CHAR(8);
DEFINE v_ent_administradoras		  CHAR(12);
-- variables de validacion det 02
DEFINE v_existe_cve_ent_admon         CHAR(1);
DEFINE v_existe_ent_ncmto_inv         CHAR(1);
DEFINE v_existe_diag_conf_inv         CHAR(1);
DEFINE v_existe_clas_sep_inv          CHAR(1);
DEFINE v_fec_reg_inv                  DATE;
DEFINE entidad1						  SMALLINT;
DEFINE entidad2						  SMALLINT;
DEFINE entidad3                       SMALLINT;
DEFINE entidad4                       SMALLINT;
DEFINE suma_entidad					  SMALLINT;
DEFINE entidad_exist				  SMALLINT;

-- Detalle 03 YA NO VALIDA DETALLE 03
--DEFINE v_contador_servicio_asc     CHAR(10);
--DEFINE v_asociado_asc              CHAR(11);
--DEFINE v_tipo_entidad_asociado_asc CHAR(2);
--DEFINE v_cve_entidad_asociado_asc  CHAR(3);
--DEFINE v_resultado_operacion_asc   CHAR(2); 
--DEFINE v_diagnostico1_asc          CHAR(3);  
-- variables de validacion det 03
--DEFINE v_existe_tpo_ent_asc        CHAR(1);
--DEFINE v_existe_cve_ent_asc        CHAR(1);

-- Control de errores
DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      
      -- TRACE "00";
      -- error de conversion de fecha
      IF(v_error_sql = -1204 OR 
         v_error_sql = -1205 OR 
         v_error_sql = -1206 OR
         v_error_sql = -1218)THEN
               
      END IF
   END EXCEPTION WITH RESUME;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_valida_marca_op27.trace';
   
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   -- TRACE "1";
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 02 --
   ---------------------------------
   -- recupera el detalle del archivo cargado para la validacion de valores de campos
   FOREACH SELECT TRIM(invadido),
                  TRIM(cve_afore),
                  TRIM(entidad_nacimiento),
                  TRIM(sexo),
                  TRIM(rfc),
                  TRIM(f_registro),
                  TRIM(f_marca_infosar),
                  credito_infonavit,
                  TRIM(traspaso_previo),
				  TRIM(f_notificacion),
				  TRIM(ent_administradoras)
             INTO v_invadido_inv                ,
                  v_cve_entidad_admon_inv       ,
                  v_entidad_nacimiento_inv      ,
                  v_sexo_inv                    ,
                  v_rfc_inv                     ,
                  v_f_registro_inv              ,
                  v_f_marca_infosar_inv         ,
                  v_credito_infonavit_inv       ,
                  v_traspaso_previo_inv			,
				  v_f_notificacion				,
				  v_ent_administradoras
             FROM safre_tmp:tmp_sep_det_02_op27
            WHERE 1 = 1
            
      -- TRACE "2";
      -- ***********************
      -- VALIDACIÓN NSS invadido
      -- ***********************
      -- valida nulo
      IF(LENGTH(TRIM(v_invadido_inv)) > 0)THEN
         LET v_val_nss = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_mdt_valida_numerico(v_invadido_inv) INTO v_error_sql, v_msg_sql,v_val_nss;
         
         IF(v_error_sql <> 0) THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("NSS Invadido no valido: "||v_invadido_inv);
         ELSE
            IF NOT(v_val_nss > 0) THEN
               INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
               VALUES("NSS Invadido no valido: "||v_invadido_inv);
            END IF
         END IF
      ELSE
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("NSS Invadido nulo con RFC: "||v_rfc_inv);
      END IF
      
      -- TRACE "3";
      -- *****************************
      -- VALIDACIÓN TIPO ENTIDAD ADMON
      -- *****************************
	  --*************YA NO SE HARA ESTA VALIDACION
      --IF NOT(LENGTH(TRIM(v_tipo_entidad_admon_inv)) > 0)THEN
      --   INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
      --   VALUES("Tipo entidad administradora nulo para NSS Invadido: "||v_invadido_inv);
      --ELSE
      --   IF(v_tipo_entidad_admon_inv <> '01')THEN
      --      -- TRACE "3.3";
      --      INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
      --      VALUES("Tipo entidad administradora "||v_tipo_entidad_admon_inv||" no valido para NSS Invadido: "||v_invadido_inv);
      --   END IF
      --END IF
      
      -- TRACE "4";       
      -- ******************************
      -- VALIDACIÓN CLAVE AFORE
      -- ******************************
      IF NOT(LENGTH(TRIM(v_cve_entidad_admon_inv)) > 0)THEN
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Clave entidad administradora nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         LET v_existe_cve_ent_admon = '0';
         SELECT NVL('1','0')
           INTO v_existe_cve_ent_admon
           FROM cat_afore
          WHERE afore_cod = v_cve_entidad_admon_inv;
         IF(v_existe_cve_ent_admon <> '1' OR v_existe_cve_ent_admon IS NULL)THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Clave entidad administradora "||v_cve_entidad_admon_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
      -- TRACE "5";
      -- ***********************************
      -- VALIDACIÓN CLAVE ENTIDAD NACIMIENTO
      -- ***********************************
      IF NOT(LENGTH(TRIM(v_entidad_nacimiento_inv)) > 0)THEN
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Entidad de nacimiento nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         LET v_existe_ent_ncmto_inv = '0';
         SELECT NVL('1','0')
           INTO v_existe_ent_ncmto_inv
           FROM cat_entidad_federativa
          WHERE entidad_federativa = v_entidad_nacimiento_inv;
         IF(v_existe_ent_ncmto_inv <> '1'OR v_existe_ent_ncmto_inv IS NULL)THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad de nacimiento "||v_entidad_nacimiento_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
      -- TRACE "6";
      -- ***************
      -- VALIDACIÓN SEXO
      -- ***************
      IF NOT(LENGTH(TRIM(v_sexo_inv)) > 0)THEN
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Género nulo para NSS Invadido: "||v_invadido_inv);
      ELSE
         IF(v_sexo_inv <> 1 AND 
            v_sexo_inv <> 2 AND
            v_sexo_inv <> 0 )THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Género no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
      -- TRACE "7";
      -- *************************
      -- VALIDACIÓN FECHA REGISTRO 
      -- *************************
      -- valida que se haya recuperado la fecha      
      IF NOT(LENGTH(TRIM(v_f_registro_inv)) > 0)THEN         
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha registro nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         -- valida que la fecha sea valida
         LET v_fec_reg_inv = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_sep_valida_fecha(v_f_registro_inv) INTO v_error_sql, v_msg_sql, v_fec_reg_inv;
         IF(v_error_sql <> 0)THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha registro "||v_f_registro_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
      -- TRACE "8";
      -- **********************
      -- VALIDACIÓN FECHA MARCA
      -- **********************
      IF NOT(LENGTH(TRIM(v_f_marca_infosar_inv)) > 0)THEN         
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha marca nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         -- valida que la fecha sea valida
         LET v_fec_reg_inv = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_sep_valida_fecha(v_f_marca_infosar_inv) INTO v_error_sql, v_msg_sql, v_fec_reg_inv;
         IF(v_error_sql <> 0)THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha marca "||v_f_marca_infosar_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
	  
      -- **********************
      -- VALIDACIÓN FECHA NOTIFICACION (NUEVO)
      -- **********************
      IF NOT(LENGTH(TRIM(v_f_notificacion)) > 0)THEN         
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha notificacion nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         -- valida que la fecha sea valida
         LET v_fec_reg_inv = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_sep_valida_fecha(v_f_notificacion) INTO v_error_sql, v_msg_sql, v_fec_reg_inv;
         IF(v_error_sql <> 0)THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha notificacion "||v_f_notificacion||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
	  
	  -- ***************************
	  -- VALIDACION ENTIDADES ADMINISTRADORAS (NUEVO)
	  --*************************
	  LET suma_entidad = 4;
	  LET entidad1 = v_ent_administradoras[1,3]::INTEGER;
	  LET entidad2 = v_ent_administradoras[4,6]::INTEGER;
	  LET entidad3 = v_ent_administradoras[7,9]::INTEGER;
	  LET entidad4 = v_ent_administradoras[10,12]::INTEGER;
      IF entidad1 == "" OR entidad1 IS NULL THEN
	     LET suma_entidad = suma_entidad - 1;
	  ELSE 
	     SELECT COUNT(*)
		    INTO entidad_exist
            FROM cat_afore
		    WHERE afore_cod = entidad1;
	     IF entidad_exist == 0 THEN
		    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad Administradora "||entidad1||" no valida para NSS Invadido: "||v_invadido_inv);   
		 END IF;
	  END IF;
	  IF entidad2 == "" OR entidad2 IS NULL THEN
	     LET suma_entidad = suma_entidad - 1;
	  ELSE 
	     SELECT COUNT(*)
		    INTO entidad_exist
            FROM cat_afore
		    WHERE afore_cod = entidad2;
	     IF entidad_exist == 0 THEN
		    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad Administradora "||entidad2||" no valida para NSS Invadido: "||v_invadido_inv);   
		 END IF;
	  END IF;
	  IF entidad3 == "" OR entidad3 IS NULL THEN
	     LET suma_entidad = suma_entidad - 1;
	  ELSE 
	     SELECT COUNT(*)
		    INTO entidad_exist
            FROM cat_afore
		    WHERE afore_cod = entidad3;
	     IF entidad_exist == 0 THEN
		    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad Administradora "||entidad3||" no valida para NSS Invadido: "||v_invadido_inv);   
		 END IF;
	  END IF;
	  IF entidad4 == "" OR entidad4 IS NULL THEN
	     LET suma_entidad = suma_entidad - 1;
	  ELSE 
	     SELECT COUNT(*)
		    INTO entidad_exist
            FROM cat_afore
		    WHERE afore_cod = entidad4;
	     IF entidad_exist == 0 THEN
		    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad Administradora "||entidad3||" no valida para NSS Invadido: "||v_invadido_inv);   
		 END IF;
	  END IF;
      -- TRACE "9";
      -- *************************
      -- VALIDACIÓN DIAG CONFRONTA
      -- *************************
      --IF NOT(LENGTH(TRIM(v_diag_confronta_inv)) > 0)THEN
         --INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         --VALUES("Diagnostico confronta nulo para NSS Invadido: "||v_invadido_inv);
      --ELSE
         --LET v_existe_diag_conf_inv = '0';
         --SELECT NVL('1','0')
           --INTO v_existe_diag_conf_inv
           --FROM sep_cat_diag_confronta
          --WHERE diag_confronta = v_diag_confronta_inv;
         --IF(v_existe_diag_conf_inv <> '1' OR v_existe_diag_conf_inv IS NULL)THEN
            --INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            --VALUES("Diagnostico confronta "||v_diag_confronta_inv||" no valido para NSS Invadido: "||v_invadido_inv);
         --END IF
      --END IF
      
      -- TRACE "10";
      -- *******************************
      -- VALIDACIÓN CLASIFICA SEPARACIÓN
      -- *******************************
     -- IF NOT(LENGTH(TRIM(v_clasifica_separacion_inv)) > 0)THEN
         --INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         --VALUES("Clasificación de separación nula para NSS Invadido: "||v_invadido_inv);
      --ELSE
         --LET v_existe_clas_sep_inv = '0';
         --SELECT NVL('1','0')
           --INTO v_existe_clas_sep_inv
           --FROM sep_cat_clasificacion
          --WHERE clasifica_separacion = v_clasifica_separacion_inv;
         --IF(v_existe_clas_sep_inv <> '1' OR v_existe_clas_sep_inv IS NULL)THEN
            --INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            --VALUES("Clasificación de separación "||v_clasifica_separacion_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         --END IF
      --END IF
      
      -- TRACE "11";      
      -- ************************
      -- VALIDACIÓN MARCA CRÉDITO
      -- ************************
      IF(v_credito_infonavit_inv <> '1' AND 
         v_credito_infonavit_inv <> ' ' AND 
         v_credito_infonavit_inv IS NOT NULL)THEN
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Marca crédito "||v_credito_infonavit_inv||" no valida para NSS Invadido: "||v_invadido_inv);
      END IF
      
      -- TRACE "12";
      -- ******************************
      -- VALIDACIÓN RESULTADO OPERACIÓN
      -- ******************************
	  -- *****************YA NO SE VALIDA ESTE CAMPO
      --IF NOT(LENGTH(TRIM(v_resultado_operacion_inv)) > 0)THEN
      --   INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
      --   VALUES("Resultado de operación nulo para NSS Invadido: "||v_invadido_inv);
      --ELSE
      --   IF(v_resultado_operacion_inv <> '01' AND 
      --      v_resultado_operacion_inv <> '02')THEN
      --      INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
      --      VALUES("Resultado operación "||v_resultado_operacion_inv||" no valido para NSS Invadido: "||v_invadido_inv);
      --   END IF
      --END IF
      
     -- -- TRACE "13";
     -- -- **************************
     -- -- VALIDACIÓN TRASPASO PREVIO
     -- -- **************************
     -- IF(v_traspaso_previo_inv <> '01' AND 
     --    v_traspaso_previo_inv <> ' '  AND 
     --    v_traspaso_previo_inv IS NOT NULL)THEN
     --    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
     --    VALUES("Traspaso previo "||v_traspaso_previo_inv||" no valido para NSS Invadido: "||v_invadido_inv);
     -- END IF
     -- 
     -- -- TRACE "14";
     -- -- ***********************************
     -- -- VALIDACIÓN IND CAMBIO CLASIFICACIÓN
     -- -- ***********************************
     -- IF(v_ind_cambio_clasificacion_inv <> '1' AND 
     --    v_ind_cambio_clasificacion_inv <> ' '  AND 
     --    v_ind_cambio_clasificacion_inv IS NOT NULL)THEN
     --    INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
     --    VALUES("Indicador de cambio de clasificación "||v_ind_cambio_clasificacion_inv||" no valido para NSS Invadido: "||v_invadido_inv);
     -- END IF
   END FOREACH;
   
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
  -- FOREACH SELECT TRIM(contador_servicio)     ,
  --                TRIM(asociado)              ,
  --                TRIM(tipo_entidad_asociado) ,
  --                TRIM(cve_entidad_asociado)  ,
  --                TRIM(resultado_operacion)   ,
  --                TRIM(diagnostico1)
  --           INTO v_contador_servicio_asc     ,
  --                v_asociado_asc              ,
  --                v_tipo_entidad_asociado_asc ,
  --                v_cve_entidad_asociado_asc  ,
  --                v_resultado_operacion_asc   ,
  --                v_diagnostico1_asc
  --           FROM safre_tmp:tmp_sep_det_03_op27
  --          WHERE 1 = 1
  --          
  --    -- TRACE "2";
  --    -- ***********************
  --    -- VALIDACIÓN NSS ASOCIADO
  --    -- ***********************
  --    -- valida nulo
  --    IF(LENGTH(TRIM(v_asociado_asc)) > 0)THEN
  --       LET v_val_nss = NULL;
  --       LET v_error_sql = 0;
  --       EXECUTE FUNCTION fn_mdt_valida_numerico(v_asociado_asc) INTO v_error_sql, v_msg_sql, v_val_nss;
  --       IF(v_error_sql <> 0) THEN
  --          INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --          VALUES("NSS Asociado no valido: "||v_asociado_asc);
  --       ELSE
  --          IF NOT(v_val_nss > 0) THEN
  --             INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --             VALUES("NSS Asociado no valido: "||v_asociado_asc);
  --          END IF
  --       END IF
  --    ELSE
  --       INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --       VALUES("NSS Asociado nulo con contador de servicio: "||v_contador_servicio_asc);
  --    END IF
  --    
  --    -- TRACE "3";
  --    -- ***********************
  --    -- VALIDACIÓN TIPO ENTIDAD
  --    -- ***********************
  --    IF NOT(LENGTH(TRIM(v_tipo_entidad_asociado_asc)) > 0)THEN
  --       INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --       VALUES("Tipo entidad nula para NSS Asociado: "||v_asociado_asc);
  --    ELSE
  --       LET v_existe_tpo_ent_asc = '0';
  --       SELECT NVL('1','0')
  --         INTO v_existe_tpo_ent_asc
  --         FROM sep_cat_tipo_entidad_asociado
  --        WHERE tipo_entidad_asociado = v_tipo_entidad_asociado_asc;
  --       IF(v_existe_tpo_ent_asc <> '1' OR v_existe_tpo_ent_asc IS NULL)THEN
  --          INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --          VALUES("Tipo entidad "||v_tipo_entidad_asociado_asc||" no valida para NSS Asociado: "||v_asociado_asc);
  --       END IF
  --    END IF
  --    
  --    -- TRACE "4";
  --    -- ************************
  --    -- VALIDACIÓN CLAVE ENTIDAD
  --    -- ************************
  --    IF NOT(LENGTH(TRIM(v_cve_entidad_asociado_asc)) > 0)THEN
  --       INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --       VALUES("Clave entidad nula para NSS Asociado: "||v_asociado_asc);
  --    ELSE
  --       LET v_existe_cve_ent_asc = '0';
  --       SELECT NVL('1','0')
  --         INTO v_existe_cve_ent_asc
  --         FROM cat_afore
  --        WHERE afore_cod = v_cve_entidad_asociado_asc;
  --       IF(v_existe_cve_ent_asc <> '1' OR v_existe_cve_ent_asc IS NULL)THEN
  --          INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --          VALUES("Clave entidad "||v_cve_entidad_asociado_asc||" no valida para NSS Asociado: "||v_asociado_asc);
  --       END IF
  --    END IF
  --    
  --    -- TRACE "5";
  --    -- ******************************
  --    -- VALIDACIÓN RESULTADO OPERACIÓN
  --    -- ******************************
  --    IF NOT(LENGTH(TRIM(v_resultado_operacion_asc)) > 0)THEN
  --       INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --       VALUES("Resultado de operación nulo para NSS Asociado: "||v_asociado_asc);
  --    ELSE
  --       IF(v_resultado_operacion_asc <> '01' AND 
  --          v_resultado_operacion_asc <> '02')THEN
  --          INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --          VALUES("Resultado operación "||v_resultado_operacion_asc||" no valido para NSS Asociado: "||v_asociado_asc);
  --       END IF
  --    END IF
  --    
  --    -- TRACE "6";
  --    -- **********************
  --    -- VALIDACIÓN DIAGNOSTICO
  --    -- **********************
  --    IF(v_diagnostico1_asc <> '000' AND
  --       v_diagnostico1_asc <> '001' AND
  --       v_diagnostico1_asc <> '002' AND
  --       v_diagnostico1_asc <> '003' AND
  --       v_diagnostico1_asc <> '004' AND
  --       v_diagnostico1_asc <> '005' )THEN
  --       INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
  --       VALUES("Diagnostico no valido para NSS Asociado: "||v_asociado_asc);
  --    END IF      
  -- END FOREACH;

END PROCEDURE;


