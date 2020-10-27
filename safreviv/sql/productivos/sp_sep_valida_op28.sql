






CREATE PROCEDURE "safreviv".sp_sep_valida_op28()

DEFINE v_val_nss           DECIMAL(18);

-- Detalle 02
DEFINE v_contador_servicio_inv    CHAR(10);
DEFINE v_invadido_inv             CHAR(11);
DEFINE v_ind_marca_inv            CHAR(1) ;
DEFINE v_desmarca_pantalla        CHAR(1) ;
DEFINE v_resultado_operacion_inv  CHAR(2) ;
DEFINE v_clasifica_separacion_inv CHAR(1) ;
-- variables de validacion det 02
DEFINE v_existe_clas_sep_inv          CHAR(1);

-- Detalle 03
DEFINE v_contador_servicio_asc    CHAR(10);
DEFINE v_asociado_asc             CHAR(11);
DEFINE v_ind_marca_asc            CHAR(1) ;
DEFINE v_desmarca_pantalla_asc    CHAR(1) ;
DEFINE v_resultado_operacion_asc  CHAR(2) ;

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
      --TRACE "v_error_sql:"||v_error_sql;
      --TRACE "v_error_isam:"||v_error_isam;
      --TRACE "v_msg_sql:"||v_msg_sql;
   END EXCEPTION WITH RESUME;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_valida_op28.trace';
   
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   -- TRACE "1";
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   -- recupera el detalle del archivo cargado para la validacion de valores de campos
   FOREACH SELECT contador_servicio    ,
                  invadido             ,
                  ind_marca            ,
				  desmarca_pantalla    ,
                  resultado_operacion  ,
                  clasifica_separacion
             INTO v_contador_servicio_inv    ,
                  v_invadido_inv             ,
                  v_ind_marca_inv            ,
				  v_desmarca_pantalla        ,
                  v_resultado_operacion_inv  ,
                  v_clasifica_separacion_inv
             FROM safre_tmp:tmp_sep_det02_op28
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
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("NSS Invadido no valido: "||v_invadido_inv);
         ELSE
            IF NOT(v_val_nss > 0) THEN
               INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
               VALUES("NSS Invadido no valido: "||v_invadido_inv);
            END IF
         END IF
      ELSE
         INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
         VALUES("NSS Invadido nulo con contador de servicio: "||v_contador_servicio_inv
         );
      END IF
       
      -- *****************************
      -- VALIDACIÓN MARCAJE DE CUENTA
      -- *****************************
      IF(v_ind_marca_inv <> '0' AND v_ind_marca_inv <> '1' AND v_ind_marca_inv <> ' ' )THEN
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("Marcaje de cuenta "||v_ind_marca_inv||" no valido para NSS Invadido: "||v_invadido_inv);
      END IF
	  
      -- *****************************
      -- VALIDACIÓN DESMARCA VIA PANTALLA
      -- *****************************
      IF(v_desmarca_pantalla <> '1' AND v_desmarca_pantalla <> ' ' )THEN
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("Desmarca Vía Pantalla de cuenta "||v_desmarca_pantalla||" no valido para NSS Invadido: "||v_invadido_inv);
      END IF
            
      -- TRACE "4";
      -- ******************************
      -- VALIDACIÓN RESULTADO OPERACIÓN
      -- ******************************
      IF NOT(LENGTH(TRIM(v_resultado_operacion_inv)) > 0)THEN
         INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
         VALUES("Resultado de operación nulo para NSS Invadido: "||v_invadido_inv);
      ELSE         
         IF(v_resultado_operacion_inv <> '01' AND 
            v_resultado_operacion_inv <> '02')THEN
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("Resultado operación no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
      -- TRACE "5";
      -- *******************************
      -- VALIDACIÓN CLASIFICA SEPARACIÓN
      -- *******************************
      IF NOT(LENGTH(TRIM(v_clasifica_separacion_inv)) > 0)THEN
         INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
         VALUES("Clasificación de separación nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         LET v_existe_clas_sep_inv = '0';
         SELECT NVL('1','0')
           INTO v_existe_clas_sep_inv
           FROM sep_cat_clasificacion
          WHERE clasifica_separacion = v_clasifica_separacion_inv;
         IF(v_existe_clas_sep_inv <> '1')THEN
            INSERT INTO sep_marca_tmp_inconsistencias_val(mensaje)
            VALUES("Clasificación de separación "||v_clasifica_separacion_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF
      
   END FOREACH;
   
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   FOREACH SELECT contador_servicio    ,
                  asociado             ,
                  ind_marca            ,
				  desmarca_pantalla    ,
                  resultado_operacion  
             INTO v_contador_servicio_asc    ,
                  v_asociado_asc             ,
                  v_ind_marca_asc            ,
				  v_desmarca_pantalla_asc    ,
                  v_resultado_operacion_asc  
             FROM safre_tmp:tmp_sep_det03_op28
            WHERE 1 = 1
            
      -- TRACE "B2";
      -- ***********************
      -- VALIDACIÓN NSS invadido
      -- ***********************
      -- valida nulo
      IF(LENGTH(TRIM(v_asociado_asc)) > 0)THEN
         LET v_val_nss = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_mdt_valida_numerico(v_asociado_asc) INTO v_error_sql, v_msg_sql,v_val_nss;
         
         IF(v_error_sql <> 0) THEN
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("NSS Asociado no valido: "||v_asociado_asc);
         ELSE
            IF NOT(v_val_nss >= 0) THEN
               INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
               VALUES("NSS Asociado no valido: "||v_asociado_asc);
            END IF
         END IF
      ELSE
         INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
         VALUES("NSS Asociado nulo con contador de servicio: "||v_contador_servicio_asc
         );
      END IF
      
      -- TRACE "3";
      -- *****************************
      -- VALIDACIÓN MARCAJE DE CUENTA
      -- *****************************
	  IF(v_ind_marca_asc <> 0 AND
            v_ind_marca_asc <> 1 AND
            v_ind_marca_asc <> 2 AND 
			v_ind_marca_asc <> ' ')THEN
            IF (v_asociado_asc <> "00000000000") THEN 
              INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
              VALUES("Marcaje de cuenta "||v_ind_marca_asc||" no valido para NSS Asociado: "||v_asociado_asc);
            END IF
      END IF
	  
      -- *****************************
      -- VALIDACIÓN DESMARCA VIA PANTALLA
      -- *****************************
	  IF(v_desmarca_pantalla_asc <> 1 AND v_desmarca_pantalla_asc <> ' ' )THEN
            IF (v_asociado_asc <> "00000000000") THEN 
              INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
              VALUES("Desmarca vía pantalla de cuenta "||v_desmarca_pantalla_asc||" no valido para NSS Asociado: "||v_asociado_asc);
            END IF
      END IF
            
      -- TRACE "4";
      -- ******************************
      -- VALIDACIÓN RESULTADO OPERACIÓN
      -- ******************************
      IF NOT(LENGTH(TRIM(v_resultado_operacion_asc)) > 0)THEN
         INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
         VALUES("Resultado de operación nulo para NSS Asociado: "||v_asociado_asc);
      ELSE
         IF(v_resultado_operacion_asc <> '01' AND 
            v_resultado_operacion_asc <> '02')THEN
            INSERT INTO sep_tmp_inconsistencias_val_op28(mensaje)
            VALUES("Resultado operación no valido para NSS Asociado: "||v_asociado_asc);
         END IF
      END IF
   END FOREACH;

END PROCEDURE;


