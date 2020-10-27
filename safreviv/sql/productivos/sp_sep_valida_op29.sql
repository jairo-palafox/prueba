






CREATE PROCEDURE "safreviv".sp_sep_valida_op29()

DEFINE v_val_nss           DECIMAL(18);

-- Detalle 02
DEFINE v_contador_servicio_inv    CHAR(10);
DEFINE v_invadido_inv             CHAR(11);

-- Detalle 03
DEFINE v_contador_servicio_asc    CHAR(10);
DEFINE v_asociado_asc             CHAR(11);

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
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_valida_op29.-- TRACE';
   
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   -- TRACE "1";
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   -- recupera el detalle del archivo cargado para la validacion de valores de campos
   FOREACH SELECT contador_servicio    ,
                  invadido             
             INTO v_contador_servicio_inv    ,
                  v_invadido_inv             
             FROM safre_tmp:tmp_sep_det_02_op29
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
            INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
            VALUES("NSS Invadido no valido: "||v_invadido_inv);
         ELSE
            IF NOT(v_val_nss > 0) THEN
               INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
               VALUES("NSS Invadido no valido: "||v_invadido_inv);
            END IF
         END IF
      ELSE
         INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
         VALUES("NSS Invadido nulo con contador de servicio: "||v_contador_servicio_inv
         );
      END IF
      
   END FOREACH;
   
   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   FOREACH SELECT contador_servicio    ,
                  asociado             
             INTO v_contador_servicio_asc    ,
                  v_asociado_asc             
             FROM safre_tmp:tmp_sep_det_03_op29
            WHERE 1 = 1
            
      -- TRACE "2";
      -- ***********************
      -- VALIDACIÓN NSS invadido
      -- ***********************
      -- valida nulo
      IF(LENGTH(TRIM(v_asociado_asc)) > 0)THEN
         LET v_val_nss = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_mdt_valida_numerico(v_asociado_asc) INTO v_error_sql, v_msg_sql,v_val_nss;
         
         IF(v_error_sql <> 0) THEN
            INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
            VALUES("NSS Asociado no valido: "||v_asociado_asc);
         ELSE
            IF NOT(v_val_nss > 0) THEN
             IF (v_asociado_asc <> "00000000000") THEN
               INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
               VALUES("NSS Asociado no valido: "||v_asociado_asc);
             END IF
            END IF
         END IF
      ELSE
         INSERT INTO sep_tmp_inconsistencias_val_op29(mensaje)
         VALUES("NSS Asociado nulo con contador de servicio: "||v_contador_servicio_asc
         );
      END IF
   END FOREACH;

END PROCEDURE;


