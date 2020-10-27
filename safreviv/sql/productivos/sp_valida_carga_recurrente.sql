






CREATE PROCEDURE "safreviv".sp_valida_carga_recurrente()

DEFINE v_nss               CHAR(11);
DEFINE v_num_credito       CHAR(10);
DEFINE v_edo_credito       CHAR(3);
DEFINE v_fec_ini_mandato   CHAR(8);
DEFINE v_fec_fin_mandato   CHAR(8);
DEFINE v_tpo_descuento     CHAR(1);
DEFINE v_val_descuento     CHAR(8);
DEFINE v_cve_mandato       CHAR(18);
DEFINE v_error_validacion  SMALLINT;
DEFINE v_error_val_x_reg   SMALLINT;
DEFINE v_val_nss           INTEGER;
DEFINE v_fec_ini_val       DATE;
DEFINE v_fec_fin_val       DATE;
DEFINE v_val_descuento_val DECIMAL(12,2);
DEFINE v_cve_mandato_val   DECIMAL(18,0);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);


   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      -- TRACE "0";
      -- error de conversion de caracter a numerico para el nss
      IF(v_error_sql = -1213)THEN
         -- TRACE "0.1";
         IF(v_val_nss IS NULL)THEN
            -- TRACE "0.2";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("NSS no valido: "||v_nss);
         END IF
         IF(v_cve_mandato_val IS NULL)THEN
            -- TRACE "0.2";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Clave de mandato no valida para NSS: "||v_nss);
         END IF
         IF(v_val_descuento_val IS NULL)THEN
            -- TRACE "0.2";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Valor de descuento no valido para NSS: "||v_nss);
         END IF
      END IF
      -- TRACE "00";
      -- error de conversion de fecha
      IF(v_error_sql = -1204 OR 
         v_error_sql = -1205 OR 
         v_error_sql = -1206 OR
         v_error_sql = -1218)THEN
         -- TRACE "00.1";
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha no valida para NSS: "||v_nss);
      
      END IF
   END EXCEPTION WITH RESUME;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_valida_carga_recurrente.-- TRACE';
   
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   -- TRACE "1";
   
   -- recupera el detalle del archivo cargado para la validacion de valores de campos
   FOREACH SELECT TRIM(nss),
                  num_credito,
                  TRIM(edo_credito),
                  TRIM(fec_ini_mandato),
                  TRIM(fec_fin_mandato),
                  tpo_descuento,
                  val_descuento,
                  TRIM(cve_mandato)
             INTO v_nss,
                  v_num_credito,
                  v_edo_credito,
                  v_fec_ini_mandato,
                  v_fec_fin_mandato,
                  v_tpo_descuento,
                  v_val_descuento,
                  v_cve_mandato
             FROM safre_tmp:tmp_acr_transf_30
            WHERE 1 = 1
            
      -- TRACE "2";
      -- **************
      -- VALIDACIÓN NSS
      -- **************
      -- valida nulo
      IF(LENGTH(TRIM(v_nss)) > 0)THEN
         -- TRACE "2.1";
         LET v_val_nss = NULL;
         LET v_cve_mandato_val = NULL;
         LET v_val_descuento_val = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_valida_numerico(v_nss) INTO v_error_sql, v_msg_sql,v_val_nss;
         --LET v_val_nss = v_nss;
         IF(v_error_sql <> 0) THEN
            -- TRACE "2.2";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("NSS no valido: "||v_nss);
         ELSE
            IF NOT(v_val_nss > 0) THEN
               INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
               VALUES("NSS no valido: "||v_nss);
            END IF
         END IF
      ELSE
         -- TRACE "2.3";
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("NSS nulo con crédito: "||v_num_credito);
      END IF
      -- TRACE "3";
      -- *************************
      -- VALIDACIÓN ESTADO CRÉDITO
      -- *************************
      IF NOT(LENGTH(TRIM(v_edo_credito)) > 0)THEN
         -- TRACE "3.1";
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Estado de crédito nulo para NSS: "||v_nss);
      ELSE
         -- TRACE "3.2";
         IF(v_edo_credito <> '015' AND 
            v_edo_credito <> '016' AND 
            v_edo_credito <> '017' AND 
            v_edo_credito <> '018' )THEN
            -- TRACE "3.3";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Estado de crédito "||v_edo_credito||" no valido para NSS: "||v_nss);
         
         END IF
      END IF
      -- TRACE "4";
      -- *************************
      -- VALIDACIÓN FECHAS
      -- *************************
      -- valida que se haya recuperado la fecha
      IF NOT(LENGTH(TRIM(v_fec_ini_mandato)) > 0)THEN
         -- TRACE "4.1";
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha inicio de mandato nula para nss: "||v_nss);
      ELSE
         -- TRACE "4.2";
         -- valida que la fecha sea valida
         LET v_fec_ini_val = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_valida_fecha(v_fec_ini_mandato) INTO v_error_sql, v_msg_sql, v_fec_ini_val;
         
         IF(v_error_sql <> 0)THEN
            -- TRACE "4.3";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha inicio de mandato "||v_fec_ini_mandato||" no valida para nss: "||v_nss);
         END IF
      END IF
      
      -- TRACE "5";
      -- valida que la fecha sea valida
      LET v_fec_fin_val = NULL;
      EXECUTE FUNCTION fn_valida_fecha(v_fec_fin_mandato) INTO v_error_sql, v_msg_sql, v_fec_fin_val;
      
      IF(v_error_sql <> 0)THEN
         -- TRACE "5.1";
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha culmina mandato no valida para nss: "||v_nss);
      ELSE
         -- TRACE "5.2";
         -- valida que la fecha inicial sea menor que la final
         IF(v_fec_ini_val > v_fec_fin_val)THEN
            -- TRACE "5.3";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha inicio mandato mayor que fecha culmina para nss: "||v_nss);
         END IF
      END IF
      -- TRACE "6";
      -- ****************************
      -- VALIDACIÓN TIPO DE DESCUENTO
      -- ****************************
      IF NOT(LENGTH(TRIM(v_tpo_descuento)) > 0)THEN
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Tipo de descuento nulo para nss: "||v_nss);
      ELSE
         IF(v_tpo_descuento <> "4")THEN
            -- TRACE "6.1";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Tipo de descuento "||v_tpo_descuento||" no valido para nss: "||v_nss);
         END IF
      END IF
      -- TRACE "7";
      -- *****************************
      -- VALIDACIÓN VALOR DE DESCUENTO
      -- *****************************
      IF NOT(LENGTH(TRIM(v_val_descuento)) > 0)THEN
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Valor de descuento nulo para nss: "||v_nss);
      ELSE
         LET v_val_descuento_val = NULL;
         LET v_cve_mandato_val = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_valida_decimal(v_val_descuento) INTO v_error_sql, v_msg_sql,v_val_descuento_val;
         IF(v_error_sql <> 0)THEN
            -- TRACE "7.1";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Valor de descuento "||v_val_descuento||" no valido para nss: "||v_nss);
         END IF
      END IF
      --LET v_val_descuento_val = v_val_descuento;
      
      -- TRACE "8";
      -- ***************************
      -- VALIDACIÓN CLAVE DE MANDATO
      -- ***************************
      LET v_cve_mandato_val = NULL;
      IF NOT(LENGTH(TRIM(v_cve_mandato)) > 0)THEN
         INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
         VALUES("Clave de mandato nula para nss: "||v_nss);
      ELSE
         IF(LENGTH(TRIM(v_cve_mandato)) <> 18 )THEN
            -- TRACE "8.1";
            INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
            VALUES("Clave de mandato "||v_cve_mandato||" no valida para nss: "||v_nss);
         ELSE
            -- TRACE "8.2";
            LET v_error_sql = 0;
            EXECUTE FUNCTION fn_valida_numerico(v_cve_mandato) INTO v_error_sql, v_msg_sql,v_cve_mandato_val;
            --LET v_cve_mandato_val = v_cve_mandato;
            IF(v_error_sql <> 0)THEN
               -- TRACE "8.3";
               INSERT INTO mdt_tmp_inconsistencias_val(mensaje)
               VALUES("Clave de mandato "||v_cve_mandato||" no valida para nss: "||v_nss);
            END IF
         END IF
      END IF
      -- TRACE "9";
   END FOREACH;
   -- TRACE "10";

END PROCEDURE;


