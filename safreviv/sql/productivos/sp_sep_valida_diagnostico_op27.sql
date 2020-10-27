






CREATE PROCEDURE "safreviv".sp_sep_valida_diagnostico_op27()

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
DEFINE v_diag_confronta_inv           CHAR(2);
DEFINE v_clasifica_separacion_inv     CHAR(1);
DEFINE v_credito_infonavit_inv        CHAR(1);
DEFINE v_resultado_operacion_inv      CHAR(2);
DEFINE v_traspaso_previo_inv          CHAR(2);
DEFINE v_ind_cambio_clasificacion_inv CHAR(1);
-- variables de validacion det 02
DEFINE v_existe_cve_ent_admon         CHAR(1);
DEFINE v_existe_ent_ncmto_inv         CHAR(1);
DEFINE v_existe_diag_conf_inv         CHAR(1);
DEFINE v_existe_clas_sep_inv          CHAR(1);
DEFINE v_fec_reg_inv                  DATE;

-- Detalle 03
DEFINE v_contador_servicio_asc     CHAR(10);
DEFINE v_contador_registro_asc     CHAR(10);
DEFINE v_asociado_asc              CHAR(11);
DEFINE v_tipo_entidad_asociado_asc CHAR(2);
DEFINE v_cve_entidad_asociado_asc  CHAR(3);
DEFINE v_resultado_operacion_asc   CHAR(2);
DEFINE v_diagnostico1_asc          CHAR(3);
-- variables de validacion det 03
DEFINE v_existe_tpo_ent_asc        CHAR(1);
DEFINE v_existe_cve_ent_asc        CHAR(1);

-- Control de errores
DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);
DEFINE v_diag_conf  CHAR(02);
DEFINE v_result  CHAR(02);
DEFINE v_clasifica_separacion  CHAR(01);
   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql


      -- error de conversion de fecha
      IF(v_error_sql = -1204 OR
         v_error_sql = -1205 OR
         v_error_sql = -1206 OR
         v_error_sql = -1218)THEN

      END IF
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_valida_diagnostico_op27.trace';

   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";

   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   -- recupera el detalle del archivo cargado para la validacion de valores de campos
   FOREACH SELECT TRIM(invadido),
                  TRIM(tipo_entidad_admon),
                  TRIM(cve_entidad_admon),
                  TRIM(entidad_nacimiento),
                  TRIM(sexo),
                  TRIM(rfc),
                  TRIM(f_registro),
                  TRIM(f_marca_infosar),
                  TRIM(diag_confronta),
                  TRIM(clasifica_separacion),
                  credito_infonavit,
                  TRIM(resultado_operacion),
                  TRIM(traspaso_previo),
                  TRIM(ind_cambio_clasificacion)
             INTO v_invadido_inv                ,
                  v_tipo_entidad_admon_inv      ,
                  v_cve_entidad_admon_inv       ,
                  v_entidad_nacimiento_inv      ,
                  v_sexo_inv                    ,
                  v_rfc_inv                     ,
                  v_f_registro_inv              ,
                  v_f_marca_infosar_inv         ,
                  v_diag_confronta_inv          ,
                  v_clasifica_separacion_inv    ,
                  v_credito_infonavit_inv       ,
                  v_resultado_operacion_inv     ,
                  v_traspaso_previo_inv         ,
                  v_ind_cambio_clasificacion_inv
             FROM safre_tmp:tmp_sep_det02_op27_diag
            WHERE 1 = 1


      -- ***********************
      -- VALIDACIÓN NSS invadido
      -- ***********************
      -- valida nulo
      IF(LENGTH(TRIM(v_invadido_inv)) > 0)THEN
         LET v_val_nss = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_mdt_valida_numerico(v_invadido_inv) INTO v_error_sql, v_msg_sql,v_val_nss;

         IF(v_error_sql <> 0) THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("NSS Invadido no valido: "||v_invadido_inv);
         ELSE
            IF NOT(v_val_nss > 0) THEN
               INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
               VALUES("NSS Invadido no valido: "||v_invadido_inv);
            END IF
         END IF
      ELSE
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("NSS Invadido nulo con RFC: "||v_rfc_inv);
      END IF


      -- *****************************
      -- VALIDACIÓN TIPO ENTIDAD ADMON
      -- *****************************
      IF NOT(LENGTH(TRIM(v_tipo_entidad_admon_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Tipo entidad administradora nulo para NSS Invadido: "||v_invadido_inv);
      ELSE
         IF(v_tipo_entidad_admon_inv <> '01')THEN

            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Tipo entidad administradora "||v_tipo_entidad_admon_inv||" no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- ******************************
      -- VALIDACIÓN CLAVE ENTIDAD ADMON
      -- ******************************
      IF NOT(LENGTH(TRIM(v_cve_entidad_admon_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Clave entidad administradora nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         LET v_existe_cve_ent_admon = '0';
         SELECT NVL('1','0')
           INTO v_existe_cve_ent_admon
           FROM cat_afore
          WHERE afore_cod = v_cve_entidad_admon_inv;
         IF(v_existe_cve_ent_admon <> '1' OR v_existe_cve_ent_admon IS NULL)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Clave entidad administradora "||v_cve_entidad_admon_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- ***********************************
      -- VALIDACIÓN CLAVE ENTIDAD NACIMIENTO
      -- ***********************************
      IF NOT(LENGTH(TRIM(v_entidad_nacimiento_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Entidad de nacimiento nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         LET v_existe_ent_ncmto_inv = '0';
         SELECT NVL('1','0')
           INTO v_existe_ent_ncmto_inv
           FROM cat_entidad_federativa
          WHERE entidad_federativa = v_entidad_nacimiento_inv;
         IF(v_existe_ent_ncmto_inv <> '1' OR v_existe_ent_ncmto_inv IS NULL)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Entidad de nacimiento "||v_entidad_nacimiento_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- ***************
      -- VALIDACIÓN SEXO
      -- ***************
      IF NOT(LENGTH(TRIM(v_sexo_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Género nulo para NSS Invadido: "||v_invadido_inv);
      ELSE
         IF(v_sexo_inv <> 1 AND
            v_sexo_inv <> 2 )THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Género no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- *************************
      -- VALIDACIÓN FECHA REGISTRO
      -- *************************
      -- valida que se haya recuperado la fecha
      IF NOT(LENGTH(TRIM(v_f_registro_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha registro nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         -- valida que la fecha sea valida
         LET v_fec_reg_inv = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_sep_valida_fecha(v_f_registro_inv) INTO v_error_sql, v_msg_sql, v_fec_reg_inv;

         IF(v_error_sql <> 0)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha registro "||v_f_registro_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- **********************
      -- VALIDACIÓN FECHA MARCA
      -- **********************
      IF NOT(LENGTH(TRIM(v_f_marca_infosar_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Fecha marca nula para NSS Invadido: "||v_invadido_inv);
      ELSE
         -- valida que la fecha sea valida
         LET v_fec_reg_inv = NULL;
         LET v_error_sql = 0;
         EXECUTE FUNCTION fn_sep_valida_fecha(v_f_marca_infosar_inv) INTO v_error_sql, v_msg_sql, v_fec_reg_inv;
         IF(v_error_sql <> 0)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Fecha marca "||v_f_marca_infosar_inv||" no valida para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- *************************
      -- VALIDACIÓN DIAG CONFRONTA
      -- *************************
      IF NOT(LENGTH(TRIM(v_diag_confronta_inv)) > 0)THEN
       IF v_resultado_operacion_inv <> "02" THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Diagnostico confronta nulo para NSS Invadido: "||v_invadido_inv);
       ELSE 
         LET v_diag_confronta_inv = "02";
         LET v_existe_diag_conf_inv = 1;
       END IF 
      ELSE
         LET v_existe_diag_conf_inv = '0';
         SELECT NVL('1','0')
           INTO v_existe_diag_conf_inv
           FROM sep_cat_diag_confronta
          WHERE diag_confronta = v_diag_confronta_inv;
         IF(v_existe_diag_conf_inv <> '1' OR v_existe_diag_conf_inv IS NULL)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Diagnostico confronta "||v_diag_confronta_inv||" no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- *******************************
      -- VALIDACIÓN CLASIFICA SEPARACIÓN
      -- *******************************
      -- sólo se valida la clasificación si el diagonóstico es 01 aceptado
      IF( v_diag_confronta_inv = '01' )THEN
         IF NOT(LENGTH(TRIM(v_clasifica_separacion_inv)) > 0)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Clasificación de separación nula para NSS Invadido: "||v_invadido_inv);
         ELSE
            LET v_existe_clas_sep_inv = '0';
            SELECT NVL('1','0')
              INTO v_existe_clas_sep_inv
              FROM sep_cat_clasificacion
             WHERE clasifica_separacion = v_clasifica_separacion_inv;
            IF(v_existe_clas_sep_inv <> '1' OR v_existe_clas_sep_inv IS NULL)THEN
               INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
               VALUES("Clasificación de separación "||v_clasifica_separacion_inv||" no valida para NSS Invadido: "||v_invadido_inv);
            END IF
         END IF
      END IF


      -- ************************
      -- VALIDACIÓN MARCA CRÉDITO
      -- ************************
      IF(v_credito_infonavit_inv <> '1' AND
         v_credito_infonavit_inv <> ' ' AND
         v_credito_infonavit_inv IS NOT NULL)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Marca crédito "||v_credito_infonavit_inv||" no valida para NSS Invadido: "||v_invadido_inv);
      END IF


      -- ******************************
      -- VALIDACIÓN RESULTADO OPERACIÓN
      -- ******************************
      IF NOT(LENGTH(TRIM(v_resultado_operacion_inv)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Resultado de operación nulo para NSS Invadido: "||v_invadido_inv);
      ELSE
         IF(v_resultado_operacion_inv <> '01' AND
            v_resultado_operacion_inv <> '02')THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Resultado operación "||v_resultado_operacion_inv||" no valido para NSS Invadido: "||v_invadido_inv);
         END IF
      END IF


      -- **************************
      -- VALIDACIÓN TRASPASO PREVIO
      -- **************************
      IF(v_traspaso_previo_inv <> '01' AND
         v_traspaso_previo_inv <> ' '  AND
         v_traspaso_previo_inv IS NOT NULL)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Traspaso previo "||v_traspaso_previo_inv||" no valido para NSS Invadido: "||v_invadido_inv);
      END IF


      -- ***********************************
      -- VALIDACIÓN IND CAMBIO CLASIFICACIÓN
      -- ***********************************
      IF(v_ind_cambio_clasificacion_inv <> '1' AND
         v_ind_cambio_clasificacion_inv <> ' '  AND
         v_ind_cambio_clasificacion_inv IS NOT NULL)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Indicador de cambio de clasificación "||v_ind_cambio_clasificacion_inv||" no valido para NSS Invadido: "||v_invadido_inv);
      END IF
   END FOREACH;

   ---------------------------------
   -- VALIDACIÓN TABLA DETALLE 03 --
   ---------------------------------
   FOREACH SELECT TRIM(contador_servicio)     ,
                  TRIM(contador_registro)     ,
                  TRIM(asociado)              ,
                  TRIM(tipo_entidad_asociado) ,
                  TRIM(cve_entidad_asociado)  ,
                  TRIM(resultado_operacion)   ,
                  TRIM(diagnostico1)
             INTO v_contador_servicio_asc     ,
                  v_contador_registro_asc     ,
                  v_asociado_asc              ,
                  v_tipo_entidad_asociado_asc ,
                  v_cve_entidad_asociado_asc  ,
                  v_resultado_operacion_asc   ,
                  v_diagnostico1_asc
             FROM safre_tmp:tmp_sep_det_03_op27_diag
            WHERE 1 = 1

      -- Solo se validan los campos si el nss es diferente de ceros
      -- si nss es ceros, indica que el registro viene rechazado, por lo
      -- tanto no se pueden validar los otros campos
      IF(v_asociado_asc <> '00000000000')THEN

         -- ***********************
         -- VALIDACIÓN NSS ASOCIADO
         -- ***********************
         -- valida nulo
         IF(LENGTH(TRIM(v_asociado_asc)) > 0)THEN
            LET v_val_nss = NULL;
            LET v_error_sql = 0;
            EXECUTE FUNCTION fn_mdt_valida_numerico(v_asociado_asc) INTO v_error_sql, v_msg_sql, v_val_nss;
            IF(v_error_sql <> 0) THEN
               INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
               VALUES("NSS Asociado no valido: "||v_asociado_asc);
            ELSE
               IF NOT(v_val_nss > 0) THEN
                  INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
                  VALUES("NSS Asociado no valido: "||v_asociado_asc);
               END IF
            END IF
         ELSE
          LET v_diag_conf = "";
          LET v_clasifica_separacion = "";
          LET v_result = "";

          SELECT diag_confronta ,
                 clasifica_separacion,
                 resultado_operacion
          INTO   v_diag_conf ,
                 v_clasifica_separacion,
                 v_result
          FROM safre_tmp:tmp_sep_det02_op27_diag
          WHERE contador_registro = v_contador_registro_asc - 1;


           IF (v_diag_conf = "01"           AND
               v_clasifica_separacion <> "A" AND
               v_clasifica_separacion <> "B" AND
               v_result <> "02" ) THEN -- nulo con diagnostico <> 02
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("NSS Asociado nulo con contador de servicio: "||v_contador_servicio_asc);
           END IF
         END IF


         -- ***********************
         -- VALIDACIÓN TIPO ENTIDAD
         -- ***********************
{
         IF NOT(LENGTH(TRIM(v_tipo_entidad_asociado_asc)) > 0)THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Tipo entidad nula para NSS Asociado: "||v_asociado_asc);
         ELSE
            LET v_existe_tpo_ent_asc = '0';
            SELECT NVL('1','0')
              INTO v_existe_tpo_ent_asc
              FROM sep_cat_tipo_entidad_asociado
             WHERE tipo_entidad_asociado = v_tipo_entidad_asociado_asc;
            IF(v_existe_tpo_ent_asc <> '1' OR v_existe_tpo_ent_asc IS NULL)THEN
               INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
               VALUES("Tipo entidad "||v_tipo_entidad_asociado_asc||" no valida para NSS Asociado: "||v_asociado_asc);
            END IF
         END IF


         -- ************************
         -- VALIDACIÓN CLAVE ENTIDAD
         -- ************************
         -- se valida clave solo si tipo entidad es diferente de 00
         -- se valida si es nulo para mostrar mensaje
         IF(v_tipo_entidad_asociado_asc <> '00' OR LENGTH(TRIM(v_tipo_entidad_asociado_asc)) = 0)THEN
            IF NOT(LENGTH(TRIM(v_cve_entidad_asociado_asc)) > 0)THEN
               INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
               VALUES("Clave entidad nula para NSS Asociado: "||v_asociado_asc);
            ELSE
               LET v_existe_cve_ent_asc = '0';
               SELECT NVL('1','0')
                 INTO v_existe_cve_ent_asc
                 FROM cat_afore
                WHERE afore_cod = v_cve_entidad_asociado_asc;
               IF(v_existe_cve_ent_asc <> '1' OR v_existe_cve_ent_asc IS NULL)THEN


                  INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
                  VALUES("Clave entidad "||v_cve_entidad_asociado_asc||" no valida para NSS Asociado: "||v_asociado_asc);
               END IF
            END IF
         END IF
}

         -- **********************
         -- VALIDACIÓN DIAGNOSTICO
         -- **********************
       IF v_resultado_operacion_asc = "01" THEN
         IF(v_diagnostico1_asc <> '000' AND
            v_diagnostico1_asc <> '   ' AND
            v_diagnostico1_asc <> '001' AND
            v_diagnostico1_asc <> '002' AND
            v_diagnostico1_asc <> '003' AND
            v_diagnostico1_asc <> '004' AND
            v_diagnostico1_asc <> '005' )THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Diagnostico no valido para NSS Asociado: "||v_asociado_asc);
         END IF
      END IF
     END IF

      -- ******************************
      -- VALIDACIÓN RESULTADO OPERACIÓN
      -- ******************************
      IF NOT(LENGTH(TRIM(v_resultado_operacion_asc)) > 0)THEN
         INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
         VALUES("Resultado de operación nulo para NSS Asociado: "||v_asociado_asc);
      ELSE
         IF(v_resultado_operacion_asc <> '01' AND
            v_resultado_operacion_asc <> '02')THEN
            INSERT INTO sep_diag_tmp_inconsistencias_val(mensaje)
            VALUES("Resultado operación "||v_resultado_operacion_asc||" no valido para NSS Asociado: "||v_asociado_asc);
         END IF
      END IF
   END FOREACH;

END PROCEDURE;


