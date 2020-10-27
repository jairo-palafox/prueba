






CREATE FUNCTION "safreviv".fn_deo_pre_integra_op98(p_usuario_cod CHAR(20), p_pid DECIMAL(9,0), p_folio DECIMAL(10)) 
       RETURNING SMALLINT, INTEGER, VARCHAR(255)
 
 DEFINE v_datos_validos_tpo_registro            CHAR(2);
 DEFINE v_datos_validos_id_servicio             CHAR(2);
 DEFINE v_datos_validos_f_operacion             CHAR(8);
 DEFINE v_dte_f_valido_f_operacion              DATE;
 --
 DEFINE v_datos_top_validos_tpo_registro        CHAR(2);
 DEFINE v_datos_top_validos_id_servicio         CHAR(2);
 DEFINE v_datos_top_validos_id_operacion        CHAR(2);
 DEFINE v_datos_top_validos_tpo_entidad_destino CHAR(2);
 DEFINE v_datos_top_validos_cve_entidad_destino CHAR(3);
 DEFINE v_datos_top_validos_f_operacion         CHAR(8);
 --
 DEFINE v_sumario_importe_total                 DECIMAL(22,2);
 DEFINE v_sumario_aiv_97                        DECIMAL(18,6);
 DEFINE v_sumario_aiv_92                        DECIMAL(18,6);
 DEFINE v_detalle_importe_total                 DECIMAL(22,2);
 DEFINE v_detalle_aiv_97                        DECIMAL(18,6);
 DEFINE v_detalle_aiv_92                        DECIMAL(18,6);
 
  
 DEFINE cod_rechazo_1                           SMALLINT;
 DEFINE cod_rechazo_2                           SMALLINT;
 DEFINE cod_rechazo_3                           SMALLINT;
 --
 DEFINE v_nombre_archivo CHAR(18);
 DEFINE v_f_operacion_procesar DATE;
 DEFINE v_f_carga_afore DATE;
 DEFINE v_resultado_operacion SMALLINT;
 DEFINE v_cod_rechazo_1 SMALLINT;
 DEFINE v_cod_rechazo_2 SMALLINT;
 DEFINE v_cod_rechazo_3 SMALLINT;

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 DEFINE v_c_msj                         VARCHAR(255);
 DEFINE v_si_resultado                  SMALLINT;
 
 -- se establece el valor de retorno de las excepciones
 ON EXCEPTION SET sql_err, isam_err, err_txt
    LET v_si_resultado = sql_err;
    
    RETURN v_si_resultado, isam_err, err_txt;
 END EXCEPTION


 -- se asume que el proceso termino correctamente
 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = 'El proceso finalizó correctamente';

   LET cod_rechazo_1 = 0;
   LET cod_rechazo_2 = 0;
   LET cod_rechazo_3 = 0;

   LET v_datos_validos_tpo_registro = NULL;
   LET v_datos_validos_id_servicio  = NULL;
   LET v_datos_validos_f_operacion  = NULL;
 -- Obtener datos de validacion de:
 -- Encabezado global de datos de devoluciones. Tabla temporal.

   SELECT tpo_registro, id_servicio, f_operacion
     INTO  v_datos_validos_tpo_registro
          ,v_datos_validos_id_servicio 
          ,v_datos_validos_f_operacion 
     FROM safre_tmp:tmp_enc_gral_op98;

   -- 
   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(v_datos_validos_f_operacion)
      INTO v_dte_f_valido_f_operacion;
      
   -- <Datos obligatoros en registro.>
   -- <    ERROR : '10'              >
   IF(v_datos_validos_tpo_registro <> '10' OR v_datos_validos_tpo_registro IS NULL)THEN
      -- ERROR de encabezado.
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 = 200;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 200;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 200;
            END IF
         END IF
      END IF
   END IF
   -- <Datos obligatoros en registro.>
   -- <    ERROR : '04'              >
   IF(v_datos_validos_id_servicio <> '04' OR v_datos_validos_id_servicio IS NULL)THEN
      -- ERROR de encabezado.
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 = 201;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 201;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 201;
            END IF
         END IF
      END IF
   END IF
   --  01-07-1997
   -- <Datos obligatoros en registro.>
   -- < ERROR : Fecha operación              >
   IF(v_dte_f_valido_f_operacion<= '07-01-1997' OR v_dte_f_valido_f_operacion IS NULL)THEN
      -- ERROR de encabezado.
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 = 202;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 202;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 202;
            END IF
         END IF
      END IF
   END IF
   
    -- Obtener datos de validacion de:
   -- Segundo encabezado enc_tipo_op98. Tabla temporal.
   FOREACH SELECT DISTINCT tpo_registro, id_servicio, id_operacion, tpo_entidad_destino
          , cve_entidad_destino, f_operacion
     INTO v_datos_top_validos_tpo_registro
          , v_datos_top_validos_id_servicio
          , v_datos_top_validos_id_operacion
          , v_datos_top_validos_tpo_entidad_destino
          , v_datos_top_validos_cve_entidad_destino
          , v_datos_top_validos_f_operacion
     FROM safre_tmp:tmp_enc_top_op98

      -- 
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_datos_top_validos_f_operacion)
         INTO v_dte_f_valido_f_operacion;
      
      -- <Datos obligatoros en registro.>
      -- <    ERROR : '02'              >
      IF(v_datos_top_validos_tpo_registro <> '02' 
      	  OR v_datos_top_validos_tpo_registro IS NULL)THEN
         -- ERROR de sub encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 = 100;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 100;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 100;
               END IF
            END IF
         END IF
      END IF
      -- <Datos obligatoros en registro.>
      -- <    ERROR : '04'              >
      IF(v_datos_top_validos_id_servicio <> '04' 
      	  OR v_datos_top_validos_id_servicio IS NULL)THEN
         -- ERROR de encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 = 201;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 201;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 201;
               END IF
            END IF
         END IF
      END IF
      -- <Datos obligatoros en registro.>
      -- <    ERROR : '98'              >
      IF(v_datos_top_validos_id_operacion <> '98' 
      	  OR v_datos_top_validos_id_operacion IS NULL)THEN
         -- ERROR de encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 = 102;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 102;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 102;
               END IF
            END IF
         END IF
      END IF
         -- <Datos obligatoros en registro.>
      -- <    ERROR : '04'              >
      IF(v_datos_top_validos_tpo_entidad_destino <> '04' 
      	  OR v_datos_top_validos_tpo_entidad_destino IS NULL)THEN
         -- ERROR de encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 =103;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 103;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 103;
               END IF
            END IF
         END IF
      END IF
      -- <Datos obligatoros en registro.>
      -- <    ERROR : '002'             >
      IF(v_datos_top_validos_cve_entidad_destino <> '002' 
      	  OR v_datos_top_validos_cve_entidad_destino IS NULL)THEN
         -- ERROR de encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 = 104;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 104;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 104;
               END IF
            END IF
         END IF
      END IF
      -- <Datos obligatoros en registro.>
      -- <    ERROR : Fecha operación de encabezados debe ser igual             >
      IF(v_datos_top_validos_f_operacion <>
      	  v_datos_validos_f_operacion
      	  OR v_datos_top_validos_f_operacion IS NULL)THEN
         -- ERROR de encabezado.
         IF(cod_rechazo_1 = 0)THEN
            LET cod_rechazo_1 = 105;
         ELSE
            IF(cod_rechazo_2 = 0)THEN
               LET cod_rechazo_2 = 105;
            ELSE
               IF(cod_rechazo_3 = 0)THEN
                  LET cod_rechazo_3 = 105;
               END IF
            END IF
         END IF
      END IF
   END FOREACH;
   
   -- Obtiene datos de importe total de sumario y el total de aivs 97 y 92
   SELECT 
       CASE (SUM(importe_total)) 
         WHEN 0 THEN 0 ELSE SUM(importe_total) /100
        END
        , CASE (SUM(aivs_97))
             WHEN 0 THEN 0
             ELSE SUM(aivs_97)/1000000
          END 
        , CASE (SUM(aivs_92))
             WHEN 0 THEN 0
             ELSE SUM(aivs_92)/1000000
          END 
     INTO v_sumario_importe_total, v_sumario_aiv_97, v_sumario_aiv_92
     FROM safre_tmp:tmp_sum_afore_op98;

   --SELECT 
   --    CASE (importe_total) 
   --      WHEN 0 THEN 0 ELSE importe_total  /100
   --     END
   --     , CASE (aivs_97)
   --          WHEN 0 THEN 0
   --          ELSE aivs_97/1000000
   --       END 
   --     , CASE (aivs_92)
   --          WHEN 0 THEN 0
   --          ELSE aivs_92/1000000
   --       END 
   --  INTO v_sumario_importe_total, v_sumario_aiv_97, v_sumario_aiv_92
   --  FROM safre_tmp:tmp_sum_afore_op98;
     
   -- Obtiene los datos de importe total y aivs del registro detalle para comparar
   SELECT SUM(acc_97), SUM(acc_92), SUM(imp_viv)
     INTO v_detalle_aiv_97, v_detalle_aiv_92, v_detalle_importe_total
     FROM TABLE(MULTISET(
   SELECT
       CASE (num_aplic_interes_97)
        WHEN 0 THEN 0 ELSE  num_aplic_interes_97 /1000000
       END as acc_97,
      CASE (num_aplic_interes_92)
        WHEN 0 THEN 0 ELSE num_aplic_interes_92 /1000000
       END as acc_92,
      (CASE (importe_viv97)
        WHEN 0 THEN 0 ELSE importe_viv97        /100
       END +
      CASE (importe_viv92)
        WHEN 0 THEN 0 ELSE importe_viv92        /100
       END) as imp_viv
     FROM safre_tmp:tmp_detalle_op98
     ));
   
   -- Compara importes totales de sumario vs archivo detalle.
   IF(v_detalle_importe_total <> v_sumario_importe_total OR 
      (v_detalle_importe_total IS NULL OR v_sumario_importe_total IS NULL))THEN
      
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 = 110;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 110;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 110;
            END IF
         END IF
      END IF
   END IF
   
   -- Compara el total de ivs de viv 97 del detalle VS el sumario
   IF(v_detalle_aiv_97 <> v_sumario_aiv_97 OR 
      (v_detalle_aiv_97 IS NULL OR v_sumario_aiv_97 IS NULL))THEN
      
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 =111;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 111;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 111;
            END IF
         END IF
      END IF
   END IF
   
   -- Compara el total de ivs de viv 92 del detalle VS el sumario
   IF(v_detalle_aiv_92 <> v_sumario_aiv_92 OR 
      (v_detalle_aiv_92 IS NULL OR v_sumario_aiv_92 IS NULL))THEN
      
      IF(cod_rechazo_1 = 0)THEN
         LET cod_rechazo_1 = 112;
      ELSE
         IF(cod_rechazo_2 = 0)THEN
            LET cod_rechazo_2 = 112;
         ELSE
            IF(cod_rechazo_3 = 0)THEN
               LET cod_rechazo_3 = 112;
            END IF
         END IF
      END IF
   END IF


   -- Según sean los errores ocurridos de la validación de encabezado y sumarios.
   IF ( cod_rechazo_1 <> 0 OR cod_rechazo_2 <> 0 OR cod_rechazo_3 <> 0 ) THEN
      
      -- Recupera nombre de archivo
      SELECT nombre_archivo
        INTO v_nombre_archivo
        FROM safre_viv:glo_ctr_archivo
       WHERE proceso_cod = 801
         AND opera_cod   = 1
         AND estado = 1;
      
      -- Recupeda errores regitrados
      LET v_f_operacion_procesar = TODAY;
      LET v_f_carga_afore        = TODAY;
      LET v_resultado_operacion  = 111;
      LET v_cod_rechazo_1 = cod_rechazo_1;
      LET v_cod_rechazo_2 = cod_rechazo_2;
      LET v_cod_rechazo_3 = cod_rechazo_3;

      -- Inserta archivo con errores
      INSERT INTO safre_viv:deo_cza_op98_rch
         (folio, nombre_archivo, f_operacion_procesar, f_carga_afore, 
          resultado_operacion, cod_rechazo_1, cod_rechazo_2,  cod_rechazo_3)
       VALUES (p_folio, v_nombre_archivo, v_f_operacion_procesar
          ,v_f_carga_afore, v_resultado_operacion, v_cod_rechazo_1
          , v_cod_rechazo_2, v_cod_rechazo_3
          );
      
      -- Actualiza el folio para el archivo.
      UPDATE safre_viv:glo_ctr_archivo
         SET folio = p_folio
         ,estado   = 2 -- integrado con errores
       WHERE proceso_cod = 801
         AND opera_cod   = 1
         AND estado = 1;
     
      -- se indica que hubo rechazos en la validacion del encabezado general 
      LET v_si_resultado = 1;
      LET isam_err = 0;
      LET v_c_msj = 'Se encontraron rechazos en el encabezado general: Cód. Rechazo 1: ' || cod_rechazo_1 || "; Cód. Rechazo 2: " || cod_rechazo_2 || "; Cód. Rechazo 3: " || cod_rechazo_3 || ". Verificar consulta de rechazos.";

   END IF
 
 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION -- fn_deo_pre_integra_op98
;


