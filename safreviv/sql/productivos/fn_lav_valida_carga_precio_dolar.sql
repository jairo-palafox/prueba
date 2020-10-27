






CREATE FUNCTION "safreviv".fn_lav_valida_carga_precio_dolar(p_usuario_cod CHAR(20), 
                                                 p_pid         INTEGER, 
                                                 p_proceso_cod SMALLINT,
                                                 p_opera_cod   SMALLINT,
                                                 p_folio       DECIMAL (9,0))

   RETURNING INTEGER, 
             INTEGER, 
             CHAR(200),
             INTEGER, 
             INTEGER,
             INTEGER

   DEFINE v_total_registros  INTEGER;
   DEFINE v_total_aceptados  INTEGER;
   DEFINE v_total_rechazados INTEGER;
   DEFINE v_resultado        INTEGER;
   DEFINE v_tipo_registro    CHAR(2);
   DEFINE v_f_valuacion      CHAR(10);
   DEFINE v_fecha_correcta   CHAR(8);
   DEFINE v_valor_dolar      DECIMAL(10,4);
   DEFINE v_tmp_valor_dolar  CHAR(10);
   -- Control de Excepciones
   DEFINE sql_err            INTEGER;
   DEFINE isam_err           INTEGER;
   DEFINE err_txt            CHAR(200);
   DEFINE v_existe_fecha     DATE;
   DEFINE v_resultado_fecha  SMALLINT;
   DEFINE v_fecha_salida     DATE;    
   
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado        = sql_err;
      LET v_total_registros  = 0;
      LET v_total_aceptados  = 0;
      LET v_total_rechazados = 0;
      LET err_txt = "Error :" || v_tipo_registro ||" - " || v_f_valuacion ||" - " || v_tmp_valor_dolar;   

      RETURN v_resultado, 
             isam_err, 
             err_txt,
             v_total_registros,
             v_total_aceptados,
             v_total_rechazados;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/valida_fecha.lav";
   --TRACE ON;
   
   LET v_total_registros  = 0;
   LET v_total_aceptados  = 0;
   LET v_total_rechazados = 0;
   LET v_resultado        = 0;
   LET v_tipo_registro    = "";
   LET v_f_valuacion      = "";
   LET v_valor_dolar      = 0;
   LET v_tmp_valor_dolar  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "Validación exitosa";

   FOREACH
      SELECT *
      INTO   v_tipo_registro,
             v_f_valuacion,
             v_tmp_valor_dolar
      FROM   safre_tmp:tmp_det_valor_dolar

      --Valida tipo de registro
      IF v_tipo_registro <> "01" THEN
         INSERT INTO lav_rch_precio_dolar
         VALUES (
                 seq_lav_rch_precio_dolar.NEXTVAL,
                 p_folio,
                 1,
                 "Error en tipo de registro",
                 v_tipo_registro ||" - " || v_f_valuacion ||" - " || v_valor_dolar
                 );

         LET v_total_rechazados = v_total_rechazados + 1;
      ELSE
         --Convierte de DD/MM/YYYY A YYYYMMDD
         LET v_fecha_correcta = v_f_valuacion[7,10] || v_f_valuacion[4,5] || v_f_valuacion[1,2];
         
         -- Cambia formato de fecha de YYYYMMDD a MMDDYYYY
         EXECUTE PROCEDURE sp_cambia_formato_fecha (p_proceso_cod,v_fecha_correcta)
                 INTO  v_resultado_fecha,
                       v_fecha_salida;
         --Valida fecha correcta
         IF v_resultado_fecha = 0 AND v_fecha_salida IS NOT NULL THEN 
            --Si la estructura de la fecha es correcta, consulta que la fecha no exista
            SELECT f_valor
            INTO   v_existe_fecha
            FROM   lav_tipo_cambio
            WHERE  f_valor = v_fecha_salida;
            
            --Si la fecha YA existe
            IF v_existe_fecha IS NOT NULL THEN
               INSERT INTO lav_rch_precio_dolar
               VALUES (
                       seq_lav_rch_precio_dolar.NEXTVAL,
                       p_folio,
                       2,
                       "Error fecha duplicada",
                       v_tipo_registro ||" - " || v_f_valuacion ||" - " || v_valor_dolar
                       ); 
               
               LET v_total_rechazados = v_total_rechazados + 1;
            ELSE
               LET v_valor_dolar = v_tmp_valor_dolar;
               --Valida precio dólar
               IF v_valor_dolar <= 0 OR
                  v_valor_dolar IS NULL THEN

                  INSERT INTO lav_rch_precio_dolar
                  VALUES (
                          seq_lav_rch_precio_dolar.NEXTVAL,
                          p_folio,
                          3,
                          "Error en precio dólar",
                           v_tipo_registro ||" - " || v_f_valuacion ||" - " || v_valor_dolar
                          ); 
               
                  LET v_total_rechazados = v_total_rechazados + 1;
               ELSE 
                  --Si pasa validación de montos y fechas, inserta aceptados
                  INSERT INTO lav_tipo_cambio
                  VALUES (
                          seq_lav_tipo_cambio.NEXTVAL,
                          v_valor_dolar,
                          v_fecha_salida,
                          TODAY,
                          p_usuario_cod
                          );
                  LET v_total_aceptados = v_total_aceptados + 1;
               END IF   
            END IF          
         ELSE
            INSERT INTO lav_rch_precio_dolar
            VALUES (
                    seq_lav_rch_precio_dolar.NEXTVAL,
                    p_folio,
                    4,
                    "Error en formato fecha",
                    v_tipo_registro ||" - " || v_f_valuacion ||" - " || v_valor_dolar
                    ); 
   
            LET v_total_rechazados = v_total_rechazados + 1;
         END IF
      END IF
      LET v_total_registros = v_total_registros + 1;
   END FOREACH
   
   UPDATE glo_ctr_archivo
   SET    folio     = p_folio, 
          estado    = 2 -- integrado
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 1   -- etapa de carga
   AND    estado      = 1;  -- archivo cargado

   UPDATE statistics FOR TABLE lav_tipo_cambio;
   UPDATE statistics FOR TABLE lav_rch_precio_dolar;

   RETURN v_resultado, 
          isam_err, 
          err_txt,
          v_total_registros,
          v_total_aceptados,
          v_total_rechazados;

END FUNCTION;


