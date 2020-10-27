






CREATE FUNCTION "safreviv".fn_pre_integra_credito(p_usuario_cod CHAR(20), 
                                             p_pid DECIMAL(9,0),
                                             p_folio_lote_solicitud DECIMAL(9,0)) 
                RETURNING INTEGER, CHAR(200)
-- Variables utilizadas para el registro inicial de devoluciones
-- de aportaciones pagadas en exceso solo INFONAVIT
DEFINE v_ini_tpo_registro    DECIMAL(1,0);
DEFINE v_ini_f_registro      CHAR(8);
DEFINE v_date_ini_f_registro DATE;
DEFINE v_ini_filler        CHAR(94);
-- Variables utilizadas para el encabezado de transacción de devoluciones
-- de aportaciones pagadas en exceso solo INFONAVIT
DEFINE v_cza_tpo_registro   DECIMAL(1,0);
DEFINE v_cza_transacciones  DECIMAL(4,0);
DEFINE v_cza_f_archivo      CHAR(8);
DEFINE v_date_cza_f_archivo DATE;
DEFINE v_cza_filler         CHAR(90);
-- Variables utilizadas para el sumario de transacción de devoluciones
-- de aportaciones pagadas en exceso solo INFONAVIT
DEFINE v_sum_tpo_registro               DECIMAL(1,0);
DEFINE v_sum_transacciones              DECIMAL(4,0);
DEFINE v_sum_tot_registro               DECIMAL(10,0);
DEFINE v_sum_imp_tot_aporta_amortizar_1 DECIMAL(14,2);
DEFINE v_sum_imp_tot_amortizacion_1     DECIMAL(14,2);
DEFINE v_sum_imp_tot_aporta_amortizar_2 DECIMAL(14,2);
DEFINE v_sum_imp_tot_amortizacion_2     DECIMAL(14,2);
DEFINE v_sum_filler                     CHAR(40);
-- Variables utilizadas para el registro final de devoluciones
-- de aportaciones pagadas en exceso solo INFONAVIT
DEFINE v_fin_tpo_registro               DECIMAL(1,0);
DEFINE v_fin_tot_reg_contenidos_arch    DECIMAL(10,0);
DEFINE v_fin_filler_1                   DECIMAL(4,0);
DEFINE v_fin_imp_tot_aporta_amortizar_1 DECIMAL(14,2);
DEFINE v_fin_imp_tot_amortizacion_1     DECIMAL(14,2);
DEFINE v_fin_imp_tot_aporta_amortizar_2 DECIMAL(14,2);
DEFINE v_fin_imp_tot_amortizacion_2     DECIMAL(14,2);
DEFINE v_fin_filler_2                   CHAR(40);
--
DEFINE v_tmp_sum_imp_tot_aporta_amortizar_1 DECIMAL(9,2);
DEFINE v_tmp_sum_imp_tot_amortizacion_1     DECIMAL(9,2);
DEFINE v_tmp_fin_imp_tot_aporta_amortizar_1 DECIMAL(9,2);
DEFINE v_tmp_fin_imp_tot_amortizacion_1     DECIMAL(9,2);


-- Variable que determina si es que existe un error
DEFINE v_si_resultado SMALLINT;
DEFINE v_c_error CHAR(200);
-- Variable que almacena le valor maximo id_dpe_referencia
DEFINE v_d_id_referencia DECIMAL(9,0);

---------- REGISTRO INICIAL Y ENCABEZADO INFONAVIT -------------
-- Obtener datos de validacion de la tabla temporal
-- Variables que almacenan el codigo de error

   LET v_d_id_referencia = 0;
   LET v_si_resultado = 0;
   
-- Variables que almacenan informacion para su validacion
-- Registro inicial
   LET v_ini_tpo_registro = NULL;
   LET v_ini_f_registro   = NULL;
   LET v_ini_filler       = NULL;
-- Variables que almacenan informacion para su validacion
-- Encabezado
   LET v_cza_tpo_registro  = NULL;
   LET v_cza_transacciones = NULL;
   LET v_cza_f_archivo     = NULL;
   LET v_cza_filler        = NULL;
-- Variables que almacenan informacion para su validacion
-- Sumario
   LET v_sum_tpo_registro               = NULL;
   LET v_sum_transacciones              = NULL;
   LET v_sum_tot_registro               = NULL;
   LET v_sum_imp_tot_aporta_amortizar_1 = NULL;
   LET v_sum_imp_tot_amortizacion_1     = NULL;
   LET v_sum_imp_tot_aporta_amortizar_2 = NULL;
   LET v_sum_imp_tot_amortizacion_2     = NULL;
   LET v_sum_filler                     = NULL;
-- Variables que almacenan informacion para su validacion
-- Registro final
   LET v_fin_tpo_registro               = NULL;
   LET v_fin_tot_reg_contenidos_arch    = NULL;
   LET v_fin_filler_1                   = NULL;
   LET v_fin_imp_tot_aporta_amortizar_1 = NULL;
   LET v_fin_imp_tot_amortizacion_1     = NULL;
   LET v_fin_imp_tot_aporta_amortizar_2 = NULL;
   LET v_fin_imp_tot_amortizacion_2     = NULL;
   LET v_fin_filler_2                   = NULL;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.pre.integra_infonavit.txt";

-- Selecciona la informacion insertada al cargar el archivo para su validacion
-- Registro inicial,encabezado,sumario,registro final de devolucion de aportaciones
-- pagadas en exceso solo INFONAVIT

   --SELECT MAX(id_dpe_referencia) INTO v_d_id_referencia
   --  FROM dpe_soloinfonavit_arch;
   --  -- Verifica que el id_referencia no venga nullo
   --  -- en caso de ser contrario, se asigna el valor que trae
   --     IF(v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
   --        LET v_d_id_referencia = 0;
   --     END IF
   
   --LET v_d_id_referencia = v_d_id_referencia + 1;
   SELECT seq_dpe_soloinfonavit_arch.NEXTVAL
    INTO  v_d_id_referencia
	  FROM  SYSTABLES
	 WHERE  tabname = "dpe_soloinfonavit_arch";
   
   --trace "Selecciona la informacion de tmp_ini_recauda_hipoteca";
   
   LET v_c_error = "Selecciona la informacion de tmp_ini_recauda_hipoteca";
   
   SELECT tpo_registro,
          fec_registro,
          filler
     INTO v_ini_tpo_registro,
          v_ini_f_registro,
          v_ini_filler
     FROM safre_tmp:tmp_ini_recauda_hipoteca;
   
   -- Inician validaciones de datos obligatorios registro inicial
   -- Valida el tipo de registro
   --trace "Valida tipo de registro para el registro inicial 0";
   LET v_c_error = "Valida tipo de registro para el registro inicial 0";
   IF (v_ini_tpo_registro <> 0 OR v_ini_tpo_registro IS NULL) THEN
      -- ERROR de registro inicial.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,0,v_d_id_referencia,"No",70,
         "Tipo registro -"||v_ini_tpo_registro);
      LET v_si_resultado = 1;   
   END IF
   
   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(v_ini_f_registro)
      INTO v_date_ini_f_registro;
   
   -- Valida la fecha de registro
   --trace "Valida fecha de registro";
   LET v_c_error = "Valida fecha de registro";
   IF(v_date_ini_f_registro > TODAY OR v_date_ini_f_registro IS NULL)THEN
      -- ERROR de registro inicial.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,0,v_d_id_referencia,"No",71,
         "Fecha registro -"||v_date_ini_f_registro);
      LET v_si_resultado = 1;   
   END IF
   
   --trace "Selecciona la informacion de tmp_cza_recauda_hipoteca";
   LET v_c_error = "Selecciona la informacion de tmp_cza_recauda_hipoteca";
   
   SELECT tpo_registro,
          transacciones,
          fec_generacion_archivo,
          filler
     INTO v_cza_tpo_registro,
          v_cza_transacciones,
          v_cza_f_archivo,
          v_cza_filler
     FROM safre_tmp:tmp_cza_recauda_hipoteca;
      
   -- Inician validaciones de datos obligatorios encabezado
   -- Valida el tipo de registro
   --trace "Selecciona la informacion de tmp_cza_recauda_hipoteca";
   LET v_c_error = "Valida tipo de registro para el encabezado 1";
   IF (v_cza_tpo_registro <> 1 OR v_cza_tpo_registro IS NULL) THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,1,v_d_id_referencia,"No",72,
         "Tipo registro -"||v_cza_tpo_registro);
      LET v_si_resultado = 1;   
   END IF
   
   -- Valida transacciones
   --trace "Valida tipo de transacciones";
   LET v_c_error = "Valida tipo de transacciones";
   IF (v_cza_transacciones IS NULL) THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,1,v_d_id_referencia,"No",73,
         "Transacciones -"||v_cza_transacciones);
      LET v_si_resultado = 1;   
   END IF
   
   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(v_cza_f_archivo)
      INTO v_date_cza_f_archivo;
   
   -- Valida la fecha de registro
   --trace "Valida fecha de generacion de archivo";
   LET v_c_error = "Valida fecha de generacion de archivo";
   IF(v_date_cza_f_archivo > TODAY OR v_date_cza_f_archivo IS NULL)THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
         p_folio_lote_solicitud,1,v_d_id_referencia,"No",74,
         "Fecha generacion archivo -"||v_date_cza_f_archivo);
      LET v_si_resultado = 1;   
   END IF
   
   --trace "Selecciona la informacion de tmp_sum_recauda_hipoteca";
   LET v_c_error = "Selecciona la informacion de tmp_sum_recauda_hipoteca";
   
   SELECT tpo_registro,
          transacciones,
          tot_registros_transaccion,
          imp_tot_aporta_amortizar_1,
          imp_tot_amortizacion_1,
          imp_tot_aporta_amortizar_2,
          imp_tot_amortizacion_2,
          filler
     INTO v_sum_tpo_registro,
          v_sum_transacciones,
          v_sum_tot_registro,
          v_sum_imp_tot_aporta_amortizar_1,
          v_sum_imp_tot_amortizacion_1,
          v_sum_imp_tot_aporta_amortizar_2,
          v_sum_imp_tot_amortizacion_2,
          v_sum_filler
     FROM safre_tmp:tmp_sum_recauda_hipoteca;
     
     -- Inician validaciones de datos obligatorios sumario
     -- Valida el tipo de registro
     --trace "Valida tipo de registro para el sumario 3";
     LET v_c_error = "Valida tipo de registro para el sumario 3";
     IF (v_sum_tpo_registro <> 3 OR v_sum_tpo_registro IS NULL) THEN
        -- ERROR de sumario.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,3,v_d_id_referencia,"No",75,
           "Tipo registro -"||v_sum_tpo_registro);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida transacciones
     --trace "Valida tipo de transacciones";
     LET v_c_error = "Valida tipo de transacciones";
     IF (v_sum_transacciones IS NULL) THEN
        -- ERROR de sumario.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,3,v_d_id_referencia,"No",76,
           "Transacciones -"||v_sum_transacciones);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida total de registro de la transacción
     LET v_c_error = "Valida total de registro de la transaccion";
     IF (v_sum_tot_registro IS NULL) THEN
        -- ERROR de sumario.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,3,v_d_id_referencia,"No",77,
           "Total de registro transaccion -"||v_sum_tot_registro);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida importe total aportación p/Amortizar
     --trace "Valida importe total aportación p/Amortizar";
     LET v_c_error = "Valida importe total aportación p/Amortizar";
     IF (v_sum_imp_tot_aporta_amortizar_1 IS NULL) THEN
        -- ERROR de sumario.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,3,v_d_id_referencia,"No",78,
           "Total importe aporta/amortizar -"||v_sum_imp_tot_aporta_amortizar_1);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida importe total amortización
     LET v_c_error = "Valida importe total Amortizacion";
     IF (v_sum_imp_tot_amortizacion_1 IS NULL) THEN
        -- ERROR de sumario.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,3,v_d_id_referencia,"No",79,
           "Total importe Amortizacion -"||v_sum_imp_tot_amortizacion_1);
        LET v_si_resultado = 1;   
     END IF
     
     --trace "Selecciona la informacion de tmp_fin_recauda_hipoteca";
     LET v_c_error = "Selecciona la informacion de tmp_fin_recauda_hipoteca";
   
   SELECT tpo_registro,
          tot_reg_contenidos_arch,
          filler_1,
          imp_tot_aporta_amortizar_1,
          imp_tot_amortizacion_1,
          imp_tot_aporta_amortizar_2,
          imp_tot_amortizacion_2,
          filler_2
     INTO v_fin_tpo_registro,
          v_fin_tot_reg_contenidos_arch,
          v_fin_filler_1,
          v_fin_imp_tot_aporta_amortizar_1,
          v_fin_imp_tot_amortizacion_1,
          v_fin_imp_tot_aporta_amortizar_2,
          v_fin_imp_tot_amortizacion_2,
          v_fin_filler_2
     FROM safre_tmp:tmp_fin_recauda_hipoteca;
     
     -- Inician validaciones de datos obligatorios registro final
     -- Valida el tipo de registro
     --trace "Valida tipo de registro para el registro final 4";
     LET v_c_error = "Valida tipo de registro para el registro final 4";
     IF (v_fin_tpo_registro <> 4 OR v_fin_tpo_registro IS NULL) THEN
        -- ERROR de registro final.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,4,v_d_id_referencia,"No",80,
           "Tipo registro -"||v_fin_tpo_registro);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida el total de registros del archivo
     --trace "Valida total de registros del archivo";
     LET v_c_error = "Valida total de registros del archivo";
     IF (v_fin_tot_reg_contenidos_arch IS NULL) THEN
        -- ERROR de registro final.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,4,v_d_id_referencia,"No",81,
           "Total de registro archivo -"||v_fin_tot_reg_contenidos_arch);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida el importe total aportacion p/Amortizacion
     --trace "Valida el importe total aportacion p/Amortizacion";
     LET v_c_error = "Valida el importe total aportacion p/Amortizacion";
     IF (v_fin_imp_tot_aporta_amortizar_1 IS NULL) THEN
        -- ERROR de registro final.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,4,v_d_id_referencia,"No",82,
           "Importe total aportacion p/Amortizacion -"||v_fin_imp_tot_aporta_amortizar_1);
        LET v_si_resultado = 1;   
     END IF
     
     -- Valida el importe total Amortizacion
     --trace "Valida el importe total Amortizacion";
     LET v_c_error = "Valida el importe total Amortizacion";
     IF (v_fin_imp_tot_amortizacion_1 IS NULL) THEN
        -- ERROR de registro final.
        EXECUTE PROCEDURE sp_dpe_inserta_rechazo(
           p_folio_lote_solicitud,4,v_d_id_referencia,"No",83,
           "Importe total Amortizacion -"||v_fin_imp_tot_amortizacion_1);
        LET v_si_resultado = 1;   
     END IF
    -- Inserta en la tabla dpe_soloinfonavit_arch exista o no rechazos
    
    LET v_sum_imp_tot_aporta_amortizar_1 = v_sum_imp_tot_aporta_amortizar_1/100;
    LET v_sum_imp_tot_amortizacion_1 = v_sum_imp_tot_amortizacion_1/100;
    LET v_fin_imp_tot_aporta_amortizar_1 = v_fin_imp_tot_aporta_amortizar_1/100;
    LET v_fin_imp_tot_amortizacion_1 = v_fin_imp_tot_amortizacion_1/100;
    
    LET v_tmp_sum_imp_tot_aporta_amortizar_1 = v_sum_imp_tot_aporta_amortizar_1;
    LET v_tmp_sum_imp_tot_amortizacion_1 = v_sum_imp_tot_amortizacion_1;
    LET v_tmp_fin_imp_tot_aporta_amortizar_1 = v_fin_imp_tot_aporta_amortizar_1;
    LET v_tmp_fin_imp_tot_amortizacion_1 = v_fin_imp_tot_amortizacion_1;
      
    --trace "Inserta en la tabla dpe_soloinfonavit_arch ";
    LET v_c_error = "Inserta en la tabla dpe_soloinfonavit_arch";
    
    INSERT INTO dpe_soloinfonavit_arch(id_dpe_referencia,
                                                 folio,
                                                 fecha_registro,
                                                 transacciones,
                                                 fecha_archivo,
                                                 tot_reg_transaccion,
                                                 tot_aportacion_reg,
                                                 tot_amortizacion_reg,
                                                 tot_aportacion_sol,
                                                 tot_amortizacion_sol,
                                                 tot_reg_archivo)
       VALUES (v_d_id_referencia,
               p_folio_lote_solicitud,
               v_date_ini_f_registro,
               v_cza_transacciones,
               v_date_cza_f_archivo,
               v_sum_tot_registro,
               v_tmp_sum_imp_tot_aporta_amortizar_1,
               v_tmp_sum_imp_tot_amortizacion_1,
               v_tmp_fin_imp_tot_aporta_amortizar_1,
               v_tmp_fin_imp_tot_amortizacion_1,
               v_fin_tot_reg_contenidos_arch);
                 
   update statistics for table dpe_soloinfonavit_arch;
   
   --trace "v_si_resultado: "||v_si_resultado;
   
   IF v_si_resultado = 1 THEN
      -- Existio error en el archivo
      --trace "La pre-integración contiene errores";
      LET v_c_error = "La pre-integración contiene errores";
      RETURN 1, v_c_error;
   ELSE
      -- Sin error el archivo
      --trace "La pre-integración se cargo correctamente";
      LET v_c_error = "La pre-integración se cargo correctamente";
      RETURN 0, v_c_error;
   END IF
   
END FUNCTION -- fn_pre_integra_INFONAVIT
;


