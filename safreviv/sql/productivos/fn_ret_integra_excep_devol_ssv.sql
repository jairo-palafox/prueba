






CREATE FUNCTION "safreviv".fn_ret_integra_excep_devol_ssv(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                                p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)


-- detalle de la tabla temporal


DEFINE tmp_reg_excep_num_delega       CHAR(02);
DEFINE tmp_reg_excep_nss              CHAR(11);
DEFINE tmp_reg_excep_beneficiario     CHAR(60);
DEFINE tmp_reg_excep_importe          CHAR(11);
DEFINE tmp_reg_excep_entidad          CHAR(02);
DEFINE tmp_reg_excep_juicio           CHAR(10);
DEFINE tmp_reg_excep_num_acuerdo      CHAR(10);
DEFINE tmp_reg_excep_desc_juez        CHAR(40);
DEFINE tmp_reg_excep_facultado        CHAR(50);
DEFINE tmp_reg_excep_puesto           CHAR(40);
DEFINE tmp_reg_excep_fch_ejecuta      CHAR(08);
DEFINE tmp_reg_excep_procede_juicio   CHAR(40);
DEFINE tmp_reg_excep_tipo_sol         CHAR(02);
DEFINE tmp_reg_excep_tipo_prod        CHAR(02);
DEFINE tmp_reg_excep_correo_elec      CHAR(40);
DEFINE tmp_reg_excep_cve_rechazo      CHAR(03);
DEFINE tmp_reg_excep_desc_rechazo     CHAR(40);

-- tablas destino
-- ret_his_anexo1
DEFINE registro_excep_id_solicitud     DECIMAL(9,0);
DEFINE registro_excep_nss              CHAR(11);  
DEFINE registro_excep_folio            DECIMAL(9,0);
DEFINE registro_excep_estado_solicitud SMALLINT; 
DEFINE registro_excep_cod_rechazo      SMALLINT;
DEFINE registro_excep_num_delega       CHAR(02);
DEFINE registro_excep_beneficiario     CHAR(60);
DEFINE registro_excep_importe          DECIMAL(14,2);
DEFINE registro_excep_entidad          CHAR(02);
DEFINE registro_excep_juicio           CHAR(10);
DEFINE registro_excep_num_acuerdo      CHAR(10);
DEFINE registro_excep_desc_juez        CHAR(40);
DEFINE registro_excep_facultado        CHAR(50);
DEFINE registro_excep_puesto           CHAR(40);
DEFINE registro_excep_fch_ejecuta      DATE;
DEFINE registro_excep_procede_juicio   CHAR(40);
DEFINE registro_excep_tipo_sol         CHAR(02);
DEFINE registro_excep_tipo_prod        CHAR(02);
DEFINE registro_excep_correo_elec      CHAR(40);
DEFINE registro_excep_desc_rechazo     CHAR(40);


-- variables de soporte al proceso
DEFINE v_id_solicitud                          DECIMAL(9,0);
DEFINE v_importe                               DECIMAL(14,2);
DEFINE v_anio_cont                             CHAR(4);
DEFINE v_mes_cont                              CHAR(2);
DEFINE v_dia_cont                              CHAR(2);
DEFINE v_anio_doc                              CHAR(4);
DEFINE v_mes_doc                               CHAR(2);
DEFINE v_dia_doc                               CHAR(2);
DEFINE v_fecha                                 CHAR(8);
DEFINE v_id_derechohabiente                    DECIMAL(9,0);
DEFINE v_marca_excep_dev_ssv                       SMALLINT;
DEFINE v_i_estado_marca                        SMALLINT;
DEFINE v_ape_paterno                           CHAR(40);
DEFINE v_ape_materno                           CHAR(40);
DEFINE v_nombre                                CHAR(40);
DEFINE v_posicion_signo                        SMALLINT;
DEFINE v_posicion_segundo_signo                SMALLINT;
DEFINE v_f_nacimiento                          DATE;
DEFINE v_sin_paterno                           SMALLINT;
DEFINE v_sin_materno                           SMALLINT;
DEFINE v_tipo_sol_valido                       SMALLINT;
DEFINE v_tipo_prod_valido                      SMALLINT;

-- =============================================================================

-- para rechazos
DEFINE v_b_rechazo_detalle                     SMALLINT;
 
DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
DEFINE v_existe_en_hist                        SMALLINT;

-- arreglo de codigos de rechazo
DEFINE v_indice_codigos_rechazo                INTEGER; 

-- conteo de rechazos e inserciones
DEFINE v_reg_det_insertados                    INTEGER; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    INTEGER; -- total de registros de detalle rechazados
 
-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado             INTEGER;
DEFINE v_error_det_datos_laudo_incompletos       INTEGER;
DEFINE v_error_det_delegacion_no_existe          INTEGER;

DEFINE v_error_det_tpo_registro_invalido         INTEGER;

DEFINE v_error_det_tpo_solicitud                 INTEGER;
DEFINE v_error_det_tpo_producto                  INTEGER;
DEFINE v_error_det_correo_elec                   INTEGER;

DEFINE v_error_monto_invalido                    INTEGER; -- error 
DEFINE v_error_marca                             INTEGER;
DEFINE v_error_datos_insuficientes               INTEGER;
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 
-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_reg_excep_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/safreviv_int/BD/integra_ssv_siaff.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   LET v_indice_codigos_rechazo = 0;
   LET v_importe = 0;

   -- Inicializa las variables temporales
   LET tmp_reg_excep_num_delega       = NULL;  
   LET tmp_reg_excep_nss              = NULL;  
   LET tmp_reg_excep_beneficiario     = NULL;  
   LET tmp_reg_excep_importe          = NULL;  
   LET tmp_reg_excep_entidad          = NULL;  
   LET tmp_reg_excep_juicio           = NULL;  
   LET tmp_reg_excep_num_acuerdo      = NULL;  
   LET tmp_reg_excep_desc_juez        = NULL;  
   LET tmp_reg_excep_facultado        = NULL;  
   LET tmp_reg_excep_puesto           = NULL;  
   LET tmp_reg_excep_fch_ejecuta      = NULL;  
   LET tmp_reg_excep_procede_juicio   = NULL;  
   LET tmp_reg_excep_tipo_sol         = NULL;  
   LET tmp_reg_excep_tipo_prod        = NULL;  
   LET tmp_reg_excep_correo_elec      = NULL;  
   LET tmp_reg_excep_cve_rechazo      = NULL;  
   LET tmp_reg_excep_desc_rechazo     = NULL;  

   LET v_anio_cont = NULL;
   LET v_anio_doc = NULL;
   LET v_mes_cont = NULL;
   LET v_mes_doc = NULL;
   LET v_dia_cont = NULL;
   LET v_dia_doc = NULL;
   LET v_ape_paterno = NULL;
   LET v_ape_materno = NULL;
   LET v_nombre = NULL;
   LET v_f_nacimiento = NULL;
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso    = 0;
   LET v_si_resultado       = 0;
   LET isam_err             = 0;
   LET v_c_msj              = 'El proceso finalizó exitosamente.';
   LET tmp_reg_excep_nss = NULL;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 999;
   LET v_error_det_datos_laudo_incompletos       = 998;
   LET v_error_det_delegacion_no_existe          = 997;
   LET v_error_det_tpo_solicitud                 = 996;
   LET v_error_det_tpo_producto                  = 995;
   LET v_error_det_correo_elec                   = 994;

   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_monto_invalido                    = 11;
   LET v_error_marca                             = 12;
   LET v_error_datos_insuficientes               = 13;
   LET registro_excep_estado_solicitud             = 0;     
   LET registro_excep_cod_rechazo                  = 0;
   LET v_id_derechohabiente                      = 0;
   LET registro_excep_id_solicitud                 = 0;
   LET registro_excep_folio                        = p_folio;
   LET v_marca_excep_dev_ssv                         = 820;
   
   -- ===================================================
   -- validaciones encabezado contra detalle
   --- se cuentan los registros de la tabla temporal de detalle
   -- trace on;  
   -- integracion de detalle
   FOREACH
   SELECT num_delega, nss,       beneficiario, importe,
          entidad,    juicio,    num_acuerdo,  desc_juez,
          facultado,  puesto,    fch_ejecuta,  procede_juicio,
          tipo_sol,   tipo_prod, correo_elec,  cve_rechazo, 
          desc_rechazo
   INTO 
        tmp_reg_excep_num_delega,
        tmp_reg_excep_nss,
        tmp_reg_excep_beneficiario,
        tmp_reg_excep_importe,
        tmp_reg_excep_entidad,
        tmp_reg_excep_juicio,
        tmp_reg_excep_num_acuerdo,
        tmp_reg_excep_desc_juez,
        tmp_reg_excep_facultado,
        tmp_reg_excep_puesto,
        tmp_reg_excep_fch_ejecuta,
        tmp_reg_excep_procede_juicio,
        tmp_reg_excep_tipo_sol,
        tmp_reg_excep_tipo_prod,
        tmp_reg_excep_correo_elec,
        tmp_reg_excep_cve_rechazo,
        tmp_reg_excep_desc_rechazo
   FROM
   safre_tmp:tmp_ret_excep_devol_ssv
   
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;


      -- se busca el derechohabiente
      -- se busca que exista en el maestro de derechohabientes
      LET v_existe_en_hist = 0;
      SELECT COUNT(*)
      INTO   v_existe_en_hist
      FROM   afi_derechohabiente 
      WHERE  nss = tmp_reg_excep_nss;

      LET registro_excep_estado_solicitud = 10;
      LET registro_excep_cod_rechazo = 0;
      -- si no se encontro el id_derechohabiente
      IF ( v_existe_en_hist = 0 OR v_existe_en_hist IS NULL ) THEN
         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_nss_no_encontrado;
         LET v_id_derechohabiente = 0;
         LET v_b_rechazo_detalle = 1;
      END IF 
      IF (((tmp_reg_excep_tipo_prod = "03" OR
         tmp_reg_excep_tipo_prod = "06" OR
         tmp_reg_excep_tipo_prod = "07" OR 
         tmp_reg_excep_tipo_prod = "09") AND 
         (tmp_reg_excep_entidad IS NULL OR
          tmp_reg_excep_juicio IS NULL OR
          tmp_reg_excep_num_acuerdo IS NULL OR
          tmp_reg_excep_desc_juez IS NULL OR
          tmp_reg_excep_facultado IS NULL OR
          tmp_reg_excep_puesto IS NULL OR
          tmp_reg_excep_fch_ejecuta IS NULL OR
          tmp_reg_excep_procede_juicio IS NULL))) AND
          v_b_rechazo_detalle = 0 THEN 

         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_datos_laudo_incompletos;
         LET v_b_rechazo_detalle = 1;
      END IF
      IF (tmp_reg_excep_num_delega IS NULL OR    
         tmp_reg_excep_num_delega = "  ") AND
         v_b_rechazo_detalle = 0 THEN
         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_delegacion_no_existe;
         LET v_b_rechazo_detalle = 1;
      END IF
      SELECT COUNT(*)
      INTO   v_tipo_sol_valido
      FROM   ret_cat_tpo_sol_dap
      WHERE  tpo_solicitud = tmp_reg_excep_tipo_sol;
      IF v_tipo_sol_valido = 0 AND v_b_rechazo_detalle = 0 THEN 
         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_tpo_solicitud;
         LET v_b_rechazo_detalle = 1;
      END IF 
      SELECT COUNT(*)
      INTO   v_tipo_prod_valido
      FROM   ret_cat_tpo_prod_dap
      WHERE  tpo_producto = tmp_reg_excep_tipo_prod;
      IF v_tipo_prod_valido = 0 AND v_b_rechazo_detalle = 0 THEN 
         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_tpo_producto;
         LET v_b_rechazo_detalle = 1;
      END IF
      IF (tmp_reg_excep_correo_elec IS NULL OR TRIM(tmp_reg_excep_correo_elec) = "") AND
          v_b_rechazo_detalle = 0 THEN
         LET registro_excep_estado_solicitud = 100; -- rechazada
         LET registro_excep_cod_rechazo = v_error_det_correo_elec;
         LET v_b_rechazo_detalle = 1;
      END IF 

      IF TRIM(tmp_reg_excep_importe) = "" THEN 
         LET v_importe = 0;
      ELSE 
         LET v_importe = tmp_reg_excep_importe;
      END IF

    
      -- se obtiene el numero de solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO v_id_solicitud
      FROM systables
      WHERE tabid = 1;

      -- se transfieren los datos a registro

      LET registro_excep_id_solicitud                = v_id_solicitud;
      LET registro_excep_folio                       = p_folio;
      LET registro_excep_nss                         = tmp_reg_excep_nss;

      LET registro_excep_num_delega       = tmp_reg_excep_num_delega;  
      LET registro_excep_beneficiario     = tmp_reg_excep_beneficiario;  
      LET registro_excep_importe          = v_importe;  
      LET registro_excep_entidad          = tmp_reg_excep_entidad;  
      LET registro_excep_juicio           = tmp_reg_excep_juicio;  
      LET registro_excep_num_acuerdo      = tmp_reg_excep_num_acuerdo;  
      LET registro_excep_desc_juez        = tmp_reg_excep_desc_juez;  
      LET registro_excep_facultado        = tmp_reg_excep_facultado;  
      LET registro_excep_puesto           = tmp_reg_excep_puesto;
      IF tmp_reg_excep_fch_ejecuta IS NOT NULL THEN 
         LET registro_excep_fch_ejecuta             = mdy(tmp_reg_excep_fch_ejecuta[5,6],tmp_reg_excep_fch_ejecuta[7,8],tmp_reg_excep_fch_ejecuta[1,4]);
      ELSE 
         LET registro_excep_fch_ejecuta = NULL;
      END IF

      LET registro_excep_procede_juicio   = tmp_reg_excep_procede_juicio;  
      LET registro_excep_tipo_sol         = tmp_reg_excep_tipo_sol;  
      LET registro_excep_tipo_prod        = tmp_reg_excep_tipo_prod;  
      LET registro_excep_correo_elec      = tmp_reg_excep_correo_elec;  
      LET v_i_estado_marca = 1;
      IF registro_excep_estado_solicitud = 10 THEN
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente 
         WHERE  nss = tmp_reg_excep_nss;
         EXECUTE FUNCTION fn_marca_cuenta(
                 v_id_derechohabiente
                ,v_marca_excep_dev_ssv -- marca de excepciones de la devolución del SSV
                ,v_id_solicitud    -- seq_ret_excep_devol_ssv.CURRVAL
                ,p_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;

         IF v_i_estado_marca <> 0 THEN 
            LET registro_excep_estado_solicitud = 100; -- rechazada
            LET registro_excep_cod_rechazo = v_error_marca;
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF 
      END IF       
      IF registro_excep_cod_rechazo = 0 THEN 
         LET registro_excep_desc_rechazo     = NULL;
      ELSE 
         SELECT descripcion
         INTO registro_excep_desc_rechazo
         FROM  ret_cat_rechazo_dap
         WHERE cod_rechazo = registro_excep_cod_rechazo;
      END IF 
      IF TRIM(tmp_reg_excep_beneficiario) IS NULL OR
         TRIM(tmp_reg_excep_beneficiario) = "" THEN 
         SELECT TRIM(ap_paterno_af) || " " || 
                TRIM(ap_materno_af) || " " ||
                TRIM(nombre_af)
         INTO   tmp_reg_excep_beneficiario
         FROM   afi_derechohabiente
         WHERE  nss = tmp_reg_excep_nss;
         LET registro_excep_beneficiario     = tmp_reg_excep_beneficiario;  
      END IF 
      -- se inserta en tabla destino
      INSERT INTO ret_excep_devol_ssv (
          id_solicitud, nss, folio, estado_solicitud, cod_rechazo,
          num_delega, beneficiario, importe, entidad, juicio, num_acuerdo, 
          desc_juez, facultado, puesto, fch_ejecuta, procede_juicio, 
          tipo_sol, tipo_prod, correo_elec, desc_rechazo, f_solicitud
      ) VALUES (
          registro_excep_id_solicitud, registro_excep_nss,
          registro_excep_folio,        registro_excep_estado_solicitud, 
          registro_excep_cod_rechazo,  registro_excep_num_delega, 
          registro_excep_beneficiario, registro_excep_importe, 
          registro_excep_entidad,      registro_excep_juicio, 
          registro_excep_num_acuerdo,  registro_excep_desc_juez, 
          registro_excep_facultado,    registro_excep_puesto, 
          registro_excep_fch_ejecuta,  registro_excep_procede_juicio,
          registro_excep_tipo_sol,     registro_excep_tipo_prod, 
          registro_excep_correo_elec,  registro_excep_desc_rechazo, TODAY);
      LET v_reg_det_insertados = v_reg_det_insertados + 1;
   END FOREACH;
   --trace off;
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_excep_devol_ssv;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_reg_excep_nss;
END FUNCTION;


