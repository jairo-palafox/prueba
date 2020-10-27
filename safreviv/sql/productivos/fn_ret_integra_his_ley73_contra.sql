






CREATE FUNCTION "safreviv".fn_ret_integra_his_ley73_contra(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                                p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)


-- detalle de la tabla temporal
--tmp_ret_det_his_ley73
DEFINE tmp_ret_det_l73_id_consecutivo             INTEGER      ;
DEFINE tmp_ret_det_l73_nss                        CHAR(11)     ;
DEFINE tmp_ret_det_l73_monto                      DECIMAL(13,2);
DEFINE tmp_ret_det_l73_num_operacion              SMALLINT     ;
DEFINE tmp_ret_det_l73_monto_total                DECIMAL(13,2);

-- tablas destino
-- ret_his_anexo1
DEFINE ret_his_anexo1_id_solicitud                DECIMAL(9,0) ;
define ret_his_anexo1_folio                       DECIMAL(9,0) ;
DEFINE ret_his_anexo1_estado_solicitud            SMALLINT     ;
DEFINE ret_his_anexo1_cod_rechazo                 SMALLINT     ;
DEFINE ret_his_anexo1_id_consecutivo              INTEGER      ;
DEFINE ret_his_anexo1_nss                         CHAR(11)     ;
DEFINE ret_his_anexo1_monto                       DECIMAL(13,2);
DEFINE ret_his_anexo1_num_operacion               SMALLINT     ;
DEFINE ret_his_anexo1_monto_total                 DECIMAL(13,2);

-- variables de soporte al proceso
DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================

-- para rechazos
DEFINE v_b_rechazo_detalle                     SMALLINT;
 
DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
DEFINE v_existe_en_hist                        SMALLINT;

-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 

-- conteo de rechazos e inserciones
DEFINE v_reg_det_insertados                    SMALLINT; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    SMALLINT; -- total de registros de detalle rechazados
 
-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado             SMALLINT;
DEFINE v_error_det_tpo_registro_invalido         SMALLINT;
DEFINE v_error_monto_invalido                    SMALLINT; -- error 

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
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_l73_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_fc.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   LET v_indice_codigos_rechazo = 0;
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';
   LET tmp_ret_det_l73_nss = NULL;

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
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_monto_invalido                    = 11;
   LET ret_his_anexo1_estado_solicitud           = 0;     
   LET ret_his_anexo1_cod_rechazo                = 0;
   LET tmp_ret_det_l73_id_consecutivo            = "";
   LET tmp_ret_det_l73_nss                       = "";
   LET tmp_ret_det_l73_monto                     = 0;
   LET tmp_ret_det_l73_num_operacion             = "";
   LET tmp_ret_det_l73_monto_total               = 0;
   -- ===================================================
   -- validaciones encabezado contra detalle
   --- se cuentan los registros de la tabla temporal de detalle
   
   -- integracion de detalle
   FOREACH
   SELECT

      id_consecutivo         ,
      nss                    ,
      monto                  ,
      num_operacion            ,
      monto_total                                  
   INTO
      tmp_ret_det_l73_id_consecutivo             ,
      tmp_ret_det_l73_nss                        ,
      tmp_ret_det_l73_monto                      ,
      tmp_ret_det_l73_num_operacion              ,
      tmp_ret_det_l73_monto_total                
   FROM
   safre_tmp:tmp_ret_ley73_contracargo_his_anexo1
   
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;
   

      -- se busca el derechohabiente
      -- se busca que exista en el historico del Anexo 1
      LET v_existe_en_hist = 0;
      SELECT COUNT(*)
      INTO   v_existe_en_hist
      FROM   ret_his_anexo1
      WHERE  nss = tmp_ret_det_l73_nss;

      LET ret_his_anexo1_estado_solicitud = 10;
      LET ret_his_anexo1_cod_rechazo      = 0;
      -- si no se encontro el id_derechohabiente
      IF ( v_existe_en_hist = 0 OR v_existe_en_hist IS NULL ) THEN
         LET ret_his_anexo1_estado_solicitud = 100; -- rechazada
         LET ret_his_anexo1_cod_rechazo      = v_error_det_nss_no_encontrado;
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
      
      -- el monto debe ser mayor a cero
      IF ( tmp_ret_det_l73_monto  IS NULL OR tmp_ret_det_l73_monto <= 0 ) THEN
         -- se marca la bandera de rechazo de detalle
         LET ret_his_anexo1_estado_solicitud = 100; -- rechazada
         LET ret_his_anexo1_cod_rechazo      = v_error_monto_invalido;
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- el monto total debe ser mayor a cero
      IF ( tmp_ret_det_l73_monto_total  IS NULL OR tmp_ret_det_l73_monto_total <= 0 ) THEN
         -- se marca la bandera de rechazo de detalle
         LET ret_his_anexo1_estado_solicitud = 100; -- rechazada
         LET ret_his_anexo1_cod_rechazo      = v_error_monto_invalido;
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
      
      -- se obtiene el numero de solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO v_id_solicitud
      FROM systables
      WHERE tabid = 1;

      -- se transfieren los datos a registro

      LET ret_his_anexo1_id_solicitud                = v_id_solicitud;
      LET ret_his_anexo1_folio                       = p_folio;
      LET ret_his_anexo1_id_consecutivo              = tmp_ret_det_l73_id_consecutivo;
      LET ret_his_anexo1_nss                         = tmp_ret_det_l73_nss;
      LET ret_his_anexo1_monto                       = tmp_ret_det_l73_monto;
      LET ret_his_anexo1_num_operacion               = tmp_ret_det_l73_num_operacion;
      LET ret_his_anexo1_monto_total                 = tmp_ret_det_l73_monto_total;

      
      -- se inserta en tabla destino
      INSERT INTO ret_his_anexo1_contracargos (
         id_solicitud              ,
         folio                     ,
         estado_solicitud          ,
         cod_rechazo               ,
         id_consecutivo            ,
         nss                       ,
         monto                     ,
         num_operacion             ,
         monto_total               
      ) VALUES (
         ret_his_anexo1_id_solicitud              ,
         ret_his_anexo1_folio                     ,
         ret_his_anexo1_estado_solicitud          ,
         ret_his_anexo1_cod_rechazo               ,
         ret_his_anexo1_id_consecutivo            ,
         ret_his_anexo1_nss                       ,
         ret_his_anexo1_monto                     ,
         ret_his_anexo1_num_operacion             ,
         ret_his_anexo1_monto_total             
      );
   
   END FOREACH;
   
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_his_anexo1_contracargos;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_l73_nss;
END FUNCTION;


