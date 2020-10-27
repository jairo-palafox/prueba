






CREATE FUNCTION "safreviv".fn_ret_integra_conting_solo_infonavit(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11), INTEGER, INTEGER, INTEGER


-- campos de la tabla de encabezado de retiros contingente solo infonavit
-- tmp_ret_cza_solo_infonavit
DEFINE tmp_ret_cza_sinf_tpo_registro         char(2);
DEFINE tmp_ret_cza_sinf_id_servicio          char(2);
DEFINE tmp_ret_cza_sinf_tpo_operacion        char(2);
DEFINE tmp_ret_cza_sinf_f_operacion          date   ;
DEFINE tmp_ret_cza_sinf_resultado_operacion  char(2);
DEFINE tmp_ret_cza_sinf_motivo_rech_1        char(3);
DEFINE tmp_ret_cza_sinf_motivo_rech_2        char(3);


-- campos de la tabla de detalle de retiros contingente infonavit
-- tmp_ret_det_solo_infonavit
DEFINE tmp_ret_det_sinf_tpo_registro         char(2)      ;
DEFINE tmp_ret_det_sinf_id_servicio          char(2)      ;
DEFINE tmp_ret_det_sinf_id_operacion         char(2)      ;
DEFINE tmp_ret_det_sinf_nss                  char(11)     ;
DEFINE tmp_ret_det_sinf_f_solicitud          date         ;
DEFINE tmp_ret_det_sinf_f_liquidacion        date         ;
DEFINE tmp_ret_det_sinf_tot_aivs_viv97       decimal(18,0);
DEFINE tmp_ret_det_sinf_tot_imp_viv97        decimal(20,0);
DEFINE tmp_ret_det_sinf_cta_clabe            char(18)     ;
DEFINE tmp_ret_det_sinf_cve_banco            decimal(6,0) ;
DEFINE tmp_ret_det_sinf_ent_fed              decimal(6,0) ;
DEFINE tmp_ret_det_sinf_causal_retiro        decimal(6,0) ;
DEFINE tmp_ret_det_sinf_resultado_operacion  char(2)      ;
DEFINE tmp_ret_det_sinf_motivo_rech_1        char(3)      ;
DEFINE tmp_ret_det_sinf_motivo_rech_2        char(3)      ;


-- campos de la tabla de sumario de retiros contingente infonavit
-- tmp_ret_sum_solo_infonavit
  
DEFINE tmp_ret_sum_sinf_tpo_registro     char(2)      ;
DEFINE tmp_ret_sum_sinf_id_servicio      char(2)      ;
DEFINE tmp_ret_sum_sinf_id_operacion     char(2)      ;

-- tablas destino
-- ret_solo_infonavit
DEFINE ret_solo_infonavit_id_solicitud        decimal(9,0)          ;
DEFINE ret_solo_infonavit_id_derechohabiente  decimal(9,0)          ;
DEFINE ret_solo_infonavit_nss                 char(11)              ;
DEFINE ret_solo_infonavit_f_solicitud         date                  ;
DEFINE ret_solo_infonavit_estado_solicitud    smallint              ;
DEFINE ret_solo_infonavit_folio               decimal(9,0)          ;
DEFINE ret_solo_infonavit_aivs_viv97          decimal(18,6)         ;
DEFINE ret_solo_infonavit_importe_viv97       decimal(12,2)         ;
DEFINE ret_solo_infonavit_clabe               char(18)              ;
DEFINE ret_solo_infonavit_banco               smallint              ;
DEFINE ret_solo_infonavit_entidad_federativa  smallint              ;
DEFINE ret_solo_infonavit_causal_retiro       smallint              ;
DEFINE ret_solo_infonavit_f_valuacion         date                  ;
DEFINE ret_solo_infonavit_f_captura           date                  ;
DEFINE ret_solo_infonavit_h_captura           datetime day to second;
DEFINE ret_solo_infonavit_usuario             char(20)              ;
DEFINE ret_solo_infonavit_cod_rechazo         smallint              ;
DEFINE v_aivs_valuadas                        DECIMAL(18,6);
DEFINE v_pesos_valuados                       DECIMAL(12,2);
DEFINE v_pesos_validacion                     DECIMAL(12,2);

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);

-- para rechazos
DEFINE v_b_rechazo_encabezado               SMALLINT;
DEFINE v_b_rechazo_detalle                  SMALLINT;
DEFINE v_validar_3_primeros_campos          VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                          SMALLINT; -- clave de afore
-- id matriz derecho
DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS
 
DEFINE v_sumario_importe_total        DECIMAL(24,6);
DEFINE v_detalle_importe_total        DECIMAL(24,6);
DEFINE v_sumario_importe_total_pesos  DECIMAL(20,2);
DEFINE v_detalle_importe_total_pesos  DECIMAL(20,2);
DEFINE v_sumario_total_registros      DECIMAL(2,0) ;
DEFINE v_total_registros              DECIMAL(9,0) ;
DEFINE v_numero_registros             DECIMAL(9,0) ;
DEFINE v_valor_fondo                  DECIMAL(19,14);

DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 

-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados                    integer; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    integer; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    integer; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    integer; -- total de registros de detalle rechazados
DEFINE v_reg_det_procesados                    integer; -- total de registros procesados
 
-- codigos de error en revisiones
DEFINE v_error_sum_aivs_no_coinciden            SMALLINT;
DEFINE v_error_sum_pesos_no_coinciden           SMALLINT;
DEFINE v_error_cza_tpo_registro_invalido         SMALLINT;
DEFINE v_error_cza_id_servicio_invalido          SMALLINT;
DEFINE v_error_cza_sin_precio_fondo              SMALLINT;
DEFINE v_error_cza_sin_fecha_procesar            SMALLINT;
DEFINE v_error_cza_sin_fecha_valuacion           SMALLINT;
-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado             SMALLINT;
DEFINE v_error_det_tpo_registro_invalido         SMALLINT;
DEFINE v_error_det_id_servicio_invalido          SMALLINT;
DEFINE v_error_det_id_operacion_invalido         SMALLINT;
DEFINE v_error_det_matriz_derecho_no_encontrado  SMALLINT;
DEFINE v_error_det_sec_pension_invalido          SMALLINT;
DEFINE v_error_det_fec_solicitud_invalido        SMALLINT;
DEFINE v_error_det_afore_invalido                SMALLINT;
DEFINE v_error_det_lote_invalido                 SMALLINT; -- el lote quedo invalido porque hubo rechazos
DEFINE v_error_det_nss_no_es_77                  SMALLINT; -- el NSS no inicia con 77
DEFINE v_error_det_avis_pesos_no_corresponde     SMALLINT; -- valuacion pesos/aivs es incorrecta
DEFINE v_error_det_sin_cuenta_solo_infonavit     SMALLINT; -- el NSS no tiene movimientos en la cuenta solo inf
 
-- estatus del proceso
DEFINE v_estatus_proceso      SMALLINT;
 
-- para marcar las cuentas
DEFINE v_i_estado_marca       INTEGER;
DEFINE v_marca_solo_infonavit INTEGER; -- 808
DEFINE v_movimiento           SMALLINT; -- clave de movimiento
DEFINE v_saldo_97_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97

-- Control de Excepciones
DEFINE v_si_resultado SMALLINT;
DEFINE sql_err        INTEGER;
DEFINE isam_err       INTEGER;
DEFINE err_txt        VARCHAR(250);
DEFINE v_c_msj        VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      
      RETURN sql_err, isam_err, err_txt, tmp_ret_det_sinf_nss, v_reg_det_procesados, v_reg_det_insertados, v_reg_det_rechazados;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_fc.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   LET v_reg_det_procesados  = 0; -- registros procesados
   LET tmp_ret_det_sinf_nss  = NULL;
   LET v_sumario_importe_total = 0;
   LET v_detalle_importe_total = 0;
   LET v_sumario_importe_total_pesos = 0;
   LET v_detalle_importe_total_pesos = 0;
   LET v_valor_fondo = 0;
   LET ret_solo_infonavit_cod_rechazo = 0;

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;
   
   UPDATE bat_ctr_proceso
   SET    folio = p_folio
   WHERE  pid = p_pid;

   -- se inician los codigos de error en encabezado
   LET v_error_sum_aivs_no_coinciden       = 1000;
   LET v_error_sum_pesos_no_coinciden      = 2000;
   LET v_error_cza_tpo_registro_invalido   = 2;
   LET v_error_cza_id_servicio_invalido    = 3;
   LET v_error_cza_sin_precio_fondo        = 4;
   LET v_error_cza_sin_fecha_procesar      = 5;
   LET v_error_cza_sin_fecha_valuacion     = 6;
 
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_det_id_servicio_invalido          = 3;
   LET v_error_det_id_operacion_invalido         = 4;
   LET v_error_det_matriz_derecho_no_encontrado  = 5;
   LET v_error_det_sec_pension_invalido          = 6;
   LET v_error_det_fec_solicitud_invalido        = 7;
   LET v_error_det_afore_invalido                = 8;
   LET v_error_det_lote_invalido                 = 9;
   LET v_error_det_nss_no_es_77                  = 10;
   LET v_error_det_avis_pesos_no_corresponde     = 11;
   LET v_error_det_sin_cuenta_solo_infonavit     = 12;


   -- se inician las variables para marca
   LET v_marca_solo_infonavit = 801; -- marca retiro solo infonavit
   LET v_i_estado_marca    = 0;
   
   -- se verifican los totales en AIVS
   SELECT SUM(tot_aivs_viv97)
   INTO   v_detalle_importe_total
   FROM   safre_tmp:tmp_ret_det_solo_infonavit;
   
   SELECT SUM(tot_aivs_viv97)
   INTO   v_sumario_importe_total
   FROM   safre_tmp:tmp_ret_sum_solo_infonavit;
 
   -- si no coinciden las sumas de AIVs es un error
   IF ( v_detalle_importe_total <> v_sumario_importe_total ) THEN

      LET v_si_resultado = v_error_sum_aivs_no_coinciden;
      LET v_c_msj = "La suma de AIVs en el detalle del archivo no coincide con la suma de AIVs en el sumario.";
      RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_sinf_nss, v_reg_det_procesados, v_reg_det_insertados, v_reg_det_rechazados;
   END IF

   -- se verifican los totales en pesos
   SELECT SUM(tot_imp_viv97)
   INTO   v_detalle_importe_total_pesos
   FROM   safre_tmp:tmp_ret_det_solo_infonavit;
   
   SELECT SUM(tot_pesos_viv97)
   INTO   v_sumario_importe_total_pesos
   FROM   safre_tmp:tmp_ret_sum_solo_infonavit;
 
   -- si no coinciden las sumas de AIVs es un error
   IF ( v_detalle_importe_total_pesos <> v_sumario_importe_total_pesos ) THEN

      LET v_si_resultado = v_error_sum_pesos_no_coinciden;
      LET v_c_msj = "La suma de PESOS en el detalle del archivo no coincide con la suma de PESOS en el sumario.";
      RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_sinf_nss, v_reg_det_procesados, v_reg_det_insertados, v_reg_det_rechazados;
   END IF

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
   id_codigo       SMALLINT,
   codigo_rechazo SMALLINT
   );
   
   
   -- se obtienen los datos del encabezado
   FOREACH
   SELECT 
      tpo_registro         ,
      id_servicio          ,
      tpo_operacion        ,
      f_operacion          ,
      resultado_operacion  ,
      motivo_rech_1        ,
      motivo_rech_2        
   INTO
      tmp_ret_cza_sinf_tpo_registro         ,
      tmp_ret_cza_sinf_id_servicio          ,
      tmp_ret_cza_sinf_tpo_operacion        ,
      tmp_ret_cza_sinf_f_operacion          ,
      tmp_ret_cza_sinf_resultado_operacion  ,
      tmp_ret_cza_sinf_motivo_rech_1        ,
      tmp_ret_cza_sinf_motivo_rech_2        
   FROM
      safre_tmp:tmp_ret_cza_solo_infonavit 
   
      -- se asume que no hay error
      LET v_b_rechazo_encabezado = 0;
      
      -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;



-- falta que se hace con el encabezado
     
      -- se cuenta un encabezado insertado
      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;
   
   END FOREACH;

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
  
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle = 0;
      
   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      tpo_registro         ,
      id_servicio          ,
      id_operacion         ,
      nss                  ,
      f_solicitud          ,
      f_liquidacion        ,
      tot_aivs_viv97       ,
      tot_imp_viv97        ,
      cta_clabe            ,
      cve_banco            ,
      ent_fed              ,
      causal_retiro        ,
      resultado_operacion  ,
      motivo_rech_1        ,
      motivo_rech_2        
   INTO
      tmp_ret_det_sinf_tpo_registro         ,
      tmp_ret_det_sinf_id_servicio          ,
      tmp_ret_det_sinf_id_operacion         ,
      tmp_ret_det_sinf_nss                  ,
      tmp_ret_det_sinf_f_solicitud          ,
      tmp_ret_det_sinf_f_liquidacion        ,
      tmp_ret_det_sinf_tot_aivs_viv97       ,
      tmp_ret_det_sinf_tot_imp_viv97        ,
      tmp_ret_det_sinf_cta_clabe            ,
      tmp_ret_det_sinf_cve_banco            ,
      tmp_ret_det_sinf_ent_fed              ,
      tmp_ret_det_sinf_causal_retiro        ,
      tmp_ret_det_sinf_resultado_operacion  ,
      tmp_ret_det_sinf_motivo_rech_1        ,
      tmp_ret_det_sinf_motivo_rech_2        
   FROM
      safre_tmp:tmp_ret_det_solo_infonavit
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;
      
      -- se cuenta un registro procesado
      LET v_reg_det_procesados = v_reg_det_procesados + 1;

      -- el NSS debe iniciar con 77. 15 NOV 2013. Ya no se rechazan
      --Se atiende PRODINF-125 
      --IF ( tmp_ret_det_sinf_nss[1,2] <> "77" ) THEN
      --   -- se marca la bandera de rechazo de detalle
      --   LET v_b_rechazo_detalle    = 1;
      --   LET v_id_derechohabiente   = 0;
      --
      --   INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_es_77);
      --
      --   -- se incrementa el indice
      --   LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      --END IF

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_ret_det_sinf_nss;

      -- ==========================================================================
      
      -- el id_solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
      
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
      
      -- ==========================================================================

      --TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      ---- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;
      
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);
      
         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
      
      -- se obtiene el valor del fondo segun la fecha de valuacion del registro
      SELECT precio_fondo
      INTO   v_valor_fondo
      FROM   glo_valor_fondo
      WHERE  f_valuacion = tmp_ret_det_sinf_f_liquidacion
      AND    fondo = 11;
      
      LET v_aivs_valuadas = tmp_ret_det_sinf_tot_aivs_viv97 / 1000000;
      LET v_pesos_valuados = tmp_ret_det_sinf_tot_imp_viv97 / 100;
      
      -- se valuan las aivs para obtener pesos
      LET v_pesos_validacion = v_aivs_valuadas * v_valor_fondo;
/*      
      -- se verifica si el monto de pesos corresponde al monto de avis multiplicado por el valor del fondo
      IF ( v_pesos_valuados <> v_pesos_validacion ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
      
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_avis_pesos_no_corresponde);
      
         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;      
      END IF
*/     

      -- se verifica si el nss tiene movimientos en la cuenta de solo infonavit
      -- Se modifica la validación según req SACI2018-3 -- En caso de que no exista la subcuenta 44
      --  se deberá crear la subcuenta con el importe del abono recibido de la subcuenta 4
      SELECT SUM(monto_acciones)
      INTO   v_saldo_97_aivs
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    subcuenta          = 44 -- solo infonavit
      AND    fondo_inversion    = 11;
      
      -- 15Nov2013. si no tiene, no se procesa el retiro
      IF ( v_saldo_97_aivs IS NULL ) THEN
         SELECT SUM(monto_acciones)
         INTO   v_saldo_97_aivs
         FROM   cta_movimiento
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    subcuenta          = 4 -- solo infonavit
         AND    fondo_inversion    = 11;
         IF ( v_saldo_97_aivs IS NULL ) THEN

            -- se rechaza la solicitud
            LET v_b_rechazo_detalle = 1;
            
            -- no tiene movimientos en la cuenta
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_sin_cuenta_solo_infonavit);
         
            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;      
         END IF

      END IF

      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN
      
         LET v_motivo_rechazo_1 = 0;
      --   LET v_motivo_rechazo_2 = 0;
      --   LET v_motivo_rechazo_3 = 0;
      --
      --
         -- se leen los tres primeros errores
         FOREACH
         SELECT FIRST 1
            id_codigo,
            codigo_rechazo
         INTO 
            v_indice_codigos_rechazo, v_codigos_rechazo
         FROM
            tmp_codigos_rechazo
         ORDER BY
            id_codigo
            
            -- se asignan los primeros 3 codigos de rechazo
            LET ret_solo_infonavit_cod_rechazo = v_codigos_rechazo;
            
            EXIT FOREACH;
         END FOREACH;
         
         -- se cuenta un rechazo
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados
         
         -- se inserta el rechazo
         LET ret_solo_infonavit_id_solicitud        = v_id_solicitud; --decimal(9,0)          
         LET ret_solo_infonavit_id_derechohabiente  = v_id_derechohabiente; --decimal(9,0)
         LET ret_solo_infonavit_nss                 = tmp_ret_det_sinf_nss;
         LET ret_solo_infonavit_f_solicitud         = tmp_ret_det_sinf_f_solicitud; --date
         LET ret_solo_infonavit_estado_solicitud    = 100; -- rechazada
         LET ret_solo_infonavit_folio               = p_folio;
         LET ret_solo_infonavit_aivs_viv97          = tmp_ret_det_sinf_tot_aivs_viv97 / 1000000; 
         LET ret_solo_infonavit_importe_viv97       = tmp_ret_det_sinf_tot_imp_viv97 / 100;
         LET ret_solo_infonavit_clabe               = tmp_ret_det_sinf_cta_clabe; --char(18)              
         LET ret_solo_infonavit_banco               = tmp_ret_det_sinf_cve_banco; --smallint              
         LET ret_solo_infonavit_entidad_federativa  = tmp_ret_det_sinf_ent_fed; --smallint              
         LET ret_solo_infonavit_causal_retiro       = tmp_ret_det_sinf_causal_retiro; --smallint              
         LET ret_solo_infonavit_f_valuacion         = tmp_ret_det_sinf_f_liquidacion; --date                  
         LET ret_solo_infonavit_f_captura           = TODAY; --date                  
         LET ret_solo_infonavit_h_captura           = CURRENT DAY TO SECOND; --datetime day to second
         LET ret_solo_infonavit_usuario             = p_usuario_cod;
         
         -- se inserta en estado de rechazo
         INSERT INTO ret_solo_infonavit (
            id_solicitud        ,
            id_derechohabiente  ,
            nss                 ,
            f_solicitud         ,
            estado_solicitud    ,
            folio               ,
            aivs_viv97          ,
            importe_viv97       ,
            clabe               ,
            banco               ,
            entidad_federativa  ,
            causal_retiro       ,
            f_valuacion         ,
            f_captura           ,
            h_captura           ,
            usuario             ,
            cod_rechazo         
         )
         VALUES (
            ret_solo_infonavit_id_solicitud        ,
            ret_solo_infonavit_id_derechohabiente  ,
            ret_solo_infonavit_nss                 ,
            ret_solo_infonavit_f_solicitud         ,
            ret_solo_infonavit_estado_solicitud    ,
            ret_solo_infonavit_folio               ,
            ret_solo_infonavit_aivs_viv97          ,
            ret_solo_infonavit_importe_viv97       ,
            ret_solo_infonavit_clabe               ,
            ret_solo_infonavit_banco               ,
            ret_solo_infonavit_entidad_federativa  ,
            ret_solo_infonavit_causal_retiro       ,
            ret_solo_infonavit_f_valuacion         ,
            ret_solo_infonavit_f_captura           ,
            ret_solo_infonavit_h_captura           ,
            ret_solo_infonavit_usuario             ,
            ret_solo_infonavit_cod_rechazo         
         );
        
         CONTINUE FOREACH;
      END IF

      -- ==========================================================================
      -- registro aceptado

      LET ret_solo_infonavit_id_solicitud        = v_id_solicitud;
      LET ret_solo_infonavit_id_derechohabiente  = v_id_derechohabiente;
      LET ret_solo_infonavit_nss                 = tmp_ret_det_sinf_nss;
      LET ret_solo_infonavit_f_solicitud         = tmp_ret_det_sinf_f_solicitud;
      LET ret_solo_infonavit_estado_solicitud    = 10; -- capturada
      LET ret_solo_infonavit_folio               = p_folio;
      LET ret_solo_infonavit_aivs_viv97          = tmp_ret_det_sinf_tot_aivs_viv97 / 1000000; 
      LET ret_solo_infonavit_importe_viv97       = tmp_ret_det_sinf_tot_imp_viv97 / 100;
      LET ret_solo_infonavit_clabe               = tmp_ret_det_sinf_cta_clabe;
      LET ret_solo_infonavit_banco               = tmp_ret_det_sinf_cve_banco;
      LET ret_solo_infonavit_entidad_federativa  = tmp_ret_det_sinf_ent_fed;
      LET ret_solo_infonavit_causal_retiro       = tmp_ret_det_sinf_causal_retiro;
      LET ret_solo_infonavit_f_valuacion         = tmp_ret_det_sinf_f_liquidacion;
      LET ret_solo_infonavit_f_captura           = TODAY;
      LET ret_solo_infonavit_h_captura           = CURRENT DAY TO SECOND;
      LET ret_solo_infonavit_usuario             = p_usuario_cod;
      LET ret_solo_infonavit_cod_rechazo         = 0;
      
       
      -- se marca la cuenta
      LET v_i_estado_marca = 0;
      EXECUTE FUNCTION fn_marca_cuenta(
              ret_solo_infonavit_id_derechohabiente
             ,v_marca_solo_infonavit -- marca de retiros contingente solo infonavit
             ,ret_solo_infonavit_id_solicitud
             ,ret_solo_infonavit_folio
             ,0 -- estado marca
             ,0 -- codigo de rechazo
             ,0 -- marca de la causa
             ,NULL -- fecha de la causa
             ,p_usuario_cod
             ,p_proceso_cod)
         INTO v_i_estado_marca;
         
      -- si no se pudo marcar, se rechaza el registro
      IF ( v_i_estado_marca > 0 ) THEN
         LET ret_solo_infonavit_estado_solicitud = 100; -- rechazada
         LET ret_solo_infonavit_cod_rechazo      = 130; -- marca no convive
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados
      ELSE
         -- se cuenta un registro insertado
         LET v_reg_det_insertados = v_reg_det_insertados + 1;
      END IF
      
      -- se inserta en la tabla historia de detalle de retiro por fortalecimiento al credito
      INSERT INTO ret_solo_infonavit (
         id_solicitud        ,
         id_derechohabiente  ,
         nss                 ,
         f_solicitud         ,
         estado_solicitud    ,
         folio               ,
         aivs_viv97          ,
         importe_viv97       ,
         clabe               ,
         banco               ,
         entidad_federativa  ,
         causal_retiro       ,
         f_valuacion         ,
         f_captura           ,
         h_captura           ,
         usuario             ,
         cod_rechazo         
      )
      VALUES (
         ret_solo_infonavit_id_solicitud        ,
         ret_solo_infonavit_id_derechohabiente  ,
         ret_solo_infonavit_nss                 ,
         ret_solo_infonavit_f_solicitud         ,
         ret_solo_infonavit_estado_solicitud    ,
         ret_solo_infonavit_folio               ,
         ret_solo_infonavit_aivs_viv97          ,
         ret_solo_infonavit_importe_viv97       ,
         ret_solo_infonavit_clabe               ,
         ret_solo_infonavit_banco               ,
         ret_solo_infonavit_entidad_federativa  ,
         ret_solo_infonavit_causal_retiro       ,
         ret_solo_infonavit_f_valuacion         ,
         ret_solo_infonavit_f_captura           ,
         ret_solo_infonavit_h_captura           ,
         ret_solo_infonavit_usuario             ,
         ret_solo_infonavit_cod_rechazo              
      );
      
   END FOREACH;
 
   UPDATE STATISTICS FOR TABLE ret_solo_infonavit;
  
   -- si no hubo registros aceptados se considera un error
   IF ( v_reg_det_insertados < 1 ) THEN
      LET v_si_resultado = 1000;
      LET isam_err = 0;
      LET v_c_msj = "ERROR. Todos los registros fueron rechazados.";
   END IF
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_sinf_nss, v_reg_det_procesados, v_reg_det_insertados, v_reg_det_rechazados;
END FUNCTION;


