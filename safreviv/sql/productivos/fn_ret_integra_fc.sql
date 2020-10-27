






CREATE FUNCTION "safreviv".fn_ret_integra_fc(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)


-- campos de la tabla de encabezado de retiros por fortalecimiento al credito
DEFINE tmp_ret_cza_fc_tpo_registro  CHAR(2);
DEFINE tmp_ret_cza_fc_id_servicio   CHAR(2);
DEFINE tmp_ret_cza_fc_id_operacion  CHAR(2);

-- campos de la tabla de detalle de retiros por fortalecimiento al credito
DEFINE tmp_ret_det_fc_tpo_registro CHAR(2) ;
DEFINE tmp_ret_det_fc_id_servicio  CHAR(2) ;
DEFINE tmp_ret_det_fc_id_operacion CHAR(2) ;
DEFINE tmp_ret_det_fc_nss          CHAR(11);

-- campos de la tabla de sumario de retiros por fortalecimiento al credito
DEFINE tmp_ret_sum_fc_tpo_registro    CHAR(2)     ;
DEFINE tmp_ret_sum_fc_id_servicio     CHAR(2)     ;
DEFINE tmp_ret_sum_fc_id_operacion    CHAR(2)     ;
DEFINE tmp_ret_sum_fc_total_registros DECIMAL(6,0);

-- tablas destino
-- encabezado de la tabla historica/integrada de retiros por fortalecimiento al credito
-- ret_fortalecimiento_credito
DEFINE ret_fc_id_solicitud        decimal(9,0)          ;
DEFINE ret_fc_id_derechohabiente  decimal(9,0)          ;
DEFINE ret_fc_folio               decimal(9,0)          ;
DEFINE ret_fc_f_solicitud         date                  ;
DEFINE ret_fc_h_solicitud         datetime day to second;
DEFINE ret_fc_importe_viv         decimal(12,2)         ;
DEFINE ret_fc_estado_solicitud    smallint              ;
DEFINE ret_fc_cod_rechazo         smallint              ;
DEFINE ret_fc_usuario             char(20)              ;


-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
-- para calcular las AIVs a pesos
DEFINE v_valor_fondo                        DECIMAL(14,6);
DEFINE v_pesos_aiv97                        decimal(14,6);
DEFINE v_pesos_aiv92                        decimal(14,6);
DEFINE v_saldo_aivs                         DECIMAL(18,6); -- saldo del derechohabiente en viv92
DEFINE v_saldo_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv97
DEFINE v_resultado_consulta                 SMALLINT;

-- para rechazos
DEFINE v_b_rechazo_encabezado               SMALLINT;
DEFINE v_b_rechazo_detalle                  SMALLINT;
DEFINE v_validar_3_primeros_campos          VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                          SMALLINT; -- clave de afore
-- id matriz derecho
DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS
 
DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(2,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(2,0) ;
DEFINE v_saldo_cuenta                          DECIMAL(14,6);

DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 

-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados                    SMALLINT; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    SMALLINT; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    SMALLINT; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    SMALLINT; -- total de registros de detalle rechazados
 
-- codigos de error en encabezado
DEFINE v_error_cza_reg_totales_no_coinciden      DECIMAL(10,5);
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
 
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 
-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_fc                                INTEGER; -- 808
DEFINE v_movimiento                              SMALLINT; -- clave de movimiento

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_fc.txt";

    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET 
        folio = P_folio,
       estado = 2 -- integrado
     WHERE proceso_cod    = p_proceso_cod
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = P_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden      = 1000;
   LET v_error_cza_tpo_registro_invalido         = 2;
   LET v_error_cza_id_servicio_invalido          = 3;
   LET v_error_cza_sin_precio_fondo              = 4;
   LET v_error_cza_sin_fecha_procesar            = 5;
   LET v_error_cza_sin_fecha_valuacion           = 6;
 
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


   -- se inician las variables para marca
   LET v_marca_fc = 807; -- marca RETIRO POR FORTALECIMIENTO AL CREDITO
   LET v_i_estado_marca    = 0;
   
   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO v_total_registros
   FROM
      safre_tmp:tmp_ret_det_fc;
   
   SELECT total_registros
   INTO v_sumario_total_registros
   FROM
      safre_tmp:tmp_ret_sum_fc;
 
   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN

      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET v_c_msj = "No coinciden numero de registros cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
   id_codigo       SMALLINT,
   codigo_rechazo SMALLINT
   );
   --
   --
   ---- se obtienen los datos del encabezado
   --FOREACH
   --SELECT 
   --   tpo_registro           ,
   --   id_servicio            ,
   --   tpo_entidad_origen     ,
   --   cve_entidad_origen     ,
   --   tpo_entidad_destino    ,
   --   cve_entidad_destino    ,
   --   f_operacion            ,
   --   f_valor_transferencia  ,
   --   val_participacion      ,
   --   resultado_operacion    ,
   --   motivo_rech_1          ,
   --   motivo_rech_2          ,
   --   motivo_rech_3          
   --INTO
   --   tmp_ret_cza_tpo_registro          ,
   --   tmp_ret_cza_id_servicio           ,
   --   tmp_ret_cza_tpo_entidad_origen    ,
   --   tmp_ret_cza_cve_entidad_origen    ,
   --   tmp_ret_cza_tpo_entidad_destino   ,
   --   tmp_ret_cza_cve_entidad_destino   ,
   --   tmp_ret_cza_f_operacion           ,
   --   tmp_ret_cza_f_valor_transferencia ,
   --   tmp_ret_cza_val_participacion     ,
   --   tmp_ret_cza_resultado_operacion   ,
   --   tmp_ret_cza_motivo_rech_1         ,
   --   tmp_ret_cza_motivo_rech_2         ,
   --   tmp_ret_cza_motivo_rech_3         
   --FROM
   --   safre_tmp:tmp_ret_cza_disposicion 
   --
   --   -- se asume que no hay error
   --   LET v_b_rechazo_encabezado = 0;
   --   
   --   -- se borra la tabla de errores
   --   DELETE FROM tmp_codigos_rechazo WHERE 1=1;
   --
   --   -- se asignan los datos al registro de encabezado historico
   --   LET ret_cza_disposicion_folio                 = p_folio;
   --   LET ret_cza_disposicion_nombre_archivo        = p_nombre_archivo;
   --   LET ret_cza_disposicion_f_operacion_procesar  = tmp_ret_cza_f_operacion;
   --   LET ret_cza_disposicion_f_carga               = TODAY;
   --   LET ret_cza_disposicion_h_carga               = CURRENT HOUR TO MINUTE;
   --   LET ret_cza_disposicion_f_valor_transferencia = tmp_ret_cza_f_valor_transferencia; 
   --   --TRACE("Valor del fondo en archivo:");
   --   --TRACE(tmp_ret_cza_val_participacion);
   --
   --   LET ret_cza_disposicion_precio_fondo          = tmp_ret_cza_val_participacion / 1000000;
   --
   --   TRACE("Despues de dividir el valor del fondo es:");
   --   TRACE(ret_cza_disposicion_precio_fondo);
   --
	 --   LET ret_cza_disposicion_total_registros       = v_numero_registros; -- numero de registros
	 --   LET ret_cza_disposicion_total_importe         = 0; -- importe total -- se sumara al final
   --   LET ret_cza_disposicion_usuario               = p_usuario_cod;
   --
   --   -- se establece el valor del fondo
   --   LET v_valor_fondo = ret_cza_disposicion_precio_fondo;
   --   TRACE("Valor del fondo:");
   --   TRACE(v_valor_fondo);
   --
   --   -- se reinicia el indice de codigos de rechazo
   --   LET v_indice_codigos_rechazo = 1;
   --   
   --   --TRACE("Validando encabezado");
   --   -- =========================================================
   --   -- validando tipo de registro
   --   IF ( tmp_ret_cza_tpo_registro <> "01" OR tmp_ret_cza_id_servicio <> "04" ) THEN
   --      -- se activa la bandera de rechazo de encabezado
   --      LET v_b_rechazo_encabezado = 1;
   --   
   --      -- 1	Tipo de Registro	X	02	00	001	-	002	"01" Encabezado de lote
   --      IF ( tmp_ret_cza_tpo_registro <> "01" ) THEN
   --        INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_tpo_registro_invalido);
   --
   --        -- se incrementa el indice
   --        LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
   --      END IF
   --      
   --      -- validando identificador de servicio
   --      -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
   --      IF ( tmp_ret_cza_id_servicio <> "04" ) THEN
   --        INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_id_servicio_invalido);
   --
   --        -- se incrementa el indice
   --        LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
   --      END IF
   --   END IF
   --   
   --   -- =========================================================
   --   -- se verifica si el encabezado contiene el precio del fondo
   --   IF ( ret_cza_disposicion_precio_fondo <= 0 ) THEN
   --     INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_precio_fondo);
   --
   --     -- se incrementa el indice
   --     LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
   --   END IF      
   --   
   --   -- =========================================================
   --   -- se verifica si el encabezado contiene fecha procesar
   --   IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
   --     INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_procesar);
   --
   --     -- se incrementa el indice
   --     LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
   --   END IF      
   --
   --   -- =========================================================
   --   -- se verifica si el encabezado contiene fecha de valuacion
   --   IF ( tmp_ret_cza_f_valor_transferencia IS NULL ) THEN
   --     INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_valuacion);
   --
   --     -- se incrementa el indice
   --     LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
   --   END IF      
   --   
   --   
   --   -- si hubo rechazo de encabezado no se inserta en historico
   --   IF ( v_b_rechazo_encabezado = 1 ) THEN
   --      
   --      -- se llenan los datos generales de rechazo del encabezado
   --      LET ret_cza_disposicion_rch_folio                 = p_folio; -- DECIMAL(9,0)                    ;
   --      LET ret_cza_disposicion_rch_f_operacion_procesar  = tmp_ret_cza_f_operacion; -- DATE                            ;
   --      LET ret_cza_disposicion_rch_nombre_archivo        = p_nombre_archivo; -- CHAR(20)                        ;
   --      LET ret_cza_disposicion_rch_f_carga               = TODAY; -- DATE                            ;
   --      LET ret_cza_disposicion_rch_h_carga               = CURRENT HOUR TO SECOND; -- DATETIME FRACTION TO FRACTION(3);
   --      LET ret_cza_disposicion_rch_f_valor_transferencia = tmp_ret_cza_f_valor_transferencia; -- DATE                            ;
   --      LET ret_cza_disposicion_rch_precio_fondo          = ret_cza_disposicion_precio_fondo; -- DECIMAL(14,6)                   ;
   --      LET ret_cza_disposicion_rch_total_registros       = 0; -- INTEGER                         ;
   --      LET ret_cza_disposicion_rch_total_importe         = 0; -- DECIMAL(18,6)                   ;
   --      LET ret_cza_disposicion_rch_usuario               = p_usuario_cod; -- CHAR(20)                        ;
   --      LET ret_cza_disposicion_rch_resultado_operacion   = 2; -- rechazado
   --
   --      -- se asignan los primeros tres errores
   --      LET v_motivo_rechazo_1 = 0;
   --      LET v_motivo_rechazo_2 = 0;
   --      LET v_motivo_rechazo_3 = 0;
   --
   --      -- se leen los tres primeros errores
   --      FOREACH
   --      SELECT FIRST 3
   --         id_codigo,
   --         codigo_rechazo
   --      INTO 
   --         v_indice_codigos_rechazo, v_codigos_rechazo
   --      FROM
   --         tmp_codigos_rechazo
   --      ORDER BY
   --         id_codigo
   --         
   --         IF ( v_indice_codigos_rechazo = 1 ) THEN
   --            -- se asignan los primeros 3 codigos de rechazo
   --            LET v_motivo_rechazo_1 = v_codigos_rechazo;
   --         END IF
   --
   --         IF ( v_indice_codigos_rechazo = 2 ) THEN
   --            -- se asignan los primeros 3 codigos de rechazo
   --            LET v_motivo_rechazo_2 = v_codigos_rechazo;
   --         END IF
   --
   --         IF ( v_indice_codigos_rechazo = 3 ) THEN
   --            -- se asignan los primeros 3 codigos de rechazo
   --            LET v_motivo_rechazo_3 = v_codigos_rechazo;
   --            EXIT FOREACH;
   --         END IF 
   --      END FOREACH;
   --
   --      LET ret_cza_disposicion_rch_cod_rechazo_1  = v_motivo_rechazo_1;
   --      LET ret_cza_disposicion_rch_cod_rechazo_2  = v_motivo_rechazo_2;
   --      LET ret_cza_disposicion_rch_cod_rechazo_3  = v_motivo_rechazo_3;
   --
   --      
   --      -- se inserta el rechazo
   --      INSERT INTO ret_cza_disposicion_rch (
   --         folio                 ,
   --         f_operacion_procesar  ,
   --         nombre_archivo        ,
   --         f_carga               ,
   --         h_carga               ,
   --         f_valor_transferencia ,
   --         precio_fondo          ,
   --         total_registros       ,
   --         total_importe         ,
   --         usuario               ,
   --         resultado_operacion   ,
   --         cod_rechazo_1         ,
   --         cod_rechazo_2         ,
   --         cod_rechazo_3         
   --      )
   --      VALUES (
   --         ret_cza_disposicion_rch_folio                 ,
   --         ret_cza_disposicion_rch_f_operacion_procesar  ,
   --         ret_cza_disposicion_rch_nombre_archivo        ,
   --         ret_cza_disposicion_rch_f_carga               ,
   --         ret_cza_disposicion_rch_h_carga               ,
   --         ret_cza_disposicion_rch_f_valor_transferencia ,
   --         ret_cza_disposicion_rch_precio_fondo          ,
   --         ret_cza_disposicion_rch_total_registros       ,
   --         ret_cza_disposicion_rch_total_importe         ,
   --         ret_cza_disposicion_rch_usuario               ,
   --         ret_cza_disposicion_rch_resultado_operacion   ,
   --         ret_cza_disposicion_rch_cod_rechazo_1         ,
   --         ret_cza_disposicion_rch_cod_rechazo_2         ,
   --         ret_cza_disposicion_rch_cod_rechazo_3         
   --      );
   --      
   --      -- se cuenta un encabezado rechazado
   --      LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;
   --      
   --      -- el registro fue rechazado y no se inserta en el historico
   --      CONTINUE FOREACH;
   --
   --   END IF
   --   
   --   
   --   -- se inserta en la tabla historica del encabezado de retiros por disposicion
   --   INSERT INTO ret_cza_disposicion (
   --      folio                 ,
   --      nombre_archivo        ,
   --      f_operacion_procesar  ,
   --      f_carga               ,
   --      h_carga               ,
   --      f_valor_transferencia , 
   --      precio_fondo          ,
	 --      total_registros       ,
	 --      total_importe         ,
   --      usuario               
   --   )
   --   VALUES (
   --      ret_cza_disposicion_folio                 ,
   --      ret_cza_disposicion_nombre_archivo        ,
   --      ret_cza_disposicion_f_operacion_procesar  ,
   --      ret_cza_disposicion_f_carga               ,
   --      ret_cza_disposicion_h_carga               ,
   --      ret_cza_disposicion_f_valor_transferencia , 
   --      ret_cza_disposicion_precio_fondo          , 
	 --      ret_cza_disposicion_total_registros       ,
	 --      ret_cza_disposicion_total_importe         ,
   --      ret_cza_disposicion_usuario               
   --   );
   --   
   --   -- se cuenta un encabezado insertado
   --   LET v_reg_cza_insertados = v_reg_cza_insertados + 1;
   --
   --END FOREACH;

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
  
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;
      
   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      tpo_registro ,
      id_servicio  ,
      id_operacion ,
      nss          
   INTO
      tmp_ret_det_fc_tpo_registro ,
      tmp_ret_det_fc_id_servicio  ,
      tmp_ret_det_fc_id_operacion ,
      tmp_ret_det_fc_nss          
   FROM
      safre_tmp:tmp_ret_det_fc
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle    = 0;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO v_id_derechohabiente
      FROM
         afi_derechohabiente
      WHERE
         nss = tmp_ret_det_fc_nss;

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      
      -- el id_solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
      
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
      
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================

      --TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      LET v_validar_3_primeros_campos = tmp_ret_det_fc_tpo_registro || tmp_ret_det_fc_id_servicio || tmp_ret_det_fc_id_operacion;
      
      -- si la concatenacion no es igual a la esperada, entonces algun campo es incorrecto
      IF ( v_validar_3_primeros_campos <> "030411" ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
                  
         -- 1	Tipo de Registro
         IF ( tmp_ret_det_fc_tpo_registro <> "03" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 2	Identificador de Servicio
         IF ( tmp_ret_det_fc_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 3	Identificador de Operación
         IF ( tmp_ret_det_fc_id_operacion <> "11" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_operacion_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
      END IF
    
      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN
      
         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;

      
         -- se leen los tres primeros errores
         FOREACH
         SELECT FIRST 3
            id_codigo,
            codigo_rechazo
         INTO 
            v_indice_codigos_rechazo, v_codigos_rechazo
         FROM
            tmp_codigos_rechazo
         ORDER BY
            id_codigo
            
            IF ( v_indice_codigos_rechazo = 1 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_1 = v_codigos_rechazo;
            END IF

            IF ( v_indice_codigos_rechazo = 2 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_2 = v_codigos_rechazo;
            END IF

            IF ( v_indice_codigos_rechazo = 3 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_3 = v_codigos_rechazo;
               EXIT FOREACH;
            END IF 
         END FOREACH;
               
         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- si fue rechazado no se inserta en el historico
         CONTINUE FOREACH;
      END IF

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- se inician las variables para obtener el saldo
      LET v_saldo_aivs  = 0;
      LET v_saldo_pesos = 0;

      -- se asingnan los datos al registro de detalle

      LET ret_fc_id_solicitud        = v_id_solicitud; -- decimal(9,0)          ;
      LET ret_fc_id_derechohabiente  = v_id_derechohabiente; -- decimal(9,0)          ;
      LET ret_fc_folio               = p_folio; -- decimal(9,0)          ;
      LET ret_fc_f_solicitud         = TODAY; -- date                  ;
      LET ret_fc_h_solicitud         = CURRENT HOUR TO SECOND; -- datetime day to second;
      LET ret_fc_importe_viv         = 0; -- el saldo en pesos encontrado
      LET ret_fc_estado_solicitud    = 30; -- se asume que se acepta
      LET ret_fc_cod_rechazo         = 0; -- smallint              ;
      LET ret_fc_usuario             = p_usuario_cod; -- char(20)              ;

      
      -- se verifica si el derechohabiente tiene saldo
      EXECUTE FUNCTION fn_saldo_dia(NULL,
                                    v_id_derechohabiente,
                                    49,
                                    NULL)
                       INTO v_resultado_consulta, v_saldo_aivs, v_saldo_pesos;

      -- si tiene saldo se acepta
      IF ( v_saldo_pesos > 0 ) THEN       
         LET ret_fc_importe_viv         = v_saldo_pesos; -- el saldo en pesos encontrado
         -- se cuenta un registro insertado
         LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_marca_cuenta(
                 ret_fc_id_derechohabiente
                ,v_marca_fc -- marca de retiros fortalecimiento al credito
                ,ret_fc_id_solicitud
                ,ret_fc_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;
            
         -- si no se pudo marcar, se rechaza el registro
         IF ( v_i_estado_marca > 0 ) THEN
            LET ret_fc_estado_solicitud = 100; -- rechazada
            LET ret_fc_cod_rechazo      = 130; -- marca no convive
         END IF
      ELSE
         -- se rechaza la solicitud por no tener saldo
         LET ret_fc_importe_viv         = 0; -- no tiene saldo
         LET ret_fc_estado_solicitud    = 100; -- rechazada
         LET ret_fc_cod_rechazo         = 10; -- motivo: sin saldo
      END IF
      
      -- se inserta en la tabla historia de detalle de retiro por fortalecimiento al credito
      INSERT INTO ret_fortalecimiento_credito (
         id_solicitud       ,
         id_derechohabiente ,
         folio              ,
         f_solicitud        ,
         h_solicitud        ,
         importe_viv        ,
         estado_solicitud   ,
         cod_rechazo        ,
         usuario            
      )
      VALUES (
         ret_fc_id_solicitud      ,
         ret_fc_id_derechohabiente,
         ret_fc_folio             ,
         ret_fc_f_solicitud       ,
         ret_fc_h_solicitud       ,
         ret_fc_importe_viv       ,
         ret_fc_estado_solicitud  ,
         ret_fc_cod_rechazo       ,
         ret_fc_usuario           
      );

      
   END FOREACH;
 
   UPDATE STATISTICS FOR TABLE ret_fortalecimiento_credito;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


