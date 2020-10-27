






CREATE FUNCTION "safreviv".fn_ret_integra_fondo_ahorro_cont(   p_usuario_cod    CHAR(20)
                                              , p_folio          DECIMAL(9,0)
                                              , p_nombre_archivo VARCHAR(40)
                                              , p_pid            DECIMAL(9,0)
                                              , p_proceso_cod    SMALLINT
                                              )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)
   
-- campos de la tabla de encabezado de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_cza_tpo_registro                CHAR(2)    ;
DEFINE tmp_ret_cza_id_servicio                 CHAR(2)    ;
DEFINE tmp_ret_cza_tpo_operacion               CHAR(2)    ;
DEFINE tmp_ret_cza_f_operacion                 DATE       ;
DEFINE tmp_ret_cza_resultado_operacion         CHAR(2)    ;
DEFINE tmp_ret_cza_motivo_rech_1               CHAR(3)    ;
DEFINE tmp_ret_cza_motivo_rech_2               CHAR(3)    ;

-- campos de la tabla de detalle de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_det_tpo_registro                CHAR(2)    ;
DEFINE tmp_ret_det_id_servicio                 CHAR(2)    ;
DEFINE tmp_ret_det_id_operacion                CHAR(2)    ;
DEFINE tmp_ret_det_nss                         CHAR(11)   ;
DEFINE tmp_ret_det_f_solicitud                 DATE       ;
DEFINE tmp_ret_det_f_liquidacion               DATE       ;
DEFINE tmp_ret_det_cve_refer                   VARCHAR(20);
DEFINE tmp_ret_det_imp_viv72                   DECIMAL(14);
DEFINE tmp_ret_det_tanto_imp_viv72             DECIMAL( 8);
DEFINE tmp_ret_det_entidad                     CHAR(6)    ;
DEFINE tmp_ret_det_causal_retiro               CHAR(6)    ;
DEFINE tmp_ret_det_caso_adai                   CHAR(10)   ;
DEFINE tmp_ret_det_rfc                         CHAR(13)   ;
DEFINE tmp_ret_det_id_transpaso                CHAR(1)    ;
DEFINE tmp_ret_det_resultado_operacion         CHAR(2)    ;
DEFINE tmp_ret_det_motivo_rech_1               CHAR(3)    ;
DEFINE tmp_ret_det_motivo_rech_2               CHAR(3)    ;
DEFINE tmp_ret_det_tpo_mov_cta                 CHAR(1)    ;

-- detalle de la tabla historica/integrada de retiros de fondo ahorro
-- ret_fondo_ahorro
DEFINE ret_fondo_ahorro_id_solicitud         DECIMAL(9,0)           ;
DEFINE ret_fondo_ahorro_id_derechohabiente   DECIMAL(9,0)           ;
DEFINE ret_fondo_ahorro_nss                  CHAR(11)               ;
DEFINE ret_fondo_ahorro_rfc                  CHAR(13)               ;
DEFINE ret_fondo_ahorro_f_solicitud          DATE                   ;
DEFINE ret_fondo_ahorro_estado_solicitud     SMALLINT               ;
DEFINE ret_fondo_ahorro_causal_retiro        SMALLINT               ;
DEFINE ret_fondo_ahorro_id_datamart          DECIMAL(9,0)           ;
DEFINE ret_fondo_ahorro_folio                DECIMAL(9,0)           ;
DEFINE ret_fondo_ahorro_cve_referencia       VARCHAR(20,0)          ;
DEFINE ret_fondo_ahorro_saldo_viv72          DECIMAL(14,2)          ;
DEFINE ret_fondo_ahorro_tanto_adicional      DECIMAL(14,2)          ;
DEFINE ret_fondo_ahorro_caso_adai            DECIMAL(10,0)          ;
DEFINE ret_fondo_ahorro_entidad_federativa   SMALLINT               ;
DEFINE ret_fondo_ahorro_f_liquidacion        DATE                   ;
DEFINE ret_fondo_ahorro_id_traspaso          CHAR(1)                ;
DEFINE ret_fondo_ahorro_f_captura            DATE                   ;
DEFINE ret_fondo_ahorro_h_captura            DATETIME HOUR TO SECOND;
DEFINE ret_fondo_ahorro_usuario              VARCHAR(20,0)          ;
DEFINE ret_fondo_ahorro_cod_rechazo          SMALLINT               ;
DEFINE ret_fondo_ahorro_tpo_mov_cta			 CHAR(1)                ; -- PRODINF-354;
-- variables de soporte al proceso
DEFINE v_id_derechohabiente                  DECIMAL(9,0);
DEFINE v_id_afi_fondo72                      DECIMAL(9,0);
DEFINE v_encontrado_por_nss_rfc              SMALLINT; -- indica la forma como se encontro el NSS/RFC
DEFINE v_const_ENCONTRADO_NSS_RFC            SMALLINT; -- se encontro con NSS y RFC
DEFINE v_ocurrencias                         SMALLINT; -- cantidad registros por busqueda
DEFINE v_const_ENCONTRADO_NSS                SMALLINT; -- se encontro con NSS
DEFINE v_const_ENCONTRADO_RFC                SMALLINT; -- se encontro con RFC

DEFINE v_rec_tot_saldo_viv                   DECIMAL(22,2);
DEFINE v_solicitud                           INTEGER;
DEFINE v_total_422                           INTEGER;
DEFINE v_total_182                           INTEGER; 
--variable de solicitd   
DEFINE v_id_solicitud                        DECIMAL(9,0);
DEFINE v_ret_fondo_ahorro_id_afi_fondo72     DECIMAL(9,0);     

-- =============================================================================
-- para calcular las AIVs a pesos
DEFINE v_valor_fondo                           DECIMAL(14,6);
DEFINE v_pesos_aiv97                           DECIMAL(14,6);
DEFINE v_pesos_aiv92                           DECIMAL(14,6);
                                               
-- para rechazos                               
DEFINE v_b_rechazo_encabezado                  SMALLINT;
DEFINE v_b_rechazo_detalle                     SMALLINT;
DEFINE v_validar_3_primeros_campos             VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                             SMALLINT; -- clave de afore
-- id matriz derecho                           
DEFINE v_id_ret_matriz_derecho                 SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS                                    
                                               
DEFINE v_sumario_importe_total                 DECIMAL(22,2);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;

DEFINE v_numero_registros                      DECIMAL(9,0);
DEFINE v_total_imp72                           DECIMAL(22,2);
DEFINE v_total_tanto_imp72                     DECIMAL(22,2);
DEFINE v_saldo_cuenta                          DECIMAL(24,6);
                                               
DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo               
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 
DEFINE v_subcuenta                             SMALLINT;
                                               
-- conteo de rechazos e inserciones            
DEFINE v_reg_cza_insertados                    DECIMAL(9,0); -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    DECIMAL(9,0); -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    DECIMAL(9,0); -- total de registros de detalle rechazados

DEFINE v_reg_det_insert_15                     DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_insert_18                     DECIMAL(9,0); -- total de registros de detalle rechazados

-- codigos de error en encabezado
DEFINE v_error_cza_tpo_registro_invalido           SMALLINT;
DEFINE v_error_cza_id_servicio_invalido            SMALLINT;
DEFINE v_error_cza_id_operacion_invalido           SMALLINT;
DEFINE v_error_det_fecha_operacion_no_capturada    SMALLINT;

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado               SMALLINT;
DEFINE v_error_det_tpo_registro_invalido           SMALLINT;
DEFINE v_error_det_id_servicio_invalido            SMALLINT;
DEFINE v_error_det_id_operacion_invalido           SMALLINT;
DEFINE v_error_det_fecha_liquidacion_no_capturada  SMALLINT;
DEFINE v_error_det_tot_impviv72_no_capturado       SMALLINT;
DEFINE v_error_det_caso_adai_no_capturado          SMALLINT;
DEFINE v_error_det_tanto_no_es_doble_viv97         SMALLINT;

-- codigos de error en sumario
DEFINE v_error_sum_totales_no_coinciden          SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_fondo_ahorro                      INTEGER; -- 802 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION
     
   --SET DEBUG FILE TO "/safreviv_int/BD/fn_ret_integra_fondo_ahorro.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados              = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados              = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados              = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados              = 0; -- total de registros de detalle rechazados
   LET v_rec_tot_saldo_viv               = 0;
   LET v_ret_fondo_ahorro_id_afi_fondo72 = 0;   
   LET v_reg_det_insert_15               = 0;
   LET v_reg_det_insert_18               = 0;
   LET v_subcuenta                       = 40;
   LET v_encontrado_por_nss_rfc          = 0; -- forma en como se encuentra al NSS/RFC
   LET v_const_ENCONTRADO_NSS_RFC        = 300; -- se encontro con NSS y RFC
   LET v_const_ENCONTRADO_NSS            = 200; -- se encontro con NSS
   LET v_const_ENCONTRADO_RFC            = 100; -- se encontro con RFC

   
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';
   LET tmp_ret_det_nss = NULL;

   -- se inician los codigos de error en encabezado
   LET v_error_cza_tpo_registro_invalido            = 1;
   LET v_error_cza_id_servicio_invalido             = 2;
   LET v_error_cza_id_operacion_invalido            = 3;
   LET v_error_det_fecha_operacion_no_capturada     = 4;
    
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                = 49; -- NSS NO ENCONTRADO
   LET v_error_det_tpo_registro_invalido            = 6;
   LET v_error_det_id_servicio_invalido             = 7;
   LET v_error_det_id_operacion_invalido            = 8;
   LET v_error_det_fecha_liquidacion_no_capturada   = 9;
   LET v_error_det_tot_impviv72_no_capturado        = 10;
   LET v_error_det_caso_adai_no_capturado           = 11;
   LET v_error_det_tanto_no_es_doble_viv97          = 12;
   LET v_error_sum_totales_no_coinciden             = 13;

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_i_estado_marca     = 0;
   
   -- inicializo variables
   LET  v_solicitud = 0;
   LET  v_total_182 = 0;
   LET  v_total_422 = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio  = p_folio,
          estado = 2             -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;
   
   -- se cuentan los registros de la tabla temporal de detalle
   {SELECT COUNT(*)
   INTO v_numero_registros
   FROM safre_tmp:tmp_ret_det_fondo_ahorro
   WHERE tpo_mov_cta = 'V';
   --#Validacion no son todos los que integra
   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT SUM(imp_viv72),SUM( tanto_imp_viv72)
   INTO   v_total_imp72,v_total_tanto_imp72
   FROM   safre_tmp:tmp_ret_det_fondo_ahorro
   WHERE tpo_mov_cta = 'V';
   
   SELECT tot_pesos_viv72
   INTO   v_sumario_importe_total
   FROM   safre_tmp:tmp_ret_sum_fondo_ahorro
  
      
   -- si no coincide el total es un error
   IF ((v_total_imp72 + v_total_tanto_imp72) <> v_sumario_importe_total) THEN

      -- se rechaza el lote y no integra
      --RETURN v_error_cza_reg_totales_no_coinciden; -- codigo de error
      LET v_si_resultado = v_error_sum_totales_no_coinciden;
      LET v_c_msj = "No coinciden los montos cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
   END IF}

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
                                          id_derechohabiente  DECIMAL(9,0)
                                         ,id_codigo          SMALLINT
                                         ,codigo_rechazo     SMALLINT
                                         );
   -- se obtienen los datos del encabezado
   FOREACH
   SELECT 
       tpo_registro
      ,id_servicio
      ,tpo_operacion
      ,f_operacion
      ,resultado_operacion
      ,motivo_rech_1
      ,motivo_rech_2
   INTO
       tmp_ret_cza_tpo_registro
      ,tmp_ret_cza_id_servicio
      ,tmp_ret_cza_tpo_operacion
      ,tmp_ret_cza_f_operacion
      ,tmp_ret_cza_resultado_operacion
      ,tmp_ret_cza_motivo_rech_1
      ,tmp_ret_cza_motivo_rech_2
   FROM
      safre_tmp:tmp_ret_cza_fondo_ahorro
 
      -- se asume que no hay error
      LET v_b_rechazo_encabezado = 0;
      
      -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
 
      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;
      LET v_id_derechohabiente = 0;
      
      -- =========================================================
      -- validando tipo de registro
      IF ( tmp_ret_cza_tpo_registro <> "01" OR tmp_ret_cza_id_servicio <> "04" ) THEN
         -- se activa la bandera de rechazo de encabezado
         LET v_b_rechazo_encabezado = 1;
         --trace("Validando encabezado entro ");
      
         -- 1	Tipo de Registro	X	02	00	001	-	002	"01" Encabezado de lote
         IF ( tmp_ret_cza_tpo_registro <> "01" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_cza_tpo_registro_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
         IF ( tmp_ret_cza_id_servicio <> "04" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_cza_id_servicio_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      END IF
      
      -- =========================================================
      -- se verifica si el encabezado contiene fecha procesar
      IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
        INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_fecha_operacion_no_capturada);

        -- se incrementa el indice
        LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      

      -- =========================================================
       
      -- si hubo rechazo de encabezado no se inserta en historico
      IF ( v_b_rechazo_encabezado = 1 ) THEN

         -- se leen los tres primeros errores
         FOREACH
            SELECT FIRST 3 id_codigo,
                           codigo_rechazo
              INTO  v_indice_codigos_rechazo, v_codigos_rechazo
              FROM  tmp_codigos_rechazo
            ORDER BY   id_codigo
               
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
        
         -- se cuenta un encabezado rechazado
         LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;
         
         -- el registro fue rechazado y no se inserta en el historico
         CONTINUE FOREACH;

      END IF
      -- se cuenta un encabezado insertado
      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;
 
   END FOREACH;
   
   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;    


   -- se obtienen los datos del detalle
   FOREACH
   SELECT
       tpo_registro
      ,id_servicio
      ,id_operacion
      ,nss
      ,f_solicitud
      ,f_liquidacion
      ,cve_refer
      ,imp_viv72
      ,tanto_imp_viv72
      ,entidad
      ,causal_retiro
      ,caso_adai
      ,rfc_trabajador
      ,id_transpaso
      ,resultado_operacion
      ,motivo_rech_1
      ,motivo_rech_2
	  ,tpo_mov_cta --nuevo campo PRODINF-354 
   INTO
       tmp_ret_det_tpo_registro
      ,tmp_ret_det_id_servicio
      ,tmp_ret_det_id_operacion
      ,tmp_ret_det_nss
      ,tmp_ret_det_f_solicitud
      ,tmp_ret_det_f_liquidacion
      ,tmp_ret_det_cve_refer
      ,tmp_ret_det_imp_viv72
      ,tmp_ret_det_tanto_imp_viv72
      ,tmp_ret_det_entidad
      ,tmp_ret_det_causal_retiro
      ,tmp_ret_det_caso_adai
      ,tmp_ret_det_rfc
      ,tmp_ret_det_id_transpaso
      ,tmp_ret_det_resultado_operacion
      ,tmp_ret_det_motivo_rech_1
      ,tmp_ret_det_motivo_rech_2
      ,tmp_ret_det_tpo_mov_cta	  -- nuevo campo PRODINF-354 
   FROM safre_tmp:tmp_ret_det_fondo_ahorro  -- tabla para almacenar el detalle
      WHERE tpo_mov_cta = 'V'
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;
      LET v_id_derechohabiente = 0;

      -- ==========================================================================
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
	  LET v_id_afi_fondo72 = NULL;
	  LET v_encontrado_por_nss_rfc = 0; -- se inicia la variable de busqueda

      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1; --ERV revisar esta parte si es lo correcto que realiza dos cuentas de los rechaza     
      
      LET v_indice_codigos_rechazo = 1;
     
	  -- DIFERENTE DE TRASPASOS
	  IF ( tmp_ret_det_id_transpaso <> "T" ) THEN
	  
	     -- se busca por NSS-RFC
         SELECT FIRST 1 id_afi_fondo72 
         INTO   v_id_afi_fondo72
         FROM   afi_fondo72
         WHERE  nss = tmp_ret_det_nss
         AND    rfc = tmp_ret_det_rfc;

         LET v_ocurrencias = 0;
         SELECT count(id_afi_fondo72)
         INTO   v_ocurrencias
         FROM   afi_fondo72
         WHERE  nss = tmp_ret_det_nss
         AND    rfc = tmp_ret_det_rfc;
         
	     IF v_ocurrencias <> 1 THEN 
             LET v_id_afi_fondo72 = NULL;
         END IF
         -- si no se encontro al derechohabiente
         IF ( v_id_afi_fondo72 IS NULL ) THEN
		 
		    -- si no se tienen NSS
            IF ( tmp_ret_det_nss IS NULL ) THEN
			   -- se busca por RFC
               SELECT FIRST 1 id_afi_fondo72 
               INTO   v_id_afi_fondo72
               FROM   afi_fondo72
               WHERE  rfc = tmp_ret_det_rfc;

               LET v_ocurrencias = 0;
               SELECT count(id_afi_fondo72)
               INTO   v_ocurrencias
               FROM   afi_fondo72
               WHERE  rfc = tmp_ret_det_rfc;
               IF v_ocurrencias <> 1 THEN 
                   LET v_id_afi_fondo72 = NULL;
               END IF

			   
               IF ( v_id_afi_fondo72 IS NULL ) THEN 
                 -- se marca la bandera de rechazo de detalle
                 LET v_b_rechazo_detalle    = 1;
                 LET v_id_derechohabiente   = 0;
         
                 INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);
         
                 -- se incrementa el indice
                 LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
			   ELSE
			     -- se encontro por RFC
				 LET v_encontrado_por_nss_rfc = v_const_ENCONTRADO_RFC;
               END IF 
            ELSE 
			   -- se busca por NSS
               SELECT FIRST 1 id_afi_fondo72 
               INTO   v_id_afi_fondo72
               FROM   afi_fondo72
               WHERE  nss = tmp_ret_det_nss;

               LET v_ocurrencias = 0;
               SELECT count(id_afi_fondo72)
               INTO   v_ocurrencias
               FROM   afi_fondo72
               WHERE  nss = tmp_ret_det_nss;
               IF v_ocurrencias <> 1 THEN 
                   LET v_id_afi_fondo72 = NULL;
               END IF
               
			   -- si no se encontro
			   IF ( v_id_afi_fondo72 IS NULL ) THEN
			      -- se busca por rfc				  
                  SELECT FIRST 1 id_afi_fondo72 
                  INTO   v_id_afi_fondo72
                  FROM   afi_fondo72
                  WHERE  rfc = tmp_ret_det_rfc;

                  LET v_ocurrencias = 0;
                  SELECT count(id_afi_fondo72)
                  INTO   v_ocurrencias
                  FROM   afi_fondo72
                  WHERE  rfc = tmp_ret_det_rfc;
                  IF v_ocurrencias <> 1 THEN 
                      LET v_id_afi_fondo72 = NULL;
                  END IF
                  
                  -- si no se encontro				  
				  IF ( v_id_afi_fondo72 IS NULL ) THEN			   
                    -- se marca la bandera de rechazo de detalle
                    LET v_b_rechazo_detalle    = 1;
                    LET v_id_derechohabiente   = 0;
                    INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);
                    -- se incrementa el indice
                    LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
				  ELSE				   					
			        -- se encontro por RFC
				    LET v_encontrado_por_nss_rfc = v_const_ENCONTRADO_RFC;
				  END IF
			   ELSE
			      -- se encontro por NSS
			      LET v_encontrado_por_nss_rfc = v_const_ENCONTRADO_NSS;
			   END IF
            END IF
		 ELSE
		   -- se encontro el derechohabiente por nss-rfc
		   LET v_encontrado_por_nss_rfc = v_const_ENCONTRADO_NSS_RFC;
         END IF


	  END IF

--trace "encontrado por NSS/RFC: " || v_encontrado_por_nss_rfc;
	  
      LET v_validar_3_primeros_campos = tmp_ret_det_tpo_registro || tmp_ret_det_id_servicio || tmp_ret_det_id_operacion;
      
      -- si la concatenacion no es igual a la esperada, entonces algun campo es incorrecto
      IF ( v_validar_3_primeros_campos <> "030419" ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
                  
                  
         -- 1	Tipo de Registro	X	02	00	001	-	002	03 Detalle Transacciones
         IF ( tmp_ret_det_tpo_registro <> "03" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 2	Identificador de Servicio	X	02	00	003	-	004	04 Retiros
         IF ( tmp_ret_det_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 3	Identificador de Operación	X	02	00	005	-	006	19 RETIRO FONDO AHORRO
         IF ( tmp_ret_det_id_operacion <> "19" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_id_operacion_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
      END IF

      -- la fecha de liquidacion debe existir 
      IF ( tmp_ret_det_f_liquidacion IS NULL ) THEN
         -- se rechaza
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_fecha_liquidacion_no_capturada);
      
         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      
      END IF

      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN
--trace "registro rechazado";
      
         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;
         
         -- se leen los tres primeros errores
         FOREACH
            SELECT FIRST 3 id_codigo,  codigo_rechazo
            INTO   v_indice_codigos_rechazo, v_codigos_rechazo
            FROM   tmp_codigos_rechazo
            ORDER  BY id_codigo
            
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

      END IF

      -- ==========================================================================
	  -- se verifica el saldo segun se haya encontrado al trabajador
      LET v_id_derechohabiente = NULL;
	  IF ( v_encontrado_por_nss_rfc <> 0) THEN
	     -- buscando por NSS/RFC
         SELECT SUM(NVL(importe,0))
         INTO   v_rec_tot_saldo_viv
         FROM   afi_fondo72 afi
                LEFT OUTER JOIN cta_fondo72 cta
                             ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                            AND cta.subcuenta      =  v_subcuenta
                            AND cta.movimiento     <> 422  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
         WHERE afi.id_afi_fondo72 = v_id_afi_fondo72;

		 
         SELECT FIRST 1 id_derechohabiente 
         INTO   v_id_derechohabiente
         FROM   afi_fondo72
         WHERE  id_afi_fondo72 = v_id_afi_fondo72
         AND    nss <> "00000000000"
         AND    id_derechohabiente IS NOT NULL; 
	  END IF

      -- si no se encontro id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se verifica si esta en la tabla de id_derechohabientes nuevos
         SELECT FIRST 1 ad.id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente ad, afi_fondo72 af
         WHERE  af.id_afi_fondo72 = v_id_afi_fondo72
         AND    af.nss            = ad.nss  --tmp_ret_det_nss
         AND    af.nss           <> "00000000000"
         AND    af.rfc            = ad.rfc;  --tmp_ret_det_rfc;

         -- si no se encontro
         IF ( v_id_derechohabiente IS NULL ) THEN
             SELECT FIRST 1 ad.id_derechohabiente
             INTO   v_id_derechohabiente
             FROM   afi_derechohabiente ad, afi_fondo72 af
             WHERE  af.id_afi_fondo72 = v_id_afi_fondo72
             AND    af.nss            = "00000000000"
             AND    af.rfc            = ad.rfc;  --tmp_ret_det_rfc;

             IF ( v_id_derechohabiente IS NULL ) THEN
                 SELECT FIRST 1 id_derechohabiente
                 INTO   v_id_derechohabiente
                 FROM   afi_fondo72_d 
                 WHERE  nss            = tmp_ret_det_nss
                 AND    rfc            = tmp_ret_det_rfc;

                 IF ( v_id_derechohabiente IS NULL ) THEN
                     --trace "se busca en la afi_fondo72_d";
                     -- 11jul2013. Se crea el id_derechohabiente usando la secuencia de afi_derechohabiente
                     -- en una tabla donde se relacione el nss, rfc, e id_derechohabiente creado
                     SELECT seq_derechohabiente.nextVal
                     INTO   v_id_derechohabiente
                     FROM   systables
                     WHERE  tabid = 1;

                     -- se asigna el id_derechohabiente a la tabla de relacion
                     INSERT INTO afi_fondo72_d (nss, rfc, id_derechohabiente)
                     VALUES (tmp_ret_det_nss,tmp_ret_det_rfc,v_id_derechohabiente);
                 END IF 
             END IF
         END IF
         IF (v_id_derechohabiente IS NOT NULL ) THEN
             -- Actualiza la tabla afi_fondo72 con el id_derechohabiente encontrado
             UPDATE afi_fondo72
                SET id_derechohabiente = v_id_derechohabiente
              WHERE id_afi_fondo72 = v_id_afi_fondo72;
         END IF

      END IF 

--trace "id der luego de buscarlo: " || v_id_derechohabiente;
      -- se obtiene el id_solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
       
      -- se asignan los datos al registro de rechazo de detalle                   
      LET ret_fondo_ahorro_id_solicitud       = v_id_solicitud                ;     
      LET ret_fondo_ahorro_id_derechohabiente = v_id_derechohabiente          ;
	  LET ret_fondo_ahorro_nss                = tmp_ret_det_nss;
	  LET ret_fondo_ahorro_rfc                = tmp_ret_det_rfc;
      LET ret_fondo_ahorro_f_solicitud        = tmp_ret_det_f_solicitud       ;     
      LET ret_fondo_ahorro_causal_retiro      = tmp_ret_det_causal_retiro     ;     
      LET ret_fondo_ahorro_id_datamart        = "0"                           ;     
      LET ret_fondo_ahorro_folio              = p_folio                       ;     
      LET ret_fondo_ahorro_cve_referencia     = tmp_ret_det_cve_refer         ;     
      LET ret_fondo_ahorro_saldo_viv72        = tmp_ret_det_imp_viv72 / 100   ;
      LET ret_fondo_ahorro_caso_adai          = tmp_ret_det_caso_adai         ;
      LET ret_fondo_ahorro_entidad_federativa = tmp_ret_det_entidad           ;
	  -- campos agregados 09ene2013 (f_liquidacion,id_traspaso)
	  LET ret_fondo_ahorro_f_liquidacion      = tmp_ret_det_f_liquidacion     ;
	  LET ret_fondo_ahorro_id_traspaso        = tmp_ret_det_id_transpaso      ;	  
      LET ret_fondo_ahorro_f_captura          = tmp_ret_cza_f_operacion       ;   
      LET ret_fondo_ahorro_h_captura          = CURRENT HOUR TO SECOND        ;
      LET ret_fondo_ahorro_usuario            = p_usuario_cod                 ;
      LET ret_fondo_ahorro_tpo_mov_cta        = tmp_ret_det_tpo_mov_cta; --PRODINF 354
      -- si se tiene tanto adicional
      IF ( tmp_ret_det_tanto_imp_viv72 > 0 ) THEN
         LET ret_fondo_ahorro_tanto_adicional = tmp_ret_det_tanto_imp_viv72 / 100;
      ELSE 
      	 LET ret_fondo_ahorro_tanto_adicional  = 0;
      END IF
        
      LET v_sumario_importe_total = ret_fondo_ahorro_saldo_viv72 + ret_fondo_ahorro_tanto_adicional;
      -- # Se comenta porque no lo necesitamos en la integracion 
      -- si el saldo de vivienda es igual al total
      {IF ( ret_fondo_ahorro_saldo_viv72 = v_rec_tot_saldo_viv ) THEN
	     -- si no fue rechazado
         IF ( v_b_rechazo_detalle = 0 ) THEN 
            IF ( ret_fondo_ahorro_tanto_adicional = ret_fondo_ahorro_saldo_viv72 
                 OR ret_fondo_ahorro_tanto_adicional = 0 ) THEN 
               LET ret_fondo_ahorro_estado_solicitud   = "15" ;
               LET ret_fondo_ahorro_cod_rechazo        = "0"  ;
               LET v_reg_det_insert_15                 = v_reg_det_insert_15 + 1;
            ELSE                                                                                   
               LET ret_fondo_ahorro_estado_solicitud   = "100" ;
               LET ret_fondo_ahorro_cod_rechazo        = v_error_det_tanto_no_es_doble_viv97  ;
            END IF
         ELSE 
            LET ret_fondo_ahorro_estado_solicitud   = "100" ;
            LET ret_fondo_ahorro_cod_rechazo        = v_codigos_rechazo ;
         END IF 
      ELSE
	     -- si no fue rechazado
         IF ( v_b_rechazo_detalle = 0 ) THEN 
            LET ret_fondo_ahorro_estado_solicitud       = "15" ; --saldo solicitado diferente al existente se ajusta con la pantalla de ejuste de saldo
            LET ret_fondo_ahorro_cod_rechazo            = "0"  ;  
            --LET v_reg_det_insert_18                     = v_reg_det_insert_18 + 1;
         ELSE 
            LET ret_fondo_ahorro_estado_solicitud   = "100" ;
            LET ret_fondo_ahorro_cod_rechazo        = v_codigos_rechazo ;
         END IF 
      END IF }

      -- se marca la cuenta
      {LET v_i_estado_marca = 0;
      --#No se marca la cuenta se comenta
      -- si la solicitud esta aceptada se marca la cuenta 
      IF ( ret_fondo_ahorro_estado_solicitud = 15 ) THEN 
         --trace("genera marca" );
         EXECUTE FUNCTION fn_marca_cuenta(
                ret_fondo_ahorro_id_derechohabiente
               ,v_marca_fondo_ahorro -- marca de fondo de ahorro
               ,ret_fondo_ahorro_id_solicitud  
               ,p_folio
               ,0 -- estado marca
               ,0 -- codigo de rechazo
               ,0 -- marca de la causa
               ,NULL -- fecha de la causa
               ,p_usuario_cod
               ,p_proceso_cod)
            INTO v_i_estado_marca;
               
         
         -- si no se pudo marcar, se rechaza el registro
         IF ( v_i_estado_marca > 0 ) THEN
            LET ret_fondo_ahorro_estado_solicitud = 100; -- rechazada
            LET ret_fondo_ahorro_cod_rechazo      = 54; -- marca no convive
         END IF
      END IF }
	  
      --#busco con el caso adai en ret_fondo_ahorro
	     LET ret_fondo_ahorro_estado_solicitud =15;
		 LET ret_fondo_ahorro_cod_rechazo = 0;
		 
		 SELECT MAX(id_solicitud)
         INTO   v_solicitud
         FROM   ret_fondo_ahorro
         WHERE caso_adai =  ret_fondo_ahorro_caso_adai;
         
         SELECT importe
         INTO v_total_182
		 FROM cta_fondo72 cta
		 WHERE id_referencia = v_solicitud
		 AND cta.subcuenta      =  v_subcuenta
         AND f_liquida = tmp_ret_det_f_liquidacion
		 AND cta.movimiento     = 182  	;	 	 
		 
		 SELECT importe
		 INTO v_total_422
		 FROM cta_fondo72 cta
		 WHERE id_referencia = v_solicitud
		 AND cta.subcuenta      =  v_subcuenta
		 AND f_liquida = tmp_ret_det_f_liquidacion
         AND cta.movimiento     = 422  ;
		 
		 IF v_total_182 <> tmp_ret_det_imp_viv72 OR  v_total_422 <> tmp_ret_det_tanto_imp_viv72  THEN
         		LET ret_fondo_ahorro_estado_solicitud = 100; -- rechazada
                LET ret_fondo_ahorro_cod_rechazo      = 54; -- marca no convive
         END IF
			 	 
		 
	  --#termina
	  
      -- se inserta en la tabla historia de detalle de retiro de fondo ahorro
      INSERT INTO ret_fondo_ahorro( 
                              id_solicitud
                             ,id_derechohabiente
							 ,nss
							 ,rfc
                             ,f_solicitud
                             ,estado_solicitud
                             ,causal_retiro
                             ,id_datamart
                             ,folio
                             ,cve_referencia
                             ,saldo_viv72
                             ,tanto_adicional
                             ,caso_adai
                             ,entidad_federativa
							 ,f_liquidacion
							 ,id_traspaso
                             ,f_captura
                             ,h_captura
                             ,usuario
                             ,cod_rechazo
							 ,tpo_mov_cta --nuevo campo PRODINF-354 
                             )
         VALUES (
                 ret_fondo_ahorro_id_solicitud
                ,ret_fondo_ahorro_id_derechohabiente
				,ret_fondo_ahorro_nss
				,ret_fondo_ahorro_rfc
                ,ret_fondo_ahorro_f_solicitud
                ,ret_fondo_ahorro_estado_solicitud
                ,ret_fondo_ahorro_causal_retiro
                ,ret_fondo_ahorro_id_datamart
                ,ret_fondo_ahorro_folio
                ,ret_fondo_ahorro_cve_referencia
                ,ret_fondo_ahorro_saldo_viv72
                ,ret_fondo_ahorro_tanto_adicional
                ,ret_fondo_ahorro_caso_adai
                ,ret_fondo_ahorro_entidad_federativa
				,ret_fondo_ahorro_f_liquidacion
				,ret_fondo_ahorro_id_traspaso
                ,ret_fondo_ahorro_f_captura
                ,ret_fondo_ahorro_h_captura
                ,ret_fondo_ahorro_usuario
                ,ret_fondo_ahorro_cod_rechazo
				,ret_fondo_ahorro_tpo_mov_cta --Nvo campo PRODINF-354
                );
    
      -- se buscan las coincidencias de trabajadores con el NSS y el RFC dados
	  -- se verifica el saldo segun se haya encontrado al trabajador
	  IF ( v_encontrado_por_nss_rfc <> 0) THEN
	     -- buscando por NSS/RFC
         FOREACH 
            SELECT afi.id_afi_fondo72,
                   SUM(NVL(importe,0))
            INTO   v_ret_fondo_ahorro_id_afi_fondo72,
                   v_rec_tot_saldo_viv
            FROM   afi_fondo72 afi 
                   LEFT OUTER JOIN cta_fondo72 cta
                                ON afi.id_afi_fondo72 = cta.id_afi_fondo72
                               AND cta.subcuenta      = v_subcuenta
                               AND cta.movimiento    <> 422 -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONA
            WHERE  afi.id_afi_fondo72 IS NOT NULL
            AND    afi.id_afi_fondo72 = v_id_afi_fondo72
            GROUP BY afi.id_afi_fondo72
            
            IF ( v_ret_fondo_ahorro_id_afi_fondo72 IS NOT NULL ) THEN
               -- 24Ene2012. Si la cuenta esta sobregirada se vuelve a sobregirar
               IF ( v_rec_tot_saldo_viv < 0 ) THEN
                  LET v_rec_tot_saldo_viv = 0;
               END IF
             
               INSERT INTO ret_det_fondo72 ( 
                  id_afi_fondo72
                  ,id_solicitud
                  ,saldo_viv72
                  ,tanto_adicional
                  ,id_datamart
                  ,f_saldo
                  ,h_saldo
                  ,estado_detalle
                  ,cod_rechazo)
               VALUES ( 
                  v_ret_fondo_ahorro_id_afi_fondo72
                  ,ret_fondo_ahorro_id_solicitud
                  ,v_rec_tot_saldo_viv
                  ,ret_fondo_ahorro_tanto_adicional
                  ,ret_fondo_ahorro_id_datamart
                  ,ret_fondo_ahorro_f_captura
                  ,ret_fondo_ahorro_h_captura
                  ,"1"
                  ,ret_fondo_ahorro_cod_rechazo
               );
            END IF 
            
            -- se cuenta un registro insertado
            LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados
         
         END FOREACH;
      END IF
    
  END FOREACH;
   
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_fondo_ahorro;
   UPDATE STATISTICS FOR TABLE ret_det_fondo72;

   -- si no hubo error
   IF ( v_si_resultado = 0 ) THEN 
      -- si se insertaron registro
      IF ( v_reg_det_insert_15 > 0 ) THEN  
         LET isam_err =  v_reg_det_insertados;
      END IF
   END IF 

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


