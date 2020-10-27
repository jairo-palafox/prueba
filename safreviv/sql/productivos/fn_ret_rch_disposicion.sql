






CREATE FUNCTION "safreviv".fn_ret_rch_disposicion(p_folio DECIMAL(9,0), p_marca_disposicion INTEGER,
                                       p_usuario_cod CHAR(20), p_proceso_cod SMALLINT)
RETURNING INTEGER, INTEGER, VARCHAR(250)

-- encabezado de la tabla historica/integrada de retiros por disposicion de recursos
DEFINE ret_cza_disposicion_folio                 DECIMAL(9,0)           ;
DEFINE ret_cza_disposicion_nombre_archivo        CHAR(20)               ;
DEFINE ret_cza_disposicion_f_operacion_procesar  DATE                   ;
DEFINE ret_cza_disposicion_f_carga               DATE                   ; -- esperando cambio de nombre antes: f_carga_infonavit
DEFINE ret_cza_disposicion_h_carga               DATETIME HOUR TO SECOND;
DEFINE ret_cza_disposicion_f_valor_transferencia DATE                   ;
DEFINE ret_cza_disposicion_precio_fondo          DECIMAL(14,6)          ;
DEFINE ret_cza_disposicion_total_registros       INTEGER                ; -- nuevo
DEFINE ret_cza_disposicion_total_importe         DECIMAL(18,6)          ; -- nuevo
DEFINE ret_cza_disposicion_usuario               CHAR(20)               ;

-- detalle de la tabla historica/integrada de retiros por disposicion de recursos
-- ret_disposicion
DEFINE ret_disposicion_id_solicitud           DECIMAL(9,0) ;
DEFINE ret_disposicion_id_derechohabiente     DECIMAL(9,0) ;
DEFINE ret_disposicion_f_solicitud            DATE         ;
DEFINE ret_disposicion_id_ret_matriz_derecho  SMALLINT     ;
DEFINE ret_disposicion_sec_pension            CHAR(2)      ;
DEFINE ret_disposicion_diag_registro          CHAR(3)      ;
DEFINE ret_disposicion_folio                  DECIMAL(9,0) ;
DEFINE ret_disposicion_curp                   CHAR(18)     ;
DEFINE ret_disposicion_nombre_afore           CHAR(40)     ;
DEFINE ret_disposicion_paterno_afore          CHAR(40)     ;
DEFINE ret_disposicion_materno_afore          CHAR(40)     ;
DEFINE ret_disposicion_f_inicio_pension       DATE         ;
DEFINE ret_disposicion_f_resolucion           DATE         ;
DEFINE ret_disposicion_porcentaje_valuacion   DECIMAL(5,2) ;
DEFINE ret_disposicion_semanas_cotizadas      INTEGER      ;
DEFINE ret_disposicion_cve_doc_probatorio     SMALLINT     ;
DEFINE ret_disposicion_f_nacimiento           DATE         ;
DEFINE ret_disposicion_aseguradora            CHAR(3)      ;
DEFINE ret_disposicion_actuario               CHAR(7)      ;
DEFINE ret_disposicion_num_plan_privado       CHAR(8)      ;
DEFINE ret_disposicion_periodo_primer_pago    INTEGER      ;
DEFINE ret_disposicion_aivs_ret97             DECIMAL(14,6);
DEFINE ret_disposicion_aivs_cv                DECIMAL(14,6);
DEFINE ret_disposicion_aivs_cs                DECIMAL(14,6);
DEFINE ret_disposicion_aivs_sar92             DECIMAL(14,6);
DEFINE ret_disposicion_aivs_viv97             DECIMAL(14,6);
DEFINE ret_disposicion_aivs_viv92             DECIMAL(14,6);
DEFINE ret_disposicion_consec_trabajador      DECIMAL(11,0);
DEFINE ret_disposicion_importe_viv72          DECIMAL(14,2);
DEFINE ret_disposicion_estado_sub_viv         SMALLINT     ;
DEFINE ret_disposicion_cve_afore              SMALLINT     ;
DEFINE ret_disposicion_cod_rechazo            SMALLINT     ;
DEFINE ret_disposicion_estado_solicitud       SMALLINT     ;

-- =============================================================================
-- rechazo de encabezado
-- ret_cza_disposicion_rch
DEFINE ret_cza_disposicion_rch_folio                 DECIMAL(9,0)           ;
DEFINE ret_cza_disposicion_rch_nombre_archivo        CHAR(20)               ;
DEFINE ret_cza_disposicion_rch_f_operacion_procesar  DATE                   ;
DEFINE ret_cza_disposicion_rch_f_carga               DATE                   ;
DEFINE ret_cza_disposicion_rch_h_carga               DATETIME HOUR TO SECOND;
DEFINE ret_cza_disposicion_rch_f_valor_transferencia DATE                   ;
DEFINE ret_cza_disposicion_rch_precio_fondo          DECIMAL(14,6)          ;
DEFINE ret_cza_disposicion_rch_total_registros       INTEGER                ;
DEFINE ret_cza_disposicion_rch_total_importe         DECIMAL(18,6)          ;
DEFINE ret_cza_disposicion_rch_usuario               CHAR(20)               ;
DEFINE ret_cza_disposicion_rch_resultado_operacion   SMALLINT               ;
DEFINE ret_cza_disposicion_rch_cod_rechazo_1         SMALLINT               ;
DEFINE ret_cza_disposicion_rch_cod_rechazo_2         SMALLINT               ;
DEFINE ret_cza_disposicion_rch_cod_rechazo_3         SMALLINT               ;

-- =============================================================================
-- rechazo de detalle
-- ret_disposicion_rch
DEFINE ret_dispos_rch_id_solicitud         decimal(9,0) ;
DEFINE ret_dispos_rch_id_derechohabiente   decimal(9,0) ;
DEFINE ret_dispos_rch_f_solicitud          date         ;
DEFINE ret_dispos_rch_folio                decimal(9,0) ;
DEFINE ret_dispos_rch_nss                  char(11)     ;
DEFINE ret_dispos_rch_curp                 char(18)     ;
DEFINE ret_dispos_rch_nombre_afore         char(40)     ;
DEFINE ret_dispos_rch_paterno_afore        char(40)     ;
DEFINE ret_dispos_rch_materno_afore        char(40)     ;
DEFINE ret_dispos_rch_sec_pension          char(2)      ;
DEFINE ret_dispos_rch_tipo_retiro          char(1)      ;
DEFINE ret_dispos_rch_regimen              char(2)      ;
DEFINE ret_dispos_rch_tpo_seguro           char(2)      ;
DEFINE ret_dispos_rch_tpo_pension          char(2)      ;
DEFINE ret_dispos_rch_tpo_prestacion       char(2)      ;
DEFINE ret_dispos_rch_f_inicio_pension     date         ;
DEFINE ret_dispos_rch_f_resolucion         date         ;
DEFINE ret_dispos_rch_porcentaje_valuacion decimal(5,2) ;
DEFINE ret_dispos_rch_semanas_cotizadas    integer      ;
DEFINE ret_dispos_rch_cve_doc_probatorio   smallint     ;
DEFINE ret_dispos_rch_f_nacimiento         date         ;
DEFINE ret_dispos_rch_aseguradora          char(3)      ;
DEFINE ret_dispos_rch_actuario             char(7)      ;
DEFINE ret_dispos_rch_num_plan_privado     char(8)      ;
DEFINE ret_dispos_rch_periodo_primer_pago  integer      ;
DEFINE ret_dispos_rch_aivs_ret_97          decimal(14,6);
DEFINE ret_dispos_rch_aivs_cv              decimal(14,6);
DEFINE ret_dispos_rch_aivs_cs              decimal(14,6);
DEFINE ret_dispos_rch_aivs_sar92           decimal(14,6);
DEFINE ret_dispos_rch_aivs_viv97           decimal(14,6);
DEFINE ret_dispos_rch_aivs_viv92           decimal(14,6);
DEFINE ret_dispos_rch_consec_trabajador    decimal(11,0);
DEFINE ret_dispos_rch_importe_viv72        decimal(14,2);
DEFINE ret_dispos_rch_diag_registro        char(3)      ;
DEFINE ret_dispos_rch_estado_sub_viv       smallint     ;
DEFINE ret_dispos_rch_cve_afore            smallint     ;
DEFINE ret_dispos_rch_estado_solicitud     smallint     ;
DEFINE ret_dispos_rch_resultado_operacion  smallint     ;
DEFINE ret_dispos_rch_cod_rechazo_1        smallint     ;
DEFINE ret_dispos_rch_cod_rechazo_2        smallint     ;
DEFINE ret_dispos_rch_cod_rechazo_3        smallint     ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);
DEFINE v_i_estado_marca                     INTEGER;

-- variables para control de excepciones
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_mensaje    VARCHAR(250);


   -- en caso de ocurrir un error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje
      RETURN v_sql_error, v_isam_error, v_mensaje;
   END EXCEPTION;
   
   
   -- se asume que no hay error
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_mensaje    = "Rechazo de lote realizado correctamente";
   LET v_i_estado_marca = 0;
   
   -- se rechaza el lote por haber encontrado error
   
   -- ====================================================================
   -- RECHAZO DEL ENCABEZADO
   -- ====================================================================
   SELECT 
      folio                 ,
      nombre_archivo        ,
      f_operacion_procesar  ,
      f_carga               ,
      h_carga               ,
      f_valor_transferencia , 
      precio_fondo          ,
	    total_registros       ,
	    total_importe         ,
      usuario               
   INTO 
      ret_cza_disposicion_folio                 ,
      ret_cza_disposicion_nombre_archivo        ,
      ret_cza_disposicion_f_operacion_procesar  ,
      ret_cza_disposicion_f_carga               ,
      ret_cza_disposicion_h_carga               ,
      ret_cza_disposicion_f_valor_transferencia , 
      ret_cza_disposicion_precio_fondo          , 
	    ret_cza_disposicion_total_registros       ,
	    ret_cza_disposicion_total_importe         ,
      ret_cza_disposicion_usuario               
   FROM ret_cza_disposicion
   WHERE folio = p_folio;
        
   -- se llenan los datos generales de rechazo del encabezado
   LET ret_cza_disposicion_rch_folio                 = ret_cza_disposicion_folio;
   LET ret_cza_disposicion_rch_f_operacion_procesar  = ret_cza_disposicion_f_operacion_procesar;
   LET ret_cza_disposicion_rch_nombre_archivo        = ret_cza_disposicion_nombre_archivo;
   LET ret_cza_disposicion_rch_f_carga               = ret_cza_disposicion_f_carga;
   LET ret_cza_disposicion_rch_h_carga               = ret_cza_disposicion_h_carga;
   LET ret_cza_disposicion_rch_f_valor_transferencia = ret_cza_disposicion_f_valor_transferencia;
   LET ret_cza_disposicion_rch_precio_fondo          = ret_cza_disposicion_precio_fondo;
   LET ret_cza_disposicion_rch_total_registros       = ret_cza_disposicion_total_registros;
   LET ret_cza_disposicion_rch_total_importe         = ret_cza_disposicion_total_importe;
   LET ret_cza_disposicion_rch_usuario               = ret_cza_disposicion_usuario;
   LET ret_cza_disposicion_rch_resultado_operacion   = 2; -- rechazado

   -- se indica que se rechazo por validacion de lote
   LET ret_cza_disposicion_rch_cod_rechazo_1  = 9; -- poner un valor de rechazo por validacion de lote
   LET ret_cza_disposicion_rch_cod_rechazo_2  = 0;
   LET ret_cza_disposicion_rch_cod_rechazo_3  = 0;

   
   -- se inserta el rechazo
   INSERT INTO ret_cza_disposicion_rch (
      folio                 ,
      f_operacion_procesar  ,
      nombre_archivo        ,
      f_carga               ,
      hora_carga            ,
      f_valor_transferencia ,
      precio_fondo          ,
      total_registros       ,
      total_importe         ,
      usuario               ,
      resultado_operacion   ,
      cod_rechazo_1         ,
      cod_rechazo_2         ,
      cod_rechazo_3         
   )
   VALUES (
      ret_cza_disposicion_rch_folio                 ,
      ret_cza_disposicion_rch_f_operacion_procesar  ,
      ret_cza_disposicion_rch_nombre_archivo        ,
      ret_cza_disposicion_rch_f_carga               ,
      ret_cza_disposicion_rch_h_carga               ,
      ret_cza_disposicion_rch_f_valor_transferencia ,
      ret_cza_disposicion_rch_precio_fondo          ,
      ret_cza_disposicion_rch_total_registros       ,
      ret_cza_disposicion_rch_total_importe         ,
      ret_cza_disposicion_rch_usuario               ,
      ret_cza_disposicion_rch_resultado_operacion   ,
      ret_cza_disposicion_rch_cod_rechazo_1         ,
      ret_cza_disposicion_rch_cod_rechazo_2         ,
      ret_cza_disposicion_rch_cod_rechazo_3         
   );
      
   -- ==================================================================
   -- RECHAZO DE DETALLE DEL LOTE
      
   -- se borran todos los registros de la tabla de detalle y se transfieren a la tabla de rechazos
   -- porque un rechazo implica el rechazo del lote completo
   FOREACH
   SELECT 
      id_solicitud          ,                              
      id_derechohabiente    ,  
      f_solicitud           ,  
      id_ret_matriz_derecho ,  
      sec_pension           ,  
      diag_registro         ,  
      folio                 ,  
      curp                  ,  
      nombre_afore          ,  
      paterno_afore         ,  
      materno_afore         ,  
      f_inicio_pension      ,  
      f_resolucion          ,  
      porcentaje_valuacion  ,  
      semanas_cotizadas     ,  
      cve_doc_probatorio    ,  
      f_nacimiento          ,  
      aseguradora           ,  
      actuario              ,  
      num_plan_privado      ,  
      periodo_primer_pago   ,  
      aivs_viv97            ,  
      aivs_viv92            ,  
      consec_trabajador     ,  
      importe_viv72         ,  
      estado_sub_viv        ,  
      cve_afore             ,  
      cod_rechazo           ,  
      estado_solicitud         
   INTO
      v_id_solicitud                        ,                            
      ret_disposicion_id_derechohabiente    ,
      ret_disposicion_f_solicitud           ,
      ret_disposicion_id_ret_matriz_derecho ,
      ret_disposicion_sec_pension           ,
      ret_disposicion_diag_registro         ,
      ret_disposicion_folio                 ,
      ret_disposicion_curp                  ,
      ret_disposicion_nombre_afore          ,
      ret_disposicion_paterno_afore         ,
      ret_disposicion_materno_afore         ,
      ret_disposicion_f_inicio_pension      ,
      ret_disposicion_f_resolucion          ,
      ret_disposicion_porcentaje_valuacion  ,
      ret_disposicion_semanas_cotizadas     ,
      ret_disposicion_cve_doc_probatorio    ,
      ret_disposicion_f_nacimiento          ,
      ret_disposicion_aseguradora           ,
      ret_disposicion_actuario              ,
      ret_disposicion_num_plan_privado      ,
      ret_disposicion_periodo_primer_pago   ,
      ret_disposicion_aivs_viv97            ,
      ret_disposicion_aivs_viv92            ,
      ret_disposicion_consec_trabajador     ,
      ret_disposicion_importe_viv72         ,
      ret_disposicion_estado_sub_viv        ,
      ret_disposicion_cve_afore             ,
      ret_disposicion_cod_rechazo           ,
      ret_disposicion_estado_solicitud       
   FROM  ret_disposicion
   WHERE folio = p_folio

      -- se desmarca la cuenta en cuestion
      LET v_i_estado_marca = 0;
      EXECUTE FUNCTION fn_desmarca_cuenta(
              ret_disposicion_id_derechohabiente
             ,p_marca_disposicion -- marca de disposicion
             ,v_id_solicitud -- identificador de registro de archivo o lote
             ,40 -- estado marca / rechazo por validacion
             ,p_marca_disposicion -- marca de la causa / se usa la misma por ser por validacion
             ,p_usuario_cod
             ,p_proceso_cod)
         INTO v_i_estado_marca;

      -- se obtiene el nss
      SELECT nss
      INTO   ret_dispos_rch_nss
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = ret_disposicion_id_derechohabiente;
      
      -- se obtiene la combinacion de retiro
      SELECT
         tpo_retiro      ,
         regimen         ,
         tpo_seguro      ,
         tpo_pension     ,
         tpo_prestacion  
      INTO
         ret_dispos_rch_tipo_retiro    ,
         ret_dispos_rch_regimen        ,
         ret_dispos_rch_tpo_seguro     ,
         ret_dispos_rch_tpo_pension    ,
         ret_dispos_rch_tpo_prestacion 
      FROM  ret_matriz_derecho
      WHERE id_ret_matriz_derecho = ret_disposicion_id_ret_matriz_derecho;
      

      -- se asignan los datos al registro de rechazo
      LET ret_dispos_rch_id_solicitud          = v_id_solicitud; 
      LET ret_dispos_rch_id_derechohabiente    = ret_disposicion_id_derechohabiente;
      LET ret_dispos_rch_f_solicitud           = ret_disposicion_f_solicitud;
      LET ret_dispos_rch_folio                 = p_folio;
      LET ret_dispos_rch_curp                  = ret_disposicion_curp;
      LET ret_dispos_rch_nombre_afore          = ret_disposicion_nombre_afore;
      LET ret_dispos_rch_paterno_afore         = ret_disposicion_paterno_afore;
      LET ret_dispos_rch_materno_afore         = ret_disposicion_materno_afore;
      LET ret_dispos_rch_sec_pension           = ret_disposicion_sec_pension;
      LET ret_dispos_rch_f_inicio_pension      = ret_disposicion_f_inicio_pension;
      LET ret_dispos_rch_f_resolucion          = ret_disposicion_f_resolucion;
      LET ret_dispos_rch_porcentaje_valuacion  = ret_disposicion_porcentaje_valuacion;
      LET ret_dispos_rch_semanas_cotizadas     = ret_disposicion_semanas_cotizadas;
      LET ret_dispos_rch_cve_doc_probatorio    = ret_disposicion_cve_doc_probatorio;
      LET ret_dispos_rch_f_nacimiento          = ret_disposicion_f_nacimiento;
      LET ret_dispos_rch_aseguradora           = ret_disposicion_aseguradora;
      LET ret_dispos_rch_actuario              = ret_disposicion_actuario;
      LET ret_dispos_rch_num_plan_privado      = ret_disposicion_num_plan_privado;
      LET ret_dispos_rch_periodo_primer_pago   = ret_disposicion_periodo_primer_pago;
      LET ret_dispos_rch_aivs_viv97            = ret_disposicion_aivs_viv97;
      LET ret_dispos_rch_aivs_viv92            = ret_disposicion_aivs_viv92;
      LET ret_dispos_rch_consec_trabajador     = ret_disposicion_consec_trabajador;
      LET ret_dispos_rch_importe_viv72         = ret_disposicion_importe_viv72;
      LET ret_dispos_rch_diag_registro         = ret_disposicion_diag_registro;
      LET ret_dispos_rch_estado_sub_viv        = ret_disposicion_estado_sub_viv;
      LET ret_dispos_rch_cve_afore             = ret_disposicion_cve_afore;
      LET ret_dispos_rch_estado_solicitud      = 0; -- clave de rechazo por lote
      LET ret_dispos_rch_resultado_operacion   = 2; -- rechazado
      LET ret_dispos_rch_cod_rechazo_1         = 9;
      LET ret_dispos_rch_cod_rechazo_2         = 0;
      LET ret_dispos_rch_cod_rechazo_3         = 0;

      -- se inserta el registro de rechazo
      INSERT INTO ret_disposicion_rch(
         id_solicitud         ,     
         id_derechohabiente   ,     
         f_solicitud          ,     
         folio                ,     
         nss                  ,     
         curp                 ,     
         nombre_afore         ,     
         paterno_afore        ,     
         materno_afore        ,     
         sec_pension          ,     
         tipo_retiro          ,     
         regimen              ,     
         tpo_seguro           ,     
         tpo_pension          ,     
         tpo_prestacion       ,     
         f_inicio_pension     ,     
         f_resolucion         ,     
         porcentaje_valuacion ,     
         semanas_cotizadas    ,     
         cve_doc_probatorio   ,     
         f_nacimiento         ,     
         aseguradora          ,     
         actuario             ,     
         num_plan_privado     ,     
         periodo_primer_pago  ,     
         aivs_viv97           ,     
         aivs_viv92           ,     
         consec_trabajador    ,     
         importe_viv72        ,     
         diag_registro        ,     
         estado_sub_viv       ,     
         cve_afore            ,     
         estado_solicitud     ,     
         resultado_operacion  ,
         cod_rechazo_1        ,
         cod_rechazo_2        ,
         cod_rechazo_3        
      )
      VALUES (
         seq_ret_solicitud.NEXTVAL           , 
         ret_dispos_rch_id_derechohabiente   , 
         ret_dispos_rch_f_solicitud          , 
         ret_dispos_rch_folio                , 
         ret_dispos_rch_nss                  , 
         ret_dispos_rch_curp                 , 
         ret_dispos_rch_nombre_afore         , 
         ret_dispos_rch_paterno_afore        , 
         ret_dispos_rch_materno_afore        , 
         ret_dispos_rch_sec_pension          , 
         ret_dispos_rch_tipo_retiro          , 
         ret_dispos_rch_regimen              , 
         ret_dispos_rch_tpo_seguro           , 
         ret_dispos_rch_tpo_pension          , 
         ret_dispos_rch_tpo_prestacion       , 
         ret_dispos_rch_f_inicio_pension     , 
         ret_dispos_rch_f_resolucion         , 
         ret_dispos_rch_porcentaje_valuacion , 
         ret_dispos_rch_semanas_cotizadas    , 
         ret_dispos_rch_cve_doc_probatorio   , 
         ret_dispos_rch_f_nacimiento         , 
         ret_dispos_rch_aseguradora          , 
         ret_dispos_rch_actuario             , 
         ret_dispos_rch_num_plan_privado     , 
         ret_dispos_rch_periodo_primer_pago  , 
         ret_dispos_rch_aivs_viv97           , 
         ret_dispos_rch_aivs_viv92           , 
         ret_dispos_rch_consec_trabajador    , 
         ret_dispos_rch_importe_viv72        , 
         ret_dispos_rch_diag_registro        , 
         ret_dispos_rch_estado_sub_viv       , 
         ret_dispos_rch_cve_afore            , 
         ret_dispos_rch_estado_solicitud     , 
         ret_dispos_rch_resultado_operacion  ,
         ret_dispos_rch_cod_rechazo_1        ,
         ret_dispos_rch_cod_rechazo_2        ,
         ret_dispos_rch_cod_rechazo_3        
      );

   
   END FOREACH ;
           
   -- se borran los registros de la tabla de detalle
   DELETE FROM ret_disposicion
   WHERE  folio = p_folio;
   
   -- se borran los registros del encabezado
   DELETE FROM ret_cza_disposicion
   WHERE  folio = p_folio;
   
   -- se devuelve el resultado de la ejecucion del proceso
   RETURN v_sql_error, v_isam_error, v_mensaje;
END FUNCTION;


