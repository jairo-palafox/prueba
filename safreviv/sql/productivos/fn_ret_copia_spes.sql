






CREATE FUNCTION "safreviv".fn_ret_copia_spes(p_usuario_cod CHAR(20),
                                  p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)

   -- campos de la tabla de detalla de carga de archivo SPES 
   DEFINE tmp_ret_spes_tpo_registro                    CHAR(2)             ;
   DEFINE tmp_ret_spes_id_servicio                     CHAR(2)             ;
   DEFINE tmp_ret_spes_id_operacion                    CHAR(2)             ;
   DEFINE tmp_ret_spes_nss                             CHAR(11)            ;
   DEFINE tmp_ret_spes_curp                            CHAR(18)            ;
   DEFINE tmp_ret_spes_nombre_datamart                 CHAR(50)            ;
   DEFINE tmp_ret_spes_nombre_afore                    CHAR(40)            ;
   DEFINE tmp_ret_spes_ap_paterno_afore                CHAR(40)            ;
   DEFINE tmp_ret_spes_ap_materno_afore                CHAR(40)            ;
   DEFINE tmp_ret_spes_sec_pension                     CHAR(2)             ;
   DEFINE tmp_ret_spes_tpo_movimiento                  CHAR(3)             ;
   DEFINE tmp_ret_spes_regimen                         CHAR(2)             ;
   DEFINE tmp_ret_spes_tpo_seguro                      CHAR(2)             ;
   DEFINE tmp_ret_spes_tpo_pension                     CHAR(2)             ;
   DEFINE tmp_ret_spes_tpo_prestacion                  CHAR(2)             ;
   DEFINE tmp_ret_spes_art_negativa                    CHAR(3)             ;
   DEFINE tmp_ret_spes_fracc_negativa                  CHAR(2)             ;
   DEFINE tmp_ret_spes_num_considerando                CHAR(2)             ;
   DEFINE tmp_ret_spes_fec_inicio_pension              CHAR(10)             ;
   DEFINE tmp_ret_spes_fec_resolucion                  CHAR(10)             ;
   DEFINE tmp_ret_spes_porc_valuacion                  DECIMAL(5,0)        ;
   DEFINE tmp_ret_spes_semanas_cotizadas               DECIMAL(4,0)        ;
   DEFINE tmp_ret_spes_diag_retiro                     DECIMAL(3,0)        ;
   DEFINE tmp_ret_spes_estatus_subcuenta               CHAR(1)             ;
   DEFINE tmp_ret_spes_importe_viv97                   DECIMAL(14,0)       ;
   DEFINE tmp_ret_spes_importe_viv92                   DECIMAL(14,0)       ;
   DEFINE tmp_ret_spes_importe_fondo72                 DECIMAL(14,0)       ;
   DEFINE tmp_ret_spes_cve_afore                       CHAR(3)             ;


   --tabal destino ret_datamart
   -- campos de la tabla de  retiros datamart  
   DEFINE  ret_data_mart_id_datamart                DECIMAL(9,0)            ;
   DEFINE  ret_data_mart_nss                        CHAR(11)                ;
   DEFINE  ret_data_mart_sec_pension                SMALLINT                ;
   DEFINE  ret_data_mart_regimen                    SMALLINT                ;
   DEFINE  ret_data_mart_tpo_seguro                 CHAR(2)                 ;
   DEFINE  ret_data_mart_tpo_pension                CHAR(2)                 ;
   DEFINE  ret_data_mart_tpo_prestacion             CHAR(2)                 ;
   DEFINE  ret_data_mart_diag_registro              CHAR(3)                 ;
   DEFINE  ret_data_mart_curp                       CHAR(18)                ;
   DEFINE  ret_data_mart_nombre_datamart            CHAR(40)                ;
   DEFINE  ret_data_mart_nombre_afore               CHAR(40)                ;
   DEFINE  ret_data_mart_paterno_afore              CHAR(40)                ;
   DEFINE  ret_data_mart_materno_afore              CHAR(40)                ;
   DEFINE  ret_data_mart_tpo_movimiento             CHAR(3)                 ;
   DEFINE  ret_data_mart_articulo_negativa          CHAR(3)                 ;
   DEFINE  ret_data_mart_fraccion_negativa          CHAR(2)                 ;
   DEFINE  ret_data_mart_num_considerando           CHAR(2)                 ;
   DEFINE  ret_data_mart_f_inicio_pension           DATE                    ;
   DEFINE  ret_data_mart_f_resolucion               DATE                    ;
   DEFINE  ret_data_mart_porcentaje_valuacion       DECIMAL(5,2)            ;
   DEFINE  ret_data_mart_semanas_cotizadas          INTEGER                 ;
   DEFINE  ret_data_mart_estado_sub_viv             SMALLINT                ;
   DEFINE  ret_data_mart_aivs_viv97                 DECIMAL(14,6)           ;
   DEFINE  ret_data_mart_aivs_viv92                 DECIMAL(14,6)           ;
   DEFINE  ret_data_mart_importe_viv72              DECIMAL(12,2)           ;
   DEFINE  ret_data_mart_cve_afore                  SMALLINT                ;  
   
   -- encabezado de carga de datamart
   DEFINE ret_cza_datamart_folio                    decimal(9,0)           ;
   DEFINE ret_cza_datamart_nombre_archivo           char(20)               ;
   DEFINE ret_cza_datamart_f_carga_datamart         date                   ;
   DEFINE ret_cza_datamart_f_carga_infonavit        date                   ;
   DEFINE ret_cza_datamart_h_carga_infonavit        datetime hour to second;
   DEFINE ret_cza_datamart_total_registros          integer                ;
   DEFINE ret_cza_datamart_usuario                  char(20)               ;

   --PARA REGRESAR EL ESATODO DE LAS SENTENCIAS SQL 
   DEFINE v_estatus_proceso                         SMALLINT                ;  
   DEFINE v_id_datamart                             DECIMAL (9,0)           ;
   DEFINE v_cont_matriz_derecho                     SMALLINT                ;  
   DEFINE v_mensajeno_insert                        VARCHAR(255)            ;
   DEFINE v_mensaje                                 VARCHAR(255)            ;   
   DEFINE v_cont_registros                          INTEGER                 ;
   DEFINE v_fecha_texto                             VARCHAR(10);
   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_copia_spes.txt";
   
   -- se actualiza la glo_ctr_archivo
   UPDATE safre_mig:glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2
   WHERE  nombre_archivo = p_nombre_archivo
   AND    proceso_cod = 1510
   AND    opera_cod   = 1;
   
   LET v_estatus_proceso     = 0 ;
   LET v_id_datamart         = 0 ;       
   LET v_cont_matriz_derecho = 0;
   LET v_cont_registros      = 0 ;
   
   -- se cuentan los registros
   SELECT COUNT(*)
   INTO v_cont_registros
   FROM safre_mig:tmp_spes;
   
   --TRACE "nombre de archivo: " || p_nombre_archivo;
   
   -- se inserta el encabezado de la carga del datamart
   LET ret_cza_datamart_folio              = p_folio;
   LET ret_cza_datamart_nombre_archivo     = p_nombre_archivo;
   LET ret_cza_datamart_f_carga_datamart   = TODAY;
   LET ret_cza_datamart_f_carga_infonavit  = TODAY; -- revisar cual es
   LET ret_cza_datamart_h_carga_infonavit  = CURRENT HOUR TO SECOND; -- revisar cual es
   LET ret_cza_datamart_total_registros    = v_cont_registros;
   LET ret_cza_datamart_usuario            = p_usuario_cod;
                                              
   INSERT INTO ret_cza_datamart (
      folio             ,
      nombre_archivo    ,
      f_carga_datamart  ,
      f_carga_infonavit ,
      h_carga_infonavit ,
      total_registros   ,
      usuario           
   )
   VALUES (
      ret_cza_datamart_folio            ,
      ret_cza_datamart_nombre_archivo   ,
      ret_cza_datamart_f_carga_datamart ,
      ret_cza_datamart_f_carga_infonavit,
      ret_cza_datamart_h_carga_infonavit,
      ret_cza_datamart_total_registros  ,
      ret_cza_datamart_usuario          
   );
   
   LET v_mensajeno_insert = "No se insertaron los registros con " ;
   LET v_mensaje = "Se insertaron " ;

   --- se consultan los datos de la tabla temporal
   FOREACH
    SELECT      
       tpo_registro         ,
       id_servicio          ,
       id_operacion         ,
       nss                  ,
       curp                 ,
       nombre_datamart      ,
       nombre_afore         ,
       ap_paterno_afore     ,
       ap_materno_afore     ,
       sec_pension          ,
       tpo_movimiento       ,
       regimen              ,
       tpo_seguro           ,
       tpo_pension          ,
       tpo_prestacion       ,
       art_negativa         ,
       fracc_negativa       ,
       num_considerando     ,
       fec_inicio_pension   ,
       fec_resolucion       ,
       porc_valuacion       ,
       semanas_cotizadas    ,
       diag_retiro          ,
       estatus_subcuenta    ,
       importe_viv97        ,
       importe_viv92        ,
       importe_fondo72      ,
       cve_afore            
    INTO 
       tmp_ret_spes_tpo_registro           ,
       tmp_ret_spes_id_servicio            ,
       tmp_ret_spes_id_operacion           ,
       tmp_ret_spes_nss                    ,
       tmp_ret_spes_curp                   ,
       tmp_ret_spes_nombre_datamart        ,
       tmp_ret_spes_nombre_afore           ,
       tmp_ret_spes_ap_paterno_afore       ,
       tmp_ret_spes_ap_materno_afore       ,
       tmp_ret_spes_sec_pension            ,
       tmp_ret_spes_tpo_movimiento         ,
       tmp_ret_spes_regimen                ,
       tmp_ret_spes_tpo_seguro             ,
       tmp_ret_spes_tpo_pension            ,
       tmp_ret_spes_tpo_prestacion         ,
       tmp_ret_spes_art_negativa           ,
       tmp_ret_spes_fracc_negativa         ,
       tmp_ret_spes_num_considerando       ,
       tmp_ret_spes_fec_inicio_pension     ,
       tmp_ret_spes_fec_resolucion         ,
       tmp_ret_spes_porc_valuacion         ,
       tmp_ret_spes_semanas_cotizadas      ,
       tmp_ret_spes_diag_retiro            ,
       tmp_ret_spes_estatus_subcuenta      ,
       tmp_ret_spes_importe_viv97          ,
       tmp_ret_spes_importe_viv92          ,
       tmp_ret_spes_importe_fondo72        ,
       tmp_ret_spes_cve_afore              
    FROM
      safre_mig:tmp_spes
      
                                      
      -- se asignan los datos
      LET ret_data_mart_nss                  = tmp_ret_spes_nss                       ;
      LET ret_data_mart_sec_pension          = tmp_ret_spes_sec_pension               ;
      LET ret_data_mart_regimen              = tmp_ret_spes_regimen                   ;
      LET ret_data_mart_tpo_seguro           = tmp_ret_spes_tpo_seguro                ;
      LET ret_data_mart_tpo_pension          = tmp_ret_spes_tpo_pension               ;
      LET ret_data_mart_tpo_prestacion       = tmp_ret_spes_tpo_prestacion            ;
      LET ret_data_mart_diag_registro        = tmp_ret_spes_diag_retiro               ;
      LET ret_data_mart_curp                 = tmp_ret_spes_curp                      ;
      LET ret_data_mart_nombre_datamart      = tmp_ret_spes_nombre_datamart           ;
      LET ret_data_mart_nombre_afore         = tmp_ret_spes_nombre_afore              ;
      LET ret_data_mart_paterno_afore        = tmp_ret_spes_ap_paterno_afore          ;
      LET ret_data_mart_materno_afore        = tmp_ret_spes_ap_materno_afore          ;
      LET ret_data_mart_tpo_movimiento       = tmp_ret_spes_tpo_movimiento            ;
      LET ret_data_mart_articulo_negativa    = tmp_ret_spes_art_negativa              ;
      LET ret_data_mart_fraccion_negativa    = tmp_ret_spes_fracc_negativa            ;
      LET ret_data_mart_num_considerando     = tmp_ret_spes_num_considerando          ;
      
      LET v_fecha_texto = tmp_ret_spes_fec_inicio_pension[5,6] || "/" || tmp_ret_spes_fec_inicio_pension[7,8] || "/" || tmp_ret_spes_fec_inicio_pension[1,4];      
      LET ret_data_mart_f_inicio_pension     = DATE(v_fecha_texto);
      
      LET v_fecha_texto = tmp_ret_spes_fec_resolucion[5,6] || "/" || tmp_ret_spes_fec_resolucion[7,8] || "/" || tmp_ret_spes_fec_resolucion[1,4];
      LET ret_data_mart_f_resolucion         = DATE(v_fecha_texto);

      LET ret_data_mart_porcentaje_valuacion = tmp_ret_spes_porc_valuacion    /100    ;
      LET ret_data_mart_semanas_cotizadas    = tmp_ret_spes_semanas_cotizadas         ;
      LET ret_data_mart_estado_sub_viv       = tmp_ret_spes_estatus_subcuenta         ;
      LET ret_data_mart_aivs_viv97           = tmp_ret_spes_importe_viv97     /1000000;
      LET ret_data_mart_aivs_viv92           = tmp_ret_spes_importe_viv92     /1000000;
      LET ret_data_mart_importe_viv72        = tmp_ret_spes_importe_fondo72   /100    ;
      LET ret_data_mart_cve_afore            = tmp_ret_spes_cve_afore                 ;
   
      SELECT COUNT (*)
       INTO v_cont_matriz_derecho
       FROM ret_matriz_derecho 
      WHERE regimen        = ret_data_mart_regimen
        AND tpo_seguro     = ret_data_mart_tpo_seguro
        AND tpo_pension    = ret_data_mart_tpo_pension
        AND tpo_prestacion = ret_data_mart_tpo_prestacion ;

      ----TRACE 'DATOS A  comparar en ret_matriz_derecho  = '|| ret_data_mart_regimen ||' '||ret_data_mart_tpo_seguro||' '||ret_data_mart_tpo_pension||' '||ret_data_mart_tpo_prestacion ;   
      ----TRACE 'v_cont_matriz_derecho  = '|| v_cont_matriz_derecho ;
      --si existe registro en  RET_MATRIZ_DERECHO se inserta en ret_datamart
      IF v_cont_matriz_derecho > 0 THEN     
      	 LET v_cont_registros = v_cont_registros + 1 ;
      	 
     
        --se obtienela secuencia del archivo  
        SELECT seq_ret_datamart.NEXTVAL
          INTO ret_data_mart_id_datamart
          FROM systables
         WHERE tabid = 1;
   
           ----TRACE 'DATOS A INSERTAR  = '|| ret_data_mart_id_datamart ||' '|| ret_data_mart_nss||' '||ret_data_mart_sec_pension ||' '|| ret_data_mart_regimen ||' '||ret_data_mart_tpo_seguro ||' '||ret_data_mart_tpo_pension ||' '||ret_data_mart_tpo_prestacion ||' '||ret_data_mart_diag_registro  ||' '||ret_data_mart_curp||' '||ret_data_mart_nombre_datamart ||' '||ret_data_mart_nombre_afore||' '||ret_data_mart_paterno_afore||' '||ret_data_mart_materno_afore||' '||ret_data_mart_tpo_movimiento||' '||ret_data_mart_articulo_negativa ||' '||ret_data_mart_fraccion_negativa  ||' '||ret_data_mart_num_considerando||' '||ret_data_mart_f_inicio_pension||' '||ret_data_mart_f_resolucion||' '||ret_data_mart_porcentaje_valuacion ;
           ----TRACE 'DATOS A INSERTAR  = '|| ret_data_mart_semanas_cotizadas ||' '||ret_data_mart_estado_sub_viv||' '||ret_data_mart_aivs_viv97||' '||ret_data_mart_aivs_viv92||' '||ret_data_mart_importe_viv72||' '||ret_data_mart_cve_afore ;
   
        --sse insertan los registros en ret_datamart 
        INSERT INTO ret_datamart (
                                   id_datamart            ,
                                   nss                    ,
                                   sec_pension            ,
                                   regimen                ,
                                   tpo_seguro             ,
                                   tpo_pension            ,
                                   tpo_prestacion         ,
                                   diag_registro          ,
                                   folio                  ,
                                   curp                   ,
                                   nombre_datamart        ,
                                   nombre_afore           ,
                                   paterno_afore          ,
                                   materno_afore          ,
                                   tpo_movimiento         ,
                                   articulo_negativa      ,
                                   fraccion_negativa      ,
                                   num_considerando       ,
                                   f_inicio_pension       ,
                                   f_resolucion           ,
                                   porcentaje_valuacion   ,
                                   semanas_cotizadas      ,
                                   estado_sub_viv         ,
                                   aivs_viv97             ,
                                   aivs_viv92             ,
                                   importe_viv72          ,
                                   cve_afore
                                  )
                          VALUES ( 
                                   ret_data_mart_id_datamart           ,
                                   ret_data_mart_nss                   ,
                                   ret_data_mart_sec_pension           ,
                                   ret_data_mart_regimen               ,
                                   ret_data_mart_tpo_seguro            ,
                                   ret_data_mart_tpo_pension           ,
                                   ret_data_mart_tpo_prestacion        ,
                                   ret_data_mart_diag_registro         ,
                                   p_folio                             , -- el folio enviado como parametro
                                   ret_data_mart_curp                  ,
                                   ret_data_mart_nombre_datamart       ,
                                   ret_data_mart_nombre_afore          ,
                                   ret_data_mart_paterno_afore         ,
                                   ret_data_mart_materno_afore         ,
                                   ret_data_mart_tpo_movimiento        ,
                                   ret_data_mart_articulo_negativa     ,
                                   ret_data_mart_fraccion_negativa     ,
                                   ret_data_mart_num_considerando      ,
                                   ret_data_mart_f_inicio_pension      ,
                                   ret_data_mart_f_resolucion          ,
                                   ret_data_mart_porcentaje_valuacion  ,
                                   ret_data_mart_semanas_cotizadas     ,
                                   ret_data_mart_estado_sub_viv        ,
                                   ret_data_mart_aivs_viv97            ,
                                   ret_data_mart_aivs_viv92            ,
                                   ret_data_mart_importe_viv72         ,
                                   ret_data_mart_cve_afore             
                                  );
      --caso contrario NO EXISTE EN RET_MATRIZ_DERECHO no se inserta en ret_datamart	
      ELSE 	
        LET v_mensajeno_insert = v_mensajeno_insert|| " regimen = "||ret_data_mart_tpo_seguro||" seguro = "||ret_data_mart_tpo_seguro||
                               "pension = "||ret_data_mart_tpo_pension||" prestacion = " ||ret_data_mart_tpo_prestacion  ||"\n";
                               
                               
                               
      END IF 
   END FOREACH;
   LET v_mensaje = v_mensaje||"   "||v_cont_registros||" Regitros  "||v_mensajeno_insert ;
   
   --TRACE ('mensaje = '||v_mensaje) ;
   
   --TRACE 'Finaliza el store procedure de registro historicos de SIN CAMBIO EN NSS';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de carga de SPESS finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt;
END FUNCTION;


