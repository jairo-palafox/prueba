






CREATE FUNCTION "safreviv".fn_ret_integra_spes(p_usuario_cod CHAR(20),
                                  p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)

   -- campos de la tabla de encabezado del archivo SPES
   DEFINE ret_cza_spes_f_carga_datamart                CHAR(10)         ;


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
   DEFINE tmp_ret_spes_cod_rechazo                     CHAR(3)             ;


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
   DEFINE  ret_data_mart_nombre_datamart            CHAR(50)                ;
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
   DEFINE v_cont_registros_det                      INTEGER                 ;
   DEFINE v_cont_registros_rechazo                  INTEGER                 ;
   DEFINE v_id_derechohabiente                      DECIMAL(9,0)            ;
   DEFINE v_existe_derechohabiente                  SMALLINT                ;
   DEFINE v_resultado                               SMALLINT                ;
   
   -- variables de fecha auxiliares
   DEFINE v_fecha_cza                                CHAR(10)               ;
   DEFINE v_fecha_det_f_inicio_pension               CHAR(10)               ;
   DEFINE v_fecha_det_f_resolucion                   CHAR(10)               ;
   DEFINE v_fecha_valida                             DATE                   ;
   DEFINE v_res_valida_fecha                         INTEGER                ;
   DEFINE v_registros_en_temp                        INTEGER                ;
   
   -- Variables indicadoras de rechazos en campos de fecha
   DEFINE v_error_fch_emision_resolucion             SMALLINT               ;
   DEFINE v_error_fch_inicio_pension                 SMALLINT               ;
   DEFINE v_error_reg_repetido                       SMALLINT               ;
   DEFINE v_cod_rechazo                              SMALLINT               ;
   
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
   
   LET v_estatus_proceso        = 0;
   LET v_id_datamart            = 0;       
   LET v_cont_matriz_derecho    = 0;
   LET v_cont_registros         = 0;   
   LET v_cont_registros_det     = 0;
   LET v_cont_registros_rechazo = 0;
   LET v_registros_en_temp      = 0;
   
   --trace "nombre de archivo: " || p_nombre_archivo;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
      SET folio = p_folio,
          estado = 2 -- integrado
    WHERE proceso_cod    = p_proceso_cod
      AND opera_cod      = 1 -- archivo cargado
      AND estado         = 1
      AND folio          IS NULL; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;

   -- se asigna el folio al proceso
   UPDATE bat_ctr_proceso
      SET folio       = p_folio
    WHERE pid         = p_pid;  
   -- se cuentan los registros
   SELECT COUNT(*)
   INTO v_cont_registros_det
   FROM safre_tmp:tmp_ret_det_spess;
   
   -- se obtiene la fecha de carga
   SELECT f_carga_datamart
   INTO   ret_cza_spes_f_carga_datamart
   FROM safre_tmp:tmp_ret_cza_spess;

   -- Se crea el archivo de salida con los registros con el porcentaje de valuación erroneo
   DROP TABLE IF EXISTS ret_spess_porcentaje;
   CREATE TEMP TABLE ret_spess_porcentaje(
        nss CHAR(11),
        porcentaje_original DECIMAL(5,2),
        porcentaje DECIMAL(5,2)
    ) WITH NO LOG;

   LET v_fecha_cza = ret_cza_spes_f_carga_datamart[5,6] || "/" || ret_cza_spes_f_carga_datamart[7,8] || "/" || ret_cza_spes_f_carga_datamart[1,4];       
   
   -- se inserta el encabezado de la carga del datamart
   LET ret_cza_datamart_folio              = p_folio;
   LET ret_cza_datamart_nombre_archivo     = p_nombre_archivo;
   LET ret_cza_datamart_f_carga_datamart   = DATE(v_fecha_cza);
   LET ret_cza_datamart_f_carga_infonavit  = TODAY; -- revisar cual es
   LET ret_cza_datamart_h_carga_infonavit  = CURRENT HOUR TO SECOND; -- revisar cual es
   LET ret_cza_datamart_total_registros    = v_cont_registros_det;
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
   
   LET v_mensajeno_insert = "No se insertarón los registros con " ;
   LET v_mensaje = "Se insertarón " ;

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
       apaterno_afore       ,
       amaterno_afore       ,
       sec_pension          ,
       tpo_movimiento       ,
       regimen              ,
       tpo_seguro           ,
       tpo_pension          ,
       tpo_prestacion       ,
       art_negativa         ,
       fraccion_negativa    ,
       num_considerando     ,
       f_inicio_pension     ,
       f_resolucion         ,
       porcentaje_valuacion ,
       semanas_cotizadas    ,
       diagnostico          ,
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
      safre_tmp:tmp_ret_det_spess
            
      
      -- se transfieren los datos
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

      IF tmp_ret_spes_tpo_pension = 'VI' OR tmp_ret_spes_tpo_pension = 'VO' OR
         tmp_ret_spes_tpo_pension = 'OR' OR tmp_ret_spes_tpo_pension = 'AS' THEN 

         LET v_id_derechohabiente     = 0;
         LET v_existe_derechohabiente = 0;

         SELECT COUNT(*)
         INTO   v_existe_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = tmp_ret_spes_nss;

         IF v_existe_derechohabiente = 1 THEN
            SELECT id_derechohabiente
            INTO   v_id_derechohabiente
            FROM   afi_derechohabiente
            WHERE  nss = tmp_ret_spes_nss;
            
            EXECUTE FUNCTION fn_afi_fallecido(v_id_derechohabiente, '10', p_usuario_cod)
            INTO v_resultado;

         END IF 
      END IF 
      
      -- se cambia de formato la fecha
      -- Se valida que las fechas sean correctas
      LET v_res_valida_fecha             = 0;
      LET v_error_fch_inicio_pension     = 0;
      LET v_error_fch_emision_resolucion = 0;
      LET v_error_reg_repetido           = 0;
      
      LET v_fecha_det_f_inicio_pension       =  tmp_ret_spes_fec_inicio_pension[5,6] || "/" || tmp_ret_spes_fec_inicio_pension[7,8] || "/" || tmp_ret_spes_fec_inicio_pension[1,4];
      CALL fn_ret_valida_fecha(tmp_ret_spes_fec_inicio_pension[7,8],tmp_ret_spes_fec_inicio_pension[5,6],tmp_ret_spes_fec_inicio_pension[1,4]) RETURNING v_res_valida_fecha;
      IF v_res_valida_fecha <> 0 THEN
          LET v_error_fch_inicio_pension  = 8; 
      ELSE 
          LET ret_data_mart_f_inicio_pension     =  DATE(v_fecha_det_f_inicio_pension)    ; 
      END IF 
      
      LET v_fecha_det_f_resolucion           =  tmp_ret_spes_fec_resolucion[5,6] || "/" || tmp_ret_spes_fec_resolucion[7,8] || "/" || tmp_ret_spes_fec_resolucion[1,4];       
      CALL fn_ret_valida_fecha(tmp_ret_spes_fec_resolucion[7,8],tmp_ret_spes_fec_resolucion[5,6],tmp_ret_spes_fec_resolucion[1,4]) RETURNING v_res_valida_fecha;      
      IF v_res_valida_fecha <> 0 THEN
          LET v_error_fch_emision_resolucion  = 9;
      ELSE 
          LET ret_data_mart_f_resolucion         =  DATE(v_fecha_det_f_resolucion)        ;
      END IF 
      
      --- 768 registro duplicado
      -- busca la existencia de registro previo
      SELECT COUNT(*)
      INTO   v_error_reg_repetido
      FROM   ret_datamart
      WHERE  nss                  = tmp_ret_spes_nss
      AND    curp                 = tmp_ret_spes_curp
      AND    sec_pension          = tmp_ret_spes_sec_pension
      AND    regimen              = tmp_ret_spes_regimen
      AND    tpo_seguro           = tmp_ret_spes_tpo_seguro
      AND    tpo_pension          = tmp_ret_spes_tpo_pension
      AND    tpo_prestacion       = tmp_ret_spes_tpo_prestacion
      AND    nombre_datamart      = tmp_ret_spes_nombre_datamart
      AND    nombre_afore         = tmp_ret_spes_nombre_afore
      AND    paterno_afore        = tmp_ret_spes_ap_paterno_afore
      AND    materno_afore        = tmp_ret_spes_ap_materno_afore
      AND    tpo_movimiento       = tmp_ret_spes_tpo_movimiento
      AND    articulo_negativa    = tmp_ret_spes_art_negativa
      AND    fraccion_negativa    = tmp_ret_spes_fracc_negativa
      AND    num_considerando     = tmp_ret_spes_num_considerando
      AND    f_inicio_pension     = DATE(v_fecha_det_f_inicio_pension)
      AND    f_resolucion         = DATE(v_fecha_det_f_resolucion)
      AND    porcentaje_valuacion = tmp_ret_spes_porc_valuacion    /100
      AND    semanas_cotizadas    = tmp_ret_spes_semanas_cotizadas
      AND    estado_sub_viv       = tmp_ret_spes_estatus_subcuenta
      AND    aivs_viv97           = tmp_ret_spes_importe_viv97     /1000000
      AND    aivs_viv92           = tmp_ret_spes_importe_viv92     /1000000
      AND    importe_viv72        = tmp_ret_spes_importe_fondo72   /100
      AND    cve_afore            = tmp_ret_spes_cve_afore;

      LET ret_data_mart_porcentaje_valuacion = tmp_ret_spes_porc_valuacion    /100    ;
      IF ret_data_mart_porcentaje_valuacion < 1 AND ret_data_mart_porcentaje_valuacion > 0 THEN
         INSERT INTO ret_spess_porcentaje VALUES (tmp_ret_spes_nss,ret_data_mart_porcentaje_valuacion,ret_data_mart_porcentaje_valuacion*100);
         LET ret_data_mart_porcentaje_valuacion = ret_data_mart_porcentaje_valuacion * 100;
      END IF
      IF ret_data_mart_porcentaje_valuacion > 100 THEN 
         INSERT INTO ret_spess_porcentaje VALUES (tmp_ret_spes_nss,ret_data_mart_porcentaje_valuacion,ret_data_mart_porcentaje_valuacion/100);
         LET ret_data_mart_porcentaje_valuacion = ret_data_mart_porcentaje_valuacion / 100;
      END IF
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

      --TRACE 'DATOS A  comparar en ret_matriz_derecho  = '|| ret_data_mart_regimen ||' '||ret_data_mart_tpo_seguro||' '||ret_data_mart_tpo_pension||' '||ret_data_mart_tpo_prestacion ;   
      --TRACE 'v_cont_matriz_derecho  = '|| v_cont_matriz_derecho ;
      --si existe registro en  RET_MATRIZ_DERECHO se inserta en ret_datamart
      IF v_cont_matriz_derecho = 0 OR
         v_error_fch_inicio_pension <> 0 OR 
         v_error_fch_emision_resolucion <> 0 OR
         v_error_reg_repetido > 0 THEN     
         IF v_error_reg_repetido > 0 THEN
             LET v_cod_rechazo = 10;
         ELSE 
            IF v_error_fch_emision_resolucion <> 0 THEN
                LET v_cod_rechazo = 9;
            ELSE 
                IF v_error_fch_inicio_pension <> 0 THEN
                    LET v_cod_rechazo = 8;
                ELSE 
                    LET v_cod_rechazo = 4;
                END IF
            END IF
        END IF 
        LET tmp_ret_spes_cod_rechazo = v_cod_rechazo;
      --caso contrario NO EXISTE EN RET_MATRIZ_DERECHO no se inserta en ret_datamart	
      --- ****** Matri derecho no existe ***** ---
      --- se debe insertar en la tabla de rechazos ---
      
        LET v_mensajeno_insert = v_mensajeno_insert|| " regimen = "||ret_data_mart_tpo_seguro||" seguro = "||ret_data_mart_tpo_seguro||
                               "pension = "||ret_data_mart_tpo_pension||" prestacion = " ||ret_data_mart_tpo_prestacion  ||"\n";
        
        -- se cuenta un registro rechazado
        LET v_cont_registros_rechazo = v_cont_registros_rechazo + 1;
        INSERT INTO ret_datamart_rch_carga (
                   folio                ,
                   cod_rechazo          ,
                   tpo_registro         ,
                   id_servicio          ,
                   id_operacion         ,
                   nss                  ,
                   curp                 ,
                   nombre_datamart      ,
                   nombre_afore         ,
                   apaterno_afore       ,
                   amaterno_afore       ,
                   sec_pension          ,
                   tpo_movimiento       ,
                   regimen              ,
                   tpo_seguro           ,
                   tpo_pension          ,
                   tpo_prestacion       ,
                   art_negativa         ,
                   fraccion_negativa    ,
                   num_considerando     ,
                   f_inicio_pension     ,
                   f_resolucion         ,
                   porcentaje_valuacion ,
                   semanas_cotizadas    ,
                   diagnostico          ,
                   estatus_subcuenta    ,
                   importe_viv97        ,
                   importe_viv92        ,
                   importe_fondo72      ,
                   cve_afore            )
        VALUES (
                   p_folio                             ,
                   tmp_ret_spes_cod_rechazo            ,
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
                   tmp_ret_spes_cve_afore              );

      ELSE
      	 LET v_cont_registros = v_cont_registros + 1 ;

        --se obtienela secuencia del archivo  
        SELECT seq_ret_datamart.NEXTVAL
          INTO ret_data_mart_id_datamart
          FROM systables
         WHERE tabid = 1;
   
           --TRACE 'DATOS A INSERTAR  = '|| ret_data_mart_id_datamart ||' '|| ret_data_mart_nss||' '||ret_data_mart_sec_pension ||' '|| ret_data_mart_regimen ||' '||ret_data_mart_tpo_seguro ||' '||ret_data_mart_tpo_pension ||' '||ret_data_mart_tpo_prestacion ||' '||ret_data_mart_diag_registro  ||' '||ret_data_mart_curp||' '||ret_data_mart_nombre_datamart ||' '||ret_data_mart_nombre_afore||' '||ret_data_mart_paterno_afore||' '||ret_data_mart_materno_afore||' '||ret_data_mart_tpo_movimiento||' '||ret_data_mart_articulo_negativa ||' '||ret_data_mart_fraccion_negativa  ||' '||ret_data_mart_num_considerando||' '||ret_data_mart_f_inicio_pension||' '||ret_data_mart_f_resolucion||' '||ret_data_mart_porcentaje_valuacion ;
           --TRACE 'DATOS A INSERTAR  = '|| ret_data_mart_semanas_cotizadas ||' '||ret_data_mart_estado_sub_viv||' '||ret_data_mart_aivs_viv97||' '||ret_data_mart_aivs_viv92||' '||ret_data_mart_importe_viv72||' '||ret_data_mart_cve_afore ;
   
        --sse insertan los registros en ret_datamart 
        INSERT INTO  ret_datamart (
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
      END IF 
   END FOREACH;
   LET v_mensaje = "Registros aceptados: " || v_cont_registros||" == Registros Rechazados: " || v_cont_registros_rechazo || " == Registros Totales:" || v_cont_registros_det ;

   SELECT COUNT(*)
   INTO   v_registros_en_temp
   FROM   ret_spess_porcentaje;

   IF v_registros_en_temp > 0 THEN 
      CREATE EXTERNAL TABLE ret_spess_porcentaje_ext 
      SAMEAS ret_spess_porcentaje
      USING (
        DATAFILES ("DISK:/safreviv_int/ret/envio/spess_porcentajes_erroneos.unl") 
        );

      INSERT INTO ret_spess_porcentaje_ext SELECT * FROM ret_spess_porcentaje;

      DROP  TABLE IF EXISTS ret_spess_porcentaje_ext;
      DROP  TABLE IF EXISTS ret_spess_porcentaje;
   END IF  
   
   --trace ('mensaje = '||v_mensaje) ;
   
   --TRACE 'Finaliza el store procedure de registro historicos de SIN CAMBIO EN NSS';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
      
   RETURN v_si_resultado, isam_err, v_mensaje;
END FUNCTION;


