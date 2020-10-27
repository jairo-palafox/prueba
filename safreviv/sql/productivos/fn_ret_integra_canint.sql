






CREATE FUNCTION "safreviv".fn_ret_integra_canint(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0), p_nombre_archivo VARCHAR(40), 
                                   p_pid DECIMAL(9,0),p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)


-- campos de la tabla de encabezado del archivo CANIN sin filler)
DEFINE tmp_ret_cza_tpo_registro              CHAR(2)      ;
DEFINE tmp_ret_cza_id_servicio               CHAR(2)      ;
DEFINE tmp_ret_cza_id_operacion              CHAR(2)      ;
DEFINE tmp_ret_cza_tpo_ent_origen            CHAR(2)      ;
DEFINE tmp_ret_cza_cve_ent_origen            CHAR(3)      ;
DEFINE tmp_ret_cza_tpo_ent_dest              CHAR(2)      ;
DEFINE tmp_ret_cza_cve_ent_dest              CHAR(3)      ;
DEFINE tmp_ret_cza_f_operacion               CHAR(8)      ;
DEFINE tmp_ret_cza_resultado_operacion       CHAR(2)      ;
DEFINE tmp_ret_cza_motivo_rech               CHAR(3)      ;
DEFINE tmp_ret_cza_total_registros           CHAR(6)      ;

-- campos de la tabla de detalle del archivo CANINT
DEFINE tmp_ret_det_canint_nss                CHAR(11)     ;
DEFINE tmp_ret_det_canint_desc_movto         CHAR(18)     ;
DEFINE tmp_ret_det_canint_f_liquidacion      CHAR(8)      ;
DEFINE tmp_ret_det_canint_tpo_movto          CHAR(2)      ;
DEFINE tmp_ret_det_canint_aivs               CHAR(15)     ;

-- campos de la tabla de sumario delarchivo CANINT (sin filler)
DEFINE tmp_ret_sum_tpo_registro              CHAR(2)      ;
DEFINE tmp_ret_sum_id_servicio               CHAR(2)      ;
DEFINE tmp_ret_sum_id_operacion              CHAR(2)      ;
DEFINE tmp_ret_sum_tpo_ent_origen            CHAR(2)      ;
DEFINE tmp_ret_sum_cve_ent_origen            CHAR(3)      ;
DEFINE tmp_ret_sum_tpo_ent_dest              CHAR(2)      ;
DEFINE tmp_ret_sum_cve_ent_dest              CHAR(3)      ;
DEFINE tmp_ret_sum_f_operacion               CHAR(8)      ;
DEFINE tmp_ret_sum_total_registros           CHAR(9)      ;

-- encabezado de la tabla historica/integrada del archivo CANINT
DEFINE ret_cza_canint_folio                  DECIMAL(9,0) ;
DEFINE ret_cza_canint_tpo_registro           CHAR(2)      ;
DEFINE ret_cza_canint_id_servicio            CHAR(2)      ;
DEFINE ret_cza_canint_id_operacion           CHAR(2)      ;
DEFINE ret_cza_canint_tpo_ent_origen         CHAR(2)      ;
DEFINE ret_cza_canint_cve_ent_origen         CHAR(3)      ;
DEFINE ret_cza_canint_tpo_ent_dest           CHAR(2)      ; 
DEFINE ret_cza_canint_cve_ent_dest           CHAR(3)      ;
DEFINE ret_cza_canint_f_operacion            DATE         ;
DEFINE ret_cza_canint_resultado_operacion    CHAR(2)      ;
DEFINE ret_cza_canint_motivo_rech            CHAR(3)      ;
DEFINE ret_cza_canint_total_registros        DECIMAL(6,0) ;

-- detalle de la tabla historica/integrada del archivo CANINT
DEFINE ret_det_canint_consec_detalle         DECIMAL(9,0) ;
DEFINE ret_det_canint_folio                  DECIMAL(9,0) ;
DEFINE ret_det_canint_estado_solicitud       SMALLINT     ;
DEFINE ret_det_canint_cod_rechazo_sol        SMALLINT     ;

DEFINE ret_det_canint_id_derechohabiente     DECIMAL(9,0) ;
DEFINE ret_det_canint_nss                    CHAR(11)     ;
DEFINE ret_det_canint_desc_movto             CHAR(18)     ;
DEFINE ret_det_canint_f_liquidacion          DATE         ;
DEFINE ret_det_canint_tpo_movto              CHAR(02)     ;
DEFINE ret_det_canint_aivs                   DECIMAL(15,6);

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                  DECIMAL(9,0) ;
DEFINE v_consec_canint                       DECIMAL(9,0) ;
DEFINE v_caracter                            CHAR(1)      ;
DEFINE i                                     INTEGER      ;
DEFINE v_sumario_total_registros             DECIMAL(6,0) ;
DEFINE v_total_registros                     DECIMAL(6,0) ;
DEFINE v_numero_registros                    DECIMAL(6,0) ;

-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo                     CHAR(30)     ; -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo              DECIMAL(6,0) ; 

-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados                  DECIMAL(6,0) ; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                  DECIMAL(6,0) ; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                  DECIMAL(6,0) ; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                  DECIMAL(6,0) ; -- total de registros de detalle rechazados

-- codigos de error en encabezado
DEFINE v_error_cza_reg_totales_no_coinciden  DECIMAL(6,0) ;
DEFINE v_error_cza_id_servicio_invalido      DECIMAL(6,0) ;
DEFINE v_error_cza_id_operacion_invalido     DECIMAL(6,0) ;
DEFINE v_error_cza_sin_f_operacion           DECIMAL(6,0) ;

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado         DECIMAL(6,0) ;
DEFINE v_error_det_id_servicio_invalido      DECIMAL(6,0) ;
DEFINE v_error_det_id_operacion_invalido     DECIMAL(6,0) ;
DEFINE v_error_det_desc_movto_invalido       DECIMAL(6,0) ;
DEFINE v_error_det_f_liquidacion_invalida    DECIMAL(6,0) ;
DEFINE v_error_det_tpo_operacion_invalido    DECIMAL(6,0) ;
DEFINE v_error_det_aivs_invalidas            DECIMAL(6,0) ;
DEFINE v_error_estado_sol_rech               DECIMAL(6,0) ;


-- estatus del proceso
DEFINE v_estatus_proceso                     SMALLINT     ;

-- estado solicitud
DEFINE v_estado_solicitud                    SMALLINT     ;

-- codigo de rechazo de la solicitud
DEFINE v_cod_rechazo_sol                     SMALLINT     ;

-- variable de trabajo para la fecha
DEFINE v_c_f_operacion                       CHAR(10)     ;
DEFINE v_fecha_f                             DATE         ;
DEFINE v_res_call_function                   SMALLINT     ;

-- Control de Excepciones
DEFINE v_si_resultado                        SMALLINT     ;
DEFINE sql_err                               INTEGER      ;
DEFINE isam_err                              INTEGER      ;
DEFINE err_txt                               VARCHAR(250) ;
DEFINE v_c_msj                               VARCHAR(250) ;


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_pmg.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados                       = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados                       = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados                       = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados                       = 0; -- total de registros de detalle rechazados
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso                          = 0;
   LET v_si_resultado                             = 0;
   LET isam_err                                   = 0;
   LET v_c_msj                                    = 'El proceso finalizó exitosamente.';

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden       = 32;
   LET v_error_cza_id_servicio_invalido           = 5;
   LET v_error_cza_id_operacion_invalido          = 6;
   LET v_error_cza_sin_f_operacion                = 239;
 
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado              = 49      ; -- segun PROCESAR
   LET v_error_det_id_servicio_invalido           = 111     ;
   LET v_error_det_id_operacion_invalido          = 3       ;
   LET v_error_det_desc_movto_invalido            = 400     ;
   LET v_error_det_f_liquidacion_invalida         = 401     ;
   LET v_error_det_tpo_operacion_invalido         = 402     ;
   LET v_error_det_aivs_invalidas                 = 403     ;
   LET v_error_estado_sol_rech                    = 110     ;  -- INCONSISTENTE

-- se inicializan el estado y codigo de rechazo de la solicitud
   LET v_estado_solicitud                         = 0;
   LET v_cod_rechazo_sol                          = 0;
--   LET v_caracter                                 = '';
   LET i                                          = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1
   AND    folio          IS NULL; -- etapa de carga
   
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

    
   --- se cuentan los registros de la tabla temporal de detalle
   SELECT COUNT(*)
   INTO   v_numero_registros
   FROM   safre_tmp:tmp_ret_det_canint;

   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO v_total_registros
   FROM
      safre_tmp:tmp_ret_det_canint;
   
   SELECT total_registros
   INTO v_sumario_total_registros
   FROM
      safre_tmp:tmp_ret_sum_canint;
 
   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN
      -- se rechaza el lote y no integra
      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET v_c_msj = "No coinciden numero de registros cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   -- se obtienen los datos del encabezado
   FOREACH
   SELECT 
      tpo_registro                      ,
      id_servicio                       ,
      id_operacion                      ,
      tpo_ent_origen                    ,
      cve_ent_origen                    ,
      tpo_ent_dest                      ,
      cve_ent_dest                      ,
      f_operacion                       ,
      resultado_operacion               ,
      motivo_rech                       
   INTO
      tmp_ret_cza_tpo_registro          ,
      tmp_ret_cza_id_servicio           ,
      tmp_ret_cza_id_operacion          ,
      tmp_ret_cza_tpo_ent_origen        ,
      tmp_ret_cza_cve_ent_origen        ,
      tmp_ret_cza_tpo_ent_dest          ,
      tmp_ret_cza_cve_ent_dest          ,
      tmp_ret_cza_f_operacion           ,
      tmp_ret_cza_resultado_operacion   ,
      tmp_ret_cza_motivo_rech           
   FROM
      safre_tmp:tmp_ret_cza_canint
 
      -- se asignan los datos al registro de encabezado historico
      LET ret_cza_canint_folio                 = p_folio;
      LET ret_cza_canint_tpo_registro          = tmp_ret_cza_tpo_registro;
      LET ret_cza_canint_id_servicio           = tmp_ret_cza_id_servicio;
      LET ret_cza_canint_id_operacion          = tmp_ret_cza_id_operacion;
      LET ret_cza_canint_tpo_ent_origen        = tmp_ret_cza_tpo_ent_origen;
      LET ret_cza_canint_cve_ent_origen        = tmp_ret_cza_cve_ent_origen;
      LET ret_cza_canint_tpo_ent_dest          = tmp_ret_cza_tpo_ent_dest;
      LET ret_cza_canint_cve_ent_dest          = tmp_ret_cza_cve_ent_dest;
      LET v_c_f_operacion                      = SUBSTR(tmp_ret_cza_f_operacion,5,2) || "/" || SUBSTR(tmp_ret_cza_f_operacion,7,2) || "/" || SUBSTR(tmp_ret_cza_f_operacion,1,4);
      LET v_fecha_f                            = DATE(v_c_f_operacion);
      LET ret_cza_canint_resultado_operacion   = tmp_ret_cza_resultado_operacion;
      LET ret_cza_canint_motivo_rech           = tmp_ret_cza_motivo_rech;
      LET ret_cza_canint_total_registros       = v_numero_registros; -- numero de registros
 
      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;
      LET ret_cza_canint_resultado_operacion   = 1; -- aceptado
      LET ret_cza_canint_motivo_rech           = 0;
      
      -- validando tipo de registro
      IF ( tmp_ret_cza_id_servicio  <> "04" OR
           tmp_ret_cza_id_operacion <> "62" OR
           tmp_ret_cza_f_operacion IS NULL ) THEN
         -- se activa la bandera de rechazo de encabezado
         LET ret_cza_canint_resultado_operacion   = 2; -- rechazado
       
         -- validando identificador de servicio
         -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
         IF ( tmp_ret_cza_id_servicio <> "04" ) THEN
           LET ret_cza_canint_motivo_rech = v_error_cza_id_servicio_invalido;
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF

         -- validando identificador de operacion
         -- 3	Identificador de Operacion	X	02	00	005	-	006	"62" CANINT
         IF ( tmp_ret_cza_id_operacion <> "62" ) THEN
           LET ret_cza_canint_motivo_rech = v_error_cza_id_operacion_invalido;
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      
         -- se verifica si el encabezado contiene fecha operacion
         IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
           LET ret_cza_canint_motivo_rech = v_error_cza_sin_f_operacion;
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
         END IF      
      END IF

      
      -- se inserta en la tabla historica del encabezado de retiros por disposicion
      INSERT INTO ret_cza_canint (
         folio                                     ,
         tpo_registro                              ,
         id_servicio                               ,
         id_operacion                              ,
         tpo_ent_origen                            ,
         cve_ent_origen                            ,  
         tpo_ent_dest                              ,
         cve_ent_dest                              ,
         f_operacion                               ,
         resultado_operacion                       ,
         motivo_rech                               ,
         total_registros
      )
      VALUES (
         ret_cza_canint_folio                      ,
         ret_cza_canint_tpo_registro               ,
         ret_cza_canint_id_servicio                ,
         ret_cza_canint_id_operacion               ,
         ret_cza_canint_tpo_ent_origen             ,
         ret_cza_canint_cve_ent_origen             ,  
         ret_cza_canint_tpo_ent_dest               ,
         ret_cza_canint_cve_ent_dest               ,
         v_fecha_f                                 ,
         ret_cza_canint_resultado_operacion        ,
         ret_cza_canint_motivo_rech                ,
         ret_cza_canint_total_registros
      );
      
      -- se cuenta un encabezado insertado
      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;
 
   END FOREACH;

   -- se inicia la variable que almacenaria el consecutivo
   LET v_consec_canint          = 0;
   LET v_id_derechohabiente     = 0;
   LET v_indice_codigos_rechazo = 0;
   LET v_reg_det_insertados     = 0;
   
   -- se obtienen los datos del detalle
   FOREACH
       SELECT
          nss                              ,
          desc_movto                       ,
          f_liquidacion                    ,
          tpo_movto                        ,
          aivs 
       INTO
          tmp_ret_det_canint_nss           ,
          tmp_ret_det_canint_desc_movto    ,
          tmp_ret_det_canint_f_liquidacion ,
          tmp_ret_det_canint_tpo_movto     ,
          tmp_ret_det_canint_aivs 
       FROM
          safre_tmp:tmp_ret_det_canint
       LET v_estado_solicitud            = 0;
       LET v_cod_rechazo_sol             = 0;
       IF ( TRIM(tmp_ret_det_canint_nss) IS NULL ) THEN
           -- se marca la bandera de rechazo de detalle
           LET v_id_derechohabiente   = 0;
           LET v_estado_solicitud     = v_error_estado_sol_rech;  -- INCONSISTENTE
           LET v_cod_rechazo_sol      = v_error_det_nss_no_encontrado;
           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
       ELSE       
           -- se obtiene el id_derechohabiente
           SELECT id_derechohabiente
           INTO   v_id_derechohabiente
           FROM   afi_derechohabiente
           WHERE  nss = tmp_ret_det_canint_nss;
           -- si no se encontro el id_derechohabiente
           IF ( v_id_derechohabiente IS NULL ) THEN
               -- se marca la bandera de rechazo de detalle
               LET v_id_derechohabiente   = 0;
               LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
               LET v_cod_rechazo_sol      = v_error_det_nss_no_encontrado;
               -- se incrementa el indice
               LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
           END IF
       END IF
      
       IF ( TRIM(tmp_ret_det_canint_desc_movto) = "" ) THEN
           -- se marca la bandera de rechazo de detalle
           LET v_id_derechohabiente   = 0;
           LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
           LET v_cod_rechazo_sol      = v_error_det_desc_movto_invalido;
           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
       END IF
       IF ( TRIM(tmp_ret_det_canint_f_liquidacion) IS NULL ) THEN
           -- se marca la bandera de rechazo de detalle
           LET v_id_derechohabiente   = 0;
           LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
           LET v_cod_rechazo_sol      = v_error_det_f_liquidacion_invalida;
           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
       END IF
       IF ( TRIM(tmp_ret_det_canint_tpo_movto) IS NULL OR 
            (tmp_ret_det_canint_tpo_movto <> 32 AND 
             tmp_ret_det_canint_tpo_movto <> 33 AND 
             tmp_ret_det_canint_tpo_movto <> 62 AND 
             tmp_ret_det_canint_tpo_movto <> 63)) THEN
           -- se marca la bandera de rechazo de detalle
           LET v_id_derechohabiente   = 0;
           LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
           LET v_cod_rechazo_sol      = v_error_det_tpo_operacion_invalido;
           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
       END IF
       IF ( TRIM(tmp_ret_det_canint_aivs) IS NULL OR
            tmp_ret_det_canint_aivs = "000000000000000") THEN
           -- se marca la bandera de rechazo de detalle
           LET v_id_derechohabiente   = 0;
           LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
           LET v_cod_rechazo_sol      = v_error_det_aivs_invalidas;
           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
       END IF

       -- se asignan los datos al registro
       LET ret_det_canint_folio                 = p_folio;                       -- decimal(9,0) ;
       LET ret_det_canint_id_derechohabiente    = v_id_derechohabiente;          -- decimal(9,0) ;
       LET ret_det_canint_nss                   = tmp_ret_det_canint_nss;        -- char(11)     ;
       LET ret_det_canint_desc_movto            = tmp_ret_det_canint_desc_movto; -- char(18)     ;
       LET v_c_f_operacion                      = SUBSTR(tmp_ret_det_canint_f_liquidacion,5,2) || "/" || SUBSTR(tmp_ret_det_canint_f_liquidacion,7,2) || "/" || SUBSTR(tmp_ret_det_canint_f_liquidacion,1,4);

       SELECT fn_ret_valida_fecha(SUBSTR(tmp_ret_det_canint_f_liquidacion,7,2),SUBSTR(tmp_ret_det_canint_f_liquidacion,5,2),SUBSTR(tmp_ret_det_canint_f_liquidacion,1,4)) 
       INTO   v_res_call_function
       FROM   systables 
       WHERE  tabid = 1;

       IF v_res_call_function <> 0 THEN 
          LET v_c_f_operacion = "01/01/0001";
          LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
          LET v_cod_rechazo_sol      = v_error_det_f_liquidacion_invalida;
       END IF
       LET v_fecha_f                            =  DATE(v_c_f_operacion);
       LET ret_det_canint_tpo_movto             = tmp_ret_det_canint_tpo_movto;  -- char(02)     ;
       FOR i = 1 TO (LENGTH(tmp_ret_det_canint_aivs) )
           LET v_caracter = SUBSTR(tmp_ret_det_canint_aivs,i,1);
           IF (v_caracter <> '0' AND 
               v_caracter <> '1' AND 
               v_caracter <> '2' AND
               v_caracter <> '3' AND 
               v_caracter <> '4' AND 
               v_caracter <> '5' AND 
               v_caracter <> '6' AND 
               v_caracter <> '7' AND 
               v_caracter <> '8' AND 
               v_caracter <> '9') THEN 
               LET v_estado_solicitud     = v_error_estado_sol_rech; -- INCONSISTENTE
               LET v_cod_rechazo_sol      = v_error_det_aivs_invalidas;
               EXIT FOR;
           END IF
       END FOR
       IF v_cod_rechazo_sol = v_error_det_aivs_invalidas THEN
           LET ret_det_canint_aivs                  = 0;
       ELSE 
           LET ret_det_canint_aivs                  = tmp_ret_det_canint_aivs / 1000000; -- decimal(15,6);
       END IF
           
       LET ret_det_canint_estado_solicitud      = v_estado_solicitud;            -- smallint ;
       LET ret_det_canint_cod_rechazo_sol       = v_cod_rechazo_sol;             -- smallint ;
                    
       -- se obtiene el id_solicitud
       SELECT seq_ret_det_canint.NEXTVAL
       INTO v_consec_canint
       FROM systables
       WHERE tabid = 1;

       -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
       INSERT INTO ret_det_canint (
         consec_detalle                     ,  
         folio                              ,   
         estado_solicitud                   ,
         cod_rechazo                        ,
         id_derechohabiente                 ,   
         nss                                ,   
         desc_movto                         ,   
         f_liquidacion                      ,   
         tpo_movto                          ,   
         aivs
       )
       VALUES (
         v_consec_canint      ,
         ret_det_canint_folio               ,
         ret_det_canint_estado_solicitud    ,
         ret_det_canint_cod_rechazo_sol     ,
         ret_det_canint_id_derechohabiente  ,
         ret_det_canint_nss                 ,
         ret_det_canint_desc_movto          ,
         v_fecha_f                          ,
         ret_det_canint_tpo_movto           ,
         ret_det_canint_aivs
       );
      
       -- se cuenta un registro insertado
       LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

   END FOREACH;
 
   UPDATE STATISTICS FOR TABLE ret_det_canint;
   UPDATE STATISTICS FOR TABLE ret_cza_canint;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


