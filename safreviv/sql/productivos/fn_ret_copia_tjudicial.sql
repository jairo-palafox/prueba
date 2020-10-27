






CREATE FUNCTION "safreviv".fn_ret_copia_tjudicial(p_usuario_cod CHAR(20),
                                  p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)
   -- campos de la tabla de detalle de carga inicial de tramite judicial
   DEFINE tmp_ret_det_TJ_tpo_registro          CHAR(2)      ;
   DEFINE tmp_ret_det_TJ_nss                   CHAR(11)     ;
   DEFINE tmp_ret_det_TJ_cod_amparo_laudo      DECIMAL(2,0) ;
   DEFINE tmp_ret_det_TJ_f_pago                CHAR(8)      ;
   DEFINE tmp_ret_det_TJ_importe_pagado        DECIMAL(10,0);
   DEFINE tmp_ret_det_TJ_num_registro          DECIMAL(8,0) ;
   DEFINE tmp_ret_det_TJ_resultado_operacion   CHAR(2)      ;
   DEFINE tmp_ret_det_TJ_motivo_rechazo1       CHAR(3)      ;
   DEFINE tmp_ret_det_TJ_motivo_rechazo2       CHAR(3)      ;

   -- tabla destino ret_datamart
   DEFINE ret_pago_juridico_id_solicitud         DECIMAL(9,0) ;
   DEFINE ret_pago_juridico_id_derechohabiente   DECIMAL(9,0) ;
   DEFINE ret_pago_juridico_tipo_resolucion      SMALLINT     ;
   DEFINE ret_pago_juridico_f_pago               DATE         ;
   DEFINE ret_pago_juridico_importe_pagado       DECIMAL(10,2);
   DEFINE ret_pago_juridico_num_registro         SMALLINT     ;
   DEFINE ret_pago_juridico_cod_rechazo          SMALLINT     ;

   -- para validaciones del contenido de archivo
   DEFINE v_tipo_resolucion                      SMALLINT;
   
   -- encabezado de carga de datamart

   --PARA REGRESAR EL ESATODO DE LAS SENTENCIAS SQL 
   DEFINE v_estatus_proceso                         SMALLINT     ;  
   DEFINE v_id_datamart                             DECIMAL (9,0);
   DEFINE v_cont_matriz_derecho                     SMALLINT     ;  
   DEFINE v_mensajeno_insert                        VARCHAR(255) ;
   DEFINE v_mensaje                                 VARCHAR(255) ;   
   DEFINE v_cont_registros                          INTEGER      ;
   DEFINE v_fecha                                   VARCHAR(10)  ;

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


   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_copia_tjudicial.txt";
   
   LET v_estatus_proceso     = 0 ;
   LET v_id_datamart         = 0 ;       
   LET v_cont_matriz_derecho = 0;
   LET v_cont_registros      = 0 ;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE safre_mig:glo_ctr_archivo
    SET folio = P_folio, estado = 2 -- integrado
    WHERE proceso_cod    = p_proceso_cod
      AND opera_cod      = 1 -- archivo cargado
      AND estado         = 1; -- etapa de carga
   
    -- Agregar folio a operacion de integracion
    UPDATE safre_mig:bat_ctr_operacion 
    SET folio         = P_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;

   --- se consultan los datos de la tabla temporal
   FOREACH
    SELECT      
      tpo_registro          ,
      nss                   ,
      cod_amparo_laudo      ,
      f_pago                ,
      importe_pagado        ,
      num_registro          ,
      resultado_operacion   ,
      motivo_rechazo1       ,
      motivo_rechazo2       
    INTO 
      tmp_ret_det_TJ_tpo_registro         ,
      tmp_ret_det_TJ_nss                  ,
      tmp_ret_det_TJ_cod_amparo_laudo     ,
      tmp_ret_det_TJ_f_pago               ,
      tmp_ret_det_TJ_importe_pagado       ,
      tmp_ret_det_TJ_num_registro         ,
      tmp_ret_det_TJ_resultado_operacion  ,
      tmp_ret_det_TJ_motivo_rechazo1      ,
      tmp_ret_det_TJ_motivo_rechazo2      
    FROM
      safre_mig:tmp_det_tramite_judicial
                   
     LET ret_pago_juridico_id_derechohabiente = 0 ;
       
      SELECT id_derechohabiente 
        INTO ret_pago_juridico_id_derechohabiente 
        FROM afi_derechohabiente
       WHERE nss = tmp_ret_det_TJ_nss;    
      
      IF ( ret_pago_juridico_id_derechohabiente IS NULL ) THEN
         --TRACE "No se encontro id_derechohabiente para NSS: " || tmp_ret_det_TJ_nss;
         CONTINUE FOREACH;
      END IF
       
      
      -- se transforma la fecha a mes, dia, ano
      LET v_fecha = tmp_ret_det_TJ_f_pago[5,6] || "/" || tmp_ret_det_TJ_f_pago[7,8] || "/" || tmp_ret_det_TJ_f_pago[1,4];
      
      -- se valida el tipo de resolucion
      LET v_tipo_resolucion = 0;
      
      SELECT tipo_resolucion
      INTO v_tipo_resolucion
      FROM ret_tipo_resolucion
      WHERE tipo_resolucion = tmp_ret_det_TJ_cod_amparo_laudo;
      
      -- si no se encontro, entonces no se inserta el registro
      IF ( v_tipo_resolucion IS NULL ) THEN
         CONTINUE FOREACH;
      END IF
      
      -- se asginan los datos al registro destino
      -- numero de solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO ret_pago_juridico_id_solicitud
      FROM systables
      WHERE tabid = 1;
      
      -- resto de los datos
      LET ret_pago_juridico_tipo_resolucion     = tmp_ret_det_TJ_cod_amparo_laudo; -- 16 o 26 ¿VALIDAR?
      LET ret_pago_juridico_f_pago              = DATE(v_fecha); --tmp_ret_det_TJ_f_pago; -- DATE         ;
      LET ret_pago_juridico_importe_pagado      = tmp_ret_det_TJ_importe_pagado / 100;
      LET ret_pago_juridico_num_registro        = tmp_ret_det_TJ_num_registro; 
      LET ret_pago_juridico_cod_rechazo         = 0; 
   
      --sse insertan los registros en ret_datamart 
      INSERT INTO ret_pago_juridico (
         id_solicitud       ,
         id_derechohabiente ,
         tipo_resolucion    ,
         f_pago             ,
         importe_pagado     ,
         num_registro       ,
         cod_rechazo        
                 )
         VALUES ( 
         ret_pago_juridico_id_solicitud       ,
         ret_pago_juridico_id_derechohabiente ,
         ret_pago_juridico_tipo_resolucion    ,
         ret_pago_juridico_f_pago             ,
         ret_pago_juridico_importe_pagado     ,
         ret_pago_juridico_num_registro       ,
         ret_pago_juridico_cod_rechazo        
                 );
   END FOREACH;
   
   --TRACE 'termino integracion de carga inicial de Tramite Judicial';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de carga inicial de Retiros Trámite Judicial finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt;
END FUNCTION;


