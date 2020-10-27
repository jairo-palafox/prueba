






CREATE FUNCTION "safreviv".fn_ret_integra_resp_fico_dap(p_id_archivo_fico DECIMAL(9,0),pid_proceso DECIMAL(9,0),p_folio DECIMAL(9,0),p_nom_archivo CHAR(40))
 RETURNING SMALLINT, SMALLINT, INTEGER, INTEGER, VARCHAR(250)

   --Variables de salida
   DEFINE v_respuesta         SMALLINT;
   DEFINE v_estado_solicitud  SMALLINT;
   DEFINE v_ax_cont_regs_det  INTEGER;
   DEFINE v_ax_error          SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_isam_err          INTEGER;
   DEFINE v_c_msj             VARCHAR(250);
   DEFINE v_modalidad_retiro  SMALLINT; -- modalidad de retiro de la solicitud
   DEFINE v_delegacion        CHAR(2);
   DEFINE v_concepto          CHAR(3);
   DEFINE v_id_solicitud      DECIMAL(9,0);       
   DEFINE v_monto             DECIMAL(10,2);
   DEFINE v_banco             CHAR(4);  
   DEFINE v_fecha_pago        CHAR(10);
   DEFINE v_fecha_pago_date   DATE; 
   DEFINE v_ref_definitiva    CHAR(25); 
   DEFINE v_acreedor          CHAR(10); 
   DEFINE v_sociedad          CHAR(4); 
   DEFINE v_gpo_ctas_acredor  CHAR(4); 
   DEFINE v_nom_acreedor1     CHAR(35); 
   DEFINE v_nom_acreedor2     CHAR(35); 
   DEFINE v_nom_beneficiario  CHAR(60);
   DEFINE v_clave_pais        CHAR(3); 
   DEFINE v_estado            CHAR(3); 
   DEFINE v_rfc               CHAR(13); 
   DEFINE v_nss               CHAR(11); 
   DEFINE v_lis_vias_pago     CHAR(10); 
   DEFINE v_deleg_municipio   CHAR(35); 
   DEFINE v_cta_contabilidad  CHAR(10); 
   DEFINE v_cta_clabe         CHAR(18); 
   DEFINE v_bandera           INTEGER;
   DEFINE v_cta_x_pagar       CHAR(10);
   DEFINE v_anho              CHAR(4);
   DEFINE v_acreedor_res      CHAR(10);
   DEFINE v_banco_inter       CHAR(4);
   DEFINE v_des_error         CHAR(35);
   DEFINE v_cod_rechazo       INTEGER;
     
   --DEFINE v_rechazo_ads       SMALLINT;
   ON EXCEPTION SET v_ax_error, v_isam_err,v_c_msj
       
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
   END EXCEPTION
  
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_integra_fico_dap.trace";
   
   SET PDQPRIORITY HIGH; 

   -- Se inicializan las variables de respuesta
   LET v_respuesta        = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_ax_error         = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_isam_err         = 0;
   LET v_c_msj            = "Integración ejecutada correctamente.";
   LET v_cod_rechazo      = 64; --Codigo de rechazo de fico
--trace "antes de actualizar monitor de procesos";
  --actualiza el folio de las tabla de control de operación
  UPDATE bat_ctr_operacion
  SET    folio = p_folio
  WHERE  pid   = pid_proceso ; 
  
  UPDATE bat_ctr_proceso
  SET    folio = p_folio
  WHERE  pid   = pid_proceso;
  
  -- actualiza el folio con el nombre de archivo
  UPDATE glo_ctr_archivo
  SET    folio = p_folio,
         estado = 2 -- integrado
  WHERE  nombre_archivo = p_nom_archivo;
  
--trace "antes de iniciar foreach de la temporal";
   -- se verifica la respuesta de cada caso
   FOREACH
      SELECT delegacion          ,
             concepto            ,
             referencia          ,
             monto               ,
             fecha_pago          ,
             ref_definitiva      ,
             acreedor            ,
             sociedad            ,
             gpo_ctas_acreedoras ,
             nom_acreedor1       ,
             nom_acreedor2       ,
             cve_pais            ,
             estado              ,
             rfc                 ,
             nss                 ,
             lis_vias_pago       ,
             deleg_municipio     ,
             cta_contabilidad    ,
             bandera             ,
             ctaxpagar           ,
             anho                ,
             acreedor_res        ,
             des_error       
   INTO     v_delegacion      ,   
            v_concepto        , 
            v_id_solicitud    , 
            v_monto           , 
            v_fecha_pago      , 
            v_ref_definitiva  , 
            v_acreedor        , 
            v_sociedad        , 
            v_gpo_ctas_acredor, 
            v_nom_acreedor1   , 
            v_nom_acreedor2   , 
            v_clave_pais      , 
            v_estado          , 
            v_rfc             , 
            v_nss             , 
            v_lis_vias_pago   , 
            v_deleg_municipio , 
            v_cta_contabilidad, 
            v_bandera         , 
            v_cta_x_pagar     , 
            v_anho            , 
            v_acreedor_res    , 
            v_des_error               
   FROM     safre_tmp:tmp_ret_res_fico_dap
--trace "obtener el edo solicitud y modalidad";
      -- se obtiene el estado de la solicitud y la modalidad
      SELECT estado_solicitud,
             modalidad_retiro
      INTO   v_estado_solicitud,
             v_modalidad_retiro
      FROM   ret_solicitud_generico      
      WHERE  id_solicitud = v_id_solicitud;
      
      -- si esta en espera de confirmacion
      IF ( v_estado_solicitud = 70 ) THEN
         --trace "evaluando bandera";
         -- si es rechazada
         IF ( v_bandera <> 0 ) THEN
               --trace "actualiza solicitudes rechazadas";
            -- se actualiza el estado de la solicitud a rechazada
            UPDATE ret_solicitud_generico 
            SET    estado_solicitud = 90,
                   cod_rechazo      = v_cod_rechazo
            WHERE  id_solicitud     = v_id_solicitud;
            
            -- se verifica la modalida de retiro 
            IF ( v_modalidad_retiro = 2 ) THEN -- FONDO DE AHORRO
               -- se actualiza la tabla de solicitudes historica   
               UPDATE ret_fondo_ahorro_generico
               SET    estado_solicitud = 90,
                      cod_rechazo      = v_cod_rechazo
               WHERE  id_solicitud     = v_id_solicitud;
            END IF
            
            --Se incrementa el contador
            LET v_ax_cont_regs_det = v_ax_cont_regs_det + 1;
	     ELSE
			   --Se actualiza el estado de la solicitud a Resp. archivo de solicitud pago fico
			   UPDATE ret_solicitud_generico 
            SET    estado_solicitud = 700,
                   cod_rechazo      = 0
            WHERE  id_solicitud     = v_id_solicitud;
			
			   -- se verifica la modalida de retiro 
            IF ( v_modalidad_retiro = 2 ) THEN -- FONDO DE AHORRO
               -- se actualiza la tabla de solicitudes historica   
               UPDATE ret_fondo_ahorro_generico
               SET    estado_solicitud = 700,
                      cod_rechazo      = 0
               WHERE  id_solicitud     = v_id_solicitud;
            END IF
			
         END IF
         --trace "actualiza solicitud con archivo";
         -- se actualiza que la solicitud fue respondida en el archivo de FICO dado por p_id_archivo_fico
         UPDATE ret_solicitud_generico 
         SET    id_archivo_respuesta = p_id_archivo_fico
         WHERE  id_solicitud         = v_id_solicitud;
         
         -- Se modifica la respuesta para indicar que se realizó al menos una actualización
         LET v_respuesta = 1;
      
      END IF;
      
      -- formatea la fecha
      LET v_fecha_pago_date = v_fecha_pago[4,5]||"/"||v_fecha_pago[1,2]||"/"||v_fecha_pago[7,10];

      --trace "inserta en respuesta fico";
      -- inserta en la tabla de respuesta FICO
      INSERT INTO ret_respuesta_fico (
         folio            ,
         delegacion       ,
         concepto         ,
         referencia       ,
         monto            ,
         fecha_pago       ,
         ref_definitiva   ,
         acreedor         ,
         sociedad         ,
         gpo_ctas_acredor ,
         nom_acreedor1    ,
         nom_acreedor2    ,
         clave_pais       ,
         estado           ,
         rfc              ,
         nss              ,
         lis_vias_pago    ,
         deleg_municipio  ,
         cta_contabilidad ,
         bandera          ,
         cta_x_pagar      ,
         anho             ,
         acreedor_res     ,
         des_error        ,
         f_actualiza      )   
      VALUES ( 
         p_folio           ,
         v_delegacion      ,   
         v_concepto        , 
         v_id_solicitud    , 
         v_monto           , 
         v_fecha_pago_date , 
         v_ref_definitiva  , 
         v_acreedor        , 
         v_sociedad        , 
         v_gpo_ctas_acredor, 
         v_nom_acreedor1   , 
         v_nom_acreedor2   ,
         v_clave_pais      , 
         v_estado          , 
         v_rfc             , 
         v_nss             , 
         v_lis_vias_pago   , 
         v_deleg_municipio , 
         v_cta_contabilidad, 
         v_bandera         , 
         v_cta_x_pagar     , 
         v_anho            , 
         v_acreedor_res    , 
         v_des_error       ,
         TODAY             );

   END FOREACH;
   
   -- ACTUALIZA ESTADÍSTICAS
   UPDATE STATISTICS FOR TABLE ret_respuesta_fico;
   
   SET PDQPRIORITY DEFAULT;
    
   -- se devuelve el resultado de la ejecucion
   RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
END FUNCTION;


