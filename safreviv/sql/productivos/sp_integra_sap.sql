






CREATE FUNCTION "safreviv".sp_integra_sap(p_id_archivo_fico DECIMAL(9,0),pid_proceso DECIMAL(9,0),
                               p_folio DECIMAL(9,0),p_nom_archivo CHAR(40))
 RETURNING SMALLINT, SMALLINT, INTEGER, INTEGER, VARCHAR(250)

--Variables de salida
DEFINE v_respuesta         SMALLINT;
DEFINE v_estado_solicitud  SMALLINT;
DEFINE v_ax_cont_regs_det  INTEGER;
DEFINE v_ax_error          SMALLINT; -- contiene el código de error en caso de ocurrir
DEFINE v_isam_err          INTEGER;
DEFINE v_c_msj             VARCHAR(250);
DEFINE v_id_solicitud      DECIMAL(9,0);       
DEFINE v_sociedad          CHAR(4); 
DEFINE v_num_documento     CHAR(10);
DEFINE v_ejercicio         CHAR(4);
DEFINE v_bandera           SMALLINT;
DEFINE v_cta_x_pagar       CHAR(10);
DEFINE v_anho              CHAR(4);
DEFINE v_des_error         CHAR(35);
DEFINE v_modalidad_retiro  SMALLINT;
     
   --DEFINE v_rechazo_ads       SMALLINT;
   ON EXCEPTION SET v_ax_error, v_isam_err,v_c_msj
       
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
   END EXCEPTION
  
   
   SET PDQPRIORITY HIGH; 

   -- Se inicializan las variables de respuesta
   LET v_respuesta        = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_ax_error         = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_isam_err         = 0;
   LET v_c_msj            = "Integración ejecutada correctamente.";
   LET v_modalidad_retiro = 0;

   --actualiza el folio de las tabla de control de operación
   UPDATE bat_ctr_operacion
   SET    folio = p_folio
   WHERE  pid = pid_proceso ; 
   
   UPDATE bat_ctr_proceso
   SET    folio = p_folio
   WHERE  pid = pid_proceso;
   
   -- actualiza el folio con el nombre de archivo
   UPDATE glo_ctr_archivo
   SET    folio = p_folio
   WHERE  nombre_archivo = p_nom_archivo;
  

   -- se verifica la respuesta de cada caso
   FOREACH
   SELECT sociedad            ,
          num_documento       ,
          ejercicio           ,
          bandera             ,
          cta_x_pagar         ,
          ano                 ,
          des_error           
   INTO   v_sociedad        , 
          v_num_documento   , 
          v_ejercicio       , 
          v_bandera         , 
          v_cta_x_pagar     , 
          v_anho            , 
          v_des_error               
   FROM safre_tmp:tmp_ret_res_cxp_fico
    
      --Se obtiene el número de solicitud de la ret_respuesta_fico
      SELECT referencia 
      INTO   v_id_solicitud 
      FROM   ret_respuesta_fico  
      WHERE  cta_x_pagar = v_num_documento;
    
      -- inserta en la tabla de respuesta FICO de CXP
      INSERT INTO ret_respuesta_fico_cxp (
         folio        ,
         id_solicitud ,
         sociedad     ,
         num_documento,
         ejercicio    ,
         bandera      ,
         cta_x_pagar  ,
         ano          ,
         des_error    ,
         f_actualiza  )
      VALUES (
         p_folio           ,
         v_id_solicitud    ,   
         v_sociedad        ,  
         v_num_documento   ,  
         v_ejercicio       ,  
         v_bandera         ,  
         v_cta_x_pagar     ,  
         v_anho            ,  
         v_des_error       ,
         TODAY);
         
      -- si se encontro la solicitud, se cambia su estatus a 212, cancelada cuenta por pagar
      IF ( v_id_solicitud IS NOT NULL AND v_bandera = 0 ) THEN
         UPDATE ret_solicitud_generico
         SET    estado_solicitud    = 212,
                id_archivo_resp_cxp = p_id_archivo_fico -- id del archivo de respuesta
         WHERE  id_solicitud        = v_id_solicitud;
         
         -- se obtiene la modalidad para actualizar la tabla de solicitudes de retiro
         SELECT modalidad_retiro
         INTO   v_modalidad_retiro
         FROM   ret_solicitud_generico
         WHERE  id_solicitud = v_id_solicitud;
         
         -- se realiza la actualizacion
         IF ( v_modalidad_retiro = 2 ) THEN -- FONDO DE AHORRO
            UPDATE ret_fondo_ahorro_generico
            SET    estado_solicitud = 212            
            WHERE  id_solicitud     = v_id_solicitud;
         END IF
         
         IF ( v_modalidad_retiro = 3 ) THEN -- LEY 73
            UPDATE ret_ley73_generico
            SET    estado_solicitud = 212            
            WHERE  id_solicitud     = v_id_solicitud;
         END IF
         
         IF ( v_modalidad_retiro = 9 ) THEN -- AMORTIZACIONES EXCEDENTES
            UPDATE ret_amort_excedente
            SET    estado_solicitud = 212            
            WHERE  id_solicitud     = v_id_solicitud;
         END IF
         
         IF ( v_modalidad_retiro = 10 ) THEN -- APORTACIONES VOLUNTARIAS
            UPDATE ret_voluntaria
            SET    estado_solicitud = 212            
            WHERE  id_solicitud     = v_id_solicitud;
         END IF

      END IF

   END FOREACH;
   
   -- ACTUALIZA ESTADÍSTICAS
   UPDATE STATISTICS FOR TABLE ret_respuesta_fico_cxp;
   
   SET PDQPRIORITY DEFAULT;
    
   -- se devuelve el resultado de la ejecucion
   RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
END FUNCTION;


