






CREATE FUNCTION "safreviv".sp_integra_fico(p_id_archivo_fico DECIMAL(9,0),pid_proceso DECIMAL(9,0),p_folio DECIMAL(9,0),p_nom_archivo CHAR(40))
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
   DEFINE v_fecha_pago        VARCHAR(10);
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
   DEFINE v_consec_beneficiario SMALLINT;
   DEFINE v_id_solicitud_sin_consec DECIMAL(9,0);
   DEFINE v_existe_ret_sol_gen SMALLINT;
   DEFINE v_ref_paso          CHAR(20);
   DEFINE v_posicion_dssv     SMALLINT;
   DEFINE v_total_beneficiarios SMALLINT;
   DEFINE v_beneficiario_procesados SMALLINT;
   DEFINE v_beneficiarios_700 SMALLINT;
   
     
   --DEFINE v_rechazo_ads       SMALLINT;
   ON EXCEPTION SET v_ax_error, v_isam_err,v_c_msj
       
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
   END EXCEPTION
  
   
--   SET PDQPRIORITY HIGH; 

   -- Se inicializan las variables de respuesta
   LET v_respuesta        = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_ax_error         = 0;
   LET v_ax_cont_regs_det = 0;
   LET v_isam_err         = 0;
   LET v_c_msj            = "Integración ejecutada correctamente.";
   LET v_cod_rechazo      = 64; --Codigo de rechazo de fico
   LET v_consec_beneficiario = 1;
   LET v_id_solicitud_sin_consec = 0;
   LET v_existe_ret_sol_gen = 0;

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
  

   -- se verifica la respuesta de cada caso
   FOREACH
      SELECT delegacion          ,
             concepto            ,
             referencia          ,
             monto               ,
             banco               ,
             fecha_pago          ,
             ref_definitiva      ,
             acreedor            ,
             sociedad            ,
             gpo_ctas_acreedoras ,
             nom_acreedor1       ,
             nom_acreedor2       ,
             nom_beneficiario    ,
             cve_pais            ,
             estado              ,
             rfc                 ,
             nss                 ,
             lis_vias_pago       ,
             deleg_municipio     ,
             cta_contabilidad    ,
             cta_clabe           ,
             bandera             ,
             ctaxpagar           ,
             anho                ,
             acreedor_res        ,
             banco_inter         ,
             des_error       
   INTO     v_delegacion      ,   
            v_concepto        , 
            v_ref_paso        , 
            v_monto           , 
            v_banco           , 
            v_fecha_pago      , 
            v_ref_definitiva  , 
            v_acreedor        , 
            v_sociedad        , 
            v_gpo_ctas_acredor, 
            v_nom_acreedor1   , 
            v_nom_acreedor2   , 
            v_nom_beneficiario,
            v_clave_pais      , 
            v_estado          , 
            v_rfc             , 
            v_nss             , 
            v_lis_vias_pago   , 
            v_deleg_municipio , 
            v_cta_contabilidad, 
            v_cta_clabe       , 
            v_bandera         , 
            v_cta_x_pagar     , 
            v_anho            , 
            v_acreedor_res    , 
            v_banco_inter     , 
            v_des_error               
   FROM     safre_tmp:tmp_ret_res_fico
      
      -- Se limpia la referencia para quitarle las letras DSSV cuando es de beneficiarios
      LET v_posicion_dssv = 0;
      LET v_posicion_dssv = INSTR(v_ref_paso, "D");
      IF v_posicion_dssv = 0 THEN 
         LET v_id_solicitud = v_ref_paso;
      ELSE 
         LET v_id_solicitud = SUBSTR(v_ref_paso,1,v_posicion_dssv - 1);
      END IF
      -- se obtiene el estado de la solicitud y la modalidad
      
      SELECT COUNT(*)
      INTO   v_existe_ret_sol_gen
      FROM   ret_solicitud_generico      
      WHERE  id_solicitud = v_id_solicitud;
      
      IF (v_existe_ret_sol_gen = 0) THEN --- Se busca en beneficiario_juridico
         SELECT estado_solicitud, 3, consec_beneficiario, id_solicitud
         INTO   v_estado_solicitud,
                v_modalidad_retiro,
                v_consec_beneficiario,
                v_id_solicitud_sin_consec
         FROM   ret_beneficiario_juridico
         WHERE  id_solicitud||consec_beneficiario = v_id_solicitud;
         IF v_estado_solicitud = 70 THEN 
            IF ( v_bandera <> 0 ) THEN
                LET v_estado_solicitud = 90;
                LET v_cod_rechazo      = 64;
                --Se incrementa el contador
                LET v_ax_cont_regs_det = v_ax_cont_regs_det + 1;
            ELSE 
                LET v_estado_solicitud = 700;
                LET v_cod_rechazo      = 0;
            END IF 
            LET  v_beneficiario_procesados = 0;
            LET v_total_beneficiarios      = 0;
            LET v_beneficiarios_700        = 0;
            -- se actualiza el estado de la solicitud a rechazada
            UPDATE ret_beneficiario_juridico
            SET    estado_solicitud    = v_estado_solicitud,
                   cod_rechazo         = v_cod_rechazo
            WHERE  id_solicitud        = v_id_solicitud_sin_consec
            AND    consec_beneficiario = v_consec_beneficiario;
            --  Valida si ya llegaron todos los beneficiarios
            SELECT COUNT(*)
            INTO   v_beneficiario_procesados
            FROM   ret_beneficiario_juridico a,
                   ret_beneficiario_generico b
            WHERE  a.id_solicitud        = b.id_solicitud
            AND    a.consec_beneficiario = b.consec_beneficiario
            AND    a.estado_solicitud    IN (700,90)
            AND    b.porcentaje          > 0
            AND    a.id_solicitud        IN (SELECT id_solicitud
                                             FROM   ret_beneficiario_juridico
                                             WHERE  id_solicitud||consec_beneficiario = v_id_solicitud);

            SELECT COUNT(*)
            INTO   v_total_beneficiarios
            FROM   ret_beneficiario_generico
            WHERE  id_solicitud IN (SELECT id_solicitud
                                    FROM   ret_beneficiario_juridico
                                    WHERE  id_solicitud||consec_beneficiario = v_id_solicitud)
            AND    porcentaje   > 0;
            
            IF v_beneficiario_procesados = v_total_beneficiarios THEN 
               -- Busca el estado que se le pondrá a la solicitud
               SELECT COUNT(*)
               INTO   v_beneficiarios_700
               FROM   ret_beneficiario_juridico a,
                      ret_beneficiario_generico b
               WHERE  a.id_solicitud        = b.id_solicitud
               AND    a.consec_beneficiario = b.consec_beneficiario
               AND    a.estado_solicitud    IN (700)
               AND    b.porcentaje          > 0
               AND    a.id_solicitud        IN (SELECT id_solicitud
                                                FROM   ret_beneficiario_juridico
                                                WHERE  id_solicitud||consec_beneficiario = v_id_solicitud);
               IF v_beneficiarios_700 = v_total_beneficiarios THEN 
                  LET v_estado_solicitud = 700;
                  LET v_cod_rechazo      =   0;
               ELSE 
                  IF v_beneficiarios_700 = 0 THEN 
                     LET v_estado_solicitud = 90;
                     LET v_cod_rechazo      = 64;
                  ELSE 
                     LET v_estado_solicitud = 790;   --- Aceptados Parcialmente
                     LET v_cod_rechazo      =  64;
                  END IF 
               END IF 
               UPDATE ret_solicitud_generico 
               SET    estado_solicitud     = v_estado_solicitud,
                      cod_rechazo          = v_cod_rechazo,
                      id_archivo_respuesta = p_id_archivo_fico
               WHERE  id_solicitud         = v_id_solicitud_sin_consec
               AND    estado_solicitud     = 70;
                  
               UPDATE ret_ley73_generico
               SET    estado_solicitud = v_estado_solicitud,
                      cod_rechazo      = v_cod_rechazo
               WHERE  id_solicitud     = v_id_solicitud_sin_consec
               AND    estado_solicitud = 70;
            END IF 
            -- Se modifica la respuesta para indicar que se realizó al menos una actualización
            LET v_respuesta = 1;
         END IF 
      ELSE 
         SELECT estado_solicitud,
                modalidad_retiro
         INTO   v_estado_solicitud,
                v_modalidad_retiro
         FROM   ret_solicitud_generico      
         WHERE  id_solicitud = v_id_solicitud;

         -- si esta en espera de confirmacion
         IF ( v_estado_solicitud = 70) THEN
            
            -- si es rechazada
            IF ( v_bandera <> 0 ) THEN
                LET v_estado_solicitud = 90;
                LET v_cod_rechazo      = 64;
                --Se incrementa el contador
                LET v_ax_cont_regs_det = v_ax_cont_regs_det + 1;
            ELSE 
                LET v_estado_solicitud = 700;
                LET v_cod_rechazo      = 0;
            END IF 
            -- se actualiza el estado de la solicitud a rechazada
            UPDATE ret_solicitud_generico 
            SET    estado_solicitud     = v_estado_solicitud,
                   cod_rechazo          = v_cod_rechazo,
                   id_archivo_respuesta = p_id_archivo_fico
            WHERE  id_solicitud         = v_id_solicitud;

            -- se verifica la modalida de retiro 
            IF ( v_modalidad_retiro = 2 ) THEN -- FONDO DE AHORRO
               -- se actualiza la tabla de solicitudes historica   
               UPDATE ret_fondo_ahorro_generico
               SET    estado_solicitud = v_estado_solicitud,
                      cod_rechazo      = v_cod_rechazo
               WHERE  id_solicitud     = v_id_solicitud;
            END IF

            IF ( v_modalidad_retiro = 3 ) THEN -- LEY 73
               -- se actualiza la tabla de solicitudes historica   
               UPDATE ret_ley73_generico
               SET    estado_solicitud = v_estado_solicitud,
                      cod_rechazo      = v_cod_rechazo
               WHERE  id_solicitud     = v_id_solicitud;
            END IF

            IF ( v_modalidad_retiro = 9 ) THEN -- AMORTIZACIONES EXCEDENTES
               -- se actualiza la tabla de solicitudes historica   
               UPDATE ret_amort_excedente
               SET    estado_solicitud = v_estado_solicitud,
                      cod_rechazo      = v_cod_rechazo
               WHERE  id_solicitud     = v_id_solicitud;
            END IF
          
            -- Se modifica la respuesta para indicar que se realizó al menos una actualización
            LET v_respuesta = 1;
         
         END IF;
      END IF 
      -- formatea la fecha
      LET v_fecha_pago_date=v_fecha_pago[4,5]||"/"||v_fecha_pago[1,2]||"/"||v_fecha_pago[7,10];

      
      -- inserta en la tabla de respuesta FICO
      INSERT INTO ret_respuesta_fico(folio            ,
                                     delegacion       ,
                                     concepto         ,
                                     referencia       ,
                                     monto            ,
                                     banco            ,
                                     fecha_pago       ,
                                     ref_definitiva   ,
                                     acreedor         ,
                                     sociedad         ,
                                     gpo_ctas_acredor ,
                                     nom_acreedor1    ,
                                     nom_acreedor2    ,
                                     nom_beneficiario ,
                                     clave_pais       ,
                                     estado           ,
                                     rfc              ,
                                     nss              ,
                                     lis_vias_pago    ,
                                     deleg_municipio  ,
                                     cta_contabilidad ,
                                     cta_clabe        ,
                                     bandera          ,
                                     cta_x_pagar      ,
                                     anho             ,
                                     acreedor_res     ,
                                     banco_inter      ,
                                     des_error        ,
                                     f_actualiza      )   
      VALUES( p_folio           ,
              v_delegacion      ,   
              v_concepto        , 
              v_id_solicitud    , 
              v_monto           , 
              v_banco           , 
              v_fecha_pago_date , 
              v_ref_definitiva  , 
              v_acreedor        , 
              v_sociedad        , 
              v_gpo_ctas_acredor, 
              v_nom_acreedor1   , 
              v_nom_acreedor2   ,
              v_nom_beneficiario,
              v_clave_pais      , 
              v_estado          , 
              v_rfc             , 
              v_nss             , 
              v_lis_vias_pago   , 
              v_deleg_municipio , 
              v_cta_contabilidad, 
              v_cta_clabe       , 
              v_bandera         , 
              v_cta_x_pagar     , 
              v_anho            , 
              v_acreedor_res    , 
              v_banco_inter     , 
              v_des_error,
              TODAY);
      
          

   END FOREACH;
   -- ACTUALIZA ESTADÍSTICAS
   UPDATE STATISTICS FOR TABLE ret_respuesta_fico;
   
   
--   SET PDQPRIORITY DEFAULT;
    
   -- se devuelve el resultado de la ejecucion
   RETURN v_ax_error, v_respuesta, v_ax_cont_regs_det, v_isam_err, v_c_msj;
END FUNCTION;


