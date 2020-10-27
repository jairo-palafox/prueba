






CREATE FUNCTION "safreviv".fn_afi_integra_baja_sinf(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                    p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                    p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), DECIMAL(9,0), DECIMAL(9,0), DECIMAL(9,0)
  
-- campos de tmp_afi_baja
DEFINE tmp_afi_baja_tpo_movimiento      char(2)     ;
DEFINE tmp_afi_baja_espacios            char(2)     ;
DEFINE tmp_afi_baja_nrp                 char(11)    ;
DEFINE tmp_afi_baja_f_movimiento        char(8)     ;
DEFINE tmp_afi_baja_curp_rfc            char(18)    ;
DEFINE tmp_afi_baja_t_trabajador        decimal(1,0);
DEFINE tmp_afi_baja_nss                 char(11)    ;
DEFINE tmp_afi_baja_nombre              char(50)    ;
DEFINE tmp_afi_baja_presentacion_extemp decimal(1,0);
DEFINE tmp_afi_baja_jornada_semana      decimal(1,0);
DEFINE tmp_afi_baja_sdi                 decimal(6,0);
DEFINE tmp_afi_baja_sexo                decimal(1,0);
DEFINE tmp_afi_baja_nss_correcto        char(11)    ;
DEFINE tmp_afi_baja_nombre_correcto     char(50)    ;

-- campos de la tabla de rechazos afi_rch_afiliatorio
DEFINE afi_rch_afiliatorio_tpo_movimiento      char(2)     ;
DEFINE afi_rch_afiliatorio_espacios            char(2)     ;
DEFINE afi_rch_afiliatorio_nrp                 char(11)    ;
DEFINE afi_rch_afiliatorio_f_movimiento        char(8)     ;
DEFINE afi_rch_afiliatorio_curp_rfc            char(18)    ;
DEFINE afi_rch_afiliatorio_t_trabajador        decimal(1,0);
DEFINE afi_rch_afiliatorio_nss                 char(11)    ;
DEFINE afi_rch_afiliatorio_nombre              char(50)    ;
DEFINE afi_rch_afiliatorio_presentacion_extemp decimal(1,0);
DEFINE afi_rch_afiliatorio_jornada_semana      decimal(1,0);
DEFINE afi_rch_afiliatorio_sdi                 decimal(6,0);
DEFINE afi_rch_afiliatorio_sexo                decimal(1,0);
DEFINE afi_rch_afiliatorio_nss_correcto        char(11)    ;
DEFINE afi_rch_afiliatorio_nombre_correcto     char(50)    ;
DEFINE afi_rch_afiliatorio_cod_rechazo         smallint    ;

-- campos de la tabla afi_his_derechohabiente
DEFINE afi_his_derechohab_id_derechohabiente   decimal(9,0);
DEFINE afi_his_derechohab_f_modifica           date        ;
DEFINE afi_his_derechohab_folio_lote_modifica  decimal(9,0);
DEFINE afi_his_derechohab_ind_modifica         char(18)    ;
DEFINE afi_his_derechohab_curp                 char(18)    ;
DEFINE afi_his_derechohab_rfc                  char(13)    ;
DEFINE afi_his_derechohab_ind_nrp              char(1)     ;
DEFINE afi_his_derechohab_f_nacimiento         date        ;
DEFINE afi_his_derechohab_nombre_imss          char(50)    ;
DEFINE afi_his_derechohab_nombre_af            char(40)    ;
DEFINE afi_his_derechohab_ap_paterno_af        char(40)    ;
DEFINE afi_his_derechohab_ap_materno_af        char(40)    ;

-- campos de la tabla afi_relacion_laboral
DEFINE afi_relacion_laboral_id_derechohabiente decimal(9,0);
DEFINE afi_relacion_laboral_nrp                char(11)    ;
DEFINE afi_relacion_laboral_f_alta_nrp         date        ;
DEFINE afi_relacion_laboral_ind_relacion       smallint    ;
DEFINE afi_relacion_laboral_folio_lote         decimal(9,0);
DEFINE afi_relacion_laboral_f_actualiza        date        ;
DEFINE afi_relacion_laboral_usuario            char(20)    ;
  
DEFINE v_rfc                  VARCHAR(13);
DEFINE v_curp                 VARCHAR(18);
DEFINE v_rfc_curp             VARCHAR(18);
DEFINE v_conteo               INTEGER;
                                                     
-- Control de Excepciones
DEFINE v_i_resultado             SMALLINT;
DEFINE sql_err                   INTEGER;
DEFINE isam_err                  INTEGER;
DEFINE err_txt                   VARCHAR(255);
-- Variables de validaciones
DEFINE v_d_id_referencia         DECIMAL(9,0);
DEFINE v_id_derechohabiente      DECIMAL(9,0);
DEFINE v_fecha_valida            SMALLINT;
DEFINE v_fecha_movimiento        DATE;
DEFINE v_codigo_rechazo          SMALLINT;

-- conteo de bajas procesadas, aceptadas y rechazadas
DEFINE v_num_bajas_PROCESADAS    DECIMAL(9,0);
DEFINE v_num_bajas_ACEPTADAS     DECIMAL(9,0);
DEFINE v_num_bajas_RECHAZADAS    DECIMAL(9,0);

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt, v_num_bajas_PROCESADAS, v_num_bajas_ACEPTADAS, v_num_bajas_RECHAZADAS;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de movimientos afiliatorios de baja finalizó correctamente.";
   LET v_codigo_rechazo   = 0;
   
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_baja.trace";

   -- se inician los contadores
   LET v_num_bajas_PROCESADAS = 0;
   LET v_num_bajas_ACEPTADAS  = 0;
   LET v_num_bajas_RECHAZADAS = 0;

   --------------------------- BAJA DE RELACION LABORAL ----------------------------- 
   --trace "Se leen las bajas";
   FOREACH SELECT
      tpo_movimiento      ,
      nrp                 ,
      f_movimiento        ,
      curp_rfc            ,
      t_trabajador        ,
      nss                 ,
      nombre              ,
      presentacion_extemp ,
      jornada_semana      ,
      sdi                 ,
      sexo                ,
      nss_correcto        ,
      nombre_correcto     
   INTO 
      tmp_afi_baja_tpo_movimiento      ,
      tmp_afi_baja_nrp                 ,
      tmp_afi_baja_f_movimiento        ,
      tmp_afi_baja_curp_rfc            ,
      tmp_afi_baja_t_trabajador        ,
      tmp_afi_baja_nss                 ,
      tmp_afi_baja_nombre              ,
      tmp_afi_baja_presentacion_extemp ,
      tmp_afi_baja_jornada_semana      ,
      tmp_afi_baja_sdi                 ,
      tmp_afi_baja_sexo                ,
      tmp_afi_baja_nss_correcto        ,
      tmp_afi_baja_nombre_correcto     
   FROM safre_tmp:tmp_afi_sinf_baja

      -- se cuenta un registro procesado
      LET v_num_bajas_PROCESADAS = v_num_bajas_PROCESADAS + 1;

      -- se valida la fecha de movimiento
      EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_baja_f_movimiento,"ddmmyyyy")
         INTO v_fecha_valida, v_fecha_movimiento;
         
      -- si la fecha de movimiento es posterior a la actual, se rechaza el registro
      IF ( v_fecha_movimiento > TODAY ) THEN
         -- se rechaza el registro por tener fecha de movimiento invalida
         LET v_codigo_rechazo = 9;
         
         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                             tmp_afi_baja_nrp                 ,
                                                             tmp_afi_baja_f_movimiento        ,
                                                             tmp_afi_baja_curp_rfc            ,
                                                             tmp_afi_baja_t_trabajador        ,
                                                             tmp_afi_baja_nss                 ,
                                                             tmp_afi_baja_nombre              ,
                                                             tmp_afi_baja_presentacion_extemp ,
                                                             tmp_afi_baja_jornada_semana      ,
                                                             tmp_afi_baja_sdi                 ,
                                                             tmp_afi_baja_sexo                ,
                                                             tmp_afi_baja_nss_correcto        ,
                                                             tmp_afi_baja_nombre_correcto     ,
                                                             v_codigo_rechazo                 ,
                                                             p_folio                   
                                                             );

      
         -- se continua con el siguiente registro
         CONTINUE FOREACH;
      END IF


      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM  afi_derechohabiente
      WHERE nss = tmp_afi_baja_nss;
      
      -- si no se encuentra el NSS
      IF ( v_id_derechohabiente IS NULL ) THEN
      
         -- NSS no existe
         LET v_codigo_rechazo = 1;
         
         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                             tmp_afi_baja_nrp                 ,
                                                             tmp_afi_baja_f_movimiento        ,
                                                             tmp_afi_baja_curp_rfc            ,
                                                             tmp_afi_baja_t_trabajador        ,
                                                             tmp_afi_baja_nss                 ,
                                                             tmp_afi_baja_nombre              ,
                                                             tmp_afi_baja_presentacion_extemp ,
                                                             tmp_afi_baja_jornada_semana      ,
                                                             tmp_afi_baja_sdi                 ,
                                                             tmp_afi_baja_sexo                ,
                                                             tmp_afi_baja_nss_correcto        ,
                                                             tmp_afi_baja_nombre_correcto     ,
                                                             v_codigo_rechazo                      ,
                                                             p_folio                   
                                                             );
         CONTINUE FOREACH;
      END IF

      -- se da de baja la relacion laboral
      DELETE FROM afi_relacion_laboral
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    nrp                = tmp_afi_baja_nrp;
      
      -- se verifica si se elimino un registro
      IF ( DBINFO('sqlca.sqlerrd2') > 0 ) THEN
         LET v_num_bajas_ACEPTADAS = v_num_bajas_ACEPTADAS + 1;
         
         -- si ya no se tienen registros de relacion laboral, se actualiza el derechohabiente
         -- en afi_Derechohabiente como que no tiene relacion laboral alguna
         SELECT COUNT(*)
         INTO v_conteo
         FROM afi_relacion_laboral
         WHERE id_derechohabiente = v_id_derechohabiente;
             
         -- si no hay
         IF ( v_conteo < 1 ) THEN
	       
		        -- se guarda el historico del campo que se esta cambiando
		        SELECT nombre_imss  ,
                   curp         ,
                   rfc          ,
                   ind_nrp      ,
                   f_nacimiento ,
                   nombre_af    ,
                   ap_paterno_af,
                   ap_materno_af
            INTO   afi_his_derechohab_nombre_imss   ,
                   afi_his_derechohab_curp          ,
                   afi_his_derechohab_rfc           ,
                   afi_his_derechohab_ind_nrp       ,
                   afi_his_derechohab_f_nacimiento  ,
                   afi_his_derechohab_nombre_af     ,
                   afi_his_derechohab_ap_paterno_af ,
                   afi_his_derechohab_ap_materno_af
            FROM   afi_derechohabiente
            WHERE  id_derechohabiente = v_id_derechohabiente;
         
            -- se asingnan los datos al registro de historico
		        LET afi_his_derechohab_id_derechohabiente   = v_id_derechohabiente;
            LET afi_his_derechohab_f_modifica           = TODAY; -- fecha de cambio
            LET afi_his_derechohab_folio_lote_modifica  = p_folio;
            LET afi_his_derechohab_ind_modifica         = 7; -- BAJA RELACION LABORAL

		        -- se inserta el historico
		        INSERT INTO afi_his_derechohabiente (
			              id_derechohabiente   ,
                    f_modifica           ,
                    folio_lote_modifica  ,
                    ind_modifica         ,
                    curp                 ,
                    rfc                  ,
                    ind_nrp              ,
                    f_nacimiento         ,
                    nombre_imss          ,
                    nombre_af            ,
                    ap_paterno_af        ,
                    ap_materno_af        
            ) VALUES (
                    afi_his_derechohab_id_derechohabiente  ,
                    afi_his_derechohab_f_modifica          ,
                    afi_his_derechohab_folio_lote_modifica ,
                    afi_his_derechohab_ind_modifica        ,
                    afi_his_derechohab_curp                ,
                    afi_his_derechohab_rfc                 ,
                    afi_his_derechohab_ind_nrp             ,
                    afi_his_derechohab_f_nacimiento        ,
                    afi_his_derechohab_nombre_imss         ,
                    afi_his_derechohab_nombre_af           ,
                    afi_his_derechohab_ap_paterno_af       ,
                    afi_his_derechohab_ap_materno_af       
            );
	  
            -- se actualiza el derechohabiente indicando que ya no tiene relaciones laborales
            UPDATE afi_derechohabiente
            SET ind_nrp = 0
            WHERE id_derechohabiente = v_id_derechohabiente;
         END IF
      ELSE
         -- no existia la relacion entre el NRP y el NSS dado
         -- se rechaza el registro
         LET v_codigo_rechazo = 12;
         
         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                             tmp_afi_baja_nrp                 ,
                                                             tmp_afi_baja_f_movimiento        ,
                                                             tmp_afi_baja_curp_rfc            ,
                                                             tmp_afi_baja_t_trabajador        ,
                                                             tmp_afi_baja_nss                 ,
                                                             tmp_afi_baja_nombre              ,
                                                             tmp_afi_baja_presentacion_extemp ,
                                                             tmp_afi_baja_jornada_semana      ,
                                                             tmp_afi_baja_sdi                 ,
                                                             tmp_afi_baja_sexo                ,
                                                             tmp_afi_baja_nss_correcto        ,
                                                             tmp_afi_baja_nombre_correcto     ,
                                                             v_codigo_rechazo                 ,
                                                             p_folio                   
                                                             );
      END IF      
   END FOREACH;
           
   -- se actualizan las estadisticas de las tablas destino   
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
  
   -- se cuentan las bajas rechazadas
   SELECT COUNT(*)
   INTO   v_num_bajas_RECHAZADAS
   FROM   afi_rch_afiliatorio
   WHERE  tpo_movimiento = "02"
   AND    folio_lote     = p_folio;
  
   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Bajas realizadas: " || v_num_bajas_ACEPTADAS;
  
   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt, v_num_bajas_PROCESADAS, v_num_bajas_ACEPTADAS, v_num_bajas_RECHAZADAS;
END FUNCTION
;


