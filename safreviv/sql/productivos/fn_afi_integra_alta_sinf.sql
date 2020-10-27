






CREATE FUNCTION "safreviv".fn_afi_integra_alta_sinf(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                    p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                    p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER
  
-- campos de tmp_afi_alta
DEFINE tmp_afi_alta_tpo_movimiento      char(2)     ;
DEFINE tmp_afi_alta_espacios            char(2)     ;
DEFINE tmp_afi_alta_nrp                 char(11)    ;
DEFINE tmp_afi_alta_f_movimiento        char(8)     ;
DEFINE tmp_afi_alta_curp_rfc            char(18)    ;
DEFINE tmp_afi_alta_t_trabajador        decimal(1,0);
DEFINE tmp_afi_alta_nss                 char(11)    ;
DEFINE tmp_afi_alta_nombre              char(50)    ;
DEFINE tmp_afi_alta_presentacion_extemp decimal(1,0);
DEFINE tmp_afi_alta_jornada_semana      decimal(1,0);
DEFINE tmp_afi_alta_sdi                 decimal(6,0);
DEFINE tmp_afi_alta_sexo                decimal(1,0);
DEFINE tmp_afi_alta_nss_correcto        char(11)    ;
DEFINE tmp_afi_alta_nombre_correcto     char(50)    ;

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


-- campos de la tabla afi_relacion_laboral
DEFINE afi_relacion_laboral_id_derechohabiente decimal(9,0);
DEFINE afi_relacion_laboral_nrp                char(11)    ;
DEFINE afi_relacion_laboral_f_alta_nrp         date        ;
DEFINE afi_relacion_laboral_ind_relacion       smallint    ;
DEFINE afi_relacion_laboral_folio_lote         decimal(9,0);
DEFINE afi_relacion_laboral_f_actualiza        date        ;
DEFINE afi_relacion_laboral_usuario            char(20)    ;
  
-- para cambiar el formato de la fecha
DEFINE v_fecha_texto          VARCHAR(10);
  
DEFINE v_rfc                  VARCHAR(13);
DEFINE v_curp                 VARCHAR(18);
DEFINE v_rfc_curp             VARCHAR(18);
                                                     
-- Control de Excepciones
DEFINE v_i_resultado             SMALLINT;
DEFINE sql_err                   INTEGER;
DEFINE isam_err                  INTEGER;
DEFINE err_txt                   VARCHAR(255);
DEFINE v_fecha_valida            SMALLINT;
DEFINE v_fecha_movimiento        DATE;


-- Variables de validaciones
DEFINE v_d_id_referencia                 DECIMAL(9,0);
DEFINE v_id_derechohabiente              DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado    DECIMAL(9,0);
DEFINE v_diagnostico_unificador          SMALLINT;
DEFINE v_diagnostico_unificadas          SMALLINT;
DEFINE v_diagnostico_rechazo             SMALLINT;
DEFINE v_estado_familia_unificador       SMALLINT;
DEFINE v_estado_familia_unificado        SMALLINT;

-- numero de altas aceptadas y rechzadas
DEFINE v_num_altas_aceptadas             INTEGER;
DEFINE v_num_altas_rechazadas            INTEGER;
DEFINE v_registro_rechazado              SMALLINT; -- booleana para verificar un registro rechazado
DEFINE v_solo_NRP                        SMALLINT; -- para indicar si solo se debe dar de alta el NRP
-- Variable para marca de cuenta
DEFINE v_i_estado_marca                  INTEGER;

-- constantes para codigos de error
DEFINE v_error_nss_vacio                SMALLINT;
DEFINE v_error_nss_ya_existe            SMALLINT;
DEFINE v_error_fecha_movimiento_vacia   SMALLINT;
DEFINE v_error_nrp_vacio                SMALLINT;
DEFINE v_error_nombre_vacio             SMALLINT;
DEFINE v_error_fec_mov_posterior_actual SMALLINT; -- la fecha de movimiento es posterior a la actual

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de Alta finalizó correctamente.";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_alta_sinf.trace";

   -- se definen las constantes de codigo de error
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_ya_existe            = 2;
   LET v_error_fecha_movimiento_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_fec_mov_posterior_actual = 9;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_num_altas_aceptadas  = 0;
   LET v_num_altas_rechazadas = 0;

   --------------------------- ALTA ----------------------------- 
   --trace "Se leen las altas";
   FOREACH SELECT
      tpo_movimiento      ,
      espacios            ,
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
      tmp_afi_alta_tpo_movimiento      ,
      tmp_afi_alta_espacios            ,
      tmp_afi_alta_nrp                 ,
      tmp_afi_alta_f_movimiento        ,
      tmp_afi_alta_curp_rfc            ,
      tmp_afi_alta_t_trabajador        ,
      tmp_afi_alta_nss                 ,
      tmp_afi_alta_nombre              ,
      tmp_afi_alta_presentacion_extemp ,
      tmp_afi_alta_jornada_semana      ,
      tmp_afi_alta_sdi                 ,
      tmp_afi_alta_sexo                ,
      tmp_afi_alta_nss_correcto        ,
      tmp_afi_alta_nombre_correcto     
   FROM safre_tmp:tmp_afi_sinf_alta
      
      -- se asume que el registro es correcto
      LET v_registro_rechazado = 0;
      LET v_solo_NRP = 0;
      
      LET v_fecha_valida     = 0;
      LET v_fecha_movimiento = NULL;
 
      -- ====================================================================
      -- ====================================================================
      --   VERIFICACION DE ERRORES EN LA INFORMACION CARGADA      
      -- ====================================================================
      -- ====================================================================
      
      -- se verifica que la fecha de movimiento exista
      IF ( tmp_afi_alta_f_movimiento IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
        
         LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacia     
      END IF
      
      -- se valida la fecha de movimiento
      EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_alta_f_movimiento,"ddmmyyyy")
         INTO v_fecha_valida, v_fecha_movimiento;
            
      -- si la fecha no es valida, es decir, es erronea, no se puede validar el registro
      IF ( v_fecha_valida = 0 ) THEN
         -- fecha invalida
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
        
         LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacia     
      END IF
      
      -- se verifica que la fecha de movimiento sea igual o inferior a la actual
      IF ( v_fecha_movimiento > TODAY AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
                  
         LET afi_rch_afiliatorio_cod_rechazo = v_error_fec_mov_posterior_actual; -- La fecha de movimiento es posterior a la actual
      END IF

      
      -- se verifica que el NSS venga
      IF ( tmp_afi_alta_nss IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
         
         LET tmp_afi_alta_nss = "NULO";
         
         LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_vacio; -- NSS vacio
      END IF
      
      -- se verifica si el NSS ya se ha dado de alta
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_afi_alta_nss;
      
      -- si el nss se encontro entonces ya no se agrega
      IF ( v_id_derechohabiente IS NOT NULL AND v_registro_rechazado = 0 ) THEN
         --TRACE "El NSS ya existia en base de datos";
         
         SELECT nrp
         INTO   afi_relacion_laboral_nrp
         FROM   afi_relacion_laboral
         WHERE  nrp = tmp_afi_alta_nrp
         AND    id_derechohabiente = v_id_derechohabiente;
         
         IF ( afi_relacion_laboral_nrp IS NULL ) THEN
            -- se indica que solo se de de alta el NRP
            LET v_solo_NRP = 1;
         ELSE
            -- se marca el rechazo porque ya existe el NSS y el NRP
            LET v_registro_rechazado = 1;
            LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_ya_existe; -- NSS YA EXISTE
         END IF
         
      END IF
      
      -- se verifica que el NRP exista
      IF ( tmp_afi_alta_nrp IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
         
         LET tmp_afi_alta_nrp = "NULO";
         
         LET afi_rch_afiliatorio_cod_rechazo = v_error_nrp_vacio; -- NRP vacio
      END IF

      -- se verifica que el nombre exista
      IF ( tmp_afi_alta_nombre IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
         
         LET tmp_afi_alta_nombre = "NULO";
         
         LET afi_rch_afiliatorio_cod_rechazo = v_error_nombre_vacio; -- Nombre vacio
      END IF
      
      IF ( v_registro_rechazado = 1 ) THEN
         -- se inserta en la tabla de rechazos
         -- no se pudo abrir la cuenta, se registra en rechazos
         LET afi_rch_afiliatorio_tpo_movimiento      = tmp_afi_alta_tpo_movimiento; -- char(2)     ;
         LET afi_rch_afiliatorio_nrp                 = tmp_afi_alta_nrp; -- char(11)    ;       
         LET afi_rch_afiliatorio_f_movimiento        = tmp_afi_alta_f_movimiento; -- char(8)     ;
         LET afi_rch_afiliatorio_curp_rfc            = tmp_afi_alta_curp_rfc; -- char(18)    ;
         LET afi_rch_afiliatorio_t_trabajador        = tmp_afi_alta_t_trabajador; -- decimal(1,0);
         LET afi_rch_afiliatorio_nss                 = tmp_afi_alta_nss; -- char(11)    ;
         LET afi_rch_afiliatorio_nombre              = tmp_afi_alta_nombre; -- char(50)    ;
         LET afi_rch_afiliatorio_presentacion_extemp = tmp_afi_alta_presentacion_extemp; -- decimal(1,0);
         LET afi_rch_afiliatorio_jornada_semana      = tmp_afi_alta_jornada_semana; -- decimal(1,0);
         LET afi_rch_afiliatorio_sdi                 = tmp_afi_alta_sdi; -- decimal(6,0);
         LET afi_rch_afiliatorio_sexo                = tmp_afi_alta_sexo; -- decimal(1,0);
         LET afi_rch_afiliatorio_nss_correcto        = tmp_afi_alta_nss_correcto; -- char(11)    ;
         LET afi_rch_afiliatorio_nombre_correcto     = tmp_afi_alta_nombre_correcto; -- char(50)    ;

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(afi_rch_afiliatorio_tpo_movimiento      ,
                                                            afi_rch_afiliatorio_nrp                 ,
                                                            afi_rch_afiliatorio_f_movimiento        ,
                                                            afi_rch_afiliatorio_curp_rfc            ,
                                                            afi_rch_afiliatorio_t_trabajador        ,
                                                            afi_rch_afiliatorio_nss                 ,
                                                            afi_rch_afiliatorio_nombre              ,
                                                            afi_rch_afiliatorio_presentacion_extemp ,
                                                            afi_rch_afiliatorio_jornada_semana      ,
                                                            afi_rch_afiliatorio_sdi                 ,
                                                            afi_rch_afiliatorio_sexo                ,
                                                            afi_rch_afiliatorio_nss_correcto        ,
                                                            afi_rch_afiliatorio_nombre_correcto     ,
                                                            afi_rch_afiliatorio_cod_rechazo         ,
                                                            p_folio                   );
            
         -- se cuenta una alta rechazada
         LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;
         
         -- se continua con el siguiente registro
         CONTINUE FOREACH;
      END IF

      -- se asigna el curp/rfc
      LET v_rfc_curp = tmp_afi_alta_curp_rfc;

      -- se verifica si se recibio RFC o CURP
      IF ( LENGTH(v_rfc_curp) < 14 ) THEN
         -- es un RFC
         LET v_rfc  = v_rfc_curp;
         LET v_curp = NULL;
      ELSE -- se tiene una curp
         LET v_rfc  = NULL;
         LET v_curp = v_rfc_curp;
      END IF
      
      -- se verifica si se recibio NRP para crear su relacion laboral
      IF ( tmp_afi_alta_nrp IS NOT NULL ) THEN
         -- con relacion laboral
         LET afi_relacion_laboral_ind_relacion = 1;
      ELSE
         -- sin relacion laboral
         LET afi_relacion_laboral_ind_relacion = 0;
      END IF
      
      -- si se marco que solo se de de alta el NRP (v_solo_NRP=1) entonces no se invoca el alta
      -- de derechohabiente
      IF ( v_solo_NRP = 0 ) THEN

         -- se invoca el alta del derechohabiente
         EXECUTE FUNCTION fn_apertura_cuenta_afi(tmp_afi_alta_nss                  -- NSS
                                             ,v_curp                            -- CURP
                                             ,v_rfc                             -- RFC
                                             ,afi_relacion_laboral_ind_relacion -- Ind relacion laboral 
                                             ,tmp_afi_alta_nombre               -- Nombre
                                             ,"S"                               -- tipo trabajador (Solo Infonavit)
                                             ,0                                 -- id_credito (sin credito)
                                             ,p_folio                           -- folio         
                                             ,"A"      )                        -- origen de afiliacion
                       INTO v_id_derechohabiente;
      END IF
                       
      IF ( v_id_derechohabiente IS NOT NULL ) THEN
         -- se cuenta un derechohabiente dado de alta
         LET v_num_altas_aceptadas = v_num_altas_aceptadas + 1;
         
         -- se agrega en relacion laboral
         LET afi_relacion_laboral_id_derechohabiente = v_id_derechohabiente; -- decimal(9,0);
         LET afi_relacion_laboral_nrp                = tmp_afi_alta_nrp; -- char(11)    ;
         LET afi_relacion_laboral_f_alta_nrp         = v_fecha_movimiento; -- date        ;
         LET afi_relacion_laboral_ind_relacion       = 1; -- smallint    ;
         LET afi_relacion_laboral_folio_lote         = p_folio; -- decimal(9,0);
         LET afi_relacion_laboral_f_actualiza        = TODAY; -- date        ;
         LET afi_relacion_laboral_usuario            = p_usuario_cod; -- char(20)    ;

         -- se inserta en relacion laboral
         INSERT INTO afi_relacion_laboral (
            id_derechohabiente ,
            nrp                ,
            f_alta_nrp         ,
            ind_relacion       ,
            folio_lote         ,
            f_actualiza        ,
            usuario            
         ) VALUES (
            afi_relacion_laboral_id_derechohabiente ,
            afi_relacion_laboral_nrp                ,
            afi_relacion_laboral_f_alta_nrp         ,
            afi_relacion_laboral_ind_relacion       ,
            afi_relacion_laboral_folio_lote         ,
            afi_relacion_laboral_f_actualiza        ,
            afi_relacion_laboral_usuario            
         );
      ELSE
      
         -- no se pudo abrir la cuenta, se registra en rechazos
         LET afi_rch_afiliatorio_tpo_movimiento      = tmp_afi_alta_tpo_movimiento; -- char(2)     ;
         LET afi_rch_afiliatorio_espacios            = tmp_afi_alta_espacios; -- char(2)     ;
         LET afi_rch_afiliatorio_nrp                 = tmp_afi_alta_nrp; -- char(11)    ;
         LET afi_rch_afiliatorio_f_movimiento        = tmp_afi_alta_f_movimiento; -- char(8)     ;
         LET afi_rch_afiliatorio_curp_rfc            = tmp_afi_alta_curp_rfc; -- char(18)    ;
         LET afi_rch_afiliatorio_t_trabajador        = tmp_afi_alta_t_trabajador; -- decimal(1,0);
         LET afi_rch_afiliatorio_nss                 = tmp_afi_alta_nss; -- char(11)    ;
         LET afi_rch_afiliatorio_nombre              = tmp_afi_alta_nombre; -- char(50)    ;
         LET afi_rch_afiliatorio_presentacion_extemp = tmp_afi_alta_presentacion_extemp; -- decimal(1,0);
         LET afi_rch_afiliatorio_jornada_semana      = tmp_afi_alta_jornada_semana; -- decimal(1,0);
         LET afi_rch_afiliatorio_sdi                 = tmp_afi_alta_sdi; -- decimal(6,0);
         LET afi_rch_afiliatorio_sexo                = tmp_afi_alta_sexo; -- decimal(1,0);
         LET afi_rch_afiliatorio_nss_correcto        = tmp_afi_alta_nss_correcto; -- char(11)    ;
         LET afi_rch_afiliatorio_nombre_correcto     = tmp_afi_alta_nombre_correcto; -- char(50)    ;
         LET afi_rch_afiliatorio_cod_rechazo         = 3; -- NO SE PUDO ABRIR LA CUENTA

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(afi_rch_afiliatorio_tpo_movimiento      ,
                                                            afi_rch_afiliatorio_nrp                 ,
                                                            afi_rch_afiliatorio_f_movimiento        ,
                                                            afi_rch_afiliatorio_curp_rfc            ,
                                                            afi_rch_afiliatorio_t_trabajador        ,
                                                            afi_rch_afiliatorio_nss                 ,
                                                            afi_rch_afiliatorio_nombre              ,
                                                            afi_rch_afiliatorio_presentacion_extemp ,
                                                            afi_rch_afiliatorio_jornada_semana      ,
                                                            afi_rch_afiliatorio_sdi                 ,
                                                            afi_rch_afiliatorio_sexo                ,
                                                            afi_rch_afiliatorio_nss_correcto        ,
                                                            afi_rch_afiliatorio_nombre_correcto     ,
                                                            afi_rch_afiliatorio_cod_rechazo         ,
                                                            p_folio                   );
            
         -- se cuenta una alta rechazada
         LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;
      END IF
          
   END FOREACH;
           
   --trace "Finaliza el proceso";
   
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
  
    -- se indica cuantas cuentas se abrieron
    LET err_txt = "Altas realizadas: " || v_num_altas_aceptadas || " Altas rechazadas: " || v_num_altas_rechazadas;
  
    -- se devuelve el resultado de la ejecucion del SP
    RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas;
END FUNCTION
;


